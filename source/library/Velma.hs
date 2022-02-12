{-# LANGUAGE RankNTypes #-}

module Velma where

import qualified Control.Monad as Monad
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Distribution.Compat.Lens as Lens
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Simple as Cabal
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Types.BenchmarkInterface as BenchmarkInterface
import qualified Distribution.Types.CondTree as CondTree
import qualified Distribution.Types.HookedBuildInfo as HookedBuildInfo
import qualified Distribution.Types.Lens as Cabal
import qualified Distribution.Types.LocalBuildInfo as LocalBuildInfo
import qualified Distribution.Types.TestSuiteInterface as TestSuiteInterface
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Velma.SymbolicPath as SymbolicPath

defaultMain :: IO ()
defaultMain = Cabal.defaultMainWithHooks userHooks

userHooks :: Cabal.UserHooks
userHooks = Cabal.simpleUserHooks { Cabal.confHook = confHook }

confHook
    :: (Cabal.GenericPackageDescription, HookedBuildInfo.HookedBuildInfo)
    -> Cabal.ConfigFlags
    -> IO LocalBuildInfo.LocalBuildInfo
confHook (gpd1, hbi) cf = do
    gpd2 <- discover gpd1
    Cabal.confHook Cabal.simpleUserHooks (gpd2, hbi) cf

discover
    :: Cabal.GenericPackageDescription -> IO Cabal.GenericPackageDescription
discover = discoverWith listDirectoryRecursively

discoverWith
    :: Monad m
    => (FilePath -> m [FilePath])
    -> Cabal.GenericPackageDescription
    -> m Cabal.GenericPackageDescription
discoverWith f = concatM
    [ overF Cabal.condLibrary (mapM . overF condTreeData $ discoverLibrary f)
    , overF
        Cabal.condSubLibraries
        (mapM . overF (Lens._2 . condTreeData) $ discoverLibrary f)
    , overF
        Cabal.condForeignLibs
        (mapM . overF (Lens._2 . condTreeData) $ discoverForeignLib f)
    , overF
        Cabal.condExecutables
        (mapM . overF (Lens._2 . condTreeData) $ discoverExecutable f)
    , overF
        Cabal.condTestSuites
        (mapM . overF (Lens._2 . condTreeData) $ discoverTestSuite f)
    , overF
        Cabal.condBenchmarks
        (mapM . overF (Lens._2 . condTreeData) $ discoverBenchmark f)
    ]

concatM :: Monad m => [a -> m a] -> a -> m a
concatM = foldr (Monad.>=>) pure

overF :: Functor f => Lens.Lens' s a -> (a -> f a) -> s -> f s
overF l f x = (\y -> Lens.set l y x) <$> f (Lens.view l x)

discoverLibrary
    :: Monad m
    => (FilePath -> m [FilePath])
    -> Cabal.Library
    -> m Cabal.Library
discoverLibrary f = concatM
    [ discoverComponent Cabal.exposedModules (Lens.view Cabal.otherModules) f
    , discoverComponent Cabal.otherModules (Lens.view Cabal.exposedModules) f
    ]

discoverForeignLib
    :: Monad m
    => (FilePath -> m [FilePath])
    -> Cabal.ForeignLib
    -> m Cabal.ForeignLib
discoverForeignLib = discoverComponent Cabal.otherModules $ const []

discoverExecutable
    :: Monad m
    => (FilePath -> m [FilePath])
    -> Cabal.Executable
    -> m Cabal.Executable
discoverExecutable =
    discoverComponent Cabal.otherModules
        $ Maybe.maybeToList
        . filePathToModuleName
        . Lens.view Cabal.modulePath

discoverTestSuite
    :: Monad m
    => (FilePath -> m [FilePath])
    -> Cabal.TestSuite
    -> m Cabal.TestSuite
discoverTestSuite = discoverComponent Cabal.otherModules $ \ts ->
    case Lens.view Cabal.testInterface ts of
        TestSuiteInterface.TestSuiteExeV10 _ fp ->
            Maybe.maybeToList $ filePathToModuleName fp
        TestSuiteInterface.TestSuiteLibV09 _ mn -> [mn]
        TestSuiteInterface.TestSuiteUnsupported _ -> []

discoverBenchmark
    :: Monad m
    => (FilePath -> m [FilePath])
    -> Cabal.Benchmark
    -> m Cabal.Benchmark
discoverBenchmark = discoverComponent Cabal.otherModules $ \ts ->
    case Lens.view Cabal.benchmarkInterface ts of
        BenchmarkInterface.BenchmarkExeV10 _ fp ->
            Maybe.maybeToList $ filePathToModuleName fp
        BenchmarkInterface.BenchmarkUnsupported _ -> []

discoverComponent
    :: (Cabal.HasBuildInfo a, Applicative m)
    => Lens.Lens' a [ModuleName.ModuleName]
    -> (a -> [ModuleName.ModuleName])
    -> (FilePath -> m [FilePath])
    -> a
    -> m a
discoverComponent includeL toExclude f component =
    case maybeRemove velmaDiscoverModuleName $ Lens.view includeL component of
        Nothing -> pure component
        Just include ->
            let
                addDiscovered discovered = Lens.set
                    includeL
                    (ListUtils.nubOrd
                    . mappend include
                    . Set.toAscList
                    . Set.difference discovered
                    . Set.fromList
                    $ toExclude component
                    )
                    component
            in addDiscovered <$> getModuleNames f component

getModuleNames
    :: (Cabal.HasBuildInfo a, Applicative m)
    => (FilePath -> m [FilePath])
    -> a
    -> m (Set.Set ModuleName.ModuleName)
getModuleNames f =
    fmap (Set.fromList . Maybe.mapMaybe filePathToModuleName . mconcat)
        . traverse (\d -> fmap (FilePath.makeRelative d) <$> f d)
        . getHsSourceDirs

condTreeData :: Lens.Lens' (CondTree.CondTree v c a) a
condTreeData f ct =
    fmap (\d -> ct { CondTree.condTreeData = d }) . f $ CondTree.condTreeData
        ct

getHsSourceDirs :: Cabal.HasBuildInfo a => a -> [FilePath]
getHsSourceDirs =
    ListUtils.nubOrd
        . withDefault ["."]
        . fmap SymbolicPath.toFilePath
        . Lens.view Cabal.hsSourceDirs

maybeRemove :: Eq a => a -> [a] -> Maybe [a]
maybeRemove x ys = case ys of
    [] -> Nothing
    y : zs -> if x == y then Just zs else (:) y <$> maybeRemove x zs

velmaDiscoverModuleName :: ModuleName.ModuleName
velmaDiscoverModuleName = ModuleName.fromString "Velma.Discover"

filePathToModuleName :: FilePath -> Maybe ModuleName.ModuleName
filePathToModuleName filePath = do
    base <- FilePath.stripExtension "hs" filePath
    Parsec.simpleParsec . List.intercalate "." $ FilePath.splitDirectories base

withDefault :: Foldable t => t a -> t a -> t a
withDefault d x = if null x then d else x

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively directory = do
    let
        helper filePath = do
            isDirectory <- Directory.doesDirectoryExist filePath
            if isDirectory
                then listDirectoryRecursively filePath
                else pure [filePath]
    entries <- Directory.listDirectory directory
    mconcat <$> traverse (helper . FilePath.combine directory) entries

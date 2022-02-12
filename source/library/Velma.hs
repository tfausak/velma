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

-- | The default entrypoint for this custom setup script. This calls Cabal's
-- 'Cabal.defaultMainWithHooks' with our custom 'userHooks'.
--
-- If you're trying to use Velma in your own project, you should create a
-- @Setup.hs@ file like this:
--
-- > -- Setup.hs
-- > import Velma
-- > main = defaultMain
defaultMain :: IO ()
defaultMain = Cabal.defaultMainWithHooks userHooks

-- | Like Cabal's 'Cabal.simpleUserHooks' but with our custom 'confHook'.
userHooks :: Cabal.UserHooks
userHooks = Cabal.simpleUserHooks { Cabal.confHook = confHook }

-- | Calls 'discover' before handing things off to the 'Cabal.confHook' from
-- Cabal's 'Cabal.simpleUserHooks'.
confHook
    :: (Cabal.GenericPackageDescription, HookedBuildInfo.HookedBuildInfo)
    -> Cabal.ConfigFlags
    -> IO LocalBuildInfo.LocalBuildInfo
confHook (gpd1, hbi) cf = do
    gpd2 <- discover gpd1
    Cabal.confHook Cabal.simpleUserHooks (gpd2, hbi) cf

-- | Simply calls 'discoverWith' with 'listDirectoryRecursively'.
discover
    :: Cabal.GenericPackageDescription -> IO Cabal.GenericPackageDescription
discover = discoverWith listDirectoryRecursively

-- | Discovers modules in all of the components of this package description.
-- You can think of this function as calling 'discoverComponent' for each
-- component: library, sub-libraries, foreign libraries, executables, test
-- suites, and benchmarks.
discoverWith
    :: Monad m
    => (FilePath -> m [FilePath]) -- ^ See 'listDirectoryRecursively'.
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

-- | Applies all of the functions left-to-right using '(Monad.>=>)'.
--
-- >>> let printAnd f x = do { putStrLn $ "x = " <> show x; pure $ f x }
-- >>> concatM [ printAnd (+ 2), printAnd (* 2) ] 3
-- x = 3
-- x = 5
-- 10
concatM :: Monad m => [a -> m a] -> a -> m a
concatM = foldr (Monad.>=>) pure

-- | Like 'Lens.over' except the modification function can perform arbitrary
-- effects.
--
-- >>> overF _2 (Just . (+ 2)) ('a', 3)
-- Just ('a',5)
-- >>> overF _2 (const Nothing) ('a', 3)
-- Nothing
overF :: Functor f => Lens.Lens' s a -> (a -> f a) -> s -> f s
overF l f x = (\y -> Lens.set l y x) <$> f (Lens.view l x)

-- | Thin wrapper around 'discoverComponent' for libraries.
discoverLibrary
    :: Monad m
    => (FilePath -> m [FilePath]) -- ^ See 'listDirectoryRecursively'.
    -> Cabal.Library
    -> m Cabal.Library
discoverLibrary f = concatM
    [ discoverComponent Cabal.exposedModules (Lens.view Cabal.otherModules) f
    , discoverComponent Cabal.otherModules (Lens.view Cabal.exposedModules) f
    ]

-- | Thin wrapper around 'discoverComponent' for foreign libraries.
discoverForeignLib
    :: Applicative m
    => (FilePath -> m [FilePath]) -- ^ See 'listDirectoryRecursively'.
    -> Cabal.ForeignLib
    -> m Cabal.ForeignLib
discoverForeignLib = discoverComponent Cabal.otherModules $ const []

-- | Thin wrapper around 'discoverComponent' for executables.
discoverExecutable
    :: Applicative m
    => (FilePath -> m [FilePath]) -- ^ See 'listDirectoryRecursively'.
    -> Cabal.Executable
    -> m Cabal.Executable
discoverExecutable =
    discoverComponent Cabal.otherModules
        $ Maybe.maybeToList
        . filePathToModuleName
        . Lens.view Cabal.modulePath

-- | Thin wrapper around 'discoverComponent' for test suites.
discoverTestSuite
    :: Applicative m
    => (FilePath -> m [FilePath]) -- ^ See 'listDirectoryRecursively'.
    -> Cabal.TestSuite
    -> m Cabal.TestSuite
discoverTestSuite = discoverComponent Cabal.otherModules $ \ts ->
    case Lens.view Cabal.testInterface ts of
        TestSuiteInterface.TestSuiteExeV10 _ fp ->
            Maybe.maybeToList $ filePathToModuleName fp
        TestSuiteInterface.TestSuiteLibV09 _ mn -> [mn]
        TestSuiteInterface.TestSuiteUnsupported _ -> []

-- | Thin wrapper around 'discoverComponent' for benchmarks.
discoverBenchmark
    :: Applicative m
    => (FilePath -> m [FilePath]) -- ^ See 'listDirectoryRecursively'.
    -> Cabal.Benchmark
    -> m Cabal.Benchmark
discoverBenchmark = discoverComponent Cabal.otherModules $ \ts ->
    case Lens.view Cabal.benchmarkInterface ts of
        BenchmarkInterface.BenchmarkExeV10 _ fp ->
            Maybe.maybeToList $ filePathToModuleName fp
        BenchmarkInterface.BenchmarkUnsupported _ -> []

-- | Discovers modules in the given component, using the provided lens to
-- select which field to update. This is the main workhorse of the package.
discoverComponent
    :: (Cabal.HasBuildInfo a, Applicative m)
    => Lens.Lens' a [ModuleName.ModuleName]
    -- ^ Typically something like 'Cabal.exposedModules'.
    -> (a -> [ModuleName.ModuleName])
    -- ^ This function is used to get a list of module names to avoid
    -- discovering. For example if you're populating 'Cabal.exposedModules',
    -- then you'll want to use 'Cabal.otherModules' here to avoid discovering
    -- duplicates.
    -> (FilePath -> m [FilePath]) -- ^ See 'listDirectoryRecursively'.
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

-- | Gets module names for the given component, using the provided function to
-- list directory contents. This basically just glues together
-- 'getHsSourceDirs', 'listDirectoryRecursively', and 'filePathToModuleName'.
getModuleNames
    :: (Cabal.HasBuildInfo a, Applicative m)
    => (FilePath -> m [FilePath]) -- ^ See 'listDirectoryRecursively'.
    -> a
    -> m (Set.Set ModuleName.ModuleName)
getModuleNames f =
    fmap (Set.fromList . Maybe.mapMaybe filePathToModuleName . mconcat)
        . traverse (\d -> fmap (FilePath.makeRelative d) <$> f d)
        . getHsSourceDirs

-- | A lens for the 'CondTree.condTreeData' field.
condTreeData :: Lens.Lens' (CondTree.CondTree v c a) a
condTreeData f ct =
    fmap (\d -> ct { CondTree.condTreeData = d }) . f $ CondTree.condTreeData
        ct

-- | Gets @hs-source-dirs@ from the given component.
--
-- - If @hs-source-dirs@ isn't set (or is empty), this will return the inferred
-- directory, which is the current directory (@"."@).
-- - Duplicates are removed from the result using 'ListUtils.nubOrd'.
-- - This should probably return @SymbolicPath@ values, but that type was only
-- introduced in recent versions (>= 3.6) of Cabal.
getHsSourceDirs :: Cabal.HasBuildInfo a => a -> [FilePath]
getHsSourceDirs =
    ListUtils.nubOrd
        . withDefault ["."]
        . fmap SymbolicPath.toFilePath
        . Lens.view Cabal.hsSourceDirs

-- | Attempts to remove an element from the list. If it succeeds, returns the
-- list without that element. If it fails, returns 'Nothing'.
--
-- >>> maybeRemove 'b' "abc"
-- Just "ac"
-- >>> maybeRemove 'z' "abc"
-- Nothing
--
-- Note that only the first matching element is removed.
--
-- >>> maybeRemove 'b' "abcb"
-- Just "acb"
maybeRemove :: Eq a => a -> [a] -> Maybe [a]
maybeRemove x ys = case ys of
    [] -> Nothing
    y : zs -> if x == y then Just zs else (:) y <$> maybeRemove x zs

-- | The magic module name that enables automatic module discovery with Velma.
-- This will always be @Velma.Discover@.
velmaDiscoverModuleName :: ModuleName.ModuleName
velmaDiscoverModuleName = ModuleName.fromString "Velma.Discover"

-- | Attempts to convert a 'FilePath' into a 'ModuleName.ModuleName'. This
-- works by stripping certain extensions, then converting directory separators
-- into module separators, and finally trying to parse that as a module name.
--
-- >>> filePathToModuleName "Velma.hs"
-- Just (ModuleName "Velma")
-- >>> filePathToModuleName "Velma/SymbolicPath.hs"
-- Just (ModuleName "Velma.SymbolicPath")
-- >>> filePathToModuleName "README.markdown"
-- Nothing
-- >>> filePathToModuleName "library/Velma.hs"
-- Nothing
filePathToModuleName :: FilePath -> Maybe ModuleName.ModuleName
filePathToModuleName filePath = do
    base <- FilePath.stripExtension "hs" filePath
    Parsec.simpleParsec . List.intercalate "." $ FilePath.splitDirectories base

-- | Returns the given default value if the other value is 'null'. For example:
--
-- >>> withDefault ["default"] []
-- ["default"]
-- >>> withDefault ["default"] ["something"]
-- ["something"]
withDefault
    :: Foldable t
    => t a -- ^ The default value.
    -> t a
    -> t a
withDefault d x = if null x then d else x

-- | Lists all of the directory contents recursively. The returned file paths
-- will include the directory prefix, unlike 'Directory.listDirectory'. For
-- example:
--
-- >>> listDirectoryRecursively "source/library"
-- ["source/library/Velma.hs","source/library/Velma/SymbolicPath.hs"]
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

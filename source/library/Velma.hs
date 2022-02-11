module Velma where

import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Simple as Cabal
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Types.Benchmark as Benchmark
import qualified Distribution.Types.BuildInfo as BuildInfo
import qualified Distribution.Types.CondTree as CondTree
import qualified Distribution.Types.Executable as Executable
import qualified Distribution.Types.ForeignLib as ForeignLib
import qualified Distribution.Types.GenericPackageDescription as GenericPackageDescription
import qualified Distribution.Types.HookedBuildInfo as HookedBuildInfo
import qualified Distribution.Types.Library as Library
import qualified Distribution.Types.LocalBuildInfo as LocalBuildInfo
import qualified Distribution.Types.TestSuite as TestSuite
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Velma.SymbolicPath as SymbolicPath

defaultMain :: IO ()
defaultMain = Cabal.defaultMainWithHooks userHooks

userHooks :: Cabal.UserHooks
userHooks = Cabal.simpleUserHooks { Cabal.confHook = confHook }

confHook
    :: ( GenericPackageDescription.GenericPackageDescription
       , HookedBuildInfo.HookedBuildInfo
       )
    -> Cabal.ConfigFlags
    -> IO LocalBuildInfo.LocalBuildInfo
confHook (gpd1, hbi) cf = do
    gpd2 <- discoverPackageModules gpd1
    Cabal.confHook Cabal.simpleUserHooks (gpd2, hbi) cf

discoverPackageModules
    :: GenericPackageDescription.GenericPackageDescription
    -> IO GenericPackageDescription.GenericPackageDescription
discoverPackageModules gpd = do
    let buildInfos = mconcat
            [ fmap (Library.libBuildInfo . CondTree.condTreeData)
            . Maybe.maybeToList
            $ GenericPackageDescription.condLibrary gpd
            , Library.libBuildInfo
            . CondTree.condTreeData
            . snd
            <$> GenericPackageDescription.condSubLibraries gpd
            , ForeignLib.foreignLibBuildInfo
            . CondTree.condTreeData
            . snd
            <$> GenericPackageDescription.condForeignLibs gpd
            , Executable.buildInfo
            . CondTree.condTreeData
            . snd
            <$> GenericPackageDescription.condExecutables gpd
            , TestSuite.testBuildInfo
            . CondTree.condTreeData
            . snd
            <$> GenericPackageDescription.condTestSuites gpd
            , Benchmark.benchmarkBuildInfo
            . CondTree.condTreeData
            . snd
            <$> GenericPackageDescription.condBenchmarks gpd
            ]
        hsSourceDirs =
            Set.toList . Set.unions $ fmap getHsSourceDirs buildInfos
        toMap directory =
            Map.singleton directory
                . fmap (FilePath.makeRelative directory)
                <$> listDirectoryRecursively directory
    directoryContents <- foldTraverse toMap hsSourceDirs
    pure $ discoverPackageModulesWith gpd directoryContents

discoverPackageModulesWith
    :: GenericPackageDescription.GenericPackageDescription
    -> Map.Map FilePath [FilePath]
    -> GenericPackageDescription.GenericPackageDescription
discoverPackageModulesWith gpd directoryContents = gpd
    { GenericPackageDescription.condLibrary =
        CondTree.mapTreeData (discoverLibraryModules directoryContents)
            <$> GenericPackageDescription.condLibrary gpd
    , GenericPackageDescription.condSubLibraries =
        overSnd
                (CondTree.mapTreeData
                $ discoverLibraryModules directoryContents
                )
            <$> GenericPackageDescription.condSubLibraries gpd
    , GenericPackageDescription.condForeignLibs =
        overSnd
                (CondTree.mapTreeData
                $ discoverForeignLibModules directoryContents
                )
            <$> GenericPackageDescription.condForeignLibs gpd
    , GenericPackageDescription.condExecutables =
        overSnd
                (CondTree.mapTreeData
                $ discoverExecutableModules directoryContents
                )
            <$> GenericPackageDescription.condExecutables gpd
    , GenericPackageDescription.condTestSuites =
        overSnd
                (CondTree.mapTreeData
                $ discoverTestSuiteModules directoryContents
                )
            <$> GenericPackageDescription.condTestSuites gpd
    , GenericPackageDescription.condBenchmarks =
        overSnd
                (CondTree.mapTreeData
                $ discoverBenchmarkModules directoryContents
                )
            <$> GenericPackageDescription.condBenchmarks gpd
    }

overSnd :: (b -> c) -> (a, b) -> (a, c)
overSnd f (x, y) = (x, f y)

discoverBenchmarkModules
    :: Map.Map FilePath [FilePath]
    -> Benchmark.Benchmark
    -> Benchmark.Benchmark
discoverBenchmarkModules directoryContents benchmark =
    let
        oldBuildInfo = Benchmark.benchmarkBuildInfo benchmark
        newBuildInfo =
            discoverOtherModules directoryContents Set.empty oldBuildInfo
    in benchmark { Benchmark.benchmarkBuildInfo = newBuildInfo }

discoverTestSuiteModules
    :: Map.Map FilePath [FilePath]
    -> TestSuite.TestSuite
    -> TestSuite.TestSuite
discoverTestSuiteModules directoryContents testSuite =
    let
        oldBuildInfo = TestSuite.testBuildInfo testSuite
        newBuildInfo =
            discoverOtherModules directoryContents Set.empty oldBuildInfo
    in testSuite { TestSuite.testBuildInfo = newBuildInfo }

discoverExecutableModules
    :: Map.Map FilePath [FilePath]
    -> Executable.Executable
    -> Executable.Executable
discoverExecutableModules directoryContents executable =
    let
        oldBuildInfo = Executable.buildInfo executable
        newBuildInfo =
            discoverOtherModules directoryContents Set.empty oldBuildInfo
    in executable { Executable.buildInfo = newBuildInfo }

discoverForeignLibModules
    :: Map.Map FilePath [FilePath]
    -> ForeignLib.ForeignLib
    -> ForeignLib.ForeignLib
discoverForeignLibModules directoryContents foreignLib =
    let
        oldBuildInfo = ForeignLib.foreignLibBuildInfo foreignLib
        newBuildInfo =
            discoverOtherModules directoryContents Set.empty oldBuildInfo
    in foreignLib { ForeignLib.foreignLibBuildInfo = newBuildInfo }

discoverLibraryModules
    :: Map.Map FilePath [FilePath] -> Library.Library -> Library.Library
discoverLibraryModules directoryContents =
    discoverOtherLibraryModules directoryContents
        . discoverExposedLibraryModules directoryContents

discoverExposedLibraryModules
    :: Map.Map FilePath [FilePath] -> Library.Library -> Library.Library
discoverExposedLibraryModules directoryContents library =
    case maybeRemove velmaDiscover $ Library.exposedModules library of
        Nothing -> library
        Just exposedModules ->
            let
                directories = getHsSourceDirs $ Library.libBuildInfo library
                entries = concat . Map.elems $ Map.restrictKeys
                    directoryContents
                    directories
                excluded =
                    Set.fromList
                        . BuildInfo.otherModules
                        $ Library.libBuildInfo library
                discovered =
                    filter (`Set.notMember` excluded)
                        $ Maybe.mapMaybe filePathToModuleName entries
                allModules = ListUtils.nubOrd $ exposedModules <> discovered
            in library { Library.exposedModules = allModules }

discoverOtherLibraryModules
    :: Map.Map FilePath [FilePath] -> Library.Library -> Library.Library
discoverOtherLibraryModules directoryContents library =
    let
        oldBuildInfo = Library.libBuildInfo library
        excluded = Set.fromList $ Library.exposedModules library
        newBuildInfo =
            discoverOtherModules directoryContents excluded oldBuildInfo
    in library { Library.libBuildInfo = newBuildInfo }

discoverOtherModules
    :: Map.Map FilePath [FilePath]
    -> Set.Set ModuleName.ModuleName
    -> BuildInfo.BuildInfo
    -> BuildInfo.BuildInfo
discoverOtherModules directoryContents excluded buildInfo =
    case maybeRemove velmaDiscover $ BuildInfo.otherModules buildInfo of
        Nothing -> buildInfo
        Just otherModules ->
            let
                directories = getHsSourceDirs buildInfo
                entries = concat . Map.elems $ Map.restrictKeys
                    directoryContents
                    directories
                discovered =
                    filter (`Set.notMember` excluded)
                        $ Maybe.mapMaybe filePathToModuleName entries
                allModules = ListUtils.nubOrd $ otherModules <> discovered
            in buildInfo { BuildInfo.otherModules = allModules }

getHsSourceDirs :: BuildInfo.BuildInfo -> Set.Set FilePath
getHsSourceDirs =
    Set.fromList
        . withDefault ["."]
        . fmap SymbolicPath.toFilePath
        . BuildInfo.hsSourceDirs

maybeRemove :: Eq a => a -> [a] -> Maybe [a]
maybeRemove x ys = case ys of
    [] -> Nothing
    y : zs -> if x == y then Just zs else (:) y <$> maybeRemove x zs

velmaDiscover :: ModuleName.ModuleName
velmaDiscover = ModuleName.fromString "Velma.Discover"

filePathToModuleName :: FilePath -> Maybe ModuleName.ModuleName
filePathToModuleName filePath = do
    base <- FilePath.stripExtension "hs" filePath
    Parsec.simpleParsec . List.intercalate "." $ FilePath.splitDirectories base

withDefault :: Foldable t => t a -> t a -> t a
withDefault d x = if null x then d else x

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively directory = do
    entries <- listDirectory directory
    foldTraverse listDirectoryHelper entries

listDirectory :: FilePath -> IO [FilePath]
listDirectory directory =
    fmap (FilePath.combine directory) <$> Directory.listDirectory directory

foldTraverse
    :: (Applicative m, Monoid b, Traversable t) => (a -> m b) -> t a -> m b
foldTraverse f = fmap Foldable.fold . traverse f

listDirectoryHelper :: FilePath -> IO [FilePath]
listDirectoryHelper entry = do
    isDirectory <- Directory.doesDirectoryExist entry
    if isDirectory then listDirectoryRecursively entry else pure [entry]

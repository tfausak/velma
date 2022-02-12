import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec
import qualified Velma

main :: IO ()
main = Hspec.hspec . Hspec.describe "Velma" $ do

    Hspec.it "does not modify a basic package" $ do
        gpd <- parse $ unlines ["name: p", "version: 0"]
        discover [] gpd `Hspec.shouldReturn` gpd

    Hspec.it "does nothing without Velma.Discover module" $ do
        gpd <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M1"
            ]
        discover [(".", ["M1.hs", "M2.hs"])] gpd `Hspec.shouldReturn` gpd

    Hspec.it "removes Velma.Discover module" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            ]
        expected <-
            parse $ unlines
                ["cabal-version: >= 1.2", "name: p", "version: 0", "library"]
        discover [] input `Hspec.shouldReturn` expected

    Hspec.it "does not remove multiple Velma.Discover modules" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            ]
        discover [] input `Hspec.shouldReturn` expected

    Hspec.it "ignores things under conditionals" $ do
        gpd <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " if true"
            , "  exposed-modules: Velma.Discover"
            ]
        discover [] gpd `Hspec.shouldReturn` gpd

    Hspec.it "discovers a shallow module" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "discovers a deep module" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M1.M2"
            ]
        discover [(".", ["M1/M2.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "deduplicates discovered modules" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M"
            ]
        discover [(".", ["M.hs", "M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "sorts discovered modules" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M1 M2"
            ]
        discover [(".", ["M2.hs", "M1.hs"])] input
            `Hspec.shouldReturn` expected

    Hspec.it "allows explicit modules before discovery" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M1 Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M1 M2"
            ]
        discover [(".", ["M1.hs", "M2.hs"])] input
            `Hspec.shouldReturn` expected

    Hspec.it "allows explicit modules after discovery" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover M2"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M2 M1"
            ]
        discover [(".", ["M1.hs", "M2.hs"])] input
            `Hspec.shouldReturn` expected

    Hspec.it "discovers modules from a custom directory" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            , " hs-source-dirs: d"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M"
            , " hs-source-dirs: d"
            ]
        discover [("d", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "discovers modules from multiple directories" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            , " hs-source-dirs: d1 d2"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M1 M2"
            , " hs-source-dirs: d1 d2"
            ]
        discover [("d1", ["M1.hs"]), ("d2", ["M2.hs"])] input
            `Hspec.shouldReturn` expected

    Hspec.it "does not duplicate other modules" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            , " other-modules: M"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " other-modules: M"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "discovers other modules" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " other-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " other-modules: M"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "does not duplicate exposed modules" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M"
            , " other-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "prefers discovering exposed modules" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: Velma.Discover"
            , " other-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library"
            , " exposed-modules: M"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "discovers exposed modules in a sub library" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library l"
            , " exposed-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library l"
            , " exposed-modules: M"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "discovers other modules in a sub library" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library l"
            , " other-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "library l"
            , " other-modules: M"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "discovers other modules in an executable" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "executable e"
            , " other-modules: Velma.Discover"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "executable e"
            , " other-modules: M"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "does not duplicate main module in executable" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "executable e"
            , " other-modules: Velma.Discover"
            , " main-is: M.hs"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "executable e"
            , " main-is: M.hs"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "discovers other modules in a foreign library" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "foreign-library f"
            , " other-modules: Velma.Discover"
            , " type: native-static"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "foreign-library f"
            , " other-modules: M"
            , " type: native-static"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "discovers other modules in a test suite" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "test-suite t"
            , " other-modules: Velma.Discover"
            , " type: exitcode-stdio-1.0"
            , " main-is: T.hs"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "test-suite t"
            , " other-modules: M"
            , " type: exitcode-stdio-1.0"
            , " main-is: T.hs"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "does not duplicate main module in test suite" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "test-suite t"
            , " other-modules: Velma.Discover"
            , " type: exitcode-stdio-1.0"
            , " main-is: M.hs"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "test-suite t"
            , " type: exitcode-stdio-1.0"
            , " main-is: M.hs"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "does not duplicate main module in detailed test suite" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "test-suite t"
            , " other-modules: Velma.Discover"
            , " type: detailed-0.9"
            , " test-module: M"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "test-suite t"
            , " type: detailed-0.9"
            , " test-module: M"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "discovers other modules in a benchmark" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "benchmark b"
            , " other-modules: Velma.Discover"
            , " type: exitcode-stdio-1.0"
            , " main-is: B.hs"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "benchmark b"
            , " other-modules: M"
            , " type: exitcode-stdio-1.0"
            , " main-is: B.hs"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

    Hspec.it "does not duplicate main module in benchmark" $ do
        input <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "benchmark b"
            , " other-modules: Velma.Discover"
            , " type: exitcode-stdio-1.0"
            , " main-is: M.hs"
            ]
        expected <- parse $ unlines
            [ "cabal-version: >= 1.2"
            , "name: p"
            , "version: 0"
            , "benchmark b"
            , " type: exitcode-stdio-1.0"
            , " main-is: M.hs"
            ]
        discover [(".", ["M.hs"])] input `Hspec.shouldReturn` expected

parse :: String -> IO Cabal.GenericPackageDescription
parse string = do
    let text = Text.pack string
    let byteString = Text.encodeUtf8 text
    let parseResult = Cabal.parseGenericPackageDescription byteString
    let (pWarnings, result) = Cabal.runParseResult parseResult
    Monad.unless (null pWarnings) . fail $ show pWarnings
    either (fail . show) pure result

discover
    :: Monad m
    => [(FilePath, [FilePath])]
    -> Cabal.GenericPackageDescription
    -> m Cabal.GenericPackageDescription
discover = Velma.discoverWith . files

files :: Applicative m => [(FilePath, [FilePath])] -> FilePath -> m [FilePath]
files ts k =
    pure . fmap (FilePath.combine k) . Maybe.fromMaybe [] $ lookup k ts

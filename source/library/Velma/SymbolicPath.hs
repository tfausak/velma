{-# LANGUAGE CPP #-}

-- https://github.com/haskell/cabal/pull/7121
module Velma.SymbolicPath where

{-
# if MIN_VERSION_Cabal(3, 5, 0)
-}

import qualified Distribution.Utils.Path

type SymbolicPath = Distribution.Utils.Path.SymbolicPath

toFilePath :: SymbolicPath a b -> FilePath
toFilePath = Distribution.Utils.Path.getSymbolicPath

{-
# else
-}

type SymbolicPath a b = FilePath

toFilePath :: SymbolicPath a b -> FilePath
toFilePath = id

{-
# endif
-}

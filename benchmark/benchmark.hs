{-# LANGUAGE UnicodeSyntax #-}

-- -------------------------------------------------------------------------- --
-- |
-- Module: Main
-- Copyright: (c) Lars Kuhtz <lakuhtz@gmail.com> 2017
-- License: MIT
-- Maintainer: lakuhtz@gmail.com
-- Stability: experimental
--
-- -------------------------------------------------------------------------- --

module Main
( main
) where

import Benchmarks
import Criterion.Main
import Data.ByteString.Random

main ∷ IO ()
main = defaultMain $ largeBenchmarks [Impl "Data.ByteString.Random" random]


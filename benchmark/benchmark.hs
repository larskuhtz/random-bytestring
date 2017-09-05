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
import qualified Data.ByteString.Random.MWC as MWC
import qualified Data.ByteString.Random.PCG as PCG

main ∷ IO ()
main = defaultMain $ compareBenchmarks
    [ Impl "random-bytestring+mwc" MWC.random
    , Impl "random-bytestring+pcg" PCG.random
    ]


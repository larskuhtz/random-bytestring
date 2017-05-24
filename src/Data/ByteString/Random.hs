{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns #-}

-- -------------------------------------------------------------------------- --
-- |
-- Module: Data.ByteString.Random
-- Copyright: (c) Lars Kuhtz <lakuhtz@gmail.com> 2017
-- License: MIT
-- Maintainer: lakuhtz@gmail.com
-- Stability: experimental
-- -------------------------------------------------------------------------- --

module Data.ByteString.Random
( random
, randomGen
) where

import Control.Exception (bracketOnError)

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackAddressLen)
import Data.Word (Word8, Word64)

import Foreign (mallocBytes, poke, plusPtr, free, castPtr)

import GHC.Ptr (Ptr(..))

import Numeric.Natural (Natural)

import System.Random.MWC (uniform, GenIO, withSystemRandom)

-- $setup
-- >>> import qualified Data.ByteString as B
-- >>> import Test.QuickCheck

-- | Generate a random bytestring of length n. The PRNG is seeded
-- from the system randomness source.
--
-- prop> ioProperty $ ((fromIntegral n ===) . B.length) <$> random n
-- prop> n > 4 ==> ioProperty $ (/=) <$> random n <*> random n
--
random
    ∷ Natural
        -- ^ Length of the result bytestring in bytes
    → IO ByteString
random n = withSystemRandom $ \gen → randomGen gen n

-- | Generate a random bytestring of length n using the given
-- PRNG.
--
randomGen
    ∷ GenIO
        -- ^ A PRNG that is used to generate random bytes.
    → Natural
        -- ^ Length of the result bytestring in bytes
    → IO ByteString
randomGen gen n =
    bracketOnError (mallocBytes len8) free $ \ptr@(Ptr !addr) → do
        {-# SCC "go" #-} go ptr
        {-# SCC "pack" #-} unsafePackAddressLen len8 addr
  where
    len8, len64 ∷ Int
    !len8 = fromIntegral n
    !len64 = len8 `div` 8

    go ∷ Ptr Word64 → IO ()
    go !startPtr = loop64 startPtr
      where
        -- Would it help to add more manual unrolling levels?
        -- How smart is the compiler about unrolling loops?

        -- Generate 64bit values
        fin64Ptr ∷ Ptr Word64
        !fin64Ptr = startPtr `plusPtr` (len64 * 8)

        loop64 ∷ Ptr Word64 → IO ()
        loop64 !curPtr
            | curPtr < fin64Ptr = {-# SCC "loop64" #-} do
                !b ← uniform gen ∷ IO Word64
                {-# SCC "poke64" #-} poke curPtr b
                loop64 $ {-# SCC "ptr_inc" #-} curPtr `plusPtr` 8
            | otherwise = loop8 $ castPtr curPtr

        -- Generate 8bit values
        fin8Ptr ∷ Ptr Word8
        !fin8Ptr = startPtr `plusPtr` len8

        loop8 ∷ Ptr Word8 → IO ()
        loop8 !curPtr
            | curPtr < fin8Ptr = {-# SCC "loop8" #-} do
                !b ← uniform gen ∷ IO Word8
                poke curPtr b
                loop8 $ curPtr `plusPtr` 1
            | otherwise = return ()


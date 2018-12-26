{-# LANGUAGE
    DataKinds
  #-}

module Data.ByteString.Z85.Internal where

import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as V
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Bits ((.&.), shiftR)
import Data.Word (Word32)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import Data.Traversable (traverse)
import Control.Monad (forM_)
import Control.Monad.ST (runST)



type Base85 = Word32


newtype Z85Char = Z85Char
  { getZ85Char :: Char
  } deriving (Eq, Ord, Show, Generic)

instance Arbitrary Z85Char where
  arbitrary = oneof (V.toList z85Chars)


type Z85Chunk = Vector 5 Z85Char


printZ85Chunk :: Z85Chunk -> Text
printZ85Chunk = T.pack . map getZ85Char . V.toList



z85Chars :: Vector 85 Char
z85Chars = Z85Char <$> fromJust (V.fromList "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#")


lookupZ85Char :: Base85 -> Z85Char
lookupZ85Char idx = z85Chars `V.unsafeIndex` (fromIntegral idx)


charCodeToBase85 :: Vector 96 Base85
charCodeToBase85 = fromJust (V.fromList
  [ nan, 0x44, nan, 0x54, 0x53, 0x52, 0x48, nan
  , 0x4B, 0x4C, 0x46, 0x41, nan, 0x3F, 0x3E, 0x45
  , 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07
  , 0x08, 0x09, 0x40, nan, 0x49, 0x42, 0x4A, 0x47
  , 0x51, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A
  , 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32
  , 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A
  , 0x3B, 0x3C, 0x3D, 0x4D, nan, 0x4E, 0x43, nan
  , nan, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10
  , 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18
  , 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20
  , 0x21, 0x22, 0x23, 0x4F, nan, 0x50, 0x00, 0x00
  ])
  where
    nan = 0


z85CharToIndex :: Z85Char -> Int
z85CharToIndex c = ord c - 32


lookupBase85 :: Z85Char -> Base85
lookupBase85 c = V.unsafeIndex charCodeToBase85 (z85CharToIndex c)


encodeWord :: Word32 -> Z85Chunk
encodeWord word =
  let value = runST $ do
        valueRef <- newSTRef 0
        forM_ [0..3] $ \j ->
          let base256 :: Word32
              base256 = 0xFF
              byteShift :: Int
              byteShift = 8 * (4 - j - 1)
              byteChunk :: Word32
              byteChunk = shiftR word byteShift .&. base256
          in  modifySTRef valueRef (\val -> (val * 256) + byteChunk)
        readSTRef valueRef
      getC :: Word32 -> Z85Char
      getC n =
        let divisor :: Word32
            divisor = 85 ^ n
            idx :: Base85
            idx = (value `div` divisor) `mod` 85
        in  lookupZ85Char idx
  in  V.fromTuple (getC 4, getC 3, getC 2, getC 1, getC 0)



decodeWord :: Z85Chunk -> Word32
decodeWord chunk =
  let value :: Word32
      value = runST $ do
        valueRef <- newSTRef 0
        let addPartValue part =
              modifySTRef valueRef (\val -> (val * 85) + part)
        _ <- traverse addPartValue base85Values
        readSTRef valueRef
      divisor :: Int -> Word32
      divisor n = 256 ^ n
  in  runST $ do
        wordRef <- newSTRef 0
        forM_ [3,2..0] $ \n ->
          let go word =
                let magnitude = word * 256
                    dust = (value `div` divisor n) `mod` 256
                in  magnitude + dust
          in  modifySTRef wordRef go
        readSTRef wordRef
  where
    base85Values :: Vector 5 Base85
    base85Values = lookupBase85 <$> chunk

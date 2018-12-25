{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.ByteString.Z85 where

import Data.ByteString.Z85.Internal (Z85Char, decodeWord, encodeWord)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Sized as Vn


data EncodeError
  = BSMod4


encode :: ByteString -> Either EncodeError Text
encode bs
  | BS.length bs `mod` 4 /= 0 = Left BSMod4
  | otherwise = Left BSMod4


data DecodeError
  = TextMod5


decode :: Text -> Either DecodeError ByteString
decode t'
  | tLen `mod` 5 /= 0 = Left TextMod5
  | otherwise = Right (fst (T.foldl' go (BS.empty, []) t))
  where
    tLen = T.length t
    valuesLen = tLen `div` 5
    bytesLen = valuesLen * 4
    go :: Char -> (ByteString, Vector Z85Char) -> (ByteString, Vector Z85char)
    go c (acc,charsFoFar)
      | V.length charsSoFar /= 5 = (acc, charsSoFar `V.snoc` c)
      | otherwise = case Vn.toSized charsSoFar of
          Just cs ->
            let word = decodeWord cs
            in  
          Nothing -> error $ "couldn't caste chars so far: " ++ show charsSoFar

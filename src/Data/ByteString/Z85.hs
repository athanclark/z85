{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.ByteString.Z85 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text as T


data EncodeError
  = BSMod4


encode :: ByteString -> Either EncodeError Text
encode bs
  | BS.length bs `mod` 4 /= 0 = Left BSMod4
  | otherwise = Left BSMod4


data DecodeError
  = TextMod5


decode :: Text -> Either DecodeError ByteString
decode t
  | T.length t `mod` 5 /= 0 = Left TextMod5
  | otherwise = Left TextMod5

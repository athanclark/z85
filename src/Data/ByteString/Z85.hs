{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.ByteString.Z85 where

import Data.ByteString (ByteString)

import Data.Text (Text)


data EncodeError
  = BSMod4


encode :: ByteString -> Either EncodeError Text
encode _ = Left BSMod4


data DecodeError
  = TestMod5


decode :: Text -> Either DecodeError ByteString
decode _ = Left TestMod5

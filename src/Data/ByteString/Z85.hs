{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.ByteString.Z85 where

import Data.ByteString (ByteString)

import Data.Text (Text)


data EncodeError
  = BSNotMod4


encode :: ByteString -> Either EncodeError Text
encode _ = Left BSNotMod4


data DecodeError
  = TestNotMod5


decode :: Text -> Either DecodeError ByteString
decode _ = Left TestNotMod5

{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.ByteString.Z85 where

import Data.ByteString (ByteString)

import Data.Text (Text)



encode :: ByteString -> Text
encode _ = ""


decode :: Text -> Either String ByteString
decode _ = Left ""

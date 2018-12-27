module Data.Attoparsec.ByteString.Z85 where

import Data.ByteString.Z85.Internal (Z85Char (..), Z85Chunk, z85Chars, encodeWord, printZ85Chunk)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.Binary (anyWord32be)
import Data.Text (Text)
import Data.Word (Word32)
import Data.Foldable (fold)
import Control.Applicative (many)



anyWord32leEncoded :: Parser Z85Chunk
anyWord32leEncoded = encodeWord <$> anyWord32be


z85Encoded :: Parser Text
z85Encoded = fold <$> many (printZ85Chunk <$> anyWord32leEncoded)

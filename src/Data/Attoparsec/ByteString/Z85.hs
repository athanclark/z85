module Data.Attoparsec.ByteString.Z85 where

import Data.ByteString.Z85.Internal (Z85Chunk, encodeWord, printZ85Chunk)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.Binary (anyWord32be)
import Data.Text (Text)
import Data.Foldable (fold)
import Control.Applicative (many)



anyWord32beEncoded :: Parser Z85Chunk
anyWord32beEncoded = encodeWord <$> anyWord32be


z85Encoded :: Parser Text
z85Encoded = fold <$> many (printZ85Chunk <$> anyWord32beEncoded)

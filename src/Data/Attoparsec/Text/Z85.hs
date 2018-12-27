module Data.Attoparsec.Text.Z85 where

import Data.ByteString.Z85.Internal (Z85Char (..), Z85Chunk, z85Chars, decodeWord)
import qualified Data.Vector.Sized as V
import Data.Maybe (fromJust)
import Data.Word (Word32)
import Data.Foldable (fold)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (word32BE, toLazyByteString)
import Data.Attoparsec.Text (Parser, char, satisfy, inClass)
import Control.Applicative (many)
import Control.Monad (replicateM)



anyZ85Char :: Parser Z85Char
anyZ85Char = Z85Char <$> satisfy (inClass (getZ85Char <$> V.toList z85Chars))


z85Char :: Z85Char -> Parser Z85Char
z85Char (Z85Char c) = Z85Char <$> char c


anyZ85Chunk :: Parser Z85Chunk
anyZ85Chunk =
  (fromJust . V.fromList) <$> replicateM 5 anyZ85Char


anyZ85ChunkDecoded :: Parser Word32
anyZ85ChunkDecoded = decodeWord <$> anyZ85Chunk


z85Decoded :: Parser ByteString
z85Decoded = (toStrict . toLazyByteString . fold) <$> many (word32BE <$> anyZ85ChunkDecoded)

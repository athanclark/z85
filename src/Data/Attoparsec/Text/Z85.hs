module Data.Attoparsec.Text.Z85 where

import Data.ByteString.Z85.Internal (Z85Char (..), Z85Chunk, z85Chars, decodeWord, isZ85Char)
import qualified Data.Vector.Sized as V
import Data.Maybe (fromJust)
import Data.Word (Word32)
import Data.Foldable (fold)
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (word32BE, toLazyByteString)
import Data.Attoparsec.Text (Parser, char, satisfy, inClass, (<?>))
import Control.Applicative (many, optional)
import Control.Monad (replicateM)



anyZ85Char :: Parser Z85Char
anyZ85Char = Z85Char <$> satisfy isZ85Char <?> "Z85Char character class"


z85Char :: Z85Char -> Parser Z85Char
z85Char (Z85Char c) = Z85Char <$> char c


anyZ85Chunk :: Parser Z85Chunk
anyZ85Chunk =
  (fromJust . V.fromList) <$> replicateM 5 anyZ85Char


anyZ85ChunkDecoded :: Parser Word32
anyZ85ChunkDecoded = decodeWord <$> anyZ85Chunk


z85Decoded :: Parser ByteString
z85Decoded = do
  mX <- optional anyZ85ChunkDecoded
  case mX of
    Nothing -> pure empty
    Just x ->
      let x' = toStrict (toLazyByteString (word32BE x))
      in  (x' <>) <$> z85Decoded

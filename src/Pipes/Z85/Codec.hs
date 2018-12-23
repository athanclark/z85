module Pipes.Z85.Codec where

import Data.ByteString.Z85.Internal (Z85Chunk, encodeWord, decodeWord)

import Pipes (Pipe, await, yield)
import Data.Word (Word32)
import Data.Text (Text)
import Data.ByteString (ByteString)



decodeSentence :: Monad m => Pipe Z85Chunk Word32 m ()
decodeSentence = forever (await >>= yield . decodeWord)


encodeSentence :: Monad m => Pipe Word32 Z85Chunk m ()
encodeSentence = forever (await >>= yield . encodeWord)



encode :: Monad m => MonadBaseControl ST m => STRef Text -> Pipe Text ByteString m ()
encode =
  cs <- await


decode :: Monad m => Pipe ByteString Text m ()
decode = ?

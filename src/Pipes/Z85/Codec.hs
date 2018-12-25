{-# LANGUAGE
    OverloadedLists
  #-}

module Pipes.Z85.Codec where

import Data.ByteString.Z85.Internal (Z85Chunk, encodeWord, decodeWord)

import Pipes (Pipe, await, yield)
import Data.Word (Word32)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Sized as Vs
import Control.Monad.Base (MonadBase (liftBase))



-- decodeSentence :: Monad m => Pipe Z85Chunk Word32 m ()
-- decodeSentence = forever (await >>= yield . decodeWord)


-- encodeSentence :: Monad m => Pipe Word32 Z85Chunk m ()
-- encodeSentence = forever (await >>= yield . encodeWord)



encode :: MonadBase ST m => STRef ByteString -> Pipe ByteString Text m ()
encode leftoverRef =
  prev <- liftBase (readSTRef leftoverRef)
  cs <- await
  let (cs',leftover) =
  

decode :: MonadBase ST m => STRef Text -> Pipe Text ByteString m ()
decode leftoverRef =
  prev <- liftBase (readSTRef leftoverRef)
  cs <- await
  let (cs',leftover) =
        let go :: Text -> (Vector Word32, Text)
            go x
              | T.length x < 5 = ([], x)
              | otherwise =
                let (pre,suf) = T.splitAt 5 x
                    (cs'',l) = go suf
                in  ((decodeWord $ Vs.fromList $ T.unpack pre) `V.cons` cs'',l)
        in  go (prev <> cs)
  liftBase (writeSTRef leftoverRef leftover)
  yield $ LBS.toStrict $ BSB.toLazyByteString $ V.foldMap BSB.word32LE cs'

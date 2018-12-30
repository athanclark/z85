{-# LANGUAGE
    OverloadedLists
  #-}

module Pipes.Z85.Codec where

import Data.Attoparsec.ByteString.Z85 (z85Encoded)
import Data.Attoparsec.Text.Z85 (z85Decoded)

import Pipes (Pipe, Producer, await, yield)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text as AT
-- import Data.Vector (Vector)
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Sized as Vs
import Data.IORef (IORef, readIORef, writeIORef)
import Control.Monad.State (StateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (throwIO)
import System.IO.Error (userError)




-- encode :: Pipe ByteString Text m ()
-- encode =



-- encode' :: Monad m => Parser ByteString m (Maybe (Either ParsingError Text))
-- encode' = parse z85Encoded


-- decode' :: Monad m => Parser Text m (Maybe (Either ParsingError ByteString))
-- decode' = parse z85Decoded




-- decodeSentence :: Monad m => Pipe Z85Chunk Word32 m ()
-- decodeSentence = forever (await >>= yield . decodeWord)


-- encodeSentence :: Monad m => Pipe Word32 Z85Chunk m ()
-- encodeSentence = forever (await >>= yield . encodeWord)



-- encode :: MonadBase ST m => STRef ByteString -> Pipe ByteString Text m ()
-- encode leftoverRef =
--   prev <- liftIO (readSTRef leftoverRef)
--   cs <- await
--   let (cs',leftover) =
  


decode :: MonadIO m
       => IORef (Either Text (Text -> AT.Result ByteString)) -- ^ Either unparsed input, or overparsed input
       -> Pipe Text ByteString m ()
decode leftoverRef = do
  mPrev <- liftIO (readIORef leftoverRef)
  r <-  let f = case mPrev of
              Right prevF -> prevF
              Left prevT -> \current -> AT.parse z85Decoded (prevT <> current)
        in  f <$> await
  case r of
    e@(AT.Fail i _ _) -> liftIO $ do
      writeIORef leftoverRef (Left i)
      throwIO $ userError $ show e
    AT.Partial f -> do
      liftIO (writeIORef leftoverRef (Right f))
      decode leftoverRef
    AT.Done i x -> do
      liftIO (writeIORef leftoverRef (Left i))
      yield x
      decode leftoverRef

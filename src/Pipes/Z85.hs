{-# LANGUAGE
    OverloadedLists
  , DeriveGeneric
  #-}

module Pipes.Z85 where

import Data.Attoparsec.ByteString.Z85 (z85Encoded)
import Data.Attoparsec.Text.Z85 (z85Decoded)

import Pipes (Pipe, await, yield)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text as AT
import Data.IORef (IORef, readIORef, writeIORef)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (throw, Exception)
import GHC.Generics (Generic)



data Z85ParsingError
  = EncodeError [String] String
  | DecodeError [String] String
  deriving (Eq, Show, Generic)
instance Exception Z85ParsingError


encode :: MonadIO m
       => IORef (Either ByteString (ByteString -> AB.Result Text)) -- ^ Either unparsed input, or overparsed input
       -> Pipe ByteString Text m ()
encode leftoverRef = do
  mPrev <- liftIO (readIORef leftoverRef)
  r <-  let f = case mPrev of
              Right prevF -> prevF
              Left prevB -> \current -> AB.parse z85Encoded (prevB <> current)
        in  f <$> await
  case r of
    AT.Fail i es e -> liftIO $ do
      writeIORef leftoverRef (Left i)
      throw (EncodeError es e)
    AT.Partial f -> do
      liftIO (writeIORef leftoverRef (Right f))
      encode leftoverRef
    AT.Done i x -> do
      liftIO (writeIORef leftoverRef (Left i))
      yield x
      encode leftoverRef



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
    AT.Fail i es e -> liftIO $ do
      writeIORef leftoverRef (Left i)
      throw (DecodeError es e)
    AT.Partial f -> do
      liftIO (writeIORef leftoverRef (Right f))
      decode leftoverRef
    AT.Done i x -> do
      liftIO (writeIORef leftoverRef (Left i))
      yield x
      decode leftoverRef

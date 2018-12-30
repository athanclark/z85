{-# LANGUAGE
    OverloadedLists
  , DeriveGeneric
  #-}

module Pipes.Z85 where

import Data.Attoparsec.ByteString.Z85 (z85Encoded)
import Data.Attoparsec.Text.Z85 (z85Decoded)

import Pipes (Pipe, await, yield)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text as AT
import Data.IORef (IORef, readIORef, writeIORef)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (throw, Exception)
import GHC.Generics (Generic)



data Z85ParsingError
  = EncodeError [String] String
  | DecodeError [String] String
  | CantCompletePartial
  deriving (Eq, Show, Generic)
instance Exception Z85ParsingError


encode :: MonadIO m
       => IORef ByteString -- ^ Unparsed input
       -> Pipe ByteString Text m ()
encode leftoverRef = do
  prev <- liftIO (readIORef leftoverRef)
  r <-  let f current = AB.parse z85Encoded (prev <> current)
        in  f <$> await
  let onFail i es e = liftIO $ do
        writeIORef leftoverRef i
        throw (EncodeError es e)
      onDone i x = do
        liftIO (writeIORef leftoverRef i)
        yield x
        encode leftoverRef
  case r of
    AB.Fail i es e -> onFail i es e
    AB.Partial f ->
      case f BS.empty of
        AB.Fail i es e -> onFail i es e
        AB.Done i x -> onDone i x
        AB.Partial _ ->
          throw CantCompletePartial
    AB.Done i x -> onDone i x



decode :: MonadIO m
       => IORef Text -- ^ Unparsed input
       -> Pipe Text ByteString m ()
decode leftoverRef = do
  prev <- liftIO (readIORef leftoverRef)
  r <-  let f current = AT.parse z85Decoded (prev <> current)
        in  f <$> await
  let onFail i es e = liftIO $ do
        writeIORef leftoverRef i
        throw (DecodeError es e)
      onDone i x = do
        liftIO (writeIORef leftoverRef i)
        yield x
        decode leftoverRef
  case r of
    AT.Fail i es e -> onFail i es e
    AT.Partial f ->
      case f T.empty of
        AT.Fail i es e -> onFail i es e
        AT.Done i x -> onDone i x
        AT.Partial _ ->
          throw CantCompletePartial
    AT.Done i x -> onDone i x

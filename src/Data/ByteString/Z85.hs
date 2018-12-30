{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.ByteString.Z85 where

import qualified Pipes.Z85 as PZ
import qualified Pipes.ByteString as PB
import qualified Pipes.Text as PT
import Pipes ((>->), Producer, Effect, for, runEffect)

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.IORef (readIORef, newIORef, modifyIORef)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import System.IO.Unsafe (unsafePerformIO)



data Z85Error
  = TextNotMod5
  | BSNotMod4
  | ParsingError PZ.Z85ParsingError
  deriving (Eq, Show)


-- | Fails by checking the length is @mod 4@ - warning, checking length on a potentially infinite stream may not be possible, or may
-- cause faulty memory management. Use 'encode\'' if you want to verify offsets after.
encode :: ByteString -> Either Z85Error Text
encode bs
  | LBS.length bs `mod` 4 /= 0 = Left BSNotMod4
  | otherwise = unsafePerformIO $ do
    let go :: IO Text
        go = do
          leftoverRef <- newIORef ""
          let encoded :: Producer T.Text IO ()
              encoded = PB.fromLazy bs >-> PZ.encode leftoverRef
          PT.toLazyM encoded
    eX <- try go
    pure $ case eX of
      Left e -> Left (ParsingError e)
      Right x -> Right x

-- | Fails by checking the residual unparsed input after doing all the work -
-- use 'encode' if you want to check length early.
encode' :: ByteString -> Either Z85Error Text
encode' bs = unsafePerformIO $ do
  let go :: IO (Either Z85Error Text)
      go = do
        leftoverRef <- newIORef ""
        let encoded :: Producer T.Text IO ()
            encoded = PB.fromLazy bs >-> PZ.encode leftoverRef
        x <- PT.toLazyM encoded
        leftover <- readIORef leftoverRef
        pure $
          if BS.length leftover /= 0
          then Left BSNotMod4
          else Right x
  eX <- try go
  pure $ case eX of
    Left e -> Left (ParsingError e)
    Right x -> x



-- | Fails by checking the length is @mod 5@ - warning, checking length on a potentially infinite stream may not be possible, or may
-- cause faulty memory management. Use 'decode\'' if you want to verify offsets after.
decode :: Text -> Either Z85Error ByteString
decode t
  | LT.length t `mod` 5 /= 0 = Left TextNotMod5
  | otherwise = unsafePerformIO $ do
    let go :: IO ByteString
        go = do
          leftoverRef <- newIORef ""
          let decoded :: Producer BS.ByteString IO ()
              decoded = PT.fromLazy t >-> PZ.decode leftoverRef
          PB.toLazyM decoded
    eX <- try go
    pure $ case eX of
      Left e -> Left (ParsingError e)
      Right x -> Right x

-- | Fails by checking the residual unparsed input after doing all the work -
-- use 'decode' if you want to check length early.
decode' :: Text -> Either Z85Error ByteString
decode' t = unsafePerformIO $ do
  let go :: IO (Either Z85Error ByteString)
      go = do
        leftoverRef <- newIORef ""
        let decoded :: Producer BS.ByteString IO ()
            decoded = PT.fromLazy t >-> PZ.decode leftoverRef
        x <- PB.toLazyM decoded
        leftover <- readIORef leftoverRef
        pure $
          if T.length leftover /= 0
          then Left TextNotMod5
          else Right x
  eX <- try go
  pure $ case eX of
    Left e -> Left (ParsingError e)
    Right x -> x



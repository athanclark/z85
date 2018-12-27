{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.ByteString.Z85 where

-- import qualified Pipes.Z85.Codec as PZ
-- import qualified Pipes.ByteString as PB

-- import Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString.Lazy as LBS

-- import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy as LT


-- data EncodeError
--   = BSMod4


-- encode :: ByteString -> Either EncodeError Text
-- encode bs
--   | BS.length bs `mod` 4 /= 0 = Left BSMod4
--   | otherwise = Right (runEffect (PB.fromLazy bs >~ PZ.decode))


-- data DecodeError
--   = TextMod5


-- decode :: Text -> Either DecodeError ByteString
-- decode t'
--   | tLen `mod` 5 /= 0 = Left TextMod5
--   | otherwise = Right (fst (T.foldl' go (BS.empty, []) t))
--   where
--     tLen = T.length t
--     valuesLen = tLen `div` 5
--     bytesLen = valuesLen * 4
--     go :: Char -> (ByteString, Vector Z85Char) -> (ByteString, Vector Z85char)
--     go c (acc,charsFoFar)
--       | V.length charsSoFar /= 5 = (acc, charsSoFar `V.snoc` c)
--       | otherwise = case Vn.toSized charsSoFar of
--           Just cs ->
--             let word = decodeWord cs
--             in  
--           Nothing -> error $ "couldn't caste chars so far: " ++ show charsSoFar

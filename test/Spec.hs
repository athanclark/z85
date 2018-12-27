{-# LANGUAGE
    OverloadedStrings
  #-}

import Data.ByteString.Z85.Internal (encodeWord, decodeWord)
import Data.Attoparsec.ByteString.Z85 (z85Encoded)

import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.QuickCheck (Property, (===))

import Data.Word (Word32)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (word8, toLazyByteString)
import qualified Data.Attoparsec.ByteString as AB




main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ testGroup "Unit Tests"
    [ testCase "\"HelloWorld\" == 0x86,0x4F,0xD2,0x6F,0xB5,0x59,0xF7,0x5B" $
      let bs = toStrict $ toLazyByteString $ foldMap word8 [0x86,0x4F,0xD2,0x6F,0xB5,0x59,0xF7,0x5B]
      in  AB.parseOnly z85Encoded bs @=? Right "HelloWorld"
    ]
  , testGroup "Property Tests"
    [ testProperty "decode / encode word iso" decodeWordIso
    ]
  ]




decodeWordIso :: Word32 -> Property
decodeWordIso w = w === decodeWord (encodeWord w)

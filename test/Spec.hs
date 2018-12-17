import Data.ByteString.Z85.Internal (encodeWord, decodeWord)

import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, (===))

import Data.Word (Word32)



main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ testProperty "decode / encode word iso" decodeWordIso
  ]




decodeWordIso :: Word32 -> Property
decodeWordIso w = w === decodeWord (encodeWord w)

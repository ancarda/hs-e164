module Main where

import Data.Either   (Either(Left))
import Data.Function (($))
import Data.Maybe    (Maybe(Nothing, Just), fromJust)
import Data.String   (String)
import Test.Hspec    (hspec, describe, it, shouldBe)
import Text.Show     (show)
import System.IO     (IO)

import Telephony.PSTN.E164
import Telephony.PSTN.E164.NANP

justParseE164 :: String -> E164Number
justParseE164 s = fromJust $ parseE164 s

main :: IO ()
main = hspec $ do
  describe "E164Number" $
    it "should parse E.164 numbers" $ do
      -- TODO(ancarda): These tests can be activated if these methods become
      -- reachable by the unit tests. This might be possible if there are
      -- internal modules that aren't exported to the outside world, but are
      -- visible to the tests.
      --
      -- I have no idea how to accomplish that.
      --
      -- These can't be exported as they are internal implementation details.
      {-
      onlyNumbers "+1 (202) 555-8601"   `shouldBe` "12025558601"
      filter isNumeric "1-800-555-8601" `shouldBe` "18005558601"
      -}

      isUSCA "202" `shouldBe` (Just $ Left "US")
      isUSCA "403" `shouldBe` (Just $ Left "CA")

      {-
      candidates "+447447099060" `shouldBe` Just (44, Left "GB")
      candidates "+12025558601"  `shouldBe` Just (1,  Left "US")
      candidates "+14035558601"  `shouldBe` Just (1,  Left "CA")
      -}

      prefix  (justParseE164 "1-202-555-8601")  `shouldBe` 1
      country (justParseE164 "1-202-555-8601")  `shouldBe` Just "US"
      purpose (justParseE164 "1-202-555-8601")  `shouldBe` Nothing

      show    (justParseE164 "+1.210.555.7890") `shouldBe` "+1.2105557890"
      show    (justParseE164 "+1.800.555.7890") `shouldBe` "+1.8005557890"
      show    (justParseE164 "+1.368.555.7890") `shouldBe` "+1.3685557890"

      human   (justParseE164 "+1.210.555.7890") `shouldBe` "(210) 555-7890"
      human   (justParseE164 "+1.800.555.7890") `shouldBe` "1-800-555-7890"
      human   (justParseE164 "+1.368.555.7890") `shouldBe` "+1.3685557890"

      e164arpa (justParseE164 "+1.210.555.7890") `shouldBe`
        "0.9.8.7.5.5.5.0.1.2.1.e164.arpa."
      e164arpa (justParseE164 "+44.7700900461")  `shouldBe`
        "1.6.4.0.0.9.0.0.7.7.4.4.e164.arpa."

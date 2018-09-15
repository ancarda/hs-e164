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
main = hspec $
  describe "E164Number" $ do
    it "should parse E.164 numbers" $ do
      -- TODO(ancarda): These can be tested once moved to internal modules.
      {-
      onlyNumbers "+1 (202) 555-8601"   `shouldBe` "12025558601"
      filter isNumeric "1-800-555-8601" `shouldBe` "18005558601"

      candidates "+447447099060" `shouldBe` Just (44, Left "GB")
      candidates "+12025558601"  `shouldBe` Just (1,  Left "US")
      candidates "+14035558601"  `shouldBe` Just (1,  Left "CA")
      -}

      prefix  (justParseE164 "1-202-555-8601") `shouldBe` 1
      country (justParseE164 "1-202-555-8601") `shouldBe` Just "US"
      purpose (justParseE164 "1-202-555-8601") `shouldBe` Nothing

    it "should support the North American Numbering Plan (+1)" $ do
      isUSCA "202" `shouldBe` (Just $ Left "US")
      isUSCA "403" `shouldBe` (Just $ Left "CA")

    it "should convert E164Number to string" $ do
      show  (justParseE164 "+1.210.555.7890") `shouldBe` "+1.2105557890"
      show  (justParseE164 "+1.800.555.7890") `shouldBe` "+1.8005557890"
      show  (justParseE164 "+1.368.555.7890") `shouldBe` "+1.3685557890"
      human (justParseE164 "+1.210.555.7890") `shouldBe` "(210) 555-7890"
      human (justParseE164 "+1.800.555.7890") `shouldBe` "1-800-555-7890"
      human (justParseE164 "+1.368.555.7890") `shouldBe` "+1.3685557890"

    it "should do DNS mapping of E.164 numbers" $ do
      e164arpa (justParseE164 "+1.210.555.7890") `shouldBe`
        "0.9.8.7.5.5.5.0.1.2.1.e164.arpa."
      e164arpa (justParseE164 "+44.7700900461")  `shouldBe`
        "1.6.4.0.0.9.0.0.7.7.4.4.e164.arpa."

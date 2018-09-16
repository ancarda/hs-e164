-- |
-- Copyright   : (c) Mark Dain 2018
-- License     : MIT
-- Maintainer  : mark@markdain.net
--
-- An [E.164](https://en.wikipedia.org/wiki/E.164) analyzer and validator.
module Telephony.PSTN.E164 (
  E164Number,
  parseE164,
  human,
  Country,
  country,
  Prefix,
  prefix,
  Number,
  Purpose (TollFree, PremiumRate),
  purpose,
  e164arpa
) where

import Data.Bool     (Bool(True, False))
import Data.Char     (Char)
import Data.Either   (Either(Left, Right))
import Data.Eq       ((==))
import Data.Function (($))
import Data.List     (drop, take, intersperse, filter, reverse, length, (++))
import Data.Maybe    (Maybe(Nothing, Just), isNothing, fromJust)
import Data.String   (String)
import Text.Show     (show)
import Data.Tuple    (fst, uncurry)

import Telephony.PSTN.E164.Types (Country, E164Number (E164Number), Number,
  Prefix, Purpose(TollFree, PremiumRate))
import Telephony.PSTN.E164.NANP  (isUSCA_)

-- | Resolves an E164Number to a human representable string.
--
-- This function pays attention to 'Prefix', 'Country', and 'Purpose' to
-- generate a Human comfortable string of this number. For example, +1 U.S.
-- numbers are returned as @(XXX) XXX-XXXX@ but @+1 800@ numbers are returned
-- as @1-800-XXX-XXXX@ as an international prefix is very common for that
-- purpose as 1-800 numbers aren't tied to a specific country.
--
-- This function attempts to make a best-effort guess of how most people would
-- write the given number as. No accuracy guarantees are, or can, be made.
-- For custom formatting, you can use 'country', 'purpose', and 'prefix' to
-- extract information from an 'E164Number'. If you have suggestions on how
-- a specific number may be formatted differently, please raise an issue on
-- GitHub.
--
-- For numbers where no specific implementation exists, a generic
-- implementation is used, which returns a string like @+XXX.XXXXXXXXXXXX@.
human :: E164Number -> String
human (E164Number _ (Left  c) n) | c == "US" =
  "(" ++ take 3 n ++ ") " ++ take 3 (drop 3 n) ++ "-" ++ drop 6 n
human (E164Number p (Right _) n) | p == 1    =
  "1-" ++ take 3 n ++ "-" ++ take 3 (drop 3 n) ++ "-" ++ drop 6 n
human n = show n

candidates :: String -> Maybe (Prefix, Either Country Purpose)
candidates s | take 2 s == "+1"   = isUSCA_ s
candidates s | take 3 s == "+20"  = Just (20,  Left "EG") -- Egypt
candidates s | take 3 s == "+27"  = Just (27,  Left "ZA") -- South Africa
candidates s | take 3 s == "+30"  = Just (30,  Left "GR") -- Greece
candidates s | take 3 s == "+31"  = Just (31,  Left "NL") -- Netherlands
candidates s | take 3 s == "+32"  = Just (32,  Left "BE") -- Belgium
candidates s | take 3 s == "+33"  = Just (33,  Left "FR") -- France
candidates s | take 3 s == "+34"  = Just (34,  Left "ES") -- Spain
candidates s | take 3 s == "+36"  = Just (36,  Left "HU") -- Hungary
candidates s | take 3 s == "+39"  = Just (39,  Left "IT") -- Italy
candidates s | take 3 s == "+40"  = Just (40,  Left "RO") -- Romania
candidates s | take 3 s == "+44"  = Just (44,  Left "GB") -- United Kingdom
candidates s | take 3 s == "+51"  = Just (51,  Left "PE") -- Peru
candidates s | take 3 s == "+52"  = Just (52,  Left "MX") -- Mexico
candidates s | take 3 s == "+53"  = Just (53,  Left "CU") -- Cuba
candidates s | take 3 s == "+54"  = Just (54,  Left "AR") -- Argentina
candidates s | take 3 s == "+55"  = Just (55,  Left "BR") -- Brazil
candidates s | take 3 s == "+56"  = Just (56,  Left "CL") -- Chile
candidates s | take 3 s == "+57"  = Just (57,  Left "CO") -- Colombia
candidates s | take 3 s == "+58"  = Just (58,  Left "VE") -- Venezuela
candidates s | take 3 s == "+60"  = Just (60,  Left "MY") -- Malaysia
candidates s | take 3 s == "+61"  = Just (61,  Left "AU") -- Australia
candidates s | take 3 s == "+7"   = Just (7,   Left "RU") -- Russia
candidates _                      = Nothing

isNumeric :: Char -> Bool
isNumeric '0' = True
isNumeric '1' = True
isNumeric '2' = True
isNumeric '3' = True
isNumeric '4' = True
isNumeric '5' = True
isNumeric '6' = True
isNumeric '7' = True
isNumeric '8' = True
isNumeric '9' = True
isNumeric _   = False

onlyNumbers :: String -> String
onlyNumbers = filter isNumeric

-- | Return the country associated with this number. As not all numbers are
-- from prefixes assigned to a number, this function may return 'Nothing' if
-- that's the case.
country :: E164Number -> Maybe Country
country (E164Number _ (Left c) _) = Just c
country _                         = Nothing

-- | Return the purpose associated with this number. As most numbers are for
-- regular use within a country, this function is likely to return 'Nothing'.
-- Where this is 'Nothing', the number can be treated as "No Explicit Purpose".
purpose :: E164Number -> Maybe Purpose
purpose (E164Number _ (Right x) _) = Just x
purpose _                          = Nothing

-- | Return the prefix for the numbering plan associated with this number. This
-- shouldn't be used if you want the 'Country' associated, use the 'country'
-- function for that.
prefix :: E164Number -> Prefix
prefix (E164Number p _ _) = p

rdns :: String -> String
rdns s = intersperse '.' $ reverse s

-- | Convert this number to [an e164.arpa domain](https://en.wikipedia.org/wiki/E.164#DNS_mapping_of_E.164_numbers).
-- This can be used -- in theory -- to perform a DNS lookup for a SIP address
-- for any E.164 number.
--
-- This follows the same reverse-pattern that @ip6.arpa@ uses:
--
-- @1-800-555-7890 becomes 0.9.8.7.5.5.5.0.0.8.1.e164.arpa.@
--
-- In practice, this feature isn't widely implemented.
e164arpa :: E164Number -> String
e164arpa (E164Number p _ n) = rdns n ++ "." ++ rdns (show p) ++ ".e164.arpa."

-- | Attempt to parse an
-- [E.164 telephone number](https://en.wikipedia.org/wiki/E.164). This
-- __must be prefixed__ with the international dialing prefix and local area
-- code.
--
-- For example, if your number is @555-7890@ and you live in Dallas, TX, your
-- full number is likely to be @+1-214-555-7890@. To indicate this is a full
-- number including the international prefix, a plus is prefixed at the start.
parseE164 :: String -> Maybe E164Number
parseE164 s = do
  let candidate = candidates $ "+" ++ onlyNumbers s
  if isNothing candidate then
    Nothing
  else
    Just $ uncurry E164Number (fromJust candidate) (drop (length $ show $ fst $ fromJust candidate) $ onlyNumbers s)

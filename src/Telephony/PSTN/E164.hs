-- |
-- Copyright   : (c) Mark Dain 2018
-- License     : MIT
-- Maintainer  : mark@markdain.net
--
-- An [E.164](https://en.wikipedia.org/wiki/E.164) analyzer and validator.
module Telephony.PSTN.E164 (
  E164Number,
  parseE164,
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
import Data.Eq       (Eq, (==))
import Data.Function (($))
import Data.Int      (Int)
import Data.List     (drop, take, intersperse, filter, reverse, length, (++))
import Data.Maybe    (Maybe(Nothing, Just), isNothing, fromJust)
import Data.String   (String)
import Text.Show     (Show, show)
import Data.Tuple    (fst, uncurry)

-- | An [E.164 Number](https://en.wikipedia.org/wiki/E.164) is a standardized
-- way of writing a Public Switched Telephone Network (PSTN) phone number.
--
-- __Example:__ @+1 800 555 7890@
data E164Number = E164Number Prefix (Either Country Purpose) Number
  deriving (Eq)

-- | Resolves an E164Number to a human representable string.
--
-- This function pays attention to 'Prefix', 'Country', and 'Purpose' to
-- generate a Human comfortable string of this number. For example, +1 U.S.
-- numbers are returned as @(XXX) XXX-XXXX@ but @+1 800@ numbers are returned
-- as @1-800-XXX-XXXX@ as an international prefix is very common for that
-- purpose as 1-800 numbers aren't tied to a specific country.
--
-- This function attempts to make a best-effort guess of how most people would
-- write the given number as. No accuracy garauntees are, or can, be made.
-- For custom formatting, you can use 'country', 'purpose', and 'prefix' to
-- extract information from an 'E164Number'. If you have suggestions on how
-- a specific number may be formatted differently, please raise an issue on
-- GitHub.
--
-- For numbers where no specific implementation exists, a generic
-- implementation is used, which returns a string like @+XXX.XXXXXXXXXXXX@.
instance Show E164Number where
  show :: E164Number -> String
  show (E164Number _ (Left  c) n) | c == "US" = "(" ++ take 3 n ++ ") " ++ take 3 (drop 3 n) ++ "-" ++ drop 6 n
  show (E164Number p (Left  _) n)             = "+" ++ show p ++ "." ++ n
  show (E164Number p (Right _) n) | p == 1    = "1-" ++ take 3 n ++ "-" ++ take 3 (drop 3 n) ++ "-" ++ drop 6 n
  show (E164Number p (Right _) n)             = "+" ++ show p ++ "." ++ n

-- | Country a phone number is associated with. This is a 2 character
-- [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes) code
-- such as @US@ for the United States of America.
type Country = String

-- | E164 prefix, such as @+1@. This usually determines the numbering plan,
-- country, and purpose, but __cannot reliably be used by itself__; @+1@ is
-- used by both the USA and Canada. Use 'country' and 'purpose' for analysis.
--
-- This is stored without the leading "+" prefix.
type Prefix = Int

-- | The right hand side of a phone number, e.g. without the 'Prefix'. For
-- example, if @+1@ is the prefix, Number will be @555-555-5555@.
--
-- This is stored without any spaces.
type Number = String

-- | Purpose is used instead of 'Country' for phone numbers associated with a
-- particular service or use rather than general purpose use for a 'Country'.
-- For example, @+1-800-XXX-XXXX@ is 'TollFree'.
data Purpose = TollFree | PremiumRate
  deriving (Eq)

isUSCA :: String -> Maybe (Either Country Purpose)
isUSCA "205" = Just $ Left "US" -- Alabama, US
isUSCA "251" = Just $ Left "US" -- Alabama, US
isUSCA "256" = Just $ Left "US" -- Alabama, US
isUSCA "334" = Just $ Left "US" -- Alabama, US
isUSCA "938" = Just $ Left "US" -- Alabama, US
-- +1 907 is assigned to Alaska, US but can also be used by less people in
-- British Columbia. https://en.wikipedia.org/wiki/Area_code_907
isUSCA "907" = Just $ Left "US" -- Alaska, US (also used in British Columbia, CA)
isUSCA "480" = Just $ Left "US" -- Arizona, US
isUSCA "520" = Just $ Left "US" -- Arizona, US
isUSCA "602" = Just $ Left "US" -- Arizona, US
isUSCA "623" = Just $ Left "US" -- Arizona, US
isUSCA "928" = Just $ Left "US" -- Arizona, US
isUSCA "327" = Just $ Left "US" -- Arkansas, US
isUSCA "479" = Just $ Left "US" -- Arkansas, US
isUSCA "501" = Just $ Left "US" -- Arkansas, US
isUSCA "870" = Just $ Left "US" -- Arkansas, US
isUSCA "209" = Just $ Left "US" -- California, US
isUSCA "213" = Just $ Left "US" -- California, US
isUSCA "279" = Just $ Left "US" -- California, US
isUSCA "310" = Just $ Left "US" -- California, US
isUSCA "323" = Just $ Left "US" -- California, US
isUSCA "341" = Just $ Left "US" -- California, US
isUSCA "408" = Just $ Left "US" -- California, US
isUSCA "415" = Just $ Left "US" -- California, US
isUSCA "424" = Just $ Left "US" -- California, US
isUSCA "442" = Just $ Left "US" -- California, US
isUSCA "510" = Just $ Left "US" -- California, US
isUSCA "530" = Just $ Left "US" -- California, US
isUSCA "559" = Just $ Left "US" -- California, US
isUSCA "562" = Just $ Left "US" -- California, US
isUSCA "619" = Just $ Left "US" -- California, US
isUSCA "626" = Just $ Left "US" -- California, US
isUSCA "628" = Just $ Left "US" -- California, US
isUSCA "650" = Just $ Left "US" -- California, US
isUSCA "657" = Just $ Left "US" -- California, US
isUSCA "661" = Just $ Left "US" -- California, US
isUSCA "669" = Just $ Left "US" -- California, US
isUSCA "707" = Just $ Left "US" -- California, US
isUSCA "714" = Just $ Left "US" -- California, US
isUSCA "747" = Just $ Left "US" -- California, US
isUSCA "760" = Just $ Left "US" -- California, US
isUSCA "805" = Just $ Left "US" -- California, US
isUSCA "818" = Just $ Left "US" -- California, US
isUSCA "820" = Just $ Left "US" -- California, US
isUSCA "831" = Just $ Left "US" -- California, US
isUSCA "858" = Just $ Left "US" -- California, US
isUSCA "909" = Just $ Left "US" -- California, US
isUSCA "916" = Just $ Left "US" -- California, US
isUSCA "925" = Just $ Left "US" -- California, US
isUSCA "949" = Just $ Left "US" -- California, US
isUSCA "951" = Just $ Left "US" -- California, US
isUSCA "303" = Just $ Left "US" -- Colorado, US
isUSCA "719" = Just $ Left "US" -- Colorado, US
isUSCA "720" = Just $ Left "US" -- Colorado, US
isUSCA "970" = Just $ Left "US" -- Colorado, US
isUSCA "203" = Just $ Left "US" -- Connecticut, US
isUSCA "475" = Just $ Left "US" -- Connecticut, US
isUSCA "860" = Just $ Left "US" -- Connecticut, US
isUSCA "959" = Just $ Left "US" -- Connecticut, US
isUSCA "302" = Just $ Left "US" -- Delaware, US
isUSCA "202" = Just $ Left "US" -- District of Columbia, US
isUSCA "239" = Just $ Left "US" -- Florida, US
isUSCA "305" = Just $ Left "US" -- Florida, US
isUSCA "321" = Just $ Left "US" -- Florida, US
isUSCA "352" = Just $ Left "US" -- Florida, US
isUSCA "386" = Just $ Left "US" -- Florida, US
isUSCA "407" = Just $ Left "US" -- Florida, US
isUSCA "561" = Just $ Left "US" -- Florida, US
isUSCA "689" = Just $ Left "US" -- Florida, US
isUSCA "727" = Just $ Left "US" -- Florida, US
isUSCA "754" = Just $ Left "US" -- Florida, US
isUSCA "772" = Just $ Left "US" -- Florida, US
isUSCA "786" = Just $ Left "US" -- Florida, US
isUSCA "813" = Just $ Left "US" -- Florida, US
isUSCA "850" = Just $ Left "US" -- Florida, US
isUSCA "863" = Just $ Left "US" -- Florida, US
isUSCA "904" = Just $ Left "US" -- Florida, US
isUSCA "941" = Just $ Left "US" -- Florida, US
isUSCA "954" = Just $ Left "US" -- Florida, US
isUSCA "229" = Just $ Left "US" -- Georgia, US
isUSCA "404" = Just $ Left "US" -- Georgia, US
isUSCA "470" = Just $ Left "US" -- Georgia, US
isUSCA "478" = Just $ Left "US" -- Georgia, US
isUSCA "678" = Just $ Left "US" -- Georgia, US
isUSCA "706" = Just $ Left "US" -- Georgia, US
isUSCA "762" = Just $ Left "US" -- Georgia, US
isUSCA "770" = Just $ Left "US" -- Georgia, US
isUSCA "912" = Just $ Left "US" -- Georgia, US
isUSCA "808" = Just $ Left "US" -- Hawaii, US
isUSCA "208" = Just $ Left "US" -- Idaho, US
isUSCA "986" = Just $ Left "US" -- Idaho, US
isUSCA "217" = Just $ Left "US" -- Illinois, US
isUSCA "224" = Just $ Left "US" -- Illinois, US
isUSCA "309" = Just $ Left "US" -- Illinois, US
isUSCA "312" = Just $ Left "US" -- Illinois, US
isUSCA "331" = Just $ Left "US" -- Illinois, US
isUSCA "447" = Just $ Left "US" -- Illinois, US
isUSCA "464" = Just $ Left "US" -- Illinois, US
isUSCA "618" = Just $ Left "US" -- Illinois, US
isUSCA "630" = Just $ Left "US" -- Illinois, US
isUSCA "708" = Just $ Left "US" -- Illinois, US
isUSCA "730" = Just $ Left "US" -- Illinois, US
isUSCA "773" = Just $ Left "US" -- Illinois, US
isUSCA "779" = Just $ Left "US" -- Illinois, US
isUSCA "815" = Just $ Left "US" -- Illinois, US
isUSCA "847" = Just $ Left "US" -- Illinois, US
isUSCA "872" = Just $ Left "US" -- Illinois, US
isUSCA "219" = Just $ Left "US" -- Indiana, US
isUSCA "260" = Just $ Left "US" -- Indiana, US
isUSCA "317" = Just $ Left "US" -- Indiana, US
isUSCA "463" = Just $ Left "US" -- Indiana, US
isUSCA "574" = Just $ Left "US" -- Indiana, US
isUSCA "765" = Just $ Left "US" -- Indiana, US
isUSCA "812" = Just $ Left "US" -- Indiana, US
isUSCA "930" = Just $ Left "US" -- Indiana, US
isUSCA "319" = Just $ Left "US" -- Iowa, US
isUSCA "515" = Just $ Left "US" -- Iowa, US
isUSCA "563" = Just $ Left "US" -- Iowa, US
isUSCA "641" = Just $ Left "US" -- Iowa, US
isUSCA "712" = Just $ Left "US" -- Iowa, US
isUSCA "316" = Just $ Left "US" -- Kansas, US
isUSCA "620" = Just $ Left "US" -- Kansas, US
isUSCA "785" = Just $ Left "US" -- Kansas, US
isUSCA "913" = Just $ Left "US" -- Kansas, US
isUSCA "270" = Just $ Left "US" -- Kentucky, US
isUSCA "364" = Just $ Left "US" -- Kentucky, US
isUSCA "502" = Just $ Left "US" -- Kentucky, US
isUSCA "606" = Just $ Left "US" -- Kentucky, US
isUSCA "859" = Just $ Left "US" -- Kentucky, US
isUSCA "225" = Just $ Left "US" -- Lousiana, US
isUSCA "318" = Just $ Left "US" -- Lousiana, US
isUSCA "337" = Just $ Left "US" -- Lousiana, US
isUSCA "504" = Just $ Left "US" -- Lousiana, US
isUSCA "985" = Just $ Left "US" -- Lousiana, US
isUSCA "207" = Just $ Left "US" -- Maine, US
isUSCA "227" = Just $ Left "US" -- Maryland, US
isUSCA "240" = Just $ Left "US" -- Maryland, US
isUSCA "301" = Just $ Left "US" -- Maryland, US
isUSCA "410" = Just $ Left "US" -- Maryland, US
isUSCA "443" = Just $ Left "US" -- Maryland, US
isUSCA "667" = Just $ Left "US" -- Maryland, US
isUSCA "339" = Just $ Left "US" -- Massachusetts, US
isUSCA "351" = Just $ Left "US" -- Massachusetts, US
isUSCA "413" = Just $ Left "US" -- Massachusetts, US
isUSCA "508" = Just $ Left "US" -- Massachusetts, US
isUSCA "617" = Just $ Left "US" -- Massachusetts, US
isUSCA "774" = Just $ Left "US" -- Massachusetts, US
isUSCA "781" = Just $ Left "US" -- Massachusetts, US
isUSCA "857" = Just $ Left "US" -- Massachusetts, US
isUSCA "978" = Just $ Left "US" -- Massachusetts, US
isUSCA "231" = Just $ Left "US" -- Michigan, US
isUSCA "248" = Just $ Left "US" -- Michigan, US
isUSCA "269" = Just $ Left "US" -- Michigan, US
isUSCA "313" = Just $ Left "US" -- Michigan, US
isUSCA "517" = Just $ Left "US" -- Michigan, US
isUSCA "586" = Just $ Left "US" -- Michigan, US
isUSCA "616" = Just $ Left "US" -- Michigan, US
isUSCA "734" = Just $ Left "US" -- Michigan, US
isUSCA "810" = Just $ Left "US" -- Michigan, US
isUSCA "906" = Just $ Left "US" -- Michigan, US
isUSCA "947" = Just $ Left "US" -- Michigan, US
isUSCA "989" = Just $ Left "US" -- Michigan, US
isUSCA "218" = Just $ Left "US" -- Minnesota, US
isUSCA "320" = Just $ Left "US" -- Minnesota, US
isUSCA "507" = Just $ Left "US" -- Minnesota, US
isUSCA "612" = Just $ Left "US" -- Minnesota, US
isUSCA "651" = Just $ Left "US" -- Minnesota, US
isUSCA "763" = Just $ Left "US" -- Minnesota, US
isUSCA "952" = Just $ Left "US" -- Minnesota, US
isUSCA "228" = Just $ Left "US" -- Mississippi, US
isUSCA "601" = Just $ Left "US" -- Mississippi, US
isUSCA "662" = Just $ Left "US" -- Mississippi, US
isUSCA "769" = Just $ Left "US" -- Mississippi, US
isUSCA "314" = Just $ Left "US" -- Missouri, US
isUSCA "417" = Just $ Left "US" -- Missouri, US
isUSCA "573" = Just $ Left "US" -- Missouri, US
isUSCA "636" = Just $ Left "US" -- Missouri, US
isUSCA "660" = Just $ Left "US" -- Missouri, US
isUSCA "816" = Just $ Left "US" -- Missouri, US
isUSCA "406" = Just $ Left "US" -- Montana, US
isUSCA "308" = Just $ Left "US" -- Nebraska, US
isUSCA "402" = Just $ Left "US" -- Nebraska, US
isUSCA "531" = Just $ Left "US" -- Nebraska, US
isUSCA "702" = Just $ Left "US" -- Nevada, US
isUSCA "725" = Just $ Left "US" -- Nevada, US
isUSCA "775" = Just $ Left "US" -- Nevada, US
isUSCA "603" = Just $ Left "US" -- New Hampshire, US
isUSCA "201" = Just $ Left "US" -- New Jersey, US
isUSCA "551" = Just $ Left "US" -- New Jersey, US
isUSCA "609" = Just $ Left "US" -- New Jersey, US
isUSCA "640" = Just $ Left "US" -- New Jersey, US
isUSCA "732" = Just $ Left "US" -- New Jersey, US
isUSCA "848" = Just $ Left "US" -- New Jersey, US
isUSCA "856" = Just $ Left "US" -- New Jersey, US
isUSCA "862" = Just $ Left "US" -- New Jersey, US
isUSCA "908" = Just $ Left "US" -- New Jersey, US
isUSCA "973" = Just $ Left "US" -- New Jersey, US
isUSCA "505" = Just $ Left "US" -- New Mexico, US
isUSCA "575" = Just $ Left "US" -- New Mexico, US
isUSCA "212" = Just $ Left "US" -- New York, US
isUSCA "315" = Just $ Left "US" -- New York, US
isUSCA "332" = Just $ Left "US" -- New York, US
isUSCA "347" = Just $ Left "US" -- New York, US
isUSCA "516" = Just $ Left "US" -- New York, US
isUSCA "518" = Just $ Left "US" -- New York, US
isUSCA "585" = Just $ Left "US" -- New York, US
isUSCA "607" = Just $ Left "US" -- New York, US
isUSCA "631" = Just $ Left "US" -- New York, US
isUSCA "646" = Just $ Left "US" -- New York, US
isUSCA "680" = Just $ Left "US" -- New York, US
isUSCA "716" = Just $ Left "US" -- New York, US
isUSCA "718" = Just $ Left "US" -- New York, US
isUSCA "838" = Just $ Left "US" -- New York, US
isUSCA "845" = Just $ Left "US" -- New York, US
isUSCA "914" = Just $ Left "US" -- New York, US
isUSCA "917" = Just $ Left "US" -- New York, US
isUSCA "929" = Just $ Left "US" -- New York, US
isUSCA "934" = Just $ Left "US" -- New York, US
isUSCA "252" = Just $ Left "US" -- North Carolina, US
isUSCA "336" = Just $ Left "US" -- North Carolina, US
isUSCA "704" = Just $ Left "US" -- North Carolina, US
isUSCA "743" = Just $ Left "US" -- North Carolina, US
isUSCA "828" = Just $ Left "US" -- North Carolina, US
isUSCA "910" = Just $ Left "US" -- North Carolina, US
isUSCA "919" = Just $ Left "US" -- North Carolina, US
isUSCA "980" = Just $ Left "US" -- North Carolina, US
isUSCA "984" = Just $ Left "US" -- North Carolina, US
isUSCA "701" = Just $ Left "US" -- North Dakota, US
isUSCA "216" = Just $ Left "US" -- Ohio, US
isUSCA "220" = Just $ Left "US" -- Ohio, US
isUSCA "234" = Just $ Left "US" -- Ohio, US
isUSCA "326" = Just $ Left "US" -- Ohio, US
isUSCA "330" = Just $ Left "US" -- Ohio, US
isUSCA "380" = Just $ Left "US" -- Ohio, US
isUSCA "419" = Just $ Left "US" -- Ohio, US
isUSCA "440" = Just $ Left "US" -- Ohio, US
isUSCA "513" = Just $ Left "US" -- Ohio, US
isUSCA "567" = Just $ Left "US" -- Ohio, US
isUSCA "614" = Just $ Left "US" -- Ohio, US
isUSCA "740" = Just $ Left "US" -- Ohio, US
isUSCA "937" = Just $ Left "US" -- Ohio, US
isUSCA "405" = Just $ Left "US" -- Oklahoma, US
isUSCA "539" = Just $ Left "US" -- Oklahoma, US
isUSCA "580" = Just $ Left "US" -- Oklahoma, US
isUSCA "918" = Just $ Left "US" -- Oklahoma, US
isUSCA "458" = Just $ Left "US" -- Oregon, US
isUSCA "503" = Just $ Left "US" -- Oregon, US
isUSCA "541" = Just $ Left "US" -- Oregon, US
isUSCA "971" = Just $ Left "US" -- Oregon, US
isUSCA "215" = Just $ Left "US" -- Pennsylvania, US
isUSCA "223" = Just $ Left "US" -- Pennsylvania, US
isUSCA "267" = Just $ Left "US" -- Pennsylvania, US
isUSCA "272" = Just $ Left "US" -- Pennsylvania, US
isUSCA "412" = Just $ Left "US" -- Pennsylvania, US
isUSCA "445" = Just $ Left "US" -- Pennsylvania, US
isUSCA "484" = Just $ Left "US" -- Pennsylvania, US
isUSCA "570" = Just $ Left "US" -- Pennsylvania, US
isUSCA "610" = Just $ Left "US" -- Pennsylvania, US
isUSCA "717" = Just $ Left "US" -- Pennsylvania, US
isUSCA "724" = Just $ Left "US" -- Pennsylvania, US
isUSCA "814" = Just $ Left "US" -- Pennsylvania, US
isUSCA "878" = Just $ Left "US" -- Pennsylvania, US
isUSCA "401" = Just $ Left "US" -- Rhode Island
isUSCA "803" = Just $ Left "US" -- South Carolina, US
isUSCA "843" = Just $ Left "US" -- South Carolina, US
isUSCA "854" = Just $ Left "US" -- South Carolina, US
isUSCA "864" = Just $ Left "US" -- South Carolina, US
isUSCA "605" = Just $ Left "US" -- South Dakota, US
isUSCA "423" = Just $ Left "US" -- Tennessee, US
isUSCA "615" = Just $ Left "US" -- Tennessee, US
isUSCA "629" = Just $ Left "US" -- Tennessee, US
isUSCA "731" = Just $ Left "US" -- Tennessee, US
isUSCA "865" = Just $ Left "US" -- Tennessee, US
isUSCA "901" = Just $ Left "US" -- Tennessee, US
isUSCA "931" = Just $ Left "US" -- Tennessee, US
isUSCA "210" = Just $ Left "US" -- Texas, US
isUSCA "214" = Just $ Left "US" -- Texas, US
isUSCA "254" = Just $ Left "US" -- Texas, US
isUSCA "281" = Just $ Left "US" -- Texas, US
isUSCA "325" = Just $ Left "US" -- Texas, US
isUSCA "346" = Just $ Left "US" -- Texas, US
isUSCA "361" = Just $ Left "US" -- Texas, US
isUSCA "409" = Just $ Left "US" -- Texas, US
isUSCA "430" = Just $ Left "US" -- Texas, US
isUSCA "432" = Just $ Left "US" -- Texas, US
isUSCA "469" = Just $ Left "US" -- Texas, US
isUSCA "512" = Just $ Left "US" -- Texas, US
isUSCA "682" = Just $ Left "US" -- Texas, US
isUSCA "713" = Just $ Left "US" -- Texas, US
isUSCA "726" = Just $ Left "US" -- Texas, US
isUSCA "737" = Just $ Left "US" -- Texas, US
isUSCA "806" = Just $ Left "US" -- Texas, US
isUSCA "817" = Just $ Left "US" -- Texas, US
isUSCA "830" = Just $ Left "US" -- Texas, US
isUSCA "832" = Just $ Left "US" -- Texas, US
isUSCA "903" = Just $ Left "US" -- Texas, US
isUSCA "915" = Just $ Left "US" -- Texas, US
isUSCA "936" = Just $ Left "US" -- Texas, US
isUSCA "940" = Just $ Left "US" -- Texas, US
isUSCA "956" = Just $ Left "US" -- Texas, US
isUSCA "972" = Just $ Left "US" -- Texas, US
isUSCA "979" = Just $ Left "US" -- Texas, US
isUSCA "385" = Just $ Left "US" -- Utah, US
isUSCA "435" = Just $ Left "US" -- Utah, US
isUSCA "801" = Just $ Left "US" -- Utah, US
isUSCA "802" = Just $ Left "US" -- Vermont, US
isUSCA "276" = Just $ Left "US" -- Virginia, US
isUSCA "434" = Just $ Left "US" -- Virginia, US
isUSCA "540" = Just $ Left "US" -- Virginia, US
isUSCA "571" = Just $ Left "US" -- Virginia, US
isUSCA "703" = Just $ Left "US" -- Virginia, US
isUSCA "757" = Just $ Left "US" -- Virginia, US
isUSCA "804" = Just $ Left "US" -- Virginia, US
isUSCA "206" = Just $ Left "US" -- Washington, US
isUSCA "253" = Just $ Left "US" -- Washington, US
isUSCA "360" = Just $ Left "US" -- Washington, US
isUSCA "425" = Just $ Left "US" -- Washington, US
isUSCA "509" = Just $ Left "US" -- Washington, US
isUSCA "564" = Just $ Left "US" -- Washington, US
isUSCA "304" = Just $ Left "US" -- West Virginia, US
isUSCA "681" = Just $ Left "US" -- West Virginia, US
isUSCA "262" = Just $ Left "US" -- Wisconsin, US
isUSCA "274" = Just $ Left "US" -- Wisconsin, US
isUSCA "414" = Just $ Left "US" -- Wisconsin, US
isUSCA "534" = Just $ Left "US" -- Wisconsin, US
isUSCA "608" = Just $ Left "US" -- Wisconsin, US
isUSCA "715" = Just $ Left "US" -- Wisconsin, US
isUSCA "920" = Just $ Left "US" -- Wisconsin, US
isUSCA "307" = Just $ Left "US" -- Wyoming, US
isUSCA "368" = Just $ Left "CA" -- Alberta, CA
isUSCA "403" = Just $ Left "CA" -- Alberta, CA
isUSCA "587" = Just $ Left "CA" -- Alberta, CA
isUSCA "780" = Just $ Left "CA" -- Alberta, CA
isUSCA "825" = Just $ Left "CA" -- Alberta, CA
isUSCA "236" = Just $ Left "CA" -- British Columbia, CA
-- +1 250 is used in Hyder, Alaska, US, but by more people in British Columbia
-- so, 250 is treated as Canada. https://en.wikipedia.org/wiki/Area_code_250
isUSCA "250" = Just $ Left "CA" -- British Columbia, CA (also used in Hyder, Alaska)
isUSCA "604" = Just $ Left "CA" -- British Columbia, CA
isUSCA "672" = Just $ Left "CA" -- British Columbia, CA
isUSCA "778" = Just $ Left "CA" -- British Columbia, CA
isUSCA "204" = Just $ Left "CA" -- Manitoba, CA
isUSCA "431" = Just $ Left "CA" -- Manitoba, CA
isUSCA "584" = Just $ Left "CA" -- Manitoba, CA
isUSCA "428" = Just $ Left "CA" -- New Brunswick, CA
isUSCA "506" = Just $ Left "CA" -- New Brunswick, CA
isUSCA "709" = Just $ Left "CA" -- Newfoundland and Labrador, CA
isUSCA "879" = Just $ Left "CA" -- Newfoundland and Labrador, CA
isUSCA "226" = Just $ Left "CA" -- Ontario, CA
isUSCA "249" = Just $ Left "CA" -- Ontario, CA
isUSCA "289" = Just $ Left "CA" -- Ontario, CA
isUSCA "343" = Just $ Left "CA" -- Ontario, CA
isUSCA "365" = Just $ Left "CA" -- Ontario, CA
isUSCA "382" = Just $ Left "CA" -- Ontario, CA
isUSCA "387" = Just $ Left "CA" -- Ontario, CA
isUSCA "416" = Just $ Left "CA" -- Ontario, CA
isUSCA "437" = Just $ Left "CA" -- Ontario, CA
isUSCA "519" = Just $ Left "CA" -- Ontario, CA
isUSCA "548" = Just $ Left "CA" -- Ontario, CA
isUSCA "613" = Just $ Left "CA" -- Ontario, CA
isUSCA "647" = Just $ Left "CA" -- Ontario, CA
isUSCA "683" = Just $ Left "CA" -- Ontario, CA
isUSCA "705" = Just $ Left "CA" -- Ontario, CA
isUSCA "742" = Just $ Left "CA" -- Ontario, CA
isUSCA "753" = Just $ Left "CA" -- Ontario, CA
isUSCA "807" = Just $ Left "CA" -- Ontario, CA
isUSCA "905" = Just $ Left "CA" -- Ontario, CA
isUSCA "942" = Just $ Left "CA" -- Ontario, CA
isUSCA "263" = Just $ Left "CA" -- Quebec, CA
isUSCA "354" = Just $ Left "CA" -- Quebec, CA
isUSCA "367" = Just $ Left "CA" -- Quebec, CA
isUSCA "418" = Just $ Left "CA" -- Quebec, CA
isUSCA "438" = Just $ Left "CA" -- Quebec, CA
isUSCA "450" = Just $ Left "CA" -- Quebec, CA
isUSCA "468" = Just $ Left "CA" -- Quebec, CA
isUSCA "514" = Just $ Left "CA" -- Quebec, CA
isUSCA "579" = Just $ Left "CA" -- Quebec, CA
isUSCA "581" = Just $ Left "CA" -- Quebec, CA
isUSCA "819" = Just $ Left "CA" -- Quebec, CA
isUSCA "873" = Just $ Left "CA" -- Quebec, CA
isUSCA "306" = Just $ Left "CA" -- Saskatchewan, CA
isUSCA "474" = Just $ Left "CA" -- Saskatchewan, CA
isUSCA "639" = Just $ Left "CA" -- Saskatchewan, CA
isUSCA "782" = Just $ Left "CA" -- Nova Scotia, CA; Prince Edward Island, CA
isUSCA "867" = Just $ Left "CA" -- Alberta, CA; British Columbia, CA; Northwest Territories, CA; Nunavut, CA; Yukon, CA
isUSCA "902" = Just $ Left "CA" -- Nova Scotia, CA; Prince Edward Island, CA
isUSCA "800" = Just $ Right TollFree
isUSCA "900" = Just $ Right PremiumRate
isUSCA _     = Nothing -- Alright, we're done -- time to give up.

isUSCA_ :: String -> Maybe (Prefix, Either Country Purpose)
isUSCA_ s = do
  let x = isUSCA $ take 3 $ drop 2 s
  if isNothing x then Nothing else Just (1, fromJust x)

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

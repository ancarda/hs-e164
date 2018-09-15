-- |
-- Copyright   : (c) Mark Dain 2018
-- License     : MIT
-- Maintainer  : mark@markdain.net
--
-- Basic types.
module Telephony.PSTN.E164.Types(
  Country,
  E164Number (E164Number),
  Number,
  Prefix,
  Purpose (TollFree, PremiumRate)
) where

import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (==))
import Data.Int (Int)
import Data.List (drop, take, (++))
import Data.String (String)
import Text.Show (Show, show)

-- | Country a phone number is associated with. This is a 2 character
-- [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes) code
-- such as @US@ for the United States of America.
type Country = String

-- | The right hand side of a phone number, e.g. without the 'Prefix'. For
-- example, if @+1@ is the prefix, Number will be @555-555-5555@.
--
-- This is stored without any spaces.
type Number = String

-- | E164 prefix, such as @+1@. This usually determines the numbering plan,
-- country, and purpose, but __cannot reliably be used by itself__; @+1@ is
-- used by both the USA and Canada. Use 'country' and 'purpose' for analysis.
--
-- This is stored without the leading "+" prefix.
type Prefix = Int

-- | Purpose is used instead of 'Country' for phone numbers associated with a
-- particular service or use rather than general purpose use for a 'Country'.
-- For example, @+1-800-XXX-XXXX@ is 'TollFree'.
data Purpose = TollFree | PremiumRate
  deriving (Eq, Show)

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



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

import Data.Either (Either)
import Data.Eq     (Eq)
import Data.Int    (Int)
import Data.List   ((++))
import Data.String (String)
import Text.Show   (Show, show)

-- | Country a phone number is associated with. This is a 2 character
-- [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes) code
-- such as @US@ for the United States of America.
type Country = String

-- | The right-hand side of a phone number, e.g. without the 'Prefix'. For
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

-- | Returns a generic string representation of this number that can always
-- return this instance back when fed into 'parseE164'.
--
-- This is in the format @+XXX.XXXXXXXXXXXX@.
--
-- You may want to use the function 'human' to display this number in a more
-- conventional way.
instance Show E164Number where
  show :: E164Number -> String
  show (E164Number p _ n) = "+" ++ show p ++ "." ++ n

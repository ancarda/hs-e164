# Telephony.PSTN.E164

_An E.164 analyzer and validator written in Haskell_

[![Written in Haskell](https://img.shields.io/badge/language-Haskell-blue.svg)](https://haskell.org/)
[![MIT Licensed](https://img.shields.io/badge/license-MIT-teal.svg)](https://choosealicense.com/licenses/mit/)
[![Version 0.1.0](https://img.shields.io/badge/version-0.1.0-orange.svg)]()

An [E.164](https://en.wikipedia.org/wiki/E.164) analyzer and validator written
in Haskell. This package allows you to parse a phone number string and get back
an `E164Number` which can be used with functions such as `country`, `prefix,`
and `number.

If the parser returns `Nothing`, the given string isn't a valid E.164 number.

**Note:** This package is currently version 0.x. Until then, no stability
guarantees are made, and the quality of the parser may be very low. Before 1.0,
you should expect countries to be missing, documentation to be lacking, and
bugs to be present.

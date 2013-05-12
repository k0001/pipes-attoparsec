# pipes-attoparsec

Utilities to convert an Attoparsec Parser into a Pipe.

Version 0.1.\* is not backwards compatible with previous versions.

# BIG WARNING!

`pipes-attoparsec` is currently being modified to support interleaved
and delimited parsing, relying on the upcoming `pipes-parse` library.
The current API will be revamped in a future version, so don't rely on
it. Most efforts are currently being spent in getting
[pipes-parse](https://github.com/Gabriel439/Haskell-Pipes-Parse-Library)
ready. The `interleave3` branch in this repository tries to follow the
experimental ideas carried on in `pipes-parse`.

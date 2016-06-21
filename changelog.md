# Version 0.5.1.4

* Bump upper bound dependency on `pipes`.


# Version 0.5.1.3

* Bump upper bound dependency on `transformers`.


# Version 0.5.1.2

* Bump upper bound dependency on `attoparsec`.


# Version 0.5.1.1

* Bump upper bound dependency on `text`.


# Version 0.5.1

* Bump upper bound dependency on `attoparsec`.


# Version 0.5.0

* Correctly propagate state in `parsedL`.

* `parse` and `parseL` return `Nothing` if used on an exhausted
  `Producer`.

* Performance improvements.


# Version 0.4.0.1

* Relax lower and upper dependencies on `text`.


# Version 0.4.0

* API revamped in order to support pipes-parse-3.0.*.


# Version 0.3.1

* Support attoparsec-0.11.


# Version 0.3.0

* Upgrade to pipes-4.0.0 and pipes-parse-2.0.0, removing proxy
  transformers and changing the API substantially.


# Version 0.2.0.0

* Droped the previous API in favour of a new and incompatible API
  that supports interleaved parsing by relying on pipes-parse.


# Version 0.1.0.1

* First version mentioned in NEWS file.

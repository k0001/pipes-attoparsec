{ mkDerivation, attoparsec, base, bytestring, HUnit, lib, mmorph
, pipes, pipes-parse, tasty, tasty-hunit, text, transformers
}:
mkDerivation {
  pname = "pipes-attoparsec";
  version = "0.6.0";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring pipes pipes-parse text transformers
  ];
  testHaskellDepends = [
    attoparsec base HUnit mmorph pipes pipes-parse tasty tasty-hunit
    text transformers
  ];
  homepage = "https://github.com/k0001/pipes-attoparsec";
  description = "Attoparsec and Pipes integration";
  license = lib.licenses.bsd3;
}

{ mkDerivation, base, bytestring, containers, data-default-class
, deepseq, directory, doctest, exceptions, fetchgit
, generic-deriving, hashable, mtl, process, safe, safe-exceptions
, semigroups, split, stdenv, stm, string-conv, tasty, tasty-hunit
, template-haskell, text, time, transformers, unordered-containers
, vector, vinyl
}:
mkDerivation {
  pname = "spiros";
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/sboosali/spiros";
    sha256 = "1n8f7nqfavd5m6784mf76dfpadp5vlgrrddzckvld5pih31vhx7s";
    rev = "f02cbd83ee630c8ca46509f4d6577df8733658f3";
  };
  libraryHaskellDepends = [
    base bytestring containers data-default-class deepseq directory
    exceptions generic-deriving hashable mtl process safe
    safe-exceptions semigroups split stm string-conv template-haskell
    text time transformers unordered-containers vector vinyl
  ];
  testHaskellDepends = [ base doctest tasty tasty-hunit ];
  homepage = "http://github.com/sboosali/spiros#readme";
  description = "my custom prelude";
  license = stdenv.lib.licenses.bsd3;
}

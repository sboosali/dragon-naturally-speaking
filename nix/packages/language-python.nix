{ mkDerivation, alex, array, base, containers, fetchgit, happy
, monads-tf, pretty, stdenv, transformers, utf8-string
}:
mkDerivation {
  pname = "language-python";
  version = "0.5.5";
  src = fetchgit {
    url = "https://github.com/bjpop/language-python";
    sha256 = "1m1rkjcjh4bdf9g8iaz9cgkxi0h13mxjvi4axzkl047q01dqw0jq";
    rev = "98958701b6825e90bb4dbea9372dcee1eab92f2a";
  };
  libraryHaskellDepends = [
    array base containers monads-tf pretty transformers utf8-string
  ];
  libraryToolDepends = [ alex happy ];
  homepage = "http://github.com/bjpop/language-python";
  description = "Parsing and pretty printing of Python code";
  license = stdenv.lib.licenses.bsd3;
}

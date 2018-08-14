{ mkDerivation, aeson, ansi-wl-pprint, base, bifunctors, bytestring
, containers, deepseq, directory, enumerate, exceptions
, generic-lens, hashable, hedgehog, hspec, lens, mtl, parsers
, profunctors, spiros, stdenv, text, transformers
, unordered-containers, vinyl
}:
mkDerivation {
  pname = "natlink";
  version = "0.0.0";
  src = ./natlink;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base bifunctors bytestring containers deepseq
    directory enumerate exceptions generic-lens hashable lens mtl
    parsers profunctors spiros text transformers unordered-containers
    vinyl
  ];
  testHaskellDepends = [ base hedgehog hspec ];
  homepage = "http://github.com/sboosali/dictation#readme";
  description = "Dragon NaturallySpeaking on Linux";
  license = stdenv.lib.licenses.gpl3;
}

{ mkDerivation, aeson, ansi-wl-pprint, async, base, bifunctors
, bytestring, containers, deepseq, directory, enumerate, exceptions
, generic-lens, hashable, http-client, lens, mtl, parallel
, profunctors, reducers, servant, servant-foreign, servant-server
, spiros, sqlite-simple, stdenv, text, time, transformers
, unordered-containers, vinyl, warp
}:
mkDerivation {
  pname = "natlink-http";
  version = "0.0.0";
  src = ../../natlink-http;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint async base bifunctors bytestring containers
    deepseq directory enumerate exceptions generic-lens hashable
    http-client lens mtl parallel profunctors reducers servant
    servant-foreign servant-server spiros sqlite-simple text time
    transformers unordered-containers vinyl warp
  ];
  executableHaskellDepends = [ base ];
  homepage = "http://github.com/sboosali/dictation#readme";
  description = "Dragon NaturallySpeaking on Linux";
  license = stdenv.lib.licenses.gpl3;
}

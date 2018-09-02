{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  # fullDerivation =
  #     { mkDerivation, ansi-wl-pprint, async, base, bifunctors
  #     , bytestring, containers, deepseq, directory, enumerate, exceptions
  #     , generic-lens, hashable, haxr, lens, mtl, parallel, profunctors
  #     , reducers, spiros, stdenv, text, time, transformers
  #     , unordered-containers, vinyl, warp
  #     }:
  #     mkDerivation {
  #       pname = "natlink-xmlrpc";
  #       version = "0.0.0";
  #       src = ./.;
  #       isLibrary = true;
  #       isExecutable = true;
  #       libraryHaskellDepends = [
  #         ansi-wl-pprint async base bifunctors bytestring containers deepseq
  #         directory enumerate exceptions generic-lens hashable haxr lens mtl
  #         parallel profunctors reducers spiros text time transformers
  #         unordered-containers vinyl warp
  #       ];
  #       executableHaskellDepends = [ base ];
  #       homepage = "http://github.com/sboosali/dictation#readme";
  #       description = "Dragon NaturallySpeaking on Linux";
  #       license = stdenv.lib.licenses.gpl3;
  #     };

  minimalDerivation =
    { mkDerivation, stdenv
    , haxr, HaXml
    , warp, HsOpenSSL
    }:
    mkDerivation {
      src = ./.;
    
      pname   = "natlink-xmlrpc";
      version = "0.0.0";
      license = stdenv.lib.licenses.gpl3;
    
      isLibrary    = true;
      isExecutable = true;
      
      libraryHaskellDepends = [
        haxr HaXml
        warp HsOpenSSL
      ];
    };


  haskellOverlay = (self: super:
    {
      HaXml = pkgs.haskell.lib.doJailbreak (super.HaXml);
    });

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.extend haskellOverlay;

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (modifiedHaskellPackages.callPackage minimalDerivation {});

in

  if pkgs.lib.inNixShell then drv.env else drv

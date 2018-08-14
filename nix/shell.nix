{ nixpkgs     ? (import ./nixpkgs)
, compiler    ? "default"
, doBenchmark ? false
, project     ? ./project
}:

let

  inherit (nixpkgs) pkgs;

  f = (import project);
  
  libs = (import ./system/libraries.nix { inherit pkgs; });

  exes = (import ./system/programs.nix  { inherit pkgs; });

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  g = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  x = g (haskellPackages.callPackage f {});

  y = pkgs.haskell.lib.addBuildTools x exes;

  z = if pkgs.lib.inNixShell then y.env else y;

in

  z

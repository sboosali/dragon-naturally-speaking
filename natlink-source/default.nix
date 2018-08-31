{ nixpkgs ? import <nixpkgs> {}
}:
##################################################
let

pkgs = nixpkgs.pkgs;

mkDerivation = pkgs.stdenv.mkDerivation;

in
##################################################
let

drv = import ./natlink-installer.nix { inherit mkDerivation; };

in
##################################################

pkgs.callPackage drv {}

##################################################
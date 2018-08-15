##################################################
let

nixpkgs =
 import ./nix/nixpkgs;



in
##################################################

with nixpkgs;

haskellPackages.extend (haskell.lib.packageSourceOverrides {
  natlink      = ./natlink;
  natlink-http = ./natlink-http;
})

##################################################
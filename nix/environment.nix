###################################################
# [Usage]
#
#    $ nix-env -i -f ./nix/environment.nix
#
#
###################################################
# [Purpose]
#
# C library:   zlib
# C libraries: crypto, ssl
#
# 
##################################################

{ nixpkgs ? (import ./nixpkgs)
}:

##################################################
let

inherit (nixpkgs) pkgs;

packages = with pkgs; [
 zlib.dev                       # 
 openssl                        # ssl
 ];

in
##################################################
pkgs.buildEnv
  {
      name                  = "dragon-naturally-speaking-environment";

      paths                 = packages;
      pathsToLink           = [ "/" "/lib" "/include" ];

      buildInputs           = packages;
      extraOutputsToInstall = [ "out" "dev" ];

  }
##################################################
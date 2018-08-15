###################################################
# [Usage]
#
# before installation:
#
#    $ find ~/.nix-profile/lib/     | grep 'libz\.so.*'
#    $ find ~/.nix-profile/include/ | grep 'z.*\.h'
#
# install:
#
#    $ nix-env --uninstall zlib
#    # ^ if you've already installed `zlib`.
#
#    $ nix-env -i -f ./nix/sboo/zlib.nix -A sboo.environments.zlib
#
# after installation:
#
#    $ find ~/.nix-profile/lib/     | grep 'libz\.so.*'
#    /home/sboo/.nix-profile/lib/libz.so
#    /home/sboo/.nix-profile/lib/libz.so.1.2.11
#    ...
#    $ find ~/.nix-profile/include/ | grep 'z.*\.h'
#    /home/sboo/.nix-profile/include/zlib.h
#    /home/sboo/.nix-profile/include/zconf.h
#    ...
#
##################################################
{ nixpkgs ? (import ../nixpkgs)
}:
##################################################
let

inherit (nixpkgs) pkgs;

zlib-dev  = pkgs.zlib.dev;

zlib-sboo = pkgs.buildEnv
    {
      name                  = "zlib-sboo";

      paths                 = [ zlib-dev ];
      pathsToLink           = [ "/" "/lib" "/include" ];

      buildInputs           = [ zlib-dev ];
      extraOutputsToInstall = [ "out" "dev" ];

    };

in
##################################################
{
  sboo.environments =
    {
      zlib = zlib-sboo;
    };
}
##################################################
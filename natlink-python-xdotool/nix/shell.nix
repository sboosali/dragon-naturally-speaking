##################################################
{ nixpkgs ? (import ./nixpkgs)
}:

##################################################
let

name = "natlink-xdotool-environment";

packages = with nixpkgs.pkgs; [
  python27
  xdotool
  #wmctrl
];

in
##################################################
nixpkgs.pkgs.buildEnv
  {
      inherit name;

      paths                 = packages;
      pathsToLink           = [ "/" "/bin" ];

      buildInputs           = packages;
      extraOutputsToInstall = [ "out" ];

  }
##################################################
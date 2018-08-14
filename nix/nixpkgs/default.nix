##################################################
let

nixpkgs  = <nixpkgs>;

config   = import ../config;

overlays = import ../overlays;

in
##################################################

import nixpkgs { inherit config overlays; }

##################################################
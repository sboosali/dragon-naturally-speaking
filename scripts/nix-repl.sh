#!/bin/bash
set -e
##################################################
# Usage
# =====

# e.g.
# 
#   $ source ./scripts/nix-repl.sh
#
# NOTE run it with `source`, don't execute it directly,
#      since it calls an interpreter.

# wraps `nix repl`.
#
# shadow `NIX_PATH` (temporarily),
# with our project-specific `nixpkgs`.
# 

##################################################

read -r -d '' NIX_REPL_RC <<EOF

_nixpkgs = import <nixpkgs> {}
nixpkgs  = import ./nix/nixpkgs
b  = builtins
c  = nixpkgs.config
l  = nixpkgs.lib
ps = nixpkgs.pkgs
hs = ps.haskellPackages
hl = nixpkgs.haskell.lib
:a nixpkgs
:a nixpkgs.lib
<nixpkgs>
EOF

##################################################

echo
echo -e "$NIX_REPL_RC"
echo

##################################################

NIX_PATH="nixpkgs=./nix/nixpkgs" nix repl

##################################################
# Notes ##########################################
##################################################
# 
##################################################
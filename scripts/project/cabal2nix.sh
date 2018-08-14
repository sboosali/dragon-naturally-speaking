#!/bin/bash
set -e
##################################################

# e.g.
#
# either:
#
#   $ ./scripts/project/cabal2nix.sh
#
# or any of:
#
#   $ ./scripts/project/cabal2nix.sh natlink
#   $ ./scripts/project/cabal2nix.sh natlink/
#   $ ./scripts/project/cabal2nix.sh ./natlink
#   $ ./scripts/project/cabal2nix.sh ./natlink/
#

##################################################

Location=$@

Package=$(basename ${1?"Usage: $0 ./PACKAGE"})

##################################################

Directory=./nix/project

Filename="${Directory}"/"${Package}".nix

##################################################

mkdir -p "${Directory}"

echo

cabal2nix ${Location} > "${Filename}"

echo

cat "${Filename}"

echo

find "${Directory}" -type f

echo

##################################################
# Notes ##########################################
##################################################

# e.g.
# 
# $ cat usage-message.sh
# > #!/bin/bash
# > 
# > : ${1?"Usage: $0 ARGUMENT"}
# >
# > # ^ Script exits here if command-line parameters are absent,
# > #   with the following error message:
# > #
# > #        usage-message.sh: 1: Usage: usage-message.sh ARGUMENT
# > #

# Parameter Expansions:
#
# - ${varname:=word} which sets the undefined varname instead of returning the word value;
# - ${varname:?message} which either returns varname if it's defined and is not null or prints the message and aborts the script (like the first example);
# - ${varname:+word} which returns word only if varname is defined and is not null; returns null otherwise.
#

# See:
#  - https://stackoverflow.com/questions/6482377/check-existence-of-input-argument-in-a-bash-shell-script
#

##################################################
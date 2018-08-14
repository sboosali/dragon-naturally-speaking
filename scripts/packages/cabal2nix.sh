#!/bin/bash
set -e
shopt -s extglob
##################################################

# e.g.
#
#   $ ./scripts/packages/cabal2nix.sh "https://github.com/bjpop/language-python"
#

##################################################

Location=$@

Package=$(basename "$1" | cut -d. -f1)

##################################################

Directory=./nix/packages

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
# $ shopt -s extglob
# $ url=http://www.foo.bar/file.ext
# $ echo ${url##+(*/)}
#    file.ext

# e.g.
#
# $ shopt -s extglob
#
# $ basename "http://www.foo.bar/file.tar.gz" | cut -d. -f1
#   file
# $ basename "http://www.foo.bar/file.txt" | cut -d. -f1
#   file
# $ basename "http://www.foo.bar/file" | cut -d. -f1
#   file
# 
# ^ A combination of `basename` and `cut` works, even in case of double-endings, like `.tar.gz`.
# 

# See;
#  - https://unix.stackexchange.com/questions/64432/extract-the-base-file-name-from-a-url-using-bash
#  - https://stackoverflow.com/questions/2664740/extract-file-basename-without-path-and-extension-in-bash
# 

##################################################
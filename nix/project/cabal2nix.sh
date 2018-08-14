#!/bin/bash
set -e
shopt -s extglob
##################################################

# 

##################################################

Location=${1?"Usage: $0 URI (e.g. https://github.com/REPOSITORY/PACKAGE)"}

##################################################

Directory=./.

Package=$(basename "${Location}" | cut -d. -f1)

Filename="${Package}".nix

Filepath="${Directory}"/"${Filename}"

##################################################

mkdir -p "${Directory}"

echo

cabal2nix $@ > "${Filename}"

echo

cat "${Filepath}"

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
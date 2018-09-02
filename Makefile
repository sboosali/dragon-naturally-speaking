##################################################
##################################################
default: compile

####################
all: compile docs release tags

####################
.PHONY: default all configure check compile clean tags docs release update

##################################################
##################################################
configure:
	nix-env -i -f ./nix/environment.nix
#	nix-shell --run 'cabal new-configure --project-file ./cabal.project'

####################
check:
	cabal new-build -fno-code -O0 all

####################
repl: repl-natlink

.PHONY: repl

####################
repl-natlink: repl-natlink
	cabal new-repl natlink

.PHONY: repl-natlink

####################

repl-natlink-xmmlrpc:
	cabal new-repl natlink-xmlrpc

.PHONY: repl-natlink-xmmlrpc

####################

repl-natlink-http:
	cabal new-repl natlink-http

.PHONY: repl-natlink-http

# ####################
# install:
# 	cabal --enable-nix new-build all

####################
# execute:
# 	cabal new-run ...

####################
clean:
	rm -rf "dist/" "dist-newstyle/" ".sboo/"
	rm -f *.project.local .ghc.environment.*

##################################################
##################################################
# Project Packages (and/or Cabal Components)

####################
natlink:
	cabal new-build natlink
	cabal new-test  natlink

####################
natlink-http:
	cabal new-build natlink-http
	cabal new-run   natlink-http-start

####################
natlink-xmlrpc:
	cabal new-build natlink-xmlrpc
	cabal new-run   natlink-xmlrpc-start

##################################################
##################################################

####################
compile: cabal-compile

# compile: nix-compile
# compile: cabal-compile
# compile: stack-compile

####################
nix-compile:
	nix-shell --run 'cabal new-build all'

####################
cabal-compile:
	cabal new-build all --allow-newer

####################
stack-compile:
	stack --nix build

##################################################
##################################################
build: check compile

####################
tags: compile
	mkdir -p     ".sboo/tags/natlink"
	fast-tags -o ".sboo/tags/natlink" -R ./natlink
	cat          ".sboo/tags/natlink"

####################
update:
	cabal new-update

##################################################
##################################################

########################
build-docs: compile
	cabal --enable-nix new-haddock all
# 	cp -aRv dist-newstyle/build/*/*/unpacked-containers-0/doc/html/unpacked-containers/* docs
# 	cd docs && git commit -a -m "update haddocks" && git push && cd ..

########################
copy-docs: build-docs
	rm -fr   ".sboo/documentation/"
	mkdir -p ".sboo/documentation/"
	cp -aRv  ./dist-newstyle/build/*-*/ghc-*/natlink-*/noopt/doc/html/natlink ".sboo/documentation/natlink"

########################
open-docs:
	xdg-open ".sboo/documentation/index.html"

########################
docs: build-docs copy-docs open-docs

##################################################
##################################################

####################
sdist:
	(cd ./natlink/ && cabal check && cabal sdist)

####################
release: sdist
	mkdir -p "release/"
	(cd ./natlink/ && cp "dist/natlink-*.tar.gz" "../release/")
        #TODO `VERSION` Makefile variable.
        #TODO `cabal new-sdist`?
        #
        # e.g.
        #    dist/natlink-0.0.tar.gz

##################################################
##################################################
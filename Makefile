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
	nix-shell --run 'cabal new-configure --project-file ./cabal.project'

####################
check:
	cabal --enable-nix new-build -fno-code -O0 all

####################
repl:
	cabal --enable-nix new-repl natlink

# ####################
# install:
# 	cabal --enable-nix new-build all

####################
execute:
	cabal --enable-nix new-run ...

####################
clean:
	rm -rf dist/ dist-newstyle/ .sboo/
	rm -f *.project.local .ghc.environment.*

##################################################
##################################################

####################
compile: nix-compile

####################
nix-compile:
	nix-shell --run 'cabal new-build all'

####################
cabal-compile:
	cabal new-build all

# ####################
# stack-compile:
# 	stack build
#

##################################################
##################################################
build: check compile

####################
tags: compile
	mkdir -p ".sboo/tags/natlink"
	fast-tags -o ".sboo/tags/natlink" -R ./natlink
	cat ".sboo/tags/natlink"

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
	rm -fr ".sboo/documentation/"
	mkdir -p ".sboo/documentation/"
	cp -aRv  ./dist-newstyle/build/*-*/ghc-*/natlink-*/noopt/doc/html/natlink/* ".sboo/documentation/"

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
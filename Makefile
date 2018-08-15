##################################################
##################################################
all: build

####################
.PHONY:	all check configure build clean docs update rebuild

##################################################
##################################################
configure:
	cabal --enable-nix new-configure --project-file ./cabal.project

####################
check:
	cabal --enable-nix new-build -fno-code -O0 all

####################
compile:
	cabal --enable-nix new-build all

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
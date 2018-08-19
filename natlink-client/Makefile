##################################################

default: build

.PHONY: default build test install clean

##################################################

build:
	find sources/ -name '*.py' -exec python -m compileall {} \;

#TODO test:
#TODO 	py.test tests

install:
	nix-env -i -A nixpkgs.python27
	nix-env -i -A nixpkgs.python27.pkgs.requests

#TODO pip install -r requirements.txt

clean:
	find sources/ -name '*.pyc' -exec rm {} \;

##################################################
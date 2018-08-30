##################################################

default: check

.PHONY: default

##################################################

build:
	find sources/ -name '*.py' -exec python -m compileall {} \;
#	find sources/ -name '*.py' -print0 | xargs -n 1 -0 python -m compileall

.PHONY: build

# See
#     https://apple.stackexchange.com/questions/49042/how-do-i-make-find-fail-if-exec-fails
# 

##################################################

run:
	find sources/ -name '*.py' -print0 | xargs -n 1 -0 python

.PHONY: run

##################################################

#TODO test:
#TODO 	py.test tests

#.PHONY: test

##################################################

install:
	nix-env -i -A nixpkgs.python27
	nix-env -i -A nixpkgs.python27.pkgs.requests
	nix-env -i -A nixpkgs.python27.pkgs.msgpack

.PHONY: install

#TODO pip install -r requirements.txt

##################################################

clean:
	find sources/ -name '*.pyc' -exec rm {} \;

.PHONY: clean

##################################################

check: build clean run
	@echo 
	@echo --------------------------------------------------
	@echo '[SUCCESS]'
	@echo 
	find sources/ -type f -name '*.py'
	@echo 
	@echo --------------------------------------------------

.PHONY: check

##################################################
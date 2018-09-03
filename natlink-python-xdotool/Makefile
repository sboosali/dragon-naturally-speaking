##################################################

Directory=./sources

Client=$(Directory)/client.py
Server=$(Directory)/server.py
Parser=$(Directory)/parser.py

Environment=SBOO_NATLINK=1

Nixpkgs=./nix/nixpkgs

Python=python

CheckPython=$(Python) -m compileall
CheckLists=jsonlint
CheckRules=$(Python) $(Parser)

##################################################
#------------------------------------------------#

default: check build

.PHONY: default

#------------------------------------------------#

# test: check
# .PHONY: test

#------------------------------------------------#

check: check-lists check-rules

.PHONY: check

##################################################
#------------------------------------------------#

build:
	export $(Environment)
	find $(Directory) -name '*.py' -print0 | xargs -n 1 -0 $(CheckPython)

.PHONY: build

#------------------------------------------------#

check-rules:
	export $(Environment)
	@echo 
	@echo --------------------------------------------------
	@echo 
	find $(DIRECTORY) -type f -name '*.sapi' -print0 | xargs -n 1 -0 $(CheckRules)
	@echo 
	@echo --------------------------------------------------

.PHONY: check-rules

#------------------------------------------------#

check-lists:
	export $(Environment)
	@echo 
	@echo --------------------------------------------------
	@echo 
	find $(Directory) -type f -name '*.json' -print0 | xargs -n 1 -0 $(CheckLists)
	@echo 
	@echo --------------------------------------------------

.PHONY: check-lists

##################################################
#------------------------------------------------#

execute: execute-server

.PHONY: execute

#------------------------------------------------#

execute-server:
	export $(Environment)
	$(Python) $(Server)

.PHONY: execute-server

#------------------------------------------------#

execute-client:
	export $(Environment)
	$(Python) $(Client)

.PHONY: execute-client

##################################################
#------------------------------------------------#

clean:
	export $(Environment)
	find $(Directory) -name '*.pyc' -exec rm {} \;

.PHONY: clean

#------------------------------------------------#

install:

	nix-env -i -f $(Nixpkgs) -A python27
	nix-env -i -f $(Nixpkgs) -A xdotool

	nix-env -i -f $(Nixpkgs) -A nodePackages.jsonlint
	nix-env -i -f $(Nixpkgs) -A python27Packages.enum34

.PHONY: install

##################################################
##################################################

.EXPORT_ALL_VARIABLES:

##################################################

SBOO_NATLINK=1
# ^ Project-Specific Environment-Variable;
# it means that we're on the (non-Windows) host,
# either running the server or testing the client.

Python=python

#------------------------------------------------#

SourceDirectory=.

NixDirectory=./nix

#------------------------------------------------#

Client=$(SourceDirectory)/client.py
Server=$(SourceDirectory)/server.py
Parser=$(SourceDirectory)/parser.py

CheckPython=$(Python) -m compileall
CheckLists=jsonlint
CheckRules=$(Python) $(Parser)

Environment=SBOO_NATLINK=1

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
	find $(SourceDirectory) -name '*.py' -print0 | xargs -n 1 -0 $(CheckPython)

.PHONY: build

#------------------------------------------------#

check-rules:
	export $(Environment)
	@echo 
	@echo --------------------------------------------------
	@echo 
	find $(SourceDirectory) -type f -name '*.sapi' -print0 | xargs -n 1 -0 $(CheckRules)
	@echo 
	@echo --------------------------------------------------

.PHONY: check-rules

#------------------------------------------------#

check-lists:
	export $(Environment)
	@echo 
	@echo --------------------------------------------------
	@echo 
	find $(SourceDirectory) -type f -name '*.json' -print0 | xargs -n 1 -0 $(CheckLists)
	@echo 
	@echo --------------------------------------------------

.PHONY: check-lists

##################################################
#------------------------------------------------#

execute: execute-client

.PHONY: execute

#------------------------------------------------#

serve:
	export $(Environment)
	$(Python) -m server.main

.PHONY: serve

#------------------------------------------------#

execute-client:
	export $(Environment)
	$(Python) -m server.client

.PHONY: execute-client

#------------------------------------------------#

# execute-server:
# 	export $(Environment)
# 	$(Python) -m server.main
#
# .PHONY: execute-server

##################################################
#------------------------------------------------#

clean:
	export $(Environment)
	find $(SourceDirectory) -name '*.pyc'                          -exec rm {} \;
	find $(SourceDirectory) -name 'error_info_natlink_grammar.txt' -exec rm {} \;

.PHONY: clean

#------------------------------------------------#

install:

	nix-env -i -f $(NixDirectory)/nix -A python27
	nix-env -i -f $(NixDirectory)/nix -A xdotool

	nix-env -i -f $(NixDirectory)/nix -A nodePackages.jsonlint
	nix-env -i -f $(NixDirectory)/nix -A python27Packages.enum34

.PHONY: install

##################################################
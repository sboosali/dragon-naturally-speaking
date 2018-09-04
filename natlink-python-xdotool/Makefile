##################################################

.EXPORT_ALL_VARIABLES:

##################################################

SBOO_NATLINK=1
# ^ Project-Specific Environment-Variable;
# it means that we're on the (non-Windows) host,
# either running the server or testing the client.

Python=python

#------------------------------------------------#

SourceDirectory=./sboo
PluginsDirectory=$(SourceDirectory)/plugins

NixDirectory=./nix

#------------------------------------------------#

Client=sboo.client.client
Server=sboo.server.server

Parser=./parser.py
Plugin=./_dictation.py

CheckPython=$(Python) -m compileall
CheckRules=$(Python) $(Parser)
CheckLists=jsonlint

Environment=SBOO_NATLINK=1

##################################################
#------------------------------------------------#

default: check execute-plugin

.PHONY: default

#------------------------------------------------#

# test: check
# .PHONY: test

#------------------------------------------------#

check: check-python check-lists check-rules

.PHONY: check

##################################################
#------------------------------------------------#

check-python:
	export $(Environment)
	find . -name '*.py' -print0 | xargs -n 1 -0 $(CheckPython)

.PHONY: check-python

#------------------------------------------------#

check-rules:
	export $(Environment)
	@echo 
	@echo --------------------------------------------------
	@echo 
	find $(PluginsDirectory) -type f -name '*.sapi' -print0 | xargs -n 1 -0 $(CheckRules)
	@echo 
	@echo --------------------------------------------------

.PHONY: check-rules

#------------------------------------------------#

check-lists:
	export $(Environment)
	@echo 
	@echo --------------------------------------------------
	@echo 
	find $(PluginsDirectory) -type f -name '*.json' -print0 | xargs -n 1 -0 $(CheckLists)
	@echo 
	@echo --------------------------------------------------

.PHONY: check-lists

##################################################
#------------------------------------------------#

execute: execute-plugin

.PHONY: execute

#------------------------------------------------#

execute-plugin:
	export $(Environment)
	$(Python) $(Plugin)

.PHONY: execute-plugin

#------------------------------------------------#

execute-client:
	export $(Environment)
	$(Python) -m $(Client)

.PHONY: execute-client

#------------------------------------------------#

execute-server:
	export $(Environment)
	$(Python) -m $(Server)

.PHONY: execute-server

##################################################
#------------------------------------------------#

clean:
	export $(Environment)
	find . -name '*.pyc'                          -exec rm {} \;
	find . -name 'error_info_natlink_grammar.txt' -exec rm {} \;

.PHONY: clean

#------------------------------------------------#

install:

	nix-env -i -f $(NixDirectory)/nix -A python27
	nix-env -i -f $(NixDirectory)/nix -A xdotool

	nix-env -i -f $(NixDirectory)/nix -A nodePackages.jsonlint
	nix-env -i -f $(NixDirectory)/nix -A python27Packages.enum34

.PHONY: install

##################################################
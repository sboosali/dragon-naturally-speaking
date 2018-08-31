##################################################

SOURCE_DIRECTORY=./sources

COMMANDS_FILE="$(SOURCE_DIRECTORY)/_commands.py"
SELECTION_FILE="$(SOURCE_DIRECTORY)/_selection.py"
DICTATION_FILE="$(SOURCE_DIRECTORY)/_dictation.py"

##################################################

default: execute clean

.PHONY: default

##################################################

build:
	find "$(SOURCE_DIRECTORY)/" -name '*.py' -exec python -m compileall {} \;
#	find "$(SOURCE_DIRECTORY)/" -name '*.py' -print0 | xargs -n 1 -0 python -m compileall

.PHONY: build

# See
#     https://apple.stackexchange.com/questions/49042/how-do-i-make-find-fail-if-exec-fails
# 

##################################################

execute:
	SBOO_NATLINK=1 python $(COMMANDS_FILE)
#	SBOO_NATLINK=1 python $(SELECTION_FILE)
#	SBOO_NATLINK=1 python $(DICTATION_FILE)

.PHONY: execute

##################################################

# run:
# 	python 

# .PHONY: run

##################################################

# test:

# .PHONY: test

##################################################

install:
	nix-env -i -A nodePackages.jsonlint
#	nix-env -i -A nixpkgs.python27
#	nix-env -i -A nixpkgs.python27.pkgs.requests
#	nix-env -i -A nixpkgs.python27.pkgs.msgpack

.PHONY: install

# #TODO pip install -r requirements.txt

##################################################

clean:
	find "$(SOURCE_DIRECTORY)/" -name '*.pyc' -exec rm {} \;

.PHONY: clean

##################################################

validate:
	@echo 
	@echo --------------------------------------------------
	@echo '[JSON]'
	@echo 
	find "$(SOURCE_DIRECTORY)/" -type f -name '*.json' -print0 | xargs -n 1 -0 jsonlint
	@echo 
	@echo --------------------------------------------------

.PHONY: validate

##################################################

#check: build clean execute

check: build clean execute
	@echo 
	@echo --------------------------------------------------
	@echo '[SUCCESS]'
	@echo 
	find "$(SOURCE_DIRECTORY)/" -type f -name '*.py'
	@echo 
	@echo --------------------------------------------------

.PHONY: check

##################################################
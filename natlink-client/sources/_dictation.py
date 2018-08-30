# -*- coding: utf-8 -*-
##################################################
# notes
# =====
#
# this is an example (natlink-)plugin
# of a simple "Dictation" grammar.
# 
# the name of this file is prefixed with an underscore;
# this tells Dragon that the grammar is "global"
# (i.e. active independent of the current active window).
#
# 
##################################################
# (standard-library modules)

#import traceback
import os

##################################################
# (project (local) modules)

from   sboo.types   import *
from   sboo.grammar import *

##################################################
# (natlink13 modules)

if os.environ.get('SBOO_NATLINK'):
    # the environment-variable is set.
    from sboo.natlinkmain import (setCheckForGrammarChanges)
    # ^ for testing on the host, import stubbed `natlink` signatures.

else:
    # tyhe environment-variable is unset or set to a falsy value.
    from      natlinkmain import (setCheckForGrammarChanges)
    # ^ for running on the guest.

##################################################
# The User's Grammar

exports = [  ]

rules = '''
<dgndictation> imported;
<dgnletters>   imported;

'''
lists = { }

##################################################

config = GrammarConfig(exports=exports,
                       rules=rules,
                       lists=lists)

props = fastProperties

##################################################
# The Server's Address

##################################################
# The boilerplate that `natlink` requires of a `natlink` plugin.

GRAMMAR = None
# ^ a mutable variable (global by intent).

def load():
    global GRAMMAR

    setCheckForGrammarChanges(1)
    # ^ automatically reload on file change
    # (versus only when the microphone toggles on,
    # the default behavior).
    
    GRAMMAR = NarcissisticGrammar()
    GRAMMAR.initialize(config, properties=defaultProperties)

def unload():
    global GRAMMAR
    
    if GRAMMAR:
        GRAMMAR.unload()

    GRAMMAR = None

load()
##################################################
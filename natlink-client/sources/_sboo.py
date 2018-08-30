# -*- coding: utf-8 -*-
##################################################
# notes
# =====
#
# this is an example (natlink-)plugin, with a simple grammar.
# 
# the name of this file is prefixed with an underscore;
# this tells Dragon that the grammar is "global"
# (i.e. active independent of the current active window).
#
# 
##################################################
# (project (local) modules)

import sboo.types
import sboo.grammar

##################################################
# (natlink13 modules)

from   natlinkmain import (setCheckForGrammarChanges)
import natlink
       # ^ (a DLL)

##################################################
# (standard-library modules)

import traceback

##################################################
# The User's Grammar

# Interpolations from "H"askell:

H_RULES  = {__rules__}
H_LISTS  = {__lists__}
H_EXPORT = {__export__}

H_SERVER_HOST = {__serverHost__}
H_SERVER_PORT = {__serverPort__}

H_PROPERTIES = {__properties__}

# e.g. for debugging
# H_RULES  = '''<test> exported = \{test};'''
# H_LISTS  = \{'test', ['upcase region']}
# H_EXPORT = 'test'
# H_SERVER_HOST = "192.168.56.1"
# H_SERVER_PORT = '8666'
# H_PROPERTIES = \{'status': True , 'exclusivity': 0, 'shouldEavesdrop': 1, 'shouldHypothesize': 1}

##################################################

server_address = "http://%s:%s" % (H_SERVER_HOST, H_SERVER_PORT)
#TODO: HTTP versus HTTPS?

##################################################

microphone_rule = '''<microphone> exported = mike on | mike off | mike dead ;'''
microphone_export = "microphone"

##################################################



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
    GRAMMAR.initialize()

def unload():
    global GRAMMAR
    
    if GRAMMAR:
        GRAMMAR.unload()

    GRAMMAR = None

load()
##################################################
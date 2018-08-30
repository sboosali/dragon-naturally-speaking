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

from   sboo.types   import *
from   sboo.grammar import *

##################################################
# (natlink13 modules)

from   natlinkmain import (setCheckForGrammarChanges)

##################################################
# (standard-library modules)

#import traceback

##################################################
# The User's Grammar

exports = [ 'dictating', 'spelling', 'commands' ]

rules = '''
<dgndictation> imported;
<dgnletters>   imported;

<dictating> exported = say <dgndictation> [ stop ];
<spelling>  exported = spell <dgnletters> [ stop ];
<commands>  exported = {commands}+;
'''

lists = { 'commands':
          [ 'cut', 'copy', 'paste', 'undo', 'select all' ] }

##################################################

config = GrammarConfig(exports=exports,
                       rules=rules,
                       lists=lists)

props = fastProperties

#props = voraciousProperties
#props = fastProperties

##################################################
# The Server's Address

host = '192.168.56.1'
port = 3428
# ^ our default port is the "dialpad-encoding" of "D-I-C-T".

url = "http://%s:%s" % (host, port)
# address of the (http) dictation server, expected to be running on the host.

#TODO: HTTP versus HTTPS?

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

# microphone_rule = '''<microphone> exported = mike on | mike off | mike dead ;'''
# microphone_export = "microphone"

# e.g. for debugging
# H_RULES  = '''<test> exported = \{test};'''
# H_LISTS  = \{'test', ['upcase region']}
# H_EXPORT = 'test'
# H_PROPERTIES = \{'status': True , 'exclusivity': 0, 'shouldEavesdrop': 1, 'shouldHypothesize': 1}

# e.g. for debugging
# HOST = "192.168.56.1"
# PORT = '8666'

##################################################
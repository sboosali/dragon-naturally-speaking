# -*- coding: utf-8 -*-
##################################################
# notes
# =====
#
# this is a (natlink-)plugin for a simple "Dictation" grammar.
#
# the grammar has only:
# - `<dgndictation>` (i.e. arbitrary dictation); and
# - `<dgnletters> (i.e. spelling letters and numbers)
#
# the client supports only "forwarding" the recognitions to a server.
# it is simple and stateless; there are no other features,
# like corrections (`ResObj.correction()`),
# or microphone toggling (`setMicState()`).
# 
# the name of this file is prefixed with an underscore;
# this tells Dragon that the grammar is "global"
# (i.e. active independent of the current active window).
#

##################################################
# (standard-library modules)

#import traceback
import os

##################################################
# (project (local) modules)

#from   sboo.natlink.utilities import *
from   sboo.natlink.api       import *
from   sboo.natlink.grammar   import *
from   sboo.natlink.types     import *

##################################################
# (natlink13 modules)

if os.environ.get('SBOO_NATLINK'):
    # the environment-variable is set.
    from sboo.natlink.natlinkmain import (setCheckForGrammarChanges)
    # ^ for testing on the host, import stubbed `natlink` signatures.

else:
    # the environment-variable is unset or set to a falsy value.
    from      natlinkmain import (setCheckForGrammarChanges)
    # ^ for running on the guest.

if os.environ.get('SBOO_NATLINK'):
    import sboo.natlink.gramparser as gramparser

else:
    import                    gramparser

# ^ `gramparser`:
#
#      This module contains the Python code to convert the textual represenation
#   of a command and control grammar in the standard SAPI CFG binary format.
#

##################################################
# The User's Grammar

exports = [  ]

rules = '''
<dgndictation> imported;
<dgnletters>   imported;

<dictating> exported = [ say ] <dgndictation>;
<spelling>  exported = spell   <dgnletters>;

#TODO# <inserting> exported = send [ <insertion_method> ] <dgndictation>;
#TODO# <insertion_method> = text | keys | clip | paste;
'''

lists = { }

##################################################

config = GrammarConfig(exports=exports,
                       rules=rules,
                       lists=lists)

dictationProperties = Properties( activity               = Activity.ACTIVE,
                                 exclusivity            = Exclusivity.INCLUSIVE,
                                 shouldEavesdrop        = True,
                                 shouldHypothesize      = True,
                                 doOnlyGotResultsObject = True)

properties = dictationProperties
#properties = generousProperties

##################################################
# The Server's Address

address = defaultAddress

##################################################
# The boilerplate that `natlink` requires of a `natlink` plugin.

GRAMMAR = None
# ^ a mutable variable (global by intent).

def load():
    global GRAMMAR

    setCheckForGrammarChanges(1)
    # ^ automatically reload on file change
    # (versus only when the microphone toggles back on,
    # which is the default behavior).
    
    GRAMMAR = NarcissisticGrammar()
    GRAMMAR.initialize(config, properties=defaultProperties)

def unload():
    global GRAMMAR
    
    if GRAMMAR:
        GRAMMAR.unload()

    GRAMMAR = None

load()

##################################################


print '--------------------------------------------------'
print '[source grammar]'
print
print rules
print 
print '--------------------------------------------------'
print '[binary grammar]'
print 

linedRules = rules.split('\n')
parser = gramparser.GramParser(linedRules)
parser.doParse()
parser.checkForErrors()

binaryRules = parser.dumpString()

print binaryRules
print 
print '--------------------------------------------------'
print '[grammar EXPORTS]'
print
print exports
print 
print '--------------------------------------------------'
print '[grammar LISTS]'
print
print lists
print 
print '--------------------------------------------------'
print '[grammar PROPERTIES]'
print
print properties
print 
print '--------------------------------------------------'
print '[server ADDRESS]'
print
print address
print 
print '--------------------------------------------------'
print '--------------------------------------------------'

##################################################
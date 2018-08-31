# -*- coding: utf-8 -*-
##################################################
# notes
# =====
#
# this is an example (natlink-)plugin
# of a simple "Command & Control" simple grammar.
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
import os.path

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
    # type environment-variable is unset or set to a falsy value.
    from      natlinkmain import (setCheckForGrammarChanges)
    # ^ for running on the guest.

# # # # # # # # # # # # # # # # # # # # # # # # #

if os.environ.get('SBOO_NATLINK'):
    import sboo.gramparser as gramparser
    
else:
    import                    gramparser

# ^ `gramparser`:
#
#      This module contains the Python code to convert the textual represenation
#   of a command and control grammar in the standard SAPI CFG binary format.
#

##################################################
# Variables

__directory__ = os.path.dirname(__file__)

def commands_data_file(filename):
    return os.path.join(__directory__, 'commands', filename)

##################################################
# The User's Grammar

# # # # # # # # # # # # # # # # # # # # # # # # #

exports_file = commands_data_file('exports.txt')

exports = None
with open(exports_file, 'r') as f:
    exports_string = f.read()
    exports = [line.strip() for line in exports_string.split('\n') if not line.isspace()]

# # # # # # # # # # # # # # # # # # # # # # # # #

rules_file = commands_data_file('rules.sapi')

rules = None
with open(rules_file, 'r') as f:
    rules = f.read()

# # # # # # # # # # # # # # # # # # # # # # # # #

lists_file = commands_data_file('lists.json')

lists = None
try:
    with open(lists_file, 'r') as f:
        lists_string = f.read()
        lists = json.loads(lists_string)

except (IOError, ValueError) as e:
    lists = {}

    print '--------------------------------------------------'
    print '[WARNING]'
    print
    print e
    print
    print '--------------------------------------------------'

    raise e

    # ^ failure is acceptable, whether during file-reading or json-decoding.
    # lists, unlike rules, can be mutated dynamically.
    # so, we're only loading the initial/default values.

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

# # # # # # # # # # # # # # # # # # # # # # # # #

def load():
    global GRAMMAR

    setCheckForGrammarChanges(1)
    # ^ automatically reload on file change
    # (versus only when the microphone toggles on,
    # the default behavior).
    
    GRAMMAR = NarcissisticGrammar()
    GRAMMAR.initialize(config, properties=defaultProperties)

# # # # # # # # # # # # # # # # # # # # # # # # #

def unload():
    global GRAMMAR
    
    if GRAMMAR:
        GRAMMAR.unload()

    GRAMMAR = None

# # # # # # # # # # # # # # # # # # # # # # # # #

load()

##################################################

# if __name__ == "__main__":

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
print '--------------------------------------------------'

# e.g. `GramParser`:
#
# >>> gramSpec = ['<rule> exported = action;']
# >>> parser = GramParser(gramSpec)
# >>> parser.doParse()
# >>> parser.checkForErrors()
# >>> print parser.dumpString()
# knownRules: 
# {'rule': 1}
# knownWords: 
# {'action': 1}
# exportRules: 
# {'rule': 1}
# ruleDefines: 
# {'rule': [('word', 1)]}
#

# e.g.
#
# knownRules:
# {'action': 13,
#  'button': 16,
#  'click': 10,
#  'command': 6,
#  'commands': 5,
#  'dgndictation': 1,
#  'dgnletters': 2,
#  'dictating': 3,
#  'key': 15,
#  'keyboard': 7,
#  'keychord': 14,
#  'keysequence': 8,
#  'modifiers': 11,
#  'mouse': 9,
#  'number': 12,
#  'spelling': 4}
# knownLists:
# {'button': 6,
#  'cardinal': 2,
#  'command': 3,
#  'key': 4,
#  'modifier': 1,
#  'multiplier': 5}
# knownWords:
# {'button-code': 8,
#  'click': 7,
#  'key': 5,
#  'key-code': 6,
#  'press': 4,
#  'say': 1,
#  'spell': 3,
#  'stop': 2}
# exportRules:
# {'commands': 5, 'dictating': 3, 'keyboard': 7, 'mouse': 9, 'spelling': 4}
# importRules:
# {'dgndictation': 1, 'dgnletters': 2}
# ruleDefines:
# {'action': [('list', 3)],
#  'button': [('start', 2),
#             ('list', 6),
#             ('start', 1),
#             ('word', 8),
#             ('rule', 12),
#             ('end', 1),
#             ('end', 2)],
#  'click': [('start', 1),
#            ('start', 4),
#            ('rule', 11),
#            ('end', 4),
#            ('start', 4),
#            ('list', 5),
#            ('end', 4),
#            ('start', 4),
#            ('rule', 16),
#            ('end', 4),
#            ('word', 7),
#            ('end', 1)],
#  'command': [('start', 1),
#              ('start', 4),
#              ('rule', 12),
#              ('end', 4),
#              ('rule', 13),
#              ('end', 1)],
#  'commands': [('start', 1),
#               ('start', 3),
#               ('rule', 6),
#               ('end', 3),
#               ('start', 4),
#               ('word', 2),
#               ('end', 4),
#               ('end', 1)],
#  'dictating': [('start', 1),
#                ('word', 1),
#                ('rule', 1),
#                ('start', 4),
#                ('word', 2),
#                ('end', 4),
#                ('end', 1)],
#  'key': [('start', 2),
#          ('list', 4),
#          ('start', 1),
#          ('word', 6),
#          ('rule', 12),
#          ('end', 1),
#          ('end', 2)],
#  'keyboard': [('start', 1),
#               ('word', 4),
#               ('rule', 8),
#               ('start', 4),
#               ('word', 2),
#               ('end', 4),
#               ('end', 1)],
#  'keychord': [('start', 1),
#               ('start', 4),
#               ('rule', 11),
#               ('end', 4),
#               ('rule', 15),
#               ('start', 4),
#               ('word', 5),
#               ('end', 4),
#               ('end', 1)],
#  'keysequence': [('start', 2),
#                  ('start', 3),
#                  ('rule', 14),
#                  ('end', 3),
#                  ('start', 1),
#                  ('start', 4),
#                  ('rule', 11),
#                  ('end', 4),
#                  ('rule', 2),
#                  ('end', 1),
#                  ('end', 2)],
#  'modifiers': [('start', 3), ('list', 1), ('end', 3)],
#  'mouse': [('start', 1),
#            ('rule', 10),
#            ('start', 4),
#            ('word', 2),
#            ('end', 4),
#            ('end', 1)],
#  'number': [('list', 2)],
#  'spelling': [('start', 1),
#               ('word', 3),
#               ('rule', 2),
#               ('start', 4),
#               ('word', 2),
#               ('end', 4),
#               ('end', 1)]}
#

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
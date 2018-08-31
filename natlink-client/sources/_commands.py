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

exports = [ 'dictating', 'spelling', 'commands' ]

rules = '''
<dgndictation> imported;
<dgnletters>   imported;

<dictating> exported = say   <dgndictation> [ stop ];
<spelling>  exported = spell <dgnletters>   [ stop ];
<commands>  exported =       <command>+     [ stop ];
<keyboard>  exported = press <keysequence>  [ stop ];
<mouse>     exported =       <click>        [ stop ];

<modifiers> = {modifier}*;

<number> = {cardinal};

<command> = [ <number> ] <action>;
<action>  = {command}; 

<keysequence> = <keychord>+
              | <modifiers> <dgnletters>;
<keychord>    = <modifiers> <key> [ key ];
<key>         = {key}
              | key-code <number>;

<click>  = <modifiers> [ {multiplier} ] [ <button> ] click;
<button> = {button}
         | button-code <number>;

'''

lists = {

    'command': [
        'paste', 'undo',
        'cut', 'copy', 
        'select all',
    ],

    'multiplier': [
        'single', 'double', 'triple'
    ],
    
    'button': [
        'left', 'middle', 'right'
    ],
    
    'modifier': [
        'control', 'alt', 'shift', 'win', 'meta',
        'con', 'met'
    ],

    'key': [
        
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',

        'alfa\\A', 'bravo\\B', 'charlie\\C', 'delta\\D', 'echo\\E', 'foxtrot\\F', 'golf\\G', 'hotel\\H', 'india\\I', 'juliett\\J', 'kilo\\K', 'lima\\L', 'mike\\M', 'november\\N', 'oscar\\O', 'papa\\P', 'quebec\\Q', 'romeo\\R', 'sierra\\S', 'tango\\T', 'uniform\\U', 'victor\\V', 'whiskey\\W', 'x\\X-ray', 'yankee\\Y', 'zulu\\Z',

        'escape', 'grave', 'minus', 'equal', 'delete', 'forward-delete', 'left-bracket', 'right-bracket', 'backslash', 'semicolon', 'quote', 'comma', 'period', 'slash'
        # (shifted)


        'tab', 'space', 'return',

        'up', 'down', 'left', 'right'

        'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine',
        # (shifted)


        'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12', 'F13', 'F14', 'F15', 'F16', 'F17', 'F18', 'F19', 'F20',

        # aliases...
        
        'cape', 'eek', 'del', 'fell',
        'lack', 'rack',
        'lace', 'race',
        'lore', 'roar',
        'stroke', 'sem', 'coal', 'ma', 'dot', 
        #TODO sem, ma,
        'ace','ret'
    ],

    'cardinal': [
        
	'zero', 'nought', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine',
        'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen',
        "twenty", "twenty-one", "twenty-two", "twenty-three", "twenty-four", "twenty-five", "twenty-six", "twenty-seven", "twenty-eight", "twenty-nine",
        "thrity", "thrity-one", "thrity-two", "thrity-three", "thrity-four", "thrity-five", "thrity-six", "thrity-seven", "thrity-eight", "thrity-nine",
        "forty", "forty-one", "forty-two", "forty-three", "forty-four", "forty-five", "forty-six", "forty-seven", "forty-eight", "forty-nine",
        "fifty", "fifty-one", "fifty-two", "fifty-three", "fifty-four", "fifty-five", "fifty-six", "fifty-seven", "fifty-eight", "fifty-nine",
        "sixty", "sixty-one", "sixty-two", "sixty-three", "sixty-four", "sixty-five", "sixty-six", "sixty-seven", "sixty-eight", "sixty-nine",
        "seventy", "seventy-one", "seventy-two", "seventy-three", "seventy-four", "seventy-five", "seventy-six", "seventy-seven", "seventy-eight", "seventy-nine",
        "eighty", "eighty-one", "eighty-two", "eighty-three", "eighty-four", "eighty-five", "eighty-six", "eighty-seven", "eighty-eight", "eighty-nine",
        "ninety", "ninety-one", "ninety-two", "ninety-three", "ninety-four", "ninety-five", "ninety-six", "ninety-seven", "ninety-eight", "ninety-nine",
        "hundred"
        
        ],

    'ordinal': [

        "zeroth", "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth",
        "tenth", "eleventh", "twelfth", "thirteenth", "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth",
        "twentieth", "twenty-first", "twenty-second", "twenty-third", "twenty-fourth", "twenty-fifth", "twenty-sixth", "twenty-seventh", "twenty-eighth", "twenty-ninth",
        "thritieth", "thrity-first", "thrity-second", "thrity-third", "thrity-fourth", "thrity-fifth", "thrity-sixth", "thrity-seventh", "thrity-eighth", "thrity-ninth",
        "fortieth", "forty-first", "forty-second", "forty-third", "forty-fourth", "forty-fifth", "forty-sixth", "forty-seventh", "forty-eighth", "forty-ninth",
        "fiftieth", "fifty-first", "fifty-second", "fifty-third", "fifty-fourth", "fifty-fifth", "fifty-sixth", "fifty-seventh", "fifty-eighth", "fifty-ninth",
        "sixtieth", "sixty-first", "sixty-second", "sixty-third", "sixty-fourth", "sixty-fifth", "sixty-sixth", "sixty-seventh", "sixty-eighth", "sixty-ninth",
        "seventieth", "seventy-first", "seventy-second", "seventy-third", "seventy-fourth", "seventy-fifth", "seventy-sixth", "seventy-seventh", "seventy-eighth", "seventy-ninth",
        "eightieth", "eighty-first", "eighty-second", "eighty-third", "eighty-fourth", "eighty-fifth", "eighty-sixth", "eighty-seventh", "eighty-eighth", "eighty-ninth",
        "ninetieth", "ninety-first", "ninety-second", "ninety-third", "ninety-fourth", "ninety-fifth", "ninety-sixth", "ninety-seventh", "ninety-eighth", "ninety-ninth",
        "hundredth"

    ]

  }

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
    GRAMMAR.initialize(config, properties=defaultProperties)

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
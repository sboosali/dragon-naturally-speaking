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

import os
import os.path

import json
from   ConfigParser import ConfigParser

#import traceback

##################################################
# (project (local) modules)

from   sboo.types     import *
from   sboo.grammar   import *
from   sboo.utilities import *

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

def eavesdrop_data_file(filename):
    return os.path.join(__directory__, 'eavesdrop', filename)

##################################################
# The User's Grammar

# # # # # # # # # # # # # # # # # # # # # # # # #

exports_file = eavesdrop_data_file('exports.txt')

exports = None
with open(exports_file, 'r') as f:
    exports_string = f.read()
    exports = to_words(exports_string)

# # # # # # # # # # # # # # # # # # # # # # # # #

rules_file = eavesdrop_data_file('rules.sapi')

rules = None
with open(rules_file, 'r') as f:
    rules = f.read()

# # # # # # # # # # # # # # # # # # # # # # # # #

lists_file = eavesdrop_data_file('lists.json')

lists = None
try:
    with open(lists_file, 'r') as f:
        lists_string = f.read()
        lists = json.loads(lists_string)

except (IOError, ValueError) as e:
    lists = {}

    print_exception_as_warning(e)

    # ^ failure is acceptable, whether during file-reading or json-decoding.
    # lists, unlike rules, can be mutated dynamically.
    # so, we're only loading the initial/default values.

# # # # # # # # # # # # # # # # # # # # # # # # #
# The Server's Address
        
defaultAddress = Address(host = "192.168.56.1",
                         port = 3428)

address_file = eavesdrop_data_file('client.ini')

address = None
try:
    with open(lists_file, 'r') as f:
        address_string = f.read()
        
        properties_ini = ConfigParser()
        properties_ini.read(address_file)
        
        ##properties_ini.options("http")
        host_string = ini_get_text(properties_ini, 'http', 'host', defaultAddress.host)
        port_string = ini_get_text(properties_ini, 'http', 'port', defaultAddress.port)

        host = host_string
        port = int(port_string)

        address = Address(host = host,
                          port = port)

except (IOError, ValueError) as e:
    address = defaultAddress

    print_exception_as_warning(e)

    # ^ failure is acceptable, whether during file-reading or ini-decoding.
    # because we can fallback to the default server address.

url = "http://%s:%s" % (address.host, address.port)
# # properties of the (http) dictation server, expected to be running on the host.
# #TODO: HTTP versus HTTPS?    with open(lists_file, 'r') as f

# # # # # # # # # # # # # # # # # # # # # # # # #
# The Grammar's Initial Properties

properties_file = eavesdrop_data_file('properties.ini')

properties = None
try:
    with open(lists_file, 'r') as f:
        properties_string = f.read()
        
        properties_ini = ConfigParser()
        properties_ini.read(properties_file)

        active     = ini_get_enum(Activity,
                                  properties_ini, 'grammar', 'active',
                                  defaultProperties.activity)
        exclusive  = ini_get_enum(Exclusivity,
                                  properties_ini, 'grammar', 'exclusive',
                                  defaultProperties.exclusivity)
        eavesdrop  = ini_get_bool(properties_ini, 'grammar', 'eavesdrop',
                                  defaultProperties.shouldEavesdrop)
        hypotheses = ini_get_bool(properties_ini, 'grammar', 'hypotheses',
                                  defaultProperties.shouldHypothesize)
        early      = ini_get_bool(properties_ini, 'grammar', 'early',
                                  defaultProperties.doOnlyGotResultsObject)

        properties = Properties(activity               = active,
                                exclusivity            = exclusive,
                                shouldEavesdrop        = eavesdrop,
                                shouldHypothesize      = hypotheses,
                                doOnlyGotResultsObject = early)

except (IOError, ValueError) as e:
    properties = defaultProperties

    print_exception_as_warning(e)

# [Notes] INI Defaulting.
#
# to specify default values for a specific section, winter:
#
# or call read_dict() with a (possibly incomplete) dict, before calling read() on file.
# 
# or (2) .has_option(section, option)
#

# # # # # # # # # # # # # # # # # # # # # # # # #

##################################################

config = GrammarConfig(exports=exports,
                       rules=rules,
                       lists=lists)

##################################################

class EavesdropGrammar(GrammarBase):
    ''' this Grammar "eavesdrop" on others, doing nothing but logging recognitions and hypotheses.
    requires the eavesdropped-upon grammars to be inclusive (i.e. `Exclusivity.INCLUSIVE`).
    '''

    ##############################################
    
    def initialize(self, grammar, properties = defaultProperties):
        # `defaultProperties`, being a `namedtuple`, is immutable;
        # thus, it's a safe value as a keyword-argument default.
        ''' 

        properties :: Properties
        grammar    :: Grammar

        '''
        
        self.set_rules(grammar.rules, grammar.exports, properties)

        self.set_lists(grammar.lists)

        self.doOnlyGotResultsObject = properties.doOnlyGotResultsObject

    ##############################
    
    def set_rules(self, rules, exports, properties):
        
        self.load(rules,
                  allResults = properties.shouldEavesdrop,
                  hypothesis = properties.shouldHypothesize)
        
        self.grammar_specification = rules
        # ^ set only after `load()` succeeds.

        self.set_exports(exports, properties)

    ##############################
        
    def set_exports(self, exports, properties):

        if properties.activity:
            pass # TODO guard the below?

        self.activateSet(exports,
                         exclusive = properties.exclusivity)

    # activateSet is idempotent, unlike activate
        
    # TODO must it reload the grammar too?

    ##############################

    def set_lists(self, lists):

        for (lhs, rhs) in lists.items():
            self.setList(lhs, rhs)

    ##############################################
    
    # called when speech is detected, before recognition begins.
    
    def gotBegin(self, moduleInfo):
        # handleDGNContextResponse(timeit("/context", urlopen, ("%s/context" % server_address), timeout=0.1))
        # TODO parameterize "context" API

        print
        print
        print "-  -  -  -  gotBegin  -  -  -  -"
        # NOTE moduleInfo is just the current window in Windows

    ##############################
        
    def gotHypothesis(self, words):
        print
        print "---------- gotHypothesis -------------"
        print words

    ##############################

    # recognitionType = self | reject | other

    def gotResultsObject(self, recognitionType, resultsObject):
        
        print "---------- gotResultsObject ----------"
        print "recognitionType =", recognitionType
        if not recognitionType: return
        words = next(get_results(resultsObject), [])
        data  = toUnicode(words)                                   # munge_recognition(words)
        url   = "%s/recognition/" % (server_address,)        # TODO parameterize "recognition" API

        # print 'resultsObject =',resultsObject
        print 'words =', words
        print 'url   =', url
        
        # # NOTE this correctly inserts characters into the virtual machine playString
        # natlink.playString (' '.join(words)) 

        try:
            if should_request(self,data):
                do_request(self,data)
        except Exception as e:
            print
            print "---------- error ------------------"
            print "sending the request and/or handling the response threw:"
            print e
            print traceback.format_exc()

        # don't print until the request is sent the response is handled
        try:
            print
            print "status =", response.getcode()
            print "body   =", response
        except NameError:
            print
        except Exception as e:
            print
            print "---------- error ------------------"
            print e
            print traceback.format_exc()

    ##############################################

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
    
    GRAMMAR = EavesdropGrammar()
    GRAMMAR.initialize(config, properties=properties)

# # # # # # # # # # # # # # # # # # # # # # # # #

def unload():
    global GRAMMAR
    
    if GRAMMAR:
        GRAMMAR.unload()

    GRAMMAR = None

# # # # # # # # # # # # # # # # # # # # # # # # #

load()

##################################################
# -*- coding: utf-8 -*-
##################################################
# project (local) modules:

from   types import *
import api

##################################################
# natlink13 modules:

from   natlinkutils import (GrammarBase)
import natlink
# ^ (a DLL)

##################################################
# standard-library modules:

import time
import json
import urllib2
import traceback
from   collections import (namedtuple)

##################################################

voraciousProperties = Properties( active                 = True,  # i.e. On.
                                  exclusivity            = True,  # i.e. Exclusive.
                                  shouldEavesdrop        = True,  # i.e. do eavesdrop other grammars.
                                  shouldHypothesize      = True,  # i.e. do handle all hypotheses.
                                  doOnlyGotResultsObject = False) # i.e. do call `gotResults()`.

##################################################

fastProperties = Properties( active                 = True,
                             exclusivity            = True,
                             shouldEavesdrop        = False, # (fewer callbacks)
                             shouldHypothesize      = False, # (fewer callbacks)
                             doOnlyGotResultsObject = True)  # (fewer callbacks)

##################################################

defaultProperties = fastProperties

##################################################

def first_result(resultsObject):

    return next(get_results(resultsObject), None)

def get_results(resultsObject):

    try:
        for number in xrange(10):
            yield resultsObject.getWords(number)

    except natlink.OutOfRange:
        return

        # "exceptions aren't exceptional" lmfao.
        # iterators are more idiomatic.

##################################################

class NarcissisticGrammar(GrammarBase):
    ''' this Grammar is 'Narcissistic' because:

    * `load(..,     allResults = 1)` means: every recognition triggers gotResultsObject.
    * `load(..,     hypothesis = 1)` means: every hypothesis, before the recognition, triggers gotHypothesis.
    * `activate(.., exclusive  = 1)` means: deactivate every other non-exclusive rule.

    When both flags are set on load, NarcissisticGrammar.gotResultsObject is called on
    every recognition of every exclusive rule, including of course this class's rules 
    (though I only expect the one grammar of this class to be active globally).

    '''

    ##############################

    gramSpec = None             # :: String

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

#     def configure(self, allResults=True , hypothesis=True , doOnlyGotResultsObject=True):
#        self.load(self.gramSpec, allResults=int(allResults), hypothesis=int(hypothesis))
#        self.doOnlyGotResultsObject = doOnlyGotResultsObject

    ##############################

    # TODO    should include export for safety?
    
    def set_rules(self, rules, exports, properties):
        
        self.load(rules,
                  allResults = properties.shouldEavesdrop,
                  hypothesis = properties.shouldHypothesize)
        
        self.gramSpec = rules
        # ^ set only after `load()` succeeds.

        self.set_exports(exports, properties)

    ##############################
        
    def set_exports(self, exports, properties):

        if properties.active:
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

    

    ##############################

##################################################
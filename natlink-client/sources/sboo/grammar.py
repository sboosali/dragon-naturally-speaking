# -*- coding: utf-8 -*-
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

Properties = namedtuple('Properties',

                        [ 'active',                # :: Bool
                                                   #
                                                   # =  Off | On
                                                   #
                          
                          'exclusivity',           # :: Bool
                                                   #
                                                   # =  Inclusive | Exclusive
                                                   #
                          
                          'shouldEavesdrop',       # :: Bool
                                                   #
                                                   # if `exclusivity` is Inclusive (i.e. `False`), then `shouldEavesdrop` being `True` means:
                                                   # call the `gotResultsObject()` methods of other grammars which are active (i.e. their `active` is `On`).
                                                   #
                          
                          'shouldHypothesize',     # :: Bool
                                                   #
                                                   # Whether `gotHypothesis()` gets called;
                                                   # i.e. the streaming recognition-hypotheses, not just the recognition.
                                                   #
                          
                          'doOnlyGotResultsObject' # :: Bool
                                                   #
                                                   # Whether any methods after `gotResultsObject()` get called (i.e. no `getResults()`).
                                                   #
                        ],
                        
                        verbose=True)

##################################################

Grammar = namedtuple('Grammar',

                     [ 'exports',   # :: [ String ]
                                    #
                                    # a list of **rule-names**.
                                    # e.g. the string 'emacs' is a valid rule-name if, and only if,
                                    # `rules` has a line like this:
                                    # 
                                    #      '''exported <emacs> = ...;''',
                                    # 
                                    # 
                       
                       'rules',     # :: String
                                    #
                                    # a **grammar-specification** string.
                                    # its format is a variant of (extended) BNF.
                                    #
                                    # for the syntax and meaning of this grammar-format, see:
                                    # 
                                    #     TODO
                                    # 
                                    # 
                       
                       'lists',     # :: { String: [String] }
                                    #
                                    # a `dict` mapping **list-names** to a list of words.
                                    # e.g.
                                    # 
                                    #    { 'action': ['cut','copy','paste','undo'],
                                    #      'button': ['left','middle','right'],
                                    #      ...
                                    #    } 
                                    # 
                                    # a valid list-name is referenced somewhere within `rules` references a **list-production**;
                                    # and vice versa, `rules` is valid if (among other invariants) each referenced list-production
                                    # is a key of `lists`.
                                    # 
                                    # e.g. for 'button', `rules` could have `{button}` within some right-hand-side, like so:
                                    # 
                                    #     '''<click> = (single | double | triple)? {button}'''
                                    # 
                                    # 
                       
                     ],
                     
                     verbose=True)

##################################################

defaultProperties = Properties
                        ( active                 = True, # i.e. On.
                          exclusivity            = True, # i.e. Exclusive.
                          shouldEavesdrop        = True, # i.e. do eavesdrop other grammars.
                          shouldHypothesize      = True, # i.e. do handle all hypotheses.
                          doOnlyGotResultsObject = True) # i.e. do not call `gotResults()`.

##################################################

def get_results(resultsObject):
    try:
        # "exceptions aren't exceptional" lmfao.
        # iterators are more idiomatic.
        for number in xrange(10):
            yield resultsObject.getWords(number)
    except: #TODO natlink.OutOfRange
        return

##################################################

class NarcissisticGrammar(GrammarBase):
    ''' this Grammar is 'Narcissistic' because:

    * load(.., allResults=1)     means: every recognition triggers gotResultsObject
    * load(.., hypothesis=1)     means: every hypothesis, before the recognition, triggers gotHypothesis
    * activate(.., exclusive=1)  means: deactivate every other non-exclusive rule

    When both flags are set on load, NarcissisticGrammar.gotResultsObject is called on
    every recognition of every exclusive rule, including of course this class's rules 
    (though I only expect the one grammar of this class to be active globally).

    '''

    ##############################
    
    def initialize(self, grammar, properties = defaultProperties):
        ''' 

        properties :: Properties
        grammar    :: Grammar

        '''
        
        self.set_rules(grammar.rules, grammar.exports, properties)

        self.set_lists(grammar.lists)

        self.doOnlyGotResultsObject = properties.doOnlyGotResultsObject  # aborts all further processing after calling gotResultsObject

    ##############################

#     def configure(self, allResults=True , hypothesis=True , doOnlyGotResultsObject=True):
#        self.load(self.gramSpec, allResults=int(allResults), hypothesis=int(hypothesis))
#        self.doOnlyGotResultsObject = doOnlyGotResultsObject

    ##############################

    # TODO    should include export for safety?
    
    def set_rules(self, rules, exports, properties):
        
        self.gramSpec = rules #TODO necessary?
        
        self.load(rules, allResults = properties.shouldEavesdrop, hypothesis = properties.shouldHypothesize)

        self.set_exports(exports, properties)

    ##############################
        
    def set_exports(self, exports, properties):

        if properties.active:
            pass # TODO guard the below?

        self.activateSet( exports, exclusive = properties.exclusivity )

    # activateSet is idempotent, unlike activate
        
    # TODO must it reload the grammar too?

    ##############################

    def set_lists(self, lists):

        for (lhs, rhs) in lists.items():
            self.setList(lhs, rhs)

    ##############################
    
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
                print 'data  =', json.dumps(data)
                request  = urllib2.Request(url, json.dumps(data), \{"Content-Type": "application/json"})
                response = urllib2.urlopen(request)
                handleResponse(self, response) 
            pass
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

    ##############################

    # for debugging only, shows whether specific rules (rather than the generic dgndictation) are matching the recognition
    # not called when (self.doOnlyGotResultsObject=True)

    def gotResults(self, words, fullResults):
        print
        print "---------- gotResultsObject ----------"
        print "fullResults =", fullResults

    ##############################

##################################################
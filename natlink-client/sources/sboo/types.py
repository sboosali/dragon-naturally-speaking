# -*- coding: utf-8 -*-
"""Sum Types and Product Types only"""
##################################################
# project (local) modules:

from   enum import (Enum)
       # ^ vendored `enum34` package, version `1.1.6`.

##################################################
# (standard-library modules)

from   collections import (namedtuple)

##################################################

DEBUG = False

##################################################

class MicrophoneState(Enum):
    ON       = 0
    SLEEPING = 1
    OFF      = 2
    DEAD     = 3

##################################################

class CorrectionStatus(Enum):
    SUCCESS      = 0
    HETEROPHONIC = 1
    INVALID      = 2

##################################################

class Activity(Enum):
    '''`bool`-like.

    i.e. two variants, the "falsy" coming before the "truthy".
    '''
    INACTIVE = 0
    ACTIVE   = 1

##################################################

class Exclusivity(Enum):
    '''`bool`-like.

    i.e. two variants, the "falsy" coming before the "truthy".
    '''
    INCLUSIVE = 0
    EXCLUSIVE = 1

##################################################

class RecognitionType(Enum):
    SELF   = 0
    REJECT = 1
    OTHER  = 2

##################################################

Address = namedtuple('Address',

                        [ 'host',           # :: String
                          
                          'port'            # :: Int
                        ])

##################################################

Properties = namedtuple('Properties',

                        [ 'activity',              # :: `Activity` (Enum)
                                                   #
                          
                          'exclusivity',           # :: `Exclusivity` (Enum)
                                                   #
                          
                          'shouldEavesdrop',       # :: Bool
                                                   #
                                                   # if `exclusivity` is Inclusive (i.e. `False`), then `shouldEavesdrop` being `True` means:
                                                   # call the `gotResultsObject()` methods of other grammars which are active (i.e. their `activity` is `ACTIVE`).
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
                        
                        verbose=DEBUG)

##################################################

GrammarConfig = namedtuple('GrammarConfig',

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
                       
                     ])

##################################################

Recognition = namedtuple('Recognition',

                       [ 'results',    # :: Results

                         'recognition' # :: Recognition

                       ])

##################################################

Hypotheses = namedtuple('Hypotheses',

                       [ 'results',   # :: Results

                         'hypotheses' # :: [Recognition]

                       ])

##################################################

Corrected = namedtuple('Corrected',

                       [ 'results', # :: Results

                         'status'   # :: CorrectionStatus
                       ])

##################################################

class Results(object):
    # (explicitly declare new-style class, because we're in Python2)

    def __init__(self, identifier):
        self.identifier = int(identifier)

##################################################
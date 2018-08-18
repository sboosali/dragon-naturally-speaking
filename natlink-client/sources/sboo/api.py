# -*- coding: utf-8 -*-
##################################################
# (natlink13 modules)

from natlinkmain import (setCheckForGrammarChanges)

import natlink
# ^ (a DLL)

##################################################
# (standard-library modules)

import time
import json
import urllib2
import traceback
from collections import (namedtuple)

##################################################

# TODO             handleDGNUpdate(grammar, response)
def handleDGNUpdate(grammar, response):
    pass

def should_request(grammar,data):
    b = data and not handle_microphone(grammar,data) and isUnicode(data)
    print "should_request=", b
    return b

# returns true if it matched the recognition (and executed the magic action).
# in which case, don't send a request to the server to execute any non-magic actions.
# "mike off" deactivates all grammars besides the microphone grammer, "putting the microphone to sleep".
def handle_microphone(grammar,data):
    raw = " ".join(data)

    if   raw == "mike on":
        # grammar.setMicState("on")
        grammar.activateSet([microphone_export, H_EXPORT], exclusive=1)
        return True
    elif raw == "mike off":
        # grammar.setMicState("sleeping")
        grammar.activateSet([microphone_export],exclusive=1)
        return True
    elif raw == "mike dead":
        # the natlink.setMicState("off") # can't even be manually turned back on via the GUI
        return True
    else:
        return False

'''
''' 
def handleResponse(grammar, response) : 
    pass 

##################################################
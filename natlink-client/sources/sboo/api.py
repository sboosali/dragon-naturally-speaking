# -*- coding: utf-8 -*-
##################################################
# project (local) modules:

from   types import *
import utilities

##################################################
# (natlink13 modules)

from   natlinkmain import (setCheckForGrammarChanges)
import natlink
# ^ (a DLL)

##################################################
# (standard-library modules)

import json
import urllib2

##################################################

json_content_type = {"Content-Type": "application/json"}

##################################################

def post_json(url,data):
    
    request  = urllib2.Request(url,
                               json.dumps(data),
                               json_content_type)
    
    response = urllib2.urlopen(request)
    return response

##################################################

def recognition(resultsObject):

    data = todo(resultsObject)

    response = post_json("/recognition", data)

    pass

##################################################

##################################################

def hypotheses(resultsObject):

    data = todo(resultsObject)

    response = post_json("/hypotheses", data)

    pass

##################################################

##################################################

def listening():

    response = post_json("/listening", None)

    pass

##################################################

##################################################

def corrected(resultsObject, didCorrectionSucceed):

    data = { result: todo(resultsObject),
             status: didCorrectionSucceed }

    response = post_json("/corrected", data)

    pass

##################################################

##################################################
##################################################
##################################################

def do_request(self,url,data):
                print 'data  =', json.dumps(data)
                response = post_json(url, data)
                handleResponse(self, response)

##################################################

def should_request(grammar,data):
    b = data and not handle_microphone(grammar,data) and isUnicode(data)
    print "should_request=", b
    return b

##################################################

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

##################################################
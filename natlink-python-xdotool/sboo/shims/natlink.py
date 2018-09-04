# -*- coding: utf-8 -*-
##################################################

class OutOfRange(Exception):
    pass

##################################################

_MicState_ = 'sleeping'

def setMicState(s):
    global _MicState_
    _MicState_ = s

def getMicState():
    global _MicState_
    return _MicState_

##################################################
# -*- coding: utf-8 -*-
##################################################
try:
    
    from         natlink import (setMicState, getMicState, OutOfRange)

##################################################
except ImportError:
    
    from ..shims.natlink import (setMicState, getMicState, OutOfRange)

##################################################
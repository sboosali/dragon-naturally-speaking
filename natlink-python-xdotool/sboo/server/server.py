# -*- coding: utf-8 -*-
##################################################

from .xdotool import *

##################################################

import datetime
import xmlrpclib
from   SimpleXMLRPCServer import (SimpleXMLRPCServer)

##################################################

def count(xs):
    return len(xs)

def day(days):
    today = datetime.datetime.today()
    day = today + datetime.timedelta(days=1)
    return xmlrpclib.DateTime(day)

##################################################

def __main__():
    
    server = SimpleXMLRPCServer(("localhost", 8000))
    
    server.register_function(day, "day")
    server.register_function(count)
    #server.register_function(xdotool_keys, "keys")
    server.register_function(xdotool_chars, "text")
    
    server.register_introspection_functions() 
    
    server.serve_forever()

##################################################

if __name__ == '__main__':
    __main__()

##################################################
# -*- coding: utf-8 -*-

import datetime
import xmlrpclib
from SimpleXMLRPCServer import (SimpleXMLRPCServer)

def today():
    today = datetime.datetime.today()
    return xmlrpclib.DateTime(today)

server = SimpleXMLRPCServer(("localhost", 8000))
server.register_function(today, "today")
server.serve_forever()


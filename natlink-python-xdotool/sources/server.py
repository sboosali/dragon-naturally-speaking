# -*- coding: utf-8 -*-

import datetime
import xmlrpclib
from SimpleXMLRPCServer import (SimpleXMLRPCServer)

def day(days):
    today = datetime.datetime.today()
    day = today + datetime.timedelta(days=1)
    return xmlrpclib.DateTime(day)

server = SimpleXMLRPCServer(("localhost", 8000))
server.register_function(day, "day")
server.serve_forever()


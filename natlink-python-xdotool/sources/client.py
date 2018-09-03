# -*- coding: utf-8 -*-

import xmlrpclib
import datetime

# from sboo.types import *
# def to_url(address):
#   return ("http://%s:%s/") % (address.host, address.port)
# xmlrpclib.ServerProxy( to_url(address), allow_none=True )

proxy = xmlrpclib.ServerProxy("http://localhost:8000/")

today = proxy.today()

converted = datetime.datetime.strptime(today.value, "%Y%m%dT%H:%M:%S")
# ^ convert the ISO8601 string to a datetime object

print "Today: %s" % converted.strftime("%d.%m.%Y, %H:%M")


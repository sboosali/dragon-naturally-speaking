# -*- coding: utf-8 -*-

import xmlrpclib
import datetime

# from sboo.types import *
# def to_url(address):
#   return ("http://%s:%s/") % (address.host, address.port)
# xmlrpclib.ServerProxy( to_url(address), allow_none=True )

proxy = xmlrpclib.ServerProxy("http://localhost:8000/")

day = proxy.day(30)

converted = datetime.datetime.strptime(day.value, "%Y%m%dT%H:%M:%S")
# ^ convert the ISO8601 string to a datetime object

print "Day: %s" % converted.strftime("%d.%m.%Y, %H:%M")

recognition = proxy.text("#hello $world")


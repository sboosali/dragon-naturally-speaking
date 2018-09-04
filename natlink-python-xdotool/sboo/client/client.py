# -*- coding: utf-8 -*-
##################################################

import xmlrpclib
import datetime

##################################################

# from sboo.types import *

##################################################

# def to_url(address):
#   return ("http://%s:%s/") % (address.host, address.port)
# xmlrpclib.ServerProxy( to_url(address), allow_none=True )

##################################################

proxy = xmlrpclib.ServerProxy("http://localhost:8000/", verbose=True)

# day = proxy.day(30)
# converted = datetime.datetime.strptime(day.value, "%Y%m%dT%H:%M:%S")
# # ^ convert the ISO8601 string to a datetime object
# print "Day: %s" % converted.strftime("%d.%m.%Y, %H:%M")

##################################################

def main():
    print '=================================================='
    print
    length = proxy.count("#hello <world>\n")
    print
    print length
    print
    print '=================================================='
    print
    s = proxy.system.listMethods()
    print
    print s
    print
    print '=================================================='
    print
    s = proxy.system.methodSignature('count')
    print
    print s
    print
    # ^ "signatures not supported"
    #
    # lol python being dynamic kills signature inference:
    # https://bugs.python.org/issue13404
    #
    print '=================================================='
    print
    s = proxy.system.methodHelp('count')
    print
    print s
    print
    # ^ 
    #
    # the `docstring`
    #
    print '=================================================='
    print

##################################################

if __name__ == '__main__':
    main()

##################################################
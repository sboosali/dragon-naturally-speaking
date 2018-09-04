# -*- coding: utf-8 -*-

import subprocess

import datetime
import xmlrpclib
from SimpleXMLRPCServer import (SimpleXMLRPCServer)

def day(days):
    today = datetime.datetime.today()
    day = today + datetime.timedelta(days=1)
    return xmlrpclib.DateTime(day)

def xdotool(subcommand, arguments):

    script = [ "xdotool", "sleep", "1", subcommand, "--clearmodifiers" ] + arguments

    exit_code = subprocess.check_call( script,
                           shell=False,
                           stdin=None,
                           stdout=None,
                           stderr=None,
                           cwd=None,
                           env=None,
                           universal_newlines=False
    )

    return exit_code == 0

def xdotool_keys(ks):
    pass
    # for k in ks:
    #     xdotool( "key", k )

def xdotool_chars(s):
    return xdotool( "type", [s] )

server = SimpleXMLRPCServer(("localhost", 8000))

server.register_function(day, "day")
server.register_function(xdotool_chars, "text")
server.register_function(xdotool_keys, "keys")

server.register_introspection_functions() 
server.serve_forever()

# https://docs.python.org/2/library/subprocess.html


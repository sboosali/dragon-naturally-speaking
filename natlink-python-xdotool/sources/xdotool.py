# -*- coding: utf-8 -*-
##################################################

import subprocess

##################################################

def xdotool(subcommand, arguments):
    '''

    '''

    script = [ "xdotool",
               "sleep", "1",
               subcommand,
               "--clearmodifiers"] + arguments

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

##################################################

def xdotool_keys(ks):
    pass
    # for k in ks:
    #     xdotool( "key", k )

def xdotool_chars(s):
    return xdotool("type", [s])

##################################################

# https://docs.python.org/2/library/subprocess.html

##################################################
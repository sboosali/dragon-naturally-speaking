# -*- coding: utf-8 -*-
##################################################
# notes
# =====
#
# this is a wrapper around `gramparser`,
# to test `.sapi` files from the command-line.
#

##################################################
# (standard-library modules)

import os
import sys

##################################################
# (project modules)

##################################################
# (natlink13 modules)

if os.environ.get('SBOO_NATLINK'):
    import sboo.shims.gramparser as gramparser
else:
    import                    gramparser

    # ^ `gramparser`:
    #
    #      This module contains the Python code to convert the textual represenation
    #   of a command and control grammar in the standard SAPI CFG binary format.
    #

##################################################
##################################################

def parse_sapi_file(rules_file):
    
    rules_string = None
    with open(rules_file, 'r') as f:
        rules_string = f.read()
    
    rules_list = rules_string.split('\n')
    
    parser = gramparser.GramParser(rules_list)
    parser.doParse()
    parser.checkForErrors()
    
    rules_binary = parser.dumpString()
    
    return rules_binary

##################################################

if __name__ == "__main__":
    
    rules_file = sys.argv[1]
    
    parse_sapi_file(rules_file)

##################################################
##################################################

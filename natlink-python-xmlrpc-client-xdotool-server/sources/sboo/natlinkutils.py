# -*- coding: utf-8 -*-
##################################################

class GramObj():

    def load( self, binary, allResults=0, hypothesis=0, grammarName=None ):
        pass

    def unload( self ):
        pass

    def activate(self, ruleName, window=0, exclusive=None, noError=0):
        pass

    def deactivate( self, ruleName ):
        pass

##################################################

class GrammarBase(GramObj):

    def activateSet(self, ruleNames, window=0, exclusive=None):
        pass

    def setList(self, listName, words):
        pass

##################################################
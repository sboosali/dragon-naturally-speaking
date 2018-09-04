#-*- coding: utf-8 -*-
##################################################
# project (local) modules:

from   ..enum import (Enum)
       # ^ vendored `enum34` package, version `1.1.6`.

##################################################
# (standard-library modules)

import time
import traceback

# import json
# from   ConfigParser import ConfigParser

##################################################

'''
json.dumps("cafe'") (i.e. with acute accent) causes
```UnicodeDecodeError: 'utf8' codec can't decode byte 0xe9 in position 3: unexpected end of data```

>>> 'caf\xe9'.decode('cp1252').encode('utf-8')
u'caf\xe9'

'''
def is_unicode(data): # TODO
    try:
        for word in data:
            word.decode('cp1252').encode('utf8')
        return True
    except UnicodeDecodeError as e:
        print e
        print traceback.format_exc()
        return False

##################################################

def to_unicode(data): # TODO
    try:
        return [word.decode('cp1252').encode('utf8') for word in data]
    except UnicodeDecodeError as e:
        print e
        print traceback.format_exc()
        return False

##################################################

def to_words(string):
    return [line.strip()
            for line in string.split('\n')
            if not line.isspace()]

##################################################

def to_enum(cls, i):
    ''' like Haskell `toEnum`.

    e.g.

    >>> from enum import Enum

    >>> class Exclusivity(Enum):
    ...     INCLUSIVE = 0
    ...     EXCLUSIVE = 1

    >>> to_enum(Exclusivity, False)
    <Exclusivity.INCLUSIVE: 0>

    >>> to_enum(Exclusivity, 1)
    <Exclusivity.EXCLUSIVE: >

    '''
    return cls._value2member_map_[int(i)]

##################################################

def ini_get_bool(config, section, option, default):
    '''
    parse an INI-sstyle flag to a `bool` value. 
    
    i.e. "Y", "yes", "True", "1" are all `True` (or `1`).

    case-insensitive(?)
    '''

    if config.has_option(section, option):
        b = config.getboolean(section, option)
        return b
        # ^ `int` acts like `toEnum`.

    else:
        return default

##################################################

def ini_get_enum(EnumClass, config, section, option, default):
    '''
    parse an INI-sstyle flag to a the (given) `Enum` value. 
    
    i.e. "Y", "yes", "True", "1" are all `True` (or `1`).

    case-insensitive(?)
    '''

    if config.has_option(section, option):
        b = config.getboolean(section, option)
        e = to_enum(EnumClass, b)
        return e
    
        # ^ `int` also acts like `to_enum`,
        # but doesn't preserve metadata for better rendering.

    else:
        return default

##################################################

def ini_get_text(config, section, option, default):
    '''
    '''

    if config.has_option(section, option):
        s = config.get(section, option)
        return s

    else:
        return default

##################################################

# current time in milliseconds
def now():
    return int(time.clock() * 1000)

##################################################

# http://stackoverflow.com/questions/1685221/accurately-measure-time-python-function-takes
def timeit(message, callback, *args, **kwargs):
    before = time.clock()
    result = callback(*args,**kwargs)
    after = time.clock()
    print message, ': ', (after - before) * 1000, 'ms'
    return result

##################################################

def print_exception_as_warning(e):
    print '--------------------------------------------------'
    print '[WARNING]'
    print
    traceback.print_exc(e)
    print
    print '--------------------------------------------------'

#print_exception_with_dividers

##################################################



##################################################

##################################################
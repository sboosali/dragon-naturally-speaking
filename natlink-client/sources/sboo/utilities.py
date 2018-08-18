#-*- coding: utf-8 -*-
##################################################
# (standard-library modules)

import time

##################################################

# current time in milliseconds
def now():
    return int(time.clock() * 1000)

# http://stackoverflow.com/questions/1685221/accurately-measure-time-python-function-takes
def timeit(message, callback, *args, **kwargs):
    before = time.clock()
    result = callback(*args,**kwargs)
    after = time.clock()
    print message, ': ', (after - before) * 1000, 'ms'
    return result

'''
json.dumps("cafe'") (i.e. with acute accent) causes
```UnicodeDecodeError: 'utf8' codec can't decode byte 0xe9 in position 3: unexpected end of data```

>>> 'caf\xe9'.decode('cp1252').encode('utf-8')
u'caf\xe9'

'''
def isUnicode(data): # TODO
    try:
        for word in data:
            word.decode('cp1252').encode('utf8')
        return True
    except UnicodeDecodeError as e:
        print e
        print traceback.format_exc()
        return False

def toUnicode(data): # TODO
    try:
        return [word.decode('cp1252').encode('utf8') for word in data]
    except UnicodeDecodeError as e:
        print e
        print traceback.format_exc()
        return False

def first_result(resultsObject):
    return next(get_results(resultsObject), None)

##################################################
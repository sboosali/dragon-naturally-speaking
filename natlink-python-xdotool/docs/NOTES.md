# `xmlrpclib`

## 



## e.g. server

```python

import datetime
import xmlrpclib

from SimpleXMLRPCServer import (SimpleXMLRPCServer)

def today():
    today = datetime.datetime.today()
    return xmlrpclib.DateTime(today)

server = SimpleXMLRPCServer(("localhost", 8000))
server.register_function(today, "today")
server.serve_forever()
```


## e.g. client

```python
import xmlrpclib
import datetime

proxy = xmlrpclib.ServerProxy("http://localhost:8000/")

today = proxy.today()

# convert the ISO8601 string to a datetime object
converted = datetime.datetime.strptime(today.value, "%Y%m%dT%H:%M:%S")

print "Today: %s" % converted.strftime("%d.%m.%Y, %H:%M")
```


## Notes


### [Types]

Conformable Datatypes (i.e. Python Types that can be marshalled through XML into XML-RPC Types, and back), include:


* `boolean`
= `bool`


* `int` or `i4`
int or long; in range from -2147483648 to 2147483647.


* `double`
= `float`


* `string`
= `str` or `unicode`.


* `array`
= `list` or `tuple`; containing conformable elements.


* `struct`	
= `dict`
Keys must be strings, values may be any conformable type. 
Objects of user-defined classes can be passed in; only their __dict__ attribute is transmitted.


* `dateTime.iso8601`
= `DateTime` or `datetime.datetime`
Returned type depends on values of the `use_datetime` flags.


* `base64`
= `Binary`


* `nil`
= `None` (The None constant)
Passing is allowed only if `allow_none` is `True`.


### [Errors]

MethodCalls may also raise:

* `Fault`
< `xmlrpclib.Error`
a special Fault instance, used to signal XML-RPC server errors.

* `ProtocolError`
< `xmlrpclib.Error`
used to signal an error in the HTTP/HTTPS transport layer. 


### [Values]

```python
xmlrpclib.ServerProxy( to_url(address), allow_none=True )
```

## Links

* [xmlrpclib](https://docs.python.org/2/library/xmlrpclib.html)
* [SimpleXMLRPCServer](https://docs.python.org/2/library/simplexmlrpcserver.html)

# `XML`-`RPC`

## Example MethodCall
==================================================

### (Code)
--------------------------------------------------

from:

```python
import xmlrpclib
import datetime

proxy = xmlrpclib.ServerProxy("http://localhost:8000/", verbose=True)

i = proxy.count("hello <world>\n")
print i
# ^ 14


```


### (`stdout`)
--------------------------------------------------

```
send: "POST / HTTP/1.1\r\nHost: localhost:8000\r\nAccept-Encoding: gzip\r\nUser-Agent: xmlrpclib.py/1.0.1 (by www.pythonware.com)\r\nContent-Type: text/xml\r\nContent-Length: 169\r\n\r\n<?xml version='1.0'?>\n<methodCall>\n<methodName>text</methodName>\n<params>\n<param>\n<value><string>#hello &lt;world&gt;\n</string></value>\n</param>\n</params>\n</methodCall>\n"
reply: 'HTTP/1.0 200 OK\r\n'
header: Server: BaseHTTP/0.3 Python/2.7.15
header: Date: Tue, 04 Sep 2018 03:55:32 GMT
header: Content-type: text/xml
header: Content-length: 129
body: "<?xml version='1.0'?>\n<methodResponse>\n<params>\n<param>\n<value><boolean>1</boolean></value>\n</param>\n</params>\n</methodResponse>\n"
```

i.e...

### Example Request

```
send: "POST / HTTP/1.1\r\nHost: localhost:8000\r\nAccept-Encoding: gzip\r\nUser-Agent: xmlrpclib.py/1.0.1 (by www.pythonware.com)\r\nContent-Type: text/xml\r\nContent-Length: 169\r\n\r\n<?xml version='1.0'?>\n<methodCall>\n<methodName>text</methodName>\n<params>\n<param>\n<value><string>#hello &lt;world&gt;\n</string></value>\n</param>\n</params>\n</methodCall>\n"
#hello <world>
```

### Example Response
--------------------------------------------------

```
reply: 'HTTP/1.0 200 OK\r\n'

header: Server: BaseHTTP/0.3 Python/2.7.15
header: Date: Tue, 04 Sep 2018 03:55:32 GMT
header: Content-type: text/xml
header: Content-length: 129

body: "<?xml version='1.0'?>\n<methodResponse>\n<params>\n<param>\n<value><boolean>1</boolean></value>\n</param>\n</params>\n</methodResponse>\n"

```



==================================================
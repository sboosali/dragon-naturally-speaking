# `XML`-`RPC`

## Example MethodCall

### (Code)


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


```
send: "POST / HTTP/1.1\r\nHost: localhost:8000\r\nAccept-Encoding: gzip\r\nUser-Agent: xmlrpclib.py/1.0.1 (by www.pythonware.com)\r\nContent-Type: text/xml\r\nContent-Length: 170\r\n\r\n<?xml version='1.0'?>\n<methodCall>\n<methodName>count</methodName>\n<params>\n<param>\n<value><string>#hello &lt;world&gt;\n</string></value>\n</param>\n</params>\n</methodCall>\n"
reply: 'HTTP/1.0 200 OK\r\n'
header: Server: BaseHTTP/0.3 Python/2.7.15
header: Date: Tue, 04 Sep 2018 05:07:25 GMT
header: Content-type: text/xml
header: Content-length: 122
body: "<?xml version='1.0'?>\n<methodResponse>\n<params>\n<param>\n<value><int>15</int></value>\n</param>\n</params>\n</methodResponse>\n"
```

i.e...

### Example Request

```
[send] "POST / HTTP/1.1\r\nHost: localhost:8000\r\nAccept-Encoding: gzip\r\nUser-Agent: xmlrpclib.py/1.0.1 (by www.pythonware.com)\r\nContent-Type: text/xml\r\nContent-Length: 170\r\n\r\n<?xml version='1.0'?>\n<methodCall>\n<methodName>count</methodName>\n<params>\n<param>\n<value><string>#hello &lt;world&gt;\n</string></value>\n</param>\n</params>\n</methodCall>\n"
```

### Example Response


```
[reply]  'HTTP/1.0 200 OK\r\n'

[header] Server: BaseHTTP/0.3 Python/2.7.15
[header] Date: Tue, 04 Sep 2018 05:07:25 GMT
[header] Content-type: text/xml
[header] Content-length: 122

[body]   "<?xml version='1.0'?>\n<methodResponse>\n<params>\n<param>\n<value><int>15</int></value>\n</param>\n</params>\n</methodResponse>\n"
```

### Example Request (formatted)

```
POST / HTTP/1.1
Host: localhost:8000
Accept-Encoding: gzip
User-Agent: xmlrpclib.py/1.0.1 (by www.pythonware.com)
Content-Type: text/xml
Content-Length: 170

<?xml version=1.0?>
<methodCall>
<methodName>count</methodName>
<params>
<param>
<value><string>#hello &lt;world&gt;
</string></value>
</param>
</params>
</methodCall>
```

### Example RequestMethod (formatted)

```
POST / HTTP/1.1
```

### Example RequestHeaders (formatted/cleaned)


```ini
Host            : localhost:8000
Accept-Encoding : gzip
User-Agent      : xmlrpclib.py/1.0.1
Content-Type    : text/xml
Content-Length  : 170
```

### Example RequestBody (formatted)

```xml
<?xml version=1.0?>

<methodCall>
  
  <methodName>count</methodName>
  
  <params>
    <param>
      <value>

        <string>hello &lt;world&gt;
</string>

      </value>
    </param>
  </params>
</methodCall>
```

### Example ResponseBody (formatted)


```xml
<?xml version=1.0?>

<methodResponse>
  <params>
    <param>
      <value>

        <int>15</int>

      </value>
    </param>
  </params>
</methodResponse>
```



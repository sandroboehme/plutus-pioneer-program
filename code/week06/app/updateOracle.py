import urllib2

newADA_USDTRate = 2
headers = {'Accept': '*/*',
       'Accept-Charset': 'utf-8',
       'Content-Type': 'application/json'}

with open('./../oracle.cid', 'r') as content_file:
    uuid = content_file.read()
    url = 'http://127.0.0.1:8080/api/new/contract/instance/' + uuid + '/endpoint/update/'
    req = urllib2.Request(url, str(newADA_USDTRate * 1000000), headers)
    out = urllib2.urlopen(req)
    result = out.read()
    print(result)

import urllib2

uuid = '4561385f-3a96-4ef6-8402-a9346355f201'
url = 'http://127.0.0.1:8080/api/new/contract/instance/' + uuid + '/endpoint/update/'

headers = {'Accept': '*/*',
       'Accept-Charset': 'utf-8',
       'Content-Type': 'application/json'}
req = urllib2.Request(url, '[ 10.33 ]', headers)
out = urllib2.urlopen(req)
result = out.read()
print(result)

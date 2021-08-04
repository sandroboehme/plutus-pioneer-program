#1/bin/sh


url="http://localhost:8080/api/new/contract/instance/$(cat ../oracle.cid)/endpoint/update"
echo ""$url

# curl -X POST -H "Content-Type: application/json" \
# -d '[ 10.33 ]' $url


curl -i --header 'Content-Type: application/json;charset=utf-8' \
--request POST \
--data-raw '10000000' \
$url
# --data-raw '[ 10 ]' \

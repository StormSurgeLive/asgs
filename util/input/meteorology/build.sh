#!/bin/bash
URL="https://api.metget.zachcobell.com/status"
curl  -v  \
   -H "Content-Type: application/json" \
   -H "x-api-key: $METGET_API_KEY" \
    $URL | json_pp > stuff.json


#!/usr/bin/awk
BEGIN {
   date=strftime("%Y-%m-%d-T%H:%M:%S%z")
}
{ print "["date"] "level": "this": "$0 }

#!/usr/bin/awk
BEGIN {
   date=strftime("%Y-%m-%d-T%H:%M:%S%z")
}
{ print "["date"] "this": "$0 }

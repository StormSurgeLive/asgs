#!/usr/bin/awk
BEGIN {
   # replace strftime with direct call to `date`
   "date +%Y-%m-%d-T%H:%M:%S%z" | getline date
}
{ print "["date"] "level": "this": "$0 }

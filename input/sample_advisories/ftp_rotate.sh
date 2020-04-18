#!/bin/bash
STORM=05
NAME=DORIAN
YEAR=2019
START=01
END=64
SLEEP=21600 # 6 hours
LASTFILE=
for i in $(seq -f "%02g" $START $END); do 
  # RSS via HTTP
  echo cp ${i}.${STORM}${YEAR}.index-at.xml ~/rss-data/index-at.xml
  cp ${i}.${STORM}${YEAR}.index-at.xml ~/rss-data/index-at.xml
  echo "<h1>ASGS Storm Replay</h1><br/>Currently Showing $NAME Advisory $i:<ul><li><a href=/index-at.xml>index-at.xml</a></li><li><a href=ftp://142.93.48.99/atcf/>ftp listing</a></li></ul>" > ~/rss-data/index.html

  # via FTP
  echo cp ${i}.${STORM}${YEAR}.index-at.xml ~/data/atcf/afst/index-at.xml
  cp ${i}.${STORM}${YEAR}.index-at.xml ~/data/atcf/afst/index-at.xml
  echo cp ${i}.bal${STORM}${YEAR}.dat  ~/data/atcf/btk/bal${STORM}${YEAR}.dat
  cp ${i}.bal${STORM}${YEAR}.dat  ~/data/atcf/btk/bal${STORM}${YEAR}.dat
  # copying over best track as forecast also
  echo cp ${i}.al${STORM}${YEAR}.dat  ~/data/atcf/afst/al${STORM}${YEAR}.fst
  cp ${i}.bal${STORM}${YEAR}.dat  ~/data/atcf/afst/al${STORM}${YEAR}.fst
  if [ -e "$LASTFILE" ]; then
    echo rm -f $LASTFILE
    rm -f $LASTFILE
  fi

  # provide some info to clients via .message
  echo "$NAME Advisory $i" > ~/data/atcf/.message
  LASTFILE=~/data/atcf/$NAME:$i
  echo touch $LASTFILE
  touch $LASTFILE

  # sleep
  echo sleeping $SLEEP seconds
  sleep $SLEEP
done

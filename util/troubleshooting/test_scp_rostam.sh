#!/bin/sh
while [ 1 ] ; do 
   DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'` 
   echo "[${DATETIME}] INFO: Copying index-at.xml to rostam.cct.lsu.edu:/data/opendap" >> test_scp_rostam.log 
   scp -vvv -P 8000 index-at.xml jfleming@rostam.cct.lsu.edu:~ >> test_scp_rostam.log 2>&1
   DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'` 
   echo "[${DATETIME}] INFO: Finished." >> test_scp_rostam.log 
   sleep 10 
done

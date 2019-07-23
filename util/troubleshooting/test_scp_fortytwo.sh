#!/bin/sh
while [ 1 ] ; do 
   DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'` 
   echo "[${DATETIME}] INFO: Copying index-at.xml to fortytwo.cct.lsu.edu:/data/opendap" >> test_scp_fortytwo.log 
   scp -vvv -P 2525 index-at.xml jgflemin@fortytwo.cct.lsu.edu:/data/opendap >> test_scp_fortytwo.log 2>&1
   DATETIME=`date +'%Y-%h-%d-T%H:%M:%S%z'` 
   echo "[${DATETIME}] INFO: Finished." >> test_scp_fortytwo.log 
   sleep 10 
done

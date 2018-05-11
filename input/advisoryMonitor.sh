#!/bin/bash
#
# advisoryMonitor.sh: Downloads hindcast and forecast data at configurable
# interval to trace how the NHC posts data over time. 
#
INTERVALMINUTES=$1
STORMNUMBER=$2
YEAR=$3
ASGSDIR=$4
#
RSSSITE=www.nhc.noaa.gov         # site information for retrieving advisories
FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
FDIR=/atcf/afst                  # forecast dir on nhc ftp site
HDIR=/atcf/btk   
TRIGGER=rssembedded
ADVISORY=0
forecastFileNameXML=index-at.xml
forecastFileNameHTML=al${STORMNUMBER}${YEAR}.fst.html
hindcastFileName=bal${STORMNUMBER}${YEAR}.dat
oldAdvisoryNum=0
oldBESTEndDate=0
#
OPTIONS="--storm $STORMNUMBER --year $YEAR --ftpsite $FTPSITE --fdir $FDIR --hdir $HDIR --rsssite $RSSSITE --trigger $TRIGGER --adv 0"
while [[ 1 ]]; do
   advisoryNum=`perl $ASGSDIR/get_atcf.pl $OPTIONS 2>> advisoryMonitor.log`
   if [[ $advisoryNum != $oldAdvisoryNum ]]; then
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      echo "[${DATETIME}] INFO: New forecast advisory detected." >> advisoryMonitor.log
      oldAdvisoryNum=$advisoryNum
   else
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`    
      echo "[${DATETIME}] INFO: The advisory number has not changed." >> advisoryMonitor.log
   fi
   bestEndDate=`tail -n 1 $hindcastFileName | awk '{ print $3 }' | sed -e "s/,//"`
   if [[ $oldBESTEndDate = 0 ]]; then
      oldBESTEndDate=$bestEndDate
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      echo "[${DATETIME}] INFO: Final BEST track line date/time initialized to $oldBESTEndDate." >> advisoryMonitor.log
   fi
   if [[ $bestEndDate -gt $oldBESTEndDate ]]; then
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      echo "[${DATETIME}] INFO: New BEST track data detected." >> advisoryMonitor.log
      oldBESTEndDate=$bestEndDate
   else
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`    
      echo "[${DATETIME}] INFO: The BEST track end time has not changed." >> advisoryMonitor.log
   fi
   # save the files themselves
   DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`  
   mv $forecastFileNameXML ${forecastFileNameXML}.${DATETIME}
   mv $forecastFileNameHTML ${forecastFileNameHTML}.${DATETIME}
   mv $hindcastFileName ${hindcastFileName}.${DATETIME}
   # sleep for the specified length of time
   sleep `echo "$INTERVALMINUTES*60" | bc`
done

#!/bin/bash
#
# advisoryDownloader.sh: Downloads hindcast and forecast data at configurable
# interval to work around requirement for https at NHC 
#
INTERVALMINUTES=$1
STORMNUMBER=$2
YEAR=$3
ASGSDIR=$4
#
#RSSSITE=www.nhc.noaa.gov        # site information for retrieving advisories
RSSSITE=filesystem         
#FTPSITE=ftp.nhc.noaa.gov         # hindcast/nowcast ATCF formatted files
FTPSITE=filesystem        # hindcast/nowcast ATCF formatted files
#FDIR=/atcf/afst                 # forecast dir on nhc ftp site
#
#FDIR=/scratch/bblanton/asgs-advisories/ 
#HDIR=/atcf/btk   
#
FDIR=${ASGSDIR}/input/sample_advisories/2019 
HDIR=${FDIR}   
#
TRIGGER=rssembedded
ADVISORY=0
forecastFileNameXML=index-at.xml
forecastFileNameHTML=al${STORMNUMBER}${YEAR}.fst.html
hindcastFileName=bal${STORMNUMBER}${YEAR}.dat
oldAdvisoryNum=0
oldBESTEndDate=0
#
# hack in a call to curl to actually download the data first because get_atcf.pl
# does not support https
#
OPTIONS="--storm $STORMNUMBER --year $YEAR --ftpsite $FTPSITE --fdir $FDIR --hdir $HDIR --rsssite $RSSSITE --trigger $TRIGGER --adv $ADVISORY"
while [[ 1 ]]; do
   curl https://www.nhc.noaa.gov/index-at.xml > $FDIR/index-at.xml
   curl http://ftp.nhc.noaa.gov/atcf/btk/$hindcastFileName > $FDIR/$hindcastFileName
   advisoryNum=`perl $ASGSDIR/get_atcf.pl $OPTIONS 2>> advisoryMonitor.log`
   if [[ $advisoryNum != $oldAdvisoryNum ]]; then
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      echo "[${DATETIME}] INFO: New forecast advisory detected." >> advisoryMonitor.log
      oldAdvisoryNum=$advisoryNum
      cp $forecastFileNameXML ${forecastFileNameXML}.${DATETIME}
      cp $forecastFileNameHTML ${forecastFileNameHTML}.${DATETIME}
   fi
   curl http://ftp.nhc.noaa.gov/atcf/btk/$hindcastFileName > $hindcastFileName
   bestEndDate=`tail -n 1 $hindcastFileName | awk '{ print $3 }' | sed -e "s/,//"`
   if [[ $oldBESTEndDate = 0 ]]; then
      oldBESTEndDate=$bestEndDate
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      echo "[${DATETIME}] INFO: Final BEST track line date/time initialized to $oldBESTEndDate." >> advisoryMonitor.log
   fi
   if [[ $bestEndDate -gt $oldBESTEndDate ]]; then
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      echo "[${DATETIME}] INFO: New BEST track data detected." >> advisoryMonitor.log
      cp $hindcastFileName ${hindcastFileName}.${DATETIME}
      oldBESTEndDate=$bestEndDate
   fi
   # sleep for the specified length of time
   sleep `echo "$INTERVALMINUTES*60" | bc`
done

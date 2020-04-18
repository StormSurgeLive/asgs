#!/bin/bash
#
# advisoryMonitor.sh: Downloads hindcast and forecast data at configurable
# interval to trace how the NHC posts data over time. 
#

# Required, parameters $1-$4
INTERVALMINUTES=$1
STORMNUMBER=$2
YEAR=$3
ASGSDIR=$4

# Defaults are set for parameters $5-$9: FTPSITE, RSSSITE, TRIGGER. FDIR, HDIR (probably should go with getopts for more)
FTPSITE=${5-ftp.nhc.noaa.gov}         # hindcast/nowcast ATCF formatted files
RSSSITE=${6-www.nhc.noaa.gov}         # site information for retrieving advisories
TRIGGER=${7-rssembedded}              # ftp, rss, rssembedded (default)
FDIR=${8-/atcf/afst}                  # forecast dir on nhc ftp site
HDIR=${9-/atcf/btk}   

### No configurable options are below this line ###

ADVISORY=0
forecastFileNameXML=index-at.xml
forecastFileNameHTML=al${STORMNUMBER}${YEAR}.fst.html
hindcastFileName=bal${STORMNUMBER}${YEAR}.dat
oldAdvisoryNum=0
oldBESTEndDate=0
OPTIONS="--storm $STORMNUMBER --year $YEAR --ftpsite $FTPSITE --fdir $FDIR --hdir $HDIR --rsssite $RSSSITE --trigger $TRIGGER --adv 0"
LOGFILE=./advisoryMonitor.log

# wrapper around mv to inform user that files they may have expected
# are no longer there
function renameFile()
{
  OLD=$1;
  NEW=$2; 
  if [ ! -e "$OLD" ]; then
    echo "[WARNING] Can't find \"$OLD\" for renaming - (current trigger: $TRIGGER)" | tee -a $LOGFILE
    return
  fi
  mv --verbose $OLD $NEW
}

while [[ 1 ]]; do
   echo "perl $ASGSDIR/get_atcf.pl $OPTIONS"
   advisoryNum=`perl $ASGSDIR/get_atcf.pl $OPTIONS 2>&1 | tee -a $LOGFILE`
   
   if [[ $advisoryNum != $oldAdvisoryNum ]]; then
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      echo "[${DATETIME}] INFO: New forecast advisory detected." | tee -a $LOGFILE
      oldAdvisoryNum=$advisoryNum
   else
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`    
      echo "[${DATETIME}] INFO: The advisory number has not changed." | tee -a $LOGFILE
   fi
   bestEndDate=`tail -n 1 $hindcastFileName | awk '{ print $3 }' | sed -e "s/,//"`
   if [[ $oldBESTEndDate = 0 ]]; then
      oldBESTEndDate=$bestEndDate
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      echo "[${DATETIME}] INFO: Final BEST track line date/time initialized to $oldBESTEndDate." | tee -a $LOGFILE
   fi
   if [[ $bestEndDate -gt $oldBESTEndDate ]]; then
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
      echo "[${DATETIME}] INFO: New BEST track data detected." | tee -a $LOGFILE
      oldBESTEndDate=$bestEndDate
   else
      DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`    
      echo "[${DATETIME}] INFO: The BEST track end time has not changed." | tee -a $LOGFILE
   fi
   # save the files themselves
   DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`  
   renameFile $forecastFileNameXML ${forecastFileNameXML}.${DATETIME}
   renameFile $forecastFileNameHTML ${forecastFileNameHTML}.${DATETIME}
   renameFile $hindcastFileName ${hindcastFileName}.${DATETIME}
   # sleep for the specified length of time
   sleep `echo "$INTERVALMINUTES*60" | bc`
done


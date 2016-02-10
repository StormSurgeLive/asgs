#!/bin/bash
basin=al
stormNumber=01
year=2014
startAdvisory=10
endAdvisory=19
#
bestTrackFile=b${basin}${stormNumber}${year}.dat
advisory=$startAdvisory
while [ $advisory -le $endAdvisory ] ; do
   if [[ -e index-at.xml || -h index-at.xml ]] ; then
      rm index-at.xml
   fi
   if [[ -e $bestTrackFile || -h $bestTrackFile ]] ; then
      rm ${bestTrackFile}
   fi
   format="%02d"
   advisoryString=`printf "$format" $advisory`
   ln -s /projects/ncfs/data/asgs19420/${advisoryString}/${bestTrackFile} .
   advisoryString=`printf "$format" $((advisory++))`
   ln -s /projects/ncfs/data/asgs19420/${advisoryString}/index-at.xml .
   sleep 10800
done

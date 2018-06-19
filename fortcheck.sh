#!/usr/bin/env bash
#
# fortcheck.sh - a fast pct-complete evaluator for ADCIRC/ASGS output
# Brian Blanton - RENCI
# May 2018, for ASGS OAD 
#

HSTIME="hstime"
NCKS="ncks"

##NCDUMP="ncdump"

if [ "$#" -ne 1 ] ; then
   f='fort.61.nc'
else
   f=$1
fi
if [[ ! -e $f ]] ; then
        echo "$f DNE."
        exit 1
fi

#echo "Interrogating $f"
htime=0
ihot=$($NCKS -x $f |grep 'ihot' | awk '{print $3}') 
#echo "ihot=$ihot"
if [ "$ihot" -gt 0  ] ; then
        if   [[ "$ihot" -eq 567 || "$ihot" -eq 367 ]]  ; then
                fht="-n -f fort.67.nc"
        elif [[ "$ihot" -eq 568 || "$ihot" -eq 368 ]] ; then
                fht="-n -f fort.68.nc"
        elif [ "$ihot" -eq 67 ] ; then
                fht="-f fort.67"
        elif [ "$ihot" -eq 68 ] ; then
                fht="-f fort.68"
        fi
        htime=$($HSTIME $fht)
fi
htime=$(echo "scale=4;$htime/86400" | bc )
#echo "htime=$htime"

rnday=$($NCKS -x $f | grep -i 'rnday' | awk '{print $3}')
rnday=$(echo "scale=4;$rnday-$htime" | bc )
#echo "rnday=$rnday"

#t1=$($NCDUMP -v time  $f | grep -m 2  'time ='   |sed 1d  | awk '{print $3}' | sed s/,//g )
t1=$($NCKS  -V -u -H --variable time --dimension time,0 $f | sed '/time = [0-9]/!d' | awk '{print $3}')
#echo "t1=$t1"
#t2=$($NCDUMP -v time  $f | grep -m 2  'time ='   |sed 1d  | awk '{print $4}' | sed s/,//g )
t2=$($NCKS  -V -u -H --variable time --dimension time,1 $f | sed '/time = [0-9]/!d' | awk '{print $3}')
#echo "t2=$t2"
dt=$(echo $t2-$t1 | bc)
#nt=$($NCDUMP -v time  $f | grep -m 1  'time ='  | awk '{print $6}'  | sed s/\(//)
nt=$($NCKS -V -u  --variable time $f | sed '/UNLIMITED/!d' | awk '{print $6}' | sed s/\(//)
tt=$(echo $rnday*86400/$dt | bc)
pct=$(echo "scale=2;100*$nt/$tt" | bc)
echo $pct
exit 0

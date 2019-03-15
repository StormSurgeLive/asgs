#!/usr/bin/env bash
#
# fortcheck.sh - a fast pct-complete evaluator for ADCIRC/ASGS output
# Brian Blanton - RENCI
# May 2018, for ASGS OAD 
#

HSTIME="hstime"
NCKS="ncks"
##NCDUMP="ncdump"

if ! [ -x "$(command -v $NCKS)" ]; then
  echo "Error: $NCKS was not found." >&2
  exit 1
fi
if ! [ -x "$(command -v $HSTIME)" ]; then
  echo "Error: $HSTIME was not found." >&2
  exit 1
fi

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
#ihot=$($NCKS -x $f |grep 'ihot' | awk '{print $3}') 
#echo "ihot=$ihot"
#if [ "$ihot" -gt 0  ] ; then
#        if   [[ "$ihot" -eq 567 || "$ihot" -eq 367 ]]  ; then
#                fh="fort.67.nc"
#                fht="-n -f $fh"
#        elif [[ "$ihot" -eq 568 || "$ihot" -eq 368 ]] ; then
#                fh="fort.68.nc"
#                fht="-n -f $fh"
#        elif [ "$ihot" -eq 67 ] ; then
#                fh="fort.67"
#                fht="-f  $fh"
#        elif [ "$ihot" -eq 68 ] ; then
#                fh="fort.68"
#                fht="-f $fh" 
#        fi
#        if [ ! -e $fh ]; then 
#                echo "Hotstart indicated (ihot=$ihot), but $fh not found. Setting htime to 0. Pct Complete likely to be incorrect."
#        else
#                htime=$($HSTIME $fht)
#        fi
#fi
htime=$(echo "scale=4;$htime/86400" | bc )
#echo "htime=$htime"

rnday=$($NCKS -x $f | grep -i 'rnday' | awk '{print $3}')
rnday=$(echo "scale=4;$rnday-$htime" | bc )
#echo "rnday=$rnday"

# get first time value
t1=$($NCKS -V -u -H --variable time --dimension time,0 $f)  >/dev/null 2>&1 
if [ $? -ne 0 ] ; then
        echo "$NCKS failed to get first time (0) from $f. time might be 0-length."
        exit 
fi
t1=$(echo "$t1" | sed '/time = [0-9]/!d' | awk '{print $3}')
t1=$(echo "scale=6;$t1/86400" | bc)
#echo "t1=$t1"

# get second time value
t2=$($NCKS -V -u -H --variable time --dimension time,1 $f )  >/dev/null 2>&1
if [ $? -ne 0 ] ; then
        echo "$NCKS failed to get second time from $f. time might be 0-length."
        exit 
fi
t2=$(echo "$t2" | sed '/time = [0-9]/!d' | awk '{print $3}')
t2=$(echo "scale=6;$t2/86400" | bc)
#echo "t2=$t2"

dt=$(echo $t2-$t1 | bc)
#echo "dt=$dt"

nt=$($NCKS -V -u  --variable time $f | sed '/UNLIMITED/!d' | awk '{print $6}' | sed s/\(//)
#echo "nt=$nt"
nt=$(echo $nt-1 | bc)


# get last time value
tend=$($NCKS -V -u -H --variable time --dimension time,$nt $f )  >/dev/null 2>&1
if [ $? -ne 0 ] ; then
        echo "$NCKS failed to get last time dimension from $f. time might be 0-length."
        exit 
fi
tend=$(echo "$tend" | sed '/time = [0-9]/!d' | awk '{print $3}')
tend=$(echo "scale=6;$tend/86400" | bc)
#echo "tend=$tend"

rlen=$(echo "scale=6;$rnday-$t1" | bc)
#echo "rlen=$rlen"
pct=$(echo "scale=6;$tend-$t1" | bc)
#echo "pct=$pct"
pct=$(echo "scale=6;100*$pct/$rlen" | bc) 
#echo "pct=$pct"

#exit

#tt=$(echo $rnday*86400/$dt | bc)
#echo "tt=$tt"
#pct=$(echo "scale=2;100*$nt/$tt" | bc)
echo $pct
exit 0

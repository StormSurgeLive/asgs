#!/bin/bash
STORM=$1
YEAR=$2
STORMNAME=$3
METHOD=$4
FNAME=$5     # input file name; "default" for best track filename
#
PERL5LIB=~/asgs/trunk/PERL
export PERL5LIB
SCRIPTDIR=~/asgs/trunk
ADCIRCDIR=~/adcirc/newvortex/work
OPTIONS=""
#
DEFAULTNAME=bal${STORM}${YEAR}.dat
if [[ $FNAME = default ]]; then
    FNAME=$DEFAULTNAME
fi
# courtneyknaff2009 use storm translation speed so we need to run
# the file through aswip first to calculate this
if [[ $METHOD = "courtneyknaff2009" ]]; then
   # generate forecast increments ... output file name is same
   # as input file name but prepended with "fcst_"
   perl ${SCRIPTDIR}/doc/best2fcst.pl --input $FNAME
   # run aswip to get the storm translation velocity ... output file
   # is automatically named NWS_19_fort.22
   ${ADCIRCDIR}/aswip -m 2 -w fcst_${FNAME}
   # rename file to reflect the storm and year
   if [[ -e $DEFAULTNAME ]]; then
      mv $DEFAULTNAME ${DEFAULTNAME}.orig
   fi
   mv NWS_19_fort.22 $DEFAULTNAME
   OPTIONS="$OPTIONS --trackfiletype nws19"
fi
#
OPTIONS="$OPTIONS --method $METHOD --storm $STORM --year $YEAR --forecastlength 120 --trackfile $FNAME"
perl ${SCRIPTDIR}/doc/pressure_predict.pl $OPTIONS
gnuplot ${DEFAULTNAME}_${METHOD}.gp
for file in `ls pcf_all_${DEFAULTNAME}_${METHOD}_???.ps`; do
   convert -rotate 90 $file `basename $file ps`jpg
done
rm p_${STORMNAME}.mp4
ffmpeg -r 5 -i pcf_all_${DEFAULTNAME}_${METHOD}_%03d.jpg p_${STORMNAME}_${METHOD}.mp4
rm *.jpg *.ps

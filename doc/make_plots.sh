#!/bin/sh
STORM=$1
YEAR=$2
STORMNAME=$3
METHOD=$4
#
PERL5LIB=~/asgs/trunk/PERL
#
perl pressure_predict.pl --method $METHOD --storm $STORM --year $YEAR --forecastlength 120
gnuplot bal${STORM}${YEAR}.dat_${METHOD}.gp
for file in `ls pcf_all_bal${STORM}${YEAR}.dat_${METHOD}_???.ps`; do 
   convert -rotate 90 $file `basename $file ps`jpg 
done
rm p_${STORMNAME}.mp4
ffmpeg -r 5 -i pcf_all_bal${STORM}${YEAR}.dat_${METHOD}_%03d.jpg p_${STORMNAME}_${METHOD}.mp4
rm *.jpg *.ps

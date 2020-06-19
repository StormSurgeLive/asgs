#!/bin/bash


INPUT=$1
OUTPUT=$2
IFS=,

[ -f $OUTPUT ] && rm $OUTPUT

[ ! -f $INPUT ] && { echo "$INPUT file not found"; exit 99; }
while read f1 f2 f2 f3 f4 f5 f6 f7 f8 f9
do
   slat=${f6%?}
   slon=${f7%?}

   lat="$(echo $slat/10|bc -l)"
   lon="$(echo $slon/10|bc -l)"
   lon="$(echo $lon*-1|bc -l)"

   echo $(printf "%0.2f\t%0.2f\n" $lon $lat) >> $OUTPUT

done < $INPUT


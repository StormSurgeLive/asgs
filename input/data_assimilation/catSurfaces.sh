#!/bin/bash
adv=17; 
while [[ $adv -lt 48 ]]; do 
   advStr=`printf '%02d' $adv` 
   awk 'NR>3 { print $0 }' $adv.end.surface.dat >> stepped_offset.dat
   echo '##' >> stepped_offset.dat
   adv=`expr $adv + 1`
done

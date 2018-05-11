#!/bin/bash
adv=1; 
while [[ $adv -lt 48 ]]; do 
   advStr=`printf '%02d' $adv` 
   if [[ $adv -eq 1 ]]; then
      prevStr=init
   else
      prevAdv=`expr $adv - 1`
      prevStr=`printf %02d $prevAdv`
   fi
   filename=$advStr.nowcast.surface.dat
   echo "!@jasonfleming: nowcast surfaces matthew advisory $advStr" > $filename
   echo "21600.0  ! time interval (s)" >> $filename
   echo "0.0      ! default value (m)" >> $filename
   awk 'NR>3 { print $0 }' $prevStr.end.surface.dat >> $filename
   echo "##" >> $filename
   awk 'NR>3 { print $0 }' $advStr.end.surface.dat >> $filename  
   echo "##" >> $filename
   adv=`expr $adv + 1`
done

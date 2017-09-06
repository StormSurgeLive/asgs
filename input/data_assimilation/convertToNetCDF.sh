#!/bin/bash
adv=1; 
while [[ $adv -lt 2 ]]; do 
   advStr=`printf '%02d' $adv` 
   ~/asgs/2014stable/output/adcirc2netcdf.x --meshfile /projects/ncfs/data/input/hsofs.14 --defaultfilename fort.63 --datafile $advStr.end.surface.63 
   ~/asgs/2014stable/output/generateXDMF.x --datafile $advStr.end.surface.63.nc
   adv=`expr $adv + 1`
done

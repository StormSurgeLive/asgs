#!/bin/bash
#
PERL5LIB=~/asgs/trunk/PERL ; export PERL5LIB
for file in `ls bal*.dat`; do
   perl ~/asgs/trunk/doc/best2fcst.pl --input $file
   # generate the animation
   ~/asgs/trunk/output/viz_driver.sh fcst_${file} hind 3600.0 0
   # change the name of the final animation to reflect the storm
   # number and year
   mv anim_${TIMESTEP}_all.mp4 anim_${file}_${TIMESTEP}.mp4
done

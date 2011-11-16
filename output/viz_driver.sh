#!/bin/sh
ADCIRCDIR=~/adcirc/trunk_newvortex/work
PERL5LIB=~/asgs/trunk/PERL ; export PERL5LIB
ADVISORY=025
TIMESTEP=default # "default" to hit the cycle times
FRAME=0          # "0" for all frames
MYPROC=0         # "0" in serial
#
DELAY=10
if [ $TIMESTEP = default ]; then
   DELAY=100
fi
# 
# clean up pre-existing data and image files, if any
rm -f fort.22 radii_???.ps radialv_???.ps radialp_???.ps page???.gif page???.ps pages???.ps spatial_data_???.d spatial_cycle???.gif montage_geom_???.gif spatial_data_???.d full_circle_rmaxes_???.d radii_??kt_cycle???.d radialvp_latlon_???.d full_circle_latlon_???.d spatial_data_???.d *.png compass_full_circle_rmaxes_???.d radialvp_???.d montage_geom_fullsize_???.gif montage_geom_???.gif
#
# make a link to the fort.22 that we are interested in
ln -s fort.22.orig.irene.${ADVISORY} fort.22
#
# generate a preprocessed output file
$ADCIRCDIR/aswip -m 1
#
# TODO: for parallel execution:
# add code that can count up the frames, make subdirs for the
# subprocesses to run in (to avoid collisions between GMT history subdirs),
# count up how many and which frames will be rendered by which processors
# and eliminate any other collisions between subprocesses (all avoided if
# executing in subdirs?)
#
# execute frame driver in serial for all frames
./frame_driver.sh $TIMESTEP $ADVISORY $FRAME $MYPROC $ADCIRCDIR $PERL5LIB
#
# make animation out of the montage sequence of images
convert -loop 0 -delay $DELAY montage_geom_???.gif anim_all_${ADVISORY}.gif

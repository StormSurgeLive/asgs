#!/bin/bash
TIMESTEP=$1  # "default" to look at each forecast increment ; (sec) otherwise
ADVISORY=$2  # set to 025, 030, 035, Hindcast, etc
FRAME=$3     # "all" for whole fort.22; otherwise set to frame of interest
MYPROC=$4    # processor number in parallel (0 in serial)
ADCIRCDIR=$5 # directory where adcirc executables are located
PERL5LIB=$6
#
export PERL5LIB
ASWIPTIMESTEP=""
VORTVIZTIMESTEP=""
ASWIPFRAMEOPT=""
VORTVIZFRAMEOPT=""
#
if [ $TIMESTEP != "default" ]; then
   ASWIPTIMESTEP="-o $TIMESTEP"
   VORTVIZTIMESTEP="--outputincrement $TIMESTEP"
fi
if [ $FRAME != "0" ]; then
   ASWIPFRAMEOPT="-f $FRAME"
   VORTVIZFRAMEOPT="--frame $FRAME"
fi
#
# write full circle Rmaxes data
$ADCIRCDIR/aswip -m 1 -a $ASWIPTIMESTEP $ASWIPFRAMEOPT
# write radial v and p
$ADCIRCDIR/aswip -m 1 -v $ASWIPTIMESTEP $ASWIPFRAMEOPT
# generate shell script(s) that will create maps in gmt
perl vortex_viz_gen.pl --advisorynum $ADVISORY $VORTVIZTIMESTEP $VORTVIZFRAMEOPT
# execute shell script to create figures with gmt
chmod +x plot_radii.sh
./plot_radii.sh
# execute gnuplot script to create line graphs with gmt
gnuplot radial_v_and_p.gp
#
# create 3D viz of surface wind speeds
$ADCIRCDIR/aswip -s $ASWIPTIMESTEP $ASWIPFRAMEOPT # write spatial v and p
files=""
if [ $FRAME = "0" ]; then
   files=`ls spatial_data_???.d`
else
   files=`printf "spatial_data_%03d.d" $FRAME`
fi
for file in $files; do
   num=`expr substr $file 14 3`
   python unstreamline.py $num 
done
#
# make a montage of the radii, v(r) plot, p(r) plot, and 3D viz
files=""
if [ $FRAME = "0" ]; then
   files=`ls radii_???.ps`
else
   files=`printf "radii_%03d.ps" $FRAME`
fi
for file in $files; do
   num=`expr substr $file 7 3`
   convert -rotate 90 radii_${num}.ps radii_${num}.png
   convert -rotate 90 radialv_${num}.ps radialv_${num}.png
   convert -rotate 90 radialp_${num}.ps radialp_${num}.png
   montage radii_${num}.png spatial_data_${num}.png radialv_${num}.png radialp_${num}.png -geometry +2+2 montage_geom_fullsize_${num}.gif
   convert -resize 50% montage_geom_fullsize_${num}.gif montage_geom_${num}.gif
done

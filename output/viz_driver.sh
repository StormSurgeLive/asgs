#!/bin/sh
INPUT=$1     # fort.22 file used as input
ADVISORY=$2  # used in file names and plot titles
TIMESTEP=$3  # seconds, e.g., 1800.0 # "default" to hit the cycle times
MESHFILE=$4  # fort.14 file name or "auto" to autogenerate mesh
DESCRIPT="$5"  # very short description for the streamlines viz
PLOTMAX=$6   # max in wind speed range for plots, or "auto" to autoscale
NWS=$7       # 15 or 19
RMAX=$8      # percent adjustment in Rmax (%), 0 to leave Rmax alone
VMAX=$9      # percent adjustment in Vmax (%), 0 to leave Vmax alone
TIMEOFFSET=${10} # for NWS15, time in seconds of start time of analysis
TIMEZONE=${11} # used in the timestamp on the animation; GMT, EDT, or CDT
#
ASGSDIR=~/asgs/trunk
ADCIRCDIR=~/adcirc/trunk/work
PERL5LIB=$ASGSDIR/PERL ; export PERL5LIB
#
FRAME=0         # "0" for all frames
MYPROC=0        # "0" in serial
#MESHFILE=~/adcirc/examples/parallel_ec95d/coldnetcdf/ec_95d.grd  # "auto" to autogenerate mesh
#
DELAY=10
if [ $TIMESTEP = default ]; then
   DELAY=100
fi
#
# clean up pre-existing data and image files, if any
rm -f  radii_???.?ps radialv_???.?ps radialp_???.?ps page???.gif page???.?ps pages???.?ps spatial_data_???.d spatial_cycle???.gif montage_geom_???.gif montage*.jpg spatial_data_???.d full_circle_rmaxes_???.d radii_??kt_cycle???.d radialvp_latlon_???.d full_circle_latlon_???.d spatial_data_???.d *.png compass_full_circle_rmaxes_???.d radialvp_???.d montage_geom_fullsize_???.gif montage_geom_???.gif
#
# generate a preprocessed output file
if [ $NWS = 19 ]; then
   $ADCIRCDIR/aswip -n 19 -m 2 -w $INPUT -p $RMAX -x $VMAX
fi
#
# TODO: for parallel execution?
#
# execute frame driver in serial for all frames
$ASGSDIR/output/frame_driver.sh $NWS $INPUT $TIMESTEP $ADVISORY $FRAME $MYPROC $ADCIRCDIR $ASGSDIR $PERL5LIB $MESHFILE "$DESCRIPT" $PLOTMAX $RMAX $VMAX $TIMEOFFSET $TIMEZONE
#
# make animation out of the montage sequence of images
FRAMERATE=5
if [[ $TIMESTEP = default ]]; then
   FRAMERATE=1
fi
#convert -loop 0 -delay $DELAY montage_geom_???.gif anim_all_${ADVISORY}.gif
rm anim_${TIMESTEP}_all.mp4
ffmpeg -r $FRAMERATE -sameq -i montage_geom_%03d.jpg anim_${TIMESTEP}_all.mp4
# convert to H.264 with handbrake
HandBrakeCLI --input anim_${TIMESTEP}_all.mp4 --output anim_${TIMESTEP}_all_264.mp4 --encoder x264 --two-pass
# create ogv file 
ffmpeg2theora --videoquality 10 --output anim_${TIMESTEP}_all.ogv --seek-index --speedlevel 0 anim_${TIMESTEP}_all.mp4
# create webm file 
#ffmpeg -r 5 -sameq -i montage_geom_%03d.jpg anim_${TIMESTEP}_all.webm
#
# YouTube: use 1280x720 image size and the following command line; numbers in
# file name must be sequential starting from zero or ffmpeg will not be able
# to find them
#FRAMERATE=15; ffmpeg -r $FRAMERATE -sameq -i youtube_test.%04d.jpg -vcodec libx264 -preset slow -crf 18 -pix_fmt yuv420p  yt_vel_anim.mp4

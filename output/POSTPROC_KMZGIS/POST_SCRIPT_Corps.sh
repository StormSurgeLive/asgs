#!/bin/bash
#-----------------------------------------------------------------------
# Copyright(C) 2010 Jason Fleming
# Copyright(C) 2008, 2009 Robert J Weaver
#-----------------------------------------------------------------------
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#-----------------------------------------------------------------------
# Robert J Weaver
# UNC-CH IMS
# rjweaver@email.unc.edu
# 06/2009 
#
# POST_SCRIPT_Corps.sh: 
# This script will run the KMZ and GIS shapefile generator 
# and make a JPG of the maxele file.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ATTENTION OPERATORS: The following manual steps are required preparation:
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# (1)
# If your grid has not already been decomposed, you must do this first!
# There is a program in the $POSTPROC_DIR/grids folder called ./grd2gmt.x.
# You should compile this with your local fortran compiler.
# Here is an example of how to run it for our purposes:
#  ./grd2gmt.x sl15v3_2007_r10.grd sl15v3_2007_r10
# This will produce 4 files: 
# sl15v3_2007_r10.gmt.bnd.xy
# sl15v3_2007_r10.gmt.tri
# sl15v3_2007_r10.gmt.xy
# sl15v3_2007_r10.gmt.xyz
# 
# (2)
# The postprocessing for KMZ, GIS, and JPG depends on the configuration
# setting in $PPDIR/config_simple_gmt_pp.sh. This is the main configuration
# file for the KMZ scripts.
#
# It contains settings for the computer on which you are running (TARGET),
# and the bounding box for your output (BOX). The TARGET variable is normally
# set in the configuration file for the ASGS (in the post processing section).
# (a) Please set TARGET in your main configuration file for the ASGS.
#
# (b) In $PPDIR/config_simple_gmt_pp.sh: For TARGET, be sure to double
# check the paths to the indicated software packages. These are all required
# so if they do not currently exist, you will need to install them. If you
# do not specify the TARGET, "topsail" will be used.
# 
# (c) Please be sure that BOX is passed as an argument to this script. In 
# $PPDIR/config_simple_gmt_pp.sh: For BOX, if you do not see your region
# of your interest, then simply add your region using the existing region
# definitions as a template. If you do not specify BOX, "NC" will be used.
#
# (d) There is a C program called FindMax that is used to find the maxes
# and mins of a fort.63 during the generation of a KMZ file. It should be
# recompiled on your platform; instructions for compiling it with gcc are
# found at the top of the source code.
#
# (e) There is a C program called WriteTiledKML that is used during the 
# generation of KMZ files. It should be recompiled on your platform;
# instructions for compiling it with gcc are found at the top of the
# source code.
#
# (f) There is a Fortran program called splitFort63.f that is used to break
# up ADCIRC fort.63 files that contain multiple output datasets into individual
# datasets for use in making animations. It should be recompiled on your 
# platform. TODO: This program contains several obvious bugs and shortcomings
# that must be corrected.
#  
# (3)
# The JPG generation uses FigureGen, which should be compiled by your local
# fortran compiler. (a) Please compile FigureGen locally; the source code is
# in $POSTPROC_DIR/FigGen/FigureGen32.F90.
# (b) Please be sure to specify the name FigureGen executable you'd like to 
# use, as FIGUREGENEXECUTABLE. The default is "FigureGen32_prompt_inp.exe". 
#
# (4) 
# The FigureGen template input file for creating JPG files should be passed
# as an argument to this script as FIGUREGENTEMPLATE. The default is 
# $OUTPUTDIR/POSTPROC_KMZGIS/FigGen/FG_asgs.inp.orig.
# 
ADVISDIR=$1
OUTPUTDIR=$2
STORM=$3
YEAR=$4
ADVISORY=$5
HOSTNAME=$6
ENSTORM=$7
GRIDFILE=$8 
CONFIG=$9
BOX=${10}
FIGUREGENEXECUTABLE=${11}
FIGUREGENTEMPLATE=${12}
#
# GENERAL SET UP
#
. ${CONFIG} # grab all static config info
cd $ADVISDIR/$ENSTORM
#
# set path to the POSTPROC_KMZGIS directory
POSTPROC_DIR=$OUTPUTDIR/POSTPROC_KMZGIS
export GRDFILES=$POSTPROC_DIR/grids
#
# OUTPUTFILE : set location of ADCIRC output file to visualize
OUTPUTFILE=$ADVISDIR/$ENSTORM/maxele.63
#
# GRIDPREFIX : identify which grid set will be used
if [[ ${GRIDFILE:(-3)} = .14 ]]; then
   GRIDPREFIX=`basename $GRIDFILE .14`
fi
if [[ ${GRIDFILE:(-4)} = .grd ]]; then
   GRIDPREFIX=`basename $GRIDFILE .grd`
fi
#
# OUTPUTPREFIX : set output filename prefix
OUTPUTPREFIX=${STORM}_${YEAR}_${ENSTORM}_${ADVISORY}
# #####################################################
#
#           K M Z
#
# add location of KMZ scripts to PATH 
export PPDIR=$POSTPROC_DIR/RenciGETools-1.0/src
#
# NUMLAYER : number of layers to the KMZ file; for street level
# resolution we recommend 4 layers.
# On topsail.unc.edu 4 layers for a 270000 node grid took 14 minutes.
# To test the script, use only 1 layer.
NUMLAYER=3
#
# log file for kmz info and errors
KMZLOGFILE=$ADVISDIR/$ENSTORM/kmz.log
#
# create track line and points if available
perl $PPDIR/make_track_files.pl >> $KMZLOGFILE 2>&1
date >> $KMZLOGFILE
#
# Now that we have set up the configuration variables, execute the script
# to actually generate the kmz (Google Earth) file. TARGET and BOX should
# be specified in the CONFIG file specified above (i.e., the main ASGS
# configuration file). 
echo "INFO: POST_SCRIPT_Corps.sh: Generating Google Earth image with the following command: $PPDIR/adc_max_simple_plot_gmt.sh -f $OUTPUTFILE -g $GRIDPREFIX -p $OUTPUTPREFIX -n $NUMLAYER -a $TARGET -b $BOX >> $KMZLOGFILE 2>&1."  
$PPDIR/adc_max_simple_plot_gmt.sh -f $OUTPUTFILE -g $GRIDPREFIX -p $OUTPUTPREFIX -n $NUMLAYER -a $TARGET -b $BOX >> $KMZLOGFILE 2>&1  
date >> $KMZLOGFILE
# #####################################################
#
# .. GIS ..
# 
# For the GIS shapefiles we need the full path name of the grid that you
# are running and we will use the same file as above for the input file 
# (you of course can change this as you desire)
# The program name for the script is actoshape

# Excecute config file to get bounds for output box (the script sets
# NORTH SOUTH EAST and WEST based on the BOX argument) and location (Path)
# to grid files
source $PPDIR/config_simple_gmt_pp.sh $TARGET $BOX
echo 
if [[ -e $GRDFILES/$GRIDPREFIX.grd ]]; then
   GRID=$GRDFILES/$GRIDPREFIX.grd
elif [[ -e $GRDFILES/$GRIDPREFIX.14 ]]; then
   GRID=$GRDFILES/$GRIDPREFIX.14
else
   echo "ERROR: POST_SCRIPT_Corps.sh: Cannot find $GRDFILES/$GRIDPREFIX.grd or $GRDFILES/$GRIDPREFIX.14."
   exit 1
fi
GISLOGFILE=$ADVISDIR/$ENSTORM/gis.log # log file for gis-related info/errors
date >> $GISLOGFILE
echo "INFO: POST_SCRIPT_Corps.sh: Starting Java script ArcGIS POST PROC", $WEST, $SOUTH, $EAST, $NORTH
echo $GRID, $OUTPUTFILE, $OUTPUTPREFIX.shp
#
# Run actoshape to create GIS shape file from ADCIRC output.
java -Xmx1500M -jar $POSTPROC_DIR/actoshape/actoshape.jar \
 --box $WEST $SOUTH $EAST $NORTH \
 --clipcoast \
 $GRID \
 $OUTPUTFILE \
 $OUTPUTPREFIX.shp >> $GISLOGFILE 2>&1  
date >> $GISLOGFILE

# #####################################################
#
# .. JPG ..
# Use the same BOX as above
#
#  Usage Example:
#   perl make_JPG.pl --storm 01 --year 2006 --adv 05 --n 30.5 --s 28.5 --e -88.5 --w -90.5 --outputprefix 01_2006_nhcconsensus_05
#
JPGLOGFILE=$ADVISDIR/$ENSTORM/jpg.log # log file for jpg-related info/errors
date >> $JPGLOGFILE
# 
# use default FigureGen executable if it has not been specified
if [[ $FIGUREGENEXECUTABLE = "" ]]; then
   FIGUREGENEXECUTABLE=FigureGen32_prompt_inp.exe
fi
#
# Use the default FigureGen template if it has not been specified
if [[ $FIGUREGENTEMPLATE = "" ]]; then
   FIGUREGENTEMPLATE=$OUTPUTDIR/POSTPROC_KMZGIS/FigGen/FG_asgs.inp.orig
fi
#
# create a temp directory for FigureGen to use
mkdir Temp 2>> $JPGLOGFILE
# copy the color palette to the ensemble directory
cp $POSTPROC_DIR/FigGen/Default2.pal $ADVISDIR/$ENSTORM 2>> $JPGLOGFILE
# copy the logo to the ensemble directory
cp $POSTPROC_DIR/FigGen/adcirc_logo_white.eps $ADVISDIR/$ENSTORM 2>> $JPGLOGFILE
#
# Fill in FigureGen template file to create an input file for this plot.
perl ${POSTPROC_DIR}/FigGen/make_JPG.pl --figuregen_template $FIGUREGENTEMPLATE --gmthome $GMTHOME --storm ${STORM} --year ${YEAR} --adv $ADVISORY --n $NORTH --s $SOUTH --e $EAST --w $WEST --outputprefix $OUTPUTPREFIX 2>> $JPGLOGFILE 2>&1
#
# Now execute FigureGen to create the jpg file.
echo "INFO: POST_SCRIPT_Corps.sh: Executing the command $POSTPROC_DIR/FigGen/${FIGUREGENEXECUTABLE} -I FigGen_${OUTPUTPREFIX}.inp >> $JPGLOGFILE 2>&1 to create the JPG file." 
# now call FigureGen and feed in the input file
$POSTPROC_DIR/FigGen/${FIGUREGENEXECUTABLE} -I FigGen_${OUTPUTPREFIX}.inp >> $JPGLOGFILE 2>&1 
#
# Clean up the FigureGen temp directory. 
rm -rf Temp 2>> $JPGLOGFILE
date >> $JPGLOGFILE
#        
# #####################################################
#
# .. PACKAGING .. KMZ GIS and JPG
#
mkdir $OUTPUTPREFIX-KMZ_GIS_files 
mv $OUTPUTPREFIX.* $OUTPUTPREFIX-KMZ_GIS_files
tar -czf $OUTPUTPREFIX-KMZ_GIS.tar.gz $OUTPUTPREFIX-KMZ_GIS_files
rm -rf $OUTPUTPREFIX-KMZ_GIS_files $OUTPUTPREFIX*.png
###
exit 0

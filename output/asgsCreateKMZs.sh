#!/bin/bash -x
# 
# Copyright (c) 2008  Renaissance Computing Institute. All rights reserved.
# 
# This software is open source. See the bottom of this file for the license.
# 
# Renaissance Computing Institute, 
# (A Joint Institute between the University of North Carolina at Chapel Hill,
# North Carolina State University, and Duke University)
# http://www.renci.org
# 
# For questions, comments please contact software@renci.org
# 
# 
# This is expecting to be called in the run directory.
# Note that there is a lot of hard coded stuff in this script:
# If we change any of the grids or the name of any of the input/output
# files, we will need to change this script accordingly.

#
#                       U S A G E
# 
function Usage
{
   echo "Usage: $0 -w windGrid -a adcircGrid [-t] [-s] [-b] [-c] [-e]"
   echo ""
   echo "This is expecting to be called in the top level results directory."
   echo ""
   echo "     -w: The grid for the wind field."
   echo "     -a: The adcirc grid."
   echo "     -e: Process the wind files."
   echo "     -c: Process adcirc including the swan_HS.63 file."
}
#
#             S E T   B O U N D I N G   B O X 
#
# Take in the name of a precompute file and set the bounding box
function setBoundingBox {
  west=`head -1 $1 | awk '{print $1}'`
  north=`head -1 $1 | awk '{print $2}'`
  width=`head -1 $1 | awk '{print $4}'`
  south=`echo "$north-$width" | bc`
  east=`echo "$west+$width" | bc`
 }

#
#     P R O C E S S   W I N D   F I L E S
#
function processWindFiles {

   # The wind files, if any are in the adcirc.0 directory
   cd $baseDir/adcirc.0

   # Process the fort.222. There is only one, so we don't have to do this for the
   # adcirc.1 directory.
   # Now let's unzip the fort.222 file (if needed)
   if [ -e fort.222.gz ] ; then
      gunzip fort.222
   fi

   if [ -e fort.222 ] ; then
      # Convert the fort.222 to a single dimensional file.
      # Note that we are also producing the direction file, though we are not yet using it.
      $PPDIR/convertWindToSpeed fort.222 $TRACKNAME-speed.221  $TRACKNAME-dir.221

     # Convert the wind speed to NetFEMCDF
     $NETFEMDIR/src/OWItoNetFEMCDF $TRACKNAME-speed.221 $windTimeStep $TRACKNAME  $NETFEMDIR/elements/$windGrid.ele speed wind_speed meters/second

      # Get the number of steps:
      nSteps=`$PPDIR/ReadADimension --gridFile=$TRACKNAME-speed.221.nc --dimension=time`

      # make a "max version" of this file
      ncra --overwrite -y max $TRACKNAME-speed.221.nc $TRACKNAME-speed.221-max.nc

      # set up the bounding box from the precompute file
      setBoundingBox $PREPROCESSBASE/owi/$windGrid/$windGrid.1.1.1.pre

      # Build the wind speed movie
      $PPDIR/MakeGEMovieFromJava.pl --dataFile=$TRACKNAME-speed.221.nc --fort63Mode=netCDFScoop --prefix=$TRACKNAME-wind-speed --nLevels=2 --startDate=$adcOStartTime --firstStep=1 --nSteps=$nSteps --stepInterval=1 --varName=speed --legend="Speed"  --precomputeDir=$PREPROCESSBASE/owi/$windGrid --precomputePrefix=$windGrid --outputType=p --outputDir=graphics --west=$west --north=$north --south=$south --east=$east --timeStep=$windTimeStepSecs

      # Build the wind speed max
      $PPDIR/MakeGEMovieFromJava.pl --dataFile=$TRACKNAME-speed.221-max.nc --fort63Mode=netCDFScoop --prefix=$TRACKNAME-wind-speed-max --nLevels=1 --startDate=$adcOStartTime --firstStep=1 --nSteps=1 --stepInterval=1 --varName=speed --legend="MaxSpeed"  --precomputeDir=$PREPROCESSBASE/owi/$windGrid --precomputePrefix=$windGrid --outputType=p --outputDir=graphics --west=$west --north=$north --south=$south --east=$east --timeStep=$windTimeStepSecs
      gzip --fast fort.222
   fi

   # Process the fort.221. There is only one, so we don't have to do this for the
   # adcirc.1 directory.
   # Now let's unzip the fort.221 file (if needed)
   if [ -e fort.221.gz ] ; then
      gunzip fort.221
   fi

   if [ -e fort.221 ] ; then
      # Convert the wind speed to NetFEMCDF
      $NETFEMDIR/src/OWItoNetFEMCDF fort.221 $windTimeStep $TRACKNAME  $NETFEMDIR/elements/$windGrid.ele pressure pressure mb

       # Get the number of steps:
       nSteps=`$PPDIR/ReadADimension --gridFile=fort.221.nc --dimension=time`

       # make a "min version" of this file
       ncra --overwrite -y min fort.221.nc fort.221-min.nc

       # set up the bounding box from the precompute file
       setBoundingBox $PREPROCESSBASE/owi/$windGrid/$windGrid.1.1.1.pre

       # Build the pressure movie
       $PPDIR/MakeGEMovieFromJava.pl --dataFile=fort.221.nc -fort63Mode=netCDFScoop --prefix=$TRACKNAME-pressure --nLevels=2 --startDate=$adcOStartTime --firstStep=1 --nSteps=$nSteps --stepInterval=1 --varName=pressure --legend="Pressure" --precomputeDir=$PREPROCESSBASE/owi/$windGrid --precomputePrefix=$windGrid --outputType=p --outputDir=graphics --west=$west --north=$north --south=$south --east=$east --timeStep=$windTimeStepSecs --highClip=1100 --lowClip=900

       # Build the pressure max
       $PPDIR/MakeGEMovieFromJava.pl --dataFile=fort.221-min.nc --fort63Mode=netCDFScoop --prefix=$TRACKNAME-pressure-min --nLevels=1 --startDate=$adcOStartTime --firstStep=1 --nSteps=1 --stepInterval=1 --varName=pressure --legend="MaxPressure"  --precomputeDir=$PREPROCESSBASE/owi/$windGrid --precomputePrefix=$windGrid --outputType=p --outputDir=graphics --west=$west --north=$north --south=$south --east=$east --timeStep=$windTimeStepSecs --highClip=1100 --lowClip=900
       gzip --fast fort.221
    fi
}

#
#      P R O C E S S   A D C I R C   1
#
function processAdcirc1 {
   # Now for ADCIRC.1
   cd $baseDir
 
   # set up the bounding box from the precompute file
   setBoundingBox $PREPROCESSBASE/adcirc/$adcircGrid/$adcircGrid.1.1.1.pre

   # Check to see if there is a zipped netcdf file already
   if [ -e swan_HS.63.nc.gz ] ; then
      gunzip swan_HS.63.nc.gz
   fi

   # Now look for the unzipped NetCDF file, which may or may not have just been unzipped
   if [ ! -e swan_HS.63.nc ] ; then
      # No NetCDF file, let's try to make it from the model output

      # if fort.63 is gzipped, let's unzip it
      if [ -e "swan_HS.63.gz" ] ; then
         gunzip swan_HS.63
      fi

      if [ -e "swan_HS.63" ] ; then
         # convert the file to NetCDF
         $PPDIR/convert_adc_native_2_netCDF swan_HS.63
      fi
   fi

   if [ -e swan_HS.63.nc ] ; then
      # Get the number of steps:
      nSteps=`$PPDIR/ReadADimension --gridFile=swan_HS.63.nc --dimension=time`

      # Get the time step in seconds from the fort.63 file.
      adcStepSecs=`sed -n '3q;2p' swan_HS.63 | awk '{print $3}' | awk -F"E" 'BEGIN{OFMT="%10.10f"} {print $1 * (10 ^ $2)}'`

      # Build the surge  movie
      $PPDIR/MakeGEMovieFromJava.pl --dataFile=swan_HS.63.nc --fort63Mode=netCDFScoop --prefix=$TRACKNAME-HS --nLevels=2 --startDate=$adcStartTime --firstStep=1 --nSteps=$nSteps --stepInterval=1 --varName=zeta --legend="HS"  --highClip=40 --lowClip=-1 --precomputeDir=$PREPROCESSBASE/adcirc/$adcircGrid --precomputePrefix=$adcircGrid --outputType=p --outputDir=graphics --west=$west --north=$north --south=$south --east=$east --timeStep=$adcStepSecs --grid=$GRIDDIR/$adcircGrid.nc
      gzip swan_HS.63.nc
   fi

   # Check to see if there is a zipped netcdf file already
   if [ -e fort.63.nc.gz ] ; then
      gunzip fort.63.nc.gz
   fi

   # Now look for the unzipped NetCDF file, which may or may not have just been unzipped
   if [ ! -e fort.63.nc ] ; then
      # No NetCDF file, let's try to make it from the model output
      # if fort.63 is gzipped, let's unzip it
      if [ -e "fort.63.gz" ] ; then
         gunzip fort.63
      fi

      # convert the compact format fort.63 to a netCDF file
      if [ ! -e "fort.63.nc" ] ; then
         $PPDIR/convert_adc_native_2_netCDF fort.63
      fi
   fi

   if [ fort.63.nc ] ; then
      # Get the number of steps:
      nSteps=`$PPDIR/ReadADimension --gridFile=fort.63.nc --dimension=time`

      # Get the time step in seconds from the fort.63 file.
      adcStepSecs=`sed -n '3q;2p' fort.63 | awk '{print $3}' | awk -F"E" 'BEGIN{OFMT="%10.10f"} {print $1 * (10 ^ $2)}'`
 
      # Build the surge  movie
      $PPDIR/MakeGEMovieFromJava.pl --dataFile=fort.63.nc --fort63Mode=netCDFScoop --prefix=$TRACKNAME-adcirc --nLevels=2 --startDate=$adcStartTime --firstStep=1 --nSteps=$nSteps --stepInterval=1 --varName=zeta --legend="Zeta"  --highClip=30 --lowClip=-5 --precomputeDir=$PREPROCESSBASE/adcirc/$adcircGrid --precomputePrefix=$adcircGrid --outputType=p --outputDir=graphics --west=$west --north=$north --south=$south --east=$east --timeStep=$adcStepSecs --grid=$GRIDDIR/$adcircGrid.nc
      gzip fort.63.nc
   fi

   # Check to see if there is a zipped netcdf file already
   if [ -e maxele.63.nc.gz ] ; then
      gunzip maxele.63.nc.gz
   fi

   # Now look for the unzipped NetCDF file, which may or may not have just been unzipped
   if [ ! -e maxele.63.nc ] ; then
      # No NetCDF file, let's try to make it from the model output
      # if fort.63 is gzipped, let's unzip it
      if [ -e "maxele.63.gz" ] ; then
        gunzip maxele.63.gz
      fi

      if [ -e "maxele.63" ] ; then
         $PPDIR/convert_adc_native_2_netCDF maxele.63
      fi
   fi

   if [ -e maxele.63.nc ] ; then
      $PPDIR/MakeGEMovieFromJava.pl --dataFile=maxele.63.nc --fort63Mode=netCDFScoop --prefix=$TRACKNAME-adcirc-max --nLevels=5 --startDate=$adcStartTime --firstStep=1 --nSteps=1 --stepInterval=1 --varName=zeta --legend="MaxZeta"  --highClip=30 --lowClip=-5 --precomputeDir=$PREPROCESSBASE/adcirc/$adcircGrid --precomputePrefix=$adcircGrid --outputType=p --outputDir=graphics --west=$west --north=$north --south=$south --east=$east --timeStep=$adcStepSecs --grid=$GRIDDIR/$adcircGrid.nc
      gzip maxele.63.nc
   fi
}
#
#              M A I N 
#
. /shared/apps/setup.sh
module load gmt/4.5.6
module load java/1.6.0_14
module load nco/4.0.6-gcc4.1

# set some default values:
windGrid="extraTropical"
adcircGrid="nc_inundation_v6b_msl-inundation"
configFile="/shared/apps/software-data/RenciGETools/trunk/src/setupPostProcessGraphics.sh"
doWW3=0
doAdcirc1=0
doWinds=0

# By default this code is executed in the current directory.
# Can be overwritten with the -b flag.
baseDir=`pwd`
while getopts ":w:a:f:b:ce" optname
  do
    case "$optname" in
      "w")
        windGrid=$OPTARG
        ;;
      "a")
        adcircGrid=$OPTARG
        ;;
      "f")
        configFile=$OPTARG
        ;;
      "c")
        doAdcirc1=1
        ;;
      "e")
        doWinds=1
        ;;
      "b")
        baseDir=$OPTARG
        ;;
      "?")
        echo "Unknown option $OPTARG"
        Usage
        exit 1
        ;;
      ":")
        echo "No argument value for option $OPTARG"
        Usage
        exit 1
        ;;
      *)
      # Should not occur
        echo "Unknown error while processing options"
        ;;
    esac
  done
#
# bring in configuration information
. $configFile
#
cd $baseDir
LOG_DIR=$baseDir
LOG_FILE=$LOG_DIR/`date +%F_%T | awk '{print $1}' | sed /\:/s//-/g`.log
START=`date +%s`
if [ -e run.properties ] ; then
   centuries=20
   currentDate=`grep currentdate run.properties | awk '{print $3}'`
   currentCycle=`grep currentcycle run.properties | awk '{print $3}'` 
   stormnumber=`grep stormnumber run.properties | awk '{print $3}'` 
   prodID=`grep prodID run.properties | awk '{print $3}'` 
   adcStartTime=${centuries}${currentDate}${currentCycle}0000
#   TRACKNAME=${centuries}${currentDate}${currentCycle}_$stormnumber
   TRACKNAME=`echo $prodID | sed 's|<field>_Z.nc.gz||'`
else
   adcStartTime="20110101000000"
fi

# as long as we are cheating, these are all 15 minute winds.
windTimeStepSecs=900

mkdir $baseDir/graphics

if [ $doWinds -eq 1 ] ; then
   # process the wind file(s)
   processWindFiles
fi

if [ $doAdcirc1 -eq 1 ] ; then
   # process the ADCIRC.1 file(s)
   processAdcirc1
fi

echo "Starting cleanup" >> $LOG_FILE
cd $baseDir
END=`date +%s`
LEN=`echo -e $END - $START | bc`
echo "Post-process took $LEN seconds" >> $LOG_FILE
# ***************************************************************************
# 
# RENCI Open Source Software License
# The University of North Carolina at Chapel Hill
# 
# The University of North Carolina at Chapel Hill (the "Licensor") through 
# its Renaissance Computing Institute (RENCI) is making an original work of 
# authorship (the "Software") available through RENCI upon the terms set 
# forth in this Open Source Software License (this "License").  This License 
# applies to any Software that has placed the following notice immediately 
# following the copyright notice for the Software:  Licensed under the RENCI 
# Open Source Software License v. 1.0.
# 
# Licensor grants You, free of charge, a world-wide, royalty-free, 
# non-exclusive, perpetual, sublicenseable license to do the following to 
# deal in the Software without restriction, including without limitation the 
# rights to use, copy, modify, merge, publish, distribute, sublicense, 
# and/or sell copies of the Software, and to permit persons to whom the 
# Software is furnished to do so, subject to the following conditions:
# 
# . Redistributions of source code must retain the above copyright notice, 
# this list of conditions and the following disclaimers.
# 
# . Redistributions in binary form must reproduce the above copyright 
# notice, this list of conditions and the following disclaimers in the 
# documentation and/or other materials provided with the distribution.
# 
# . Neither You nor any sublicensor of the Software may use the names of 
# Licensor (or any derivative thereof), of RENCI, or of contributors to the 
# Software without explicit prior written permission.  Nothing in this 
# License shall be deemed to grant any rights to trademarks, copyrights, 
# patents, trade secrets or any other intellectual property of Licensor 
# except as expressly stated herein.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
# THE CONTIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
# OTHER DEALINGS IN THE SOFTWARE.
# 
# You may use the Software in all ways not otherwise restricted or 
# conditioned by this License or by law, and Licensor promises not to 
# interfere with or be responsible for such uses by You.  This Software may 
# be subject to U.S. law dealing with export controls.  If you are in the 
# U.S., please do not mirror this Software unless you fully understand the 
# U.S. export regulations.  Licensees in other countries may face similar 
# restrictions.  In all cases, it is licensee's responsibility to comply 
# with any export regulations applicable in licensee's jurisdiction.
# 
# ***************************************************************************# 

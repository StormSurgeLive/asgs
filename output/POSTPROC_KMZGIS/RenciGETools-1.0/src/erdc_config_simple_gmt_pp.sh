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

echo Exec-ing config file config_gmt_pp.sh
rm -rf .gmt*



#Set Target by  hard-wire.
#TARGET="sapphire";
#TARGET="topsail";
#TARGET="ranger";
#TARGET="queenbee";
TARGET="jade";

# Outputs diagnostic messages
DEBUG=1

#----------- SYS DIRS -------------
#----------- SYS DIRS -------------
# define cases as needed, following this pattern. 
case "$TARGET" in 
# for Ranger
	"ranger") 
		GMTHOME=/share/home/01053/rweaver/GMT4.4.0			   # The GMT installation
		PPDIR=/share/home/01053/rweaver/ASGS/asgs_2009/output/POSTPROC_KMZGIS/RenciGETools-1.0/src
                GRDFILES=/share/home/01053/rweaver/ASGS/asgs_2009/output/POSTPROC_KMZGIS/grids
		GS=/usr/bin/gs				   # The ghostscript binary (gs)
        	ImageMagick=/usr/bin			   # path to ImageMagick binaries
        	ZIP=/usr/bin/zip			   # path to zip
		;; 
        "topsail")
           echo Target is TOPSAIL
                GMTHOME=/ifs1/home/rjweaver/GMT4.3.1              # The GMT installation
                PPDIR=/ifs1/scr/rjweaver/POSTPROC_KMZGIS/RenciGETools-1.0/src      # The shell script dir
                GRDFILES=/ifs1/scr/rjweaver/POSTPROC_KMZGIS/grids                    #grids # The GMT-formatted & ADCIRC grid files
                GS=/usr/bin/gs                             # The ghostscript binary (gs)
                ImageMagick=/usr/bin                       # path to ImageMagick binaries
                ZIP=/usr/bin/zip                           # path to zip
                ;;
        "queenbee")
                GMTHOME=/home/rweaver/GMT4.3.1      # The GMT installation
                PPDIR=/home/rweaver/POSTPROC_KMZGIS/RenciGETools-1.0/src    # The shell script dir
                GRDFILES=/home/rweaver/POSTPROC_KMZGIS/grids                # The GMT-formatted& ADCIRC grid files
                GS=/usr/bin/gs                        # The ghostscript binary (gs)
                ImageMagick=/usr/local/packages/imageMagick-6.0.8/bin               # path to ImageMagick binaries
                ZIP=/usr/bin/zip                            # path to zip
                ;;
        "jade")
           echo Target is Jade
                GMTHOME=/usr/local/usp/gmt  # The GMT installation
                PPDIR=~/asgs/trunk/output/POSTPROC_KMZGIS/RenciGETools-1.0/src
                # grids  The GMT-formatted & ADCIRC grid files
                GRDFILES=~/asgs/trunk/output/POSTPROC_KMZGIS/grids
                GS=/usr/bin/gs     # The ghostscript binary (gs)
                # path to ImageMagick binaries
                ImageMagick=/usr/local/usp/ImageMagick
                ZIP=/usr/bin/zip   # path to zip
                ;;
esac

#  input the LAT LON bounds your desire for your output
#BOX="TX";
BOX="LA";
#BOX="WFL";
#BOX="NC";
case "$BOX" in
# Texas Coast
       "TX")
      NORTH=30.5
      SOUTH=25.5
      WEST=-98
      EAST=-93
       ;;
# Louisiana Coast
       "LA")
      NORTH=31
      SOUTH=26
      WEST=-93.5
      EAST=-88.5
       ;;
# Western Florida (Not Panhandle but Gold Coast)
       "WFL")
      NORTH=29
      SOUTH=24
      WEST=-85
      EAST=-80
       ;;
# North Carolina Coast
       "NC")
      NORTH=37
      SOUTH=33
      WEST=-79
      EAST=-75
       ;;
esac



function Make_GMT_matjet_cpt_File 
{

cat << END  > GMT_matjet.cpt
#       $Id: config_simple_gmt_pp.sh,v 1.4 2008/09/19 21:06:27 howard Exp $
#
# Simulates the MATLAB jet colormap better
# COLOR_MODEL = RGB
-1.000 000 000 159 -0.935 000 000 191
-0.935 000 000 191 -0.871 000 000 223
-0.871 000 000 223 -0.806 000 000 255
-0.806 000 000 255 -0.742 000 031 255
-0.742 000 031 255 -0.677 000 063 255
-0.677 000 063 255 -0.613 000 095 255
-0.613 000 095 255 -0.548 000 127 255
-0.548 000 127 255 -0.484 000 159 255
-0.484 000 159 255 -0.419 000 191 255
-0.419 000 191 255 -0.355 000 223 255
-0.355 000 223 255 -0.290 000 255 255
-0.290 000 255 255 -0.226 031 255 223
-0.226 031 255 223 -0.161 063 255 191
-0.161 063 255 191 -0.097 095 255 159
-0.097 095 255 159 -0.032 127 255 127
-0.032 127 255 127  0.032 159 255 095
 0.032 159 255 095  0.097 191 255 063
 0.097 191 255 063  0.161 223 255 031
 0.161 223 255 031  0.226 255 255 000
 0.226 255 255 000  0.290 255 223 000
 0.290 255 223 000  0.355 255 191 000
 0.355 255 191 000  0.419 255 159 000
 0.419 255 159 000  0.484 255 127 000
 0.484 255 127 000  0.548 255 095 000
 0.548 255 095 000  0.613 255 063 000
 0.613 255 063 000  0.677 255 031 000
 0.677 255 031 000  0.742 255 000 000
 0.742 255 000 000  0.806 223 000 000
 0.806 223 000 000  0.871 191 000 000
 0.871 191 000 000  0.935 159 000 000
 0.935 159 000 000  1.000 127 000 000
END
}

function GSConvertOld
{
	echo Converting $1 to $2 at $RES dpi
	if [ $UsePs2Raster -eq 0 ] ; then
		if [ $DEBUG  -eq 1 ] ; then echo "   DEBUG :: $GS $GSARG$2 $1"; fi
		$GS $GSARG$2 $1 > gs.diag  2>&1
	else
		if [ $FPEXT == "png" ] ; then ifmt=g; fi
		if [ $FPEXT == "tiff" ] ; then ifmt=t; fi
		$arg=" $1 -A -E$RES -T$ifmt"
		if [ $DEBUG  -eq 1 ] ; then echo "   DEBUG :: $GMTHOME/bin/ps2raster $arg"; fi
		$GMTHOME/bin/ps2raster $1 -A -E$RES -T$ifmt 
	fi
}



# Build "matjet" color table palette, if needed
GMT_matjet_file=$PPDIR/GMT_matjet.cpt
if [ ! -e $GMT_matjet_file ] ; then
	echo "Making $GMT_matjet_file file"
	Make_GMT_matjet_cpt_File
fi

#---------- GHOSTSCRIPT DEFINITIONS -----
#---------- GHOSTSCRIPT DEFINITIONS -----
RES=72
GSDEV=png16m
FPEXT=png

GSARG=" -r$RES -dSAFER -dBATCH -dNOPAUSE -sDEVICE=$GSDEV -dGraphicsAlphaBits=1 -sOutputFile="
GSPIPECOM=" | $GS $GSARG -"
#CONVERTARG="-crop 0x0"
MOGRIFYARG="-transparent white"

# Use GMT's ps2raster (1) or direct ghostscript (0)
UsePs2Raster=0

#---------- MISC STUFF -----
#---------- MISC STUFF -----
CompressFiles=0
NumDigitsMaxText=1

#---------- PLOT STUFF -----
#---------- PLOT STUFF -----

# Location of Colorbar
SCALEPOS="5i/4.8i/2.6i/0.15i"                
PSSCALEARGS="-D$SCALEPOS -O -E"
#PSSCALEARGS=""
NUMBEROFCOLORS=32

# Don't know why this works.  Seems like GMT is confused about
# screen resolution... This is picked to give a 512x512 image at 72dpi.
PROJ1="X7.111i/7.111i"            # Flat projection 512x512

# Storm track parameters
#TrackLineThickness=3p         #  point width
#TrackLineColor="brown"
#TrackSymbol="c"
#TrackSymbolSize="0.25c"       # in cms
#TrackSymbolColor="black"

# Adcirc Boundary
#BoundaryLineColor="brown"
#BoundaryLineThickness="thin"

# Bathy Lines
#BathyLineColor="red"
#BathyLineThickness="thin"

# Don't touch!
#TrackSymbolArg="$TrackSymbol""$TrackSymbolSize"
#TrackLineArg="$TrackLineThickness""$TrackLineColor,-"
#BoundaryLineArg="$BoundaryLineThickness,$BoundaryLineColor"
#BathyLineArg="$BathyLineThickness,$BathyLineColor"

# color for "dry"; this should be set to "white" 
# bec/ the color white is set to transparent later
# by ImageMagick. 
DryColor="255/255/255"        

# ADCIRC-ONLY
#Plot-Tweak Variables
#DrawContourLines=0
#DrawContourLabels=0
#MaxSymbolPlot=0
#MaxSymbol="c"
#MaxSymbolColor="black" 
#DrawBathyContourLines=0

#BathyContourVals="-30 -20 -10 0 25 50 70 100 500 1000"

#---------- EDIT/SET DEFAULT GMT SETUP -------------
#---------- EDIT/SET DEFAULT GMT SETUP -------------
echo ' '
echo Setting GMT Defaults
#$GMTHOME/bin/gmtset COLOR_FOREGROUND = $DryColor
$GMTHOME/bin/gmtset COLOR_BACKGROUND = $DryColor
$GMTHOME/bin/gmtset COLOR_NAN = $DryColor
$GMTHOME/bin/gmtset HEADER_FONT_SIZE = 24p
$GMTHOME/bin/gmtset HEADER_FONT = Courier
$GMTHOME/bin/gmtset LABEL_FONT_SIZE = 18p
$GMTHOME/bin/gmtset LABEL_FONT = Courier
$GMTHOME/bin/gmtset ANNOT_FONT_SIZE_PRIMARY = 14p
$GMTHOME/bin/gmtset ANNOT_FONT_PRIMARY = Courier
$GMTHOME/bin/gmtset ANNOT_FONT_SIZE_SECONDARY = 10p
$GMTHOME/bin/gmtset ANNOT_FONT_SECONDARY = Courier
$GMTHOME/bin/gmtset PAPER_MEDIA = Custom_512x512
#$GMTHOME/bin/gmtset PAPER_MEDIA = letter
#$GMTHOME/bin/gmtset PAGE_ORIENTATION = landscape
$GMTHOME/bin/gmtset HEADER_OFFSET = 0.2c
#$GMTHOME/bin/gmtset UNIX_TIME = TRUE
#$GMTHOME/bin/gmtset UNIX_TIME_POS = 0.5c/-1.5c
#$GMTHOME/bin/gmtset UNIX_TIME_POS = 0.5c/-1.5c
$GMTHOME/bin/gmtset X_ORIGIN = 0i
$GMTHOME/bin/gmtset Y_ORIGIN = 0i
$GMTHOME/bin/gmtset X_AXIS_LENGTH = 5.12i
$GMTHOME/bin/gmtset Y_AXIS_LENGTH = 5.12i



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

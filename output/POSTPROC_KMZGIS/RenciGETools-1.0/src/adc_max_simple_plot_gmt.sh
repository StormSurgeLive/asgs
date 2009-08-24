#!/bin/bash
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
#
# File: adc_max_simple_plot_gmt.sh
# Purpose: 
#
# V1.0   :  Brian Blanton/RENCI  :  10 Jun 2007
# V2.0   :  Howard Lander/RENCI  :  August 2008
# V2.1   :  Howard Lander/RENCI  :  September 2008, beta-release


####################
####################   Functions
####################

function Usage 
{
	echo "Usage: $0 -f fortname -g grid -p prefix -n nLevels [-d startDate] [-o outputmode] [-k kmzPrefix] [-m mode] [-c colorrange] [-u units]"
    echo "  -f: The name of the fort.63 file to visualize."
    echo "  -g: The name of the gmt versions of the relevant ADCIRC grid."
    echo "  -p: The prefix for all artifacts including the resulting KMZ file."
    echo "      You can specify the prefix of the kmz file separately using -k"
    echo "      but you probably don't want to: that functionality is provided"
    echo "      for the use of other programs."
    echo "  -n: The number of levels of plots in the KMZ files. See"
    echo "      TiledREADME.pdf for more details."
    echo "  -d: The starting date of the simulation being visualized. Use"
    echo "      yyyy-mm-ddThh:mm:ss format. Setting a date greater than -1"
    echo "      causes a timestamp to be added to the kml file. If you are"
    echo "      producing a single kmz file using \"both\" (the default) as the"
    echo "      output mode, omit this parameter."
    echo "  -e: The ending date of the simulation being visualized."
    echo "  -o: The output mode: this controls what this script actually produces."
    echo "      The modes are:"
    echo "         both: produce both the colormap and the complete kmz. Default"
    echo "         colormap: produce only the colormap"
    echo "         kmz: produce the kmz, including a previously produced colormap"
    echo "              This is used when multiple kmzs are sharing a colormap"
    echo "  -k: The prefix for the kmz file produced. The default is "
    echo "      the prefix specified with the -p flag"
    echo "  -m: The mode for the fort.63 file.  Possible values are"
    echo "         full: Full ascii format"
    echo "         netCDFScoop: NetCDF SCOOP format"
    echo "         compact: Currently Unsupported. Compact ascii format"
    echo "      the default is \"full\""
    echo "  -t: The timestep in the fort.63 file to visualize. This is"
    echo "      only supported with netCDF format fort.63 files."
    echo "  -c: The colon seperated min max values to use for the colormap. For"
    echo "      example -c 0:2 will produce a colormap between 0 and 2 units."
    echo "  -u: The units for the display.  Must be either M (meters) or "
    echo "      FT for feet.  Default is meters."
   
}

function StripFile
{

    if [ "$Mode63" = "compact" ] ||  [ "$Mode63" = "full" ] ; then
	   sed '1,3d' $MaxFile  > temp.dat              # Clobber first 3 lines
       if [ "$UNITS" = "M" ] ; then
          cat temp.dat | awk '{print $1 "   " $2 * 1}' > temp.dat2
       else
          cat temp.dat | awk '{print $1 "   " $2 * 3.2808}' > temp.dat2
       fi
          # paste with nodal coords
	      awk '{print $2}'  temp.dat2  | paste $XY - > max.temp
	      rm temp.dat temp.dat2
       else
       # A netcdf file
          buildArg="--dataFile=$MaxFile --fort63Mode=$Mode63 --scale=$ScaleFactor --outFile=max.temp --timeStep=$timeStep"
          echo "calling BuildContourFile $buildArg"
          result=`$PPDIR/BuildContourFile $buildArg`
       fi
}

function getMinMax {
     if [ "$MINMAXMODE" = "AUTO" ]  ; then
       maxArg="--north=$NORTH --south=$SOUTH --east=$EAST --west=$WEST --highClip=100 --lowClip=-100 --fort63Mode=$Mode63  --scale=$ScaleFactor --dataFile=$MaxFile"
       if [ "$Mode63" = "compact" ] ||  [ "$Mode63" = "full" ] ; then
         maxArg="$maxArg --gridFile=$XY"
       else
         # In netCDF mode we are assuming the grid is in the same
         # netCDF file.
         maxArg="$maxArg --gridFile=$MaxFile"
       fi
       echo "calling FindMax $maxArg"
       minmax=`$PPDIR/FindMax $maxArg`

       echo "minmax value is $minmax"
       min=`echo $minmax | awk '{print $1}'`
       max=`echo $minmax | awk '{print $2}'`
    else
       min=$MIN
       max=$MAX
    fi
    echo $min $max
}

function MakePalette
{
    deltaRange=$(echo "scale=4;$max - $min"|bc)
    deltaColor=$(echo "scale=$ColorPrecision;$deltaRange/$NUMBEROFCOLORS"|bc)
	deltaColor="$(printf '%f' $deltaColor)"
    echo "deltaColor $deltaColor\n";
	res=$(echo "$deltaColor==0" | bc)
	if [ $res -eq 1 ] ; then 
		echo "Color increment ($deltaColor) too small; reset ColorPrecision"
		exit 1
	fi
  	ZCOLRANGE=$min/$max/$deltaColor

    $GMTHOME/bin/gmtset COLOR_BACKGROUND = $DryColor
  	echo "Making color palette  $CPTZ with args -C$CTABLE -T$ZCOLRANGE"
	$GMTHOME/bin/makecpt -C$CTABLE -T$ZCOLRANGE > $CPTZ
}

function GSConvert
{
	echo Converting $1.ps to $1.$FPEXT at $RES dpi
	echo $GS$GSARG $1.$FPEXT $1.ps 
	$GS $GSARG$1.$FPEXT $1.ps >> gs.diag  2>&1
}

function ConvertAndMogrify
{
#	echo Converting $1.ps to $1.$FPEXT
#	echo $CONVERT $CONVERTARG $1.ps $1.$FPEXT
#	$CONVERT $CONVERTARG $1.ps $1.$FPEXT  >> gs.diag  2>&1

    	GSConvert $1
	echo Mogrifying $1.$FPEXT
	echo $ImageMagick/mogrify $MOGRIFYARG $1.$FPEXT
	$ImageMagick/mogrify $MOGRIFYARG $1.$FPEXT  >> gs.diag  2>&1
}

function CleanUp
{
	# Clean up
	echo Cleaning up ...
	rm -f temp.dat temp2.dat fort.997 gs.diag
	if [ $CompressFiles -eq "1" ] ; then 
		echo Bzipping $FP.ps $FP.$FPEXT 
		bzip2 --force $FP.$FPEXT $FP.ps
	fi
}

function MakeColorbar
{
	cpt=$1
	barg=$2
  	targ=$3
            
	$GMTHOME/bin/gmtset X_ORIGIN = 0.i
	$GMTHOME/bin/gmtset Y_ORIGIN = 0.i
	#-D defines the position of the center/top (for horizontal scale) or center/left (for vertical scale) and the dimensions of the scale
	# -Dxpos/ypos/length/width
	#arg1="-P -R0/0/1/1r -JX1i/10i -B0 -K"
	#arg2="-D0i/5i/10i/1i -O -B$barg -C$cpt"
	arg1="-P -R0/0/1/1r -JX0.5i/5i -B0 -K"
	arg2="-D0i/2.5i/5i/0.5i -O -B$barg -C$cpt"
        #arg3="-crop 120x390+60+340"   # topsail
        arg3=" -crop 110x400+00+110"

	if [ $DEBUG ] ; then 
      echo Drawing colorbar :  $GMTHOME/bin/psbasemap $arg1 \>\> $targ.ps 
      echo Drawing colorbar :  $GMTHOME/bin/psscale $arg2 \>\> $targ.ps 
	  echo Converting : $ImageMagick/convert $arg3 $targ.ps $targ.png
	fi
    rm .gmtdefaults4
	$GMTHOME/bin/psbasemap $arg1 > $targ.ps
        $GMTHOME/bin/psscale $arg2 >> $targ.ps
	$ImageMagick/convert $arg3 $targ.ps $targ.png
}

function MakePlot
{
	local file cpt max units barg title BOX targ
	file=$1
	cpt=$2
	max=$3
	units=$4
	barg=$5
	BOX=$6
    	level=$7

    # This is the number of tiles on a side at this level
    nTiles=$(echo "2^($level - 1)" |bc)

    # The change in Lat/Lon over the entire figure
    DeltaLongitude=$(echo "-($WEST - $EAST)" |bc)
    DeltaLatitude=$(echo "($NORTH - $SOUTH)" |bc)

    # The PER TILE change in Lat/Lon 
    DeltaLonPerTile=$(echo "scale=4;$DeltaLongitude/$nTiles" |bc)
    DeltaLatPerTile=$(echo "scale=4;$DeltaLatitude/$nTiles" |bc)

    # These are the dimensions for the current tile being drawn  
    TileSouth=$SOUTH
    TileNorth=$(echo "scale=4;$TileSouth  + $DeltaLatPerTile"|bc)
    TileWest=$WEST
    TileEast=$(echo "scale=4;$TileWest  + $DeltaLonPerTile"|bc)

    # Tile name is level.x.y where 1.1 is lower left
    # The loop proceeds from west to east and south to north
    for ((y=1; y<=nTiles; y++))
    do
        for ((x=1; x<=nTiles; x++))
        do
            BOX="$TileWest/$TileEast/$TileSouth/$TileNorth"
	        targ="$Prefix.$level.$x.$y"
	        echo ' '
	        echo Generating figure ... $@ $targ with BOX $BOX
           
	        # Draw Colored triangles
	        arg="$file $PSCONTOURARGS -R$BOX -C$cpt "
	        if [ $DEBUG ] ; 
            then 
                echo DEBUG :: Drawing triangles :  $GMTHOME/bin/pscontour $arg \> $targ.ps ; 
            fi 
	     $GMTHOME/bin/pscontour $arg -K  > $targ.ps
    # add track line and points if available
    #       perl $PPDIR/make_track_files.pl 
        if [  -e track_point.dat ] ; then
            $GMTHOME/bin/psxy ./track_point.dat -: -R -JX -O -K -P -G0 -Skhurricane -V >>  $targ.ps
        fi
        if [  -e track_line.dat ] ; then
            $GMTHOME/bin/psxy ./track_line.dat -: -R -JX -O  -P -W5.0  -V >>  $targ.ps
        fi

            ConvertAndMogrify $targ

            # Make the kml file for this tile
            KMLArg="--prefix=$Prefix --level=$level --maxLevel=$NLevels --tileX=$x --tileY=$y --minPix=256 --maxPix=512 --north=$TileNorth --south=$TileSouth --east=$TileEast --west=$TileWest --colorbar=$colorbarName.png --date=$Date --endDate=$EndDate --logo=$logo"
            echo $KMLArg
            $PPDIR/WriteTiledKML $KMLArg

            # Now call the world file builder.
            worldArg="--image=$targ.png --world=$targ.world --north=$TileNorth --south=$TileSouth --east=$TileEast --west=$TileWest"
            echo $worldArg
            $PPDIR/makeWorldFile.pl $worldArg

            # Reset East and West for next tile.
            TileWest=$TileEast
            TileEast=$(echo "scale=4;$TileEast  + $DeltaLonPerTile"|bc)
        done
        # Reset North and South for next row of tiles.
        TileSouth=$TileNorth
        TileNorth=$(echo "scale=4;$TileNorth  + $DeltaLatPerTile"|bc)

        # Reset East and West to the beginning of the row.
        TileWest=$WEST
        TileEast=$(echo "scale=4;$TileWest  + $DeltaLonPerTile"|bc)
    done

    # make a montage for this level.
    $PPDIR/makeMontage.sh $Prefix $level
	rm -rf temp.trk temp.6h.trk ll.temp
}


####################
####################   MAIN MAIN MAIN
####################
# This code is set up to make 512x512 tiles. If you want to change 
# that you need to set PROJ1, PAPER_MEDIA, X_AXIS_LENGTH and Y_AXIS_LENGTH 
# in $PPDIR/config_simple_gmt_pp.sh

# Set a default mode
MaxFile=""
AdcGrid=""
Prefix=""
NLevels=""
Date="-1"
EndDate="-1"
Mode="both"
Mode63="full"
KMZ=""
MIN=""
MAX=""
MINMAXMODE="AUTO"
timeStep=1
logo="adcirc_logo_white.png"
#logo="renci_logo_white_www_sm.png"
UNITS="M"

while getopts ":f:g:p:n:d:e:o:k:m:c:t:u:" optname
  do
    case "$optname" in
      "f")
        MaxFile=$OPTARG
        ;;
      "g")
        AdcGrid=$OPTARG
        ;;
      "p")
        Prefix=$OPTARG
        ;;
      "n")
        NLevels=$OPTARG
        ;;
      "d")
        Date=$OPTARG
        ;;
      "e")
        EndDate=$OPTARG
        ;;
      "o")
        Mode=$OPTARG
        ;;
      "k")
        KMZ=$OPTARG
        ;;
      "m")
        Mode63=$OPTARG
        ;;
      "c")
        # User has passed in a color map mode
        # format is MINxMAX ie 0:2
        MINMAXMODE=USER
        delim=`expr index "$OPTARG" :`
        delim=`expr $delim - 1`
        MIN=${OPTARG:0:$delim}
        delim=`expr $delim + 1`
        MAX=${OPTARG:$delim:10} 
        echo color range set by user: $MIN $MAX $delim
        ;;
      "t")
        timeStep=$OPTARG
        ;;
      "u")
        UNITS=$OPTARG
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

if [ "$Mode63" = "compact" ] && [ "$Mode" != "colormap" ] ; then
    echo ""
    echo "Sorry: compact mode not yet supported"
    echo ""
    Usage
    echo ""
    echo "Sorry: compact mode not yet supported"
    echo ""
    exit 1
fi

if [ ! -n "$MaxFile" ] ; then 
   echo "No fort.63 specified"
   Usage
   exit 1
fi

if [ ! -n "$AdcGrid" ] ; then 
   echo "No grid specified"
   Usage
   exit 1
fi

if [ ! -n "$Prefix" ] ; then 
   echo "No prefix specified"
   Usage
   exit 1
fi

if [ ! -n "$NLevels" ] ; then 
   echo "No number of levels specified"
   Usage
   exit 1
fi

if [ ! -n "$Date" ] ; then 
   echo "No start date specified"
   Usage
   exit 1
fi

if [ ! -n "Mode" ] ; then 
   echo "No mode specified"
   Usage
   exit 1
fi

#----------- EXEC Config File -------------------------------------
#----------- Sets SYS DIRS, GMT variables, etc ... ----------------
source $PPDIR/config_simple_gmt_pp.sh

echo MaxFile    = $MaxFile
echo AdcGrid    = $AdcGrid
echo Prefix     = $Prefix
echo NLevels    = $NLevels
echo Date       = $Date
echo EndDate    = $EndDate
echo Mode       = $Mode
echo Mode63     = $Mode63
if [ -n "$KMZ" ] ; then
echo KMZ        = $KMZ
else 
KMZ=$Prefix
echo KMZ        = $KMZ
fi

# start at the top level
currentLevel=1

# If needed, copy in some files
if [ `pwd` != $PPDIR ] ; then
   cp $PPDIR/$logo .
   cp $PPDIR/GMT_matjet.cpt .
fi

#--------- GRID FILES --------------
#--------- GRID FILES --------------
if [ "$Mode63" = "compact" ] ||  [ "$Mode63" = "full" ] ; then
   TRI=$GRDFILES/$AdcGrid.gmt.tri
   XY=$GRDFILES/$AdcGrid.gmt.xy
   XYZ=$GRDFILES/$AdcGrid.gmt.xyz
   BNDXY=$GRDFILES/$AdcGrid.gmt.bnd.xy
   if [ ! -e $TRI ] ; then
         echo Could not find required grid file : $TRI
         exit 1
   elif [ ! -e $XY ] ; then
         echo Could not find required grid file : $XY
         exit 1
   elif [ ! -e $BNDXY ] ; then
         echo Could not find required grid file : $BNDXY
         exit 1
   fi
else 
#  TRI=$GRDFILES/max.temp.tri
   TRI=max.temp.tri
fi
# Plot Definitions
# View limits:
#NORTH=31
#SOUTH=21
#WEST=-98
#EAST=-88

#BOXLIMS[0]="-79/-75/33/37"				  
BOXLIMS[0]="$WEST/$EAST/$SOUTH/$NORTH"

CTABLE=matjet
PSCONTOURARGS="-T$TRI -J$PROJ1 -P -I"           # Common contour arguments  -X1.5

ColorPrecision=3
ColorbarDelta=.2
numberofticks=8

case $UNITS in 
	M ) ScaleFactor=1 ;;
	FT ) ScaleFactor=3.2808 ;;
	* ) 
	echo Unsupported units of $UNITS
	exit 1;;
esac

# set the min and max
getMinMax

# Calculate the color range and the ticking so that we
# get a reasonable looking colorbar.
colorrange=$(echo "scale=$ColorPrecision;$max - $min"|bc)
echo "colorrange $colorrange";
deltaColor=$(echo "scale=$ColorPrecision;$colorrange/$NUMBEROFCOLORS"|bc)
deltaColor="$(printf '%f' $deltaColor)"
deltaTick=$(echo "scale=1;$colorrange/$numberofticks"|bc)
echo "deltaColor $deltaColor";
if [[ $deltaTick == 0 ]]; then
    deltaTick=.1
fi
echo "deltaTick $deltaTick"

#BARG=$(printf 'f%.2fa%.2f:WaterLevel:/:%s:\n' $ColorbarDelta $ColorbarDelta $UNITS)
BARG=$(printf 'f%.2fa%.2f:WaterLevel:/:%s:\n' $deltaTick $deltaTick $UNITS)
echo $BARG
echo $ScaleFactor

#Cont_Label_Spec[0]="-A+kblack -Gd1i:1i"

#--------- Color Palatte Tables -------
#--------- Color Palatte Tables -------
# Max elevation in meters
   CPTZ=zeta.cpt
#   CPTZLINES=zeta.lines.cpt
#   ZLINESCOLRANGE=-1/5/.5
#   BATHYLINES=bathy.vals.cpt

#--------- START PROCESSING --------
#--------- START PROCESSING --------

# List the gmtdefaults for debugging purposes.
#if [ $DEBUG -eq 1 ] ; then 
#	$GMTHOME/bin/gmtdefaults -L
#fi

#----------- Generate plot
echo ' '
echo \#----------- Generating plot from $MaxFile

if [ -e $MaxFile  ] ; then

    if [ "$Mode" = "both" ] || [ "$Mode" = "colormap" ] ; then
       echo "Making the palette"
       MakePalette
    fi

    
    if [[ "$Mode" = "both" || "$Mode" = "kmz" ]] ; then
       echo "Making the tiles"
	    # strip headers, scale data, paste to xy file
	    if [ $DEBUG -eq 1 ] ; then
		    echo Stripping and Scaling with ScaleFactor = $ScaleFactor ...
	    fi

	   StripFile
       colorbarName=$Prefix-colorbar
   	   for ((currentLevel=1; currentLevel<=NLevels; currentLevel++))
    	  do
	         for b in "${BOXLIMS[@]}" ; do
	            echo MakePlot max.temp $CPTZ $max $UNITS $BARG  $b $currentLevel 
	    		MakePlot      max.temp $CPTZ $max $UNITS $BARG  $b $currentLevel 
	    	 done
    	  done

    	MakeColorbar $CPTZ $BARG $colorbarName
   fi	
else
	echo $FI does not exist.  Terminal. 
        exit 1
fi

echo ' '

# Note the extra dot in the png glob.  That's to prevent
# the montage files from being placed in the kmz
if [[ "$Mode" = "both" || "$Mode" = "kmz" ]] ; then
   arg="$KMZ.kmz $Prefix*.kml $Prefix.*.png $Prefix-colorbar.png $logo"
   echo Making the KMZ file $ZIP $arg
   $ZIP $arg

   # Clean up the files:
   #arg="$Prefix*.kml $Prefix.*.png $Prefix-colorbar.png $Prefix*.ps gs.diag max.temp"
   arg="$Prefix*.kml $Prefix.*.png $Prefix-colorbar.png $Prefix*.ps gs.diag max.temp*"
   rm -rf $arg
fi
rm -rf .gmt* .GMT* zeta.cpt 

if [[ "$Mode" = "both" ]] ; then
   # we don't need the world files
   arg="$Prefix*.world"
   rm -rf $arg 
fi
# If needed, delete the files we copied in
if [ `pwd` != $PPDIR ] ; then
   rm -rf $logo GMT_matjet.cpt 
fi


echo ' '

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

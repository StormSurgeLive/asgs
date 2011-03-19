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
    echo "  -a: The target platform (topsail, ranger, sapphire, etc). This" 
    echo "      allows the user to specify the path to required executables."
    echo "      The various platforms are defined in config_simple_gmt_pp.sh."
    echo "      The default is topsail."
    echo "  -b: The bounding box to be used for graphics (NC, TX, LA etc)."
    echo "      The different boxes are defined in config_simple_gmt_pp.sh."
    echo "      The default is NC."
}
#
# Just echoes to the screen 
logMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] INFO: $@"
  echo ${MSG} 
}
#
# Just echoes to the screen and exits with error status
errorMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] ERROR: $@"
  echo ${MSG} 
  exit 1
}
#
# log a debug message
debugMessage()
{ DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  MSG="[${DATETIME}] DEBUG: $@"
  echo ${MSG} 
}
#
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
       logMessage "adc_max_simple_plot_gmt.sh: StripFile: Calling BuildContourFile with the following argument(s): $buildArg."
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
       logMessage "adc_max_simple_plot_gmt.sh: getMinMax: Calling FindMax with the following arguments: $maxArg."
       minmax=`$PPDIR/FindMax $maxArg`

       logMessage "adc_max_simple_plot_gmt.sh: getMinMax: minmax value is $minmax."
       min=`echo $minmax | awk '{print $1}'`
       max=`echo $minmax | awk '{print $2}'`
    else
       min=$MIN
       max=$MAX
    fi
    logMessage "adc_max_simple_plot_gmt.sh: getMinMax: min is $min and max is $max."
}

function MakePalette
{
    deltaRange=$(echo "scale=4;$max - $min"|bc)
    deltaColor=$(echo "scale=$ColorPrecision;$deltaRange/$NUMBEROFCOLORS"|bc)
    deltaColor="$(printf '%f' $deltaColor)"
    logMessage "adc_max_simple_plot_gmt.sh: MakePallette: deltaColor $deltaColor\n";
    res=$(echo "$deltaColor==0" | bc)
    if [ $res -eq 1 ] ; then 
       errorMessage "adc_max_simple_plot_gmt.sh: MakePallette: Color increment ($deltaColor) too small; reset ColorPrecision. Exiting."
       # exit 1
    fi
    ZCOLRANGE=$min/$max/$deltaColor
    $GMTHOME/bin/gmtset COLOR_BACKGROUND = $DryColor
    logMessage "adc_max_simple_plot_gmt.sh: MakePallette: Making color palette $CPTZ with the following arguments: -C$CTABLE -T$ZCOLRANGE."
    $GMTHOME/bin/makecpt -C$CTABLE -T$ZCOLRANGE > $CPTZ
}

function GSConvert
{
    logMessage "adc_max_simple_plot_gmt.sh: GSConvert: Converting $1.ps to $1.$FPEXT at $RES dpi with the following command: $GS $GSARG$1.$FPEXT $1.ps >> gs.diag 2>&1."
    $GS $GSARG$1.$FPEXT $1.ps >> gs.diag  2>&1
}

function ConvertAndMogrify
{
#	echo Converting $1.ps to $1.$FPEXT
#	echo $CONVERT $CONVERTARG $1.ps $1.$FPEXT
#	$CONVERT $CONVERTARG $1.ps $1.$FPEXT  >> gs.diag  2>&1

    	GSConvert $1
	logMessage "adc_max_simple_plot_gmt.sh: ConvertAndMogrify: Mogrifying $1.$FPEXT with the following command: $ImageMagick/mogrify $MOGRIFYARG $1.$FPEXT >> gs.diag 2>&1."
	$ImageMagick/mogrify $MOGRIFYARG $1.$FPEXT  >> gs.diag  2>&1
}

function CleanUp
{
	# Clean up
	logMessage "adc_max_simple_plot_gmt.sh: CleanUp: Cleaning via rm -f temp.dat temp2.dat fort.997 gs.diag."
	rm -f temp.dat temp2.dat fort.997 gs.diag
	if [ $CompressFiles -eq "1" ] ; then 
		logMessage "adc_max_simple_plot_gmt.sh: CleanUp: Bzipping $FP.ps $FP.$FPEXT." 
		bzip2 --force $FP.$FPEXT $FP.ps
	fi
}

function MakeColorbar
{
    cpt=$1
    barg=$2
    targ=$3
#            
    $GMTHOME/bin/gmtset X_ORIGIN = 0.i
    $GMTHOME/bin/gmtset Y_ORIGIN = 0.i
    #-D defines the position of the center/top (for horizontal scale) or center/left (for vertical scale) and the dimensions of the scale
    # -Dxpos/ypos/length/width
    #     arg1="-P -R0/0/1/1r -JX2i/8i -B0 -K"
    #     arg2="-D0.25i/3.5i/5i/.5i -O -B$barg -C$cpt"
    #     arg3="-crop 130x400+00+50"   
    #   arg1="-P -R0/0/1/1r -JX1i/10i -B0 -K"
    #   arg2="-D0i/5i/10i/1i -O -B$barg -C$cpt"
    arg1="-P -R0/0/1/1r -JX0.5i/5i -B0 -K"
    arg2="-D0i/2.5i/5i/0.5i -O -B$barg -C$cpt"
    arg3="-crop 130x460+60+340"   # topsail
#
    if [ $DEBUG ] ; then 
       debugMessage "adc_max_simple_plot_gmt.sh: MakeColorbar: Drawing colorbar : $GMTHOME/bin/psbasemap $arg1 \>\> $targ.ps." 
       debugMessage "adc_max_simple_plot_gmt.sh: MakeColorbar: Drawing colorbar : $GMTHOME/bin/psscale $arg2 \>\> $targ.ps." 
       debugMessage "adc_max_simple_plot_gmt.sh: MakeColorbar: Converting : $ImageMagick/convert $arg3 $targ.ps $targ.png."
    fi
    rm .gmtdefaults4
    $GMTHOME/bin/psbasemap $arg1 > $targ.ps
    $GMTHOME/bin/psscale $arg2 >> $targ.ps
    $ImageMagick/convert $arg3 $targ.ps $targ.png
}

function MakePlot
{
	local file cpt max units barg title BOXLIMS targ 
	file=$1
	cpt=$2
	max=$3
	units=$4
	barg=$5
	BOXLIMS=$6
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
    for ((y=1; y<=nTiles; y++)); do
        for ((x=1; x<=nTiles; x++)); do
            BOXLIMS="$TileWest/$TileEast/$TileSouth/$TileNorth"
            targ="$Prefix.$level.$x.$y"
            echo ' '
            logMessage "adc_max_simple_plot_gmt.sh: MakePlot: Generating figure ... $@ $targ with BOX $BOXLIMS on platform $TARGET."
            #           
            # Draw Colored triangles
            arg="$file $PSCONTOURARGS -R$BOXLIMS -C$cpt "
            if [ $DEBUG ] ; then 
                debugMessage "adc_max_simple_plot_gmt.sh: MakePlot: Drawing triangles :  $GMTHOME/bin/pscontour $arg \> $targ.ps." 
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
            logMessage "adc_max_simple_plot_gmt.sh: MakePlot: Making kml file for this tile with the command $PPDIR/WriteTiledKML $KMLArg."
            $PPDIR/WriteTiledKML $KMLArg

            # Now call the world file builder.
            worldArg="--image=$targ.png --world=$targ.world --north=$TileNorth --south=$TileSouth --east=$TileEast --west=$TileWest --imagemagickpath $ImageMagick"
            logMessage "adc_max_simple_plot_gmt.sh: MakePlot: Calling world file builder with the command $PPDIR/makeWorldFile.pl $worldArg."
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
    logMessage "adc_max_simple_plot_gmt.sh: MakePlot: Making a montage for this level with the command $PPDIR/makeMontage.sh $Prefix $level $TARGET $BOX."
    $PPDIR/makeMontage.sh $Prefix $level $TARGET $BOX
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
TARGET="topsail"
BOX="NC"
#
while getopts ":f:g:p:n:d:e:o:k:m:c:t:u:a:b:" optname
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
      "a")
        TARGET=$OPTARG
        ;;
      "b")
        BOX=$OPTARG
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
source $PPDIR/config_simple_gmt_pp.sh $TARGET $BOX

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
	            echo MakePlot max.temp $CPTZ $max $UNITS $BARG  $b $currentLevel $TARGET
                    MakePlot max.temp $CPTZ $max $UNITS $BARG $b $currentLevel $TARGET 
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

#!/bin/sh
# -vx

usage() {
  echo >&2 "Usage: ` basename $0` --plotname image_filename --adcircrundir adcirc_run_dir --westlon westernmost_lon_to_plot --eastlon easternmost_lon_to_plot --southlat southernmost_lat_to_plot --northlat northernmost_lon_to_plot --units SI_or_ft_or_feet_or_metric_or_m --plotminvalue minimum_height_in_colortable --plotmaxvalue maximum_height_in_colortable [--plottrackyn y_or_n]

Option details:
  --plotname               name for output plots, e.g. katrina_adv18_nhcNWS20
  --adcircrundir           directory containing fort.14, maxele, etc. Also where plots will be created. E.g. /homeq/qrisq/adcirc/fcst/texasIke/37/nhcConcensus
  --westlon		   Westernmost Longitude to plot, e.g. -97.0
  --eastlon		   Easternmost Longitude to plot, e.g. -94.0
  --southlat		   Southernmost Latitude to plot, e.g. 29.0
  --northlat		   Northernmost Latitude to plot, e.g. 31.0
  --units		   Metric or standard. E.g. 'feet' or 'm'
  --plotminvalue	   Smallest value to appear in color bar, e.g. 0.0
  --plotmaxvalue	   Largest value to appear in color bar, e.g. 20.0
  --plottrackyn		   Optional. Y or N (case insensitive) to plot storm track from fort.22

Example execution:
fgwrapper.sh --plotname testme --adcircrundir ~/adcirc/fcst/cpraKatrina/16/nhcConsensus/ --westlon -90.0 --eastlon -89.25 --southlat 30.0 --northlat 30.5 --units feet --plotminvalue 0.0 --plotmaxvalue 16.0 --plottrackyn y"

}

if [ $# -lt 1 ]; then
  usage
  exit 1
fi

PLOTTRACKYN="n"

while [[ $# > 1 ]]
do
	key=$1
        case $key in
                --plotname)
                PLOTNAME="$2"
                shift
                ;;
                --adcircrundir)
                ADCIRCRUNDIR="$2"
                shift
                ;;      
                --westlon)
                WESTLON="$2"
                shift
                ;;
                --eastlon)
                EASTLON="$2"
                shift
                ;;
                --southlat)
                SOUTHLAT="$2"
                shift
                ;;
                --northlat)
                NORTHLAT="$2"
                shift
                ;;
                --units)
                UNITS="$2"
                shift
                ;;
                --plotminvalue)
                PLOTMINVALUE="$2"
                shift
                ;;
                --plotmaxvalue)
                PLOTMAXVALUE="$2"
                shift
                ;;
		--plottrackyn)
		PLOTTRACKYN="$2"
		shift
		;;
        esac
        shift
done

template=/homeq/qrisq/bin/fgwrap/FG41_maxele_template.inp
palette=/homeq/qrisq/bin/fgwrap/Default2.pal

cd $ADCIRCRUNDIR
rm -f temp.fg.inp
cp $template temp.fg.inp
cp $palette .

sed -i s/%plotname%/"${PLOTNAME}                     "/g temp.fg.inp
sed -i s/%westextent%/$WESTLON/g temp.fg.inp
sed -i s/%eastextent%/$EASTLON/g temp.fg.inp
sed -i s/%southextent%/$SOUTHLAT/g temp.fg.inp
sed -i s/%northextent%/$NORTHLAT/g temp.fg.inp

deltalon=`echo "($EASTLON - $WESTLON + 0.5)/1" | bc`
if [ $deltalon -gt 2 ]; then
	intervalsize=1.0
elif [ $deltalon -gt 1 -a $deltalon -le 2 ]; then
	intervalsize=0.5
else
	intervalsize=0.25
fi

if [ $UNITS == 'SI' -o $UNITS == 'ft' -o $UNITS == 'feet' -o $UNITS == 'Feet' ]; then
	conversion=3.2808
	contourunit='Feet'
else
	conversion=1.0
	contourunit='m'
fi

sed -i s/%conversion%/$conversion/g temp.fg.inp
sed -i s/%contourunit%/$contourunit/g temp.fg.inp

contourminmax=" "$PLOTMINVALUE","$PLOTMAXVALUE

sed -i s/%contourminmax%/"${contourminmax}"/g temp.fg.inp

sed -i s/%latloninterval%/$intervalsize/g temp.fg.inp

if [ `echo "${PLOTTRACKYN,,}"` == 'y' ]; then
	rm -f $ADCIRCRUNDIR/stormtrack.txt
	nws=`grep NWS $ADCIRCRUNDIR/fort.15 | awk '{print $1}'`
	if [ $nws -eq 20 -o $nws -eq 320 ]; then
		lonlats=($(grep " 34, NEQ" fort.22 | awk -F', ' '{print $8"_"$7}'))
#		for lonlat in "`grep " 34, NEQ" fort.22 | awk -F', ' '{print $8"_"$7}'`"
		for lonlat in "${lonlats[@]}"
		do
			echo $lonlat
			lonW=`echo $lonlat | awk -F'_' '{print $1}'`
			latN=`echo $lonlat | awk -F'_' '{print $2}'`
			WE=`echo "${lonW: -1}"`
			NS=`echo "${latN: -1}"`
			lon=`echo $lonW | sed 's/.$//'`
			lat=`echo $latN | sed 's/.$//'`
			lon=`echo "scale=2; $lon / 10.0 " | bc`
			lat=`echo "scale=2; $lat / 10.0 " | bc`
			if [ $WE == "W" ]; then
				lon=`echo "scale=2; $lon * ( 0.0 - 1.0 )" | bc`
			fi
			echo " $lon $lat" >> $ADCIRCRUNDIR/stormtrack.txt
			
		done
	fi
	sed -i s/%trackcolorfilename%/"2,Black,stormtrack.txt         "/g temp.fg.inp
else
	sed -i s/%trackcolorfilename%/"0,color,filename               "/g temp.fg.inp
fi


FigureGen41-states.x << EOF
temp.fg.inp
EOF

exit

#!/bin/sh -vx

whatwasrun=`echo "${0} ${@}"`

while [[ $# > 1 ]]
do
        key=$1
        case $key in
                --advisorynum)
                ADVISORYNUM="$2"
                shift
                ;;
		--instance)
		INSTANCE="$2"
		shift
		;;	
		--midlat)
		MIDLAT="$2"
		shift
		;;
		--midlon)
		MIDLON="$2"
		shift
		;;
		--maxdist)
		MAXDIST="$2"
		shift
		;;
        esac
        shift
done

adcircbase=~/adcirc/fcst
nosbase=~/meteo/obs/nos

instancedir=$adcircbase/$INSTANCE

#for advisorydir in $(ls -1d $instancedir/b*k/)
for advisorydir in $(ls -1d $instancedir/$ADVISORYNUM/*/ | grep -v nowcast)
do
	advisory=$ADVISORYNUM	#`basename $advisorydir`
	ensemble=`basename $advisorydir`
#	if [ $advisory -eq $advisory ] 2>/dev/null; then
#		cd $advisorydir/nhcConsensus
		cd $advisorydir
		minlon=`echo "scale=2;$MIDLON - 1.25" | bc `
		maxlon=`echo "scale=2;$MIDLON + 1.25" | bc `
		minlat=`echo "scale=2;$MIDLAT - 1.0" | bc `
		maxlat=`echo "scale=2;$MIDLAT + 1.0" | bc `
		
		fgwrapper.sh --plotname ${INSTANCE}_${advisory}_$ensemble --adcircrundir $advisorydir --westlon $minlon \
			--eastlon $maxlon --southlat $minlat --northlat $maxlat --units m --plotminvalue 0.0 \
			--plotmaxvalue 6.0 --plottrackyn y
		

#./nos_get.sh --adcircrundir /homeq/qrisq/adcirc/fcst/al-nwfl-test/ivan_ebtrk/coldstart --event ivan_tides --csdate 2004081100 --getlat 29.88 --getlon -87.25 --maxdist 3.0 --plotstart 20040811 --plotend 20080910 --offsetfile `pwd`/offset_data/al-ivan-offset.dat
		

		coldstarttime=`grep ColdStartTime run.properties | head -1 | awk '{print $3}'`
		plotstart=`grep RunStartTime run.properties | head -1 | awk '{print $3}' | cut -c 1-8`
		plotend=`grep RunEndTime run.properties | head -1 | awk '{print $3}' | cut -c 1-8`
		
		cd $nosbase
		offsetopt=""
		if [ -e $nosbase/offset_data/$INSTANCE.dat ]; then
			offsetopt="--offsetfile $nosbase/offset_data/$INSTANCE.dat"
		fi
		
		./nos_get.sh --adcircrundir $advisorydir --event ${INSTANCE}_${advisory}_$ensemble \
			--csdate $coldstarttime --getlat $MIDLAT --getlon $MIDLON --maxdist 3.0 \
			--plotstart $plotstart --plotend $plotend $offsetopt
		
		

#	fi
done

exit

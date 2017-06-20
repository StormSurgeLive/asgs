#!/bin/sh -vx
# -vx


# ECHO All the script call information into the unique directory for this data run
echo "${0} ${@}" > whatwasrun.txt 

while [[ $# > 1 ]]
do
        key=$1
        case $key in
                --adcircrundir)
                ADCIRCRUNDIR="$2"
                shift
                ;;
		--rtmadir)
		RTMADIR="$2"
		shift
		;;	
		--hrrrdir)
		HRRRDIR="$2"
		shift
		;;
                --event)
                EVENT="$2"
                shift
                ;;
                --csdate)
                CSDATE="$2"
                shift
                ;;
                --hstime)
                HSTIME="$2"
                shift
                ;;
		--getlat)
		GETLAT="$2"
		shift
		;;
		--getlon)
		GETLON="$2"
		shift
		;;
		--maxdist)
		MAXDIST="$2"
		shift
		;;
                --plotstart)
                PLOTSTART="$2"
                shift
                ;;
		--plotend)
		PLOTEND="$2"
		shift
		;;
		--offsetfile)
		OFFSETFILE="$2"
		shift
		;;
                
                
#               ;;
        esac
        shift
done

#event=$1
event=$EVENT
analysis=nos
topdir=$HOME/meteo/obs/nos
templatedir=$topdir/plots
plotdir=$topdir/$event/plots

coldstartdate=$CSDATE

yyyystart=`echo $PLOTSTART | cut -c 1-4`
mmstart=`echo $PLOTSTART | cut -c 5-6`
ddstart=`echo $PLOTSTART | cut -c 7-8`
yyyyend=`echo $PLOTEND | cut -c 1-4`
mmend=`echo $PLOTEND | cut -c 5-6`
ddend=`echo $PLOTEND | cut -c 7-8`
#-------------------------------------------------------------------#
# For offset calcs
#need coldstartdir
#need coldstart (tides only) start and end dates
# ----------------------------------------------------------------- #


adcircrundir=$ADCIRCRUNDIR
rtmadir=$RTMADIR
hrrrdir=$HRRRDIR
# fema R4 tides adcircrundir=/home/qrisq/adcirc/event/asgs3083/initialize/hindcast
getlat=$GETLAT
getlon=$GETLON
maxdist=$MAXDIST

stationlist=noslist_$event.txt


#make event directory
if [ ! -d $event ]; then
	mkdir $event
# move the program call with options file to the event specific directory
	mv $topdir/whatwasrun.txt $event/. 
else
	mv $topdir/whatwasrun.txt $event/.
fi

# make directory for event plots
if [ ! -d $plotdir ]; then
	mkdir $plotdir
fi


#run perl script to find event data, and create text list of stations

if [ -z "$OFFSETFILE" ]; then
	perl nos-create.pl --getlat $getlat --getlon $getlon --maxdist $maxdist \
		--event $event --startdate $PLOTSTART --enddate $PLOTEND
	offsetyn=n
else
	offsetfile=$OFFSETFILE
	if [ ! -e $offsetfile ]; then
		perl nos-predictions.pl --getlat $getlat --getlon $getlon --maxdist $maxdist \
			--event $event --startdate $PLOTSTART --enddate $PLOTEND
		offsetyn=n
	else
		perl nos-create.pl --getlat $getlat --getlon $getlon --maxdist $maxdist \
			--event $event --startdate $PLOTSTART --enddate $PLOTEND
		offsetyn=y
	fi
fi

#CD into the event directory where the station data exists
cd $topdir/$event

## move the program call with options file to the event specific directory
#mv $topdir/whatwasrun.txt . 

# Download Data
if  [[ -n "$RTMADIR" && -n "$HRRRDIR" ]];  then
	rtmadir=$RTMADIR
	hrrrdir=$HRRRDIR
	template=nos_template_offset_hrrrrtma.gp
	perl $topdir/station_transpose.pl --filetotranspose elevation --controlfile $rtmadir/fort.15 \
			--stationfile $rtmadir/fort.61 --format space --coldstartdate $coldstartdate \
			--gmtoffset 0 --timezone GMT --units si
	mv $rtmadir/fort.61_transpose.txt $topdir/$event/fort.61_transpose_rtma.txt
	perl $topdir/station_transpose.pl --filetotranspose elevation --controlfile $hrrrdir/fort.15 \
			--stationfile $hrrrdir/fort.61 --format space --coldstartdate $coldstartdate \
			--gmtoffset 0 --timezone GMT --units si
	mv $hrrrdir/fort.61_transpose.txt $topdir/$event/fort.61_transpose_hrrr.txt
	fort61sites=`head -2 fort.61_transpose_hrrr.txt | tail -1`
else
	template=nos_template_offset.gp
	perl $topdir/station_transpose.pl --filetotranspose elevation --controlfile $adcircrundir/fort.15 \
		--stationfile $adcircrundir/fort.61 --format space --coldstartdate $coldstartdate \
		--gmtoffset 0 --timezone GMT --units si
	mv $adcircrundir/fort.61_transpose.txt $topdir/$event/.
	fort61sites=`head -2 fort.61_transpose.txt | tail -1`
fi

#loop through the file names in the makenos.list created by perl script.
for file in `cat makenos.list`
do
	exit_code=0
	sed -i 's/,/ /g' $file
	basename=`basename $file .txt`
	ID=`basename $file | cut -d '-' -f1`


# Find the correct column that data for the NOS site is located in the fort.61_transpose.txt
	count=1
	topcount=300
	while [ $count -lt $topcount ]
	do 
#		station61=`echo $fort61sites | cut -d '"' -f$count`
		station61=`echo $fort61sites | awk -v var=$count -F '" "' '{print $var}'`
#		echo $count
#		echo $station61
#		echo $ID
# essential echo
		echo $station61 | grep $ID
		status=$?
		if [ $status -eq 0 ] ; then
#			echo "doing sed"
			column=`expr $count + 3`
#			sed -i s/sedcountsed/$count/g $basename.gp
			count=9999
		else
			count=`expr $count + 1` 
		fi
	done
#	sitename=`grep $ID $stationlist | awk '{print $1}'`



# If offset is "n" then, and offset file needs to be generated	
	if [[ "$offsetyn" == "n" && -n $OFFSETFILE ]]; then

# Get average values from month long predicted tides at the NOS location
		predictfile=`grep $ID makeoffset.list`
		sed -i 's/,/ /g' $predictfile
		predictavg=`awk '{ total += $3; count++ } END { print total/count }' $predictfile`

# Calculate the average value for the column of data in the fort.61 transpose file.
		adcircavg=-99999
#		adcircavg=`awk '{ total += $'$column'; count++ } END { print total/count }' fort.61_transpose.txt`
		adcircavg=`grep -v '#' fort.61_transpose.txt | awk '{ total += $'$column'; count++ } END { print total/count }'`
		
# Now calculate the offset ADCIRC - NOS-Predict
		offset=`echo "( $adcircavg - $predictavg )" | bc`
		
		

# BPJ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Need to test if offset greater than 1.0? Some other small value? If lt 100.0?
#  currently skewed on some sites by being dry for either part or full duration.

		absoffset=${offset/-/}
		absoffset=`expr $absoffset/1 | bc`

		if [ $absoffset -lt 5 ]; then			
# Insert data into the offset file
			echo $ID $offset $adcircavg $predictavg >> $offsetfile
		else
			offset="-99999"
			echo $ID $offset $adcircavg $predictavg >> $offsetfile
		fi
	
			
	elif [[ "$offsetyn" == "n" && -z $OFFSETFILE ]]; then
		offset="0.0"
	else	
		offset=`grep $ID $offsetfile | tail -1 | awk '{print $2}'`
#		if [ $offset -eq "-99999" ]; then
#			offset="0.0"
#		fi	
	fi
	
	if [ "$offset" == "-99999" ]; then
		offset="0.0"
	fi	

	

	sitename=`grep $ID $stationlist | cut -d ',' -f1`
	
#	analysis=ADCIRC

	if [ $status -eq 0 ]; then
# Remove Header from NOS data file
		sed -i 1d $file

# Missing data has a missing data column, so sed out the flag, and
# replace it with a "-99999", which is read by nos-template.gp as missing data.
		sed -i 's/  1 1 1 1 p/-99999/g' $file

		
		cp $templatedir/$template $basename.gp
		sed -i s:sedtitlesed:"${ID}-${sitename}":g $basename.gp
		sed -i s/sedanalysissed/$analysis/g $basename.gp
		sed -i s/sedcountsed/$column/g $basename.gp
		sed -i s/sedoffsetsed/$offset/g $basename.gp
		sed -i s/sedyyyystartsed/$yyyystart/g $basename.gp
		sed -i s/sedmmstartsed/$mmstart/g $basename.gp
		sed -i s/sedddstartsed/$ddstart/g $basename.gp
		sed -i s/sedyyyyendsed/$yyyyend/g $basename.gp
		sed -i s/sedmmendsed/$mmend/g $basename.gp
		sed -i s/sedddendsed/$ddend/g $basename.gp
		if [ "$template" == "nos_template_offset_hrrrrtma.gp" ]; then
			sed -i s/sedpngfilesed/${basename}.png/g $basename.gp
			sed -i s/sedanlrtmafilesed/fort.61_transpose_rtma.txt/g $basename.gp
			sed -i s/sedanlhrrrfilesed/fort.61_transpose_hrrr.txt/g $basename.gp
		else
			sed -i s/sedpsfilesed/${basename}.ps/g $basename.gp
			sed -i s/sedanltxtfilesed/fort.61_transpose.txt/g $basename.gp
		fi
		sed -i s/sedobtxtfilesed/$basename.txt/g $basename.gp
		
	
		gnuplot $basename.gp
		
		if [ "$template" == "nos_template_offset.gp" ]; then
			convert -trim -rotate 90 -density 120 -border 15 -bordercolor white +antialias \
				${basename}.ps ${basename}.png
		fi
		mv ${basename}.png $plotdir/.
	fi
done

exit

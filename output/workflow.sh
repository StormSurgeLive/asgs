#!/bin/bash
SCRIPTDIR=~/asgs/branches/2014stable
DATAPATH=~/projects/BizDev/Donan/20181115/Florence
TEMPLATEPATH=~/projects/BizDev/Donan/20181115
STATIONFILE=$DATAPATH/FlorenceLocationsforSeahorse.txt
TZONE=EDT
GMTOFFSET=4
STATIONLABEL=std
#-----------------------------------------------------
#-----------------------------------------------------
#  E X T R A C T    W A T E R   C U R R E N T    D A T A 
#-----------------------------------------------------
# extract water current at project locations 
$SCRIPTDIR/output/pullStationTimeSeries.x --netcdf --datafile fort.64.nc \
   --stationfile $STATIONFILE --outputfile fort.62
#
# max water current and time of occurrence
$SCRIPTDIR/output/pullStationTimeSeries.x --netcdf --datafile maxvel.63.nc \
   --stationfile $STATIONFILE --outputfile max.62
#
#-----------------------------------------------------
#  R E F O R M A T   W I N D    D A T A 
#-----------------------------------------------------
# water current time series
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose velocity \
   --stationfile $STATIONFILE --datafile fort.62 --gmtoffset $GMTOFFSET \
   --timezone $TZONE --runproperties run.properties \
   --stationlabel std \
   --reformattedfile vel.dat \
   --vectoroutput compassdegrees --units fps
#
# max water current
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose maxvelocity \
   --stationfile $STATIONFILE --datafile max.62 \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL --units fps \
   --reformattedfile max_vel.dat
#
# max water current time of occurrence
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose timemaxvelocity \
   --stationfile $STATIONFILE --datafile max.62 --format comma \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL \
   --reformattedfile time_max_vel.dat
#
#-----------------------------------------------------
#  C R E A T E   W A T E R   C U R R E N T   P L O T   F I L E S 
#-----------------------------------------------------
# create water current gnuplot files 
perl ~/asgs/branches/2014stable/output/autoplot.pl --fileToPlot vel.dat \
 --plotType velocity \
 --plotDir $PWD --template $TEMPLATEPATH/vel.gp --timezone EDT --stormname Florence \
 --enstorm Track --advisory hindcast --legend plottype --units fps \
 --minmaxfile max_vel.dat --timeminmaxfile time_max_vel.dat \
 --gpscriptname stationid

#   
#-----------------------------------------------------
#-----------------------------------------------------
#  E X T R A C T    W A V E   H E I G H T   D A T A 
#-----------------------------------------------------
#
# extract wave heights at project locations
$SCRIPTDIR/output/pullStationTimeSeries.x --netcdf --datafile swan_HS.63.nc \
   --stationfile $STATIONFILE --outputfile swan_HS.61
#
# max wave height and time of occurrence
$SCRIPTDIR/output/pullStationTimeSeries.x --netcdf --datafile swan_HS_max.63.nc \
   --stationfile $STATIONFILE --outputfile swan_HS_max.61
#
#-----------------------------------------------------
#  R E F O R M A T   W A V E   H E I G H T   D A T A  
#-----------------------------------------------------
# wave height time series
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose significantwaveheight \
   --stationfile $STATIONFILE --datafile swan_HS.61 \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel std --units ft \
   --reformattedfile hs.dat
#
# max wave height 
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose maxsignificantwaveheight \
   --stationfile $STATIONFILE --datafile swan_HS_max.61 \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL --units ft \
   --reformattedfile max_hs.dat
#
# max wave height time of occurrence
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose timemaxsignificantwaveheight \
   --stationfile $STATIONFILE --datafile swan_HS_max.61 --format comma \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL \
   --reformattedfile time_max_hs.dat
#
#-----------------------------------------------------
#     C R E A T E   G N U P L O T   F I L E S 
#  F O R   S I G N I F I C A N T   W A V E   H E I G H T
#-----------------------------------------------------
# create significant wave height gnuplot files 
perl ~/asgs/branches/2014stable/output/autoplot.pl --fileToPlot hs.dat \
 --plotType significantwaveheight \
 --plotDir $PWD --template $TEMPLATEPATH/wave.gp --timezone EDT --stormname Florence \
 --enstorm Track --advisory hindcast --legend plottype --units ft \
 --minmaxfile max_hs.dat --timeminmaxfile time_max_hs.dat \
 --gpscriptname stationid   
#
#
#-----------------------------------------------------
#-----------------------------------------------------
#  E X T R A C T    W I N D    D A T A 
#-----------------------------------------------------
# extract wind velocity at project locations 
$SCRIPTDIR/output/pullStationTimeSeries.x --netcdf --datafile wind10m.fort.74.nc \
   --stationfile $STATIONFILE --outputfile wind10m.fort.72
#
# max wind speed and time of occurrence
$SCRIPTDIR/output/pullStationTimeSeries.x --netcdf --datafile wind10m.maxwvel.63.nc \
   --stationfile $STATIONFILE --outputfile wind10m.max.72
#
#-----------------------------------------------------
#  R E F O R M A T   W I N D    D A T A 
#-----------------------------------------------------
#
# wind time series
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose windvelocity \
   --stationfile $STATIONFILE --datafile wind10m.fort.72 --gmtoffset $GMTOFFSET \
   --timezone $TZONE --runproperties run.properties \
   --stationlabel std \
   --reformattedfile wind10m.dat \
   --vectoroutput compassdegrees --units mph --averagingperiod 1min
#
# max wind speed 
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose maxwindvelocity \
   --stationfile $STATIONFILE --datafile wind10m.max.72 \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL --units mph --averagingperiod 1min \
   --reformattedfile max_wind10m.dat
#
# max wind speed time of occurrence
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose timemaxwindvelocity \
   --stationfile $STATIONFILE --datafile wind10m.max.72 --format comma \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL \
   --reformattedfile time_max_wind10m.dat
#
#-----------------------------------------------------
#  C R E A T E   W I N D   P L O T   F I L E S 
#-----------------------------------------------------
#
# create wind gnuplot files 
perl ~/asgs/branches/2014stable/output/autoplot.pl --fileToPlot wind10m.dat \
 --plotType windvelocity --every 5 \
 --plotDir $PWD --template $TEMPLATEPATH/wind.gp --timezone EDT --stormname Florence \
 --enstorm Track --advisory hindcast --legend plottype --units mph \
 --minmaxfile max_wind10m.dat --timeminmaxfile time_max_wind10m.dat \
 --gpscriptname stationid
#
#
#-----------------------------------------------------
#-----------------------------------------------------
#  E X T R A C T   E L E V A T I O N   D A T A 
#-----------------------------------------------------
# water surface elevation at project locations as well as bathytopo 
$SCRIPTDIR/output/pullStationTimeSeries.x --netcdf --datafile fort.63.nc \
   --stationfile $STATIONFILE --outputfile fort.61
#
# max water surface elevation and time of occurrence
$SCRIPTDIR/output/pullStationTimeSeries.x --netcdf --datafile maxele.63.nc \
   --stationfile $STATIONFILE --outputfile max.61
#
#-----------------------------------------------------
#  R E F O R M A T   E L E V A T I O N   D A T A 
#-----------------------------------------------------
#
# water surface elevation
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose elevation \
   --stationfile $STATIONFILE --datafile fort.61 \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL --units ft \
   --reformattedfile wse.dat
#
# max water surface elevation
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose maxelevation \
   --stationfile $STATIONFILE --datafile max.61 \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL --units ft \
   --reformattedfile max_wse.dat
#
# max water surface elevation time of occurrence
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose timemaxelevation \
   --stationfile $STATIONFILE --datafile time_max.61 --format comma \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL \
   --reformattedfile time_max_wse.dat
#
# mesh bathytopo
perl $SCRIPTDIR/output/station_transpose.pl --filetotranspose bathytopo \
   --stationfile $STATIONFILE --datafile station_bathytopo.61 \
   --gmtoffset $GMTOFFSET --timezone $TZONE --runproperties run.properties \
   --stationlabel $STATIONLABEL --units ft \
   --reformattedfile bathytopo.dat
#
# TODO: compute duration(s) of exposure to water of a certain height 
# TODO: lookup vdatum conversions
#
#-----------------------------------------------------
#   C R E A T E   W S E   G N U P L O T   F I L E S 
#-----------------------------------------------------
#
# create water surface elevation gnuplot files 
perl ~/asgs/branches/2014stable/output/autoplot.pl --fileToPlot wse.dat \
 --plotType elevation \
 --plotDir $PWD --template $TEMPLATEPATH/wse.gp --timezone EDT --stormname Florence \
 --enstorm Track --advisory hindcast --datum MSL --legend plottype --units english \
 --minmaxfile max_wse.dat --timeminmaxfile time_max_wse.dat \
 --bathytopo bathytopo.dat --gpscriptname stationid
#
#----------------------------------------------------------------------
#
#-----------------------------------------------------
#       C R E A T E   P L O T S
#-----------------------------------------------------
# create water surface elevation plots in postscript format
for file in `ls *.gp`; do
   echo "gnuplot $file"
   gnuplot $file
done
#
# convert postscript plots to png
for file in `ls *.ps`; do
   convertedFile=`basename $file .ps`.png
   echo "$file $convertedFile"
   convert -flatten -rotate 90 $file $convertedFile
done
#----------------------------------------------------------------------

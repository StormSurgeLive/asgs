datacollectandsurffitV2.m

Main code, calls all the others. 
Sets constants related to the stations whose measured data will be downloaded 
and the date range.  
Sets parameters and constants that control how the OI surface is constructed, the output file format, etc

calls functions to compute the oi surface:

executed with the following command line:

matlab -nodisplay -nosplash -nodesktop -r "try, run('./datacollectandsurffitV2.m'), catch me, fprintf('%s / %s\n',me.identifier,me.message), end, exit"

Something else to try, with command line options:

matlab -nodisplay -nosplash -nodesktop -r "dodownload=1; try, run('./datacollectandsurffitV2.m'), catch me, fprintf('%s / %s\n',me.identifier,me.message), end, exit"

# file options
offsetOptions="commandLineOptions=true;"
offsetOptions="$offsetOptions refwlmode=2; writeoutfil=63; filrefwl='20.adcircAvg.dat'; filstations='offset_source_stations.txt';"
# cold start date/time
offsetOptions="$offsetOptions csyear=2016; csmonth=8; csday=29; cshour=12; csmin=0; cssec=0;"
# time period of interest
offsetOptions="$offsetOptions timesecStart=2902188.0; timesecEnd=2991600.0;"
# execute offset surface generator with previously defined options
matlab -nodisplay -nosplash -nodesktop -r "${offsetOptions} try, run('./offsetSurfaceGen.m'), catch me, fprintf('%s / %s\n',me.identifier,me.message), end, exit"

on hatteras:

module load matlab/2016b

optimalInterp.m

Interpolates a scalar field between two sets of point locations. 

p_poly_dist.m

Find minimum distances from points to a polyline or to a closed polygon.

readfort14.m

Reads adcirc mesh file (Hill/Asher).

RegularizeData3D.m

Produces a smooth 3D surface from scattered input data. 


Scripting:

# computing station averages of adcirc data

# for 14x M2 tidal cycles!
avgPeriod=625884
advisory=23 ; ts=2430516; te=3056400 ; while [[ $advisory -lt 48 ]]; do ~/asgs/branches/da/output/stationProcessor.x --datafile fort.61.nc --timesec-end $te --timesec-start $ts --stationfile ~/offset_source_stations.txt --strict-time-range ; mv processedStations.dat ${advisory}.14x.adcircAvg.dat ; ts=`expr $ts + 21500` ; te=`expr $te + 21500` ; advisory=`expr $advisory + 1` ; done

# for 2x M2 tidal cycles
avgPeriod=89412
advisory=23 ; ts=2966988; te=3056400 ; while [[ $advisory -lt 48 ]]; do ~/asgs/branches/da/output/stationProcessor.x --datafile fort.61.nc --timesec-end $te --timesec-start $ts --stationfile ~/offset_source_stations.txt --strict-time-range ; mv processedStations.dat ${advisory}.adcircAvg.dat ; ts=`expr $ts + 21500` ; te=`expr $te + 21500` ; advisory=`expr $advisory + 1` ; done

# manual processing of adcirc data after computing averages
# 2x m2 tidal cycles
advisory=37; while [[ advisory -lt 48 ]]; do echo $advisory ; awk '$1!="#" && $1!=8662245 { print $5 }' ${advisory}.adcircAvg.dat  > col5.filtered.${advisory}.adcircAvg.dat ; advisory=`expr $advisory + 1`; done

# 2x m2 tidal cycles, cape fear river station removed
advisory=44; while [[ advisory -lt 48 ]]; do echo $advisory ; awk '$1!="#" && $1!=8662245 && $1!=8658120 { print $5 }' ${advisory}.adcircAvg.dat  > col5.filtered.${advisory}.nocfr.adcircAvg.dat ; advisory=`expr $advisory + 1`; done

# 2x m2 tidal cycles, springmaid pier (myrtle beach) removed
advisory=44; while [[ advisory -lt 48 ]]; do echo $advisory ; awk '$1!="#" && $1!=8662245 && $1!=8661070 { print $5 }' ${advisory}.adcircAvg.dat  > col5.filtered.${advisory}.nosmp.adcircAvg.dat ; advisory=`expr $advisory + 1`; done

# 14x m2 tidal cycles
advisory=45; while [[ advisory -lt 48 ]]; do echo $advisory ; awk '$1!="#" && $1!=8662245 { print $5 }' ${advisory}.14x.adcircAvg.dat  > col5.filtered.${advisory}.14x.adcircAvg.dat ; advisory=`expr $advisory + 1`; done

# old
advisory=23 ; cp ${advisory}.adcircAvg.dat filtered.${advisory}.adcircAvg.dat
vim filtered.${advisory}.adcircAvg.dat
awk '{ print $5 }' filtered.${advisory}.adcircAvg.dat  > col5.filtered.${advisory}.adcircAvg.dat

# generate surface
advisory=34 ; startSeconds=3204588 ; stopSeconds=3294000 ; while [[ $advisory -lt 48 ]]; do echo $advisory ; ln -s /projects/ncfs/opendap/data/tc/matthew/00/hsofs/hatteras.renci.org/matthsofsmsl/gahm/col5.filtered.${advisory}.adcircAvg.dat . ; matlab -nodisplay -nosplash -nodesktop -r "startSeconds=${startSeconds}; stopSeconds=${stopSeconds}; adv=${advisory}; try, run('./offsetSurfaceGen.m'), catch me, fprintf('%s / %s\n',me.identifier,me.message), end, exit" ; cp oi_surface.dat ${advisory}.oi.fort.63 ; bzip2 ${advisory}.oi.fort.63 ; startSeconds=`expr $startSeconds + 21600`; stopSeconds=`expr $stopSeconds + 21600`; advisory=`expr $advisory + 1` ; done 

# generate surface without cape fear river
advisory=44 ; startSeconds=3420588 ; stopSeconds=3510000 ; while [[ $advisory -lt 48 ]]; do echo $advisory ; ln -s /projects/ncfs/opendap/data/tc/matthew/00/hsofs/hatteras.renci.org/matthsofsmsl/gahm/col5.filtered.${advisory}.nocfr.adcircAvg.dat . ; matlab -nodisplay -nosplash -nodesktop -r "startSeconds=${startSeconds}; stopSeconds=${stopSeconds}; adv=${advisory}; try, run('./offsetSurfaceGen.m'), catch me, fprintf('%s / %s\n',me.identifier,me.message), end, exit" ; cp oi_surface.dat ${advisory}.nocfr.oi.fort.63 ; bzip2 ${advisory}.nocfr.oi.fort.63 ; startSeconds=`expr $startSeconds + 21600`; stopSeconds=`expr $stopSeconds + 21600`; advisory=`expr $advisory + 1` ; done 

# generate surface without springmaid pier
advisory=44 ; startSeconds=3420588 ; stopSeconds=3510000 ; while [[ $advisory -lt 48 ]]; do echo $advisory ; ln -s /projects/ncfs/opendap/data/tc/matthew/00/hsofs/hatteras.renci.org/matthsofsmsl/gahm/col5.filtered.${advisory}.nosmp.adcircAvg.dat . ; matlab -nodisplay -nosplash -nodesktop -r "startSeconds=${startSeconds}; stopSeconds=${stopSeconds}; adv=${advisory}; try, run('./offsetSurfaceGen.m'), catch me, fprintf('%s / %s\n',me.identifier,me.message), end, exit" ; cp oi_surface.dat ${advisory}.nosmp.oi.fort.63 ; bzip2 ${advisory}.nosmp.oi.fort.63 ; startSeconds=`expr $startSeconds + 21600`; stopSeconds=`expr $stopSeconds + 21600`; advisory=`expr $advisory + 1` ; done 

# 14x m2
avgPeriod=625884
advisory=45 ; startSeconds=2668116 ; stopSeconds=3294000 ; while [[ $advisory -lt 48 ]]; do echo $advisory ; ln -s /projects/ncfs/opendap/data/tc/matthew/00/hsofs/hatteras.renci.org/matthsofsmsl/gahm/col5.filtered.${advisory}.14x.adcircAvg.dat . ; matlab -nodisplay -nosplash -nodesktop -r "startSeconds=${startSeconds}; stopSeconds=${stopSeconds}; adv=${advisory}; try, run('./offsetSurfaceGen.m'), catch me, fprintf('%s / %s\n',me.identifier,me.message), end, exit" ; cp oi_surface.dat ${advisory}.14x.oi.fort.63 ; bzip2 ${advisory}.14x.oi.fort.63 ; startSeconds=`expr $startSeconds + 21600`; stopSeconds=`expr $stopSeconds + 21600`; advisory=`expr $advisory + 1` ; done 

# generate just one surface

advisory=44 ; startSeconds=3420588 ; stopSeconds=3510000 ; ln -s /projects/ncfs/opendap/data/tc/matthew/00/hsofs/hatteras.renci.org/matthsofsmsl/gahm/col5.filtered.${advisory}.adcircAvg.dat . ; matlab -nodisplay -nosplash -nodesktop -r "startSeconds=${startSeconds}; stopSeconds=${stopSeconds}; adv=${advisory}; try, run('./offsetSurfaceGen.m'), catch me, fprintf('%s / %s\n',me.identifier,me.message), end, exit" ; cp oi_surface.dat ${advisory}.oi.fort.63 ; bzip2 ${advisory}.oi.fort.63 

# download and visualize

advisory=31 ; while [[ $advisory -lt 48 ]]; do scp ncfs@ht1.renci.org:/home/ncfs/asgs/branches/da/input/data_assimilation/${advisory}.oi.fort.63.bz2 . ; bunzip2 ${advisory}.oi.fort.63.bz2 ; ~/asgs/2014stable/output/adcirc2netcdf.x --datafile ${advisory}.oi.fort.63 --defaultfilename fort.63 --meshfile ./hsofs.14 ; ~/asgs/2014stable/output/generateXDMF.x --datafile ${advisory}.oi.fort.63.nc ; advisory=`expr $advisory + 1`; done 

# download and visualize 14x m2 cycles

advisory=45 ; while [[ $advisory -lt 48 ]]; do scp ncfs@ht1.renci.org:/home/ncfs/asgs/branches/da/input/data_assimilation/${advisory}.14x.oi.fort.63.bz2 . ; bunzip2 ${advisory}.14x.oi.fort.63.bz2 ; ~/asgs/2014stable/output/adcirc2netcdf.x --datafile ${advisory}.14x.oi.fort.63 --defaultfilename fort.63 --meshfile ./hsofs.14 ; ~/asgs/2014stable/output/generateXDMF.x --datafile ${advisory}.14x.oi.fort.63.nc ; advisory=`expr $advisory + 1`; done

# download and visualize 2x m2 cycles without cape fear river

advisory=44 ; while [[ $advisory -lt 48 ]]; do scp ncfs@ht1.renci.org:/home/ncfs/asgs/branches/da/input/data_assimilation/${advisory}.nocfr.oi.fort.63.bz2 . ; bunzip2 ${advisory}.nocfr.oi.fort.63.bz2 ; ~/asgs/2014stable/output/adcirc2netcdf.x --datafile ${advisory}.nocfr.oi.fort.63 --defaultfilename fort.63 --meshfile ./hsofs.14 ; ~/asgs/2014stable/output/generateXDMF.x --datafile ${advisory}.nocfr.oi.fort.63.nc ; advisory=`expr $advisory + 1`; done

# download and visualize 2x m2 cycles without springmaid pier

advisory=44 ; while [[ $advisory -lt 48 ]]; do scp ncfs@ht1.renci.org:/home/ncfs/asgs/branches/da/input/data_assimilation/${advisory}.nosmp.oi.fort.63.bz2 . ; bunzip2 ${advisory}.nosmp.oi.fort.63.bz2 ; ~/asgs/2014stable/output/adcirc2netcdf.x --datafile ${advisory}.nosmp.oi.fort.63 --defaultfilename fort.63 --meshfile ./hsofs.14 ; ~/asgs/2014stable/output/generateXDMF.x --datafile ${advisory}.nosmp.oi.fort.63.nc ; advisory=`expr $advisory + 1`; done




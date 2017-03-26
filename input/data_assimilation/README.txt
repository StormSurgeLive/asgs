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

optimalInterp.m

Interpolates a scalar field between two sets of point locations. 

p_poly_dist.m

Find minimum distances from points to a polyline or to a closed polygon.

readfort14.m

Reads adcirc mesh file (Hill/Asher).

RegularizeData3D.m

Produces a smooth 3D surface from scattered input data. 


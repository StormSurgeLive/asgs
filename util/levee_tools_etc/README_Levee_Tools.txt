Here are some Fortran programs I've used to deal with weirheights.  

EXTRACT_BARLANHT.f - reads a fort.14 and outputs an xyz file of the 
IBTYPE 24 and 23 boundary heights.

INSERT_WEIRHEIGHTS.f - takes a grid file and two xyz files for input 
(one that has higher priority than the other, you can just use a dummy
file for the lower priority if you want).  Then for each weirheight 
in the grid, if the weirheight is less than -99999, it replaces the
weirheight value with the z value of the closest point from the xyz 
file. The closest value must be closer than the two weir nodes are 
together for it to be replaced. It also works with the IBTYPE 23
boundaries.

CHKWEIRS.f - (uses the GRIDSTUFF module) 
this checks to make sure the nodal elevations aren't higher than the
weirheights (adjusts the node elevation if necessary) it also fills
in any weir or landbarrier height values that are still set to -99999
by linearly interpolating along the boundary. 

I believe all these programs now work with the IBTYPE 23 boundaries
as well as the IBTYPE 24 boundaries.

I use the programs in a few different ways:

1) if I just want to set the values for one weir, I open up the grid 
in SMS (or otherwise) set that boundary barrier height values to -99999
then run INSERT_WEIRHEIGHTS with the xyz data I want to replace it with, 
then run check_weirs_and_landBarrers to fill in missing values. 
Then run EXTRACT_BARLANHT to make sure it looks correct 
(or look at it in SMS)

2) to set the values for all weirs there are some lines you can 
uncomment in INSERT_WEIRHEIGHTS that will make it replace all the 
values with -99999.  

3)Sometimes I'll run EXTRACT_BARLANHT on the grid I'm working with 
to get a "lower priority dataset" then I'll run INSERT_WEIRHEIGHTS 
as in #2 using a different higher priority dataset(e.g. from RTK data).
this way you end up effective only replacing the values where you 
have newer/better data, while values at points where you don't have 
new data get preserved.  

email if you have questions or run into any problems ndill(AT)whgrp.com

-Nate Dill


ps. I've also included:

INSERT_GRID.f - (uses GRIDSTUFF.f module) will insert one grid into another
 see the source code for further description.

STATIONS2KML.f - (uses GRIDSTUFF.f module) makes a kml file showing a small
 cutoutof the grid around each specified station location. helps to identify
 if stations are in the proper place.

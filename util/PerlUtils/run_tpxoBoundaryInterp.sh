#!/bin/bash

GRID=/home/nate/cera/asgs/2014stable/input/meshes/naccs/NAC2014_R01_ClosedRivers.grd
TPXO=/work/nate/h_tpxo7.2
CONST="m2,s2,n2,k2,k1,o1,p1"
OUT=tidal_bnd_data.txt

perl tpxoBoundaryInterp.pl --gridfile $GRID --tpxofile $TPXO --constituents $CONST --outfile $OUT

#!/usr/bin/bash
#
# netcdf.sh: tell asgs where netcdf is installed based on Operator user id 
THIS=$PWD/netcdf.sh
case $USER in
   "jgflemin")
      export PATH=$PATH:${WORK}/asgs/bin
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${WORK}/asgs/lib
      ;;
   # add another asgs Operator:
   # "myuser")
   #     MYPATH etc
   #     ;;
   *)
      echo "ERROR: ${THIS}: Could not find ASGS Operator '$USER' to set PATH and LD_LIBRARY_PATH for netCDF."
      ;;
esac

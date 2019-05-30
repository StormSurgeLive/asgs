#!/usr/bin/bash
#
# netcdf.sh: tell asgs where netcdf is installed based on Operator user id 
THIS=$PWD/netcdf.sh
case $USER in
   "ncfs")
      # @jasonfleming : when building netcdf-fortran-4.2 on hatteras, 
      # had to use FFLAGS="-assume no2underscore" for some reason
      export PATH=$PATH:${HOME}/local/bin
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${HOME}/local/lib
      ;;
   # add another asgs Operator:
   # "myuser")
   #     MYPATH etc
   #     ;;
   *)
      echo "ERROR: ${THIS}: Could not find ASGS Operator '$USER' to set PATH and LD_LIBRARY_PATH for netCDF."
      ;;
esac

#!/usr/bin/bash
#
# gdal.sh: tell asgs where gdal is installed based on the Operator userid 
THIS=$PWD/gdal.sh
case $USER in 
   "jgflemin")
      GDAL_PATH=$WORK/asgs/gdal/bin
      export PATH=$PATH:$GDAL_PATH
      GDAL_LD_LIBRARY_PATH=$WORK/asgs/gdal/lib
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GDAL_LD_LIBRARY_PATH
      export GDAL_DATA=$WORK/asgs/gdal/lib
      ;;
   # add another asgs Operator:
   # "myuser")
   #     MYPATH etc
   #     ;;
   *)
      echo "ERROR: ${THIS}: Could not find ASGS Operator '$USER' to set PATH and LD_LIBRARY_PATH for GDAL."
      ;;
esac

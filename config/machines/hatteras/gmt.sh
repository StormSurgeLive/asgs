#!/usr/bin/bash
#
# gmt.sh: tell asgs where gmt is installed based on the Operator user ID 
THIS=$PWD/gmt.sh
case $USER in
   "ncfs")
      GMT_HOME=/home/ncfs/asgs/gmt/gmt-4.5.18
      export PATH=$PATH:${GMT_HOME}/bin
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${GMT_HOME}/lib
      ;;
   # add another asgs Operator:
   # "myuser")
   #     MYPATH etc
   #     ;;
   *)
      echo "ERROR: ${THIS}: Could not find ASGS Operator '$USER' to set PATH and LD_LIBRARY_PATH for GMT."
      ;;
esac

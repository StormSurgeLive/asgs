#!/usr/bin/bash
#
# fftw.sh: tell asgs where fftw is installed based on the Operator user ID 
THIS=$PWD/fftw.sh
case $USER in
   "ncfs")
      FFTW_HOME=/home/ncfs/asgs/fftw
      export PATH=$PATH:${FFTW_HOME}/bin
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${FFTW_HOME}/lib
      ;;
   # add another asgs Operator:
   # "myuser")
   #     MYPATH etc
   #     ;;
   *)
      echo "ERROR: ${THIS}: Could not find ASGS Operator '$USER' to set PATH and LD_LIBRARY_PATH for FFTW."
      ;;
esac

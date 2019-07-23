#!/usr/bin/bash
#
# imagemagick.sh: tell asgs where imagemagick is installed based on the Operator userid 
THIS=$PWD/imagemagick.sh
case $USER in 
   "jgflemin")
      IMAGEMAGICK_PATH=$HOME/local/bin
      export PATH=$IMAGEMAGICK_PATH:$PATH
      IMAGEMAGICK_LD_LIBRARY_PATH=$HOME/local/lib
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$IMAGEMAGICK_LD_LIBRARY_PATH
      ;;
   # add another asgs Operator:
   # "myuser")
   #     MYPATH etc
   #     ;;
   *)
      echo "ERROR: ${THIS}: Could not find ASGS Operator '$USER' to set PATH and LD_LIBRARY_PATH for Imagemagick."
      ;;
esac

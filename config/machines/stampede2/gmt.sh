#!/usr/bin/bash
#
# gmt.sh: tell asgs where gmt is installed based on the Operator user ID 
THIS=$PWD/gmt.sh
case $USER in
   "jgflemin")
      export PATH=/work/00976/jgflemin/stampede2/asgs/gmt/gmt-4.5.18/bin:$PATH
      ;;
   # add another asgs Operator:
   # "myuser")
   #     MYPATH etc
   #     ;;
   *)
      echo "ERROR: ${THIS}: Could not find ASGS Operator '$USER' to set PATH and LD_LIBRARY_PATH for GMT."
      ;;
esac

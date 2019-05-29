#!/usr/bin/bash
#
# openssl.sh: tell asgs where openssl is installed based on Operator user id 
THIS=$PWD/openssl.sh
case $USER in
   "jgflemin")
      export PATH=$PATH:/work/00976/jgflemin/lonestar/asgs/openssl/bin
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/work/00976/jgflemin/lonestar/asgs/openssl/lib
      ;;
   # add another asgs Operator:
   # "myuser")
   #     MYPATH etc
   #     ;;
   *)
      echo "ERROR: ${THIS}: Could not find ASGS Operator '$USER' to set PATH and LD_LIBRARY_PATH for openssl."
      ;;
esac

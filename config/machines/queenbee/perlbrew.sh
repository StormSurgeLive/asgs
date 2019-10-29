#!/usr/bin/bash
#
# perlbrew.sh: load up custom asgs perl environment 
THIS=$PWD/perlbrew.sh
case $USER in 
   "jgflemin")
      source ~/perl5/perlbrew/etc/bashrc
      ;;
   "mbilskie")
      source /project/mbilskie/perlbrew/etc/bashrc
      ;;
   # add another asgs Operator:
   # "myuser")
   #     MYPATH etc
   #     ;;
   *)
      echo "ERROR: ${THIS}: Could not find ASGS Operator '$USER' to set PATH and LD_LIBRARY_PATH for perlbrew."
      ;;
esac

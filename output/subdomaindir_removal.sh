#!/bin/bash
HOTTIFYPATH=$1
logFile="subdomaindir_removal.log"
instancedir=$PWD
for advisdir in `ls`; do
   # operate on advisory directories
   if [[ -d $advisdir ]]; then
      echo "advisdir is $advisdir" >> $instancedir/$logFile
      cd $advisdir 2>> $instancedir/$logFile
      # operate on different runs within the advisory directory
      for stormdir in `ls`; do
         if [[ -d $stormdir ]]; then
            echo "stormdir is $stormdir" >> $instancedir/$logFile 
            cd $stormdir 2>> $instancedir/$logFile
            # preserve the swan log file so we can see 
            if [[ -e ./PE0000/asgs_swan.prt ]]; then
               cp ./PE0000/asgs_swan.prt . 2>> $instancedir/$logFile
            fi
            # if it is a nowcast, archive the swan hotstart file if any
            if [[ $stormdir = nowcast ]]; then
               # construct fulldomain swan hotstart file and compress
               if [[ -e ./PE0000/swan.68 ]]; then
                  ${HOTTIFYPATH}/HottifySWAN.x <<EOF >> $instancedir/$logFile 2>&1 
1
swan.68
F
EOF
                  bzip2 swan.68 >> $instancedir/$logFile 2>&1
               fi
            fi
            # now delete the subdomain directories
            rm -rf PE* 2>> $instancedir/$logFile
            # switch back to advisory directory
            cd ..
         fi
      done
      # switch back to instance directory
      cd ..
      echo "next advisory" >> $instancedir/$logFile
   fi
done

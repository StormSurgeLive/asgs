#!/bin/bash

# makecat.sh
# Generates a plain-text matrix of directory names for ASGS/THREDDS data access.  
# If NetCDF Operators (nco) are available, this script will call nccopy/ncks to 
# chunk netCDF files.  This can substantially speed up remote access to large files. 
#
# Run this as:
#
# sh makecat.sh > catalog.tree
# 
# This can be put into cron with something like: 
#
# 0,15,30,45 * * * * $SP/makecat.sh > catalog.tree
#
# which will write a new catalog.tree file every 15 minutes.

# Brian Blanton
# Renaissance Computing Institute 
# The University of North Carolina at Chapel Hill
# Original: July 2012
#  updated to new TDS structure: 3 Sep 2019 
unset CDPATH

#regexp='^[-|+|0-9|.][.0-9]*$'
ChunkFort63Files=0
ChunkCommand='nccopy  -c node/5000  fort.63.nc fort.63.chunked.nc'
ChunkCommand2='ncks -4 -L 1  --cnk_dmn node,500  -O fort.63.nc fort.63.chunked.nc'
TcStartDate=`date +%s  --date=20200601`
NamStartDate=`date +%s --date=20200701`
IgnoreTc=0
IgnoreNam=0

if  [ $IgnoreTc -eq 1 ] ; then
	TcStartDate=`date +%s --date=21000101`
fi

# to ignore the daily NAM runs, and create a catalog file that only contains tropical events, set
# IgnoreNam to 1.  This will set NamStartDate to the year 2100.  Otherwise, set NamStartDate to the 
# date at which cataloging should begin.
if  [ $IgnoreNam -eq 1 ] ; then
	NamStartDate=`date +%s --date=21000101`
fi

rm -f temp*.txt

##############
# Functions
##############

MyDir()
{
   #find . -maxdepth 1 -type d -o -type l -not -path "*/.*/*" -not -name ".*" | sed "s/\.\///"   
   find . -maxdepth 1 -type d  -not -path "*/.*/*" -not -name ".*" | sed "s/\.\///"   
}

filesec()
{
   perl -e '$epoch = (stat("$ARGV[0]"))[9]; print "$epoch";'  "$1"
}

filetime()
{
   perl -e '$tt = print scalar(localtime($ARGV[0])),qq(\n);' "$1"
}

# The directory structure for ASGS/THREDDS has the following format/fields/names:
# StormName/AdvisoryNumber/AdcircGrid/Machine/Instance/EnsembleName
# GetEntries assumes this exists and builds a text matrix of the directory names:

GetEntries()
{
   storms=(`MyDir`)
   for storm in "${storms[@]}" ; do  
#     if [ $storm != "matthew" ] ; then continue; fi
     cd  $storm 
#     echo "storm = " $storm

     advs=(`MyDir`)
     for adv in "${advs[@]}" ; do  
       #echo "adv      = " "${adv:0:2}" , $adv
       #echo "+${adv:0:2}+"

#       if [ "${adv:0:2}" != "99" ] ; then continue; fi
#       echo "xyz:   $storm: +${adv:0:2}+"
       if [ "${adv:0:5}" == "shape" ] ; then continue; fi
       if [ "$storm" == "nam" ] ; then
         ThisSecs=`date +%s --date=${adv:0:8}`
       else
         ThisSecs="0"
       fi

       # this prevents the inclusion of nam runs older than $NamStartDate 
       if ( [[ "${#adv}" -lt 4  ]]  || [[ $ThisSecs -gt $NamStartDate ]] ) ; then
         cd  $adv

         grids=(`MyDir`)
         for grid in "${grids[@]}"; do 
            cd $grid
            
            machines=(`MyDir`)
            for machine in "${machines[@]}"; do 
              cd $machine
              
              instances=(`MyDir`)
              for instance in "${instances[@]}"; do 
                cd $instance

                enss=(`MyDir`)
                for ens in "${enss[@]}"; do 
                  cd $ens

                  if [ -e "maxele.63.nc" ] ; then
#                     echo "Processing    $storm : $adv : $grid : $instance : $ens ... "
                     fs=`filesec maxele.63.nc`
                     ft=`filetime $fs`
		     if ( [[  $fs -gt $TcStartDate  ]] ) ; then
                     	printf "%10s$%12s $ %12s $ %30s $ %30s $ %10s $ %24s $ %20s\n" $fs  $storm $adv $grid $machine $instance $ens "$ft"
	             fi
                  fi

                  if [ $ChunkFort63Files == "1" ] ; then
                     if [ -e "fort.63.nc" ] ; then
                       if [ ! -e "fort.63.chunked.nc" ] ; then
                         echo "Chunking $storm/$adv/$grid/$machine/$instance/$ens/fort.63.nc..."
                         if [ $storm = "nam" ] ; then
                           `$ChunkCommand2`
                         else
                           `$ChunkCommand`
                         fi
                       fi
                     fi
                  fi

                  cd ../  # back out of ensembles
                  
                done    # end (ensemble) 
                cd ../  # back out of instance
              
              done    # end (instance)
              cd ../  # back out of machine
              
            done    # end (machines)
            cd ../  # back out of grid
            
         done    # end (grids)
         cd ../  # back out of advisory
       
       fi      # end of nam/time if
     
     done    # end adv for
     cd ../  # back out of storm dir

   done    # end storm for

} # end of GetEntries function 


##############
# Main
##############

# generate the catalog list
GetEntries  > temp.txt

# sort on first column, which is the creation time in seconds
sort -r temp.txt > temp2.txt

# remove the first column
cut -d$ -f 2- temp2.txt > temp3.txt


# send catalog file contents to stdout.  
printf "%s\n" "--------------------------------------------------------------------------------------------------------------------------------------------------------------"
printf "%12s $ %12s $ %30s $ %30s $ %10s $ %24s $ %20s\n"  "Storms" "Advisories" "Grids" "Machines" "Instances" "Ensembles"  "CreateTime"
printf "%s\n" "--------------------------------------------------------------------------------------------------------------------------------------------------------------"
cat temp3.txt

rm -f temp*txt


#!/bin/bash
#set -o verbose
#set -o xtrace 


# Definitions and paths
MakeCatalog=1

TDSHOME="/projects/ncfs/opendap/data/2020"
ASGSHOME="/home/ncfs-dev/asgs/monitoring"

###functions
filesec()
{
   perl -e '$epoch = (stat("$ARGV[0]"))[9]; print "$epoch";'  "$1"
}

filetime()
{
   perl -e '$tt = print scalar(localtime($ARGV[0])),qq(\n);' "$1"
}
now()
{
	date --utc +%s
}

cd $TDSHOME

echo ' '

if [ "$MakeCatalog" == "1" ]  ; then 
    if [ -e "catalog.tree" ] ; then
        mv catalog.tree catalog.tree.old
    fi
    sh $ASGSHOME/makecat.sh  > catalog.tree

    # split catalog into nam and tc parts, for convenience
    sed -n '1,3p' catalog.tree > header
    sed '/nam/d' catalog.tree > catalog.tree.tc

#    sed -n '/nc_/p' catalog.tree.tc > temp 
#    cat temp >> header
#    mv header catalog.tree.tc-nc

    sed -n '/nam/p' catalog.tree > temp 
    cat temp >> header
    mv header catalog.tree.nam
    rm temp

else
	echo Skipping catalog generation/update
fi

# update ncml in "current" directories
#python $TDSHOME/make_ncml.py /projects/ncfs/opendap/data/2019/NCFS_CURRENT_TROPICAL /projects/ncfs/opendap/data/SSV-Ncml/ RenciAsgs_LatestTropical.ncml LatestTropical
#python $TDSHOME/make_ncml.py /projects/ncfs/opendap/data/NCFS_CURRENT_DAILY /projects/ncfs/opendap/data/SSV-Ncml/  RenciAsgs_LatestDaily.ncml  LatestDaily




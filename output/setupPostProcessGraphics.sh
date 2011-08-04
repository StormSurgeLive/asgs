#!/bin/sh
export NCFMPHOME=/data/ncfmp/ncfmp
#export LOG_DIR=/projects/storm_surge/ncfmp/Visualization/runs
export CURDIR=`pwd`
export BINDIR=/data/ncfmp/bin
export IMAGES_WWW_DIR=/home/kgamiel/production/apache-tomcat-6.0.14/webapps/ncfmpview
export NETCDFHOME=/opt/gmt/4.2.0/install/netcdf-3.6.2
export PATH=.:/opt/gmt/4.2.0/bin:$PATH
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$NETCDFHOME/lib
export PPDIR=/shared/apps/software-data/RenciGETools/trunk/src
export TRACKNAME=`basename $CURDIR`
export NETFEMDIR=/shared/apps/software-data/translationTools/trunk/NetFEM
export PREPROCESSBASE=/shared/apps/software-data/adcircRenderTools/precomputes
export GRIDDIR=/shared/apps/software-data/adcircRenderTools/grids
export ImageMagick=/usr/bin

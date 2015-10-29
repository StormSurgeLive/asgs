#!/bin/bash
#
# regionalMSLcalculator.sh
#
# This script calculates a regional mean sea level from tide
# predictions obtained from the NOAA CO-OPs datagetter api
#
# it uses the getCOOPSdata.pl Perl program to get the data
#
# It is initially setup to use stations along the Northern Gulf
# of Mexico coast and calculate the mean predicted water level
# relative to msl over the past fortnight.  
#
# It produces a log file which that gives some information on
# the stations considered, their indidual mean values, etc.
#
# It prints the final regional mean value to STDOUT 
#
# If you want it to, it will save csv files containing the
# tide data it downloaded and used from datagetter. note: 
# getCOOPS.pl strips the headerline off the csv file content
# provided by CO-Ops.
#
# It could be easily reconfigured to calculate means of other 
# products, use different datums, ue  other stations for different 
# regions, etc... see comments below
# 
#
# 
#######################################################################
# Copyright (C) 2015 Nathan Dill
# 
# This program  is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of the 
# License, or (at your option) any later version. 
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software 
# Foundation, Inc., 59 Temple Place - Suite 330,Boston, MA  02111-1307,
# USA.
######################################################################yy#                                       


######################################################################yy#                                       
#---------------------------------------------------------------------
# some upfront configuration
daysAgo=7               # number of days to go back for average msl 
daysAhead=7             # number of days to look forward for 
                         # daysAhead must be zero for observations

ndays=$((daysAgo+daysAhead))

# Here is a list of open coast stations for LA and vicinity
# Texas Point (8770822) - not currently having data 20151030
# Calcasieu Pass (8768094) 
# FreshwaterCanalLocks (8766072)
# Amerada Pass (8764227) 
# Port Fourchon (8762075)
# Grand Isle (8761724)
# Polots Station East (8760922)
# Shell Beach (8761305)
# Bay Waveland (8747437)
# Dauphin Island (8735180)
stations=(8770822 8768094 8766072 8764227 8762075 8761724 8760922 8761305 8747437 8735180)
staNames=("Texas Point" "Calcasieu Pass" "Freshwater Canal Locks" "Amerada Pass"  "Port Fourchon" "Grand Isle" "Pilots Station East" "Shell Beach"  "Bay Waveland"  "Dauphin Island")

# some settings for the datagetter form...
units=metric
timezone=gmt
product=predictions #or "water_level" for observed
datum=MSL

saveCSVfiles=false;  # if true it will save csv files of the tide data
                    # minus the initial headerline delievered by CO-OPs

#-------------------------------------------------------------------
####################################################################yy

# get starting date
beginDate=`date --utc --date="$daysAgo days ago" +%Y%m%d`

# get ending date
endDate=`date --utc --date="$daysAhead day" +%Y%m%d`

logfile=NG_msl_${product}_${beginDate}-${endDate}.log
echo "-*- calculating regional msl for `date` -*-" > $logfile
echo "Begin date: $beginDate" >> $logfile
echo "End date: $endDate" >> $logfile

# loop over the stations
mmsl=0
nsta=0
cnt=0
nrecords=$((ndays*240))
ngood=0
echo "stations must have at least $nrecords records to be considered in the regional mean" >> $logfile
for station in ${stations[@]} 
do 
   echo "---Getting data for ${staNames[$cnt]} $station" >> $logfile
   # get the data from NOAA relative to msl and use awk to calculate the average waterlevel 
   options="--station $station --begin $beginDate --end $endDate --product $product --datum $datum --units $units --timezone $timezone --format csv --out STDOUT"
   echo "   calling getCOOPSdata.pl with options:  $options" >> $logfile
  
   # here we get the data and calculate the mean for this station
   if "$saveCSVfiles"; then
      perl getCOOPSdata.pl $options > $station.csv
      msl_cnt=`awk -F , 'NR>1 {sum += $2; n++} END {print sum/n, n}' $station.csv`
   else
      msl_cnt=`perl getCOOPSdata.pl $options | awk -F, 'NR>1 {sum += $2; n++} END {print sum/n, n}'`
   fi

   MSL_CNT=($msl_cnt)
   msl=${MSL_CNT[0]}
   n=${MSL_CNT[1]}
   echo "   ${staNames[$cnt]} $station mean is $msl $units with $n values" >> $logfile

   #keep tally of the mean of the means
   if [ "$n" -lt "$nrecords" ]; then
     echo "   ${staNames[$cnt]} $station doesn't have enough records to be counted" >> $logfile
   else
     echo "   ${staNames[$cnt]} $station  will be counted" >> $logfile
     mmsl=`echo "$mmsl $msl" | awk '{print $1+$2}'` 
     ((ngood++))
     echo "   mmsl tally is $mmsl, ngood is $ngood" >> $logfile
   fi

   ((cnt++))

done

mmsl=`echo "$mmsl $ngood" | awk '{print $1/$2}'` 
echo "$ngood stations had enough records" >> $logfile
echo "regional mean is $mmsl $units $datum" >> $logfile
echo "$mmsl"




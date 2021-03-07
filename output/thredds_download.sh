#!/usr/bin/env bash
#----------------------------------------------------------------
# thredds_download.sh
#
# download scenario data from thredds
#----------------------------------------------------------------
# Copyright(C) 2021 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------
#http://tds.renci.org:8080/thredds/fileServer/2021/nam/2021020700/hsofs/hatteras.renci.org/hsofs-nam-bob-2021/nowcast/fort.61.nc
meteorology="nam"
server="tds.renci.org:8080"
mesh="hsofs"
hpcenv="hatteras.renci.org"
instancename="hsofs-nam-bob-2021"
files=( "fort.61.nc" "fort.15" "run.properties" )
scenarios=( "nowcast" "namforecast" )
for year in 2021; do 
    #echo "year is $year"
    for month in 01 02 ; do 
        day=1
        # find the number of days in the month (including leap days)
        month_days=`cal $month $year | egrep -v [a-z] | wc -w`
        while [[ $day -le $month_days ]]; do
            daystring=`printf "%02d" $day` 
            #echo "daystring is $daystring"
            for hour in 00 06 12 18 ; do 
                #echo "hour is $hour"
                for scenario in "${scenarios[@]}" ; do
                    #echo "scenario is $scenario"
                    dir="$year/$meteorology/$year$month$daystring$hour/$mesh/$hpcenv/$instancename/$scenario"
                    #echo "dir is $dir"
                    localdir="./$dir"
                    #echo "localdir is $localdir"
                    mkdir -p $localdir
                    for f in "${files[@]}" ; do
                        echo "f is $f"
                        url="http://$server//thredds/fileServer/$dir/$f"
                        echo "url is $url"
                        curl -O $url
                        # if this is a netcdf file, check to see if we downloaded a real netcdf file successfully
                        echo "checking to see if this is a netcdf file"
                        if [[ ${f##*.} == "nc" ]]; then
                            echo "this is a netcdf file"
                            l=`ncdump -k $f 2>&1`
                            echo $l
                            if [[ $l == *"NetCDF: Unknown file format"* ]]; then 
                                echo "unknown"
                                rm $f
                                break
                            fi
                        else
                            echo "not a netcdf file"
                        fi
                        # copy the file into a local directory
                        echo "moving $f to $localdir"
                        mv $f $localdir                           
                    done 
                done
            done    
            day=`expr $day + 1`
        done
    done
done
   
         
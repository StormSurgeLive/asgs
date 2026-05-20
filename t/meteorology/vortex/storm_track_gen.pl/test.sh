#!/bin/bash
#----------------------------------------------------------------
# test.sh: Driver script for testing storm_track_gen.pl
#----------------------------------------------------------------
# Copyright(C) 2026 Jason Fleming
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
rm *actual* run.properties fort.22 2> /dev/null # remove old test results
# standalone cleanup
if [[ $# -eq 1 && $1 == "clean" ]]; then
    exit
fi
# One Liners:
# 1. Collect diffs for failed tests:
# for f in $(./test.sh) ; do if [[ -e $f ]]; then echo $f ; diff ${f//actual/expected} $f ; fi ; done > diffs
# 2. Fix tests to reflect new expectations:
#   a. for f in $(ls input???.arg???.actual.*) ; do echo $f ; cp $f ${f//actual/expected} ; done
#   b. for f in $(ls single???.actual.*) ; do echo $f ; cp $f ${f//actual/expected} ; done
#   c. for f in $(ls track??.actual.*) ; do echo $f ; cp $f ${f//actual/expected} ; done
#   d. for f in $(ls branch??.actual.*) ; do echo $f ; cp $f ${f//actual/expected} ; done
# 3. Collect logs into a single file for bulk inspection:
# for f in $(ls *actual*.log); do echo $f ; cat $f ; done > logfiles
# 4. Collect run.properties into a single file for bulk inspection:
# for f in $(ls *actual*.run.properties); do echo $f ; cat $f ; done > runproperties
# 5. Add up total hours of simulation time for forecast ensemble tracks
#   a. fan ensemble: s=0; h=$(tail -n 1 track??.actual*.22 | awk 'BEGIN { FS="," } $1=="AL" { print $6 }' | sed 's/ //') ; for v in ${h[@]}; do s=$(($v + $s)) ; echo $s ; done
#   b. branching ensemble: s=0; h=$(tail -n 1 branch*.22 | awk 'BEGIN { FS="," } $1=="AL" { print $6 }' | sed 's/ //') ; for v in ${h[@]}; do s=$(($v + $s)) ; echo $s ; done
# 6. For downloading advisories and converting to track format for testing
#   a. cd $SCRIPTDIR/input/sample_advisories/2021 ; ../get_all.sh 9 2021 1 19 # get all advisories
#   b. ln -s 04.bal092021.dat bal092021.dat ; rm index-at.xml ; ln -s 04.092021.index-at.xml index-at.xml ;  perl $SCRIPTDIR/get_atcf.pl --storm 09 --year 2021 --ftpsite filesystem --fdir . --hdir . --rsssite filesystem --trigger rssembedded --adv 0 # extract advisory text
#   c. perl ${SCRIPTDIR}/nhc_advisory_bot.pl --input  al092021.fst.html --output al092021.fst --metadata forecast.properties # convert advisory text to ATCF format for use with storm_track_gen.pl
#
# For a 120 hour forecast period, a fan ensemble needs 2040 hours of simulation time
# but branching ensemble only needs 1320 hours (35% reduction)
# For a 72 hour forecast period, a fan ensemble needs 1224 hours of simulation time
# but branching ensemble only needs 504 hours (58% reduction)
#
#----------------------------------------------------------------
# Issue numbers are all https://github.com/StormSurgeLive/asgs
#
# Input set descriptions (all use al112017 IRMA):
# 001 BEST and OFCL files both present
# 002 Only BEST file is present
# 003 Only OFCL file is present
# 004 Neither BEST or OFCL file is present
# 005 run.properties file is not found or cannot be read
# 006 BEST ends after OFCL starts
# 007 BEST ends long before OFCL starts
# 008 BEST starts after OFCL starts
numInputSets=8   # number of sets of input data
#
# Properties from input set 001:
# forcing.tropicalcyclone.best.time.start : 2017082718
# forcing.tropicalcyclone.best.time.end : 2017090812
# forcing.tropicalcyclone.fcst.time.start : 2017090815
# forcing.tropicalcyclone.fcst.time.end : 2017091312
#
# define command line argument sets to be used for IRMA
# input sets defined above
declare -A argSets
# a001 missing coldstart date and hotstart time
argSets['a001']="--name testscenario --test"
# a002 missing coldstart date and hotstart time set to 0 seconds
argSets['a002']="--name testscenario --hotstartseconds 0 --test"
# a003 coldstartdate one day prior to BEST cycle just before forecast, hotstart time one day
argSets['a003']="--name testscenario --coldstartdate 2017090712 --hotstartseconds 86400 --test"
# a004 coldstartdate one day prior to BEST cycle just before forecast, hotstart time half day
argSets['a004']="--name testscenario --coldstartdate 2017090712 --hotstartseconds 43200 --test"
numArgSets=4     # number of sets of command line arguments
# SCRIPTDIR should be set if this test script is executed with
# the ASGS shell
ADVISDIR=$PWD
SCENARIODIR=$PWD
#
pass=0
fail=0
declare -a actualFails
#
for a in $(seq 1 $numArgSets) ; do
    argSetNumber=$(printf "%03d" $a)
    for i in $(seq 1 $numInputSets) ; do
        inputNumber=$(printf "%03d" $i)
        SYSLOG="input${inputNumber}.arg${argSetNumber}.actual.syslog.log"
        output=( fort.22 run.properties $SYSLOG )
        TEST=unit
        if [[ -e "input${inputNumber}/run.properties" ]]; then
            cp input${inputNumber}/run.properties . 2>> $SYSLOG
        fi
        perl $SCRIPTDIR/storm_track_gen.pl --dir "input${inputNumber}" --storm 11 --year 2017 ${argSets["a$argSetNumber"]} 2>> $SYSLOG
        # make the test-specific $SCRIPTDIR path generic for use
        # in comparing results
        for f in $(ls $SYSLOG run.properties 2>> /dev/null); do
            sed -i "s?$SCRIPTDIR?\$SCRIPTDIR?g" $f
            sed -i "s?$HOME?\$HOME?g" $f
        done
        for o in ${output[@]} ; do
            for f in $(ls *$o 2> /dev/null); do
                if [[ -e $f && $f != *actual* && $f != *expected* ]]; then
                    mv $f input${inputNumber}.arg${argSetNumber}.actual.$f
                fi
            done
        done
    done
done
# one-off tests for individual cases to
# prevent regression
numSingleTests=1
argSets['s001']="--dir ./single001 --storm 07 --year 2010 --name nowcast --nws 320 --hotstartseconds 2592000.00000000 --coldstartdate 2010073000 --strengthPercent null --test"
#
# set up fan ensemble tracks for advisories/storms
names=( LAURA IDA IAN MILTON )
declare -A namesNumbers
namesNumbers['LAURA']=13
namesNumbers['IDA']=09
namesNumbers['IAN']=09
namesNumbers['MILTON']=14
declare -A namesYears
namesYears['LAURA']=2020
namesYears['IDA']=2021
namesYears['IAN']=2022
namesYears['MILTON']=2024
declare -A namesAdvs
namesAdvs['LAURA']=19
namesAdvs['IDA']=04
namesAdvs['IAN']=13
namesAdvs['MILTON']=08
# storm coldstart dates
declare -A namesColdstarts
namesColdstarts['LAURA']=2020082406
namesColdstarts['IDA']=2021082706
namesColdstarts['IAN']=2022092606
namesColdstarts['MILTON']=2024100706
# forecast hotstart times
declare -A namesHotstartSeconds
#namesHotstartSeconds['LAURA']=$(( 86400 + ( 6 * 3600 ) ))   # including 6 hour nowcast
namesHotstartSeconds['LAURA']=0
namesHotstartSeconds['IDA']=0
namesHotstartSeconds['IAN']=0
namesHotstartSeconds['MILTON']=0
declare -A namesTaus
# forecast period to calculate (tau)
namesTaus['LAURA']=120
namesTaus['IDA']=72
namesTaus['IAN']=120
namesTaus['MILTON']=72
#
for storm in ${names[@]}; do
    v=-100 # starting veer amount
    for s in $(seq 1 17); do
        trackNum=$(printf "%02d" $s )
        trackPrefix=
        case ${v:0:1} in
        "-")
            trackPrefix=Left
            ;;
        "0")
            trackPrefix=nhcTrack
            ;;
        *)
            trackPrefix=Right
            ;;
        esac
        trackNamePercent=$(echo "$v" | sed 's/-//')
        trackName="${trackNum}.veer$trackPrefix$trackNamePercent"
        if (( $(echo "$v == 0.0" | bc -l) )); then
            trackName="${trackNum}.nhcTrack"
        fi
        #echo "--dir . --storm ${namesNumbers[$storm]} --year ${namesYears[$storm]} --name $trackName --nws 20 --hotstartseconds ${namesHotstartSeconds[$storm]} --coldstartdate ${namesColdstarts[$storm]} --forecastend ${namesTaus[$storm]} --percent $v --test"
        argSets[f$storm$trackNum]="--dir . --storm ${namesNumbers[$storm]} --year ${namesYears[$storm]} --name $trackName --nws 20 --hotstartseconds ${namesHotstartSeconds[$storm]} --coldstartdate ${namesColdstarts[$storm]} --forecastend ${namesTaus[$storm]} --percent $v --test"
        v=$(echo "scale=1; $v + 12.5" | bc)
    done
done
#
# set up branching ensemble tracks
b=1   # branch number
#       01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17   # branch
tau=( 0 48 60  0 60 48 36 48 60  0 60 48 36 48 60  0 60 48 ) # hotstart time (hours beyond base forecast)
for storm in ${names[@]}; do
    for b in $(seq 1 17); do
        branchNum=$(printf "%02d" $b)
        hstime=$(( ${numHotstartSeconds[$storm]} + ( ${tau[$b]} * 3600 ) ))
        trackName="branching$branchNum"
        #echo "--dir . --storm ${namesNumbers[$storm]} --year ${namesYears[$storm]} --name $trackName --nws 20 --hotstartseconds $hstime --coldstartdate ${namesColdstarts[$storm]} --forecastend ${namesTaus[$storm]} --percent $v --test"
        argSets[b$storm$branchNum]="--dir . --storm ${namesNumbers[$storm]} --year ${namesYears[$storm]} --name $trackName --nws 20 --hotstartseconds $hstime --coldstartdate ${namesColdstarts[$storm]} --forecastend ${namesTaus[$storm]} --percent $v --test"
    done
done
#
# generate fan and branching tracks
for storm in ${names[@]}; do
    for e in f b ; do  # fan and branching
        for t in $(seq 1 17) ; do
            trackNumber=$(printf "%02d" $t)
            SYSLOG="$storm.$e$trackNumber.actual.syslog.log"
            output=( fort.22 run.properties $SYSLOG )
            TEST=unit
            perl $SCRIPTDIR/storm_track_gen.pl ${argSets["$e$storm$trackNumber"]} 2>> $SYSLOG
            # make the test-specific $SCRIPTDIR path generic for use
            # in comparing results
            for f in $(ls $SYSLOG run.properties 2>> /dev/null); do
                sed -i "s?$SCRIPTDIR?\$SCRIPTDIR?g" $f
                sed -i "s?$HOME?\$HOME?g" $f
            done
            for o in ${output[@]} ; do
                for f in $(ls *$o 2> /dev/null); do
                    if [[ -e $f && $f != *actual* && $f != *expected* ]]; then
                        mv $f $storm.$e${trackNumber}.actual.$f
                    fi
                done
            done
        done
    done
done
#
# collect fan and branching ensemble track files together into a single .vtp file
# for visualization and quality checking
for storm in ${names[@]}; do
    for e in f b ; do  # fan and branching
        trackFiles=
        for t in $(seq 1 17); do
            trackNum=$(printf "%02d" $t)
            trackFile="$storm.$e${trackNum}.actual.fort.22"
            trackFiles+="${trackFile},"
        done
        SYSLOG="$storm.$e.tracks.actual.syslog.log"
        perl $SCRIPTDIR/output/adc2vtk.pl --trackfiles ${trackFiles%,} --test 2>> $SYSLOG
        mv tracks.vtp $storm.$e.tracks.actual.vtp
    done
done
#
# compare results
for f in $(ls *actual*) ; do
   g=${f//actual/expected}
   diff $g $f > /dev/null 2>&1
   if [[ $? -eq 0 ]]; then
      ((pass++))
   else
      actualFails+=( $f )
      ((fail++))
   fi
done
#
# check to make sure that all the expected
# files were actually produced
for g in $(ls *expected*) ; do
   f=${g//expected/actual}
   if [[ ! -e $f ]]; then
      ((fail++))
      actualFails+=( $g )
   fi
done
echo "Results: $pass tests passed. $fail tests failed."
if [[ $fail -gt 0 ]]; then
   echo "Failed:"
   for t in ${actualFails[@]}; do
      echo $t
   done
fi
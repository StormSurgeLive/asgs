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
# One Liners:
# 1. Collect diffs for failed tests:
# for f in $(./test.sh) ; do if [[ -e $f ]]; then echo $f ; diff ${f//actual/expected} $f ; fi ; done > diffs
# 2. Fix tests to reflect new expectations:
# for f in $(ls test???.actual.*) ; do echo $f ; cp $f ${f//actual/expected} ; done
#----------------------------------------------------------------
#
# Test Descriptions:
# Issue numbers are all https://github.com/StormSurgeLive/asgs
#
# 001 BEST and OFCL files both present
# 002 Only BEST file is present
# 003 Only OFCL file is present
# 004 Neither BEST or OFCL file is present
# 005 run.properties file is not found or cannot be read
# 006 BEST ends after OFCL starts
# 007 BEST ends long before OFCL starts
# 008 BEST starts after OFCL starts
#
# SCRIPTDIR should be set if this test script is executed with
# the ASGS shell
#
# initialize test number and log files
t=1
ADVISDIR=$PWD
SCENARIODIR=$PWD
#
rm *actual* run.properties fort.22 2> /dev/null # remove old test results
pass=0
fail=0
declare -a actualFails
numTests=8 # number of tests
for t in $(seq 1 $numTests) ; do
   testNumber=$(printf "%03d" $t)
   SYSLOG="test${testNumber}.actual.syslog.log"
   output=( fort.22 run.properties $SYSLOG )
   TEST=unit
   if [[ -e "test${testNumber}/run.properties" ]]; then
      cp test${testNumber}/run.properties . 2>> $SYSLOG
   fi
   perl $SCRIPTDIR/storm_track_gen.pl --dir "test${testNumber}" --storm 11 --year 2017 --test 2>> $SYSLOG
   # make the test-specific $SCRIPTDIR path generic for use
   # in comparing results
   for f in $(ls *.yaml *.log *.properties 2>> /dev/null); do
      sed -i "s?$SCRIPTDIR?\$SCRIPTDIR?g" $f
      sed -i "s?$HOME?\$HOME?g" $f
   done
   for o in ${output[@]} ; do
      for f in $(ls *$o 2> /dev/null); do
         if [[ -e $f && $f != *actual* && $f != *expected* ]]; then
            mv $f test${testNumber}.actual.$f
         fi
      done
   done
done
# now compare results
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
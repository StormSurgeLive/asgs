#!/bin/bash
#----------------------------------------------------------------
# test.sh: Driver script for unit testing control_file_gen.pl
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
# 3. Collect logs into a single file for bulk inspection:
# for f in $(ls *actual*.log); do echo $f ; cat $f ; done > logfiles
#----------------------------------------------------------------
# Issue numbers are all https://github.com/StormSurgeLive/asgs
# SCRIPTDIR will be set if this test script is executed with
# the ASGS shell
# Test Descriptions:
# 1: Fixed integer overflow with small timesteps #981
# 2: Validate sparse ascii output #1630
t=1
ADVISDIR=$PWD
SCENARIODIR=$PWD
#
rm *actual* run.properties fort.13 fort.26 jobFailed 2> /dev/null # remove old test results
# standalone cleanup
if [[ $# -eq 1 && $1 == "clean" ]]; then
      exit
fi
pass=0
fail=0
declare -a actualFails
numTests=8 # number of tests
output=( fort.15 run.properties run-control.properties fort.13 fort.26 syslog.log )
for t in $(seq 1 $numTests) ; do
   testNumber=$(printf "%03d" $t)
   SYSLOG="test${testNumber}.actual.syslog.log"
   cat test${testNumber}.control_parameters.yaml   \
      | sed "s?\$SCRIPTDIR?$SCRIPTDIR?g"           \
      | sed "s?\$HOME?$HOME?g"                     \
      | perl $SCRIPTDIR/control_file_gen.pl --test \
      > test${testNumber}.actual.fort.15           \
     2> $SYSLOG
   for o in ${output[@]} ; do
      for f in $(ls *$o 2> /dev/null); do
         if [[ -e $f && $f != *actual* && $f != *expected* && $f != *.yaml ]]; then
            mv $f test${testNumber}.actual.$f
         fi
      done
   done
done
for f in $(ls *actual* 2>> /dev/null); do
   sed -i "s?$SCRIPTDIR?\$SCRIPTDIR?g" $f
   sed -i "s?$HOME?\$HOME?g" $f
done
# now compare results
for f in $(ls *actual*) ; do
   g=${f//actual/expected}
   if [[ ! -e $g ]]; then
      echo "Missing: $g"
      ((fail++))
      continue
   fi
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
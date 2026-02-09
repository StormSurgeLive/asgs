#!/bin/bash
#----------------------------------------------------------------
# test.sh: Driver script for the creating yaml to feed
# control_file_gen.pl
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
# Test Descriptions:
# Issue numbers are all https://github.com/StormSurgeLive/asgs
#
# SCRIPTDIR should be set if this test script is executed with
# the ASGS shell
#
# initialize test number and log files
t=1
ADVISDIR=$PWD
SCENARIODIR=$PWD
#
rm *actual* run.properties fort.13 fort.26 jobFailed 2> /dev/null # remove old test results
pass=0
fail=0
numTests=6 # number of tests
output=( fort.15 run-control.properties control_parameters.yaml fort.13 fort.26 tide_fac.out )
for t in $(seq 1 $numTests) ; do
   testNumber=$(printf "%03d" $t)
   SYSLOG="test${testNumber}.actual.syslog.log"
   SCENARIOLOG="test${testNumber}.actual.scenario.log"
   source test${testNumber}.config.sh
   # produce yaml for control_file_gen.pl and
   # properties for run.properties
   TEST=unit
   generateDynamicInput
   rm run.properties 2> /dev/null # this is just a duplicate of run-control.properties
   # make the test-specific $SCRIPTDIR path generic for use
   # in comparing results
   for f in $(ls *.yaml *.log); do
      sed -i "s?$SCRIPTDIR?\$SCRIPTDIR?g" $f
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
      ((fail++))
   fi
done
# check to make sure that all the expected
# files were actually produced
for g in $(ls *expected*) ; do
   f=${g//expected/actual}
   if [[ ! -e $f ]]; then
      ((fail++))
   fi
done
echo "Results: $pass tests passed. $fail tests failed."
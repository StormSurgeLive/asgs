#!/bin/bash
#----------------------------------------------------------------
# test.sh: Driver script for testing get_atcf.pl
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
#----------------------------------------------------------------
# Test Descriptons:
# Related to github issue #1031
# 1. only covers that case where files are loaded
#    from the local filesystem (not via http or ftp)
# 2. tests (non)existence of BEST and OFCL directories
#    and files
# 3. compares both stdout and stderr
# 4. could be extended to include mock ftp and https
#    endpoints
#----------------------------------------------------------------
rm *actual* 2> /dev/null # remove old test results
pass=0
fail=0
declare -a actualFails
i=1
storm=18
year=2012
testPath=t/meteorology/vortex/get_atcf.pl
# either it exists or it doesn't
fdirs=( nofdir yesfdir )
hdirs=( nohdir yeshdir )
ofcls=( noofcl index-at.xml )
bests=( nobest bal182012 )
types=( stdout stderr )
for f in ${fdirs[@]} ; do
   for h in ${hdirs[@]} ; do
      for o in ${ofcls[@]} ; do
         for b in ${bests[@]}; do
            testNumber=$(printf "%03d" $i)
            perl $SCRIPTDIR/get_atcf.pl --test --ftpsite filesystem --rsssite filesystem --fdir $SCRIPTDIR/$testPath/$f --hdir $SCRIPTDIR/$testPath/$h --trigger rssembedded --storm $storm --year $year \
               > test${testNumber}.actual.stdout 2> test${testNumber}.actual.stderr
            ((i + 2))
         done
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
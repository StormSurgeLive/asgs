#!/bin/bash
#
# Test Descriptons:
# Related to github issue #1031
# 1. only covers that case where files are loaded
#    from the local filesystem (not via http or ftp)
# 2. tests (non)existence of BEST and OFCL directories
#    and files
# 3. compares both stdout and stderr
# 4. could be extended to include mock ftp and https
#    endpoints
#
rm *actual* 2> /dev/null # remove old test results
pass=0
fail=0
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
            for t in ${types[@]} ; do
               diff test${testNumber}.expected.$t test${testNumber}.actual.$t > /dev/null 2>&1
               if [[ $? -eq 0 ]]; then
                  ((pass++))
               else
                  ((fail++))
               fi
               ((i++))
            done
         done
      done
   done
done
echo "Results: $pass tests passed. $fail tests failed."
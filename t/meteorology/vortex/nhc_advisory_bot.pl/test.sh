#!/bin/bash
#
rm *actual* 2> /dev/null # remove old test results
pass=0
fail=0
i=1
files=( $(ls test???.input.*) )
while [[ $i -le ${#files[@]} ]] ; do
   testNumber=$(printf "%03d" $i)
   perl $SCRIPTDIR/nhc_advisory_bot.pl --input ${files[$i-1]} --output test${testNumber}.actual.ofcl.fst --metadata test${testNumber}.actual.run.properties 2> test${testNumber}.actual.stderr
   ((i++))
done
# now compare results
i=1
o=( ofcl.fst run.properties stderr )
while [[ $i -le ${#files[@]} ]] ; do
   testNumber=$(printf "%03d" $i)
   for output in ${o[*]}; do
      diff test${testNumber}.expected.${output} test${testNumber}.actual.${output} > /dev/null 2>&1
      if [[ $? -eq 0 ]]; then
         ((pass++))
      else
         ((fail++))
      fi
   done
   ((i++))
done
echo "Results: $pass tests passed. $fail tests failed."
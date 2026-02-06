#!/bin/bash
#
# Test Descriptions:
# 1: Fixed integer overflow with small timesteps #981
# 2: Validate sparse ascii output #1630
# Issue numbers are all https://github.com/StormSurgeLive/asgs
rm *actual* 2> /dev/null # remove old test results
pass=0
fail=0
numTests=2 # number of tests
output=( fort.15 fort.13 fort.26 run-control.properties stderr )
#
for t in $(seq 1 $numTests) ; do
   testNumber=$(printf "%03d" $t)
   opts=( $(<test${testNumber}.options.txt) )
   cat test${testNumber}.control_parameters.yaml \
      | perl $SCRIPTDIR/control_file_gen.pl "${opts[@]}" \
      > test${testNumber}.actual.fort.15 \
     2> test${testNumber}.actual.stderr
   for o in ${output[@]} ; do
      if [[ -e $o ]]; then
         mv $o test${testNumber}.actual.$o
      fi
   done
done
# now compare results
for t in $(seq 1 $numTests) ; do
   testNumber=$(printf "%03d" $t)
   for o in ${output[*]}; do
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
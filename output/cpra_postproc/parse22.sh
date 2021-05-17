#/bin/bash
awk 'BEGIN { FS="," } { printf "-%0.2f  %0.2f\n", $8/10.0, $7/10.0 }' fort.22 > fort.22.trk

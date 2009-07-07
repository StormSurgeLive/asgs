set yrange [%windmin%:%windmax%]
set title  "Wind Speed at %stationname%, advisory %advisorynum% UTC"
set output "%plotname%"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:%track1col% title "%track1name%" with points 7,\
"allruns.out" every 10 using 1:%track2col% title "%track2name%" with points 4,\
"allruns.out" every 10 using 1:%track3col% title "%track3name%" with points 8,\
"allruns.out" every 10 using 1:%track4col% title "%track4name%" with points 1,\
"allruns.out" every 10 using 1:%track5col% title "%track5name%" with points 2,\
"allruns.out" using 1:%track1col% title "" with lines 1,\
"allruns.out" using 1:%track2col% title "" with lines,\
"allruns.out" using 1:%track3col% title "" with lines,\
"allruns.out" using 1:%track4col% title "" with lines,\
"allruns.out" using 1:%track5col% title "" with lines 2

set terminal postscript "Times-Roman" 14
set grid
set xlabel "Date CDT (mm/dd)"
set xdata time
set format x "%m/%d"
set xtics 86400
set mxtics 24
set key top left box
#set missing "-99999" 
set timefmt "%Y-%m-%d %H:%M:%S"
set xrange ["2016-07-28 19:00:00":"2016-08-08 19:00:00"]
#set yrange [%windmin%:%windmax%]
#
set title  "Boundary Node 75, Cold Start Date Arbitrarily Set to 00Z 2016-07-28"
set output "boundary_node_75.ps"
set ylabel "Elevation (m MSL)"
plot "fort.19_transpose" using 1:76 title "Elevation Boundary Node 75" with lines
#
#set title "Wind Speed at Harvey Canal at Lapalco Blvd, advisory%forecastdate% "
#set output "harvey_canal_windspeed.ps"
#set ylabel "Wind Speed (kts)"
#plot "allruns.out" every 10 using 1:20  title "Consensus Rmax46" with points 7,\
#"allruns.out" every 10 using 1:23 title "Consensus Rmax35" with points 4,\
#"allruns.out" every 10 using 1:26 title "Consensus Rmax30" with points 8,\
#"allruns.out" every 10 using 1:29 title "Unused" with points 1,\
#"allruns.out" every 10 using 1:32 title "Unused" with points 2,\
#"allruns.out" using 1:20  title "" with lines 1,\
#"allruns.out" using 1:23 title "" with lines,\
#"allruns.out" using 1:26 title "" with lines,\
#"allruns.out" using 1:29 title "" with lines,\
#"allruns.out" using 1:32 title "" with lines 2
#

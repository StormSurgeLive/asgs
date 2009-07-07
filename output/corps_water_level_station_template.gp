set terminal postscript "Times-Roman" 14
set grid
set xlabel "Date UTC (mm/dd)"
set xdata time
set format x "%m/%d"
set xtics 86400
set key top left box
set missing "-99999" 
set timefmt "%Y%m%d %H:%M:%S"
set xrange ["%startgraph%":"%endforecast%"]
set yrange [%windmin%:%windmax%]
#
set title  "Wind Speed at London Ave Canal Outlet, advisory%forecastdate% UTC"
set output "london_ave_canal_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:5 title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:8 title "Consensus w/RMW=25nm" with points 4,\
"allruns.out" every 10 using 1:11 title "Unused" with points 8,\
"allruns.out" every 10 using 1:14 title "Unused" with points 1,\
"allruns.out" every 10 using 1:17 title "Unused" with points 2,\
"allruns.out" using 1:5 title "" with lines 1,\
"allruns.out" using 1:8 title "" with lines,\
"allruns.out" using 1:11 title "" with lines,\
"allruns.out" using 1:14 title "" with lines,\
"allruns.out" using 1:17 title "" with lines 2
#
set title "Wind Speed at Harvey Canal at Lapalco Blvd, advisory%forecastdate% UTC"
set output "harvey_canal_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:20  title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:23 title "Consensus w/RMW=25nm" with points 4,\
"allruns.out" every 10 using 1:26 title "Unused" with points 8,\
"allruns.out" every 10 using 1:29 title "Unused" with points 1,\
"allruns.out" every 10 using 1:32 title "Unused" with points 2,\
"allruns.out" using 1:20  title "" with lines 1,\
"allruns.out" using 1:23 title "" with lines,\
"allruns.out" using 1:26 title "" with lines,\
"allruns.out" using 1:29 title "" with lines,\
"allruns.out" using 1:32 title "" with lines 2
#
set title "Wind Speed at St. Charles, advisory%forecastdate% UTC"
set output "st_charles_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:35  title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:38 title "Consensus w/RMW=25nm" with points 4,\
"allruns.out" every 10 using 1:41 title "Unused" with points 8,\
"allruns.out" every 10 using 1:44 title "Unused" with points 1,\
"allruns.out" every 10 using 1:47 title "Unused" with points 2,\
"allruns.out" using 1:35  title "" with lines 1,\
"allruns.out" using 1:38 title "" with lines,\
"allruns.out" using 1:41 title "" with lines,\
"allruns.out" using 1:44 title "" with lines,\
"allruns.out" using 1:47 title "" with lines 2
#
set title "Wind Speed at Golden Triangle, advisory%forecastdate% UTC"
set output "golden_triangle_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:50  title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:53 title "Consensus w/RMW=25nm" with points 4,\
"allruns.out" every 10 using 1:56 title "Unused" with points 8,\
"allruns.out" every 10 using 1:59 title "Unused" with points 1,\
"allruns.out" every 10 using 1:62 title "Unused" with points 2,\
"allruns.out" using 1:50  title "" with lines 1,\
"allruns.out" using 1:53 title "" with lines,\
"allruns.out" using 1:56 title "" with lines,\
"allruns.out" using 1:59 title "" with lines,\
"allruns.out" using 1:62 title "" with lines 2
#
set title "Wind Speed at Caernarvon, advisory%forecastdate% UTC"
set output "caernarvon_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:65  title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:68 title "Consensus w/RMW=25nm" with points 4,\
"allruns.out" every 10 using 1:71 title "Unused" with points 8,\
"allruns.out" every 10 using 1:74 title "Unused" with points 1,\
"allruns.out" every 10 using 1:77 title "Unused" with points 2,\
"allruns.out" using 1:65  title "" with lines 1,\
"allruns.out" using 1:68 title "" with lines,\
"allruns.out" using 1:71 title "" with lines,\
"allruns.out" using 1:74 title "" with lines,\
"allruns.out" using 1:77 title "" with lines 2
#
set title "Wind Speed at Northwest West Bank, advisory%forecastdate% UTC"
set output "northwest_west_bank_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:80  title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:83 title "Consensus w/RMW=25nm" with points 4,\
"allruns.out" every 10 using 1:86 title "Unused" with points 8,\
"allruns.out" every 10 using 1:89 title "Unused" with points 1,\
"allruns.out" every 10 using 1:92 title "Unused" with points 2,\
"allruns.out" using 1:80  title "" with lines 1,\
"allruns.out" using 1:83 title "" with lines,\
"allruns.out" using 1:86 title "" with lines,\
"allruns.out" using 1:89 title "" with lines,\
"allruns.out" using 1:92 title "" with lines 2
#
set title "Wind Speed at Larose, advisory%forecastdate% UTC"
set output "larose_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:95  title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:98 title "Consensus w/RMW=25nm" with points 4,\
"allruns.out" every 10 using 1:101 title "Unused" with points 8,\
"allruns.out" every 10 using 1:104 title "Unused" with points 1,\
"allruns.out" every 10 using 1:107 title "Unused" with points 2,\
"allruns.out" using 1:95  title "" with lines 1,\
"allruns.out" using 1:98 title "" with lines,\
"allruns.out" using 1:101 title "" with lines,\
"allruns.out" using 1:104 title "" with lines,\
"allruns.out" using 1:107 title "" with lines 2
#
set yrange [%watermin%:%watermax%]
set title "Water Level Offset at London Ave. Canal Outlet, advisory%forecastdate% UTC"
set output "london_ave_canal_water_level.ps"
set ylabel "Water Level Offset (ft) NAVD88"
plot "allruns.out" every 10 using 1:108 title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:109 title "Consensus w/RMW=25nm" with points 4, \
"allruns.out" every 10 using 1:110 title "Unused" with points 8, \
"allruns.out" every 10 using 1:111 title "Unused" with points 1, \
"allruns.out" every 10 using 1:112 title "Unused" with points 2,\
"allruns.out" using 1:108 title "" with lines 1,\
"allruns.out" using 1:109 title "" with lines, \
"allruns.out" using 1:110 title "" with lines, \
"allruns.out" using 1:111 title "" with lines, \
"allruns.out" using 1:112 title "" with lines 2
#
set title "Water Level Offset at Harvey Canal at Lapalco Blvd, advisory%forecastdate% UTC"
set output "harvey_canal_water_level.ps"
set ylabel "Water Level Offset (ft) NAVD88"
plot "allruns.out" every 10 using 1:113 title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:114 title "Consensus w/RMW=25nm" with points 4, \
"allruns.out" every 10 using 1:115 title "Unused" with points 8, \
"allruns.out" every 10 using 1:116 title "Unused" with points 1, \
"allruns.out" every 10 using 1:117 title "Unused" with points 2,\
"allruns.out" using 1:113 title "" with lines 1,\
"allruns.out" using 1:114 title "" with lines, \
"allruns.out" using 1:115 title "" with lines, \
"allruns.out" using 1:116 title "" with lines, \
"allruns.out" using 1:117 title "" with lines 2
#
set title "Water Level Offset at St. Charles, advisory%forecastdate% UTC"
set output "st_charles_water_level.ps"
set ylabel "Water Level Offset (ft) NAVD88"
plot "allruns.out" every 10 using 1:118 title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:119 title "Consensus w/RMW=25nm" with points 4, \
"allruns.out" every 10 using 1:120 title "Unused" with points 8, \
"allruns.out" every 10 using 1:121 title "Unused" with points 1, \
"allruns.out" every 10 using 1:122 title "Unused" with points 2,\
"allruns.out" using 1:118 title "" with lines 1,\
"allruns.out" using 1:119 title "" with lines, \
"allruns.out" using 1:120 title "" with lines, \
"allruns.out" using 1:121 title "" with lines, \
"allruns.out" using 1:122 title "" with lines 2
#
set title "Water Level Offset at Golden Triangle, advisory%forecastdate% UTC"
set output "golden_triangle_water_level.ps"
set ylabel "Water Level Offset (ft) NAVD88"
plot "allruns.out" every 10 using 1:123 title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:124 title "Consensus w/RMW=25nm" with points 4, \
"allruns.out" every 10 using 1:125 title "Unused" with points 8, \
"allruns.out" every 10 using 1:126 title "Unused" with points 1, \
"allruns.out" every 10 using 1:127 title "Unused" with points 2,\
"allruns.out" using 1:123 title "" with lines 1,\
"allruns.out" using 1:124 title "" with lines, \
"allruns.out" using 1:125 title "" with lines, \
"allruns.out" using 1:126 title "" with lines, \
"allruns.out" using 1:127 title "" with lines 2
#
set title "Water Level Offset at Caernarvon, advisory%forecastdate% UTC"
set output "caernarvon_water_level.ps"
set ylabel "Water Level Offset (ft) NAVD88"
plot "allruns.out" every 10 using 1:128 title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:129 title "Consensus w/RMW=25nm" with points 4, \
"allruns.out" every 10 using 1:130 title "Unused" with points 8, \
"allruns.out" every 10 using 1:131 title "Unused" with points 1, \
"allruns.out" every 10 using 1:132 title "Unused" with points 2,\
"allruns.out" using 1:128 title "" with lines 1,\
"allruns.out" using 1:129 title "" with lines, \
"allruns.out" using 1:130 title "" with lines, \
"allruns.out" using 1:131 title "" with lines, \
"allruns.out" using 1:132 title "" with lines 2
#
set title "Water Level Offset at Northwest West Bank, advisory%forecastdate% UTC"
set output "northwest_west_bank_water_level.ps"
set ylabel "Water Level Offset (ft) NAVD88"
plot "allruns.out" every 10 using 1:133 title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:134 title "Consensus w/RMW=25nm" with points 4, \
"allruns.out" every 10 using 1:135 title "Unused" with points 8, \
"allruns.out" every 10 using 1:136 title "Unused" with points 1, \
"allruns.out" every 10 using 1:137 title "Unused" with points 2,\
"allruns.out" using 1:133 title "" with lines 1,\
"allruns.out" using 1:134 title "" with lines, \
"allruns.out" using 1:135 title "" with lines, \
"allruns.out" using 1:136 title "" with lines, \
"allruns.out" using 1:137 title "" with lines 2
#
set title "Water Level Offset at Larose, advisory%forecastdate% UTC"
set output "larose_water_level.ps"
set ylabel "Water Level Offset (ft) NAVD88"
plot "allruns.out" every 10 using 1:138 title "Consensus w/RMW=15nm" with points 7,\
"allruns.out" every 10 using 1:139 title "Consensus w/RMW=25nm" with points 4, \
"allruns.out" every 10 using 1:140 title "Unused" with points 8, \
"allruns.out" every 10 using 1:141 title "Unused" with points 1, \
"allruns.out" every 10 using 1:142 title "Unused" with points 2,\
"allruns.out" using 1:138 title "" with lines 1,\
"allruns.out" using 1:139 title "" with lines, \
"allruns.out" using 1:140 title "" with lines, \
"allruns.out" using 1:141 title "" with lines, \
"allruns.out" using 1:142 title "" with lines 2

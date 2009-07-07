set terminal postscript "Times-Roman" 14
set grid
set xlabel "Date CDT (mm/dd)"
set xdata time
set format x "%m/%d"
set xtics 86400
set mxtics 24
set key top left box
set missing "-99999" 
set timefmt "%Y%m%d %H:%M:%S"
set xrange ["%startgraph%":"%endforecast%"]
set yrange [%windmin%:%windmax%]
#
set title  "Wind Speed at London Ave Canal Outlet, advisory%forecastdate% "
set output "london_ave_canal_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:5 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:8 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:11 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:14 title "Unused" with points 1,\
"allruns.out" every 10 using 1:17 title "Unused" with points 2,\
"allruns.out" using 1:5 title "" with lines 1,\
"allruns.out" using 1:8 title "" with lines,\
"allruns.out" using 1:11 title "" with lines,\
"allruns.out" using 1:14 title "" with lines,\
"allruns.out" using 1:17 title "" with lines 2
#
set title "Wind Speed at Harvey Canal at Lapalco Blvd, advisory%forecastdate% "
set output "harvey_canal_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:20  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:23 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:26 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:29 title "Unused" with points 1,\
"allruns.out" every 10 using 1:32 title "Unused" with points 2,\
"allruns.out" using 1:20  title "" with lines 1,\
"allruns.out" using 1:23 title "" with lines,\
"allruns.out" using 1:26 title "" with lines,\
"allruns.out" using 1:29 title "" with lines,\
"allruns.out" using 1:32 title "" with lines 2
#
set title "Wind Speed at St. Charles, advisory%forecastdate% "
set output "st_charles_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:35  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:38 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:41 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:44 title "Unused" with points 1,\
"allruns.out" every 10 using 1:47 title "Unused" with points 2,\
"allruns.out" using 1:35  title "" with lines 1,\
"allruns.out" using 1:38 title "" with lines,\
"allruns.out" using 1:41 title "" with lines,\
"allruns.out" using 1:44 title "" with lines,\
"allruns.out" using 1:47 title "" with lines 2
#
set title "Wind Speed at Golden Triangle, advisory%forecastdate% "
set output "golden_triangle_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:50  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:53 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:56 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:59 title "Unused" with points 1,\
"allruns.out" every 10 using 1:62 title "Unused" with points 2,\
"allruns.out" using 1:50  title "" with lines 1,\
"allruns.out" using 1:53 title "" with lines,\
"allruns.out" using 1:56 title "" with lines,\
"allruns.out" using 1:59 title "" with lines,\
"allruns.out" using 1:62 title "" with lines 2
#
set title "Wind Speed at Caernarvon, advisory%forecastdate% "
set output "caernarvon_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:65  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:68 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:71 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:74 title "Unused" with points 1,\
"allruns.out" every 10 using 1:77 title "Unused" with points 2,\
"allruns.out" using 1:65  title "" with lines 1,\
"allruns.out" using 1:68 title "" with lines,\
"allruns.out" using 1:71 title "" with lines,\
"allruns.out" using 1:74 title "" with lines,\
"allruns.out" using 1:77 title "" with lines 2
#
set title "Wind Speed at Northwest West Bank, advisory%forecastdate% "
set output "northwest_west_bank_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:80  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:83 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:86 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:89 title "Unused" with points 1,\
"allruns.out" every 10 using 1:92 title "Unused" with points 2,\
"allruns.out" using 1:80  title "" with lines 1,\
"allruns.out" using 1:83 title "" with lines,\
"allruns.out" using 1:86 title "" with lines,\
"allruns.out" using 1:89 title "" with lines,\
"allruns.out" using 1:92 title "" with lines 2
#
set title "Wind Speed at Larose, advisory%forecastdate% "
set output "larose_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:95  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:98 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:101 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:104 title "Unused" with points 1,\
"allruns.out" every 10 using 1:107 title "Unused" with points 2,\
"allruns.out" using 1:95  title "" with lines 1,\
"allruns.out" using 1:98 title "" with lines,\
"allruns.out" using 1:101 title "" with lines,\
"allruns.out" using 1:104 title "" with lines,\
"allruns.out" using 1:107 title "" with lines 2
#
set title "Wind Speed, Lake Pont. at Mandeville (85575), advisory%forecastdate%"
set output "lake_pont_at_mandeville_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:110  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:113 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:116 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:119 title "Unused" with points 1,\
"allruns.out" every 10 using 1:122 title "Unused" with points 2,\
"allruns.out" using 1:110  title "" with lines 1,\
"allruns.out" using 1:113 title "" with lines,\
"allruns.out" using 1:116 title "" with lines,\
"allruns.out" using 1:119 title "" with lines,\
"allruns.out" using 1:122 title "" with lines 2
#
set title "Wind Speed, Lake Pont. at West End (85625), advisory%forecastdate%"
set output "lake_pont_at_west_end_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:125  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:128 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:131 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:134 title "Unused" with points 1,\
"allruns.out" every 10 using 1:137 title "Unused" with points 2,\
"allruns.out" using 1:125  title "" with lines 1,\
"allruns.out" using 1:128 title "" with lines,\
"allruns.out" using 1:131 title "" with lines,\
"allruns.out" using 1:134 title "" with lines,\
"allruns.out" using 1:137 title "" with lines 2
#
set title "Wind Speed, IHNC New Orleans (76160), advisory%forecastdate%"
set output "ihnc_new_orleans_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:140  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:143 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:146 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:149 title "Unused" with points 1,\
"allruns.out" every 10 using 1:152 title "Unused" with points 2,\
"allruns.out" using 1:140  title "" with lines 1,\
"allruns.out" using 1:143 title "" with lines,\
"allruns.out" using 1:146 title "" with lines,\
"allruns.out" using 1:149 title "" with lines,\
"allruns.out" using 1:152 title "" with lines 2
#
set title "Wind Speed, Sellers Canal at Hwy 90 (82720) advisory%forecastdate%"
set output "sellers_canal_at_hwy_90_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:155  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:158 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:161 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:164 title "Unused" with points 1,\
"allruns.out" every 10 using 1:167 title "Unused" with points 2,\
"allruns.out" using 1:155  title "" with lines 1,\
"allruns.out" using 1:158 title "" with lines,\
"allruns.out" using 1:161 title "" with lines,\
"allruns.out" using 1:164 title "" with lines,\
"allruns.out" using 1:167 title "" with lines 2
#
set title "Wind Speed, Harvey Canal at Boomtown (76230), advisory%forecastdate%"
set output "harvey_canal_at_boomtown_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:170 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:173 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:176 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:179 title "Unused" with points 1,\
"allruns.out" every 10 using 1:182 title "Unused" with points 2,\
"allruns.out" using 1:170  title "" with lines 1,\
"allruns.out" using 1:173 title "" with lines,\
"allruns.out" using 1:176 title "" with lines,\
"allruns.out" using 1:179 title "" with lines,\
"allruns.out" using 1:182 title "" with lines 2
#
set title "Wind Speed, Barataria Waterway at Lafitte (82875), advisory%forecastdate%"
set output "barataria_waterway_at_lafitte_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:185  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:188 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:191 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:194 title "Unused" with points 1,\
"allruns.out" every 10 using 1:197 title "Unused" with points 2,\
"allruns.out" using 1:185  title "" with lines 1,\
"allruns.out" using 1:188 title "" with lines,\
"allruns.out" using 1:191 title "" with lines,\
"allruns.out" using 1:194 title "" with lines,\
"allruns.out" using 1:197 title "" with lines 2
#
set title "Wind Speed, Golden Meadow Floodgate (South) (82260), advisory%forecastdate%"
set output "golden_meadow_floodgate_south_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:200  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:203 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:206 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:209 title "Unused" with points 1,\
"allruns.out" every 10 using 1:212 title "Unused" with points 2,\
"allruns.out" using 1:200  title "" with lines 1,\
"allruns.out" using 1:203 title "" with lines,\
"allruns.out" using 1:206 title "" with lines,\
"allruns.out" using 1:209 title "" with lines,\
"allruns.out" using 1:212 title "" with lines 2
#
set title "Wind Speed, USGS Lake Pont. at Midlake, advisory%forecastdate%"
set output "usgs_lake_pont_at_midlake_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:215  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:218 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:221 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:224 title "Unused" with points 1,\
"allruns.out" every 10 using 1:227 title "Unused" with points 2,\
"allruns.out" using 1:215  title "" with lines 1,\
"allruns.out" using 1:218 title "" with lines,\
"allruns.out" using 1:221 title "" with lines,\
"allruns.out" using 1:224 title "" with lines,\
"allruns.out" using 1:227 title "" with lines 2
#
set title "Wind Speed, USGS Little Irish Bayou, advisory%forecastdate%"
set output "usgs_little_irish_bayou_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:230  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:233 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:236 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:239 title "Unused" with points 1,\
"allruns.out" every 10 using 1:242 title "Unused" with points 2,\
"allruns.out" using 1:230  title "" with lines 1,\
"allruns.out" using 1:233 title "" with lines,\
"allruns.out" using 1:236 title "" with lines,\
"allruns.out" using 1:239 title "" with lines,\
"allruns.out" using 1:242 title "" with lines 2
#
set title "Wind Speed, USGS Suzie Bayou at L Hermitage, advisory%forecastdate%"
set output "usgs_suzie_bayou_at_l_hermitage_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:245  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:248 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:251 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:254 title "Unused" with points 1,\
"allruns.out" every 10 using 1:257 title "Unused" with points 2,\
"allruns.out" using 1:245  title "" with lines 1,\
"allruns.out" using 1:248 title "" with lines,\
"allruns.out" using 1:251 title "" with lines,\
"allruns.out" using 1:254 title "" with lines,\
"allruns.out" using 1:257 title "" with lines 2
#
set title "Wind Speed, USGS Barataria Pass at Grand Isle, advisory%forecastdate%"
set output "usgs_barataria_pass_at_grand_isle_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:260 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:263 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:266 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:269 title "Unused" with points 1,\
"allruns.out" every 10 using 1:272 title "Unused" with points 2,\
"allruns.out" using 1:260  title "" with lines 1,\
"allruns.out" using 1:263 title "" with lines,\
"allruns.out" using 1:266 title "" with lines,\
"allruns.out" using 1:269 title "" with lines,\
"allruns.out" using 1:272 title "" with lines 2
#
set title "Wind Speed, USGS NE Bay Gardene near Point-A-LA-Hache, advisory%forecastdate%"
set output "usgs_ne_bay_gardene_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:275  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:278 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:281 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:284 title "Unused" with points 1,\
"allruns.out" every 10 using 1:287 title "Unused" with points 2,\
"allruns.out" using 1:275  title "" with lines 1,\
"allruns.out" using 1:278 title "" with lines,\
"allruns.out" using 1:281 title "" with lines,\
"allruns.out" using 1:284 title "" with lines,\
"allruns.out" using 1:287 title "" with lines 2
#
set title "Wind Speed, Caernarvon Outfall Channel, advisory%forecastdate%"
set output "usgs_caernarvon_outfall_channel_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:290  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:293 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:296 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:299 title "Unused" with points 1,\
"allruns.out" every 10 using 1:302 title "Unused" with points 2,\
"allruns.out" using 1:290  title "" with lines 1,\
"allruns.out" using 1:293 title "" with lines,\
"allruns.out" using 1:296 title "" with lines,\
"allruns.out" using 1:299 title "" with lines,\
"allruns.out" using 1:302 title "" with lines 2
#
set title "Wind Speed, IWW at I-510 Bridge, advisory%forecastdate% "
set output "usgs_iww_at_510_bridge_paris_rd_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:305 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:308 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:311 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:314 title "Unused" with points 1,\
"allruns.out" every 10 using 1:317 title "Unused" with points 2,\
"allruns.out" using 1:305  title "" with lines 1,\
"allruns.out" using 1:308 title "" with lines,\
"allruns.out" using 1:311 title "" with lines,\
"allruns.out" using 1:314 title "" with lines,\
"allruns.out" using 1:317 title "" with lines 2
#
set title "Wind Speed, USGS Little Lake near Bayou Dogris, advisory%forecastdate%"
set output "usgs_little_lake_near_bayou_dogris_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:320  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:323 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:326 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:329 title "Unused" with points 1,\
"allruns.out" every 10 using 1:332 title "Unused" with points 2,\
"allruns.out" using 1:320  title "" with lines 1,\
"allruns.out" using 1:323 title "" with lines,\
"allruns.out" using 1:326 title "" with lines,\
"allruns.out" using 1:329 title "" with lines,\
"allruns.out" using 1:332 title "" with lines 2
#
set title "Wind Speed, USGS MS Sound at Grand Pass, advisory%forecastdate%"
set output "usgs_ms_sound_at_grand_pass_windspeed.ps"
set ylabel "Wind Speed (kts)"
plot "allruns.out" every 10 using 1:335  title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:338 title "Consensus Rmax35" with points 4,\
"allruns.out" every 10 using 1:341 title "Consensus Rmax30" with points 8,\
"allruns.out" every 10 using 1:344 title "Unused" with points 1,\
"allruns.out" every 10 using 1:347 title "Unused" with points 2,\
"allruns.out" using 1:335  title "" with lines 1,\
"allruns.out" using 1:338 title "" with lines,\
"allruns.out" using 1:341 title "" with lines,\
"allruns.out" using 1:344 title "" with lines,\
"allruns.out" using 1:347 title "" with lines 2
#
set yrange [%watermin%:%watermax%]
# 1
set title "Stage at London Ave. Canal Outlet, advisory%forecastdate%"
set output "london_ave_canal_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:348 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:349 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:350 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:351 title "Unused" with points 1, \
"allruns.out" every 10 using 1:352 title "Unused" with points 2,\
"allruns.out" using 1:348 title "" with lines 1,\
"allruns.out" using 1:349 title "" with lines, \
"allruns.out" using 1:350 title "" with lines, \
"allruns.out" using 1:351 title "" with lines, \
"allruns.out" using 1:352 title "" with lines 2
# 2
set title "Stage at Harvey Canal at Lapalco Blvd, advisory%forecastdate%"
set output "harvey_canal_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:353 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:354 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:355 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:356 title "Unused" with points 1, \
"allruns.out" every 10 using 1:357 title "Unused" with points 2,\
"allruns.out" using 1:353 title "" with lines 1,\
"allruns.out" using 1:354 title "" with lines, \
"allruns.out" using 1:355 title "" with lines, \
"allruns.out" using 1:356 title "" with lines, \
"allruns.out" using 1:357 title "" with lines 2
# 3
set title "Stage at St. Charles, advisory%forecastdate%"
set output "st_charles_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:358 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:359 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:360 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:361 title "Unused" with points 1, \
"allruns.out" every 10 using 1:362 title "Unused" with points 2,\
"allruns.out" using 1:358 title "" with lines 1,\
"allruns.out" using 1:359 title "" with lines, \
"allruns.out" using 1:360 title "" with lines, \
"allruns.out" using 1:361 title "" with lines, \
"allruns.out" using 1:362 title "" with lines 2
# 4
set title "Stage at Golden Triangle, advisory%forecastdate%"
set output "golden_triangle_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:363 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:364 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:365 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:366 title "Unused" with points 1, \
"allruns.out" every 10 using 1:367 title "Unused" with points 2,\
"allruns.out" using 1:363 title "" with lines 1,\
"allruns.out" using 1:364 title "" with lines, \
"allruns.out" using 1:365 title "" with lines, \
"allruns.out" using 1:366 title "" with lines, \
"allruns.out" using 1:367 title "" with lines 2
# 5
set title "Stage at Caernarvon, advisory%forecastdate%"
set output "caernarvon_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:368 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:369 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:370 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:371 title "Unused" with points 1, \
"allruns.out" every 10 using 1:372 title "Unused" with points 2,\
"allruns.out" using 1:368 title "" with lines 1,\
"allruns.out" using 1:369 title "" with lines, \
"allruns.out" using 1:370 title "" with lines, \
"allruns.out" using 1:371 title "" with lines, \
"allruns.out" using 1:372 title "" with lines 2
# 6
set title "Stage at Northwest West Bank, advisory%forecastdate%"
set output "northwest_west_bank_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:373 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:374 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:375 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:376 title "Unused" with points 1, \
"allruns.out" every 10 using 1:377 title "Unused" with points 2,\
"allruns.out" using 1:373 title "" with lines 1,\
"allruns.out" using 1:374 title "" with lines, \
"allruns.out" using 1:375 title "" with lines, \
"allruns.out" using 1:376 title "" with lines, \
"allruns.out" using 1:377 title "" with lines 2
# 7
set title "Stage at Larose, advisory%forecastdate%"
set output "larose_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:378 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:379 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:380 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:381 title "Unused" with points 1, \
"allruns.out" every 10 using 1:382 title "Unused" with points 2,\
"allruns.out" using 1:378 title "" with lines 1,\
"allruns.out" using 1:379 title "" with lines, \
"allruns.out" using 1:380 title "" with lines, \
"allruns.out" using 1:381 title "" with lines, \
"allruns.out" using 1:382 title "" with lines 2
# 8
set title "Stage, Lake Pont. at Mandeville (85575), advisory%forecastdate%"
set output "lake_pont_at_mandeville_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:383 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:384 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:385 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:386 title "Unused" with points 1, \
"allruns.out" every 10 using 1:387 title "Unused" with points 2,\
"allruns.out" using 1:383 title "" with lines 1,\
"allruns.out" using 1:384 title "" with lines, \
"allruns.out" using 1:385 title "" with lines, \
"allruns.out" using 1:386 title "" with lines, \
"allruns.out" using 1:387 title "" with lines 2
# 9
set title "Stage, Lake Pont. at West End (85625), advisory%forecastdate% "
set output "lake_pont_at_west_end_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:388 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:389 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:390 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:391 title "Unused" with points 1, \
"allruns.out" every 10 using 1:392 title "Unused" with points 2,\
"allruns.out" using 1:388 title "" with lines 1,\
"allruns.out" using 1:389 title "" with lines, \
"allruns.out" using 1:390 title "" with lines, \
"allruns.out" using 1:391 title "" with lines, \
"allruns.out" using 1:392 title "" with lines 2
# 10
set title "Stage, IHNC New Orleans (76160), advisory%forecastdate%"
set output "ihnc_new_orleans_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:393 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:394 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:395 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:396 title "Unused" with points 1, \
"allruns.out" every 10 using 1:397 title "Unused" with points 2,\
"allruns.out" using 1:393 title "" with lines 1,\
"allruns.out" using 1:394 title "" with lines, \
"allruns.out" using 1:395 title "" with lines, \
"allruns.out" using 1:396 title "" with lines, \
"allruns.out" using 1:397 title "" with lines 2
# 11
set title "Stage, Sellers Canal at Hwy 90 (82720), advisory%forecastdate%"
set output "sellers_canal_at_hwy_90_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:398 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:399 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:400 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:401 title "Unused" with points 1, \
"allruns.out" every 10 using 1:402 title "Unused" with points 2,\
"allruns.out" using 1:398 title "" with lines 1,\
"allruns.out" using 1:399 title "" with lines, \
"allruns.out" using 1:400 title "" with lines, \
"allruns.out" using 1:401 title "" with lines, \
"allruns.out" using 1:402 title "" with lines 2
# 12
set title "Stage, Harvey Canal at Boomtown (76230), advisory%forecastdate%"
set output "harvey_canal_at_boomtown_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:403 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:404 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:405 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:406 title "Unused" with points 1, \
"allruns.out" every 10 using 1:407 title "Unused" with points 2,\
"allruns.out" using 1:403 title "" with lines 1,\
"allruns.out" using 1:404 title "" with lines, \
"allruns.out" using 1:405 title "" with lines, \
"allruns.out" using 1:406 title "" with lines, \
"allruns.out" using 1:407 title "" with lines 2
# 13
set title "Stage, Barataria Waterway at Lafitte (82875), advisory%forecastdate%"
set output "barataria_waterway_at_lafitte_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:408 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:409 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:410 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:411 title "Unused" with points 1, \
"allruns.out" every 10 using 1:412 title "Unused" with points 2,\
"allruns.out" using 1:408 title "" with lines 1,\
"allruns.out" using 1:409 title "" with lines, \
"allruns.out" using 1:410 title "" with lines, \
"allruns.out" using 1:411 title "" with lines, \
"allruns.out" using 1:412 title "" with lines 2
# 14
set title "Stage, Golden Meadow Floodgate (South) (82260), advisory%forecastdate%"
set output "golden_meadow_floodgate_south_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:413 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:414 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:415 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:416 title "Unused" with points 1, \
"allruns.out" every 10 using 1:417 title "Unused" with points 2,\
"allruns.out" using 1:413 title "" with lines 1,\
"allruns.out" using 1:414 title "" with lines, \
"allruns.out" using 1:415 title "" with lines, \
"allruns.out" using 1:416 title "" with lines, \
"allruns.out" using 1:417 title "" with lines 2
# 15
set title "Stage, USGS Lake Pont. at Midlake, advisory%forecastdate% "
set output "usgs_lake_pont_at_midlake_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:418 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:419 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:420 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:421 title "Unused" with points 1, \
"allruns.out" every 10 using 1:422 title "Unused" with points 2,\
"allruns.out" using 1:418 title "" with lines 1,\
"allruns.out" using 1:419 title "" with lines, \
"allruns.out" using 1:420 title "" with lines, \
"allruns.out" using 1:421 title "" with lines, \
"allruns.out" using 1:422 title "" with lines 2
# 16
set title "Stage, USGS Little Irish Bayou, advisory%forecastdate%"
set output "usgs_little_irish_bayou_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:423 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:424 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:425 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:426 title "Unused" with points 1, \
"allruns.out" every 10 using 1:427 title "Unused" with points 2,\
"allruns.out" using 1:423 title "" with lines 1,\
"allruns.out" using 1:424 title "" with lines, \
"allruns.out" using 1:425 title "" with lines, \
"allruns.out" using 1:426 title "" with lines, \
"allruns.out" using 1:427 title "" with lines 2
# 17
set title "Stage, USGS Suzie Bayou at L. Hermitage Rd, advisory%forecastdate%"
set output "usgs_suzie_bayou_at_l_hermitage_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:428 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:429 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:430 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:431 title "Unused" with points 1, \
"allruns.out" every 10 using 1:432 title "Unused" with points 2,\
"allruns.out" using 1:428 title "" with lines 1,\
"allruns.out" using 1:429 title "" with lines, \
"allruns.out" using 1:430 title "" with lines, \
"allruns.out" using 1:431 title "" with lines, \
"allruns.out" using 1:432 title "" with lines 2
# 18 
set title "Stage, USGS Barataria Pass at Grand Isle, advisory%forecastdate% "
set output "usgs_barataria_pass_at_grand_isle_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:428 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:429 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:430 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:431 title "Unused" with points 1, \
"allruns.out" every 10 using 1:432 title "Unused" with points 2,\
"allruns.out" using 1:428 title "" with lines 1,\
"allruns.out" using 1:429 title "" with lines, \
"allruns.out" using 1:430 title "" with lines, \
"allruns.out" using 1:431 title "" with lines, \
"allruns.out" using 1:432 title "" with lines 2
# 19
set title "Stage, USGS NE Bay Gardene near Point-A-LA-Hache, advisory%forecastdate%"
set output "usgs_ne_bay_gardene_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:433 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:434 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:435 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:436 title "Unused" with points 1, \
"allruns.out" every 10 using 1:437 title "Unused" with points 2,\
"allruns.out" using 1:433 title "" with lines 1,\
"allruns.out" using 1:434 title "" with lines, \
"allruns.out" using 1:435 title "" with lines, \
"allruns.out" using 1:436 title "" with lines, \
"allruns.out" using 1:437 title "" with lines 2
# 20
set title "Stage, USGS Caernarvon Outfall Channel, advisory%forecastdate%"
set output "usgs_caernarvon_outfall_channel_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:438 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:439 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:440 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:441 title "Unused" with points 1, \
"allruns.out" every 10 using 1:442 title "Unused" with points 2,\
"allruns.out" using 1:438 title "" with lines 1,\
"allruns.out" using 1:439 title "" with lines, \
"allruns.out" using 1:440 title "" with lines, \
"allruns.out" using 1:441 title "" with lines, \
"allruns.out" using 1:442 title "" with lines 2
# 21
set title "Stage, USGS IWW at I-510 Bridge, advisory%forecastdate% "
set output "usgs_iww_at_510_bridge_paris_rd_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:443 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:444 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:445 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:446 title "Unused" with points 1, \
"allruns.out" every 10 using 1:447 title "Unused" with points 2,\
"allruns.out" using 1:443 title "" with lines 1,\
"allruns.out" using 1:444 title "" with lines, \
"allruns.out" using 1:445 title "" with lines, \
"allruns.out" using 1:446 title "" with lines, \
"allruns.out" using 1:447 title "" with lines 2
# 22
set title "Stage, USGS Little Lake near Bayou Dogris, advisory%forecastdate%"
set output "usgs_little_lake_near_bayou_dogris_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:448 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:449 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:450 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:451 title "Unused" with points 1, \
"allruns.out" every 10 using 1:452 title "Unused" with points 2,\
"allruns.out" using 1:448 title "" with lines 1,\
"allruns.out" using 1:449 title "" with lines, \
"allruns.out" using 1:450 title "" with lines, \
"allruns.out" using 1:451 title "" with lines, \
"allruns.out" using 1:452 title "" with lines 2
# 23
set title "Stage, USGS MS Sound at Grand Pass, advisory%forecastdate%"
set output "usgs_ms_sound_at_grand_pass_water_level.ps"
set ylabel "Stage (ft) NAVD88"
plot "allruns.out" every 10 using 1:453 title "Consensus Rmax46" with points 7,\
"allruns.out" every 10 using 1:454 title "Consensus Rmax35" with points 4, \
"allruns.out" every 10 using 1:455 title "Consensus Rmax30" with points 8, \
"allruns.out" every 10 using 1:456 title "Unused" with points 1, \
"allruns.out" every 10 using 1:457 title "Unused" with points 2,\
"allruns.out" using 1:453 title "" with lines 1,\
"allruns.out" using 1:454 title "" with lines, \
"allruns.out" using 1:455 title "" with lines, \
"allruns.out" using 1:456 title "" with lines, \
"allruns.out" using 1:457 title "" with lines 2

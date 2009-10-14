set terminal postscript "Times-Roman" 14
set grid
set xlabel "Date %timezone% (mm/dd)"
set xdata time
set format x "%m/%d"
set xtics 86400
set mxtics 24
set key top left box
set missing "-99999"
set timefmt "%Y-%m-%d %H:%M:%S"
#set xrange ["%startgraph%":"%endforecast%"]
set yrange [%ymin%:%ymax%]
#
set title  "%plottitle%"
set output "%psplotname%"
set ylabel "%ylabel%"
plot "%transpose%" every 10 using 1:%col% title "%datatitle%" with points 7,\
"%transpose%" using 1:%col% title "" with lines 1

set terminal postscript "Times-Roman" 14
set grid
set xlabel "Date UTC (mm/dd)"
set xdata time
set format x "%m/%d"
set xtics 86400
set mxtics 24
set key top left box
set missing "-99999" 
set timefmt "%Y%m%d %H:%M:%S"
set xrange ["%startgraph%":"%endforecast%"]

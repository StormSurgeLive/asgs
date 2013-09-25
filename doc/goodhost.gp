set terminal postscript color "Times-Roman" 14 
set grid
set xlabel "Date UTC (mm/dd)"
set xdata time
set format x "%m/%d"
#set xtics 86400
#set mxtics 24
set key top left box
set datafile missing "-99999"
set timefmt "%Y-%m-%d %H:%M:%S"
#set xrange ["%startgraph%":"%endforecast%"]
set yrange [-1.5:1.5]
set ytics 1
set ylabel "Job status"
#
# compute-1-14
set title  "Croatan compute-1-14 run status"
set output "croatan_compute-1-14.ps"
plot "compute-1-14goodlog.txt.dat" using 1:3 title "Success" with impulses lt 1 lc rgbcolor "green" lw 5, \
"compute-1-14badlog.txt.dat" using 1:3 title "Failure" with impulses lt 1 lc rgbcolor "red" lw 5
#
# compute-0-2
set title  "Croatan compute-0-2 run status"
set output "croatan_compute-0-2.ps"
plot "compute-0-2goodlog.txt.dat" using 1:3 title "Success" with impulses lt 1 lc rgbcolor "green" lw 5, \
"compute-0-2badlog.txt.dat" using 1:3 title "Failure" with impulses lt 1 lc rgbcolor "red" lw 5
#
# compute-0-0
set title  "Croatan compute-0-0 run status"
set output "croatan_compute-0-0.ps"
plot "compute-0-0goodlog.txt.dat" using 1:3 title "Success" with impulses lt 1 lc rgbcolor "green" lw 5, \
"compute-0-0badlog.txt.dat" using 1:3 title "Failure" with impulses lt 1 lc rgbcolor "red" lw 5

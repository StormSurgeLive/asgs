set terminal postscript "Times-Roman" 14
set grid
set xlabel "Date UTC (mm/dd)"
set xdata time
set format x "%m/%d"
set xtics 86400
set mxtics 24
set key top left box
set datafile missing "-99999"
set timefmt "%Y-%m-%d %H:%M:%S"
#set xrange ["%startgraph%":"%endforecast%"]
#
# plot the iterations per time step
# 
set yrange [0:22]
set title  "SWAN Iterations Per SWAN Time Step"
set output "swan_it.ps"
set ylabel "Iterations Per Time Step"
plot "./23/padcswan.nhcConsensus.out.d" every 20 using 1:3 title "Advisory 23" with points pt 7, \
"./23/padcswan.nhcConsensus.out.d" using 1:3 title "" with lines lt 1, \
"./25/padcswan.nhcConsensus.out.d" every 20 using 1:3 title "Advisory 25" with points pt 6 lc rgb 'blue', \
"./25/padcswan.nhcConsensus.out.d" using 1:3 title "" with lines lt 1 lc rgb 'blue', \
"./28/padcswan.nhcConsensus.out.d" every 20 using 1:3 title "Advisory 28" with points pt 5 lc rgb 'red',\
"./28/padcswan.nhcConsensus.out.d" using 1:3 title "" with lines lt 1 lc rgb 'red'
#
# plot the % converged at the end of each swan time step
#
set yrange [90:100]
set title  "SWAN Percent Accuracy OK"
set output "swan_accuracy.ps"
set ylabel "Mesh Nodes with Accuracy OK (%)"
plot "./23/padcswan.nhcConsensus.out.d" using 1:4 title "Advisory 23" with lines lt 1,\
"./25/padcswan.nhcConsensus.out.d" using 1:4 title "Advisory 25" with lines lt 1 lc rgb "blue",\
"./28/padcswan.nhcConsensus.out.d" using 1:4 title "Advisory 28" with lines lt 1 lc rgb 'red'

set terminal postscript "Times-Roman" 14
set grid
set xlabel "Date %timezone% (mm/dd)"
set xdata time
set format x "%m/%d"
set xtics 86400
set mxtics 24
set key top left box
set datafile missing "-99999"
set timefmt "%Y-%m-%d %H:%M:%S"
#set xrange ["%startgraph%":"%endforecast%"]
set yrange [%ymin%:%ymax%]
#
set title  "%plottitle%"
set output "%psplotname%"
set ylabel "%ylabel%"
# 
# count the number of non-missing values
count1(fname)=system(sprintf("awk 'NR>2 && $%col%!=-99999 { print $0 }' %s | wc -l",fname))
k=count1("%transpose%")
#
# if this column consists of only missing values, write a message but
# don't plot anything; only pretend to plot something so that we get the
# right time range across the bottom; if there are real values, plot them
if ( k==0 ) \
   set label 1 "no data here" at graph 0.5, graph 0.5; \
   plot "%transpose%" using 1:1 title "";  \
else \
plot "%transpose%" every 10 using 1:%col% pt 7 title "%datatitle%" with points,\
"%transpose%" using 1:%col% lt 1 title "" with lines

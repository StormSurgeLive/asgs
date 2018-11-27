set terminal postscript "Times-Roman" 14
set grid xtics
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
set y2range [%y2min%:%y2max%]
#
set title  "%plottitle%"
set output "%psplotname%"
set ylabel "%ylabel%"
set y2label "%y2label%"
set ytics nomirror
set y2tics %y2tics%
#
# minmax (and possibly time of minmax) label
%minmaxlabel%
#
# bathytopo elevation label
%bathytopolabel%
#
# count the number of non-missing values
count1(fname)=system(sprintf("awk 'NR>2 && $%magcol%!=-99999 { print $0 }' %s | wc -l",fname))
k=count1("%transpose%")
#
# if this column consists of only missing values, write a message but
# don't plot anything; only pretend to plot something so that we get the
# right time range across the bottom; if there are real values, plot them
# (the curly bracket if/else syntax requires gnuplot later than version 4.4)
if ( k==0 ) {
   set label 1 "no data here" at graph 0.5, graph 0.5
   plot "%transpose%" using 1:1 title ""
} else { 
   plot "%transpose%" every %every% using 1:%magcol% pt 7 title "%magdatatitle%" with points,\
   "%transpose%" using 1:($%magcol%) lt 1 title "" with lines,\
   "%transpose%" every %every% using 1:%dircol% pt 1 axes x1y2 title "%dirdatatitle%" with points,\
   "%transpose%" using 1:($%dircol%) lt 1 axes x1y2 title "" with lines
}

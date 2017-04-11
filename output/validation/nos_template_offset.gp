set terminal postscript size 6.4,4.8 "Helevetica-Bold" 14 lw 2 blacktext 
set grid front
set style line 1 lc rgb "#000000" lt 1 lw 0.5 pt 7 ps 0.5
set xlabel "Date GMT (mm/dd)"
set xdata time
set format x "%m/%d"
set xtics 86400
set mxtics 24
set key top left box off
set datafile missing "-99999"
set timefmt "%Y-%m-%d %H:%M:%S"
##set xrange ["not implemented":"not implemented"]
set xrange ["sedyyyystartsed-sedmmstartsed-sedddstartsed 00:00:00":"sedyyyyendsed-sedmmendsed-sedddendsed 00:00:00"]
#set yrange [-5:5]
#
set title  "sedtitlesed vs. sedanalysissed"
set output "sedpsfilesed"
set ylabel "Water Level + offset: sedoffsetsed"
set style fill transparent solid 0.70 noborder
set style function filledcurves y1=0


# 
# count the number of non-missing values
count1(fname)=system(sprintf("awk 'NR>2 && $column!=-99999 { print $0 }' %s | wc -l",fname))
k=count1("sedanltxtfilesed")

#
# if this column consists of only missing values, write a message but
# don't plot anything; only pretend to plot something so that we get the
# right time range across the bottom; if there are real values, plot them
if ( k==0 ) \
   set label 1 "Station Dry for Duration" at graph 0.5, graph 0.5; \
   plot "sedanltxtfilesed" using 1:1 title "";  \
else \
plot "sedanltxtfilesed" using 1:($sedcountsed) with filledcurves x1 lc rgb "blue", \
"sedobtxtfilesed" using 1:($3+sedoffsetsed) title "Water Level" with linespoints ls 1


#set datafile separator ","


# 
## count the number of non-missing values
#count1(fname)=system(sprintf("awk 'NR>2 && $2!=-99999 { print $0 }' %s | wc -l",fname))
#k=count1("sedobtxtfilesed")

##
## if this column consists of only missing values, write a message but
## don't plot anything; only pretend to plot something so that we get the
## right time range across the bottom; if there are real values, plot them

#if ( k==0 ) \
#   set label 1 "Station Dry for Duration" at graph 0.5, graph 0.5; \
#   plot "sedobtxtfilesed" using 1:($2) title "";  \
#else \
#plot "sedobtxtfilesed" using 1:($2) title "Water Level" with linespoints ls 1




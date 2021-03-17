#!/bin/sh
PERL5LIB=~/asgs/trunk/PERL
for advisory in 025 030 035 Hindcast; do
#for advisory in 030 035; do
   time=`date`
   echo "$time Starting advisory $advisory"
   rm fort.22 radii_???.ps radialv.ps radialp.ps page???.gif page???.ps 
   ln -s fort.22.orig.irene.${advisory} fort.22
   # write full circle Rmaxes
   ~/adcirc/trunk/work/aswip -a -o 1800.0 # requires NWS9 fort.22, writes NWS_19_fort.22
#   ~/adcirc/trunk/work/aswip -a  # requires NWS9 fort.22, writes NWS_19_fort.22
   # write radial v and p
   ~/adcirc/trunk/work/aswip -v -o 1800.0 # requires NWS9 fort.22, writes NWS_19_fort.22
#   ~/adcirc/trunk/work/aswip -v # requires NWS9 fort.22, writes NWS_19_fort.22
   # generate shell scripts that will create maps in gmt
   time=`date`
   echo "$time Finished writing out Rmax and radial V and P data."
   time=`date`
   echo "$time Starting generation of plotting scripts."
   perl vortex_viz_gen.pl --advisorynum $advisory --outputincrement 1800.0
#   perl vortex_viz_gen.pl --advisorynum $advisory 
   # execute shell script to create figures with gmt
   chmod +x plot_radii.sh
   time=`date`
   echo "$time Finished generation of plotting scripts."
   time=`date`
   echo "$time Starting execution of plotting scripts."
   ./plot_radii.sh
   # execute gnuplot script to create line graphs with gmt
   gnuplot radial_v_and_p.gp
   time=`date`
   echo "$time Finished execution of plotting scripts."
   # combine plots into single document
   plotnames=""
   time=`date`
   echo "$time Starting conversion from ps to gif."
   for file in `ls radii_???.ps`; do
      cycle=`expr substr $file 7 3`
      plotnames="$file radialv_${cycle}.ps radialp_${cycle}.ps"
      psmerge -opages${cycle}.ps $plotnames
      # write wind radii plots to new document, three to a page
      psnup -n 3 -c -d -pletter pages${cycle}.ps > page${cycle}.ps
      # now create gifs for animation
      convert -rotate 180 page${cycle}.ps page${cycle}.gif
   done
   time=`date`
   echo "$time Finished conversion from ps to gif."
   # merge to one big doc and convert ps to pdf
   pages_name=`ls page???.ps`
   psmerge -ostuff2.ps `ls page???.ps`
   ps2pdf stuff2.ps irene_${advisory}.pdf
   convert -loop 0 -delay 10 page???.gif anim_${advisory}.gif
   time=`date`
   echo "$time Finished advisory ${advisory}."
done
#rm fort.22 radii_cycle???.ps radialv.ps radialp.ps page???.gif page???.ps 

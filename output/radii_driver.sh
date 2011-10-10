#!/bin/sh
PERL5LIB=~/asgs/trunk/PERL
for advisory in 25 30 35 Hindcast; do
   rm fort.22 radii_cycle??.ps radialv.ps radialp.ps 
   ln -s fort.22.orig.irene.${advisory} fort.22
   # write full circle Rmaxes
   ~/adcirc/trunk/work/aswip -a # requires NWS9 fort.22, writes NWS_19_fort.22
   # write radial v and p
   ~/adcirc/trunk/work/aswip -v # requires NWS9 fort.22, writes NWS_19_fort.22
   # generate shell scripts that will create maps in gmt
   perl vortex_viz_gen.pl --advisorynum $advisory
   # execute shell script to create figures with gmt
   chmod +x plot_radii.sh
   ./plot_radii.sh
   # execute gnuplot script to create line graphs with gmt
   gnuplot radial_v_and_p.gp
   # combine plots into single document
   plotnames=""
   for file in `ls radii_cycle??.ps`; do
      cycle=`expr substr $file 12 2`
      plotnames="${plotnames} $file radialv_${cycle}.ps radialp_${cycle}.ps"
   done
   psmerge -ostuff.ps $plotnames
   # write wind radii plots to new document, six to a page
   psnup -n 3 -c -d -pletter stuff.ps > stuff2.ps
   # convert ps to pdf
   ps2pdf stuff2.ps irene_${advisory}.pdf
done

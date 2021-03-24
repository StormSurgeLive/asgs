#! /bin/csh
#BSUB -J viz
#BSUB -o viz.%J
#BSUB -e viz.%J
#BSUB -W 120
#BSUB -n 1
#BSUB -q ccee
#BSUB -R span[ptile=1]

source /usr/local/apps/python/python2713.csh
source /usr/local/apps/grass/grass.csh

setenv LD_LIBRARY_PATH /usr/local/apps/proj/gcc483-4.8.0/lib:${LD_LIBRARY_PATH}
setenv LD_LIBRARY_PATH /usr/local/apps/proj/gcc483-2.1.1/lib:${LD_LIBRARY_PATH}

source /usr/local/apps/mpich3/centos7/intelmpi2016.csh

date > rgrow_pll.started
python ./kalpana_vizMod.py --storm storm --filetype maxele.63.nc --contourrange "0 21 0.5" --units english --grow yes --grownoutput outputPlaceholder --growradius none
date > rgrow_pll.finished

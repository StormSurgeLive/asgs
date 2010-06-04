#!/bin/bash
#
# Copyright(C) 2008, 2009 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#
   CONFIG=$1
   ADVISDIR=$2
   STORM=$3
   YEAR=$4
   ADVISORY=$5
   HOSTNAME=$6
   ENSTORM=$7
   CSDATE=$8
   HSTIME=$9
   GRIDFILE=${10}
   OUTPUTDIR=${11}
   SYSLOG=${12}
  
   #
   . ${CONFIG} # grab all static config info
   #
   # grab storm class and name from file
  #   STORMNAME=`cat nhcClassName` 
  #   STORMNAME=${STORMNAME}" "${YEAR}

   # switch to tracking directory
   initialDirectory=`pwd`;
   mkdir ${ADVISDIR}/${ENSTORM}/PartTrack
   # mv *.txt *.csv ${ADVISDIR}/$ENSTORM/tracking
   cd ${ADVISDIR}/$ENSTORM/PartTrack

   # 1) Process Grid
   GRIDPREFIX=`basename $GRIDFILE .grd`

     TRACKDIR=$OUTPUTDIR/PartTrack

     ln -fs  ${ADVISDIR}/${ENSTORM}/fort.14  ./${GRIDPREFIX}
     ln -fs $TRACKDIR/connect2D_optimized.exe ./

   # Generate the input file with initial positions

       ln -fs $TRACKDIR/gen_part_track_input.sh
     ./gen_part_track_input.sh $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $ENSTORM $OUTPUTDIR 
      echo '  ./gen_part_track_input.sh $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $ENSTORM $OUTPUTDIR '  > $HOME/gen_log.log

   # create drogue_input_1 file

   # name of input file is hardwired as input_deepwater.din
   # output will be input_deepwater.pth
        echo input_deepwater > drogue_input_1
        echo ${GRIDPREFIX} >> drogue_input_1

   # process the grid file

   # before we process the file lets look for an already processed file in 
   # the $outputdir/parttrack/input directory

       
     if [ ! -e /corral/hurricane/grids/${GRIDPREFIX}.gr2 ] 
     then
    ./connect2D_optimized.exe 
      cp ./${GRIDPREFIX}.gr2 /corral/hurricane/grids/${GRIDPREFIX}.gr2
     else
   #  ln -fs  /corral/hurricane/grids/${GRIDPREFIX}.gr2 ./${GRIDPREFIX}.gr2
      cp  /corral/hurricane/grids/${GRIDPREFIX}.gr2 ./${GRIDPREFIX}.gr2
     fi
   
   # 2) Track Particles
      # Write CB_2D.h file

    NumElem=$(head -2 ./${GRIDPREFIX} | tail -1 | awk '{print $1}')
    NumNode=$(head -2 ./${GRIDPREFIX} | tail -1 | awk '{print $2}')
    NumFreq=1
    MxDrog=40000
    MxTime=3600
    Tol=50.0D0
     echo $NumElem $NumNode
     echo "         IMPLICIT NONE                  "     >  ./CB_2D.h
     echo "         INTEGER NNE,ND,NFR,MXDRG,MXTIME,MXFILETIME" >>  ./CB_2D.h
     echo "         REAL*8 REARTH,TOL                 "    >> ./CB_2D.h
     echo "            PARAMETER (NNE=${NumElem})     "    >> ./CB_2D.h
     echo "            PARAMETER (ND=${NumNode})      "    >> ./CB_2D.h
     echo "            PARAMETER (NFR=${NumFreq})     "    >> ./CB_2D.h
     echo "            PARAMETER (MXDRG=${MxDrog})    "    >> ./CB_2D.h
     echo "            PARAMETER (MXTIME=${MxTime})   "    >> ./CB_2D.h
     echo "            PARAMETER (REARTH=6.3675D6)    "    >> ./CB_2D.h
     echo "            PARAMETER (TOL=${Tol})         "    >> ./CB_2D.h

     ln -fs $TRACKDIR/src/drog2dsp_deepwater.f ./drog2dsp_deepwater.f

   module unload intel/9.1
   module load intel/11.1

     ifort -w  drog2dsp_deepwater.f -o drog2dsp_deepwater.exe
#     ifort -w -mcmodel medium -shared-intel drog2dsp_deepwater.f -o drog2dsp_deepwater.exe


   # need the fort.64 file
     ln -fs $ADVISDIR/$ENSTORM/fort.64  ./fort.64.v2c
# run on logingnode until solution to shared files available on compute nodes
#        ./drog2dsp_deepwater.exe > ./PartTrack_01.out

        SERQSCRIPT=ranger.PartTrack.template.serial
       SERQSCRIPTOPTIONS="--account $ACCOUNTpost --adcircdir $TRACKDIR --advisdir $ADVISDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER --serqscript $TRACKDIR/$SERQSCRIPT"
       perl $TRACKDIR/ranger.PartTrack.serial.pl  $SERQSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/PartTrack/parttrack.serial.sge 2>> ${SYSLOG}
       echo "Submitting $ADVISDIR/$ENSTORM/PartTrack/parttrack.serial.sge"
       qsub $ADVISDIR/$ENSTORM/PartTrack/parttrack.serial.sge >> ${SYSLOG} 2>&1
           counter1=0
       while [ ! -e ./run.finish ]; do
              counter1=`expr $counter1 + 1`
          #echo $counter1 ' particle path file not ready yet'
           sleep 60
       done
            echo  $counter1
           sleep 30
   # 3) Generate vizualizations
      
     ln -fs $TRACKDIR/FigureGen42_serial.exe ./
#     ln -fs $TRACKDIR/FigureGen42_parallel.exe ./
#     ln -fs $TRACKDIR/FG42_template.inp ./
     ln -fs $TRACKDIR/Hue_standard_01.pal ./
     ln -fs $ADVISDIR/$ENSTORM/maxele.63 ./
     ln -fs $ADVISDIR/$ENSTORM/fort.64  ./fort.64


    # create FigGen input File
        GSHOME2=/usr/bin/
       GMTHOME2=/work/01053/rweaver/GMT4.5.0/bin/
       STARTTIME=$(head -1 $ADVISDIR/$ENSTORM/hotstartdate | tail -1 | awk '{print $1}')
       NumRecords=$(head -2 ./fort.64 | tail -1 | awk '{print $1}')  # 

       STARTTIME2=${STARTTIME}0000
        echo $STARTTIME2
       OUTPUTPREFIX_fg=PartTrack_${ADVISORY}_
       OUTPUTPREFIX_kmz=PartTrack_${ADVISORY}

         ln -fs $TRACKDIR/make_ptFG_input.pl ./
  perl make_ptFG_input.pl --outputdir $TRACKDIR  --gmthome $GMTHOME2 --gridfile $GRIDPREFIX --gshome $GSHOME2 --storm ${STORM} --year ${YEAR} --adv $ADVISORY --n 31.0 --s 18.0 --e -80.0 --w -98.0 --outputprefix ${OUTPUTPREFIX_fg} --starttime $STARTTIME2 --numrecords $NumRecords
 #   perl make_ptFG_input.pl --outputdir $TRACKDIR --gmthome $GMTHOME --gridfile $GRIDPREFIX --gshome $GSHOME --storm ${STORM} --year ${YEAR} --adv $ADVISORY --n $NORTH --s $SOUTH --e $EAST --w $WEST --outputprefix $OUTPUTPREFIX --starttime $STARTTIME --numrecords $NumRecords

  # Serial run on login(Head)  node
      ./FigureGen42_serial.exe >> $ADVISDIR/$ENSTORM/figgen_track.log 2>&1  &

  # Serial run
      # SERQSCRIPT=ranger.PartTrackFG.template.serial
      # SERQSCRIPTOPTIONS="--account $ACCOUNT --adcircdir $TRACKDIR --advisdir $ADVISDIR --enstorm $ENSTORM --notifyuser $NOTIFYUSER --serqscript $TRACKDIR/$SERQSCRIPT"
      # perl $TRACKDIR/ranger.FigGen42.serial.pl  $SERQSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/PartTrack/parttrackfg.serial.sge 2>> ${SYSLOG}
      # logMessage "Submitting $ADVISDIR/$ENSTORM/PartTrack/parttrackfg.serial.sge"
      # qsub $ADVISDIR/$ENSTORM/PartTrack/parttrackfg.serial.sge >> ${SYSLOG} 2>&1


  # Parallel run
  #     QSCRIPT=ranger.PartTrackFG.template.sge
  #     QSCRIPTOPTIONS="--ncpu 80 --ncpudivisor 16 --queuename development --account $ACCOUNT --adcircdir $TRACKDIR --advisdir $ADVISDIR --qscript $TRACKDIR/$QSCRIPT --enstorm $ENSTORM --notifyuser $NOTIFYUSER --walltime 00:20:00  --syslog $SYSLOG"
   #    perl $TRACKDIR/ranger.FigGen42.sge.pl  $PARQSCRIPTOPTIONS > $ADVISDIR/$ENSTORM/parttrackfg.parallel.sge 2>> ${SYSLOG}
   #    logMessage "Submitting $ADVISDIR/$ENSTORM/parttrackfg.parallel.sge"
   #    qsub $ADVISDIR/$ENSTORM/parttrackfg.parallel.sge >> ${SYSLOG} 2>&1
     
  #    ./FigureGen42_parallel.exe >> $ADVISDIR/$ENSTORM/figgen_track.log 2>&1  &

#    while [ ! -e ./run.finish ]; do
    while [ ! -e ./${OUTPUTPREFIX_kmz}.kmz ]; do
      sleep 60
   done
     sleep 30
  
#        cp ./${OUTPUTPREFIX_kmz}.kmz  ${HOME}
        cp ./${OUTPUTPREFIX_kmz}.kmz /corral/hurricane/rweaver
        cp ./input_deepwater.pth  /corral/hurricane/rweaver/${OUTPUTPREFIX_kmz}.pth
                      
   cd $initialDirectory

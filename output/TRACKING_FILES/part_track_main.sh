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
# part_track_main.sh ./part_track_config.sh  
   CONFIG=$1
# config set the following variables
# STORM=  # storm number, e.g. 05=ernesto in 2006 
# STORMNAME= Name of storm
# YEAR= # year of the storm (useful for historical storms) 
# CSDATE= date of cold start of adcirc run ie the refernce date time for the time given for each data record
# OUTPUTDIR= dir containing post processing scripts
# INPUTDATADIR= location of the fort.14 fort.63 fort.64 fort.74 files
# TRACKDIR= location of files needed to run this script
# ADVISORY= 
# ADVISDIR=
# ENSTORM=
# PARTICLEFILE= path and name of initial position file 
# PARTFILETYPE= does part initial pos file include particle numbers or not (4columns or 3)
# NORTH= plot bounding box limit
# SOUTH= plot bounding box limit
# EAST= plot bounding box limit
# WEST= plot bounding box limit
# SYSLOG= logfile 
#

   #
   . ${CONFIG} # grab all static config info
   #
   # grab storm class and name from file

   # switch to tracking directory
   initialDirectory=`pwd`;
   
   mkdir ${STORMNAME}
   mkdir ${STORMNAME}/TRACKING
   mkdir ${STORMNAME}/TRACKING/IMAGES
   cd ${STORMNAME} 
     ADVISDIR=$initialDirectory/${STORMNAME}
     PTDIR=$initialDirectory/${STORMNAME}/TRACKING
     PTIMAGESDIR=$initialDirectory/${STORMNAME}/TRACKING/IMAGES
   # 1) GATHER REQUIRED FILES

     ln -fs  ${INPUTDATADIR}/fort.14  ./
     ln -fs  ${INPUTDATADIR}/fort.63  ./
     ln -fs  ${INPUTDATADIR}/fort.64  ./
     ln -fs  ${INPUTDATADIR}/fort.74  ./
     ln -fs $TRACKDIR/Date ./

# run script to generate the Elevation and Velocity images
     
    ln -fs $TRACKDIR/FG_elev_vel_images.sh ./
    ./FG_elev_vel_images.sh  $CONFIG > FG_ELEV_VEL_log.log 2>> ${SYSLOG} &
     
  
    # now enter TRACKING directory to track particles
     cd $PTDIR

     GRID=fort.14
     VELINPUTFILE=fort.64   
     ln -fs $TRACKDIR/connect2D_optimized.exe ./
     ln -fs  ${INPUTDATADIR}/fort.64  ./
     ln -fs  ${INPUTDATADIR}/fort.14  ./
     ln -fs $TRACKDIR/Date ./

   # Generate the input file with initial positions
          
       ln -fs $TRACKDIR/gen_part_track_input.sh ./
     ./gen_part_track_input.sh $CONFIG $VELINPUTFILE $TRACKDIR $PARTICLEFILE 
      echo ' ./gen_part_track_input.sh' $CONFIG $VELINPUTFILE $TRACKDIR $PARTICLEFILE  >> $SYSLOG

   # create drogue_input_1 file

   # name of input file is hardwired as input_deepwater.din
   # output will be input_deepwater.pth
        echo input_deepwater > drogue_input_1
        echo ${GRID} >> drogue_input_1

   # process the grid file

    ./connect2D_optimized.exe 
   
   # 2) Track Particles
      # Write CB_2D.h file

    NumElem=$(head -2 ./${GRID} | tail -1 | awk '{print $1}')
    NumNode=$(head -2 ./${GRID} | tail -1 | awk '{print $2}')
    NumFreq=1
    MxDrog=$(wc -l $PARTICLEFILE | awk '{print $1}')
    MxTime=3600
    Tol=50.0D0
     echo $NumElem $NumNode

       NumNode=$NumNode+100000

     echo "         IMPLICIT NONE                  "     >  ./CB_2D.h
     echo "         INTEGER NNE,ND,NFR,MXDRG,MXTIME,MXFILETIME" >>  ./CB_2D.h
     echo "         INTEGER NDIV                              " >>  ./CB_2D.h
     echo "         REAL*8 REARTH,TOL                 "    >> ./CB_2D.h
     echo "            PARAMETER (NNE=${NumElem})     "    >> ./CB_2D.h
     echo "            PARAMETER (ND=${NumNode})      "    >> ./CB_2D.h
     echo "            PARAMETER (NFR=${NumFreq})     "    >> ./CB_2D.h
     echo "            PARAMETER (MXDRG=${MxDrog})    "    >> ./CB_2D.h
     echo "            PARAMETER (MXTIME=${MxTime})   "    >> ./CB_2D.h
     echo "            PARAMETER (REARTH=6.3675D6)    "    >> ./CB_2D.h
     echo "            PARAMETER (TOL=${Tol})         "    >> ./CB_2D.h
     echo "            PARAMETER (NDIV=1000)          "    >> ./CB_2D.h


   module purge
   module load TACC
   module load intel/11.1
 
     if [ $PARTFILETYPE -eq 0 ]; then
      # no particle numbers 3 columns oil locations
     ln -fs $TRACKDIR/drog2dsp_deepwater.f ./drog2dsp_deepwater.f
     elif [ $PARTFILETYPE -eq 1 ]; then 
     ln -fs $TRACKDIR/drog2dsp_deepwater_node.f ./drog2dsp_deepwater.f
     fi

     ifort -w  drog2dsp_deepwater.f -o drog2dsp_deepwater.exe


   # need the fort.64 file
     ln -fs ./fort.64  ./fort.64.v2c

        SERQSCRIPT=ranger.PartTrack.template.serial
       SERQSCRIPTOPTIONS="--account $ACCOUNT --ptdir $PTDIR --queue request --notifyuser $NOTIFYUSER --serqscript $TRACKDIR/$SERQSCRIPT"
         echo $SERQSCRIPTOPTIONS >> $SYSLOG
       perl $TRACKDIR/ranger.PartTrack.serial.pl  $SERQSCRIPTOPTIONS > $PTDIR/parttrack.serial.sge 2>> ${SYSLOG}
       echo "Submitting $PTDIR/parttrack.serial.sge"
       qsub $PTDIR/parttrack.serial.sge >> ${SYSLOG} 2>&1
          counter1=0
       while [ ! -e ./run.pt.finish ]; do
              if [ -e ./run.pt.error ]
              then
               echo "Particle Tracking finished with error"  >> $SYSLOG
               exit
              fi
              counter1=`expr $counter1 + 1`
           sleep 60
       done
           echo  $counter1
           sleep 30

   # 3) Generate vizualizations
       OUTPUTPREFIX=${TYPE}_${ADVISORY}_
       OUTPUTPREFIX_file=${TYPE}_${ADVISORY}
     InitPartTime=$(head -1 ./InitialParticleTime.txt | tail -1 | awk '{print $1}')

      OUTPUTPREFIX_final=PrtTrk_${KIND}${ADVISORY}_IPL${InitPartTime}

     ln -fs $TRACKDIR/FG_particle_images.sh 
    ./FG_particle_images.sh $CONFIG $PTDIR
   
      
    while [ ! -e $PTDIR/run.ps.finish ]; do
             if [ -e ./run.ps.error ]
              then
               echo "ps2raster on Particle Images finished with error"  >> $SYSLOG
               exit
              fi
      sleep 30
   done
    cd $ADVISDIR
      sleep 30
   
   mkdir $ADVISDIR/MONTAGE
   cd $ADVISDIR/MONTAGE

   ls $PTDIR/*.jpg > Part_FileList
   ls $ADVISDIR/*.jpg > Elev_FileList
    
        QSCRIPT=montage.template.serial.sge
 QSCRIPTOPTIONS="--account $ACCOUNT --dir $ADVISDIR/MONTAGE --queue vis  --qscript $TRACKDIR/$QSCRIPT --notifyuser $NOTIFYUSER --storm $STORMNAME"
   perl $TRACKDIR/ranger.montage.serial.pl $QSCRIPTOPTIONS > $ADVISDIR/MONTAGE/montage.sge  2>> ${SYSLOG}
      echo "Submitting $ADVISDIR/MONTAGE/montage.sge"  >> $SYSLOG
       qsub $ADVISDIR/MONTAGE/montage.sge >> ${SYSLOG} 2>&1
   while [ ! -e $ADVISDIR/MONTAGE/run.mon.finish ]; do
      sleep 30
   done

     mkdir /corral/hurricane/rweaver/$STORMNAME_${OUTPUTPREFIX_final}
      cp *.gif /corral/hurricane/rweaver/$STORMNAME_${OUTPUTPREFIX_final}
      cp *.jpg /corral/hurricane/rweaver/$STORMNAME_${OUTPUTPREFIX_final}
      
   cd $initialDirectory

  
#        mv ./${OUTPUTPREFIX_kmz}.kmz  ${OUTPUTPREFIX_final}.kmz
#        mv ./input_deepwater.pth      ${OUTPUTPREFIX_final}.pth
#        cp ./${OUTPUTPREFIX_final}.kmz /corral/hurricane/rweaver/${OUTPUTPREFIX_final}.kmz
#        cp ./${OUTPUTPREFIX_final}.pth /corral/hurricane/rweaver/${OUTPUTPREFIX_final}.pth

  # change group and permissions to be sure everyone can read the archived files

    chgrp -R G-81535  /corral/hurricane/rweaver/*gif
    chgrp -R G-81535  /corral/hurricane/rweaver/*jpg
    chmod 640 /corral/hurricane/rweaver/*pth /corral/hurricane/rweaver/*gif
    chmod 640 /corral/hurricane/rweaver/*pth /corral/hurricane/rweaver/*jpg
 

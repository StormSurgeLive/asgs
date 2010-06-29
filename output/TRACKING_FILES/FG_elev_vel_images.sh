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
   ADVISDIR=`pwd`
   echo "pwd =" `pwd`

   # start vis of elevation and velocity in top directory
   # here we just need to run figgen on the ADCIRC output files
     ln -fs $TRACKDIR/Default2.pal ./
     ln -fs $TRACKDIR/ParticlePalette1.txt ./
     ln -fs $TRACKDIR/adcirc_logo_white.eps ./
     ln -fs $TRACKDIR/Plot_Title_maker.pl ./
     ln -fs $TRACKDIR/Date ./
     echo `pwd`
  grep '  -9.9999000000E+04' $ADVISDIR/fort.63 > date_time_info.txt
      TYPE=Elev
     PlotTitleScriptOptions="--cst $CSDATE --gmtoffset 0 --fortdate date_time_info.txt --storm $STORMNAME --kind $KIND --type $TYPE --partinfo $PARTICLEFILE"
     perl ./Plot_Title_maker.pl $PlotTitleScriptOptions   2>> ${SYSLOG}
  # outputfilenme is $storm."_".$type."_Title.txt"
  # now make input file for elevation and velocity run   
         ln -fs $TRACKDIR/make_ptFG_input.pl ./

       OUTPUTPREFIX=${TYPE}_${ADVISORY}_
       OUTPUTPREFIX_file=${TYPE}_${ADVISORY}
        NumRecords=$(head -2 ./fort.64 | tail -1 | awk '{print $1}')  #


    FG_INPUTSCRIPTOPTIONS="--outputdir $TRACKDIR  --gmthome $GMTHOME2 --gridfile fort.14 --gshome $GSHOME2 --storm ${STORM} --year ${YEAR} --adv $ADVISORY --n $NORTH --s $SOUTH --e $EAST --w $WEST --outputprefix ${OUTPUTPREFIX} --starttime $CSDATE --numrecords $NumRecords --type $TYPE --vectorlimits $VECTORLIM --contourlimits $CONTOURLIM"
       echo `date` "  Creating FG input file"  >> $SYSLOG
       echo $FG_INPUTSCRIPTOPTIONS  >> $SYSLOG
  perl make_ptFG_input.pl $FG_INPUTSCRIPTOPTIONS  2>> ${SYSLOG}

          NumCPU=$NumRecords+1
        let remainder=$NumCPU%16
         until [ $remainder == 0 ]
          do
            let NumCPU=$NumCPU+1
            let remainder=$NumCPU%16
            echo $NumCPU
          done
           echo $NumCPU is divisible by 16

          ln -fs $TRACKDIR/FigureGen42_PartTrack_parallel.exe ./
  #Paralllel run
     QSCRIPT=ranger.PartTrackFG.template.sge
     QSCRIPTOPTIONS="--ncpu  $NumCPU --ncpudivisor 16 --queue request --account $ACCOUNT --dir $ADVISDIR --qscript $TRACKDIR/$QSCRIPT --notifyuser $NOTIFYUSER --storm $STORMNAME "
 
       perl $TRACKDIR/ranger.FigGen42.sge.pl  $QSCRIPTOPTIONS > $ADVISDIR/parttrackfg.parallel.sge 2>> ${SYSLOG}
       echo "Submitting $ADVISDIR/parttrackfg.parallel.sge"  >> $SYSLOG

       qsub $ADVISDIR/parttrackfg.parallel.sge >> ${SYSLOG} 2>&1

     while [ ! -e ./run.fig.finish ]; do
       sleep 60
   done
      sleep 30

     rm -f $ADVISDIR/Temp/*

     ls *.ps > PS_FileList

    QSCRIPT=ps2raster.template.serial.sge
 QSCRIPTOPTIONS="--account $ACCOUNT  --dir $ADVISDIR --queue vis  --gmthome $GMTHOME2 --gshome $GSHOME2 --storm ${STORM} --qscript $TRACKDIR/$QSCRIPT --notifyuser $NOTIFYUSER"

   perl $TRACKDIR/ranger.ps2raster.serial.pl $QSCRIPTOPTIONS > $ADVISDIR/ps2raster.sge  2>> ${SYSLOG}
      echo "Submitting $ADVISDIR/ps2raster.sge"  >> $SYSLOG
       qsub $ADVISDIR/ps2raster.sge >> ${SYSLOG} 2>&1
     while [ ! -e ./run.ps.finish ]; do
          if [ -e ./run.ps.error ]
          then
            echo "ps2raster on Elevation/Velocity Images finished with error"  >> $SYSLOG
            exit
          fi
       sleep 30
     done
    
     rm -f  $ADVISDIR/*.ps


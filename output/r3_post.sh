#!/bin/bash
#
# Copyright(C) 2008, 2009, 2010, 2011 Jason Fleming
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
SSHKEY=${13}
#
. ${CONFIG} # grab all static config info
#
#
# write the target area to the run.properties file for the CERA
# web app
echo "asgs : va" >> run.properties
#
# grab storm class and name from file
if [[ $BACKGROUNDMET = on ]]; then
   # the NAM cycle time is the last two digits of the "advisory"
   namcyclehour=${ADVISORY:8:2}
   STORMNAME="NAM ${namcyclehour}Z"
fi
if [[ $TROPICALCYCLONE = on ]]; then 
   STORMCLASSNAME=`cat nhcClassName` 
   # find the space between the storm class (TD, TS, HU, etc) and the NHC name
   ind=`expr index "$STORMCLASSNAME" ' '`
   # just use the storm's name 
   STORMNAME=${STORMCLASSNAME:$ind}
fi
#
#  G I S     K M Z      J P G 
#
# name of bounding box for contour plots (see config_simple_gmt_pp.sh
# for choices)
BOX=VA
# FigureGen executable to use for making JPG files (assumed to be located
# in $OUTPUTDIR/POSTPROC_KMZGIS/FigGen/
FIGUREGENEXECUTABLE=FigureGen32_prompt_inp.exe
# The full path and name for the FigureGen template file.
FIGUREGENTEMPLATE=$OUTPUTDIR/POSTPROC_KMZGIS/FigGen/FG_asgs.inp.orig
#
#  now create the Google Earth (kmz), jpg, and GIS contour plots
export PATH=$PATH:$IMAGEMAGICKPATH
${OUTPUTDIR}/POSTPROC_KMZGIS/POST_SCRIPT_Corps.sh $ADVISDIR $OUTPUTDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $GRIDFILE $CONFIG $BOX $FIGUREGENEXECUTABLE $FIGUREGENTEMPLATE
#
#  P U B L I C A T I O N
#
# grab the names of the output files
GISKMZJPG=`ls *KMZ_GIS.tar.gz`
PLOTS=`ls *plots.tar.gz`
#
# now create the index.html file to go with the output
perl ${OUTPUTDIR}/corps_index.pl --stormname $STORMNAME --advisory $ADVISORY --templatefile ${OUTPUTDIR}/corps_index_template.html --giskmzjpgarchive $GISKMZJPG --plotsarchive $PLOTS > index.html
#
# now copy plots and visualizations to the website, based on the forcing
# (i.e., NAM or NHC tropical cyclone), machine on which they were run, the 
# grid name, and the advisory 
if [[ $BACKGROUNDMET = on ]]; then
   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "mkdir -p ${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/0.25DegreeResolution/$ADVISORY"
   scp -i $SSHKEY index.html ${WEBUSER}@${WEBHOST}:${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/0.25DegreeResolution/$ADVISORY
   scp -i $SSHKEY $GISKMZJPG ${WEBUSER}@${WEBHOST}:${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/0.25DegreeResolution/$ADVISORY
   scp -i $SSHKEY $PLOTS ${WEBUSER}@${WEBHOST}:${WEBPATH}/NAM/$GRIDFILE/$HOSTNAME/0.25DegreeResolution/$ADVISORY
   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "chmod -R 755 ${WEBPATH}/NAM"
fi
if [[ $TROPICALCYCLONE = on ]]; then 
   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "mkdir -p ${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}"
   scp -i $SSHKEY index.html ${WEBUSER}@${WEBHOST}:${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}
   scp -i $SSHKEY $GISKMZJPG ${WEBUSER}@${WEBHOST}:${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}
   scp -i $SSHKEY $PLOTS ${WEBUSER}@${WEBHOST}:${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}
   ssh ${WEBHOST} -l ${WEBUSER} -i $SSHKEY "chmod -R 755 ${WEBPATH}/$STORMNAME$YEAR/$GRIDFILE/$HOSTNAME/$ENSTORM/advisory_${ADVISORY}"
fi
#
# Convert results to NetCDF format and copy to RENCI for post processing.
# Copy to directory where they can be published via opendap
REMOTEPPDIR=/projects/ncfs/apps/asgs/trunk/output
perl ${OUTPUTDIR}/asgsConvertR3ToNETCDF.pl --ppdir ${SCRIPTDIR}/output --griddir ${OUTPUTDIR}/POSTPROC_KMZGIS/grids --opendapbasedir $OPENDAPBASEDIR --pathonly
OPENDAPPATH=`cat opendappath.log`
ssh ${OPENDAPHOST} "mkdir -p $OPENDAPPATH"
perl ${OUTPUTDIR}/asgsConvertR3ToNETCDF.pl --ppdir ${SCRIPTDIR}/output --remoteppdir $REMOTEPPDIR  --griddir ${OUTPUTDIR}/POSTPROC_KMZGIS/grids --opendapbasedir $OPENDAPBASEDIR --remote --sshkey $SSHKEY --opendapuser $OPENDAPUSER --opendaphost $OPENDAPHOST --tolist "jason.fleming@seahorsecoastal.com, jason.g.fleming@gmail.com"
ssh ${OPENDAPHOST} "chmod +r $OPENDAPPATH/*"

#!/bin/bash
#------------------------------------------------------------------------
# kalpana_post.sh: Provides test case to show that the shapefile and
# kmz generation script Kalpana.py is functioning properly. 
#------------------------------------------------------------------------
# Copyright(C) 2015 Jason Fleming
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
#------------------------------------------------------------------------
#
# example invokation:
# bash ~/asgs/trunk/output/test/kalpana_post.sh ~/asgs/config/asgs_config_arthur_swan_hatteras14_nc9.99_ncgahm_kalpanatest.sh  /srv/asgs/asgs3053/12 01 2014 12 hatteras nhcConsensus 2014042900 5680800.00000000 nc_inundation_v9.99.grd ~/asgs/trunk/output ~/asgs/log/kalpana_test.log ~/.ssh/id_rsa ~/asgs/trunk
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
SCRIPTDIR=${14}
#
STORMDIR=${ADVISDIR}/${ENSTORM}       # shorthand
cd ${STORMDIR}
# get the forecast ensemble member number 
ENMEMNUM=`grep "forecastEnsembleMemberNumber" ${STORMDIR}/run.properties | sed 's/forecastEnsembleMemberNumber.*://' | sed 's/^\s//'` 2>> ${SYSLOG}
#
# grab all config info
si=$ENMEMNUM
#
# grab all config info
. ${CONFIG} 
# Bring in logging functions
. ${SCRIPTDIR}/logging.sh
# Bring in platform-specific configuration
. ${SCRIPTDIR}/platforms.sh
# dispatch environment (using the functions in platforms.sh)
env_dispatch ${TARGET}
# grab all config info (again, last, so the CONFIG file takes precedence)
. ${CONFIG}
#
# write the target area to the run.properties file for the CERA
# web app
echo "asgs : kalpanatest" >> run.properties 2>> $SYSLOG
echo "enstorm : $ENSTORM" >> run.properties 2>> $SYSLOG
#
# Grab the name of the storm from the run.properties file if this is 
# a tropical cyclone; otherwise set the storm name to NAM.
if [[ $TROPICALCYCLONE = on ]]; then
   STORMNAME=`grep "stormname" ${STORMDIR}/run.properties | sed 's/stormname.*://' | sed 's/^\s//g' | tail -n 1` 2>> ${SYSLOG}
   # make the storm name lower case
   STORMNAMELC=`echo $STORMNAME | tr '[:upper:]' '[:lower:]'` 2>> ${SYSLOG}
fi
#
# Format/construct the name of the storm as Kalpana expects to receive it.
KALPANANAME=asgs.${STORMNAMELC}.${ADVISORY}.${ENSTORM}.${GRIDNAME}.${INSTANCE}
#
# Call the script to generate the input file for Kalpana. 
perl ${OUTPUTDIR}/kalpana_input.pl --template ${OUTPUTDIR}/kalpana_input.template --name $KALPANANAME --filechoice 2 --shape B --vchoice Y --domain Y --l '36 33.5 -60 -100' --lonlatbuffer 0 > input-kml.maxele 2>> ${SYSLOG}
perl ${OUTPUTDIR}/kalpana_input.pl --template ${OUTPUTDIR}/kalpana_input.template --name $KALPANANAME --filechoice 2 --shape B --vchoice X --domain N > input-shp.maxele 2>> ${SYSLOG}
#
# Link in the palette file(s) that Kalpana expects to find in the local directory.
ln -s ${OUTPUTDIR}/water-level.pal ${STORMDIR} 2>> ${SYSLOG}
#
# Link in the logo bar that Kalpana expects to find in the local directory
# for the top of the Google Earth visualization. 
ln -s ${OUTPUTDIR}/kalpana_logo.png ${STORMDIR}/logo.png 2>> ${SYSLOG}
#
# Call Kalpana to generate the output product(s).
python ${OUTPUTDIR}/kalpana.py < input-kml.maxele 2>> ${SYSLOG}
python ${OUTPUTDIR}/kalpana.py < input-shp.maxele 2>> ${SYSLOG}
#
# Zip kml, colorbar, and logo into distributable kmz file. 
zip Maximum-Water-Levels.kmz Maximum-Water-Levels.kml Colorbar-water-levels.png logo.png 2>> ${SYSLOG}
#
# Zip GIS files together for distribution.
zip -r Maximum-Water-Levels-gis.zip water-level 2>> ${SYSLOG}






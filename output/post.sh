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
convert()
{
ADVISDIR=$1
ENTRACK=$2
# convert to 1 minute winds in CDT
awk 'NF==3 {$2=$2*1.136; $3=$3*1.136; print $0} NF==2 { $1=$1-(3600.0*5.0); print $0 } NF!=3 && NF!=2 { print $0 }' $ADVISDIR/$(ENTRACK[$si])/fort.74 > 1min_CDT_fort.74 
}
# create the directories for the results on the hosts where they will
# be posted 
init_pubsites()
{
 HOSTNAME=$1
 STORM=$2
 YEAR=$3
 #
 # 
 let m=${#WEBUSER[*]}
 let i=0
 while [ $i -lt $m ]; do
     # create directory for the output
     ssh ${WEBHOST[$i]} -l ${WEBUSER[$i]} -i $SSHKEY "mkdir -p ${WEBPATH[$i]}/$HOSTNAME/$STORM$YEAR" >> ${SYSLOG}
     # upload placeholder page for the storm
     scp -i $SSHKEY $SCRIPTDIR/placeholder.html ${WEBUSER[$i]}@${WEBHOST[$i]}:${WEBPATH[$i]}/$HOSTNAME/$STORM$YEAR/index.html >> ${SYSLOG}
     ssh ${WEBHOST[$i]} -l ${WEBUSER[$i]} -i $SSHKEY "chmod -R 755 ${WEBPATH[$i]}/$HOSTNAME/$STORM$YEAR/index.html" >> ${SYSLOG}
     i=$(( $i + 1 ));
 done
}

#
# create directory for the results from a specific advisory and copy 
# the results for that advisory
init_pub_dirs()
{ HOSTNAME=$1
  STORM=$2
  YEAR=$3
  ADVISORY=$4
  let m=${#WEBUSER[*]}
  let i=0
  while [ $i -lt $m ]; do
      ssh ${WEBHOST[$i]} -l ${WEBUSER[$i]} -i $SSHKEY "mkdir -p ${WEBPATH[$i]}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}" >> ${SYSLOG}
      scp -i $SSHKEY $SCRIPTDIR/index.html ${WEBUSER[$i]}@${WEBHOST[$i]}:${WEBPATH[$i]}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY} >> ${SYSLOG}
      ssh ${WEBHOST[$i]} -l ${WEBUSER[$i]} -i $SSHKEY "chmod -R 755 ${WEBPATH[$i]}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}" >> ${SYSLOG}
      i=$(( $i + 1 ));
  done
}

# copy graph to website(s)
pub_result()
{ HOSTNAME=$1
  STORM=$2
  YEAR=$3
  ADVISORY=$4
  FILENAME=$5
  let m=${#WEBUSER[*]}
  let i=0
  while [ $i -lt $m ]; do
      scp -i $SSHKEY $FILENAME ${WEBUSER[$i]}@${WEBHOST[$i]}:${WEBPATH[$i]}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY} >> ${SYSLOG}
      ssh ${WEBHOST[$i]} -l ${WEBUSER[$i]} -i $SSHKEY "chmod 755 ${WEBPATH[$i]}/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}/$FILENAME" >> ${SYSLOG}
      i=$(( $i + 1 ));
  done
}

# email notification of new results to results recipients and attach
# graphs for middle canal to email 
#     $SCRIPTDIR/scp_files.exp $SCRIPTDIR $ADVISDIR $RESULTSHOST $RESULTSPATH $RESULTSPROMPT $RESULTSUSERNAME $RESULTSPASSWORD $HOSTNAME $STORM $YEAR 2>> ${SYSLOG}


email_results()
{ HOSTNAME=$1
  STORM=$2
  YEAR=$3
  ADVISORY=$4
  PLOT1=$5
  PLOT2=$6

  cat <<END > $ADVISDIR/email.txt 2>> ${SYSLOG}
This is an automated message from the Lake Pontchartrain Forecasting System
running on ${HOSTNAME}.

The results of advisory $ADVISORY for storm $STORM are complete and
are available at the following website(s):

http://www.unc.edu/ims/ccats/LPFS/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}
http://www.seahorsecoastal.com/LPFS/$HOSTNAME/$STORM$YEAR/advisory_${ADVISORY}

The wind speed and storm surge predictions for the London Avenue canal
gate have also been attached to this email.
END
#     cat ${SYSLOG} >> $ADVISDIR/email.txt
#     cat $ADVISDIR/email.txt | asgs-sendmail --subject "storm surge advisory $ADVISORY for storm $STORM" --to "$MAILINGLIST" 2>> ${SYSLOG}
     # send email using do loop ... can't just use the variable itself since
     # the addresses must be separated by commas

#     let pm=${#PLOTS[*]}
#     let pi=0
#     while [ $pi -lt $pm ]; do
#        PLOTLIST=$PLOTLIST" "$(PLOTS[$pi])
#     done  
     for address in $MAILINGLIST; do
          python $SCRIPTDIR/emailattach.py ${address} "surge advisory $ADVISORY for storm $STORM from $HOSTNAME" $ADVISDIR/email.txt "jgflemin@email.unc.edu" localhost $PLOT1 $PLOT2
     done
}







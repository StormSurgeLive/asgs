#!/bin/bash
#
# set variables to be used in json template
# 1st test : parallel (padcirc) job
SCRIPTDIR=~/Campaigns/Development/asgs
ADCIRCDIR=/work/operator/adcirc/work
ADVISDIR=.
JOBTYPE=padcirc
qScriptRequestTemplate=$SCRIPTDIR/qscript_request_template.json
qScriptRequest=qscript_request_$JOBTYPE.json
qScriptResponse=qscript_response_$JOBTYPE.json
QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
parallelism=parallel
NCPU=19
forncpu=19
NUMWRITERS=1
joblauncher=srun
WALLTIME=07:00:00
WALLTIMEFORMAT=minutes
PPN=20
QUEUENAME=workq
SERQUEUE=single
ACCOUNT=ADCIRC-ALLOCATION_001
ASGSADMIN=operator@email.net
SCENARIO=nowcast
RESERVATION=null
CONSTRAINT=null
QOS=highQuality
SYSLOG=syslog.log
SCENARIOLOG=./scenario.log
HOTSTARTCOMP=fulldomain
#QUEUESYS=SLURM
QUEUESYS=mpiexec
HPCENVSHORT=penguin
THIS="asgs_main.sh"
DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
# keep sed from getting confused by escaping slashes
escSCRIPTDIR=${SCRIPTDIR////'\/'}
escADCIRCDIR=${ADCIRCDIR////'\/'}
escQSCRIPTTEMPLATE=${QSCRIPTTEMPLATE////'\/'}
escADVISDIR=${ADVISDIR////'\/'}
escSYSLOG=${SYSLOG////'\/'}
escSCENARIOLOG=${SCENARIOLOG////'\/'}
#
# create queue script request by filling in template
# with data needed to create queue script
#
sed \
    -e "s/%jobtype%/$JOBTYPE/" \
    -e "s/%qscripttemplate%/$escQSCRIPTTEMPLATE/" \
    -e "s/%parallelism%/$parallelism/" \
    -e "s/%ncpu%/$NCPU/" \
    -e "s/%forncpu%/$NCPU/" \
    -e "s/%numwriters%/$NUMWRITERS/" \
    -e "s/%joblauncher%/$joblauncher/" \
    -e "s/%walltime%/$WALLTIME/" \
    -e "s/%walltimeformat%/$WALLTIMEFORMAT/" \
    -e "s/%ppn%/$PPN/" \
    -e "s/%queuename%/$QUEUENAME/" \
    -e "s/%serqueue%/$SERQUEUE/" \
    -e "s/%account%/$ACCOUNT/" \
    -e "s/%advisdir%/$escADVISDIR/" \
    -e "s/%scriptdir%/$escSCRIPTDIR/" \
    -e "s/%adcircdir%/$escADCIRCDIR/" \
    -e "s/%scenario%/$SCENARIO/" \
    -e "s/%reservation%/$RESERVATION/" \
    -e "s/%constraint%/$CONSTRAINT/" \
    -e "s/%qos%/$QOS/" \
    -e "s/%syslog%/$escSYSLOG/" \
    -e "s/%scenariolog%/$escSCENARIOLOG/" \
    -e "s/%hotstartcomp%/$HOTSTARTCOMP/" \
    -e "s/%queuesys%/$QUEUESYS/" \
    -e "s/%hpcenvshort%/$HPCENVSHORT/" \
    -e "s/%asgsadmin%/$ASGSADMIN/" \
    -e "s/%NULLLASTUPDATER%/$THIS/" \
    -e "s/%NULLLASTUPDATETIME%/$DATETIME/" \
     < $qScriptRequestTemplate \
     > $qScriptRequest
#
# request queue script
$SCRIPTDIR/qscript.pl < $qScriptRequest   \
                      > $qScriptResponse 2>> $SYSLOG
# extract queue script name from response
qscript=$(bashJSON.pl --key "qScriptFileName"        \
                      < $qScriptResponse 2>> $SYSLOG)
# extract queue script from response
bashJSON.pl --key "script" < $qScriptResponse 2>> $SYSLOG \
                           | base64 -d                    \
                           > $qscript 2>> $SYSLOG
# update the run.properties file
echo "hpc.job.$JOBTYPE.file.qscript : $qscript" >> run.properties

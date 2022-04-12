#!/bin/bash
#
# set variables to be used in json template
# 1st test : parallel (padcirc) job
SCRIPTDIR=~/Campaigns/Development/asgs
PLATFORMYAML=$SCRIPTDIR/config/platforms.yaml
OPENDAPYAML=$SCRIPTDIR/config/opendap_servers.yaml
ADVISDIR=/work/user/asgs22222/15
SCENARIODIR=/work/user/asgs22222/15/nowcast
STATUSDIR=/work/user/asgs22222/status
CUSTOMDIR=/mypath
SYSLOG=/work/user/syslog.log
CYCLELOG=/work/user/asgs22222/15/cycle.log
SCENARIOLOG=/work/user/asgs22222/15/nhcConsensus/scenario.log
RUNPROPERTIES=/work/user/asgs22222/15/nhcConsensus/run.properties
COLDSTARTDATE=2005072500
GRIDNAME=HSOFS
HPCENV=supermic.hpc.lsu.edu
HPCENVSHORT=supermic
ADVISORY=15
SCENARIO=nowcast
SCENARIONUMBER=-1
TROPICALCYCLONE=on
BACKGROUNDMET=off
YEAR=2005               # YEAR is only a config param when TROPICALCYCLONE=on
STORMNUMBER=12          # STORMNUMBER is only a config param when TROPICALCYCLONE=on
BASIN=al                # HARDCODED
INSTANCENAME=HSOFS_al122005_ope
DEFAULTDESTS=( lsu_tds tacc_tds2 )
ARCHIVEDESTS=( backblaze )
STATUSDESTS=( lsu_tds )
CUSTOMDESTS=( myserver )
enableOpendapNotify=no
OPENDAPEMAILS=( person@recipient.net person2@other.net )
THIS="asgs_main.sh"
DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
#
# parameters populated by control_file_gen.pl
WINDMODEL=vortex-nws320 # forcing.nwpModel
STORMNAME=PSEUDOKAT     # forcing.stormName
RUNSTARTTIME=2005082900 # control.runStartTime
#
# keep sed from getting confused by escaping slashes
escSCRIPTDIR=${SCRIPTDIR////'\/'}
escCUSTOMDIR=${CUSTOMDIR////'\/'}
escPLATFORMYAML=${PLATFORMYAML////'\/'}
escOPENDAPYAML=${OPENDAPYAML////'\/'}
escCYCLEDIR=${ADVISDIR////'\/'}
escSCENARIODIR=${SCENARIODIR////'\/'}
escSTATUSDIR=${STATUSDIR////'\/'}
escSYSLOG=${SYSLOG////'\/'}
escCYCLELOG=${CYCLELOG////'\/'}
escSCENARIOLOG=${SCENARIOLOG////'\/'}
escRUNPROPERTIES=${RUNPROPERTIES////'\/'}
escOPENDAPSCRIPTTEMPLATE=${OPENDAPSCRIPTTEMPLATE////'\/'}
#
opendapEmailList=$(printf ",\"%s\"" "${OPENDAPEMAILS[@]}")
defaultDestList=$(printf ",\"%s\"" "${DEFAULTDESTS[@]}")
archiveDestList=$(printf ",\"%s\"" "${ARCHIVEDESTS[@]}")
statusDestList=$(printf ",\"%s\"" "${STATUSDESTS[@]}")
customDestList=$(printf ",\"%s\"" "${CUSTOMDESTS[@]}")
#
# create queue script request by filling in template
# with data needed to create queue script
#
opendapPostTemplate=$SCRIPTDIR/output/opendap_post_gen.tt2
escOPENDAPPOSTTEMPLATE=${opendapPostTemplate////'\/'}
opendapPostTemplateJSON=$SCRIPTDIR/output/opendap_post_gen_template.json
escOPENDAPPOSTTEMPLATEJSON=${opendapPostTemplateJSON////'\/'}
opendapPostJSON=opendap_post_gen.json
opendapPostFiles=post.opendap.files.json
sed \
    -e "s/%RUNPROPERTIES%/$escRUNPROPERTIES/" \
    -e "s/%PLATFORMYAML%/$escPLATFORMYAML/" \
    -e "s/%OPENDAPYAML%/$escOPENDAPYAML/" \
    -e "s/%SCRIPTTEMPLATE%/$escOPENDAPPOSTTEMPLATE/" \
    -e "s/%SCRIPTTEMPLATEJSON%/$escOPENDAPPOSTTEMPLATEJSON/" \
    -e "s/%OPENDAPPOSTJSON%/$escOPENDAPPOSTJSON/" \
    -e "s/%SCRIPTDIR%/$escSCRIPTDIR/" \
    -e "s/%CYCLEDIR%/$escCYCLEDIR/" \
    -e "s/%SCENARIODIR%/$escSCENARIODIR/" \
    -e "s/%STATUSDIR%/$escSTATUSDIR/" \
    -e "s/%COLDSTARTDATE%/$COLDSTARTDATE/" \
    -e "s/%YEAR%/$YEAR/" \
    -e "s/%STORMNUMBER%/$STORMNUMBER/" \
    -e "s/%GRIDNAME%/$GRIDNAME/" \
    -e "s/%HPCENV%/$HPCENV/" \
    -e "s/%HPCENVSHORT%/$HPCENVSHORT/" \
    -e "s/%CYCLE%/$ADVISORY/" \
    -e "s/%SCENARIO%/$SCENARIO/" \
    -e "s/%SCENARIONUMBER%/$SCENARIONUMBER/" \
    -e "s/%TROPICALCYCLONE%/$TROPICALCYCLONE/" \
    -e "s/%BACKGROUNDMET%/$BACKGROUNDMET/" \
    -e "s/%SYSLOG%/$escSYSLOG/" \
    -e "s/%CYCLELOG%/$escCYCLELOG/" \
    -e "s/%SCENARIOLOG%/$escSCENARIOLOG/" \
    -e "s/%INSTANCENAME%/$INSTANCENAME/" \
    -e "s/\"%DEFAULTDESTS%\"/${defaultDestList:1}/" \
    -e "s/\"%ARCHIVEDESTS%\"/${archiveDestList:1}/" \
    -e "s/\"%STATUSDESTS%\"/${statusDestList:1}/" \
    -e "s/\"%CUSTOMDESTS%\"/${customDestList:1}/" \
    -e "s/\"%OPENDAPEMAILS%\"/${opendapEmailList:1}/" \
    -e "s/%ENABLEOPENDAPNOTIFY%/$enableOpendapNotify/" \
    -e "s/%NULLLASTUPDATER%/$THIS/" \
    -e "s/%NULLLASTUPDATETIME%/$DATETIME/" \
    -e "s/%RUNSTARTTIME%/$RUNSTARTTIME/" \
    -e "s/%WINDMODEL%/$WINDMODEL/" \
    -e "s/%STORMNAME%/$STORMNAME/" \
    -e "s/\"%.*%\"/null/" \
     < $opendapPostTemplateJSON \
     > opendap_post_gen.json
#
# request opendap post script
$SCRIPTDIR/opendap_post_gen.pl --postType default --filesJSON $opendapPostFiles  \
   < $opendapPostJSON         \
   > opendap_post_default.sh

# extract queue script name from response
#qscript=$(bashJSON.pl --key "qScriptFileName"        \
#                      < $qScriptResponse 2>> $SYSLOG)
# extract queue script from response
#bashJSON.pl --key "script" < $qScriptResponse 2>> $SYSLOG \
#                           | base64 -d                    \
#                           > $qscript 2>> $SYSLOG
# update the run.properties file
#echo "hpc.job.$JOBTYPE.file.qscript : $qscript" >> run.properties

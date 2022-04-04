#!/bin/bash
#
# set variables to be used in json template
# 1st test : parallel (padcirc) job
SCRIPTDIR=~/Campaigns/Development/asgs
PLATFORMYAML=$SCRIPTDIR/config/platforms.yaml
OPENDAPYAML=$SCRIPTDIR/config/opendap_servers.yaml
ADVISDIR=/work/user/asgs22222/15
ADVISDIR=/work/user/asgs22222/15/nowcast
STATUSDIR=/work/user/asgs22222/status
CUSTOMDIR=/mypath
SYSLOG=/work/user/syslog.log
CYCLELOG=/work/user/asgs22222/15/cycle.log
SCENARIOLOG=/work/user/asgs22222/15/nhcConsensus/scenario.log
RUNPROPERTIES=/work/user/asgs22222/15/nhcConsensus/run.properties
COLDSTARTDATE=2005072500
RUNSTARTTIME=2005082900
GRIDNAME=HSOFS
HPCENV=supermic.hpc.lsu.edu
HPCENVSHORT=supermic
ADVISORY=15
SCENARIO=nowcast
SCENARIONUMBER=-1
TROPICALCYCLONE=on
BACKGROUNDMET=off
YEAR=2005
NWPMODEL=GAHM
STORMNAME=PSEUDOKAT
STORMNUMBER=12
BASIN=al
INSTANCENAME=HSOFS_al122005_ope
DEFAULTDESTS=( lsu_tds tacc_tds )
ARCHIVEDESTS=( backblaze )
STATUSDESTS=( lsu_tds )
CUSTOMDESTS=( myserver )
DEFAULTFILES=( inputfile1 metadatafile outputfile1 outputfile2 )
ARCHIVEFILES=( inputfile1 inputfile2 metadatafile1 metadatafile2 )
STATUSFILES=( status1.json status2.json )
CUSTOMFILES=( myfile )
enableOpendapNotify=no
DEFAULTEMAILS=( person@recipient.net person2@other.net )
ARCHIVEMAILS=( datamanager@data.net )
STATUSEMAILS=( )
CUSTOMEMAILS=( student@u.edu )
THIS="asgs_main.sh"
DATETIME=$(date +'%Y-%h-%d-T%H:%M:%S%z')
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
defaultDestList=$(printf ",\"%s\"" "${DEFAULTDESTS[@]}")
archiveDestList=$(printf ",\"%s\"" "${ARCHIVEDESTS[@]}")
statusDestList=$(printf ",\"%s\"" "${STATUSDESTS[@]}")
customDestList=$(printf ",\"%s\"" "${CUSTOMDESTS[@]}")
defaultFileList=$(printf ",\"%s\"" "${DEFAULTFILES[@]}")
archiveFileList=$(printf ",\"%s\"" "${ARCHIVEFILES[@]}")
statusFileList=$(printf ",\"%s\"" "${STATUSFILES[@]}")
customFileList=$(printf ",\"%s\"" "${CUSTOMFILES[@]}")
defaultEmailList=$(printf ",\"%s\"" "${DEFAULTEMAILS[@]}")
archiveEmailList=$(printf ",\"%s\"" "${ARCHIVEEMAILS[@]}")
statusEmailList=$(printf ",\"%s\"" "${STATUSEMAILS[@]}")
customEmailList=$(printf ",\"%s\"" "${CUSTOMEMAILS[@]}")
#
# create queue script request by filling in template
# with data needed to create queue script
#
opendapPostTemplate=$SCRIPTDIR/output/opendap_post_gen.tt2
escOPENDAPPOSTTEMPLATE=${opendapPostTemplate////'\/'}
opendapPostTemplateJSON=$SCRIPTDIR/output/opendap_post_gen_template.json
escOPENDAPPOSTTEMPLATEJSON=${opendapPostTemplateJSON////'\/'}
opendapPostJSON=$SCRIPTDIR/opendap_post_gen.json
escOPENDAPPOSTJSON=${opendapPostJSON////'\/'}
qScriptRequest=qscript_request_$JOBTYPE.json
qScriptResponse=qscript_response_$JOBTYPE.json
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
    -e "s/\"%DEFAULTEMAILS%\"/${defaultEmailList:1}/" \
    -e "s/\"%ARCHIVEEMAILS%\"/${archiveEmailList:1}/" \
    -e "s/\"%STATUSEMAILS%\"/${statusEmailList:1}/" \
    -e "s/\"%CUSTOMEMAILS%\"/${customEmailList:1}/" \
    -e "s/%ENABLEOPENDAPNOTIFY%/$enableOpendapNotify/" \
    -e "s/\"%OPENDAPEMAILLIST%\"/${opendapNotifyEmailAdressList:1}/" \
    -e "s/%NULLLASTUPDATER%/$THIS/" \
    -e "s/%NULLLASTUPDATETIME%/$DATETIME/" \
    -e "s/%RUNSTARTTIME%/$RUNSTARTTIME/" \
    -e "s/\"%DEFAULTFILES%\"/${defaultFileList:1}/" \
    -e "s/\"%ARCHIVEFILES%\"/${archiveFileList:1}/" \
    -e "s/\"%STATUSFILES%\"/${statusFileList:1}/" \
    -e "s/\"%CUSTOMFILES%\"/${customFileList:1}/" \
     < $opendapPostTemplateJSON \
     > opendap_post_gen.json
#
# request opendap post script
#$SCRIPTDIR/opendap_post_gen.pl < $opendapPostTemplate $qScriptRequest   \
#                      > $qScriptResponse 2>> $SYSLOG
# extract queue script name from response
#qscript=$(bashJSON.pl --key "qScriptFileName"        \
#                      < $qScriptResponse 2>> $SYSLOG)
# extract queue script from response
#bashJSON.pl --key "script" < $qScriptResponse 2>> $SYSLOG \
#                           | base64 -d                    \
#                           > $qscript 2>> $SYSLOG
# update the run.properties file
#echo "hpc.job.$JOBTYPE.file.qscript : $qscript" >> run.properties

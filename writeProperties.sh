#!/bin/bash
#----------------------------------------------------------------
# writeProperties.sh: Writes configuration and specification
# info to run.properties metadata file.
#----------------------------------------------------------------
# Copyright(C) 2021 Jason Fleming
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
#----------------------------------------------------------------
#
# Write general properties to the run.properties file that are associated with
# the ASGS configuration as well as real time properties specific to this
# scenario.
writeProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeProperties()"
   local STORMDIR_RUN_PROPERTIES="$STORMDIR/run.properties"
   logMessage "$THIS: Writing properties associated with ASGS configuration to $1/run.properties."
   # this is the first set of properties that will be written; if there is
   # a stray file from a previous (interrupted) asgs execution, this function
   # should overwrite whatever may have been there
   #
   # basic asgs configuration
   echo "config.file : $CONFIG" > $STORMDIR_RUN_PROPERTIES  # <--<< OVERWRITE
   echo "instancename : $INSTANCENAME" >> $STORMDIR_RUN_PROPERTIES
   echo "operator : $operator" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.time.coldstartdate : $CSDATE" >> $STORMDIR_RUN_PROPERTIES
   echo "path.adcircdir : $ADCIRCDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "path.scriptdir : $SCRIPTDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "path.inputdir : $INPUTDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "path.outputdir : $OUTPUTDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "path.scratchdir : $SCRATCHDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.backgroundmet : $BACKGROUNDMET" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tidefac : $TIDEFAC" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone : $TROPICALCYCLONE" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.varflux : $VARFLUX" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.staticoffset : $STATICOFFSET" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.schedule.cycletimelimit : $CYCLETIMELIMIT" >> $STORMDIR_RUN_PROPERTIES
   echo "coupling.waves : $WAVES" >> $STORMDIR_RUN_PROPERTIES
   # static hpc environment properties
   echo "hpc.hpcenv : $HPCENV" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.hpcenvshort : $HPCENVSHORT" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.queuesys : $QUEUESYS" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.joblauncher : $JOBLAUNCHER" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.submitstring : $SUBMITSTRING" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.executable.qscriptgen : $QSCRIPTGEN" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.jobs.ncpucapacity : $NCPUCAPACITY" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.walltimeformat : $WALLTIMEFORMAT" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.default.account : $ACCOUNT" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.default.queuename : $QUEUENAME" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.default.serqueue : $SERQUEUE" >> $STORMDIR_RUN_PROPERTIES
   # static input files, templates, and property files
   echo "adcirc.file.input.gridfile : $GRIDFILE" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.file.input.unitoffsetfile : $UNITOFFSETFILE" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.gridname : $GRIDNAME" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.file.properties.meshproperties : $MESHPROPERTIES" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.file.input.nafile : $NAFILE" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.file.properties.naproperties : $NAPROPERTIES" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.file.template.controltemplate : $CONTROLTEMPLATE" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.file.properties.controlproperties : $CONTROLPROPERTIES" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.file.elevstations : $ELEVSTATIONS" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.file.velstations : $VELSTATIONS" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.file.metstations : $METSTATIONS" >> $STORMDIR_RUN_PROPERTIES
   # other adcirc specific
   echo "adcirc.hotstartformat : $HOTSTARTFORMAT" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.timestepsize : $TIMESTEPSIZE" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.hotstartcomp : $HOTSTARTCOMP" >> $STORMDIR_RUN_PROPERTIES
   echo "file.preppedarchive : $PREPPEDARCHIVE" >> $STORMDIR_RUN_PROPERTIES
   echo "file.hindcastarchive : $HINDCASTARCHIVE" >> $STORMDIR_RUN_PROPERTIES
   echo "adcirc.minmax : $MINMAX" >> $STORMDIR_RUN_PROPERTIES
   # notification
   echo "notification.emailnotify : $EMAILNOTIFY" >> $STORMDIR_RUN_PROPERTIES
   echo "notification.executable.notify_script : $NOTIFY_SCRIPT" >> $STORMDIR_RUN_PROPERTIES
   echo "notification.email.activate_list : \"$ACTIVATE_LIST\"" >> $STORMDIR_RUN_PROPERTIES
   echo "notification.email.new_advisory_list : \"$NEW_ADVISORY_LIST\"" >> $STORMDIR_RUN_PROPERTIES
   echo "notification.email.post_init_list : \"$POST_INIT_LIST\"" >> $STORMDIR_RUN_PROPERTIES
   echo "notification.email.job_failed_list : \"$JOB_FAILED_LIST\"" >> $STORMDIR_RUN_PROPERTIES
   echo "notification.hpc.email.notifyuser : deprecated - do not use - will be removed in the future" >> $STORMDIR_RUN_PROPERTIES
   echo "notification.opendap.email.opendapnotify : $OPENDAPNOTIFY" >> $STORMDIR_RUN_PROPERTIES
   echo "notification.email.asgsadmin : $ASGSADMIN" >> $STORMDIR_RUN_PROPERTIES
   # monitoring (includes logging)
   echo "monitoring.rmqmessaging.enable : $RMQMessaging_Enable " >> $STORMDIR_RUN_PROPERTIES
   # only record other RMQ parameters if it's actually "on"
   if [ "$RMQMessaging_Enable" == "on" ]
   then
     echo "monitoring.rmqmessaging.transmit : $RMQMessaging_Transmit" >> $STORMDIR_RUN_PROPERTIES
     echo "monitoring.rmqmessaging.script : $RMQMessaging_Script" >> $STORMDIR_RUN_PROPERTIES
     echo "monitoring.rmqmessaging.scriptrp : $RMQMessaging_Script_RP" >> $STORMDIR_RUN_PROPERTIES
     echo "monitoring.rmqmessaging.ncohome : $RMQMessaging_NcoHome" >> $STORMDIR_RUN_PROPERTIES
     echo "monitoring.rmqmessaging.locationname : $RMQMessaging_LocationName" >> $STORMDIR_RUN_PROPERTIES
     echo "monitoring.rmqmessaging.clustername : $RMQMessaging_ClusterName" >> $STORMDIR_RUN_PROPERTIES
   fi
   echo "monitoring.logging.file.syslog : $SYSLOG" >> $STORMDIR_RUN_PROPERTIES
   # post processing
   echo "post.intendedaudience : $INTENDEDAUDIENCE" >> $STORMDIR_RUN_PROPERTIES
   echo "post.executable.initpost : $INITPOST" >> $STORMDIR_RUN_PROPERTIES
   echo "post.executable.postprocess : ( ${POSTPROCESS[@]} )" >> $STORMDIR_RUN_PROPERTIES
   echo "post.opendap.additionalFiles : ( ${postAdditionalFiles[@]} )" >> $STORMDIR_RUN_PROPERTIES
   echo "post.opendap.tds : ( ${TDS[@]} )" >> $STORMDIR_RUN_PROPERTIES
   echo "post.opendap.target : $TARGET" >> $STORMDIR_RUN_PROPERTIES
   echo "post.file.sshkey : $SSHKEY" >> $STORMDIR_RUN_PROPERTIES
   # archiving
   echo "archive.executable.archive : $ARCHIVE" >> $STORMDIR_RUN_PROPERTIES
   echo "archive.path.archivebase : $ARCHIVEBASE" >> $STORMDIR_RUN_PROPERTIES
   echo "archive.path.archivedir : $ARCHIVEDIR" >> $STORMDIR_RUN_PROPERTIES
   # forecast scenario package size
   echo "forecast.scenariopackagesize : $SCENARIOPACKAGESIZE" >> $STORMDIR_RUN_PROPERTIES
   # runtime
   echo "path.rundir : $RUNDIR" >> $STORMDIR_RUN_PROPERTIES
   # each scenario
   echo "path.fromdir : $FROMDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "path.lastsubdir : $LASTSUBDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "scenario : $ENSTORM" >> $STORMDIR_RUN_PROPERTIES
   # FIXME: the following are legacy properties from 2014stable
   # and should not be carried forward
   echo "forecast.ensemblesize : $SCENARIOPACKAGESIZE" >> $STORMDIR_RUN_PROPERTIES
   echo "asgs.path.fromdir : $FROMDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "asgs.path.lastsubdir : $LASTSUBDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "asgs.enstorm : $ENSTORM" >> $STORMDIR_RUN_PROPERTIES
   echo "enstorm : $ENSTORM" >> $STORMDIR_RUN_PROPERTIES
   #
   ADCIRCVERSION=`${ADCIRCDIR}/adcirc -v`
   echo "adcirc.version : $ADCIRCVERSION" >> $STORMDIR_RUN_PROPERTIES
   #
   # properties for backward compatibility
   echo "hostname : $HPCENV" >> $STORMDIR_RUN_PROPERTIES
   echo "instance : $INSTANCENAME" >> $STORMDIR_RUN_PROPERTIES
   echo "pseudostorm : $PSEUDOSTORM" >> $STORMDIR_RUN_PROPERTIES
   echo "intendedAudience : $INTENDEDAUDIENCE" >> $STORMDIR_RUN_PROPERTIES
   if [[ $NWS -eq 0 ]]; then
      echo "WindModel : none" >> $STORMDIR_RUN_PROPERTIES
   fi
}
#
# write properties that depend on the scenario but are not known
# at the start of setup for the scenario
writeScenarioProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeScenarioProperties()"
   local STORMDIR_RUN_PROPERTIES="$STORMDIR/run.properties"
   logMessage "$THIS: Writing properties associated with this scenario to $1/run.properties."
   echo "path.cycledir : $ADVISDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "path.scenariodir : $STORMDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "scenario.number : $si" >> $STORMDIR_RUN_PROPERTIES
   echo "monitoring.logging.file.cyclelog : $CYCLELOG" >> $STORMDIR_RUN_PROPERTIES
   echo "monitoring.logging.file.scenariolog : $SCENARIOLOG" >> $STORMDIR_RUN_PROPERTIES
   # this is used in forming the path where the results will be
   # posted to a remote server
   if [[ $BACKGROUNDMET != "off" ]]; then
      echo "forcing.nwp.year : ${ADVISORY:0:4}" >> $STORMDIR_RUN_PROPERTIES
   fi
   # FIXME: the following are legacy properties from 2014stable
   # and should not be carried forward
   echo "asgs.path.advisdir : $ADVISDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "asgs.path.stormdir : $STORMDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "path.advisdir : $ADVISDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "path.stormdir : $STORMDIR" >> $STORMDIR_RUN_PROPERTIES
}
#
# write properties to the run.properties file that are associated with
# NAM forcing.
writeNAMProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeNAMProperties()"
   local STORMDIR_RUN_PROPERTIES="$STORMDIR/run.properties"
   logMessage "$THIS: Writing properties associated with meterorological forcing with the NAM model to $1/run.properties."
   echo "forcing.metclass : synoptic" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.stormname : NA" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nwp.model : nam" >> $STORMDIR_RUN_PROPERTIES

   echo "forcing.nam.schedule.forecast.forecastcycle : \"${FORECASTCYCLE}\"" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nwp.schedule.forecast.forecastselection : $forecastSelection" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nam.forecast.download : $forecastDownload" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nam.backsite : $BACKSITE" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nam.backdir : $BACKDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nam.forecastlength : $FORECASTLENGTH" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nam.reprojection.ptfile : $PTFILE" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nam.local.altnamdir : $ALTNAMDIR" >> $STORMDIR_RUN_PROPERTIES
   # legacy from 2014stable, depcrecated
   echo "config.forcing.nam.schedule.forecast.forecastcycle : \"${FORECASTCYCLE}\"" >> $STORMDIR_RUN_PROPERTIES
   echo "config.forcing.nam.backsite : $BACKSITE" >> $STORMDIR_RUN_PROPERTIES
   echo "config.forcing.nam.backdir : $BACKDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "config.forcing.nam.forecastlength : $FORECASTLENGTH" >> $STORMDIR_RUN_PROPERTIES
   echo "config.forcing.nam.reprojection.ptfile : $PTFILE" >> $STORMDIR_RUN_PROPERTIES
   echo "config.forcing.nam.local.altnamdir : $ALTNAMDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "WindModel : WNAMAW12-NCP" >> $STORMDIR_RUN_PROPERTIES
}
#
# write properties to the run.properties file for GFS
writeGFSProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeGFSProperties()"
   local STORMDIR_RUN_PROPERTIES="$STORMDIR/run.properties"
   logMessage "$THIS: Writing properties for meterorological forcing with the GFS model to $1/run.properties."
   echo "forcing.metclass : synoptic" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.stormname : NA" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nwp.model : GFS" >> $STORMDIR_RUN_PROPERTIES

   echo "forcing.gfs.schedule.forecast.forecastcycle : \"${FORECASTCYCLE}\"" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.nwp.schedule.forecast.forecastselection : $forecastSelection" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.gfs.forecast.download : $forecastDownload" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.gfs.backsite : $GFSBACKSITE" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.gfs.backdir : $GFSBACKDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.gfs.forecastlength : $GFSFORECASTLENGTH" >> $STORMDIR_RUN_PROPERTIES
   # legacy from 2014stable, depcrecated
   echo "config.forcing.gfs.schedule.forecast.forecastcycle : \"${FORECASTCYCLE}\"" >> $STORMDIR_RUN_PROPERTIES
   echo "WindModel : GFS" >> $STORMDIR_RUN_PROPERTIES
}

#
# write properties to the run.properties file that are associated with
# tropical cyclone forcing configuration.
writeTropicalCycloneProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeTropicalCycloneProperties()"
   local STORMDIR_RUN_PROPERTIES="$STORMDIR/run.properties"
   logMessage "$THIS: Writing properties associated with meterorological forcing configuration with a parametric vortex model to $1/run.properties."
   echo "forcing.metclass : tropical" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.stormname : $STORM" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone.vortexmodel : $VORTEXMODEL" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone.stormnumber : $STORM" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone.year : $YEAR" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone.pseudostorm : $PSEUDOSTORM" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone.forecast.trigger : $TRIGGER" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone.forecast.rsssite : $RSSSITE" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone.forecast.path.fdir : $FDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone.best.ftpsite : $FTPSITE" >> $STORMDIR_RUN_PROPERTIES
   echo "forcing.tropicalcyclone.best.path.hdir : $HDIR" >> $STORMDIR_RUN_PROPERTIES
   # each scenario
   if [[ $RMAX != default ]]; then
      echo "forcing.tropicalcyclone.enstorm.variation.rmax : $RMAX" >> $STORMDIR_RUN_PROPERTIES
   fi
   if [[ $PERCENT != default ]]; then
      echo "forcing.tropicalcyclone.enstorm.variation.percent : $PERCENT" >> $STORMDIR_RUN_PROPERTIES
   fi
   # legacy properties
   echo "storm : $STORM" >> $STORMDIR_RUN_PROPERTIES
   echo "stormnumber : $STORM" >> $STORMDIR_RUN_PROPERTIES
   echo "WindModel : vortex-nws$NWS" >> $STORMDIR_RUN_PROPERTIES
}
#
# write properties to the run.properties file that are associated with
# tropical cyclone forcing configuration.
writeTropicalCycloneForecastProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeTropicalCycloneForecastProperties()"
   local STORMDIR_RUN_PROPERTIES="$STORMDIR/run.properties"
   logMessage "$THIS: Writing properties associated with a particular forecast using a parametric vortex model to $1/run.properties."
    # write the start and end dates of the forecast to the run.properties file
    if [[ -e $RUNDIR/forecast.properties ]]; then
      cat $RUNDIR/forecast.properties >> $STORMDIR_RUN_PROPERTIES
    fi
}
#
# write properties to the run.properties file that are associated with
# swan coupling.
writeWaveCouplingProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeWaveCouplingProperties()"
   local STORMDIR_RUN_PROPERTIES="$STORMDIR/run.properties"
   logMessage "$THIS: Writing properties associated with wave coupling to $1/run.properties."
   echo "path.swandir : $SWANDIR" >> $STORMDIR_RUN_PROPERTIES
   echo "coupling.waves.swan.reinitializeswan : $REINITIALIZESWAN" >> $STORMDIR_RUN_PROPERTIES
   echo "coupling.waves.swan.swanhscompression : $SWANHSCOMPRESSION" >> $STORMDIR_RUN_PROPERTIES
   echo "swan.swandt : $SWANDT" >> $STORMDIR_RUN_PROPERTIES
   echo "swan.input.file.swantemplate : $SWANTEMPLATE" >> $STORMDIR_RUN_PROPERTIES
   echo "swan.input.file.swaninit : swaninit.template" >> $STORMDIR_RUN_PROPERTIES
}
#
# write properties to the run.properties file that are associated with
# the cpu request for a particular job submitted to an hpc queue
writeJobResourceRequestProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeJobResourceRequestProperties()"
   local STORMDIR_RUN_PROPERTIES="$STORMDIR/run.properties"
   logMessage "$THIS: Writing properties associated with compute job to $1/run.properties."

   # adjusts $QUEUENAME, if criteria is met; otherwise returns current value as the default;
   CPUREQUEST=$(($NCPU + $NUMWRITERS))
   QUEUENAME=$(HPC_Queue_Hint "$QUEUENAME" "$HPCENV" "$QOS" "$CPUREQUEST")

   echo "hpc.job.${JOBTYPE}.queuename : $QUEUENAME" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.${JOBTYPE}.serqueue : $SERQUEUE" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.${JOBTYPE}.file.qscripttemplate : $QSCRIPTTEMPLATE" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.${JOBTYPE}.account : $ACCOUNT" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.${JOBTYPE}.ncpu : $NCPU" >> $STORMDIR_RUN_PROPERTIES
   if [[ $NCPU -gt 1 ]]; then
      echo "hpc.job.${JOBTYPE}.parallelism : parallel" >> $STORMDIR_RUN_PROPERTIES
      echo "hpc.job.${JOBTYPE}.numwriters : $NUMWRITERS" >> $STORMDIR_RUN_PROPERTIES
   fi
   echo "hpc.job.limit.hindcastwalltime : $HINDCASTWALLTIME" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.limit.nowcastwalltime : $NOWCASTWALLTIME" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.limit.forecastwalltime : $FORECASTWALLTIME" >> $STORMDIR_RUN_PROPERTIES
   echo "hpc.job.limit.adcprepwalltime : $ADCPREPWALLTIME" >> $STORMDIR_RUN_PROPERTIES

   # adjusts $_PPN, if criteria is met; othewise returns current value as the defaults;
   # $PPN is not adjusted so the original value is preserved; yet the "corrected" value
   # is written to $STORMDIR_RUN_PROPERTIES, which is where ./qscript.pl gets the value
   # for "$ppn"
   _PPN=$(HPC_PPN_Hint "parallel" "$SERQUEUE" "$HPCENV" "$QOS" "$PPN")
   echo "hpc.job.${JOBTYPE}.ppn : ${_PPN}" >> $STORMDIR_RUN_PROPERTIES
   unset _PPN

   if [[ $QUEUESYS = SLURM ]]; then
      # adjusts $RESERVATION, if criteria is met; othewise returns current value as the defaults;
      _RESERVATION=$(HPC_Reservation_Hint "$RESERVATION" "$HPCENV" "$QOS" "$CPUREQUEST")
      echo "hpc.slurm.job.${JOBTYPE}.reservation : ${_RESERVATION}" >> $STORMDIR_RUN_PROPERTIES
      unset _RESERVATION
      echo "hpc.slurm.job.${JOBTYPE}.constraint : $CONSTRAINT" >> $STORMDIR_RUN_PROPERTIES
      echo "hpc.slurm.job.${JOBTYPE}.qos : $QOS" >> $STORMDIR_RUN_PROPERTIES
   fi

   # legacy properties
   echo "cpurequest : $CPUREQUEST" >> ${STORMDIR}/run.properties
   echo "ncpu : $NCPU" >> ${STORMDIR}/run.properties  # number of compute CPUs
   echo "numwriters : $NUMWRITERS" >> ${STORMDIR}/run.properties  # number of dedicated writer CPUs
}

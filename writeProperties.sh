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
   logMessage "$THIS: Writing properties associated with ASGS configuration to $1/run.properties."
   # this is the first set of properties that will be written; if there is
   # a stray file from a previous (interrupted) asgs execution, this function
   # should overwrite whatever may have been there
   #
   # basic asgs configuration
   echo "config.file : $CONFIG" > $STORMDIR/run.properties  # <--<< OVERWRITE
   echo "instancename : $INSTANCENAME" >> $STORMDIR/run.properties
   echo "operator : $operator" >> $STORMDIR/run.properties
   echo "adcirc.time.coldstartdate : $CSDATE" >> $STORMDIR/run.properties
   echo "path.adcircdir : $ADCIRCDIR" >> $STORMDIR/run.properties
   echo "path.scriptdir : $SCRIPTDIR" >> $STORMDIR/run.properties
   echo "path.inputdir : $INPUTDIR" >> $STORMDIR/run.properties
   echo "path.outputdir : $OUTPUTDIR" >> $STORMDIR/run.properties
   echo "path.scratchdir : $SCRATCHDIR" >> $STORMDIR/run.properties
   echo "forcing.backgroundmet : $BACKGROUNDMET" >> $STORMDIR/run.properties
   echo "forcing.tidefac : $TIDEFAC" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone : $TROPICALCYCLONE" >> $STORMDIR/run.properties
   echo "forcing.varflux : $VARFLUX" >> $STORMDIR/run.properties
   echo "forcing.staticoffset : $STATICOFFSET" >> $STORMDIR/run.properties
   echo "forcing.schedule.cycletimelimit : $CYCLETIMELIMIT" >> $STORMDIR/run.properties
   echo "coupling.waves : $WAVES" >> $STORMDIR/run.properties
   # static hpc environment properties
   echo "hpc.hpcenv : $HPCENV" >> $STORMDIR/run.properties
   echo "hpc.hpcenvshort : $HPCENVSHORT" >> $STORMDIR/run.properties
   echo "hpc.queuesys : $QUEUESYS" >> $STORMDIR/run.properties
   echo "hpc.joblauncher : $JOBLAUNCHER" >> $STORMDIR/run.properties
   echo "hpc.submitstring : $SUBMITSTRING" >> $STORMDIR/run.properties
   echo "hpc.executable.qscriptgen : $QSCRIPTGEN" >> $STORMDIR/run.properties
   echo "hpc.jobs.ncpucapacity : $NCPUCAPACITY" >> $STORMDIR/run.properties
   echo "hpc.walltimeformat : $WALLTIMEFORMAT" >> $STORMDIR/run.properties
   echo "hpc.job.default.account : $ACCOUNT" >> $STORMDIR/run.properties
   echo "hpc.job.default.queuename : $QUEUENAME" >> $STORMDIR/run.properties
   echo "hpc.job.default.serqueue : $SERQUEUE" >> $STORMDIR/run.properties
   # static input files, templates, and property files
   echo "adcirc.file.input.gridfile : $GRIDFILE" >> $STORMDIR/run.properties
   echo "adcirc.file.input.unitoffsetfile : $UNITOFFSETFILE" >> $STORMDIR/run.properties
   echo "adcirc.gridname : $GRIDNAME" >> $STORMDIR/run.properties
   echo "adcirc.file.properties.meshproperties : $MESHPROPERTIES" >> $STORMDIR/run.properties
   echo "adcirc.file.input.nafile : $NAFILE" >> $STORMDIR/run.properties
   echo "adcirc.file.properties.naproperties : $NAPROPERTIES" >> $STORMDIR/run.properties
   echo "adcirc.file.template.controltemplate : $CONTROLTEMPLATE" >> $STORMDIR/run.properties
   echo "adcirc.file.properties.controlproperties : $CONTROLPROPERTIES" >> $STORMDIR/run.properties
   echo "adcirc.file.elevstations : $ELEVSTATIONS" >> $STORMDIR/run.properties
   echo "adcirc.file.velstations : $VELSTATIONS" >> $STORMDIR/run.properties
   echo "adcirc.file.metstations : $METSTATIONS" >> $STORMDIR/run.properties
   # other adcirc specific
   echo "adcirc.hotstartformat : $HOTSTARTFORMAT" >> $STORMDIR/run.properties
   echo "adcirc.timestepsize : $TIMESTEPSIZE" >> $STORMDIR/run.properties
   echo "adcirc.hotstartcomp : $HOTSTARTCOMP" >> $STORMDIR/run.properties
   echo "file.preppedarchive : $PREPPEDARCHIVE" >> $STORMDIR/run.properties
   echo "file.hindcastarchive : $HINDCASTARCHIVE" >> $STORMDIR/run.properties
   echo "adcirc.minmax : $MINMAX" >> $STORMDIR/run.properties
   # notification
   echo "notification.emailnotify : $EMAILNOTIFY" >> $STORMDIR/run.properties
   echo "notification.executable.notify_script : $NOTIFY_SCRIPT" >> $STORMDIR/run.properties
   echo "notification.email.activate_list : \"$ACTIVATE_LIST\"" >> $STORMDIR/run.properties
   echo "notification.email.new_advisory_list : \"$NEW_ADVISORY_LIST\"" >> $STORMDIR/run.properties
   echo "notification.email.post_init_list : \"$POST_INIT_LIST\"" >> $STORMDIR/run.properties
   echo "notification.email.job_failed_list : \"$JOB_FAILED_LIST\"" >> $STORMDIR/run.properties
   echo "notification.hpc.email.notifyuser : deprecated - do not use - will be removed in the future" >> $STORMDIR/run.properties
   echo "notification.opendap.email.opendapnotify : $OPENDAPNOTIFY" >> $STORMDIR/run.properties
   echo "notification.email.asgsadmin : $ASGSADMIN" >> $STORMDIR/run.properties
   # monitoring (includes logging)
   echo "monitoring.rmqmessaging.enable : $RMQMessaging_Enable " >> $STORMDIR/run.properties
   # only record other RMQ parameters if it's actually "on"
   if [ "$RMQMessaging_Enable" == "on" ]
   then
     echo "monitoring.rmqmessaging.transmit : $RMQMessaging_Transmit" >> $STORMDIR/run.properties
     echo "monitoring.rmqmessaging.script : $RMQMessaging_Script" >> $STORMDIR/run.properties
     echo "monitoring.rmqmessaging.scriptrp : $RMQMessaging_Script_RP" >> $STORMDIR/run.properties
     echo "monitoring.rmqmessaging.ncohome : $RMQMessaging_NcoHome" >> $STORMDIR/run.properties
     echo "monitoring.rmqmessaging.locationname : $RMQMessaging_LocationName" >> $STORMDIR/run.properties
     echo "monitoring.rmqmessaging.clustername : $RMQMessaging_ClusterName" >> $STORMDIR/run.properties
   fi
   echo "monitoring.logging.file.syslog : $SYSLOG" >> $STORMDIR/run.properties
   # post processing
   echo "post.intendedaudience : $INTENDEDAUDIENCE" >> $STORMDIR/run.properties
   echo "post.executable.initpost : $INITPOST" >> $STORMDIR/run.properties
   POSTPROCESSSTRING="( "
   for script in ${POSTPROCESS[*]}; do
      POSTPROCESSSTRING="$POSTPROCESSSTRING $script"
   done
   POSTPROCESSSTRING="$POSTPROCESSSTRING )"
   echo "post.executable.postprocess : $POSTPROCESSSTRING" >> $STORMDIR/run.properties
   echo "post.opendap.target : $TARGET" >> $STORMDIR/run.properties
   THREDDS="( "
   for thredds_data_server in ${TDS[*]}; do
      THREDDS="$THREDDS $thredds_data_server"
   done
   THREDDS="$THREDDS )"
   echo "post.opendap.tds : $THREDDS" >> $STORMDIR/run.properties
   echo "post.file.sshkey : $SSHKEY" >> $STORMDIR/run.properties
   # archiving
   echo "archive.executable.archive : $ARCHIVE" >> $STORMDIR/run.properties
   echo "archive.path.archivebase : $ARCHIVEBASE" >> $STORMDIR/run.properties
   echo "archive.path.archivedir : $ARCHIVEDIR" >> $STORMDIR/run.properties
   # forecast scenario package size
   echo "forecast.scenariopackagesize : $SCENARIOPACKAGESIZE" >> $STORMDIR/run.properties
   # runtime
   echo "path.rundir : $RUNDIR" >> $STORMDIR/run.properties
   # each scenario
   echo "path.fromdir : $FROMDIR" >> $STORMDIR/run.properties
   echo "path.lastsubdir : $LASTSUBDIR" >> $STORMDIR/run.properties
   echo "scenario : $ENSTORM" >> $STORMDIR/run.properties
   # FIXME: the following are legacy properties from 2014stable
   # and should not be carried forward
   echo "forecast.ensemblesize : $SCENARIOPACKAGESIZE" >> $STORMDIR/run.properties
   echo "asgs.path.fromdir : $FROMDIR" >> $STORMDIR/run.properties
   echo "asgs.path.lastsubdir : $LASTSUBDIR" >> $STORMDIR/run.properties
   echo "asgs.enstorm : $ENSTORM" >> $STORMDIR/run.properties
   echo "enstorm : $ENSTORM" >> $STORMDIR/run.properties
   #
   ADCIRCVERSION=`${ADCIRCDIR}/adcirc -v`
   echo "adcirc.version : $ADCIRCVERSION" >> $STORMDIR/run.properties
   #
   # properties for backward compatibility
   echo "hostname : $HPCENV" >> $STORMDIR/run.properties
   echo "instance : $INSTANCENAME" >> $STORMDIR/run.properties
   echo "pseudostorm : $PSEUDOSTORM" >> $STORMDIR/run.properties
   echo "intendedAudience : $INTENDEDAUDIENCE" >> $STORMDIR/run.properties
   if [[ $NWS -eq 0 ]]; then
      echo "WindModel : none" >> $STORMDIR/run.properties
   fi
}
#
# write properties that depend on the scenario but are not known
# at the start of setup for the scenario
writeScenarioProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeScenarioProperties()"
   logMessage "$THIS: Writing properties associated with this scenario to $1/run.properties."
   echo "path.cycledir : $ADVISDIR" >> $STORMDIR/run.properties
   echo "path.scenariodir : $STORMDIR" >> $STORMDIR/run.properties
   echo "monitoring.logging.file.cyclelog : $CYCLELOG" >> $STORMDIR/run.properties
   echo "monitoring.logging.file.scenariolog : $SCENARIOLOG" >> $STORMDIR/run.properties
   # FIXME: the following are legacy properties from 2014stable
   # and should not be carried forward
   echo "asgs.path.advisdir : $ADVISDIR" >> $STORMDIR/run.properties
   echo "asgs.path.stormdir : $STORMDIR" >> $STORMDIR/run.properties
   echo "path.advisdir : $ADVISDIR" >> $STORMDIR/run.properties
   echo "path.stormdir : $STORMDIR" >> $STORMDIR/run.properties
}
#
# write properties to the run.properties file that are associated with
# NAM forcing.
writeNAMProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeNAMProperties()"
   logMessage "$THIS: Writing properties associated with meterorological forcing with the NAM model to $1/run.properties."
   echo "forcing.metclass : synoptic" >> $STORMDIR/run.properties
   echo "forcing.stormname : NA" >> $STORMDIR/run.properties
   echo "forcing.nwp.model : nam" >> $STORMDIR/run.properties

   echo "forcing.nam.schedule.forecast.forecastcycle : \"${FORECASTCYCLE}\"" >> $STORMDIR/run.properties
   echo "forcing.nwp.schedule.forecast.forecastselection : $forecastSelection" >> $STORMDIR/run.properties
   echo "forcing.nam.forecast.download : $forecastDownload" >> $STORMDIR/run.properties
   echo "forcing.nam.backsite : $BACKSITE" >> $STORMDIR/run.properties
   echo "forcing.nam.backdir : $BACKDIR" >> $STORMDIR/run.properties
   echo "forcing.nam.forecastlength : $FORECASTLENGTH" >> $STORMDIR/run.properties
   echo "forcing.nam.reprojection.ptfile : $PTFILE" >> $STORMDIR/run.properties
   echo "forcing.nam.local.altnamdir : $ALTNAMDIR" >> $STORMDIR/run.properties
   # legacy from 2014stable, depcrecated
   echo "config.forcing.nam.schedule.forecast.forecastcycle : \"${FORECASTCYCLE}\"" >> $STORMDIR/run.properties
   echo "config.forcing.nam.backsite : $BACKSITE" >> $STORMDIR/run.properties
   echo "config.forcing.nam.backdir : $BACKDIR" >> $STORMDIR/run.properties
   echo "config.forcing.nam.forecastlength : $FORECASTLENGTH" >> $STORMDIR/run.properties
   echo "config.forcing.nam.reprojection.ptfile : $PTFILE" >> $STORMDIR/run.properties
   echo "config.forcing.nam.local.altnamdir : $ALTNAMDIR" >> $STORMDIR/run.properties
   echo "WindModel : WNAMAW12-NCP" >> $STORMDIR/run.properties
}
#
# write properties to the run.properties file for GFS
writeGFSProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeGFSProperties()"
   logMessage "$THIS: Writing properties for meterorological forcing with the GFS model to $1/run.properties."
   echo "forcing.metclass : synoptic" >> $STORMDIR/run.properties
   echo "forcing.stormname : NA" >> $STORMDIR/run.properties
   echo "forcing.nwp.model : GFS" >> $STORMDIR/run.properties

   echo "forcing.gfs.schedule.forecast.forecastcycle : \"${FORECASTCYCLE}\"" >> $STORMDIR/run.properties
   echo "forcing.nwp.schedule.forecast.forecastselection : $forecastSelection" >> $STORMDIR/run.properties
   echo "forcing.gfs.forecast.download : $forecastDownload" >> $STORMDIR/run.properties
   echo "forcing.gfs.backsite : $GFSBACKSITE" >> $STORMDIR/run.properties
   echo "forcing.gfs.backdir : $GFSBACKDIR" >> $STORMDIR/run.properties
   echo "forcing.gfs.forecastlength : $FORECASTLENGTH" >> $STORMDIR/run.properties
   # legacy from 2014stable, depcrecated
   echo "config.forcing.gfs.schedule.forecast.forecastcycle : \"${FORECASTCYCLE}\"" >> $STORMDIR/run.properties
   echo "WindModel : GFS" >> $STORMDIR/run.properties
}

#
# write properties to the run.properties file that are associated with
# tropical cyclone forcing configuration.
writeTropicalCycloneProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeTropicalCycloneProperties()"
   logMessage "$THIS: Writing properties associated with meterorological forcing configuration with a parametric vortex model to $1/run.properties."
   echo "forcing.metclass : tropical" >> $STORMDIR/run.properties
   echo "forcing.stormname : $STORM" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone.vortexmodel : $VORTEXMODEL" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone.stormnumber : $STORM" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone.year : $YEAR" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone.pseudostorm : $PSEUDOSTORM" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone.forecast.trigger : $TRIGGER" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone.forecast.rsssite : $RSSSITE" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone.forecast.path.fdir : $FDIR" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone.best.ftpsite : $FTPSITE" >> $STORMDIR/run.properties
   echo "forcing.tropicalcyclone.best.path.hdir : $HDIR" >> $STORMDIR/run.properties
   # each scenario
   if [[ $RMAX != default ]]; then
      echo "forcing.tropicalcyclone.enstorm.variation.rmax : $RMAX" >> $STORMDIR/run.properties
   fi
   if [[ $PERCENT != default ]]; then
      echo "forcing.tropicalcyclone.enstorm.variation.percent : $PERCENT" >> $STORMDIR/run.properties
   fi
   # legacy properties
   echo "storm : $STORM" >> $STORMDIR/run.properties
   echo "stormnumber : $STORM" >> $STORMDIR/run.properties
   echo "WindModel : vortex-nws$NWS" >> $STORMDIR/run.properties
}
#
# write properties to the run.properties file that are associated with
# tropical cyclone forcing configuration.
writeTropicalCycloneForecastProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeTropicalCycloneForecastProperties()"
   logMessage "$THIS: Writing properties associated with a particular forecast using a parametric vortex model to $1/run.properties."
    # write the start and end dates of the forecast to the run.properties file
    if [[ -e $RUNDIR/forecast.properties ]]; then
      cat $RUNDIR/forecast.properties >> ${STORMDIR}/run.properties
    fi
}
#
# write properties to the run.properties file that are associated with
# swan coupling.
writeWaveCouplingProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeWaveCouplingProperties()"
   logMessage "$THIS: Writing properties associated with wave coupling to $1/run.properties."
   echo "path.swandir : $SWANDIR" >> $STORMDIR/run.properties
   echo "coupling.waves.swan.reinitializeswan : $REINITIALIZESWAN" >> $STORMDIR/run.properties
   echo "coupling.waves.swan.swanhscompression : $SWANHSCOMPRESSION" >> $STORMDIR/run.properties
   echo "swan.swandt : $SWANDT" >> $STORMDIR/run.properties
   echo "swan.input.file.swantemplate : $SWANTEMPLATE" >> $STORMDIR/run.properties
   echo "swan.input.file.swaninit : swaninit.template" >> $STORMDIR/run.properties
}
#
# write properties to the run.properties file that are associated with
# the cpu request for a particular job submitted to an hpc queue
writeJobResourceRequestProperties()
{
   STORMDIR=$1
   local THIS="asgs_main->writeJobResourceRequestProperties()"
   logMessage "$THIS: Writing properties associated with compute job to $1/run.properties."

   # adjusts $QUEUENAME, if criteria is met; otherwise returns current value as the default;
   CPUREQUEST=$(($NCPU + $NUMWRITERS))
   QUEUENAME=$(HPC_Queue_Hint "$QUEUENAME" "$HPCENV" "$QOS" "$CPUREQUEST")

   echo "hpc.job.${JOBTYPE}.queuename : $QUEUENAME" >> $STORMDIR/run.properties
   echo "hpc.job.${JOBTYPE}.serqueue : $SERQUEUE" >> $STORMDIR/run.properties
   echo "hpc.job.${JOBTYPE}.file.qscripttemplate : $QSCRIPTTEMPLATE" >> $STORMDIR/run.properties
   echo "hpc.job.${JOBTYPE}.account : $ACCOUNT" >> $STORMDIR/run.properties
   echo "hpc.job.${JOBTYPE}.ncpu : $NCPU" >> $STORMDIR/run.properties
   if [[ $NCPU -gt 1 ]]; then
      echo "hpc.job.${JOBTYPE}.parallelism : parallel" >> $STORMDIR/run.properties
      echo "hpc.job.${JOBTYPE}.numwriters : $NUMWRITERS" >> $STORMDIR/run.properties
   fi
   echo "hpc.job.limit.hindcastwalltime : $HINDCASTWALLTIME" >> $STORMDIR/run.properties
   echo "hpc.job.limit.nowcastwalltime : $NOWCASTWALLTIME" >> $STORMDIR/run.properties
   echo "hpc.job.limit.forecastwalltime : $FORECASTWALLTIME" >> $STORMDIR/run.properties
   echo "hpc.job.limit.adcprepwalltime : $ADCPREPWALLTIME" >> $STORMDIR/run.properties

   # adjusts $_PPN, if criteria is met; othewise returns current value as the defaults;
   # $PPN is not adjusted so the original value is preserved; yet the "corrected" value
   # is written to $STORMDIR/run.properties, which is where ./qscript.pl gets the value
   # for "$ppn"
   _PPN=$(HPC_PPN_Hint "parallel" "$SERQUEUE" "$HPCENV" "$QOS" "$PPN")
   echo "hpc.job.${JOBTYPE}.ppn : ${_PPN}" >> $STORMDIR/run.properties
   unset _PPN

   if [[ $QUEUESYS = SLURM ]]; then
      # adjusts $RESERVATION, if criteria is met; othewise returns current value as the defaults;
      _RESERVATION=$(HPC_Reservation_Hint "$RESERVATION" "$HPCENV" "$QOS" "$CPUREQUEST")
      echo "hpc.slurm.job.${JOBTYPE}.reservation : ${_RESERVATION}" >> $STORMDIR/run.properties
      unset _RESERVATION
      echo "hpc.slurm.job.${JOBTYPE}.constraint : $CONSTRAINT" >> $STORMDIR/run.properties
      echo "hpc.slurm.job.${JOBTYPE}.qos : $QOS" >> $STORMDIR/run.properties
   fi

   # legacy properties
   echo "cpurequest : $CPUREQUEST" >> ${STORMDIR}/run.properties
   echo "ncpu : $NCPU" >> ${STORMDIR}/run.properties  # number of compute CPUs
   echo "numwriters : $NUMWRITERS" >> ${STORMDIR}/run.properties  # number of dedicated writer CPUs
}

#!/bin/bash
#----------------------------------------------------------------
# operator_defaults.sh : Functions required for initializing
# parameters that are Operator dependent.  
#----------------------------------------------------------------
# Copyright(C) 2019 Jason Fleming
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
THIS=config/operator_defaults.sh
operator=$USER
allMessage "$THIS: Setting default values for the Operator '${operator}'."
#
# set values based on Operator ... not basing this on $USER b/c on some platforms (e.g., hatteras)
# user name is ambiguous
case $operator in
   "ncfs-dev")
      op="bob"
      # RMQ
      RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
      RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
      NOTIFY_SCRIPT=ncfs_nam_notify.sh
      ems="bblanton@renci.org"
      EMAILNOTIFY=yes         # no | yes to have host HPC platform email notifications
      ACTIVATE_LIST="$ems"
      NEW_ADVISORY_LIST="$ems"
      POST_INIT_LIST=null
      POST_LIST=null
      # opendap email notification
      OPENDAPNOTIFY="bblanton@renci.org"
      #OPENDAPNOTIFY="asgs.cera.lsu@gmail.com jason.g.fleming@gmail.com" 
      # the following are related to job failure
      JOB_FAILED_LIST="bblanton@renci.org"
      NOTIFYUSER="bblanton@renci.org"
      ASGSADMIN="bblanton@renci.org"
      ;;
   "jgflemin"|"ncfs"|"jason")
      op="jgf"                      # initials as nickname for appending to asgs instance names
      # RMQ
      RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
      RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
      # email notifications
      NOTIFY_SCRIPT=corps_cyclone_notify.sh
      EMAILNOTIFY=yes         # no | yes to have host HPC platform email notifications
      ACTIVATE_LIST=null
      NEW_ADVISORY_LIST=null
      POST_INIT_LIST=null
      POST_LIST=null
      # opendap email notification
      OPENDAPNOTIFY="jason.g.fleming@gmail.com" # space delimited if a list
      #OPENDAPNOTIFY="asgs.cera.lsu@gmail.com jason.g.fleming@gmail.com" 
      # the following are related to job failure
      JOB_FAILED_LIST="jason.fleming@seahorsecoastal.com"
      NOTIFYUSER=jason.fleming@seahorsecoastal.com
      ASGSADMIN=jason.fleming@seahorsecoastal.com
      ;;
   "alireza"|"akheirkhahan"|"akheir")
      op="al"                      # initials as nickname for appending to asgs instance names
      # RMQ
      RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
      RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
      # email notifications
      NOTIFY_SCRIPT=corps_nam_notify.sh
      EMAILNOTIFY=yes         # no | yes to have host HPC platform email notifications
      ACTIVATE_LIST=null
      NEW_ADVISORY_LIST=null
      POST_INIT_LIST=null
      POST_LIST=null
      # opendap email notification
      OPENDAPNOTIFY="kheirkhahan@gmail.com asgs.cera.lsu@gmail.com" # space delimited if a list
      # the following are related to job failure
      JOB_FAILED_LIST="kheirkhahan@gmail.com"
      NOTIFYUSER=kheirkhahan@gmail.com
      ASGSADMIN=kheirkhahan@gmail.com
      #
      # public keys:
      #
      ;;
   "mbilskie")
       op="mvb"
      # RMQ
      RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
      RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
      # email notifications
      NOTIFY_SCRIPT=corps_nam_notify.sh
      ACTIVATE_LIST=null
      NEW_ADVISORY_LIST=null
      POST_INIT_LIST=null
      POST_LIST=null
      # opendap email notification
      OPENDAPNOTIFY="asgs.cera.lsu@gmail.com MBilskie.ASGS@gmail.com" # space delimited if a list
      # the following are related to job failure
      JOB_FAILED_LIST="mbilsk3@lsu.edu"
      NOTIFYUSER=MBilskie.ASGS@gmail.com
      ASGSADMIN=MBilskie.ASGS@gmail.com
      ;;
    "estrabd")
op=bde
ACCOUNT=DesignSafe-CERA
QUEUENAME=skx-dev
SERQUEUE=skx-dev
PARALLELMODULES=
SERIALMODULES=
PPN=48
NCPU=47               # number of compute CPUs for all simulations
NUMWRITERS=1
NCPUCAPACITY=96
EMAILNOTIFY=no         # no | yes to have host HPC platform email notifications
ACTIVATE_LIST=null
NEW_ADVISORY_LIST=null
POST_INIT_LIST=null
POST_LIST=null
JOB_FAILED_LIST=null
NOTIFYUSER=
ASGSADMIN=
;;
   *)
      warn "cycle $CYCLE: $SCENARIO: $THIS: Operator $operator was not recognized."
      ;;
esac

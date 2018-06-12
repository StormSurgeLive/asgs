#!/bin/bash
#----------------------------------------------------------------
#
# platforms.sh: This file contains functions required for initializing
# variables that are architecture (platform) dependent. 
# It is sourced by asgs_main.sh and any other shell script that 
# is platform dependent. 
#
#----------------------------------------------------------------
# Copyright(C) 2012--2015 Jason Fleming
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
# initialization subroutines for the various machines/architectures
#
# Suggested aliases to support the Operator's tasks. Add these
# to .bashrc, .bash_profile or similar
#
# alias lsta='ls -lth *.state | head'
#
init_supermike()
{ #<- can replace the following with a custom script
  HOSTNAME=mike.hpc.lsu.edu
  QUEUESYS=PBS
  QCHECKCMD=qstat
  QUEUENAME=workq
  SERQUEUE=single
  #ACCOUNT=pleaseSetAccountParamToLONIAllocationInASGSConfig
  SUBMITSTRING=qsub
  SCRATCHDIR=/work/$USER
  #SCRATCHDIR=/work/cera
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=supermike.template.pbs
  PREPCONTROLSCRIPT=supermike.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=16
  # alias cdwo='cd /work/jgflemin'
  # alias cdasgs='cd ~/asgs/2014stable'
}
init_queenbee()
{ #<- can replace the following with a custom script
  HOSTNAME=queenbee.loni.org
  QUEUESYS=PBS
  QCHECKCMD=qstat
  QUEUENAME=workq
  SERQUEUE=single
  ACCOUNT=pleaseSetAccountParamToLONIAllocationInASGSConfig
  SUBMITSTRING=qsub
  SCRATCHDIR=/work/$USER
  #SCRATCHDIR=/work/cera
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=queenbee.template.pbs
  PREPCONTROLSCRIPT=queenbee.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=20
  module load intel
  module load netcdf
  module load netcdf_fortran
  module load gcc
  # alias cdwo='cd /work/jgflemin'
  # alias cdasgs='cd ~/asgs/2014stable'
}
init_arete()
{ #<- can replace the following with a custom script
  HOSTNAME=arete.cct.lsu.edu
  QUEUESYS=SLURM
  QCHECKCMD=sacct
  ACCOUNT=null
  SUBMITSTRING=srun
  SCRATCHDIR=/scratch/$USER
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=arete.template.slurm
  PREPCONTROLSCRIPT=arete.adcprep.template.slurm
  QSCRIPTGEN=hatteras.slurm.pl
  PPN=8
}
init_camellia()
{ #<- can replace the following with a custom script
  HOSTNAME=camellia.worldwindsinc.com
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=bpj
  SUBMITSTRING=qsub
  SCRATCHDIR=$HOME/tmp
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=ww.template.pbs
  PREPCONTROLSCRIPT=ww.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=12
}
init_blueridge()
{ #<- can replace the following with a custom script
  HOSTNAME=blueridge.renci.org
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=noaccount
  SUBMITSTRING=submitstring
  SCRATCHDIR=/projects/ncfs/data
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=renci.template.pbs
  PREPCONTROLSCRIPT=renci.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=8
}
init_croatan()
{ #<- can replace the following with a custom script
  HOSTNAME=croatan.renci.org
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=noaccount
  SUBMITSTRING=submitstring
  SCRATCHDIR=/projects/ncfs/data
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=croatan.template.pbs
  PREPCONTROLSCRIPT=croatan.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=16
}
init_hatteras()
{ #<- can replace the following with a custom script
  HOSTNAME=hatteras.renci.org
  QUEUESYS=SLURM
  QCHECKCMD=sacct
  ACCOUNT=bblanton
  SUBMITSTRING=sbatch
  SCRATCHDIR=/scratch/bblanton/data
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=hatteras.template.slurm
  PREPCONTROLSCRIPT=hatteras.adcprep.template.slurm
  RESERVATION=null     # ncfs or null
  PARTITION=batch       # ncfs or batch
  CONSTRAINT=hatteras # ivybridge or sandybridge
  QSCRIPTGEN=hatteras.slurm.pl
  PPN=20
}
init_stampede()
{ #<- can replace the following with a custom script
  HOSTNAME=stampede.tacc.utexas.edu
  QUEUESYS=SLURM
  QCHECKCMD=sacct
  ACCOUNT=PleaseSpecifyACCOUNTInYourAsgsConfigFile
  SUBMITSTRING=sbatch
  SCRATCHDIR=$SCRATCH
  SSHKEY=~/.ssh/id_rsa_stampede
  QSCRIPT=stampede.template.slurm
  PREPCONTROLSCRIPT=stampede.adcprep.template.slurm
  QSCRIPTGEN=hatteras.slurm.pl
  PPN=16
  module load netcdf/4.3.2
  #jgf20150610: Most likely QUEUENAME=normal SERQUEUENAME=serial
}
init_kittyhawk()
{ #<- can replace the following with a custom script
  HOSTNAME=kittyhawk.renci.org
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=noaccount
  SUBMITSTRING=submitstring
  SCRATCHDIR=/work/$USER
  SSHKEY=~/.ssh/id_rsa_kittyhawk
  QSCRIPT=kittyhawk.template.pbs
  PREPCONTROLSCRIPT=kittyhawk.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=4
}
init_sapphire()
{ #<- can replace the following with a custom script
  HOSTNAME=sapphire.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="aprun"
  SCRATCHDIR=/work2/$USER
  SSHKEY=~/.ssh/id_rsa_sapphire
  QSCRIPT=erdc.template.pbs
  PREPCONTROLSCRIPT=erdc.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=erdc.adcprep.hotstart.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  ulimit -s unlimited
  ulimit -v 2097152   # needed for NAMtoOWI.pl to avoid Out of memory error
}

init_jade()
{ #<- can replace the following with a custom script
  HOSTNAME=jade.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="aprun"
# INTERSTRING="qsub -l size=1,walltime=00:10:00 -A $ACCOUNT -q $QUEUENAME -I"
  INTERSTRING=
  SCRATCHDIR=/work/$USER
  SSHKEY=~/.ssh/id_rsa_jade
  QSCRIPT=erdc.template.pbs
  PREPCONTROLSCRIPT=erdc.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=erdc.adcprep.hotstart.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  ulimit -s unlimited
  ulimit -v 2097152   # needed for NAMtoOWI.pl to avoid Out of memory error
}

init_diamond()
{ #<- can replace the following with a custom script
  HOSTNAME=diamond.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="mpiexec_mpt"
  SCRATCHDIR=/work/$USER
  SSHKEY=~/.ssh/id_rsa_diamond
  QSCRIPT=erdc.diamond.template.pbs
  PREPCONTROLSCRIPT=erdc.diamond.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=erdc.diamond.adcprep.hotstart.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  PPN=8
}

init_garnet()
{ #<- can replace the following with a custom script
  HOSTNAME=garnet.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="aprun"
  SCRATCHDIR=$WORKDIR 
  SSHKEY=~/.ssh/id_rsa_garnet
  QSCRIPT=garnet.template.pbs
  PREPCONTROLSCRIPT=garnet.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=garnet.adcprep.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  PPN=32
  IMAGEMAGICKBINPATH=/usr/local/usp/ImageMagick/default/bin 
}
init_spirit()
{ #<- can replace the following with a custom script
  # This requires the user to have a .personal.bashrc file in the $HOME 
  # directory with the following contents:
  # echo "Loading modules in .personal.bashrc ..."
  # module load intel-compilers/12.1.0
  # module load netcdf-fortran/intel/4.4.2
  # module load hdf5/intel/1.8.12
  # module load hdf5-mpi/intel/sgimpt/1.8.12
  # module load mpt/2.12
  # echo "... modules loaded."
  HOSTNAME=spirit.afrl.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=erdcvhsp
  SUBMITSTRING="mpiexec_mpt"
  SCRATCHDIR=$WORKDIR 
  SSHKEY=~/.ssh/id_rsa_spirit
  QSCRIPT=spirit.template.pbs
  PREPCONTROLSCRIPT=spirit.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=spirit.adcprep.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  PPN=16
  IMAGEMAGICKBINPATH=/usr/local/usp/ImageMagick/default/bin 
}
init_topaz()
{ #<- can replace the following with a custom script
  # This requires the Operator to have a ~/.bash_profile file in the $HOME 
  # directory with the following contents:
  echo "Loading modules in .bash_profile ..."
  module unload compiler/intel/16.0.0
  module load compiler/intel/15.0.3
  module load usp-netcdf/intel-15.0.3/4.3.3.1
  module load imagemagick/6.9.2-5
  echo "... modules loaded."
  HOSTNAME=topaz.erdc.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  #ACCOUNT=ERDCV00898N10
  ACCOUNT=ERDCV00898HSP
  SUBMITSTRING="qstat"
  SCRATCHDIR=$WORKDIR 
  SSHKEY=~/.ssh/id_rsa_topaz
  QSCRIPT=topaz.template.pbs
  PREPCONTROLSCRIPT=topaz.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=topaz.adcprep.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  PPN=36
  IMAGEMAGICKBINPATH=/app/unsupported/ImageMagick/6.9.2-5/bin/convert
  # fyi topaz has a 4hr time limit for the background queue
}
init_thunder()
{ #<- can replace the following with a custom script
  # This requires the Operator to have a ~/.personal.bashrc file in the $HOME 
  # directory with the following contents:
  echo "Loading modules in .bash_profile ..."
  module load costinit
  module load git
  module load netcdf-fortran/intel/4.4.2
  echo "... modules loaded."
  HOSTNAME=thunder.afrl.hpc.mil
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=ERDCV00898N10
  #ACCOUNT=ERDCV00898HSP
  SUBMITSTRING="qstat"
  SCRATCHDIR=$WORKDIR 
  SSHKEY=~/.ssh/id_rsa_thunder
  QSCRIPT=thunder.template.pbs
  PREPCONTROLSCRIPT=thunder.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=thunder.adcprep.template.pbs
  QSCRIPTGEN=erdc.pbs.pl
  PPN=36
  IMAGEMAGICKBINPATH=/app/unsupported/ImageMagick/6.9.2-5/bin/convert
}
init_tezpur()
{ #<- can replace the following with a custom script
  HOSTNAME=tezpur.hpc.lsu.edu
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=loni_asgs2009
  SUBMITSTRING="mpirun"
  SCRATCHDIR=/work/cera
  SSHKEY=id_rsa_tezpur
  QSCRIPT=tezpur.template.pbs
  PREPCONTROLSCRIPT=tezpur.adcprep.template.pbs
  PREPHOTSTARTSCRIPT=tezpur.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=4
}
init_mike()
{ #<- can replace the following with a custom script
  HOSTNAME=mike.hpc.lsu.edu
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=pleaseSetAccountParamToHPCAllocationInASGSConfig
  SUBMITSTRING="mpirun"
  SCRATCHDIR=/work/$USER
  SSHKEY=id_rsa_mike
  QSCRIPT=mike.template.pbs
  PREPCONTROLSCRIPT=mike.adcprep.template.pbs
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=16
  soft add +netcdf-4.1.3-Intel-13.0.0 
  #/usr/local/packages/netcdf/4.1.3/Intel-13.0.0
}
init_ranger()
{ #<- can replace the following with a custom script
  HOSTNAME=ranger.tacc.utexas.edu
  QUEUESYS=SGE
  QCHECKCMD=qstat
  NCPUDIVISOR=16
  ACCOUNT=TG-DMS100024
  SUBMITSTRING="ibrun tacc_affinity"
  SCRATCHDIR=$SCRATCH
  SSHKEY=id_rsa_ranger
  QSCRIPT=ranger.template.sge
  QSCRIPTGEN=ranger.sge.pl
  SERQSCRIPT=ranger.template.serial
  SERQSCRIPTGEN=ranger.serial.pl
  UMASK=006
  GROUP="G-81535"
}
init_lonestar()
{ #<- can replace the following with a custom script
  HOSTNAME=lonestar.tacc.utexas.edu
  QUEUESYS=SLURM
  QUEUENAME=normal
  SERQUEUE=normal
  QCHECKCMD=squeue
  PPN=24
  ACCOUNT=ADCIRC
  SUBMITSTRING="ibrun"
  SCRATCHDIR=$SCRATCH
  SSHKEY=id_rsa_lonestar
  QSCRIPT=lonestar.template.slurm
  QSCRIPTGEN=hatteras.slurm.pl
  PREPCONTROLSCRIPT=lonestar.template.serial.slurm
  SERQSCRIPTGEN=hatteras.slurm.pl
  UMASK=006
  GROUP="G-803086"
  module load netcdf/4.3.3.1 
}
init_desktop()
{
  HOSTNAME=jason-desktop
  QUEUESYS=mpiexec
  QCHECKCMD="ps -aux | grep mpiexec "
  SUBMITSTRING="mpiexec -n"
  SCRATCHDIR=/srv/asgs
  SSHKEY=id_rsa_jason-desktop
  ADCOPTIONS='compiler=gfortran MACHINENAME=jason-desktop'
  SWANMACROSINC=macros.inc.gfortran
}
init_Poseidon()
{
  HOSTNAME=poseidon.vsnet.gmu.edu
  QUEUESYS=mpiexec
  QCHECKCMD="ps -aux | grep mpiexec "
  SUBMITSTRING="mpiexec -n"
  SCRATCHDIR=/home/fhrl/Documents/asgs_processing
  SSHKEY=id_rsa_jason-desktop
  ADCOPTIONS='compiler=gfortran MACHINENAME=jason-desktop'
  SWANMACROSINC=macros.inc.gfortran
}

init_topsail()
{ #<- can replace the following with a custom script
  HOSTNAME=topsail.unc.edu
  QUEUESYS=LSF
  INTERSTRING="bsub -q int -Ip"
  SCRATCHDIR=/ifs1/scr/$USER
  SSHKEY=id_rsa_topsail
}
# THREDDS Data Server (TDS, i.e., OPeNDAP server) at RENCI
init_renci_tds()
{
   OPENDAPHOST=ht4.renci.org
   DOWNLOADPREFIX="http://tds.renci.org:8080/thredds/fileServer"
   CATALOGPREFIX="http://tds.renci.org:8080/thredds/catalog"
   OPENDAPBASEDIR=/projects/ees/DataLayers/asgs/
   SSHPORT=22
   LINKABLEHOSTS=(hatteras hatteras.renci.org) # list of hosts where we can just create symbolic links for thredds service, rather than having to scp the files to an external machine
   COPYABLEHOSTS=(null) # list of hosts where we can copy for thredds service, rather than having to scp the files to an external machine
}
# THREDDS Data Server (TDS, i.e., OPeNDAP server) at LSU
init_lsu_tds()
{
   OPENDAPHOST=fortytwo.cct.lsu.edu
   DOWNLOADPREFIX="http://${OPENDAPHOST}:8080/thredds/fileServer"
   CATALOGPREFIX="http://${OPENDAPHOST}:8080/thredds/catalog"
   OPENDAPBASEDIR=/scratch/opendap
   SSHPORT=2525
   LINKABLEHOSTS=(null) # list of hosts where we can just create symbolic links
   COPYABLEHOSTS=(null) # list of hosts where we can copy for thredds service, rather than having to scp the files to an external machine
}
# THREDDS Data Server (TDS, i.e., OPeNDAP server) at Texas
# Advanced Computing Center (TACC)
init_tacc_tds()
{
   OPENDAPHOST=adcircvis.tacc.utexas.edu
   DOWNLOADPREFIX="http://${OPENDAPHOST}:8080/thredds/fileServer/asgs"
   CATALOGPREFIX="http://${OPENDAPHOST}:8080/thredds/catalog/asgs"
   OPENDAPBASEDIR=/corral-tacc/utexas/hurricane/ASGS/2017
   SSHPORT=null
   LINKABLEHOSTS=(null) # list of hosts where we can just create symbolic links for thredds service, rather than having to scp the files to an external machine
   COPYABLEHOSTS=(lonestar lonestar.tacc.utexas.edu) # list of hosts where we can copy for thredds service, rather than having to scp the files to an external machine
}
init_test()
{ #<- can replace the following with a custom script
  QUEUESYS=Test
  NCPU=-1
}
# used to dispatch environmentally sensitive actions
# such as queue interactions
env_dispatch(){
 case $1 in
  "camellia") consoleMessage "platforms.sh: Camellia(WorldWinds) configuration found."
          init_camellia
          ;;
  "lsu_tds") consoleMessage "platforms.sh: LSU THREDDS Data Server configuration found."
          init_lsu_tds
          ;;
  "renci_tds") consoleMessage "platforms.sh: RENCI THREDDS Data Server configuration found."
          init_renci_tds
          ;;
  "tacc_tds") consoleMessage "platforms.sh: TACC THREDDS Data Server configuration found."
          init_tacc_tds
          ;;
  "kittyhawk") consoleMessage "platforms.sh: Kittyhawk (RENCI) configuration found."
          init_kittyhawk
          ;;
  "blueridge") consoleMessage "platforms.sh: Blueridge (RENCI) configuration found."
          init_blueridge
          ;;
  "croatan") consoleMessage "platforms.sh: Croatan (RENCI) configuration found."
          init_croatan
          ;;
  "hatteras") consoleMessage "platforms.sh: Hatteras (RENCI) configuration found."
          init_hatteras
          RMQMessage "INFO" "$CURRENT_EVENT" "platforms.sh"  "RUNN" "Hatteras (RENCI) configuration found."  0
          ;;
  "hatteras14") consoleMessage "platforms.sh: Hatteras (RENCI) configuration found."
          init_hatteras14
          ;;
  "sapphire") consoleMessage "platforms.sh: Sapphire (ERDC) configuration found."
          init_sapphire
          ;;
  "jade") consoleMessage "platforms.sh: Jade (ERDC) configuration found."
          init_jade
          ;;
  "diamond") consoleMessage "platforms.sh: Diamond (ERDC) configuration found."
          init_diamond
          ;;
  "garnet") consoleMessage "platforms.sh: Garnet (ERDC) configuration found."
          init_garnet
          ;;
  "spirit") consoleMessage "platforms.sh: Spirit (AFRL) configuration found."
          init_spirit
          ;;
  "topaz") consoleMessage "platforms.sh: Topaz (ERDC) configuration found."
          init_topaz
          ;;
  "thunder") consoleMessage "platforms.sh: Thunder (AFRL) configuration found."
          init_thunder
          ;;
  "supermike") consoleMessage "platforms.sh: Supermike (LSU) configuration found."
          init_supermike
          ;;
  "queenbee") consoleMessage "platforms.sh: Queenbee (LONI) configuration found."
          init_queenbee
          ;;
  "tezpur") consoleMessage "platforms.sh: Tezpur (LSU) configuration found."
          init_tezpur
          ;;
  "mike") consoleMessage "platforms.sh: SuperMike-II (LSU) configuration found."
          init_mike
          ;;
  "topsail") consoleMessage "platforms.sh: Topsail (UNC) configuration found."
          init_topsail
          ;;
  "ranger") consoleMessage "platforms.sh: Ranger (TACC) configuration found."
          init_ranger
          ;;
  "lonestar") consoleMessage "platforms.sh: Lonestar (TACC) configuration found."
          init_lonestar
          ;;
  "stampede") consoleMessage "platforms.sh: Stampede (TACC) configuration found."
          init_stampede
          ;;
  "arete") consoleMessage "platforms.sh: Arete (CCT) configuration found."
          init_arete
          ;;
  "desktop") consoleMessage "platforms.sh: desktop configuration found."
          init_desktop
           ;;
  "poseidon") consoleMessage "platforms.sh: desktop configuration found."
          init_Poseidon
           ;;
  "test") consoleMessage "platforms.sh: test environment (default) configuration found."
          init_test
          ;;
  *) fatal "platforms.sh: '$1' is not a supported environment; currently supported options: kittyhawk, blueridge, sapphire, jade, diamond, ranger, lonestar, stampede, supermike, queenbee, topsail, desktop, arete, spirit, topaz, thunder, lsu_tds, renci_tds, tacc_tds"
     ;;
  esac
}

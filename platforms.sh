#!/bin/bash
#----------------------------------------------------------------
#
# $THIS: This file contains functions required for initializing
# variables that are architecture (platform) dependent. 
# It is sourced by asgs_main.sh and any other shell script that 
# is platform dependent. 
#
#----------------------------------------------------------------
# Copyright(C) 2012--2019 Jason Fleming
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
  THIS="platforms.sh>env_dispatch()>init_supermike()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=mike.hpc.lsu.edu
  QUEUESYS=PBS
  QCHECKCMD=qstat
  QUEUENAME=workq
  SERQUEUE=single
  ACCOUNT=null
  SUBMITSTRING=qsub
  JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
  SCRATCHDIR=/work/$USER
  #SCRATCHDIR=/work/cera
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCHDIR
  ARCHIVEDIR=$SCRATCHDIR
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=$SCRIPTDIR/input/machines/supermike/supermike.template.pbs
  MATLABEXE=mex
  MCRROOT=/usr/local/packages/license/matlab/r2017a # for matlab mex files
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=16
  TDS=(lsu_tds)
  if [[ $operator = "jgflemin" ]]; then
     ADCIRCDIR=${HOME}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${HOME}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
  fi
  MAKEJOBS=8
}
#
init_queenbee()
{ #<- can replace the following with a custom script
  THIS="platforms.sh>env_dispatch()>init_queenbee()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=queenbee.loni.org
  QUEUESYS=PBS
  PPN=20
  QCHECKCMD=qstat
  QSUMMARYCMD=showq
  QUOTACHECKCMD=showquota
  ALLOCCHECKCMD=showquota
  QUEUENAME=workq
  SERQUEUE=single
  SUBMITSTRING=qsub
  SCRATCHDIR=/work/$operator
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl # asgs looks in $SCRIPTDIR for this
  RMQMessaging_LocationName="LONI"
  RMQMessaging_ClusterName="Queenbee"
  RMQMessaging_Enable="on"      # "on"|"off"
  RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
  RMQMessaging_NcoHome="$HOME/local"
  JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
  ACCOUNT=null
  PLATFORMMODULES='module load intel netcdf netcdf_fortran'
  # modules for CPRA post processing
  MATLABEXE=script # "script" means just execute matlab
  MCRROOT=/usr/local/packages/license/matlab/r2017a
  SERIALMODULES='module load matlab/r2015b'
  PARALLELMODULES='module load mvapich2'
  JOBENVDIR=$SCRIPTDIR/config/machines/queenbee
  JOBENV=( )
  TDS=( lsu_tds )
  # needed for asgs perl
  #source ~/perl5/perlbrew/etc/bashrc
  if [ -z "$_ASGS_PID" ]; then 
    module purge
    $PLATFORMMODULES
    $SERIALMODULES
  fi
  if [[ $operator = "jgflemin" || $USER = "jgflemin" ]]; then
     ACCOUNT=loni_cera_2020
     ADCIRCDIR=${HOME}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${HOME}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
     JOBENV=( ) # all exes are in /work/jgflemin/opt/default/bin ; all libs are in /work/jgflemin/default/lib
     for script in $JOBENV; do
        source $JOBENVDIR/$script
     done
     MATLABEXE=mex  # "mex" means use the precompiled mex files
  fi
  if [[ $operator == "mbilskie" || $USER = "mbilskie" ]]; then
     ACCOUNT=loni_lsu_ccr_19
     ADCIRCDIR=/home/mbilskie/src/PADCIRC/adcirc-cg-53.04/work # ADCIRC executables
     SWANDIR=/home/mbilskie/src/PADCIRC/adcirc-cg-53.04/swan # ADCIRC executables
     # needed for asgs perl
     source /project/mbilskie/perlbrew/etc/bashrc
     JOBENV=( perlbrew.sh )
     for script in $JOBENV; do
        source $JOBENVDIR/$script
     done
     TDS=(lsu_ccr_tds)
     RMQMessaging_LocationName="QB2"
     RMQMessaging_ClusterName="Queenbee2"
     RMQMessaging_Enable="on"      # "on"|"off"
     RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
     RMQMessaging_NcoHome="$HOME/local"
     SCRATCHDIR=/work/$operator/asgs/2019/runs/
  fi
  THIS=platforms.sh
  SSHKEY=~/.ssh/id_rsa.pub
  REMOVALCMD="rmpurge"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCHDIR
  ARCHIVEDIR=$SCRATCHDIR
  # @jasonfleming: for ~/.bashrc: Prevent git push from opening up a graphical
  # dialog box to ask for a password; it will interactively ask for
  # a password instead
  unset SSH_ASKPASS
  MAKEJOBS=8
}
#
init_rostam()
{ #<- can replace the following with a custom script
  THIS="platforms.sh>env_dispatch()>init_rostam()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=rostam.cct.lsu.edu
  QUEUESYS=SLURM
  QCHECKCMD=squeue
  QSUMMARYCMD=squeue
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  QUEUENAME=marvin  # same as SLURM partition 
  SERQUEUE=marvin
  ACCOUNT=null
  SUBMITSTRING=sbatch
  #JOBLAUNCHER='srun -N %nnodes%'
  JOBLAUNCHER='salloc -p marvin -N %nnodes% -n %totalcpu%' 
  SCRATCHDIR=${HOME}/asgs
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCHDIR
  ARCHIVEDIR=$SCRATCHDIR
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=rostam.template.slurm
  QSCRIPTGEN=hatteras.slurm.pl
  PPN=16
  CONSTRAINT=null
  RESERVATION=null
  REMOVALCMD="rm"
  TDS=( lsu_tds )
  if [[ $operator = "jgflemin" ]]; then
     ADCIRCDIR=${HOME}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${HOME}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
  fi
  PLATFORMMODULES='module load mpi/mpich-3.0-x86_64'
  $PLATFORMMODULES
  # modules for CPRA post processing
  #module load mpi/mpich-3.0-x86_64
  module purge 
  module load impi/2017.3.196 
  MAKEJOBS=8
}
init_supermic()
{ #<- can replace the following with a custom script
  THIS="platforms.sh>env_dispatch()>init_supermic()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=supermic.hpc.lsu.edu
  QUEUESYS=PBS
  PPN=20
  QCHECKCMD=qstat
  QSUMMARYCMD=showq
  QUOTACHECKCMD=showquota
  ALLOCCHECKCMD=showquota
  QUEUENAME=workq
  SERQUEUE=single
  SUBMITSTRING=qsub
  SCRATCHDIR=/work/$operator
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  RMQMessaging_LocationName="LSU"
  RMQMessaging_ClusterName="SuperMIC"
  RMQMessaging_Enable="on"      # "on"|"off"
  RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
  RMQMessaging_NcoHome="$HOME/local"
  JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
  ACCOUNT=null
  PLATFORMMODULES='module load intel/14.0.2 hdf5/1.8.12/INTEL-140-MVAPICH2-2.0 netcdf/4.2.1.1/INTEL-140-MVAPICH2-2.0 netcdf_fortran/4.2/INTEL-140-MVAPICH2-2.0'
  # modules for CPRA post processing
  MATLABEXE=script # "script" means just execute matlab; only for LSU staff, not affiliates
  MCRROOT=/usr/local/packages/license/matlab/r2017a
  SERIALMODULES='module load matlab/r2017a'
  PARALLELMODULES='module load mvapich2'
  JOBENVDIR=$SCRIPTDIR/config/machines/supermic
  PERL5LIB=${PERL5LIB}:${SCRIPTDIR}/PERL
  JOBENV=( )
  if [[ $operator = "jgflemin" ]]; then
     ADCIRCDIR=${HOME}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${HOME}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
     ACCOUNT=hpc_cera_2019c
     #ACCOUNT=hpc_crc_smi_19
     JOBENV=( )
     for script in $JOBENV; do
        source $JOBENVDIR/$script
     done
  fi
  if [[ $operator = "alireza" ]]; then  # User config for Al
     ACCOUNT=hpc_cera_2019c
  fi
  THIS="platforms.sh>env_dispatch()>init_supermic()"
  SSHKEY=~/.ssh/id_rsa.pub
  REMOVALCMD="rmpurge"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCHDIR
  ARCHIVEDIR=$SCRATCHDIR
  TDS=( lsu_tds )
  module purge
  $PLATFORMMODULES
  $SERIALMODULES
  MAKEJOBS=8
}
init_pod()
{ #<- can replace the following with a custom script
  THIS="platforms.sh>env_dispatch()>init_pod()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=pod.penguincomputing.com
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=null
  QSUMMARYCMD=null
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  QUEUENAME=B30
  SERQUEUE=B30
  SUBMITSTRING=qsub
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl # asgs looks in $SCRIPTDIR for this
  SCRATCHDIR=$HOME/asgs
  #
  RMQMessaging_LocationName="Penguin"
  RMQMessaging_ClusterName="POD"
  RMQMessaging_Enable="on"      # "on"|"off"
  RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
  RMQMessaging_NcoHome="$HOME/local"
  JOBLAUNCHER='mpirun -np %totalcpu% -machinefile $PBS_NODEFILE'
  PLATFORMMODULES='module load gcc/6.2.0'
  # modules for CPRA post processing
  SERIALMODULES='module load '
  PARALLELMODULES='module load openmpi/2.1.2/gcc.6.2.0'
  JOBENV=( )
  if [[ $operator = "jgflemin" ]]; then
     ADCIRCDIR=${HOME}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${HOME}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
     JOBENV=( netcdf.sh )
     for script in $JOBENV; do
        source $JOBENVDIR/$script
     done
  fi
  THIS="platforms.sh>env_dispatch()>init_pod()"
  SSHKEY=~/.ssh/id_rsa.pub
  RESERVATION=null
  PPN=28
   if [[ $operator = bblanton ]]; then
      SCRATCHDIR=/home/bblanton/asgs_scratch
      RMQMessaging_NcoHome="/home/bblanton/"
   fi
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCHDIR
  ARCHIVEDIR=$SCRATCHDIR
  TDS=(renci_tds)
  module purge
  $PLATFORMMODULES
  $SERIALMODULES
  MAKEJOBS=8
}
init_hatteras()
{ #<- can replace the following with a custom script
  THIS="platforms.sh>env_dispatch()>init_hatteras()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=hatteras.renci.org
  QUEUESYS=SLURM
  QUEUENAME=batch # <---<< PARTITION synonym on slurm
  SERQUEUE=batch
  PPN=null
  CONSTRAINT=null      # ivybridge or sandybridge
  RESERVATION=null    # ncfs or null, causes job to run on dedicated cores
  PARTITION=null
  QCHECKCMD=sacct
  JOBLAUNCHER='srun '
  ACCOUNT=null
  SUBMITSTRING=sbatch
  SCRATCHDIR=/projects/ncfs/data
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  WALLTIMEFORMAT="minutes"
  QSUMMARYCMD=null
  QUOTACHECKCMD="df -h /projects/ncfs"
  ALLOCCHECKCMD=null
  TDS=( renci_tds )
  # 
  MATLABEXE=script # "script" means just execute matlab (don't use mex files)
  #
  RMQMessaging_Enable="on"      # "on"|"off"
  RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
  RMQMessaging_NcoHome="/home/ncfs"
  RMQMessaging_LocationName="RENCI"
  RMQMessaging_ClusterName="Hatteras"

  #
  # specify location of platform- and Operator-specific scripts to 
  # set up environment for different types of jobs
  JOBENVDIR=$SCRIPTDIR/config/machines/hatteras
  JOBENV=( gdal.sh gmt.sh fftw.sh netcdf.sh )
  case $USER in 
  bblanton) 
     export MODULEPATH=$MODULEPATH:/projects/acis/modules/modulefiles
     ACCOUNT=bblanton # Brian you can override these values in your asgs config file for each instance (or even make these values different for different ensemble members)
     SCRATCHDIR=/scratch/bblanton/data
     PYTHONVENV=/projects/storm_surge/anaconda
     PLATFORMMODULES='module load mvapich2/2.0-acis'
     SERIALMODULES='module load' # no extra modules for serial jobs
     ;;
  ncfs-dev)
     export MODULEPATH=$MODULEPATH:/projects/acis/modules/modulefiles
     ADCIRCDIR="${HOME}/ADCIRC/v53release/work" # ADCIRC executables
     SWANDIR="${HOME}/ADCIRC/v53release/swan" # ADCIRC executables
     SCRATCHDIR=/scratch/ncfs-dev/
     ACCOUNT=ncfs-dev
     PARTITION=ncfs       # ncfs or batch, gives priority
     PYTHONVENV="$HOME/miniconda2"
     RMQMessaging_NcoHome="${HOME}"
     PLATFORMMODULES='module load intelc/18.0.0 intelfort/18.0.0 hdf5/1.8.12-acis netcdf/4.1.2-acis mvapich2/2.0-acis'
     SERIALMODULES='module load' # no extra modules for serial jobs
     TDS=(renci_tds)
     ;;
  ncfs)
     export MODULEPATH=$MODULEPATH:/projects/acis/modules/modulefiles
     ADCIRCDIR=${HOME}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${HOME}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
     ACCOUNT=ncfs
     QUEUENAME=ncfs     # SLURM partition---ncfs or batch---gives priority
     PYTHONVENV=~/asgs/asgspy/venv
     PLATFORMMODULES='module load intelc/18.0.0 intelfort/18.0.0 zlib/1.2.11_intel-18.0.0'
     PLATFORMMODULES="$PLATFORMMODULES mvapich2/2.0-acis"
     SERIALMODULES='module load' # no extra modules for serial jobs
     ;;
  *)
     PLATFORMMODULES='module load intelc/18.0.0 openmpi/intel_3.0.0'
     ;;
  esac
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCHDIR
  ARCHIVEDIR=$SCRATCHDIR
  module purge
  $PLATFORMMODULES
  $PARALLELMODULES
  $SERIALMODULES
  MAKEJOBS=8
}
#
init_frontera()
{ #<- can replace the following with a custom script
  THIS="platforms.sh>env_dispatch()>init_frontera()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=frontera.tacc.utexas.edu
  QUEUESYS=SLURM
  QUEUENAME=normal
  SERQUEUE=normal
  PPN=56
  CONSTRAINT=null
  RESERVATION=null
  QOS=null
  QCHECKCMD=sacct
  JOBLAUNCHER='ibrun '
  ACCOUNT=null
  SUBMITSTRING=sbatch
  SCRATCHDIR=$SCRATCH
  SSHKEY=~/.ssh/id_rsa_frontera
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  GROUP="G-803086"
  QSUMMARYCMD=null
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  #
  RMQMessaging_LocationName="TACC"
  RMQMessaging_ClusterName="Frontera"
  RMQMessaging_Enable="on"      # "on"|"off"
  RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
  RMQMessaging_NcoHome="$WORK/local"
  #
  PLATFORMMODULES='module load intel/19.0.5 xalt/2.7.19 TACC'
  SERIALMODULES='module load' # no extra modules for serial jobs
  PARALLELMODULES='module load impi/19.0.5'
  # matlab
  MATLABEXE=script # "script" means just execute matlab (don't use mex files)
  # specify location of platform- and Operator-specific scripts to 
  # set up environment for different types of jobs
  JOBENVDIR=$SCRIPTDIR/config/machines/frontera
  JOBENV=( )
  if [[ $operator = jgflemin ]]; then
     ADCIRCDIR=${WORK}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${WORK}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
     ACCOUNT=ASC20001
     # don't use built in netcdf module
     JOBENV=( netcdf.sh gmt.sh gdal.sh )
     for script in $JOBENV; do 
        source $JOBENVDIR/$script
     done
  fi
  THIS="platforms.sh>env_dispatch()>init_frontera()"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=/corral-tacc/utexas/hurricane/ASGS
  ARCHIVEDIR=2020 # is this used? 
  TDS=( tacc_tds )
  MAKEJOBS=8
  # only run env module commands if not in asgsh
  if [ -z "$_ASGS_PID" ]; then 
    $PLATFORMMODULES
    $SERIALMODULES
  fi
}
#
init_stampede2()
{ #<- can replace the following with a custom script
  THIS="platforms.sh>env_dispatch()>init_stampede2()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=stampede2.tacc.utexas.edu
  QUEUESYS=SLURM
  QUEUENAME=skx-normal # same as SLURM partition
  SERQUEUE=skx-normal
  PPN=48
  CONSTRAINT=null
  RESERVATION=null
  QOS=null
  QCHECKCMD=sacct
  JOBLAUNCHER='ibrun '
  ACCOUNT=null
  SUBMITSTRING=sbatch
  SCRATCHDIR=$SCRATCH
  SSHKEY=~/.ssh/id_rsa_stampede2
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  GROUP="G-803086"
  QSUMMARYCMD=null
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  #
  RMQMessaging_LocationName="TACC"
  RMQMessaging_ClusterName="Stampede2"
  RMQMessaging_Enable="on"              # "on"|"off"
  RMQMessaging_Transmit="on"            #  enables message transmission ("on" | "off")
  RMQMessaging_NcoHome="$WORK/local"
  PLATFORMMODULES='module unload python2/2.7.15 ; module load intel/18.0.2 xalt/2.6.5 TACC'
  SERIALMODULES='module load matlab' # no extra modules for serial jobs
  PARALLELMODULES='module load libfabric/1.7.0 impi/18.0.2'
  # matlab
  MATLABEXE=script # "script" means just execute matlab (don't use mex files)
  # specify location of platform- and Operator-specific scripts to 
  # set up environment for different types of jobs
  JOBENVDIR=$SCRIPTDIR/config/machines/stampede2
  JOBENV=( )
  if [[ $operator = jgflemin ]]; then
     ADCIRCDIR=${WORK}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${WORK}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
     ACCOUNT=DesignSafe-CERA
     # don't use built in netcdf module
     JOBENV=( netcdf.sh gmt.sh gdal.sh )
     for script in $JOBENV; do 
        source $JOBENVDIR/$script
     done
  fi
  THIS="platforms.sh>env_dispatch()>init_stampede2()"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=/corral-tacc/utexas/hurricane/ASGS
  ARCHIVEDIR=2020
  TDS=( tacc_tds )
  $PLATFORMMODULES
  $SERIALMODULES
  MAKEJOBS=8
}
#
init_lonestar5()
{ #<- can replace the following with a custom script
  THIS="platforms.sh>env_dispatch()>init_lonestar5()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=lonestar5.tacc.utexas.edu
  QUEUESYS=SLURM
  QUEUENAME=normal # same as SLURM partition
  SERQUEUE=normal
  CONSTRAINT=null
  RESERVATION=null
  QCHECKCMD=squeue
  JOBLAUNCHER='ibrun '
  ACCOUNT=null
  PPN=24
  SUBMITSTRING=sbatch
  SCRATCHDIR=$SCRATCH
  SSHKEY=id_rsa_lonestar5
  QSCRIPTTEMPLATE=$SCRIPTDIR/qscript.template
  QSCRIPTGEN=qscript.pl
  UMASK=006
  GROUP="G-803086"
  QSUMMARYCMD=null
  QUOTACHECKCMD=null
  ALLOCCHECKCMD=null
  # matlab
  MATLABEXE=script # "script" means just execute matlab (don't use mex files)
  #
  RMQMessaging_LocationName="TACC"
  RMQMessaging_ClusterName="Lonestar5"
  RMQMessaging_Enable="on"      # "on"|"off"
  RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
  RMQMessaging_NcoHome="$WORK/local"
  #
  ml reset
  PLATFORMMODULES='module unload python3/3.7.0 ; module load intel/18.0.2 TACC/1.0'
  SERIALMODULES='module load' # no extra modules for serial jobs
  PARALLELMODULES='module load cray_mpich/7.7.3'
  # specify location of platform- and Operator-specific scripts to
  # set up environment for different types of jobs
  JOBENVDIR=$SCRIPTDIR/config/machines/lonestar5
  JOBENV=( )
  if [[ $operator = jgflemin ]]; then
     ADCIRCDIR=${WORK}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${WORK}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
     ACCOUNT=ADCIRC
     # don't use built in netcdf module
     JOBENV=( )
     for script in $JOBENV; do
        source $JOBENVDIR/$script
     done
  fi
  THIS="platforms.sh>env_dispatch()>init_lonestar5()"
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=/corral-tacc/utexas/hurricane/ASGS
  ARCHIVEDIR=2020
  TDS=(tacc_tds)
  $PLATFORMMODULES
  $SERIALMODULES
  # btw git on lonestar5 is messed up when it outputs things like diffs,
  # found the solution:
  # git config --global core.pager "less -r"
  MAKEJOBS=8
}

# placeholder for vagrant bootstrap
init_vagrant() {
  MAKEJOBS=2
}

init_desktop()
{
  THIS="platforms.sh>env_dispatch()>init_desktop()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=jason-desktop.seahorsecoastal.com
  QUEUESYS=mpiexec
  QCHECKCMD="ps -aux | grep mpiexec "
  SUBMITSTRING="mpiexec -n "
  SCRATCHDIR=/srv/asgs
  SSHKEY=id_rsa_jason-desktop
  ADCOPTIONS='compiler=gfortran MACHINENAME=jason-desktop'
  SWANMACROSINC=macros.inc.gfortran
  if [[ $operator = "jason" ]]; then
     ADCIRCDIR=${HOME}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${HOME}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
     RMQMessaging_Enable="on"   # "on"|"off"
     RMQMessaging_Transmit="on" #  enables message transmission ("on" | "off")
     RMQMessaging_NcoHome=$HOME
     RMQMessaging_LocationName="Seahorse"
     RMQMessaging_ClusterName="jason-desktop"
  fi
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCHDIR
  ARCHIVEDIR=$SCRATCHDIR
  TDS=(renci_tds)
  MAKEJOBS=1
}

init_desktop_serial() # changed from init_desktop-serial due to bash complaints 
{
  THIS="platforms.sh>env_dispatch()>init_desktop-serial()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=jason-desktop-serial
  QUEUESYS=serial
  QCHECKCMD="ps -aux | grep adcirc "
  SUBMITSTRING="./"
  SCRATCHDIR=/srv/asgs
  SSHKEY=id_rsa_jason-desktop-serial
  ADCOPTIONS='compiler=gfortran MACHINENAME=jason-desktop-serial'
  SWANMACROSINC=macros.inc.gfortran
  if [[ $operator = "jason" ]]; then
     ADCIRCDIR=${HOME}/adcirc-cg/jasonfleming/v53release/work # ADCIRC executables
     SWANDIR=${HOME}/adcirc-cg/jasonfleming/v53release/swan   # SWAN executables
     RMQMessaging_Enable="on"   # "on"|"off"
     RMQMessaging_Transmit="on" #  enables message transmission ("on" | "off")
     RMQMessaging_Script="/set/RMQMessaging_Script/in/asgs/config"
     RMQMessaging_NcoHome=$HOME
     RMQMessaging_LocationName="Seahorse"
     RMQMessaging_ClusterName="jason-desktop-serial"
  fi
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCHDIR
  ARCHIVEDIR=$SCRATCHDIR
  TDS=(renci_tds)
  MAKEJOBS=1
}

init_Poseidon()
{
  HPCENV=poseidon.vsnet.gmu.edu
  QUEUESYS=mpiexec
  QCHECKCMD="ps -aux | grep mpiexec "
  SUBMITSTRING="mpiexec -n"
  SCRATCHDIR=/home/fhrl/Documents/asgs_processing
  SSHKEY=id_rsa_jason-desktop
  ADCOPTIONS='compiler=gfortran MACHINENAME=jason-desktop'
  SWANMACROSINC=macros.inc.gfortran
  ARCHIVE=enstorm_pedir_removal.sh
  ARCHIVEBASE=$SCRATCHDIR
  ARCHIVEDIR=$SCRATCHDIR
  MAKEJOBS=1
}
init_penguin()
{ #<- can replace the following with a custom script
  THIS="platforms.sh>env_dispatch()>init_penguin()"
  scenarioMessage "$THIS: Setting platforms-specific parameters."
  HPCENV=pod.penguincomputing.com
  #HOSTNAME=login-29-45.pod.penguincomputing.com
  QUEUESYS=PBS
  QCHECKCMD=qstat
  SCRATCHDIR=/home/$USER
  SUBMITSTRING="mpirun"
  QSCRIPT=penguin.template.pbs
  QSCRIPTGEN=penguin.pbs.pl
  PPN=40
  MAKEJOBS=8
}
init_test()
{ #<- can replace the following with a custom script
  QUEUESYS=Test
  NCPU=-1
  MAKEJOBS=1
}
#
# Writes properties related to the combination of the HPC platform, the Operator, 
# and the THREDDS data server the results are to be posted to. 
writeTDSProperties()
{
   THIS="platforms.sh>writeTDSProperties()"
   scenarioMessage "$THIS: Setting platforms-specific parameters for ${SERVER}."
   operator=$USER
   SERVER=$1
   CATALOGPREFIX=""    # after thredds/catalog
   DOWNLOADPREFIX=""   # after thredds/fileServer
   OPENDAPUSER=$operator
   OPENDAPMAILSERVER=mailx  # this is the local default mail server executable on the HPC
   case $SERVER in
   "renci_tds")
      # THREDDS Data Server (TDS, i.e., OPeNDAP server) at RENCI
      # http://tds.renci.org:8080/thredds/fileServer/DataLayers/asgs/tc/nam/2018070806/ec_95d/pod.penguin.com/podtest/namforecast/maxele.63.nc
      # http://tds.renci.org:8080/thredds/dodsC/     DataLayers/asgs/tc/nam/2018070806/ec_95d/pod.penguin.com/podtest/namforecast/maxele.63.nc
      # http://tds.renci.org:8080/thredds/catalog/                   tc/nam/2018070806/ec_95d/pod.penguin.com/podtest/namforecast/catalog.html
      OPENDAPHOST=ht4.renci.org
      THREDDSHOST=tds.renci.org
      OPENDAPPORT=":8080"
      OPENDAPPROTOCOL="http"
      OPENDAPBASEDIR=/projects/ncfs/opendap/data
      SSHPORT=22
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> run.properties
      echo "post.opendap.${SERVER}.copyablehosts : ( hatteras )" >> run.properties
      if [[ $operator = jgflemin ]]; then
         OPENDAPUSER=ncfs
      fi
      #DOWNLOADPREFIX="http://tds.renci.org:8080/thredds/fileServer/DataLayers/asgs/"
      #CATALOGPREFIX="http://tds.renci.org:8080/thredds/DataLayers/asgs/"
      #OPENDAPBASEDIR=/projects/ees/DataLayers/asgs/
      ;;

   # THREDDS Data Server (TDS, i.e., OPeNDAP server) at LSU
   "lsu_tds") 
      OPENDAPHOST=fortytwo.cct.lsu.edu
      THREDDSHOST=$OPENDAPHOST
      OPENDAPPORT=":443"
      OPENDAPPROTOCOL="https"
      OPENDAPBASEDIR=/data/opendap
      SSHPORT=2525
      if [[ $USER = "ncfs" || $USER = "jgflemin" ]]; then
         OPENDAPUSER="jgflemin"
      fi
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> run.properties
      echo "post.opendap.${SERVER}.copyablehosts : ( null )" >> run.properties
      ;;

   # THREDDS Data Server (TDS, i.e., OPeNDAP server) at LSU Center for Coastal Resiliency
   "lsu_ccr_tds")
      OPENDAPHOST=chenier.cct.lsu.edu
      THREDDSHOST=$OPENDAPHOST
      OPENDAPPORT=":8080"
      OPENDAPPROTOCOL="http"
      CATALOGPREFIX=/asgs/ASGS-2019
      DOWNLOADPREFIX=/asgs/ASGS-2019
      OPENDAPBASEDIR=/data/thredds/ASGS/ASGS-2019
      SSHPORT=2525
      if [[ $USER = "ncfs" || $USER = "jgflemin" ]]; then
         OPENDAPUSER="jgflemin"
      fi
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> run.properties
      echo "post.opendap.${SERVER}.copyablehosts : ( null )" >> run.properties
      ;;
   #
   # THREDDS Data Server (TDS, i.e., OPeNDAP server) at Texas
   # Advanced Computing Center (TACC)
   "tacc_tds")
      OPENDAPHOST=adcircvis.tacc.utexas.edu
      THREDDSHOST=$OPENDAPHOST
      OPENDAPPORT=":8080"
      OPENDAPPROTOCOL="http"
      DOWNLOADPREFIX=/asgs
      CATALOGPREFIX=/asgs
      OPENDAPBASEDIR=/corral-tacc/utexas/hurricane/ASGS
      SSHPORT=null
      if [[ $USER = "ncfs" || $USER = "jgflemin" ]]; then
         OPENDAPUSER="jgflemin"
      fi
      echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> run.properties
      echo "post.opendap.${SERVER}.copyablehosts : ( lonestar5 stampede2 frontera )" >> run.properties
      ;;
   *)
      echo "$THIS: ERROR: THREDDS Data Server $SERVER was not recognized."
   esac
   # now write properties
   echo "post.opendap.${SERVER}.opendaphost : $OPENDAPHOST" >> run.properties
   echo "post.opendap.${SERVER}.threddshost : $THREDDSHOST" >> run.properties
   echo "post.opendap.${SERVER}.downloadprefix : $OPENDAPPROTOCOL://$THREDDSHOST$OPENDAPPORT/thredds/fileServer$DOWNLOADPREFIX" >> run.properties
   echo "post.opendap.${SERVER}.catalogprefix : $OPENDAPPROTOCOL://$THREDDSHOST$OPENDAPPORT/thredds/catalog$CATALOGPREFIX" >> run.properties
   echo "post.opendap.${SERVER}.opendapbasedir : $OPENDAPBASEDIR" >> run.properties
   echo "post.opendap.${SERVER}.sshport : $SSHPORT" >> run.properties
   echo "post.opendap.${SERVER}.opendapuser : $OPENDAPUSER" >> run.properties
   # if the Operator has an asgs-global.conf file, assume that a perl mail client capability is 
   # set up and ready to use
   # FIXME: create something more reliable/repeatable
   if [[ -e $HOME/asgs-global.conf ]]; then
      OPENDAPMAILSERVER=aws
   fi
   echo "notification.opendap.email.opendapmailserver : $OPENDAPMAILSERVER" >> run.properties
}
#
# set the values of HPCENV and HPCENVSHORT
set_hpc() {
   THIS="platforms.sh>set_hpc()"
   echo "$THIS: Setting the values of HPCENV and HPCENVSHORT."
   fqdn=`hostname --long` 
   echo "$THIS: The fully qualified domain name is ${fqdn}."
   HPCENV=null
   HPCENVSHORT=null
   if [[ ${fqdn:(-25)} = "stampede2.tacc.utexas.edu" ]]; then
      HPCENV=${fqdn:(-25)}
      HPCENVSHORT=stampede2
      return
   fi
   if [[ ${fqdn:(-19)} = "ls5.tacc.utexas.edu" ]]; then
      HPCENV=lonestar5.tacc.utexas.edu
      HPCENVSHORT=lonestar5
      return
   fi
   if [[ ${fqdn:(-24)} = "frontera.tacc.utexas.edu" ]]; then
      HPCENV=frontera.tacc.utexas.edu
      HPCENVSHORT=frontera
      return
   fi
   if [[ ${fqdn:0:2} = "qb" ]]; then
      HPCENV=queenbee.loni.org
      HPCENVSHORT=queenbee
   fi 
   if [[ ${fqdn:0:4} = "smic" ]]; then
      HPCENV=supermic.hpc.lsu.edu
      HPCENVSHORT=supermic 
   fi
   if [[ ${fqdn:0:2} = "ht" ]]; then
      HPCENV=hatteras.renci.org
      HPCENVSHORT=hatteras
   fi 
   if [[ ${fqdn:0:5} = "jason" ]]; then
      HPCENV=desktop.seahorsecoastal.com
      HPCENVSHORT=desktop
   fi 
   echo "$THIS: The value of HPCENV is ${HPCENV}."
   echo "$THIS: The value of HPCENVSHORT is ${HPCENVSHORT}."
}
#
# used to dispatch environmentally sensitive actions
env_dispatch() {
 HPCENVSHORT=$1
 THIS="platforms.sh>env_dispatch()"
 scenarioMessage "$THIS: Initializing settings for ${HPCENVSHORT}."
 echo "$THIS: Initializing settings for ${HPCENVSHORT}."
 case $HPCENVSHORT in
  "pod") allMessage "$THIS: POD (Penguin) configuration found."
          init_pod
          ;;
  "hatteras") allMessage "$THIS: Hatteras (RENCI) configuration found."
          init_hatteras
          ;;
  "supermike") allMessage "$THIS: Supermike (LSU) configuration found."
          init_supermike
          ;;
  "queenbee") allMessage "$THIS: Queenbee (LONI) configuration found."
          init_queenbee
          ;;
  "supermic") allMessage "$THIS: SuperMIC (LSU HPC) configuration found."
          init_supermic
          ;;
  "lonestar5") allMessage "$THIS: Lonestar (TACC) configuration found."
          init_lonestar5
          ;;
  "stampede2") allMessage "$THIS: Stampede2 (TACC) configuration found."
          init_stampede2
          ;;
  "frontera") allMessage "$THIS: Frontera (TACC) configuration found."
          init_frontera
          ;;
  "desktop") allMessage "$THIS: desktop configuration found."
          init_desktop
           ;;
  "desktop-serial") consoleMessage "$THIS: desktop-serial configuration found."
          init_desktop-serial
           ;;
  "poseidon") allMessage "$THIS: Poseidon configuration found."
          init_Poseidon
           ;;
  "penguin") allMessage "$THIS: Penguin configuration found."
          init_penguin
           ;;
  "rostam") allMessage "$THIS: rostam configuration found."
          init_rostam
           ;;
  "vagrant") allMessage "$THIS: vagrant configuration found."
          init_vagrant
           ;; 
  "test") allMessage "$THIS: test environment (default) configuration found."
          init_test
           ;;
  *) fatal "$THIS: '$HPCENVSHORT' is not a supported environment; currently supported options: stampede2, lonestar5, supermike, queenbee, supermic, hatteras, desktop, desktop-serial, su_tds, lsu_ccr_tds, renci_tds, tacc_tds"
     ;;
  esac
}

#!/bin/bash
#PBS -V
#PBS -N %jobtype%.%enstorm%
#PBS -l nodes=1:ppn=20
#PBS -l walltime=%walltime%
#PBS -M %notifyuser%
#PBS -A %account%
#PBS -j oe
#PBS -o %advisdir%/%enstorm%/%jobtype%.%enstorm%.out
#PBS -q priority
cd %advisdir%/%enstorm%
# load netcdf modules in case they are needed
module load intel
module load netcdf
module load netcdf_fortran
#
echo "%enstorm% job starting at `date`"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] : %jobtype%.%enstorm% starting in %advisdir%/%enstorm%" > %jobtype%.%enstorm%.run.start
%adcircdir%/adcprep --np %ncpu% --%jobtype% >> adcprep.log 2>&1
ERROVALUE=$?
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
if [ $ERROVALUE == 0 ] ; then
   RUNSUFFIX="finish"
else
   RUNSUFFIX="error"
fi
echo "[${DATETIME}] : adcprep finished in %advisdir%/%enstorm% with return value = $ERROVALUE" > %jobtype%.%enstorm%.run.${RUNSUFFIX}
echo "adcprep job finished at $DATETIME."

#!/bin/bash
#----------------------------------------------------------------------------
#            Q U E U E   S Y S T E M   D I R E C T I V E S
#----------------------------------------------------------------------------
#SBATCH --job-name="adcprep"
#SBATCH --time=02:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --partition=normal
#SBATCH --account=ADCIRC
#SBATCH --output=%RUNDIR%/adcprep.out
#SBATCH --mail-type=FAIL,TIME_LIMIT
#SBATCH --mail-user=asgsnotify@memenesia.net

cd %RUNDIR%

adcprep<<EOADCPREP
%WCPU%
1
fort.14
EOADCPREP

adcprep<<EOADCPREP
%WCPU%
2
EOADCPREP

touch %RUNDIR%/adcprep.done 

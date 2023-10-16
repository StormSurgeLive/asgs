#!/bin/bash
#----------------------------------------------------------------------------
#            Q U E U E   S Y S T E M   D I R E C T I V E S
#----------------------------------------------------------------------------
#SBATCH --job-name="padcirc"
#SBATCH --time=36:00:00
#SBATCH --ntasks-per-node=%PPN%
#SBATCH --ntasks=%NCPU%
#SBATCH --nodes=%NODES%
#SBATCH --partition=normal
#SBATCH --account=ADCIRC
#SBATCH --output=%RUNDIR%/padcirc.out
#SBATCH --mail-type=FAIL,TIME_LIMIT
#SBATCH --mail-user=asgsnotify@memenesia.net

cd %RUNDIR% 

time ibrun padcirc -W 1 # %WCPU% + 1 writer = %NCPU% total CPUs

touch %RUNDIR%/padcirc.done

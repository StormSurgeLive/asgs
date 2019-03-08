#!/bin/bash
#
#----------------------------------------------------------------------------
#                    S L U R M   D I R E C T I V E S
#----------------------------------------------------------------------------
#SBATCH -J mpihelloworld
#SBATCH -t 300
#SBATCH -n 160 
#SBATCH --partition=ncfs
#SBATCH -o /projects/ncfs/data/mpihelloworld_mvapich2_2.0_ch3.out
#
#----------------------------------------------------------------------------
#        L O G   M E S S A G E S   T O   S T A R T   T H E   J O B
#----------------------------------------------------------------------------
cd /projects/ncfs/data
#
hostname | tee CONTROL.TXT
echo "Job Run on Nodes"  | tee --append CONTROL.TXT
echo "----------------"  | tee --append CONTROL.TXT
echo $SLURM_JOB_NODELIST | tee --append CONTROL.TXT
echo "----------------"  | tee --append CONTROL.TXT
#
#----------------------------------------------------------------------------
#                      L O A D   M O D U L E S
#----------------------------------------------------------------------------
module load hdf5/1.10.1_intel-18.0.0
module load intelc/18.0.0
module load intelfort/18.0.0
#module load mvapich2/2.3b_intel-18.0.0_ch3_ofed-4.1
module load mvapich2/2.0_intel-18.0.0_ch3_ofed-4.1
module load netcdf-C/4.5.0_intel-18.0.0
module load netcdf-Fortran/4.4.0_intel-18.0.0
module load zlib/1.2.11_intel-18.0.0
module list
# 
#----------------------------------------------------------------------------
#                  E X E C U T E   T H E   J O B
#----------------------------------------------------------------------------
srun /home/ncfs/asgs/jasonfleming/2014stable/monitoring/mpi_hello_world_mvapich2_2.0_ch3.x 

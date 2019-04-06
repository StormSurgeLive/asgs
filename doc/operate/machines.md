# HPC Platform Specifics

This document describes specific idiosyncracies and logistics 
associated with specific HPC platforms. 


## Stampede2 (TACC) 

### CERA Allocation 2019

Project a 42,000 SU (node-hour) allocation on Stampede2. Default 500GB allocation on our archival storage system Ranch.

The project name is DesignSafe-CERA and Carola Kaiser is the PI.

Manage the project members using the TACC User Portal https://portal.tacc.utexas.edu

After login then navigate to Allocations https://portal.tacc.utexas.edu/projects-and-allocations to add users.

For technical assistance, submit a ticket via the portal as well https://portal.tacc.utexas.edu/tacc-consulting/-/consult/tickets/create

To access Stampede2, need to set up Multi-Factor Authentication as well https://portal.tacc.utexas.edu/tutorials/multifactor-authentication

## Penguin On Demand (POD) 

To check disk usage against quota:

lfs quota -h $HOME

There may be issues with linking to the correct parallel libraries when
compiling on a head node. 

I was trying to compile a parallel application on mt2 with gcc/gfortran 
and openmpi and was getting an error about not being able to find 
libfabric.so.1 ... 

Here are the modules I had loaded:

````
[jgflemin@podmt2-2699cc6 work]$ module list
Currently Loaded Modulefiles:
1) binutils/2.27/gcc.6.2.0 3) openmpi/2.1.2/gcc.6.2.0
2) gcc/6.2.0 4) netcdf/4.4.1.1/gcc.6.2.0
````

Here is the compilation that make attempted along with the associated error message:

```
mpif90 -DADCNETCDF -I/public/apps/netcdf/4.4.1.1/gcc.6.2.0/include -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -c -I . -I /home/jgflemin/adcirc-cg/jasonfleming/v53release/prep -O2 -ffixed-line-length-none -DADCNETCDF -I/public/apps/netcdf/4.4.1.1/gcc.6.2.0/include -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -DREAL8 -DLINUX -DCSCA -DCMPI -I /home/jgflemin/adcirc-cg/jasonfleming/v53release/work/odir4/ -o /home/jgflemin/adcirc-cg/jasonfleming/v53release/work/odir4/sizes.o /home/jgflemin/adcirc-cg/jasonfleming/v53release/src/sizes.F
mpif90: error while loading shared libraries: libfabric.so.1: cannot open shared object file: No such file or directory
make[1]: *** [/home/jgflemin/adcirc-cg/jasonfleming/v53release/work/odir4/sizes.o] Error 127
make[1]: Leaving directory `/home/jgflemin/adcirc-cg/jasonfleming/v53release/work'
make: *** [padcirc] Error 2
````

To correct this, compile ADCIRC on a compute node.

Login nodes do not have an infiniband connection.
You can compile on a compute node by using an interactive job:
 
````
qsub -I -q B30 -l nodes=1:ppn=28 
```` 
 
This will start a job on a compute node and connect you to the shell session.

When loading modules, it is best to first issue the command `module 
purge` to remove any pre-existing configuration. In addition, do not 
use `~/.bashrc` to load modules, as this will carry over into parallel 
job execution and could cause module conflicts. 


## Troubleshooting Output and Queue Scripts

Hello Jason,
 
The details of the job are not matching what you reported.
 
 
Below is the jobscript used for 25367446 which shows a different path and output file than you mentioned.
 
The log appears to show the job was run multiple times.
There is a padcirc.0274.{start,out} but no finish log.
It looks like job 25367445 used the same logfiles. 
 
It is best to either not change the default output file, or append the ${PBS_JOBID} to the filename so only one job is shown in the log file.  Attempting to troubleshoot job issues when they all use the same logfile just leads to confusion. 
I recommend leaving the ouptut file set to the default, then using  "<command> | tee -a padcirc.0274.run.start" 
This will result in the individual log files you setup, and the main joblog will contain all output for the job for easier troubleshooting.

```` 
#!/bin/bash
#PBS -S /bin/sh
## 
#PBS -q B30
#PBS -M jason.fleming@seahorsecoastal.com
#PBS -M jason.fleming@seahorsecoastal.com
#PBS -l nodes=10:ppn=28
#PBS -l walltime=30:00:00
#PBS -j oe
#PBS -N 0274
#PBS -o /home/jgflemin/projects/Agnew/output-noproject/0274/padcirc.0274.out

#PBS -m ea

module add gcc/6.2.0
module add netcdf/4.4.1.1/gcc.6.2.0
module add openmpi/2.0.1/gcc.6.2.0


cd /home/jgflemin/projects/Agnew/output-noproject/0274

echo "Test job starting at `date`"
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "[${DATETIME}] : Job starting" >> padcirc.0274.run.start
echo "[${DATETIME}] : output-noproject 0274" >> padcirc.0274.run.start
echo "[${DATETIME}] : padcirc" >> padcirc.0274.run.start

mpirun -machinefile $PBS_NODEFILE -np 280 /home/jgflemin/adcirc-cg/jasonfleming/v53release/work/padcirc -W 1 >> padcirc.0274.out

ERROVALUE=$?
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
if [ $ERROVALUE == 0 ] ; then
echo "[${DATETIME}] : Job finished with return value = $ERROVALUE" >> padcirc.0274.run.finish
echo "[${DATETIME}] : output-noproject 0274" >> padcirc.0274.run.finish
echo "[${DATETIME}] : padcirc" >> padcirc.0274.run.finish
else
echo "[${DATETIME}] : Job finished with return value = $ERROVALUE" >> run.error
echo "[${DATETIME}] : output-noproject 0274" >> padcirc.0274.run.error
echo "[${DATETIME}] : padcirc" >> padcirc.0274.run.error
fi
echo "Test job finished at `date`"

````

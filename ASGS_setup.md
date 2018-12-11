# ASGS : Setup/Getting Started
This is the RENCI fork of the ASGS code, for instrumenting ASGS with RabbitMQ messaging and porting to Penguin On Demand.  This also documents the process of configuring ASGS for a test run using the ec95d grid with NAM forcing.  The target platform (TP) is a large commercial HPC cluster called Penguin On Demand (POD), which uses the OpenPBS job scueduler.  Configuration details that are specific to the POD platform will be marked in *__bold/italics__* and you should substitute the appropriate information for your TP. 

## A few preliminary notes:
* Make your life easier by using padcirc, and not padcswan to get started. The wave model only complicates things. 
* Use the ec95d grid for setup and initial testing since it is very small

## Running ASGS
The following will run ASGS once it is configured, in this case for NAM-driven runs on the ec95d mesh on the POD cluster. 

`./asgs_main.sh -c config/2018/asgs_config_nam_pod.sh -e POD`

## Compile ADCIRC on the target platform.
Compile adcirc, padcirc, padcswan, hstime, aswip with netCDF support.  If the TP uses modules for 
loading specific applications/libaries/etc, you will need these later on to set up the template jobcontrol file.  
Whatever you use for compiling ADCIRC should be used when submitting the job.

This "work" directory is called $ADCIRCDIR in asgs_main.sh, i.e.,

`ADCIRCDIR=</path/to/adcirccode>/work`

## Configuring ASGS:
Switch to the asgs/2014stable branch.  Then, 

<ol>
<li> Go to config/2018 and copy one of the config files to edit.  It can be named anything, but something rational is helpful. 
  
 `cp asgs_config_nam_hatteras_ec95d.sh asgs_config_nam_penguin_ec95d.sh`
 
 Edit this new config file.  Many things can be left alone, but here is a list of the variables I modified to run ASGS on POD: 

```
INSTANCENAME=podtest                      # "name" of this ASGS process
H="/home/bblanton/"                       # Home dir for ASGS (I added this, just to use immeditely below)
ADCIRCDIR=${H}/adcirc-cg-52release/work   # ADCIRC executables
SCRIPTDIR=${H}/asgs                       # ASGS executables 
OUTPUTDIR=${SCRIPTDIR}/output             # post processing scripts 
PERL5LIB=${SCRIPTDIR}/PERL                # DateCale.pm perl module 
BACKGROUNDMET=on                          # NAM download/forcing 
TIDEFAC=on                                # tide factor recalc
TROPICALCYCLONE=off                       # tropical cyclone forcing 
WAVES=off                                 # wave forcing 
VARFLUX=off                               # variable river flux forcing 
SCRATCHDIR=/home/bblanton/asgs-scratch 
GRIDFILE=ec_95d.grd                       # mesh (fort.14) file 
GRIDNAME=ec95d                            # grid name 
CONTROLTEMPLATE=ec_95_fort.15_template    # fort.15 template 
FORT61="--fort61freq 900.0 --fort61netcdf" 
FORT62="--fort62freq 900.0 --fort62netcdf" 
FORT63="--fort63freq 3600.0 --fort63netcdf" 
FORT64="--fort64freq 3600.0 --fort64netcdf"
FORT7172="--fort7172freq 3600.0 --fort7172netcdf" 
FORT7374="--fort7374freq 3600.0 --fort7374netcdf" 
NETCDF4="--netcdf4" 
EMAILNOTIFY=yes                           # yes to have host HPC platform email notifications 
ems="bblanton@renci.org" 
ACTIVATE_LIST=$ems 
NEW_ADVISORY_LIST=$ems 
POST_INIT_LIST=$ems
POST_LIST=$ems
JOB_FAILED_LIST=$ems
NOTIFYUSER=$ems
ASGSADMIN=$ems
ARCHIVEBASE=/home/bblanton/asgs-archive
```
 
<li> Edit the `platform.pl` perl script to include variables that specify the HPC target and its job schedule variables.  
Probably easiest to copy one of the existing blocks and edit.  For POD, I added the following block: 

```
init_penguin()
{ #<- can replace the following with a custom script
  HOSTNAME=something.pod.penguin.com
  QUEUESYS=PBS
  QCHECKCMD=qstat
  ACCOUNT=noaccount
  SUBMITSTRING=submitstring
  SCRATCHDIR=/home/bblanton/asgs_scratch
  SSHKEY=~/.ssh/id_rsa.pub
  QSCRIPT=penguin.template.pbs
  PREPCONTROLSCRIPT=penguin.adcprep.template.pbs
  RESERVATION=null
  QUEUE=B30     # aka the partition in SLURM parlance
  QSCRIPTGEN=tezpur.pbs.pl
  PPN=28
}
```
<li> Now, we need to configure the details of the HPC resource (TP).  The main things are 1) which job scheduler does the TP use? 2) are modules used to load resources like compilers and netCDF? and 3) how mpi jobs are executed within the submit script.  In the POD case, the answers to these questions are PBS, yes, and mpirun.  Look at submit script templates in the machine directories and use one if possible.   
    
* In the inputs/machines directory, mkdir a new machine directory
    * `mkdir penguin`
* Copy other template files from a machine that is similar into this new directory and change the names appropriately.
    * `cp ../queenbee/* .`
    * `mv queenbee.template.pbs penguin.template.pbs`
    * `mv queenbee.adcprep.template.pbs penguin.adcprep.template.pbs`
* Edit these files for the TP.  For example, I added modules to both template files since these are used on POD
```
# specify modules used to compile ADCIRC
module purge
module load intel/2016
module load hdf5/1.8.17/intel.2016
module load netcdf/4.4.1.1/intel.2016
module load openmpi/2.0.1/intel.2016
```
* It is possible that one of the existing job scheduler scripts will work.  Check out tezper.pbs.pl or hatteras.slurm.pl and use one if you can.  Otherwise, you will need to copy one and modify it. POD uses PBS, so we'll try to use the tezpur script.  Make sure this script name is targeted in the platform.pl script (the QSCRIPTGEN variable). 
</ol>

 

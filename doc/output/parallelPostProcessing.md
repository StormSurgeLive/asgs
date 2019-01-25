% Parallel Post Processing
% ADCIRC Surge Guidance System
% July 2017

<!--  
~/.cabal/bin/pandoc -o parallelPostProcessing.pdf --variable mainfont=Georgia --latex-engine=xelatex --variable sansfont=Arial --variable fontsize=12pt --variable geometry:margin=1in parallelPostProcessing.md
-->

Introduction
============

There are many post processing tasks for ADCIRC results that are easily
parallelizable and have existing serial workflows. One example would be
visualization and rendering of individual time steps in the fulldomain time
varying water surface elevation `fort.63` file and fulldomain time varying wind
velocity `fort.74` files.  

The ASGS typically executes an Operator-supplied script in serial to perform
post processing tasks. However, this script can also create and a submit a
queue script that executes a large number of serial tasks as a job array that
allows all tasks to execute simultaneously. 

The next two sections describe the job array capability in general and the 
implementation of parallel post processing via job arrays in ASGS. 

Job Arrays
==========

Job Arrays are a feature of High Performance Computing (HPC) queueing systems
like SLURM and PBS. They allow an analyst to construct a single queue script
that will be submitted to the queueing system in as many identical copies as
specified.

The description here refers to the SLURM implementation because that is the
queueing system where ASGS has implemented job arrays. Once the PBS
implementation has been built and tested in ASGS, this document will be
updated.

SLURM
-----

In the case of SLURM, the only difference between the identical copies of the
job array queue script is that each one has been supplied with a unique value
of of the `SLURM_ARRAY_TASK_ID` parameter. As a result, the queue script must
use the value of `SLURM_ARRAY_TASK_ID` to select, configure, and perform the
correct unit of work.

The key elements of the SLURM queue script header are shown in the following
snippet: 

    #SBATCH --array=1-85%85
    #SBATCH -t 45
    #SBATCH -n 1

The `--array` option is used to establish the number of copies of the queue
script that are to be submitted, the value of `SLURM_ARRAY_TASK_ID` to be
assigned to each of the copies, and the number of tasks that should be allowed
to run concurrently. In the code snippet above, the `--array` option indicates
that 85 copies of the job array queue script should be comitted, they should be
assigned the task numbers 1, 2, ..., 85 consecutively, and that all 85 tasks
should be allowed to run concurrently.

The `-t` option specifies the estimated wall clock time in minutes that each
individual task will require, once it starts execution. It does not represent
the total time required for all job array tasks to complete. It measures time
starting from when a task begins execution, not from its submission. It is used
for every task, and cannot be set differently for different copies of the queue
script.  

The `-n` option provides the number of cores that each task should use. In this
example, each task runs on one core, i.e., serially.

The main caveat in the use of job arrays is the need to avoid collisions
between the tasks. An example would be unintentionally having tasks write
simultaneously to log files or shared output files. 

CERA Contour
------------

The parallel post production of otherwise serially generated visualization
products from the timesteps of fulldomain time varying output files is a
perfect candidate for accelerated production via job arrays in the ASGS.
However, the job array paradigm prohibition against collisions between serial
tasks precludes the use of any tool that relies on a statically named
configuration file, temp file, or statefile that cannot be named differently or
relocated for use by simultaneously executing tasks. 

The cera_contour.py script, which generates vector contours and produces
shapefiles from the fulldomain ADCIRC output files (both time varying files and
min/max files) is an ideal candidate for execution via job arrays. It accepts
the the ADCIRC output file name, shapefile name, and time step number to
process on the command line. It uses no statically named temp files, log files,
or output files, and produces individual sets of output files for each time
step. 

ASGS Implementation
===================

Parallel post processing with job arrays in ASGS consists of three components:

1. A driver script (e.g., `output/hsofs_renci_post.sh`), called by ASGS once a forecast job completes, manages the process of creating and running the job array, monitoring task completion progress, and packaging results. 

2. An application-specific post processing script (e.g., `output/cera_contour_post.sh`), called by the driver script, examines the available output files and fills in a job array queue script template with appropriate values, submits it to the queue, and immediately returns control to the driver script.

3. The job array queue script template (e.g., `output/cera_contour/ceraContourJobArray.template.slurm`) whose main challenge is matching the `SLURM_ARRAY_TASK_ID` value supplied to the task with the appropriate unit of work to be performed. Other duties of the job array script include reporting progress to the driver script and producing log files for troubleshooting and/or performance assessment.








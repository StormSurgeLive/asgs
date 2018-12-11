# ASGS : Enabling Messaging
This is the RENCI fork of the ASGS code, for instrumenting ASGS with RabbitMQ messaging and porting to Penguin On Demand (POD).

## Requirements
The messaging apparatus needs python, with pika, json, netCDF4 and other packages available.  If the system python 
does not have these, then the easiest thing to do is to create a virtual environment.   In the POD case, pika is not 
available,n the system python so I created asgspy virtual env as follows:

### make sure conda is available.
module load anaconda/5.0.1/python2.7

### create the virtual env
ASGS=/home/bblanton/asgs
cd $ASGS
conda env create -f asgs_py_env.yml --prefix=$ASGS/asgspy
conda activate $ASGS/asgspy
conda clean --all   # to save space since we are charged for it. 

Once the virtual env is created, you do not need to activate it to use its python.  Just use the full path 
to the python version explicitly, instead of the system python.  I.e., 

## set the python variable in config/<YYYY>/"your config file" to the python in this asgspy env. For example,
 
RMQMessaging_Python="/home/bblanton/asgs/asgspy/bin/python"


## Controlling Messaging
Messaging is controlled with variables in the config file.  The actual messaging code is written in python, 
called asgs-msgr.py, which is called from RMQMessage in logging.sh.  The actual message content is a json string,
and its content is critical.  Do not mess with it.  Any change to the transmitted message MUST be coordinated with
the messaging server and database.



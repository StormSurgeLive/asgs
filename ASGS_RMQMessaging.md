# ASGS : Enabling and Using Messaging
This is the RENCI fork of the ASGS code, for instrumenting ASGS with RabbitMQ messaging and porting to Penguin On Demand (POD).  This new (late 2018) feature sends ASGS status messages using the RabbitMQ implementation of AMQP.  The messaging code is written in python, and is called from the ASGS logging.sh facility.  Specific messages are generated within asgs_main.sh to reflect the current activity of the main script.  The messages are sent (via pika) to a RabbitMQ server/queue at RENCI.  Each incoming message is parsed and stored in a PostGRES database.  A Django-based web infrastructure then queries the database (at a 3-sec interval, e.g.) to populate a webpage with ASGS instance status and runtime information.  The basic system looks like this: 

![ASGS-Dash-Schem](/doc/figures/asgs-schem.png)


The draft/version 0.5 webpage looks like this:

![ASGS-Dash](/doc/figures/asgsdash.png)

## Requirements
The messaging apparatus needs python, with pika, json, netCDF4 and other packages available.  If the system python does not have these, then the easiest thing to do is to create a virtual environment.   In the POD case, pika is not available in the system python, so I created a virtual environment called asgspy as follows:

````
### make sure conda is available.
module load anaconda/5.0.1/python2.7

### create the virtual env
ASGS=/home/bblanton/asgs
cd $ASGS
conda env create -f asgs_py_env.yml --prefix=$ASGS/asgspy
conda activate $ASGS/asgspy
conda clean --all   # to save space since we are charged for it. 
````

Once the virtual env is created, you do not need to activate it to use its python.  Just use the full path 
to the python version explicitly, instead of the system python.  I.e., 

## set the python variable in the main config file
In "config/<YYYY>/your config file, set the RMQMessaging_Python variable to the python in this asgspy env. For example,
 
````
RMQMessaging_Python="/home/bblanton/asgs/asgspy/bin/python"
````

## Controlling Messaging 
Messaging is controlled with variables in the config file.  The actual messaging code is written in python, 
called asgs-msgr.py, which is called from RMQMessage in logging.sh.  The actual message content is a json string,
and its content is critical.  Do not mess with it.  Any change to the transmitted message MUST be coordinated with
the messaging server and database.  The following variables must be set for messaging to work:

````
# RMQ Messaging
RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
RMQMessaging_Script="${SCRIPTDIR}/asgs-msgr.py"
RMQMessaging_NcoHome="/home/bblanton/"
RMQMessaging_Python="/projects/storm_surge/anaconda/bin/python"
RMQMessaging_LocationName="PenguinComputing"
RMQMessaging_ClusterName="POD"
````



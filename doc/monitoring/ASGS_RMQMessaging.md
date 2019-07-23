# ASGS : Enabling and Using Messaging

RENCI has implemented a real-time monitoring facility for ASGS.  This new (late 2018) feature sends ASGS status messages using the RabbitMQ implementation of AMQP.  The messaging code is written in python, and is called from the ASGS logging.sh facility.  Specific messages are generated within asgs_main.sh to reflect the current activity of the main script.  The messages are sent (via pika) to a RabbitMQ server/queue, currently at RENCI.  Each incoming message is parsed and stored in a PostGRES database.  A Django-based web infrastructure then queries the database (at a 3-sec interval, e.g.) to populate a webpage with ASGS instance status and runtime information.  

The basic system looks like this: 

![ASGS-Dash-Schem](/doc/figures/asgs-schem.png)

The current version of the dashboard looks like this:

![ASGS-Dash](/doc/figures/asgsdash.png)

Three things need to be done in order to use messaging. 
1) Make sure the python pika and netCDF4 modules are available.  
2) Configure messaging in ASGS
3) Send machine name, from which messages will be sent, to brian_blanton@renci.org and lisa@renci.org.  The machine name must be added to the messaging server.


## Step 1) Requirements
The messaging apparatus needs python, with pika, json, netCDF4 and other packages available.  If the system python does not have these, then the easiest thing to do is to create a virtual environment. A virtual environment can be created  as follows, in this case called asgspy.  Specifics may vary, such as if anaconda is by default available.  On some HPC systems, software is loaded when needed using the "module" facility. If conda is already available, the first step below (make sure conda is available) is not needed.  Note that you will need to set the path a place to put the environment.  In the example below, it is in the main ASGS location, but need not be. 

````
### make sure conda is available.
	module load anaconda/5.0.1/python2.7
### create the virtual env
	ASGS=/home/<asgsoperator>/asgs   # set this to point to the main ASGS directory.
	conda env create -f $ASGS/asgs_py_env.yml --prefix=$HOME/asgspy
	conda activate $HOME/asgspy
	conda clean --all   # to save space since we are charged for it. 
````

In the case that conda is not available, you can use virtualenv (if available), or install the miniconda python package. In this example, miniconda is installed into an asgspy directory, separate from the ASGS source directory. 

````
### Download the Linux bash 64-bit install script (https://conda.io/en/latest/miniconda.html)
	wget https://repo.anaconda.com/miniconda/Miniconda2-latest-Linux-x86_64.sh  
### Execute the script:
	bash Miniconda2-latest-Linux-x86_64.sh
### Follow installation instructions, installing into a directory like asgspy (for example)
### Add this (suitably modified) to your shell rc file: 
	source /home/<asgsoperator>/asgspy/etc/profile.d/conda.sh
### Activate the miniconda2 environment
	source asgspy/bin/activate
### Install packages with conda as usual:
	conda install numpy
	conda install h5py
	conda install netCDF4
	conda install -c jmcmurray json
	conda install -c conda-forge/label/gcc7 pika
### Deactivate and move on ...
	conda deactivate
````
 

Once the virtual env is created, you do not need to activate it to use its python.  Just use the full path to the python version explicitly, instead of the system python.

## Setp 2) Configure messaging:

Set the python variable in the main config file

In "config/<YYYY>/your config file", set the RMQMessaging_Python variable to the python in this asgspy env. For example,
 
````
RMQMessaging_Python="/home/<asgsoperator>/asgspy/bin/python"
````

### Controlling Messaging 
Messaging is controlled with variables in the config file.  The actual messaging code is written in python, called asgs-msgr.py, which is called from RMQMessage in logging.sh.  The actual message content is a json string, and its content is critical.  Do not mess with it.  Any change to the transmitted message MUST be coordinated with the messaging server and database.  The following variables must be set for messaging to work:

````
# RMQ Messaging
RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
RMQMessaging_Script="${SCRIPTDIR}/monitoring/asgs-msgr.py"
RMQMessaging_NcoHome="NA"   # not currently used, future development
RMQMessaging_Python="<path to asgs python>"  #  /projects/storm_surge/anaconda/bin/python"
RMQMessaging_LocationName="PenguinComputing"
RMQMessaging_ClusterName="POD"
````

Step 3) Send machine name to renci.org 

Send an email to brian_blanton@renci.org and lisa@renci.org with the machine name that will send messages. Alternatively, send a message via the Slack ASGS2019 channel. 


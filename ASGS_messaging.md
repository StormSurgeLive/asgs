# ASGS for IDIOTS, enabling messaging
This is the RENCI fork of the ASGS code, for instrumenting ASGS with RabbitMQ messaging and porting to Penguin On Demand.   

# the messaging apparatus needs python, with pika, json, yaml, netCDF4 and other packages available.  If the system python does not have these, 
then the easiest thing to do is to create a virtual environment.   In the POD case, pika is not available, so I created asgspy as follows:

# make sure conda is available.
module load anaconda/5.0.1/python2.7

ASGS=/home/bblanton/asgs

cd $ASGS
conda env create -f asgs_py_env.yml --prefix=$ASGS/asgspy
conda activate $ASGS/asgspy
conda clean --all   # to save space since we're charged for it. 

# set the python variable in config/<YYYY>/"your config file" to the python in this asgspy env.
 
RMQMessaging_Python="/home/bblanton/asgs/asgspy/bin/python"




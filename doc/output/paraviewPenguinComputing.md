# Paraview on Penguin on Demand (POD) HPC

You can launch pvserver via an interactive job, then connect to it with paraview.
On the compute nodes, you'll need to use paraview/5.4.1_mesa since the compute nodes do not have GPUs.
 
To submit interactive job:

````
qsub -I -q S30 -l nodes=2:ppn=40
```` 
 
Once the job starts and the shell is started:

```` 
module load paraview/5.4.1_mesa
mpirun pvserver
````
 
pvserver will then show a connection URL which you an use on paraview client to via SCW.  Make sure to use 5.4.1 on scw so that GPU is utilized.
 
Or, you can start pvdataserver on the compute node, and run the renderserver on scw to utilize the gpu.
 
To start the renderserver on pod.scw, open terminal

````
source /var/spool/torque/torque.cfg
module load paraview/5.4.1
pvrenderserver --hostname=$QSUBHOST
````
 
Then choose "Client / Data Server / Render Server" as the Server Type when connecting with paraview.
Note, make sure the port numbers are correct. When i tried the gui defaulted to 22222 but renderserver was started on 22221
 
The paraview server can be started on compute nodes by running the following as an interactive job via "qsub -I"

```` 
module load paraview/5.4.1_mesa
mpirun pvserver
````
 
Paraview also has a Client / Data Server /Render Server which may be more useful since compute nodes do not have GPUs
This method, you run "pvdataserver" instead of pvserver on the compute nodes (same script as above), then on your login node run:

```` 
source /var/spool/torque/torque.cfg
module load paraview/5.4.1
pvrenderserver --hostname $QSUBHOST
```` 
 
Then in the paraview gui, File -> Connect -> Add server
Then use the URL info provided by pvserver  or  pvdataserver and pvrednerserver 

## Client/Server

Hello Jason,
 
The only paraview client/server configuration we support is via pod.scw.
 
It is technically possible to accomplish this by using multiple ssh tunnels on a per job basis, which you are welcome to try, however it will be unsupported.
 
The following link may help with the configuration if you choose to attempt it.  https://hpc.llnl.gov/software/visualization-software/paraview/running-paraview-client-server-mode
 
Regards,




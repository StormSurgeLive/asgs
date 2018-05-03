# ASGS for idiots

This is the RENCI fork of the ASGS code, for instrumenting ASGS with RabbitMQ messaging and porting to Penguin On Demand. 

## Running asgs
The following will run ASGS once it is configured, in this case for NAM-driven runs on the ec95d mesh on RENCI's Hatteras cluster. 

`./asgs_main.sh -c config/2018/asgs_config_nam_bob.sh -e hatteras`

## Compile ADCIRC on the target platform.

Compile adcirc, padcirc, padcswan, hstime, aswip with netCDF support

This "work" directory is called $ADCIRCDIR in asgs_main.sh, i.e.,

`ADCIRCDIR=</path/to/adcirccode>/work`

## Configuring ASGS:
Switch to the asgs/2014stable branch.  Then, 

<ol>
<li>
<li>
</ol>


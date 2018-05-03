This is the RENCI fork of the ASGS code, for instrumenting ASGS with RabbitMQ messaging and porting to Penguin On Demand. 

The following will run ASGS once it is configured, in this case for NAM-driven runs on the ec95d mesh on RENCI's Hatteras cluster. 

./asgs_main.sh -c config/2018/asgs_config_nam_bob.sh -e hatteras

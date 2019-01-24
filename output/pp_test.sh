#!/bin/bash

CONFIG="/home/bblanton/asgs/config/2018/asgs_config_nam_penguin_ec95d.sh"
ADVISDIR="/home/bblanton/asgs-scratch/asgs8851/2018070612/"
STORM=99
YEAR=2018
ADVISORY=2018070612 
HOSTNAME="tds.renci.org"
ENSTORM="namforecast"
CSDATE=2018052700 
HSTIME=3499200.0 
GRIDFILE="fort.14" 
OUTPUTDIR="/home/bblanton/asgs/output"
SYSLOG="pp_test.log"
SSHKEY="~/.ssh/id_rsa.pub" 

/home/bblanton/asgs/output/pod_post.sh  $CONFIG $ADVISDIR $STORM $YEAR $ADVISORY $HOSTNAME $ENSTORM $CSDATE $HSTIME $GRIDFILE $OUTPUTDIR $SYSLOG $SSHKEY

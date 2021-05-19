#!/bin/bash

storm='AL28'
advisory=$1

if [ "$advisory" = "" ]; then
    echo "Please include advisory number..."
    exit
fi

for i in *.inp; do

    echo $i

    if [ "$i" = "FG51_SELA_maxele.inp" ]; then
        ensemble="nhcConsensus"
    elif [ "$i" = "FG51_SELA_maxele_veerLeft100.inp" ]; then
        ensemble="veerLeft100"
    elif [ "$i" = "FG51_SELA_maxele_veerLeft50.inp" ]; then
        ensemble="veerLeft50"
    elif [ "$i" = "FG51_SELA_maxele_veerRight100.inp" ]; then
        ensemble="veerRight100"
    elif [ "$i" = "FG51_SELA_maxele_veerRight50.inp" ]; then
        ensemble="veerRight50"
    fi

    fname="${storm}_GAHM_Adv${advisory}_${ensemble}_maxele_"
    sed -i "7s/.*/${fname}/" $i 
    
    maxfile="${storm}_${advisory}_${ensemble}.maxele.63.nc"
    sed -i "19s/.*/${maxfile}/" $i 

done

python dofiguregen.py

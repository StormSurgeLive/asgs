#!/bin/bash


storm=$(grep "storm name" run.properties)
storm=${storm/storm name : }

grid=$(grep ADCIRCgrid run.properties)
grid=${grid/ADCIRCgrid : }

forecastValidStart=$(grep forecastValidStart run.properties)
forecastValidStart=${forecastValidStart/forecastValidStart : }

coldStartTime=$(grep ColdStartTime run.properties)
coldStartTime=${coldStartTime/ColdStartTime : }

advisory=$(grep "advisory :" run.properties)
advisory=${advisory/advisory : }

oFile=cpraHydro.info

# Send to output file
echo $storm > $oFile
echo $grid >> $oFile
echo $forecastValidStart >> $oFile
echo $coldStartTime >> $oFile
echo $advisory >> $oFile

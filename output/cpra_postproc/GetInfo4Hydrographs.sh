#!/bin/bash


storm=$(grep "storm name" run.properties)
storm=${storm/storm name : }

grid=$(grep ADCIRCgrid run.properties)
grid=${grid/ADCIRCgrid : }

forecastValidStart=$(grep forecastValidStart run.properties)
forecastValidStart=${forecastValidStart/forecastValidStart : }
temp1=${forecastValidStart:0:8}
temp2=${forecastValidStart:8:4}
forecastValidStartUTC="${temp1} ${temp2} UTC"
forecastValidStartCDT=$(TZ="America/Chicago" date -d "${forecastValidStartUTC}" "+%Y%m%d%H%M%S")

coldStartTime=$(grep ColdStartTime run.properties)
coldStartTime=${coldStartTime/ColdStartTime : }
temp1=${coldStartTime:0:8}
temp2=${coldStartTime:8:4}
coldStartTimeUTC="${temp1} ${temp2} UTC"
coldStartTimeCDT=$(TZ="America/Chicago" date -d "${coldStartTimeUTC}" "+%Y%m%d%H%M%S")

advisory=$(grep "advisory :" run.properties)
advisory=${advisory/advisory : }

oFile=cpraHydro.info

# Send to output file
echo $storm > $oFile
echo $grid >> $oFile
echo $forecastValidStartCDT >> $oFile
echo $coldStartTimeCDT >> $oFile
echo $advisory >> $oFile

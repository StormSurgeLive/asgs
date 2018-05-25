#!/bin/bash

module load matlab/r2015b

# Parse run.properties to get ColdStartTime
coldStartTime=$(grep ColdStartTime run.properties)
coldStartTime=${coldStartTime/ColdStartTime : }

# Parse run.properties to get storm name
storm=$(grep "storm name" run.properties)
storm=${storm/storm name : }

# Parse run.properties to get advisory
advisory=$(grep "advisory :" run.properties)
advisory=${advisory/advisory : }

# Parse run.properties to get forecast advisory start time
forecastValidStart=$(grep forecastValidStart run.properties)
forecastValidStart=${forecastValidStart/forecastValidStart : }

# Set asgs directory
SCRIPTDIR=/work/mbilskie/NGOM_v18/2014stable   # ASGS executables
OUTPUTDIR=${SCRIPTDIR}/output/cpra_postproc # post processing scripts
TOOLSDIR=${OUTPUTDIR}/tools

# Check for all executables in asgs/output/crpa... location


# Convert fort.61.nc -> fort.61
#./cpra_postproc/tools/netcdf2adcirc.exe --datafile fort.61.nc

# Convert fort.61 -> fort.61.imeds
# Jason - Need to grab the station list used in asgs to build the fort.15
#stationList='combined_stations_20180525.txt'
stationList='cpra_stations_test.txt'
numSta=$(wc -l < $stationList)
echo $numSta | cat - $stationList > temp.txt
# Build input file for Fort61ToIMEDS
echo "fort.61" > input.temp
echo "temp.txt" >> input.temp
echo "fort.61.imeds" >> input.temp
echo $coldStartTime >> input.temp
#./cpra_postproc/tools/Fort61ToIMEDS.exe < input.temp
rm temp.txt input.temp

# Parse run.properties file
chmod u+x GetInfo4Hydrographs.sh
./GetInfo4Hydrographs.sh

# Run matlab script to create hydrographs
#matlab -nodisplay -nosplash -nodesktop -r "plot_usace_adcirc, exit"

# Runpython script to generate PPT stack
python buildPPT.py


# E-mail PPT and upload to public-facing URL
emailList='mbilsk3@lsu.edu matt.bilskie@gmail.com'
subjectLine="$storm Advisory $advisory PPT"
message="This is an automated message from the ADCIRC Surge Guidance System (ASGS).
New results are attached for STORM $storm ADVISORY $advisory issued on $forecastValidStart"

attachFile=$(cat pptFile.temp)

echo "$message" | mail -s "$subjectLine" -a "$attachFile" $emailList

# Remove temporary stuff
rm cpraHydro.info
rm pptFile.temp

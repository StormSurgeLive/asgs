#!/bin/bash

####################################################
# JASON - Can we make this seamless pending the machine that is selected when launching asgs?

# Modules for QB2
#module load matlab/r2015b
#module load python/2.7.12-anaconda-tensorflow

# Modules for hatteras
module load python_modules/2.7
module load matlab/2017b
####################################################


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

# Parse run.properties file
chmod u+x GetInfo4Hydrographs.sh
./GetInfo4Hydrographs.sh

# Run matlab script to create hydrographs
matlab -nodisplay -nosplash -nodesktop -r "plot_usace_adcirc, exit"

# Runpython script to generate PPT stack
python buildPPT.py

# E-mail PPT and upload to public-facing URL
#emailList='mbilsk3@lsu.edu matt.bilskie@gmail.com jason.fleming@seahorsecoastal.com ckaiser@cct.lsu.edu'
emailList='mbilsk3@lsu.edu'
subjectLine="$storm Advisory $advisory PPT"
message="This is an automated message from the ADCIRC Surge Guidance System (ASGS).
New results are attached for STORM $storm ADVISORY $advisory issued on $forecastValidStart"

attachFile=$(cat pptFile.temp)

#echo "$message" | mail -s "$subjectLine" -a "$attachFile" $emailList

# Remove temporary stuff
rm cpraHydro.info
rm pptFile.temp

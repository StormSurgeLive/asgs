#!/bin/bash

####################################################
# JASON - Can we make this seamless pending the machine that is selected when launching asgs?

# Modules for QB2
#module load matlab/r2015b
#module load python/2.7.12-anaconda-tensorflow

# Modules for hatteras
#module load python_modules/2.7
#module load matlab/2017b
####################################################

# Grab command line arguments
POSITIONAL=()
while [[ $# -gt 0 ]]
do
    key="$1"

    case $key in
        -i)
            runDir="$2"
            shift # past argument
            shift # past value
            ;;
    esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters
export MATLABPATH=${runDir}

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

# Parse run.properties file
chmod u+x GetInfo4Hydrographs.sh
${runDir}/GetInfo4Hydrographs.sh

# Run matlab script to create hydrographs
matlab -nodisplay -nosplash -nodesktop -r "run plot_usace_adcirc.m, exit"

# Runpython script to generate PPT stack
python ${runDir}/buildPPT.py

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

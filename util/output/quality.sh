#!/bin/bash
#----------------------------------------------------------------
# quality.sh: Checks model results for various issues.
#----------------------------------------------------------------
# Copyright(C) 2024--2025 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------
#
# command line arguments
THIS=$(basename -- $0)
# Count command line arguments; use them if provided or use
# run.properties if not.
declare -A properties
dir=$PWD
RUNPROPERTIES=$dir/run.properties
# this script can be called with just one command line option: the
# full path to the run.properties file
if [[ $# -eq 1 ]]; then
   RUNPROPERTIES="$1"
fi
if [[ ! -e "$RUNPROPERTIES" ]]; then
    echo "[$(date +'%Y-%h-%d-T%H:%M:%S%z')] ERROR: quality.sh: The run properties file '$RUNPROPERTIES' was not found."
    exit 1
fi
#
# get loadProperties function -- try to use the SCRIPTDIR from run.properties
# unless it does not exist ... fall back to the SCRIPTDIR used by this
# ASGS instance
runPropScriptDir=$(sed -n 's/[ ^]*$//;s/path.scriptdir\s*:\s*//p' $RUNPROPERTIES)
if [[ -d $runPropScriptDir ]]; then
    SCRIPTDIR=$runPropScriptDir
fi
#
source $SCRIPTDIR/properties.sh
# load run.properties file into associative array
loadProperties $RUNPROPERTIES
#
runPropWaves=${properties['coupling.waves']}
jobID="null"
if [[ -e jobID ]]; then
    jobID=$(<jobID)    # slurm job number, bash process ID, etc, stored in jobID file
fi
CYCLE=${properties['advisory']}     # e.g., 2025080600 for NAM or GFS, two digit advisory number for TCs
SCENARIO=${properties['scenario']}  # namforecast|veerRight100|etc
QUALITYCONTROL=${properties['post.qualitycontrol.script']}  # off|$SCRIPTDIR/util/output/quality.sh
QUALITYSETTING=${properties['post.qualitycontrol.setting']}  # strict|allow-nonfatal-instability
#
THIS=quality.sh             # name of this script for use in log messages
#
ERROVALUE=0
ERROMSG=""
declare -A netcdfVarName
netcdfVarName["maxele.63.nc"]="zeta_max"
netcdfVarName["fort.63.nc"]="zeta"
netcdfVarName["swan_HS_max.63.nc"]="swan_HS_max"
netcdfVarName["swan_HS.63.nc"]="swan_HS"
netcdfVarName["swan_TPS_max.63.nc"]="swan_TPS_max"
netcdfVarName["swan_TPS.63.nc"]="swan_TPS"
netcdfVarName["swan_DIR_max.63.nc"]="swan_DIR_max"
netcdfVarName["swan_DIR.63.nc"]="swan_DIR"
netcdfVarName["maxwvel.63.nc"]="wind_max"
netcdfVarName["fort.74.nc"]="windx"
declare -A filesNumDataSets    # number of datasets in each file
#
# look for numerical instability errors in the stdout/stderr files
for file in adcirc.log scenario.log padcirc.out padcswan.out ; do
jobtype="null"
if [ -e $file ]; then
    if [[ $file == "padcswan.out" || $file == "padcirc.out" ]]; then
        jobtype=${file%.*}
    fi
    numMsg=$(grep WarnElev $file | wc -l)
    if [ $numMsg -eq 0 ]; then
        echo "cycle ${CYCLE}: ${SCENARIO}: No numerical instability detected in '$file' after completion of job '$jobID'." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
    else
        if [[ $QUALITYSETTING == "allow-nonfatal-instability" ]]; then
            echo "cycle ${CYCLE}: ${SCENARIO}: WARNING: Detected '$numMsg' numerical instability messages in '$file' after completion of job '$jobID'. QUALITYSETTING is set to '$QUALITYSETTING' so this will only result in a warning."
            # write this result to run.properties for downstream notification
            echo "post.qualitycontrol.warnelev.nummessages : $numMsg" >> $RUNPROPERTIES
        else
            ERROVALUE=1
            ERROMSG="$ERROMSG Detected '$numMsg' numerical instability messages in '$file' after completion of job '$jobID'. "
        fi
    fi
fi
done
# look for numerical results that indicate an error occurred
#
# create a list of files that will be quality checked
fileList=( )
if [[ $SCENARIO != *"Wind10m" ]]; then
    # any scenario that is not purely meteorological will have
    # data in the following files
    fileList=( maxele.63.nc fort.63.nc )
    # if it is not a met-only scenario or tidal initialization
    # then also check the swan output files for issues
    if [[ $runPropWaves == "on" && $SCENARIO != "hindcast" ]]; then
        fileList+=( swan_HS_max.63.nc swan_HS.63.nc swan_TPS.63.nc swan_TPS_max.63.nc swan_DIR.63.nc swan_DIR_max.63.nc )
    fi
fi
# any scenario other than tidal initialization will have
# meteorological output
if [[ $SCENARIO != "hindcast" ]]; then
    fileList+=( maxwvel.63.nc fort.74.nc )
fi
#
# compile statistics for each file
filesFoundList=( )
for file in ${fileList[@]}; do
    # check that the file exists
    if [[ ! -e $file ]]; then
        filesNumDataSets[$file]=0
        ERROVALUE=1
        ERROMSG="$ERROMSG The '$file' file does not exist, indicating that the '$jobtype.$layer' job with ID '$jobID' did not finish successfully. "
        continue
    fi
    filesFoundList+=( $file )
    # count the number of nodes in the mesh
    np=$(ncks --trd -m -M $file | grep -E -i ": node, size =" | cut -f 7 -d ' ' | tr -d "," | uniq)
    # find number of datasets in the file
    filesNumDataSets[$file]=$(ncks --trd -M $file | grep -E -i "^Root record dimension 0:" | cut -f 10- -d ' ')
    # check for zero records in the file
    if [[ ${filesNumDataSets[$file]} -eq 0 ]]; then
        ERROVALUE=1
        ERROMSG="$ERROMSG The '$file' file contains no data, indicating that the '$jobtype.$layer' job with ID '$jobID' did not finish successfully. "
        continue
    fi
    echo "cycle $CYCLE: $SCENARIO: job ID '$jobID' output file '$file' contains '${filesNumDataSets[$file]}' data set(s). Computing statistics." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
    if [[ -e statistics_${file}.txt ]]; then
        rm statistics_${file}.txt
    fi
    smokeTest.x --datafile $file --varname ${netcdfVarName["$file"]} > statistics_${file}.txt 2>> smokeTest_${file}.log
done
echo "cycle $CYCLE: $SCENARIO: Finished computing statistics for job ID '$jobID' output files." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
# perform quality checks
echo "cycle $CYCLE: $SCENARIO: Checking quality of results for job ID '$jobID' output files." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
for file in ${filesFoundList[@]}; do
    numMissing=0
    numZero=0
    if [[ ${filesNumDataSets[$file]} -eq 0 ]]; then
        continue   # skip checks on files now known to be empty
    fi
    # the missing and zero quality checks are slightly different for the different file types
    case $file in
    "maxwvel.63.nc"|"fort.74.nc")
        # for wind output, check for any missing values (should not occur)
        numMissing=$(awk '$1==-99999 || $2==-99999 || $3==-99999 || $4==-99999 || $5>0 { print $0 }' statistics_${file}.txt | wc -l)
        # also check for max, avg, or stdev are zero (min can be zero)
        numZero=$(awk '$2==0.0 || $3==0.0 || $4==0.0 { print $0 }' statistics_${file}.txt | wc -l)
        ;;
    "maxele.63.nc"|"fort.63.nc")
        # count the number of nodes that would be considered dry upon cold start based only on
        # negative topobathy value
        numLand=$(smokeTest.x --datafile $file --count-negative-topo 2>> smokeTest_${file}.log)
        # water level results can contain missing (dry) values, but need to check
        # to see if any of the min, max, avg, or stdev are set to the missing value or zero
        numMissing=$(awk '$1==-99999 || $2==-99999 || $3==-99999 || $4==-99999  { print $0 }' statistics_${file}.txt | wc -l)
        # also check to see if the number of missing values is greater than the number of coldstart dry values
        compareDry=$(awk -v nl=$numLand '$5>nl { print $0 }' statistics_${file}.txt | wc -l)
        # just a warning
        if [[ $compareDry -gt 0 ]]; then
            echo "cycle $CYCLE: $SCENARIO: QUALITY CHECK WARNING: There are more dry values in '$compareDry' datasets in the '$file' file than the total number of negative topobathy depths ('$numLand') in the mesh for job ID '$jobID'." 2>&1 | awk -v level=WARN -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
        fi
        # also check for max, avg, or stdev set to zero (ok for min to be zero, although presumably unusual)
        numZero=$(awk '$2==0.0 || $3==0.0 || $4==0.0 { print $0 }' statistics_${file}.txt | wc -l)
        ;;
    "swan_HS_max.63.nc"|"swan_HS.63.nc"|"swan_TPS_max.63.nc"|"swan_TPS.63.nc"|"swan_DIR_max.63.nc"|"swan_DIR.63.nc")
        # swan results can have zero or the missing value in summary statistics if it was cold
        # started, so only a warning (not an error) will be issued (min can be zero in any case)
        numMissing=$(awk '$1==-99999 || $2==-99999 || $3==-99999 || $4==-99999 { print $0 }' statistics_${file}.txt | wc -l)
        # issue a warning if these are found
        if [[ $numMissing -gt 0 ]]; then
            echo "cycle $CYCLE: $SCENARIO: There were '$numMissing' missing values in the statistics for file '$file' for job ID '$jobID'. This may be ok if SWAN is cold starting." 2>&1 | awk -v level=WARNING -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
        fi
        # also check for max, avg, or stdev are zero (min can be zero)
        numZero=$(awk '$2==0.0 || $3==0.0 || $4==0.0 { print $0 }' statistics_${file}.txt | wc -l)
        if [[ $numZero -gt 0 ]]; then
            echo "cycle $CYCLE: $SCENARIO: There were '$numZero' zero values in the statistics for file '$file' for job ID '$jobID'. This may be ok if SWAN is cold starting." 2>&1 | awk -v level=WARNING -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
        fi
        # SWAN results should skip the checks and associated error messages below
        if [[ $numMissing -gt 0 || $numZero -gt 0 ]]; then
            continue
        fi
        ;;
    *)
        # should be unreachable
        ;;
    esac
    if [[ $numMissing -eq 0 ]]; then
        echo "cycle $CYCLE: $SCENARIO: PASSED QUALITY CHECK: Missing values (-99999) OK in output file '$file' for job ID '$jobID'." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
    else
        echo "cycle $CYCLE: $SCENARIO: FAILED QUALITY CHECK: Missing values (-99999) NOT OK in '$numMissing' dataset(s) of output file '$file'. Failure for job ID '$jobID'." 2>&1 | awk -v level=ERROR -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
        ERROVALUE=1
        ERROMSG="$ERROMSG FAILED QUALITY CHECK: Missing values (-99999) NOT OK in output file '$file'. Failure for job ID '$jobID'. "
    fi
    if [[ $numZero -eq 0 ]]; then
        echo "cycle $CYCLE: $SCENARIO: PASSED QUALITY CHECK: No zero values in summary statistics in output file '$file' for job ID '$jobID'." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
    else
        echo "cycle $CYCLE: $SCENARIO: FAILED QUALITY CHECK: Found '$numZero' datasets with zero values for summary statistics of output file '$file', which should not occur. Failure for job ID '$jobID'." 2>&1 | awk -v level=ERROR -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
        ERROVALUE=1
        ERROMSG="$ERROMSG FAILED QUALITY CHECK: Found '$numZero' datasets with zero values for summary statistics of output file '$file', which should not occur. Failure for job ID '$jobID'."
    fi
done
if [[ $ERROVALUE -ne 0 ]]; then
    echo "cycle $CYCLE: $SCENARIO: Error summary: $ERROMSG" 2>&1 | awk -v this=$THIS -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk
    echo $ERROMSG > jobFailed
fi
exit $ERROVALUE

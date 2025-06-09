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
jobtype=$1    # padcirc|padcswan
layer=$2      # nhcConsensus|nhcConsensusWind10m|etc
jobID=$3      # slurm job number, bash process ID, etc, stored in jobID file
CYCLE=$4      # e.g., 2025080600 for NAM or GFS, two digit advisory number for TCs
SCENARIO=$5   # namforecast|veerRight100|etc
SCRIPTDIR=$6  # ASGS installation location
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
netcdfVarName["maxwvel.63.nc"]="wind_max"
netcdfVarName["fort.74.nc"]="windx"
declare -A filesNumDataSets    # number of datasets in each file
#
# look for numerical instability errors in the stdout/stderr files
for file in adcirc.log scenario.log ${jobtype}.out ; do
if [ -e $file ]; then
    numMsg=$(grep WarnElev $file | wc -l)
    if [ $numMsg -eq 0 ]; then
        echo "cycle ${CYCLE}: ${SCENARIO}: No numerical instability detected in '$file' after completion of '$jobtype.$layer' job '$jobID'." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
    else
        ERROVALUE=1
        ERROMSG="$ERROMSG Detected '$numMsg' numerical instability messages in '$file' after completion of '$jobtype.$layer' job '$jobID'. "
    fi
fi
done
# look for numerical results that indicate an error occurred
# create a list of files that will be quality checked
fileList=( )
if [[ $layer == $SCENARIO && $SCENARIO != *"Wind10m" ]]; then
    fileList=( maxele.63.nc fort.63.nc )
    if [[ $jobtype == "adcswan" || $jobtype == "padcswan" ]]; then
        fileList+=( swan_HS_max.63.nc swan_HS.63.nc )
    fi
    fi
if [[ -e maxwvel.63.nc ]]; then
    fileList+=( maxwvel.63.nc fort.74.nc )
fi
# compile statistics for each file
for file in ${fileList[@]}; do
    # check that the file exists
    if [[ ! -e $file ]]; then
        filesNumDataSets[$file]=0
        ERROVALUE=1
        ERROMSG="$ERROMSG The '$file' file does not exist, indicating that the '$jobtype.$layer' job with ID '$jobID' did not finish successfully. "
        continue
    fi
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
    echo "cycle $CYCLE: $SCENARIO: '$jobtype.$layer' job ID '$jobID' output file '$file' contains '${filesNumDataSets[$file]}' data set(s). Computing statistics." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
    if [[ -e statistics_${file}.txt ]]; then
        rm statistics_${file}.txt
    fi
    # compute min, max, avg, stdev, and number of missing (i.e., dry) values
    # over the full domain for each time step and write to file
    #
    # !FIXME! TODO: These must be replaced with something other than ncap2
    # which has unsatisfiable dependencies ... commented out for now
    ################################################################
    #for snap in $(seq 0 $((${filesNumDataSets[$file]} - 1))) ; do
    #    if [[ $file == "maxele.63.nc" || $file == "maxwvel.63.nc" || $file == "swan_HS_max.63.nc" ]]; then
    #        ncap2 -O -v -s "data[node]=0.0;data.set_miss(-99999.0);data=${netcdfVarName[$file]}(0:$(($np - 1)):1);print(data.min(),\"%f \");print(data.max(),\"%f \");print(data.avg(),\"%f \");print((data-data.avg()).rmssdn(),\"%f \");print(data.number_miss(),\"%d\n\");" $file statistics.nc >> statistics_${file}.txt
    #    else
    #        ncap2 -O -v -s "data[node]=0.0;data.set_miss(-99999.0);data=${netcdfVarName[$file]}($snap:$snap:1,0:$(($np - 1)):1);print(data.min(),\"%f \");print(data.max(),\"%f \");print(data.avg(),\"%f \");print((data-data.avg()).rmssdn(),\"%f \");print(data.number_miss(),\"%d\n\");" $file statistics.nc >> statistics_${file}.txt
    #    fi
    #done
    ################################################################
done
echo "cycle $CYCLE: $SCENARIO: Finished computing statistics for '$jobtype.$layer' job ID '$jobID' output files." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
# perform quality checks
echo "cycle $CYCLE: $SCENARIO: Checking quality of results for '$jobtype.$layer' job ID '$jobID' output files." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
for file in ${fileList[@]}; do
    if [[ ${filesNumDataSets[$file]} -eq 0 ]]; then
        continue   # skip checks on files known to be empty
    fi
    # check for NaN values anywhere in the dataset
    nanFound=$(ncks --chk_nan $file)
    if [[ $nanFound == "" ]]; then
        echo "cycle $CYCLE: $SCENARIO: QUALITY CHECK PASSED: No NaN (not a number) values in output file '$file' for '$jobtype.$layer' job ID '$jobID'." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
    else
        echo "cycle $CYCLE: $SCENARIO: QUALITY CHECK FAILED: Found NaN (not a number) value(s) in output file '$file'. Failure for '$jobtype.$layer' job ID '$jobID'." 2>&1 | awk -v level=ERROR -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
        echo $nanFound > "nanFound_$file"
        ERROVALUE=1
        ERROMSG="$ERROMSG QUALITY CHECK FAILED: Found NaN (not a number) value(s) in output file '$file'. Failure for '$jobtype.$layer' job ID '${%JOBID%}'. "
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
        # count the number of nodes that would be considered dry upon cold start based only on negative topobathy value
        #
        #
        # !FIXME! TODO: These must be replaced with something other than ncap2
        # which has unsatisfiable dependencies ... commented out for now
        ################################################################
        #numLand=$(ncap2 -O -v -s "land_msk[node]=0.0;land_msk.set_miss(-99999.0);where(depth < 0.0) land_msk=-99999.0; elsewhere land_msk=depth; print(land_msk.number_miss(),\"%d\n\");" $file statistics.nc)
        ################################################################
        # water level results can contain missing (dry) values, but need to check
        # to see if any of the min, max, avg, or stdev are set to the missing value or zero
        numMissing=$(awk '$1==-99999 || $2==-99999 || $3==-99999 || $4==-99999  { print $0 }' statistics_${file}.txt | wc -l)
        # also check to see if the number of missing values is greater than the number of coldstart dry values
        compareDry=$(awk '$5>$numLand { print $0 }' statistics_${file}.txt | wc -l)
        # just a warning
        if [[ $compareDry -gt 0 ]]; then
            echo "cycle $CYCLE: $SCENARIO: QUALITY CHECK WARNING: There are more dry values in '$compareDry' datasets in the '$file' file than the total number of negative topobathy depths ('$numLand') in the mesh for '$jobtype.$layer' job ID '$jobID'." 2>&1 | awk -v level=WARN -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
        fi
        # also check for min, max, avg, or stdev set to zero
        numZero=$(awk '$1==0.0 || $2==0.0 || $3==0.0 || $4==0.0 { print $0 }' statistics_${file}.txt | wc -l)
        ;;
    "swan_HS_max.63.nc"|"swan_HS.63.nc")
        # wave height results should not have zero or the missing value in summary statistics (min can be zero)
        numMissing=$(awk '$1==-99999 || $2==-99999 || $3==-99999 || $4==-99999 { print $0 }' statistics_${file}.txt | wc -l)
        # also check for max, avg, or stdev are zero (min can be zero)
        numZero=$(awk '$2==0.0 || $3==0.0 || $4==0.0 { print $0 }' statistics_${file}.txt | wc -l)
        ;;
    *)
        # should be unreachable
        ;;
    esac
    if [[ $numMissing -eq 0 ]]; then
        echo "cycle $CYCLE: $SCENARIO: QUALITY CHECK PASSED: Missing values (-99999) OK in output file '$file' for '$jobtype.$layer' job ID '$jobID'." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
    else
        echo "cycle $CYCLE: $SCENARIO: QUALITY CHECK FAILED: Missing values (-99999) NOT OK in '$numMissing' dataset(s) of output file '$file'. Failure for '$jobtype.$layer' job ID '$jobID'." 2>&1 | awk -v level=ERROR -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
        ERROVALUE=1
        ERROMSG="$ERROMSG QUALITY CHECK FAILED: Missing values (-99999) NOT OK in output file '$file'. Failure for '$jobtype.$layer' job ID '$jobID'. "
    fi
    if [[ $numZero -eq 0 ]]; then
        echo "cycle $CYCLE: $SCENARIO: QUALITY CHECK PASSED: No zero values in summary statistics in output file '$file' for '$jobtype.$layer' job ID '$jobID'." 2>&1 | awk -v level=INFO -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
    else
        echo "cycle $CYCLE: $SCENARIO: QUALITY CHECK FAILED: Found '$numZero' datasets with zero values for summary statistics of output file '$file', which should not occur. Failure for '$jobtype.$layer' job ID '$jobID'." 2>&1 | awk -v level=ERROR -v this=$THIS -f $SCRIPTDIR/monitoring/timestamp.awk
        ERROVALUE=1
        ERROMSG="$ERROMSG QUALITY CHECK FAILED: Found '$numZero' datasets with zero values for summary statistics of output file '$file', which should not occur. Failure for '$jobtype.$layer' job ID '$jobID'."
    fi
done
if [[ $ERROVALUE -ne 0 ]]; then
    echo "cycle $CYCLE: $SCENARIO: $THIS: $ERROMSG" 2>&1 | awk -v this=$THIS -v level=ERROR -f $SCRIPTDIR/monitoring/timestamp.awk
    echo $ERROMSG > jobFailed
fi
exit $ERROVALUE
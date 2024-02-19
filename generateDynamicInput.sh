#!/bin/bash
#----------------------------------------------------------------
# generateDynamicInput.sh: subroutine that generates dynamic
# input files like tide_fac.out, fort.13, fort.15, and fort.26.
#----------------------------------------------------------------
# Copyright(C) 2024 Jason Fleming
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
generateDynamicInput()
{
    #
    local THIS="asgs_main.sh>generateDynamicInput.sh"
    logMessage "$SCENARIO: $THIS: Generating tide_fac.out, fort.13, and fort.15."

    # tidal forcing
    tidefac_file="$SCENARIODIR/tide_fac.out"
    tidefac_cmd="tide_fac.x --length $runLength --year ${CSDATE:0:4} --month ${CSDATE:4:2} --day ${CSDATE:6:2} --hour ${CSDATE:8:2} -n 8 m2 s2 n2 k1 k2 o1 q1 p1 --outputformat simple --outputdir $SCENARIODIR 2>> $SYSLOG"
    tidal_potential_comment="Tide nodal factors and equilibrium arguments generated with the command '$tidefac_cmd'."
    tidal_boundary_comment=$tidal_potential_comment
    if [[ $TIDEFAC == "on" ]]; then
        tideFacMessage=$($tidefac_cmd)
        if [[ $tide_fac_message =~ /ERROR|WARNING/ ]]; then
            error "There was an issue when running tide_fac.x: '$tideFacMessage'."
        else
            logMessage "Tide nodal factors and equilibrium arguments were written to the file '$SCENARIODIR/tide_fac.out'."
            scenarioMessage "$ENSTORM: $THIS: '$SCENARIODIR/tide_fac.out' is as follows:"
            cat $SCENARIODIR/tide_fac.out >> $SCENARIOLOG
        fi
    else
        tidefac_file="notset"
    fi
    # nodal attributes
    na_defaults="\n"
    for k in ${!nodal_attribute_default_values[@]}; do
        na_defaults="$na_defaults      $k: \"${nodal_attribute_default_values[$k]}\"\n"
    done
    na_activate_list=""
    for k in ${nodal_attribute_activate[@]}; do
        na_activate_list="$na_activate_list\n      - \"$k\""
    done
    sed \
    -e "s/%ADCIRCVER%/$(adcirc -v)/" \
    -e "s/%IM_ETC%/$solver_time_integration/" \
    -e "s/%HINDCASTLENGTH%/$HINDCASTLENGTH/" \
    -e "s/%A00B00C00%/$time_weighting_coefficients/" \
    -e "s/%NWSET%/${owiWinPre["NWSET"]}/" \
    -e "s/%NWBS%/${owiWinPre["NWBS"]}/" \
    -e "s/%DWM%/${owiWinPre["DWM"]}/" \
    -e "s/%startdatetime%/${owiWinPre["startDateTime"]}/" \
    -e "s/%enddatetime%/${owiWinPre["endDateTime"]}/" \
    -e "s/%lateral_turbulence%/$lateral_turbulence/" \
    -e "s/%ESLM%/$eddy_viscosity_coefficient/" \
    -e "s/%ESLM_Smagorinsky%/$smagorinsky_coefficient/" \
    -e "s/%tidal_forcing%/$TIDEFAC/" \
    -e "s?%tidefac_file%?$tidefac_file?" \
    -e "s?%tidal_potential_comment%?$tidal_potential_comment?" \
    -e "s?%tidal_boundary_comment%?$tidal_boundary_comment?" \
    -e "s/%NFOVER%/$nfover/" \
    -e "s/%NABOUT%/$log_level/" \
    -e "s/%H0%/$h0/" \
    -e "s/%VELMIN%/$velmin/" \
    -e "s/%FFACTOR%/$bottom_friction_limit/" \
    -e "s/%advection%/$advection/" \
    -e "s/%WTIMINC%/$WTIMINC/" \
    -e "s/%storm_name%/$storm_name/" \
    -e "s?%NCPROJ%?${netcdf_metadata["NCPROJ"]}?" \
    -e "s?%NCINST%?${netcdf_metadata["NCINST"]}?" \
    -e "s?%NCSOUR%?${netcdf_metadata["NCSOUR"]}?" \
    -e "s?%NCHIST%?${netcdf_metadata["NCHIST"]}?" \
    -e "s?%NCREF%?${netcdf_metadata["NCREF"]}?" \
    -e "s?%NCCOM%?${netcdf_metadata["NCCOM"]}?" \
    -e "s?%NCHOST%?${netcdf_metadata["NCHOST"]}?" \
    -e "s?%NCCONV%?${netcdf_metadata["NCCONV"]}?" \
    -e "s?%NCCONT%?${netcdf_metadata["NCCONT"]}?" \
    -e "s?%NCDATE%?${netcdf_metadata["NCDATE"]}?" \
    -e "s/%DragLawString%/${metControl["DragLawString"]}/" \
    -e "s/%WindDragLimit%/${metControl["WindDragLimit"]}/" \
    -e "s/%outputWindDrag%/${metControl["outputWindDrag"]}/" \
    -e "s/%outputNodeCode%/${wetDryControl["outputNodeCode"]}/" \
    -e "s/%outputNOFF%/${wetDryControl["outputNOFF"]}/" \
    -e "s/%noffActive%/${wetDryControl["noffActive"]}/" \
    -e "s/%inundationOutput%/${inundationOutputControl["inundationOutput"]}/" \
    -e "s/%inunThresh%/${inundationOutputControl["inunThresh"]}/" \
    -e "s/%WAVES%/$WAVES/" \
    -e "s/%wave_model%/$wave_model/" \
    -e "s/%RSTIMINC%/$SWANDT/" \
    -e "s/%MXITNS%/${swan["MXITNS"]}/" \
    -e "s/%NPNTS%/${swan["NPNTS"]}/" \
    -e "s?%nodal_attributes_template_file%?$INPUTDIR/$NAFILE?" \
    -e "s/%nodal_attribute_activate_list%/$na_activate_list/" \
    -e "s/%nodal_attribute_default_values_hash%/$na_defaults/" \
        < $controlParametersTemplate \
        > $SCENARIODIR/control_parameters.yaml
    if [[ $? != 0 ]]; then
        echo "$THIS: Failed to fill in control parameters template with sed."
    fi
    #BOB
    controlFile="$SCENARIODIR/fort.15"
    swanFile="$SCENARIODIR/fort.26"
    perl $SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS < $SCENARIODIR/control_parameters.yaml > $controlFile 2>> ${SYSLOG}
    controlExitStatus=$?
    if [[ $controlExitStatus != 0 ]]; then
        controlMsg="The control_file_gen.pl script failed with the following error code: '$controlExitStatus'."
    fi
    if [[ ! -e $controlFile || ! -s $controlFile ]]; then
        controlExitStatus=1
        controlMsg="$controlMsg Failed to generate the ADCIRC '$controlFile' file."
    fi
    if [[ $WAVES == "on" && $NWS != "0" ]]; then
        if [[ ! -e $swanFile || ! -s $swanFile ]]; then
            controlExitStatus=1
            controlMsg="$controlMsg Failed to generate the SWAN '$swanFile' file."
        fi
    fi
    if [[ $controlExitStatus -ne 0 ]]; then
        warn "$THIS: $SCENARIO: $controlMsg The $SCENARIO run will be abandoned."
        echo "$THIS: $SCENARIO: $controlMsg The $SCENARIO run will be abandoned." >> jobFailed
    fi
    #BOB
    cat $SCENARIODIR/run-control.properties >> $SCENARIODIR/run.properties 2>> $SYSLOG
}




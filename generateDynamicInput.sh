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
    #
    # tidal forcing
    tidefac_file="$SCENARIODIR/tide_fac.out"
    tidefac_cmd="tide_fac.x --length $runLength --year ${CSDATE:0:4} --month ${CSDATE:4:2} --day ${CSDATE:6:2} --hour ${CSDATE:8:2} -n ${#tidalConstituents[@]} ${tidalConstituents[@]} --outputformat simple --outputdir $SCENARIODIR 2>> $SYSLOG"
    tidal_potential_comment="Tide nodal factors and equilibrium arguments generated with the command '$tidefac_cmd'."
    tidal_boundary_comment=$tidal_potential_comment
    if [[ $TIDEFAC == "on" ]]; then
        tideFacMessage=$($tidefac_cmd)
        if [[ $tide_fac_message =~ /ERROR|WARNING/ ]]; then
            error "There was an issue when running tide_fac.x: '$tideFacMessage'."
        else
            logMessage "Tide nodal factors and equilibrium arguments were written to the file '$SCENARIODIR/tide_fac.out'."
            scenarioMessage "$SCENARIO: $THIS: '$SCENARIODIR/tide_fac.out' is as follows:"
            cat $SCENARIODIR/tide_fac.out >> $SCENARIOLOG
        fi
    else
        tidefac_file="notset"
    fi
    #
    # flux boundary forcing
    periodicity="null"
    if [[ $VARFLUX == "on" || $VARFLUX == "default" ]]; then
        periodicity="aperiodic"
    fi
    if [[ $PERIODICFLUX != "null" ]]; then
        periodicity="periodic"
    fi
    #
    # nodal attribute default values to be written to nodal attributes (fort.13) file
    na_defaults="\n"
    for k in ${!nodal_attribute_default_values[@]}; do
        na_defaults="$na_defaults      $k: \"${nodal_attribute_default_values[$k]}\"\n"
    done
    #
    # set up options for fort.15 file(s) based on the layer being generated
    declare -a layers
    local na_activate_list
    layers=( $SCENARIO )
    if [[               $createWind10mLayer == "yes"       && \
                                       $NWS != "0"         && \
                                  $SCENARIO != *"Wind10m"  && \
           ${#nodal_attribute_activate[@]} -ne 0           && \
                                    $NAFILE != *"null"     && \
                                    $NAFILE != *"notset"   ]]; then
        layers+=( "wind10m" )
    fi
    for layer in ${layers[@]}; do
        local layerWaves="$WAVES"
        na_activate_list=""  # clear out list of nodal attributes that will be activated for this layer
        layerOptions="--controltemplate ${INPUTDIR}/${CONTROLTEMPLATE}"
        if [[ $layer == $SCENARIO && $SCENARIO != *"Wind10m" ]]; then
            outputInventory="full"
            layerOptions+=" --nws $NWS --dt $TIMESTEPSIZE $OUTPUTOPTIONS"
            for k in ${nodal_attribute_activate[@]}; do
                na_activate_list="$na_activate_list\n      - \"$k\""
            done
        fi
        logMessage "layer is '$layer' SCENARIO is '$SCENARIO'" # jgfdebug
        if [[ $layer == "wind10m" || ( $layer == $SCENARIO && $SCENARIO == *"Wind10m" ) ]]; then
            logMessage "this is a Wind10m scenario"            # jgfdebug
            if [[ $layer == $SCENARIO && $SCENARIO == *"Wind10m" && $CONTROLTEMPLATENOROUGH != "null" ]]; then
                layerOptions="--controltemplate ${INPUTDIR}/${CONTROLTEMPLATENOROUGH}"
            fi
            outputInventory="metonly"
            metonlyNWS=$BASENWS
            if [[ $BACKGROUNDMET == *"Blend" ]]; then
                metonlyNWS=$(($BASENWS + 10))  # e.g., 20 becomes 30
            fi
            layerOptions+=" --nws $metonlyNWS"
            layerOptions+=" --dt 300.0"      # 5 minute time steps
            layerOptions+=" --fort61freq 0 --fort62freq 0 --fort63freq 0 --fort64freq 0"
            layerOptions+=" --fort7172freq 300.0 --fort7172netcdf"
            layerOptions+=" --fort7374freq 3600.0 --fort7374netcdf"
            if [[ $OUTPUTOPTIONS =~ "--netcdf4" ]]; then
                layerOptions+=" --netcdf4"
            fi
            for k in ${nodal_attribute_activate[@]}; do
                if [[ $k == "surface_directional_effective_roughness_length" || $k == "surface_canopy_coefficient" || $k == "elemental_slope_limiter" ]]; then
                    continue  # deactivate nodal attributes that reduce wind to ground level (or update ESLNodes.63)
                fi
                na_activate_list="$na_activate_list\n      - \"$k\""
            done
            layerWaves="off"
        fi
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
        -e "s/%metresults%/$WTIMINC/" \
        -e "s/%storm_name%/$storm_name/" \
        -e "s/%periodicity%/$periodicity/" \
        -e "s?%periodic_flux_file%?$PERIODICFLUX?" \
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
        -e "s/%invertedBarometerOnElevationBoundary%/${metControl["invertedBarometerOnElevationBoundary"]}/" \
        -e "s/%outputNodeCode%/${wetDryControl["outputNodeCode"]}/" \
        -e "s/%outputNOFF%/${wetDryControl["outputNOFF"]}/" \
        -e "s/%noffActive%/${wetDryControl["noffActive"]}/" \
        -e "s/%slim%/${wetDryControl["slim"]}/" \
        -e "s/%windlim%/${wetDryControl["windlim"]}/" \
        -e "s/%directvelWD%/${wetDryControl["directvelWD"]}/" \
        -e "s/%useHF%/${wetDryControl["useHF"]}/" \
        -e "s/%inundationOutput%/${inundationOutputControl["inundationOutput"]}/" \
        -e "s/%inunThresh%/${inundationOutputControl["inunThresh"]}/" \
        -e "s/%WAVES%/$layerWaves/" \
        -e "s/%wave_model%/$wave_model/" \
        -e "s/%RSTIMINC%/$SWANDT/" \
        -e "s/%SWAN_OutputTPS%/${SWANOutputControl["SWAN_OutputTPS"]}/" \
        -e "s/%SWAN_OutputTM01%/${SWANOutputControl["SWAN_OutputTM01"]}/" \
        -e "s/%SWAN_OutputHS%/${SWANOutputControl["SWAN_OutputHS"]}/" \
        -e "s/%SWAN_OutputDIR%/${SWANOutputControl["SWAN_OutputDIR"]}/" \
        -e "s/%SWAN_OutputTMM10%/${SWANOutputControl["SWAN_OutputTMM10"]}/" \
        -e "s/%SWAN_OutputTM02%/${SWANOutputControl["SWAN_OutputTM02"]}/" \
        -e "s/%MXITNS%/${swan["MXITNS"]}/" \
        -e "s/%NPNTS%/${swan["NPNTS"]}/" \
        -e "s?%nodal_attributes_template_file%?$INPUTDIR/$NAFILE?" \
        -e "s/%nodal_attribute_activate_list%/$na_activate_list/" \
        -e "s/%nodal_attribute_default_values_hash%/$na_defaults/" \
        -e "s/%inventory%/$outputInventory/" \
            < $controlParametersTemplate \
            > $SCENARIODIR/${layer}.control_parameters.yaml
        if [[ $? != 0 ]]; then
            echo "$THIS: Failed to fill in control parameters template with sed."
        fi
        #
        controlFile="$SCENARIODIR/${layer}.fort.15"
        swanFile="$SCENARIODIR/fort.26"
        logMessage "$SCENARIO: $THIS: Generating ADCIRC Control File (${layer}.fort.15) for $SCENARIO with the following options: $CONTROLOPTIONS $layerOptions."
        perl $SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS $layerOptions < $SCENARIODIR/${layer}.control_parameters.yaml > $controlFile 2>> ${SYSLOG}
        controlExitStatus=$?
        if [[ $controlExitStatus != 0 ]]; then
            controlMsg="The control_file_gen.pl script failed with the following error code: '$controlExitStatus'."
        fi
        if [[ ! -e $controlFile || ! -s $controlFile ]]; then
            controlExitStatus=1
            controlMsg="$controlMsg Failed to generate the ADCIRC '$controlFile' file."
        fi
        if [[ $layer == $SCENARIO && $layerWaves == "on" && $NWS != "0" && $SCENARIO != *"Wind10m" ]]; then
            if [[ ! -e $swanFile || ! -s $swanFile ]]; then
                controlExitStatus=1
                controlMsg="$controlMsg Failed to generate the SWAN '$swanFile' file."
            fi
        fi
        if [[ $controlExitStatus -ne 0 ]]; then
            warn "$THIS: $SCENARIO: $controlMsg The $SCENARIO run will be abandoned."
            echo "$THIS: $SCENARIO: $controlMsg The $SCENARIO run will be abandoned." >> jobFailed
        fi
        #
        mv $SCENARIODIR/run-control.properties $SCENARIODIR/${layer}.run-control.properties 2>>$SYSLOG
    done
    cat $SCENARIODIR/${SCENARIO}.run-control.properties >> $SCENARIODIR/run.properties 2>> $SYSLOG
}

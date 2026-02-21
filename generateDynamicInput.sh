#!/bin/bash
#----------------------------------------------------------------
# generateDynamicInput.sh: subroutine that generates dynamic
# input files like tide_fac.out, fort.13, fort.15, and fort.26.
#----------------------------------------------------------------
# Copyright(C) 2024--2026 Jason Fleming
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
    logMessage "$SCENARIO: $THIS: Generating input files (usually includes tide_fac.out, fort.13, and fort.15)."
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
    local na_activate_list=""
    for k in ${nodal_attribute_activate[@]}; do
        na_activate_list="$na_activate_list\n      - \"$k\""
    done
    # set wind exposure
    local exposure=$windExposure
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
        local controlTemplate="$INPUTDIR/$CONTROLTEMPLATE"
        outputInventory="full"
        # make adjustments for meteorology-only scenarios and layers
        if [[ $layer == "wind10m" || ( $layer == $SCENARIO && $SCENARIO == *"Wind10m" ) ]]; then
            outputInventory="metonly" # only produce meteorological output, no water levels
            exposure="10m"            # turn off overalnd wind roughness and canopy coefficient
            # if a hardcoded fort.15 template without wind roughness has been specified
            # for a standalone scenario, then use the associated fort.15 template
            if [[ $layer == $SCENARIO && $SCENARIO == *"Wind10m" && $CONTROLTEMPLATENOROUGH != "null" ]]; then
                layerControlTemplate="${INPUTDIR}/${CONTROLTEMPLATENOROUGH}"
            fi
            outputInventory="metonly"
            metonlyNWS=$BASENWS
            if [[ $BACKGROUNDMET == *"Blend" && $stage == "NOWCAST" ]]; then
                metonlyNWS=-$(($BASENWS + 10))  # e.g., 20 becomes -30
            fi
            for k in ${nodal_attribute_activate[@]}; do
                if [[ $k == "surface_directional_effective_roughness_length" || $k == "surface_canopy_coefficient" || $k == "elemental_slope_limiter" ]]; then
                    continue  # deactivate nodal attributes that reduce wind to ground level (or update ESLNodes.63)
                fi
                na_activate_list="$na_activate_list\n      - \"$k\""
            done
            layerWaves="off"
        fi
        # record adcirc version support (according to config/mesh_defaults.sh)
        avs="" ;
        for v in ${adcircVersions[*]}; do
        avs+=" \'$v\', "
        done
        avs="${avs%, }" # remove trailing comma
        # fill in the template
        sed \
        -e "s/%GRIDNAME%/$GRIDNAME/" \
        -e "s?%CONTROLTEMPLATE%?$controlTemplate?" \
        -e "s/%ADCIRCVER%/$(adcirc -v)/" \
        -e "s/%adcircVersions%/$avs/" \
        -e "s/%CSDATE%/$CSDATE/" \
        -e "s/%HSTIME%/$HSTIME/" \
        -e "s/%projection%/${coordinateSystem["projection"]}/" \
        -e "s/%reprojection%/${coordinateSystem["reprojection"]}/" \
        -e "s/%earthCurvature%/${coordinateSystem["earthCurvature"]}/" \
        -e "s/%rotation%/${coordinateSystem["rotation"]}/" \
        -e "s/%ADVISORY%/$ADVISORY/" \
        -e "s/%SCENARIO%/$SCENARIO/" \
        -e "s/%ENDTIME%/$endTime/" \
        -e "s/%NWS%/$NWS/" \
        -e "s/%BASENWS%/$BASENWS/" \
        -e "s/%TIMESTEPSIZE%/$TIMESTEPSIZE/" \
        -e "s/%IM_ETC%/$solver_time_integration/" \
        -e "s/%HINDCASTLENGTH%/$HINDCASTLENGTH/" \
        -e "s/%A00B00C00%/$time_weighting_coefficients/" \
        -e "s/%windExposure%/$exposure/" \
        -e "s/%NWSET%/${owiWinPre["NWSET"]}/" \
        -e "s/%NWBS%/${owiWinPre["NWBS"]}/" \
        -e "s/%DWM%/${owiWinPre["DWM"]}/" \
        -e "s/%DWM%/${owiWinPre["DWM"]}/" \
        -e "s/%startdatetime%/${owiWinPre["startDateTime"]}/" \
        -e "s/%enddatetime%/${owiWinPre["endDateTime"]}/" \
        -e "s/%rhoAir%/${metControl["rhoAir"]}/" \
        -e "s/%nPowellSearchDomains%/${metControl["nPowellSearchDomains"]}/" \
        -e "s/%lateral_turbulence%/$lateral_turbulence/" \
        -e "s/%ESLM%/$eddy_viscosity_coefficient/" \
        -e "s/%ESLM_Smagorinsky%/$smagorinsky_coefficient/" \
        -e "s/%smag_comp_flag%/${Smag_Control["smag_comp_flag"]}/" \
        -e "s/%smag_upper_lim%/${Smag_Control["smag_upper_lim"]}/" \
        -e "s/%smag_lower_lim%/${Smag_Control["smag_lower_lim"]}/" \
        -e "s/%tidal_forcing%/$TIDEFAC/" \
        -e "s?%tidefac_file%?$tidefac_file?" \
        -e "s?%tidal_potential_comment%?$tidal_potential_comment?" \
        -e "s?%tidal_boundary_comment%?$tidal_boundary_comment?" \
        -e "s/%NABOUT%/$log_level/" \
        -e "s/%H0%/$h0/" \
        -e "s/%VELMIN%/$velmin/" \
        -e "s/%FFACTOR%/$bottom_friction_limit/" \
        -e "s/%advection%/$advection/" \
        -e "s/%NWS%/$NWS/" \
        -e "s/%WTIMINC%/$WTIMINC/" \
        -e "s/%pureVortex%/$pureVortex/" \
        -e "s/%pureBackground%/$pureBackground/" \
        -e "s/%BLADJ%/$BLADJ/" \
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
        -e "s/%StatPartWetFix%/${wetDryControl["StatPartWetFix"]}/" \
        -e "s/%How2FixStatPartWet%/${wetDryControl["How2FixStatPartWet"]}/" \
        -e "s/%inundationOutput%/${inundationOutputControl["inundationOutput"]}/" \
        -e "s/%inunThresh%/${inundationOutputControl["inunThresh"]}/" \
        -e "s/%WAVES%/$WAVES/" \
        -e "s/%wave_model%/$wave_model/" \
        -e "s/%RSTIMINC%/$SWANDT/" \
        -e "s/%WaveWindMultiplier%/${waveCoupling["WaveWindMultiplier"]}/" \
        -e "s/%Limit_WaveStressGrad%/${waveCoupling["Limit_WaveStressGrad"]}/" \
        -e "s/%WaveStressGrad_Cap%/${waveCoupling["WaveStressGrad_Cap"]}/" \
        -e "s?%SWANTEMPLATE%?${SWANTEMPLATEDIR}/${SWANTEMPLATE}?" \
        -e "s/%HOTSWAN%/$HOTSWAN/" \
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
        -e "s/%NFOVER%/${nfover['NFOVER']}/" \
        -e "s/%WarnElev%/${nfover['WarnElev']}/" \
        -e "s/%iWarnElevDump%/${nfover['iWarnElevDump']}/" \
        -e "s/%WarnElevDumpLimit%/${nfover['WarnElevDumpLimit']}/" \
        -e "s/%ErrorElev%/${nfover['ErrorElev']}/" \
        -e "s/%NSCREEN%/$NSCREEN/" \
        -e "s/%NETCDF34%/$netCDF34/" \
        -e "s/%metOnlyTimeStepSize%/$metOnlyTimeStepSize/" \
        -e "s/%HOTSTARTINPUTFORMAT%/$HOTSTARTFORMAT/" \
        -e "s/%HOTSTARTOUTPUTFORMAT%/$HOTSTARTFORMAT/" \
        -e "s?%ELEVSTATIONS%?$INPUTDIR/$ELEVSTATIONS?" \
        -e "s?%VELSTATIONS%?$INPUTDIR/$VELSTATIONS?" \
        -e "s?%METSTATIONS%?$INPUTDIR/$METSTATIONS?" \
        -e "s/%fort61incr%/${fort61['incr_seconds']}/" \
        -e "s/%fort61format%/${fort61['format']}/" \
        -e "s/%fort61append%/${fort61['append']}/" \
        -e "s/%fort62incr%/${fort62['incr_seconds']}/" \
        -e "s/%fort62format%/${fort62['format']}/" \
        -e "s/%fort62append%/${fort62['append']}/" \
        -e "s/%fort7172incr%/${fort7172['incr_seconds']}/" \
        -e "s/%fort7172format%/${fort7172['format']}/" \
        -e "s/%fort7172append%/${fort7172['append']}/" \
        -e "s/%fort63incr%/${fort63['incr_seconds']}/" \
        -e "s/%fort63format%/${fort63['format']}/" \
        -e "s/%fort63append%/${fort63['append']}/" \
        -e "s/%fort64incr%/${fort64['incr_seconds']}/" \
        -e "s/%fort64format%/${fort64['format']}/" \
        -e "s/%fort64append%/${fort64['append']}/" \
        -e "s/%fort7374incr%/${fort7374['incr_seconds']}/" \
        -e "s/%fort7374format%/${fort7374['format']}/" \
        -e "s/%fort7374append%/${fort7374['append']}/" \
        -e "s/%write_SWAN_OutputTPS%/${SWANOutputControl['write_SWAN_OutputTPS']}/" \
        -e "s/%write_SWAN_OutputTM01%/${SWANOutputControl['write_SWAN_OutputTM01']}/" \
        -e "s/%write_SWAN_OutputHS%/${SWANOutputControl['write_SWAN_OutputHS']}/" \
        -e "s/%write_SWAN_OutputDIR%/${SWANOutputControl['write_SWAN_OutputDIR']}/" \
        -e "s/%write_SWAN_OutputTMM10%/${SWANOutputControl['write_SWAN_OutputTMM10']}/" \
        -e "s/%write_SWAN_OutputTM02%/${SWANOutputControl['write_SWAN_OutputTM02']}/" \
        -e "s/%write_WarnElev%/${nfover['write_WarnElev']}/" \
        -e "s/%write_iWarnElevDump%/${nfover['write_iWarnElevDump']}/" \
        -e "s/%write_WarnElevDumpLimit%/${nfover['write_WarnElevDumpLimit']}/" \
        -e "s/%write_ErrorElev%/${nfover['write_ErrorElev']}/" \
        -e "s/%write_smag_comp_flag%/${Smag_Control['write_smag_comp_flag']}/" \
        -e "s/%write_smag_upper_lim%/${Smag_Control['write_smag_upper_lim']}/" \
        -e "s/%write_smag_lower_lim%/${Smag_Control['write_smag_lower_lim']}/" \
        -e "s/%write_WindDragLimit%/${metControl['write_WindDragLimit']}/" \
        -e "s/%write_DragLawString%/${metControl['write_DragLawString']}/" \
        -e "s/%write_outputWindDrag%/${metControl['write_outputWindDrag']}/" \
        -e "s/%write_rhoAir%/${metControl['write_rhoAir']}/" \
        -e "s/%write_invertedBarometerOnElevationBoundary%/${metControl['write_invertedBarometerOnElevationBoundary']}/" \
        -e "s/%write_nPowellSearchDomains%/${metControl['write_nPowellSearchDomains']}/" \
        -e "s/%write_outputNodeCode%/${wetDryControl['write_outputNodeCode']}/" \
        -e "s/%write_outputNOFF%/${wetDryControl['write_outputNOFF']}/" \
        -e "s/%write_noffActive%/${wetDryControl['write_noffActive']}/" \
        -e "s/%write_StatPartWetFix%/${wetDryControl['write_StatPartWetFix']}/" \
        -e "s/%write_How2FixStatPartWet%/${wetDryControl['write_How2FixStatPartWet']}/" \
        -e "s/%write_slim%/${wetDryControl['write_slim']}/" \
        -e "s/%write_windlim%/${wetDryControl['write_windlim']}/" \
        -e "s/%write_directvelWD%/${wetDryControl['write_directvelWD']}/" \
        -e "s/%write_useHF%/${wetDryControl['write_useHF']}/" \
        -e "s/%write_inundationOutput%/${inundationOutputControl['write_inundationOutput']}/" \
        -e "s/%write_inunThresh%/${inundationOutputControl['write_inunThresh']}/" \
        -e "s/%write_WaveWindMultiplier%/${waveCoupling['write_WaveWindMultiplier']}/" \
        -e "s/%write_Limit_WaveStressGrad%/${waveCoupling['write_Limit_WaveStressGrad']}/" \
        -e "s/%write_WaveStressGrad_Cap%/${waveCoupling['write_WaveStressGrad_Cap']}/" \
            < $controlParametersTemplate \
            > $SCENARIODIR/${layer}.control_parameters.yaml
        if [[ $? != 0 ]]; then
            logMessage "$THIS: $SCENARIO: Failed to fill in control parameters template with sed."
        fi
        #
        controlPath=$SCENARIODIR
        controlFile=${layer}.fort.15
        swanFile=fort.26
        controlOpts=""
        if [[ $TEST == "unit" ]]; then
            controlPath=$PWD
            controlOpts="--test"
        fi
        logMessage "$SCENARIO: $THIS: Generating ADCIRC Control File ($controlFile) for $SCENARIO with parameters from the file '${layer}.control_parameters.yaml'."
        perl $SCRIPTDIR/control_file_gen.pl $controlOpts < $SCENARIODIR/${layer}.control_parameters.yaml > $controlPath/$controlFile 2>> ${SYSLOG}
        controlExitStatus=$?
        controlMsg=""
        if [[ $controlExitStatus != 0 ]]; then
            controlMsg="The control_file_gen.pl script failed with the following error code: '$controlExitStatus'."
        fi
        if [[ ! -e $controlFile || ! -s $controlFile ]]; then
            controlExitStatus=1
            controlMsg+="$controlMsg Failed to generate the ADCIRC '$controlFile' file."
        fi
        if [[ $layer == $SCENARIO && $layerWaves == "on" && $NWS != "0" && $SCENARIO != *"Wind10m" ]]; then
            if [[ ! -e $swanFile || ! -s $swanFile ]]; then
                controlExitStatus=1
                controlMsg="$controlMsg Failed to generate the SWAN '$swanFile' file."
            fi
        fi
        if [[ $controlExitStatus -ne 0 ]]; then
            finalErrMessage="$THIS: $SCENARIO: The $SCENARIO run will be abandoned: $controlMsg"
            warn $finalErrMessage
            echo $finalErrMessage >> jobFailed
        fi
        #
        mv $SCENARIODIR/run-control.properties $SCENARIODIR/${layer}.run-control.properties 2>>$SYSLOG
    done
    cat $SCENARIODIR/${SCENARIO}.run-control.properties >> $SCENARIODIR/run.properties 2>> $SYSLOG
}
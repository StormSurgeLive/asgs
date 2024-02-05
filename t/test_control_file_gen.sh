#!/bin/bash
#
# set variables to be used in yaml template
# SCRIPTDIR is defined by the asgsh environment
adcirc_version="v53release"
solver_time_integration="implicit"
time_weighting_coefficients="0.35 0.3 0.35"
lateral_turbulence="eddy_viscosity"
eddy_viscosity_coefficient=50.0
smagorinsky_coefficient=0.2
h0=0.1
velmin=0.1
bottom_friction_limit=0.001
advection="on"
# &metControl WindDragLimit=floatValue, DragLawString='stringValue', rhoAir=floatValue, outputWindDrag=logicalValue /
declare -A metControl
metControl["DragLawString"]="garratt"
metControl["WindDragLimit"]="0.0025"
metControl["outputWindDrag"]="no"
# &wetDryControl outputNodeCode=logicalValue, outputNOFF=logicalValue, noffActive=logicalValue /
declare -A wetDryControl
wetDryControl["outputNodeCode"]="no"
wetDryControl["outputNOFF"]="no"
wetDryControl["noffActive"]="on"
# &inundationOutputControl inundationOutput=logicalValue0, inunThresh =floatValue /
declare -A inundationOutputControl
inundationOutputControl["inundationOutput"]="yes"
inundationOutputControl["inunThresh"]="0.6"
declare -a nodal_attribute_activate
nodal_attribute_activate=( "sea_surface_height_above_geoid" "mannings_n_at_sea_floor" )
declare -A netcdf_metadata
netcdf_metadata["NCPROJ"]="ASGS"
netcdf_metadata["NCINST"]="Seahorse Consulting"
netcdf_metadata["NCSOUR"]="ADCIRC"
netcdf_metadata["NCHIST"]="ASGS Workflow"
netcdf_metadata["NCREF"]="https://doi.org/10.1061/40990(324)48"
netcdf_metadata["NCCOM"]="Trusted since 2006."
netcdf_metadata["NCHOST"]="www.seahorsecoastal.com"
netcdf_metadata["NCCONV"]="CF"
netcdf_metadata["NCCONT"]="jason.fleming@adcirc.live"
netcdf_metadata["NCDATE"]="2010-05-01 00:00:00 UTC"
# swan
swan["MXITNS"]="20"
swan["NPNTS"]="95"
# nodal attributes
nodal_attributes_template_file="shinnecock_nodal_attributes.template"
declare -A nodal_attribute_default_values
nodal_attribute_default_values["sea_surface_height_above_geoid"]="0.0"
nodal_attribute_default_values["mannings_n_at_sea_floor"]="0.02"
#
# fill in template
#
sed \
    -e "s/%ADCIRCVER%/$adcirc_version/" \
    -e "s/%IM_ETC%/$solver_time_integration/" \
    -e "s/%A00B00C00%/$time_weighting_coefficients/" \
    -e "s/%lateral_turbulence%/$lateral_turbulence/" \
    -e "s/%ESLM%/$eddy_viscostity_coefficient/"                   \
    -e "s/%ESLM_Smagorinsky%/$smagorinsky_coefficient/" \
    -e "s/%H0/$h0/" \
    -e "s/%VELMIN%/$velmin/" \
    -e "s/%FFACTOR%/$bottom_friction_limit/" \
    -e "s/%advection%/$advection/" \
    -e "s/%DragLawString%/${metControl["DragLawString"]}/" \
    -e "s/%WindDragLimit%/${metControl["WindDragLimit"]}/" \
    -e "s/%outputWindDrag%/${metControl["outputWindDrag"]}/" \
    -e "s/%outputNodeCode%/${wetDryControl["outputNodeCode"]}/" \
    -e "s/%outputNOFF%/${wetDryControl["outputNOFF"]}/" \
    -e "s/%noffActive%/${wetDryControl["noffActive"]}/" \
    -e "s/%inundationOutput%/${inundationOutputControl["inundationOutput"]}/" \
    -e "s/%inunThresh%/${inundationOutputControl["inunThresh"]}/" \
    -e "s/%NCPROJ%/${netcdf_metadata["NCPROJ"]}/" \
    -e "s/%NCINST%/${netcdf_metadata["NCINST"]}/" \
    -e "s/%NCSOUR%/${netcdf_metadata["NCSOUR"]}/" \
    -e "s/%NCHIST%/${netcdf_metadata["NCHIST"]}/" \
    -e "s/%NCREF%/${netcdf_metadata["NCREF"]}/" \
    -e "s/%NCCOM%/${netcdf_metadata["NCCOM"]}/" \
    -e "s/%NCHOST%/${netcdf_metadata["NCHOST"]}/" \
    -e "s/%NCCONV%/${netcdf_metadata["NCCONV"]}/" \
    -e "s/%NCCONT%/${netcdf_metadata["NCCONT"]}/" \
    -e "s/%NCDATE%/${netcdf_metadata["NCDATE"]}/" \
    -e "s/%SWANMXITNS%/${swan["MXITNS"]}/" \
    -e "s/%SWANNPNTS%/${swan["NPNTS"]}/" \
    -e "s/%nodal_attributes_template_file%/$nodal_attributes_template_file/" \
    -e "s/%nodal_attributes_default_values%/$nodal_attributes_default_values_hash/" \
     < $SCRIPTDIR/$controlParametersTemplateName \
     > "$filledControlParametersTemplateName"
if [[ $? != 0 ]]; then
    echo "$THIS: Failed to fill in control parameters template with sed."
fi
NSCREEN=${NSCREEN:-"-1000"} # frequency (in time steps) of output to adcirc.log
# water surface elevation station output
FORT61="--fort61freq 300.0 --fort61netcdf"
# water current velocity station output
FORT62="--fort62freq 0"
# full domain water surface elevation output
FORT63="--fort63freq 3600.0 --fort63netcdf"
# full domain water current velocity output
FORT64="--fort64freq 3600.0 --fort64netcdf"
# met station output
FORT7172="--fort7172freq 300.0 --fort7172netcdf"
# full domain meteorological output
FORT7374="--fort7374freq 3600.0 --fort7374netcdf"
#SPARSE="--sparse-output"
SPARSE=""
NETCDF4="--netcdf4"
OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
# fulldomain or subdomain hotstart files
if [[ $HOTSTARTCOMP != "subdomain" ]]; then
   HOTSTARTCOMP="fulldomain"
fi
# binary, netcdf, or netcdf3 hotstart files
if [[ $HOTSTARTFORMAT != "netcdf3" && $HOTSTARTFORMAT != "binary" ]]; then
   HOTSTARTFORMAT="netcdf"
fi
# "continuous" or "reset" for maxele.63 etc files
MINMAX=reset
#
if [[ ${ENSTORM:(-7)} = "Wind10m" ]]; then
   scenarioMessage "$THIS: Setting parameters to trigger ADCIRC met-only mode for ${ENSTORM}."
   ADCPREPWALLTIME="01:00:00"  # adcprep wall clock time, including partmesh
   FORECASTWALLTIME="01:00:00" # forecast wall clock time
   CONTROLTEMPLATE=$CONTROLTEMPLATENOROUGH  # CONTROLTEMPLATENOROUGH set in config/mesh_defaults.sh
   TIMESTEPSIZE=300.0          # 15 minute time steps
   NCPU=15                     # dramatically reduced resource requirements
   NUMWRITERS=1                # multiple writer procs might collide
   WAVES=off                   # deactivate wave forcing
   FORT61="--fort61freq 0"     # turn off water surface elevation station output
   FORT62="--fort62freq 0"     # turn off water current velocity station output
   FORT63="--fort63freq 0"     # turn off full domain water surface elevation output
   FORT64="--fort64freq 0"     # turn off full domain water current velocity output
   FORT7172="--fort7172freq 300.0 --fort7172netcdf"    # met station output
   FORT7374="--fort7374freq 3600.0 --fort7374netcdf"   # full domain meteorological output
   #SPARSE="--sparse-output"
   SPARSE=""
   NETCDF4="--netcdf4"
   OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
   POSTPROCESS=( null_post.sh )
fi


OUTPUTOPTIONS=
CONTROLOPTIONS="--name $ENSTORM"
CONTROLOPTIONS="$CONTROLOPTIONS  --scriptdir $SCRIPTDIR
 --advisorynum $ADVISORY
  --advisdir $ADVISDIR
   --cst $CSDATE
    --endtime $HINDCASTLENGTH
     --dt $TIMESTEPSIZE
      --nws $NWS
       --hsformat $HOTSTARTFORMAT
        --advisorynum 0
          $OUTPUTOPTIONS"
CONTROLOPTIONS="$CONTROLOPTIONS --elevstations ${INPUTDIR}/${ELEVSTATIONS} --velstations ${INPUTDIR}/${VELSTATIONS} --metstations ${INPUTDIR}/${METSTATIONS}"
CONTROLOPTIONS="$CONTROLOPTIONS --gridname $GRIDNAME" # for run.properties
CONTROLOPTIONS="$CONTROLOPTIONS --periodicflux $PERIODICFLUX"  # for specifying constant periodic flux
CONTROLOPTIONS="$CONTROLOPTIONS --nscreen $NSCREEN"
if [[ $NOFORCING = true ]]; then
    CONTROLOPTIONS="$_RPCONTROLOPTIONS --specifiedRunLength $HINDCASTLENGTH"
else
    CONTROLOPTIONS="$CONTROLOPTIONS --endtime $HINDCASTLENGTH  --nws $NWS  --advisorynum 0"
fi
$SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS --controltemplate $filledControlParametersTemplateName < $filledControlParametersTemplateName > fort.15 2>> control_file_gen.pl.log

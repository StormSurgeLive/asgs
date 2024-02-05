#!/bin/bash
#
# set variables used by this script
INPUTDIR=$SCRIPTDIR/input/meshes/shinnecock
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
# set variables to be used in command line options
SCENARIO=nowcast
ADVISORY=20
ADVISDIR=$SCRATCH
CSDATE=20240101
HINDCASTLENGTH=30.0
TIMESTEPSIZE=2.0
NWS=20
HOTSTARTFORMAT=netcdf
ELEVSTATIONS=${INPUTDIR}/shinnecock_stations.txt
VELSTATIONS=$ELEVSTATIONS
METSTATIONS=$ELEVSTATIONS
GRIDNAME=shinnecock_inlet_coarse.grd
PERIODICFLUX=null
NSCREEN=-1000
FORT61="--fort61freq 300.0 --fort61netcdf"
FORT62="--fort62freq 0"
FORT63="--fort63freq 3600.0 --fort63netcdf"
FORT64="--fort64freq 3600.0 --fort64netcdf"
FORT7172="--fort7172freq 300.0 --fort7172netcdf"
FORT7374="--fort7374freq 3600.0 --fort7374netcdf"
SPARSE=""
NETCDF4="--netcdf4"
OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
SWANDT=1200
SWANTEMPLATE=${SCRIPTDIR}/input/meshes/common/adcirc_swan_v53_parameters_fort.26.template
HOTSWAN="yes"
BLADJ=0.9
PUREVORTEX=3.0
PUREBACKGROUND=5.0
HSTIME=86400.0
NHCNAME="KATRINA"
#
C="--name $SCENARIO"
C="$C --scriptdir $SCRIPTDIR"
C="$C --advisorynum $ADVISORY"
C="$C --advisdir $ADVISDIR"
C="$C --stormdir $SCRATCH"
C="$C --cst $CSDATE"
C="$C --endtime $HINDCASTLENGTH"
C="$C --dt $TIMESTEPSIZE"
C="$C --nws $NWS"
C="$C --bladj $BLADJ"
C="$C --pureVortex $PUREVORTEX"
C="$C --pureBackground $PUREBACKGROUND"
C="$C --nhcName $NHCNAME"
C="$C --hsformat $HOTSTARTFORMAT"
C="$C --hstime $HSTIME"
C="$C --elevstations ${INPUTDIR}/${ELEVSTATIONS}"
C="$C --velstations ${INPUTDIR}/${VELSTATIONS}"
C="$C --metstations ${INPUTDIR}/${METSTATIONS}"
C="$C --gridname $GRIDNAME"          # for run.properties
C="$C --periodicflux $PERIODICFLUX"  # for specifying constant periodic flux
C="$C --nscreen $NSCREEN"
C="$C --swantemplate $SWANTEMPLATE"
C="$C --swandt $SWANDT"
C="$C $OUTPUTOPTIONS"
C="$C --controltemplate $INPUTDIR/shinnecock-parameters.fort.15.template"
#
$SCRIPTDIR/control_file_gen.pl $CONTROLOPTIONS < $filledControlParametersTemplateName > fort.15 2>> control_file_gen.pl.log
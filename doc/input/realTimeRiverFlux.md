% Real Time River Flux Design
% ADCIRC Surge Guidance System
% July 2017

<!--  
~/.cabal/bin/pandoc -o realTimeRiveFlux.pdf --variable mainfont=Georgia --latex-engine=xelatex --variable sansfont=Arial --variable fontsize=12pt --variable geometry:margin=1in realTimeRiverFlux.md
-->

Real Time River Flux
====================

ADCIRC meshes with river boundaries present some unique challenges 
for real time deployment. These include the following:

1. The ADCIRC mesh file format makes it difficult to know what river boundary nodes correspond with each river.
2. ADCIRC requires that river boundary conditions be presented in terms of flux per unit width, which is not straightforward to provide for the following reasons:
    a. Rivers flux data are rarely available; river stage data are more common by far. 
    b. In order to compute the flux from the stage, a stage-discharge curve must be available. 
    c. Computing flux per unit width requires knowledge of the vertex spacing and bathymetric depth along the river boundary in the mesh, which is not immediately obvious when looking at the boundary specification in the mesh file. 
3. The ADCIRC type 52 flux/radiation boundary condition cannot handle variable river flux because ADCIRC stores stage at the boundary due to constant flux alone and uses this value to radiate storm surge across the boundary out of the domain. 
4. A river boundary may be set to a constant flux or a variable flux, and these two states use two different input files with different formats (i.e., fort.15 and fort.20). 
5. The analyst may wish to compute the river boundary condition by various means: 
    a. based on a static specified stage
    b. based on a static specified discharge
    c. falling back to a default constant stage or discharge
    c. using real time gage data that are accessed over a web service
    d. with a known relationship between two different rivers (e.g., the statutorily mandated relationship between the Atchafalaya and Mississippi river flows)
    e. leveraging a river forecast model that produces variable flux files in ADCIRC's fort.20 variable flux format
7. River boundaries that are partially or fully above mean sea level require initialization of the water surface elevation throughout the river course to prevent flux into a dry area.
6. It should be easy for the Operator to configure the ASGS to assign river boundary conditions based on all the above considerations. 

This document describes how these challenges were addressed in the 
design of variable flux river boundary conditions in the ASGS.

Using this new infrastructure requires two steps: (1) the ASGS 
developer, who installed the ASGS and obtained the mesh must provide 
information about the mesh in a properties file; and (2) the ASGS 
Operator must configure the boundary conditions for a particular 
instance of the ASGS. These two steps are described in the following 
subsections. 

Developer
---------

The flux boundary condition is set via the following algorithm:

__Step 1.__ Add properties to the `$GRIDFILE.properties` file manually when the mesh is installed in ASGS, including metadata about the order of the boundaries in the file, the boundary types, and the default water surface elevation above mesh zero to handle cases where the mesh boundary is partly or fully above mean sea level. For example:

    mississippiRiverBoundaryMeshFilePosition : 1
    mississippiRiverBoundaryType : 52
    mississippiRiverBoundaryNominalWSE : 0.0
    atchafalayaRiverBoundaryMeshFilePosition : 2
    atchafalayaRiverBoundaryType : 52
    atchafalayaRiverBoundaryNominalWSE : 0.0

__Step 2.__ Add properties to the `$CONTROLTEMPLATE.properties` file manually when the mesh is installed in ASGS, including metadata about the gage station where ASGS should get the stage and/or discharge data. For example:

    mississippiRiverBoundaryStageDischargeRelationshipName : 
        "Mississippi River at Baton Rouge"
    mississippiRiverBoundaryStageGageName : 
        "Mississippi River at Baton Rouge"
    mississippiRiverBoundaryStageGageID : 01160
    mississippiRiverBoundaryStageGageAgency : USACE
    atchafalayaRiverBoundaryStageDischargeRelationshipName : 
        "Atchafalaya River at Simmesport" 
    atchafalayaRiverBoundaryStageGageName : 
        "Atchafalaya River at Simmesport"
    atchafalayaRiverBoundaryStageGageID : 03045 
    atchafalayaRiverBoundaryStageGageAgency : USACE
    atchafalayaRiverBoundaryDischargeGageName : 
        "Atchafalaya River at Simmesport - Discharge"
    atchafalayaRiverBoundaryDischargeGageID : 03045Q 
    atchafalayaRiverBoundaryDischargeGageAgency : USACE

__Step 3.__ Run the `boundaryFinder.f90` program to create a 
boundaries lengthsdepths file. This file contains the depths at each 
node and the distances between them. This information is required 
for computing the flux per unit width. Example invokation is as 
follows:

     ${SCRIPTDIR}/util/mesh/boundaryFinder.x --meshfile ${GRIDFILE} 
         --outputfile ${GRIDFILE}.inflow_flux_boundaries_lengthsdepths.txt 
         --boundarytype inflow_flux --lengthsdepths

The boundary data are produced in the following example format 
(actual results for the hsdrrs2014 mesh are shown):

    19 52
          192.9019710000     1153.7200880671 # totalEffDepth(m) totalLength(m)
           4.2500000000       32.7078026074 # effDepth(m) effLength(m)
          10.4000000000       61.1029113234 # effDepth(m) effLength(m)
          12.2600000000       55.6271036008 # effDepth(m) effLength(m)
          14.1200000000       61.1918078167 # effDepth(m) effLength(m)
          13.0400000000       66.1989426613 # effDepth(m) effLength(m)
          13.8631070000       66.7583515272 # effDepth(m) effLength(m)
          16.9561900000       75.1748935241 # effDepth(m) effLength(m)
          16.6091390000       74.3108510539 # effDepth(m) effLength(m)
          16.8379350000       67.8602817319 # effDepth(m) effLength(m)
          15.9140560000       66.3323268674 # effDepth(m) effLength(m)
          14.6089820000       62.7174362868 # effDepth(m) effLength(m)
          12.0605730000       62.5747939792 # effDepth(m) effLength(m)
           9.4094670000       62.8000130263 # effDepth(m) effLength(m)
           7.2902380000       60.1456320118 # effDepth(m) effLength(m)
           5.8460780000       60.0730363241 # effDepth(m) effLength(m)
           2.9112060000       61.5396403061 # effDepth(m) effLength(m)
           2.6700000000       61.9466953071 # effDepth(m) effLength(m)
           2.5700000000       62.9037291478 # effDepth(m) effLength(m)
           1.2850000000       31.7538389638 # effDepth(m) effLength(m)
    13 52
         279.6019710000     2200.9398214913 # totalEffDepth(m) totalLength(m)
           3.6125000000       34.8173843029 # effDepth(m) effLength(m)
           7.2250000000       69.6347686059 # effDepth(m) effLength(m)
           7.2250000000       69.6347686096 # effDepth(m) effLength(m)
           7.2250000000       76.1715143864 # effDepth(m) effLength(m)
           7.2250000000       88.5111506544 # effDepth(m) effLength(m)
           7.2250000000      100.8565728764 # effDepth(m) effLength(m)
           7.2250000000      107.3991046034 # effDepth(m) effLength(m)
           7.2250000000      107.4032871647 # effDepth(m) effLength(m)
           7.2250000000       96.0896657836 # effDepth(m) effLength(m)
           7.2250000000       84.7718618412 # effDepth(m) effLength(m)
           7.2250000000       84.7718618412 # effDepth(m) effLength(m)
           7.2250000000       84.7718618375 # effDepth(m) effLength(m)
           3.6125000000       42.3859309169 # effDepth(m) effLength(m)

Operator
--------

The operator must configure the ASGS to use the type of river 
forcing deemed appropriate for the goals of the modelling effort. 
The different options are specified using the VARFLUX parameter in 
the ASGS configuration file. This parameter can be set to the 
following values: off, default, steadybc, and nssl.ou.fort.20 (same 
as the now-deprecated value of on). The effects of these choices are 
described in the remainder of this section. 

__off.__ When `VARFLUX` is set to `off`, and there are no river boundaries
in the mesh file, the fort.20 file will not be created, and the NFFR
line in the fort.15 will not included. If there are river boundaries
in the mesh, a fort.20 file will be created with flux per unit width
values equal to zero. 

__default.__ When `VARFLUX` is set to `default`, the Operator must also
specify the name of the fort.20 file containing the default flux. This
fort.20 file will be copied to the run directory for every simulation,
and the value of NFFR in the fort.15 will be set to 0. 

__steadybc.__ When `VARFLUX` is set to `steadybc`, then the 
boundaries will be forced with a constant flux and the Operator must 
set the values of the boundary conditions in a separate parameter 
called `VARFLUXBC`. There are three possible values for the constant
forcing in the `VARFLUXBC` parameter: stage value, discharge value, 
auto, and gage. Examples of setting the `VARFLUXBC` parameter for 
the HSDRRS2014 mesh are as follows:

     VARFLUX=steadybc
     # explicit constant discharge in thousands of cubic feet per second,
     # set to ASGS use statutory discharge 
     VARFLUXBC="mississippiRiverBoundaryCondition=600kcfs,
      atchafalayaRiverBoundaryCondition=auto"  

or

     VARFLUX=steadybc
     # explicitly set both boundaries to a particular constant discharge
     # in thousands of cubic meters per second
     VARFLUXBC="mississippiRiverBoundaryCondition=17kcms,
     atchafalayaRiverBoundaryCondition=11.3kcms" 

or

     VARFLUX=steadybc
     # set to a particular stage in meters, and let ASGS compute 
     # the discharge from its stage discharge curve, and then compute
     # the second boundary based on a statutorily mandated discharge ratio
     VARFLUXBC="mississippiRiverBoundaryCondition=10m, 
     atchafalayaRiverBoundaryStage=auto" 

or

     VARFLUX=steadybc
     # let ASGS set one boundary based on current gage value on 
     # rivergages.com and then compute the second boundary bsed
     # on statutorily mandated discharge ratio
     VARFLUXBC="mississippiRiverBoundaryCondition=gage, 
     atchafalayaRiverBoundaryStage=auto"

In each of the above cases, the flux will be constant over the course
of a forecast.

__nssl.ou.fort.20__ This value of `VARFLUX` produces the same result
as the now-deprecated value of `on`. This setting causes the ASGS
to use the `get_flux.pl` script to retrieve nowcast and forecast
fort.20 files and construct a fort.20 file that fits the current nowcast
or forecast time range. It also requires the Operator to provide the
name of a fort.20 file to be used in hindcasting (initialization) as
well as a default fort.20 file to be used if real time river flux
boundary condition data cannot be obtained. 

ASGS
----

Once the mesh properties have been set and the ASGS has been configured,
the ASGS sets river flux values using the `set_flux.pl` script as follows:

a. reads the mesh.properties file to find how many river flux boundaries are in the mesh file, and in what order; 
b. it either uses statically configured total fluxes from asgs config file that have been written to run.properties, or
c. it uses statically configured stages from the asgs config file file that have been written to run.properties, or
d. uses a web service to get the relevant water surface elevation and or discharge at a gage location
e. if using water surface elevation, it interpolates a stage discharge curve to get the total flux at each boundary
f. the total flux at each boundary is then written to the run.properties file if it was not read from the run.properties
g. reads the boundaries lengths and depths file to find the edge lengths and depths at each node along the boundary
h. computes the flux per unit widths along the boundary nodes;
i. writes out the periodic (steady) flux per unit width(s) in fort.20 format

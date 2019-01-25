% Precomputed Winds
% ADCIRC Surge Guidance System
% July 2017

<!--  
~/.cabal/bin/pandoc -o precomputedWinds.pdf --variable mainfont=Georgia --latex-engine=xelatex --variable sansfont=Arial --variable fontsize=12pt --variable geometry:margin=1in precomputedWinds.md
-->

Precomputed Winds
=================

There are many situations where the immediate production of meteorological data
output is advantageous, including the following:

* starting on lengthy post processing without delay
* forwarding wind speed plots to decision makers very soon after meteorological input such as a new forecast/advisory becomes available
* producing meterological data with different characteristics than will actually used in the ADCIRC water surface elevation calculations (e.g., eliminating directional wind roughness and/or canopy coefficient from precomputed wind data)

Fortunately, there are features within ADCIRC and the ASGS that allow us to
produce the meteorological output from an ADCIRC run quickly and with 
minimal resources. 

ADCIRC
------

The ADCIRC code is capable of running in "met only" mode, where it does nothing
but read meteorological input data and write the meteorologically-related
output files: `fort.71`, `fort.72`, `fort.73`, and `fort.74`. This mode is
triggered when all other types of output, other than meteorological output, are
turned off in the `fort.15` control file (including hotstart output).

When reading a `fort.15` file that has no output specified other than
meteorological output, ADCIRC will turn off the evaluation of the continuity
equation and the momentum equation and will simply read and write
meteorological data. It will also write a log message like the following to the `fort.16` log file:

    INFO: hotstart: Only meterological output was requested. ADCIRC will not solve the GWCE or momentum equations.

There are some caveats and best practices associated with the met-only mode in ADCIRC:

* only the standalone ADCIRC or parallel ADCIRC executables should be used, not the ADCIRC+SWAN coupled model 
* maximum performance is achieved when the ADCIRC timestep `DTDP` is set to the desired meteorological output interval and the timestep interval for meterological output is set to 1 
* resource requirements are low because meteorological throughput is fast when there is no need to compute water surface elevation
* if met-only mode is run in parallel, and dedicated writer processors are used, then only a single dedicated writer processor should be specified because the high frequency of output could cause multiple dedicated writer processors to interleave their output and corrupt the output file

The next section describes how to take advantage of ADCIRC met-only mode in the
ASGS to precompute wind data for faster turnaround. 

ASGS
----

The forecast ensemble capabilities in the ASGS allow us to take advantage
of met-only mode in ADCIRC by simply specifying an additional ensemble member
with its input parameters tweaked to conform to the values that will trigger 
this mode. 

A representative snippet from an example ASGS configuration file that sets up
an ensemble to use forcing from the North American Mesoscale (NAM) model to
precompute wind data is as follows:

    # Forecast ensemble members
    RMAX=default
    PERCENT=default
    ENSEMBLESIZE=2 # number of storms in the ensemble
    case $si in
    -1)
          # do nothing ... this is not a forecast
       ;;
    0)
       ENSTORM=namforecastMetOnly
       ADCPREPWALLTIME="00:20:00"  # adcprep wall clock time, including partmesh
       FORECASTWALLTIME="00:20:00" # forecast wall clock time
       TIMESTEPSIZE=900.0    # 15 minute time steps
       NCPU=15               # drastically reduced resource requirements
       NUMWRITERS=1          # multiple writer procs might collide
       WAVES=off             # deactivate wave forcing
       # turn off water surface elevation station output
       FORT61="--fort61freq 0"
       # turn off water current velocity station output
       FORT62="--fort62freq 0"
       # turn off full domain water surface elevation output
       FORT63="--fort63freq 0"
       # turn off full domain water current velocity output
       FORT64="--fort64freq 0"
       # met station output
       FORT7172="--fort7172freq 900.0 --fort7172netcdf"
       # full domain meteorological output
       FORT7374="--fort7374freq 3600.0 --fort7374netcdf"
       #SPARSE="--sparse-output"
       SPARSE=""
       NETCDF4="--netcdf4"
       OUTPUTOPTIONS="${SPARSE} ${NETCDF4} ${FORT61} ${FORT62} ${FORT63} ${FORT64} ${FORT7172} ${FORT7374}"
       # prevent collisions in prepped archives
       PREPPEDARCHIVE=prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz
       # special post processing for the precomputed met-only data  
       POSTPROCESS=namforecastMetOnly_post.sh
       ;;
    1)
       ENSTORM=namforecast
       ;;
    *)
       echo "CONFIGRATION ERROR: Unknown ensemble member number: '$si'."
       ;;
    esac


The example above shows that the activation of the met-only mode in ADCIRC can
be achieved in the ASGS entirely in the ASGS configuration file; no other
changes are required. Because this is a forecast ensemble, the ASGS
automatically turns off hotstart output.

The reduction in resource requirements is clear from the reduced wall clock
time estimates as well as the `NCPU` parameter and `NUMWRITER` parameters that
specify only 15 compute cores and 1 dedicated writer core. The meteorological
data for the 3.5 day NAM forecast are finished in less than 3 minutes in this
ASGS deployment of the NOAA HSOFS mesh (1.8M vertices).  In contrast, producing
the full set of ADCIRC+SWAN output for the corresponding `namforecast` ensemble
member takes approximately 90 minutes on 608 compute cores with 16 dedicated
writer cores. 

The ADCIRC time step in the example is set to 900.0 seconds (15 minutes) because
we want 15 minute output at the meteorological stations and because 900.0 divides evenly into 3600.0 so that hourly fulldomain output can be produced easily as well. 

Since the `NCPU` parameter is set differently between the `namforecast` and
`namforecastWindOnly` ensemble members, the ASGS will produce a separate
archive of preprocessed input files (`fort.13`, `fort.14`, and `fort.18`) so
that it does not have to run `adcprep --np 15 --prepall` everytime it runs the
met-only ensemble member. In contrast, it only has to run `adcprep --np 15
--prep15` which is much faster. So we need to make sure that the
`PREPPEDARCHIVE` parameter for this ensemble member is uniquely named in the
defintion of the met-only ensemble member so that the `namforecastMetOnly`
ensemble member does not try to use the same archive as the specified by the
`namforecast` ensemble member. 

Finally, the fact that the precomputed meteorological data have been set up as
a fully-fledged forecast ensemble member allows us to use any special purpose
post processing that is particular to the precomputed wind data. This script or
executable can create visualizations, post results to external servers, send
notifications to decision makers, or perform any other task that could be
applied to a normal ensemble member.

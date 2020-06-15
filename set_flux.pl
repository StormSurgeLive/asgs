#!/usr/bin/env perl
#------------------------------------------------------------------------
# set_flux.pl
#
# Reads a mesh.properties file and a boundary information file 
# writes out the flux per unit width boundary condition for the 
# ADCIRC fort.15 file. 
#
#------------------------------------------------------------------------
# Copyright(C) 2013--2016 Jason Fleming
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
#------------------------------------------------------------------------
#
#------------------------------------------------------------------------
#      A S G S   D E V E L O P E R   D O C U M E N T A T I O N #------------------------------------------------------------------------
# The flux boundary condition is set via the following algorithm:
#
# 0. Add properties from mesh.properties file manually when the mesh 
#    is installed in ASGS, including metadata about the gage station 
#    where asgs should get the stage and/or discharge data for example:
#
#    mississippiRiverBoundaryMeshFilePosition : 1
#    mississippiRiverBoundaryType : 52
#    mississippiRiverBoundaryNominalWSE : 0.0
#    mississippiRiverBoundaryStageDischargeRelationshipName : "Mississippi River at Baton Rouge"
#    mississippiRiverBoundaryStageGageName : "Mississippi River at Baton Rouge"
#    mississippiRiverBoundaryStageGageID : 01160
#    mississippiRiverBoundaryStageGageAgency : USACE
#
#    atchafalayaRiverBoundaryMeshFilePosition : 2
#    atchafalayaRiverBoundaryType : 52
#    atchafalayaRiverBoundaryNominalWSE : 0.0
#    atchafalayaRiverBoundaryStageDischargeRelationshipName : "Atchafalaya River at Simmesport" 
#    atchafalayaRiverBoundaryStageGageName : "Atchafalaya River at Simmesport"
#    atchafalayaRiverBoundaryStageGageID : 03045 
#    atchafalayaRiverBoundaryStageGageAgency : USACE
#    atchafalayaRiverBoundaryDischargeGageName : "Atchafalaya River at Simmesport - Discharge"
#    atchafalayaRiverBoundaryDischargeGageID : 03045Q 
#    atchafalayaRiverBoundaryDischargeGageAgency : USACE
#
# 1. When the mesh is installed in ASGS, it will run the boundaryFinder.f90
#    program to create a boundaries xyz file; example invokation is as follows:
#
# ${SCRIPTDIR}/util/mesh/boundaryFinder.x --meshfile ${GRIDFILE} --outputfile    ${GRIDFILE}.inflow_flux_boundaries_lengthsdepths.txt --boundarytype inflow_flux --lengths-depths
#
# If the mesh file is changed, the ASGS will detect this and automatically 
# rerun the boundaryFinder.x program. 
#
# The boundary data are produced in the following example format (actual
# results for the hsdrrs2014 mesh):
#
#19 52
#      192.9019710000     1153.7200880671 # totalEffDepth(m) totalLength(m)
#        4.2500000000       32.7078026074 # effDepth(m) effLength(m)
#       10.4000000000       61.1029113234 # effDepth(m) effLength(m)
#       12.2600000000       55.6271036008 # effDepth(m) effLength(m)
#       14.1200000000       61.1918078167 # effDepth(m) effLength(m)
#       13.0400000000       66.1989426613 # effDepth(m) effLength(m)
#       13.8631070000       66.7583515272 # effDepth(m) effLength(m)
#       16.9561900000       75.1748935241 # effDepth(m) effLength(m)
#       16.6091390000       74.3108510539 # effDepth(m) effLength(m)
#       16.8379350000       67.8602817319 # effDepth(m) effLength(m)
#       15.9140560000       66.3323268674 # effDepth(m) effLength(m)
#       14.6089820000       62.7174362868 # effDepth(m) effLength(m)
#       12.0605730000       62.5747939792 # effDepth(m) effLength(m)
#        9.4094670000       62.8000130263 # effDepth(m) effLength(m)
#        7.2902380000       60.1456320118 # effDepth(m) effLength(m)
#        5.8460780000       60.0730363241 # effDepth(m) effLength(m)
#        2.9112060000       61.5396403061 # effDepth(m) effLength(m)
#        2.5700000000       62.9037291478 # effDepth(m) effLength(m)
#        1.2850000000       31.7538389638 # effDepth(m) effLength(m)
#13 52
#      279.6019710000     2200.9398214913 # totalEffDepth(m) totalLength(m)
#        3.6125000000       34.8173843029 # effDepth(m) effLength(m)
#        7.2250000000       69.6347686059 # effDepth(m) effLength(m)
#        7.2250000000       69.6347686096 # effDepth(m) effLength(m)
#        7.2250000000       76.1715143864 # effDepth(m) effLength(m)
#        7.2250000000       88.5111506544 # effDepth(m) effLength(m)
#        7.2250000000      100.8565728764 # effDepth(m) effLength(m)
#        7.2250000000      107.3991046034 # effDepth(m) effLength(m)
#        7.2250000000      107.4032871647 # effDepth(m) effLength(m)
#        7.2250000000       96.0896657836 # effDepth(m) effLength(m)
#        7.2250000000       84.7718618412 # effDepth(m) effLength(m)
#        7.2250000000       84.7718618412 # effDepth(m) effLength(m)
#        7.2250000000       84.7718618375 # effDepth(m) effLength(m)
#        3.6125000000       42.3859309169 # effDepth(m) effLength(m)
#
# 2. For each simulation, the ASGS writes properties from the ASGS
#    config file relevant to river flux to the run.properties file, e.g,
#
#    varflux : steadybc
#    varfluxbc : "mississippiRiverBoundaryCondition : 17kcms, atchafalayaRiverBoundaryCondition : 11.3kcms"
#
# 3. set_flux.pl script
#    a. reads the mesh.properties file to find how many river flux 
#       boundaries are in the mesh file, and in what order; 
#    b. it either uses statically configured total fluxes from
#       asgs config file that have been written to run.properties, or
#    c. it uses statically configured stages from the 
#       asgs config file that have been written to run.properties, or
#    d. uses a web service to get the relevant water surface elevation
#       and or discharge at a gage location
#    e. if using water surface elevation, it interpolates a stage
#       discharge curve to get the total flux at each boundary
#    f. the total flux at each boundary is then written to the 
#       run.properties file if it was not read from the run.properties
#    g. reads the boundaries lengths and depths file to find the 
#       edge lengths and depths at each node along the boundary
#    h. computes the flux per unit widths along the boundary nodes;
#    i. writes out the periodic (steady) flux per unit width(s) 
#       in fort.20 format
#
#------------------------------------------------------------------------
#                  A S G S   O P E R A T O R 
#       F L U X   C O N F I G U R A T I O N   O P T I O N S
#------------------------------------------------------------------------
#
# Operator's choices in specifying river flux forcing, using only ASGS 
# config file:
# 
# Constraints / Assumptions
#
# 0. boundary must be placed in location where we have a stage discharge
#    curve available if stage is to be used as input
#
# Options
#
# 1. A default fort.20 file found in the input directory that will 
#    be copied to the run directory for every run. It may also be used
#    under certain circumstances if real time results are not available.
#
# Example: VARFLUX=default
#
# 2. Configure steady fluxes via asgs config file and let asgs propagate to fort.15.
#    If a flux is set to 'auto' then it will be assigned according to the relationship
#    to other fluxes hard coded for that river boundary in stage_discharge.pl. 
#    (There is a federally mandated relationship between the flux in the 
#     Mississippi and the flux in the Atchafalaya coded into stage_discharge.pl).
#
# Example: VARFLUX=steadybc
#          VARFLUXBC="mississippiRiverBoundaryCondition=600kcfs, atchafalayaRiverBoundaryCondition=auto"  
#    or
#          VARFLUXBC="mississippiRiverBoundaryCondition=17kcms, atchafalayaRiverBoundaryCondition=11.3kcms" 
#    or
#          VARFLUXBC="mississippiRiverBoundaryCondition=17kcms, atchafalayaRiverBoundaryCondition=auto" 
#
# 3. Configure steady river *stage* in asgs config file and let asgs 
#    set steady flux via stage-discharge curve in fort.15
#
# Example: VARFLUX=steadybc
#          VARFLUXBC="mississippiRiverBoundaryCondition=10m, atchafalayaRiverBoundaryStage=auto" 
#    or
#          VARFLUXBC="mississippiRiverBoundaryCondition=32.8ft, atchafalayaRiverBoundaryStage=auto"
#
# 4. Let asgs get the stage and/or discharge via web service, compute 
#    the discharge, and create a fort.20 for every cycle; for type 52
#    boundaries this option requires the ASGS to change the EtaDisc 
#    in the hotstart file according to the stage discharge curve at
#    the boundary. 
#
# Example: VARFLUX=steadybc
#          VARFLUXBC="mississippiRiverBoundaryCondition=gage, atchafalayaRiverBoundaryStage=auto"
#
# 5. fort.20 files ready-to-use : this is used for the North Carolina
#    rivers where OU/NSSL produce fort.20 files directly
#
# Example: VARFLUX=nssl.ou.fort.20
#
#  This was the default behavior in the past when VARFLUX=on. Setting
#  VARFLUX=on is now deprecated. 
#
#  In each of the above cases, the flux will be constant over the course
#  of a forecast, but can be reset for each nowcast (type 52 boundaries
#  require knowledge of stage discharge curve at the boundary in order
#  for stepwise reset to work properly
#
# 6. Future work: use real time river forecast data to create the fort.20 file;
#    for type 52 boundaries this will require implementing the stage discharge
#    curve into ADCIRC itself.
#------------------------------------------------------------------------
use strict;
use warnings;
use Getopt::Long;
use Date::Calc;
use Date::Handler;
#
sub stderrMessage($$);
#
my $inputdir = "INPUTDIRnotset"; # directory where mesh/input files are stored
our %properties; # key/value pairs _read from_ properties files
my $boundaryFile = "nullboundaryfile";  # produced by boundaryFinder.f90
my $varflux;  # asgs configuration value from run.properties that 
               # specifies the type of river flux forcing boundary condition 

my $varfluxbc; # asgs configuration value from run.properties that 
                 # specifies the boundary condition for each boundary, 
                 # key/value pairs separated by commas and enclosed in
                 # a single set of quotation marks
my @fluxbcs;    # $varfluxbc after stripping quotation marks and breaking on commas
my @boundaryNames; # used for labelling lines output to fort.20 file
my @riverBoundaryName; # name of river boundary parsed out of property key
my $bcindex;  # counter for boundary conditions
my $csdate; # yyyymmddhh24 corresponding to ADCIRC cold start
my $hstime; # hotstart time in seconds since adcirc cold start
my @bcproperties; # 
my $outputFile = "fort.20";
my $fluxProfile = "proportional";
my @supported_profiles = qw(uniform proportional);
my $inputFluxUnits = "null";
my @supported_units = qw(cfs kcfs cms kcms m ft);
my $meshfile = "fort.14" ; # only needed so we can form the name of the properties file
my $numNodes = -999; # the number of nodes on a boundary
my $numBoundaries = 0; # total number of flux boundaries in the file 
my $sumDepths = -999.0; # the sum of the depth values at each node on the boundary (m)
my $sumLengths = -999.0; # the sum of the distances corresponding to each node on the boundary (m)
my $dep = -999.0;       # depth at an individual boundary node
my $dist = -999.0;      # distance along the boundary that a node's flux applies to (m)
my $fluxFraction = -999.0; # fraction of the total flux on the boundary attributed to a particular node 
my @nodalFluxPerUnitWidth = -999.0; # flux boundary condition at a node ((m^3/s)/m)
#
GetOptions(
           "csdate=s" => \$csdate,
           "hstime=s" => \$hstime,
           "inputdir=s" => \$inputdir,
           "meshfile=s" => \$meshfile,
           "flux-profile=s" => \$fluxProfile,
           "outputfile=s" => \$outputFile
           );

#-----------------------------------------------------------------
#
#                 C H E C K   I N P U T  
#
#-----------------------------------------------------------------
# check to see if the fluxProfile string is a supported value;
# two possible values are "uniform" and "proportional"
unless (is_member($fluxProfile,@supported_profiles)) {
   my $sf = join(",",@supported_profiles);
   &stderrMessage("ERROR","The specified unit ('$fluxProfile') is not among the supported types: $sf."); 
}
#-----------------------------------------------------------------
#
#       R E A D   M E S H   P R O P E R T I E S   F I L E
#
#-----------------------------------------------------------------
# read the mesh.properties file to find how many river flux 
# boundaries are in the mesh file, and in what order
if ( -e "$meshfile.properties" ) { 
   unless (open(MP,"<$meshfile.properties")) {
      &stderrMessage("ERROR","Found the $meshfile.properties file but could not open it: $!.");
      die;
   }
} else {
   &stderrMessage("ERROR","The $meshfile.properties file was not found.");
   die;
}
# look for property keys that contain the string "RiverBoundary" and 
# load them into a hash; example properties:
#
#    mississippiRiverBoundaryMeshFilePosition : 1
#    mississippiRiverBoundaryType : 52
#    mississippiRiverBoundaryNominalWSE : 0.0
#
while (<MP>) {
   if ($_ =~ /RiverBoundary/) {
      &loadProperty($_);
   }
}
close(MP);
my @riverBoundaryNames;
#
# parse out the names of the river boundaries and put them into a list 
# in order (the value of the property is the 1-indexed order in the list)
while (my ($key, $value) = each(%properties)) {
   #stderrMessage("DEBUG","key is $key and value is $value");
   if ( $key =~ /(\w+)RiverBoundaryMeshFilePosition/ ) {
      $riverBoundaryNames[$value-1] = $1;
      $numBoundaries++;
   }
}
#-----------------------------------------------------------------
#
#       R E A D   R U N . P R O P E R T I E S   F I L E
#
#-----------------------------------------------------------------
# read the run.properties file to determine boundary condition 
# specification for rivers - these specifications were placed
# there by asgs_main.sh using values from the ASGS config file
if ( -e "run.properties" ) { 
   unless (open(RP,"<run.properties")) {
      &stderrMessage("ERROR","Found the run.properties file but could not open it: $!.");
      die;
   }
} else {
   &stderrMessage("ERROR","The run.properties file was not found.");
   die;
}
#
# look for property keys that contain the string "RiverBoundary" and 
# load them into a hash; example properties:
#
# Example: varflux : steadybc
#          varfluxbc : "mississippiRiverBoundaryCondition : 10m, atchafalayaRiverBoundaryCondition : auto"
#
while (<RP>) {
   if ($_ =~ /varflux/) {
      &loadProperty($_);
   }
   if ($_ =~ /RiverBoundary/) {
      &loadProperty($_);
   }
}
close(RP);
$varflux = $properties{"varflux"};
#
# if the varflux property is set to "steadybc" then the value may be
# a comma separated list of values and we need to extract these values
if ($varflux eq "steadybc") { 
   # iterate through the run.properties file to find the boundary conditions
   $varfluxbc = $properties{"varfluxbc"};
   # get rid of the quotation marks, if any
   $varfluxbc =~ s/"//g;    
   $varfluxbc =~ s/'//g;
   # split into a list of key/value pairs
   @fluxbcs = split(",",$varfluxbc);
} else {
   &stderrMessage("ERROR","The river flux boundary condition type is set to $varflux but this value was not recognized.");
   die;
}
#-----------------------------------------------------------------
#
#         P R O C E S S   B O U N D A R Y   C O N D I T I O N S
#
#         F R O M   R U N . P R O P E R T I E S    F I L E
#
#-----------------------------------------------------------------
# parse and process each of the river boundary conditions; if a boundary is
# set to a stage value then convert this to a flux in units of kcms; 
# if boundary is set to auto then set the flux appropriately
my @bcvalues;
my @bcunits;
my $meshFilePosition;
my @bcBoundaryEqualsValue;
my $autoFluxWasSpecified = 0;
foreach my $bc (@fluxbcs) { 
   print "$bc\n";
   @bcBoundaryEqualsValue = split(":",$bc);
   # strip leading and trailing spaces
   foreach my $props (@bcBoundaryEqualsValue) {
      $props =~ s/^\s+//g;
      $props =~ s/\s+$//g; 
   }
   print "'$bcBoundaryEqualsValue[1]'\n";
   # parse out the name of the associated river boundary
   $bcBoundaryEqualsValue[0] =~ /(\w+)RiverBoundaryCondition/;
   # determine order of this boundary condition as it would appear in the mesh file
   my $meshFilePositionKey = $1 . "RiverBoundaryMeshFilePosition";
   $meshFilePosition = $properties{$meshFilePositionKey};
   # save the name of this river
   $riverBoundaryName[$meshFilePosition] = $1;
   #-----------------------------------------------------------------
   #
   #    A U T O   B O U N D A R Y   C O N D I T I O N   S E T U P
   #
   #-----------------------------------------------------------------
   if ( $bcBoundaryEqualsValue[1] eq "auto" ) { 
      $bcvalues[$meshFilePosition-1] = "auto";
      $bcunits[$meshFilePosition-1] = "auto";
      # check to see if we have an automatic relationship coded for this
      # river; the atchafalaya is the only river known to have an automated
      # relationship (Federal law mandates that the Atchafalaya contain
      # one third of the flow from the upper Mississippi while the lower
      # Mississippi river receives two thirds of the flow)
      unless ( $riverBoundaryName[$meshFilePosition] =~ /atchafalaya/) {
         stderrMessage("ERROR","The river '$riverBoundaryName[$meshFilePosition]' has its boundary condition value set to 'auto' but the river flux for this river does not have an automatic relationship to any other river flow.");
         die;
      }
      $autoFluxWasSpecified = 1;
      # Since we don't know whether this river will come before or after
      # the MS river, we may or may not have the MS river flux value at this
      # point. As a result, we have to wait until the MS river flux has 
      # been computed, at the end of the foreach $bc loop below.
   }
   #-----------------------------------------------------------------
   #
   #         G A G E   B O U N D A R Y   C O N D I T I O N
   #
   #-----------------------------------------------------------------
   if ( $bcBoundaryEqualsValue[1] eq "gage" ) {
      #
      #  Example properties:
      #    mississippiRiverBoundaryStageGageName : "Mississippi River at Baton Rouge"
      #    mississippiRiverBoundaryStageGageID : 01160
      #    mississippiRiverBoundaryStageGageAgency : USACE
      #
      # determine whether it is a stage or discharge gage
      my $gageDataType = "Stage"; 
      if ( defined $properties{"$riverBoundaryName[$meshFilePosition]RiverBoundaryDischargeGageAgency"} ) {
         # true in the rare case where the gage provides discharge info
         $gageDataType = "Discharge";         
      }
      # only USACE web service is currently supported
      my $gageDataTypeKey = $riverBoundaryName[$meshFilePosition] . "RiverBoundary" . $gageDataType . "GageAgency"; 
      #unless ( $properties{$gageDataType} =~ /USACE/ ) {
      #   stderrMessage("ERROR","The river boundary property '$gageDataTypeKey' is set to $properties{$gageDataTypeKey} but only USACE gages are supported.");
      #   die;
      #}
      # look for the mesh properties related to getting data from gages for this boundary
      my $gageIDKey = $riverBoundaryName[$meshFilePosition] . "RiverBoundary" . $gageDataType . "GageID";
      my $gageID = $properties{$gageIDKey};
      #
      # determine GMT start date and time from coldstart date and hotstart time
      $csdate=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
      my $cy = $1;
      my $cm = $2;
      my $cd = $3;
      my $ch = $4;
      my $cmin = 0.0;
      my $cs = 0.0;
      # determine the date and time at the start of the simulation (in GMT)
      (my $hy, my $hm, my $hd, my $hh, my $hmin, my $hs) =
       Date::Calc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,0,0,0,$hstime);
      # determine time zone of the gage data
      my $gageTimeZoneKey = $riverBoundaryName[$meshFilePosition] . "RiverBoundary" . $gageDataType . "TimeZone";
      my $gageTimeZone = $properties{$gageTimeZoneKey};
      # use Date::Handler to convert the date/time to the time zone of the
      # gage for use in data acquisition
      #time_zone => 'GMT',
      my $date = new Date::Handler({ date => { 
         year => $hy, month => $hm, day => $hd, hour => $hh, min => $hmin, sec => $hs, } }, time_zone => 'GMT' );
      print "Greenwich Mean Time: ".$date->LocalTime()."\n";
      $date->TimeZone($gageTimeZone);
      print "$gageTimeZone Time: ".$date->LocalTime()."\n";      
      #
      # retrieve xml via curl
      my $stageURL = "http://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfm?meth=getValues&location=$gageID&variable=HG&beginDate=2016-08-21T00:00&endDate=2016-08-21T00:00&authToken=RiverGages";
      # discharge gage only produces one value per day, so we must ask for one full
      # day of data to get one value #'http://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfm?meth=getSiteInfo&site=',station,'&authToken=RiverGages'],'Gtestsiteinfo.xml'
      my $dischargeURL = "http://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfm?meth=getValues&location=$gageID&variable=QR&beginDate=2016-08-21T00:00&endDate=2016-08-21T23:00&authToken=RiverGages";      
      my @resultXML;
      stderrMessage("DEBUG","$riverBoundaryName[$meshFilePosition]");
      stderrMessage("DEBUG","gageDataType is '$gageDataType'");
      if ( $gageDataType eq "Stage" ) {
         stderrMessage("DEBUG","stageURL is '$stageURL'");
         @resultXML = `curl '$stageURL'`;
      } else {
         @resultXML = `curl '$dischargeURL'`;
      }         
      my $gageValue = "null";
      my $gageUnits = "null";
      foreach my $line (@resultXML) {
         stderrMessage("DEBUG","line is '$line'");
         # gage value
         # <value dateTime="2016-08-21T00:00:00">18.000</value>
         # <value dateTime="2016-08-21T09:00:00">183000.000</value>
         if ( $line =~ /<value.*>([0-9]+\.[0-9]+)<\/value>/ ) {
             $gageValue = $1;
         }
         # gage units
         # <units unitsAbbreviation="cfs" unitsCode="35">CFS</units>
         # <units unitsAbbreviation="ft" unitsCode="48">Feet</units>
         if ( $line =~ /<units unitsAbbreviation="(\w+)"/ ) {
            $gageUnits = $1;
         }
      }
      &stderrMessage("DEBUG","gageValue is $gageValue");
      &stderrMessage("DEBUG","gageUnits is $gageUnits");
      unless ( $gageValue ne "null" && $gageUnits ne "null" ) {
         &stderrMessage("ERROR","Could not retrieve gage value and/or units via web service.");
         die;
      }
      $bcBoundaryEqualsValue[1] = $gageValue . $gageUnits;
      &stderrMessage("DEBUG","bcBoundaryEqualsValue[1] is $bcBoundaryEqualsValue[1]");  
   }
   #-----------------------------------------------------------------
   #
   #      C O N S T A N T   B O U N D A R Y   C O N D I T I O N
   #
   #-----------------------------------------------------------------      
   # handle results from gage boundary condition or constant specified
   # boundary condition
   if ( $bcBoundaryEqualsValue[1] ne "auto" ) {
      # parse value to find the number and its units
      $bcBoundaryEqualsValue[1] =~ /([0-9]*\.*[0-9]*)(\w+)/;
      $bcvalues[$meshFilePosition-1] = $1;
      $bcunits[$meshFilePosition-1] = $2;
      #
      # check to see if we support the input flux units; if so, set the
      # right units conversion value
      unless ( is_member($bcunits[$meshFilePosition-1],@supported_units)) {
         my $sf = join(",",@supported_units);
         &stderrMessage("ERROR","The specified unit ('$bcunits[$bcindex]') is not among the supported types: $sf (where ft and m require a known stage discharge relationship)."); 
         die;
      }
      if ( $bcunits[$meshFilePosition-1] eq "cfs" ) {
         $bcvalues[$meshFilePosition-1] = $bcvalues[$meshFilePosition-1] / ( 35.3147 * 1000.0 );
      }
      if ( $bcunits[$meshFilePosition-1] eq "kcfs" ) {
         $bcvalues[$meshFilePosition-1] = $bcvalues[$meshFilePosition-1] / 35.3147;
      }
      if ( $bcunits[$meshFilePosition-1] eq "cms" ) {
         $bcvalues[$meshFilePosition-1] = $bcvalues[$meshFilePosition-1] / 1000.0;
      }
      if ( $bcunits[$meshFilePosition-1] eq "ft" || $bcunits[$meshFilePosition-1] eq "m" ) {
         # TODO: implement stage discharge
         if ( $bcunits[$meshFilePosition-1] eq "ft" ) {
            # convert ft to m if necessary
            $bcvalues[$meshFilePosition-1] = $bcvalues[$meshFilePosition-1] * 0.3048;
         }
         # parse name of stage discharge relationship from mesh.properties,
         # for example:    
         # mississippiRiverBoundaryStageDischargeRelationshipName : "Mississippi River at Baton Rouge"
         my $stageDischargeName = "null";
         my $stageDischargeNameFound = 0;   
         &stderrMessage("DEBUG","Looking for the key '$riverBoundaryNames[$meshFilePosition-1]RiverBoundaryStageDischargeRelationshipName'");
         while (my ($key, $value) = each(%properties)) {
             print "'$key'\n";
            if ( $key =~ /$riverBoundaryNames[$meshFilePosition-1]RiverBoundaryStageDischargeRelationship/ ) {
               $stageDischargeName = $value;
               $stageDischargeNameFound = 1; 
            }
         }
         if ( $stageDischargeNameFound == 1 ) {        
            # call subroutine with stage in m to get discharge in kcms
            $bcvalues[$meshFilePosition-1] = &getDischarge($bcvalues[$meshFilePosition-1], $stageDischargeName);
         } else {
            &stderrMessage("ERROR","Could not find stage discharge curve named '$stageDischargeName' in the mesh properties file named '$meshfile.properties'.");
            die;
         }
      }
   }
}
#-----------------------------------------------------------------
#
#    A U T O   B O U N D A R Y   C O N D I T I O N   F I N I S H
#
#-----------------------------------------------------------------
# If the Atchafalaya river is present, and its boundary flux was specified
# as "auto" in the ASGS config, then compute the Atchafalaya flux now, 
# setting it to half the MS river flux (and therefore one third of the 
# MS river flux upstream of the control structure)
if ( $autoFluxWasSpecified == 1 ) {
   # The bcvalues array is 0-indexed, while the mesh file position of the
   # boundaries is 1-indexed; this is why we subtract 1 from the mesh file
   # position to get the bcvalues array index.
   $bcvalues[$properties{"atchafalayaRiverBoundaryMeshFilePosition"}-1] = 0.5 * $bcvalues[$properties{"mississippiRiverBoundaryMeshFilePosition"}-1];
}
#-----------------------------------------------------------------
#
#   I N I T I A L I Z E   F O R T . 2 0   O U T P U T   F I L E 
#
#-----------------------------------------------------------------
# open the output file; output to be written in the fort.20 format
unless (open(OUT,">$outputFile")) {
   &stderrMessage("ERROR","Could not open the nodal flux per unit width output file '$outputFile': $!.");
   die;
}
#
# write a very large time increment to the top of the fort.20 file; this
# is to make sure that even a long simulation will have plenty of river 
# flux data when we only have two data sets (this creates a steady river
# flux)
printf OUT "31536000  ! number of seconds in a year\n";
#
# check to see if the boundaryFile exists; if it does, open it
$boundaryFile = $meshfile . ".inflow_flux_boundaries_lengthsdepths.txt";
if ( -e $boundaryFile ) { 
   unless (open(BF,"<$boundaryFile")) {
      &stderrMessage("ERROR","Found the boundary information '$boundaryFile' but could not open it: $!.");
      die;
   }
} else {
   &stderrMessage("ERROR","The boundary information file ('$boundaryFile') was not found.");
   die;
}
#-----------------------------------------------------------------
#
#          R E A D   B O U N D A R Y   F I L E 
#
#   C O M P U T E   F L U X   P E R   U N I T   W I D T H
#
#-----------------------------------------------------------------
# read in the boundaryFile data and compute the flux per unit width 
# using the specified stage/discharge relationship and the specified
# (or default) fluxProfile method
my @totalEffDepth;
my @totalLength;
my @numNodes;  # number of nodes on each boundary
my @ibtype;    # boundary type for each boundary (integer)
my $boundaryCounter = 0;
my $nodeCounter = 0;
while(<BF>) { 
   #
   # read the number of nodes and the boundary type for this boundary 
   my @fields = split(" ",$_); # numNodes ibtype
   $numNodes[$boundaryCounter] = $fields[0];  
   $ibtype[$boundaryCounter] = $fields[1];
   #
   # read the depths at each node on this boundary and the distances 
   # between them; write the corresponding flux per unit width to the
   # fort.20 file
   @fields = split(" ",<BF>); # totalEffDepth(m) totalLength(m)
   $totalEffDepth[$boundaryCounter] = $fields[0];
   $totalLength[$boundaryCounter] = $fields[1];
   if ( $fluxProfile eq "uniform" ) {
      $fluxFraction = 1.0 / $totalLength[$boundaryCounter];
   }
   for (my $i=0; $i<$numNodes[$boundaryCounter]; $i++ ) {
      @fields = split(" ",<BF>); # effDepth(m) effLength(m)
      $dep = $fields[0];  
      $dist = $fields[1]; 
      if ( $fluxProfile eq "proportional" ) {
         $fluxFraction = ( $dep / $totalEffDepth[$boundaryCounter] ) / $dist;
      }
      # multiply by 1000.0 to convert from kcms to cms     
      $nodalFluxPerUnitWidth[$nodeCounter] = $bcvalues[$boundaryCounter] * $fluxFraction * 1000.0;
      # record the name of the river associated with this node for use in 
      # comments written to the fort.20 file
      $boundaryNames[$nodeCounter] = $riverBoundaryNames[$boundaryCounter];
      $nodeCounter++;
   }
   $boundaryCounter++;
}
#-----------------------------------------------------------------
#
#       W R I T E   F O R T . 2 0   O U T P U T   F I L E 
#
#-----------------------------------------------------------------
# Now write the full set of flux per unit width values twice: one data
# set to represent the simulation start time and another set to represent the
# simulation end time. This assumes a constant flux.
# Include an extra 0.0 on every line in case these values are cut and pasted
# into the fort.15 as a periodic boundary condition. 
for (my $j=0; $j<2; $j++ ) {
   my $thisNodeCounter = 1;
   my $boundaryCounter = 0;
   for (my $i=0; $i<$nodeCounter; $i++ ) { 
      if ( $thisNodeCounter > $numNodes[$boundaryCounter] ) {
         $thisNodeCounter = 1;
         $boundaryCounter++;
      }
      my $formattedTotalFlux = sprintf("%.1f",$bcvalues[$boundaryCounter]);
      my $formattedNodalFlux = sprintf("%.1f",$nodalFluxPerUnitWidth[$i]);
      if ($thisNodeCounter == 1) {
         printf OUT "$formattedNodalFlux 0.0   ! $boundaryNames[$i] node $thisNodeCounter; total flux set to $formattedTotalFlux kcms\n";
      } else {
         printf OUT "$formattedNodalFlux 0.0  ! $boundaryNames[$i] node $thisNodeCounter\n";         
      }      
      $thisNodeCounter++;
   }   
}
#
# close boundary input file and fort.20 output file
close(BF);
close(OUT);
#-----------------------------------------------------------------
#
#       W R I T E   B O U N D A R Y   C O N D I T I O N S   
#
#        T O   R U N . P R O P E R T I E S   F I L E 
#
#-----------------------------------------------------------------
# open run.properties file to write boundary conditions (flux) specifications
# for rivers
if ( -e "run.properties" ) { 
   unless (open(RP,">>run.properties")) {
      &stderrMessage("ERROR","Found the run.properties file but could not open it: $!.");
      die;
   }
} 
for (my $i=0; $i<$numBoundaries; $i++ ) {
   my $formattedTotalFlux = sprintf("%.1f",$bcvalues[$i]);
   my $totalFluxProperty = "$riverBoundaryNames[$i]RiverADCIRCBoundaryCondition : $formattedTotalFlux" . "kcms"; 
   printf RP $totalFluxProperty . "\n";
}
close(RP);


#-----------------------------------------------------------------
#
#       F U N C T I O N     G E T   D I S C H A R G E
#
#-----------------------------------------------------------------
# Take a river stage value in m and convert to discharge in kcms.
sub getDischarge {
   my $stage = $_[0];
   my $dname = $_[1];
   my $discharge = 0.0;
   #
   my @stages;
   my @discharges;
   my $stageDischargeFound = 0;
   # TODO: only one stage discharge curve at the moment
   if ( $dname =~ /Mississippi River at Baton Rouge/ ) {
      # The values for the MS river at Baton Rouge were taken from the red 
      # line in Figure 3 of the following reference by Rose Martyr, et al: 
      # Simulating Hurricane Storm Surge in the Lower Mississippi River under
      # Varying Flow Conditions, DOI: 10.1061/(ASCE)HY.1943-7900.0000699
      stderrMessage("INFO","Stage discharge relationship was found.");
      @stages =     (1.3, 3.4,  5.5,  7.5,  9.1, 11.0, 12.3);   # in m above NAVD88
      @discharges = (4.9, 9.2, 14.0, 18.8, 23.0, 27.6, 32.6);   # in kcms
      $stageDischargeFound = 1;
   }
   if ( $stageDischargeFound == 1 ) {
      if ( $stage<$stages[0] ) {
         stderrMessage("WARNING","River stage of $stage meters is lower than the lowest value in the stage/discharge curve. Setting river stage to lowest available value of $stages[0].");
         $stage = $stages[0];
         $discharge = $discharges[0];
      } elsif ( $stage > $stages[-1] ) {
         # if the specified river stage is higher than the highest value of
         # the stage discharge curve 
         # extrapolate the radius
         stderrMessage("WARNING","Specified river stage of $stage meters is higher than the highest river stage in the stage discharge curve. Setting the stage to the highest value in the stage discharge curve: $stages[-1].");
         $stage = $stages[-1];
         $discharge = $discharges[-1];
      } elsif ( $stage >= $stages[0] && $stage <= $stages[-1]) {
         # stage is within our stage discharge curve, find the values that bracket
         # it and perform linear interpolation
         my $npoints=@stages;
         for ( my $i=0; $i<=($npoints-2); ++$i ) {
            if ( $stage >= $stages[$i] && $stage <= $stages[$i+1] ) {
                $discharge = (($stage - $stages[$i]) / ($stages[$i+1] - $stages[$i]) )
                   * ($discharges[$i+1] - $discharges[$i])
                   + $discharges[$i];
            }
         }
      } else { 
         stderrMessage("ERROR","Failed to interpolate discharge at river stage of $stage meters using the stage/discharge curve '$dname'.");
      }
   } else {
      &stderrMessage("ERROR", "The stage discharge relationship '$dname' was not recognized.");
      die;
   }
   return $discharge;
}
#-----------------------------------------------------------------


#-----------------------------------------------------------------
#
#       F U N C T I O N     L O A D   P R O P E R T Y 
#
#-----------------------------------------------------------------
# take a string that consists of a key and value separated 
# by a colon (:) that has been read from a properties file and load it
# into a hash  
sub loadProperty {
   my $propertyString = $_[0];
   #stderrMessage("DEBUG","propertyString is $propertyString");
   my $k;  # property key
   my $v;  # property value
   my $colon = index($propertyString,":");
   #($k,$v)=split(':',$_);
   $k = substr($propertyString,0,$colon);
   $v = substr($propertyString,$colon+1,length($propertyString));
   chomp($k);
   chomp($v);
   # remove leading and trailing whitespaces from the key and value
   $k =~ s/^\s+//g;
   $k =~ s/\s+$//g; 
   $v =~ s/^\s+//g;
   $v =~ s/\s+$//g; 
   &stderrMessage("DEBUG","loadProperty: key is '$k' and value is '$v'");
   $properties{$k}=$v;
}
#
# General subroutine used to test if an element is already in an array
sub is_member {
  my $test = shift;
  my $ret = 0;
  if (defined($test)) {
     # This way to test if something is a member is significantly faster
     # ..thanks, PM!
     if (grep {$_ eq $test} @_) {
        $ret++;
     }
  }
  return $ret;
}
#-----------------------------------------------------------------


#-----------------------------------------------------------------
#
#       F U N C T I O N     S T D E R R   M E S S A G E 
#
#-----------------------------------------------------------------
sub stderrMessage ($$) {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $theTime = "[$year-".sprintf("%3s",$months[$month])."-".sprintf("%02d",$dayOfMonth)."-T".sprintf("%02d",$hour).":".sprintf("%02d",$minute).":".sprintf("%02d",$second)."]";
   printf STDERR "$theTime $level: set_flux.pl: $message\n";
}
#-----------------------------------------------------------------

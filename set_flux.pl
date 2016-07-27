#!/usr/bin/perl
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
# Operator's choices in specifying river flux forcing:
#
# 0. hard code steady fluxes in fort.15 template 
# 1. configure steady fluxes via asgs config file (how to do this 
#    in a general way?) and let asgs propagate to fort.15 ... this
#    can be different for different ensemble members
# 2. configure steady river *stage* in asgs config file and let asgs 
#    set steady flux via stage-discharge curve in fort.15
# 3. configure the gage station where asgs should get the stage, and 
#    perhaps the corresponding date, and let asgs get the stage, 
#    compute the discharge, and set it in fort.15; if performed
#    dynamically for type 52 boundary, it will require changing the 
#    EtaDisc in the hotstart file according to stage discharge curve
# 4. same as 1 but time series of discharges (requires stage-discharge code in ADCIRC)
# 5. same as 2 but time series of stages (requires stage-discharge code in ADCIRC)
# 6. fort.20 files ready-to-use
#
# Constraints / Assumptions
#
# 0. boundary must be placed in location where we have a stage discharge
#    curve available if stage is to be used
# 1. asgs should only use fort.20 to set fluxes, never fort.15
# 2. operator only needs to touch asgs config
# 3. must allow different fluxes for different ensemble members
# 
#
#
#
# If VARFLUX=on then ASGS looks at another parameter: FLUXSOURCE
#
# if [[ $FLUXSOURCE = nssl.ou.fort.20 ]]; then
#     # these are hindcast and forecast fort.20 files provided by hydrologic model
# fi
# if [[ $FLUXSOURCE = boundary.properties ]]; then
#     # the asgs config file has code in it to write the boundaries.properties file
# fi
# 
#
#
#
# The flux boundary condition is set via the following algorithm:
#
# 0. Add properties from mesh.properties file manually when the mesh 
#    is installed in ASGS, for example:
#
#    mississippiRiverBoundaryMeshFilePosition : 1
#    atchafalayaRiverBoundaryMeshFilePosition : 2
#    mississippiRiverBoundaryType : 52
#    atchafalayaRiverBoundaryType : 52
#
# 1. When the mesh is installed in ASGS, run the boundaryFinder.f90
#    program to create a boundaries xyz file; example invokation is as follows:
#
# ~/asgs/master/util/mesh/boundaryFinder.x --meshfile HSDRRS2014_MRGO_leveeupdate_fixSTC_MX.grd --outputfile boundaries_xyz.txt --boundarytype inflow_flux --xyz
#
# The boundary data are produced in the following example format:
#
# 19 52
#     -91.1940310      30.4222130       8.5000000
#     -91.1945900      30.4225390      10.4000000
#     -91.1950730      30.4228250      12.2600000
#     -91.1955410      30.4230930      14.1200000
#     -91.1961210      30.4234320      13.0400000
#     -91.1966730      30.4237520      13.8631070
#     -91.1972630      30.4240960      16.9561900
#     -91.1979580      30.4245010      16.6091390
#     -91.1985320      30.4248380      16.8379350
#     -91.1991150      30.4251810      15.9140560
#     -91.1996650      30.4255000      14.6089820
#     -91.2001900      30.4258020      12.0605730
#     -91.2007380      30.4261190       9.4094670
#     -91.2012640      30.4264270       7.2902380
#     -91.2017640      30.4267210       5.8460780
#     -91.2022920      30.4270240       2.9112060
#     -91.2028160      30.4273340       2.6700000
#     -91.2033510      30.4276410       2.5700000
#     -91.2038910      30.4279610       2.5700000
# 13 52
#     -91.7997400      30.9802500       7.2250000
#     -91.8004550      30.9802350       7.2250000
#     -91.8011700      30.9802200       7.2250000
#     -91.8018850      30.9802050       7.2250000
#     -91.8027340      30.9801800       7.2250000
#     -91.8037000      30.9801170       7.2250000
#     -91.8048000      30.9800450       7.2250000
#     -91.8059000      30.9799730       7.2250000
#     -91.8070000      30.9799000       7.2250000
#     -91.8078700      30.9798700       7.2250000
#     -91.8087400      30.9798400       7.2250000
#     -91.8096100      30.9798100       7.2250000
#     -91.8104800      30.9797800       7.2250000
#
# 2. the stage_discharge.pl script 
#    a. reads the mesh.properties file to find how many river flux 
#       boundaries are in the mesh file, and in what order; 
#    b. it either uses statically configured total fluxes from
#       the control.properties or run.properties file, or
#    c. uses a web service to get the relevant water surface elevation
#    d. if using water surface elevation, it interpolates a stage
#       discharge curve to get the total flux at each boundary
#    e. if a web service was used, the total flux at each boundary is 
#       written to the run.properties file, as in the following example:
#
#    mississippiRiverBoundaryConditionFlux : 16kcms
#    mississippiRiverBoundaryPeriodicity : periodic
#    atchafalayaRiverBoundaryConditionFlux : 8kcms
#    atchafalayaRiverBoundaryPeriodicity : periodic
#
# 3. set_flux.pl 
#    a. reads the control.properties file to determine the 
#       flux(es) at the boundaries, and periodicity (periodic boundaries, 
#       e.g. steady boundaries, are specified in the fort.15 while 
#       aperiodic boundaries are specified in the fort.20) ... only periodic
#       (steady) boundaries are currently supported; 
#    b. reads the boundaries_xyz.txt file to find the edge lengths and 
#       depths at each node;
#    c. computes the flux per unit widths along the boundary nodes;
#    d. writes out the periodic (steady) flux per unit width(s) suitable
#       for direct inclusion in the fort.15 file
#
#------------------------------------------------------------------------
use strict;
use warnings;
use Getopt::Long;
#
sub stderrMessage($$);
#
my $boundaryFile = "boundaries_xyz.txt";
my @inputFluxValues;
my $outputFile = "periodicFluxPerUnitWidth.txt";
my $fluxProfile = "proportional";
my @supported_profiles = qw(uniform proportional);
my $inputFluxUnits = "kcms";
my @supported_units = qw(m^3/s m^3s^-1 cfs kcfs cms kcms);
my $numNodes; # the number of nodes on a boundary
my $sumDepths; # the sum of the depth values at each node on the boundary (m)
my $sumLengths; # the sum of the distances corresponding to each node on the boundary (m)
my $dep;       # depth at an individual boundary node
my $dist;      # distance along the boundary that a node's flux applies to (m)
my $fluxFraction; # fraction of the total flux on the boundary attributed to a particular node 
my $nodalFluxPerUnitWidth; # flux boundary condition at a node ((m^3/s)/m)
#
GetOptions(
           "input-flux-values=s" => \@inputFluxValues,
           "boundaryfile=s" => \$boundaryFile,
           "flux-profile=s" => \$fluxProfile,
           "input-flux-units=s" => \$inputFluxUnits,
           "outputfile=s" => \$outputFile
           );
#
# check to see if we support the input flux units; if so, set the
# right units conversion value
unless ( is_member($inputFluxUnits,@supported_units)) {
   my $sf = join(",",@supported_units);
   &stderrMessage("ERROR","The specified unit ('$inputFluxUnits') is not among the supported types: $sf."); 
}
my $fluxUnitConversion = 1.0;
if ( $inputFluxUnits eq "cfs" ) {
   $fluxUnitConversion = 1.0/35.3147;
}
if ( $inputFluxUnits eq "kcfs" ) {
   $fluxUnitConversion = 1000.0/35.3147;
}
#
# check to see if the fluxProfile string is a supported value;
# two possible values are "uniform" and "proportional"
unless (is_member($fluxProfile,@supported_profiles)) {
   my $sf = join(",",@supported_profiles);
   &stderrMessage("ERROR","The specified unit ('$fluxProfile') is not among the supported types: $sf."); 
}
#
# check to see if the boundaryFile exists; if it does, open it
if ( -e $boundaryFile ) { 
   unless (open(BF,"<$boundaryFile")) {
      &stderrMessage("ERROR","Found the boundary information '$boundaryFile' but could not open it: $!.");
      die;
   }
} else {
   &stderrMessage("ERROR","The boundary information file ('$boundaryFile') was not found.");
   die;
}
#
# open the output file; output to be written in the fort.15 format
unless (open(OUT,">$outputFile")) {
   &stderrMessage("ERROR","Could not open the nodal flux per unit width output file '$outputFile': $!.");
   die;
}
#
# break up the flux input and see how many flux values we have
@inputFluxValues = split(/,/,join(',',@inputFluxValues));
my $numInputFluxValues = @inputFluxValues;
my $inputFluxString;
foreach my $val (@inputFluxValues) {
   $inputFluxString .= $val . " ";
}
#&stderrMessage("DEBUG","The input $numInputFluxValues input flux values are '$inputFluxString'.");
#
# read in the boundaryFile data and write to the outputFile, using
# the specified fluxProfile method
my $agrid = <BF>;
chomp($agrid);
my @fields = split(" ",<BF>);
my $btype = $fields[0];
@fields = split(" ",<BF>);
my $numBoundaries = $fields[0];
if ( $numInputFluxValues != $numBoundaries ) {
   &stderrMessage("ERROR","There are $numBoundaries boundaries in the boundary information file '$boundaryFile' but $numInputFluxValues input flux value(s) was/were provided on the command line. The number of input flux values must match the number of boundaries.");
}
my $comment = " ! $agrid, boundary type: $btype,";  
for (my $i=0; $i<$numBoundaries; $i++ ) {
   @fields = split(" ",<BF>);
   $numNodes = $fields[1];
   $sumDepths = $fields[2];
   $sumLengths = $fields[3];
   $comment .= " boundary start";
   for (my $j=0; $j<$numNodes; $j++ ) {
      @fields = split(" ",<BF>);
      $dep = $fields[1]; # nodeNum effDepth(m) effLength(m) 
      $dist = $fields[2]; 
      if ( $fluxProfile eq "uniform" ) {
         $fluxFraction = 1.0 / $sumLengths;
         if ( $j == 0 || $j == $numNodes-1 ) {
            $fluxFraction *= 0.5;
         }
      }
      if ( $fluxProfile eq "proportional" ) {
         $fluxFraction = ( $dep / $sumDepths ) / $dist;
      }
      #&stderrMessage("DEBUG","inputFluxValues[$i]=$inputFluxValues[$i]\n");
      $nodalFluxPerUnitWidth = $inputFluxValues[$i] * $fluxUnitConversion * $fluxFraction;
      if ( $j == $numNodes-1 ) {
         $comment .= " boundary end";
      }
      printf OUT "$nodalFluxPerUnitWidth  0.00 $comment\n";
      $comment = " !";
   }
}
#
# close all files
close(BF);
close(OUT);
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

sub stderrMessage ($$) {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $theTime = "[$year-".sprintf("%3s",$months[$month])."-".sprintf("%02d",$dayOfMonth)."-T".sprintf("%02d",$hour).":".sprintf("%02d",$minute).":".sprintf("%02d",$second)."]";
   printf STDERR "$theTime $level: set_flux.pl: $message\n";
}

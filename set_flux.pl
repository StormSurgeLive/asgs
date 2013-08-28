#!/usr/bin/perl
#----------------------------------------------------------------
# set_flux.pl
#
# Takes a set of flux values for the flux specified boundaries
# of a particular mesh along with information about the boundaries
# themselves and calculates a corresponding flux per unit width
# for use with ADCIRC.
#
#----------------------------------------------------------------
# Copyright(C) 2013 Jason Fleming
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
use strict;
use warnings;
use Getopt::Long;
#
sub stderrMessage($$);
#
my $boundaryFile = "boundary.txt";
my @inputFluxValues;
my $outputFile = "flux_per_unit_width.txt";
my $fluxProfile = "proportional";
my @supported_profiles = qw(uniform proportional);
my $inputFluxUnits = "cumecs";
my @supported_units = qw(cumecs cumec m^3/s m^3s^-1 cfs kcfs);
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

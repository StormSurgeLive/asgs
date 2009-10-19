#!/usr/bin/env perl
#----------------------------------------------------------------
# autoplot.pl
#
# Fills out a gnuplot template for each station in the transposed
# station output file.  
#
#----------------------------------------------------------------
# Copyright(C) 2009 Jason Fleming
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
#
$^W++;
use strict;
use Date::Pcalc;
use Getopt::Long;
#
sub stderrMessage($$);
#
my $fileToPlot; # name of file to plot 
my $plotType; # type of file to plot 
my $outputDir; # location of template.gp file
my $vectorOutput = "magnitude"; # magnitude or direction or raw
my $timezone="CDT"; # time zone designation to be placed on graphs
my $units = "english"; # output units, english or si
my $plotDir;  # directory where the plots should be created
my $plotType; # type of data to be plotted (elevation, velocity, etc)
my $gpscript; # name of script file to create
my @supported_files = qw(elevation velocity windvelocity barometricpressure);
my $datum = "MSL";
my $enstorm = "Consensus Forecast"; # name of the storm in the ensemble
my $stormname; # NHC name, e.g., Ike, Katrina, Camille, etc
my $advisory;  # advisory number
#
GetOptions(
           "filetoplot=s" => \$fileToPlot,
           "plotType=s" => \$plotType,
           "plotdir=s" => \$plotDir,
           "outputdir=s" => \$outputDir,
           "vectoroutput=s" => \$vectorOutput,
           "timezone=s" => \$timezone,
           "units=s" => \$units,
           "stormname=s" => \$stormname,
           "enstorm=s" => \$enstorm,
           "advisory=s" => \$advisory,
           "datum=s" => \$datum
           );
# 
# check to make sure that the transposed file to plot is in the list
unless ( is_member($plotType,@supported_files)) {
   my $sf = join(",",@supported_files);
   stderrMessage("ERROR","The file type to plot ($plotType) is not one of the supported file types, which are as follows: $sf.");
}
#
# form label of y axis on plot based on command line arguments
my $ylabel;       # full label for y axis
my $vectorLabel;  # describes the vector quantity 
my $labelUnits;  # units of the vector quantity
my $ymin;         # min cutoff on the graph
my $ymax;         # max cutoff on the graph
my $titlePrefix;  # the first words in the title of the plot
if ( $vectorOutput eq "magnitude" ) {
   $vectorLabel = "Speed ";
   $labelUnits = "";
} else {
   $vectorLabel = "Compass Direction ";
   $labelUnits = "(degrees)";
   $ymin = 0.0;
   $ymax = 360.0;
}
if ( $plotType eq "elevation" ) {
   $titlePrefix = "Stage above " . $datum;
   if ( $units eq "english" ) {
      $labelUnits ="(ft)";
      $ymin = "-3.0";
      $ymax = "12.0";
   } else { 
      $labelUnits = "(m)";
      $ymin = "-1.0";
      $ymax = "4.0";
   }
} elsif ( $plotType eq "velocity" ) {
   if ( $units eq "english" && $vectorOutput eq "magnitude" ) {
      $labelUnits = "(ft/s)";
      $ymin = "0.0";
      $ymax = "3.0";
   } elsif ( $units ne "english" && $vectorOutput eq "magnitude" ) { 
      $labelUnits = "(m/s)";
      $ymin = "0.0";
      $ymax = "1.0";
   }
   $titlePrefix = "Water Current " . $vectorLabel;
   # $ylabel = "Water Current " . $vectorLabel . $vectorUnits;
} elsif ( $plotType eq "barometricpressure" ) {
   $titlePrefix = "Barometric Pressure ";
   $labelUnits = "(mb)"; # english units not available
   $ymin = "920.0";
   $ymax = "1013.0";
} elsif ( $plotType eq "windvelocity" ) {
   $titlePrefix = "Wind " . $vectorLabel;
   if ( $units eq "english" && $vectorOutput eq "magnitude" ) {
      $labelUnits = "(kts)";
      $ymin = "0.0";
      $ymax = "90.0";
   } elsif ( $units ne "english" && $vectorOutput eq "magnitude" ) { 
      $labelUnits = "(m/s) ";
      $ymin = "0.0";
      $ymax = "45.0";
   }
}
$ylabel = $titlePrefix . $labelUnits;
#
# Open transposed file to determine the number of columns and grab station
# names
unless (open(TRANSPOSE,"<$fileToPlot")) {
   stderrMessage("ERROR","Could not open $fileToPlot: $!.");
   exit(1);
}
#
# assume that the format is space-delimited since that is what gnuplot needs
my $startgraph = "not implemented";  # date on which the graph should start 
my $endforecast = "not implemented"; # date on which the graph should end (end of forecast)
my @stanames;    # the names of the stations
my $numCol;      # number of stations
while (<TRANSPOSE>) {
   if ( $. == 2 ) { # second line is a header with station names
      @stanames = ($_ =~ /(".*?"|\S+)/g);
      $numCol = @stanames;    
   }
}
#
# create a gnuplot script file for each column
for (my $i=3; $i<$numCol; $i++ ) {
   #
   # Open template file 
   unless (open(TEMPLATE,"<$outputDir/template.gp")) {
      stderrMessage("ERROR","Could not open $outputDir/template.gp: $!.");
      exit(1);
   }
   #
   # form plot title and filename
   $stanames[$i] =~ /\"\s*(.+)\s*\"/; # strip quotes and any surrounding space
   my $stationName = $1; 
   my $plottitle = $stormname . ", " . "advisory " . $advisory . ", " . $titlePrefix . " at " . $stationName;
   my @titleWords = split(" ",$stationName);
   my $stanameUnderscore = join("_",@titleWords);
   my $shortnameUnderscore = substr($stanameUnderscore,0,30); 
   my @titlePrefixWords = split(" ",$titlePrefix);
   my $titlePrefixUnderscore = join("_",@titlePrefixWords);
   my @enstormWords = split(" ",$enstorm);
   my $enstormUnderscore = join("_",@enstormWords);
   my @stormnameWords = split(" ",$stormname);
   my $stormnameUnderscore = join("_",@stormnameWords);
   my $gpscript = $stormnameUnderscore . "." . $advisory . "." . $enstormUnderscore . "." . $titlePrefixUnderscore . "." . $stanameUnderscore . ".gp";
   my $psplotName =  $stormnameUnderscore . "." . $advisory . "." . $titlePrefixUnderscore . "." . $shortnameUnderscore . ".ps";  
   #
   my $col = $i + 1;
   #
   # Create gnuplotscript file 
   unless (open(GPSCRIPT,">$plotDir/$gpscript")) {
      stderrMessage("ERROR","Could not create $plotDir/$gpscript: $!.");
      exit(1);
   }
   while(<TEMPLATE>) {
      s/%ymin%/$ymin/;
      s/%ymax%/$ymax/;
      s/%ylabel%/$ylabel/;
      s/%startgraph%/$startgraph/;
      s/%endforecast%/$endforecast/;
      s/%plottitle%/$plottitle/;
      s/%psplotname%/$psplotName/;
      s/%transpose%/$fileToPlot/;
      s/%datatitle%/$enstorm/;
      s/%col%/$col/;
      s/%timezone%/$timezone/;
      print GPSCRIPT $_;
   }
}
close(TEMPLATE);
close(GPSCRIPT);
#
#
sub stderrMessage ($$) {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $theTime = "[$year-".sprintf("%3s",$months[$month])."-".sprintf("%02d",$dayOfMonth)."-T".sprintf("%02d",$hour).":".sprintf("%02d",$minute).":".sprintf("%02d",$second)."]";
   printf STDERR "$theTime $level: autoplot.pl: $message\n";
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


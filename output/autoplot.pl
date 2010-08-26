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
my $labelUnits;   # units of the vector quantity
#
# we used to let gnuplot autorange the graphs, but this was confusing to 
# end users: tiny, cm-scale variations in water levels looked like giant
# oscillations b/c gnuplot autoscaled the graphs ... now we set a default
# scale, and if the numbers exceed the range, we adjust so that we can 
# see the actual range 
my $default_ymin; # min cutoff on the graph
my $default_ymax; # max cutoff on the graph
my $titlePrefix;  # the first words in the title of the plot
if ( $vectorOutput eq "magnitude" ) {
   $vectorLabel = "Speed ";
   $labelUnits = "";
} else {
   $vectorLabel = "Compass Direction ";
   $labelUnits = "(degrees)";
   $default_ymin = 0.0;
   $default_ymax = 360.0;
}
if ( $plotType eq "elevation" ) {
#   $titlePrefix = "Stage above " . $datum;
   $titlePrefix = "Stage ";
   if ( $units eq "english" ) {
      $labelUnits ="(ft)";
      $default_ymin = "-3.0";
      $default_ymax = "12.0";
   } else { 
      $labelUnits = "(m)";
      $default_ymin = "-1.0";
      $default_ymax = "4.0";
   }
} elsif ( $plotType eq "velocity" ) {
   if ( $units eq "english" && $vectorOutput eq "magnitude" ) {
      $labelUnits = "(ft/s)";
      $default_ymin = "0.0";
      $default_ymax = "3.0";
   } elsif ( $units ne "english" && $vectorOutput eq "magnitude" ) { 
      $labelUnits = "(m/s)";
      $default_ymin = "0.0";
      $default_ymax = "1.0";
   }
   $titlePrefix = "Water Current " . $vectorLabel;
   # $ylabel = "Water Current " . $vectorLabel . $vectorUnits;
} elsif ( $plotType eq "barometricpressure" ) {
   $titlePrefix = "Atm.Press. ";
   $labelUnits = "(mb)"; # english units not available
   $default_ymin = "920.0";
   $default_ymax = "1013.0";
} elsif ( $plotType eq "windvelocity" ) {
   $titlePrefix = "Wind " . $vectorLabel;
   if ( $units eq "english" && $vectorOutput eq "magnitude" ) {
      $labelUnits = "(kts)";
      $default_ymin = "0.0";
      $default_ymax = "75.0";
   } elsif ( $units ne "english" && $vectorOutput eq "magnitude" ) { 
      $labelUnits = "(m/s) ";
      $default_ymin = "0.0";
      $default_ymax = "25.0";
   }
}
$ylabel = $titlePrefix . $labelUnits;
#
# Open transposed file to determine the number of columns, grab station
# names, and determine min and max for each station for adjusting ymin and
# ymax
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
my @ymins;       # the minimum value at each station
my @ymaxes;      # the maximum value at each station
my @station_vals; # the value at each station at a given time
while (<TRANSPOSE>) {
   if ( $. == 2 ) { # second line is a header with station names
      @stanames = ($_ =~ /(".*?"|\S+)/g);
      $numCol = @stanames;    
   }
   if ( ($. != 1) && ($. != 2) ) { 
      @station_vals = split;
      if ( $. == 3 ) {
         # initialize the min and max arrays if this is the first set of values
         for (my $i=3; $i<$numCol; $i++ ) {
            $ymins[$i] = $station_vals[$i];
            $ymaxes[$i] = $station_vals[$i];
         }
      } else {
         # see if we have set a new ymin or a new ymax
         for (my $i=3; $i<$numCol; $i++ ) {
            if ( ($station_vals[$i] < $ymins[$i]) || ($ymins[$i] == -99999) ) {
               $ymins[$i] = $station_vals[$i];
            }
            if ( $station_vals[$i] > $ymaxes[$i] ) {
               $ymaxes[$i] = $station_vals[$i];
            }
         }
      }
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
   # if station value exceeds ymax, then use the station max as the max on the
   # plot; if station value is less than ymin (but not -99999) then use it as
   # the min on the plot
   my $myYmax;
   my $myYmin;
   $myYmax = $default_ymax;
   $myYmin = $default_ymin;
   if ( $ymaxes[$i] > $default_ymax ) {
     $myYmax = $ymaxes[$i];
     stderrMessage("INFO","Station $stanames[$i] has a maximum value of $myYmax, which is greater than $default_ymax, the normal maximum range for the plot $plottitle ... therefore the y-axis of this plot will be set to max out at $myYmax.");
   } 
   if ( ($ymins[$i] < $default_ymin) && ($ymins[$i] != -99999) ) {
     $myYmin = $ymins[$i];
     stderrMessage("INFO","Station $stanames[$i] has a minimum value of $myYmin, which is less than $default_ymin, the normal minimum range for the plot $plottitle ... therefore the y-axis of this plot will be set to bottom out at $myYmin.");
   } 
   #
   # Create gnuplotscript file 
   unless (open(GPSCRIPT,">$plotDir/$gpscript")) {
      stderrMessage("ERROR","Could not create $plotDir/$gpscript: $!.");
      exit(1);
   }
   while(<TEMPLATE>) {
      s/%ymin%/$myYmin/;
      s/%ymax%/$myYmax/;
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


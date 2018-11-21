#!/usr/bin/env perl
#----------------------------------------------------------------
# autoplot.pl
#
# Fills out a gnuplot template for each station in the transposed
# station output file.  
#
#----------------------------------------------------------------
# Copyright(C) 2009--2018 Jason Fleming
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
my $template = "template.gp"; # gnuplot template file, including full path to the file
my $vectorOutput = "magnitude"; # magnitude or direction or raw
my $timezone="CDT"; # time zone designation to be placed on graphs
my $units = "english"; # output units, english or si
my $plotDir;  # directory where the plots should be created
my $plotType; # type of data to be plotted (elevation, velocity, etc)
my $gpscript; # name of script file to create
my @supported_files = qw(elevation velocity windvelocity barometricpressure);
my %plotTypesDescriptions = ( "elevation", "water surface elevation",
                             "velocity", "water current, speed and direction" ,
                             "windvelocity", "wind speed and direction",
                             "barometricpressure", "atmospheric pressure"  
                             );
my %maxTypesDescriptions = ( "elevation", "Peak water level",
                             "velocity", "Peak water current speed",
                             "windvelocity", "Peak sustained wind speed",
                             "barometricpressure", "Minimum atmospheric pressure"
                             );
my $datum = "MSL";
my $stationsOfInterest = "all"; # std format subset of stations to actually plot
my $enstorm = "Consensus Forecast"; # name of the storm in the ensemble
my $stormname; # NHC name, e.g., Ike, Katrina, Camille, etc
my $legend = "enstorm";  # "enstorm" or ""
my $datatitle = "Track";
my $advisory;  # advisory number
my $minmaxfile = "null";
my $timeminmaxfile = "null";
my $bathytopo = "null";
my $gpscriptname = "null";
#
GetOptions(
           "filetoplot=s" => \$fileToPlot,
           "stationsofinterest=s" => \$stationsOfInterest,
           "plotType=s" => \$plotType,
           "plotdir=s" => \$plotDir,
           "legend=s" => \$legend,
           "template=s" => \$template,
           "vectoroutput=s" => \$vectorOutput,
           "timezone=s" => \$timezone,
           "units=s" => \$units,
           "bathytopo=s" => \$bathytopo,
           "gpscriptname=s" => \$gpscriptname,
           "minmaxfile=s" => \$minmaxfile,
           "timeminmaxfile=s" => \$timeminmaxfile,
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
#-------------------------------------------------------------------
#    R E A D   L I S T   O F   S T A T I O N S   T O   P L O T  
#-------------------------------------------------------------------
# If there is a subset of stations of interest to plot, instead of simply
# plotting all the stations found in the file, read in the list of stations
# of interest
my @stationLons;
my @stationLats;
my @stationIDs;
my @stationAgencies;
my @stationDescriptions;
my $num_sta = 0;
if ( $stationsOfInterest ne "all" ) {
   unless (open(STATIONS,"<$stationsOfInterest")) {
      stderrMessage("ERROR","Could not open $stationsOfInterest: $!.");
      exit(1);
   }
   # pop off the header line and report
   # commented out b/c there is no header line
   #my $line = <STATIONS>; 
   #chomp($line);
   #&stderrMessage("INFO","First header line in stations-of-interest file '$stationsOfInterest' is '$line'.");
   # now read stations and record ID etc   
   while(<STATIONS>) {
      my @fields = split("!");
      my $latlon = $fields[0];
      my @coords = split(" ",$latlon);
      $stationLons[$num_sta] = $coords[0];
      $stationLats[$num_sta] = $coords[1];
      $stationIDs[$num_sta] = $fields[1];
      $stationAgencies[$num_sta] = $fields[2];
      $stationDescriptions[$num_sta] = $fields[3];
      $num_sta++;
   }
}
#-------------------------------------------------------------------
#     L A B E L   Y - A X I S  A N D   
#
#  S E T  D E F A U L T   Y - R A N G E  
#-------------------------------------------------------------------
#
# form label of y axis on plot based on command line arguments
my $ylabel;       # full label for y axis
my $vectorLabel;  # describes the vector quantity 
my $labelUnits;   # units of the quantity being plotted
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
   $labelUnits = "degrees";
   $default_ymin = 0.0;
   $default_ymax = 360.0;
}
if ( $plotType eq "elevation" ) {
#   $titlePrefix = "Stage above " . $datum;
#   $titlePrefix = "Stage ";
   $titlePrefix = "Water Level";
   if ( $units eq "english" ) {
      $labelUnits ="ft above " . $datum;
      $default_ymin = "0.0";
      $default_ymax = "10.0";
   } else { 
      $labelUnits = "m above " . $datum;
      $default_ymin = "0.0";
      $default_ymax = "3.5";
   }
} elsif ( $plotType eq "velocity" ) {
   if ( $units eq "english" && $vectorOutput eq "magnitude" ) {
      $labelUnits = "ft/s";
      $default_ymin = "0.0";
      $default_ymax = "3.0";
   } elsif ( $units ne "english" && $vectorOutput eq "magnitude" ) { 
      $labelUnits = "m/s";
      $default_ymin = "0.0";
      $default_ymax = "1.0";
   }
   $titlePrefix = "Water Current " . $vectorLabel;
   # $ylabel = "Water Current " . $vectorLabel . $vectorUnits;
} elsif ( $plotType eq "barometricpressure" ) {
   $titlePrefix = "Atm.Press. ";
   $labelUnits = "mb"; # english units not available
   $default_ymin = "920.0";
   $default_ymax = "1013.0";
} elsif ( $plotType eq "windvelocity" ) {
   $titlePrefix = "Wind " . $vectorLabel;
   if ( $units eq "english" && $vectorOutput eq "magnitude" ) {
      $labelUnits = "kts";
      $default_ymin = "0.0";
      $default_ymax = "75.0";
   } elsif ( $units ne "english" && $vectorOutput eq "magnitude" ) { 
      $labelUnits = "m/s";
      $default_ymin = "0.0";
      $default_ymax = "25.0";
   }
}
$ylabel = "$titlePrefix ($labelUnits)";
#
#-------------------------------------------------------------------
#        O P E N   A N D   R E A D   D A T A   F I L E
#-------------------------------------------------------------------
#
# Open transposed file to determine the number of columns, grab station
# names, and determine min and max for each station for adjusting ymin and
# ymax
unless (open(TRANSPOSE,"<$fileToPlot")) {
   stderrMessage("ERROR","Could not open $fileToPlot: $!.");
   die;
}
stderrMessage("INFO","Opened $fileToPlot.");
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
   # parse non-header lines
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
stderrMessage("INFO","Finished reading station names from $fileToPlot.");
#
#-------------------------------------------------------------------
#                M I N   M A X   V A L U E S 
#-------------------------------------------------------------------
# open minmax, time of minmax, and bathytopo files if specified 
# so that values can be read and used to annotate plots
my @minMaxFiles = ( $minmaxfile, $timeminmaxfile );
my @minMaxColumns;
my @timeMinMaxColumns;
for (my $f=0; $f<=1; $f++ ) {
   if ( $minMaxFiles[$f] ne "null" ) {         
      # open the minmax file and get the value from the correct column
      unless (open(MINMAXFILE,"<$minMaxFiles[$f]")) {
         stderrMessage("ERROR","Could not open $minMaxFiles[$f] for reading: $!.");
         die;
      }
      # get the minmax value
      my $minmaxseparator;
      while(<MINMAXFILE>) {
         # check to see what the format of the file is 
         # (csv or space-separated columns)
         if ( $. == 1 ) {
            if ( $_ =~ /format=comma/ ) {
               &stderrMessage("INFO","Column format of $minMaxFiles[$f] is comma-separated.");
               $minmaxseparator = ",";
            } else {
               &stderrMessage("INFO","Column format of $minMaxFiles[$f] is space-separated.");
               $minmaxseparator = " ";
            }
         }
         if ( $. == 3 ) {
            if ( $f == 0 ) {
               @minMaxColumns = split($minmaxseparator);
            } else {
               @timeMinMaxColumns = split($minmaxseparator);                           
            }
         }
      }
      close(MINMAXFILE);
   }
}
#-------------------------------------------------------------------
#            B A T H Y T O P O    V A L U E S  
#-------------------------------------------------------------------
my @bathytopoColumns;
my $bathytopounits = "null";
if ( $bathytopo ne "null" ) {         
   # open the bathytopo file and get the value from the correct column
   unless (open(BATHYTOPO,"<$bathytopo")) {
      stderrMessage("ERROR","Could not open $bathytopo for reading: $!.");
      die;
   }
   # get the bathytopo value
   my $bathytoposeparator = " ";
   while(<BATHYTOPO>) {
      # check to see what the format of the file is 
      # (csv or space-separated columns)
      if ( $. == 1 ) {
         if ( $_ =~ /format=comma/ ) {
            &stderrMessage("INFO","Column format of $bathytopo is comma-separated.");
            $bathytoposeparator = ",";
         } else {
            &stderrMessage("INFO","Column format of $bathytopo is space-separated.");
         }
         # pull the bathytopo units and datum from the header of the
         # bathytopo.dat file
         $_ =~ /units=([a-z]*)/;
         $bathytopounits = $1;
      }
      # get the bathytopo value and form the gnuplot label
      if ( $. == 3 ) {
         @bathytopoColumns = split($bathytoposeparator);
      }
   }
   close(BATHYTOPO);
}
#
#-------------------------------------------------------------------
#        C R E A T E   G N U P L O T   S C R I P T S
#-------------------------------------------------------------------
#
# create a gnuplot script file for each column
for (my $i=3; $i<$numCol; $i++ ) {
   my $stationAgency;
   my $stationID;
   my $stationDescription;
   #
   # Open template file 
   unless (open(TEMPLATE,"<$template")) {
      &stderrMessage("ERROR","Could not open template file $template: $!.");
      die;
   }
   #&stderrMessage("DEBUG","Opened template file $template.");
   #
   # form plot title and filename
   $stanames[$i] =~ /\"\s*(.+)\s*\"/; # strip quotes and any surrounding space
   my $stationName = $1; 
   # if the header is in std station metadata format, pull out the ID and desc
   my @fields = split("!",$stationName);
   my $numFields = @fields;
   #print "stationName is $stationName numFields is $numFields\n";
   # if station name is in standard metadata format
   if ( $numFields >= 3 ) {
      $fields[0] =~ s/^\s+|\s+$//g;
      $stationAgency = $fields[0];
      $stationID = $fields[1];
      $stationID =~ s/^\s+|\s+$//g;
      $stationDescription = $fields[2];
      $stationDescription =~ s/^\s+|s+$//g; # strip quotes and any surrounding space
      print "stuff\n";
      #print "stationAgency is \"$stationAgency\"\n";
      #print "stationID is \"$stationID\"\n";
      #print "stationDescription is \"$stationDescription\"\n";
      # only include the specified stations if only certain stations were 
      # specified
      if ( $stationsOfInterest ne "all" ) {
         print "stuff\n";         
         unless ( is_member($stationID,@stationIDs)) {
           next;
         }
      }
      # station ID and station description
      $stationName = "$stationAgency $stationID $stationDescription";
   } 

   #
   #                P L O T   T I T L E
   #  
   my $plottitle = $stormname . ", " . "advisory " . $advisory . ", " 
      . $titlePrefix . " at " . $stationName;
   if ( $advisory eq "hindcast" ) {
      $plottitle = $stormname . ", " . $advisory . ", " . $titlePrefix 
      . " at " . $stationName;      
   }
   #
   #             P L O T   F I L E   N A M E 
   #  
   # use stationID and station description in the filename by default
   my @titleWords = split(" ",$stationName);
   my $stanameUnderscore = join("_",@titleWords);
   if ( $gpscriptname eq "stationdescription" ) {
      @titleWords = split(" ",$stationName);
   }
   if ( $gpscriptname eq "stationid" ) {
      @titleWords = split(" ",$stationID);
   }
   my $stanameUnderscore = join("_",@titleWords);
   # prevent the file name from getting too long for the filesystem to handle
   my $shortnameUnderscore = substr($stanameUnderscore,0,30); 
   my @titlePrefixWords = split(" ",$titlePrefix);
   my $titlePrefixUnderscore = join("_",@titlePrefixWords);
   my @enstormWords = split(" ",$enstorm);
   my $enstormUnderscore = join("_",@enstormWords);
   my @stormnameWords = split(" ",$stormname);
   my $stormnameUnderscore = join("_",@stormnameWords);
   my $filename = $stormnameUnderscore . "." . $advisory . "." 
         . $enstormUnderscore . "." . $titlePrefixUnderscore . "." 
         . $stanameUnderscore;
   $filename =~ s/\///g; # strip forward slashes from file name
   my $gpscript = $filename . ".gp";
   my $psplotName =  $filename . ".ps";       
   $psplotName =~ s/\///g; # strip forward slashes from file name
   # set plot legend
   my $datatitle = $enstorm;
   if ( $legend eq "stationname" ) {
      $datatitle = $stationName;
   } elsif ( $legend eq "plottype" ) {
      $datatitle = $plotTypesDescriptions{$plotType};
   }
   #
   my $col = $i + 1;       # gnuplot index for data column in time varying files
   my $minmaxcol = $i - 3; # perl index for data column in minmax and time of minmax files
   #
   #                M I N   M A X   L A B E L  
   #
   # add gnuplot labels for minmax and time of minmax if specified 
   # on autoplot command line 
   my $labeltext = "null";
   my $labelcontent;
   my $minMaxValue = -99999;
   my $timeMinMaxValue = -99999;
   if ( @minMaxColumns ) {
      $minMaxValue = sprintf("%.1f",$minMaxColumns[$minmaxcol]);
      $labelcontent = "$maxTypesDescriptions{$plotType} $minMaxValue$labelUnits";
   }
   if ( @timeMinMaxColumns ) {
      $timeMinMaxValue = $timeMinMaxColumns[$minmaxcol];
      $labelcontent = $labelcontent . " at $timeMinMaxValue";  
   }
   $labeltext = "set label \"$labelcontent\" at graph 0.1,0.1";
   #
   #            B A T H Y T O P O    L A B E L   
   #
   my $bathytopolabeltext = "null";
   my $bathytopolabelcontent;
   if ( $bathytopo ne "null" ) {         
      # limit numeric results to one decimal place of precision 
      $bathytopo = sprintf("%.1f",$bathytopoColumns[$minmaxcol]);
      # set label on bathytopo value
      my $relation = "above";
      # adcirc's bathytopo coordinate system is positive downward
      if ( $bathytopo > 0.0 ) {
         $relation = "below";
      } else {
         $bathytopo = abs($bathytopo);
      }
      # add the bathytopo datum
      my $datumlabel = "datum";
      if ( $datum ne "null" ) {
         $datumlabel = $datum;
      } 
      $bathytopolabelcontent = "Site elevation is $bathytopo$bathytopounits $relation $datumlabel";
   }
   $bathytopolabeltext = "set label \"$bathytopolabelcontent\" at graph 0.1,0.05";
   #
   #              S E T   Y   R A N G E
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
      die;
   }
   #
   #            F I L L   T E M P L A T E
   #     
   #&stderrMessage("DEBUG","Opened $plotDir/$gpscript.");
   while(<TEMPLATE>) {
      #print $_;
      s/%ymin%/$myYmin/;
      s/%ymax%/$myYmax/;
      s/%ylabel%/$ylabel/;
      s/%startgraph%/$startgraph/;
      s/%endforecast%/$endforecast/;
      s/%plottitle%/$plottitle/;
      s/%psplotname%/$psplotName/;
      s/%transpose%/$fileToPlot/;
      s/%datatitle%/$datatitle/;
      s/%col%/$col/;
      s/%minmaxlabel%/$labeltext/;      
      s/%bathytopolabel%/$bathytopolabeltext/;
      s/%timezone%/$timezone/;
      #print $_;
      unless ( $_ =~ "null" ) {         
         print GPSCRIPT $_;
      } else {
         print GPSCRIPT "# ".$_;
      }
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


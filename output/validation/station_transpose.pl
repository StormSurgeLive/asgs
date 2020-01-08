#!/usr/bin/env perl
#----------------------------------------------------------------
# station_transpose.pl
#
# Gets the number in each station category (elevation, wind, etc) 
# and the names of all the stations from the fort.15 file and then
# transposes the station output files so that the columns represent 
# different stations and the rows represent time. 
#
#----------------------------------------------------------------
# Copyright(C) 2009--2015 Jason Fleming
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
# usage sample:
# export PERL5LIB=~/asgs/trunk/PERL
# perl ~/asgs/trunk/output/station_transpose.pl --filetotranspose barometricpressure --controlfile ./fort.15 --stationfile ./fort.71 --format space --coldstartdate 2009110100 --gmtoffset 0 --timezone GMT --units si
#
#----------------------------------------------------------------
use strict;
use warnings;
use Date::Calc;
use Getopt::Long;
use Math::Trig;
#
sub stderrMessage($$);
#
my $pi = 3.14159265;
my $fileToTranspose; # type of file to transpose 
my $controlFile = "./fort.15"; # full path name of control file
my $stationFile; # full path name of station output file 
my $format = "space"; # column separator in transposed file (space or comma)
my $vectorOutput = "magnitude"; # magnitude or direction or raw
my $coldstartdate; # yyyymmddhh24 when the simulation was coldstarted
my $gmtoffset=-5; # number of hours between gmt and local time
my $timezone="CDT"; # time zone designation to be placed on graphs
my $units = "english"; # output units, english or si
my $stationlabel = "after exclamation point"; # how to parse the station label from fort.15
# on command line, use 
# --stationlabel betweenbangs
# if the station comment line has the station ID between two exclamation 
# points and only the station ID should be used on the plots; e.g., the
# station line in the fort.15 looks like this
# -88.55779 30.43825 ! SSS-MS-JAC-051WL ! storm tide, water level Jackson Mississippi hwm: 5.42 ft NAVD88 at 16:30:28 8/29/2012 GMT
my $firstBang;   # string location where first exclamation point appears
my $secondBang;  # string location where second exclamation point appears
my $labelLength; # length of station label string
my $stationPlotLabel; # the string that is pulled from the fort.15 for each station to be used in labeling the associated plot
my @supported_files = qw(elevation velocity windvelocity barometricpressure);
my $year;
my $month;
my $day;
my $hour;
my $min;
my $sec;
my $cs_year;
my $cs_mon;
my $cs_day;
my $cs_hour;
my $cs_min;
my $cs_sec;
#
GetOptions(
           "filetotranspose=s" => \$fileToTranspose,
           "controlfile=s" => \$controlFile,
           "stationfile=s" => \$stationFile,
           "format=s" => \$format,
           "vectoroutput=s" => \$vectorOutput,
           "coldstartdate=s" => \$coldstartdate,
           "gmtoffset=s" => \$gmtoffset,
           "timezone=s" => \$timezone,
           "stationlabel=s" => \$stationlabel,
           "units=s" => \$units
           );
# 
# check to make sure that the output file to be transposed is in the list
unless ( is_member($fileToTranspose,@supported_files)) {
   my $sf = join(",",@supported_files);
   stderrMessage("ERROR","The file type to transpose ($fileToTranspose) is not one of the supported file types, which are as follows: $sf."); 
}
my $separator = " "; 
if ( $format eq "comma" ) {
   $separator = ",";
}
$coldstartdate =~ /(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
$cs_year = $1;
$cs_mon = $2;
$cs_day = $3;
$cs_hour = $4;
$cs_min = 0;
$cs_sec = 0;
#
######################################################################
# first, we parse the fort.15 file to get number of each kind of station
# and their names
#######################################################################
my $sta_name;
my $num_sta;
my $elevsta_ts_incr = 0; # time step increment btw elevation station output
my $num_elev_sta = 0;
my @elev_sta_names;
my $velsta_ts_incr = 0;  # time step increment btw velocity station output
my $num_vel_sta = 0;
my @vel_sta_names;
my $metsta_ts_incr = 0;  # time step increment btw met station output
my $num_met_sta = 0;
my @met_sta_names;
my @sta_names;
my $sta_type = ""; # which station type we are currently parsing
my $controlFileFound = 1;
unless(open(FORT15,"<$controlFile")) {
   stderrMessage("WARNING","Could not open $controlFile for reading.");
   stderrMessage("WARNING","The number of stations will be gleaned from the datafile itself.");
   stderrMessage("WARNING","The station names will default to 'Station1', 'Station2', etc.");
   $controlFileFound = 0;
} else {
   stderrMessage("INFO","The number of stations and the station names will be parsed from the file '$controlFile'.");
   while(<FORT15>) {
      my @Fld = split;
      if ( /(NSTAE)/ || /(NSTAV)/ || /(NSTAM)/ ) {
         $num_sta = $Fld[0];
         my $station_type = $1;
         for ( my $i=0; $i<$num_sta; $i++ ) {
            my $line =<FORT15>; 
            chomp($line);
            #
            # determine the location(s) of the exclamation point(s) that
            # are used to mark out the station name and/or station ID 
            $firstBang = index($line,"!") + 1;
            $secondBang = index($line,"!",$firstBang+1);
            $labelLength = -1;
            if ( $stationlabel eq "betweenbangs" && $firstBang ne -1 && $secondBang ne -1 ) {
               $labelLength = $secondBang - $firstBang;
            } else {
               $labelLength = length($line) - $firstBang;
            }
            if ( $stationlabel eq "after exclamation point" || $stationlabel eq "betweenbangs" ) {           
               $stationPlotLabel = substr($line,$firstBang,$labelLength);
               $stationPlotLabel =~ s/^\s+|\s+$//g ;
            }
            if ( $stationlabel eq "numbered" ) { 
               $stationPlotLabel = sprintf("%d",$i + 1);
            }
            if ( $station_type eq "NSTAE" ) {
               $num_elev_sta = $num_sta;
               push(@elev_sta_names,$stationPlotLabel);
            }
            if ( $station_type eq "NSTAV" ) {
               $num_vel_sta = $num_sta;
               push(@vel_sta_names,$stationPlotLabel);  
            }
            if ( $station_type eq "NSTAM" ) {
               $num_met_sta = $num_sta;
               push(@met_sta_names,$stationPlotLabel);  
            }
         }
      }
   }
   close(FORT15);
   if ( $fileToTranspose eq "elevation" ) {
      @sta_names = @elev_sta_names;
   } elsif ( $fileToTranspose eq "velocity" ) {
      @sta_names = @vel_sta_names;
   } elsif ( $fileToTranspose eq "windvelocity" || $fileToTranspose eq "barometricpressure" ) {
      @sta_names = @met_sta_names;
   }
}
#
# jgfdebug
stderrMessage("DEBUG","According to the fort.15 file, there are $num_elev_sta elevation stations, $num_vel_sta velocity stations, and $num_met_sta meteorological stations.\n");
#
# check to see if this is scalar data or vector data
my $fileType;
my $filename;
my $fileExtension;
my $total_stations = 0;
my @station_val = ();
my @vector_tuple_1 = ();
my @vector_tuple_2 = ();
if ( $fileToTranspose eq "elevation" 
	|| $fileToTranspose eq "barometricpressure" ) {
   $fileType = "scalar";
} else {
   $fileType = "vector";
}
if ( $format eq "comma" ) {
   $fileExtension = ".csv";
} else {
   $fileExtension = ".txt";
}
unless (open(STAFILE,"<$stationFile")) {
   stderrMessage("ERROR","Could not open $stationFile: $!.");
   exit(1);
} 
my $transposeFilename = $stationFile . "_transpose" . $fileExtension;
unless (open(TRANSPOSE,">$transposeFilename")) {
   stderrMessage("ERROR","Could not open $transposeFilename: $!.");
   exit(1);
}
my $time;
while (<STAFILE>) {
   #
   # the first line in the file is a comment line; transcribe it to the 
   # transposed file as a gnuplot comment line
   if ($. == 1 ) {
      chomp;
      printf TRANSPOSE "# " . $_ . "\n";
      next;
   } 
   #
   # 2nd line in the file contains the number of stations;
   # this number must match the number of stations from the fort.15 file
   if ($. == 2) {
      m/^\s*([^\s]*)\s*([^\s]*)/;
      $total_stations = $2;
      stderrMessage("INFO","Output file '$transposeFilename' contains $total_stations stations.\n");
      # if the user didn't specify a fort.15, create default station names,
      # using the number of stations we just found in the station output file
      if ( $controlFileFound == 0 ) { 
         for (my $i=0; $i<$total_stations; $i++ ) {
            my $default_station_number = $i+1;
            $sta_names[$i] = "Station" . $default_station_number;
         }
      }
      # write the names of the stations on the next line according to the requested format
      printf TRANSPOSE "#DATE" . $separator . "TIME" . $separator . "TIMEZONE" . $separator;
      foreach (@sta_names) {
          printf TRANSPOSE "\"$_\"" . $separator;
      }
      printf TRANSPOSE "\n";
      next;
   }
   #
   # there is a header line with the time, at the start of each dataset
   if ($. > 2 && ($.-3) % ($total_stations+1) == 0) {
      #
      # grab the new time (assumed to be in gmt), converting to 
      # specified local time 
      m/^\s*([^\s]*)\s*([^\s]*)\s*$/;
      ($year,$month,$day,$hour,$min,$sec)
         = Date::Calc::Add_Delta_DHMS($cs_year,$cs_mon,$cs_day,
            $cs_hour,$cs_min,$cs_sec,0,$gmtoffset,0,sprintf("%2d",$1));
      
      $time = sprintf("%4s-%02s-%02s$separator%02s:%02s:%02d$separator",
                $year,$month,$day,$hour,$min,$sec);
      next;
   }
   #
   # this is data (not the file header or the individual dataset header)
   my $stationNumber;
   if ( $fileType eq "scalar" ) {
      my $scalar;
      # parse out the station number and the value
      m/^\s*(\d*)\s*(.*)\s*$/;
      $stationNumber = $1;
      # set missing value or station value
      if ( $2 == "-0.9999900000E+05" || $2 =~/NaN/) {
         $scalar = -99999;
      } else {
         $scalar = $2; 
      }
      #
      # units in output file are assumed to be SI; convert to english
      # if requested 
      if ( $scalar != -99999 && $units eq "english" ) {
         if ( $fileToTranspose eq "elevation" ) {
            # convert m to ft
            $scalar *= (100.0 / (2.54 * 12.0));
         }
         if ( $fileToTranspose eq "barometricpressure" ) {
            stderrMessage("ERROR","Output in english units is not available for barometric pressure.");
         }
      } 
      if ( $scalar != -99999 && $fileToTranspose eq "barometricpressure" ) {
         # convert from m of water to mb: multiply by rho_0 g to get 
         # Pascals, then divide by 100 to get mb
         $scalar *= (1000.0 * 9.81 / 100.0);
      }
      $station_val[$1-1] = $scalar;
   } else {
      # this is vector data
      my $tuple_1;
      my $tuple_2;
      my $multiplier = 1.0;
      if ( $units eq "english" ) {
         $multiplier = 1.0/0.514444444; # convert m/s to kts 
      }
      if ( $fileToTranspose eq "windvelocity" ) {
         $multiplier *= 1.136; # convert 10 minute winds to 1 minute winds
      }
      m/^\s*([^\s]*)\s*([^\s]*)\s*([^\s]*)\s*$/;
      $stationNumber = $1;
      if ( !($2 =~ /NaN/) && !($2 =~/Inf/) && !($2 == -99999 ) ) {
         $tuple_1 = $2 * $multiplier;
      } else {
         $tuple_1 = -99999;
      }
      if ( !($3 =~ /NaN/) && !($3 =~/Inf/) && !($3 == -99999 ) ) {
         $tuple_2 = $3 * $multiplier;
      } else {
         $tuple_2 = -99999;
      }
      $vector_tuple_1[$1-1] = $tuple_1;
      $vector_tuple_2[$1-1] = $tuple_2;
   }   
   #
   # if we have collected a complete dataset, write it now
   if ( $stationNumber == $total_stations ) {
      printf TRANSPOSE $time . "$timezone" . $separator;
      # write scalar data 
      if ( $fileType eq "scalar" ) { 
         # scalar data
         foreach (@station_val) {
            printf TRANSPOSE ("%20s",$_);
            printf TRANSPOSE $separator;
         }
         @station_val = ();
      } else {
         # write vector data
         for (my $i=0; $i<$total_stations; $i++ ) {
            if ( $vectorOutput eq "raw" ) {
               printf TRANSPOSE ("%20s %20s",$vector_tuple_1[$i], $vector_tuple_2[$i]);
            # we have two valid values
            } elsif ( $vector_tuple_1[$i] != -99999 
                   && $vector_tuple_2[$i] != -99999 ) {
               if ( $vectorOutput eq "magnitude" ) {
                  printf TRANSPOSE ("%20s",sqrt($vector_tuple_1[$i]*$vector_tuple_1[$i] + $vector_tuple_2[$i]*$vector_tuple_2[$i]));
               } elsif ( $vectorOutput eq "direction" ) {
                  # assumes vectors are in east and north components
                  my $direction = 0.0;
                  # if north component is zero
                  #if ( $vector_tuple_2[$i] ) {
                  #   # if east component is negative
                  #   if ( $vector_tuple_1[$i] < 0.0 ) {
                  #      $direction = 270.0;
                  #   } else { # east component is negative or zero
                  #      $direction = 90.0;
                  #   }
                  #} 
                  # if both components are nonzero
                  if ( $vector_tuple_1[$i] == 0.0 && $vector_tuple_2[$i] == 0.0 ) {  
                    $direction = -99999;
                  } else {
                    # my $direction = 360.0 - atan($vector_tuple_2[$i]/$vector_tuple_1[$i])*(360.0/(2.0*$pi));
                     $direction = (180.0+(atan2($vector_tuple_1[$i],$vector_tuple_2[$i])*180.0/$pi ) ) % 360.0;
                  }
                  printf TRANSPOSE ("%20s",$direction);
               }
            } else {
               # we have invalid values
               printf TRANSPOSE "-99999";
            }
            printf TRANSPOSE $separator;
         }
         @vector_tuple_1 = ();
         @vector_tuple_2 = ();
      }
      printf TRANSPOSE "\n";
   }
}
close(STAFILE);
close(TRANSPOSE);

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
   printf STDERR "$theTime $level: station_transpose.pl: $message\n";
}

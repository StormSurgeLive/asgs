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
# Copyright(C) 2009--2020 Jason Fleming
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
our %properties; # key/value pairs _read from_ properties files
our $runproperties = "null"; # full path to run.properties metadata file from ASGS
my $pi = 3.14159265;
my $g = 9.81;
my $density = 1000.0;
my $s = 0;                  # index of the station in the station list or dataset
my $averagingperiod = "10min";
my $fileToTranspose = "null"; # type of file to transpose
my $controlfile = "null";  # full path name of control file (parsed to obtain list of stations)
my $stationfile = "null";  # full path name of station file (list of stations) in standard format
my $datafile = "null";     # full path name to file containing data at each station
my $drymaskfile = "null"; # elevation station file used to mask dry areas in other file types
my $format = "space";      # column separator in transposed file (space or comma)
my $separator = " ";
my $vectorOutput = "raw";  # "raw" (for east and north values), "compassdegrees" or "trigdegrees" along with magnitude
my $coldstartdate = "null"; # yyyymmddhh24 when the simulation was coldstarted; assumed to be in GMT
my $gmtoffset=-5; # number of hours between gmt and local time
my $timezone="GMT"; # time zone designation to be placed on graphs
my $units = "null"; # output units, english or si
my $stationlabel = "full";  # how to parse the station label from fort.15
# on command line, use
# --stationlabel full
#    uses the entire line from fort.15 and copies it to the header in the
#    transposed file
# --stationlabel std
#    assumes that the fort.15 station metadata are in the standard adcirc
#    metadata format and that the stations will be labeled with the ID
#    and description
# --stationlabel 'after exclamation point'
#    to use everything after the first exclamation point as the station label
# --stationlabel betweenbangs
#    if the station comment line has the station ID between two exclamation
#    points and only the station ID should be used on the plots; e.g., the
#    station line in the fort.15 looks like this
#    -88.55779 30.43825 ! SSS-MS-JAC-051WL ! storm tide, water level Jackson Mississippi hwm: 5.42 ft NAVD88 at 16:30:28 8/29/2012 GMT
# --stationlabel raw
#    uses the station number from the fort.61 file
my $firstBang;  # where first "!" appears in station metadata in fort.15
my $secondBang; # where second "!" appears in station metadata in fort.15
my $thirdBang;  # where third "!" appears in station metadata in fort.15
my $labelLength; # length of station label string
my @stationPlotLabels; # the string that is pulled from the fort.15 for each station to be used in labeling the associated plot
my @supported_files = qw( elevation velocity windvelocity barometricpressure wavedirection significantwaveheight bathytopo maureparticle_count maureparticle_count/area maureparticle_count/volume );
my @supported_minmax_files = qw( maxsignificantwaveheight maxelevation maxvelocity maxinundationdepth maxwindvelocity );
my @supported_time_minmax_files = qw( timemaxelevation timemaxwindvelocity timemaxvelocity timemaxsignificantwaveheight );
my $multiplier = "null"; # generic multiplier to use on the data
my $defaultStationLabels = 0;
my $multiplierarg = "1.0";
my $reformattedfile = "null"; # name of the reformatted (transposed) data file
# initialize the default units for each data type coming from ADCIRC
my %reformattedUnits = ("elevation", "m", "velocity", "m/s", "windvelocity",
   "m/s", "barometricpressure", "mH2O", "wavedirection", "degrees",
   "significantwaveheight", "m", "maxsignificantwaveheight", "m",
   "maxelevation", "m", "maxinundationdepth", "m", "maxwindvelocity", "m/s",
   "bathytopo", "m",
   "particle count", "p", "particle densiy", "p/m2", "particle density", "p/m3" );
#   maureparticle_count maureparticle_count/area maureparticle_count/volume
my $hstime = "null"; # optional parameter, used to compute transposed date/times based
                    # on hotstart time, coldstartdate, and the output frequency
                    # in the file, rather than the number of seconds associated
                    # with each data set in the file
my $datum;
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
           "controlfile=s" => \$controlfile,
           "stationfile=s" => \$stationfile,
           "drymaskfile=s" => \$drymaskfile,
           "datafile=s" => \$datafile,
           "format=s" => \$format,
           "vectoroutput=s" => \$vectorOutput,
           "coldstartdate=s" => \$coldstartdate,
           "hstime=s" => \$hstime,
           "gmtoffset=s" => \$gmtoffset,
           "datum=s" => \$datum,
           "timezone=s" => \$timezone,
           "stationlabel=s" => \$stationlabel,
           "runproperties=s" => \$runproperties,
           "density=s" => \$density,
           "averagingperiod=s" => \$averagingperiod,
           "multiplier=s" => \$multiplierarg,
           "g=s" => \$g,
           "reformattedfile=s" => \$reformattedfile,
           "units=s" => \$units
           );
#
# check to make sure that the output file to be transposed is in the list
unless ( is_member($fileToTranspose,@supported_files) || is_member($fileToTranspose,@supported_minmax_files) || is_member($fileToTranspose,@supported_time_minmax_files) ) {
   my $sf = join(",",@supported_files,@supported_minmax_files,@supported_time_minmax_files);
   stderrMessage("ERROR","The file type to transpose ($fileToTranspose) is not one of the supported file types, which are as follows: $sf.");
   die;
}
#
# check to see if this is a minmax or time of minmax file
my $minmax = 0;
if ( is_member($fileToTranspose,@supported_minmax_files) ) {
   $minmax = 1;
}
#
# if we are producing a csv file
if ( $format eq "comma" ) {
   $separator = ",";
}
#
# if coldstartdate was supplied on the command line, use that
if ( $runproperties ne "null" ) {
   # read the run.properties file if it was specified
   if ( -e $runproperties ) {
      &loadProperties;
   }
   # open run.properties file and read the coldstartdate from there
   $coldstartdate = $properties{"adcirc.time.coldstartdate"};
   if ( $coldstartdate ) {
       &stderrMessage("INFO","The coldstartdate from the $runproperties file is $coldstartdate.");
   } else {
       &stderrMessage("ERROR","The coldstartdate property could not be found in the run properties file $runproperties.");
       $coldstartdate = "null";
       $defaultStationLabels = 1;
   }
}
unless ( $coldstartdate eq "null" ) {
   $coldstartdate =~ /(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $cs_year = $1;
   $cs_mon = $2;
   $cs_day = $3;
   $cs_hour = $4;
   $cs_min = 0;
   $cs_sec = 0;
   &stderrMessage("DEBUG","coldstartdate $cs_year $cs_mon $cs_day $cs_hour $cs_min $cs_sec");
} else {
   &stderrMessage("WARNING","The coldstartdate was not specified on the command line and it could not be read from the run.properties file. The times associated with the output will be in units of seconds since ADCIRC cold start.");
}
#
# check to see if this is a minmax or time of minmax file
my $time_minmax = 0;
if ( is_member($fileToTranspose,@supported_time_minmax_files) ) {
   $time_minmax = 1;
   if ( $coldstartdate eq "null" ) {
      $units = "seconds"
   } else {
      $units = "datetime"
   }
}
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
my @sta_lines;
my $sta_type = ""; # which station type we are currently parsing
# neither fort.15 or station file provided
if ( $controlfile eq "null" && $stationfile eq "null" ) {
   &stderrMessage("WARNING","The number of stations will be gleaned from the datafile itself.");
   &stderrMessage("WARNING","The station labels will be 'station001', 'station002', etc because the neither the associated control (fort.15) file nor a standalone station file were provided on the command line.");
   $defaultStationLabels = 1;
}
# both fort.15 and station file provided
if ( $controlfile ne "null" && $stationfile ne "null" ) {
   &stderrMessage("WARNING","Both the control (fort.15) file and station file were provided, so the station file will be parsed for the list of stations and the control file will be ignored.");
}
# read station lines from stations file
if ( $stationfile ne "null" ) {
   unless( -e $stationfile && open(STATIONFILE,"<$stationfile")) {
      stderrMessage("ERROR","Could not open $stationfile for reading.");
      die;
   } else {
      stderrMessage("INFO","The number of stations and the station names will be parsed from the stationfile '$stationfile'.");
      # parse out station names
      $num_sta = 0;
      while(<STATIONFILE>) {
         chomp;
         push(@sta_lines,$_);
         $num_sta++;
      }
      close(STATIONFILE);
   }
}
# read station lines from fort.15
if ( ($controlfile ne "null") &&  ($stationfile eq "null") ) {
   unless(-e $controlfile && open(FORT15,"<$controlfile")) {
      stderrMessage("ERROR","Could not open $controlfile for reading.");
      die;
   } else {
      stderrMessage("INFO","The number of stations and the station names will be parsed from the file '$controlfile'.");
      # parse out station names
      while(<FORT15>) {
         my @Fld = split;
         # look for the specified station list
         if ( ( ( $fileToTranspose eq "elevation" ||
                  $fileToTranspose eq "maxelevation" ||
                  $fileToTranspose eq "maxinundationdepth" )
                  && /(NSTAE)/ )
                  ||
              ( ( $fileToTranspose eq "velocity" )
                  && /(NSTAV)/ )
                  ||
              ( ( $fileToTranspose eq "windvelocity" ||
                  $fileToTranspose eq "barometricpressure" ||
                  $fileToTranspose eq "wavedirection" ||
                  $fileToTranspose eq "significantwaveheight" ||
                  $fileToTranspose eq "maxsignificantwaveheight" ||
                  $fileToTranspose eq "maxwindvelocity" )
                  && /(NSTAM)/ ) ) {
            $num_sta = $Fld[0];
            my $station_type = $1;
            for ( my $i=0; $i<$num_sta; $i++ ) {
               my $line =<FORT15>;
               chomp($line);
               $sta_lines[$i] = $line;
            }
            close(FORT15);
            last;
         }
      }
   }
}
# parse the station lines (from whatever source) to get metadata if
# it was provided in standard ASGS metadata format for stations
#
#
# determine the location(s) of the exclamation point(s) that
# are used to mark out the station name and/or station ID
my @sta_coords; # two numbers: longitude and latitude
my @sta_IDs;    # station IDs
my @sta_agencies; # organization that assigned the ID
my @sta_descriptions;    # human-readable station descriptions
for ( my $i=0; $i<$num_sta; $i++ ) {
   $firstBang = index($sta_lines[$i],"!");
   $secondBang = index($sta_lines[$i],"!",$firstBang+1);
   $thirdBang = index($sta_lines[$i],"!",$secondBang+1);
   #print "firstBang $firstBang secondBang $secondBang thirdBang $thirdBang\n";
   # make a guess as to whether the station definition
   # line is in adcirc-standard format as follows:
   # lon lat ! stationID ! agency ! description
   my $standardMetaData = 0;
   if ( $firstBang != -1 && $secondBang != -1 && $thirdBang != -1 ) {
      $standardMetaData = 1;
      $sta_coords[$i] = substr($sta_lines[$i],0,$firstBang);
      $sta_coords[$i] =~ s/^\s+//g; # strip spaces
      $sta_coords[$i] =~ s/\s+$//g;
      $sta_IDs[$i] = substr($sta_lines[$i],$firstBang+1,($secondBang-1-$firstBang));
      $sta_IDs[$i] =~ s/^\s+//g; # strip spaces
      $sta_IDs[$i] =~ s/\s+$//g;
      $sta_agencies[$i] = substr($sta_lines[$i],$secondBang+1,($thirdBang-1-$secondBang));
      $sta_agencies[$i] =~ s/^\s+//g; # strip spaces
      $sta_agencies[$i] =~ s/\s+$//g;
      $sta_descriptions[$i] = substr($sta_lines[$i],$thirdBang+1);
      $sta_descriptions[$i] =~ s/^\s+//g; # strip spaces
      $sta_descriptions[$i] =~ s/\s+$//g;
      #print "$sta_coords[$i]\n";
      #print "$sta_IDs[$i]\n";
      #print "$sta_agencies[$i]\n";
      #print "$sta_descriptions[$i]\n";
      if ( $stationlabel eq "std" ) {
         $stationPlotLabels[$i] = "$sta_agencies[$i] ! $sta_IDs[$i] ! $sta_descriptions[$i]";
      }
   }

   $labelLength = -1;
   if ( $stationlabel eq "betweenbangs" && $firstBang ne -1 && $secondBang ne -1 ) {
      $labelLength = $secondBang - $firstBang;
   } else {
      $labelLength = length($sta_lines[$i]) - $firstBang;
   }
   if ( $stationlabel eq "after exclamation point" || $stationlabel eq "betweenbangs" ) {
      $stationPlotLabels[$i] = substr($sta_lines[$i],$firstBang,$labelLength);
      $stationPlotLabels[$i] =~ s/^\s+|\s+$//g ;
   } elsif ( $stationlabel eq "numbered" ) {
      $stationPlotLabels[$i] = sprintf("%d",$i + 1);
   } elsif ( $stationlabel eq "full" ) {
      $stationPlotLabels[$i] = $sta_lines[$i];
   } elsif ( $stationlabel eq "stationID" ) {
      $stationPlotLabels[$i] = $sta_IDs[$i];
   } elsif ( $stationlabel eq "stationdescription" ) {
      $stationPlotLabels[$i] = $sta_descriptions[$i];
   } elsif ( $stationlabel eq "stationcoords" ) {
      $stationPlotLabels[$i] = $sta_coords[$i];
   }
   # remove leading and trailing whitespaces
   unless ( $stationlabel eq "raw" ) {
      $stationPlotLabels[$i] =~ s/^\s+//g;
      $stationPlotLabels[$i] =~ s/\s+$//g;
   }
}
#
# jgfdebug
stderrMessage("DEBUG","There are $num_sta stations.\n");
#
# check to see if this is scalar data or vector data
my $fileRank = "scalar";
my $filename;
my $fileExtension;
my $total_stations = 0;
my @station_val = ();
my @stationNumber = ();
my @vector_tuple_1 = ();
my @vector_tuple_2 = ();
my $header;       # start on comment on first line of reformatted file
if ($units ne "null" ) {
   $reformattedUnits{$fileToTranspose} = $units;
}
$header = "dataype=$fileToTranspose units=$reformattedUnits{$fileToTranspose}";
$header = $header . " stationfile=$stationfile coldstartdate=$coldstartdate";
$header = $header . " multiplier=$multiplierarg format=$format";
if ( $fileToTranspose eq "elevation" ||
     $fileToTranspose eq "maxelevation" ) {
   $header = "water surface " . $header;
} elsif ( $fileToTranspose eq "maxwindvelocity" ||
          $fileToTranspose eq "timemaxwindvelocity" ) {
   $header .= " windaveragingperiod=$averagingperiod";
} elsif ( $fileToTranspose eq "bathytopo" ) {
   $header = "bathy/topo " . $header;
} elsif ( $fileToTranspose eq "wavedirection" ||
          $fileToTranspose eq "significantwaveheight" ||
          $fileToTranspose eq "maxsignificantwaveheight" ||
          $fileToTranspose eq "maxinundationdepth" ) {
   # $header = ""; ... placeholder
} elsif ( $fileToTranspose eq "barometricpressure" ) {
   $header .= " waterdensity=$density gravitationalaccel=$g";
} elsif ( $fileToTranspose eq "velocity" ) {
   $header .= " vectoroutput=$vectorOutput";
   $fileRank = "vector";
} elsif ( $fileToTranspose eq "windvelocity" ) {
   $header .= " direction=$vectorOutput";
   $fileRank = "vector";
   $header .= " windaveragingperiod=$averagingperiod";
}
# set separator character whether a space-delimited or comma-delimited file
if ( $format eq "comma" ) {
   $fileExtension = ".csv";
} else {
   $fileExtension = ".txt";
}
#---------------------------------------------------------
#      R E A D   S T A T I O N   D A T A   A N D
#     W R I T E   R E F O R M A T T E D   F I L E
#---------------------------------------------------------
# open up the station data file for reading
unless (open(DATAFILE,"<$datafile")) {
   &stderrMessage("ERROR","Could not open data file $datafile for reading: $!.");
   die;
}
# open the dry mask file (assumed to be in fort.61 station file format)
# for use in adding missing (-99999) values in dry areas for non-elevation
# data
if ( $drymaskfile ne "null" ) {
   unless (open(DRYMASKFILE,"<$drymaskfile")) {
     &stderrMessage("ERROR","Could not open data file $drymaskfile for reading: $!.");
     die;
   }
}
# open up the transposed data file for writing
my $transposeFilename = $datafile . "_transpose" . $fileExtension;
if ( $reformattedfile ne "null" ) {
   $transposeFilename = $reformattedfile;
}
unless (open(TRANSPOSE,">$transposeFilename")) {
   &stderrMessage("ERROR","Could not open $transposeFilename for writing: $!.");
   die;
}
#---------------------------------------------------------
#  READ FILE AND PROCESS LINES ACCORDINGLY
#---------------------------------------------------------
my $time;
my $num_datasets = 0;
my $output_frequency = "null";
$s = 0;
while (<DATAFILE>) {
   my $isDry = 0;
   my $drymaskline;
   my @drymaskfields;
   if ( $drymaskfile ne "null" ) {
      $drymaskline = <DRYMASKFILE>;
      if ($. > 2 && ($.-3) % ($total_stations+1) != 0) {
         @drymaskfields = split(' ',$drymaskline);
         if ( $drymaskfields[1] == -99999 ) {
            $isDry = 1;
         }
      }
   }
   #
   # reset the multiplier before processing this line
   $multiplier = 1.0;
   if ($multiplierarg ne "null") {
      $multiplier = $multiplierarg; # save for writing to header of output file
   }
   #
   # if this is the first line in the file it is a comment line; transcribe it to the
   # transposed file as a gnuplot comment line
   if ($. == 1 ) {
      chomp;
      printf TRANSPOSE "# $header comment=\"" . $_ . "\"\n";
      next;
   }
   #
   # 2nd line in the file contains the number of stations;
   # this number must match the number of stations from the fort.15 file
   if ($. == 2) {
      m/^\s*([^\s]*)\s*([^\s]*)\s*([^\s]*)/;
      $total_stations = $2;
      $output_frequency = $3;
      if ( $total_stations != $num_sta ) {
         &stderrMessage("ERROR","The total number of stations in the data file is $total_stations but the total number of stations in the station file is $num_sta. The number of stations must match.");
      }
      stderrMessage("INFO","Output file '$transposeFilename' contains $total_stations stations.\n");
      stderrMessage("INFO","Output frequency is $output_frequency seconds.\n");
      # if the user didn't specify a fort.15 or station file, create default station names,
      # using the number of stations we just found in the station output file
      if ( $defaultStationLabels == 1 ) {
         for (my $i=0; $i<$total_stations; $i++ ) {
            my $default_station_number = $i+1;
            $sta_names[$i] = "Station" . $default_station_number;
         }
      }

      next;
   }
   #
   # there is a header line with the time, at the start of each dataset
   if ($. > 2 && ($.-3) % ($total_stations+1) == 0) {
      $s=0;  # reinitialize the station counter
      #
      # grab the new time (assumed to be in gmt)
      my $data_seconds = "null";
      if ( $hstime ne "null" ) {
         # use the hotstart time and output frequency to compute the time
         # in seconds associated with this dataset
         $data_seconds = $hstime + $output_frequency * ($num_datasets + 1);
      } else {
         m/^\s*([^\s]*)\s*([^\s]*)\s*$/;
         $data_seconds = $1;
      }
      if ( $coldstartdate ne "null" ) {
         # converting to specified local time
         ($year,$month,$day,$hour,$min,$sec)
            = Date::Calc::Add_Delta_DHMS($cs_year,$cs_mon,$cs_day,
               $cs_hour,$cs_min,$cs_sec,0,$gmtoffset,0,sprintf("%2d",$data_seconds));
         $time = sprintf("%4s-%02s-%02s %02s:%02s:%02d$separator",
                   $year,$month,$day,$hour,$min,$sec);
      } else {
         $time = $data_seconds;
      }
      next;
   }

   # this is data (not the file header or the individual dataset header)

   if ( $fileRank eq "scalar" ) {
      my $scalar;
      # parse out the station number and the value
      m/^\s*(\d*)\s*(\S*)\s*/;
      $stationNumber[$s] = $1;
      if ( $stationlabel eq "raw" ) {
         $stationPlotLabels[$s] = $stationNumber[$s];
      }
      # set missing value or station value
      if ( $2 == "-0.9999900000E+05" || $2 =~/NaN/ || $isDry == 1 ) {
         $scalar = -99999;
      } else {
         # apply the user-specified multiplier given on the command line
         $scalar = $2*$multiplier;
      }
      #
      # the default is to pass through the data in ADCIRC SI units but
      # conversions are possible
      if ( $scalar != -99999 ) {
         if  ( ($fileToTranspose eq "elevation") ||
               ($fileToTranspose eq "maxelevation") ||
               ($fileToTranspose eq "maxvelocity") ||
               ($fileToTranspose eq "bathytopo") ||
               ($fileToTranspose eq "maxinundationdepth") ||
               ($fileToTranspose eq "significantwaveheight") ||
               ($fileToTranspose eq "maxsignificantwaveheight")
                ) {
            if ( ($units eq "ft") || ($units eq "feet") || ($units eq "fps" ) ) {
               # convert m to ft
               $scalar *= (100.0 / (2.54 * 12.0));
            }
         }
         if ( $fileToTranspose eq "barometricpressure" ) {
            if ( ($units eq "mb") || ($units eq "millibar" ) || ($units eq "millibars" ) ) {
               # convert from m of water to mb: multiply by rho_0 g to get
               # Pascals, then divide by 100 to get mb
               $scalar *= ($density * $g / 100.0);
            }
         }
         if ( $fileToTranspose eq "maxwindvelocity" ) {
            if ( $units eq "mph" ) {
               $scalar *= 2.2369363;
            } elsif ( $units eq "kts" || $units eq "kts" ) {
               $scalar *= 1.0/0.514444444; # convert m/s to kts
            }
            if ( $averagingperiod eq "1min" ) {
               $scalar *= 1.136; # convert 10 minute winds to 1 minute winds
            }
         }
         # if this is time of occurrence data convert to a date based
         # on the cold start date/time and the value
         if ( $fileToTranspose eq "timemaxelevation" ||
              $fileToTranspose eq "timemaxwindvelocity" ||
              $fileToTranspose eq "timemaxvelocity" ||
              $fileToTranspose eq "timemaxsignificantwaveheight" ) {
            if ( $coldstartdate ne "null" ) {
               # converting to date/time string with timezone
               ($year,$month,$day,$hour,$min,$sec)
                  = Date::Calc::Add_Delta_DHMS($cs_year,$cs_mon,$cs_day,
                  $cs_hour,$cs_min,$cs_sec,0,$gmtoffset,0,sprintf("%2d",$scalar));
               $scalar = sprintf("\"%4s-%02s-%02s %02s:%02s:%02d $timezone\"",
                   $year,$month,$day,$hour,$min,$sec);
               if ( $format eq "comma" ) {
                  $scalar = sprintf("%4s-%02s-%02s %02s:%02s:%02d $timezone",
                   $year,$month,$day,$hour,$min,$sec);
               }
            } else {
               # leave the data value as the time in seconds since cold
               # start if we don't have a date/time associated with cold start
            }
         }
      }
      $station_val[$s] = $scalar;
   } else {
      # this is vector data
      my $tuple_1;
      my $tuple_2;
      if ( $units eq "kts" || $units eq "knots" || $units eq "kt" ) {
         $multiplier *= 1.0/0.514444444; # convert m/s to kts
      }
      if ( $fileToTranspose eq "windvelocity" && $averagingperiod eq "1min") {
         $multiplier *= 1.136; # convert 10 minute winds to 1 minute winds
      }
      if ( ($units eq "ft/s") || ($units eq "fps" )  ) {
         # convert m to ft
         $multiplier *= (100.0 / (2.54 * 12.0));
      } elsif ( $units eq "mph" ) {
         $multiplier *= 2.2369363;
      }
      # parse out the station number and the two data values
      m/^\s*([^\s]*)\s*([^\s]*)\s*([^\s]*)\s*/;
      $stationNumber[$s] = $1;
      if ( $stationlabel eq "raw" ) {
         $stationPlotLabels[$s] = $stationNumber[$s];
      }
      if ( !($2 =~ /NaN/) && !($2 =~/Inf/) && !($2 == -99999 ) && !($isDry == 1) ) {
         $tuple_1 = $2 * $multiplier;
      } else {
         $tuple_1 = -99999;
      }
      if ( !($3 =~ /NaN/) && !($3 =~/Inf/) && !($3 == -99999 ) && !($isDry == 1) ) {
         $tuple_2 = $3 * $multiplier;
      } else {
         $tuple_2 = -99999;
      }
      $vector_tuple_1[$s] = $tuple_1;
      $vector_tuple_2[$s] = $tuple_2;
   }
   #

   # if we have collected a complete dataset, write it now


   if ( $s == $total_stations-1 ) {

      # if this is the first dataset to be written, write the column
      # header with the names of the stations according to the
      #requested format
      if ( $num_datasets == 0 ) {
         if ( $minmax == 0 && $time_minmax == 0 && $fileToTranspose ne "bathytopo" ) {
            unless ( $coldstartdate eq "null" ) {
               printf TRANSPOSE "# DATE TIME" . $separator . "TIMEZONE" . $separator;
            } else {
               printf TRANSPOSE "# TIMESEC" . $separator;
            }
         }
         foreach (@stationPlotLabels) {
             # scalar plot labels
             if ( $fileRank eq "scalar" ) {
                printf TRANSPOSE "\"$_ ($reformattedUnits{$fileToTranspose})\"" . $separator;
             } else {
                # vector plot labels
                if ( $vectorOutput eq "raw" ) {
                    printf TRANSPOSE "\"$_ (east $reformattedUnits{$fileToTranspose})\"" . $separator . "\"$_ (north $reformattedUnits{$fileToTranspose})\"" . $separator ;
                } else {
                    # either trig degrees or compass degrees
                    printf TRANSPOSE "\"$_ ($reformattedUnits{$fileToTranspose})\"" . $separator . "\"$_ ($vectorOutput) \"" . $separator ;
                }
            }
         }
         printf TRANSPOSE "\n";

      }

      # first column: date/time, unless this is time_minmax file, in which
      # case, we don't need the time in the first column (there is only
      # one row)
      if ( $minmax == 0 && $time_minmax == 0 && $fileToTranspose ne "bathytopo" ) {
         # create the column for the time and time zone
         unless ( $coldstartdate eq "null" ) {
            printf TRANSPOSE $time . "$timezone" . $separator;
         } else {
            printf TRANSPOSE $time . $separator;
         }
      } else {
         # do nothing: don't need a time column in a minmax or time of minmax file
      }
      # write scalar data
      if ( $fileRank eq "scalar" ) {
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
               my $magnitude = sqrt($vector_tuple_1[$i]*$vector_tuple_1[$i] + $vector_tuple_2[$i]*$vector_tuple_2[$i]);
               my $trigdirection = 0.0;
               my $compassdirection = 0.0;
               # avoid division by zero if north velocity is zero
               if ( $vector_tuple_2[$i] == 0.0 && $vector_tuple_1[$i] >= 0.0 ) {
                  # wind/current going due east
                  $trigdirection = 0.0;
               } elsif ( $vector_tuple_2[$i] == 0.0 && $vector_tuple_1[$i] < 0.0 ) {
                  # wind/current going due west
                  $trigdirection = 180.0;
               } else {
                  $trigdirection = (180.0/$pi) * atan2($vector_tuple_2[$i],$vector_tuple_1[$i]);
                  # get a positive value from 0 to 360
                  if ( $trigdirection < 0.0 ) {
                     $trigdirection += 360.0;
                  }
               }
               # compass degrees for water current reflect the direction the current is going
               if ( $fileToTranspose eq "velocity" ) {
                  $compassdirection = 90.0 - $trigdirection; # add 360.0 later
               } else {
                  # compute wind direction in compass degrees based on the standard that winds
                  # are reported as the direction they are coming from
                  $compassdirection = 90.0 - $trigdirection;
                  if ( $compassdirection < 180.0 ) {
                     $compassdirection += 180.0;
                  } else {
                     $compassdirection -= 180.0;
                  }
               }
               if ( $compassdirection <= 0.0 ) {
                  $compassdirection += 360.0;
               }
               # write out the magnitude and direction
               if ( $vectorOutput eq "compassdegrees" ) {
                  printf TRANSPOSE (" %20s , %20s ",$magnitude,$compassdirection);
               } else {
                  printf TRANSPOSE (" %20s , %20s ",$magnitude,$trigdirection);
               }
            } else {
               # we have invalid values
               printf TRANSPOSE "-99999 -99999";
            }
            printf TRANSPOSE $separator;
         }
         @vector_tuple_1 = ();
         @vector_tuple_2 = ();
      }
      printf TRANSPOSE "\n";
      $num_datasets++;
   }
   $s++;
}
close(TRANSPOSE);
#
# Final log messages
&stderrMessage("INFO","Wrote $num_datasets datasets.\n");
#
if ( $fileToTranspose eq "windvelocity" || $fileToTranspose eq "maxwindvelocity" ) {
   &stderrMessage("INFO","The wind data were written with a multiplier corresponding to a $averagingperiod averaging period.");
   if ( $vectorOutput eq "compassdegrees" ) {
      &stderrMessage("INFO","The wind direction was written in compass degrees that indicate the direction the winds are coming FROM (i.e., a direction of 0 degrees indicates winds blowing from the north to the south).");
   }
}
if ( $vectorOutput eq "trigdegrees" ) {
   &stderrMessage("INFO","The vector direction was written in trigonometric degrees that indicate the direction the wind/current is going (i.e., a direction of 0 degrees indicates motion from west to east).");
}
#
#-----------------------------------------------------------------
#       F U N C T I O N      I S   M E M B E R
#-----------------------------------------------------------------
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
#
#-----------------------------------------------------------------
#       F U N C T I O N     L O A D   P R O P E R T I E S
#-----------------------------------------------------------------
# load all properties from the run.properties metadata file produced
# by the ASGS; these consist of key/value pairs separated by ":"
sub loadProperties {
   if ( -e $runproperties ) {
      unless (open(RP,"<$runproperties") ) {
         &stderrMessage("ERROR","Found the run.properties file but could not open it: $!.");
         die;
      } else {
         &stderrMessage("INFO","Found and opened the run.properties file.");
      }
   } else {
      &stderrMessage("INFO","The run.properties file was not found.");
      $runproperties = "null";
      return;
   }
   while (<RP>) {
      my $k;  # property key
      my $v;  # property value
      my $colon = index($_,":");
      #($k,$v)=split(':',$_);
      $k = substr($_,0,$colon);
      $v = substr($_,$colon+1,length($_));
      chomp($k);
      chomp($v);
      # remove leading and trailing whitespaces from the key and value
      $k =~ s/^\s+//g;
      $k =~ s/\s+$//g;
      $v =~ s/^\s+//g;
      $v =~ s/\s+$//g;
      #&stderrMessage("DEBUG","loadProperties: key is '$k' and value is '$v'");
      $properties{$k}=$v;
   }
   close(RP);
}
#
#-----------------------------------------------------------------
#       F U N C T I O N    S T D E R R M E S S A G E
#-----------------------------------------------------------------
sub stderrMessage ($$) {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $theTime = "[$year-".sprintf("%3s",$months[$month])."-".sprintf("%02d",$dayOfMonth)."-T".sprintf("%02d",$hour).":".sprintf("%02d",$minute).":".sprintf("%02d",$second)."]";
   printf STDERR "$theTime $level: station_transpose.pl: $message\n";
}

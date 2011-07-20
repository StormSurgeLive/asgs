#!/usr/bin/env perl
#---------------------------------------------------------------------
# storm_track_gen.pl
#
# This script accepts raw ATCF files from the NHC and produces various
# fort.22 files for ADCIRC (NWS=9). It accepts a forecast file 
# format and optionally a hindcast file. It also requires the cold start
# time. By default, it produces a fort.22 file (ADCIRC NWS=9 format, i.e.,
# for the asymmetric vortex wind model) that represents the NHC consensus
# forecast. 
#---------------------------------------------------------------------
#
# ASSUMPTIONS:
#
# 1. The ADCIRC coldstart time (or the sum of the coldstart time and the number
# of seconds from the hotstart file in the case of a hotstart) must correspond
# exactly to one of the times in the hindcast or forecast file.
#
# 2. Conversion of km to degrees (lat and lon) is only approximate and 
# could be made more accurate.
#
# 3. The fill-in of the forecast central pressure is based on an algorithm
# under development by Jason Fleming (jason.fleming@seahorsecoastal.com). This
# algorithm is the subject of current research and is subject to change.
#
#---------------------------------------------------------------------
#
# Copyright(C) 2006, 2007, 2008, 2009 Jason Fleming
# Copyright(C) 2006, 2007 Brett Estrade
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
#
#---------------------------------------------------------------------
#
# VERSION information:
# jgf20090624: initial release. Does not support Rmax variations yet. Code
# that fills in the wind radii has a bug in it.
#
use strict;
use Getopt::Long;
use Date::Pcalc;
use Cwd;
$^W++;

# usage: perl storm_track_gen.pl --dir /path/to/atcf/files --storm 09 --year 2009 --startdate 2009081400 --name nhcConsensus 
# the output will be stored in a file called fort.22

my $dir;                            # path to raw ATCF hindcast and forecast
my $storm;                          # number, e.g., 05 or 12 
my $year;                           # YYYY
my $coldstartdate;                  # YYYYMMDDHH24
my $hotstartseconds = 0.0;          # default is not hotstart
my $nws = 8;                        # the ADCIRC wind model to target
my $name = "nhcConsensus";          # default track to generate
my $percent;                        # magnitude of parameter variation
my $strengthPercent = 20.0;
my $overlandSpeedPercent = -20.0;
my $sizePercent = 20.0;
my $veerPercent = 100.0;
my $pi=3.141592653589793;
# if the NHC issues a special advisory, there may be incomplete lines in the 
# hindcast file. This hash will save the most recent complete lines, to fill
# in any missing data.
my %complete_hc_lines = ();
my $nhcName;  # NHC's current storm name (IKE, KATRINA, INVEST, ONE, etc)
my $stormClass; # NHC's current storm classification (TD, TS, HU, IN, etc)
#
#
GetOptions(
           "dir=s" => \$dir,
           "storm=s" => \$storm,
           "year=s" => \$year,
           "coldstartdate=s" => \$coldstartdate,
           "hotstartseconds=s" => \$hotstartseconds,
           "nws=s" => \$nws,
           "name=s" => \$name,
           "percent=s" => \$percent
           );
#
# check to see that all the mandatory command line arguments were specified
unless ( $dir ) {
   $dir = cwd();
   stderrMessage("WARNING","The path to the raw ATCF input files was not specified with the --dir argument. It will be assumed that the files are in the directory $dir.");
} 
unless ( $storm ) {
   stderrMessage("ERROR","The storm number was not specified using the --storm argument.");
   die;
}
unless ( $year ) {
   stderrMessage("ERROR","The year was not specified using the --year argument.");
}
unless ( $coldstartdate ) {
   my $hindcastATCF = "$dir/bal$storm$year.dat";
   unless ( open(HCST,"<$hindcastATCF") ) {
      stderrMessage("ERROR","Failed to open hindcast ATCF file $hindcastATCF for ensemble member '$name': $!.");
      die;
   }
   while(<HCST>) {
      my @fields = split(',',$_);
      $coldstartdate = $fields[2];
   }
   close(HCST);
   $coldstartdate =~ s/\s*//g; # remove spaces
   stderrMessage("INFO","The cold start date was not specified using the --coldstartdate argument. The date/time of the most recent hindcast is '$coldstartdate'. This will be used as the coldstart date/time.");
   printf STDOUT $coldstartdate;
}
# 
# set the percent for variables that can be adjusted by percentages
if ( $percent ) {
   if ( $name =~ /maxWindSpeed/) {
      $strengthPercent = $percent;
   } elsif ($name =~ /overlandSpeed/) {
      $overlandSpeedPercent = $percent;
   } elsif ( $name =~ /veer/ ) {
      $veerPercent = $percent;
   } elsif ( $name =~ /rMax/ ) {  
      # the rmax variation is controlled by the aswip program
   } else {
      stderrMessage("INFO","The option '--percent' was specified at '$percent', but the ensemble member '$name' does not use percentage information. The percentage value will be ignored."); 
   }
}
#
# send a message indicating the percent that will be used
if ( $name =~ /maxWindSpeed/ ) {
   stderrMessage("INFO","The forecast maximum wind speed will be modified by $strengthPercent percent.");
} elsif ($name =~ /overlandSpeed/) {
   stderrMessage("INFO","The forecast overland speed will be modified by $overlandSpeedPercent percent.");
} elsif ( $name =~ /veer/ ) {
   stderrMessage("INFO","The forecast track will be modified with a veer of $veerPercent percent.");
} elsif ( $name =~ /rMax/ ) { 
   # Rmax variation is implemented in aswip
}
#
# open ATCF input files
my $forecastATCF = "$dir/al$storm$year.fst";
unless (open(FCST,"<$forecastATCF")) {
   stderrMessage("ERROR","Failed to open forecast ATCF file $forecastATCF for ensemble member '$name': $!.");
   die;
}
my $hindcastATCF = "$dir/bal$storm$year.dat";
unless (open(HCST,"<$hindcastATCF")) {
   stderrMessage("ERROR","Failed to open hindcast ATCF file $hindcastATCF for ensemble member '$name': $!.");
   die;
}
#
# create the fort.22 output file, which is the wind input file for ADCIRC
unless (open(MEMBER,">fort.22")) {
   stderrMessage("ERROR","Failed to open file for ensemble member '$name' fort.22 output file: $!.");
   die;
}
#
# create the file that holds the current storm class and name ... this is 
# only used for the title on the hydrographs
unless (open(NHCCLASSNAME,">nhcClassName")) { 
   stderrMessage("ERROR","Failed to open file 'nhcClassName' to write NHC storm class and name: $!.");
   die;
}
#
# preprocess and rearrange ATCF files if necessary
my @rad;                     # wind radii in the 4 quadrants (current time)
my @oldrad;                  # wind radii in the 4 quadrants (previous time)

my $firstHindcastTime = "";
my $lasthindcasttime;
my $lasthindcastpressure;
my $lasthindcastwindspeed;
my $lasthindcastrmax;
my $old_lat;
my $old_lon;
my $hours = 0;
$coldstartdate =~ m/\s*(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $csyear = $1;
my $csmon = $2; 
my $csday = $3; 
my $cshour = $4;
my $fhcyear; my $fhcmon; my $fhcday; my $fhchour; # first relevant hindcast line
my $hyear; my $hmon; my $hday; my $hhour;         # this relevant hindcast line
my $zdyear; my $zdmon; my $zdday; my $zdhour;     # the zero date
my $zdmin; my $zdsec;                             # not used
($zdyear,$zdmon,$zdday,$zdhour,$zdmin,$zdsec) =
   Date::Pcalc::Add_Delta_DHMS($csyear,$csmon,$csday, $cshour,0,0,0,0,0,$hotstartseconds); 
my $zeroDate = sprintf("%4d%02d%02d%02d",$zdyear,$zdmon,$zdday,$zdhour); 
printf STDERR "INFO: storm_track_gen.pl: The fort.22 will be configured to start on $zeroDate UTC.\n";
my $zdFound = 0; # set to 1 if/when we find the zero date in the file
my $fyear; my $fmon; my $fday; my $fhour; # time at which forecast is valid
my $ftyear; my $ftmon; my $ftday; my $fthour; # time to which forecast applies
my $ftmin; my $ftsec;                             # not used
#---------------------------------------------------------------------
# P R O C E S S I N G   H I N D C A S T   F I L E 
#---------------------------------------------------------------------
while(<HCST>) {
    my @fields = split(',',$_);
    my $line = $_;
    # check to see if this is a complete line (meaning that all the fields
    # up to and including the storm name are there)
    my $line_length = length($line);
    #jgfdebug printf STDERR "length of line $. is $line_length\n";
    my $isotach_kts = substr($line,63,3);
    if ( $line_length >= 159 ) { # this is a complete line
       # the first isotach is 34, but can be 0 in the source data in some cases
       if ( $isotach_kts == 34 || $isotach_kts == 0 ) {
          # clear out hash so that this data is always fresh
          %complete_hc_lines = ();
       }
       # save it as-is in case we need to use it to fill in incomplete 
       # lines that may occur later 
       $complete_hc_lines{$isotach_kts} = $line;
    } else {
       stderrMessage("WARNING","Line $. in the hindcast file is incomplete: $line");
       # fill in from a corresponding complete line from the hash, if possible
       my $last_complete_line = $complete_hc_lines{$isotach_kts};
       if ( $last_complete_line ) {
          # splice the complete line onto the incomplete line
          $line = $line . substr($last_complete_line,$line_length,999);
          stderrMessage("WARNING","That line will be replaced with the following line: $line");  
       } else {
          # there wasn't a corresponding line in the hash ... safest thing
          # to do is to drop this hindcast line entirely
          stderrMessage("WARNING","The incomplete line could not be filled in with data from prior lines, and will be dropped.");
          next;
       }
    }
    #
    # grab the current storm name and class
    if (defined $fields[27]) {
       $fields[27]=~/\s*(\S*)\s*/; # strip spaces from current storm name
       $nhcName = $1;
    } else { 
       $nhcName = "STORMNAME";
    }
    if (defined $fields[10]) {
       $fields[10]=~/\s*(\S*)\s*/; # strip spaces from current storm class
       $stormClass = $1;
    } else {
       $stormClass = " ";
    }
    #
    # record the final hindcast time, this will be used in case the
    # hindcast is newer than the forecast
    $lasthindcasttime = $fields[2];
    #
    # record the last hindcast values, these may be used in the 
    # fill-in of later values
    $lasthindcastpressure=substr($line,53,4);
    $lasthindcastwindspeed=substr($line,48,3);
    $lasthindcastrmax=substr($line,109,3);
    #
    # want to save the nowcast storm position
    $old_lat=substr($line,34,4)/10.0;
    $old_lon=substr($line,41,4)/10.0;
    #
    # grab the wind radii in the four quadrants
    #$rad[0]=substr($line,74,3);
    #$rad[1]=substr($line,80,3);  
    #$rad[2]=substr($line,86,3);
    #$rad[3]=substr($line,92,3);
    # jgfdebug20090624: the sub that fills in the rmax is not working
    #populateWindRadii(\@rad,\@oldrad,$lasthindcastrmax);
    #
    # for NWS 9 and 19, check to see if the hindcast line is prior to the 
    # zero date, if it is, then it will not be placed in the fort.22 file
    # for NWS 8, put all lines in the file, it will figure out which one 
    # it needs
    # jgf20110720: Added possibility of swan coupling
    if ( $nws == 9 || $nws == 19 || $nws == 309 || $nws == 319 ) {
       if ( $fields[2] < $zeroDate ) {
          next;
       }
    }
    # check to see if we have found the zero hour in the hindcast file
    if ( $fields[2] == $zeroDate ) {
       $zdFound = 1;
    }
    if ( $nws == 9 || $nws == 19 || $nws == 309 || $nws == 319 ) {	
       if ( ($zdFound == 0) && ($fields[2] > $zeroDate) ) {
          stderrMessage("ERROR","The date '$fields[2]' was encountered in the hindcast file '$hindcastATCF'; however an exact match of the starting date '$zeroDate' should have preceded it somewhere. Therefore, the file does not contain the proper starting date (i.e., the zero date). The fort.22 file will not be written.");
          die;
       }  
    }
    # grab the first relevant hindcast line; this is the zero hour 
    unless ($firstHindcastTime) {
       $firstHindcastTime = $fields[2];
       $firstHindcastTime =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
       $fhcyear = $1;
       $fhcmon = $2; 
       $fhcday = $3; 
       $fhchour = $4;
    }    
    $fields[2] =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
    $hyear = $1; 
    $hmon = $2; 
    $hday = $3; 
    $hhour = $4;
    # get difference between zero hour and this hindcast time 
    (my $ddays,my $dhrs, my $dsec) = Date::Pcalc::Delta_DHMS($fhcyear,$fhcmon,$fhcday,$fhchour,0,0,$hyear,$hmon,$hday,$hhour,0,0);
    my $time_difference = $ddays*24 + $dhrs; # in hours  
    if ( $nws == 9 || $nws == 19 || $nws == 309 || $nws == 319 ) {
       # fill in the time difference as tau
       substr($line,29,4)=sprintf("%4d",$time_difference);
    }
    #
    # set the background pressure to 1013
    substr($line,97,4)=sprintf("%4d",1013);
    #
    # fill in the radii values
    #substr($line,74,3)=sprintf("%3d",$rad[0]);
    #substr($line,80,3)=sprintf("%3d",$rad[1]);  
    #substr($line,86,3)=sprintf("%3d",$rad[2]);
    #substr($line,92,3)=sprintf("%3d",$rad[3]);
    # write the line to the file, writing an eol if the line does not have one
    if ( /\n/ ) {
       print MEMBER $line;
    } else { 
       printf MEMBER "$line\n";
    }
}
close(HCST);
if ( $zdFound == 0 ) {
   stderrMessage("INFO","The zero date '$zeroDate' was not found in the hindcast file $hindcastATCF."); 
}
#
# write the last current storm class and name to file
printf NHCCLASSNAME $stormClass . " " . $nhcName;
close(NHCCLASSNAME);
my $forecastedDate; # as a string
my $last_pressure = $lasthindcastpressure;
my $last_windspeed = $lasthindcastwindspeed;
my $consensus_angle=0;      # direction of motion of consensus track
my $old_consensus_angle=0;  # previous direction of consensus track
#---------------------------------------------------------------------
# P R O C E S S I N G   F O R E C A S T   F I L E 
#---------------------------------------------------------------------
while(<FCST>) {
   my @fields = split(',',$_);
   my $line = $_;
   # grab the datetime at which the forecast is valid 
   $fields[2] =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $fyear = $1; 
   $fmon = $2; 
   $fday = $3; 
   $fhour = $4;
   # grab the existing forecast period, i.e., the number of hours beyond the
   # forecast datetime that the forecast applies to
   my $tau=substr($_,29,4);   
   # determine the date and time that the forecast applies to
   ($ftyear,$ftmon,$ftday,$fthour,$ftmin,$ftsec) =
     Date::Pcalc::Add_Delta_DHMS($fyear,$fmon,$fday, $fhour,0,0,0,$tau,0,0); 
   my $forecastedDate = sprintf("%4d%02d%02d%02d",$ftyear,$ftmon,$ftday,$fthour); 
   #
   # if the forecastedDate is before the last hindcast date, then ignore 
   # this line and go to the next one
   if ( $forecastedDate < $lasthindcasttime ) {
      next;
   }  
   #
   # check to see if the forecast line is prior to the zero date,
   # if it is, then it will not be placed in the fort.22 file
   if ( $forecastedDate < $zeroDate ) {
      next;
   }
   # if we have found the zero hour in the forecast file
   if ( $forecastedDate == $zeroDate ) {
      $zdFound = 1;
   }
   if ( $nws == 9 || $nws == 19 || $nws == 309 || $nws == 319 ) {
      if ( ($zdFound == 0) && ($forecastedDate > $zeroDate) ) {
         stderrMessage("ERROR","The date found in the forecast file '$forecastATCF' is after the zero hour of '$zeroDate', but exact zero date was never found.");
         die;
      }
   }
   # 
   # fill in the forecasted date for metadata purposes (i.e., this is
   # not used by ADCIRC)
   substr($line,8,10)=sprintf("%10d",$forecastedDate);
   #
   # next, calculate the difference between the forecasted date and the zero
   # hour so that we can fill in the forecast period
   (my $ddays,my $dhrs, my $dsec) = Date::Pcalc::Delta_DHMS($zdyear,$zdmon,$zdday,$zdhour,0,0,$ftyear,$ftmon,$ftday,$fthour,0,0);
   my $time_difference = $ddays*24 + $dhrs; # in hours  
   if ( $nws == 9 || $nws == 19 || $nws == 309 || $nws == 319 ) {
      # fill in the time difference as tau
      substr($line,29,4)=sprintf("%4d",$time_difference);
   }
   #
   # set the background pressure to 1013
   substr($line,97,4)=sprintf("%4d",1013);
   #
   # grab the wind radii in the four quadrants
   $rad[0]=substr($_,74,3);
   $rad[1]=substr($_,80,3);  
   $rad[2]=substr($_,86,3);
   $rad[3]=substr($_,92,3);
   # jgfdebug20090624: the sub that fills in the rmax is not working
   #populateWindRadii(@rad,@oldrad,$lasthindcastrmax);
   # fill in the radii values
   substr($line,74,3)=sprintf("%3d",$rad[0]);
   substr($line,80,3)=sprintf("%3d",$rad[1]);  
   substr($line,86,3)=sprintf("%3d",$rad[2]);
   substr($line,92,3)=sprintf("%3d",$rad[3]);
   my $forecast_windspeed=substr($_,48,3);
   my $forecast_pressure=substr($_,53,4);
   # 
   # fill in the forecast central pressure, if it is missing
   if ( $forecast_pressure == 0 ) {
      # same as last time by default
      $forecast_pressure = sprintf("%4d",$last_pressure);
      # if stronger
      if ( $forecast_windspeed > $last_windspeed ) {
         $forecast_pressure = sprintf("%4d",(1040.0-0.877*$forecast_windspeed));
         # the resulting pressure should be lower than the last ... if it isn't,
         # just use the slope
         if ($forecast_pressure > $last_pressure ) {
            $forecast_pressure = sprintf("%4d",($last_pressure 
               - 0.877*($forecast_windspeed-$last_windspeed))); 
         }
      }
      # if weaker
      if ( $forecast_windspeed < $last_windspeed ) {
         $forecast_pressure = sprintf("%4d",(1000.0-0.65*$forecast_windspeed));
         # the resulting pressure should be higher than the last ... if it isn't,
         # just use the slope
         if ($forecast_pressure < $last_pressure ) {
            $forecast_pressure = sprintf("%4d",($last_pressure 
               + 0.65*($last_windspeed-$forecast_windspeed))); 
         }
      }
      # slower windspeeds can be strange ... just use the last pressure
      if ( $forecast_windspeed <= 30 ) {
         $forecast_pressure = sprintf("%4d",$last_pressure);
      }
      # fill in the forecast central pressure value
      substr($line,53,4) = $forecast_pressure;
   }
   $last_pressure = $forecast_pressure;
   $last_windspeed = $forecast_windspeed;
   #
   # if the requested variation is max wind speed, modify the forecast
   # max wind speed
   if (($name =~ /maxWindSpeed/) && ($tau != 0)) {
       my $vmax=substr($_,47,4);
       # change it by the indicated percentage
       substr($line,47,4)=sprintf("%4d",$vmax*(1.0+($strengthPercent/100.0))); 
   }
   #
   # if the requested variation is overland speed, modify the forecast
   # period and forecastedDate
   if (($name =~ /overlandSpeed/) && ($tau != 0)) {
       my $newtau = $tau*(1.0+(-$overlandSpeedPercent/100.0));
       # determine the date and time that the forecast applies to
       ($ftyear,$ftmon,$ftday,$fthour,$ftmin,$ftsec) =
       Date::Pcalc::Add_Delta_DHMS($fyear,$fmon,$fday, $fhour,0,
          0,0,$newtau,0,0); 
       # recalculate the difference between the forecasted time and the zero
       # hour so that we can fill in the forecast period
       (my $ddays,my $dhrs, my $dsec) = Date::Pcalc::Delta_DHMS($fhcyear,$fhcmon,$fhcday,$fhchour,0,0,$ftyear,$ftmon,$ftday,$fthour,0,0);
       my $time_difference = $ddays*24 + $dhrs; # in hours  
       # fill in the time difference as tau
       substr($line,29,4)=sprintf("%4d",$time_difference);
       $forecastedDate 
          = sprintf("%4d%02d%02d%02d",$ftyear,$ftmon,$ftday,$fthour);
       # fill in the date and time for metadata purposes
       substr($line,8,10)=sprintf("%10d",$forecastedDate);
   }
   # if the requested variation is veer, modify the track so that it veers
   # as a percent of the cone of uncertainty
   # -100% will create a track that lies along the left edge of 
   # the cone of uncertainty
   # +100% will create a track that lies along the right edge of the cone
   # of uncertainty
   if (($name =~ /veer/) && ($tau != 0)) {
      my $radius;                 # radius of uncertainty
      $radius=interpolateUncertaintyRadius($tau);
      # scale to the percentage requested 
      $radius *= abs($veerPercent/100.0);
      # convert nautical miles to km
      $radius*=1.852000003180799; # to km
      # grab consensus forecast position 
      my $consensusLat=substr($_,34,4)/10.0; # from tenths of degs to degs
      my $consensusLon=substr($_,41,4)/10.0; # from tenths of degs to degs
      # find the angle that consensus storm is traveling on.
      my $lat_change=$consensusLat-$old_lat;
      my $lon_change=-1*($consensusLon-$old_lon); # lon increases leftward
      unless ( $lat_change==0.0 && $lon_change==0.0 ) {
         $consensus_angle=atan2($lat_change,$lon_change);
         # save current direction of consensus track, in case track is 
         # stationary in the future, so we can use the direction to 
         # calculate a reasonable veer track
         $old_consensus_angle = $consensus_angle;    
      } else {
         $consensus_angle=$old_consensus_angle;
      }
      # calculate position of veering track based on direction, setting
      # the angle according to the sign of the veer percent 
      my $veer_xoff = 0;
      my $veer_yoff = 0;
      my $perpendicular; 
      if ($veerPercent > 1) {
         $perpendicular = - ($pi/2); # veer right
      } else {
         $perpendicular = $pi/2;     # veer left
      }
      my $veer_angle = $consensus_angle + $perpendicular;
      # approximate offsets in degrees (radius is in km)
      $veer_xoff=$radius*cos($veer_angle)/100.0; 
      $veer_yoff=$radius*sin($veer_angle)/100.0;
      # calculate lat and lon of veer track and convert to 10ths
      # of degrees
      my $veer_lat=($consensusLat+$veer_yoff)*10.0;
      my $veer_lon=($consensusLon-$veer_xoff)*10.0;
      # paste in the new position
      substr($line,34,4)=sprintf("%4d",$veer_lat);
      substr($line,41,4)=sprintf("%4d",$veer_lon);
      $old_lat=$consensusLat;
      $old_lon=$consensusLon;
   }
   # If NWS is 8, fill in the Rmax. If the requested variation is Rmax, 
   # change it and then fill it in.
   if ( $nws == 8 ) { 
      my $rmax = $lasthindcastrmax;
      if ( $name eq "rMax") { 
         $rmax *= $sizePercent;
      }
      substr($line,109,3)=sprintf("%3d",$rmax);
   } 
   # write the line to the file, writing an eol if the line does not have one
   if ( /\n/ ) {
      print MEMBER $line;
   } else { 
      printf MEMBER "$line\n";
   }
}
close(FCST);
close(MEMBER);
if ( $zdFound == 0 ) {
   if ( $nws == 9 || $nws == 19 || $nws == 309 || $nws == 319 ) {
      stderrMessage("ERROR","The zero hour '$zeroDate' was not found in the hindcast file $hindcastATCF or the forecast file $forecastATCF."); 
   }
}
1;
    
#------------------------------------------------------------------------
# populateRadii: This subroutine checks the wind radii to see if any are
# zero (the nws9 subroutine in ADCIRC cannot generate winds if any of the
# radii are zero). The zero radii (if any) are populated according to
# the assumptions listed at the top of the program.
#------------------------------------------------------------------------
sub populateWindRadii { 
    my @rad=shift;
    my @oldrad=shift;
    my $currentRmax=shift;
    my $numNonZeroRad = 0;
    my $avgRad;
    # find nonzero radii and count them
    foreach (@rad) {
       if ($_ != 0 ) {
          $numNonZeroRad++;
          $avgRad += $_;  
       }  
    }
    # estimate the radii if necessary
    if ( $numNonZeroRad != 0 ) {
       # at least one was not zero, calculate and substitute average value
       # for the zero value(s), if any
       $avgRad /= $numNonZeroRad;
       for ( my $i=0; $i<4; $i++ ) {
          print "$i $rad[0]";
          if ( $rad[$i] == 0 ) {
              $rad[$i] = $avgRad; 
          }
       }
    } else {
       # all the radii are zero, use the previous radii, if available
       if ( @oldrad ) {
          for ( my $i=0; $i<4; $i++ ) {
             $rad[$i] = $oldrad[$i];
          }
       } else {
       # there are no previous radii available, use the nowcast value
          for ( my $i=0; $i<4; $i++ ) {
             $rad[$i] = substr($_,109,3);
          }
       }
    }
    # save the radii in case they are needed on the next time level
    for ( my $i=0; $i<4; $i++ ) {
       $oldrad[$i] = $rad[$i];
    }
}
#------------------------------------------------------------------------
# interpolateRadius: This subroutine accepts the forecast period (tau)
# in hours and returns the radius of uncertainty in nautical miles. It
# must interpolate between radii published by the NHC for specific
# forecast periods.
#------------------------------------------------------------------------
sub interpolateUncertaintyRadius($) {
    my $i;         # index into array of nhc uncertainty data 
    my $tau=shift;
    my $radius = 0;
    my @nhc_tau = (0, 12, 24, 36, 48, 72, 96, 120);
    my @nhc_radii = (9.5, 36, 62, 89, 111, 167, 230, 302);

    if ( $tau<$nhc_tau[0] ) {
	stderrMessage("WARNING","Invalid forecast period (tau) of $tau in fort.22. Setting radius of uncertainty to $nhc_radii[0].");
	return $nhc_radii[0];
    } elsif ( $tau>$nhc_tau[-1] ) {
	# if the forecast period is longer than our last available data,
	# extrapolate the radius
	stderrMessage("WARNING","Forecast period of $tau hours in fort.22 is farther in the future than NHC publishes uncertainty statistics. Extrapolating radius of uncertainty from published data at $nhc_tau[-2] and $nhc_tau[-1] hours.");
	$radius=($nhc_radii[-1]-$nhc_radii[-2])/($nhc_tau[-1]-$nhc_tau[-2])
	    *($tau-$nhc_tau[-1])+$nhc_radii[-1];
	return $radius;
    } elsif ( $tau>=$nhc_tau[0] && $tau<=$nhc_tau[-1]) {
	# forecast period is within our data, find the values that bracket
	# it an perform linear interpolation
	my $npoints=@nhc_tau;
	for ( $i=0; $i<=($npoints-2); ++$i ) {
	    if ( $tau>=$nhc_tau[$i] && $tau<=$nhc_tau[$i+1] ) {
		$radius=(($tau-$nhc_tau[$i])/($nhc_tau[$i+1]-$nhc_tau[$i]))
		    *($nhc_radii[$i+1]-$nhc_radii[$i])
		    +$nhc_radii[$i];
		return $radius;
	    }
	}
    } else { 
	stderrMessage("ERROR","Failed to interpolate radius of uncertainty at $tau hours.");
    }
}

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: storm_track_gen.pl: $message\n";
   if ($level eq "ERROR") {
      sleep 60
   }
}



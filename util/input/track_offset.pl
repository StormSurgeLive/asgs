#!/usr/bin/env perl
#---------------------------------------------------------------------
# track_offset.pl
#
# Change a track by a constant offset of latitude and/or longitude. 
#---------------------------------------------------------------------
#
# Copyright(C) 2018 Jason Fleming
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
#
use strict;
use Getopt::Long;
use Date::Calc;
use Cwd;
$^W++;
#
my $input = "fort.22"; # name of base track file
my $output = "modified_fort.22"; # name of track file after modification
#
my $latchange = 0; # specified uniform change in latitude (10ths of degrees)
my $lonchange = 0; # specified uniform change in longitude (10ths of degrees)
my $changestart = -99; # yyyymmddhh24 when change should be phased in
my $changeend = -99;   # yyyymmddhh24 when change should be complete 
my $syear = -99;  # change start year
my $smon = -99;   # change start month
my $sday = -99;   # change start day
my $shour = -99;  # change start hour
my $eyear = -99;  # change end year
my $emon = -99;   # change end month
my $eday = -99;   # change end day
my $ehour = -99;  # change end sec
my $emin = 0;
my $esec = 0;
#
GetOptions(
           "input=s" => \$input,
           "output=s" => \$output,
           "latchange=s" => \$latchange,
           "lonchange=s" => \$lonchange,
           "changestart=s" => \$changestart,
           "changeend=s" => \$changeend
           );
#
# if the start date is supplied but the end date is not, bomb out because
# this is not supported yet
if ( $changestart != -99 && $changeend == -99 ) {
    &stderrMessage("ERROR","Start date was supplied but not end date.");
}
#
# parse the start date if it was supplied
if ( $changestart != -99 ) {
   $changestart =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $syear = $1;
   $smon = $2;
   $sday = $3;
   $shour = $4;
}
# parse the end date if it was supplied
if ( $changeend != -99 ) {
   $changeend =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $eyear = $1;
   $emon = $2;
   $eday = $3;
   $ehour = $4;
}
#
# open ATCF input file
unless (open(INPUT,"<$input")) {
   stderrMessage("ERROR","Failed to open base ATCF file $input': $!.");
   die;
}
#
# open ATCF output file
unless (open(OUTPUT,">$output")) {
   stderrMessage("ERROR","Failed to open output file for writing : $!.");
   die;
}
#
# apply change in latitude and / or longitude
while(<INPUT>) {
   my @fields = split(',',$_);
   my $line = $_;
   # determine whether the line is BEST or OFCL so we can compute time
   # correctlya (if necessary)
   my $besttime = $fields[2];
   $besttime =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   my $byear = $1;
   my $bmon = $2;
   my $bday = $3;
   my $bhour = $4;
   my $bmin = 0;
   my $bsec = 0;
   my $linetype = $fields[4];
   if ( $linetype =~/OFCL/ ) {
     # grab the existing forecast period, i.e., the number of hours beyond the
     # forecast datetime that the forecast applies to
     my $tau=substr($_,29,4);
     ($byear,$bmon,$bday,$bhour,$bmin,$bsec) =
        Date::Calc::Add_Delta_DHMS($byear,$bmon,$bday,$bhour,0,0,0,$tau,0,0);
   }
   my $linedate = sprintf("%4d%02d%02d%02d",$byear,$bmon,$bday,$bhour);  
   #
   # if changestart is not supplied, but changeend is supplied, then
   # use the first line as the changestart
   if ( $changestart == -99 && $changeend != -99 ) {
      $changestart = $linedate;
      $syear = $byear;
      $smon = $bmon;
      $sday = $bday;
      $shour = $bhour;
   }
   #
   # want to save the nowcast storm position
   my $origlat=substr($line,34,4);
   my $origlon=substr($line,41,4);
   # compute the change that should be applied
   my $latchangenow = $latchange; # default
   my $lonchangenow = $lonchange; # default
   # if the linedate is before the start of the change, don't apply the shift
   if ( $changestart > $linedate ) {
      $latchangenow = 0;
      $lonchangenow = 0;
   }
   # if this is the period when the shift is linearly applied, compute
   # the interpolation factor and apply to the positions
   if ( $changestart <= $linedate && $linedate <= $changeend ) {
      # difference between start date and line date (in hours)
      (my $ddays, my $dhrs, my $dsec) = Date::Calc::Delta_DHMS($byear,$bmon,$bday,$bhour,0,0,$syear,$smon,$sday,$shour,0,0);
      $dhrs = $dhrs + $ddays*24 + $dsec/3600; 
      # difference between start date and end date in hours
      (my $deldays,my $delhrs, my $delsec) = Date::Calc::Delta_DHMS($syear,$smon,$sday,$shour,0,0,$eyear,$emon,$eday,$ehour,0,0);
      $delhrs = $delhrs + $deldays*24 + $delsec/3600; 
      my $interpfactor = abs($dhrs/$delhrs);
      $latchangenow = $interpfactor * $latchange;
      $lonchangenow = $interpfactor * $lonchange;
   }
   # change the coordinate
   my $newlat = $origlat + $latchangenow;
   my $newlon = $origlon + $lonchangenow;
   # paste in the new coordinates 
   substr($line,34,4)=sprintf("%4d",$newlat);
   substr($line,41,4)=sprintf("%4d",$newlon);
   #
   # write the line to the file, writing an eol if the line does not have one
   if ( /\n/ ) {
      print OUTPUT $line;
   } else { 
      printf OUTPUT "$line\n";
   }
}
close(INPUT); 
close(OUTPUT);

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: track_offset.pl: $message\n";
}

#!/usr/bin/env perl
#---------------------------------------------------------------------
# best2fcst.pl
#
# Take an ATCF formatted BEST track file and fill in the forecast
# increment (TAU in ATCF parlance) in hours according to the date/time
# column, starting at zero.
#
# The resulting file will be used as input to aswip and ultimately
# ADCIRC's NWS19 or NWS20.
#
# Also has the capability to time-interpolate the central pressure
# values from the BEST track data into an ADCIRC fort.22 for HWind
# data.
#
#---------------------------------------------------------------------
#
# Copyright(C) 2012--2017 Jason Fleming
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
# Example execution to rezero the hours column in a BEST track file
# to reflect a later start time (24 hours after cold start):
#
# export PERL5LIB=~/asgs/2014stable/PERL
# perl ~/asgs/2014stable/doc/best2fcst.pl --input fort.22 --csdate 2017082312 --hstime 86400.0 --bestout fort.22.24hr
#---------------------------------------------------------------------
#
use strict;
use Getopt::Long;
use Date::Calc;
$^W++;
#
my $input = "null";   # name of the BEST track file
my $issued = "null";  # yyyymmddhh24Z time of issue of corresponding advisory
my $forecastlength = "null"; # max forecast length (hours)
my $output = "null"; # name of OFCL output file
my $bestoutput = "null"; # name of BEST output file (if any)
my $rezeroHoursColumn; # used to reset the hours column of a BEST track file so it starts at zero
#
# for HWind data 
my $hwind = "null"; # name of the ADCIRC HWind fort.22 file, if any
my $csdate = "null";  # date/time of cold start, if HWind hours should
                      # be relative to that or to a hot start time (yyyymmddhh24)
my $hstime = "null";  # hot start time (sec), if the HWind hours should
                      # be relative to the hotstart time



#
# if csdate is specified on the command line, but the hstime is not, then
# the HWind hours column will be relative to the cold start time.
# if both csdate and hstime are specified on the command line, then
# the HWind hours column will be relative to the hot start time.
#
#
GetOptions(

           "input=s" => \$input,
           "output=s" => \$output,
           "rezero-hours-column" => \$rezeroHoursColumn,
           "bestoutput=s" => \$bestoutput,
           "issued=s" => \$issued,
           "forecastlength=s" => \$forecastlength,
           "hwind=s" => \$hwind,
           "csdate=s" => \$csdate,
           "hstime=s" => \$hstime
           );
#

unless(open(BEST,"<$input")) {
   stderrMessage("ERROR","Failed to open BEST track file $input: $!.");
   die;
}
if ( $output eq "null" ) {
   $output = "fcst_$input";
}
unless ( $rezeroHoursColumn ) { 
   unless(open(FCST,">$output")) {
      &stderrMessage("ERROR","Failed to open forecast track file $output: $!.");
      die;
   } else {
      &stderrMessage("INFO","Opened forecast track file $output.");
   }
}
unless ( $bestoutput eq "null" ) {
   unless(open(BESTOUT,">$bestoutput")) {
      stderrMessage("ERROR","Failed to open BEST track file $bestoutput: $!.");
      die;
   }
}
my @time_differences; # hours from the start of the best track data to the line in question
my @pc;               # mb
my $cycle = 1;
my $start_date = "null";
my $previous_date = "null";
my $sy; my $sm; my $sd; my $sh;  # starting year, month day, hour
my $fy; my $fm; my $fd; my $fh; my $fmin; # for current BEST track line
#
# compute the starting date for re-zeroing the hours column in a BEST 
# track file
my $newStartDate = "null";
if ( $rezeroHoursColumn ) {
   # parse date given on command line
   $csdate =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
   my $csy = $1;
   my $csm = $2;
   my $csd = $3;
   my $csh = $4;
   my $csmin = 0;
   my $css = 0; 
   my $ddays = 0;
   my $dhours = 0;
   my $dminutes = 0;
   my $dseconds = $hstime; 
   my ($sdy,$sdm,$sdd,$sdh,$sdmin,$sds) =
       Date::Calc::Add_Delta_DHMS($csy,$csm,$csd,$csh,$csmin,$css,$ddays,$dhours,$dminutes,$dseconds);
   $newStartDate = sprintf("%04d%02d%02d%02d",$sdy,$sdm,$sdd,$sdh);
}
#
# read each line of BEST track file
while(<BEST>) {
   my @fields = split(',',$_);
   my $date = $fields[2];
   # if the date on the BEST line is before the "issued" advisory time,
   # just skip this line, or write to the bestoutput file if specified
   if ( $issued ne "null" ) {
      if ( $date < $issued ) { 
         if ( $bestoutput ne "null" ) {
            printf BESTOUT $_;
         }            
         next;
      }
   }
   # for re-zeroing the hours column in a BEST track file to match 
   # a hotstart time, compare the date of this line with the cold start 
   # date plus the hot start time and skip if it is before the hotstart
   # time   
   if ( $rezeroHoursColumn ) {
      if ( $date < $newStartDate ) {
         next; 
      }
   }
   # parse the date on the current line
   $date =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
   $fy = $1;
   $fm = $2;
   $fd = $3;
   $fh = $4;
   # if the start date has not been set yet
   if ( $start_date eq "null" ) {
      $previous_date = $date;
      $start_date = $date;
      $sy = $1;
      $sm = $2;
      $sd = $3;
      $sh = $4;
   } elsif ( $date != $previous_date ) {
      $cycle = $cycle + 1;
   }
   $pc[$cycle] =  $fields[9]; # in mb
   # compute time difference between start date of file and date on 
   # current line
   (my $ddays, my $dhrs, my $dmin, my $dsec)
      = Date::Calc::Delta_DHMS($sy,$sm,$sd,$sh,0,0,$fy,$fm,$fd,$fh,0,0);
   $time_differences[$cycle] = $ddays*24 + $dhrs; # in hours
   # if the BEST line has gone beyond the max forecast length specified
   # on the command line, exit the loop
   if ( $forecastlength ne "null" ) {
      if ( $time_differences[$cycle] > $forecastlength ) {
         exit;
      }
   }
   my $line = $_;
   # fill in the forecast hours (tau) column
   substr($line,30,3) = sprintf("%3d",$time_differences[$cycle]);
   # change the file type column to OFCL to reflect the fact that 
   # these data are supposed to represent a forecast
   unless ( $rezeroHoursColumn ) {
      substr($line,24,4) = "OFCL";
      # make the date/time value on this line the same as the first 
      # date/time value in the file, as specified by the ATCF spec 
      # for OFCL type files
      substr($line,8,10) = sprintf("%4d%02d%02d%02d",$sy,$sm,$sd,$sh);
      printf FCST $line;
   } else {
      printf BESTOUT $line;
   }
   $previous_date = $date;
}
#
close(BEST);
close(FCST);
unless ( $bestoutput eq "null" ) {
   close(BESTOUT);
}
#
# we're done unless there is HWind data to interpolate to
if ( $hwind eq "null" ) {
   exit;
}
#
# We now have the start date of the best track file, an array of hours
# from the beginning of the file, and an array of central pressures that
# correspond to each hour.
#
# Now read the HWind fort.22 file, calculating the hours corresponding to
# each HWind file and time-interpolating the central pressures from the BEST
# track file to the times listed in the HWind fort.22.
#
if ( $hwind ne "null" ) {
   unless(open(HWIND,"<$hwind")) {
      stderrMessage("ERROR","Failed to open hwind file $hwind: $!.");
      die;
   }
}
# open file to hold HWind data with interpolated central pressures and
# hour values filled in
my $hwind_out = "fcst_" . $hwind;
unless(open(HWINDOUT,">$hwind_out")) {
   stderrMessage("ERROR","Failed to open hwind output file $hwind_out for writing: $!.");
   die;
}
my $hwind_start_date = "null";
my $hours_from_best_start;
my $hours_from_adcirc_start;
my $hwind_pc;
my $hsy; my $hsm; my $hsd; my $hsh; my $hsmin;
my $line;
$line = <HWIND>;
printf HWINDOUT $line; # comment line
$line = <HWIND>;
printf HWINDOUT $line; # multiplier
$line = <HWIND>;
printf HWINDOUT "specifiedPc\n"; # tells ADCIRC to use the Pc in this file
while(<HWIND>) {
   my @fields = split(' ',$_);
   my $hwind_file = $fields[3];
   $hwind_file =~ /([a-zA-Z]{2})(\d{2})(\d{4})_(\d{2})(\d{2})_(\d{2})(\d{2})/;
   my $basin = $1;       # "AL" for atlantic, "EP" for eastern pacific, etc
   my $stormnumber = $2; # the id number of the storm for that year
   $fy = $3;
   $fm = $4;
   $fd = $5;
   $fh = $6;
   $fmin = $7;
   #stderrMessage("DEBUG","HWind: basin=$basin stormnumber=$stormnumber year=$fy month=$fm day=$fd hour=$fh minute=$fmin.");
   #
   # this is the first time in the file; do various initializations
   if ( $hwind_start_date eq "null" ) {
      $hsy = $fy;
      $hsm = $fm;
      $hsd = $fd;
      $hsh = $fh;
      $hsmin = $fmin;
      $hwind_start_date = "found";
      # compare the hwind start date with the range of date/times
      # found in the best track file
      (my $ddays, my $dhrs, my $dmin, my $dsec)
         = Date::Calc::Delta_DHMS($sy,$sm,$sd,$sh,0,0,$hsy,$hsm,$hsd,$hsh,$hsmin,0);
      $hours_from_best_start = $ddays * 24.0 + $dhrs + $dmin/60.0 + $dsec/3600.0; # in hours
      if ( $hours_from_best_start < 0.0 ) {
         stderrMessage("WARNING","The HWind data start before the best track data; the central pressure will be set to 1013 for HWind files prior to the start of the BEST track data.");
      }
      # compute hours needed to make time relative to either the cold start time or
      # the hot start time, as specified on the command line
      if ( $csdate ne "null" ) {
         $csdate =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
         my $cy = $1;
         my $cm = $2;
         my $cd = $3;
         my $ch = $4;
         (my $ddays,my $dhrs, my $dmin, my $dsec)
         = Date::Calc::Delta_DHMS($cy,$cm,$cd,$ch,0,0,$hsy,$hsm,$hsd,$hsh,$hsmin,0);
         $hours_from_adcirc_start = $ddays * 24.0 + $dhrs + $dmin/60.0 + $dsec/3600.0; # in hours
         #stderrMessage("DEBUG","cy=$cy cm=$cm cd=$cd ch=$ch");
         #stderrMessage("DEBUG","hsy=$hsy hsm=$hsm hsd=$hsd hsh=$hsh hsmin=$hsmin");
         #tderrMessage("DEBUG","ddays=$ddays dhrs=$dhrs dsec=$dsec");
         #stderrMessage("DEBUG","hours_from_adcirc_start=$hours_from_adcirc_start");
         if ( $hstime ne "null" ) {
            $hours_from_adcirc_start -= ($hstime / 3600.0); # in hours
         }
      }

   }
   # compute the number of hours since the start of the hwind data
   (my $ddays, my $dhrs, my $dmin, my $dsec)
      = Date::Calc::Delta_DHMS($hsy,$hsm,$hsd,$hsh,$hsmin,0,$fy,$fm,$fd,$fh,$fmin,0);
   my $hours = $ddays * 24.0 + $dhrs + $dmin/60.0 + $dsec/3600.0; # in hours
   # compute the number of hours since the start of the best track data
   my $interp_hours = $hours_from_best_start + $hours;
   if ( $interp_hours < $time_differences[1] ) {
      stderrMessage("WARNING","HWind file $hwind_file is prior to best track data. Setting central pressure to 1013mb for this file.");
      $hwind_pc = "1013";
   } elsif ( $interp_hours > $time_differences[$cycle] ) {
      stderrMessage("WARNING","HWind file $hwind_file is after the end of the best track data. Persisting central pressure $hwind_pc mb from previous time level.");
      # just don't change $hwind_pc
   } else {
      for (my $i=1; $i<$cycle; $i++ ) {
         if ( $interp_hours >= $time_differences[$i]
            && $interp_hours <= $time_differences[$i+1] ) {
            # we've found our interval, now linearly interpolate
            my $wtratio = ( $interp_hours - $time_differences[$i] )
               / ( $time_differences[$i+1] - $time_differences[$i] );
            $hwind_pc = ( $pc[$i+1] - $pc[$i] ) * $wtratio + $pc[$i];
            $hwind_pc = sprintf("%.0f",$hwind_pc);
            last;
         }
      }
   }
   if ( $csdate ne "null" ) {
      $hours += $hours_from_adcirc_start;
   }
   printf HWINDOUT "$hours $hwind_pc $fields[2] $hwind_file\n";
}
close(HWIND);
close(HWINDOUT);

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: best2fcst.pl: $message\n";
}



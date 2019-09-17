#!/usr/bin/env perl
#----------------------------------------------------------------
# boundary_data_transpose.pl
#
# Reads fort.19 aperiodic elevation specified boundary data and
# (given the number of elevation specified boundary nodes) and
# transposes the data for easy plotting.  
#
#----------------------------------------------------------------
# Copyright(C) 2017 Jason Fleming
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
# perl ~/asgs/trunk/output/boundary_data_transpose.pl --file ./fort.19 --coldstartdate 2009110100 --gmtoffset 0 --timezone GMT --units si
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
my $file = 'fort.19'; # full path name of boundary data file 
my $coldstartdate = 'null'; # yyyymmddhh24 when the simulation was coldstarted
my $gmtoffset=-5; # number of hours between gmt and local time
my $timezone="CDT"; # time zone designation to be placed on graphs
my $nnodes = 0;  # number of boundary nodes
my $separator = " "; # record separator, could also be "," to produce csv
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
           "file=s" => \$file,
           "nnodes=s" => \$nnodes,
           "coldstartdate=s" => \$coldstartdate,
           "gmtoffset=s" => \$gmtoffset,
           "timezone=s" => \$timezone,
           );
#
# report time in seconds using the ETIMINC value at the top of the 
# boundary data file if the cold start date was not provided
if ( $coldstartdate ne "null" ) {
   $coldstartdate =~ /(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $cs_year = $1;
   $cs_mon = $2;
   $cs_day = $3;
   $cs_hour = $4;
   $cs_min = 0;
   $cs_sec = 0;
}
unless (open(BFILE,"<$file")) {
   stderrMessage("ERROR","Could not open $file: $!.");
   exit(1);
} 
my $transposeFilename = $file . "_transpose";
unless (open(TRANSPOSE,">$transposeFilename")) {
   stderrMessage("ERROR","Could not open $transposeFilename: $!.");
   exit(1);
}
my $etiminc = 0.0;  # time increment for elevation boundary data 
my $time = 0.0;      # time in seconds for boundary data
my $nodeNumber = 0; # elevation boundary node number
my $timeStr = "null"; # current time, either in seconds or as calendar date/time
my @boundaryData;    # all elevations in a single dataset
while (<BFILE>) {
   chomp;
   #
   # the first line in the file is the time increment; transcribe it to the 
   # transposed file as a gnuplot comment line
   if ($. == 1 ) {
      #chomp;
      $etiminc = $_;
      printf TRANSPOSE "# etiminc " . $_ . "\n";
      next;
   }   
   $boundaryData[$nodeNumber] = $_;
   # if we have collected a complete dataset, write it now as a single row
   if ( $nodeNumber == ($nnodes-1) ) {        
      if ($coldstartdate ne "null" ) {
         # formulate the calendar time from the number of seconds
         ($year,$month,$day,$hour,$min,$sec)
            = Date::Calc::Add_Delta_DHMS($cs_year,$cs_mon,$cs_day,
            $cs_hour,$cs_min,$cs_sec,0,$gmtoffset,0,sprintf("%2d",$time));
         $timeStr = sprintf("%4s-%02s-%02s$separator%02s:%02s:%02d$separator",
                $year,$month,$day,$hour,$min,$sec);
      } else {
         $timeStr = $time;
      }
      printf TRANSPOSE ("%10s",$timeStr);      
      printf TRANSPOSE " ";
      foreach (@boundaryData) {
         printf TRANSPOSE ("%10s",$_);
         printf TRANSPOSE " ";
      }
      printf TRANSPOSE "\n";
      @boundaryData = ();
      $nodeNumber = 0;
      $time = $time + $etiminc;
   } else { 
      $nodeNumber++;
   }
}
close(BFILE);
close(TRANSPOSE);


sub stderrMessage ($$) {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $theTime = "[$year-".sprintf("%3s",$months[$month])."-".sprintf("%02d",$dayOfMonth)."-T".sprintf("%02d",$hour).":".sprintf("%02d",$minute).":".sprintf("%02d",$second)."]";
   printf STDERR "$theTime $level: station_transpose.pl: $message\n";
}

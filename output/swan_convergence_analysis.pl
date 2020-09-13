#!/usr/bin/env perl
#----------------------------------------------------------------
# swan_convergence_analysis.pl
#
# Parses and analyzes the convergence behavior of a SWAN run.
#
#----------------------------------------------------------------
# Copyright(C) 2012 Jason Fleming
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
# perl ~/asgs/trunk/output/swan_convergence_analysis.pl --swanscreen padcswan.run.out --swanprint asgs_swan.prt 
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
my $gmtoffset=-5; # number of hours between gmt and local time
my $timezone="CDT"; # time zone designation to be placed on graphs
my $swanscreen = "null";
my $swanprint = "null";
my $step = 0;
my @datetime;        # date and time string for each swan time step
my @num_iterations;  # how many iterations there were for each swan time step
my $iteration = 0;
my $total_iterations = 0;
my $total_steps = 0;
my $startwc = "null";  # starting wall clock time e.g. 2012-10-08 12:23:21
my $finishwc = "null"; # finishing wall clock time e.g. 2012-10-08 14:59:56
#
GetOptions(
           "swanscreen=s" => \$swanscreen,
           "swanprint=s" => \$swanprint,
           "gmtoffset=s" => \$gmtoffset,
           "timezone=s" => \$timezone,
           "startwc=s" => \$startwc,
           "finishwc=s" => \$finishwc
           );
#
# compute the wall clock time required by the run if start and finish 
# times were provided on the command line
if ( $startwc ne "null" && $finishwc ne "null" ) {
   $startwc =~ /(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})/;
   my $s_year = $1;
   my $s_mon = $2;
   my $s_day = $3;
   my $s_hour = $4;
   my $s_min = $5;
   my $s_sec = $6;
   $finishwc =~ /(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})/;
   my $f_year = $1;
   my $f_mon = $2;
   my $f_day = $3;
   my $f_hour = $4;
   my $f_min = $5;
   my $f_sec = $6;
   # get difference
   (my $ddays, my $dhrs, my $dmin, my $dsec)
           = Date::Calc::Delta_DHMS(
                $s_year,$s_mon,$s_day,$s_hour,$s_min,$s_sec,
                $f_year,$f_mon,$f_day,$f_hour,$f_min,$f_sec);
   my $wall_clock_time = sprintf("%02d:%02d:%02d:%02d",$ddays,$dhrs,$dmin,$dsec);
   print "The elapsed wall clock time was $wall_clock_time.\n";
}
unless(open(SWANSCREEN,"<$swanscreen")) {
   stderrMessage("ERROR","Could not open swan screen file $swanscreen for reading.");
   exit 1;
}
while(<SWANSCREEN>) {
   # skip the line unless it is from SWAN and reports the timestep, 
   # iteration number, and sweep number
   #if ( $_ =~ /\+time\s\d{8}\.\d{6}\s+,\sstep\s+(\d+)\;\siteration\s+(d+)\;\ssweep\s+(\d+)/ ) {
   if ( $_ =~ /time (\d{8}\.\d{6})\s+, step\s+(\d+); iteration\s+(\d+)/ ) {
      # if this is a new iteration, increment the total number of iterations
      if ( $3 != $iteration ) {
         $total_iterations++;
      }
      # if this is a new time step, increment the total number of time steps
      if ( $2 != $step ) {
         $total_steps++;
      }
      $step = $2;
      $datetime[$step] = $1;
      $iteration = $3;
      $num_iterations[$step] = $iteration;
   }
}
close(SWANSCREEN);
print "SWAN used $total_iterations iterations in $total_steps time steps over the course of the run.\n";
my $mean_it_per_step = $total_iterations / $total_steps;
print "The mean number of iterations per swan time step was $mean_it_per_step.\n";
#
# now we have a count of the total number of iterations and the number of
# iterations that were performed at each time step
#
# read the swan print file and determine how well converged the solution
# was when swan stopped iterating for that time step and moved to the next
# one
unless(open(SWANPRINT,"<$swanprint")) {
   stderrMessage("ERROR","Could not open $swanprint for reading.");
   exit 1;
}
$step = 0;
my @percent_accuracy_ok;
while(<SWANPRINT>) {
   if ( $_ =~ /NPNTS=(\d+)/ ) {
      print "Convergence was deemed complete when the wave solution showed acceptable accuracy in $1 percent of the SWAN nodes (NPNTS=$1).\n";
   }
   if ( $_ =~ /first iteration/ ) { 
      $step++;    
   } 
   if ( $_ =~ /accuracy OK in\s+(\d{1,2}).(\d{2})/ ) {
      $percent_accuracy_ok[$step] = "$1.$2";
   }
}  
close(SWANPRINT);
#
# write the data to a file for plotting 
unless(open(SWANCONVDAT,">$swanscreen.d")) {
   &stderrMessage("ERROR","Could not open $swanscreen.d for writing.");
   exit 1;
}
for (my $i=1; $i<=$total_steps; $i++) {
   $datetime[$i] =~ /(\d{4})(\d{2})(\d{2}).(\d{2})(\d{2})(\d{2})/;
   my $year = $1;
   my $month = $2;
   my $day = $3;
   my $hour = $4;
   my $minute = $5;
   my $second = $6;
   my $time = sprintf("%4s-%02s-%02s %02s:%02s:%02d", $year,$month,$day,$hour,$minute,$second);
   print SWANCONVDAT "$time $num_iterations[$i] $percent_accuracy_ok[$i]\n";
   #print SWANCONVDAT "$time $num_iterations[$i]\n";
}
close(SWANCONVDAT);



sub stderrMessage ($$) {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $theTime = "[$year-".sprintf("%3s",$months[$month])."-".sprintf("%02d",$dayOfMonth)."-T".sprintf("%02d",$hour).":".sprintf("%02d",$minute).":".sprintf("%02d",$second)."]";
   printf STDERR "$theTime $level: swan_convergence_analysis.pl: $message\n";
}

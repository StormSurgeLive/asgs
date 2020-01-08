#!/usr/bin/env perl
#----------------------------------------------------------------
# disappeared.pl
#
# Checks to see if a job is still in the queue; if the job
# disappears from the queue without writing an appropriate
# file, then this script writes out the file to notify the ASGS
# that the job has disappeared and then exits. If the job ends
# normally, then this script exits.
#
#----------------------------------------------------------------
# Copyright(C) 2013: Jason Fleming
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
use strict;
use warnings;
use Getopt::Long;
#
my $runtype; # (presumptive addition to allow this script to pass perl -c)
my $checkInterval = 60;   # seconds between fs checks by ASGS to see job state
my $jobtype = "padcswan"; # type of job; used in naming the job status file
my $enstorm = "nowcast";  # the model stage that is running
my $jobid = "-99999";     # the job id returned by the queueing system
GetOptions(
          "check-interval=s" => \$checkInterval,
          "jobtype=s" => \$runtype,
          "enstorm=s" => \$enstorm,
          "jobid=s" => \$jobid
         );
#
# Log a message to say we are monitoring for disappearance
&stderrMessage("INFO","Monitoring for job disappearance.");
# get record number for wind velocity (v) at 10m
if ( `qstat` =~ m/$jobid/ ) {
   &stderrMessage("INFO","Still in the queue!");
}



sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: disappeared.pl: $message\n";
   if ($level eq "ERROR") {
      sleep 60
   }
}


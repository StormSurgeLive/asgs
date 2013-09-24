#!/usr/bin/env perl
#--------------------------------------------------------------
# goodhost.pl: A program to look at the list of hosts involved
# in parallel jobs, to narrow down the hosts which may be 
# causing issues. 
#--------------------------------------------------------------
# Copyright(C) 2013 Jason Fleming
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
#--------------------------------------------------------------
#
# Input to goodhost.pl is a series of log files that contain
# the list of hosts involved in successful jobs, as well as 
# a separate list of log files containing the list of hosts
# involved in failed jobs. 
#
# goodhost.pl then assigns each host a score, based on its
# participation in successful or unsuccessful jobs. Each time
# a host is involved in a successful job, its score is incremented
# by 1. Each time it is involved in a failed job, its score is 
# decremented by 1.
#
# Hosts with the highest final score are least likely to be 
# causing issues, while those with the lowest final scores are most
# likely to be problem hosts.
#
#--------------------------------------------------------------
$^W++;
use strict;
use Getopt::Long;
use Cwd;
#
my $goodlogs = "null";   # job log files from successful runs
my $badlogs = "null";    # job log files from unsuccessful runs
my $scoresfile = "scores.txt"; # relative likelihood of troublemakers
our %hostsScores; # hash of unique hostnames and their scores
#
GetOptions(
           "goodlogs=s" => \$goodlogs,
           "badlogs=s" => \$badlogs,
           "scoresfile=s" => \$scoresfile
          );
#
if ( $goodlogs ne "null" ) {
   &parseLogs($goodlogs,1);
}
if ( $badlogs ne "null" ) {
   &parseLogs($badlogs,-1);
}
#
# open the output file
unless (open(OUT,">$scoresfile")) {
   &stderrMessage("ERROR","Could not open host scores file '$scoresfile' for writing: $!.");
      die;
}
# sort the scores and write them to the output file
#
foreach my $key (sort { $hostsScores{$b} <=> $hostsScores{$a} } keys %hostsScores ) {
   printf OUT "%4d %s\n", $hostsScores{$key}, $key;
}
close(OUT);
#
# end of goodhost.pl
#
#
#
# function to parse hostnames out of a list of log files and apply the
# specified score modifier to the host
sub parseLogs () {
   my $loglist = shift;
   my $increment = shift;
   if ( -e $loglist ) {
      unless (open(LL,"<$loglist")) {
         &stderrMessage("ERROR","Found the log file list '$loglist' but could not open it: $!.");
         die;
      }
   } else {
      &stderrMessage("ERROR","The log file list ('$loglist') was not found.");
      die;
   }
   # each line in the log list file is the file name of a log file
   while (<LL>) {
      my $logfile = $_;
      chomp($logfile);
      if ( -e $logfile ) {
         unless (open(LF,"<$logfile")) {
            &stderrMessage("ERROR","Found the log file '$logfile' but could not open it: $!.");
            die;
         }
      } else {
         &stderrMessage("ERROR","The log file ('$logfile') was not found.");
         die;
      }
      # make a list of hostnames that have already appeared in this log file;
      # we need to remove duplicate hostnames; duplicates will occur because 
      # single host will have more than one core; it is assumed that the overall
      # physical host is bad, not a particular core 
      my @runHostList; 
      while(<LF>) {
         # if this is not a hostname line, skip it and go to the next
         unless ( $_ =~ /compute-\d+-\d+/ ) {
            next;
         }
         # this is a hostname line, check to see if it is already on the 
         # list of hostnames in this file, and if so, go to the next 
         # line
         my $hostname = $_;
         chomp($hostname);
         if ( &is_member($hostname,@runHostList)) {
            next;
         } 
         &stderrMessage("DEBUG","Unique hostname found: '$_'.");
         # add the hostname to the list for this run
         push(@runHostList,$hostname);
         # check to see if this host is in the hash already, and if so,
         # increment its score; if not, add it with an initial score 
         # of the specified increment
         my @host_list = keys(%hostsScores);
         if ( &is_member($hostname,@host_list)) {
            $hostsScores{$hostname} += $increment;
         } else { 
            $hostsScores{$hostname} = $increment;
         }
      }
      close(LF);
   }
   close(LL);
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
#
#  Prints a message to stderr, annotated with the date, the specified 
#  severity level, and the name of this script.
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: goodhost.pl: $message\n";
}

#!/usr/bin/env perl
#--------------------------------------------------------------
# select_nam_nowcast.pl: uses a list of cycles produced by
# get_nam_status.pl along with ASGS configuration to select
# the NAM cycle to nowcast to
#--------------------------------------------------------------
# Copyright(C) 2022 Jason Fleming
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
#--------------------------------------------------------------
$^W++;
use strict;
use Getopt::Long;
use Date::Calc;
use JSON::PP;
use Cwd;

# the following values may be set on the command
# line or read from the $RUNDIR/status/asgs.instance.status.json file
# if they are set both ways, the commnd line takes precedence
our $rundir = Cwd::cwd();   # directory where the data files will be stored
our $forecastcycle = "00,06,12,18";   # nam cycles to run a forecast
my $backsite = "ftp.ncep.noaa.gov";   # ncep ftp site for nam data
my $backdir = "/pub/data/nccf/com/nam/prod";    # dir on ncep ftp site
#
our $this = "select_nam_nowcast.pl";
my $cyclelistfile = "get_nam_status.pl.json";
my $selectedlistfile = "select_nam_nowcast.pl.json";
#
GetOptions(
           "forecastcycle=s" => \$forecastcycle
          );
# open the file containing the list of 
# cycles posted since the last cycle 
# that was run by ASGS
unless (open(F,"<$cyclelistfile")) {
    &stderrMessage("ERROR","Failed to open '$cyclelistfile': $!.");
    die;
}
# get_nam_status.pl.json looks like the following:
# {
#   "forcing.nam.cyclelist" : [
#      2022011418,
#      2022011500,
#      2022011506,
#      2022011512
#   ]
# }
# slurp the file contents into a scalar variable
my $file_content = do { local $/; <F> };
close(F);
my $ref = JSON::PP->new->decode($file_content);
my %cyclehash = %$ref;
# grab the list of cycles out of the hash
my $cyclelistref = $cyclehash{"forcing.nam.cyclelist"};
my @cyclelist = @$cyclelistref;
#
# nowcast to the most recent cycle that the Operator
# has selected for a forecast
my $foundit = 0;
my $numnotfound = 0;
my @forecastcycles = split(",",$forecastcycle);
my $numcycles = @cyclelist;
#printf STDOUT "$numcycles\n"; #jgfdebug
CYCLES : for ( my $c=-1 ; ($c + $numcycles)>0 ; $c-- ) {
    #printf STDOUT "$c $cyclelist[$c]\n"; #jgfdebug
    $cyclelist[$c] =~ /(\d{8})(\d{2})/;
    my $cycleday = $1;
    my $cyclehour = $2;
    FORECASTTIMES : for ( my $f=-1 ; ($f + scalar(@forecastcycles))>-1 ; $f-- ) {  
        #printf STDOUT "$f $forecastcycles[$f]\n"; #jgfdebug
        if ( $forecastcycles[$f] == $cyclehour ) {
            $foundit = 1;
            last CYCLES;
        } 
    }
    $numnotfound++;
}
# remove later nowcast cycles if they are after the
# forecast we want to run
if ( $foundit == 1 ) {
    for ( my $i=0 ; $i<$numnotfound ; $i++ ) {
        pop(@cyclelist);
    }
}
# now encode the list of cycles as json and write out
unless ( open(SJ,">$this.json") ) {
    &stderrMessage("ERROR","Could not open '$this.json' for writing: $!.");
    exit 1;
}
my %namcycles;
$namcycles{"forcing.nam.cyclelist"} = \@cyclelist; 
my $json = JSON::PP->new->utf8->pretty->canonical->encode(\%namcycles);
print SJ $json;
close(SJ);
#
# write a log message to stderr
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: $this: $message\n";
}
#
# write a log message to a log file dedicated to this script (typically debug messages)
sub appMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   #
   # open an application log file for get_nam.pl
   unless ( open(APPLOGFILE,">>$rundir/get_nam.pl.log") ) {
      &stderrMessage("ERROR","Could not open $rundir/get_nam.pl.log for appending: $!.");
   }
   printf APPLOGFILE "$theTime $level: $this:  $message\n";
   close(APPLOGFILE);
}

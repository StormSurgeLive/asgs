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
#
my $forecastcycle = "null";   # nam cycles to run a forecast, e.g., "06,18"
my @forecastcycles; # as a list instead of comma separated string
#
our $this = "select_nam_nowcast.pl";
my $ncepcycles = "forcing.nam.ncep.cyclelist";
my $cyclelistfile = "get_nam_status.pl.json";  # input to this script
my $selectedlistfile = "select_nam_nowcast.pl.json"; # output from this script
#
GetOptions(
           "forecastcycle=s" => \$forecastcycle
          );
#
# the command line forecastcycle will override
# the value found in the json file
if ( $forecastcycle ne "null" ) {
   my @forecastcycles = split(",",$forecastcycle);  
}
#
# open the file containing the list of 
# cycles posted since the last cycle 
# that was run by ASGS
unless (open(F,"<$cyclelistfile")) {
    &stderrMessage("ERROR","Failed to open '$cyclelistfile': $!.");
    die;
}
# slurp the file contents into a scalar variable
my $file_content = do { local $/; <F> };
close(F);
my $ref = JSON::PP->new->decode($file_content);
my %jsonhash = %$ref;
# grab the list of cycles out of the hash
my $cyclelistref = $jsonhash{$ncepcycles};
my @cyclelist = @$cyclelistref;
unless ( defined $cyclelist[0] ) {
   &stderrMessage("ERROR","The file '$cyclelistfile' property 'forcing.nam.ncep.cyclelist' did not contain any cycles.");
   die;
}
# grab the forecast cycles if it was not provided
# on the command line
unless ( defined $forecastcycles[0] ) {
   if ( %jsonhash && defined $jsonhash{"forcing.nam.config.daily.forecastcycle"} ) {
      my $forecastcyclesref = $jsonhash{"forcing.nam.config.daily.forecastcycle"};
      @forecastcycles = @$forecastcyclesref;
   } else {
      @forecastcycles = qw(00 06 12 18);   # nam cycles to run a forecast
   }
}
#
# nowcast to the most recent cycle that the Operator
# has selected for a forecast
my $foundit = 0;
my $numnotfound = 0;

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
    die;
}
$jsonhash{$ncepcycles} = \@cyclelist;
$jsonhash{"forcing.nam.config.daily.forecastcycle"} = $forecastcycle;
$jsonhash{"forcing.nam.ncep.file.json.select"} = "$this.json";
&writeJSON();
# write the cycle when the nowcast should end
# so it can be captured by the calling routine 
# (may be different than the latest nowcast)
printf STDOUT $cyclelist[-1]; # success
exit 0;
#
sub writeJSON () {
   my $lastupdatedref = $jsonhash{"lastupdated"};
   my @lastupdated = @$lastupdatedref;
   my $timestamp = &getTimeStamp;
   my $ts_ref = { $this => $timestamp };
   push(@lastupdated,$ts_ref);
   $jsonhash{"lastupdated"} = \@lastupdated;
   unless ( open(SJ,">$this.json") ) {
      &stderrMessage("ERROR","Could not open '$this.json' for writing: $!.");
      die;
   }
   my $json = JSON::PP->new->utf8->pretty->canonical->encode(\%jsonhash);
   print SJ $json;
   close(SJ);
}
#
sub setParameter () {
   my ( $paramref, $key, $default ) = @_;
   if ( $$paramref eq "null" ) {
      if ( %jsonhash && defined $jsonhash{$key} ) {
         $$paramref = $jsonhash{$key};
      } else {
         $$paramref = $default;
      }
   }
   # bomb out if there is no default value for the parameter
   if ( $$paramref eq "null" ) {
      &stderrMessage("ERROR","The parameter '$key' was not specified.");
      die;
   }
}
#
# write a log message to stderr
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my $theTime = &getTimeStamp;
   printf STDERR "[$theTime] $level: $this: $message\n";
}
#
# write a log message to a log file dedicated to this script (typically debug messages)
sub appMessage () {
   my $level = shift;
   my $message = shift;
   my $theTime = &getTimeStamp;
   #
   # open an application log file
   unless ( open(APPLOGFILE,">>$this.log") ) {
      &stderrMessage("ERROR","Could not open $this.log for appending: $!.");
   }
   printf APPLOGFILE "[$theTime] $level: $this: $message\n";
   close(APPLOGFILE);
}

sub getTimeStamp () {
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "$year-$months[$month]-$dayOfMonth-T$hms";
   return $theTime;
}



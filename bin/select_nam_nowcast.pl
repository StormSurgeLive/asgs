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
use strict;
use warnings;
use Getopt::Long;
use Date::Calc;
use JSON::PP;
use Cwd;
use ASGSUtil;
#
my $forecastcycle = "null";   # nam cycles to run a forecast, e.g., "06,18"
my @forecastcycles; # as a list instead of comma separated string
#
my $ncepcycles = "cyclelist";
my $cyclelistfile = "get_nam_status.pl.json";  # input to this script

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
# slurp the JSON request contents into a scalar variable
my $file_content = do { local $/; <> };
my $jshash_ref = JSON::PP->new->decode($file_content);
# grab the list of cycles out of the hash
my $cyclelistref = $jshash_ref->{"cyclelist"};
my @cyclelist = @$cyclelistref;
if ( ! defined $cyclelist[0] ) {
   ASGSUtil::stderrMessage(
             "ERROR",
             "The file '$cyclelistfile' property 'cyclelist' did not contain any cycles.");
   die;
}
# grab the forecast cycles if it was not provided
# on the command line
if ( ! defined $forecastcycles[0] ) {
   if ( $jshash_ref && defined $jshash_ref->{"configDailyForecastCycles"} ) {
      my $forecastcyclesref = $jshash_ref->{"configDailyForecastCycles"};
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
# forecast we want to run ... unless we are nowcasting
# from files on the filesystem, in which case we want
# to keep all the files
if ( $foundit == 1 and $jshash_ref->{"siteHost"} ne "filesystem" ) {
    for ( my $i=0 ; $i<$numnotfound ; $i++ ) {
        pop(@cyclelist);
    }
}
# now encode the list of cycles as json and write out
$jshash_ref->{$ncepcycles} = \@cyclelist;
ASGSUtil::stringify(\@forecastcycles);
$jshash_ref->{"configDailyForecastCycles"} = \@forecastcycles;
$jshash_ref->{"select"} = basename($0).".json";
ASGSUtil::timestampJSON($jshash_ref);
#ASGSUtil::writeJSON($jshash_ref);
# leading zeroes are not valid JSON, so store
# cycle hours (e.g., 00,06,12 etc) as strings
print JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
1;
#

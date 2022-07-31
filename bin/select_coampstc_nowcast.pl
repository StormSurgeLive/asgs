#!/usr/bin/env perl
#--------------------------------------------------------------
# select_coampstc_nowcast.pl: uses a list of cycles produced
# from the latest metget status along with ASGS configuration
# to select the COAMPS-TC cycle to nowcast to
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
use Util::H2O;
use JSON::PP;
use Cwd;
use ASGSUtil;
#
my $forecastcycle = "null";  # nam cycles to run a forecast, e.g., "06,18"
my @forecastcycles;
my $startcycle;              # cycle to start the nowcast on
#
my $ncepcycles = "cyclelist";

my $cyclelistfile = "metget_status.json";  # input to this script
my $source = "null";           # coampstc, hwrf, gfs, nam
my $backgroundmet = "null";    # ncep ftp site for nam data
my $tropicalcyclone = "null";  # dir on ncep ftp site
my $storm = "null";            # annual tropical cyclone number

#
GetOptions(
           "source=s" => \$source,
           "forecastcycle=s" => \$forecastcycle,
           "backgroundmet=s" => \$backgroundmet,
           "tropicalcyclone=s" => \$tropicalcyclone,
           "storm=s" => \$storm,
           "startcycle=s" => \$startcycle
          );
#
if ( $forecastcycle ne "null" ) {
   my @forecastcycles = split(",",$forecastcycle);
}
#
# slurp the JSON into a scalar variable
my $file_content = do { local $/; <> };
# grab the list of cycles out of the hash : deserialize JSON to object
my $statuso = h2o -recurse, JSON::PP->new->decode($file_content);
# coamps-tc does not work as a perl bareword
my %metget = %{$statuso->body->data->metget};
my $coampstc_ref = $metget{"coamps-tc"};
my $hwrf_ref = $metget{"hwrf"};
my $gfs_ref = $metget{"gfs-ncep"};
my $nam_ref = $metget{"nam-ncep"};
#
my $coampstcStormname = sprintf("%02dL",$storm);
my @relevantCycles;
my @relevantCyclesChronological;
foreach my $stormhash_ref (@$coampstc_ref) {
    my $stormo = h2o $stormhash_ref;
    if ( $stormo->storm eq $coampstcStormname ) {
        my $cyclelist_ref = $stormo->cycle_list;
        # reformat to yyyymmddhh
        foreach my $cycle (@$cyclelist_ref) {
            # 2022-05-20 06:00:00
            $cycle =~ /(\d{4})-(\d{2})-(\d{2}) (\d{2}):\d{2}:\d{2}/;
            my $thisCycle = "$1$2$3$4";
            if ( ! $thisCycle < $startcycle ) {
                push(@relevantCycles,$thisCycle);
            }
        }
        # sort into chronological order
        @relevantCyclesChronological = sort { $a <=> $b } @relevantCycles;
    }
}
#
# nowcast to the most recent cycle that the Operator
# has selected for a forecast
my $foundit = 0;
my $numnotfound = 0;
my $numcycles = @relevantCyclesChronological;
CYCLES : for ( my $c=-1 ; ($c + $numcycles)>0 ; $c-- ) {
    $relevantCyclesChronological[$c] =~ /(\d{8})(\d{2})/;
    my $cycleday = $1;
    my $cyclehour = $2;
    FORECASTTIMES : for ( my $f=-1 ; ($f + scalar(@forecastcycles))>-1 ; $f-- ) {
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
        pop(@relevantCyclesChronological);
    }
}
# now encode the list of cycles as json and write out
my $jshash_ref->{cyclelist} = \@relevantCyclesChronological;
ASGSUtil::stringify(\@forecastcycles);
$jshash_ref->{"configDailyForecastCycles"} = \@forecastcycles;
$jshash_ref->{"select"} = basename($0).".json";
ASGSUtil::timestampJSON($jshash_ref);
print JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
1;
#
# {
#    "body" : {
#       "accessed" : "2022-07-19T17:45:38.577417",
#       "data" : {
#          "metget" : {
#             "coamps-tc" : [
#                {
#                   "cycle_list" : [
#                      "2022-07-02 12:00:00",
#                      "2022-07-02 06:00:00",
#                      "2022-07-02 00:00:00",
#                      "2022-07-01 18:00:00",
#                      "2022-07-01 12:00:00",
#                      "2022-07-01 06:00:00",
#                      "2022-07-01 00:00:00",
#                      "2022-06-30 18:00:00",
#                      "2022-06-30 12:00:00",
#                      "2022-06-30 06:00:00",
#                      "2022-06-30 00:00:00",
#                      "2022-06-29 12:00:00",
#                      "2022-06-28 12:00:00",
#                      "2022-06-28 00:00:00",
#                      "2022-06-27 18:00:00"
#                   ],
#                   "first_available_cycle" : "2022-06-27 18:00:00",
#                   "last_available_cycle" : "2022-07-02 12:00:00",
#                   "latest_complete_forecast" : "2022-07-02 12:00:00",
#                   "latest_complete_forecast_end" : "2022-07-07 18:00:00",
#                   "latest_complete_forecast_length" : 126,
#                   "latest_complete_forecast_start" : "2022-07-02 12:00:00",
#                   "max_forecast_date" : "2022-07-07 18:00:00",
#                   "min_forecast_date" : "2022-06-27 18:00:00",
#                   "storm" : "02L"
#                },
#                {
#                   "cycle_list" : [
#                      "2022-07-03 06:00:00",
#                      "2022-07-03 00:00:00",
#                      "2022-07-02 18:00:00",
#                      "2022-07-02 12:00:00",
#                      "2022-07-02 06:00:00"
#                   ],
#                   "first_available_cycle" : "2022-07-02 06:00:00",
#                   "last_available_cycle" : "2022-07-03 06:00:00",
#                   "latest_complete_forecast" : "2022-07-03 06:00:00",
#                   "latest_complete_forecast_end" : "2022-07-08 12:00:00",
#                   "latest_complete_forecast_length" : 126,
#                   "latest_complete_forecast_start" : "2022-07-03 06:00:00",
#                   "max_forecast_date" : "2022-07-08 12:00:00",
#                   "min_forecast_date" : "2022-07-02 06:00:00",
#                   "storm" : "03L"
#                }
#             ],

#!/usr/bin/env perl
#--------------------------------------------------------------
# get_metget_status.pl: determines the latest available cycle(s)
# from MetGet for different forcing types.
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
#
#--------------------------------------------------------------
# The status json file is obtained from the service via request
# from p5-weather-metget-client. This script just reads it in
# and pulls out the status info for the specified data type.
#--------------------------------------------------------------
use strict;
use warnings;
use Getopt::Long;
use JSON::PP;
use Util::H2O;
use ASGSUtil;
#
my $startcycle = "null";       # optional arg that indicates start of range of interest
my $backgroundmet = "null";    # ncep ftp site for nam data
my $tropicalcyclone = "null";  # dir on ncep ftp site
my $storm = "null";            # annual tropical cyclone number
#
# if the startcycle was not provided, the script will return a list
# of all the cycles available from metget for the designated
# meteorological forcing type
#
GetOptions(
           "startcycle=s" => \$startcycle,
           "backgroundmet=s" => \$backgroundmet,
           "tropicalcyclone=s" => \$tropicalcyclone,
           "storm=s" => \$storm
          );
#
# JSON status message : everything
my $file_content = do { local $/; <> };
# deserialize JSON to object
my $statuso = h2o -recurse, JSON::PP->new->decode($file_content);
# coamps-tc does not work as a perl bareword
my %metget = %{$statuso->body->data->metget};
my $coampstc_ref = $metget{"coamps-tc"};
my $hwrf_ref = $metget{"hwrf"};
my $gfs_ref = $metget{"gfs-ncep"};
my $nam_ref = $metget{"nam-ncep"};
#
my $coampstcStormname = sprintf("%02dL",$storm);
my $hwrfStormname = sprintf("%02dL",$storm);    # FIXME: what is the hwrf storm naming convention
#
foreach my $stormhash_ref (@$coampstc_ref) {
    my $stormo = h2o $stormhash_ref;
    if ( $stormo->storm eq $stormname ) {
        printf("found %02dL\n",$storm);
        my $cyclelist_ref = $stormo->cycle_list;
        my @relevantCycles = @$cyclelist_ref;
        foreach my $cycle (@$cyclelist_ref) {
            printf("     found cycle %s\n",$cycle);
            $cycle =~ /(\d){4}-(\d{2})-(\d{2}) (\d{2})-(\d{2})-(\d{2})/;
            $thisCycle "$1$2$3$4";
            if ( $thisCycle < $startcycle ) {
                shift(@relevantCycles);
            }
        }
    }
}

# exit successfully
1;
#{
#  "body" : {
#      "accessed" : "2022-06-20T18:09:05.520897",
#      "data" : {
#         "metget" : {
#            "coamps-tc" : [
#               {
#                  "cycle_list" : [
#                     "2022-06-04 06:00:00",
#                     "2022-06-04 00:00:00",
#                     "2022-06-03 18:00:00"
#                  ],
#                  "storm" : "01L"
#               }
#            ],
#            "gfs-ncep" : {
#               "cycle_list" : [
#                  "2022-05-20 18:00:00",
#                  "2022-05-20 12:00:00",
#                  "2022-05-20 06:00:00"
#               ]
#            },
#            "hwrf" : [
#               {
#                  "cycle_list" : [
#                     "2022-05-20 12:00:00",
#                     "2022-05-20 06:00:00",
#                     "2022-05-20 00:00:00"
#                  ],
#                  "storm" : "gina26p"
#              }
#            ],
#            "nam-ncep" : {
#               "cycle_list" : [
#                  "2022-05-20 18:00:00",
#                  "2022-05-20 12:00:00",
#                  "2022-05-20 06:00:00"
#               ]
#            }
#         }
#      },
#      "message" : "Data retrieved from the MetGet system",
#      "request" : "caca8eae-fcb3-4381-9e83-295aa287e616",
#      "response_time" : 15.843,
#      "version" : "0.0.1"
#   },
#   "statusCode" : 200
#}

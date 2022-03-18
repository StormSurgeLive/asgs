#!/usr/bin/env perl
#--------------------------------------------------------------
# latest.pl : reads JSON output from get_nam_status.pl on
# STDIN and reports the latest available cycle to STDOUT
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
use JSON::PP qw/decode_json/;
use ASGSUtil;
#
# slurp the JSON response contents into a scalar variable
my $file_content = do { local $/; <> };
if (not $file_content) {
   ASGSUtil::stderrMessage( "ERROR", "Data piped for JSON decoding was empty.");
   exit 1;
}

local $@;
my $jshash_ref = eval { decode_json $file_content };
if ($@) {
   ASGSUtil::stderrMessage( "ERROR", "JSON::PP::decode_json failed: $@");
   warn qq{Data piped:\n$file_content\n};
   exit 1;
}

# grab the list of cycles out of the hash
my $cyclelistref = $jshash_ref->{"cyclelist"};
if ( ! defined $cyclelistref->[0] ) {
   ASGSUtil::stderrMessage( "ERROR", "The property 'cyclelist' ".  "did not contain any cycles.");
   exit 1;
}
printf $cyclelistref->[-1];
1;
#

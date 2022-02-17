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
use JSON::PP;
#
# slurp the JSON response contents into a scalar variable
my $file_content = do { local $/; <> };
my $jshash_ref = JSON::PP->new->decode($file_content);
# grab the list of cycles out of the hash
my $cyclelistref = $jshash_ref->{"forcing.nam.ncep.cyclelist"};
if ( ! defined $cyclelistref->[0] ) {
   ASGSUtil::stderrMessage(
             "ERROR",
             "The property 'forcing.nam.ncep.cyclelist' ".
             "did not contain any cycles.");
   die;
}
printf $cyclelistref->[-1];
1;
#

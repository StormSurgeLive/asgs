#!/usr/bin/env perl
#--------------------------------------------------------------------------
# opendap_post_gen.pl : create bash scripts for posting files to opendap
#--------------------------------------------------------------------------
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
#--------------------------------------------------------------------------
use strict;
use warnings;
use Template;
use Util::H2O qw/h2o/;
use ASGSUtil;
use Getopt::Long;

my $postType="default";

GetOptions("postType=s" => \$postType);

#-----------------------------------------------------------------
#       R E A D   C O N F I G U R A T I O N   J S O N
#-----------------------------------------------------------------
# slurp the JSON configuration contents into a scalar variable
my $file_content = do { local $/; <> };
my $jshash_ref = JSON::PP->new->decode($file_content);
# set the type of data/server to post to opendap
$jshash_ref->{'postType'} = $postType;
#-----------------------------------------------------------------
#       C R E A T E   T E M P L A T E   P R O C E S S O R
#-----------------------------------------------------------------
my $paths_ref = $jshash_ref->{'paths'};
my $scriptdir = $paths_ref->{'scriptdir'};
#
my $tt = Template->new({
    INCLUDE_PATH => $scriptdir,
}) || die "$Template::ERROR\n";
#
$tt->process('output/opendap_post_gen.tt2', $jshash_ref) || die $tt->error();

1;


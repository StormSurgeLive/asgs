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
use YAML::Tiny;
use Util::H2O qw/h2o/;
use ASGSUtil;
use Getopt::Long;

my $postType="default"; # default, archive, status, or custom
# JSON array of files to post (optional); the default list of
# files is assumed to be found in a file called
# $scenarioDir/post.opendap.files.json
my $filesJSON="null";

GetOptions("postType=s" => \$postType,
           "filesJSON=s" => \$filesJSON
);
#-----------------------------------------------------------------
#  R E A D   C O N F I G U R A T I O N   J S O N   F I L E S
#-----------------------------------------------------------------
# slurp the JSON configuration contents into a scalar variable
my $file_content = do { local $/; <> };
my $jshash_ref = JSON::PP->new->decode($file_content);
# set the type of data/server to post to opendap
# i.e., default, archive, status, or custom
$jshash_ref->{'postType'} = $postType;
#
ASGSUtil::stderrMessage("INFO","Opening /home/jason/Campaigns/Development/2022/opendap_test/platforms_test.yaml.");
my $platform_ref = YAML::Tiny->read('/home/jason/Campaigns/Development/2022/opendap_test/platforms_test.yaml');
$jshash_ref->{'platforms'} = $platform_ref->[0];
#print STDERR "stuff" . $jshash_ref->{'platforms'}->{'lsu_tds'}->{'threddsHost'};
#---------------------------------------------------
#             F I L E   L I S T
#-----------------------------------------------------------------
my $paths_ref = $jshash_ref->{'paths'};
my $scenarioDir = $paths_ref->{'scenarioDir'};
# load list of files
if ( $filesJSON eq "null" ) {
    $filesJSON = "$scenarioDir/post.opendap.files.json";
}
# add the opendap file list to the opendap hash before passing
# it to the template processor
unless (open(my $fl,"<", "$filesJSON")) {
    ASGSUtil::stderrMessage("WARNING","Failed to open $filesJSON: $!.");
    exit 1;
} else {
    ASGSUtil::stderrMessage("INFO","Opened $filesJSON.");
    $file_content = do { local $/; <$fl> };
    my $fileList_ref = JSON::PP->new->decode($file_content);
    # add the opendap file list to the opendap hash before passing
    # it to the template processor
    %$jshash_ref = (%$jshash_ref, %$fileList_ref);
}
#
#-----------------------------------------------------------------
#          D E R I V E D   P A R A M E T E R S
#-----------------------------------------------------------------

#-----------------------------------------------------------------
#          T E M P L A T E   P R O C E S S O R
#-----------------------------------------------------------------
# create processor
my $scriptDir = $paths_ref->{'scriptDir'};
my $tt = Template->new({
    INCLUDE_PATH => $scriptDir
}) || die "$Template::ERROR\n";
#
# now process template
$tt->process('output/opendap_post_gen.tt2', $jshash_ref) || die $tt->error();

1;


#!/usr/bin/env perl
#--------------------------------------------------------------
# deserialize.pl: Read json or yaml file and return the 
# specified property to stdout
#--------------------------------------------------------------
# Copyright(C) 2021 Jason Fleming
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
# If nowcast data is requested, the script will grab the nowcast 
# data corresponding to the current ADCIRC time, and then grab all
# successive nowcast data, if any. 
#
# If forecast data is requested, the script will grab the 
# forecast data corresponding to the current ADCIRC time.
#--------------------------------------------------------------
$^W++;
use strict;
use Getopt::Long;
use JSON::PP;
use YAML::Tiny;
use Date::Calc;
use Cwd;
#
my $property = "null"; # the property the calling script is seeking
my $file = "null";        # file that holds the json or yaml data
my %mapping;
#
GetOptions(
           "file=s" => \$file,    
           "property=s" => \$property
          );
#
# open an application log file for get_nam.pl
unless ( open(APPLOGFILE,">>deserialize.pl.log") ) { 
   &stderrMessage("ERROR","Could not open 'deserialize.pl.log' for appending: $!.");
   &appMessage("ERROR","Could not open 'deserialize.pl.log' for appending: $!.");
   exit 1;
}
# open file file
unless  ($file eq "null") { 
    unless (open(F,"<$file")) {
        stderrMessage("ERROR","Failed to open file to deserialize: $!.");
        die;
    }
    # slurp the file contents into a scalar variable
    my $file_content = do { local $/; <F> };
    close(F);
} else {
   &stderrMessage("ERROR","Did not provide the name of the file to be read.");
   &appMessage("ERROR","Did not provide the name of the file to be read.");
   exit 1;
}
if ( $type eq "json" ) {
    my $ref = JSON::PP->new->decode($file_content);
    %mapping = %$ref;
} else if ( $type eq "yaml" ) {

} else {
   &stderrMessage("ERROR","File type needs to be specified as either 'json' or 'yaml'.");
   &appMessage("ERROR","Did not provide the name of the file to be read.");    
}


if (exists($mapping{$property})) {
    print $mapping{$property};
} else {
    print "null";
}


close(APPLOGFILE);
1;

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
   #printf STDERR "$theTime $level: $enstorm: get_nam.pl: $message\n";
   printf STDERR "$theTime $level: read_json.pl: $message\n";
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
   #printf APPLOGFILE "$theTime $level: $enstorm: get_nam.pl: $message\n";
   printf APPLOGFILE "$theTime $level: read_json.pl: $message\n";
}

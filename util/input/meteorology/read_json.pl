#!/usr/bin/env perl
#--------------------------------------------------------------
# read_json.pl: Read json file and return the specified value
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
my $jsonfile = "null"; # file that holds the json data
our %properties;
#
GetOptions(
           "jsonfile=s" => \$jsonfile,
           "property=s" => \$property,
          );
#
# open an application log file for get_nam.pl
unless ( open(APPLOGFILE,">>read_json.pl.log") ) { 
   stderrMessage("ERROR","Could not open 'read_json.pl.log' for appending: $!.");
   exit 1;
}
# open json file 
unless (open(JSON,"<$jsonfile")) {
   stderrMessage("ERROR","Failed to open json file: $!.");
   die;
}
# slurp the file contents into a scalar variable
my $file_content = do { local $/; <JSON> };
close(JSON);

my $jsonref = JSON::PP->new->decode($file_content);
my %json = %$jsonref;

if (exists($json{$property})) {
    print $json{$property};
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

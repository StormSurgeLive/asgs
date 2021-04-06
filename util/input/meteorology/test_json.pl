#!/usr/bin/env perl
#--------------------------------------------------------------
# test_json.pl: testing json i/o
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
use Date::Calc;
use Cwd;
#
my $statefile = "null"; # file that holds the current simulation state
#
GetOptions(
           "statefile=s" => \$statefile
          );
#
# open an application log file for get_nam.pl
unless ( open(APPLOGFILE,">>test_json.pl.log") ) { 
   stderrMessage("ERROR","Could not open 'test_json.pl.log' for appending: $!.");
   exit 1;
}
#
# create a hash of properties from run.properties
our %properties;
# open properties file 
unless (open(RUNPROP,"<run.properties")) {
   stderrMessage("ERROR","Failed to open run.properties: $!.");
   die;
}
while (<RUNPROP>) {
   my @fields = split ':',$_, 2 ;
   # strip leading and trailing spaces and tabs
   $fields[0] =~ s/^\s|\s+$//g ;
   $fields[1] =~ s/^\s|\s+$//g ;
   $properties{$fields[0]} = $fields[1];
}
close(RUNPROP);

#my $utf8_encoded_json_text = encode_json \%properties;

#print $utf8_encoded_json_text;

my $json = JSON::PP->new->utf8->pretty->canonical->encode(\%properties);
print $json;

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
   printf STDERR "$theTime $level: test_json.pl: $message\n";
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
   printf APPLOGFILE "$theTime $level: test_json.pl: $message\n";
}

#!/usr/bin/env perl
#--------------------------------------------------------------------------
# ASGSUtil.pm : Utility subroutines for perl scripts used in ASGS.
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
#--------------------------------------------------------------------------
use strict;
use warnings;

use File::Basename qw(basename);
use Date::Calc;
use JSON::PP;

package ASGSUtil;

#
# stringify array - this is needed for numbers with
# leading zeroes because they are not valid JSON
sub stringify {
   my ($array_ref) = @_;
   foreach my $c (@$array_ref) {
      $c = "$c";
   }
   return;
}
#
# add timestamp to the JSON to indicate that the
# calling script has modified it (and when)
sub timestampJSON {
   # json passed by reference and modified in this subroutine
   my ( $jshash_ref ) = @_;
   my ( $package, $filename, $line ) = caller;
   my $calling_script = File::Basename::basename($filename);
    # add time stamp
   my $timestamp = _getTimeStamp();
   my $lastupdated_ref = $jshash_ref->{"lastupdated"};
   my $ts_ref = { $calling_script => $timestamp };
   push(@$lastupdated_ref,$ts_ref);
   # stringify list of daily forecast cycles
   # if they are present
   my $forecastcyclesref = $jshash_ref->{"configDailyForecastcycles"};
   if ( defined $forecastcyclesref ) {
      stringify($forecastcyclesref);
   }
   # add the name of this script
   $jshash_ref->{"self"} = $calling_script;
   return;
}
#
# write out the JSON file containing additional parameters
# as added by this script
sub writeJSON {
   # json passed by reference and modified in this subroutine
   my ( $jshash_ref ) = @_;
   my ( $package, $filename, $line ) = caller;
   my $calling_script = File::Basename::basename($filename);
   # add the name of this script
   $jshash_ref->{"self"} = $calling_script;
   # serialize
   my $json = JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
   # write out
   my $SJ;
   unless ( open($SJ,">","$calling_script.json") ) {
      stderrMessage("ERROR","Could not open '$calling_script.json' for writing: $!.");
      die;
   }
   print $SJ $json;
   close($SJ);
   return;
}

sub setParameter {
   my ( $jshash_ref, $param_ref, $key, $default ) = @_;
   if ( $$param_ref eq "null" ) {
      if ( %$jshash_ref && defined $jshash_ref->{$key} ) {
         $$param_ref = $jshash_ref->{$key};
      } else {
         $$param_ref = $default;
         # reset parameter value in hash to the value we
         # are actually using
         $jshash_ref->{$key} = $$param_ref;
      }
   }
   # bomb out if there is no default value for the parameter
   if ( $$param_ref eq "null" ) {
      stderrMessage("ERROR","The parameter '$key' was not specified.");
      die;
   }
   return;
}

# create a hash of properties from run.properties
sub readProperties {
   my ( $runProp_ref, $fileName ) = @_;
   # open properties file
   unless ( open(my $rp,"<","$fileName") ) {
      stderrMessage("ERROR","Failed to open '$fileName': $!.");
      return;
   }
   while (<$rp>) {
      my @fields = split ':',$_, 2 ;
      # strip leading and trailing spaces and tabs
      $fields[0] =~ s/^\s|\s+$//g ;
      $fields[1] =~ s/^\s|\s+$//g ;
      $runProp_ref->{$fields[0]} = $fields[1];
   }
   close($rp);
   return;
}

#
# write a log message to stderr
sub stderrMessage {
   my ( $level, $message ) = @_;
   my $theTime = _getTimeStamp();
   my ($package, $filename, $line) = caller;
   my $calling_script = File::Basename::basename($filename);
   print STDERR "$theTime $level: $calling_script: $message\n";
   return;
}
#
# write a log message to a log file dedicated to this script (typically debug messages)
sub appMessage {
   my ( $level, $message ) = @_;
   my $theTime = _getTimeStamp();
   my ($package, $filename, $line) = caller;
   my $calling_script = File::Basename::basename($filename);
   my $A;
   unless ( open($A,">>","$calling_script.log") ) {
      stderrMessage(
            "ERROR",
            "Could not open '$calling_script.log' for appending: ".
            "$!.");
      die;
   }
   print $A "$theTime $level: $calling_script: $message\n";
   close($A);
   return;
}

sub _getTimeStamp {
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour,
    my $dayOfMonth, my $month, my $yearOffset,
    my $dayOfWeek, my $dayOfYear, my $daylightSavings)
    = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my ($D_y,$D_m,$D_d, $Dh,$Dm,$Ds, $dst) = Date::Calc::Timezone;
   my $tzs = sprintf "%+03d00", $Dh;
   my $theTime = "$year-$months[$month]-$dayOfMonth-T$hms$tzs";
   return $theTime;
}

1;

__END__

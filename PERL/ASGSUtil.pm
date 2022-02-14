#!/usr/bin/env perl
#--------------------------------------------------------------------------
# Util.pm : Utility subroutines for perl scripts used in ASGS.
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

use File::Basename;
use Date::Calc;
use JSON::PP;

package ASGSUtil;

use constant EXIT_SUCCESS => 0;
use constant EXIT_DIE     => 255;

exit __PACKAGE__->run( \@ARGV // [] ) if not caller;

sub run {
    print "Running the ASGSUtil perl module as an app is not supported.\n";
    exit EXIT_DIE;
}

#
# write out the JSON file containing additional parameters
# as added by this script
sub writeJSON {
   my ($jshash_ref, $this) = @_;
   # json passed by reference and modified in this subroutine
    # add time stamp
   my $timestamp = _getTimeStamp();
   my $lastupdated_ref = $jshash_ref->{"lastupdated"};
   my $ts_ref = { $this => $timestamp };
   push(@$lastupdated_ref,$ts_ref);
   my $json = JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
   # write out
   my $SJ;
   unless ( open($SJ,">","$this.json") ) {
      stderrMessage("ERROR","Could not open '$this.json' for writing: $!.");
      die;
   }
   print $SJ $json;
   close($SJ);
}

sub setParameter {
   my ( $this, $jshash_ref, $param_ref, $key, $default ) = @_;
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
      stderrMessage("ERROR",$this,"The parameter '$key' was not specified.");
      die;
   }
}
#
# write a log message to stderr
sub stderrMessage {
   my ( $level, $this, $message ) = @_;
   my $theTime = _getTimeStamp();
   printf STDERR "$theTime $level: $this: $message\n";
}
#
# write a log message to a log file dedicated to this script (typically debug messages)
sub appMessage {
   my ( $level, $this, $message ) = @_;
   my $theTime = _getTimeStamp();
   #
   # open an application log file
   unless ( open(APPLOGFILE,">>$this.log") ) {
      stderrMessage($this,"ERROR","Could not open $this.log for appending: $!.");
   }
   printf APPLOGFILE "$theTime $level: $this: $message\n";
   close(APPLOGFILE);
}

sub _getTimeStamp {
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "$year-$months[$month]-$dayOfMonth-T$hms";
   return $theTime;
}

1;

__END__
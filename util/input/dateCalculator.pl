#!/usr/bin/env perl
#---------------------------------------------------------------------
# dateCalculator.pl
#
# Takes a date and a difference in days, hours, minutes, and seconds
# calculates a new date. 
#
#---------------------------------------------------------------------
#
# Copyright(C) 2013 Jason Fleming
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
#---------------------------------------------------------------------
#
use strict;
use Getopt::Long;
use Date::Calc;
$^W++;
#
my $inputDate; # in yyyymmddhh24mmss format
# difference can be positive or negative
my $ddays = 0;     
my $dhours = 0;
my $dminutes = 0;
my $dseconds = 0;
#
GetOptions(
           "input-date=s" => \$inputDate,
           "ddays=s" => \$ddays,
           "dhours=s" => \$dhours,
           "dminutes=s" => \$dminutes,
           "dseconds=s" => \$dseconds
           );
$inputDate =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})/;
my $sy = $1;
my $sm = $2;
my $sd = $3;
my $sh = $4;
my $smin = $5;
my $ss = $6; 
my ($ey,$em,$ed,$eh,$emin,$es) =
       Date::Calc::Add_Delta_DHMS($sy,$sm,$sd,$sh,$smin,$ss,$ddays,$dhours,$dminutes,$dseconds);
my $outputDate = sprintf("%04d %02d %02d %02d %02d %02d",$ey,$em,$ed,$eh,$emin,$es);
printf "The resulting date is $outputDate.\n";

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: dateCalculator.pl: $message\n";
}



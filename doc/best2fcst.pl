#!/usr/bin/env perl
#---------------------------------------------------------------------
# best2fcst.pl
#
# Take an ATCF formatted BEST track file and fill in the forecast
# increment (TAU in ATCF parlance) in hours according to the date/time
# column, starting at zero.
#
# The resulting file will be used as input to aswip and ultimately
# ADCIRC's NWS19.
#
#---------------------------------------------------------------------
#
# Copyright(C) 2012 Jason Fleming
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
use Date::Pcalc;
$^W++;
#
my $input = "null";
#
GetOptions(
           "input=s" => \$input,
           );
#
unless(open(BEST,"<$input")) {
   stderrMessage("ERROR","Failed to open BEST track file $input: $!.");
   die;
}
my $output = "fcst_$input";
unless(open(FCST,">$output")) {
   stderrMessage("ERROR","Failed to open forecast track file $output: $!.");
   die;
}
my $start_date = "null";
my $sy; my $sm; my $sd; my $sh;
my $fy; my $fm; my $fd; my $fh;
while(<BEST>) {
    my @fields = split(',',$_);
    my $date = $fields[2];
    if ( $start_date eq "null" ) {
        $start_date = $date;
        $start_date =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
       $sy = $1;
       $sm = $2;
       $sd = $3;
       $sh = $4;
    }
    # determine difference in hours between this date and the
    # start date
    $date =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
    $fy = $1;
    $fm = $2;
    $fd = $3;
    $fh = $4;
    (my $ddays,my $dhrs, my $dsec)
        = Date::Pcalc::Delta_DHMS($sy,$sm,$sd,$sh,0,0,$fy,$fm,$fd,$fh,0,0);
    my $time_difference = $ddays*24 + $dhrs; # in hours
    my $line = $_;
    substr($line,30,3) = sprintf("%3d",$time_difference);
    printf FCST $line;
}

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: best2fcst.pl: $message\n";
   if ($level eq "ERROR") {
      sleep 60
   }
}



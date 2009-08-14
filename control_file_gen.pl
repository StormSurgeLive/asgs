#!/usr/bin/env perl
#--------------------------------------------------------------------------
# control_file_gen.pl
#

# This script uses the template fort.15 file and the ATCF formatted fort.22
# file as input and produces a fort.15 file as output. The name of the template
# file and the fort.22 file to be used as input must be specified on the
# command line. 
#
# It optionally accepts the coldstarttime (YYYYMMDDHH24), that is, the 
# calendar time that corresponds to t=0 in simulation time. If it is 
# not provided, the first line in the fort.22 file is used as the cold start 
# time, and this time is written to stdout.
#
# It optionally accepts the time in a hotstart file in seconds since cold
# start.
#
# If the time of a hotstart file has been supplied, the fort.15 file 
# will be set to hotstart.
#
# It optionally accepts the end time (YYYYMMDDHH24) at which the simulation
# should stop (e.g., if it has gone too far inland to continue to be 
# of interest).
#
# If the --name option is set to nowcast, the RNDAY will be calculated such 
# that the run will end at the nowcast time.
#
# The --dt option can be used to specify the time step size if it is 
# different from the default of 3.0 seconds. 
#
# The --bladj option can be used to specify the Boundary Layer Adjustment
# parameter for the Holland model (not used by the asymmetric wind vortex
# model, NWS=9.
#
# The NHSINC will be calculated such that a hotstart file is always generated
# on the last time step of the run.
#
# usage:
#   %perl control_file_gen.pl [--cst coldstarttime] [--hst hotstarttime]
#   [--dt timestep] [--nowcast] [--controltemplate templatefile] < storm1_fort.22 
#
#--------------------------------------------------------------------------
# Copyright(C) 2006, 2007, 2008, 2009 Jason Fleming
# Copyright(C) 2006, 2007 Brett Estrade
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
#
$^W++;
use strict;
use Getopt::Long;
use Date::Pcalc; 
use Cwd;
#
my @TRACKS = (); # should be few enough to store all in an array for easy access
my $controltemplate;
my $metfile;
my $coldstarttime;
my $hotstarttime;      # time, in seconds, of hotstart file (since coldstart)
my $hotstarttime_days; # time, in days, of hotstart file (since coldstart)
my $endtime;
my $dt=3.0; 
my $bladj=0.9;
my $enstorm;    # ensemble name of the storm
my $nhcName="STORMNAME"; # storm name given by the nhc
my $tau=0; # forecast period
my $dir=getcwd();
my $nws=9;
my $advisorynum;

GetOptions("controltemplate=s" => \$controltemplate,
           "metfile=s" => \$metfile,
           "name=s" => \$enstorm, 
           "cst=s" => \$coldstarttime,
           "endtime=s" => \$endtime,
           "dt=s" => \$dt,
           "bladj=s" => \$bladj, 
           "nws=s" => \$nws, 
           "advisorynum=s" => \$advisorynum,
           "nhcName=s" => \$nhcName,
           "hst=s" => \$hotstarttime);
#
# open template file for fort.15
open(TEMPLATE,"<$controltemplate") || die "ERROR: control_file_gen.pl: Failed to open the fort.15 template file $controltemplate for reading.";
#
# open output control file
open(STORM,">fort.15") || die "ERROR: control_file_gen.pl: Failed to open the output control file $dir/fort.15.";
print STDOUT "INFO: control_file_gen.pl: The output fort.15 file will be written to the directory $dir.\n"; 
#
# open met input file
open(METFILE,"<$metfile") || die "ERROR: control_file_gen.pl: Failed to open meteorological (ATCF-formatted) fort.22 file $metfile for reading.";
#
# Build track list
#
while (<METFILE>) {
  chomp($_);
  my @tmp = ();
  # split and remove any spaces
  foreach my $item (split(',',$_)) {
    $item =~ s/\s*//g;
    push(@tmp,$item);
  }
  # 2d array of arrays; [@tmp] creates an anon array in each element of @TRACK
  push(@TRACKS,[@tmp]); 
}
#
# find last hindcast line
my $track;
my $nowcast;
foreach $track (reverse(@TRACKS)) {
   if (@{$track}[4] =~ m/BEST/) {
     if ( $nhcName eq "STORMNAME" ) {
        # We need to get the storm name from the last hindcast line
        if ( defined $track->[27] ) {
           $nhcName = $track->[27];
        } else {	 
           printf STDERR "WARNING: control_file_gen.pl: The name of the storm does not appear in the hindcast.\n";
        }
     }
     # also grab the last hindcast time; this will be the nowcast time
     $nowcast = $track->[2];     
     last;
   }
}
#
# get coldstart time
my $cstart;
unless ( $coldstarttime ) {
   $cstart = $nowcast; # adjusted later to make the difference nonzero
   print $cstart; # write cold start time to stdout for use in later runs
} else {
   $cstart = $coldstarttime;
}
# convert hotstart time (in days since coldstart) if necessary
if ( $hotstarttime ) {
   $hotstarttime_days = $hotstarttime/86400.0;
}
# get end time
my $end;
# for a nowcast, end the run at the end of the hindcast
if ( $enstorm eq "nowcast" ) { 
   $end = $nowcast;
   printf(STDOUT "INFO: control_file_gen.pl: New nowcast time is $end.\n");
} elsif ( $endtime ) {
   # if this is not a nowcast, and the end time has been specified, end then
   $end = $endtime
} else {
   # this is not a nowcast; end time was not explicitly specified, 
   # get end time based on either 
   # 1. running out of fort.22 file or 
   # 2. two or more days inland
   my $ty;  # level of tropical cyclone development
   my $now_inland; # boolean, 1 if TY is "IN"
   my $tin; # time since the first occurrence of TY as "IN"
   my $tin_year;
   my $tin_mon;
   my $tin_day;
   my $tin_hour;
   my $tin_min;
   my $tin_sec;
   my $tin_tau; # forecast period at first occurrence of TY as "IN"
   my $c_year;  # time of line currently being processed
   my $c_mon;
   my $c_day;
   my $c_hour;
   my $c_min;
   my $c_sec;
   my $ddays;
   my $dhrs;
   my $dsec; # difference btw time inland and time on current line
   foreach $track (@TRACKS) {
#     my $lat = substr(@{$track}[6],0,3); # doesn't work if only 2 digits
     #@{$track}[6] =~ /[0-9]*/;
     $_ = @{$track}[6];
     /([0-9]*)/;
     $end = $track->[2];
     $tau = $track->[5];
     $ty = @{$track}[10];
     if ( $ty eq "IN" and (not $now_inland) ) {
        $now_inland = 1;
        $tin = @{$track}[2]; # time at first occurrence of "IN" (inland) 
        $tin =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
        $tin_year = $1;
        $tin_mon = $2;
        $tin_day = $3;
        $tin_hour = $4;
        $tin_min = 0;
        $tin_sec = 0;
        $tin_tau = @{$track}[5]
     }
     if ( $now_inland ) { 
        $end =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
        $c_year = $1;
        $c_mon = $2;
        $c_day = $3;
        $c_hour = $4;
        $c_min = 0;
        $c_sec = 0;
        #
        # get difference between first occurrence of IN (inland)
        # and the time on the current track line
        ($ddays,$dhrs,$dsec) 
           = Date::Pcalc::Delta_DHMS(
                $tin_year,$tin_mon,$tin_day,$tin_hour,$tin_min,$tin_sec,
                $c_year,$c_mon,$c_day,$c_hour,$c_min,$c_sec);
        my $time_inland = $ddays + $dhrs/24 + $dsec/86400 + ($tau-$tin_tau)/24;
        if ( $time_inland >= 2.0 ) {
           last; # jump out of loop with current track as last track
        }
     }
   }
}
printf(STDOUT "INFO: control_file_gen.pl: The fort.15 file will be configured to end on $end.\n");
#
$cstart=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $cs_year = $1;
my $cs_mon = $2;
my $cs_day = $3;
my $cs_hour = $4;
my $cs_min = 0.0;
my $cs_sec = 0.0;
#
$end =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $e_year = $1;
my $e_mon = $2;
my $e_day = $3;
my $e_hour = $4;
my $e_min = 0.0;
my $e_sec = 0.0;
#
# get difference btw cold start time and end time
my ($days,$hours,$seconds) 
   = Date::Pcalc::Delta_DHMS(
      $cs_year,$cs_mon,$cs_day,$cs_hour,$cs_min,$cs_sec,
      $e_year,$e_mon,$e_day,$e_hour,$e_min,$e_sec);
# RNDAY is diff btw cold start time and end time
# For a forecast, RNDAY is one time step short of the total time to ensure 
# that we won't run out of storm data at the end of the fort.22
# For a nowcast, RNDAY will be one time step long, so that we end at
# the nowcast time, even if ADCIRC rounds down the number of timesteps
my $stopshort = 0.0;
if ( $enstorm eq "nowcast" ) {
   $stopshort = -2*$dt;
} else {
   $stopshort = $dt;
}
my $RNDAY = $days + $hours/24.0 + ($seconds-$stopshort)/86400.0; 
#
# If RNDAY is less than two timesteps, make sure it is at least two timesteps. 
# This can happen if we start up from a fort.22 that has only one BEST line,
# i.e., it starts at the nowcast. RNDAY would be zero in this case, except 
# our algorithm actually stops one ts short of the full time, so RNDAY is
# actually negative in this case. ADCIRC needs at least two timesteps from 
# coldstart to create a valid hotstart file.
my $runlength_seconds = $RNDAY*86400.0;
if ( $hotstarttime ) {
   $runlength_seconds-=$hotstarttime;
}
my $min_runlength = 2*$dt; 
# if we coldstart at the nowcast, we may not have calculated a runlength 
# longer than the minimum
if ( $runlength_seconds < $min_runlength ) { 
   printf STDERR "INFO: control_file_gen.pl: Runlength was calculated as $runlength_seconds seconds, which is less than the minimum runlength of $min_runlength seconds. The RNDAY will be adjusted so that it ADCIRC runs for the minimum length of simulation time.\n";
   # recalculate the RNDAY as the hotstart time plus the minimal runlength
   if ( $hotstarttime ) {
      $RNDAY=$hotstarttime_days + ($min_runlength/86400.0);
   } else {
      $RNDAY=$min_runlength/86400.0;
   }
   $runlength_seconds = $min_runlength;
}
#
# if this is an update from hindcast to nowcast, calculate the hotstart 
# increment so that we only write a single hotstart file at the end of 
# the run. If this is a forecast, don't write a hotstart file at all.
my $NHSINC = int($runlength_seconds/$dt);
my $NHSTAR;
if ( $enstorm eq "nowcast" ) {
   $NHSTAR = 1;
} else {
   $NHSTAR = 0;
}
# create run description
my $rundesc=$nhcName . " " . $advisorynum;
while(<TEMPLATE>) {
    # if we are looking at the first line, fill in the name of the storm
    # and the advisory number, if available
    s/%StormName%/$rundesc/;
    # if we are looking at the DT line, fill in the time step (seconds)
    s/%DT%/$dt/;
    # if we are looking at the RNDAY line, fill in the total run time (days)
    s/%RNDAY%/$RNDAY/;  
    # fill in the correct value of IHOT -- we always look for a fort.68
    # file, and since we only write one hotstart file during the run, we
    # know we will always be left with a fort.67 file.
    if ( $hotstarttime ) {
       s/%IHOT%/68/;
    } else { 
       s/%IHOT%/0/;
    }
    # fill in the parameter that selects which wind model to use
    s/%NWS%/$nws/;
    # fill in the timestep increment that hotstart files will be written at
    s/%NHSINC%/$NHSINC/;
    # fill in whether or not we want a hotstart file out of this
    s/%NHSTAR%/$NHSTAR/;
    # 
    # fill in ensemble name
    s/%EnsembleID%/$enstorm/;
    # Holland parameters -- only used by symmetric vortex model (NWS=8)
    s/%HollandParams%/$cs_year $cs_mon $cs_day $cs_hour 1 $bladj/;
    print STORM $_;
}
close(TEMPLATE);
close(STORM);
close(METFILE);

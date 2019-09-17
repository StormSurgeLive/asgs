#!/usr/bin/env perl
#---------------------------------------------------------------------
# nam_fort22_gen.pl
#
# Creates an ADCIRC fort.22 for the OWI format (NWS12) by determining 
# the number of blank snaps to insert before the actual meteorological
# data begins. The number of blank snaps is based on the difference
# between the cold start time and the beginning of the wind data, and
# on the increment in the wind data. 
#---------------------------------------------------------------------
#
# Copyright(C) 2010 Jason Fleming
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
use Cwd;
$^W++;

# the output will be stored in a file called fort.22

my $advisdir;                            # path to fort.22* files
my $csdate;                         # YYYYMMDDHH24
#my $start;                          # hotstart or coldstart
my $hstime = 0.0;                   # default is not hotstart
my $enstorm = "nowcast";            # hindcast, nowcast, nhcConsensus, veerLeft
my @supportedNames = qw/hindcast nowcast nhcConsensus maxWindSpeed overlandSpeed veer rMax/;
my $pi=3.141592653589793;
#
#
GetOptions(
           "advisdir=s" => \$advisdir,
           "csdate=s" => \$csdate,
           "hstime=s" => \$hstime,
           "enstorm=s" => \$enstorm
           );

# presumptive fix put in the satisfy 'strict' warnings
my $name          = $enstorm;
my $dir           = $advisdir;
my $coldstartdate = $csdate;
#
# check to see if the name of the storm is one that this script knows how to
# generate ... if not, bomb out
unless ( grep { $_ eq $name } @supportedNames ) {	
   die "ERROR: storm_track_gen.pl: Unable to generate the '$name' ensemble member. This type of storm variation is not suppported.\n";
}
#
# check to see that all the mandatory command line arguments were specified
unless ( $dir ) {
   $dir = cwd();
   printf STDERR "WARNING: storm_track_gen.pl: The path to the raw ATCF input files was not specified with the --dir argument. It will be assumed that the files are in the directory $dir.\n";
} 
#
# create the fort.22 output file, which is the wind input file for ADCIRC
open(MEMBER,"fort.22") || die "ERROR: nam_fort22_gen.pl: Failed to open file for ensemble member '$name' fort.22 input file: $!.";
my $wtiminc = <MEMBER>; 
if ( $wtiminc =~ /^# (\d+)/ ) {
   $wtiminc = $1;
   printf STDERR "INFO: nam_fort22_gen.pl: WTIMINC is '$wtiminc'.";
} else {
   die "ERROR: nam_fort.22_gen.pl: Could not get WTIMINC from fort.22.";
}
close(MEMBER);
#
# determine time difference in seconds between cold start date and nowcast
# time of OWI file
open(PRE,"fort.221") || die "ERROR: nam_fort22_gen.pl: Failed to open file for ensemble member '$name' fort.221 input file: $!.";
my $nowcasttime = <PRE>; 
my @fields = split(" ",$nowcasttime);
$nowcasttime = $fields[-1];
printf STDERR "INFO: nam_fort22_gen.pl: OWI nowcast time is '$nowcasttime'.";
close(PRE);
#
#
$coldstartdate =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $csYear = $1;
my $csMonth = $2;
my $csDay = $3;
my $csHour = $4;
$nowcasttime =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $nYear = $1;
my $nMonth = $2;
my $nDay = $3;
my $nHour = $4;
#
# get difference
(my $ddays, my $dhrs, my $dsec)
           = Date::Calc::Delta_DHMS(
                $csYear,$csMonth,$csDay,$csHour,0,0,
                $nYear,$nMonth,$nDay,$nHour,0,0);
#
# find the difference in seconds
my $blank_time = $ddays*86400.0 + $dhrs*3600.0 + $dsec;
printf STDERR "blank time is $blank_time";
my $nwbs = int($blank_time/$wtiminc);
printf STDERR "nwbs is $nwbs";
#
# create the fort.22 output file, which is the wind input file for ADCIRC
open(MEMBER,">fort.22") || die "ERROR: nam_fort22_gen.pl: Failed to open file for ensemble member '$name' fort.22 input file: $!.";
printf MEMBER "1";     # nwset
printf MEMBER "$nwbs"; # nwbs
printf MEMBER "1.0";   # dwm
close(MEMBER); 
1;

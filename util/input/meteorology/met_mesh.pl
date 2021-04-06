#!/usr/bin/env perl
#---------------------------------------------------------------------
# met_mesh.pl : generates a mesh for use with aswip
#---------------------------------------------------------------------
#
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
#---------------------------------------------------------------------
#
use strict;
use Getopt::Long;
use Date::Calc;
use Cwd;
$^W++;

my $np;  # number of nodes
my $radial_extent = 400; # (nm) max dist. to write V,P
my $quadrant_radial_resolution = 2; # data points per nm
my $spatial_radial_increment = 1;
my $azimuthal_increment = 5;
my $num_avalues; # node dimension in the azimuthal direction for met mesh
my $num_rvalues  # node dimension in radial direction for met mesh
my @slam;        # longitudes
my @sfea;        # latitudes
my @outputSecs;  # time in seconds for output
my @outputLons;  # longitudes at each output time
my @outputLats;  # latitudes at each output time

our $pi = 3.141592653589793;
our $deg2rad = $pi/180.0;
our $Rearth = 6378206.4; # radius of earth (m)
#
# read in track file and compute outputLats and outputLons


#
# FIXME : requires that these things divide evenly
$np = $radial_extent/$spatial_radial_increment * (360/$azimuthal_increment);
#
# create a set of nodes in circles around the center of the
# storm so that we can compute/visualize V and P in all directions
my $dr = $spatial_radial_increment;
$num_rvalues = $radial_extent/$spatial_radial_increment; # needs to be an integer
$num_avalues = 360/$azimuthal_increment;                 # needs to be an integer
@
# generate nodal coordinates
#
my $n=1; # node counter
# create nodes
for ( my $r=0; $r<$num_rvalues; $r++ ) {
    for ( my $a=0; $a<$num_avalues; $a++ ) {
        $azimuth = $a*$azimuthal_increment * $deg2rad;
        $dx = $r*$spatial_radial_increment*cos($azimuth) * 1852.0d0 # nm to m
        $dy = $r*$spatial_radial_increment*sin($azimuth) * 1852.0d0 # nm to m
        ( $sfea[$n], $slam[$n] = &xy2latlon($dx, $dy, $outputLats[$iout], $outputLons[$iout]);
        $nodex[$n] = $dx;
        $nodey($n) = $dy;
        $n++;
    }
}
$np = $n;
my $slam0 = $outputLons[$iout];
my $sfea0 = $outputLats[$iout];
foreach my $sl (@slam) {
    $sl *= $deg2rad;
}
foreach my $sf (@sfea) {
    $sf = $sf * $deg2rad + $outputLats[$iout] * $deg2rad;
}
# now write the nodes to mesh file if needed
# create fort.14 file
unless (open(MESH,">fort.14")) {
    stderrMessage("ERROR","Failed to open vtk file $outfile for writing: $!.");
    die;
}
# write the list of internal (nonboundary) nodes
    write(777,'(I12)') (np-2*num_avalues)
    do i=(2*num_avalues+1),np
        write(777,'(2(E15.8,2X),A)')
&                     nodex(i), nodey(i)," 0.0"
    enddo
    close(777)


sub xy2latlon() {
    my $x = shift;
    my $y = shift;
    my $lat0 = shift;
    my $lon0 = shift;
    my $lat = $y / ($deg2rad * $Rearth)
    my $lon = $lon0 + $x / ($deg2rad * $Rearth * cos($deg2rad*$lat0))
    return ($lat, $lon);
}




sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: $this: $message\n";
}


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

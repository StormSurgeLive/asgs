#!/usr/bin/env perl
#
# Copyright(C) 2011--2020 Jason Fleming
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
# ptFile_gen.pl: Generates a list of lat/lon points to interpolate
# NAM data onto during the conversion from lambert conformal to geographic
# coordinates.
#
$^W++;
use strict;
use Getopt::Long;
#
my $swlat=7.0;    # latitude at southwest corner of point set
my $swlon=-98.75; # longitude at southwest corner of point set
my $dx=0.25;      # longitude point spacing
my $dy=0.25;      # latitude point spacing
my $nx=160;       # number of points in east-west direction 
my $ny=160;       # number of points in north-south direction
my $output_file="ptFile_out.txt"; 
#
GetOptions("swlat=s" => \$swlat,
           "swlon=s" => \$swlon,
           "dx=s" => \$dx,
           "dy=s" => \$dy,
           "nx=s" => \$nx,
           "ny=s" => \$ny
            );
#
open(PTFILE,">$output_file") || die "ERROR: ptFile_gen.pl: Can't open $output_file file for writing: $!.";
#
my $total_pts = $nx*$ny;
printf PTFILE "$total_pts\n";
my $pt_num=0;
for (my $j=0; $j<$ny; $j++) {
   for (my $i=0; $i<$nx; $i++) {
      $pt_num++;
      my $lon=$swlon+$i*$dx;
      my $lat=$swlat+$j*$dy;
      my $line = sprintf("%d,  %3.4f , %3.4f\n",$pt_num,$lon,$lat);
      printf PTFILE $line;
   }
}
close(PTFILE);

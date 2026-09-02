#!/usr/bin/env perl
#---------------------------------------------------------------------
# isotach_viz.pl : read track file and generate geometry for
# visualizing isotachs in paraview
#---------------------------------------------------------------------
# Copyright(C) 2026 Jason Fleming
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
#---------------------------------------------------------------------
# Usage: perl isotach_viz.pl < trackfile > trackfile_isotach_geometry.xmf
#---------------------------------------------------------------------
use strict;
use Geo::Ellipsoid;
use XML::Writer;
$^W++;
#
# load track data from stdin
my $track_data = do { local $/; <> };
#
my $cycle_num = 0;
my $previous_hour = -1;    # hour ("TAU") on the previous line
my $previous_tech = -1;    # line type designation ("TECH") on the previous line
my $previous_warning_date = -1;    # warning date ("YYYYMMDDHH") on the previous line
my @isotachs_per_cycle;
my @tech;
my @cycle_years;
my @cycle_months;
my @cycle_days;
my @cycle_hours;
my @cycle_lats;
my $lat_hemisphere;
my @cycle_lons;
my $lon_hemisphere;
my @time;
my @timesec;
my @vmax;
my @nhc_rmax;
my @pc;
my $min_pc = 1015;
my $stormname;
my @tr_speeds;           # storm translation speed
my @tr_directions;       # storm translation direction
my @rmax;
my @month_labels = qw( Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec );
my $cy = "01";  # storm number within a particular year
my $basin = "al";
# store isotachs as a hash keyed to the dates; e.g., key 2020082006_34
my $isotach_key;
my %dates_isotachs;
my @isotach_datetimes;
#
foreach my $l (split /\r?\n/, $track_data) {
   # break into fields
   my @fields = split(',',$l);
   $basin = $fields[0];
   $cy = $fields[1];
   my $warning_date = $fields[2]; # YYYYMMDDHH24 UTC
   my $hour = $fields[5];
   # check the line type to see if it is BEST or OFCL
   $fields[4] =~ /([A-Z]{4})/;
   my $this_tech = $1; # BEST or OFCL
   my $new_cycle = 1;
   #jgfdebug print $new_cycle;
   # check to see if the date/time on this line is repeated
   # (this indicates that a new isotach is being specified)
   if ( $this_tech eq "BEST" ) {
      if ( $warning_date eq $previous_warning_date ) {
        $new_cycle = 0;
      }
   } elsif ( $this_tech eq "OFCL" ) {
        if ( $hour eq $previous_hour ) {
            $new_cycle = 0;
        }
   } else {
        die "ERROR: The ATCF type (TECH) '$this_tech' was not recognized. Expected either 'OFCL' or 'BEST'."
   }
   #jgfdebug print $new_cycle;
   # if the type changes from one line to the next,
   # this indicates a new cycle in any case
   if ( $this_tech ne $previous_tech ) {
      $new_cycle = 1;
   }
   # if this is a new cycle, grab all the data that is the same
   # for all isotachs in the cycle
   if ( $new_cycle == 1 ) {
      $cycle_num++;
      $rmax[$cycle_num] = "";
      $isotachs_per_cycle[$cycle_num] = 1;
      $fields[4] =~ /([A-Z]{4})/;
      $tech[$cycle_num] = $1; # BEST or OFCL
      $fields[2] =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
      $cycle_years[$cycle_num] = $1;
      $cycle_months[$cycle_num] = $2;
      $cycle_days[$cycle_num] = $3;
      $cycle_hours[$cycle_num] = $4;
      $time[$cycle_num] = $4."Z ".$2."/".$3."/".$1;
      $fields[5] =~ /(\d+)/;
      $timesec[$cycle_num] = $1 * 3600.0;
      # latitude degrees and hemisphere
      $fields[6] =~ /([0-9]+)([A-Z]{1})/;
      $cycle_lats[$cycle_num] = $1/10.0;
      $lat_hemisphere = $2;
      if ( $lat_hemisphere eq "S" ) {
            $cycle_lats[$cycle_num] *= -1.0;
      }
      # longitude degrees and hemisphere
      $fields[7] =~ /([0-9]+)([A-Z]{1})/;
      $cycle_lons[$cycle_num] = $1/10.0;
      $lon_hemisphere = $2;
      if ( $lon_hemisphere eq "W" ) {
            $cycle_lons[$cycle_num] *= -1.0;
      }
      # maximum wind speed in kt
      $fields[8] =~ /(\d+)/;
      $vmax[$cycle_num] = $1;
      # Rmax according to NHC in nautical miles
      $fields[19] =~ /(\d+)/;
      $nhc_rmax[$cycle_num] = $1;
      # central pressure in mb
      $fields[9] =~  /(\d+)/;
      $pc[$cycle_num] = $1;
      if ( $pc[$cycle_num] < $min_pc ) {
         $min_pc = $pc[$cycle_num];
      }
      # storm name
      $fields[27] =~ /([A-Z]+)/;
      $stormname = $1;
      # storm direction in compass degrees
      $fields[25] =~ /(\d+)/;
      $tr_directions[$cycle_num] = $1;
      # storm translation speed in kt
      $fields[26] =~ /(\d+)/;
      $tr_speeds[$cycle_num] = $1;
      # jgfdebug printf "$basin $cy $time[$cycle_num] $tech[$cycle_num] $timesec[$cycle_num] $cycle_lons[$cycle_num] $cycle_lats[$cycle_num] $timesec[$cycle_num] $pc[$cycle_num]\n";
   } else {
      $isotachs_per_cycle[$cycle_num]++;
   }
   $previous_hour = $hour;
   $previous_tech = $this_tech;
   $previous_warning_date = $warning_date;
   # grab isotach speed
   $fields[11] =~ /(\d+)/;
   my $speed = $1;
   # early in a storm's development (INVEST and prior) the NHC may
   # list the isotach wind speed as zero ... we use 34kt here instead
   # ... just for consistency of display in the plots
   if ( $speed == 0 ) {
      $speed = 34;
   }
   # grab isotach radii
   $isotach_datetimes[$cycle_num] = "$cycle_years[$cycle_num]$cycle_months[$cycle_num]$cycle_days[$cycle_num]$cycle_hours[$cycle_num]";
   $isotach_key = "$isotach_datetimes[$cycle_num]_$speed";
   $dates_isotachs{$isotach_key} = "$fields[13] $fields[14] $fields[15] $fields[16]";
   #jgfdebug printf "$isotach_key $dates_isotachs{$isotach_key}\n";
}
#
# set up data for visualization
my @standard_isotach_speeds = ( 34, 50, 64 );
#
# Initialize the ellipsoid model (using WGS84 or NAD27)
my $geo = Geo::Ellipsoid->new(
    ellipsoid           => 'WGS84',
    angle_unit          => 'degrees',
    distance_unit       => 'nm',    # nautical miles
    longitude_symmetric => 1        # [-180,180)
);
my @starting_bearings = ( 0.0, 90.0, 180.0, 270.0 );
#
# Initialize the writer with indentation enabled
my $namespace_uri = "http://www.w3.org/2001/XInclude";
my $writer = XML::Writer->new(
    NAMESPACES  => 1,
    DATA_MODE   => 1,
    DATA_INDENT => 4
);
$writer->xmlDecl('UTF-8');
$writer->startTag('Xdmf', 'Version'  => '3.0' );
    $writer->startTag('Domain', 'Name' => "ATCF" );
        $writer->startTag('Grid', 'Name' => 'TimeSeries', 'GridType' => 'Collection', 'CollectionType' => 'Temporal');
        # loop over forecast times
        for ( my $c=1 ; $c<$cycle_num; $c++ ) {
            my $elements;
            my $num_elements = 0;
            my $isotach_speeds;
            my $points = "$cycle_lons[$c] $cycle_lats[$c] 0  "; # origin for this time
            my $num_points = 1;

            # loop over quadrants at this time
            for ( my $q=0 ; $q<4 ; $q++ ) {  # NE SE SW NW
                my @isotach_distances;
                my $isotach_num = 0;
                foreach my $sis (@standard_isotach_speeds) {
                    my $date_isotachs = "$isotach_datetimes[$c]_$sis";
                    if ( !defined $dates_isotachs{$date_isotachs} ) {
                        next;
                    }
                    # we have an isotach in this quadrant at this time (it may be zero)
                    my @isotachs = split(' ',$dates_isotachs{$date_isotachs});
                    $isotach_distances[$isotach_num] = $isotachs[$q];
                    $isotach_num++;
                }
                # now we have the number of isotachs in this quadrant at this
                # time as well as their values (some may be zero)
                #jgfdebug printf "There are $isotach_num isotachs in quadrant $q at time $isotach_datetimes[$c] and their values are @isotach_distances\n";
                my $num_nonzero_isotachs = 0;
                foreach my $id (@isotach_distances) {
                    if ( $id != 0 ) {
                        $num_nonzero_isotachs++;
                    }
                }
                # skip this quadrant at this time if all the isotachs distances are zero
                if ( $num_nonzero_isotachs == 0 ) {
                    next;
                }
                # loop over isotachs in this quadrant, highest to lowest
                my $first_isotach_plotted = 0;
                for ( my $sis=($num_nonzero_isotachs-1) ; $sis>-1; $sis-- ) {
                    my $bearing = $starting_bearings[$q];
                    # generate points
                    for ( my $i=0; $i<11; $i++ ) {
                        my ($lat_dest, $lon_dest) = $geo->at($cycle_lats[$c], $cycle_lons[$c], $isotach_distances[$sis], $bearing);
                        $points .= "$lon_dest $lat_dest 0  ";
                        $num_points++;
                        $bearing += 9.0;  # compass degrees
                    }
                    # generate elements
                    if ( $first_isotach_plotted == 0 ) {
                        # this is the first isotach to be plotted in this quadrant
                        for ( my $i=($num_points-11); $i<($num_points-1); $i++ ) {
                            my $pn = $i;
                            my $pnn = $i + 1;
                            $elements .= "0 $pnn $pn  ";
                            $isotach_speeds .= "$standard_isotach_speeds[$sis] ";
                            $num_elements++;
                        }
                        $first_isotach_plotted = 1;
                    } else {
                        # not the first isotach to be plotted in this quadrant
                        # generate elements
                        for ( my $i=($num_points-11); $i<($num_points-1); $i++ ) {
                            my $pm1 = $i - 11;
                            my $p0 = $i - 10;
                            my $pn = $i;
                            my $pnn = $i + 1;
                            $elements .= "$p0 $pnn $pn  ";
                            $isotach_speeds .= "$standard_isotach_speeds[$sis] ";
                            $num_elements++;
                            $elements .= "$p0 $pn $pm1  ";
                            $isotach_speeds .= "$standard_isotach_speeds[$sis] ";
                            $num_elements++;
                        }
                    }
                }
            }
            if ( $num_elements == 0 ) {
                next;
            }
            # grid
            $writer->startTag('Grid', 'Name' => "Time=$isotach_datetimes[$c]", 'GridType' => 'Uniform');
            $writer->emptyTag('Time', 'Value' => "$isotach_datetimes[$c]");
            # geometry
            $writer->startTag('Geometry', 'Origin' => '', 'Type' => 'XYZ');
            $writer->startTag('DataItem', 'DataType' => 'Float', 'Dimensions' => "$num_points 3", 'Format' => 'XML');
            $writer->characters("$points");
            $writer->endTag('DataItem');
            $writer->endTag('Geometry');
            # topology
            $writer->startTag('Topology', 'Dimensions' => "$num_elements", 'Type' => 'Triangle');
            $writer->startTag('DataItem', 'DataType' => 'Int', 'Dimensions' => "$num_elements 3", 'Format' => 'XML');
            $writer->characters("$elements");
            $writer->endTag('DataItem');
            $writer->endTag('Topology');
            # attributes
            $writer->startTag('Attribute', 'Name' => 'IsotachSpeed', 'Center' => 'Cell');
            $writer->startTag('DataItem', 'DataType' => 'Int', 'Dimensions' => "$num_elements", 'Format' => 'XML');
            $writer->characters("$isotach_speeds");
            $writer->endTag('DataItem');
            $writer->endTag('Attribute');
            $writer->endTag('Grid');
        }
      $writer->endTag('Grid');
   $writer->endTag('Domain');
$writer->endTag('Xdmf');
#
# write the xml to stdout
$writer->end();
#!/usr/bin/env perl
#--------------------------------------------------------------
# hurdat2_to_atcf.pl:
# Extract one tropical cyclone from an NHC HURDAT2 file and write an
# ATCF B-deck-style best-track file.
#--------------------------------------------------------------
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
#--------------------------------------------------------------
#
# HURDAT2:
#   Header: AL052024, BERYL, 26,
#   Data:   YYYYMMDD, HHMM, record_id, status, lat, lon, vmax, mslp,
#           34NE,34SE,34SW,34NW,50NE,50SE,50SW,50NW,
#           64NE,64SE,64SW,64NW[, RMW]
#
# ATCF output:
#   One BEST record is written for each available wind-radius threshold
#   (34, 50, 64 kt). If no radii are available at a time, one record with
#   RAD=0 is written so that the center/intensity point is retained.
#
# Examples:
#   perl hurdat2_to_atcf.pl \
#       --input hurdat2-1851-2025-02172026.txt \
#       --storm AL052024   # produces bal052024.dat
#
#   perl hurdat2_to_atcf.pl \
#       --input hurdat2.txt \
#       --name BERYL --year 2024 \
#       --output bal052024.dat # select by storm name
#
# List storms available for a particular year:
#   perl hurdat2_to_atcf.pl --input hurdat2.txt --list --year 2024
#
# Notes:
#   * ATCF latitude/longitude are tenths of degrees with a hemisphere suffix.
#   * HURDAT2 status codes (TD, TS, HU, EX, SD, SS, LO, WV, DB) map directly
#     to the ATCF storm-type field.
#   * HURDAT2 "-999" missing values are converted to ATCF conventional
#     missing/zero values as described below.
#   * This script writes the commonly used extended comma-delimited ATCF
#     B-deck layout. Fields not present in HURDAT2 are left blank or set to 0.
#
#------------------------------------------------------------------------------
#
# HURDAT2 stores the 34-, 50-, and 64-knot NE/SE/SW/NW radii together in each
# observation. The script converts those into separate ATCF BEST records for
# each available threshold, preserving the quadrant data. HURDAT2 itself
# contains six-hourly position/intensity data and, for modern storms,
# storm-size information.
#
# One caveat: HURDAT2 does not contain every field available in the extended
# ATCF B-deck format, such as motion direction/speed, gusts, eye diameter, or
# outer closed-isobar information. Those fields are therefore left blank or zero
# rather than invented. The core best-track position, maximum wind, pressure,
# storm type, wind radii, storm name, and optional RMW are transferred.
#
#------------------------------------------------------------------------------
$^W++;
use strict;
use warnings;
use Getopt::Long qw(GetOptions);
use File::Basename qw(basename);
use ASGSUtil;

my $input;
my $storm_id;
my $storm_name;
my $year;
my $output;
my $list = 0;
my $help = 0;
my $include_asynoptic = 1;

GetOptions(
    'input=s'            => \$input,
    'storm=s'            => \$storm_id,
    'name=s'             => \$storm_name,
    'year=i'             => \$year,
    'output=s'           => \$output,
    'list'               => \$list,
    'asynoptic!'         => \$include_asynoptic,
    'help|h'             => \$help,
) or usage(1);

usage(0) if $help;

die "ERROR: --input is required.\n" unless defined $input;

$storm_id   = uc($storm_id)   if defined $storm_id;
$storm_name = uc($storm_name) if defined $storm_name;

if (defined $storm_id && $storm_id !~ /^[A-Z]{2}\d{6}$/) {
    die "ERROR: --storm must look like AL052024 or EP012023.\n";
}

open my $in, '<', $input or die "ERROR: cannot open '$input': $!\n";

my @storms;
my $current;

while (my $line = <$in>) {
    chomp $line;
    $line =~ s/\r$//;
    next if $line =~ /^\s*$/;

    my @f = map { trim($_) } split /,/, $line, -1;

    # Header records begin with an 8-character storm ID such as AL052024.
    if (defined $f[0] && $f[0] =~ /^([A-Z]{2})(\d{2})(\d{4})$/) {
        $current = {
            id       => $f[0],
            basin    => $1,
            number   => $2,
            year     => 0 + $3,
            name     => uc($f[1] // ''),
            declared => 0 + ($f[2] // 0),
            records  => [],
        };
        push @storms, $current;
        next;
    }

    next unless $current;

    # HURDAT2 data records have date and time in first two columns.
    if (defined $f[0] && $f[0] =~ /^\d{8}$/ &&
        defined $f[1] && $f[1] =~ /^\d{4}$/) {
        push @{$current->{records}}, \@f;
    }
}
close $in;

if ($list) {
    my @show = @storms;
    @show = grep { $_->{year} == $year } @show if defined $year;

    printf "%-8s %-4s %-16s %s\n", "ID", "YEAR", "NAME", "RECORDS";
    for my $s (@show) {
        printf "%-8s %-4d %-16s %d\n",
            $s->{id}, $s->{year}, $s->{name}, scalar @{$s->{records}};
    }
    exit 0;
}

if (!defined $storm_id && !defined $storm_name) {
    die "ERROR: specify --storm AL052024 or --name NAME [--year YYYY].\n";
}

my @matches;

if (defined $storm_id) {
    @matches = grep { $_->{id} eq $storm_id } @storms;
} else {
    @matches = grep { $_->{name} eq $storm_name } @storms;
    @matches = grep { $_->{year} == $year } @matches if defined $year;
}

if (!@matches) {
    my $what = defined $storm_id ? $storm_id : $storm_name;
    die "ERROR: storm '$what' was not found in '$input'.\n";
}

if (@matches > 1) {
    die "ERROR: storm name '$storm_name' matched multiple years; add --year YYYY.\n"
        unless defined $storm_id;
}

my $storm = $matches[0];

$output //= sprintf(
    "b%s%s%04d.dat",
    lc($storm->{basin}),
    $storm->{number},
    $storm->{year}
);

open my $out, '>', $output or die "ERROR: cannot write '$output': $!\n";

my $records_written = 0;

for my $r (@{$storm->{records}}) {
    # HURDAT2 columns
    my (
        $date, $hhmm, $record_id, $status, $lat, $lon, $vmax, $mslp,
        $r34ne, $r34se, $r34sw, $r34nw,
        $r50ne, $r50se, $r50sw, $r50nw,
        $r64ne, $r64se, $r64sw, $r64nw,
        $rmw
    ) = @$r;

    # HURDAT2 can include special/asynoptic records, e.g. landfall ("L").
    # Keep them by default; --no-asynoptic restricts output to 00/06/12/18 UTC.
    if (!$include_asynoptic && $hhmm !~ /^(0000|0600|1200|1800)$/) {
        next;
    }

    my $atcf_dt = $date . substr($hhmm, 0, 2);
    my $atcf_lat = coord_to_atcf($lat, 1);
    my $atcf_lon = coord_to_atcf($lon, 0);

    $vmax   = missing_to_zero($vmax);
    $mslp   = missing_to_zero($mslp);
    $status = trim($status // '');
    $status = 'XX' if $status eq '';

    my @radius_sets = (
        [34, $r34ne, $r34se, $r34sw, $r34nw],
        [50, $r50ne, $r50se, $r50sw, $r50nw],
        [64, $r64ne, $r64se, $r64sw, $r64nw],
    );

    my $wrote_radius_line = 0;

    for my $set (@radius_sets) {
        my ($rad, @quad) = @$set;

        # A HURDAT2 radius is missing when all four quadrants are -999.
        next if all_missing(@quad);

        @quad = map { missing_to_zero($_) } @quad;

        print {$out} atcf_line(
            basin     => $storm->{basin},
            cy        => $storm->{number},
            dtg       => $atcf_dt,
            lat       => $atcf_lat,
            lon       => $atcf_lon,
            vmax      => $vmax,
            mslp      => $mslp,
            ty        => $status,
            rad       => $rad,
            windcode  => 'NEQ',
            radii     => \@quad,
            rmax      => missing_to_zero($rmw),
            name      => $storm->{name},
            record_id => $record_id,
        ), "\n";

        $records_written++;
        $wrote_radius_line = 1;
    }

    # Preserve the center/intensity point even when no wind radii exist.
    if (!$wrote_radius_line) {
        print {$out} atcf_line(
            basin     => $storm->{basin},
            cy        => $storm->{number},
            dtg       => $atcf_dt,
            lat       => $atcf_lat,
            lon       => $atcf_lon,
            vmax      => $vmax,
            mslp      => $mslp,
            ty        => $status,
            rad       => 0,
            windcode  => '',
            radii     => [0, 0, 0, 0],
            rmax      => missing_to_zero($rmw),
            name      => $storm->{name},
            record_id => $record_id,
        ), "\n";

        $records_written++;
    }
}

close $out;

print STDERR "Storm:   $storm->{id} $storm->{name}\n";
print STDERR "Input:   $input\n";
print STDERR "Output:  $output\n";
print STDERR "HURDAT2 records: ", scalar(@{$storm->{records}}), "\n";
print STDERR "ATCF records:    $records_written\n";

exit 0;

# ----------------------------------------------------------------------
# Build an extended ATCF B-deck record.
#
# Common ATCF field order:
#  1 BASIN       2 CY          3 YYYYMMDDHH  4 TECHNUM/MIN
#  5 TECH        6 TAU         7 LAT          8 LON
#  9 VMAX       10 MSLP       11 TY          12 RAD
# 13 WINDCODE   14 RAD1       15 RAD2        16 RAD3
# 17 RAD4       18 POUT       19 ROUT        20 RMW
# 21 GUSTS      22 EYE        23 SUBREGION   24 MAXSEAS
# 25 INITIALS   26 DIR        27 SPEED       28 STORMNAME
# 29 DEPTH      30 SEAS       31 SEASCODE    32 SEAS1
# 33 SEAS2      34 SEAS3      35 SEAS4       36 USERDEFINED
# 37 USERDATA
#
# HURDAT2 does not contain many of these fields, so they are left blank
# or zero. HURDAT2 record identifiers are retained in USERDATA.
# ----------------------------------------------------------------------
sub atcf_line {
    my %a = @_;

    my @q = @{$a{radii}};

    my @fields = (
        $a{basin},            # BASIN
        $a{cy},               # CY
        $a{dtg},              # DTG
        '',                   # TECHNUM / minute field
        'BEST',               # TECH
        0,                    # TAU
        $a{lat},              # LAT
        $a{lon},              # LON
        $a{vmax},             # VMAX kt
        $a{mslp},             # MSLP hPa
        $a{ty},               # storm type
        $a{rad},              # wind threshold
        $a{windcode},         # NEQ
        @q,                   # NE, SE, SW, NW radii (nm)
        0,                    # POUT
        0,                    # ROUT
        $a{rmax},             # RMW (nm), if present in source
        0,                    # GUSTS
        0,                    # EYE
        '',                   # SUBREGION
        0,                    # MAXSEAS
        '',                   # INITIALS
        0,                    # DIR
        0,                    # SPEED
        $a{name},             # STORMNAME
        '',                   # DEPTH
        0,                    # SEAS
        '',                   # SEASCODE
        0, 0, 0, 0,          # SEAS radii
        'HURDAT2',            # USERDEFINED
        trim($a{record_id} // ''), # USERDATA: HURDAT2 special record flag
    );

    # ATCF files conventionally use comma + space delimiters.
    return join(', ', @fields);
}

sub coord_to_atcf {
    my ($coord, $is_lat) = @_;
    $coord = trim($coord // '');

    if ($coord !~ /^([0-9]+(?:\.[0-9]+)?)([NSEW])$/i) {
        die "ERROR: invalid HURDAT2 coordinate '$coord'.\n";
    }

    my ($value, $hem) = ($1, uc($2));
    my $tenths = int($value * 10.0 + 0.5);

    if ($is_lat && $hem !~ /^[NS]$/) {
        die "ERROR: latitude '$coord' has invalid hemisphere.\n";
    }
    if (!$is_lat && $hem !~ /^[EW]$/) {
        die "ERROR: longitude '$coord' has invalid hemisphere.\n";
    }

    return $tenths . $hem;
}

sub all_missing {
    for my $v (@_) {
        $v = trim($v // '');
        return 0 if $v ne '' && $v != -999;
    }
    return 1;
}

sub missing_to_zero {
    my ($v) = @_;
    $v = trim($v // '');
    return 0 if $v eq '' || $v == -999;
    return 0 + $v;
}

sub trim {
    my ($s) = @_;
    $s = '' unless defined $s;
    $s =~ s/^\s+//;
    $s =~ s/\s+$//;
    return $s;
}

sub usage {
    my ($exit_code) = @_;
    print <<"USAGE";
Usage:
  $0 --input HURDAT2 --storm AL052024 [--output bal052024.dat]
  $0 --input HURDAT2 --name BERYL --year 2024 [--output FILE]
  $0 --input HURDAT2 --list [--year 2024]

Options:
  --input FILE       HURDAT2 database file.
  --storm ID         Exact HURDAT2 storm ID, e.g. AL052024.
  --name NAME        Select by storm name.
  --year YYYY        Year restriction when selecting by name or listing.
  --output FILE      ATCF output file. Default: b<stormid>.dat.
  --[no-]asynoptic   Keep/drop non-00/06/12/18 UTC HURDAT2 records.
                     Default: keep them.
  --list             List storms in the input database.
  --help             Show this help.

Examples:
  $0 --input hurdat2.txt --storm AL052024
  $0 --input hurdat2.txt --name BERYL --year 2024
  $0 --input hurdat2.txt --list --year 2024
USAGE
    exit $exit_code;
}

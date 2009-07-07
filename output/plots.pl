#!/usr/bin/env perl
#
# Copyright(C) 2006, 2007, 2008, 2009 Jason Fleming
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
$^W++;
use strict;
use Getopt::Long;

my $advisdir;   # directory for the individual advisory
my $inputdir;  # directory where the template files are stored

GetOptions("advisdir=s" => \$advisdir,
           "inputdir=s" => \$inputdir);

my $windmin=0.0;  # minimum wind speed
my $windmax=0.0;  # maximum wind speed
my $watermin=0.0; # minimum storm surge
my $watermax=0.0; # maximum storm surge
my @Flds;
my $i;

open(DATA,"$advisdir/allruns.out") || die "ERROR: Can't open allruns.out.";
while(<DATA>) {
    @Flds = split;
    # gnuplot columns 35 38 41 44 47 (indexed from 1 counting date time as 1)
    for ( $i=34; $i<47; $i=$i+3 ) {
        if ( $Flds[$i] > $windmax ) { $windmax = $Flds[$i]; }
    }
    # gnuplot columns 58 59 60 61 62 (indexed from 1 counting date time as 1)
    for ( $i=57; $i<62; ++$i ) {
        if ( $Flds[$i] > $watermax ) { $watermax = $Flds[$i]; }
        if ( $Flds[$i] < $watermin ) { $watermin = $Flds[$i]; }   
    }
}
close(DATA);
print "windmax is $windmax, watermax is $watermax, watermin is $watermin";
if ( $windmax < 20.0 ) { $windmax = 20.0; }
if ( $watermax < 2.0 ) { $watermax = 2.0; }
if ( $watermin > 0.0 ) { $watermin = 0.0; }

open(TEMPLATE,"$inputdir/template.gp") || die "ERROR: Can't open template.gp file.";

while(<TEMPLATE>) {
    s/%watermin%/$watermin/;
    s/%watermax%/$watermax/;
    s/%windmin%/$windmin/;  
    s/%windmax%/$windmax/;  
    print $_;
}
close(TEMPLATE);

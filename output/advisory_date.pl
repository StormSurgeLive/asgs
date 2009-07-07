#!/usr/bin/env perl
#
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
#
$^W++;
use strict;
use Getopt::Long;
# usage:
#   %perl advisory_date.pl [--print-date-only] psfile
my $advisory_date; 
my $pdo='';  # useful if you just want to know the date for dir name
GetOptions( "print-date-only" => \$pdo );
open(FORT22,"fort.22") || die "ERROR: Could not open fort.22 file: $!";
while(<FORT22>) {
    my @line=split(",",$_);
    if ( $line[4]=~/OFCL/ ) {
	$advisory_date=$line[2];
	$advisory_date=~s/\s*//g;
	last;
    }
}
if ($pdo) {
    print STDOUT "$advisory_date\n";
} else {
    close(FORT22);
    $^I=".bak";
    while(<>) {
	s/Outlet/Outlet, Advisory $advisory_date/;
	print;
    }
}

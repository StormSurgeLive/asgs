#!/usr/bin/env perl
#--------------------------------------------------------------
# validate_atcf.pl: transcribes atcf file into excel csv
# format (all text fields in double quotes) column-by-column
# to examine and validate the way fields are lined up.
#--------------------------------------------------------------
# Copyright(C) 2022 Jason Fleming
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
# This script is useful for examining the output of automated
# processes that produce or consume ATCF formatted data to confirm
# that data are line up properly.
#
# It accepts an ATCF file on standard input and writes an excel
# style CSV file to standard output. The CSV does not attempt
# to represent the fields of the ATCF file; rather, it encloses
# each character (including spaces) into individual "cells"
# separated by commas. The result can be loaded into Google
# Sheets or MS Excel to see how each character lines up into
# a columnt. It also produces header lines as measuring aids
# to understand column numbering.
#--------------------------------------------------------------
use strict;
use warnings;
#
# 300 character measuring stick
my @measure;
$measure[0] = "                                                                                                   1         1         1         1         1         1         1         1         1         1         2         2         2         2         2         2         2         2         2         2         3";
$measure[1] = "         1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0";
$measure[2] = "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";

# write each number line character in its own csv cell
for (my $i=0; $i<3; $i++ ) {
    csvAtomize($measure[$i]);
}
# read and write each character in its own csv cell
while (<>) {
    chomp;
    csvAtomize($_);
}
sub csvAtomize {
    my $line = shift;
    my @characters = split(//,$line);
    my $lineLength = @characters;
    for ( my $c=0; $c<@characters; $c++ ) {
        if ( $c == ($lineLength - 1) ) {
            printf "\"$characters[$c]\"\n";
        } else {
            printf "\"$characters[$c]\",";
        }
    }
}
1;
#!/usr/bin/env perl
#
# corps_index.pl:
#    creates index.html files to display output in the form of ASGS 
#    archive files.
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

my $stormname;        # name of the storm from nhcClassName
my $advisory;         # the advisory number
my $giskmzjpgarchive; # name of the tar file that holds the gis, kmz, jpg files
my $plotsarchive;     # name of the file that holds the hydrograph and wind speed plots
my $templatefile;     # html template to be filled in with results

GetOptions("advisory=s" => \$advisory,
           "templatefile=s" => \$templatefile,
           "giskmzjpgarchive=s" => \$giskmzjpgarchive,
           "plotsarchive=s" => \$plotsarchive
           );

open(STORMNAMEFILE,"nhcClassName") || die "ERROR: corps_index.pl: Can't open 'nhcClassName' file for reading the NHC class and name of the storm.";

$stormname = <STORMNAMEFILE>;
chomp($stormname);
close(STORMNAMEFILE);

open(TEMPLATE,"<$templatefile") || die "ERROR: corps_index.pl: Can't open '$templatefile' file for reading as a template for the results display.";

while(<TEMPLATE>) {
    s/%stormname%/$stormname/;
    s/%giskmzjpgarchive%/$giskmzjpgarchive/;
    s/%plotsarchive%/$plotsarchive/;
    print $_;
}
close(TEMPLATE);

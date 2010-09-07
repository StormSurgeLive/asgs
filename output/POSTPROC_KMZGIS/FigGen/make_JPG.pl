#!/usr/bin/env perl
#--------------------------------------------------------------
# make_JPG.pl: This script will revise the input file for FigureGen in
# order to plot the output for the 5 tracks run in the ASGS.
#--------------------------------------------------------------
# Copyright(C) 2008 Robert Weaver
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
#--------------------------------------------------------------
# RJW:08/2008
# $^W++;
use strict;
use Getopt::Long;
#  Usage Example:
#   perl make_JPG.pl --outputdir $POSTPROC_DIR --gmthome $GMTHOME --storm 01 --year 2006 --adv 05 --n 30.5 --s 28.5 --e -88.5 --w -90.5 --outputprefix 01_2006_nhcconsensus_05
#
my $gmthome; # location of GMT executables
my $storm;   # nhc storm number
my $year;    # year of storm
my $adv;     # advisory number
my $north;   # north latitude extent of bounding box
my $south;   # south latitude extent of bounding box
my $east;    # east longitude extent of bounding box
my $west;    # west longitude extent of bounding box
my $outputprefix; # base name of JPG file
my $figuregen_template; # full path file name of template file for FigureGen  
GetOptions(
           "figuregen_template=s" => \$figuregen_template,
           "gmthome=s" => \$gmthome,
           "storm=s" => \$storm,
           "year=s" => \$year,
           "adv=s" => \$adv,
           "n=s" => \$north,
           "s=s" => \$south,
           "e=s" => \$east,
           "w=s" => \$west,
           "outputprefix=s" => \$outputprefix
           );
#
# set location of gmt executables
my $gmthomebin = $gmthome . "/bin";
#
# create plot flag and title
my $plotlabel = "1,HWM (NAVD88): ".$storm." ".$year." Adv #".$adv;
#
unless ( open(TEMPLATE,"<$figuregen_template") ) {
   stderrMessage("ERROR","Could not open $figuregen_template: $!");
}
# just write a new input file
unless ( open(FIGGENFILE,">FigGen_$outputprefix.inp" ) ) {
   stderrMessage("ERROR","Could not create FigGen_$outputprefix.inp: $!");
   die;
}
while(<TEMPLATE>) {
   # location of gmt executables
   s/%GMTHOMEBIN%/$gmthomebin\//;
   # set base name of figure gen output
   s/%OUTPUTPREFIX%/$outputprefix/;
   # set plot flag and title
   s/%PLOTLABEL%/$plotlabel/;
   # set western extent of bounding box
   s/%WEST%/$west/;
   # set eastern extent of bounding box
   s/%EAST%/$east/;
   # set southern extent of bounding box
   s/%SOUTH%/$south/;
   # set northern extent of bounding box
   s/%NORTH%/$north/;
#  
   print FIGGENFILE $_;
}  
close TEMPLATE;
close FIGGENFILE;
#
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: make_JPG.pl: $message\n";
   if ($level eq "ERROR") {
      sleep 60
   }
}
          

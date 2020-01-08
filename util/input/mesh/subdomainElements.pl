#!/usr/bin/env perl
#--------------------------------------------------------------------------
# subdomainElements.pl: Loads ADCIRC fort.18 (message passing) files,
# reads element mapping tables, and tallies which subdomains claim 
# which elements.
#--------------------------------------------------------------------------
# Copyright(C) 2015--2016 Jason Fleming
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
#--------------------------------------------------------------------------
#
$^W++;
use strict;
use Getopt::Long;
use Date::Calc;
use Cwd;
#
my $fulldomainDir; # path to the full domain files, containing PE* dirs
my @positives;  # num fort.18 files containing positive f.d. element number
my @negatives;  # num fort.18 files containing negative f.d. element number
my @absolutes;  # num fort.18 files containing abs val of f.d. element number
my @repeats;    # used to check if same  fd ele num repeated within one fort.18
my @subdomain;  # to visualize which subdomain an element is assigned to 
my $ne_g;      # number of fulldomain elements
# 
GetOptions("fulldomaindir=s" => \$fulldomainDir);
#
# make a list of subdomain directories
my @subdomainDirs = glob("$fulldomainDir/PE*");
#
my $initialized = 0;
my $totalNumSubdomainElements = 0; 
#
# loop over fort.18 files in subdirectories
foreach my $dir (@subdomainDirs) { 
   #
   # open fort.18 (ADCIRC subdomain info) file
   unless (open(FORT18,"<$dir/fort.18")) {
      stderrMessage("ERROR","Failed to open the $dir/fort.18 file for reading: $!");
      die;
   }
   my $line = <FORT18>; # FileFmt line; throwaway
   # MNEP = max elements in any subdomain; NELP(i) is number of elements
   # in this subdomain i 
   $line = <FORT18>;    # NELG   NE_G   MNEP   NELP(i) 
   my @fields = split(' ',$line);
   $ne_g = $fields[1];
   my $mnep = $fields[2];
   my $ne = $fields[3];
   printf "$dir/fort.18: NE_G=$ne_g  MNEP=$mnep  NE=$ne.\n";
   $totalNumSubdomainElements += $ne;
   # 
   # on the first pass through the loop, initialize all tallies to 0
   if ($initialized == 0 ) {
      for (my $i=1; $i<=$ne_g; $i++) {
         $positives[$i] = 0;
         $negatives[$i] = 0;
         $absolutes[$i] = 0;
         $subdomain[$i] = 0; 
      }
      $initialized = 1;
   }
   # on every pass through the loop, reset repeat tallies to zero
   for (my $i=1; $i<=$ne_g; $i++) {
      $repeats[$i] = 0;
   }  
   #
   # read full domain element numbers 
   for (my $i=0; $i<$ne; $i++) {
      $line = <FORT18>;
      # tally positive element numbers
      if ( $line > 0 ) {
         $positives[$line]++;
      }
      # tally negative numbers
      if ( $line < 0 ) {
         $negatives[abs($line)]++;
      }
      # tally absolute values of element numbers
      my $absElementNum = abs($line);
      $absolutes[$absElementNum]++;
      # check for repeats within a particular subdomain
      $repeats[$absElementNum]++;
      if ($repeats[$absElementNum] > 1) {
         stderrMessage("WARNING","Element number $absElementNum appeared $repeats[$absElementNum] times in subdomain $dir.");
         $repeats[$absElementNum]++;
      }
      $dir =~ /PE(\d{4})/;
      my $subdomainNumber = sprintf("%d",$1);
      $subdomainNumber++; # 1 indexed, same as partmesh.txt
      $subdomain[$absElementNum] = $subdomainNumber 
   }
   #
   # close message passing info file
   close(FORT18);
}  
#
# open file for positives
unless (open(POSITIVES,">positives.100")) {
   stderrMessage("ERROR","Failed to open the positives file for writing: $!.");
   die;
}
#
# open file for negatives 
unless (open(NEGATIVES,">negatives.100")) {
   stderrMessage("ERROR","Failed to open the negatives file for writing: $!.");
   die;
}
#
# open file for absolutes 
unless (open(ABSOLUTES,">absolutes.100")) {
   stderrMessage("ERROR","Failed to open the absolutes file for writing: $!.");
   die;
}
#
# open file for subdomain numbers
unless (open(SUBDOMAINS,">subdomains.100")) {
   stderrMessage("ERROR","Failed to open the subdomains file for writing: $!.");
   die;
}
#
printf POSITIVES "# comment line\n";
printf POSITIVES "# header line\n";
printf POSITIVES "0.000000    0\n";    # faux time and timestep
#
printf NEGATIVES "# comment line\n";
printf NEGATIVES "# header line\n";
printf NEGATIVES "0.000000    0\n";    # faux time and timestep
#
printf ABSOLUTES "# comment line\n";
printf ABSOLUTES "# header line\n";
printf ABSOLUTES "0.000000    0\n";    # faux time and timestep
#
printf SUBDOMAINS "# comment line\n";
printf SUBDOMAINS "# header line\n";
printf SUBDOMAINS "0.000000    0\n";    # faux time and timestep
#
# write positives and negatives to file
for (my $i=0; $i<$ne_g; $i++) {
   my $fdEle = $i+1;
   # write positives
   printf POSITIVES "$fdEle  $positives[$fdEle]\n";
   # write negatives
   printf NEGATIVES "$fdEle  $negatives[$fdEle]\n";
   # write absolutes
   printf ABSOLUTES "$fdEle  $absolutes[$fdEle]\n";
   # write subdomain numbers
   printf SUBDOMAINS "$fdEle  $subdomain[$fdEle]\n";
}
close(POSITIVES);
close(NEGATIVES);
close(ABSOLUTES);
close(SUBDOMAINS);
#
stderrMessage("INFO","There are $totalNumSubdomainElements subdomain elements in total.");
#
#--------------------------------------------------------------------------
#   S U B   S T D E R R  M E S S A G E
#
# Writes a log message to standard error.
#--------------------------------------------------------------------------
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: subdomainElements.pl: $message\n";
}

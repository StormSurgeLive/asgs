#!/usr/bin/env perl
#--------------------------------------------------------------------------
# subdomainNodes.pl: Loads ADCIRC fort.18 (message passing) files,
# reads node mapping tables, and tallies which subdomains claim 
# which nodes. Also reads partmesh.txt for the same reason.
#--------------------------------------------------------------------------
# Copyright(C) 2015 Jason Fleming
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
#
my $fulldomainDir; # path to the full domain files, containing PE* dirs
my @residents;  # num fort.18 files containing positive f.d. node number
my @ghosts;     # num fort.18 files containing negative f.d. node number
my @ghostmem;   # subdomain in which the node is ghost
my @absolutes;
my @subdomain;  # viz subdomain node membership according to fort.18 
my @psubdomain; # viz subdomain node membership according to partmesh.txt 
my @repeats;
my $agrid = "mesh comment line not found"; # first line from fort.14 mesh file
my $np_g;      # number of fulldomain nodes
# 
GetOptions("fulldomaindir=s" => \$fulldomainDir);
#
# see if we can get the agrid header line from the mesh file for use
# in filling out the headers in the .63 files 
if ( -e "$fulldomainDir/fort.14" ) {
   unless (open(FDMESH,"<$fulldomainDir/fort.14")) {
      stderrMessage("WARNING","Could not open mesh file (fort.14) to extract comment line: $!");
   } else {
      $agrid = <FDMESH>;
      chomp($agrid);
      close(FDMESH);
   }
}
#
# make a list of subdomain directories
my @subdomainDirs = glob("$fulldomainDir/PE*");
#
my $initialized = 0;
#
# loop over fort.18 files in subdirectories
foreach my $dir (@subdomainDirs) { 
   #
   # get the subdomain number
   $dir =~ /PE(\d{4})/;
   my $subdomainNumber = sprintf("%d",$1);
   $subdomainNumber++; # 1 indexed, same as partmesh.txt
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
   my $ne_g = $fields[1];
   my $mnep = $fields[2];
   my $ne = $fields[3];
   # read/skip through the element mapping
   for (my $i=0; $i<$ne; $i++) {
      $line = <FORT18>; # throwaway
   }
   # read in the node mapping
   # NNODG (or NP_G) is the number of nodes in the full domain;
   # MNPP is the max number of nodes occurring in any domain; 
   # NNODP is the number of nodes on this subdomain (resident + ghost). 
   $line = <FORT18>;    #  NNODG   NP_G   MNPP   NNODP(I)
   my @fields = split(' ',$line);
   $np_g = $fields[1];
   my $mnpp = $fields[2];
   my $np = $fields[3];
   # 
   # on the first pass through the loop, initialize all tallies to 0
   if ($initialized == 0 ) {
      for (my $i=1; $i<=$np_g; $i++) {
         $residents[$i] = 0;
         $ghosts[$i] = 0;
         $ghostmem[$i] = 0;
         $subdomain[$i] = 0; 
      }
      $initialized = 1;
   }
   # on every pass through the loop, reset repeat tallies to zero
   for (my $i=1; $i<=$np_g; $i++) {
      $repeats[$i] = 0;
   }  
   #
   # read full domain node numbers 
   for (my $i=0; $i<$np; $i++) {
      $line = <FORT18>;    
      # record number of subdomains listing this node with a positive value
      if ( $line > 0 ) {
         $residents[$line]++;
         # record number of subdomains listing this node with a positive value
         $subdomain[$line] = $subdomainNumber;
      }
      # record number of subdomains listing this node with a negative value
      if ( $line < 0 ) {
         $ghosts[abs($line)]++;
         # record subdomain membership for this ghost node
         $ghostmem[abs($line)] = $subdomainNumber;
      }
      #  
      # record total number of subdomains with this node, either as resident or ghost
      my $absNodeNum = abs($line);
      $absolutes[$absNodeNum]++;
      # check for repeats within a particular subdomain
      $repeats[$absNodeNum]++;
      if ($repeats[$absNodeNum] > 1) {
         stderrMessage("WARNING","Node number $absNodeNum appeared $repeats[$absNodeNum] times in subdomain $dir.");
      }
   }
   #
   # close message passing info file
   close(FORT18);
}  
#
# open file for positives
unless (open(RESIDENTS,">residents.63")) {
   stderrMessage("ERROR","Failed to open the residents.63 file for writing: $!.");
   die;
} else {
   printf RESIDENTS "# " . $agrid . " ! residents.63 is the number of fort.18 files that have this full domain node number listed as a resident (positive value) ... should be just 1\n";
   printf RESIDENTS "1 $np_g -99999.0 -99 1\n";
   printf RESIDENTS "0.000000    0\n";    # faux time and timestep
}
#
# open file for negatives 
unless (open(GHOSTS,">ghosts.63")) {
   stderrMessage("ERROR","Failed to open the ghosts.63 file for writing: $!.");
   die;
} else {
   printf GHOSTS "# " . $agrid . " ghosts.63 is the number of fort.18 files that have this full domain node number listed as a ghost (negative value)\n";
   printf GHOSTS "1 $np_g -99999.0 -99 1\n";
   printf GHOSTS "0.000000    0\n";    # faux time and timestep
}
#
# open file for negatives 
unless (open(GHOSTMEM,">ghostmem.63")) {
   stderrMessage("ERROR","Failed to open the ghostmem.63 file for writing: $!.");
   die;
} else {
   printf GHOSTMEM "# " . $agrid . " ghostmem.63 is the subdomain in which this full domain node number is a ghost\n";
   printf GHOSTMEM "1 $np_g -99999.0 -99 1\n";
   printf GHOSTMEM "0.000000    0\n";    # faux time and timestep
}
#
# open file for absolutes 
unless (open(ABSOLUTES,">absolutes.63")) {
   stderrMessage("ERROR","Failed to open the absolutes.63 file for writing: $!.");
   die;
} else {
   printf ABSOLUTES "# " . $agrid . " absolutes.63 shows the number of subdomains in which this full domain node appears, either as a resident or a ghost\n";
   printf ABSOLUTES "1 $np_g -99999.0 -99 1\n";
   printf ABSOLUTES "0.000000    0\n";    # faux time and timestep
}

#
# open file for subdomain numbers
unless (open(SUBDOMAINS,">subdomains.63")) {
   stderrMessage("ERROR","Failed to open the subdomains.63 file for writing: $!.");
   die;
} else {
   printf SUBDOMAINS "# " . $agrid . " subdomains.63 shows the subdomain number where this fulldomain node number is a resident according to fort.18\n";
   printf SUBDOMAINS "1 $np_g -99999.0 -99 1\n";
   printf SUBDOMAINS "0.000000    0\n";    # faux time and timestep
}
#
# write positives and negatives to file
for (my $i=0; $i<$np_g; $i++) {
   my $fdNode = $i+1; # fort.18 is 1 indexed
   # write residents
   printf RESIDENTS "$fdNode  $residents[$fdNode]\n";
   # write ghosts
   printf GHOSTS "$fdNode  $ghosts[$fdNode]\n";
   # write ghosts
   printf GHOSTMEM "$fdNode  $ghostmem[$fdNode]\n";
   # write absolutes
   printf ABSOLUTES "$fdNode  $absolutes[$fdNode]\n";
   # write subdomain numbers
   printf SUBDOMAINS "$fdNode  $subdomain[$fdNode]\n";
}
close(RESIDENTS);
close(GHOSTS);
close(GHOSTMEM);
close(ABSOLUTES);
close(SUBDOMAINS);
#
# Open the partmesh.txt file, generated by metis, and classify the
# nodes according to which subdomain they were assigned in the partition.
unless (open(PARTMESH,"<$fulldomainDir/partmesh.txt")) {
   stderrMessage("ERROR","Failed to open the $fulldomainDir/partmesh.txt 18 file for reading: $!");
   die;
}
# open the "subdomains according to partmesh" file 
unless (open(PSUBDOMAINS,">psubdomains.63")) {
   stderrMessage("ERROR","Failed to open the psubdomains.63 file for writing: $!.");
   die;
}
#
printf PSUBDOMAINS "# " . $agrid . " psubdomains.63 shows the subdomain number where this fulldomain node number is a resident according to partmesh.txt\n";
printf PSUBDOMAINS "1 $np_g -99999.0 -99 1\n";
printf PSUBDOMAINS "0.000000    0\n";    # faux time and timestep
#
my $i = 1; 
while(<PARTMESH>) {
   chomp;
   $psubdomain[$i] = $_;
   printf PSUBDOMAINS "$i  $psubdomain[$i]\n";
   $i++;
}
close(PARTMESH);
close(PSUBDOMAINS);
#
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

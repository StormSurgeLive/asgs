#!/usr/bin/env perl
#----------------------------------------------------------------
# particle2xdmf.pl
#
# Converts ascii output from the ADCIRC particle tracking program
# to XDMF xml format. This is useful for visualization in Paraview.
#
#----------------------------------------------------------------
# Copyright(C) 2013 Jason Fleming
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
#----------------------------------------------------------------
use strict;
use warnings;
use Getopt::Long;
use Math::Trig;
#
sub stderrMessage($$);
#
my $pathfile = "null"; # file with particle positions
my $velfile = "null";  # file with particle velocities
my $maureparticle = "null"; # output file from maureparticle program 
my $xdmffile = "particles.xmf"; # xdmf file where particle positions and/or velocities
                                 # are written
my $cpp; # use this command line option to reproject to CPP coordinates
my $slam0 = 0.0; # reference longitude for the cpp projection (degrees, -180 to 180)
my $sfea0 = 0.0; # reference latitude for the cpp projection (degrees, -80 to 80)
my $R = 6378206.4; # radius of the earth (meters)
my $pi = 3.141592653589793;
my $deg2rad = 2.0*$pi/360.0;
my @time; # array of times (in seconds) corresponding to each dataset
my @npartDataset; # array containing the number of particles in each dataset
#
GetOptions(
           "cpp" => \$cpp,
           "slam0" => \$slam0,
           "sfea0" => \$sfea0,
           "xdmffile=s" => \$xdmffile,
           "pathfile=s" => \$pathfile,
           "maureparticle=s" => \$maureparticle,
           "velfile=s" => \$velfile
           );
# 
# check to make sure that we have a file to work with
if ( $pathfile eq "null" && $maureparticle eq "null" ) {
   stderrMessage("ERROR","You must specify either a path or a maureparticle output file."); 
   die;
}
if ( $pathfile ne "null" && ! -e $pathfile ) {
   stderrMessage("ERROR","The path file $pathfile was not found."); 
   die;
}
if ( $maureparticle ne "null" && ! -e $maureparticle ) {
   stderrMessage("ERROR","The path file $pathfile was not found."); 
   die;
}
unless(open(XDMFFILE,">$xdmffile")) {
   stderrMessage("ERROR","Could not open $xdmffile for writing: $!");
   die;
} else {
   # write the header info at the top of the file
   printf XDMFFILE "<?xml version=\"1.0\" ?>\n"; 
   printf XDMFFILE "<Xdmf Version='2.1'>\n";
   printf XDMFFILE "   <Domain>\n";
   printf XDMFFILE "      <Grid Name=\"TimeSeries\" GridType=\"Collection\" CollectionType=\"Temporal\">\n";
}
my $nd; # number of datasets in the file
my $npart; # number of particles in the dataset
if ( $pathfile ne "null" ) {
   unless(open(PATHFILE,"<$pathfile")) {
      stderrMessage("ERROR","Could not open pathfile $pathfile for reading: $!");
      die;
   } else {
      # parse the header
      my $line = <PATHFILE>;
      stderrMessage("INFO","Comment line from path file: $line");
      $line = <PATHFILE>;
      my @fields = split(" ",$line);
      $nd = $fields[0];
      stderrMessage("INFO","There are $nd datasets in the path file.");
      $npart = $fields[1];
      stderrMessage("INFO","There are $npart particles in the path file.");
   }
} else {
   unless(open(MAUREPT,"<$maureparticle")) {
      stderrMessage("ERROR","Could not open maureparticle file $maureparticle for reading: $!");
      die;
   } else {
      # read the file to determine the number of datasets and the number of particles
      $nd = 1;
      $npartDataset[$nd-1] = 1;  # number of particles in each dataset
      my $line = <MAUREPT>;
      my @fields = split(" ",$line);
      # get the time associated with the first particle in the first data set
      $time[$nd-1] = $fields[3]; # time associated with each dataset
      while(<MAUREPT>) {
         @fields = split(" ",$_); 
         if ($fields[3] != $time[$nd-1]) {
            $nd++;
            $npartDataset[$nd-1] = 1;  # number of particles in each dataset
            $time[$nd-1] = $fields[3];
            my $oldDatasetNum = $nd-2;
            stderrMessage("INFO","There are $npartDataset[$nd-2] particles in dataset $oldDatasetNum.");
         } else {
            $npartDataset[$nd-1]++;
         }
      }
      close(MAUREPT);
      stderrMessage("INFO","There are $nd datasets in the path file.");
   }
}
if ($velfile ne "null") {
   unless(open(VELFILE,"<$velfile")) {
      stderrMessage("ERROR","Could not open $velfile for reading: $!");
      die;
   } else {
      # parse the header
      my $line = <VELFILE>;
      stderrMessage("INFO","Comment line from the particle velocities file: $line");
      $line = <VELFILE>;
      my @fields = split(" ",$line);
      if ($fields[0] != $nd) {
         stderrMessage("ERROR","There are $fields[0] datasets in the particle velocities file but there are $nd datasets in the particle paths file. These files must match.");
         die;
      }
      stderrMessage("INFO","There are $fields[0] datasets in the particle velocities file.");
      if ($fields[1] != $npart) {
         stderrMessage("ERROR","There are $fields[1] particles in the particle velocities file but there are $npart particles in the particle paths file. These files must match.");
         die;
      }
      stderrMessage("INFO","There are $fields[1] particles in the particle velocities file.");
   }
} 
if ( $maureparticle ne "null") { 
   # reopen the maureparticle file
   unless(open(MAUREPT,"<$maureparticle")) {
      stderrMessage("ERROR","Could not open maureparticle file $maureparticle for reading: $!");
      die;
   }
}
# read particle locations and write to xmf
for (my $i=0; $i<$nd; $i++ ) {  
   my $n = $i+1; # dataset label for following log message
   stderrMessage("INFO","Converting dataset $n of $nd to XDMF xml.");
   if ($pathfile ne "null") {
      # read and parse dataset header
      my @fields = split(" ",<PATHFILE>);
      $time[$i] = $fields[0];         # in seconds
      $npartDataset[$i] = $fields[1];   # number of particles in this dataset
   }
   # write the header for the dataset containing particle positions to the XDMF xml file
   printf XDMFFILE "               <Grid Name=\"Time=$time[$i]\"  GridType='Uniform'>\n";
   printf XDMFFILE "                  <Time Value=\"$time[$i]\"/>\n";
   printf XDMFFILE "                  <Topology TopologyType='POLYVERTEX' NumberOfElements='$npartDataset[$i]' NodesPerElement='1'/>\n";
   printf XDMFFILE "                  <Geometry GeometryType='XYZ'>\n";
   printf XDMFFILE "                     <DataItem ItemType='Uniform' Dimensions='$npartDataset[$i] 3' Format='XML'>\n";
   # read particle positions and write them to the XDMF xml file
   for (my $j=0; $j<$npartDataset[$i]; $j++ ) {
      my @coords;
      if ($pathfile ne "null") {
         @coords = split(" ",<PATHFILE>);
      } else {
         @coords = split(" ",<MAUREPT>);
         $coords[3] = 0.0  # hard code z value to zero
      }
      if (defined $cpp) {
         $coords[1] = $R * ($coords[1]*$deg2rad - $slam0*$deg2rad) * cos($sfea0*$deg2rad);
         $coords[2] = $coords[2]*$deg2rad * $R;
      }
      printf XDMFFILE "$coords[1] $coords[2] $coords[3]\n"; 
   }
   printf XDMFFILE "         </DataItem>\n";
   printf XDMFFILE "      </Geometry>\n";
   if ($velfile ne "null") {
      my @speeds; # array of velocity magnitudes (particle speeds)     
      my @fields = split(" ",<VELFILE>); # read dataset header
      if ( $fields[0] != $time[$i] ) {         # in seconds
         stderrMessage("ERROR","The time stamp for this dataset in the particle velocities file is $fields[0] but the corresponding time stamp in the particle positions file is $time[$i]. These timestamps must match.");
         die;
      }
      if ( $fields[1] != $npartDataset[$i] ) {          
         stderrMessage("ERROR","The number of particles for this dataset in the particle velocities file is $fields[1] but the corresponding number of particles in the particle positions file is $npartDataset[$i]. The number of particles in each file must match for each dataset.");
         die;
      }
      printf XDMFFILE "                <Attribute AttributeType=\"Vector\" Center=\"Node\" Name=\"particle_velocity\">\n";
      printf XDMFFILE "                   <DataItem DataType=\"Float\" Dimensions=\"$npartDataset[$i] 3\" Format=\"XML\">\n";
      for (my $j=0; $j<$npartDataset[$i]; $j++ ) {
         my @vels = split(" ",<VELFILE>);
         printf XDMFFILE "$vels[1] $vels[2] 0.0\n"; 
         $speeds[$j] = sqrt($vels[1]**2 + $vels[2]**2); 
      }      
      printf XDMFFILE "               </DataItem>\n";
      printf XDMFFILE "            </Attribute>\n";
      printf XDMFFILE "            <Attribute AttributeType=\"Scalar\" Center=\"Node\" Name=\"particle_speed\">\n";
      printf XDMFFILE "                    <DataItem DataType=\"Float\" Dimensions=\"$npartDataset[$i] 1\" Format=\"XML\">\n";
      for (my $j=0; $j<$npartDataset[$i]; $j++ ) {
         printf XDMFFILE "$speeds[$j]\n"; 
      }            
      printf XDMFFILE "            </DataItem>\n";
      printf XDMFFILE "         </Attribute>\n";
   }
   printf XDMFFILE "        </Grid>\n";
}
close(PATHFILE);
if ($velfile ne "null") {
   close(VELFILE);
}
printf XDMFFILE "     </Grid>\n";
printf XDMFFILE "  </Domain>\n";
printf XDMFFILE "</Xdmf>\n";
close(XDMFFILE);

sub stderrMessage ($$) {
   my $level = shift;
   my $message = shift;
   printf STDERR "$level: particle2xdmf.pl: $message\n";
}

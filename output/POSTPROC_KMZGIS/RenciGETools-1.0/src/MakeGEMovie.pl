#! /usr/bin/perl -w
# 
# Copyright (c) 2008  Renaissance Computing Institute. All rights reserved.
# 
# This software is open source. See the bottom of this file for the license.
# 
# Renaissance Computing Institute, 
# (A Joint Institute between the University of North Carolina at Chapel Hill,
# North Carolina State University, and Duke University)
# http://www.renci.org
# 
# For questions, comments please contact software@renci.org
# 
# 
#
#
#
use strict;
use warnings;
use FileHandle;
use Date::Format;
use Time::Local;
use Getopt::Long;

my ($dataFile,$grid,$prefix,$nLevels,$startDate);
my ($firstStep,$nSteps,$stepInterval,$outputDir);
my $thisFileExt;
my $thisFileName;
my $fort63Mode;
my $colorRange;

# define defaults
$dataFile="fort.63";
$outputDir="./";
$fort63Mode="compact";
$colorRange="none";
my $units="M";
$grid="default";

if ($#ARGV < 0) {
   &Usage;
   exit 0;
}

# override defaults with commandline options
GetOptions ('dataFile=s'     => \$dataFile, 
            'fort63Mode=s'   => \$fort63Mode,
            'grid=s'         => \$grid,
            'prefix=s'       => \$prefix,
            'nLevels=s'      => \$nLevels,
            'startDate=s'    => \$startDate,
            'firstStep=s'    => \$firstStep,
            'nSteps=s'       => \$nSteps,
            'outputDir=s'    => \$outputDir,
            'colorRange=s'   => \$colorRange,
            'units=s'        => \$units,
            'stepInterval=s' => \$stepInterval);


if (!-e $dataFile){
    print "$dataFile not found.\n" ;
    exit 1;
}

# Make sure they have the PPDIR variable set.
my $PPDIR = $ENV{'PPDIR'};
if (!defined($PPDIR) or !$PPDIR) {
   print "$PPDIR environment variable not set\n";
   exit 1;
}

# Convert the time the user gave us into seconds.
#date format is yyyymmddhhmmss
my ($sec,$min,$hour,$mday,$mon,$year);

$year = substr $startDate,0,4;
$mon = substr $startDate,4,2;

# months start at 0...
$mon --;
$mday = substr $startDate,6,2;
$hour = substr $startDate,8,2;
$min = substr $startDate,10,2;
$sec = substr $startDate,12,2;
my $userTime = timelocal($sec,$min,$hour,$mday,$mon,$year);
print "$year $mon $mday $hour $min $sec $userTime\n";
my $nRecords;
my $line;

if (($fort63Mode eq "netCDFScoop") && ($grid eq "default")){
   # No grid file specified: use the data file.
   $grid = $dataFile;
}

if (($fort63Mode eq "full") ||  ($fort63Mode eq "compact")) {
   # We need to know how many time steps are in the file.
   $line = `head -2 $dataFile | sed '1d'`;
   my @lineArray = split  " ", $line;
   $nRecords = $lineArray[0];

   if ($nSteps > $nRecords) {
     print "nSteps ($nSteps) > num steps in the $dataFile ($nRecords)\n";
     exit 1;
   }
} else {
   $nRecords = $nSteps;
}

if (-e "$prefix.kmz") {
   print "removing existing $prefix.kmz\n";
   my $rmarg = "$prefix.kmz";
   my $rmresult = `rm -f $rmarg`;
}

if ($nSteps > 1) {
  print "Creating top level KML file $prefix.kml\n";
  open FILE, ">$prefix.kml" or die $!;
  print FILE "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  print FILE "<kml xmlns=\"http://earth.google.com/kml/2.1\">\n";
  print FILE "<Document>\n";

  # Create the kmz file with this as the first file. We will add it to
  # the kmz again at end.  This ensures it's the first file that GE sees.
  my $zresult = `zip $prefix.kmz $prefix.kml`;
}

# If there is more than one time step in the file, we need the interval.
my $timeStep = 0;
if (($fort63Mode eq "full") ||  ($fort63Mode eq "compact")) {
   if ($nRecords > 1) {
      # The fourth field in the second line has the time step interval in seconds
      $line = `head -2 $dataFile | sed '1d'`;
      my @lineArray = split  " ", $line;
      $timeStep = $lineArray[3]; 
      print "timestep: $timeStep\n";
   }
} else {
   # The netCDFScoop case: Those files are 1 hour time steps.
   $timeStep = 3600; 
}

my $args;
my $colorRangeArg = "";
# call adc_max_simple_plot_gmt.sh once to create the colormap.
if ($colorRange ne "none") {
   # The user has provided a color range.
   $colorRangeArg = "-c $colorRange";
} 
$args = "-f $dataFile -g $grid -p $prefix-colormap -n 1 -d 0 -o colormap -m $fort63Mode $colorRangeArg -u $units";

print "Calling adc_max_simple_plot_gmt.sh with $args\n";
my $result = `$PPDIR/adc_max_simple_plot_gmt.sh $args`;
 
if (($fort63Mode eq "full") ||  ($fort63Mode eq "compact")) {
   # let's unpack the my fort.63 file into individual timesteps...
   # Note: if there is only one time stamp in the file, the file is just
   # copied to the new name.
   print "calling $PPDIR/splitFort63 $dataFile $prefix $fort63Mode\n";
   $result = `$PPDIR/splitFort63 $dataFile $prefix $fort63Mode`;

   # The result will be a set of single time slice fort.63 format files. The
   # file names for these files will be $prefix.xxxx where xxxx runs from
   # 0001 to the number of timesteps in the original file.
}
for (my $i = 0; $i < $nSteps; $i++) {
   my $timeForGE;
   my $timeEndForGE;
   my $timeEndForGEInSeconds;
   my $thisStep;
   # For the number of steps the user requested.
   # Calculate the file number.
   $thisFileExt = sprintf("%04s", $firstStep + ($i * $stepInterval));

   if (($fort63Mode eq "full") ||  ($fort63Mode eq "compact")) {
      # The file name is the prefix + "." + the fileExt.
      $thisFileName = $prefix . "." . $thisFileExt;
   }  else {
      # a netCDF file: we pass the original data file and the timeStep.
      $thisFileName = $dataFile;
      $thisStep = $firstStep + ($i * $stepInterval);
   }
   # We also need the time of the file.
   if ($nRecords == 1) {
      # The time the user gave us is the time of the record.
      $timeForGE = time2str("%Y-%m-%dT%H:%M:%S",$userTime);
      $timeEndForGEInSeconds = $userTime + $timeStep;
   } else {
      # The time the user gave us is the time of the beginning of 
      # the simulation. The first set of data in the fortfile is 
      # at $userTime + $timeStep.
      my $thisStepSeconds = $userTime + 
        (($firstStep + ($i * $stepInterval)) * $timeStep);
      $timeForGE = time2str("%Y-%m-%dT%H:%M:%S",$thisStepSeconds);
      $timeEndForGEInSeconds = $thisStepSeconds + $timeStep;
   }
   
   # Calculate the char version of the end of the time step
   $timeEndForGE = time2str("%Y-%m-%dT%H:%M:%S",$timeEndForGEInSeconds);

   # Call the GE generator.
   my $args;
   if (($fort63Mode eq "full") ||  ($fort63Mode eq "compact")) {
      if ($nRecords > 1) {
         # the fort63 file we are passing was produced with splitFort.
         # splitFort always produces a "full" file
         $fort63Mode = "full";
      }
      $args = "-f $thisFileName -g $grid -p $prefix.$thisFileExt -n $nLevels -d $timeForGE -e $timeEndForGE -o kmz -k $prefix -m $fort63Mode $colorRangeArg -u $units";
   } else {
      $args = "-f $thisFileName -g $thisFileName -p $prefix.$thisFileExt -n $nLevels -d $timeForGE -e $timeEndForGE -o kmz -k $prefix -m $fort63Mode -t $thisStep $colorRangeArg -u $units";
   }
   print "Calling adc_max_simple_plot_gmt.sh with $args\n";
   $result = `$PPDIR/adc_max_simple_plot_gmt.sh $args`;
   print "$result\n";

   # add to doc.kmz
   if ($nSteps > 1) {
      print FILE "<NetworkLink>\n";
      print FILE "\t<name>$prefix.$thisFileExt</name>\n";
      print FILE "\t<Link>\n";
      # Point to the top level kml for each time step.
      print FILE "\t\t<href>$prefix.$thisFileExt.1.1.1.kml</href>\n";
      print FILE "\t</Link>\n";
      print FILE "</NetworkLink>\n";
   }
}

# End the master kml document
if ($nSteps > 1) {
   print FILE "</Document>\n";
   print FILE "</kml>";
   close FILE;
}

# Now make the properties file.
open PROPFILE, ">$prefix.properties" or die $!;
print PROPFILE "nLevels $nLevels\n";
print PROPFILE "nSteps  $nSteps\n";
print PROPFILE "timeStep  $timeStep\n";
print PROPFILE "colorTable  zeta.cpt\n";
close PROPFILE;


# update the master kml
if ($nSteps > 1) {
   $result = `zip $prefix.kmz $prefix.kml`;
}

# Make a tar file with the montage and world files.
$result = `tar cvfz $prefix.tar.gz $prefix.*montage* $prefix.*.world zeta.cpt`;

# Cleanup time...
my $cleanupList = "$prefix.???? $prefix.kml $prefix.*montage* $prefix.*.world zeta.cpt";
$result = `rm -f $cleanupList`;
if ($outputDir ne "./") {
   $result = `mkdir -p $outputDir`;
   $result = `mv $prefix.kmz $prefix.tar.gz $prefix.properties $outputDir`;
}

 
sub Usage{

my $str = <<END;

Usage: 

MakeGEMovie.pl  --dataFile=fort.63 --grid=gridName --prefix=prefix 
                --nLevels=nLevels --startDate=startDate --firstStep=firstStep 
                --nSteps=nSteps --stepInterval=stepInterval [--outputDir=dir]
                [--fort63Mode=mode] [--colorRange=colorRange] [--units=units]

This function reads an ADCIRC fort.63 formatted file and converts it into 
a GoogleEarth KMZ file.  It does this using a combination of Fortran, C and sh 
script codes. Each timestep of the fort.63 file is visualized individually.
Because we are using GMT to produce the plots, you have to have the GMT
versions of the grid on which the fort.63 files were produced. See the file
TiledREADME.pdf in this directory for details on how to make these.

The arguments are

    --dataFile:  The name of the fort.63 file to visualize.
    --fort63Mode: Supported modes are:

        full: The data values are contained in the fort.63 formatted 
              file specified by the --dataFile parameter.  All nodes are
              represented in the file. The nodes composing the grid 
              are contained in the file specified by the --gridFile parameter

        compact: The data values are contained in the fort.63 formatted
                 file specified by the --dataFile parameter.  Nodes without 
                 valid data are not represented in the file. The nodes composing
                 the grid are contained in the file specified by the --gridFile 
                 parameter.

        netCDFScoop: the grid and value data are in a netCDF file that 
                     conforms to that used in the ADCIRC implementation of 
                     the SURA Coastal Ocean Observing and Prediction (SCOOP) 
                     Program.
    --grid: the name of the gmt versions of the relevant ADCIRC grid.
    --prefix: the prefix for the resulting KMZ file.
    --nLevels: the number of levels of plots in the KMZ files. See 
               TiledREADME.pdf for more details.
    --startDate: the starting date of the simulation being visualized. This
                 is in yyyymmddhhmmss format.
    --firstStep: the first timestep of the simulation to be visualized.
    --nSteps: the number of timesteps of the simulation to be visualized.
    --stepInterval: the intervals of the steps to be visualized: 1 means
                    visualize every step, 2 means every other step ...
    --outputDir: the directory in which to deposit the artifacts.
    --colorRange: The colon seperated min max values to use for the colormap. 
                  For example --colorRange 0:2 will produce a colormap 
                  between 0 and 2 units.
    --units: The units for the display.  Must be either M for meters or
             FT for feet. Default is M.

Note that this call depends on the value of the environment var PPDIR.
Set it to the dir that contains the executables...
END

print $str ."\n";

}

# ***************************************************************************
# 
# RENCI Open Source Software License
# The University of North Carolina at Chapel Hill
# 
# The University of North Carolina at Chapel Hill (the "Licensor") through 
# its Renaissance Computing Institute (RENCI) is making an original work of 
# authorship (the "Software") available through RENCI upon the terms set 
# forth in this Open Source Software License (this "License").  This License 
# applies to any Software that has placed the following notice immediately 
# following the copyright notice for the Software:  Licensed under the RENCI 
# Open Source Software License v. 1.0.
# 
# Licensor grants You, free of charge, a world-wide, royalty-free, 
# non-exclusive, perpetual, sublicenseable license to do the following to 
# deal in the Software without restriction, including without limitation the 
# rights to use, copy, modify, merge, publish, distribute, sublicense, 
# and/or sell copies of the Software, and to permit persons to whom the 
# Software is furnished to do so, subject to the following conditions:
# 
# . Redistributions of source code must retain the above copyright notice, 
# this list of conditions and the following disclaimers.
# 
# . Redistributions in binary form must reproduce the above copyright 
# notice, this list of conditions and the following disclaimers in the 
# documentation and/or other materials provided with the distribution.
# 
# . Neither You nor any sublicensor of the Software may use the names of 
# Licensor (or any derivative thereof), of RENCI, or of contributors to the 
# Software without explicit prior written permission.  Nothing in this 
# License shall be deemed to grant any rights to trademarks, copyrights, 
# patents, trade secrets or any other intellectual property of Licensor 
# except as expressly stated herein.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
# THE CONTIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
# OTHER DEALINGS IN THE SOFTWARE.
# 
# You may use the Software in all ways not otherwise restricted or 
# conditioned by this License or by law, and Licensor promises not to 
# interfere with or be responsible for such uses by You.  This Software may 
# be subject to U.S. law dealing with export controls.  If you are in the 
# U.S., please do not mirror this Software unless you fully understand the 
# U.S. export regulations.  Licensees in other countries may face similar 
# restrictions.  In all cases, it is licensee's responsibility to comply 
# with any export regulations applicable in licensee's jurisdiction.
# 
# ***************************************************************************# 

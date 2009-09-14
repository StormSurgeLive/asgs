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
#./makeWorldFile.pl  
#                 
#                
#
use strict;
use warnings;
use FileHandle;
use Date::Format;
use Time::Local;
use Getopt::Long;

my ($image,$world,$east,$west,$north,$south);
my ($firstStep,$nSteps,$stepInterval);
my $thisFileExt;
my $thisFileName;
my $imageMagickPath; # full path to ImageMagick executables, up through /bin

# May need to specify the full path.
my $IDENTIFY = "identify";

if ($#ARGV < 0) {
   &Usage;
   exit 0;
}

# override defaults with commandline options
GetOptions ('image=s'        => \$image, 
            'world=s'        => \$world,
            'east=s'         => \$east,
            'west=s'         => \$west,
            'north=s'        => \$north,
            'south=s'        => \$south,
            'imagemagickpath=s' => \$imageMagickPath);
#
# prepend the full path to image magick
if (defined $imageMagickPath) { 
   $IDENTIFY=$imageMagickPath . "/" . $IDENTIFY
}
# get the x and y size in pixels of the image.
my $fileSize =`$IDENTIFY $image |  awk '{print \$3}'`;
#my $fileSize =`identify $image |  awk '{print \$3}'`;
print "fileSize: $fileSize\n";
my @fileSizeArray = split "x",$fileSize;

my $xSize = $fileSizeArray[0];
my $ySize = $fileSizeArray[1];

my $xScale = abs(($west - $east) / $xSize);
my $yScale = -(($north - $south)/$ySize);
my $rotation = 0.0;


# Make the world file: One description of the format is at
# http://gis.enr.state.nc.us/sid/bin/zhelpworld_nofrm.htm

open FILE, ">$world" or die $!;
print FILE "$xScale\n";
print FILE "$rotation\n";
print FILE "$rotation\n";
print FILE "$yScale\n";
print FILE "$west\n";
print FILE "$north";

sub Usage{

my $str = <<END;

Usage: 

makeWorldFile.pl  --image=imageFile --world=worldFile --east=east 
               --west=west --north=north --south=south 

This function reads a graphics file and creates a "world" file suitable for
ditributing the graphics file in a mapserver or similar environment.

The arguments are

    --image:  The name of the image file for which to create the worldfile.
    --world:  The world file to produce.
    --east:   The eastern extent of the image file.
    --west:   The western extent of the image file.
    --north:  The northern extent of the image file.
    --south:  The southern extent of the image file.

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

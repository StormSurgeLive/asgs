#!/usr/bin/env perl
#######################################################################
# tpxoBoundaryInterp.pl
#
# usage:
#
# perl tpxoBoundaryInterp.pl --gridfile /path/to/fort.14 --tpxofile /path/to/h_tpxo7.2 --constituents m2,s2,n2,k2,k1,o1,p1,q1 --outfile tidal_boundary_data.txt
#
# ...or just run the script and you will be prompted for input. 
#
# This script reads the open boundary information from an ADCIRC style
# fort.14 file, extracts tidal consituent amplitude and phase data from 
# the Oregon State University TPXO 7.2 model (Fortran binary "h" file), 
# and writes it out in the proper format for insertion into a fort.15 
# file.
#
# It requires that you have the binary (not netCDF) OSU TOPEX/Poseidon 
# Global Inverse Solution for tidal heights, which can be downloaded  
# from:  ftp://ftp.oce.orst.edu/dist/tides/Global/tpxo.tar.Z 
# The constituents must be given as a comma separated list (see example) 
# 
# TPXO 7.2 has the following 13 constituents available.
#   m2,s2,n2,k2,k1,o1,p1,q1,mf,mm,m4,ms4,mn4
#
# TPXO model binary The file format was deduced from the OSU tidal
# prediction software extract_HC.f by Gary Egbert and Lana Erofeeva 
# at Oregon State University, which is avaialable here:
# ftp://ftp.oce.orst.edu/dist/tides/OTPS.tar.Z, and from the file 
# itself.  
#
####################################################################### 
# Author: Nate Dill, natedill@gmail.com
#
# Copyright (C) 2015 Nathan Dill
# 
# This program  is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of the 
# License, or (at your option) any later version. 
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software 
# Foundation, Inc., 59 Temple Place - Suite 330,Boston, MA  02111-1307,
# USA.
#                                       
#######################################################################
#
use strict;
use warnings;

use AdcGrid;                     # that contains the AdcGrid.pm module   !!!!
use Getopt::Long;


my $pi=3.14159265359;

#######################################################################
# process the command line options
#
my $gridFile;
my $tpxoHfile;
my $consts;
my @CONSTS;
my $outFile;


GetOptions   ( "gridfile=s" => \$gridFile,
               "tpxofile=s" => \$tpxoHfile,
	       "constituents=s" => \$consts,
	       "outfile=s" => \$outFile);

unless ($gridFile) {
     print "enter the name of the ADCIRC grid file:";
     $gridFile=<>;
	chomp($gridFile);
	$gridFile =~ s/^\s+//;
	$gridFile =~ s/\s+$//;
     print "\n";
}
unless ($tpxoHfile) {
     print "enter the name of the TPXO h binary file (e.g. h_tpxo7.2):";
     $tpxoHfile=<>;
	chomp($tpxoHfile);
	$tpxoHfile =~ s/^\s+//;
	$tpxoHfile =~ s/\s+$//;
     print "\n";
}
unless ($consts) {
	print "enter a comma separated list of 2 character constituent names:";
	$consts=<>;
	chomp($consts);
	$consts =~ s/\s+//;
	print "\n";
       
}
unless ($outFile) {
	print "enter the name for an output file:";
        $outFile=<>;
	chomp($outFile);
	$outFile =~ s/^\s+//;
	$outFile =~ s/\s+$//;
        print "\n";
}

 @CONSTS=split(/,/,$consts);


print " using adcirc grid $gridFile\n";
print " using tpxofile $tpxoHfile\n";
print " getting constituents $consts\n";
print " writing to $outFile\n";
#######################################################################
#
# get the necessary ADCIRC grid data, i.e. a list of the lon,lat for the 
# open boundary nodes 
#
#######################################################################

# load the grid as an AdcGrid object
my $adcGrid=AdcGrid->new($gridFile);
print "done loading grid\n";

# how many open boundaries are there?
my $nope=$adcGrid->getNOPE();
print "nope is $nope\n";

my @X;
my @Y;

# loop over the open boundaries
for my $ibnd (1..$nope) {
   # get the nodes these boundary nodes
   my ($nvdll,$ibtypee,$nodesref)=$adcGrid->getOpenBnd($ibnd);
   my @NIDS=@{$nodesref};

   # loop over the nodes in this boundary and accumulate list of x,y coords.
   foreach my $nid (@NIDS) {
       # get the xyz for this node
       my ($x,$y,$z)=$adcGrid->getNode($nid);
       push (@X,$x);
       push (@Y,$y);
   }
}



####################################################################
# read in the txpo data 
#
#
# h model file from the TPXO 7.2 model
# files are big-endian...
# with Fortran length indicators and 4 byte record size
####################################################################

# declare some variables
my $n;  # number of Longitude cells
my $m;  # number of Latitude cells
my $nc; # number of constituents
my $lon0; # origin of grid 
my $lat0; #
my $lon1; # origin of grid 
my $lat1; #
my $constNamesStr; # 4 char constituent names (13 constituents)
my @constNames;

my $h;  # complex amplitude e.g. h(t,x) = Re [ h(x) exp { i [w (t - t0) + V0(t0)] } ]
        # amplitude is sqrt( Im(h)**2 + Re(h) ),  
        # and phase is atan(-imaginary(h)/real(h));

my $buf;
my $recSize; # we're guessing its 4 bytes 
my $recLength; # total number of bytes in record
my $addr=0;  # keep track of where we are at in the file

# open and read the tpxo grid file 
open TPXOFILE, "<$tpxoHfile" or die "can't open $tpxoHfile\n";
binmode TPXOFILE;  

##############################################################################
# read the header record (n,m,nc,lat0,lon0)
# get the record length from the fortran record length indicator (i.e first 4-bytes)
$recSize=4;
$recLength=1*$recSize;
$addr=$addr+$recLength;
sysread(TPXOFILE,$buf,$recLength);
$recLength=unpack("l>",$buf);
print "the header is $recLength bytes long\n";

# read the record
$addr=$addr+$recLength;
sysread(TPXOFILE,$buf,$recLength);
($n,$m,$nc,$lat0,$lat1,$lon0,$lon1,$constNamesStr)=unpack("l>3 f>4 A*",$buf);
print "n,m,nc,latRng,lonRng: $n $m $nc $lat0 $lat1 $lon0 $lon1\n";
print "constNames: $constNamesStr\n";
$constNamesStr =~ s/^\s+//;
$constNamesStr =~ s/\s$//;
@constNames=split(/\s+/,$constNamesStr);

my $dx=($lon1-$lon0)/$n;
my $dy=($lat1-$lat0)/$m;


# tail of record
$addr=$addr+$recSize;
sysread(TPXOFILE,$buf,$recSize);
$recLength=unpack("l>",$buf);
print "the header was $recLength bytes long\n";
 

my %TPXO;  # a hash which will hold all the tpxo data


################################################
# read the constituent data
my $ic=0;
foreach my $const (@constNames) { 
   #print "reading $const\n";
	
   $TPXO{$const}={};  # create anonymous hash for this constitient
   $TPXO{$const}->{REAL}=[]; #create anonymous array within this anynymous hash
   $TPXO{$const}->{IMG}=[]; #create anonymous array within this anynymous hash

   # next record 
   # beginning record length indicator
   $addr=$addr+$recSize;
   sysread(TPXOFILE,$buf,$recSize);
   $recLength=unpack("l>",$buf);
   print "the $const is $recLength bytes long\n";

   # record data
   $addr=$addr+$recLength;
   sysread(TPXOFILE,$buf,$recLength);
   # process the buffer here
   # for now we'll just store the data b/c its not that impractical
   # however now that we know the format 
   # and with some clever index calcs and sysseek and sysread calls
   # we could probably more efficiently just pull out the data we need 
   # for each point we want to interpolate at
   # this whole loop (over the constituents) is really unnecessary
   my @data=unpack("f>*",$buf);
   my $rr=scalar(@data);
   foreach  my $j (0..$m-1){
      foreach my  $i (0..$n-1) {
         my $rl=shift(@data);
         my $im=shift(@data);
         $TPXO{$const}->{REAL}->[$i][$j] = $rl;
         $TPXO{$const}->{IMG}->[$i][$j] = $im;
      }
   }


   # tail  record length indicator
   $addr=$addr+$recSize;
   sysread(TPXOFILE,$buf,$recSize);
   $recLength=unpack("l>",$buf);
   print "just read $recLength bytes\n";

   $ic++;
}

close(TPXOFILE);
# done reading data
#################################################################


# write out the data just for fun
# close (TPXOFILE);
# foreach my $ic (0..12) {
#   open FILE, ">$constNames[$ic].xyz";
#   foreach my $j (0..$m-1) {
#      my $lt=$lat0+$dy*$j;
#       foreach my $i (0..$n-1) {
#            my $ln=$lon0+$dx*$i;
#            my $rdata_ll=$realData[$ic][$i][$j];
#            my $idata_ll=$imgData[$ic][$i][$j];
#            my $a=($rdata_ll**2 + $idata_ll**2)**0.5;
#            my $p=atan2(-1*$idata_ll,$rdata_ll);
#            print FILE "$ln $lt  $a $p\n";
#        }
#   }
#   close(FILE);
# }


# foreach my $ic (0..12) {
#      my $cst=$constNames[$ic];
#      print"cst $cst\n";
#   foreach my $j (0..$m-1) {
#      my $lt=$lat0+$dy*$j;
#       foreach my $i (0..$n-1) {
#            my $ln=$lon0+$dx*$i;
#            my $rdata_ll=$realData[$ic][$i][$j];
#            my $idata_ll=$imgData[$ic][$i][$j];
#            my $r2=$TPXO{$cst}->{REAL}->[$i][$j];
#            my $i2=$TPXO{$cst}->{IMG}->[$i][$j];
#            print "1: $rdata_ll $idata_ll\n";
#	    print "2: $r2 $i2\n";
#        }
#   }
# }



#######################################################################
# 
# now loop over the requested constituents and interpolate the amplitude
# and phase and write out the data in the proper format for insertion into 
# the fort.15 
#
#######################################################################

# periods from wikipedia
#
#   M2  12.4206012
#   S2	12
#   N2	12.65834751
#   K2	11.96723606
#   K1	23.93447213
#   O1	25.81933871
#   P1	24.06588766
#   Q1	26.868350
#   Mf	327.8599387	
#   Mm	661.3111655	
#   M4	6.210300601
#   MS4	6.103339275
#   MN4	6.269173724
#
# note ADCIRC uses 2*pi * (frequency in hertz)

my %FREQS = ( 'm2' => 0.0001405189027,
              's2' => 0.0001454441043,	
              'n2' => 0.0001378797074,	
              'k2' => 0.0001458423017,	
              'k1' => 0.0000729211508,	
              'o1' => 0.0000675977518,	
              'p1' => 0.0000725229535,
              'q1' => 0.0000649585572,
              'mf' => 0.000005323398945,	
              'mm' => 0.000002639195197,	
              'm4' => 0.000281037805434618,	
              'ms4' => 0.00028596300702853,	
              'mn4' => 0.00027839861022079);	




# open the output fole

open OUT, ">$outFile" or die "cannot open $outFile\n";

# write number of constituents
my $numConsts=scalar(@CONSTS);
print OUT "$numConsts          !NFBR number of forcing frequencies on open boundaries\n";

# write frequencies
foreach my $lc (@CONSTS){
   my $const=uc($lc); # capitlize it
   print OUT "$const     !BOUNTAG\n";
   print OUT "$FREQS{$lc}   %"."$const"."NF%  %"."$const"."EQARG%\n";
}


# interpolate and write amplitude and phase data

foreach my $lc (@CONSTS){
   my $const=uc($lc); # capitlize it
   print OUT "$const     !BOUNTAG\n";
   my $nn=0;
   print "processing $const\n";
   foreach my $lon (@X) {
      my $lat = $Y[$nn];
      $lon=$lon+360 if ($lon < 0);
      #  print "lon lat $lon $lat\n";
 
      my $xi=($lon-$lon0)/$dx + 0.5;  # coordinates with cell size units
      my $yi=($lat-$lat0)/$dy + 0.5;

      #  print "xiyi $xi $yi\n";

      my $i0=int($xi);  # index of the lower left corner height
      my $j0=int($yi);

      #  print "i0jo $i0 $j0\n";

      $xi=$xi-$i0;
      $yi=$yi-$j0;
      #  print "xiyi22 $xi $yi\n";
     
     

      my $rl_ll=$TPXO{$lc}->{REAL}->[$i0][$j0];
      my $im_ll=$TPXO{$lc}->{IMG}->[$i0][$j0];

      my $rl_lr=$TPXO{$lc}->{REAL}->[$i0+1][$j0];
      my $im_lr=$TPXO{$lc}->{IMG}->[$i0+1][$j0];

      my $rl_ul=$TPXO{$lc}->{REAL}->[$i0][$j0+1];
      my $im_ul=$TPXO{$lc}->{IMG}->[$i0][$j0+1];

      my $rl_ur=$TPXO{$lc}->{REAL}->[$i0+1][$j0+1];
      my $im_ur=$TPXO{$lc}->{IMG}->[$i0+1][$j0+1];

      my $rl=bilinear($xi,$yi,$rl_ll,$rl_lr,$rl_ur,$rl_ul);
      my $im=bilinear($xi,$yi,$im_ll,$im_lr,$im_ur,$im_ul);
      
      my $a=($rl**2 + $im**2)**0.5;
      my $p=atan2(-1*$im,$rl) * 360/2.0/$pi;

      print OUT "$a $p\n";

      $nn++;
   }


}
close(OUT);

#######################################################
# subroutine for bilinear interpolation on unit square
#
sub bilinear {
   my ($xi,$yi,$f00,$f10,$f11,$f01)=@_;
   
   my $b2=$f10-$f00;
   my $b3=$f01-$f00;
   my $b4=$f00-$f10-$f01+$f11;

   my $f=$f00 + $b2*$xi +$b3*$yi + $b4*$xi*$yi;

   return $f;
}

#


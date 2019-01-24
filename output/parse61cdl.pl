#!/usr/bin/env perl
###############################################################################
#  a script to parse a fort.61.cdl file that
#  has been dumped to CDL format by ncdump
#
#  or and ascii fort.61, which also requires
#  a stations file be given.
#
#  outputs a csv file that contains a table of time series data.  the 
#  first column of the table is the time since coldstart in seconds
#  this is followed by a column for each recording station. 
#
#  usage:
#
#  e.g. for interactive use
#
#    parse61.pl
#
#  
#  e.g.  for command line use with ASCII file with standard elev_stat.151 
#
#    parse61.pl  --infile fort.61 --stationfile elev_stat.151 --skiplines 1 --printcoords
#
#  e.g. for a netcdf file  (first run ncdump to create cdl file)
#
#    parse61.pl  --infile fort.61.cdl --printcoords
# 
#  options:
#
#  --infile - specifies the name of the input fort.61 file, 
#             must be *.61 or *.61.cdl
#
#  --stationfile - if using fort.61 you must specify a file with the stations
#                  coordinates in a format similar to what is required in 
#                  fort.15 or elev_stat.151 file 
#                  station names follow an exclamation point on each line
#
#  --skiplines - for ASCII fort.61 you need to specify how many lines to skip 
#                in the stations file before the station coordinates begin. if
#                unspecified, no lines will be skipped. 
#
#
#  --printcoords - if specified (and/or set to something other than zero)
#                  the output file csv table will be prepended with a
#                  the number of stations on the first line, followed by a 
#                  list of the station names, and then two table rows 
#                  containing the longitude and latitude
#
#
###############################################################################
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
# You can receive a copy of the GNU General Public License
# by writing to the Free Software Foundation, Inc.  
# 59 Temple Place - Suite 330,Boston, MA  02111-1307,
# USA. or see <http://www.gnu.org/licenses/>
#                                       
###############################################################################

use strict;
use warnings;
use Getopt::Long;


my $infile;
my $outFile='fort61.csv';
my $printNameAndCoords=0;
my $headerlines;
my $stationFile;


GetOptions('infile:s'      => \$infile,
           'stationfile:s'    => \$stationFile,
           'skiplines:s'   => \$headerlines,
           'outfile:s'     => \$outFile,  
           'printcoords+' => \$printNameAndCoords 
                                                   );
unless (defined $infile) {
    print "need to specify input file e.g.\n --infile fort.61.cdl\n   or \n --infile fort.61\n";
    print "enter input file name  (the extension is important):\n";
    $infile=<>;
    chomp $infile;
    $printNameAndCoords=1;
}


my @T;
my @WSE;
my @NAMES;
my @LON;
my @LAT;


#######################################################################
# if it's a cdl file do the following
if ($infile =~ m/\.cdl$/){    
   open IN, "<$infile" or die "ERROR: parse61.pl: cant open $infile\n";
   # assume the cdl file has dimensions, variables, data in that order

   # loop through the file
   $/="\n";   # explicitly start out with newline as record separator

   my %dataset;


   my $dimensionsStr='';
   my $variablesStr='';
   while (<IN>) { 
      if ($_ =~ m/dimensions:/){
         chomp ($_);
         $_ =~ s/\/\/.*$//;  #remove any comment
         $dimensionsStr=$_;
         while (<IN>){
            chomp; 
            $_ =~ s/\/\/.*$//; # remove any comment
            $variablesStr=$_;  # gets over written until jump out of loop
            last if ($_ =~ m/variables:/);
            $dimensionsStr="$dimensionsStr".$_;
         }
         if ($_ =~ m/variables:/){
            while (<IN>){
               chomp; 
               $_ =~ s/\/\/.*$//;  # remove any comment
               last if ($_ =~ m/data:/);
               $variablesStr="$variablesStr".$_;
            }
         }
         
      } 
   }
   close (IN);

   ####################################################################
   # process the dimensions

   # remove the 'dimensions: from the string
   $dimensionsStr =~ s/^dimensions://;
   my @statements=split(/;/,$dimensionsStr);  # assuming there are no semicolons in any of the character data strings... could be trouble
   foreach my $statement (@statements){
        #print "$statement\n";
        my ($key,$value)=split(/=/,$statement);
        # clean up the whitespace
        $key =~ s/^\s+//;
        $value =~ s/^\s+//;
        $key =~ s/\s+$//;
        $value =~ s/\s+$//;
        $dataset{dimensions}{$key}=$value;
   }

   # print out the data to make sure we've got it correctly
   my $ref=$dataset{dimensions};
   #print "ref is $ref\n";
   #foreach my $key (keys %{$ref}){
   #    print "$key: $ref->{$key}\n";
   #}


   ###################################################################
   # process the varibles and attributes
   # remove the 'variables:' from the string
   $variablesStr =~ s/^variables://;
   @statements=split(/;/,$variablesStr);  # assuming there are no semicolons in any of the character data strings... could be trouble
   foreach my $statement (@statements){
        #check to see if this is a variable declaration or attribute assignment
        $statement =~ s/^\s+//;  #remove leading whitespace
        if (($statement =~ m/^char/)or
            ($statement =~ m/^byte/)or
            ($statement =~ m/^short/)or
            ($statement =~ m/^int/)or
            ($statement =~ m/^long/)or
            ($statement =~ m/^float/)or
            ($statement =~ m/^real/)or
            ($statement =~ m/^double/)){
        
            my @data=split(/\s+/,$statement);
            my $type=shift (@data);
            $statement = join ('\s',@data);
            $statement =~ m/^(.+)\((.+)\)/;
            my $varName=$1;
            my $size=$2;
            $dataset{variables}{$varName}{size}=$size;
            $dataset{variables}{$varName}{type}=$type;
        }else{     # else it is a attribute statement
           my @data=split(/:/,$statement);
           my $varName=shift(@data);
           $statement = join (':',@data);
     
           $statement =~ s/^$varName:// if ($varName ne '');  
           my ($attr,$value)=split(/=/,$statement);
           # clean up the whitespace
           $attr =~ s/^\s+//;
           $value =~ s/^\s+//;
           $attr =~ s/\s+$//;
           $value =~ s/\s+$//;
           $attr =~ s/^://;
     
           if ($varName){
              $dataset{variables}{$varName}{$attr}=$value;
           }else{
              $dataset{variables}{$attr}=$value;   
           }   
        }# else its an attribute statement
   }

   # read the data
   open IN, "<$infile" or die "dying parse61_CDL.pl:  cant open $infile\n";
   $/="\n";  
   while (<IN>){
      last if ($_ =~ m/data:/);
   }


   $/=';';
   while (<IN>){
      chomp;
      $_ =~ s/^\s+//;
      last if ($_ =~ m/^}/); 
      my ($var,$valueStr)=split(/=/,$_);
      $var =~ s/\s+$//;
      my @vals=();
      if ($dataset{variables}{$var}{type} eq 'char'){
         @vals=split(/",/,$valueStr);
         foreach my $n (0..$#vals){
            $vals[$n] =~ s/^[\s"]+//; 
            $vals[$n] =~ s/[\s"]+$//; 
         }
      }else{
         @vals=split(/,/,$valueStr);
         foreach my $n (0..$#vals){
            $vals[$n] =~ s/^\s+//; 
            $vals[$n] =~ s/\s+$//; 
            $vals[$n]=$dataset{variables}{_FillValue} if ($vals[$n] eq '_');
         }
      }
      $dataset{data}{$var}=\@vals;
   }
   close(IN);
  
   # these are what we need to write out
   @T=@{$dataset{data}{time}};
   @WSE=@{$dataset{data}{zeta}};
   @NAMES=@{$dataset{data}{station_name}};
   @LAT=@{$dataset{data}{y}};
   @LON=@{$dataset{data}{x}};




}elsif ($infile =~ m/\.61/){ # end if its a cdl file
   #################################################################
   # here if its an ascii fort.61

   # check to make sure we have a station file
   unless (defined $stationFile){
      print "You're using ASCII, please give a stationsfile.\n  e.g. --stationfile elev_stat.151\n";
      print "Enter the name for a stations file:\n";
      $stationFile=<>;
      chomp ($stationFile);
      $printNameAndCoords=1;
   }

   unless (defined $headerlines) {
      print "You did not provide a number of station file header lines.\n  e.g. --skiplines 1\n";
      print "enter the number of lines to skip in the station file\n";
      $headerlines=<>;
      chomp $headerlines;
   } 

   # read the stations file and get the coordinates and names 
   # assume names are after, but not including an exclamation point
   open STA, "<$stationFile" or die "dying parse61.pl: cant open stationfile\n"; 

   while ($headerlines--){
       <STA>; 
   }

   my $k=0;
   while (<STA>){
      $k++;
      chomp;
      $_ =~ s/^\s+//;
      $_ =~ s/\s+$//;
      my ($lon,$lat)=split(/\s+/,$_);
      push @LON,$lon;
      push @LAT,$lat;
      $_ =~ m/!(.+)$/;
      my $name = 'no name for station $k';
      $name = $1 if (defined $1);
      push @NAMES, $name;
   }
   close(STA);
   open IN, "<$infile" or die "parse61.pl cant open $infile\n";

   my $line=<IN>;
   $line=<IN>;
   chomp($line);
   $line =~ s/^\s+//;
   my ($nrecs,$nstations)=split(/\s+/,$line);
   #print "$fort61 has $nrecs records for $nstations stations\n";
   die "dying parse61.pl: the stations file does not match the fort.61, or wrong number of skiplines\n"  unless ($nstations == $#LON+1); 


   foreach my $rec (0..$nrecs-1){
      $line=<IN>;
      chomp ($line);
      $line =~ s/^\s+//;
      my ($seconds,$timestep)=split(/\s+/,$line);
      my $days=$seconds/86400;
      push @T, $seconds;   # time since coldstart in days
      foreach my $sta (1..$nstations){
         $line=<IN>;
         chomp ($line);
         $line =~ s/^\s+//;
         my ($ssta,$wse)=split(/\s+/,$line);
         push @WSE,$wse;
   
      }
   }
}elsif ($infile =~ m/\.nc/){
   print "ERROR: parse61.pl:  You're trying to use a netcdf file, please convert to cdl with ncdump first\n";
   print "   e.g.  ncdump fort.61.nc > fort.61.cdl\n";
   die;
}else{
   die  "ERROR: parse61.pl: incorrect file extension for fort.61 input\n";
   
}

   
#write the output
open OUT, ">$outFile" or die "cant open output file $outFile\n";
my $nstae=$#NAMES +1;
if  ($printNameAndCoords){
   print OUT "$nstae ! NSTAE\n";
   foreach my $name (@NAMES){
      print OUT "$name\n";
   }
   my $str = join (',','Longitude',@LON);
   print OUT "$str\n";
   $str = join (',','Latitide',@LAT);
   print OUT "$str\n";
}


foreach my $t (@T){
   print OUT "$t";
   foreach my $k (1..$nstae){
       my $wse=shift(@WSE);
       print OUT ",$wse";
   }
   print OUT "\n" if ($#WSE);
}
close OUT;


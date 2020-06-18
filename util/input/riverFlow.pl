#!/use/bin/env perl
#-------------------------------------------------------------------------
# riverFlow.pl  
#
# This script calculates  unit discharge for periodic flux boundaries
# and writes the output in a format that can be inserted into fort.15
#
# If there is more than one flux boundary you need to know what order
# they appear in the fort.14 and be sure to specify the array of total
# discharges in the same order.
#
# it requires the AdcGrid.pm module   
#  
# usage example:
#
# perl riverFlow.pl --gridfile /path/to/fort.14  --units cfs 
#                        --discharge 700000,30000 [--outfile out.txt]  
#                                        
#
# Units can be cfs or cms
#
# Discharge should be specified as a comma separte list, with one value
# for each non-zero flux boundary listed in the same order they
# appear in the fort14 file
# 
# You can optionally specify an output filename with the --outfile option
# if you don't specify it the output file will be name unitDischarge.txt
#
# If you omit any of the options the script will run interactively and
# prompt you for the missing information.  If you don't specify the 
# discharge(s) the script will return some basic information on the flux
# boundaries, which may be helpful in determining the appropriate order
# for specifying the flux.
#
# e.g. 
#       perl riverFlow.pl --gridfile fort.14 
#
#--------------------------------------------------------------------------
# Copyright(C) 2016 Nathan Dill
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
use strict;
use warnings;
use AdcGrid;
use Getopt::Long; 


my $gridfile;
my $discharge_csv;
my @Discharge;
my $units;
my $outfile='unitDischarge.txt'; # default output can be changed witn option

GetOptions("gridfile=s"   => \$gridfile,
           "discharge=s"  => \$discharge_csv,
           "units=s"      => \$units,
           "outfile=s"    => \$outfile); 

#split up the discharge values if given
if ($discharge_csv){
   chomp $discharge_csv;
   $discharge_csv =~ s/^\s+//;
   $discharge_csv =~ s/\s+$//;
   @Discharge=split(/,/,$discharge_csv);
}

unless ($gridfile){
    print "You did not specify a grid file e.g .(--gridfile fort.14)\n";
    print "please enter the name of the grid file:\n";
    $gridfile=<>;
    chomp $gridfile;
}

unless ($units){
   print "You did not specify units for discharge values (e.g. --units cfs)\n";
   print "Please enter units: (cfs or cms)\n";
   $units=<>;
   chomp $units;
}


#open output file
open OUT, ">$outfile" or die "ERROR: riverFlow.pl can not open $outfile for writing\n";


# load the grid file into a adcGrid object
my $adcGrid=AdcGrid->new();
$adcGrid->loadGrid($gridfile);


# get flux boundaries
my $nbou=$adcGrid->getVar('NBOU');

# loop through the boundaries and identify non-zero flux type
my $cnt=0;
foreach my $bndId (1..$nbou){
   my @list=$adcGrid->getFluxBnd($bndId);
   my $nvell=shift(@list);
   my $ibtype=shift(@list);
   #print "bnd $bndId, ibtype $ibtype, nvell $nvell\n";


   my @nbvv=@{shift(@list)};
   if ($ibtype==52 || $ibtype==22 || $ibtype==32){
      $cnt++;
     
      # calculate some basic geometry for boundary 
      my ($xrf,$yrf,$zrf)=$adcGrid->getNode(\@nbvv);
      my @X=@{$xrf};
      my @Y=@{$yrf};
      my @Z=@{$zrf};
      #compute flow area under zero and total length
      my $A=0;
      my $bndLen=0;
      foreach my $n (0..$nvell-2){
          my $ds=&cppdist($X[$n],$Y[$n],$X[$n+1],$Y[$n+1]);
          $A=$A+$ds*($Z[$n]+$Z[$n+1])/2.0;
          $bndLen=$bndLen+$ds;  # in meters
      }
      # If the discharge array was specified at the cmd line use it
      my $disc=shift(@Discharge);
      
      print "INFO: riverFlow.pl: using discharge $disc $units for boundary $bndId, fluxbnd $cnt\n";
      print "INFO: riverFlow.pl:  ... ibtype $ibtype with $nvell nodes, length $bndLen m, area $A m^2\n";
      print "INFO: riverFlow.pl:  ...  starting on node $nbvv[0]: $X[0] $Y[0]\n"; 
      print "INFO: riverFlow.pl:  ...  ending on node $nbvv[$#nbvv]: $X[$nvell-1] $Y[$nvell-1]\n";

      # if no discharge specified, prompt the user to enter it
      # also report some basic stats on the boundary to help
      # user identify which boundary it is.     
      unless (defined ($disc)){
         print "Please specify discharge for boundary $bndId, flux bnd # $cnt\n";  
         print "   ibtype $ibtype with $nvell nodes, length $bndLen m, area $A m^2\n";
         print "   starting on node $nbvv[0]: $X[0] $Y[0]\n"; 
         print "   ending on node $nbvv[$#nbvv]: $X[$nvell-1] $Y[$nvell-1]\n";
         $disc=<>;
         chomp $disc;
      }
      my $cms;
      if (lc ($units) eq 'cfs'){
         $cms=$disc/3.280833333**3.0;
      }elsif (lc ($units) eq 'cms'){
         $cms=$disc;
      }else{
         die "ERROR: riverflow.pl: incorrect units specified, use cfs or cms \n"; 
      }
      my $averageVel=$cms/$A;
      foreach my $n (0..$nvell-1){
           my $ud=$Z[$n]*$averageVel;
           print OUT "$ud 0.0     ! unitFlux for bndId $bndId, node $nbvv[$n], $disc $units\n";
       }
    }
    

}
          
# sub to calculate distance between two points given in degrees longitude,latitude
# using cpp projection
sub cppdist { # lat lon lat0 lon0
 my $twoPiOver360=8*atan2(1,1)/360.;
 my $R=6378206.4;
 my $lat=$_[0]*$twoPiOver360;
 my $lon=$_[1]*$twoPiOver360;
 my $lat0=$_[2]*$twoPiOver360;
 my $lon0=$_[3]*$twoPiOver360;

 my $x=$R*($lon-$lon0)*cos($lat0);
 my $y=($lat-$lat0)*$R;

 my $ds=( $x**2.0 + $y**2) **0.5
}


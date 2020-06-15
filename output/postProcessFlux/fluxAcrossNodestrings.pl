#!/usr/bin/env perl
##############################################################
# fluxAlongNodestrings.pl
#
# an ADCIRC postprocessiing script
# 
# calculates the flux across a string of nodes from an ADCIRC
# model grid based on the depth and depth averaged velocity
# given in the fort.63 and fort.64 output.  It assumes the 
# model is in geographic coordinates (ICS=2) and uses the
# CPP projection to convert to Cartesian coordinates prior
# to calculating flux. Output is provided in a tabular csv
# file in metric units as well as in a kml file that includes
# kml LineString placemarks showing the nodestrings. the
# placemark descriptions will include an html table listing 
# the time and discharge across the path in english units  
#
# The flux is calculated by first determing a unit normal
# vector and representative width for each node along the
# nodestring. Then the flux at each node is calcualted by 
# multiplying the instantaneous depth at the node times
# dot product of the unit normal and the depth averaged 
# current velocity at the node. The nodal fluxes are summed
# to give the total flux across the node string.
#
# The flux is considered positive if flow is to the right
# if one is standing at the beginning of the node string and
# looking toward end (e.g. a nodestring starting on the right
# bank of a river and extending to the left bank will have 
# positive flux)
#
# Because the normal vector at the ends of the given nodestring
# is somewhat ill-defined the calculation is not precise, but 
# should be relatively accurate when the nodestrings are drawn 
# are drawn relatively straight with end points that have 
# zero flux and/or depth. 
#
#  usage:
#
#  perl fluxAlongNodestrings.pl --nodestrings nodestrings.txt --kmloutput output.kml --gridfile fort.14 --fort63 fort.63 --fort64 fort.64
#
#  nodestrings.txt is a file that lists the nodestrings across
#  which you want to calculate the flux. 
#  like the open boundary portion of the fort.14 file starting
#  with the NOPE line
#
#  output.kml is the name you want to use for the output kml 
#  file that shows the nodestrings and tables of the 
#  compted flux
#
#  the other arguments are the standard ADCIRC file names
############################################################
#
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
############################################################

use strict;
use warnings;
use Getopt::Long;

my $gridfileName='fort.14';
my $fort63='fort.63';
my $fort64='fort.64';
my $nodestringFile='nodestrings.txt';
my $outKMLfile='nodestringFlux.kml';

GetOptions( 
            "nodestrings=s" => \$nodestringFile,
            "kmloutput=s" => \$outKMLfile,
            "gridfile=s" => \$gridfileName,
            "fort63=s" => \$fort63,
            "fort64=s" => \$fort64
          );
#####################################################
#
# read in the grid node position table
#
########################################################

my @X;
my @Y;
my @Z;
my $minX=9999;
my $maxX=-9999;
my $minY=9999;
my $maxY=-9999;
my $line;
my @data;
my $i;
my $j;
my $k;
my $agrid;

# make a name 
$outKMLfile="$outKMLfile.kml" unless ($outKMLfile =~ /\.kml$/i); 
my $outCSVfile=$outKMLfile;
$outCSVfile =~ s/\.kml$/.csv/i;





open FILE, "<$gridfileName" or die "cannot open $gridfileName\n";

###########################################################
# read AGRID
$agrid=<FILE>;
chomp $agrid;
$agrid=~ s/^\s+//;        
$agrid=~ s/\s+$//; 
print "reading: $agrid\n";

###########################################################
# number of elements and nodes
$line=<FILE>;
chomp $line;               # remove end of line character
$line=~ s/^\s+//;          # remove leading  whitespace
$line =~ s/\s+$//;         # remove trailing  whitespace
my ($Ne, $Nn) = split(/\s+/,$line); # split on whitespace
print "$agrid has $Ne elements and $Nn nodes\n";

###########################################################
# read node positions
print " reading nodes\n"; 
my $cnt=1;
while ( <FILE> ){
   chomp ($_);
   $_=~ s/^\s+//;          # remove leading  whitespace
   $_ =~ s/\s+$//;    
   @data = split(/\s+/,$_); # split on whitespace
   my ($j,$x,$y,$z)=@data;
   $X[$j]=$x;
   $Y[$j]=$y;
   $Z[$j]=$z;

   $maxX=$x if $x > $maxX;
   $maxY=$y if $y > $maxY;
   $minX=$x if $x < $minX;
   $minY=$y if $y < $minY;
   last if ($cnt++ == $Nn);
}
print " reading elements\n";

close FILE;


#############################################################
# read in node strings like they are normal flux boundaries
#print " reading open boundary information\n";

open FILE, "<$nodestringFile" or die "cant open $nodestringFile";

#############################################################
# normal flow boundaries

#print " reading normal flux boundary information\n";

# NBOU
$line=<FILE>;
chomp ($line);
$line=~ s/^\s+//;        
$line=~ s/\s+$//;    
@data = split(/\s+/,$line); 
my $nbou=$data[0];
print "$agrid has $nbou normal flux boundaries\n";

# skip the NVEL
$line=<FILE>;

# NVELL,IBTYPE,NBVV,IBCONN,BARLANHT,BARLANCFSP,BARINHT,BARINCFSB,BARINCFSP
$k=1;
my @nvell;
my @ibtype;
my @nbvv;

while ($k<=$nbou) {
   $line=<FILE>;
   chomp ($line);
   $line=~ s/^\s+//;         
   $line=~ s/\s+$//;    
   @data = split(/\s+/,$line); 
   $nvell[$k]=$data[0];
   $ibtype[$k]=$data[1];
   print "nodestring $k: ibtype is $ibtype[$k] with $nvell[$k] nodes\n"; 
   $j=1;
   while  ($j<=$nvell[$k]) {
      $line=<FILE>;
      chomp ($line);
      $line=~ s/^\s+//;         
      $line=~ s/\s+$//;    
      @data = split(/\s+/,$line); 
      $nbvv[$k][$j]=$data[0];
      #print "$nbvv[$k][$j]\n";
      $j++;
   }
   $k++;
}

#################################################### 

# process the node strings
# calculate a width for each node on the string

my @NZ;
my @NX;
my @NY;
my @W;
my @normalX;  #components of unit normal vector
my @normalY;

# convert coordinates to meters
#
$k=1;
foreach my $nstr (1..$nbou){
   my @nodestring= @{$nbvv[$nstr]};
   shift @nodestring;  # since the first value is undef
   print "$k boundary number $nstr\n";
   print "nodes: @nodestring\n";
   $i=1;
   @NX=();
   @NY=();
   foreach my $node (@nodestring){
      my ($nx,$ny)= &cppd($X[$node],$Y[$node],-91.,30.);
       $NX[$i]=$nx;
       $NY[$i]=$ny;
      print "xy $nx $ny\n";
      $i++;
   }

   # determine approximate width and unit normal vector associated with 
   # each node along the string

   my $numNodes=scalar(@nodestring);
   $i=1; 

   foreach my $node (@nodestring) {
      my $dx1;  # these will hold components of dislpacemtne vectors
      my $dx2;  # that from a node halfway to the neighboring node(s)
      my $dy1;  # on the nodestring. 
      my $dy2;


      if ($i==1){
        $dx1=0;
        $dx2=0.5*($NX[$i]-$NX[$i+1]);
        $dy1=0;
        $dy2=0.5*($NY[$i]-$NY[$i+1]);
      }elsif ($i>1 && $i<$numNodes){
        $dx1=0.5*($NX[$i-1]-$NX[$i]);
        $dx2=0.5*($NX[$i]-$NX[$i+1]);
        $dy1=0.5*($NY[$i-1]-$NY[$i]);
        $dy2=0.5*($NY[$i]-$NY[$i+1]);
      }elsif ($i==$numNodes){
        $dx1=0.5*($NX[$i-1]-$NX[$i]);
        $dx2=0;
        $dy1=0.5*($NY[$i-1]-$NY[$i]);
        $dy2=0;
      }
  
     # approximate the width as the sum of the half displacements to the 
     # neibiboring node(s). Need a relatively straight node string for good approximation
      $W[$k][$i]= ( $dx1**2 + $dy1**2 )**0.5  +  ( $dx2**2 + $dy2**2 )**0.5; 
      $NZ[$k][$i]=$Z[$node];  # the cdepth for this node
   
      $normalX[$k][$i]=-1.0*($dy1+$dy2)/$W[$k][$i]; # the unit normal for this node
      $normalY[$k][$i]=($dx1+$dx2)/$W[$k][$i];


      print "$i width for node $node =  $W[$k][$i]\n";
      print "$i depth for node $node =  $NZ[$k][$i]\n";
      print " normal vector is $normalX[$k][$i] $normalY[$k][$i]\n";

      #my $nc= ($normalX[$i]**2 + $normalY[$i]**2)**0.5;
      #print "check $nc\n";

      $i++;
   }
   $k++
} # end loop over nodestrings

# get the velocity and wse data and calculate the discharge
#
open FORT63, "<$fort63" or die "cannot open $fort63";
open FORT64, "<$fort64" or die "cannot open $fort64";


open OUTFILE, ">$outCSVfile";
print OUTFILE "time(seconds)";
foreach my $nstr (1..$nbou) { 
   print OUTFILE ",Flux BND_$nstr (m^3s^-1)";
}
print OUTFILE "\n"; 

my $junk;
$junk=<FORT63>;
$junk=<FORT64>;
$junk=<FORT63>;
$junk=<FORT64>;

my @ETA;
my @VX;
my @VY;

my %tableData;  # hash of arrays to store the data for later
foreach my $nstr (1..$nbou){
   $tableData{$nstr}=['days since coldstart', 'flow in cfs'];
}


my $jkj=0;
while (1==1) {
   $line=<FORT63>;
   last unless $line;
   $line=<FORT64>;
   $line=~ s/^\s+//;          # remove leading  whitespace
   $line =~ s/\s+$//;    
   my ($time, $it) = split(/\s+/,$line); # split on whitespace
   print "reading $time $it\n";
   print OUTFILE "$time" ;

   my $n;
   my $cnt=$Nn;
   while ($cnt--){  # read the data
     $line=<FORT63>;
     chomp $line;
     $line=~ s/^\s+//;          # remove leading  whitespace
     $line =~ s/\s+$//;    
     my ($tmpn, $tmpEta) = split(/\s+/,$line); # split on whitespace
     $n=$tmpn;
     $ETA[$n]=$tmpEta;
     #  print "line: $line\n";
     #  print "cnt,n,eta: $cnt $n $ETA[$n]\n";
     $line=<FORT64>;
     chomp $line;
     $line=~ s/^\s+//;          # remove leading  whitespace
     $line =~ s/\s+$//;    
     my ($nn, $tmpVX, $tmpVY) = split(/\s+/,$line); # split on whitespace
     $VX[$nn]=$tmpVX;
     $VY[$nn]=$tmpVY;
    }
    # now we have the data for this output time, calculte the 
   
  $k=1;
 foreach my $nstr (1..$nbou){
   my @nodestring= @{$nbvv[$nstr]};
   shift @nodestring;  # since first element is undef
#   print "nodestring k $k ";
   $i=1;
   my $totalQ=0;
   my $i=0;
   foreach my $node (@nodestring) {
      my $depth = $ETA[$node]+$Z[$node];
      $depth=0 if ($depth < 0);
      my $vnormal = $VX[$node]*$normalX[$k][$i] + $VY[$node]*$normalY[$k][$i];
      my $q = $depth*$vnormal;
      my $speed =  ($VX[$node]**2+$VY[$node]**2)**0.5;
      my $q2=$depth*$speed;


   #   print "$i node $node speed is $speed vnormal is $vnormal, vx,vy $VX[$node] $VY[$node]\n";

   #   print "$i node $node qNormal is $q, depth is $depth, qfull is $q2\n";
      
      

      $totalQ = $totalQ + $q*$W[$k][$i];
      $i++;
   }
   #print "totalQ is $totalQ\n";
   printf( "%10.2f ", $totalQ );
   print OUTFILE ",$totalQ";
   my $days=$time/86400;
   push @{$tableData{$nstr}}, sprintf("%11.7f",$days);
   my $cfs=$totalQ*3.280833333**3;
   push @{$tableData{$nstr}}, sprintf("%10d",$cfs);
 $k++; 
 }
print "\n";
print OUTFILE "\n";
}
    
close FORT63;
close FORT64;
close OUTFILE;




#now  write some kml to see the data
open KML, ">$outKMLfile" or die "cant open $outKMLfile";

#write the opening
my $str=openDoc($outKMLfile);
print KML "$str\n";

#write a style
$str=blueStyle();
print KML "$str\n";


# write the placemarns
foreach my $nstr (1..$nbou){

    my $desc=HTML_table(2,$tableData{$nstr});

   # get the coordinates
   my @nodestring= @{$nbvv[$nstr]};
   shift @nodestring;  # since the first value is undef
   my @XX;
   my @YY;
   my @ZZ; 
   foreach my $node (@nodestring){
      push @XX, $X[$node];
      push @YY, $Y[$node];
      push @ZZ, $Z[$node];
   }

   my $pstr=kmlPath(\@XX,\@YY,\@ZZ,"xs $nstr",$desc,'#blueOutline');
   print KML "$pstr\n";
}
$str =closeDoc();
print KML "$str\n";
close KML;







################################################################################3
# some subroutines

#cpp projection 
sub cppd { #lon,lat,lon0,lat0  (in degrees)

   my ($lon, $lat, $lon0, $lat0)=@_;  # get the arguments

   my $R=6378206.4; #radius of Earth in meters
   my $pi=3.14159265359;
   my $radLat0=$lat0*$pi/180;
   my $x=$R*($lon-$lon0)*cos($radLat0)*$pi/180.;
   my $y=($lat-$lat0)*$R*$pi/180.;
   return ($x,$y);
}


# sub to write a kml path placemark
sub kmlPath { #

   my @px=@{$_[0]};
   my @py=@{$_[1]};
   my @pz=@{$_[2]};
   my $name=$_[3];
   my $desc=$_[4];
   my $style=$_[5];

   my $coordstr='';
   foreach my $i (0..$#px){
      my  $point=sprintf("%0.14f,%0.14f,%0.5f",$px[$i],$py[$i],$pz[$i]);
      $coordstr="$coordstr $point";
   }

   my $kmlStr='<Placemark>';
   $kmlStr="$kmlStr\n"."   <name>$name</name>";
   $kmlStr="$kmlStr\n"."   <description>\n<![CDATA[$desc]]>\n</description>";
   $kmlStr="$kmlStr\n"."   <styleUrl>$style</styleUrl>";
   $kmlStr="$kmlStr\n".'   <LineString>';
   $kmlStr="$kmlStr\n".'             <coordinates>';
   $kmlStr="$kmlStr\n"."                $coordstr";
   $kmlStr="$kmlStr\n".'             </coordinates>';
   $kmlStr="$kmlStr\n".'   </LineString>';
   $kmlStr="$kmlStr\n".'</Placemark>';
   
   return $kmlStr;
   
}

################################################################

sub blueStyle {

 my $str='<Style id="blueOutline">
	  <IconStyle>
		<scale>1.3</scale>
		<Icon>
			<href>http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png</href>
		</Icon>
		<hotSpot x="20" y="2" xunits="pixels" yunits="pixels"/>
	</IconStyle>
	<LineStyle>
		<color>ff00ff55</color>
		<width>3</width>
	</LineStyle>
	<PolyStyle>
		<fill>0</fill>
	</PolyStyle>
</Style>';

    return ('#blueOutline',$str);
}


###############################################################
#  sub openDoc('name of kml document);
#
#  return a string to write at the beginning of the kml document 
# (taken from a file saved by Google Earth)
#
#  input is a scalar that will be used for the name of the document
#
################################################################
sub openDoc{

my $name=shift;

my $str='<?xml version="1.0" encoding="UTF-8"?>';
$str="$str\n".'<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">';
$str="$str\n".'<Document>';
$str="$str\n<name>$name</name>";

return ($str);

}


######################################################
# return a string of the tags for closing the document
sub closeDoc{
   my $str='</Document>'."\n".'</kml>';
return ($str);

}


# return a string with an HTML table
sub HTML_table {
   my $htmlstr='';
   my ($ncols,$dataref)=@_;
   my @data=@{$dataref};

   $htmlstr.='<table border="1" style="width:100%">';
   $htmlstr.="\n";
    my $colnum=1;
   # start first a row
   $htmlstr.='<tr>';
   foreach my $td (@data){
      if ($colnum ==1){ 
         $htmlstr.='<td align="center" valign="center"><b>'."$td".'</b></td>';
      }else{
         $htmlstr.='<td align="center" valign="center">'."$td".'</td>';
      }
      if ($colnum == $ncols) {  # end the row
         $htmlstr.='</tr>';
         $htmlstr.="\n";
         $htmlstr.='<tr>';
         $colnum=1;
      }else{
         $colnum++;
      }
   }
   $htmlstr =~ s/<tr>$//;
   $htmlstr.='</table>'; 
   return $htmlstr;
}



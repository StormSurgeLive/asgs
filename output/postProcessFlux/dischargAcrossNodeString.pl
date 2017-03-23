#!/usr/bin/perl
#
#
use strict;



# this is the name for the input grid file
my $gridFileName="fort.14";
#my $gridFileName="ec_95d.grd";

# will attempt to calculate the flux across this string of nodes,
# this will be grid specific, and ther is no check to see if this string
# is well formed (i.e. adjacent nodes in  order) 
my @nodestring=(3772,3771,3770,3768,3767,3757,3756,3748,3747,3746,3742,3741,3740);  # for the Miss R boundry in sl15_2010_HSDRRS_2012_v9.grd
#my @nodestring=(3940,3921,3920,3919,3918,3917,3899,3881,3879,3863,3860,3859,3858,3839,3822); # just a bit downstream from the boundary
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


open FILE, "<$gridFileName" or die "cannot open $gridFileName\n";

my $junk=<FILE>;
print "reading: $junk\n";
$junk=<FILE>;
$junk=~ s/^\s+//;          # remove leading  whitespace
$junk =~ s/\s+$//;    
my ($Ne, $Nn) = split(/\s+/,$junk); # split on whitespace
print " grid has $Ne elements and $Nn nodes\n";

my $cnt=1;
while ( <FILE> ){
   chomp ($_);
   $_=~ s/^\s+//;          # remove leading  whitespace
   $_ =~ s/\s+$//;    
   my @data = split(/\s+/,$_); # split on whitespace
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


#################################################### 

# process the node string
# calculate a width for each node on the string

my @NZ;
my @NX;
my @NY;
my @W;
my @normalX;  #components of unit normal vector
my @normalY;

# convert coordinates to meters
foreach my $node (@nodestring) {
   my ($nx,$ny)= &cppd($X[$node],$Y[$node],-91.,30.);
    push(@NX,$nx);
    push(@NY,$ny); 
   print "xy $nx $ny\n";
}




# determine approximate width and unit normal vector associated with 
# each node along the string
#
my $numNodes=@nodestring;
my $i=0; 

while ($i<$numNodes) {
   my $dx1;  # these will hold components of dislpacemtne vectors
   my $dx2;  # that from a node halfway to the neighboring node(s)
   my $dy1;  # on the nodestring. 
   my $dy2;


   if ($i==0){
     $dx1=0;
     $dx2=0.5*($NX[$i]-$NX[$i+1]);
     $dy1=0;
     $dy2=0.5*($NY[$i]-$NY[$i+1]);
   }elsif ($i>0 && $i<$numNodes-1){
     $dx1=0.5*($NX[$i-1]-$NX[$i]);
     $dx2=0.5*($NX[$i]-$NX[$i+1]);
     $dy1=0.5*($NY[$i-1]-$NY[$i]);
     $dy2=0.5*($NY[$i]-$NY[$i+1]);
   }elsif ($i==$numNodes-1){
     $dx1=0.5*($NX[$i-1]-$NX[$i]);
     $dx2=0;
     $dy1=0.5*($NY[$i-1]-$NY[$i]);
     $dy2=0;
   }
  
   # approximate the width as the sum of the half displacements to the 
   # neibiboring node(s). Need a relatively straight node string for good approximation
   $W[$i]= ( $dx1**2 + $dy1**2 )**0.5  +  ( $dx2**2 + $dy2**2 )**0.5; 
   $NZ[$i]=$Z[$nodestring[$i]];  # the cdepth for this node
   
   $normalX[$i]=-1.0*($dy1+$dy2)/$W[$i]; # the unit normal for this node
   $normalY[$i]=($dx1+$dx2)/$W[$i];


   print "$i width for node $nodestring[$i] =  $W[$i]\n";
   print "$i depth for node $nodestring[$i] =  $NZ[$i]\n";
   print " normal vector is $normalX[$i] $normalY[$i]\n";

   #my $nc= ($normalX[$i]**2 + $normalY[$i]**2)**0.5;
   #print "check $nc\n";

   $i++;
}


# get the velocity and wse data and calculate the discharge
#
open FORT63, "<fort.63" or die "cannot open fort.63";
open FORT64, "<fort.64" or die "cannot open fort.64";


my $junk;
$junk=<FORT63>;
$junk=<FORT64>;
$junk=<FORT63>;
$junk=<FORT64>;

my $line;
my @ETA;
my @VX;
my @VY;

while (1==1) {
   $line=<FORT63>;
   last unless $line;
   $line=<FORT64>;
   $line=~ s/^\s+//;          # remove leading  whitespace
   $line =~ s/\s+$//;    
   my ($time, $it) = split(/\s+/,$line); # split on whitespace
   print "reading $time $it\n";
   
   my $n;
   my $cnt=$Nn;
   while ($cnt--){  # read the data
     $line=<FORT63>;
     $line=~ s/^\s+//;          # remove leading  whitespace
     $line =~ s/\s+$//;    
     ($n, $ETA[$n]) = split(/\s+/,$line); # split on whitespace
     $line=<FORT64>;
     $line=~ s/^\s+//;          # remove leading  whitespace
     $line =~ s/\s+$//;    
     ($n, $VX[$n], $VY[$n]) = split(/\s+/,$line); # split on whitespace
    }
    # now we have the data for this output time, calculte the 
   
   my $totalQ=0;
   my $i=0;
   foreach my $node (@nodestring) {
      my $depth = $ETA[$node]+$Z[$node];
      my $vnormal = $VX[$node]*$normalX[$i] + $VY[$node]*$normalY[$i];
      my $q = $depth*$vnormal;
      my $speed =  ($VX[$node]**2+$VY[$node]**2)**0.5;
      my $q2=$depth*$speed;


   #   print "$i node $node speed is $speed vnormal is $vnormal, vx,vy $VX[$node] $VY[$node]\n";

      print "$i node $node qNormal is $q, depth is $depth, qfull is $q2\n";
      
      

      $totalQ = $totalQ + $q*$W[$i];
      $i++;
   }
   print "totalQ is $totalQ\n";
}
    
close FORT63;
close FORT64;

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





#now read the wse and velocity data





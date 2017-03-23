#!/usr/bin/perl
#
#
use strict;



# this is the name for the input grid file
my $gridFileName="fort.14";
#my $gridFileName="ec_95d.grd";

# will attempt to calculate the flux across,
# all the inflow type boundaries (2,12,22,32,42, 52) in the grid file

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

open FILE, "<$gridFileName" or die "cannot open $gridFileName\n";

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

###########################################################
# read element table
my $three;
my $cnt=1;
while ( <FILE> ){
   chomp ($_);
   $_=~ s/^\s+//;          # remove leading  whitespace
   $_ =~ s/\s+$//;    
   @data = split(/\s+/,$_); # split on whitespace
   my ($id,$three,$n1,$n2,$n3)=@data;
 
   $cnt++;
    last if ($cnt > $Ne);
    #print "cnt=$cnt\n";
}

#############################################################
# open boundaries
print " reading open boundary information\n";

# NOPE
$line=<FILE>;
chomp ($line);
$line=~ s/^\s+//;        
$line=~ s/\s+$//;    
@data = split(/\s+/,$line); 
my $nope=$data[0];
print "$agrid has $nope open boundaries\n";

# NETA
$line=<FILE>;
chomp ($line);
$line=~ s/^\s+//;         
$line=~ s/\s+$//;    
@data = split(/\s+/,$line); 
my $neta=$data[0];
print "with $neta open boundary nodes\n";

# NVDLL, IBTYPEE, NBDV
$k=1;
my @nvdll;
my @ibtypee;
my @nbdv;
while ($k<=$nope) {
   $line=<FILE>;
   chomp ($line);
   $line=~ s/^\s+//;         
   $line=~ s/\s+$//;    
   @data = split(/\s+/,$line); 
   $nvdll[$k]=$data[0];
   $ibtypee[$k]=$data[1];
   print "open boundary $k: ibtypee is $ibtypee[$k] with $nvdll[$k] nodes\n"; 
   $j=1;
   while  ($j<=$nvdll[$k]) {
      $line=<FILE>;
      chomp ($line);
      $line=~ s/^\s+//;         
      $line=~ s/\s+$//;    
      @data = split(/\s+/,$line); 
      $nbdv[$k][$j]=$data[0];
      print "$nbdv[$k][$j]\n";
      $j++;
   }
   $k++;
}



#############################################################
# normal flow boundaries

print " reading normal flux boundary information\n";

# NBOU
$line=<FILE>;
chomp ($line);
$line=~ s/^\s+//;        
$line=~ s/\s+$//;    
@data = split(/\s+/,$line); 
my $nbou=$data[0];
print "$agrid has $nbou normal flux boundaries\n";

# NVEL
$line=<FILE>;
chomp ($line);
$line=~ s/^\s+//;         
$line=~ s/\s+$//;    
@data = split(/\s+/,$line); 
my $nvel=$data[0];
print "with $nvel flux boundary nodes\n";

# NVELL,IBTYPE,NBVV,IBCONN,BARLANHT,BARLANCFSP,BARINHT,BARINCFSB,BARINCFSP
$k=1;
my @nvell;
my @ibtype;
my @nbvv;
my @ibconn;
my @barlanht;
my @barlancfsp,
my @barinht;
my @barincfsb;
my @barincfsp;

while ($k<=$nbou) {
   $line=<FILE>;
   chomp ($line);
   $line=~ s/^\s+//;         
   $line=~ s/\s+$//;    
   @data = split(/\s+/,$line); 
   $nvell[$k]=$data[0];
   $ibtype[$k]=$data[1];
   print "flux boundary $k: ibtype is $ibtype[$k] with $nvell[$k] nodes\n"; 
   $j=1;
   while  ($j<=$nvell[$k]) {
      $line=<FILE>;
      chomp ($line);
      $line=~ s/^\s+//;         
      $line=~ s/\s+$//;    
      @data = split(/\s+/,$line); 
      $nbvv[$k][$j]=$data[0];
      if ($ibtype[$k]==4 or $ibtype[$k]==24) {
         $barlanht[$k][$j]=$data[1];
	 $barlancfsp[$k][$j]=$data[2];
      }elsif ($ibtype[$k]==3 or $ibtype[$k]==13 or $ibtype[$k]==23){
         $ibconn[$k][$j]=$data[1];
	 $barinht[$k][$j]=$data[2];
	 $barincfsb[$k][$j]=$data[3];
	 $barincfsp[$k][$j]=$data[4];
      }
      print "$nbvv[$k][$j]\n";
      $j++;
   }
   $k++;
}




#####################################################
# now that we've got all the grid data
# make a list of type 2,12,22... boundaries
#
my @nstrList=();

$k=1;
while  ($k<=$nbou) {
   my @nodeString=();
   if ($ibtype[$k]==2 or $ibtype[$k]==12 or $ibtype[$k]==22 or $ibtype[$k]==32 or $ibtype[$k]==42 or $ibtype[$k]==52) {
      push (@nstrList,$k);
   }
   $k++;
}

my $nnstrs=scalar(@nstrList);
print "there are $nnstrs inflow boundaries\n";




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
foreach my $nstr (@nstrList){
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
open FORT63, "<fort.63" or die "cannot open fort.63";
open FORT64, "<fort.64" or die "cannot open fort.64";

open OUTFILE, ">flux_out.csv";
print OUTFILE "time(seconds)";
foreach my $nstr (@nstrList) { 
   print OUTFILE ",Flux BND_$nstr (m^3s^-1)";
}
print OUTFILE "\n"; 

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
 foreach my $nstr (@nstrList){
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
 $k++; 
 }
print "\n";
print OUTFILE "\n";
}
    
close FORT63;
close FORT64;
close OUTFILE;
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





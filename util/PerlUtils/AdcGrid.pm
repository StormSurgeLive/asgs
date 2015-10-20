package AdcGrid;
#######################################################################
#
# This is a Perl module for dealing with adcirc grids in a OO fashion.
# 
#----------------------------------------------------------------------
# example usage:
#
# load the module
#
#    use AdcircUtils::AdcGrid;
#
# create an AdcGrid object, which also loads the grid file.
#
#    $adcGrid=AdcGrid->new('fort.14');
#
# After you've you created the object you can do stuff with it. 
# for example:
#
# get the AGRID comment line
#
#    $agrid=$adcGrid->getVAR('AGRID');
# or   
#    $agrid=$adcGrid->{AGRID};
#
# get the number of elements
# 
#    $ne=$adcGrid->getVar('NE');
# or   
#    $ne=$adcGrid->{NE};
#
# get the position of a node 
#
#    $someNodeNumber=784
#    ($x,$y,$z)=$adcGrid->getNode($someNodeNumber);
#
# # get the positions of a list of nodes
#
#    @NIDS=(1,2,3,4,7,8,30);
#    ($xref,$yref,$zref)=$adcGrid->getNode(\@NIDS);
#    @X=@{$xref};
#    @Y=@{$yref};
#    @Z=@{$zref};
#    
# There is more you can do. Please read the comments below associated 
# with the various methods to get a further description description of
# what they do.  
# 
####################################################################### 
# Author: Nathan Dill, natedill(AT)gmail.com
#
# Copyright (C) 2014 Nathan Dill
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
use warnings;
use strict;

#######################################################################
# create a new AdcGrid object
#
# e.g.
#  
#  my $gridFileName='fort.14';
#
#  my $adcGrid = AdcGrid->new( $gridFileName );
#
#  This constructor method reads the grid and stores it in a hash.  For
#  the most part it uses the varible names from the fort.14 as the keys
#
#
#######################################################################
sub new {
   
   my $class = shift;
   my $obj={};
   bless $obj, $class;

   my $gridFileName=shift;
   $obj->{GRIDFILENAME}=$gridFileName;

   my $line;
   my @data;

   my $maxx=-999e99;
   my $maxy=-999e99;
   my $maxz=-999e99;
   my $minx=999e99;
   my $miny=999e99;
   my $minz=999e99;

   
   open my $fh, "<$gridFileName" or die "AdcGrid can't open $gridFileName\n";
  
   ###########################################################
   # read AGRID
   my $agrid = readCleanLine($fh);
   print "reading: $agrid\n";
   $obj->{AGRID}=$agrid;

   ###########################################################
   # number of elements and nodes
   $line = readCleanLine($fh);
   my ($Ne, $Np) = split(/\s+/,$line); # split on whitespace
   print "$agrid has $Ne elements and $Np nodes\n";
   $obj->{NE}=$Ne;
   $obj->{NP}=$Np;

   ###########################################################
   # read node positions
   print " reading nodes\n"; 
   my $cnt=1;
   my $jn='';  # these are binary strings stored using pack
   my $xyz='1234567890122345678901234';

   foreach my $k (1..$Np){
      $line = readCleanLine($fh);  
      @data = split(/\s+/,$line); # split on whitespace
      
      my $j=shift(@data); # this should be the node number
      unless ($j==$k){
         print "!! WARNING !!  non-consecutive node number at node $k\n";
      }
      my $packed=pack("d3",$data[0],$data[1],$data[2]); # should be 24 bytes ling
      my $lenPac=length($packed);
      my $offset=$j*24; # there will be nothing valuable at offset 0
      substr ($xyz,$offset,24,$packed); 
      #$xyz .= $packed;
      $jn .= pack("l",$j); # should be 24 bytes long

      # figure grid limits box

      $maxx=$data[0] if ($data[0] > $maxx);
      $maxy=$data[1] if ($data[1] > $maxy);
      $maxz=$data[2] if ($data[2] > $maxz);
      $minx=$data[0] if ($data[0] < $minx);
      $miny=$data[1] if ($data[1] < $miny);
      $minz=$data[2] if ($data[2] < $minz);



   }
   $obj->{XYZ}=$xyz;
   $obj->{JN}=$jn;

   $obj->{MAX_X}=$maxx;
   $obj->{MAX_Y}=$maxy;
   $obj->{MAX_Z}=$maxz;
   $obj->{MIN_X}=$minx;
   $obj->{MIN_Y}=$miny;
   $obj->{MIN_Z}=$minz;

   ###########################################################
   # read element table
   print " reading elements\n";
   my $three;
   my $nm='1234567890123';   # binary string 
   foreach my $k (1..$Ne){
      $line = readCleanLine($fh);  
      @data = split(/\s+/,$line); # split on whitespace
 
      my $id=shift(@data);
      die ("!! ERROR !! bad element number. Element # $id should be $k\n")   unless ($id==$k); 
      my $three = shift(@data);
      #$nm .= pack("l3",$data[0],$data[1],$data[2]);
      my $offset=$k*12;
      my $packed = pack("l3",$data[0],$data[1],$data[2]);
      substr ($nm,$offset,12,$packed);  # nothing useful at offset 0
   }

   $obj->{NM}=$nm;
 
   #############################################################
   # open boundaries
   print " reading open boundary information\n";

   # NOPE
   $line=readCleanLine($fh);    
   @data = split(/\s+/,$line); 
   my $nope=$data[0];
   print "$agrid has $nope open boundaries\n";
   $obj->{NOPE}=$nope;

   # NETA
   $line=readCleanLine($fh);   
   @data = split(/\s+/,$line); 
   my $neta=$data[0];
   print "with $neta open boundary nodes\n";
   $obj->{NETA}=$neta;
    
   # NVDLL, IBTYPEE, NBDV
   $obj->{IBTYPEE}=[];
   $obj->{NVDLL}=[];
   $obj->{NBDV}=[];
   my @nbdv;
   foreach my $k (1..$nope) {
      $line=readCleanLine($fh);  
      @data = split(/\s+/,$line); 
      ${$obj->{NVDLL}}[$k]=$data[0];
      ${$obj->{IBTYPEE}}[$k]=$data[1];  # not sure if adcirc atually uses this variable
      foreach my $j (0 .. ${$obj->{NVDLL}}[$k]-1){
         $line=readCleanLine($fh);  
         @data = split(/\s+/,$line); 
         ${$obj->{NBDV}}[$k][$j]=$data[0];
       }
   }

   #############################################################
   # normal flow  boundaries
   print " reading normal flux boundary information\n";

   # NBOU
   $line=readCleanLine($fh);   
   @data = split(/\s+/,$line); 
   my $nbou=$data[0];
   #print "$agrid has $nbou normal flux boundaries\n";
   $obj->{NBOU}=$nbou;

   # NVEL  unless ($id==$cnt) 
   $line=readCleanLine($fh);   
   @data = split(/\s+/,$line); 
   my $nvel=$data[0];
   #print "with $nvel flux boundary nodes\n";
   $obj->{NVEL}=$nvel;

   # NVELL,IBTYPE,NBVV,IBCONN,BARLANHT,BARLANCFSP,BARINHT,BARINCFSB,BARINCFSP
   $obj->{NVELL}=[];
   $obj->{IBTYPE}=[];
   $obj->{NBVV}=[];
   $obj->{IBCONN}=[];
   $obj->{BARLANHT}=[];
   $obj->{BARLANCFSP}=[];
   $obj->{BARINHT}=[];
   $obj->{BARINCFSB}=[];
   $obj->{BARINCFSP}=[];
   $obj->{PIPEHT}=[];
   $obj->{PIPECOEF}=[];
   $obj->{PIPEDIAM}=[];

   foreach my $k (1..$nbou) {
      $line=readCleanLine($fh);
      @data = split(/\s+/,$line);
      ${$obj->{NVELL}}[$k]=$data[0];
      ${$obj->{IBTYPE}}[$k]=$data[1];
      my $ibtype=$data[1];  
      foreach my $j (0 .. ${$obj->{NVELL}}[$k]-1){
         $line=readCleanLine($fh);   
	  @data = split(/\s+/,$line);
          ${$obj->{NBVV}}[$k][$j]=$data[0];  
          if ($ibtype == 3 or $ibtype == 13 or $ibtype == 23) { # its a supercrit outflow
             ${$obj->{BARLANHT}}[$k][$j]=$data[1];
	     ${$obj->{BARLANCFSP}}[$k][$j]=$data[2];
          }
          if ($ibtype == 4 or $ibtype == 14 or $ibtype == 24) { # its a weir
             ${$obj->{IBCONN}}[$k][$j]=$data[1];
             ${$obj->{BARINHT}}[$k][$j]=$data[2];
             ${$obj->{BARINCFSB}}[$k][$j]=$data[3];
             ${$obj->{BARINCFSP}}[$k][$j]=$data[4];

          }
          if ($ibtype == 5 or $ibtype == 15 or $ibtype == 25) { # its a pipe
             ${$obj->{IBCONN}}[$k][$j]=$data[1];
             ${$obj->{BARINHT}}[$k][$j]=$data[2];
             ${$obj->{BARINCFSB}}[$k][$j]=$data[3];
             ${$obj->{BARINCFSP}}[$k][$j]=$data[4];
             ${$obj->{PIPEHT}}[$k][$j]=$data[5];
             ${$obj->{PIPECOEF}}[$k][$j]=$data[6];
             ${$obj->{PIPEDIAM}}[$k][$j]=$data[7];

	  }
      }
   }

   close($fh);

   return ($obj);
}

#######################################################################
# $adcGrid->getVar()
#
# Method to get the hash values from the object. It is intended for use
# with grid scalar variables like the AGRID, NE, NP, NOPE, .. but It
# will just return a refence if used for other variables (e.g. XYZ), 
# which may be usefull?
#
#######################################################################
sub getVar
{
   my $obj=shift;
   my $key=shift;   # should probably check to see if the key(s) is(are) valid
   $key=uc($key);
   #if (ref($key) eq 'ARRAY'){
   #   my @Keys=@{$key}
   #   my @values;
   #   foreach my $key (@keys) {
   #       push(@values,$obj->{$key});
   #   }
   #   return @values;
   #}else{
      return $obj->{$key};
      #}
}

#######################################################################
# $adcGrid->getNode()
#
# method to get xyz data for a node
#
# e.g. 
#       my ($x, $y, $dp) = $adcGrid->getNode($nodeNumber);
# 
# or for a list of node numbers
#
#       my ($xref,$yref,$dpref = $adcGrid ->getNode(\@NIDS);
#
#       my @X=@{$xref};
#       my @Y=@{$yref};
#       my @DP=@{$dpref};
#######################################################################
sub getNode 
{
   my $obj=shift;
   my $nid=shift;
 
   if (ref($nid) eq 'ARRAY') {
       my @NIDS=@{$nid};
       my @X;
       my @Y;
       my @Z;
       foreach $nid (@NIDS){
	       # print "getting nid $nid\n";
          my $jn=unpack("l", substr($obj->{JN},($nid-1)*4,4) );
          my ($x, $y, $z)=unpack("d3", substr($obj->{XYZ},$jn*24,24) );
	  push (@X,$x);
	  push (@Y,$y);
	  push (@Z,$z);
       }
       return (\@X,\@Y,\@Z);
    }else{
	    # print "getting nid $nid\n";
        my $jn=unpack("l", substr($obj->{JN},($nid-1)*4,4) );
        my ($x, $y, $z)=unpack("d3", substr($obj->{XYZ},$jn*24,24) );
        return ($x, $y, $z);
    }
}


#######################################################################
# $adcGrid->getElement();
# 
# e.g. 
#
#    my ($n1,$n2,$n3)=$adcGrid->getElement($eid);
#
#  or
# 
#    my ($n1_ref,$n2_ref,$n3_ref)=$adcGrid(\@EIDs);
#
#######################################################################
sub getElement{
   my $obj=shift;
   my $eid=shift;
 
   if (ref($eid) eq 'ARRAY') {
       my @EIDS=@{$eid};
       my @N1;
       my @N2;
       my @N3;
       foreach $eid (@EIDS){
          my ($n1, $n2, $n3)=unpack("l3", substr($obj->{NM},$eid*12,12) );
	  push (@N1,$n1);
	  push (@N2,$n2);
	  push (@N3,$n3);
       }
       return (\@N1,\@N2,\@N3);
    }else{
          my ($n1, $n2, $n3)=unpack("l3", substr($obj->{NM},$eid*12,12) );
          return ($n1, $n2, $n3);
    }
}


#######################################################################
# $adcGrid->getNOPE()
#
# method to get the number of open boundaries
#
#######################################################################
sub getNOPE {
   my $obj=shift;
   return $obj->{NOPE};
}       



#######################################################################
# $adcGrid->getOpenBnd ($openBndID)
#
# method returns boundary  NVDLL, IBTYPEE followed by a
# reference to a list of nodes for an open boundary
# 
# e.g. 
#         @list = $adcGrid->getOpenBnd(1);  # get open bnd number 1
#         $nvdll=shift(@list);
#         $ibtypee=shift(@list);
#         @nodes=@{shift(@list)};
#        
#######################################################################
sub getOpenBnd {
   my $obj=shift;
   my $bndID=shift; 
   my $ibtypee=${$obj->{IBTYPEE}}[$bndID];
   $ibtypee = 0 unless (defined $ibtypee);  # since this isn't actually found in most fort.14 files
   #  print "ibtypeeeee $ibtypee\n";
   my $nvdll=${$obj->{NVDLL}}[$bndID];
   #  print "nvdlllll $nvdll\n";

   my $nodesref=${$obj->{NBDV}}[$bndID];

   return ($nvdll, $ibtypee, $nodesref);
}


#######################################################################
# $adcGrid->getFluxBnd ($openBndID)
#
# method returns boundry NVELL, IBTYPE followed by refrences to
# list of nodes (NBDV),and if appriprate IBCONN, BARLANHT, etc...
# 
# e.g. 
#         @list = $adcGrid->getOpenBnd(1);  # get open bnd number 1
#        
#        $nvell=shift(@list);
#         $ibtype=shift(@list);
#         @nbvv=@{shift(@list)};
#
#         @ibconn=@{shift(@list)} if ($ibtype == 4 or $ibtype == 24);
#         @weirHt=@{shift(@list)} if ($ibtype == 4 or $ibtype == 24);
#        
#######################################################################
sub getFluxBnd {
   my $obj=shift;
   my $bndID=shift; 
   my $ibtype=${$obj->{IBTYPE}}[$bndID];
   my $nvell=${$obj->{NVELL}}[$bndID];
   my $nbdv=${$obj->{NBVV}}[$bndID];
   
   if ($ibtype == 3 or $ibtype == 13 or $ibtype == 23) { 
     return ($nvell, $ibtype, $nbdv,${$obj->{BARLANHT}}[$bndID],
                                    ${$obj->{BARLANCFSP}}[$bndID] );
   }
   if ($ibtype == 4 or $ibtype == 14 or $ibtype == 24) { 
     return ($nvell, $ibtype, $nbdv,${$obj->{IBCONN}}[$bndID],
	                            ${$obj->{BARINHT}}[$bndID],
                                    ${$obj->{BARINCFSB}}[$bndID],
                                    ${$obj->{BARINCFSP}}[$bndID] );
   }
   if ($ibtype == 5 or $ibtype == 15 or $ibtype == 25) { 
     return ($nvell, $ibtype, $nbdv,${$obj->{IBCONN}}[$bndID],
	                            ${$obj->{BARINHT}}[$bndID],
                                    ${$obj->{BARINCFSB}}[$bndID],
                                    ${$obj->{BARINCFSP}}[$bndID],
                                    ${$obj->{PIPEHT}}[$bndID],
                                    ${$obj->{PIPECOEF}}[$bndID],
                                    ${$obj->{PIPEDIAM}}[$bndID] );
   }
   return ($nvell, $ibtype, $nbdv);
}


#######################################################################
# reads the next line from $fh (which must be already open)
# removes trailing and leading white space and new line
#
# intended to be private
#
# e.g. 
#
#       open $fh, '<somefile.txt';
#       $cleanLine = readCleanLine($fh); 
#       $anotherLine = readCleanLine($fh); 
#       close($fh);
#######################################################################
sub readCleanLine 
{
   my $fh=shift;
	# my $obj=shift;
   my $line=<$fh>;
   chomp ($line);
   $line=~ s/^\s+//;        
   $line=~ s/\s+$//; 
   return ($line);
}




1;

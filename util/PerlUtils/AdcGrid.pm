#!/usr/bin/env perl
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
# Author: Nathan Dill, natedill@gmail.com
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

package AdcGrid;

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
# sub load13($fort13)
#
# loads the nodal attribute data into the object so you can get it out
# 
#
# attributes are stored as a hash of hashes using the attribute 
# name as the key. within each attribute hash we have the units, 
# values per node, default value, data etc..
#
# data are stored as binary strings of doubles (like the dp data) 
# with nothing useful at the first (zero) offset.
#
# if there are more than one value per node, the data are stored in 
# an array of these strings
#
#######################################################################
sub load13{
   my $obj=shift;
   my $fort13=shift;
   
   open my $fh, "<$fort13" or die "AdcGrid can't open $fort13\n";
  
   ###########################################################
   # read AGRID
   my $agrid = readCleanLine($fh);
   print "reading: $agrid\n";
   $obj->{AGRID_13}=$agrid;

   ###########################################################
   # read NumOfNodes
   my $nn =  readCleanLine($fh);
   die "!!! Danger, Will Robinson !!! NumOfNodes does not match NP\n" if ($nn != $obj->{NP});
   
   ###########################################################
   # read NAttr
   my $nattr = readCleanLine($fh);
   print "$fort13 has $nattr nodal attributes\n";
   $obj->{NATTR}=$nattr;


   # 1st loop over the attributes
   # 
   $obj->{ATTRNAME}=[];   # a list of the attributes, 

   my $cnt=$nattr;
   while ($cnt--) {
       
      # AttrName
      my $attrName = readCleanLine($fh);
      $attrName=uc($attrName);
      push (@{$obj->{ATTRNAME}},$attrName);
     
      # units
      my $units = readCleanLine($fh);
      my $valuesPerNode = readCleanLine($fh);
      my $tmp = readCleanLine($fh);
      my @defaultAttrVal = split(/\s+/,$tmp);
      
       $obj->{$attrName}{UNITS}=$units;
       $obj->{$attrName}{VALUESPERNODE} = $valuesPerNode;
       $obj->{$attrName}{DEFAULTATTRVAL} = \@defaultAttrVal;
       $obj->{$attrName}{ATTRVAL} = ();   # an array that will hold the attribute values 
                                            # (one binary string for each value per node)
    }

   # 2nd loop over the attributes (set all to default values)
   foreach my $attrName (@{$obj->{ATTRNAME}}){
       print "setting default values for $attrName\n";
       my $kk=1;
       foreach my $val (@{$obj->{$attrName}{DEFAULTATTRVAL}}){
            print "   $attrName $kk of $obj->{$attrName}{VALUESPERNODE} = $val\n";
            my $str='123455678';
            foreach my $n (1..$obj->{NP}){   #loop to fill in the string with defaults
                my $packed=pack("d1",$val);
                my $offset=($n*8);
                substr ($str,$offset,8,$packed); 
            }
        push (@{$obj->{$attrName}{ATTRVAL}},$str);
        $kk++;
       }
   }

   # 3rd loop (set non default values)
   $cnt=$nattr;
   while($cnt--){
      my $attrName = readCleanLine($fh);
       print "setting non-default values for $attrName\n";
      $attrName=uc($attrName);
      my $numNotDefault = readCleanLine($fh);
      $obj->{$attrName}{NUMNODESNOTDEFAULTVAL} = $numNotDefault;
     foreach my $ii (1..$numNotDefault){
         my $tmp = readCleanLine($fh);
         my @data=split(/\s+/,$tmp);
         my $nid=shift (@data);
         foreach my $kk (0..$obj->{$attrName}{VALUESPERNODE}-1){
            my $val=shift(@data);
            my $packed=pack("d1",$val);
            my $offset=$nid*8;
            substr (${$obj->{$attrName}{ATTRVAL}}[$kk],$offset,8,$packed);
         }
      }        
   }

}


#######################################################################
# sub getNodalAttributeValue ($attrName,$whichVal,\@NIDS)
#
# returns the value or reference to a list values for a node or referenced
# list of nodes.  $whichVal is one from 1 to the number of values per node
# only can get one value per node at a time
#
# e.g.
#      my ($ValRef,$min,$max)=$adcGrid->getNodalAttributeValue('Mannings_n_at_sea_floor',1,\@NIDS)
#
# also returns the minimum and maximum if a referenced list is given
#######################################################################
sub getNodalAttributeValue{
   my $obj = shift;
   my $attrName= shift;
   $attrName=uc($attrName);
   my $whichVal=shift;  # only gives one value per node, this specifies which one
   my $nid=shift;

    my $min;
    my $max;
 
    if (ref($nid) eq 'ARRAY') {
       my @NIDS=@{$nid};
       my @VALS;
       my $ii=0;
       foreach $nid (@NIDS){
	       # print "getting nid $nid\n";
          
          my $val=unpack("d1", substr(${$obj->{$attrName}{ATTRVAL}}[$whichVal-1],$nid*8,8) );
          if ($ii==0) {
               $min=$val;
               $max=$val;
               $ii=1;
          }
          $min = $val if ($val < $min);
          $max = $val if ($val > $max);          

	  push (@VALS,$val);
       }
       return (\@VALS, $min, $max);
    }else{
        my $val=unpack("d1", substr($obj->{$attrName}{ATTRVAL}[$whichVal-1],$nid*8,8) );
        return $val;
    }
   

}


#######################################################################
# sub setNodalAttributeValue ($attrName,$whichVal,\@NIDS,\@VALS)
#
# sets nodal attribute value, can only set one value per node (i.e. $whichVal)
#
# e.g.
#      $adcGrid->setNodalAttributeValue('Mannings_n_at_sea_floor',$whichVal,\@NIDS,\@ManningNVals)
# 
#
#######################################################################
sub setNodalAttributeValue{

   my $obj = shift;
   my $attrName= shift;
   $attrName=uc($attrName);
   my $whichVal=shift;  # only gives one value per node, this specifies which one
   $whichVal--;   # since we expect user to provide 1 indexed value (like Fortran)
   my $nid=shift;
   my $val=shift;


   if (ref($nid) eq 'ARRAY') {
       my @NIDS=@{$nid};
       my @VALS=@{$val};
       my $ii=0;
       foreach $nid (@NIDS){
	       # print "getting nid $nid\n";
          
          #my $val=unpack("d1", substr(${$obj->{$attrName}{ATTRVAL}}[$whichVal-1],$nid*8,8) );
         
          my $packed=pack("d1",$VALS[$ii]);
          my $offset=($nid*8);
          substr (${$obj->{$attrName}{ATTRVAL}}[$whichVal],$offset,8,$packed); 
          $ii++;
        }
    }else{
        my $packed=pack("d1",$val);
        my $offset=($nid*8);
        substr (${$obj->{$attrName}{ATTRVAL}}[$whichVal],$offset,8,$packed); 
    }
}


#############################################################################3
#  sub writeFort13('fort.13');
#
#  
#   e.g.   $adcGrid->writeFort13('newfort.13');
#
#  write the fort.13 file
#
################################################################################
sub writeFort13{
   my $obj=shift;
   my $fort13=shift;
   
   open F13, ">$fort13" or die "cant open $fort13 for writing";
   print F13 "$obj->{AGRID_13} by AdcGrid.pm\n";
   print F13 " $obj->{NP}\n";
   print F13 "$obj->{NATTR}\n";
  
   my $nattr=$obj->{NATTR};

   foreach my $attrName (@{$obj->{ATTRNAME}}){
      print "writing $attrName 1\n";
      my $lcname=lc($attrName);
      print F13 "$lcname\n";
      print F13 " $obj->{$attrName}{UNITS}\n";
      print F13 "$obj->{$attrName}{VALUESPERNODE}\n";
      my @defValues=@{$obj->{$attrName}{DEFAULTATTRVAL}};
      foreach my $defVal (@defValues){
         print F13 "$defVal ";
      }
      print F13 "\n";
   }
  
   foreach my $attrName (@{$obj->{ATTRNAME}}){
      print "writing $attrName 2\n";
       my @defValues=@{$obj->{$attrName}{DEFAULTATTRVAL}};
       my @NotDef=();
       foreach my $n (1..$obj->{NP}){
          $NotDef[$n]=0;
       }
       my $numValsPerNode=$obj->{$attrName}{VALUESPERNODE};
  
       # count up the non default values
       my $numNotDefault=0;
       my $whichVal=0;
       foreach my $defVal (@defValues){
           foreach my $nid (1..$obj->{NP}){
               my $val=unpack("d1", substr($obj->{$attrName}{ATTRVAL}[$whichVal],$nid*8,8) ); 
               if ($val != $defVal){
                   if ($NotDef[$nid] == 0){
                      $NotDef[$nid]=1;
                      $numNotDefault++;
                   }
               }            

           }
           $whichVal++;
       }
      
       #write the non default values
       my $lcname=lc($attrName);
       print F13 "$lcname\n";
       print F13 "$numNotDefault\n";

       foreach my $nid (1..$obj->{NP}){
           next if ($NotDef[$nid] == 0);
           
           #print F13 "$nid ";
           my $str= sprintf("%10d",$nid);
           print F13 "$str";

           foreach my $ii (0..$numValsPerNode-1){
               my $val=unpack("d1", substr($obj->{$attrName}{ATTRVAL}[$ii],$nid*8,8) ); 
              # print F13 "$val ";
               my $str=sprintf("%15.6f",$val);
               print F13 "$str";
           }
           print F13 "\n";
       }
    }
           
    close F13;

} 





#######################################################################
# sub findNodesInPoly
#
# returns a list of nodes found within a polygon
#
# e.g.
#      my (@foundNodes)=$adcGrid->findNodesInPoly(\@px,\@py,$printFound);
#
#     where @px and @py are lists of the x and y coordinates 
#     of the polygon verticies (not necessarily closed)
#
#     if $printFound is defined the found nodes (id, x, y, z) 
#     will be printed to stdout 
#
####################################################################### 
sub findNodesInPoly{

   my ($obj,$pxref,$pyref,$printFound)=@_;

   $printFound=0 unless (defined $printFound);

   # dereference arrays
   my @PX=@{$pxref};
   my @PY=@{$pyref};

   #find min/max coordinates to exclude points

   my $minx=$PX[0];
   my $maxx=$PX[0];
   foreach my $x (@PX){
      $minx=$x if $x < $minx;
      $maxx=$x if $x > $maxx;
   }
   my $miny=$PY[0];
   my $maxy=$PY[0];
   foreach my $y (@PX){
      $miny=$y if $y < $miny;
      $maxy=$y if $y > $maxy;
   }


   my @foundNodes=();

   # loop over the nodes and see if they are in the polygon

   foreach my $nid (1..$obj->{NP}){
       my ($x, $y, $dp) = $obj->getNode($nid);
       next if $x < $minx;
       next if $x > $maxx;
       next if $y < $miny;
       next if $y > $maxy;
       my $inpoly=pointInPoly($x,$y,$pxref,$pyref);
       push (@foundNodes, $nid ) if $inpoly;
       print "$nid $x $y $dp\n" if ($inpoly & $printFound);
   }


   return @foundNodes;
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


#########################################################################
# sub pointInPoly     
#
# the subroutine will determine if a point ($x,$y) is in a polygon 
# described by arrays @px,@py, note: polygon must be closed
#
# usage:
#
# $inpoly=PolyTools::pointInPoly($x,$y,\@px,\@py);
#
# returns $inpoly=1 if the point is in the polygon, $inpoly=0 otherwise
#
#########################################################################
sub pointInPoly {  # $x $y \@px \@py    note: polygon described by vectors px,py must be closed 
   
   my $crs;
   my $inPoly=0;
	
   my $x = $_[0];
   my $y = $_[1];
   my @px = @{$_[2]}; # dereference to get arrays from argument
   my @py = @{$_[3]};
   
   my $nsegs=@px;

   my $wn=0;   # the winding number

   my $i=0;
   while ($i<$nsegs-1) {
        
     # test if at least one vertex is right of the point
     if ( ($px[$i] > $x) ||  ($px[$i+1] > $x) ) {

	 if (     ($py[$i] < $y) && ($py[$i+1] > $y) ) {  # this one crosses
            $crs= ($px[$i]-$x)*($py[$i+1]-$y) - ($px[$i+1]-$x)*($py[$i]-$y) ;
	    $wn++ if ($crs > 0) ; # upward cross on the right
         }elsif (($py[$i] > $y) && ($py[$i+1] < $y) ) {
            $crs= ($px[$i]-$x)*($py[$i+1]-$y) - ($px[$i+1]-$x)*($py[$i]-$y) ;
	    $wn-- if ($crs < 0); #downward cross on the right
         }
      }
      $i++;
   }
   $inPoly=1 if ($wn !=  0);

   return $inPoly;

}





1;

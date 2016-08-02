#!/usr/bin/env perl
#######################################################################
#
# This is a Perl module for dealing with adcirc grids in a OO fashion.
# 
#----------------------------------------------------------------------
# example usage:
#
# # load the module
#
#    use AdcircUtils::AdcGrid;
#
# # create an AdcGrid object,
#
#    $adcGrid=AdcGrid->new();
#
# # load a grid file
#
#   $adcGrid->loadGrid('fort.14');
#
# # After you've you created the object and loaded a grid
# # you can do stuff with it.
# 
# # for example:
#
# # get the AGRID comment line
#
#    $agrid=$adcGrid->getVAR('AGRID');
# # or   
#    $agrid=$adcGrid->{AGRID};  # the object is just a hash ref
#
# # get the number of elements
# 
#    $ne=$adcGrid->getVar('NE');
# # or   
#    $ne=$adcGrid->{NE};
#
# # note: the variable names are for the most part identical
# # to those used on the adcirc website to describe
# # the fort.14 file.
# 
# # get the position of a node 
#
#    $someNodeNumber=784
#    ($x,$y,$dp)=$adcGrid->getNode($someNodeNumber);
#
# # get the positions all the nodes
#
#    my $np = $adcGrid->getVar('NP');
#    @NIDS=(1..$np);
#    ($xref,$yref,$dpref)=$adcGrid->getNode(\@NIDS);
#    @X=@{$xref};
#    @Y=@{$yref};
#    @DP=@{$dpref};
#    
# There is more you can do. Please read the comments below associated 
# with the various methods to get a further description description of
# what they do.  
# 
####################################################################### 
# Author: Nathan Dill, natedill@gmail.com
#
# Copyright (C) 2014-2016 Nathan Dill
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

# you can uncomment the line below and the makeElementQuadTree subroutine
# near the end of this package to get access to method to build a
# ElementQuadtree object from the AdcGrid object. Note that ElementQuadTre
# depends on GD and Storable modules as well.G 
# use AdcircUtils::ElementQuadTree;  # for the subroutine that builds a quadtree 
                                     # this module requires GD and Storable as well


#  you'll need these mapping modules if you want to use the coordinate convertion
#  capabilities -- this is not fully implemented yer 
#use lib 'c:/myPerl';
#use Mapping::UTMconvert;  # for the subroutine to conrvet cordinates
#use Mapping::P_spcs83;    #




#######################################################################
# create a new AdcGrid object
#
# e.g.
#
#  my $gridFileName='fort.14';
#  my $adcGrid = AdcGrid->new( $gridFileName );
#
#
#  or 
#
#  my $adcGrid=AdcGrid->new();
#  $adcGrid->loadGrid( $gridFileName );
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
  
   # to be compatible with older version of adcGrid.pm
   my $gridFileName=shift;
   if (defined $gridFileName){
      $obj->loadGrid($gridFileName);
   } 
   return $obj;

}


sub loadGrid{
   my $obj=shift;

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

   
   open my $fh, "<$gridFileName" or die "ERROR: AdcGrid.pm: can't open $gridFileName\n";
  
   ###########################################################
   # read AGRID
   my $agrid = readCleanLine($fh);
   print "INFO: AdcGrid.pm: reading: $agrid\n";
   $obj->{AGRID}=$agrid;

   ###########################################################
   # number of elements and nodes
   $line = readCleanLine($fh);
   my ($Ne, $Np) = split(/\s+/,$line); # split on whitespace
   print "INFO: AdcGrid.pm: $agrid has $Ne elements and $Np nodes\n";
   $obj->{NE}=$Ne;
   $obj->{NP}=$Np;

   ###########################################################
   # read node positions
   print "INFO: AdcGrid.pm: reading nodes\n"; 
   my $cnt=1;
   my $jn='';  # these are binary strings stored using pack
   my $xyz='1234567890122345678901234';

   foreach my $k (1..$Np){
      $line = readCleanLine($fh);  
      @data = split(/\s+/,$line); # split on whitespace
      
      my $j=shift(@data); # this should be the node number
      unless ($j==$k){
         print "WARNING: AdcGrid.pm: non-consecutive node number at node $k\n";
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
   print "INFO: AdcGrid.pm: reading elements\n";
   my $three;
   my $nm='1234567890123';   # binary string 
   foreach my $k (1..$Ne){
      $line = readCleanLine($fh);  
      @data = split(/\s+/,$line); # split on whitespace
 
      my $id=shift(@data);
      die ("ERROR: AdcGrid.pm: bad element number. Element # $id should be $k\n")   unless ($id==$k); 
      my $three = shift(@data);
      #$nm .= pack("l3",$data[0],$data[1],$data[2]);
      my $offset=$k*12;
      my $packed = pack("l3",$data[0],$data[1],$data[2]);
      substr ($nm,$offset,12,$packed);  # nothing useful at offset 0
   }

   $obj->{NM}=$nm;
 
   #############################################################
   # open boundaries
   print "INFO: AdcGrid.pm: reading open boundary information\n";

   # NOPE
   $line=readCleanLine($fh);    
   @data = split(/\s+/,$line); 
   my $nope=$data[0];
   print "INFO: AdcGrid.pm: $agrid has $nope open boundaries\n";
   $obj->{NOPE}=$nope;

   # NETA
   $line=readCleanLine($fh);   
   @data = split(/\s+/,$line); 
   my $neta=$data[0];
   print "INFO: AdcGrid.pm: with $neta open boundary nodes\n";
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
   print "INFO: AdcGrid.pm: reading normal flux boundary information\n";

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
         # $n1=0 unless (defined $n1);
         # $n2=0 unless (defined $n2);
         # $n3=0 unless (defined $n3);
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
#         @list = $adcGrid->getFluxBnd(1);  # get open bnd number 1
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
# sub addNodalAttribute ($attrName,$units,$numberValuesPerNode,$defaultValuesRef)
#
# # this creates an empty nodal attribute with default values only
#
sub addNodalAttribute {
   my $obj=shift;
   $obj->{NATTR}=0 unless (defined $obj->{NATTR});
   my ($attrName,$units,$nValPerNode,$defValRef)=@_;
   my @defValues=@{$defValRef};
   $attrName=uc($attrName);
   # check to see if this one is already estabilshed
   if (defined $obj->{$attrName}){
       print "already have an attribute named $attrName\n";
       print " doing nothing, - use setNodalAttribute if you want to change it\n";
       return $obj->{NATTR};
   }

   $obj->{ATTRNAME}=[]  unless defined ( $obj->{ATTRNAME});  # a list of the attributes,
   push (@{$obj->{ATTRNAME}},$attrName);
   $obj->{$attrName}{UNITS}=$units;
   $obj->{$attrName}{VALUESPERNODE} = $nValPerNode;
   $obj->{$attrName}{DEFAULTATTRVAL} = \@defValues;
   $obj->{$attrName}{NUMNODESNOTDEFAULTVAL}=0;
   $obj->{$attrName}{ATTRVAL}=();  
   # create default value string 
   foreach my $val (@{$obj->{$attrName}{DEFAULTATTRVAL}}){
        my $str='123455678';
        foreach my $n (1..$obj->{NP}){   #loop to fill in the string with defaults
             my $packed=pack("d1",$val);
             my $offset=($n*8);
             substr ($str,$offset,8,$packed); 
        }
        push (@{$obj->{$attrName}{ATTRVAL}},$str);
   }
   return $obj->{NATTR}++;
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

#my $sstr=$obj->{$attrName}{ATTRVAL}[$whichVal];
#my $len = length ($sstr);
#print "len $len\n";
#print "nid $nid, which val $whichVal\n";
#print " offset $offset\n";
#print "packed, $packed, $VALS[$ii]\n";  

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
      print "writing $attrName part 1\n";
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
      print "writing $attrName part 2\n";
       my @defValues=@{$obj->{$attrName}{DEFAULTATTRVAL}};
#print "defvals = @defValues\n";
#sleep(2);
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
#print "defVal $defVal, nid $nid, whichval $whichVal,  attrName $attrName\n";
#sleep(1);

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
#  sub write14
#
#  writes out the grid file
#
#  e.g. $adcGrid->write14('newfort.14');
#
######################################################################
sub write14{
   my $obj=shift;
   my $fname=shift;
   open OUT, ">$fname"  or die "cant open $fname\n";
   print OUT "$obj->{AGRID}\n";
   my $ne=$obj->{NE};
   my $np=$obj->{NP};
   print OUT "$ne $np\n";
 
   my ($x,$y,$z)=$obj->getNode([0..$np]);
   my @X=@{$x};
   my @Y=@{$y};
   my @Z=@{$z};

   foreach my $n (1..$np){
      print OUT "$n $X[$n] $Y[$n] $Z[$n]\n";
   }
   my ($n1,$n2,$n3)=$obj->getElement([0..$ne]);
   my @N1=@{$n1};
   my @N2=@{$n2};
   my @N3=@{$n3};
   foreach my $e (1..$ne){
      print OUT "$e 3 $N1[$e] $N2[$e] $N3[$e]\n";
   }

   my $nope=$obj->getVar('NOPE');
   print OUT "$nope   !NOPE\n";

   my $neta=$obj->getVar('NETA');
   print OUT "$neta   !NETA\n";

   foreach my $op (1..$nope){
   
      my ($nvdll, $ibtypee, $bndNodesRef)=$obj->getOpenBnd($op);
      print OUT "$nvdll $ibtypee  !NVDLL IBTYPEE\n";
      my @BND=@{$bndNodesRef};
      foreach my $bndNode (@BND){
         print OUT "$bndNode\n";
      }
   }
   my $nbou=$obj->getVar('NBOU');
   print OUT "$nbou   !NBOU\n";

   my $nvel=$obj->getVar('NVEL');
   print OUT "$nvel   !NVEL\n";

   foreach my $fb (1..$nbou){
   
      my @data=$obj->getFluxBnd($fb);
      my $nvell=shift @data;
      my $ibtype=shift @data;
      print OUT "$nvell $ibtype  !NVELL IBTYPE bnd $fb\n";

      my $nbdv=shift @data;
      if ($ibtype == 3 or $ibtype == 13 or $ibtype == 23) { # its a supercrit outflow
         my $ht=shift @data;
         my $cf=shift @data;
         my @HT=@{$ht};
         my @CF=@{$cf};
         my @NBDV=@{$nbdv};
         foreach my $n (@NBDV){
             $ht=shift(@HT);
             $cf=shift(@CF);
             print OUT "$n $ht $cf\n";
         }
       }elsif($ibtype == 4 or $ibtype == 14 or $ibtype == 24){  #weir
          my $ib=shift @data;
          my $ht=shift @data;
          my $cfsb=shift @data;
          my $cfsp=shift @data;
          my @NBDV=@{$nbdv}; 
          my @IB=@{$ib};
          my @HT=@{$ht};
          my @CFSB=@{$cfsb};
          my @CFSP=@{$cfsp};
          foreach my $n (@NBDV){
             $ht=shift(@HT);
             $cfsb=shift(@CFSB);
             $cfsp=shift(@CFSP);
             $ib=shift(@IB);
             print OUT "$n $ib $ht $cfsb $cfsp\n";
          }
       }elsif ($ibtype == 5 or $ibtype == 15 or $ibtype == 25){ #weir with pipe
          my $ib=shift @data;
          my $ht=shift @data;
          my $cfsb=shift @data;
          my $cfsp=shift @data;
          my $pht=shift @data;
          my $pcf=shift @data;
          my $pdia=shift @data;
          my @NBDV=@{$nbdv}; 
          my @IB=@{$ib};
          my @HT=@{$ht};
          my @CFSB=@{$cfsb};
          my @CFSP=@{$cfsp};
          my @PHT=@{$pht};
          my @PCF=@{$pcf};
          my @PDIA=@{$pdia};
          foreach my $n (@NBDV){
             $ht=shift(@HT);
             $cfsb=shift(@CFSB);
             $cfsp=shift(@CFSP);
             $ib=shift(@IB);
             $pht=shift(@PHT);
             $pcf=shift (@PCF);
             $pdia=shift(@PDIA);
             print OUT "$n $ib $ht $cfsb $cfsp $pht $pcf $pdia\n";
          }
       }else{
          my @NBDV=@{$nbdv};
          foreach  my $n (@NBDV){
             print OUT "$n\n";
          }
       }
   }#end nbou loop
   close (OUT);

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




sub convertCoords{
   my $obj=shift;
   my ($toOrFrom,$coordSystem,$zone,$vMultiplier,$hMultiplier)=@_;
  
   if (uc($coordSystem) eq 'UTM'){ # use UTMconvert
       if (uc($toOrFrom) eq 'TO'){
            print "converting Coordinates from Geographic to UTM NAD83\n";
       
       }elsif (uc($toOrFrom) eq 'FROM'){
            print "converting Coordinates from UTM to Geographic NAD83\n";
       
       }else{
            print "!! - Invalid toOrFrom specified - !!\n";
            print "toOrFrom can only be \'TO\' or \'FROM\'\n";
       }



   }elsif (uc($coordSystem)  eq 'SP'){ # use P_spcs83
       
       if (uc($toOrFrom) eq 'TO'){
            print "converting Coordinates from Geographic to UTM NAD83\n";
       
       }elsif (uc($toOrFrom) eq 'FROM'){
            print "converting Coordinates from UTM to Geographic NAD83\n";
       
       }else{
            print "!! - Invalid toOrFrom specified - !!\n";
            print "toOrFrom can only be \'TO\' or \'FROM\'\n";
       }

   }else{

       print "!! - Invaid coordSystem specifed - !!\n";
       print "coordSystem can \'SP\' or \'UTM\'\n";
       sleep(1000);
   }


}


################################################
# sub genNeighborTables
#
# e.g. $obj->genNeighborTables()
#
# generates a table of the node neighbors for 
# each node
# 
# the table should not have any duplicates,
#
# neighbor nodes are listed in a ccw order around 
# the node
# 
# for node that are completely surrounded the 
# the first node is repeated as the last node
# for nodes on the boundary the first and last
# are not the same. 
##################################################
sub genNeighborTables{
    my $obj=shift;

    # build a node-node edge table
    # and a node-element table
    # tables are stored as a hash with 
    # node pairs (or element-node) as a 
    # key e.g. "1,2" would mean node1-node2
    # is an edge, or element1 contains node2
    #
    # for the edge table a value 1 indicates 
    # a boundary edge and a value of 2 indicates
    # a shared edge.  for the the node-element
    # table a defined value (1) indcates
    # a connection
    my %edgeTable;
    my %nodeEleTable;
  
    # loop through elements
    foreach my $eid (1..$obj->{NE}){
       my ($n1,$n2,$n3) = $obj->getElement($eid);
       

       # edge table
       my $edg1=[$n1,$n2];
       my $edg2=[$n2,$n3];
       my $edg3=[$n3,$n1];

       foreach my $edg ($edg1, $edg2, $edg3){
          my ($tn1,$tn2)=sort { $a <=> $b } @{$edg}; # sort so we only have the upper diagonal
          my $str="$tn1,$tn2";
          unless (defined $edgeTable{"$str"}){
             $edgeTable{"$str"}=1;
          }else{
             $edgeTable{"$str"}++;
          }
       } 

       #node-element table       
       $nodeEleTable{"$eid,$n1"}=1;
       $nodeEleTable{"$eid,$n2"}=1;
       $nodeEleTable{"$eid,$n3"}=1;

    }
    $obj->{EDGETABLE}=\%edgeTable;
print " done making table hashes\n";
 
 
    # make an array from the node-element table
    my @NODE_ELE=();
    foreach my $pair (keys %nodeEleTable){
         my ($eid,$nid)=split (/,/,$pair);
         push @{$NODE_ELE[$nid]}, $eid;
    }

    my @NodeNeighbors;        # use index starting at 1
    push @NodeNeighbors, [];  # there will be nothing useful at index 0
print " done making node_ele array\n";

    foreach my $n (1..$obj->{NP}){   # loop over nodes to build node-node table
      print "node $n\n" unless (defined $NODE_ELE[$n]); 
        # ge the element for this node
        my $elesRef=$NODE_ELE[$n]; 
        my @ELEs=@{$elesRef};
        die "whoaa!! no elements for node $n" unless (@ELEs);
#print"n  ELEs, $n,  -- @ELEs\n";
        # get the node connectivity table for these elements
        my ($n1,$n2,$n3)=$obj->getElement(\@ELEs);
        my @N1=@{$n1};
        my @N2=@{$n2};
        my @N3=@{$n3};

#print "N1 @N1\n";
#print "N2 @N2\n";
#print "N3 @N3\n";
        # Check to see if this node is on a boundary
        # If it is, start with the node ccw along the boundary
        # from it.  If not it doesn't matter where we start
        # if any of the values in the edge table are 1 it is on 
        # a boundary
                
        # loop over elements neighbors and find the edges
        # going out from the node
        # if it is on a boundary only one of these will
        # will have a value of 1 in the edge table, and 
        # it will be the one ccw along the bnd from the 
        # from the node.
        my $onBnd=0;
        my $bndEle=0;        
        foreach my $i (0..$#ELEs){
            #my ($n1,$n2,$n3)=$obj->getElement($ele);
            my ($s1,$s2);
               
            ($s1,$s2)=sort {$a<=>$b} ($N1[$i],$N2[$i]) if $n==$N1[$i];
            ($s1,$s2)=sort {$a<=>$b} ($N2[$i],$N3[$i]) if $n==$N2[$i];
            ($s1,$s2)=sort {$a<=>$b} ($N3[$i],$N1[$i]) if $n==$N3[$i];
            if ($edgeTable{"$s1,$s2"} == 1){
               $onBnd=1;
               $bndEle=$ELEs[$i];
            }
        }
 
        # reorder the ELE list to put bndele in the first position
        # if necessary
        unless ($bndEle==0){
            while (1){
              last if $bndEle==$ELEs[0];
              my $el=shift(@ELEs);
              push @ELEs, $el;
              my $nn1 = shift (@N1); push @N1, $nn1;
              my $nn2 = shift (@N2); push @N2, $nn2;
              my $nn3 = shift (@N3); push @N3, $nn3;
            } 
            # after reordiring we need to re-get the node arrays
            #my ($n1,$n2,$n3)=$obj->getElement(\@ELEs);
            #@N1=@{$n1};
            #@N2=@{$n2};
            #@N3=@{$n3};
        }
         
        # starting with the first element 
        # find the going out neighbor
        # find the going in neighbor 
        # use the going in edge to find the
        # next element
        # jump  out of the loop when we've used 
        # up all the elements.
        my @Neighbors=();
        my $el =  shift (@ELEs);
         $n1 = shift (@N1);
         $n2 = shift (@N2);
         $n3 = shift (@N3);
     #   push @ELEs, $el;
     #   push @N1, $n1;
     #   push @N2, $n2;
     #   push @N3, $n3;
        my ($in1,$in2);
        my @neigh;
        if ($n==$n1){
           ($in1,$in2) = sort {$a <=> $b} ($n3,$n1);
           @neigh=($n2, $n3);
        }elsif($n==$n2){
           ($in1,$in2) = sort {$a <=> $b} ($n1,$n2);
           @neigh=($n3,$n1);
        }elsif($n==$n3){
           ($in1,$in2) = sort {$a <=> $b} ($n2,$n3);
           @neigh=($n1,$n2);
        }else{
           die "!! Whoaa - something wrong here\n"    ;
        }
        push @Neighbors, @neigh;
        my $inEdg="$in1,$in2";
        # now loop through the elements repeatedlt until
        # you find the next element, then splice it out of the
        # arrays. loop will exit when all elements have 
        # been use up
        #my $knt=0;
        while (@N1){
# print "knt $knt\n";
# print "N1 @N1\n";
# print "N2 @N2\n";
# print "N3 @N3\n";
#sleep(1);
              my $n1 = shift (@N1);  # take the first element from the list
              my $n2 = shift (@N2);
              my $n3 = shift (@N3);
              #my $el = shift (@ELEs);   
              # find where the previous in in-edge is the same as the
              # out-edge of the next element 
              my ($out1,$out2);
              if ($n==$n1){
                  ($in1,$in2) = sort {$a <=> $b} ($n3,$n1);
                  ($out1,$out2)=sort {$a <=> $b} ($n,$n2);
              }elsif($n==$n2){
                  ($in1,$in2) = sort {$a <=> $b} ($n1,$n2);
                  ($out1,$out2)=sort {$a <=> $b} ($n,$n3);
              }elsif($n==$n3){
                  ($in1,$in2) = sort {$a <=> $b} ($n2,$n3);
                  ($out1,$out2)=sort {$a <=> $b} ($n,$n1);
              }else{
                  die "!! Whoaa - something wrong here -2\n"    
              }
              my $outEdg="$out1,$out2";
              if ($inEdg eq $outEdg){    # this is the element
                  $inEdg="$in1,$in2";  # set the next in-edge
                  push @Neighbors, $n3 if $n==$n1;  #add this node to the neighbor list
                  push @Neighbors, $n1 if $n==$n2;  #add this node to the neighbor list
                  push @Neighbors, $n2 if $n==$n3;  #add this node to the neighbor list
                  #splice @N1, $knt, 1;
                  #splice @N2, $knt, 1;
                  #splice @N3, $knt, 1;
                  #splice @ELEs, $knt, 1;
              }else{                 # its not the element, add it back to the list
                  push @N1, $n1;
                  push @N2, $n2;
                  push @N3, $n3;
              }
                  
              #$knt++;
        }
   
        push @NodeNeighbors, \@Neighbors;

    } # end loop over nodes
 
    $obj->{NODENEIGHBORS}=\@NodeNeighbors;

} # end sub genNeighborTables


sub writeNodeNeighborTable{
    my $obj=shift;
    my $outFile=shift;
    open OUT, ">$outFile";
    my @NT=@{$obj->{NODENEIGHBORS}};
    foreach my $n (1..$obj->{NP}){
        my @list=@{$NT[$n]};
        print OUT "$n @list\n";
       # foreach my $neigh (@list){
       #    print OUT " $neigh";
       # }
       # print OUT "/n";
    }
    close(OUT);
}

# sub to return the list neighboring nodes if a single node
sub getNeighborNodes{
    my $obj=shift;
    my $node=shift;
    my @NT=@{$obj->{NODENEIGHBORS}};
    my @list=@{$NT[$node]};  
    return @list;
}

#############################################################
# sub makeElementQuadTree
#
# a subroutine that makes an ElementQuadTree object of the grid 
# see the AdcircUtils::ElementQuadTree package for more info
#
# you have to uncomment the "use" line near the beginning
# of this package to get this subroutine to work.
#
# e.g. 
#
#  my $tree=$adcGrid->makeElementQuadTree($maxElements[,$north,$south,$east,$west])
#
#  returns a ElementQuadTree object corresponding to the
#  grid that was loaded into the $adcGrid object
#
#  $maxElements is the maximum number of elements you want
#  in a leaf node of the tree.  
#
#  [,$north,$south,$east,$west] - optional arguments
#  to set a bounding box for the quadtree. if left out
#  bounds will be set for the entire domain.  note: the 
#  square brackets here idicate optional agruments, not 
#  an anonymous array.
# 
#
############################################################
# sub makeElementQuadTree{
#
#   my $obj=shift;
#   my $maxelems=shift;
#   my ($north,$south,$east,$west)=@_;
#
#   my $np=$obj->getVar('NP');
#   my $ne=$obj->getVar('NE');
#   my @NID=(0..$np);
#   my ($xr,$yr,$zr)=$obj->getNode(\@NID);
#   my @X=@{$xr};
#   my @Y=@{$yr};
#   my @Z=@{$zr};
#    $north=$obj->{MAX_Y} unless defined $north;
#    $south=$obj->{MIN_Y} unless defined $south;
#    $east=$obj->{MAX_X} unless defined $east;
#    $west=$obj->{MIN_X} unless defined $west;
#
#   # create the tree
#   my $tree=ElementQuadTree->new(  
#                               -NORTH=>$north,   # the region for the tree
#                               -SOUTH=>$south,
#                               -EAST =>$east,
#                               -WEST =>$west,
#                               -XNODE=>\@X,   # references to the node position table arrays (x,y,z)
#                               -YNODE=>\@Y,
#                               -ZNODE=>\@Z,
#                               -MAXELEMS=>$maxelems # maximum number of elements per tree node
#                              );                       
#   # add all the elements
#  foreach my $id (1..$ne){
#     my ($n1, $n2, $n3)=$obj->getElement($id);
#        $tree->addElement(
#                    -ID=>$id,    # the element id
#                    -N1=>$n1,    # node number 1   (index into x,y position array)
#                    -N2=>$n2,    # node number 2
#                    -N3=>$n3    # node number 3 
#                      );
#   }
#   # $tree->interpPixels();  # you can do this later 
#   return $tree;
#}



#########################################################################
#  sub setNode
#
#  sets the position of a node or array of nodes
#
#  $adcGrid->setNode(\@NID,\@X,\@Y,\@DP);  # using array references
#    or
#  $adcGrid->setNode($nid,$x,$y,$dp);      # using scalar for single node
#
sub setNode{
   my $obj=shift;
   my ($nid,$x,$y,$dp)=@_;
   my $xyz=$obj->{XYZ};
 
   if (ref($nid) eq 'ARRAY'){
      my @NID=@{$nid};
      my @X=@{$x};
      my @Y=@{$y};
      my @DP=@{$dp};
      foreach my $j (@NID){
          my $xx=shift @X;
          my $yy=shift @Y;
          my $dp=shift @DP;
          my $packed=pack("d3",$xx,$yy,$dp); # should be 24 bytes ling
          my $offset=$j*24; # there will be nothing valuable at offset 0
          substr ($xyz,$offset,24,$packed);
       }
    }else{
        my $packed=pack("d3",$x,$y,$dp); # should be 24 bytes ling
        my $offset=$nid*24; # there will be nothing valuable at offset 0
        substr ($xyz,$offset,24,$packed);
    }
    $obj->{XYZ}=$xyz; 
}


#########################################################################
#  sub setDP
#
#  sets the position of a node or array of nodes
#
#  $adcGrid->setDP(\@NID,\@DP);  # using array references
#    or
#  $adcGrid->setDP($nid,$dp);      # using scalar for single node
#
sub setDP{
   my $obj=shift;
   my ($nid,$dp)=@_;
   my $xyz=$obj->{XYZ};
 
   if (ref($nid) eq 'ARRAY'){
      my @NID=@{$nid};
      my @DP=@{$dp};
      foreach my $j (@NID){
          my $dp=shift @DP;
          my $packed=pack("d",$dp); # should be 24 bytes ling
          my $offset=$j*24+16; # there will be nothing valuable at offset 0
          substr ($xyz,$offset,8,$packed);
       }
    }else{
        my $packed=pack("d",$dp); # should be 24 bytes ling
        my $offset=$nid*24+16; # there will be nothing valuable at offset 0
        substr ($xyz,$offset,8,$packed);
    }
    $obj->{XYZ}=$xyz; 
}


##############################################################################
#  sub checkWeirheights($dz)
#
#  e.g. 
#  
#  $adcGrid->checkWeirheights($dz);
#
#  checks to see if weirheights are below the node elevation and tries to 
#  fix the problem
#
#  if $dz is negative the node elevations are adjusted by adding $dz to the
#  weir height
#
#  if $dz is positive the weirheight is adjusted by adding $dz to the nodal elevation
#
#  $dz must be non zero

sub checkWeirheights{
   my $obj = shift;
   my $dz = shift;

   my $nbou=$obj->getVar('NBOU');

   foreach my $fb (1..$nbou){
   
      my @data=$obj->getFluxBnd($fb);
      my $nvell=shift @data;
      my $ibtype=shift @data;
      my $nbdv=shift @data;
      if ($ibtype == 3 or $ibtype == 13 or $ibtype == 23) { # its a supercrit outflow
         my $ht=shift @data;
         my @HT=@{$ht};
         my @NBDV=@{$nbdv};
         my ($x,$y,$z)=$obj->getNode(\@NBDV);
         my @Z=@{$z};
         my $j=-1;
         foreach my $n (@NBDV){
             $j++;
             my $ht=shift(@HT);
             my $z=shift(@Z);
             $z=-$z;
             next if ($ht >= $z + abs($dz));
             if ($dz < 0){  # bring down the node
                 my $z2 = -1 *($ht+$dz);
                 my ($x2,$y2,$z)=$obj->getNode($n);
                 $obj->setNode($n,$x2,$y2,$z2);
                 print "node # $n, ibtype $ibtype, weirHT $ht, nodeZ $z; lowering nodeZ to $z2\n";
             }else{  # bring up the weir
                 my $ht2=$z+$dz;
                 ${$obj->{BARLANHT}}[$fb][$j]=$ht2;
                 print "node # $n, ibtype $ibtype, weirHT $ht, nodeZ $z; raising weirHt to $ht2\n";
             }
         }
       }elsif($ibtype == 4 or $ibtype == 14 or $ibtype == 24 or $ibtype == 5 or $ibtype == 25 or $ibtype == 15){  #weir
          my $ib=shift @data;
          my $ht=shift @data;
          my @NBDV=@{$nbdv}; 
          my @HT=@{$ht};
          my ($x,$y,$z)=$obj->getNode(\@NBDV);
          my @Z=@{$z};
          my $j=-1;
          foreach my $n (@NBDV){
             $j++;
             my $ht=shift(@HT);
             my $z=shift(@Z);
             $z=-$z;
             next if ($ht >= $z + abs($dz));
             if ($dz < 0){  # bring down the node
                 my $z2 = -1 *($ht+$dz);
                 my ($x2,$y2,$z)=$obj->getNode($n);
                 $obj->setNode($n,$x2,$y2,$z2);
                 print "node # $n, ibtype $ibtype, weirHT $ht, nodeZ $z; lowering nodeZ to $z2\n";
             }else{  # bring up the weir
                 my $ht2=$z+$dz;
                 ${$obj->{BARINHT}}[$fb][$j]=$ht2;
                 print "node # $n, ibtype $ibtype, weirHT $ht, nodeZ $z; raising weirHt to $ht2\n";
             }
          }
       }
   }#end nbou loop
   
}




1;

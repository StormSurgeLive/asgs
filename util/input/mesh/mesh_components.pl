#!/usr/bin/perl
# mesh_components.pl
#
# The purpose of this script is to parse a mesh file and break it into
# its component pieces.
#
# open the fort.14 (or user specified) file
open(MESHFILE,"fort.14");
#
# write the header into a file called fort.14.header, including ne and np
open (HEADERFILE,">fort.14.header");
$comment_line=<MESHFILE>;
print HEADERFILE "$comment_line\n";
$nenp_line=<MESHFILE>;
print HEADERFILE "$nenp_line\n";
close(HEADERFILE);
#
# write the nodes into a file called fort.14.nodes
open (NODEFILE,">fort.14.nodes"); 
@nenp=split(" ",$nenp_line);
$np=$nenp[1];
for ( $i=1; $i<=$np; ++$i ) {
    $node=<MESHFILE>;
    print NODEFILE $node;
}
close(NODEFILE);
#
# write the elements into a file called fort.14.elements
open (ELEMENTFILE,">fort.14.elements"); 
$ne=$nenp[0];
for ( $i=1; $i<=$ne; ++$i ) {
    $element=<MESHFILE>;
    print ELEMENTFILE $element;
}
close(ELEMENTFILE);
#
# write all the elevation boundaries into fort.14.elev.boundaries.all
open (ELEVBOUNDARIES,">fort.14.elev.boundaries.all");
# read NOPE: Number of elevation forced boundary segments
$nope=<MESHFILE>;
# read NETA: Total number of elevation specified boundary nodes
$neta=<MESHFILE>;
$neta_check=0;
for ( $k=1; $k<=$nope; ++$k ) {
    $nvdll_ibtypee_line=<MESHFILE>;
    print ELEVBOUNDARIES $nvdll_ibtypee;
    @nvdll_ibtypee = split(" ",$nvdll_ibtypee_line);
    $nvdll=$nvdll_ibtypee[0];
    for ( $j=1; $j<=$nvdll; ++$j ) {
	$nbdv=<MESHFILE>;
	print ELEVBOUNDARIES $nbdv;
	++$neta_check;
    }
} 
if ( $neta_check != $neta ) {
    print "ERROR: NETA is $neta, but there are $neta_check\n";
    print "elevation specified boundary nodes in the file.\n";
}
close(ELEVBOUNDARIES);
#
# write all the flux boundaries into fort.14.flux.boundaries.all
open (FLUXALL,">fort.14.flux.boundaries.all"); 
open (FLUXLAND,">fort.14.flux.boundaries.land"); 
open (FLUXRIVER,">fort.14.flux.boundaries.river"); 
open (FLUXINT,">fort.14.flux.boundaries.int"); 
open (FLUXEXT,">fort.14.flux.boundaries.ext"); 
open (FLUXRAD,">fort.14.flux.boundaries.rad"); 
# read NBOU: number of flux specified boundary segments
$nbou = <MESHFILE>;
# read NVEL: total number of flux specified boundary nodes
$nvel = <MESHFILE>;
$nvel_check = 0;
for ( $k=1; $k<=$nbou; ++$k ) {
    $nvell_ibtype_line=<MESHFILE>;
    print FLUXALL $nvell_ibtype_line;
    @nvell_ibtype = split(" ",$nvell_ibtype_line);
    $nvell=$nvell_ibtype[0];
    $ibtype=$nvell_ibtype[1];
    for ( $j=1; $j<=$nvell; ++$j ) {
	$fluxnode = <MESHFILE>;
	print FLUXALL $fluxnode;
	if ( $ibtype==0 || $ibtype==10 || $ibtype==20 ) {
	    # write zero flux boundaries to fort.14.flux.boundaries.land
	    print FLUXLAND $fluxnode;
	} elsif ( $ibtype==2 || $ibtype==12 || $ibtype==22 ){
	    # write nonzero flux boundaries to fort.14.flux.boundaries.river
	    print FLUXRIVER $fluxnode;
	    ++$nvel_check;
	} elsif ( $ibtype==4 || $ibtype==24 ){
            # write internal barrier boundaries to fort.14.flux.boundaries.int
	    print FLUXINT $fluxnode;
	    ++$nvel_check;
	} elsif ( $ibtype==3 || $ibtype==13 || $ibtype==23 ){
            # write external barrier boundaries to fort.14.flux.boundaries.ext
	    print FLUXEXT $fluxnode;
	    ++$nvel_check;
	} elsif ( $ibtype==30 ){
            # write radiation boundaries to fort.14.flux.boundaries.rad
	    print FLUXRAD $fluxnode;
	    ++$nvel_check;
	} else {
	    print "Boundary type $ibtype not processed by this script\n";
	}
    }
} 
if ( $nvel_check != $nvel ) {
    print "ERROR: NVEL is $nvel, but there are $nvel_check\n";
    print "flux specified boundary nodes in the file.\n";
}
close(FLUXALL);
close(FLUXLAND);
close(FLUXRIVER);
close(FLUXINT);
close(FLUXEXT);
close(FLUXRAD);

# Flux boundary types:
# Briefly       IBTYPE   Details
# Land            0      external, zero normal flux, free tangential slip
# Island          1      internal, zero normal flux, free tangential slip
# River           2      external, specified normal flux, free tangential slip
# External Weir   3      external, dynamic normal flux, free tangential slip
# Internal Weir   4      internal, dynamic normal flux, free tangential slip
# Internal Pipes  5      internal, dynamic normal flux, free tangential slip
#
# Sticky Land    10      external, zero normal flux, no tangential slip
# Sticky Island  11      internal, zero normal flux, no tangential slip
# Sticky River   12      external, specified normal flux, no tangential slip
# Sticky Ex Weir 13      external, dynamic normal flux, no tangential slip
#
# Weak Land      20      external, weak zero normal flux, free tangential slip
# Weak Island    21      internal, weak zero normal flux, free tangential slip
# Weak River     22      external, weak specified flux, free tangential slip
# Weak Ex Weir   23      external, weak dynamic flux, free tangential slip
# Weak Int Weir  24      internal, dynamic normal flux, free tangential slip
# 
# Int Weir+Pipes 25      internal, dynamic normal flux, free tangential slip
# 
# Wave radiation 30      external, dynamic normal flux, open water waves
#
# Wave rad+flux  32      external, specified normal flux + radiation flux
#
# Zero flux grad 40      external, fictitious node formulation
# Zero flux grad 41      external, Galerkin formulation


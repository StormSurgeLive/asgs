#!/usr/bin/env perl
# This script will revise the input file for 
# FigureGen42 in order to plot the o
# run in the LPFS
# RJW:08/2008

$^W++;
use strict;
use Getopt::Long;
#
my $gmthome;
my $gshome;
my $gridfile;
my $storm;
my $year;
my $adv;   
my $north;
my $south;
my $east;
my $west;
my $outputprefix;
my $outputdir;
my $starttime;
my $numrecords;
my $type;
my $vectorlimits;
my $contourlimits;
my $windvect;
my $vectcut;
my $fgscript;
my $tempdir;
GetOptions(
           "fgscript=s" => \$fgscript,
           "outputdir=s" => \$outputdir,
           "gmthome=s" => \$gmthome,
           "gridfile=s" => \$gridfile,
           "gshome=s" => \$gshome,
           "storm=s" => \$storm,
           "year=s" => \$year,
           "adv=s" => \$adv,
           "n=s" => \$north,
           "s=s" => \$south,
           "e=s" => \$east,
           "w=s" => \$west,
           "outputprefix=s" => \$outputprefix,
           "starttime=s" => \$starttime,
           "numrecords=s" => \$numrecords,
           "type=s" => \$type,
           "vectorlimits=s" => \$vectorlimits,
           "contourlimits=s" => \$contourlimits,
           "windvect=s" => \$windvect,
           "vectcut=s" => \$vectcut
           );
#
  $tempdir = "./Temp/";
 open(TEMPLATE,"$fgscript") || die "ERROR: Can't open file.";
while(<TEMPLATE>) {
    # now replace the indicated variables with values passed in from main script
    s/%outputdir%/$outputdir/g;
    s/%gmthome%/$gmthome/g;
    s/%gshome%/$gshome/g;
    s/%tempdir%/$tempdir/g;
    s/%outputprefix%/$outputprefix/g;
    s/%storm%/$storm/g;
    s/%type%/$type/g;
    s/%west%/$west/g;
    s/%east%/$east/g;
    s/%south%/$south/g;
    s/%north%/$north/g;
    s/%gridfile%/$gridfile/g;
    s/%contourlimits%/$contourlimits/g;
    s/%vectorlimits%/$vectorlimits/g;
    s/%windvect%/$windvect/g;
    s/%vectcut%/$vectcut/g;
    s/%starttime%/$starttime/g;
    print $_;
}
close(TEMPLATE);


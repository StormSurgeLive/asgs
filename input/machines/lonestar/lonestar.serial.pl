#!/usr/bin/env perl
# $Header: /Grid/Projects/LPFS/2007/hotstart/LPFS/sge.pl,v 1.2 2007/04/10 20:58:20 estrabd Exp $

$^W++;
use strict;
use Getopt::Long;

my $account;    # name of the account to take the hours from
my $adcircdir; # directory where the padcirc executable is found
my $advisdir;   # directory for the individual advisory
my $enstorm;   # name of the enesemble member (nowcast, storm3, etc)
my $notifyuser; # email address of the user to be notified in case of error
my $serqscript; # name of serial qscript file
my $walltime; # amount of wall clock time expected for adcprep
# Casey 121009: Added the ncpu and jobtype variables.
my $ncpu;
my $jobtype;

GetOptions(
           "account=s" => \$account,
           "adcircdir=s" => \$adcircdir,
           "advisdir=s" => \$advisdir,
           "enstorm=s" => \$enstorm,
           "notifyuser=s" => \$notifyuser,
           "walltime=s" => \$walltime,
           "serqscript=s" => \$serqscript,
# Casey 121009: Added the ncpu and jobtype variables.
           "ncpu=i" => \$ncpu,
           "jobtype=s" => \$jobtype,
);

open(TEMPLATE,"$serqscript") || die "ERROR: Can't open lonestar.template.serial file.";

while(<TEMPLATE>) {
    # name of the account to take the hours from
    s/%account%/$account/;
    # directory where padcirc executable is located
    s/%adcircdir%/$adcircdir/;
    # expected wall clock time 
    s/%walltime%/$walltime/;
    # directory for this particular advisory
    s/%advisdir%/$advisdir/;  
    # name of this member of the ensemble (nowcast, storm3, etc)
    s/%enstorm%/$enstorm/g;  
    # user to notify when errors occur
    s/%notifyuser%/$notifyuser/;  
# Casey 121009: Added the ncpu and jobtype variables.
    s/%ncpu%/$ncpu/;
    s/%jobtype%/$jobtype/;
    print $_;
}
close(TEMPLATE);

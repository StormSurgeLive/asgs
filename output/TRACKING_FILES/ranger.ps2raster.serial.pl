#!/usr/bin/env perl
# $Header: /Grid/Projects/LPFS/2007/hotstart/LPFS/sge.pl,v 1.2 2007/04/10 20:58:20 estrabd Exp $

$^W++;
use strict;
use Getopt::Long;

my $account;    # name of the account to take the hours from
my $dir;   # directory for the individual advisory
my $queue;   # name of the enesemble member (nowcast, storm3, etc)
my $notifyuser; # email address of the user to be notified in case of error
my $qscript; # name of serial qscript file
my $storm; # name of serial qscript file
my $gmthome;
my $gshome;
GetOptions(
           "account=s" => \$account,
           "dir=s" => \$dir,
           "queue=s" => \$queue,
           "notifyuser=s" => \$notifyuser,
           "qscript=s" => \$qscript,
           "storm=s" => \$storm,
           "gmthome=s" => \$gmthome,
           "gshome=s" => \$gshome
);

open(TEMPLATE,"$qscript") || die "ERROR: Can't open file.";

while(<TEMPLATE>) {
    # name of the account to take the hours from
    s/%account%/$account/;
    # directory for this particular advisory
    s/%dir%/$dir/g;  
    # name of queue 
    s/%queue%/$queue/g;  
    # user to notify when errors occur
    s/%notifyuser%/$notifyuser/;  
    # name of storm
    s/%storm%/$storm/g;  
    # loc of GMT
    s/%gmthome%/$gmthome/g;  
    # loc of gs
    s/%gshome%/$gshome/g;  
    print $_;
}
close(TEMPLATE);

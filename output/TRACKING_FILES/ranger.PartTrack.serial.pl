#!/usr/bin/env perl
# $Header: /Grid/Projects/LPFS/2007/hotstart/LPFS/sge.pl,v 1.2 2007/04/10 20:58:20 estrabd Exp $

$^W++;
use strict;
use Getopt::Long;

my $account;    # name of the account to take the hours from
my $ptdir;   # directory for the individual advisory
my $queue;   # name of the enesemble member (nowcast, storm3, etc)
my $notifyuser; # email address of the user to be notified in case of error
my $serqscript; # name of serial qscript file

GetOptions(
           "account=s" => \$account,
           "ptdir=s" => \$ptdir,
           "queue=s" => \$queue,
           "notifyuser=s" => \$notifyuser,
           "serqscript=s" => \$serqscript
);

open(TEMPLATE,"$serqscript") || die "ERROR: Can't open ranger.template.serial file.";

while(<TEMPLATE>) {
    # name of the account to take the hours from
    s/%account%/$account/;
    # directory for this particular advisory
    s/%ptdir%/$ptdir/;  
    # name of queue 
    s/%queue%/$queue/g;  
    # user to notify when errors occur
    s/%notifyuser%/$notifyuser/;  
    print $_;
}
close(TEMPLATE);

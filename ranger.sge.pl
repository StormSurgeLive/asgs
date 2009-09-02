#!/usr/bin/env perl
# $Header: /Grid/Projects/LPFS/2007/hotstart/LPFS/sge.pl,v 1.2 2007/04/10 20:58:20 estrabd Exp $

$^W++;
use strict;
use Getopt::Long;

my $ncpu;      # number of CPUs the job should run on
my $ncpudivisor; # integer number to divide npu by
my $queuename; # name of the queue to submit the job to
my $account;    # name of the account to take the hours from
my $adcircdir; # directory where the padcirc executable is found
my $advisdir;   # directory for the individual advisory
my $qscript;  # script to generate the queue file
my $enstorm;   # name of the enesemble member (nowcast, storm3, etc)
my $notifyuser; # email address of the user to be notified in case of error
my $walltime;  # wallclocktime
my $submitstring; # string to use to submit a job to the parallel queue
my $syslog;  # location and name of system log file

GetOptions("ncpu=i" => \$ncpu,
           "ncpudivisor=s" => \$ncpudivisor,
           "queuename=s" => \$queuename,
           "account=s" => \$account,
           "adcircdir=s" => \$adcircdir,
           "advisdir=s" => \$advisdir,
           "qscript=s" => \$qscript, 
           "enstorm=s" => \$enstorm,
           "notifyuser=s" => \$notifyuser,
           "walltime=s" => \$walltime,
           "submitstring=s" => \$submitstring,
           "syslog=s" => \$syslog);

# We expect that ncpu is the number of physical cpus (or cpu cores) that
# we intend to use. However,
# for OpenPBS (SGE) we must specify the number of CPU's to run on 
# and the number of cpus per node;
# optimally the number of cpus per node is 16 and the num cpus 
# must be divisible by 16. 
my $pbsncpu;
if ( $ncpudivisor == 1 ) {
   $pbsncpu = sprintf("%d",$ncpu);
} else {
     $ncpudivisor=$ncpudivisor."way";
   $pbsncpu = $ncpudivisor." ".$ncpu;
}

open(TEMPLATE,"$qscript") || die "ERROR: Can't open ranger.template.sge file.";

while(<TEMPLATE>) {
    # fill in the number of compute nodes to run on 
    s/%pbsncpu%/$pbsncpu/g;
    # name of the queue on which to run
    s/%queuename%/$queuename/g;
    # name of the account to take the hours from
    s/%account%/$account/g;
    # directory where padcirc executable is located
    s/%adcircdir%/$adcircdir/g;
    # directory for this particular advisory
    s/%advisdir%/$advisdir/g;  
    # directory wallclocktime
    s/%walltime%/$walltime/g;  
    # name of this member of the ensemble (nowcast, storm3, etc)
    s/%enstorm%/$enstorm/g;  
    # user to notify when errors occur
    s/%notifyuser%/$notifyuser/g;  
    # string to use to submit a job to the parallel queue
    s/%submitstring%/$submitstring/g;
    print $_;
}
close(TEMPLATE);

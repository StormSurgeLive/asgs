#!/usr/bin/env perl
# $Header: /Grid/Projects/LPFS/2007/hotstart/LPFS/sge.pl,v 1.2 2007/04/10 20:58:20 estrabd Exp $

$^W++;
use strict;
use Getopt::Long;

my $ncpu;      # number of CPUs the job should run on
my $ncpudivisor; # integer number to divide npu by
my $queue; # name of the queue to submit the job to
my $account;    # name of the account to take the hours from
my $dir; # directory where the padcirc executable is found
my $qscript;  # script to generate the queue file
my $notifyuser; # email address of the user to be notified in case of error
my $storm;  # wallclocktime

GetOptions("ncpu=i" => \$ncpu,
           "ncpudivisor=s" => \$ncpudivisor,
           "queue=s" => \$queue,
           "account=s" => \$account,
           "dir=s" => \$dir,
           "qscript=s" => \$qscript, 
           "notifyuser=s" => \$notifyuser,
           "storm=s" => \$storm);

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
    s/%queue%/$queue/g;
    # name of the account to take the hours from
    s/%account%/$account/g;
    # directory where padcirc executable is located
    s/%dir%/$dir/g;
    # user to notify when errors occur
    s/%notifyuser%/$notifyuser/g;  
    # string to use to submit a job to the parallel queue
    s/%storm%/$storm/g;
    print $_;
}
close(TEMPLATE);

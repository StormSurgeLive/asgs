#!/usr/bin/env perl
#
# Copyright(C) 2006, 2007, 2008, 2009 Jason Fleming
# Copyright(C) 2006, 2007 Brett Estrade
# 
# This file is part of the ADCIRC Surge Guidance System (ASGS).
# 
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#
$^W++;
use strict;
use integer;
use Getopt::Long;

my $ncpu;      # number of CPUs the job should run on
my $queuename; # name of the queue to submit the job to
my $ncpudivisor; # integer number to divide npu by
my $account;    # name of the account to take the hours from
my $adcircdir; # directory where the padcirc executable is found
my $advisdir;   # directory for the individual advisory
my $inputdir;  # directory where the template files are stored
my $enstorm;   # name of the enesemble member (nowcast, storm3, etc)
my $notifyuser; # email address of the user to be notified in case of error
my $submitstring; # string to use to submit a job to the parallel queue
my $walltime; # estimated maximum wall clock time  
my $qscript;  # the template file to use for the queue submission script
my $syslog;   # the log file that the ASGS uses 
my $ppn;      # the number of processors per node

# initialize to the log file that adcirc uses, just in case
$syslog="adcirc.log";

GetOptions("ncpu=s" => \$ncpu,
           "queuename=s" => \$queuename,
           "account=s" => \$account,
           "adcircdir=s" => \$adcircdir,
           "advisdir=s" => \$advisdir,
           "inputdir=s" => \$inputdir, 
           "enstorm=s" => \$enstorm,
           "notifyuser=s" => \$notifyuser,
           "walltime=s" => \$walltime,
           "qscript=s" => \$qscript,
           "syslog=s" => \$syslog,
           "submitstring=s" => \$submitstring,
           "ppn=s" => \$ppn );

# calculate the number of nodes to request, based on the number of cpus and the
# number of processors per node
unless ( $ncpu ) { die "ERROR: The number of CPUs was not specified to the queue script generator tezpur.pbs.pl."; }
unless ( $ppn ) { die "ERROR: The number of processors per node was not specified to the queue script generator tezpur.pbs.pl."; }
my $nnodes = $ncpu/$ppn; 
if ( ($ncpu%$ppn) != 0 ) {
   $nnodes++;
} 

open(TEMPLATE,"<$qscript") || die "ERROR: Can't open $qscript file for reading as a template for the queue submission script.";

while(<TEMPLATE>) {
    # fill in the number of CPUs
    s/%ncpu%/$ncpu/;
    # fill in the number of compute nodes to run on 
    s/%nnodes%/$nnodes/;
    # fill in the number of processors per node
    s/%ppn%/$ppn/;
    # name of the queue on which to run
    s/%queuename%/$queuename/;
    # the estimated amount of wall clock time
    s/%walltime%/$walltime/;
    # name of the account to take the hours from
    s/%account%/$account/;
    # directory where padcirc executable is located
    s/%adcircdir%/$adcircdir/;
    # directory for this particular advisory
    s/%advisdir%/$advisdir/;  
    # name of this member of the ensemble (nowcast, storm3, etc)
    s/%enstorm%/$enstorm/;  
    # user to notify when errors occur
    s/%notifyuser%/$notifyuser/;  
    # string to use to submit a job to the parallel queue
    s/%submitstring%/$submitstring/;
    # file to direct stdout and stderr to from the adcirc process
    s/%syslog%/$syslog/;
    print $_;
}
close(TEMPLATE);

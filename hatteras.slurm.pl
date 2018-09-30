#!/usr/bin/env perl
#
# Copyright(C) 2006--2018 Jason Fleming
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
#
# Despite the file name, this template filler is used for both Tezpur and 
# Blueridge.
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
my $cloptions=""; # command line options for adcirc, if any
my $jobtype;  # e.g., prep15, padcirc, padcswan, etc
my $localhotstart; # present if subdomain hotstart files should be written
my $reservation="null"; # name of SLURM reservation where the job should be submitted
my $partition="null";   # name of SLURM partition the job should use
my $constraint="null";  # name of SLURM constraint the job should use
my $numwriters=0;  # number of writer processors, if any

# initialize to the log file that adcirc uses, just in case
$syslog="adcirc.log";

GetOptions("ncpu=s" => \$ncpu,
           "queuename=s" => \$queuename,
           "partition=s" => \$partition,
           "reservation=s" => \$reservation,
           "constraint=s" => \$constraint,
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
           "localhotstart" => \$localhotstart,
           "numwriters=s" => \$numwriters,
           "ppn=s" => \$ppn,
           "jobtype=s" => \$jobtype );

# calculate the number of nodes to request, based on the number of cpus and the
# number of processors per node
unless ( $ncpu ) { die "ERROR: hatteras.slurm.pl: The number of CPUs was not specified."; }
unless ( $ppn ) { die "ERROR: hatteras.slurm.pl: The number of processors per node was not specified."; }
#
# add command line option if local hot start files should be written
$cloptions = "";
if ( defined $localhotstart ) {
   $cloptions = "-S";
}
if ( $numwriters != 0 ) {
   $cloptions = $cloptions . " -W " . $numwriters;
   $ncpu += $numwriters;
}
my $nnodes = int($ncpu/$ppn); 
if ( ($ncpu%$ppn) != 0 ) {
   $nnodes++;
}
# convert wall clock time HH:MM:SS to minutes
#print $walltime;
$walltime =~ /(\d{2}):(\d{2}):(\d{2})/;
my $wallminutes = $1*60 + $2;
#
open(TEMPLATE,"<$qscript") || die "ERROR: hatteras.slurm.pl: Can't open $qscript file for reading as a template for the queue submission script.";
#
while(<TEMPLATE>) {
    # fill in the number of CPUs
    s/%ncpu%/$ncpu/;
    # name of the queue on which to run
    s/%queuename%/$queuename/;
    # the estimated amount of wall clock time
    s/%walltime%/$walltime/;
    # fill in the estimated wall clock time in minutes
    s/%wallminutes%/$wallminutes/;
    # name of the account to take the hours from
    s/%account%/$account/;
    # directory where padcirc executable is located
    s/%adcircdir%/$adcircdir/;
    # directory for this particular advisory
    s/%advisdir%/$advisdir/;  
    # name of this member of the ensemble (nowcast, storm3, etc)
    s/%enstorm%/$enstorm/g;  
    # file to direct stdout and stderr to from the adcirc process
    s/%syslog%/$syslog/;
    # fill in command line options
    s/%cloptions%/$cloptions/;
    # the type of job that is being submitted
    s/%jobtype%/$jobtype/g;
    # the email address of the ASGS Operator
    s/%notifyuser%/$notifyuser/g;
    # the SLURM partition
    if ( $partition ne "null" ) {
       s/%partition%/$partition/g;
    } else {
       s/%partition%/noLineHere/g;
    }
    # the SLURM reservation
    if ( $reservation ne "null" ) {
       s/%reservation%/$reservation/g;
    } else {
       s/%reservation%/noLineHere/g;
    }
    # the SLURM constraint
    if ( $constraint ne "null" ) {
       s/%constraint%/$constraint/g;
    } else {
       s/%constraint%/noLineHere/g;
    }
    # fills in the number of nodes on platforms that require it
    s/%nnodes%/$nnodes/g;
    unless ( $_ =~ /noLineHere/ ) {
       print $_;
    }
}
close(TEMPLATE);

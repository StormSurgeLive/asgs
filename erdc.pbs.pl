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

GetOptions("ncpu=i" => \$ncpu,
           "queuename=s" => \$queuename,
           "ncpudivisor=s" => \$ncpudivisor,
           "account=s" => \$account,
           "adcircdir=s" => \$adcircdir,
           "advisdir=s" => \$advisdir,
           "inputdir=s" => \$inputdir, 
           "enstorm=s" => \$enstorm,
           "notifyuser=s" => \$notifyuser,
           "walltime=s" => \$walltime,
           "submitstring=s" => \$submitstring);

# We expect that ncpu is the number of physical cpus (or cpu cores) that
# we intend to use. However,
# for Portable Batch System (PBS) we must specify the number of compute
# nodes to run on some systems, so we must divide by the number of cpus
# per node in that case; 
# on other systems, we actually specify the number of cpus (or cpu cores)
# directly, and in these cases ncpudivisor will be unity
my $pbsncpu;
if ( $ncpudivisor == 1 ) {
   $pbsncpu = sprintf("%d",$ncpu);
} else {
   $pbsncpu = sprintf("nodes=%d:ppn=%d",($ncpu/$ncpudivisor),$ncpudivisor);
}

open(TEMPLATE,"$inputdir/template.pbs") || die "ERROR: Can't open template.pbs file.";

while(<TEMPLATE>) {
    # fill in the number of compute nodes to run on (assuming 2 CPUs per node)
    s/%pbsncpu%/$pbsncpu/;
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
    print $_;
}
close(TEMPLATE);

#!/usr/bin/env perl
#--------------------------------------------------------------------------
# qscript.pl : use json via STDIN to fill in qscript.template
# and send result to a queue script file and modified json
# to STDOUT
#--------------------------------------------------------------------------
# Copyright(C) 2006--2022 Jason Fleming
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
#--------------------------------------------------------------------------
use strict;
use warnings;
use integer;
use JSON::PP;
use MIME::Base64 qw(encode_base64 decode_base64);
use ASGSUtil;
#
my $myNCPU = "noLineHere";   # number of CPUs the job should run on (or be decomposed for)
my $totalcpu = "noLineHere"; # ncpu + numwriters
my $nnodes = "noLineHere";   # number of cluster nodes
my $walltime;      # estimated maximum wall clock time
my $wallminutes;   # integer number of minutes, calculated from HH:MM:SS
my $qscripttemplate; # template file to use for the queue submission script
my $qscript;       # queue submission script we're producing
my $qScriptScalar; # qscript as embedded in JSON response
my $cloptions="";  # command line options for adcirc, if any
my $cmd;           # the command line to execute
my $joblauncher = "noLineHere"; # executable line in qscript (ibrun, mpirun, etc)
#
#-----------------------------------------------------------------
#
#       R E A D   J O B   C O N F I G U R A T I O N   J S O N
#
#-----------------------------------------------------------------
#
# slurp the JSON request contents into a scalar variable
my $file_content = do { local $/; <> };
my $jshash_ref = JSON::PP->new->decode($file_content);
#-----------------------------------------------------------------
#
#                S E T   P A R A M E T E R S
#
#-----------------------------------------------------------------
# get jobtype, e.g., prep15, padcirc, padcswan, etc
my $jobtype = $jshash_ref->{'jobtype'}; # partmesh, prep15, padcirc etc
# get number of processors per node (if it is defined)
# the number of processors per node
my $ppn = getQueueScriptParameter($jshash_ref, "ppn");
my $hpcenvshort = $jshash_ref->{'hpcenvshort'};
#
# construct command line for running adcprep or serial job
if ( $jshash_ref->{"parallelism"} eq "serial" ) {
    $totalcpu = 1; # these are serial jobs
    $nnodes = 1;   # these are serial jobs
    if ( $jobtype eq "partmesh" || $jobtype =~ /prep/ ) {
        # get number of compute cpus
        $myNCPU = $jshash_ref->{"forncpu"}; # for adcprep
        $cmd="$jshash_ref->{'adcircdir'}/adcprep --np $myNCPU --$jobtype --strict-boundaries";
    } else {
        $cmd = $jshash_ref->{"cmd"};
    }
    my $serqueue = $jshash_ref->{'serqueue'};
}
#
# construct command line for running padcirc, padcswan, or other parallel job
if ( $jobtype eq "padcirc" || $jobtype eq "padcswan" ){
    # get number of compute cpus
    $myNCPU = $jshash_ref->{"ncpu"};
    # set subdomain hotstart files if specified
    if ( $jshash_ref->{"hotstartcomp"} eq "subdomain" ) {
       $cloptions .= "-S -R";
    }
    # set dedicated writer processors
    my $numwriters = getQueueScriptParameter($jshash_ref, "numwriters");
    if ( defined $jshash_ref->{"numwriters"} &&
                 $jshash_ref->{"numwriters"} ne "null" &&
                 $jshash_ref->{"numwriters"} != 0 ) {
        $cloptions = $cloptions . " -W " . $jshash_ref->{"numwriters"};
        $totalcpu = $myNCPU + $jshash_ref->{"numwriters"};
    } else {
        $totalcpu = $myNCPU;
    }
    # determine number of compute nodes to request
    if ( $ppn ne "noLineHere" ) {
        $nnodes = int($totalcpu/$ppn);
        if ( ($totalcpu%$ppn) != 0 ) {
            $nnodes++;
        }
    } else {
      $nnodes = "noLineHere";
    }
    $joblauncher = $jshash_ref->{"joblauncher"};
    # fill in template positions in job launcher line
    $joblauncher =~ s/%ncpu%/$myNCPU/g;
    $joblauncher =~ s/%totalcpu%/$totalcpu/g;
    $joblauncher =~ s/%nnodes%/$nnodes/g;
    $cmd="$joblauncher $jshash_ref->{'adcircdir'}/$jobtype $cloptions";
}
#
# compute wall clock time HH:MM:SS in minutes
$walltime = $jshash_ref->{"walltime"};
$walltime =~ /(\d+):(\d+):(\d+)/;
$wallminutes = $1*60 + $2;
#
# get queue script template
my $qScriptTemplate = $jshash_ref->{"qscripttemplate"};
#
# set name of qscript
my $queuesyslc = lc $jshash_ref->{"queuesys"};
$qscript = $jobtype . "." . $queuesyslc;
#
#-----------------------------------------------------------------
#
#              F I L L   I N   T E M P L A T E
#
#-----------------------------------------------------------------
my $TEMPLATE;
if ( -e $qScriptTemplate ) {
    if (! open($TEMPLATE,"<",$qScriptTemplate) ) {
        ASGSUtil::stderrMessage("ERROR",
                              "Found the queue script template file ".
                              "$qscripttemplate but could not open it: $!.");
        die;
    }
} else {
    ASGSUtil::stderrMessage("ERROR",
                            "The queue script template file $qscripttemplate ".
                            "was not found.");
    die;
}
#
ASGSUtil::stderrMessage("INFO",
                        "Processing queue script template $qScriptTemplate.");
while(<$TEMPLATE>) {
    # remove queue system directives from queueing systems other than
    # the one specified and then fill in the correct environment
    # variables for the queue system we ar using
    if ( $jshash_ref->{"queuesys"} eq "PBS" ) {
        s/#SBATCH/noLineHere/g;
        s/%JOBID%/PBS_JOBID/g;
        s/%JOBDIR%/PBS_O_WORKDIR/g;
        s/%JOBHOST%/PBS_O_HOST/g;
        s/%JOBNODES%/"`cat \$PBS_NODEFILE`"/g;  # PBS var contains name of node list file
        s/%JOBNNODES%/PBS_NUM_NODES/g;
        s/%JOBNTASKSPERNODE%/PBS_NUM_PPN/g;
        s/%JOBNTASKS%/PBS_TASKNUM/g;
    }
    if ( $jshash_ref->{"queuesys"} eq "SLURM" ) {
        s/#PBS/noLineHere/g;
        s/%JOBID%/SLURM_JOBID/g;
        s/%JOBDIR%/SLURM_SUBMIT_DIR/g;
        s/%JOBHOST%/SLURM_SUBMIT_HOST/g;
        s/%JOBNODES%/\$SLURM_JOB_NODELIST/g;  # SLURM var contains node list
        s/%JOBNNODES%/SLURM_NNODES/g;
        s/%JOBNTASKSPERNODE%/SLURM_NTASKS_PER_NODE/g;
        s/%JOBNTASKS%/SLURM_NTASKS/g;
    }
    if ( $jshash_ref->{"queuesys"} eq "mpiexec" ||
         $jshash_ref->{"queuesys"} eq "serial" ) {
        s/%JOBID%/\$/g;
        s/%JOBDIR%/PWD/g;
        s/{%JOBHOST%}/(hostname)/g;
    }
    # fill in the lower case name of the queueing system
    s/%queuesyslc%/$queuesyslc/g;
    # fill in the name of the queueing system
    s/%queuesys%/$jshash_ref->{"queuesys"}/g;
    # fill in the number of compute cores (i.e., not including writers)
    s/%ncpu%/$myNCPU/;
    # number of cores per compute node
    s/%ppn%/$ppn/;
    # fill in the total number of cores
    s/%totalcpu%/$totalcpu/;
    # the estimated amount of wall clock time
    if ( $jshash_ref->{"walltimeformat"} eq "minutes" ) {
       s/%walltime%/$wallminutes/;
    } else {
       s/%walltime%/$walltime/;
    }
    # name of the account to take the hours from
    # the value "null" is used to represent the default
    # account for the Operator; we can omit this line
    # from the queue script
    my $account = getQueueScriptParameter($jshash_ref,"account");
    s/%account%/$account/;
    # directory where adcirc executables are located
    s/%adcircdir%/$jshash_ref->{"adcircdir"}/;
    # directory where asgs executables are located
    s/%scriptdir%/$jshash_ref->{"scriptdir"}/;
    # directory for this particular advisory
    s/%advisdir%/$jshash_ref->{"advisdir"}/;
    # name of this member of the ensemble (nowcast, storm3, etc)
    s/%scenario%/$jshash_ref->{"scenario"}/g;
    # name of overall asgs log file
    s/%syslog%/$jshash_ref->{"syslog"}/g;
    # fill in command line options
    s/%cloptions%/$cloptions/;
    # fill in command to be executed
    s/%cmd%/$cmd/;
    # the type of job that is being submitted (partmesh, prep15, padcirc, etc)
    s/%jobtype%/$jobtype/g;
    # the email address of the ASGS Operator
    my $notifyuser = getQueueScriptParameter($jshash_ref,"asgsadmin");
    s/%notifyuser%/$notifyuser/;
    # reservation, constraint, and qos are only for SLURM
    # partition is not here b/c it is synonym for queuename
    my $reservation = getQueueScriptParameter($jshash_ref,"reservation");
    s/%reservation%/$reservation/;
    my $constraint = getQueueScriptParameter($jshash_ref,"constraint");
    s/%constraint%/$constraint/;
    my $qos = getQueueScriptParameter($jshash_ref,"qos");
    s/%qos%/$qos/;
    # fills in the number of nodes on platforms that require it
    s/%nnodes%/$nnodes/g;
    # fill in serial queue
    if ( $jshash_ref->{"parallelism"} eq "serial" ) {
        # name of the queue on which to run
        s/%queuename%/$jshash_ref->{"serqueue"}/;
    }
    # fill in parallel queue
    if ( $jshash_ref->{"parallelism"} eq "parallel" ) {
        # name of the queue on which to run
        s/%queuename%/$jshash_ref->{"queuename"}/;
    }
    # copy non-noLineHere lines to the queue script
    unless ( $_ =~ /noLineHere/ ) {
        $qScriptScalar .= $_;
    }
}
close($TEMPLATE);
#
$jshash_ref->{"qScriptFileName"} = $qscript;
$jshash_ref->{"cmd"} = $cmd;
$jshash_ref->{"cmdLineOptions"} = $cloptions;
$jshash_ref->{"totalcpu"} = $totalcpu;
$jshash_ref->{"nnodes"} = $nnodes;
$jshash_ref->{"script"} = encode_base64($qScriptScalar);
# write the response to a file called qscript.pl.json
# adds a timestamp to the json data
ASGSUtil::writeJSON($jshash_ref);
# write the response to STDOUT
print JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
1;

# if the value is null, return noLineHere
sub getQueueScriptParameter {
    my ( $hash_ref, $param ) = @_;
    if ( defined $hash_ref->{$param} && $jshash_ref->{$param} ne "null" ) {
        return $jshash_ref->{$param};
    } else {
        return "noLineHere";
    }
}
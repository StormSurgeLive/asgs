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
use Template;
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
my $jobtype            = $jshash_ref->{'jobtype'}; # partmesh, prep15, padcirc etc
my $asgs_container_cmd = $ENV{ASGS_SINGULARITY_CMD}   // undef;
my $adcirc_sif         = $ENV{ADCIRC_SINGULARITY_SIF} // undef;
my $container_cmd      = ($asgs_container_cmd && $adcirc_sif) ? sprintf("%s %s", $asgs_container_cmd, $adcirc_sif) : "";
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
        $cmd = "$container_cmd adcprep --np $myNCPU --$jobtype --strict-boundaries";
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
	if ( $totalcpu > $ppn ) {
           $nnodes = int($totalcpu/$ppn);
           if ( ($totalcpu%$ppn) != 0 ) {
               $nnodes++;
           }
        } else { 
	   # the number of parallel tasks is less than 
	   # the maximum tasks per node
	   # so just request one node
	   $nnodes = 1;
	}
    } else {
      $nnodes = "noLineHere";
    }
    $joblauncher = $jshash_ref->{"joblauncher"};
    # fill in template positions in job launcher line
    $joblauncher =~ s/%ncpu%/$myNCPU/g;
    $joblauncher =~ s/%totalcpu%/$totalcpu/g;
    $joblauncher =~ s/%nnodes%/$nnodes/g;
    $cmd = "$joblauncher $container_cmd $jobtype $cloptions";
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
        ASGSUtil::stderrMessage("ERROR", "Found the queue script template file ". "$qscripttemplate but could not open it: $!.");
        die;
    }
}
 else {
    ASGSUtil::stderrMessage("ERROR", "The queue script template file $qscripttemplate ". "was not found.");
    die;
}
#
ASGSUtil::stderrMessage("INFO", "Processing queue script template $qScriptTemplate.");

# update additional values for preserving in a JSON file
# before updating the contents in stored in $TEMPLATE

$jshash_ref->{qScriptFileName} = $qscript;
$jshash_ref->{cmd} = $cmd;
$jshash_ref->{cmdLineOptions} = $cloptions;
$jshash_ref->{totalcpu} = $totalcpu;
$jshash_ref->{nnodes} = $nnodes;

TEMPLATE_RENDER:
while(my $line = <$TEMPLATE>) {
    # remove queue system directives from queueing systems other than
    # the one specified and then fill in the correct environment
    # variables for the queue system we ar using
    if ( $jshash_ref->{"queuesys"} eq "PBS" ) {
        $line =~ s/#SBATCH/noLineHere/g;
        $line =~ s/%JOBID%/PBS_JOBID/g;
        $line =~ s/%JOBDIR%/PBS_O_WORKDIR/g;
        $line =~ s/%JOBHOST%/PBS_O_HOST/g;
        $line =~ s/%JOBNODES%/"`cat \$PBS_NODEFILE`"/g;  # PBS var contains name of node list file
        $line =~ s/%JOBNNODES%/PBS_NUM_NODES/g;
        $line =~ s/%JOBNTASKSPERNODE%/PBS_NUM_PPN/g;
        $line =~ s/%JOBNTASKS%/PBS_TASKNUM/g;
    }
    if ( $jshash_ref->{"queuesys"} eq "SLURM" ) {
        $line =~ s/#PBS/noLineHere/g;
        $line =~ s/%JOBID%/SLURM_JOBID/g;
        $line =~ s/%JOBDIR%/SLURM_SUBMIT_DIR/g;
        $line =~ s/%JOBHOST%/SLURM_SUBMIT_HOST/g;
        $line =~ s/%JOBNODES%/\$SLURM_JOB_NODELIST/g;  # SLURM var contains node list
        $line =~ s/%JOBNNODES%/SLURM_NNODES/g;
        $line =~ s/%JOBNTASKSPERNODE%/SLURM_NTASKS_PER_NODE/g;
        $line =~ s/%JOBNTASKS%/SLURM_NTASKS/g;
    }
    if ( $jshash_ref->{"queuesys"} eq "mpiexec" || $jshash_ref->{"queuesys"} eq "serial" ) {
        $line =~ s/%JOBID%/\$/g;
        $line =~ s/%JOBDIR%/PWD/g;
        $line =~ s/{%JOBHOST%}/(hostname)/g;
        $line =~ s/QUEUEONLY/noLineHere/g;
    }
    # fill in the lower case name of the queueing system
    $line =~ s/%queuesyslc%/$queuesyslc/g;

    # fill in the name of the queueing system
    $line =~ s/%queuesys%/$jshash_ref->{"queuesys"}/g;

    # fill in the number of compute cores (i.e., not including writers)
    $line =~ s/%ncpu%/$myNCPU/g;

    # number of cores per compute node
    $line =~ s/%ppn%/$ppn/g;

    # fill in the total number of cores
    $line =~ s/%totalcpu%/$totalcpu/g;

    # the estimated amount of wall clock time
    if ( $jshash_ref->{"walltimeformat"} eq "minutes" ) {
       $line =~ s/%walltime%/$wallminutes/g;
    } else {
       $line =~ s/%walltime%/$walltime/g;
    }
    # name of the account to take the hours from
    # the value "null" is used to represent the default
    # account for the Operator; we can omit this line
    # from the queue script
    my $account = getQueueScriptParameter($jshash_ref,"account");
    $line =~ s/%account%/$account/g;

    # directory where adcirc executables are located
    $line =~ s/%adcircdir%/$jshash_ref->{"adcircdir"}/g;

    # directory where asgs executables are located
    $line =~ s/%scriptdir%/$jshash_ref->{"scriptdir"}/g;

    # directory for this particular advisory
    $line =~ s/%advisdir%/$jshash_ref->{"advisdir"}/g;

    # name of this member of the ensemble (nowcast, storm3, etc)
    $line =~ s/%scenario%/$jshash_ref->{"scenario"}/g;

    # name of overall asgs log file
    $line =~ s/%syslog%/$jshash_ref->{"syslog"}/g;

    # whether to generate a Wind10m layer
    $line =~ s/%wind10mlayer%/$jshash_ref->{"wind10mlayer"}/g;

    # fill in command line options
    $line =~ s/%cloptions%/$cloptions/g;

    # fill in command to be executed
    $line =~ s/%cmd%/$cmd/g;

    # the type of job that is being submitted (partmesh, prep15, padcirc, etc)
    $line =~ s/%jobtype%/$jobtype/g;

    # the email address of the ASGS Operator
    my $notifyuser = getQueueScriptParameter($jshash_ref,"asgsadmin");
    $line =~ s/%notifyuser%/$notifyuser/g;

    # reservation, constraint, and qos are only for SLURM
    # partition is not here b/c it is synonym for queuename
    my $reservation = getQueueScriptParameter($jshash_ref,"reservation");
    $line =~ s/%reservation%/$reservation/g;

    my $constraint = getQueueScriptParameter($jshash_ref,"constraint");
    $line =~ s/%constraint%/$constraint/g;

    my $qos = getQueueScriptParameter($jshash_ref,"qos");
    $line =~ s/%qos%/$qos/g;

    # fills in the number of nodes on platforms that require it
    $line =~ s/%nnodes%/$nnodes/g;

    # fill in serial queue
    if ( $jshash_ref->{"parallelism"} eq "serial" ) {
        # name of the queue on which to run
        $line =~ s/%queuename%/$jshash_ref->{"serqueue"}/g;
    }

    # fill in parallel queue
    if ( $jshash_ref->{"parallelism"} eq "parallel" ) {
        # name of the queue on which to run
        $line =~ s/%queuename%/$jshash_ref->{"queuename"}/g;
    }

    # copy non-noLineHere lines to the queue script
    unless ( $line =~ /noLineHere/ ) {
        $qScriptScalar .= $line;
    }
}
close($TEMPLATE);

# Now that "$qScriptScalar" contains the contents as renderd using the "TEMPLATE_RENDER"
# loop labeled above, we shall commence with dealing with more complicated conditional
# parts of the template using Template::Toolkit

my $tt = Template->new({
    STRICT => 1,   # undefined vars become errors (great for job scripts)
    TRIM   => 1,
}) or die Template->error;

my $final_queue_script = '';
$tt->process(\$qScriptScalar, { runinfo => $jshash_ref }, \$final_queue_script)
  or die "TT process failed: " . $tt->error . "\n";

# instead of writing the rendered queue script to a file first, we are
# storing it as an encoded string in the JSON hash
$jshash_ref->{script} = encode_base64($final_queue_script);

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

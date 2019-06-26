#!/usr/bin/env perl
#--------------------------------------------------------------------------
# offset.pl
#--------------------------------------------------------------------------
# Copyright(C) 2019 Jason Fleming
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
#--------------------------------------------------------------------------
$^W++;
use strict;
use Getopt::Long;
use Date::Pcalc;
use Cwd;
#
our $csdate;
our ($cy, $cm, $cd, $ch, $cmin, $cs); # ADCIRC cold start time
our ($ny, $nm, $nd, $nh, $nmin, $ns); # ADCIRC hot start time
our ($ey, $em, $ed, $eh, $emin, $es); # ADCIRC end time
my $hstime;      # time, in seconds, of hotstart file (since coldstart)
our $scenariodir = "null"; # directory where the run.properties and fort.15 file are found
our $advisdir;  # the directory for this run
my $scriptdir = "."; # the directory containing asgs_main.sh
our $RNDAY;     # total run length from cold start, in days
our %runProps;     # create a dictionary of properties from run.properties
our %previousRunProps; # create a dictionary of properties from <fromdir>/run.properties
our $this = "offset.pl";
our $verbose; # if set, get extra logging
our $offset_line; # will be appended to fort.15
#
# Get command line option (directory where the fort.15 and run.properties files are found)
GetOptions("scenariodir=s" => \$scenariodir,
                 "verbose" => \$verbose );
#
# open properties file 
unless (open(RUNPROPS,"<$scenariodir/run.properties")) {
   stderrMessage("ERROR","Failed to open $scenariodir/run.properties: $!.");
   die;
}
while (<RUNPROPS>) {
   my @fields = split ':',$_, 2 ;
   # strip leading and trailing spaces and tabs
   $fields[0] =~ s/^\s|\s+$//g ;
   $fields[1] =~ s/^\s|\s+$//g ;
   $runProps{$fields[0]} = $fields[1];
}
close(RUNPROPS);
#
# handle case where there is nothing about the offset in the run.properties file
unless ( defined $runProps{"forcing.offset"} ) {
   $runProps{"forcing.offset"} = "off";
}
#
#     A S S I M I L A T E D    O F F S E T
#
if ( $runProps{"forcing.offset"} eq "assimilated" ) {
    &writeControlAndOffsetFiles();
    exit;
}
#--------------------------------------------------------------------
#
#                D Y N A M I C   O F F S E T 
#
#--------------------------------------------------------------------
# "Dynamic" means that the Operator has specified an offset, e.g., 30cm.
#-------------------------------------------------------------------- 
#
# locate the properties file that this run was hotstarted from (if any)
my $fromdir = "null";
my $from_properties = "null";
unless ( $runProps{"scenario"} eq "hindcast" || $runProps{"scenario"} eq "spinup" ) {
   $fromdir = $runProps{"path.fromdir"};
   if ( ! defined $runProps{"url.hotstart"} || $runProps{"url.hotstart"} eq "null" ) {
      $from_properties =  "$fromdir/run.properties";
   } else {
      # starting from a hotstart file downloaded from URL
      $from_properties =  $runProps{"path.rundir"} . "/from.run.properties";   
   }
} else {
   # this is a tide/river spinup, there are no previous run properties, this is 
   # the first run from cold start, nothing needs to be done here
   &stderrMessage("INFO","This is a hindcast, there is no possibility of a prior dynamic offset.");
}
#
# load previous run.properties if available
if ( $fromdir ne "null" ) {
   unless (open(FROMRUNPROPS,"<$from_properties")) {
      stderrMessage("ERROR","Failed to open $from_properties: $!.");
      die;
   }
   while (<FROMRUNPROPS>) {
      my @fields = split ':',$_, 2 ;
      # strip leading and trailing spaces and tabs
      $fields[0] =~ s/^\s|\s+$//g ;
      $fields[1] =~ s/^\s|\s+$//g ;
      $previousRunProps{$fields[0]} = $fields[1];
   }
   close(FROMRUNPROPS);
}
#
# parse out the pieces of the cold start date
$csdate = $runProps{"adcirc.time.coldstartdate"}; # : 2019051000
$csdate=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
$cy = $1;
$cm = $2;
$cd = $3;
$ch = $4;
$cmin = 0.0;
$cs = 0.0;
#
# hotstart time in seconds since coldstart
$hstime = $runProps{"InitialHotStartTime"};
unless (defined $hstime) {
    $hstime = 0.0;
} else {
    &stderrMessage("DEBUG","The hotstart time in seconds is $hstime.");
}
#
# run start time (yyyymmddhh24)
my $runstartdate = $runProps{"RunStartTime"};
$ny = $1;
$nm = $2;
$nd = $3;
$nh = $4;
$nmin = 0.0;
$ns = 0.0;
#
# run end time
my $runenddate = $runProps{"RunEndTime"};
$runenddate=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
$ey = $1;
$em = $2;
$ed = $3;
$eh = $4;
$emin = 0.0;
$es = 0.0;
# 
(my $ddays, my $dhrs, my $dmin, my $dsec)
    = Date::Pcalc::Delta_DHMS(
            $cy,$cm,$cd,$ch,$cmin,$cs,
            $ey,$em,$ed,$eh,$emin,$es);
$RNDAY = $ddays + $dhrs/24.0 + $dmin/(60.0*24.0) + $dsec/86400.0;
#
my $offsetFactorAtPreviousRunFinish = 0.0; # offset at end of run we are hotstarting from (if any)
$runProps{"forcing.offset.datasets"} = 1; # whether to use 1 or 2 datasets in offset.dat
$runProps{"forcing.offset.timeincrement"} = -99999.0; # in offset.dat file
$runProps{"forcing.offset.deactivated"} = "false"; # must be turned off under certain circumstances
$runProps{"forcing.offset.modified"} = "false";    # must be modified under certain circumstances
my $offsetFactorStart = $runProps{"forcing.offset.offsetfactorstart"};
my $offsetFactorFinish = $runProps{"forcing.offset.offsetfactorfinish"};
my $offsetFactorAtRunStart;   # for run.properties 
my $offsetFactorAtRunFinish;  # for run.properties
my $offsetTimeIncrement;      # time between first and second offset datasets (seconds) 
#
# if hotstarting, see if there was an offset in place at end of previous run
if ( $hstime != 0.0 ) {
    if ( defined $previousRunProps{"forcing.offset"} ) { 
        if ( $previousRunProps{"forcing.offset"} ne "off" ) {
            # collect the offset value at the end of the previous run
            $offsetFactorAtPreviousRunFinish = $previousRunProps{"forcing.offset.factor.atrunfinish"};
            &stderrMessage("INFO","This scenario is hotstarting from a run that an offset factor of $offsetFactorAtPreviousRunFinish at the time the hotstart file was written.");
        }
    } else {
        # in case the previous run was produced with a version of the asgs
        # that does not write this property
        &stderrMessage("INFO","This scenario is hotstarting from a run that either had the offset turned off or did not define any offset properties.");
        $previousRunProps{"forcing.offset"} = "off";
    }
}
#
# check to see if offset is turned off in this scenario but was turned on 
# in the previous run -- if so, then ramp it down to zero over the course
# of this scenario : case -2
if ( $runProps{"forcing.offset"} eq "off" ) {
    if ( $offsetFactorAtPreviousRunFinish != 0.0 ) {
        # offset was previously turned on, it needs to be turned on at a steady value in this
        # scenario or ADCIRC will experience numerical issues
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.severity"} = "ERROR";
        # create properties related to the modified offset
        $runProps{"forcing.offset.modified.type"} = "dynamic";
        $runProps{"forcing.offset.modified.reason"} = "Offset was configured to be off in this scenario but it was turned on in the previous run. This will cause numerical issues in ADCIRC, so it will be ramped down to zero over the course of this scenario.";       
        &stderrMesage("ERROR",$runProps{"forcing.offset.modified.reason"});
        $runProps{"forcing.offset.modified.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.modified.offsetfactor.atrunfinish"} = 0.0;
        $runProps{"forcing.offset.modified.offsetfactorstart"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.modified.offsetfactorfinish"} = 0.0;
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = $hstime;
        $runProps{"forcing.offset.modified.offsetstart"} = $runstartdate;
        &writeControlAndOffsetFiles();
        exit;
    } else {
        # offset is turned off, and was off in the previous run, or there was no
        # previous run, so no need to continue with this script
        &stderrMessage("DEBUG","Case -3.");
        &stderrMessage("INFO","This scenario is not configured to use the water level offset (i.e., bias correction). It will not be activated.");
        exit;
    }
}
#
# set offset start based on configuration; Operator can set  
# explicitly as yyyymmddhh24 values or set to "coldstart" 
if ($runProps{"forcing.offset.config.offsetstartdatetime"} eq "coldstart") {
    $runProps{"forcing.offset.offsetstartdatetime"} = $csdate;   
} else {
    $runProps{"forcing.offset.offsetstartdatetime"} = $runProps{"forcing.offset.config.offsetstartdatetime"};
} 
# 
# get offset start and end time in seconds since cold start
$runProps{"forcing.offset.offsetstartdatetime"}=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $offsy = $1;
my $offsm = $2;
my $offsd = $3;
my $offsh = $4;
my $offsmin = 0;
my $offss = 0;         
# get difference (in seconds) from the cold start time
(my $ddays, my $dhrs, my $dmin, my $dsec)
    = Date::Pcalc::Delta_DHMS(
            $cy,$cm,$cd,$ch,$cmin,$cs,
            $offsy,$offsm,$offsd,$offsh,$offsmin,$offss);
# get time difference between coldstartdate and offset start time
$runProps{"forcing.offset.offsetfactorstart.seconds"} 
    = $ddays*86400.0 + $dhrs*3600.0 + $dmin*60.0 + $dsec;
my $offsetStartSec = $runProps{"forcing.offset.offsetfactorstart.seconds"};
#
# Set offset end date based on configuration; Operator can set  
# explicitly as yyyymmddhh24 values or set to a certain number
# of hours or days after the offset start time, e.g., "12hours"
# or "10days".
my $offfy;
my $offfm;
my $offfd;
my $offfh;
my $offfmin = 0;
my $offfs = 0;         
my $offsetConfigFinish = $runProps{"forcing.offset.config.offsetfinishdatetime"};
if ($offsetConfigFinish =~ /(\d+)hours/) {
    $dhrs = $1;
    ($offfy,$offfm,$offfd,$offfh,$offfmin,$offfs) =
       Date::Pcalc::Add_Delta_DHMS($offsy,$offsm,$offsd,$offsh,$offsmin,$offss,0,$dhrs,0,0);
} elsif ($offsetConfigFinish =~ /(\d+)day/) {
    $ddays = $1;
    ($offfy,$offfm,$offfd,$offfh,$offfmin,$offfs) =
       Date::Pcalc::Add_Delta_DHMS($offsy,$offsm,$offsd,$offsh,$offsmin,$offss,$ddays,0,0,0);
} else {
    $runProps{"forcing.offset.offsetfinishdatetime"} = $runProps{"forcing.offset.config.offsetfinishdatetime"}; 
    $runProps{"forcing.offset.offsetfinishdatetime"}=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
    $offfy = $1;
    $offfm = $2;
    $offfd = $3;
    $offfh = $4;
}
#
# find total seconds difference between cold start and completion of offset
#print "$cy,$cm,$cd,$ch,$cmin,$cs,$offfy,$offfm,$offfd,$offfh,$offfmin,$offfs"; 
($ddays, $dhrs, $dmin, $dsec)
    = Date::Pcalc::Delta_DHMS(
            $cy,$cm,$cd,$ch,$cmin,$cs,
            $offfy,$offfm,$offfd,$offfh,$offfmin,$offfs); 
$runProps{"forcing.offset.offsetfactorfinish.seconds"} 
    = $ddays*86400.0 + $dhrs*3600.0 + $dmin*60.0 + $dsec;
my $offsetFinishSec = $runProps{"forcing.offset.offsetfactorfinish.seconds"};
#
# case "-1" : Operator error; offset end time before offset start time
if ($runProps{"forcing.offset.offsetfactorstart.seconds"} > $runProps{"forcing.offset.offsetfactorfinish.seconds"} ) {
    # if this is a hotstart and there was previously an offset in place,
    # keep it steady at that level
    if ( $hstime != 0.0 && $offsetFactorAtPreviousRunFinish != 0.0 ) {
        &stderrMessage("DEBUG","Case -1b-c.");
        # offset was previously turned on, it needs to be turned on at a steady value in this
        # scenario or ADCIRC will experience numerical issues
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.severity"} = "ERROR";
        # create properties related to the modified offset
        $runProps{"forcing.offset.modified.reason"} = "The specified start time of the offset is after the specified end time. The offset will be kept at a constant value, the same as at the end of the previous run.";        
        $runProps{"forcing.offset.modified.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;             
        $runProps{"forcing.offset.modified.offsetfactor.atrunfinish"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.modified.offsetfactorstart"} = $offsetFactorAtPreviousRunFinish;           
        $runProps{"forcing.offset.modified.offsetfactorfinish"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = $hstime;
        ($offsy,$offsm,$offsd,$offsh,$offsmin,$offss) =
               Date::Pcalc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,$offsetFinishSec,0,0,0);
        $runProps{"forcing.offset.modified.offsetstart"} 
               = sprintf("%4d%02d%02d%02d",$offsy,$offsm,$offsd,$offsh); 
        $runProps{"forcing.offset.modified.offsetfactorstart"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.modified.offsetfactorfinish"} = $offsetFactorAtPreviousRunFinish;
        &writeControlAndOffsetFiles();
        exit;
    } else {
        # this is a misconfiguration, there was no offset previously or it was zero,
        # deactivate offset
        &stderrMessage("DEBUG","Case -1a.");
        $runProps{"forcing.offset.deactivated.reason"} = "The offset end time is prior to the offset start time.";
        $runProps{"forcing.offset.deactivated.severity"} = "ERROR";
        &deactivateOffset();
        exit;
    }
}
#-------------------------------------------------------------------- 
#
# There are nine possible cases for the relationship between the 
# specified start and finish times of the dynamic offset and the 
# coldstart time, hotstart time, and run end time.
#          
#                                 hotstart  
#        coldstart            "off" (b)
# CASE     (a)            "dynamic" (c)                           
#  0. S---F |                        |                      |
#  1. S-----+-------------F          |                      |
#  2. S-----+------------------------+-------------F        |
#  3. S-----+------------------------+----------------------+-------F
#  4.       |         S---F          |                      |
#  5.       |         S--------------+-------------F        |
#  6.       |         S--------------+----------------------+-------F
#  7.       |                        |         S---F        |
#  8.       |                        |         S------------+-------F
#  9.       |                        |                      |   S---F
#           +------------------------+----------------------+
#           ^                        ^                      ^
#       coldstart                 hotstart                RNDAY
#
#  S: start time for dynamic offset    F: finish time for dynamic offset
#
#--------------------------------------------------------------------
# There are three possible situations in terms of how this run is started 
# and relationship to the run it was started from (if any)
#
#  a: coldstart     
#  b: hotstart, offset was previously set to "off"        
#  c: hotstart, offset was previously set to "dynamic"
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# 
# if the Operator wants ASGS to compute the right starting
# factor for the offset
if ( $offsetFactorStart eq "auto" ) {
    if ( $hstime != 0.0 ) {
        if ( $previousRunProps{"forcing.offset"} eq "off" ) {
            # hotstart with no previous offset value -- start at zero
            $offsetFactorStart = 0.0;
            $runProps{"forcing.offset.derived.reason"} = "Starting offset factor set to 0.0 because this scenario is hotstarted from a run with no offset applied."; 

        } else {
            # hotstart with previous offset value -- start at that value
            $offsetFactorStart = $offsetFactorAtPreviousRunFinish;
            $runProps{"forcing.offset.derived.reason"} = "Set starting offset factor to $offsetFactorStart because this scenario is hotstarted from a run that finished with that offset factor."; 
        }
    } else {
        # coldstart -- start at zero
        $offsetFactorStart = 0.0;
        $runProps{"forcing.offset.derived.reason"} = "Starting offset factor set to 0.0 because this scenario is a cold start."; 
    }
    $runProps{"forcing.offset.derived"} = $offsetFactorStart;
    $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorStart;
}

#
#   S E T   O F F S E T S   I N   C O L D   S T A R T
#
if ( $hstime == 0.0 ) {
    if ($offsetFactorStart != 0.0) {
        $runProps{"forcing.offset.modified.offsetfactorstart.severity"} = "ERROR";
        $runProps{"forcing.offset.modified.offsetfactorstart.reason"} =
            "Offset starting factor nonzero in cold start; resetting starting offset factor to zero.";
        &zeroStartingOffset();
    } else {
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorStart;        
    }
    #  0-2a. turn off offset feature b/c before cold start (error)
    if ($offsetStartSec < 0 && $offsetFinishSec <= ($RNDAY*86400.0)) {
        &stderrMessage("DEBUG","Case 0-2a.");
        $runProps{"forcing.offset.deactivated.reason"} 
            = "Offset deactivated because the offset start is before the cold start date/time.";
        $runProps{"forcing.offset.deactivated.severity"} = "ERROR";
        &deactivateOffset();
        exit;
    }
    #  3a. reset starting offset factor to 0.0 at coldstart time (warning message)
    #      use two datasets in offset.dat, first with offset factor of 0.0, 
    #      second using final offset factor, time increment set to (F)
    if ( $offsetStartSec < 0 && $offsetFinishSec > ($RNDAY*86400.0) ) {
        &stderrMessage("DEBUG","Case 3a.");
        $runProps{"forcing.offset.datasets"} = 2;
        # interpolate to find the offset factor at time=RNDAY
        $offsetFactorAtRunFinish = $offsetFactorFinish * ( ($RNDAY*86400.0) / $offsetFinishSec );            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        # create properties related to the modified offset
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = "0.0";
        $runProps{"forcing.offset.modified.offsetstartdatetime"} = $csdate;
        my $reason = "Offset start time was prior to coldstart."; 
        $runProps{"forcing.offset.modified.offsetfactorstartdatetime.reason"} = $reason;
        my $severity = "ERROR"; 
        $runProps{"forcing.offset.modified.offsetfactorstartdatetime.severity"} = $severity;
        $offset_line .= "# $severity: offsetControl modified: $reason\n";
        &stderrMessage($severity,$reason); 
        $runProps{"forcing.offset.timeincrement"} = $offsetFinishSec;
        &writeControlAndOffsetFiles();
        exit;
    }
    #  4,5,7a. apply offset as specified, reset starting factor to 0.0 if needed (info/warning) 1 ds/wramp
    if ( $offsetStartSec >= 0 && $offsetFinishSec <= ($RNDAY*86400.0) ) {
        &stderrMessage("DEBUG","Case 4,5,7a.");
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorFinish;
        &writeControlAndOffsetFiles();
        exit;
    }
    # cold start case 6&8a: offset starts after coldstart and ends after RNDAY
    #  apply offset as specified, reset starting factor to 0.0 if needed (info/warning)
    if ( $offsetStartSec >= 0 && $offsetStartSec < ($RNDAY*86400.0) && $offsetFinishSec > ($RNDAY*86400.0) ) {         
        &stderrMessage("DEBUG","Case 6,8a.");
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorStart;
        $offsetFactorAtRunFinish = $offsetFactorFinish * 
            ( (($RNDAY*86400.0) - $offsetStartSec)  / ($offsetFinishSec - $offsetStartSec) );            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        &writeControlAndOffsetFiles();
        exit;
    }
}                                   
#
#   S E T   O F F S E T S   I N   H O T   S T A R T
#        N O   P R E V I O U S   O F F S E T
#
if ( $hstime != 0.0 && $previousRunProps{"forcing.offset"} eq "off" ) {
    if ($offsetFactorStart != 0.0) {
        $runProps{"forcing.offset.modified.offsetfactorstart.severity"} = "WARNING";
        $runProps{"forcing.offset.modified.offsetfactorstart.reason"} =
        "Offset starting factor was nonzero but previous run had no offset applied; initial offset factor reset to zero.";             
        &zeroStartingOffset();
    } else {
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorStart;        
    }
    #  0-2b. do not use the offset feature (info/info/error message)
    if ($offsetStartSec < 0 && $offsetFinishSec < ($RNDAY*86400.0)) {
        &stderrMessage("DEBUG","Case 0-2b.");
        $runProps{"forcing.offset.deactivated.severity"} = "INFO";
        if ( $offsetFinishSec > $hstime ) {
            $runProps{"forcing.offset.deactivated.severity"} = "ERROR";
        } 
        $runProps{"forcing.offset.deactivated.reason"} 
            = "Offset deactivated because the offset start is before the cold start date/time and the previous run had no offset.";
        &deactivateOffset();
        exit;             
    }
    #  3b.,6b. use one dataset in offset.dat, ramp starting at runstartdate
    #          ending at offsetfactorfinishdatetime.
    if ( ($offsetStartSec < 0 || $offsetStartSec < $hstime) && $offsetFinishSec > ($RNDAY*86400.0) ) {
        &stderrMessage("DEBUG","Case 3,6b.");
        # interpolate to find the offset factor at time=RNDAY
        $offsetFactorAtRunFinish = $offsetFactorFinish * 
            ( (($RNDAY*86400.0) - $hstime) / ($offsetFinishSec - $hstime) );            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        # create properties releated to the modified offset
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.offsetstart.seconds"} = $hstime;
        $runProps{"forcing.offset.modified.offsetstartdatetime"} = $runstartdate; 
        &writeControlAndOffsetFiles();
        exit;
    }        
    #  4b. do not use the offset feature (info message)
    #  offset start time and end time before hot start time
    if ($offsetStartSec < $hstime && $offsetFinishSec <= $hstime ) {
        &stderrMessage("DEBUG","Case 4b.");
        $runProps{"forcing.offset.deactivated.severity"} = "INFO"; 
        $runProps{"forcing.offset.deactivated.reason"} 
            = "Offset deactivated because the offset start and end times are before the hotstart time and no offset was applied in the previous run.";
        &deactivateOffset();
        exit;             
    }         
    #  5b.  offset starts prior to hotstart time and ends between hotstart time
    #       and run end time
    if ( $offsetStartSec > 0 && $offsetStartSec < $hstime && $offsetFinishSec <= ($RNDAY*86400.0) ) {
        &stderrMessage("DEBUG","Case 5b.");
        $offsetFactorAtRunFinish = $offsetFactorFinish;            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        # create properties releated to the modified offset
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = $hstime;
        $runProps{"forcing.offset.modified.offsetstartdatetime"} = $runstartdate;
        &writeControlAndOffsetFiles();
        exit;
    }        
    #  7b. apply offset as specified
    if ( $offsetStartSec >= $hstime && $offsetFinishSec <= ($RNDAY*86400.0) ) {         
        &stderrMessage("DEBUG","Case 7b.");
        $offsetFactorAtRunFinish = $offsetFactorFinish;            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        &writeControlAndOffsetFiles();
        exit;
    }
    #  8b. apply offset as specified, reset starting factor to 0.0 if needed (info/warning)
    if ( $offsetStartSec >= $hstime && $offsetStartSec < ($RNDAY*86400) && $offsetFinishSec > ($RNDAY*86400.0) ) {
        &stderrMessage("DEBUG","Case 8b.");
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorStart;
        # interpolate to find the offset factor at time=RNDAY
        $offsetFactorAtRunFinish = $offsetFactorFinish * 
            ( (($RNDAY*86400.0) - $hstime) / ($offsetFinishSec - $hstime) );            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        &writeControlAndOffsetFiles();
        exit;
    }
}
#
#       S E T   O F F S E T S   I N   H O T   S T A R T
#     W I T H   P R E V I O U S   D Y N A M I C   O F F S E T
#
if ( $hstime != 0.0 && $previousRunProps{"forcing.offset"} eq "dynamic" ) {
    if ( $offsetFactorAtRunStart != $offsetFactorAtPreviousRunFinish ) { 
        $runProps{"forcing.offset.modified.offsetfactoratrunstart.severity"} = "INFO";
        $runProps{"forcing.offset.modified.offsetfactoratrunstart.reason"} =
            "Offset factor at end of previous run is unexpectedly different from the specified offset finish factor. Resetting to same value as end of previous run.";
        $offsetFactorStart = $offsetFactorAtPreviousRunFinish; 
        &writeControlAndOffsetFiles();
        exit;
    }
    #  0-1c. apply steady offset factor at same value as end of previous run (info message)
    #         single data set in offset.dat multiplied by offset factor from previous run
    #  4c. apply steady offset at same value as end of previous run (info message)
    #         single data set in offset.dat multiplied by offset factor from previous run
    if ( $offsetFinishSec < $hstime ) {
        &stderrMessage("DEBUG","Case 0,1c.");
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtPreviousRunFinish;
        &writeControlAndOffsetFiles();
        exit;
    }
    #  2c. reset starting offset factor to be same value as end of previous run
    #         use two datasets in offset.dat, one with factor from hotstart, one with 
    #         final offset factor, time increment set to (F - hotstart)
    if ( $offsetStartSec < 0 && $offsetFinishSec > $hstime ) {
        &stderrMessage("DEBUG","Case 2c.");
        $runProps{"forcing.offset.datasets"} = 2;
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorFinish; 
        $runProps{"forcing.offset.timeincrement"} = $offsetFinishSec - $hstime;
        &writeControlAndOffsetFiles();
        exit;
    }
    #  3c. reset starting offset factor to value at end of previous run at hotstart time (warning message)
    #         use two datasets in offset.dat, one using offset factor from hotstart,
    #         second using final offset factor, time increment set to (F - hotstart)
    #  6c. reset starting offset factor to value at end of previous run at hotstart time (warning message)
    #         use two datasets in offset.dat, one using offset factor from hotstart,
    #         second using final offset factor, time increment set to (F - hotstart)   
    if ( $offsetStartSec < 0 && $offsetFinishSec > ($RNDAY*86400.0) ) {
        &stderrMessage("DEBUG","Case 3c.");
        $runProps{"forcing.offset.datasets"} = 2;
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;            
        # interpolate to find the offset factor at time=RNDAY
        $offsetFactorAtRunFinish = ( $offsetFactorFinish - $offsetFactorStart ) * 
            ( (($RNDAY*86400.0) - $hstime) / ($offsetFinishSec - $hstime) ) + $offsetFactorStart;            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish; 
        $runProps{"forcing.offset.timeincrement"} = $offsetFinishSec - $hstime;
        &writeControlAndOffsetFiles();
        exit;        
    }
    #  5c. reset starting offset factor to value at end of previous run at hotstart time (warning message)
    #         use two datasets in offset.dat, one using offset factor from hotstart,
    #         second using final offset factor, time increment set to (F - hotstart)
    if ( $offsetStartSec > 0 && $offsetStartSec < $hstime && $offsetFinishSec > ($RNDAY*86400.0) ) {
        &stderrMessage("DEBUG","Case 5c.");
        $runProps{"forcing.offset.datasets"} = 2;
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorFinish; 
        $runProps{"forcing.offset.timeincrement"} = $offsetFinishSec - $hstime;
        &writeControlAndOffsetFiles();
        exit;        
    }
    #  7c. reset starting offset factor to value at end of previous run at hotstart time (warning message)
    #         use two datasets in offset.dat, one using offset factor from hotstart,
    #         second using final offset factor, time increment set to (F - hotstart)
    #  8c. reset starting offset factor to value at end of previous run at hotstart time (warning message)
    #         compute/reset finishing offset factor to be the same as it would be 
    #         at RNDAY with the original set of factors and timing
    #         then set time increment to (RNDAY - hotstart) and use two datasets
    #         in offset.dat
    if ( $offsetStartSec > $hstime && $offsetStartSec < ($RNDAY*86400.0) ) {
        &stderrMessage("DEBUG","Case 7,8c.");
        $runProps{"forcing.offset.datasets"} = 2;
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.reason"} =
            "Offset factor starts after the hotstart time. It will be modified so it starts at the hotstart time.";
        $runProps{"forcing.offset.modified.severity"} = "INFO";
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = $hstime;
        ($offsy,$offsm,$offsd,$offsh,$offsmin,$offss) =
            Date::Pcalc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,$hstime,0,0,0);
        $runProps{"forcing.offset.modified.offsetstart"} 
            = sprintf("%4d%02d%02d%02d",$offsy,$offsm,$offsd,$offsh); 
        if ( $offsetFactorStart != $offsetFactorAtPreviousRunFinish ) {                
            $runProps{"forcing.offset.modified.reason"} .=
            " In addition, offset factor at end of previous run is unexpectedly different from the specified offset start factor. Resetting offset start factor to same value as at end of previous run.";
            $offsetFactorStart = $offsetFactorAtPreviousRunFinish;
            $offsetFactorAtRunStart = $offsetFactorStart;
            $runProps{"forcing.offset.modified.offsetfactorstart"} = $offsetFactorAtPreviousRunFinish;
        }
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorFinish;
        $offsetTimeIncrement = $offsetFinishSec - $hstime;
        &writeControlAndOffsetFiles();
        exit;        
    }
}
#   
# case 9: offset applied after RNDAY
#  9a-c. do not use the offset feature (info message)
if ($offsetStartSec > ($RNDAY*86400.0) && $offsetFinishSec > ($RNDAY*86400.0)) {
    &stderrMessage("DEBUG","Case 9a-c.");
    $runProps{"forcing.offset.deactivated.reason"} 
        = "Offset deactivated because the offset start is after the end of the run (RNDAY).";
    $runProps{"forcing.offset.deactivated.severity"} = "INFO"; 
    &deactivateOffset();
    exit;
}
exit;
#
#--------------------------------------------------------------------------
#           S U B   Z E R O   S T A R T I N G  O F F S E T
#
# Zeroes out starting offset ... usually because the previous run did not
# have it and we don't want to create a numerical shock. 
#--------------------------------------------------------------------------
sub zeroStartingOffset() {          
    $runProps{"forcing.offset.modified"} = "true";
    $runProps{"forcing.offset.modified.offsetfactorstart"} = 0.0;        
    my $severity = $runProps{"forcing.offset.modified.offsetfactorstart.severity"};
    my $reason = $runProps{"forcing.offset.modified.offsetfactorstart.reason"}; 
    $offset_line .= "# $severity: offsetControl modified: $reason\n";
    &stderrMessage($severity,$reason);
    $runProps{"forcing.offset.offsetfactor.atrunstart"} = 0.0;
}
#
#--------------------------------------------------------------------------
#           S U B     D E A C T I V A T E    O F F S E T
#
# Turns offset off in fort.15
#--------------------------------------------------------------------------
sub deactivateOffset() {
    $runProps{"forcing.offset.deactivated"} = "true";
    my $severity = $runProps{"forcing.offset.deactivated.severity"};
    &stderrMessage($severity,$runProps{"forcing.offset.deactivated.reason"});
    $offset_line = "# offsetControl: " . $runProps{"forcing.offset.deactivated.reason"};
    &stderrMessage($severity,$runProps{"forcing.offset.deactivated.reason"});   
    unless (open(FORT15,">>$scenariodir/fort.15")) {
        stderrMessage("ERROR","Failed to open the $scenariodir/fort.15 for appending the &offsetControl namelist: $!.");
        die;
    } 
    printf FORT15 "$offset_line\n";
    close(FORT15);
    unless (open(RUNPROPS,">>$scenariodir/run.properties")) {
        stderrMessage("ERROR","Failed to open the $scenariodir/run.properties for appending the offsetControl namelist properties: $!.");
        die;
    }
    # cycle through keys associated with offset deactivation and write
    # the associated properties to the run.properties file
    while ((my $key, my $value) = each (%runProps)) {
        if ( $key =~ /forcing.offset.deactivated/ ) {
            printf RUNPROPS "$key : $value\n";
        }   
    }
    close(RUNPROPS);
    exit;
}    
#
#--------------------------------------------------------------------------
#   S U B    W R I T E   C O N T R O L   A N D   O F F S E T   F I L E S
#
# Appends to fort.15, creates the offset.dat file, and appends to 
# run.properties.
#--------------------------------------------------------------------------
sub writeControlAndOffsetFiles() {
    my $offsetFactorStart = $runProps{"forcing.offset.offsetfactorstart"};
    my $offsetFactorFinish = $runProps{"forcing.offset.offsetfactorfinish"};
    # if the Operator selected "auto" as the starting offset
    if ( defined $runProps{"forcing.offset.derived"} ) {
        my $reason = $runProps{"forcing.offset.derived.reason"};
        $offsetFactorStart = $runProps{"forcing.offset.derived"};
        &stderrMessage("INFO",$reason);
        $offset_line .= "# INFO: 'auto' setting for offsetControl: $reason\n"; 
    }
    # write modified and derived properties to run.properties file
    unless (open(RUNPROPS,">>$scenariodir/run.properties")) {
        stderrMessage("ERROR","Failed to open the $scenariodir/run.properties for appending the offsetControl namelist properties: $!.");
        die;
    }
    # cycle through keys associated with modified and derived values and write
    # the associated properties to the run.properties file
    while ((my $key, my $value) = each (%runProps)) {
        if ( $key =~ /forcing.offset.*modified/ ) {
            printf RUNPROPS "$key : $value\n";
            if ( $key eq "forcing.offset.modified.offsetfactorfinish" ) {
                $offsetFactorFinish = $value;
            }
            if ( $key eq "forcing.offset.modified.offsetstart.seconds" ) {
                $offsetStartSec = $value;
            }
            if ( $key eq "forcing.offset.modified.offsetfinish.seconds" ) {
                $offsetFinishSec = $value;
            }            
            if ( $key eq "forcing.offset.modified.offsetfactorstart" ) {
                $offsetFactorStart = $value;
            }
            if ( $key eq "forcing.offset.modified.offsetfactorfinish" ) {
                $offsetFactorFinish = $value;
            }            
        }   
    }
    #$runProps{"forcing.offset.offsetfactorstart.seconds"}
    #$runProps{"forcing.offset.offsetfactorfinish.seconds"}
    my $offsetFactorAtRunStart = $runProps{"forcing.offset.offsetfactor.atrunstart"}; 
    my $offsetFactorAtRunFinish = $runProps{"forcing.offset.offsetfactor.atrunfinish"}; 
    my $timeIncrement = $runProps{"forcing.offset.timeincrement"};
    my $datasets = $runProps{"forcing.offset.datasets"};
    printf RUNPROPS "forcing.offset.offsetfactor.atrunstart : $offsetFactorAtRunStart\n"; 
    printf RUNPROPS "forcing.offset.offsetfactor.atrunfinish : $offsetFactorAtRunFinish\n"; 
    printf RUNPROPS "forcing.offset.timeincrement : $timeIncrement\n"; 
    printf RUNPROPS "forcing.offset.datasets : $datasets\n"; 
    close(RUNPROPS);    
    #
    if ( $runProps{"forcing.offset"} eq "assimilated" ) {
        # Data assimilated offset surface, assume it starts at the hotstart 
        # time and has the right time interval and data in it.
        my $offsetFileName = $runProps{"forcing.offset.offsetfile"};
        # create the &offsetControl namelist line      
        $offset_line .= "&offsetControl offsetFileName=$offsetFileName /";   
        # &offsetControl offsetFileName='offset_test.dat', offsetSkipSnaps=0, offsetMultiplier=0.1, 
        # offsetRampStart=0.0, offsetRampEnd=43200.0, 
    } elsif ( $runProps{"forcing.offset.datasets"} == 1 ) {
        # dynamic offset using 1 dataset in offset.dat
        $offset_line .= "&offsetControl offsetFileName='offset.dat', ";
        $offset_line .= "offsetMultiplier=$offsetFactorFinish, ";
        $offset_line .= "offsetRampStart=$offsetStartSec, ";
        $offset_line .= "offsetRampEnd=$offsetFinishSec, ";
        $offset_line .= "offsetRampReferenceTime='coldstart' /";       
    } else {
        # dynamic offset using 2 datasets in offset.dat
        $offset_line .= "&offsetControl offsetFileName='offset.dat' /";
    }
    unless (open(FORT15,">>$scenariodir/fort.15")) {
        stderrMessage("ERROR","Failed to open the $scenariodir/fort.15 for appending the &offsetControl namelist: $!.");
        die;
    } 
    printf FORT15 "$offset_line\n";
    close(FORT15);
    # can exit now if the offset data file was supplied by
    # an external process
    if ( $runProps{"forcing.offset"} eq "assimilated") {
        exit;
    }
    #
    # open unit offset data file -- it is assumed to be in the 
    # input directory (i.e., same directory as the mesh)
    my $unitOffsetFileName = $runProps{"path.inputdir"} . "/" . $runProps{"forcing.offset.offsetfile"}; 
    unless (open(UNITOFFSET,"<$unitOffsetFileName")) {
        stderrMessage("ERROR","Failed to open the unit offset data file $unitOffsetFileName for reading: $!.");
        die;
    }
    # open the offset data file that will be fed to ADCIRC
    my $offsetDataFile = $runProps{"path.scenariodir"} . "/" . "offset.dat";    
    unless (open(DYNAMICOFFSET,">$offsetDataFile")) {
        stderrMessage("ERROR","Failed to open the dynamic offset file $offsetDataFile for writing: $!.");
        die;
    }
    stderrMessage("INFO","The offset.dat file will be written to the directory $scenariodir.");
    my $offsetTimeIncrement = $runProps{"forcing.offset.timeincrement"};
    my $currentDataset = 1;
    my $multiplier = 1.0;
    if ( $runProps{"forcing.offset.datasets"} == 2 ) {
        $multiplier = $offsetFactorStart;
    }
    my $comment = "$offsetDataFile from $unitOffsetFileName and $scenariodir/run.properties";
    while(<UNITOFFSET>) {
        s/%comment%/$comment/;
        s/%timeincrement%/$timeIncrement/;
        #&stderrMessage("DEBUG","multiplier is $multiplier");
        if ( $.<=3) {
            print DYNAMICOFFSET $_;
        } elsif ( $_ =~ /##/ && $.>3) {
            print DYNAMICOFFSET $_; 
            if ( $runProps{"forcing.offset.datasets"} == 1 ) {
                last;
            } else {
                $multiplier = $offsetFactorFinish;
            }
        } else {
            my @col = split(' ',$_);
            my $modvalue = $col[1] * $multiplier;
            printf DYNAMICOFFSET "$col[0] $modvalue\n";
        } 
    }
    close(UNITOFFSET);
    close(DYNAMICOFFSET);
    exit;
}
#
#--------------------------------------------------------------------------
#   S U B   S T D E R R  M E S S A G E
#
# Writes a log message to standard error.
#--------------------------------------------------------------------------
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: " . $runProps{"scenario"} .": $this : $message\n";
}


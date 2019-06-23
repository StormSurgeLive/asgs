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
#
# Get command line option (directory where the fort.15 and run.properties files are found)
GetOptions("scenariodir=s" => \$scenariodir);
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
   $runPropS{$fields[0]} = $fields[1];
}
close(RUNPROPS);
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
my $fromdir = null;
my $from_properties = null;
unless ( $runProps{"scenario"} eq "hindcast" || $runProps{"scenario"} eq "spinup" ) {
   $fromdir = $runProps{"path.fromdir"};
   if ( $runProps{"url.hotstart"} eq "null" ) {
      $from_properties =  "$fromdir/run.properties";
   } else {
      # starting from a hotstart file downloaded from URL
      $from_properties =  $runProps{"path.rundir"} . "/from.run.properties";   
   }
} else {
   # this is a tide/river spinup, there are no previous run properties, this is 
   # the first run from cold start, nothing needs to be done here
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

my $offsetFactorAtPreviousRunFinish = 0.0; # offset at end of run we are hotstarting from (if any)
$runProps{"forcing.offset.datasets"} = 1; # whether to use 1 or 2 datasets in offset.dat
$runProps{"forcing.offset.timeincrement"} = -99999.0 # in offset.dat file
$runProps{"forcing.offset.deactivated"} = "false"; # must be turned off under certain circumstances
$runProps{"forcing.offset.modified"} = "false";    # must be modified under certain circumstances
#
# if hotstarting, see if there was an offset in place at end of previous run
if ( defined $hstime ) {
    if ( defined $previousRunProps{"forcing.offset"} && $previousRunProps{"forcing.offset"} ne "off" ) {
        # collect the offset value at the end of the previous run
        $offsetFactorAtPreviousRunFinish = $previousRunProps{"forcing.offset.factor.atrunfinish"};
    }
} else {
    # in case the previous run was produced with a version of the asgs
    # that does not write this property
    $previousRunProps{"forcing.offset"} = "off";
}
#
# check to see if offset is turned off in this scenario but was turned on 
# in the previous run -- if so, then ramp it down to zero over the course
# of this scenario
if ( $runProps{"forcing.offset"} eq "off" ) {
    if ( $offsetFactorAtPreviousRunFinish != 0.0 ) {
        # offset was previously turned on, it needs to be turned on at a steady value in this
        # scenario or ADCIRC will experience numerical issues
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.severity"} = "ERROR";
        # create properties related to the modified offset
        $runProps{"forcing.offset.modified.type"} = "dynamic";
        $runProps{"forcing.offset.modified.reason"} = "Offset was configured to be off in this scenario but it was turned on in the previous run. This will cause numerical issues in ADCIRC, so it will be ramped down to zero over the course of this scenario.");       
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
        &stderrMessage("INFO","This scenario is not configured to use the water level offset (i.e., bias correction).");
        exit;
    }
}
#
# set offset start and end date/times based on configuration; Operator
# can set these explicitly as yyyymmddhh24 values or set them to 
# "coldstart", "hotstart", "end", or "auto". 
if ($runProps{"forcing.offset.config.offsetstartdatetime"} eq "coldstart") {
    $runProps{"forcing.offset.offsetstartdatetime"} = $csdate;   
} elsif ($runProps{"forcing.offset.config.offsetstartdatetime"} eq "hotstart" ||
         $runProps{"forcing.offset.config.offsetstartdatetime"} eq "auto" ) {
    $runProps{"forcing.offset.offsetstartdatetime"} = $runstartdate;
} elsif 
 

# 
# get offset start and end time in seconds since cold start
$runProps{"forcing.offset.offsetstartdatetime"}=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $offsy = $1;
my $offsm = $2;
my $offsd = $3;
my $offsh = $4;
my $offsmin = 0.0;
my $offss = 0.0;         
# get difference (in seconds) from the cold start time
(my $ddays, my $dhrs, my $dmin, my $dsec)
    = Date::Pcalc::Delta_DHMS(
            $cy,$cm,$cd,$ch,cmin,cs,
            $offsy,$offsm,$offsd,$offsh,$offsmin,$offss);
# get time difference between coldstartdate and offset start time
$runProps{"forcing.offset.offsetfactorstart.seconds"} 
    = $ddays*86400.0 + $dhrs*3600.0 + $dmin*60.0 + $dsec;
# finish      
$runProps{"forcing.offset.offsetfinishdatetime"}=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $offfy = $1;
my $offfm = $2;
my $offfd = $3;
my $offfh = $4;
my $offfmin = 0.0;
my $offfs = 0.0;         
# find total seconds difference between cold start and completion of offset
($ddays, $dhrs, $dmin, $dsec)
    = Date::Pcalc::Delta_DHMS(
            $cy,$cm,$cd,$ch,cmin,cs,
            $offfy,$offfm,$offfd,$offfh,$offfmin,$offfs); 
$runProps{"forcing.offset.offsetfactorfinish.seconds"} = 
    = $ddays*86400.0 + $dhrs*3600.0 + $dmin*60.0 + $dsec;
#
# case "-1" : Operator error; offset end time before offset start time
if ($runProps{"forcing.offset.offsetfactorstart.seconds"} > $runProps{"forcing.offset.offsetfactorfinish.seconds"} ) {
    # if this is a hotstart and there was previously an offset in place,
    # keep it steady at that level
    if ( defined $hstime && $offsetFactorAtPreviousRunFinish != 0.0 ) {
        # offset was previously turned on, it needs to be turned on at a steady value in this
        # scenario or ADCIRC will experience numerical issues
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.severity"} = "ERROR";
        # create properties related to the modified offset
        $runProps{"forcing.offset.modified.reason"} = "The specified start time of the offset is after the specified end time. The offset will be kept at a constant value, the same as at the end of the previous run.");        
        $runProps{"forcing.offset.modified.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;             
        $runProps{"forcing.offset.modified.offsetfactor.atrunfinish"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.modified.offsetfactorstart"} = $offsetFactorAtPreviousRunFinish;           
        $runProps{"forcing.offset.modified.offsetfactorfinish"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = $hstime;
        ($offsy,$offsm,$offsd,$offsh,$offsmin,$ofsss) =
               Date::Pcalc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,$offsetFinishSec,0,0,0);
        $runProps{"forcing.offset.modified.offsetstart"} 
               = sprintf("%4d%02d%02d%02d",$offsy,$offsm,$offsd,$offsh); 
        $runProp{"forcing.offset.modified.offsetfactorstart"} = $offsetFactorAtRunStart;
        $runProp{"forcing.offset.modified.offsetfactorfinish"} = $offsetFactorAtRunStart;
        &writeControlAndOffsetFiles();
        exit;
    } else {
        # this is a misconfiguration, there was no offset previously or it was zero,
        # deactivate offset
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
    if ( defined $hstime ) {
        if ( $previousRunProps{"forcing.offset"} eq "off" ) {
            # hotstart with no previous offset value
            $offsetFactorStart = 0.0;
        } else {
            # hotstart with previous offset value
            $offsetFactorStart = $offsetFactorAtPreviousRunFinish;
        }
    } else {
        # coldstart
        $offsetFactorStart = 0.0;
    }
}
#
#   S E T   O F F S E T S   I N   C O L D   S T A R T
#
unless ( defined $hstime ) {
    #  0-2a. turn off offset feature b/c before cold start (error)
    if ($offsetStartSec < 0 && $offsetFinishSec < ($RNDAY*86400.0)) {
        $runProp{"forcing.offset.deactivated.reason"} 
            = "Offset deactivated because the offset start is before the cold start date/time.";
        $runProp{"forcing.offset.deactivated.severity"} = "ERROR";
        &deactivateOffset();
        exit;
    }
    #  3a. reset starting offset factor to 0.0 at coldstart time (warning message)
    #      use two datasets in offset.dat, first with offset factor of 0.0, 
    #      second using final offset factor, time increment set to (F)
    if ( $offsetStartSec < 0 && $offsetFinishSec > ($RNDAY*86400.0) ) {
        $runProps{"forcing.offset.datasets"} = 2;
        $offsetFactorAtRunStart = 0.0;
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtRunStart;
        # interpolate to find the offset factor at time=RNDAY
        $offsetFactorAtRunFinish = $offsetFactorFinish * ( ($RNDAY*86400.0) / $offsetFinishSec );            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        # create properties releated to the modified offset
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = 0.0;
        ($offsy,$offsm,$offsd,$offsh,$offsmin,$ofsss) =
            Date::Pcalc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,$offsetFinishSec,0,0,0);
        $runProps{"forcing.offset.modified.offsetstart"} 
            = sprintf("%4d%02d%02d%02d",$offsy,$offsm,$offsd,$offsh); 
        if ( $offsetFactorStart != 0.0 ) { 
            $runProp{"forcing.offset.modified.offsetfactorstart"} = 0.0;
            $offsetFactorStart = 0.0;
        }
        $runProps{"forcing.offset.modified.reason"} =
            "Offset start time is prior to cold start; resetting it to coldstart time and initial offset factor to zero.";            
        $runProps{"forcing.offset.modified.severity"} = "WARNING";
        $runProps{"forcing.offset.timeincrement"} = $offsetFinishSec;
        &writeControlAndOffsetFiles();
        exit;
    }
    #  4,5,7a. apply offset as specified, reset starting factor to 0.0 if needed (info/warning) 1 ds/wramp
    if ( $offsetStartSec > 0 && $offsetFinishSec < ($RNDAY*86400.0) ) {
        $offset_line .= "&offsetControl offsetFileName='offset.dat', ";
        $offset_line .= "offsetMultiplier=$offsetFactorFinish, ";
        $offset_line .= "offsetRampStart=$offsetStartSec, ";
        $offset_line .= "offsetRampEnd=$offsetFinishSec, /";
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorStart;
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorFinish;
        }
    # cold start case 6&8: offset starts after coldstart and ends after RNDAY
    #  6a. apply offset as specified, reset starting factor to 0.0 if needed (info/warning)
    #  8a. apply offset as specified, reset starting factor to 0.0 if needed (info/warning)
    if ( $offsetStartSec > 0 && $offsetFinishSec > ($RNDAY*86400.0) ) {         
    $offsetDataSets = 1;
    if ( $offsetFactorStart != 0.0 ) { 
        $runProps{"forcing.offset.modified"} = "true";
        $runProp{"forcing.offset.modified.offsetfactorstart"} = 0.0;
        $offsetFactorStart = 0.0;
        $runProps{"forcing.offset.modified.reason"} =
            "Offset starting factor nonzero in cold start; resetting initial offset factor to zero.";
        $offset_line = "# WARNING: offsetControl modified: ";
        $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
    } else {
        $offset_line = "# offset applied as specified:\n";
    }                                   
    $offset_line .= "&offsetControl offsetFileName='offset.dat', ";
    $offset_line .= "offsetMultiplier=$offsetFactorFinish, ";
    $offset_line .= "offsetRampStart=$offsetStartSec, ";
    $offset_line .= "offsetRampEnd=$offsetFinishSec, /";
    $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorStart;
    $offsetFactorAtRunFinish = $offsetFactorFinish * ( ($RNDAY*86400.0) / $offsetFinishSec );            
    $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
}
    #
    #   S E T   O F F S E T S   I N   H O T   S T A R T
    #        N O   P R E V I O U S   O F F S E T
    #
    } elsif ( defined $hstime && $previousRunProps{"forcing.offset"} eq "off" ) {
        #  0b. do not use the offset feature (info message)
        #  1b. do not use the offset feature (info message)
        #  2b. do not use the offset feature (error message)
        # hot start cases 0, 1, 2 : turn off offset b/c starts before coldstart
        # and no offset was applied in the previous run 
        if ($offsetStartSec < 0 && $offsetFinishSec < ($RNDAY*86400.0)) {
        # case 0, 1, 2
        $runProp{"forcing.offset.deactivated"} = "true"; 
        $runProp{"forcing.offset.deactivated.reason"} 
            = "Offset deactivated because the offset start is before the cold start date/time and the previous run had no offset.";
        &stderrMessage("ERROR",$runProp{"forcing.offset.deactivated.reason"});
        $offset_line = "#offsetControl: " . $runProp{"forcing.offset.deactivated.reason"};
        $offsetDeactivated = 1;             
        }
        #  3b.,6b. reset starting offset factor to 0.0 at hotstart time (warning message)
        #         use two datasets in offset.dat, one using offset factor of 0.0,
        #         second using final offset factor, time increment set to (F - hotstart)
        # hot start case 3,6 : reset offset start to hotstart time and starting factor to 0.0
        #  6b. reset starting offset factor to 0.0 at hotstart time (warning message)
        #         use two datasets in offset.dat, one using offset factor of 0.0,
        #         second using final offset factor, time increment set to (F - hotstart)
        if ( ($offsetStartSec < 0 || $offsetStartSec < $hstime) && $offsetFinishSec > ($RNDAY*86400.0) ) {
        $offsetDataSets = 2;
        $offsetFactorAtRunStart = 0.0;
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtRunStart;
        # interpolate to find the offset factor at time=RNDAY
        $offsetFactorAtRunFinish = $offsetFactorFinish * 
            ( (($RNDAY*86400.0) - $hstime) / ($offsetFinishSec - $hstime) );            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        # create properties releated to the modified offset
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = $hstime;
        ($offsy,$offsm,$offsd,$offsh,$offsmin,$ofsss) =
            Date::Pcalc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,$hstime,0,0,0);
        $runProps{"forcing.offset.modified.offsetstart"} 
            = sprintf("%4d%02d%02d%02d",$offsy,$offsm,$offsd,$offsh); 
        if ( $offsetFactorStart != 0.0 ) { 
            $runProp{"forcing.offset.modified.offsetfactorstart"} = 0.0;
            $offsetFactorStart = 0.0;
            $runProps{"forcing.offset.modified.reason"} =
                "Offset starting factor was nonzero but previous run had no offset applied; initial offset factor reset to zero.";            
            &stderrMessage("WARNING",$runProp{"forcing.offset.modified.reason"});
            $offset_line = "# WARNING: offsetControl Modified: ";
            $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
        } else { 
            $offset_line = "# INFO: offsetControl applied as specified.\n";
        }            
        $offset_line .= "&offsetControl offsetFileName='offset.dat' /";
        $offsetTimeIncrement = $offsetFinishSec - $hstime;
        }        
        #  4b. do not use the offset feature (info message)
        #  offset start time and end time before hot start time
        if ($offsetStartSec < $hstime && $offsetFinishSec < $hstime ) {
        $runProp{"forcing.offset.deactivated"} = "true"; 
        $runProp{"forcing.offset.deactivated.reason"} 
            = "Offset deactivated because the offset start and end times are before the hotstart time and no offset was applied in the previous run.";
        &stderrMessage("ERROR",$runProp{"forcing.offset.deactivated.reason"});
        $offset_line = "#offsetControl: " . $runProp{"forcing.offset.deactivated.reason"};
        $offsetDeactivated = 1;             
        }         
        #  5b. reset starting offset factor to 0.0 at hotstart time (warning message)
        #         use two datasets in offset.dat, one using offset factor of 0.0,
        #         second using final offset factor, time increment set to (F - hotstart)
        if ( $offsetStartSec < 0 && $offsetFinishSec > ($RNDAY*86400.0) ) {
        $offsetDataSets = 2;
        $offsetFactorAtRunStart = 0.0;
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtRunStart;
        $offsetFactorAtRunFinish = $offsetFactorFinish;            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        # create properties releated to the modified offset
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = $hstime;
        ($offsy,$offsm,$offsd,$offsh,$offsmin,$ofsss) =
            Date::Pcalc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,$hstime,0,0,0);
        $runProps{"forcing.offset.modified.offsetstart"} 
            = sprintf("%4d%02d%02d%02d",$offsy,$offsm,$offsd,$offsh); 
        if ( $offsetFactorStart != 0.0 ) { 
            $runProp{"forcing.offset.modified.offsetfactorstart"} = 0.0;
            $offsetFactorStart = 0.0;
            $runProps{"forcing.offset.modified.reason"} =
                "Offset starting factor was nonzero but previous run had no offset applied; initial offset factor reset to zero.";            
            &stderrMessage("WARNING",$runProp{"forcing.offset.modified.reason"});
            $offset_line = "# WARNING: offsetControl Modified: ";
            $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
        } else { 
            $offset_line = "# INFO: offsetControl applied as specified.\n";
        }            
        $offset_line .= "&offsetControl offsetFileName='offset.dat' /";
        $offsetTimeIncrement = $offsetFinishSec - $hstime;
        }        
        #  7b. apply offset as specified, reset starting factor to 0.0 if needed (info/warning)
        if ( $offsetStartSec > $hstime && $offsetFinishSec < ($RNDAY*86400.0) ) {         
        $offsetDataSets = 1;
        if ( $offsetFactorStart != 0.0 ) { 
            $runProps{"forcing.offset.modified"} = "true";
            $runProp{"forcing.offset.modified.offsetfactorstart"} = 0.0;
            $offsetFactorStart = 0.0;
            $runProps{"forcing.offset.modified.reason"} =
                "Offset starting factor nonzero in cold start; resetting initial offset factor to zero.";
            $offset_line = "# WARNING: offsetControl modified: ";
            $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
        } else {
            $offset_line = "# offset applied as specified:\n";
        }                                   
        $offset_line .= "&offsetControl offsetFileName='offset.dat', ";
        $offset_line .= "offsetMultiplier=$offsetFactorFinish, ";
        $offset_line .= "offsetRampStart=$offsetStartSec, ";
        $offset_line .= "offsetRampEnd=$offsetFinishSec, /";
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorStart;
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorFinish;
        }
        #  8b. apply offset as specified, reset starting factor to 0.0 if needed (info/warning)
        if ( $offsetStartSec > $hstime && $offsetFinishSec < ($RNDAY*86400.0) ) {         
        $offsetDataSets = 1;
        if ( $offsetFactorStart != 0.0 ) { 
            $runProps{"forcing.offset.modified"} = "true";
            $runProp{"forcing.offset.modified.offsetfactorstart"} = 0.0;
            $offsetFactorStart = 0.0;
            $runProps{"forcing.offset.modified.reason"} =
                "Offset starting factor nonzero in hotstart; resetting initial offset factor to zero.";
            $offset_line = "# WARNING: offsetControl modified: ";
            $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
        } else {
            $offset_line = "# offset applied as specified:\n";
        }                                   
        $offset_line .= "&offsetControl offsetFileName='offset.dat', ";
        $offset_line .= "offsetMultiplier=$offsetFactorFinish, ";
        $offset_line .= "offsetRampStart=$offsetStartSec, ";
        $offset_line .= "offsetRampEnd=$offsetFinishSec, /";
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorStart;
        # interpolate to find the offset factor at time=RNDAY
        $offsetFactorAtRunFinish = $offsetFactorFinish * 
            ( (($RNDAY*86400.0) - $hstime) / ($offsetFinishSec - $hstime) );            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish;
        }
    }
    #
    #       S E T   O F F S E T S   I N   H O T   S T A R T
    #     W I T H   P R E V I O U S   D Y N A M I C   O F F S E T
    #
    } elsif ( defined $hstime && $previousRunProps{"forcing.offset"} eq "dynamic" ) {

        #  0-1c. apply steady offset factor at same value as end of previous run (info message)
        #         single data set in offset.dat multiplied by offset factor from previous run
        #  4c. apply steady offset at same value as end of previous run (info message)
        #         single data set in offset.dat multiplied by offset factor from previous run
        if ( $offsetFinishSec < $hstime ) {
        $offsetDataSets = 1;
        if ( $offsetFactorFinish != $offsetFactorAtPreviousRunFinish ) { 
            $runProps{"forcing.offset.modified"} = "true";
            $runProps{"forcing.offset.modified.reason"} =
                "Offset factor at end of previous run is unexpectedly different from the specified offset finish factor. Resetting to same value as end of previous run.";
            $offset_line = "# INFO: offsetControl modified: ";
            $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
            $offsetFactorFinish = $offsetFactorAtPreviousRunFinish; 
        } else {
            $offset_line = "# offset applied as specified:\n";
        }                                   
        $offset_line .= "&offsetControl offsetFileName='offset.dat', ";
        $offset_line .= "offsetMultiplier=$offsetFactorAtPreviousRunFinish /";
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtPreviousRunFinish;
        }
        #  2c. reset starting offset factor to be same value as end of previous run
        #         use two datasets in offset.dat, one with factor from hotstart, one with 
        #         final offset factor, time increment set to (F - hotstart)
        if ( $offsetStartSec < 0 && $offsetFinishSec > $hstime ) {
        $offsetDataSets = 2;
        $offsetFactorAtRunStart = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtRunStart;
        $offsetFactorAtRunFinish = $offsetFactorFinish;            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish; 
        $offset_line .= "&offsetControl offsetFileName='offset.dat' /"; 
        $offsetTimeIncrement = offsetFinishSec - $hstime;
        }
        #  3c. reset starting offset factor to value at end of previous run at hotstart time (warning message)
        #         use two datasets in offset.dat, one using offset factor from hotstart,
        #         second using final offset factor, time increment set to (F - hotstart)
        #  6c. reset starting offset factor to value at end of previous run at hotstart time (warning message)
        #         use two datasets in offset.dat, one using offset factor from hotstart,
        #         second using final offset factor, time increment set to (F - hotstart)   
        if ( $offsetStartSec < 0 && $offsetFinishSec > ($RNDAY*86400.0) ) {
        $offsetDataSets = 2;
        $offsetFactorAtRunStart = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtRunStart;            
        # interpolate to find the offset factor at time=RNDAY
        $offsetFactorAtRunFinish = ( $offsetFactorFinish - $offsetFactorStart ) * 
            ( (($RNDAY*86400.0) - $hstime) / ($offsetFinishSec - $hstime) ) + $offsetFactorStart;            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorAtRunFinish; 
        $offset_line .= "&offsetControl offsetFileName='offset.dat' /"; 
        $offsetTimeIncrement = offsetFinishSec - $hstime;
        }
        #  5c. reset starting offset factor to value at end of previous run at hotstart time (warning message)
        #         use two datasets in offset.dat, one using offset factor from hotstart,
        #         second using final offset factor, time increment set to (F - hotstart)
        if ( $offsetStartSec > 0 && $offsetStartSec < $hstime && $offsetFinishSec > ($RNDAY*86400.0) ) {
        $offsetDataSets = 2;
        $offsetFactorAtRunStart = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtRunStart;            
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorFinish; 
        $offset_line .= "&offsetControl offsetFileName='offset.dat' /"; 
        $offsetTimeIncrement = offsetFinishSec - $hstime;         
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
        $offsetDataSets = 2;
        $runProps{"forcing.offset.modified"} = "true";
        $runProps{"forcing.offset.modified.reason"} =
            "Offset factor starts after the hotstart time. It will be modified so it starts at the hotstart time.";
        $runProps{"forcing.offset.modified.offsetfactorstart.seconds"} = $hstime;
        ($offsy,$offsm,$offsd,$offsh,$offsmin,$ofsss) =
            Date::Pcalc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,$hstime,0,0,0);
        $runProps{"forcing.offset.modified.offsetstart"} 
            = sprintf("%4d%02d%02d%02d",$offsy,$offsm,$offsd,$offsh); 
        if ( $offsetFactorStart != $offsetFactorAtPreviousRunFinish ) {                
            $runProps{"forcing.offset.modified.reason"} .=
            " In addition, offset factor at end of previous run is unexpectedly different from the specified offset start factor. Resetting offset start factor to same value as at end of previous run.";
            $offsetFactorStart = $offsetFactorAtPreviousRunFinish;
            $offsetFactorAtRunStart = $offsetFactorStart;
            $runProp{"forcing.offset.modified.offsetfactorstart"} = $offsetFactorAtPreviousRunFinish;
        }
        $offset_line = "# INFO: offsetControl modified: ";
        $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
        $offset_line .= "&offsetControl offsetFileName='offset.dat' /";
        $runProps{"forcing.offset.offsetfactor.atrunstart"} = $offsetFactorAtPreviousRunFinish;
        $runProps{"forcing.offset.offsetfactor.atrunfinish"} = $offsetFactorFinish;
        $offsetTimeIncrement = $offsetFinishSec - $hstime;
        }
    }
   }
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
   # case 9: offset applied after RNDAY
   #  9a-c. do not use the offset feature (info message)
   if ($offsetStartSec > ($RNDAY*86400.0) && $offsetFinishSec > ($RNDAY*86400.0)) {
      $runProp{"forcing.offset.deactivated"} = "true"; 
      $runProp{"forcing.offset.deactivated.reason"} 
         = "Offset deactivated because the offset start is after the end of the run (RNDAY).";
      &stderrMessage("ERROR",$runProp{"forcing.offset.deactivated.reason"});
      $offset_line = "#offsetControl: " . $runProp{"forcing.offset.deactivated.reason"};
      $offsetDeactivated = 1;             
   }

   #--------------------------------------------------------------------
   #
   #         D A T A   A S S I M I L A T E D   O F F S E T 
   #
   #--------------------------------------------------------------------
   # "Assimilated" means that the Operator has specified a data 
   # assimilated offset file will be used an offset, e.g., 30cm.
   #-------------------------------------------------------------------- 
   # Data assimilated offset surface, assume it starts at the hotstart 
   # time and has the right time interval and data in it.
   if ( $runProp{"forcing.offset"} eq "assimilated" ) {
      # create the &offsetControl namelist line      
      $offset_line = "&offsetControl offsetFileName=$offsetFileName";
      $offset_line .= "offsetMultiplier=$offsetMultiplier, ";
      $offset_line .= "offsetRampStart=$ramp_start_time, ";
      $offset_line .= "offsetRampEnd=$ramp_end_time, ";
      $offset_line .= "offsetRampReferenceTime='coldstart' /";   
   }
   # &offsetControl offsetFileName='offset_test.dat', offsetSkipSnaps=0, offsetMultiplier=0.1, 
   # offsetRampStart=0.0, offsetRampEnd=43200.0, offsetRampReferenceTime='coldstart' /
}
#
#--------------------------------------------------------------------------
#           S U B   Z E R O   S T A R T I N G  O F F S E T
#
# Turns offset off in fort.15
#--------------------------------------------------------------------------
sub zeroStartingOffset() {         
    if ( $offsetFactorStart != 0.0 ) { 
        $runProps{"forcing.offset.modified"} = "true";
        $runProp{"forcing.offset.modified.offsetfactorstart"} = 0.0;
        $offsetFactorStart = 0.0;
        $runProps{"forcing.offset.modified.reason"} =
            "Offset starting factor nonzero in cold start; resetting initial offset factor to zero.";
        $offset_line = "# WARNING: offsetControl modified: ";
        $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
    } else {
        $offset_line = "# offset applied as specified:\n";
    }                                   
}
#
#--------------------------------------------------------------------------
#           S U B     D E A C T I V A T E    O F F S E T
#
# Turns offset off in fort.15
#--------------------------------------------------------------------------
sub deactivateOffset() {
    &deactivateOffset();         
    $runProp{"forcing.offset.deactivated"} = "true";
    $offsetDeactivated = 1;
    &stderrMessage("ERROR",$runProp{"forcing.offset.deactivated.reason"});
    $offset_line = "#offsetControl: " . $runProp{"forcing.offset.deactivated.reason"};

    $offset_line = "#offsetControl: " . $runProp{"forcing.offset.deactivated.reason"};
    $offsetDeactivated = 1;             
    &stderrMessage("ERROR","The offset end time is before the offset start time.");   
    $runProp{"forcing.offset.deactivated"} = "true"; 
    # maintain the previous offset at a steady value if it was already running
    $runProp{"forcing.offset.deactivated.reason"} 
        = "Offset deactivated because the offset start is after the offset end.";
    &stderrMessage("ERROR",$runProp{"forcing.offset.deactivated.reason"});
}
#
#--------------------------------------------------------------------------
#   S U B    W R I T E   C O N T R O L   A N D   O F F S E T   F I L E S
#
# Appends to fort.15, creates the offset.dat file, and appends to 
# run.properties.
#--------------------------------------------------------------------------
sub writeControlAndOffsetFiles() {
   my $offsetFile = $runProps{"forcing.offset.offsetfile"}; 

    $offset_line = "# ERROR: offsetControl Modified: ";
    $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
    $offset_line .= "&offsetControl offsetFileName='offset.dat', ";
    $offset_line .= "offsetMultiplier=$offsetFactorAtRunStart /";
    &stderrMessage("ERROR",$runProps{"forcing.offset.modified.reason"});
    $runProps{"forcing.offset.modified.reason"} =         

        $offset_line = "# ERROR: offsetControl Modified: ";
        $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
        $offset_line .= "&offsetControl offsetFileName='offset.dat', ";
        $offset_line .= "offsetMultiplier=$offsetFactorAtRunStart /";

        &stderrMessage("ERROR",$runProps{"forcing.offset.modified.reason"});
        $runProps{"forcing.offset.modified.reason"} =         

        &stderrMessage("WARNING",$runProp{"forcing.offset.modified.reason"});
        $offset_line = "# WARNING: offsetControl Modified: ";
        $offset_line .= $runProp{"forcing.offset.modified.reason"} . "\n";
        $offset_line .= "&offsetControl offsetFileName='offset.dat' /";

#
#
# append run.properties file
stderrMessage("INFO","Opening run.properties file for writing.");
unless (open(RUNPROPS,">>$stormDir/run.properties")) {
   stderrMessage("ERROR","Failed to open the $stormDir/run.properties file for writing: $!.");
   die;
}
# write offset properties
if ( $runProp{"forcing.offset"} ne "off" ) {
   printf RUNPROPS "forcing.offset.offsetfactor.runstart : $offsetRunStart\n"; 
   printf RUNPROPS "forcing.offset.offsetfactor.runfinish : $offsetRunFinish\n"; 
}
close(RUNPROPS);
stderrMessage("INFO","Wrote run.properties file $stormDir/run.properties.");
exit;

      # create static offset file
      $offsetFileName = $runProps{"path.inputdir"} . "/" . $offsetFileName; 
      unless (open(TEMPLATE,"<$offsetFileName")) {
         stderrMessage("ERROR","Failed to open the offset template file $offsetFileName for reading: $!.");
         die;
      }
      # open offset data file
      unless (open(DYNAMICOFFSET,">offset.dat")) {
         stderrMessage("ERROR","Failed to open the dynamic offset file offset.dat for writing: $!.");
         die;
      }
      stderrMessage("INFO","The offset.dat file will be written to the directory $stormDir.");
         
      }
      while(<TEMPLATE>) {
         s/%offsettimeinterval%/-99999.0/;
         # just write one dataset 
         if ( $_ =~ /##/ ) {
            print DYNAMICOFFSET $_;
            last;
         }
         
      }




}


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
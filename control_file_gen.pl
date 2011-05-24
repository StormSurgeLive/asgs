#!/usr/bin/env perl
#--------------------------------------------------------------------------
# control_file_gen.pl
#

# This script uses the template fort.15 file and the ATCF formatted fort.22
# file as input and produces a fort.15 file as output. The name of the template
# file and the fort.22 file to be used as input must be specified on the
# command line. 
#
# It optionally accepts the csdate (YYYYMMDDHH24), that is, the 
# calendar time that corresponds to t=0 in simulation time. If it is 
# not provided, the first line in the fort.22 file is used as the cold start 
# time, and this time is written to stdout.
#
# It optionally accepts the time in a hotstart file in seconds since cold
# start.
#
# If the time of a hotstart file has been supplied, the fort.15 file 
# will be set to hotstart.
#
# It optionally accepts the end time (YYYYMMDDHH24) at which the simulation
# should stop (e.g., if it has gone too far inland to continue to be 
# of interest).
#
# If the --name option is set to nowcast, the RNDAY will be calculated such 
# that the run will end at the nowcast time.
#
# The --dt option can be used to specify the time step size if it is 
# different from the default of 3.0 seconds. 
#
# The --bladj option can be used to specify the Boundary Layer Adjustment
# parameter for the Holland model (not used by the asymmetric wind vortex
# model, NWS=9.
#
# The NHSINC will be calculated such that a hotstart file is always generated
# on the last time step of the run.
#
# usage:
#   %perl control_file_gen.pl [--cst csdate] [--hst hstime]
#   [--dt timestep] [--nowcast] [--controltemplate templatefile] < storm1_fort.22 
#
#--------------------------------------------------------------------------
# Copyright(C) 2006, 2007, 2008, 2009, 2010 Jason Fleming
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
#--------------------------------------------------------------------------
#
$^W++;
use strict;
use Getopt::Long;
use Date::Pcalc; 
use Cwd;
#

my $fort61freq=0; # output frequency in SECONDS 
my $fort61append; # if defined, output files will be appended across hotstarts 
my $fort62freq=0; # output frequency in SECONDS 
my $fort62append; # if defined, output files will be appended across hotstarts 
my $fort63freq=0; # output frequency in SECONDS 
my $fort63append; # if defined, output files will be appended across hotstarts 
my $fort64freq=0; # output frequency in SECONDS 
my $fort64append; # if defined, output files will be appended across hotstarts 
my $fort7172freq=0; # output frequency in SECONDS 
my $fort7172append; # if defined, output files will be appended across hotstart 
my $fort7374freq=0; # output frequency in SECONDS 
my $fort7374append; # if defined, output files will append across hotstarts 
my ($fort61, $fort62, $fort63, $fort64, $fort7172, $fort7374);
our $sparseoutput; # if defined, then fort.63 and fort.64 will be sparse ascii
my $hsformat="binary";  # input param for hotstart format: binary or netcdf
my ($fort61netcdf, $fort62netcdf, $fort63netcdf, $fort64netcdf, $fort7172netcdf, $fort7374netcdf); # for netcdf (not ascii) output
my $hotswan = "on"; # "off" if swan has to be cold started (only on first nowcast)
#
my @TRACKS = (); # should be few enough to store all in an array for easy access
my $controltemplate;
my $swantemplate;
my $metfile;
our $csdate;
our ($ny, $nm, $nd, $nh, $nmin, $ns); # current ADCIRC time
our ($ey, $em, $ed, $eh, $emin, $es); # ADCIRC end time
my $startdatetime; # formatted for swan fort.26
my $enddatetime;   # formatted for swan fort.26
my $hstime;      # time, in seconds, of hotstart file (since coldstart)
my $hstime_days; # time, in days, of hotstart file (since coldstart)
our $endtime;    # time at which the run should end
my $dt=3.0;      # adcirc time step, in seconds
my $swandt=600.0; # swan time step, in seconds
my $bladj=0.9;
my $enstorm;    # ensemble name of the storm
my $nhcName="STORMNAME"; # storm name given by the nhc
my $tau=0; # forecast period
my $dir=getcwd();
my $nws=9;
my $advisorynum;
our $advisdir;  # the directory for this run 
my $particles;  # flag to produce fulldomain current velocity files at an 
                # increment of 30 minutes
our $NHSINC;    # time step increment at which to write hot start files
our $NHSTAR;    # writing and format of ADCIRC hotstart output file
our $RNDAY;     # total run length from cold start, in days
my $ihot;       # whether or not ADCIRC should READ a hotstart file
my $fdcv;       # line that controls full domain current velocity output
our $wtiminc;   # parameters related to met and wave timing 
our $rundesc;   # description of run, 1st line in fort.15
our $ensembleid; # run id, 2nd line in fort.15
my $waves = "off"; # set to "on" if adcirc is coupled with swan is being run
#
GetOptions("controltemplate=s" => \$controltemplate,
           "swantemplate=s" => \$swantemplate,
           "metfile=s" => \$metfile,
           "name=s" => \$enstorm, 
           "cst=s" => \$csdate,
           "endtime=s" => \$endtime,
           "dt=s" => \$dt,
           "swandt=s" => \$swandt,
           "bladj=s" => \$bladj, 
           "nws=s" => \$nws, 
           "advisorynum=s" => \$advisorynum,
           "nhcName=s" => \$nhcName,
           "hstime=s" => \$hstime,
           "advisdir=s" => \$advisdir,
           "fort61freq=s" => \$fort61freq,
           "fort62freq=s" => \$fort62freq,
           "fort63freq=s" => \$fort63freq,
           "fort64freq=s" => \$fort64freq,
           "fort7172freq=s" => \$fort7172freq,
           "fort7374freq=s" => \$fort7374freq,
           "fort61append" => \$fort61append,
           "fort62append" => \$fort62append,
           "fort63append" => \$fort63append,
           "fort64append" => \$fort64append,
           "fort7172append" => \$fort7172append,
           "fort7374append" => \$fort7374append,
           "fort61netcdf" => \$fort61netcdf,
           "fort62netcdf" => \$fort62netcdf,
           "fort63netcdf" => \$fort63netcdf,
           "fort64netcdf" => \$fort64netcdf,
           "fort7172netcdf" => \$fort7172netcdf,
           "fort7374netcdf" => \$fort7374netcdf,
           "sparse-output" => \$sparseoutput,
           "hsformat=s" => \$hsformat,
           "hotswan=s" => \$hotswan
           );

#
# determine whether SWAN has been turned on
my $waves_digit = int($nws / 100); 
if ( abs($waves_digit) == 3 ) {
   $waves = "on";   
   stderrMessage("INFO","Wave forcing is active.");
}
stderrMessage("DEBUG","nws is $nws and waves digit is $waves_digit.");
#----------------------------------------------------
#
#  A D C I R C   C O N T R O L   F I L E 
#
# open template file for fort.15
unless (open(TEMPLATE,"<$controltemplate")) {
   stderrMessage("ERROR","Failed to open the fort.15 template file $controltemplate for reading.");
   die;
}
#
# open output control file
our $stormDir = $advisdir."/".$enstorm;
unless (open(STORM,">$stormDir/fort.15")) { 
   stderrMessage("ERROR","Failed to open the output control file $stormDir/fort.15.");
   die;
}
stderrMessage("INFO","The fort.15 file will be written to the directory $stormDir."); 
#
# call subroutine that knows how to fill in the fort.15 for each particular 
# type of forcing 
if ( abs($nws) == 19 || abs($nws) == 319 ) {
   stderrMessage("DEBUG","Setting parameters appropriately for asymmetric vortex model.");
   &asymmetricParameters(); 
} 
if ( abs($nws) == 12 || abs($nws) == 312 ) {
   &owiParameters();
}
if ( $enstorm eq "hindcast" ) {
   stderrMessage("DEBUG","This is a hindcast."); 
   &hindcastParameters();
}
# we want a hotstart file if this is a nowcast or hindcast
if ( $enstorm eq "nowcast" || $enstorm eq "hindcast" ) {
   $NHSTAR = 1;
   if ( $hsformat eq "netcdf" ) {
      $NHSTAR = 3;
   }
   # write a hotstart file on the last time step of the run
   $NHSINC = int(($RNDAY*86400.0)/$dt);
} else {
   $NHSTAR = 0;
   $NHSINC = 99999;
}
# we always look for a fort.68 file, and since we only write one hotstart
# file during the run, we know we will always be left with a fort.67 file.
if ( defined $hstime ) {
   $ihot = 68;
   if ( $hsformat eq "netcdf" ) {
      $ihot+=300;
   }
} else {
   $ihot = 0;
}
# [de]activate output files with time step increment and with(out) appending.
$fort61 = &getSpecifier($fort61freq,$fort61append,$fort61netcdf) . " 0.0 365.0 " . &getIncrement($fort61freq,$dt);
$fort62 = &getSpecifier($fort62freq,$fort62append,$fort62netcdf) . " 0.0 365.0 " . &getIncrement($fort62freq,$dt);
#
my $fort63specifier = &getSpecifier($fort63freq,$fort63append,$fort63netcdf);
my $fort64specifier = &getSpecifier($fort64freq,$fort64append,$fort64netcdf);
if ( defined $sparseoutput ) {
   unless ( defined $fort63netcdf ) { 
      $fort63specifier *= 4;
   }
   unless ( defined $fort64netcdf ) { 
      $fort64specifier *= 4;
   }
}
$fort63 = $fort63specifier . " 0.0 365.0 " . &getIncrement($fort63freq,$dt);
$fort64 = $fort64specifier . " 0.0 365.0 " . &getIncrement($fort64freq,$dt);
$fort7172 = &getSpecifier($fort7172freq,$fort7172append,$fort7172netcdf) . " 0.0 365.0 " . &getIncrement($fort7172freq,$dt);
$fort7374 = &getSpecifier($fort7374freq,$fort7374append,$fort7374netcdf) . " 0.0 365.0 " . &getIncrement($fort7374freq,$dt);
if ( $enstorm eq "hindcast" ) {
   $fort7172 = "ERROR: This line should not be here! In a hindcast, the ASGS assumes that there is no met forcing. As a result, the fort.15 template file should not have output specifiers for meteorological output. Please remove the NOUTM etc line and the met stations from the template file '$controltemplate'.";
    $fort7374 = "ERROR: This line should not be here! In a hindcast, the ASGS assumes that there is no met forcing. As a result, the fort.15 template file should not have output specifiers for meteorological output. Please remove the NOUTGW etc line from the template file '$controltemplate'.";
}
# add swan time step to WTIMINC line if waves have been activated
if ( $waves eq "on" ) {
   $wtiminc.=" $swandt"
}
#
stderrMessage("INFO","Filling in ADCIRC control template (fort.15).");
while(<TEMPLATE>) {
    # if we are looking at the first line, fill in the name of the storm
    # and the advisory number, if available
    s/%StormName%/$rundesc/;
    # if we are looking at the DT line, fill in the time step (seconds)
    s/%DT%/$dt/;
    # if we are looking at the RNDAY line, fill in the total run time (days)
    s/%RNDAY%/$RNDAY/;  
    # set whether or not we are going to read a hotstart file
    s/%IHOT%/$ihot/;
    # fill in the parameter that selects which wind model to use
    s/%NWS%/$nws/;
    # fill in the timestep increment that hotstart files will be written at
    s/%NHSINC%/$NHSINC/;
    # fill in whether or not we want a hotstart file out of this
    s/%NHSTAR%/$NHSTAR/;
    # fill in ensemble name -- this is in the comment line
    s/%EnsembleID%/$ensembleid/;
    # may be asymmetric parameters, or wtiminc, rstiminc, etc
    s/%WTIMINC%/$wtiminc/;
    # output options
    s/%FORT61%/$fort61/;
    s/%FORT62%/$fort62/;
    s/%FORT63%/$fort63/;
    s/%FORT64%/$fort64/;
    s/%FORT7172%/$fort7172/;
    s/%FORT7374%/$fort7374/;
    print STORM $_;
}

close(TEMPLATE);
close(STORM);
#
#
#  S W A N   C O N T R O L   F I L E  
#
unless ( $waves eq "on" ) {
   exit;
}
# open template file for fort.26
unless (open(TEMPLATE,"<$swantemplate")) {
   stderrMessage("ERROR","Failed to open the swan template file $swantemplate for reading.");
   die;
}
#
# open output fort.26 file
unless (open(STORM,">$stormDir/fort.26")) { 
   stderrMessage("ERROR","Failed to open the output control file $stormDir/fort.26.");
   die;
}
stderrMessage("INFO","The fort.26 file will be written to the directory $stormDir."); 
#
$startdatetime = sprintf("%4d%02d%02d.%02d0000",$ny,$nm,$nd,$nh);
$enddatetime = sprintf("%4d%02d%02d.%02d0000",$ey,$em,$ed,$eh);
my $swanhs =  "INIT HOTSTART MULTIPLE 'swan.68'";
if ( $hotswan eq "off" ) {
   $swanhs = "\$ swan will coldstart";
}
#
stderrMessage("INFO","Filling in swan control template (fort.26).");
while(<TEMPLATE>) {
    # if we are looking at the first line, fill in the name of the storm
    # and the advisory number, if available
    s/%StormName%/$rundesc/;
    # if we are looking at the DT line, fill in the time step (seconds)
    s/%swandt%/$swandt/;
    # fill in ensemble name -- this is in the comment line
    s/%EnsembleID%/$ensembleid/;
    # may be asymmetric parameters, or wtiminc, rstiminc, etc
    s/%WTIMINC%/$wtiminc/;
    #
    s/%hotstart%/$swanhs/;
    # swan start time -- corresponds to adcirc hot start time
    s/%startdatetime%/$startdatetime/;
    # swan end time%
    s/%enddatetime%/$enddatetime/;
    print STORM $_;
}
close(TEMPLATE);
close(STORM);
exit;
#
#
#--------------------------------------------------------------------------
#   S U B   G E T   S P E C I F I E R
#
# Determines the correct output specifier for output files based on
# the output frequency, whether or not the files should be appended,
# and whether or not the netcdf format is used (ascii is the default).
#--------------------------------------------------------------------------
sub getSpecifier () {
   my $freq = shift;
   my $append = shift;
   my $netcdf = shift;
   my $specifier;

   if ( $freq == 0 ) {
      $specifier = "0";
   } else {
      if ( defined $append ) { 
         $specifier = "1";
      } else {
         $specifier = "-1";
      }
      if ( defined $netcdf ) {
         $specifier *= 3;
      }
   }
   return $specifier;
}
#
#--------------------------------------------------------------------------
#   S U B   G E T   I N C R E M E N T 
#
# Determines the correct time step increment based on the output frequency 
# and time step size.
#--------------------------------------------------------------------------
sub getIncrement () {
   my $freq = shift;
   my $timestepsize = shift;
   my $increment;
   if ( $freq == 0 ) {
      $increment = "99999";
   } else {
      $increment = int($freq/$timestepsize);
   }
   return $increment;
}
#
#--------------------------------------------------------------------------
#   S U B    H I N D C A S T  P A R A M E T E R S
#
# Determines parameter values for the control file when running
# ADCIRC during a hindcast with no met forcing.  
#--------------------------------------------------------------------------
sub hindcastParameters () {
    $rundesc = "cs:$csdate"."0000 cy: ASGS hindcast";
    $RNDAY = $endtime;  
    $nws = 0;
    $ensembleid = "$endtime day hindcast run";
    $wtiminc = "ERROR: This line should not be here! In a hindcast, the ASGS assumes that there is no met forcing. As a result, the fort.15 template file should not have a WTIMINC line. Please remove the WTIMINC line from the template file '$controltemplate'.";
    stderrMessage("DEBUG","Finished setting hindcast parameters.");
} 
#
#--------------------------------------------------------------------------
#   S U B   O W I  P A R A M E T E R S
#
# Determines parameter values for the control file when running
# ADCIRC with OWI formatted meteorological data (NWS12).  
#--------------------------------------------------------------------------
sub owiParameters () {
   #
   # open met file 
   open(METFILE,"<$stormDir/fort.22") || die "ERROR: control_file_gen.pl: Failed to open OWI (NWS12) fort.22 file for reading.";
   my $line = <METFILE>;
   close(METFILE);
   $line =~ /^# (\d+)/;
   $wtiminc = $1;
   #
   # determine the relationship between the start of the NAM data and the
   # current time in the ADCIRC run
   $csdate =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   my $cy = $1;
   my $cm = $2;
   my $cd = $3;
   my $ch = $4;
   if ( defined $hstime && $hstime != 0 ) {
      # now add the hotstart seconds
      ($ny,$nm,$nd,$nh,$nmin,$ns) =
         Date::Pcalc::Add_Delta_DHMS($cy,$cm,$cd,$ch,0,0,0,0,0,$hstime);
   } else {
      # the hotstart time was not provided, or it was provided and is equal to 0
      # therefore the current ADCIRC time is the cold start time, t=0
      $ny = $cy;
      $nm = $cm;
      $nd = $cd;
      $nh = $ch;
      $nmin = 0;
      $ns = 0;
   }
   #
   # open file that will contain the hotstartdate
   open(HSD,">$stormDir/hotstartdate") || die "ERROR: control_file_gen.pl: Failed to open the HOTSTARTDATE file $stormDir/hotstartdate.";
   my $hotstartdate = sprintf("%4d%02d%02d%02d",$ny,$nm,$nd,$nh);
   stderrMessage("INFO","The file containing the hotstartdate '$hotstartdate' will be written to the directory $stormDir."); 
   printf HSD $hotstartdate;
   close(HSD); 
   # determine the date time of the start of the OWI files
   my @fort221 = glob($stormDir."/NAM*.221");
   $fort221[0] =~ /NAM_(\d+)/;
   my $owistart = $1;
   # create run description
   $rundesc = "cs:$csdate"."0000 cy:$owistart ASGS NAM";
   $owistart =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   my $oy = $1;
   my $om = $2;
   my $od = $3;
   my $oh = $4;
   my $omin = 0;
   my $os = 0;
   #
   # get difference
   (my $ddays, my $dhrs, my $dsec)
           = Date::Pcalc::Delta_DHMS(
                $ny,$nm,$nd,$nh,0,0,
                $oy,$om,$od,$oh,0,0);
   # find the difference in seconds
   my $blank_time = $ddays*86400.0 + $dhrs*3600.0 + $dsec;
   stderrMessage("INFO","Blank time is '$blank_time'.");
   # calculate the number of blank snaps (or the number of 
   # snaps to be skipped in the OWI file if it starts before the 
   # current time in the ADCIRC run)
   my $nwbs = int($blank_time/$wtiminc);
   stderrMessage("INFO","nwbs is '$nwbs'");
   #
   # create the fort.22 output file, which is the wind input file for ADCIRC
   open(MEMBER,">$stormDir/fort.22") || die "ERROR: control_file_gen.pl: Failed to open file for ensemble member '$enstorm' to write fort.22 file: $!.";
   printf MEMBER "1\n";     # nwset
   printf MEMBER "$nwbs\n"; # nwbs
   printf MEMBER "1.0\n";   # dwm
   close(MEMBER);
   #
   # determine the date time of the end of the OWI files
   $fort221[0] =~ /(\d+).221$/;
   my $owiend = $1;
   stderrMessage("INFO","The OWI file ends at '$owiend'.");
   $owiend =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $ey = $1;
   $em = $2;
   $ed = $3;
   $eh = $4;
   $emin = 0;
   $es = 0;
   #
   # get difference
   (my $ddays, my $dhrs, my $dsec)
           = Date::Pcalc::Delta_DHMS(
                $cy,$cm,$cd,$ch,0,0,
                $ey,$em,$ed,$eh,0,0);
   # find the new total run length in days
   $RNDAY = $ddays + $dhrs/24.0 + $dsec/86400.0;
   # determine the number of hours of this run, from hotstart to end
   (my $ddays, my $dhrs, my $dsec)
           = Date::Pcalc::Delta_DHMS(
                $ny,$nm,$nd,$nh,0,0,
                $ey,$em,$ed,$eh,0,0);
   my $addHours = $ddays*24.0 + $dhrs + $dsec;
   $ensembleid = $addHours . " hour " . $enstorm . " run";
}
#
#--------------------------------------------------------------------------
#   S U B   A S Y M M E T R I C  P A R A M E T E R S
#
# Determines parameter values for the control file when running
# the asymmetric wind model (NWS19).  
#--------------------------------------------------------------------------
sub asymmetricParameters () {
   $ensembleid = $enstorm;
   #
   # open met file containing datetime data
   unless (open(METFILE,"<$metfile")) { 
      stderrMessage("ERROR","Failed to open meteorological (ATCF-formatted) fort.22 file '$metfile' for reading.");
      die;
   }
   stderrMessage("DEBUG","Successfully opened meteorological (ATCF-formatted) fort.22 file '$metfile' for reading.");
   #
   # determine date time at end of hindcast
   #
   # Build track list
   while (<METFILE>) {
      chomp($_);
      my @tmp = ();
      # split and remove any spaces
      foreach my $item (split(',',$_)) {
         $item =~ s/\s*//g;
         push(@tmp,$item);
      }
      # 2d array of arrays; [@tmp] creates an anon array in each 
      # element of @TRACK
      push(@TRACKS,[@tmp]); 
   }
   #
   # find last hindcast line
   my $track;
   my $nowcast;
   foreach $track (reverse(@TRACKS)) {
      if (@{$track}[4] =~ m/BEST/) {
         if ( $nhcName eq "STORMNAME" ) {
            # We need to get the storm name from the last hindcast line
            if ( defined $track->[27] ) {
               $nhcName = $track->[27];
            } else {	 
               stderrMessage("WARNING","The name of the storm does not appear in the hindcast.");
            }
         }
         # also grab the last hindcast time; this will be the nowcast time
         $nowcast = $track->[2];     
         last;
      }
   }
   #
   # get coldstart time
   my $cstart;
   unless ( $csdate ) {
      $cstart = $nowcast; # adjusted later to make the difference nonzero
      print $cstart; # write cold start time to stdout for use in later runs
   } else {
      $cstart = $csdate;
   }
   # convert hotstart time (in days since coldstart) if necessary
   if ( $hstime ) {
      $hstime_days = $hstime/86400.0;
   }
   # get end time
   my $end;
   # for a nowcast, end the run at the end of the hindcast
   if ( $enstorm eq "nowcast" ) { 
      $end = $nowcast;
      stderrMessage("INFO","New $enstorm time is $end.");
   } elsif ( $endtime ) {
      # if this is not a nowcast, and the end time has been specified, end then
      $end = $endtime
   } else {
      # this is not a nowcast; end time was not explicitly specified, 
      # get end time based on either 
      # 1. running out of fort.22 file or 
      # 2. two or more days inland
      my $ty;  # level of tropical cyclone development
      my $now_inland; # boolean, 1 if TY is "IN"
      my $tin; # time since the first occurrence of TY as "IN"
      my $tin_year;
      my $tin_mon;
      my $tin_day;
      my $tin_hour;
      my $tin_min;
      my $tin_sec;
      my $tin_tau; # forecast period at first occurrence of TY as "IN"
      my $c_year;  # time of line currently being processed
      my $c_mon;
      my $c_day;
      my $c_hour;
      my $c_min;
      my $c_sec;
      my $ddays;
      my $dhrs;
      my $dsec; # difference btw time inland and time on current line
      foreach $track (@TRACKS) {
#        my $lat = substr(@{$track}[6],0,3); # doesn't work if only 2 digits
        #@{$track}[6] =~ /[0-9]*/;
        $_ = @{$track}[6];
        /([0-9]*)/;
        $end = $track->[2];
        $tau = $track->[5];
        $ty = @{$track}[10];
        if ( $ty eq "IN" and (not $now_inland) ) {
           $now_inland = 1;
           $tin = @{$track}[2]; # time at first occurrence of "IN" (inland) 
           $tin =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
           $tin_year = $1;
           $tin_mon = $2;
           $tin_day = $3;
           $tin_hour = $4;
           $tin_min = 0;
           $tin_sec = 0;
           $tin_tau = @{$track}[5]
        }
        if ( $now_inland ) { 
           $end =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
           $c_year = $1;
           $c_mon = $2;
           $c_day = $3;
           $c_hour = $4;
           $c_min = 0;
           $c_sec = 0;
           #
           # get difference between first occurrence of IN (inland)
           # and the time on the current track line
           ($ddays,$dhrs,$dsec) 
              = Date::Pcalc::Delta_DHMS(
                $tin_year,$tin_mon,$tin_day,$tin_hour,$tin_min,$tin_sec,
                $c_year,$c_mon,$c_day,$c_hour,$c_min,$c_sec);
           my $time_inland = $ddays + $dhrs/24 + $dsec/86400 + ($tau-$tin_tau)/24;
           if ( $time_inland >= 2.0 ) {
              last; # jump out of loop with current track as last track
           }
        }
      }
   }
   stderrMessage("INFO","The fort.15 file will be configured to end on $end.");
   #
   $cstart=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   my $cs_year = $1;
   my $cs_mon = $2;
   my $cs_day = $3;
   my $cs_hour = $4;
   my $cs_min = 0.0;
   my $cs_sec = 0.0;

   my $cy = $1;
   my $cm = $2;
   my $cd = $3;
   my $ch = $4;
   if ( defined $hstime && $hstime != 0 ) {
      # now add the hotstart seconds
      ($ny,$nm,$nd,$nh,$nmin,$ns) =
         Date::Pcalc::Add_Delta_DHMS($cs_year,$cs_mon,$cs_day,$cs_hour,$cs_min,$cs_sec,0,0,0,$hstime);
   } else {
      # the hotstart time was not provided, or it was provided and is equal to 0
      # therefore the current ADCIRC time is the cold start time, t=0
      $ny = $cy;
      $nm = $cm;
      $nd = $cd;
      $nh = $ch;
      $nmin = 0;
      $ns = 0;
   }
   #
   $end =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $ey = $1;
   $em = $2;
   $ed = $3;
   $eh = $4;
   $emin = 0.0;
   $es = 0.0;
   #
   # get difference btw cold start time and end time
   my ($days,$hours,$seconds) 
      = Date::Pcalc::Delta_DHMS(
         $cs_year,$cs_mon,$cs_day,$cs_hour,$cs_min,$cs_sec,
         $ey,$em,$ed,$eh,$emin,$es);
   # RNDAY is diff btw cold start time and end time
   # For a forecast, RNDAY is one time step short of the total time to ensure 
   # that we won't run out of storm data at the end of the fort.22
   # For a nowcast, RNDAY will be one time step long, so that we end at
   # the nowcast time, even if ADCIRC rounds down the number of timesteps
   my $stopshort = 0.0;
   if ( $enstorm eq "nowcast" ) {
      $stopshort = -2*$dt;
   } else {
      $stopshort = $dt;
   }
   $RNDAY = $days + $hours/24.0 + ($seconds-$stopshort)/86400.0; 
   #stderrMessage("DEBUG","RNDAY is initially calculated as $RNDAY.");
   #
   # If RNDAY is less than two timesteps, make sure it is at least two timesteps. 
   # This can happen if we start up from a fort.22 that has only one BEST line,
   # i.e., it starts at the nowcast. RNDAY would be zero in this case, except 
   # our algorithm actually stops one ts short of the full time, so RNDAY is
   # actually negative in this case. ADCIRC needs at least two timesteps from 
   # coldstart to create a valid hotstart file.
   my $runlength_seconds = $RNDAY*86400.0;
   if ( $hstime ) {
      $runlength_seconds-=$hstime;
   }
   my $min_runlength = 2*$dt; 
   # if we coldstart at the nowcast, we may not have calculated a runlength 
   # longer than the minimum
   if ( $runlength_seconds < $min_runlength ) { 
      stderrMessage("INFO","Runlength was calculated as $runlength_seconds seconds, which is less than the minimum runlength of $min_runlength seconds. The RNDAY will be adjusted so that it ADCIRC runs for the minimum length of simulation time.");
      # recalculate the RNDAY as the hotstart time plus the minimal runlength
      if ( $hstime ) {
         $RNDAY=$hstime_days + ($min_runlength/86400.0);
      } else {
         $RNDAY=$min_runlength/86400.0;
      }
      $runlength_seconds = $min_runlength;
   }
   #
   # if this is an update from hindcast to nowcast, calculate the hotstart 
   # increment so that we only write a single hotstart file at the end of 
   # the run. If this is a forecast, don't write a hotstart file at all.
   $NHSINC = int(($RNDAY*86400.0)/$dt);
   $NHSTAR;
   # create run description
   $rundesc = "cs:$csdate"."0000 cy:$nhcName$advisorynum ASGS";
   # create the WTIMINC line
   $wtiminc = $cs_year." ".$cs_mon." ".$cs_day." ".$cs_hour." 1 ".$bladj;
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
   printf STDERR "$theTime $level: control_file_gen.pl: $message\n";
   if ($level eq "ERROR") {
      sleep 60
   }
}



#!/usr/bin/env perl
#---------------------------------------------------------------------
# storm_track_gen.pl
#
# This script accepts raw ATCF files from the NHC and produces various
# fort.22 files for ADCIRC vortex models. It accepts a forecast file
# format and optionally a BEST track file. It also requires the cold start
# time. By default, it produces a fort.22 file for one of the ADCIRC
# vortex wind models that represents the NHC consensus forecast.
#---------------------------------------------------------------------
#
# ASSUMPTIONS:
#
# 1. The ADCIRC coldstart time (or the sum of the coldstart time and the number
# of seconds from the hotstart file in the case of a hotstart) must correspond
# exactly to one of the times in the BEST or OFCL file.
#
# 2. Conversion of km to degrees (lat and lon) is only approximate and
# could be made more accurate.
#
# 3. The fill-in of the forecast central pressure is based on an algorithm
# under development by Jason Fleming (jason.fleming@seahorsecoastal.com). This
# algorithm is the subject of current research and is subject to change.
#
# 4. The code was designed for a real time context, so it assumes that
# it wouldn't make sense to apply a perturbation to a nowcast,
# only to a forecast. So therefore perturbations are applied to
# OFCL but not BEST data.
#
# 5. Also the forecast is being hotstarted from the end of a previous nowcast,
# so the forecast track file needs to pick up where the previous nowcast
# track file left off. The ADCIRC vortex met models need to start with the
# track data that correspond to the hotstart time, so at least the first
# line in the forecast track file has to be the same as the last line in
# the previous nowcast track file, i.e., it has to come from BEST track data.
# The requirements for both BEST and OFCL data could be relaxed in
# storm_track_gen.pl to make it more of a generalized scenario-generating
# tool outside the context of real time guidance.
#
#---------------------------------------------------------------------
#
# Copyright(C) 2006--2026 Jason Fleming
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
#---------------------------------------------------------------------
#
#
use strict;
use Getopt::Long;
use Date::Calc;
use Cwd;
use ASGSUtil;
$^W++;

# usage: perl storm_track_gen.pl --dir /path/to/atcf/files --storm 09 --year 2009 --startdate 2009081400 --name nhcConsensus
# the output will be stored in a file called fort.22

my $dir;                           # path to raw ATCF files
my $storm;                         # number, e.g., 05 or 12
my $year;                          # YYYY
my $coldstartdate;                 # YYYYMMDDHH24
my $hotstartseconds = 0.0;         # default is not hotstart
my $nws = 20;                      # the ADCIRC wind model to target
our $name = "nhcConsensus";        # default track to generate
my $percent = "null";              # magnitude of parameter variation
my $strengthPercent = "null";
my $overlandSpeedPercent = "null";
my $sizePercent = 20.0;
my $veerPercent = "null";
my $pi=3.141592653589793;
my $method="twoslope";              # algorithm for predicting central pressure
# if the NHC issues a special advisory, there may be incomplete lines in the
# BEST track file. This hash will save the most recent complete lines, to fill
# in any missing data.
my %complete_hc_lines = ();
my $nhcName;  # NHC's current storm name (IKE, KATRINA, INVEST, ONE, etc)
my $stormClass = " "; # NHC's current storm classification (TD, TS, HU, IN, etc)
#
# jgf20160105: Enable direct specification of scenario variations on the
# command line, rather than requiring the Operator to name the scenario
# to indicate the type of perturbation applied. This also enables
# simultaneous use of all three pertubations in a single scenario.
my $test;    # true if this is being executed as a unit test
GetOptions(
           "dir=s" => \$dir,
           "storm=s" => \$storm,
           "year=s" => \$year,
           "coldstartdate=s" => \$coldstartdate,
           "hotstartseconds=s" => \$hotstartseconds,
           "nws=s" => \$nws,
           "name=s" => \$name,
           "method=s" => \$method,
           "strengthPercent=s" => \$strengthPercent,
           "overlandSpeedPercent=s" => \$overlandSpeedPercent,
           "veerPercent=s" => \$veerPercent,
           "percent=s" => \$percent,
           "test" => \$test
           );
#
# create a dictionary of properties from run.properties
my %runProp;
my $haveRunProp = 0; # true if it is present (most common case)
if ( -e "run.properties" ) {
   $haveRunProp = 1;
   # open properties file
   unless (open(RUNPROP,"<run.properties")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open run.properties: $!.",$test);
      die;
   }
   while (<RUNPROP>) {
      my @fields = split ':',$_, 2 ;
      # strip leading and trailing spaces and tabs
      $fields[0] =~ s/^\s|\s+$//g ;
      $fields[1] =~ s/^\s|\s+$//g ;
      $runProp{$fields[0]} = $fields[1];
   }
   close(RUNPROP);
}
#
# check to see that all the mandatory command line arguments were specified
unless ( $dir ) {
   $dir = cwd();
   ASGSUtil::stderrMessage("WARNING","The path to the raw ATCF input files was not specified with the --dir argument. It will be assumed that the files are in the directory $dir.",$test);
}
# the storm number is needed to form the file names of the BEST and OFCL track files
unless ( $storm ) {
   ASGSUtil::stderrMessage("ERROR","The storm number was not specified using the --storm argument.",$test);
   die;
}
# the year is needed to form the file names of the BEST and OFCL track files
unless ( $year ) {
   ASGSUtil::stderrMessage("ERROR","The year was not specified using the --year argument.",$test);
}
my $setColdStartDate = 0; # true if we need to set it ourselves
unless ( $coldstartdate ) {
   $setColdStartDate = 1;
}
# if the cold start date was not provided on the command line, we use the
# oldest data in the BEST track file
my $firstBESTDate = "notfound";
my $bestATCF = "$dir/bal$storm$year.dat";
if ( -e $bestATCF ) {
   unless ( open(BEST,"<$bestATCF") ) {
      ASGSUtil::stderrMessage("ERROR","Failed to open BEST track ATCF file $bestATCF for scenario '$name': $!.",$test);
      die;
   }
   # find the date/time that the BEST track file starts
   while(<BEST>) {
      my @fields = split(',',$_);
      $firstBESTDate = $fields[2];
      $firstBESTDate =~ s/\s*//g; # remove spaces
      last;
   }
   close(BEST);
} else {
   ASGSUtil::stderrMessage("INFO","The BEST track ATCF file '$bestATCF' for scenario '$name' was not found and will not be processed.",$test);

}
# find the date/time that the forecast is valid
# open ATCF OFCL file, if it is present
my $firstOFCLDate = "notfound";
my $forecastATCF = "$dir/al$storm$year.fst";
if ( -e $forecastATCF ) {
   unless (open(OFCL, "<", $forecastATCF)) {
      ASGSUtil::stderrMessage("ERROR","Failed to open forecast ATCF file '$forecastATCF' for scenario '$name': $!.",$test);
      die;
   }
   while(<OFCL>) {
      my @fields = split(',',$_);
      my $line = $_;
      # grab the datetime at which the forecast is valid
      $firstOFCLDate = $fields[2];
      last;
   }
   close(OFCL);
} else {
   ASGSUtil::stderrMessage("INFO","The forecast ATCF file '$forecastATCF' for scenario '$name' was not found and will not be processed.",$test);
}
#
if ( $firstBESTDate eq "notfound" && $firstOFCLDate eq "notfound" ) {
   ASGSUtil::stderrMessage("ERROR","Neither a BEST or OFCL file is available. There is no data to generate a storm track fort.22 file from.",$test);
   die;
}
#
if ( $setColdStartDate == 1 ) {
   # pick whichever date was actually found
   if ( $firstBESTDate ne "notfound" ) {
      $coldstartdate = $firstBESTDate;  # this could change later if there are incomplete lines in the BEST track file
   } else {
      $coldstartdate = $firstOFCLDate;
   }
   # if both were found, pick the earliest
   if ( $firstBESTDate ne "notfound" && $firstOFCLDate ne "notfound" ) {
      if ( $firstOFCLDate < $firstBESTDate ) {
         $coldstartdate = $firstOFCLDate;
      }
   }
   ASGSUtil::stderrMessage("INFO","The cold start date was not specified using the --coldstartdate argument.",$test);
   ASGSUtil::stderrMessage("INFO","The starting date/time of the BEST track data is '$firstBESTDate'.",$test);
   ASGSUtil::stderrMessage("INFO","The starting date/time of the OFCL track data is '$firstOFCLDate'.",$test);
}
#
# Check to make sure that the scenario name does not match more than
# one perturbation.
my $match = 0;
if ( $name =~ /maxWindSpeed/ ) {
   $match++;
}
if ( $name =~ /overlandSpeed/ ) {
   $match++;
}
if ( $name =~ /veer/ ) {
   $match++;
}
if ( $match > 1 ) {
   ASGSUtil::stderrMessage("ERROR","The scenario name '$name' contains more than one match to perturbed scenario names (maxWindSpeed, overlandSpeed, and veer).",$test);
   die;
}
#
# jgf20160105: If the scenario name matches the name of a
# perturbation, but the percent was not specified, this is an error.
if ( $percent eq "null" && $match == 1 ) {
   ASGSUtil::stderrMessage("ERROR","The scenario name '$name' contains a match to a perturbed member name (either maxWindSpeed, overlandSpeed, or veer) but the percent variation was not specified on the command line.",$test);
   die;
}
#
# jgf20160105: Check to make sure that the "scenario name" method
# for specifying perturbations and the "direct specification" method are
# not being simultaneously used, or if they are, they don't conflict.
if ( $match == 1 ) {
   if ( ($name =~ /maxWindSpeed/ && $strengthPercent ne "null") ||
        ($name =~ /overlandSpeed/ && $overlandSpeedPercent ne "null") ||
        ($name =~ /veer/ && $veerPercent ne "null") ) {
      ASGSUtil::stderrMessage("ERROR","The scenario name '$name' contains a match to a perturbed member name (either maxWindSpeed, overlandSpeed, or veer) but the percent variation for this perturbation was also directly specified on the command line.",$test);
      die;
   }
}
#
# jgf20160105: Set percentages and write properties for the case where
# values are set via scenario name as well as direct specification.
if ( $name =~ /maxWindSpeed/ || $strengthPercent ne "null" ) {
   if ($name =~ /maxWindSpeed/) {
      $strengthPercent = $percent;
   }
   $runProp{'variation maxWindSpeed'} = $strengthPercent;
   ASGSUtil::stderrMessage("INFO","The forecast maximum wind speed will be modified by $strengthPercent percent.",$test);
}
if ($name =~ /overlandSpeed/ || $overlandSpeedPercent ne "null" ) {
   if ( $name =~ /overlandSpeed/ ) {
      $overlandSpeedPercent = $percent;
   }
   $runProp{'variation overlandSpeed'} = $overlandSpeedPercent;
   ASGSUtil::stderrMessage("INFO","The forecast overland speed will be modified by $overlandSpeedPercent percent.",$test);
}
my $sign = "";
if ( $name =~ /veer/ || $veerPercent ne "null" ) {
   if ( $name =~ /veer/ ) {
      $veerPercent = $percent;
   }
   if ($veerPercent > 0.0) {
      $sign = "+";
   }
   $runProp{'variation veer'} = "$sign$veerPercent";
   ASGSUtil::stderrMessage("INFO","The track positions will be modified by $sign$veerPercent percent relative to the cone of uncertainty.",$test);
}
if ( $name =~ /rMax/ ) {
   if ($nws == 8 ) {
      # the rmax variation is for symm model uses persistence
      ASGSUtil::stderrMessage("INFO","The rMax variation is handled by persisting the last radius to maximum winds value from the BEST track file into the forecast, modified by the percent value.",$test);
   } else {
      # the rmax variation is controlled by the aswip program for asym models
      ASGSUtil::stderrMessage("INFO","The rMax variation is handled by the aswip program for the asymmetric models, and is therefore ignored by storm_track_gen.pl.",$test);
   }
}
if ( $match == 0 && $percent ne "null" ) {
   ASGSUtil::stderrMessage("INFO","The option '--percent' was specified at '$percent', but the scenario '$name' does not contain a match for any perturbations (either maxWindSpeed, overlandSpeed, or veer). The percent value will be ignored.",$test);
}
#
# preprocess and rearrange ATCF files if necessary
my @rad;                  # wind radii in the 4 quadrants (current time)
my @oldrad;               # wind radii in the 4 quadrants (previous time)
my @bestTimes;            # list of usable BEST track dates/times
my $firstBestTime = "";
my $lastBestTime;
my $lastBestPressure;
my $lastBestWindspeed;
my $lastBestRmax;
my $old_lat;
my $old_lon;
my $hours = 0;
my $fhcyear; my $fhcmon; my $fhcday; my $fhchour; # first relevant BEST line
my $tsflag="0";  # set to 1 when the storm reaches tropical storm force
my $hotstartDate = getHotstartDate($coldstartdate, $hotstartseconds);
my $hsFound = 0; # set to 1 if/when we find the hotstart date in the file
#
# create the fort.22 output file, which is the wind input file for ADCIRC
unless (open(FORT22, ">", "./fort.22")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open file for scenario '$name' fort.22 output file: $!.",$test);
   die;
}
#---------------------------------------------------------------------
# P R O C E S S I N G   H I N D C A S T   F I L E
#---------------------------------------------------------------------
#
# open ATCF BEST file
my $bestLineNum = 0;
my $bestATCF = "$dir/bal$storm$year.dat";
unless (open(BEST, "<", $bestATCF)) {
   ASGSUtil::stderrMessage("INFO","Failed to open BEST track ATCF file '$bestATCF' for scenario '$name': $!.",$test);
} else {
   while(<BEST>) {
      $bestLineNum++;
      my $hyear; my $hmon; my $hday; my $hhour;   # date/time of current BEST line
      my @fields = split / *, */, $_;
      # ignore BEST track lines that are after the time that the forecast
      # is valid (only if a forecast file is being processed)
      if ( $firstOFCLDate ne "notfound" ) {
         if ( $fields[2] >= $firstOFCLDate ) {
            last;
         }
      }
      my $line = $_;
      # examples of incomplete lines at the top of a BEST track file (IRMA al112017)
      #                                                                                                     1         1         1         1         1         1         1         1         1         1         2         2         2         2         2
      #           1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0         1         2         3         4
      # 0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
      # AL, 11, 2017082718,   , BEST,   0, 115N,  160W,  25,    0, DB,   0,    ,    0,    0,    0,    0, 1013,    0,   0,   0,   0,    ,   0,    ,   0,   0, GENESIS023,  ,  0,    ,    0,    0,    0,    0, genesis-num, 023,
      # AL, 11, 2017082800,   , BEST,   6, 115N,  178W,  25,    0, DB,   0,    ,    0,    0,    0,    0, 1013,    0,   0,   0,   0,    ,   0,    ,   0,   0, GENESIS023,  ,  0,    ,    0,    0,    0,    0, genesis-num, 023,
      # AL, 11, 2017082806,   , BEST,  12, 116N,  193W,  25, 1009, DB,   0,    ,    0,    0,    0,    0, 1013,  150,  40,   0,   0,   L,   0,    ,   0,   0,     INVEST, S,  0,    ,    0,    0,    0,    0, genesis-num, 023, SPAWNINVEST, al712017 to al932017,
      #  0,  1,          2,  3,    4,   5,    6,     7,   8,    9, 10,  11,  12,   13,   14,   15,   16,   17,   18,  19,  20,  21,  22,  23,  24,  25,  26,         27,
      # examples of incomplete lines in a BEST track file due to special advisories issued at weird times (KATRINA al122005)
      # AL, 12, 2005082522, 30, BEST,   0, 260N,  801W,  70,  984, HU,   0,    ,    0,    0,    0,    0,
      # AL, 12, 2005082911, 10, BEST,   0, 293N,  896W, 110,  920, HU,   0,    ,    0,    0,    0,    0,
      # examples of incomplete lines in a BEST track file due to special advisories issued at weird times (MELISSA al132025)
      # AL, 13, 2025102817, 25, BEST,   0, 181N,  780W, 160,  897, HU,  34, NEQ,  170,  130,   60,   80, 1006,  270,   5,   0,   0,   L,   0,    ,   0,   0,
      # AL, 13, 2025102817, 25, BEST,   0, 181N,  780W, 160,  897, HU,  50, NEQ,   50,   45,   30,   45, 1006,  270,   5,   0,   0,   L,   0,    ,   0,   0,
      # AL, 13, 2025102817, 25, BEST,   0, 181N,  780W, 160,  897, HU,  64, NEQ,   25,   25,   20,   25, 1006,  270,   5,   0,   0,   L,   0,    ,   0,   0,
      # AL, 13, 2025102907, 20, BEST,   0, 200N,  762W, 100,  954, HU,  34, NEQ,  160,  140,   80,  100, 1007,  270,  20,   0,   0,   L,   0,    ,   0,   0,    MELISSA,  ,  0,    ,    0,    0,    0,    0, genesis-num, 035,
      # AL, 13, 2025102907, 20, BEST,   0, 200N,  762W, 100,  954, HU,  50, NEQ,   60,   70,   35,   45, 1007,  270,  20,   0,   0,   L,   0,    ,   0,   0,    MELISSA,  ,  0,    ,    0,    0,    0,    0, genesis-num, 035,
      # AL, 13, 2025102907, 20, BEST,   0, 200N,  762W, 100,  954, HU,  64, NEQ,   25,   25,   20,   25, 1007,  270,  20,   0,   0,   L,   0,    ,   0,   0,    MELISSA,  ,  0,    ,    0,    0,    0,    0, genesis-num, 035,
      # examples of incomplete lines at the end of a BEST track file (KATRINA al122005)
      # AL, 12, 2005083018,   , BEST,   0, 370N,  870W,  30,  990, TD,   0,    ,    0,    0,    0,    0,
      # AL, 12, 2005083100,   , BEST,   0, 386N,  853W,  30,  994, EX,   0,    ,    0,    0,    0,    0,
      # AL, 12, 2005083106,   , BEST,   0, 401N,  829W,  25,  996, EX,   0,    ,    0,    0,    0,    0,
      my $line_length = length($line);
      # reject lines at the start of the BEST track file that are incomplete
      unless ($firstBestTime) {
         if ( $line_length >= 160 ) {
            my $n = $fields[27];
            if ( $n =~ /GENESIS/ || $n =~ /INVEST/ ) {
               ASGSUtil::stderrMessage("INFO","The storm name in '$bestATCF' on line '$bestLineNum' contains '$n' which does not indicate a closed circulation. This line will be skipped.",$test);
               next;
            }
         }
         my $cp = $fields[9];
         if ( $cp == 0 ) {
            ASGSUtil::stderrMessage("INFO","The central pressure storm name in '$bestATCF' on line '$bestLineNum' is '$cp'. This line will be skipped.",$test);
            next;
         }
         unless ( defined $fields[10] ) {
            ASGSUtil::stderrMessage("INFO","The quadrant designation 'NEQ' in '$bestATCF' on line '$bestLineNum' is missing. This line will be skipped.",$test);
            next;
         }
      }
      my $isotach_kts = $fields[11]; #substr($line,63,3);
      # BEST lines that predate tropical cyclone formation
      # will be missing some key information like the isotach
      # magnitude, the storm quadrant string, and/or the central pressure

      if ( $line_length >= 112 ) { # this is a complete line
         # the first isotach is 34, but can be 0 in the source data in some cases
         if ( $isotach_kts == 34 || $isotach_kts == 0 ) {
            # clear out hash so that this data is always fresh
            %complete_hc_lines = ();
         }
         # save it as-is in case we need to use it to fill in incomplete
         # lines that may occur later
         $complete_hc_lines{$isotach_kts} = $line;
      } else {
         ASGSUtil::stderrMessage("WARNING","Line $. in the BEST track file is incomplete: $line",$test);
         # fill in from a corresponding complete line from the hash, if possible
         my $last_complete_line = $complete_hc_lines{$isotach_kts};
         if ( $last_complete_line ) {
            # splice the complete line onto the incomplete line
            $line = $line . substr($last_complete_line,$line_length,999);
            ASGSUtil::stderrMessage("WARNING","That line will be replaced with the following line: $line",$test);
         } else {
            # there wasn't a corresponding line in the hash ... safest thing
            # to do is to drop this BEST track line entirely
            ASGSUtil::stderrMessage("WARNING","The incomplete line could not be filled in with data from prior lines, and will be dropped.",$test);
            next;
         }
      }
      #
      # grab the current storm name and class
      if (defined $fields[27]) {
         $fields[27]=~/\s*(\S*)\s*/; # strip spaces from current storm name
         $nhcName = $1;
      } else {
         $nhcName = "STORMNAME";
      }
      if (defined $fields[10]) {
         $fields[10]=~/\s*(\S*)\s*/; # strip spaces from current storm class
         $stormClass = $1;
      }
      #
      # record the final BEST track time, this will be used in case the
      # BEST track file was issued more recently than the forecast
      $lastBestTime = $fields[2];
      #
      # record the last BEST track values, these may be used in the
      # fill-in of later values
      $lastBestPressure=$fields[9];  #substr($line,53,4);
      $lastBestWindspeed=$fields[8]; #substr($line,48,3);
      $lastBestRmax=$fields[19];     #substr($line,109,3);
      #
      # set the tsflag if the storm has achieved TS-force winds
      if ( $lastBestWindspeed > 39.0 ) {
         $tsflag = 1;
      }
      #
      # want to save the nowcast storm position
      $old_lat=$fields[6]; #substr($line,34,4)/10.0;
      $old_lon=$fields[7]; #substr($line,41,4)/10.0;
      #
      # grab the wind radii in the four quadrants
      #$rad[0]=substr($line,74,3);
      #$rad[1]=substr($line,80,3);
      #$rad[2]=substr($line,86,3);
      #$rad[3]=substr($line,92,3);
      # jgfdebug20090624: the sub that fills in the rmax is not working
      #populateWindRadii(\@rad,\@oldrad,$lastBestRmax);
      push(@bestTimes,$fields[2]);
      # grab the date/time first usable BEST line
      unless ($firstBestTime) {
         $firstBestTime = $fields[2];
         $firstBestTime =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
         $fhcyear = $1;
         $fhcmon = $2;
         $fhcday = $3;
         $fhchour = $4;
         if ( $setColdStartDate == 1 ) {
            $coldstartdate = $firstBestTime;
            $hotstartDate = getHotstartDate($coldstartdate, $hotstartseconds);
            $hsFound = 1;
            ASGSUtil::stderrMessage("INFO","The cold start date will be set to $coldstartdate UTC.",$test);
         }
      }
      #
      # for NWS 9 and 19, check to see if the BEST track line is prior to the
      # zero date, if it is, then it will not be placed in the fort.22 file
      # for NWS 8, put all lines in the file, it will figure out which one
      # it needs
      # jgf20110720: Added possibility of swan coupling
      # jgf20160515: Skip lines that are before the hotstartdate, even
      # for NWS8, because it will be easiest for control_file_gen.pl to
      # calculate the run length if there aren't any extra lines in the
      # fort.22 file.
      if ( $setColdStartDate == 0 ) {
         if ( abs($nws) == 30 || $nws == 20 || $nws == 19 || $nws == 8 || abs($nws) == 330 || $nws == 320 || $nws == 319 || $nws == 308 ) {
            if ( $fields[2] < $hotstartDate ) {
               next;
            }
         }
         # check to see if we have found the hotstart in the BEST track file
         if ( $fields[2] == $hotstartDate ) {
            $hsFound = 1;
         }
         if ( abs($nws) == 30 || $nws == 20 || $nws == 19 || $nws == 8 || abs($nws) == 330 || $nws == 320 || $nws == 319 || $nws == 308 ) {
            if ( ($hsFound == 0) && ($fields[2] > $hotstartDate) ) {
               ASGSUtil::stderrMessage("ERROR","The date '$fields[2]' was encountered in the BEST track file '$bestATCF'; however an exact match of the date corresonding to the hotstart '$hotstartDate' should have preceded it somewhere. Therefore, the file does not contain the proper starting date (i.e., the date corresponding to hotstart). The fort.22 file will not be written.",$test);
               die;
            }
         }
      }
      $fields[2] =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
      $hyear = $1;
      $hmon = $2;
      $hday = $3;
      $hhour = $4;
      # get difference between start of usable BEST track file and this BEST time
      (my $ddays,my $dhrs, my $dsec) = Date::Calc::Delta_DHMS($fhcyear,$fhcmon,$fhcday,$fhchour,0,0,$hyear,$hmon,$hday,$hhour,0,0);
      my $time_difference = $ddays*24 + $dhrs; # in hours
      if ( abs($nws) == 30 || $nws == 20 || $nws == 19 || abs($nws) == 330 || $nws == 320 || $nws == 319 ) {
         # fill in the time difference as tau
         substr($line,29,4)=sprintf("%4d",$time_difference);
      }
      #
      # set the background pressure to 1013
      substr($line,97,4)=sprintf("%4d",1013);
      #
      # fill in the radii values
      #substr($line,74,3)=sprintf("%3d",$rad[0]);
      #substr($line,80,3)=sprintf("%3d",$rad[1]);
      #substr($line,86,3)=sprintf("%3d",$rad[2]);
      #substr($line,92,3)=sprintf("%3d",$rad[3]);
      # write the line to the file, writing an eol if the line does not have one
      if ( /\n/ ) {
         print FORT22 $line;
      } else {
         printf FORT22 "$line\n";
      }
   }
   close(BEST);
   #
   # add properties to the hash
   if ( $stormClass ne " " ) {
      $runProp{'storm class'} = $stormClass;
   }
   if ( defined $nhcName ) {
      $runProp{'stormname'} = $nhcName;
      $runProp{'forcing.tropicalcyclone.stormname'} = $nhcName;
   }
   $runProp{'forcing.tropicalcyclone.best.time.start'} = "$firstBestTime";
   if ( defined $lastBestTime ) {
      $runProp{'forcing.tropicalcyclone.best.time.end'} = "$lastBestTime";
   } else {
      $lastBestTime = $firstBestTime;
      $runProp{'forcing.tropicalcyclone.best.time.end'} = "$lastBestTime";
   }
   $runProp{'track_raw_dat'} = "bal$storm$year.dat";
}
my $forecastedDate; # as a string
my $last_pressure = $lastBestPressure;
my $last_windspeed = $lastBestWindspeed;
my $consensus_angle=0;      # direction of motion of NHC track
my $old_consensus_angle=0;  # previous direction of NHC track
#
ASGSUtil::stderrMessage("INFO","The fort.22 will be configured to start on $hotstartDate UTC.",$test);
#
my $firstForecastTime;
my $lastForecastTime;
my $fyear; my $fmon; my $fday; my $fhour;     # time at which forecast is valid
my $ftyear; my $ftmon; my $ftday; my $fthour; # time to which forecast applies
my $ftmin; my $ftsec;                         # not used
#---------------------------------------------------------------------
# P R O C E S S I N G   F O R E C A S T   F I L E
#---------------------------------------------------------------------
#
# open ATCF OFCL file, if it is present
my $forecastATCF = "$dir/al$storm$year.fst";
if ( -e $forecastATCF ) {
   $runProp{'track_raw_fst'} = $forecastATCF;
   unless (open(OFCL, "<", $forecastATCF)) {
      ASGSUtil::stderrMessage("ERROR","Failed to open forecast ATCF file '$forecastATCF' for scenario '$name': $!.",$test);
      die;
   }
   while(<OFCL>) {
      my @fields = split(',',$_);
      my $line = $_;
      # grab the datetime at which the forecast is valid
      $fields[2] =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
      $fyear = $1;
      $fmon = $2;
      $fday = $3;
      $fhour = $4;
      # grab the existing forecast period, i.e., the number of hours beyond the
      # forecast datetime that the forecast applies to
      my $tau=substr($_,29,4);
      ASGSUtil::stderrMessage("INFO","The forecast period tau is $tau",$test);
      # determine the date and time that the forecast applies to
      ($ftyear,$ftmon,$ftday,$fthour,$ftmin,$ftsec) =
      Date::Calc::Add_Delta_DHMS($fyear,$fmon,$fday, $fhour,0,0,0,$tau,0,0);
      my $forecastedDate = sprintf("%4d%02d%02d%02d",$ftyear,$ftmon,$ftday,$fthour);
      unless ($firstForecastTime) {
         $firstForecastTime = $forecastedDate;
      }
      $lastForecastTime = $forecastedDate;
      #
      # check to see if the forecast line is prior to the hotstart date,
      # if it is, then it will not be placed in the fort.22 file
      if ( $forecastedDate < $hotstartDate ) {
         next;
      }
      # if we have found the hotstart time in the forecast file
      if ( $forecastedDate == $hotstartDate ) {
         $hsFound = 1;
      }
      if ( abs($nws) == 30 || $nws == 20 || $nws == 19 || $nws == 8 || abs($nws) == 330 || $nws == 320 || $nws == 319 || $nws == 308 ) {
         if ( ($hsFound == 0) && ($forecastedDate > $hotstartDate) ) {
            ASGSUtil::stderrMessage("ERROR","The date '$forecastedDate' found in the forecast file '$forecastATCF' is after the date '$hotstartDate' corresponding to the time in the hotstart file, but exact hotstart date was never found. There is no meteorological data that corresponds to the time of the hotstart.",$test);
            die;
         }
      }
      #
      # fill in the forecasted date for metadata purposes (i.e., this is
      # not used by ADCIRC unless NWS=8, symmetric vortex)
      unless ( $nws == 8 || $nws == 308 ) {
         substr($line,8,10)=sprintf("%10d",$forecastedDate);
      }
      #
      # next, calculate the difference between the forecasted date and the hotstart
      # date so that we can fill in the forecast period
      $hotstartDate =~ m/\s*(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
      my $hsyear = $1;
      my $hsmon = $2;
      my $hsday = $3;
      my $hshour = $4;
      (my $ddays,my $dhrs, my $dsec) = Date::Calc::Delta_DHMS($hsyear,$hsmon,$hsday,$hshour,0,0,$ftyear,$ftmon,$ftday,$fthour,0,0);
      my $time_difference = $ddays*24 + $dhrs; # in hours
      if ( abs($nws) == 30 || $nws == 20 || $nws == 19 || abs($nws) == 330  || $nws == 320 || $nws == 319 ) {
         # fill in the time difference as tau
         substr($line,29,4)=sprintf("%4d",$time_difference);
      }
      #
      # set the background pressure to 1013
      substr($line,97,4)=sprintf("%4d",1013);
      #
      # grab the wind radii in the four quadrants
      $rad[0]=substr($_,74,3);
      $rad[1]=substr($_,80,3);
      $rad[2]=substr($_,86,3);
      $rad[3]=substr($_,92,3);
      # jgfdebug20090624: the sub that fills in the rmax is not working
      #populateWindRadii(@rad,@oldrad,$lastBestRmax);
      # fill in the radii values
      substr($line,74,3)=sprintf("%3d",$rad[0]);
      substr($line,80,3)=sprintf("%3d",$rad[1]);
      substr($line,86,3)=sprintf("%3d",$rad[2]);
      substr($line,92,3)=sprintf("%3d",$rad[3]);
      my $forecast_windspeed=substr($_,48,3);
      my $forecast_pressure=substr($_,53,4);
      my $vmax = $forecast_windspeed; # vmax is the perturbed max wind speed
      #
      # if the requested variation is max wind speed, modify the forecast
      # max wind speed
      #
      # jgf20160216: perturb the max wind speed before computing Pc so that
      # the Pc can take the new max wind speed into account
      if ( ($strengthPercent ne "null") && ($tau != 0)) {
         # change it by the indicated percentage
         $vmax=$vmax*(1.0+($strengthPercent/100.0));
         # write it into the ATCF line
         substr($line,47,4)=sprintf("%4d",$vmax);
      }
      #
      # set the tsflag if the storm has achieved TS-force winds
      if ( $vmax > 39.0 ) {
         $tsflag = 1;
      }
      #
      # fill in the forecast central pressure, if it is missing
      if ( $forecast_pressure == 0 ) {
         # same as last time by default
         $forecast_pressure = sprintf("%4d",$last_pressure);
         # if stronger
         if ( $vmax > $last_windspeed ) {
            $forecast_pressure = sprintf("%4d",(1040.0-0.877*$vmax));
            # the resulting pressure should be lower than the last ... if it isn't,
            # just use the slope
            if ($forecast_pressure > $last_pressure ) {
               $forecast_pressure = sprintf("%4d",($last_pressure
                  - 0.877*($vmax-$last_windspeed)));
            }
         }
         # if weaker
         if ( $vmax < $last_windspeed ) {
            $forecast_pressure = sprintf("%4d",(1000.0-0.65*$vmax));
            # the resulting pressure should be higher than the last ... if it isn't,
            # just use the slope
            if ($forecast_pressure < $last_pressure ) {
               $forecast_pressure = sprintf("%4d",($last_pressure
                  + 0.65*($last_windspeed-$vmax)));
            }
         }
         # slower windspeeds can be strange
         if ( $method eq "twoslope" ) {
            # just use the last pressure
            if ( $vmax <= 30 ) {
               $forecast_pressure = sprintf("%4d",$last_pressure);
            }
         } elsif ( $method eq "asgs2012" ) {
            # slower windspeeds can be strange ... use Dvorak if the storm is
            # early in its history, or use ah77 if it is late in its history
            if ( $vmax <= 35 ) {
               if ( $tsflag == 0 ) {
                  # use Dvorak
                  $forecast_pressure = 1015 - ($vmax/3.92*0.51444444)**(1.0/0.644);
               } else {
                  # its later in the storm's history -- use AH77
                  $forecast_pressure = 1010 - ($vmax/3.4*0.51444444)**(1.0/0.644);
               }
               $forecast_pressure = sprintf("%4d",$forecast_pressure);
            }
         }
         # fill in the forecast central pressure value
         substr($line,53,4) = $forecast_pressure;
      }
      $last_pressure = $forecast_pressure;
      $last_windspeed = $vmax;
      #
      # if the requested variation is overland speed, modify the forecast
      # period and forecastedDate
      if (($overlandSpeedPercent ne "null") && ($tau != 0)) {
         my $newtau = $tau*(1.0+(-$overlandSpeedPercent/100.0));
         # determine the date and time that the forecast applies to
         ($ftyear,$ftmon,$ftday,$fthour,$ftmin,$ftsec) =
         Date::Calc::Add_Delta_DHMS($fyear,$fmon,$fday, $fhour,0,
            0,0,$newtau,0,0);
         # recalculate the difference between the forecasted time and the zero
         # hour so that we can fill in the forecast period
         (my $ddays,my $dhrs, my $dsec) = Date::Calc::Delta_DHMS($fhcyear,$fhcmon,$fhcday,$fhchour,0,0,$ftyear,$ftmon,$ftday,$fthour,0,0);
         my $time_difference = $ddays*24 + $dhrs; # in hours
         # fill in the time difference as tau
         substr($line,29,4)=sprintf("%4d",$time_difference);
         $forecastedDate
            = sprintf("%4d%02d%02d%02d",$ftyear,$ftmon,$ftday,$fthour);
         # fill in the date and time for metadata purposes
         substr($line,8,10)=sprintf("%10d",$forecastedDate);
         $lastForecastTime = $forecastedDate;
      }
      # if the requested variation is veer, modify the track so that it veers
      # as a percent of the cone of uncertainty
      # -100% will create a track that lies along the left edge of
      # the cone of uncertainty
      # +100% will create a track that lies along the right edge of the cone
      # of uncertainty
      if (($veerPercent ne "null") && ($tau != 0)) {
         my $radius;                 # radius of uncertainty
         $radius=interpolateUncertaintyRadius($tau);
         # scale to the percentage requested
         $radius *= abs($veerPercent/100.0);
         # convert nautical miles to km
         $radius*=1.852000003180799; # to km
         # grab consensus forecast position
         my $consensusLat=substr($_,34,4)/10.0; # from tenths of degs to degs
         my $consensusLon=substr($_,41,4)/10.0; # from tenths of degs to degs
         # find the angle that consensus storm is traveling on.
         my $lat_change=$consensusLat-$old_lat;
         my $lon_change=-1*($consensusLon-$old_lon); # lon increases leftward
         unless ( $lat_change==0.0 && $lon_change==0.0 ) {
            $consensus_angle=atan2($lat_change,$lon_change);
            # save current direction of consensus track, in case track is
            # stationary in the future, so we can use the direction to
            # calculate a reasonable veer track
            $old_consensus_angle = $consensus_angle;
         } else {
            $consensus_angle=$old_consensus_angle;
         }
         # calculate position of veering track based on direction, setting
         # the angle according to the sign of the veer percent
         my $veer_xoff = 0;
         my $veer_yoff = 0;
         my $perpendicular;
         if ($veerPercent > 1) {
            $perpendicular = - ($pi/2); # veer right
         } else {
            $perpendicular = $pi/2;     # veer left
         }
         my $veer_angle = $consensus_angle + $perpendicular;
         # approximate offsets in degrees (radius is in km)
         $veer_xoff=$radius*cos($veer_angle)/100.0;
         $veer_yoff=$radius*sin($veer_angle)/100.0;
         # calculate lat and lon of veer track and convert to 10ths
         # of degrees
         my $veer_lat=($consensusLat+$veer_yoff)*10.0;
         my $veer_lon=($consensusLon-$veer_xoff)*10.0;
         # paste in the new position
         substr($line,34,4)=sprintf("%4d",$veer_lat);
         substr($line,41,4)=sprintf("%4d",$veer_lon);
         $old_lat=$consensusLat;
         $old_lon=$consensusLon;
      }
      # If NWS is 8, fill in the Rmax. If the requested variation is Rmax,
      # change it and then fill it in.
      if ( $nws == 8 ) {
         my $rmax = $lastBestRmax;
         if ( $name eq "rMax") {
            $rmax *= $sizePercent;
         }
         substr($line,109,3)=sprintf("%3d",$rmax);
      }
      # write the line to the file, writing an eol if the line does not have one
      if ( /\n/ ) {
         print FORT22 $line;
      } else {
         printf FORT22 "$line\n";
      }
   }
   close(OFCL);
   $runProp{'forcing.tropicalcyclone.fcst.time.start'} = $firstForecastTime;
   $runProp{'forcing.tropicalcyclone.fcst.time.end'} = $lastForecastTime;
} else {
   ASGSUtil::stderrMessage("INFO","The forecast ATCF file '$forecastATCF' for scenario '$name' was not found and will not be processed.",$test);
}
close(FORT22);
if ( $hsFound == 0 ) {
   if ( abs($nws) == 30 || $nws == 20 || $nws == 19 || abs($nws) == 330 || $nws == 320 || $nws == 319 ) {
      ASGSUtil::stderrMessage("ERROR","The date corresponding to the hotstart '$hotstartDate' was not found in the BEST track file $bestATCF or the forecast file $forecastATCF.",$test);
   }
}
#
# open run.properties file for recording track file properties
unless ( open(PROPS,">run.properties") ) {
   ASGSUtil::stderrMessage("ERROR","Failed to open run.properties file for scenario '$name': $!.",$test);
   die;
}
foreach my $rp (sort keys %runProp) {
   print PROPS "$rp : $runProp{$rp}\n";
}
close(PROPS);

1;


#------------------------------------------------------------------------
# getHotstartDate: Compute the hotstart date/time from the coldstart
# date/time and the number of hotstart seconds.
#------------------------------------------------------------------------
sub getHotstartDate {
   my $c=shift;     # coldstartdate
   my $h=shift;     # hotstartseconds
   $c =~ m/\s*(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   my $csyear = $1;
   my $csmon = $2;
   my $csday = $3;
   my $cshour = $4;
   # calculate the date/time associated with the hotstart file
   # by starting with the coldstart date time and adding the
   # number of seconds in the hotstart file
   my $hsyear; my $hsmon; my $hsday; my $hshour;     # the hotstart date/time
   my $hsmin; my $hssec;                             # not used
   ($hsyear,$hsmon,$hsday,$hshour,$hsmin,$hssec) =
      Date::Calc::Add_Delta_DHMS($csyear,$csmon,$csday, $cshour,0,0,0,0,0,$hotstartseconds);
   return sprintf("%4d%02d%02d%02d",$hsyear,$hsmon,$hsday,$hshour);
}


#------------------------------------------------------------------------
# populateRadii: This subroutine checks the wind radii to see if any are
# zero (the nws9 subroutine in ADCIRC cannot generate winds if any of the
# radii are zero). The zero radii (if any) are populated according to
# the assumptions listed at the top of the program.
#------------------------------------------------------------------------
sub populateWindRadii {
    my @rad=shift;
    my @oldrad=shift;
    my $currentRmax=shift;
    my $numNonZeroRad = 0;
    my $avgRad;
    # find nonzero radii and count them
    foreach (@rad) {
       if ($_ != 0 ) {
          $numNonZeroRad++;
          $avgRad += $_;
       }
    }
    # estimate the radii if necessary
    if ( $numNonZeroRad != 0 ) {
       # at least one was not zero, calculate and substitute average value
       # for the zero value(s), if any
       $avgRad /= $numNonZeroRad;
       for ( my $i=0; $i<4; $i++ ) {
          print "$i $rad[0]";
          if ( $rad[$i] == 0 ) {
              $rad[$i] = $avgRad;
          }
       }
    } else {
       # all the radii are zero, use the previous radii, if available
       if ( @oldrad ) {
          for ( my $i=0; $i<4; $i++ ) {
             $rad[$i] = $oldrad[$i];
          }
       } else {
       # there are no previous radii available, use the nowcast value
          for ( my $i=0; $i<4; $i++ ) {
             $rad[$i] = substr($_,109,3);
          }
       }
    }
    # save the radii in case they are needed on the next time level
    for ( my $i=0; $i<4; $i++ ) {
       $oldrad[$i] = $rad[$i];
    }
}




#------------------------------------------------------------------------
# interpolateRadius: This subroutine accepts the forecast period (tau)
# in hours and returns the radius of uncertainty in nautical miles. It
# must interpolate between radii published by the NHC for specific
# forecast periods.
#------------------------------------------------------------------------
sub interpolateUncertaintyRadius($) {
    my $i;         # index into array of nhc uncertainty data
    my $tau=shift;
    my $radius = 0;
    #my @nhc_tau = (0, 12, 24, 36, 48, 72, 96, 120);
    #
    #my @nhc_radii = (9.5, 32, 52, 71, 90, 122, 170, 225); #2015
    #my @nhc_radii = (9.5, 30, 49, 66, 84, 115, 165, 237); #2016
    #my @nhc_radii = (9.5, 29, 45, 63, 78, 107, 159, 211); #2017
    #my @nhc_radii = (9.5, 26, 43, 56, 74, 103, 151, 198); #2018
    #my @nhc_radii = (9.5, 16, 26, 41, 55,  69,  86, 103, 151, 196); # 2020
    #my @nhc_radii = (9.5, 16, 27, 40, 55,  69,  86, 102, 148, 200); # 2021
    #my @nhc_radii = (9.5, 16, 26, 39, 52,  67,  84, 100, 142, 200); # 2022
    #my @nhc_radii = (9.5, 16, 26, 39, 53,  67,  81,  99, 145, 205); # 2023
    #my @nhc_radii = (9.5, 16, 26, 41, 55,  70,  88, 102, 151, 220); # 2024 (derived from 2025 numbers and changes)
    my @nhc_tau =   (  0,  3, 12, 24, 36,  48,  60,  72,  96, 120);
    my @nhc_radii = (9.5, 16, 26, 39, 52,  67,  83, 100, 142, 213);  # 2025 from https://www.nhc.noaa.gov/pdf/NHC_New_Products_Updates_2025.pdf

    if ( $tau<$nhc_tau[0] ) {
	ASGSUtil::stderrMessage("WARNING","Invalid forecast period (tau) of $tau in fort.22. Setting radius of uncertainty to $nhc_radii[0].",$test);
	return $nhc_radii[0];
    } elsif ( $tau>$nhc_tau[-1] ) {
	# if the forecast period is longer than our last available data,
	# extrapolate the radius
	ASGSUtil::stderrMessage("WARNING","Forecast period of $tau hours in fort.22 is farther in the future than NHC publishes uncertainty statistics. Extrapolating radius of uncertainty from published data at $nhc_tau[-2] and $nhc_tau[-1] hours.",$test);
	$radius=($nhc_radii[-1]-$nhc_radii[-2])/($nhc_tau[-1]-$nhc_tau[-2])
	    *($tau-$nhc_tau[-1])+$nhc_radii[-1];
	return $radius;
    } elsif ( $tau>=$nhc_tau[0] && $tau<=$nhc_tau[-1]) {
	# forecast period is within our data, find the values that bracket
	# it an perform linear interpolation
	my $npoints=@nhc_tau;
	for ( $i=0; $i<=($npoints-2); ++$i ) {
	    if ( $tau>=$nhc_tau[$i] && $tau<=$nhc_tau[$i+1] ) {
		$radius=(($tau-$nhc_tau[$i])/($nhc_tau[$i+1]-$nhc_tau[$i]))
		    *($nhc_radii[$i+1]-$nhc_radii[$i])
		    +$nhc_radii[$i];
		return $radius;
	    }
	}
    } else {
   	ASGSUtil::stderrMessage("ERROR","Failed to interpolate radius of uncertainty at $tau hours.",$test);
    }
}



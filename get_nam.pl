#!/usr/bin/env perl
#--------------------------------------------------------------
# get_nam.pl: downloads background meteorology data from NCEP
# for ASGS nowcasts and forecasts
#--------------------------------------------------------------
# Copyright(C) 2010 Jason Fleming
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
#--------------------------------------------------------------
# If nowcast data is requested, the script will grab the nowcast 
# data corresponding to the current ADCIRC time, and then grab all
# successive nowcast data, if any. 
#
# If forecast data is requested, the script will grab the nowcast
# and forecast data corresponding to the current ADCIRC time.
#--------------------------------------------------------------
$^W++;
use strict;
use Net::FTP;
use Getopt::Long;
use Date::Pcalc;
use Cwd;
#  Usage Example:
#
#  perl ~/asgs/trunk/get_nam.pl --rundir ~/biz/NSFOil/nam_download --backsite ftpprd.ncep.noaa.gov --backdir /pub/data/nccf/com/nam/prod --enstorm nowcast --csdate 2010051100 --hstime 86400.0
#
#
my $rundir;   # directory where the ASGS is running
my $backsite; # ncep ftp site for nam data
my $backdir;  # dir on ncep ftp site
my $enstorm;  # hindcast, nowcast, or forecast
my $csdate;   # UTC date and hour (YYYYMMDDHH) of ADCIRC cold start
my $hstime;   # hotstart time, i.e., time since ADCIRC cold start (in seconds)
my $date;     # date (UTC) corresponding to current ADCIRC time
my $hour;     # hour (UTC) corresponding to current ADCIRC time
my @targetDirs; # directories to download NAM data from 
my $forecastLength = 84; # keeps retrying until it has enough forecast files 
                    # to go for the requested time period
my $max_retries = 10; # max number of times to attempt download of forecast file
my $num_retries = 0;      
my $had_enough = 0;
GetOptions(
           "rundir=s" => \$rundir,
           "backsite=s" => \$backsite,
           "backdir=s" => \$backdir,
           "enstorm=s" => \$enstorm,
           "csdate=s" => \$csdate,
           "forecastLength=s" => \$forecastLength,
           "hstime=s" => \$hstime
          );
#
# determine date and hour corresponding to current ADCIRC time
# first, extract the date/time components from the incoming string
$csdate =~ /(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $cy = $1;
my $cm = $2;
my $cd = $3;
my $ch = $4;
my ($ny, $nm, $nd, $nh, $nmin, $ns); # current ADCIRC time
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
# form the date and hour of the current ADCIRC time
$date = sprintf("%4d%02d%02d",$ny ,$nm, $nd);
$hour = sprintf("%02d",$nh);
#
# now go to the ftp site and download the files
my $dl = 0;   # true if we were able to download the file(s) successfully
my $ftp;
my $ftp = Net::FTP->new($backsite, Debug => 0, Passive => 1); 
unless ( defined $ftp ) {
   stderrMessage("ERROR","ftp: Cannot connect to $backsite: $@");
   printf STDOUT $dl;
   exit 1;
}
my $ftpLoginSuccess = $ftp->login("anonymous",'-anonymous@');
unless ( $ftpLoginSuccess ) {
   stderrMessage("ERROR","ftp: Cannot login: " . $ftp->message);
   printf STDOUT $dl;
   exit 1;
}
# switch to binary mode
$ftp->binary();
# cd to the directory containing the NAM files
my $hcDirSuccess = $ftp->cwd($backdir);
unless ( $hcDirSuccess ) {
   stderrMessage("ERROR",
       "ftp: Cannot change working directory to '$backdir': " . $ftp->message);
   printf STDOUT $dl;
   exit 1;
}
# get the list of nam dates where data is available
my @ncepDirs = $ftp->ls(); # gets all the current data dirs, incl. nam dirs
my @namDirs; 
foreach my $dir (@ncepDirs) { 
   if ( $dir =~ /nam.\d+/ ) { # filter out non-nam dirs
      push(@namDirs,$dir);
   }
}
# now sort the NAM dirs from lowest to highest (it appears that ls() does
# not automatically do this for us
my @sortedNamDirs = sort { lc($a) cmp lc($b) } @namDirs;
# narrow the list to the target date and any later dates
my @targetDirs;
foreach my $dir (@sortedNamDirs) {
#   stderrMessage("DEBUG","Found the directory '$dir' on the NCEP ftp site.");
   $dir =~ /nam.(\d+)/;
   if ( $1 < $date ) { 
      next; 
   } else {
      push(@targetDirs,$dir);
   }
}
# determine the most recent date/hour ... this is the cycle time
$targetDirs[-1] =~ /nam.(\d+)/;
my $cycledate = $1; 
#stderrMessage("DEBUG","The cycledate is '$cycledate'.");
if ( $cycledate < $date ) { 
   stderrMessage("ERROR","The cycledate is '$cycledate' but the ADCIRC hotstart date is '$date'; therefore an error has occurred. get_nam.pl is halting this attempted download.");
   printf STDOUT $dl;
   exit;
}
$hcDirSuccess = $ftp->cwd($targetDirs[-1]);
unless ( $hcDirSuccess ) {
   stderrMessage("ERROR",
      "ftp: Cannot change working directory to '$targetDirs[-1]': " . $ftp->message);
   printf STDOUT $dl;
   exit;
}
my $cyclehour;
my @allFiles = $ftp->ls(); 
foreach my $file (@allFiles) { 
   if ( $file =~ /nam.t(\d+)z.awip1200.tm00.grib2/ ) { 
      $cyclehour = $1;
      stderrMessage("DEBUG","The cyclehour is '$cyclehour'.");
   }
}
my $cycletime = $cycledate . $cyclehour;
stderrMessage("DEBUG","The cycletime is '$cycletime'.");
#
# we need to have at least one set of files beyond the current nowcast
# time, i.e., we need fresh new files that we have not run with yet
if ( $cycletime <= ($date.$hour) ) {
   stderrMessage("DEBUG","No new files on NAM ftp site.");
   printf STDOUT $dl;
   exit;
}
#
# if we made it to here, then there must be some new files on the 
# NAM ftp site for us to run
$hcDirSuccess = $ftp->cdup();
unless ( $hcDirSuccess ) {
   stderrMessage("ERROR",
      "ftp: Cannot change working directory to parent of '$targetDirs[-1]': " . $ftp->message);
   printf STDOUT $dl;
   exit;
}
# create the directores for this cycle if needed
unless ( -e $cycletime ) { 
   unless ( mkdir($cycletime,0777) ) {
      stderrMessage("ERROR","Could not make directory '$cycletime': $!.");
      die;
   }
}
# create the nowcast and forecast directory for this cycle if needed
unless ( -e $cycletime."/nowcast" ) { 
   unless ( mkdir($cycletime."/nowcast",0777) ) {
      stderrMessage("ERROR","Could not make directory '$cycletime/nowcast': $!.");
      die;
   }
}
unless ( -e $cycletime."/namforecast" ) { 
   unless ( mkdir($cycletime."/namforecast",0777) ) {
      stderrMessage("ERROR","Could not make directory '$cycletime/namforecast': $!.");
      die;
   }
}
my $localDir;
my @targetFiles; 
#
# loop over target directories, grabbing all files relevant to a nowcast
foreach my $dir (@targetDirs) {
   stderrMessage("INFO","Downloading from directory '$dir'.");
   $hcDirSuccess = $ftp->cwd($dir);
   unless ( $hcDirSuccess ) {
      stderrMessage("ERROR",
         "ftp: Cannot change working directory to '$dir': " . $ftp->message);
      printf STDOUT $dl;
      exit;
   }
   # form list of the files we want
   # for the nowcast files, we need to create at least one deeper 
   # directory to hold the data for the NAMtoOWI.pl -- the nowcast file
   # names do not indictate the date, and we may end up having to get
   # multiple nowcasts and stringing them together ... these nowcasts 
   # may span more than one day -- the prefix "erl." is arbitrary I think
   # but NAMtoOWI.pl is hardcoded to look for it
   $dir =~ /nam.(\d+)/;
   my $dirDate = $1;
   $localDir = $cycletime."/nowcast/erl.".substr($dirDate,2);
   unless ( -e $localDir ) { 
      unless ( mkdir($localDir,0777) ) {
         stderrMessage("ERROR","Could not make the directory '$localDir': $!");
         die;
      }
   }
   #
   # get any nowcast files in this directory that are later than 
   # the current adcirc time
   my @nowcastHours = qw/00 06 12 18/;
   # remove hours from the list if we are not interested in them
   foreach my $nchour (@nowcastHours) {
      if ( $dirDate == $date ) {
         if ( $nchour < $hour ) { 
            next; # skip any that are before the current adcirc time 
         }
      } 
      if ( $dirDate == $cycledate ) {
         if ( $nchour > $cyclehour ) {
            next; # skip any that are after the most recent file we know of
         }
      }
      my $hourString = sprintf("%02d",$nchour);
      my $f = "nam.t".$hourString."z.awip1200.tm00.grib2";
      stderrMessage("INFO","Downloading '$f' to '$localDir'.");
      my $success = $ftp->get($f,$localDir."/".$f);
      unless ( $success ) {
         stderrMessage("INFO","ftp: Get '$f' failed: " . $ftp->message);
         next;
      } else {
         stderrMessage("INFO","Download complete.");
         $dl++;
      }
   }
   $hcDirSuccess = $ftp->cdup();
   unless ( $hcDirSuccess ) {
      stderrMessage("ERROR",
         "ftp: Cannot change working directory to parent of '$dir': " . $ftp->message);
      printf STDOUT $dl;
      exit;
   }
}
# now download all the files that are relevant to a forecast
@targetFiles="";
$localDir = $cycletime."/namforecast";
stderrMessage("INFO","Downloading from directory 'nam.$cycledate'.");
   $hcDirSuccess = $ftp->cwd("nam.".$cycledate);
   unless ( $hcDirSuccess ) {
      stderrMessage("ERROR",
         "ftp: Cannot change working directory to 'nam.$cycledate': " . $ftp->message);
      printf STDOUT $dl;
      exit;
   }
# forecast files are the list of files to retrieve 
for (my $i=0; $i<=$forecastLength; $i+=3 ) {
   my $hourString = sprintf("%02d",$cyclehour);
   my $f = "nam.t".$hourString."z.awip12".sprintf("%02d",$i).".tm00.grib2";
   # sometimes an error occurs in Net::FTP causing this script to bomb out;
   # the asgs will retry, but we don't want it to re-download stuff that it
   # already has
   if ( -e $localDir."/".$f ) { 
      stderrMessage("INFO","'$f' has already been downloaded to '$localDir'.");
      $dl++;
      next;
   }
   stderrMessage("INFO","Downloading '$f' to '$localDir'.");
   my $success = 0;
   $num_retries = 1;
   while ( $success == 0 && $num_retries < $max_retries ) {
      my $stat = $ftp->get($f,$localDir."/".$f);
      unless ( $stat ) {
         stderrMessage("INFO","ftp: Get '$f' failed: " . $ftp->message);
         $num_retries++;
         stderrMessage("DEBUG","num_retries is $num_retries");
         sleep 60; 
      } else {
         $dl++;
         $success = 1;
         stderrMessage("INFO","Downloaded in $num_retries attempt(s)."); 
      }
   }
   if ( $num_retries >= $max_retries ) {
      $had_enough = 1
   }
}
# if we found at least two files, we assume have enough for the next advisory
if ( ($dl >= ($forecastLength/3 + 2)) || ($had_enough == 1) ) {
   printf STDOUT $cycletime;
} else {
   printf STDOUT "0";
}
1;

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: get_nam.pl: $message\n";
   if ($level eq "ERROR") {
      sleep 60
   }
}

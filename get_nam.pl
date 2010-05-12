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
# successive nowcast data. 
#
# If forecast data is requested, the script will grab the nowcast
# and corresponding forecast data corresponding to the current 
# ADCIRC time.
#
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
GetOptions(
           "rundir=s" => \$rundir,
           "backsite=s" => \$backsite,
           "backdir=s" => \$backdir,
           "enstorm=s" => \$enstorm,
           "csdate=s" => \$csdate,
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
# save the current ADCIRC date/time as the cycle number
my $cycletime = sprintf("%4d%02d%02d%02d",$ny ,$nm, $nd, $nh);
printf STDOUT $cycletime;
#
# create the directory for this cycle if needed
unless ( -e $cycletime ) { 
   unless ( mkdir($cycletime,0777) ) {
      stderrMessage("ERROR","Could not make directory '$cycletime': $!.");
      die;
   }
}
#
# now go to the ftp site and download the files
my $dl = 0;   # true if we were able to download the file(s) successfully

while (!$dl) {
   my $ftp;
   my $ftp = Net::FTP->new($backsite, Debug => 0, Passive => 1); 
   unless ( defined $ftp ) {
      stderrMessage("ERROR","ftp: Cannot connect to $backsite: $@");
      next;
   }
   my $ftpLoginSuccess = $ftp->login("anonymous",'-anonymous@');
   unless ( $ftpLoginSuccess ) {
      stderrMessage("ERROR","ftp: Cannot login: " . $ftp->message);
      next;
   }
   my $hcDirSuccess = $ftp->cwd($backdir);
   unless ( $hcDirSuccess ) {
      stderrMessage("ERROR",
          "ftp: Cannot change working directory to '$backdir': " . $ftp->message);
      next;
   }
   # get the list of nam dates where data is available
   my @ncepDirs = $ftp->ls(); # gets all the current data dirs, incl. nam dirs
   my @namDirs; 
   foreach my $dir (@ncepDirs) { 
      if ( $dir =~ /nam.\d+/ ) { # filter out non-nam dirs
         push(@namDirs,$dir);
      }
   }
   my @targetDirs;
   # narrow the list to the target date and any later dates
   foreach my $dir (@namDirs) {
      $dir =~ /nam.(\d+)/;
      if ( $1 < $date ) { 
         next; 
      } else {
         push(@targetDirs,$dir);
      }
   } 
   # directory name that contains the file(s) we want
   foreach my $dir (@targetDirs) {
      stderrMessage("INFO","Downloading from directory '$dir'.");
      $hcDirSuccess = $ftp->cwd($dir);
      unless ( $hcDirSuccess ) {
         stderrMessage("ERROR",
            "ftp: Cannot change working directory to '$dir': " . $ftp->message);
         next;
      }
      # form list of the files we want
      my @targetFiles; 
      # if this is a nowcast, we need to create at least one deeper 
      # directory to hold the data for the NAMtoOWI.pl -- the nowcast file
      # names do not indictate the date, and we may end up having to get
      # multiple nowcasts and stringing them together ... these nowcasts 
      # may span more than one day -- the prefix "erl." is arbitrary I think
      # but NAMtoOWI.pl is hardcoded to look for it
      my $localDir;
      if ( $enstorm eq "nowcast" ) {
         # we also want to grab any/all nowcasts that are later than the
         # nowcast that corresponds to the current ADCIRC time
         $dir =~ /nam.(\d+)/; # just use the last two digits for the year
         $localDir = $cycletime."/".$enstorm."/erl.".substr($1,2);
         # get any nowcast files in this directory that are later than 
         # the current nowcast file
         my @nowcastHours = qw/00 06 12 18/;
         # if we are looking at a directory that is later than 
         # the current date, then grab all nowcast hours
         if ( $1 > $date ) {
            $hour = -1;
         }
         foreach my $nchour (@nowcastHours) {
            if ( $nchour < $hour ) { 
               next; # skip any that are earlier
            } 
            my $hourString = sprintf("%02d",$nchour);
            push(@targetFiles,"nam.t".$hourString."z.awip1200.tm00.grib2");
         }
      } else {
         # add the forecast files to the list of files to retrieve 
         for (my $i=0; $i<=84; $i+=3 ) {
            my $hourString = sprintf("%02d",$hour);
            push(@targetFiles,"nam.t".$hourString."z.awip12".sprintf("%02d",$i).".tm00.grib2");
         }
         $localDir = $cycletime."/".$enstorm;
      }
      unless ( mkdir($localDir,0777) ) {
         stderrMessage("ERROR","Could not make the directory '$localDir': $!");
         die;
      }
      foreach my $f (@targetFiles) {
         stderrMessage("INFO","Downloading '$f' to '$localDir'.");
         $dl = $ftp->get($f,$localDir."/".$f);
         unless ( $dl ) {
            stderrMessage("INFO","ftp: Get '$f' failed: " . $ftp->message);
            next;
         } else {
            stderrMessage("INFO","Download of '$f' is complete.");
         }
      }
      # if this is a forecast, we now have everything we need ... however,
      # if there are later nowcasts, this means we are behind, so we should
      # let the user know
      if ( $enstorm ne "nowcast" ) { 
         stderrMessage("INFO","Download of forecast files complete.");
         $dl = 1;
         exit;
         # TODO: check for later nowcasts and warn the user if they are present
      }
      $hcDirSuccess = $ftp->cdup();
      unless ( $hcDirSuccess ) {
         stderrMessage("ERROR",
            "ftp: Cannot change working directory to parent of '$dir': " . $ftp->message);
         next;
      }
   }
   $dl = 1;
}
1;

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hour:$minute:$second]";
   printf STDERR "$theTime $level: get_nam.pl: $message\n";
   if ($level eq "ERROR") {
      sleep 60
   }
}

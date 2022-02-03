#!/usr/bin/env perl
#--------------------------------------------------------------
# get_nam_status.pl: determines the latest available cycle(s)
# from NCEP NAM for ASGS nowcasts and forecasts
#--------------------------------------------------------------
# Copyright(C) 2022 Jason Fleming
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
$^W++;
use strict;
use Net::FTP;
use Getopt::Long;
use JSON::PP;
use Cwd;
#
my $startcycle = "null";  # optional arg that indicates start of range of interest
my $backsite = "null";          # ncep ftp site for nam data
my $backdir = "null"; # dir on ncep ftp site
my @cyclerange; # if $startcycle was supplied this array will be populated with a range of cycles from startcycle or earliest available to the latest  
#
our $this = "get_nam_status.pl";
my $jsonfile = "$this.json";
our %jsonhash;
my $ncepcycles = "forcing.nam.ncep.cyclelist";
#
our @grib_fields = ( "PRMSL","UGRD:10 m above ground","VGRD:10 m above ground" );
#
GetOptions(
           "startcycle=s" => \$startcycle,          
           "backsite=s" => \$backsite,
           "backdir=s" => \$backdir
          );
#
# read JSON file to get any configuration info
unless ( open(JF,"<$jsonfile") ) {
   stderrMessage("INFO","Could not open '$jsonfile' for reading: $!.");
} else {
   # slurp the file contents into a scalar variable
   my $file_content = do { local $/; <JF> };
   close(JF);
   # deserialize JSON
   my $ref = JSON::PP->new->decode($file_content);
   %jsonhash = %$ref;
}
#
# grab config info and use it if it was not 
# already provided on the command line
# also set reasonable defaults
if ( $backsite eq "null" ) {
   if ( %jsonhash && defined $jsonhash{"forcing.nam.backsite"} ) {
      $backsite = $jsonhash{"forcing.nam.backsite"};
   } else {
      $backsite = "ftp.ncep.noaa.gov";
   }
}
if ( $backdir eq "null" ) {
   if ( %jsonhash && defined $jsonhash{"forcing.nam.backdir"} ) {
      $backdir = $jsonhash{"forcing.nam.backdir"};
   } else {
      $backdir = "/pub/data/nccf/com/nam/prod";
   }
}
# if the startcycle was not provided, the script
# will return a list of all the cycles available
# from ncep
if ( $startcycle eq "null" && %jsonhash ) {
   my $cyclelistref = $jsonhash{"forcing.nam.ncep.cyclelist"};
   my @cyclelist = @$cyclelistref;
   if ( defined $cyclelist[0] ) {
      $startcycle = $cyclelist[0]; 
   }
}
#
&appMessage("DEBUG","Connecting to $backsite:$backdir");
our $dl = 0;   # true if latest status was determined successfully
# open ftp connection
our $ftp = Net::FTP->new($backsite, Debug => 0, Passive => 1);
unless ( defined $ftp ) {
   stderrMessage("ERROR","ftp: Cannot connect to $backsite: $@");
   printf STDOUT $dl;
   die;
}
my $ftpLoginSuccess = $ftp->login("anonymous",'-anonymous@');
unless ( $ftpLoginSuccess ) {
   stderrMessage("ERROR","ftp: Cannot login: " . $ftp->message);
   printf STDOUT $dl;
   die;
}
# switch to binary mode
$ftp->binary();
# cd to the directory containing the NAM files
my $hcDirSuccess = $ftp->cwd($backdir);
unless ( $hcDirSuccess ) {
   stderrMessage("ERROR",
       "ftp: Cannot change working directory to '$backdir': " . $ftp->message);
   printf STDOUT $dl;
   die;
}
#
# now go to the ftp site and 
# get the list of nam dates where data is available
# and report latest data available on the site
# directory entries are named e.g., nam.20220111
my @ncepDirs = $ftp->ls(); # gets all the current data dirs, incl. nam dirs
my @namDirs;
foreach my $dir (@ncepDirs) {
   if ( $dir =~ /nam.\d+/ ) { # filter out non-nam dirs
      push(@namDirs,$dir);
   }
}
# now sort the NAM dirs from lowest to highest (it appears that ls() does
# not automatically do this for us)
my @sortedNamDirs = sort { lc($a) cmp lc($b) } @namDirs;
# if the $startcycle was provided, remove any directories that
# are before the date of the $startcycle
my $startdate = "null";
if ( $startcycle ne "null" ) {
   $startcycle =~ /(\d{10})/;
   $startdate = $1; 
   my $numbefore = 0; # number of directories prior to the startcycle directory
   DIRECTORIES : foreach my $dir (@sortedNamDirs) {
      $dir =~ /nam.(\d{10})/;
      my $dirdate = $1;
      if ( $dirdate < $startdate ) {
         $numbefore++;
      }
   }
   # toss directories that are before the startcycle
   # (there may still be a directory with a date that 
   # has some cycles before the start cycle, we will
   # deal with that later)
   for ( my $i=0; $i<$numbefore; $i++ ) {
      shift(@sortedNamDirs);
   }
} else {
   # startcycle was not specified, so find the 
   # first date and time that data are available
   $sortedNamDirs[0] =~ /nam.(\d{10})/;
   $startdate = $1;
   # change to that directory and see if there are files in there
   $hcDirSuccess = $ftp->cwd("$backdir/$sortedNamDirs[0]");
   unless ( $hcDirSuccess ) {
      &stderrMessage("ERROR","ftp: Cannot change working directory to '$backdir/$sortedNamDirs[0]': " . $ftp->message);
      printf STDOUT $dl;
      die;
   }
   #my @allFiles = $ftp->ls();
   my @earliestFiles = grep /awip1200.tm00/, $ftp->ls();
   # now sort the NAM files from lowest to highest (it appears that ls() does
   # not automatically do this for us)
   my @sortedEarliestFiles = sort { lc($a) cmp lc($b) } @earliestFiles;
   $sortedEarliestFiles[0] =~ /nam.t(\d+)z.awip1200.tm00.grib2/;
   $startcycle = $1;
}
# sanity check
my $numSortedNamDirs = @sortedNamDirs;
if ( $numSortedNamDirs == 0 ) {
   stderrMessage("WARNING","Failed to find any NAM data directories.");
   printf STDOUT $dl;
   die;
}
# determine the latest NAM directory that has data in it
# (a new directory may exist and be empty for some period
#  of time, so cannot be counted on to contain the latest data)
my $targetDir = "null";  # latest directory that is not empty
my $targetDirFound = 0;  # true if the latest directory that is not empty has been found
my @sortedFiles;         # data files in the most recent nam directory
my $cycletime = "null";  # date and hour latest cycle
my $cycledate = "null";  # date of latest cycle
my $cyclehour = "null";  # hour of latest cycle
LATESTDIR : while ( ! $targetDirFound && scalar(@sortedNamDirs) != 0 ) {
   $targetDir = $sortedNamDirs[-1];
   # determine the most recent date/hour ... this is the latest nam cycle time
   $targetDir =~ /nam.(\d+)/;
   $cycledate = $1;
   &appMessage("DEBUG","The cycledate is '$cycledate'.");
   # change to that directory and see if there are files in there
   $hcDirSuccess = $ftp->cwd("$backdir/$targetDir");
   unless ( $hcDirSuccess ) {
      stderrMessage("ERROR","ftp: Cannot change working directory to '$backdir/$targetDir': " . $ftp->message);
      printf STDOUT $dl;
      die;
   }
   #my @allFiles = $ftp->ls();
   my @allFiles = grep /awip1200.tm00/, $ftp->ls();
   if (!@allFiles){
      #die "no awip1200 files yet in $targetDirs[-1]\n";
      stderrMessage("INFO","No awip1200.tm00 files yet in $targetDir.");
      #printf STDOUT $dl;
      #exit 0;
      pop(@sortedNamDirs);
   } else {
      $targetDirFound = 1;
      # now sort the NAM files from lowest to highest (it appears that ls() does
      # not automatically do this for us)
      @sortedFiles = sort { lc($a) cmp lc($b) } @allFiles;
   }
}
unless ( $targetDirFound && scalar(@sortedFiles) ) {
   stderrMessage("ERROR","Could not find any NAM files in any NAM directory in the specified time range.");
   printf STDOUT $dl;
   die;
}
#
TODAYSFILES : foreach my $file (@sortedFiles) {
   if ( $file =~ /nam.t(\d+)z.awip1200.tm00.grib2/ ) {
      $cyclehour = $1; # find the last one that matches the pattern
   }
}
unless ( $cyclehour ne "null" ) {
   stderrMessage("WARNING","Could not download the list of NAM files from NCEP.");
   printf STDOUT $dl;
   die;
} else {
   #stderrMessage("DEBUG","The cyclehour is '$cyclehour'.");
   $cycletime = $cycledate . $cyclehour;
   printf STDOUT $cycletime; # success
}
stderrMessage("DEBUG","The cycletime is '$cycletime'."); 
#
# write a JSON file 
# that contains all the cycles available between the given starting
# date/time and the latest available cycle (inclusive)
my @cyclesInRange; # between startcycle and the latest
DIRECTORIES : foreach my $dir (@sortedNamDirs) {
   #printf STDOUT "$dir\n"; #jgfdebug
   # cd to the directory containing the NAM directories
   my $hcDirSuccess = $ftp->cwd("$backdir/$dir");
   unless ( $hcDirSuccess ) {
      stderrMessage("ERROR",
            "ftp: Cannot change working directory to '$backdir/$dir': " . $ftp->message);
      printf STDOUT $dl;
      die;
   } 
   $dir =~ /nam.(\d+)/;
   my $thisdate = $1; 
   my @allFiles = grep /awip1200.tm00/, $ftp->ls();
   @sortedFiles = sort { lc($a) cmp lc($b) } @allFiles;
   my $thishour = "null";
   my $thiscycle = "null";
   FILES : foreach my $file (@sortedFiles) {
      #printf STDOUT "$file\n"; #jgfdebug
      # anchor the .grib2 to end of string to avoid matching .grib2.idx
      if ( $file =~ /nam.t(\d+)z.awip1200.tm00.grib2$/ ) {
         $thishour = $1;  
         $thiscycle = $thisdate . $thishour; 
         if ( $thiscycle >= $startcycle && $thiscycle <= $cycletime ) {
            push(@cyclesInRange,$thiscycle);
         }
      }
   }
}
# add the parameters and the cycle list to the hash 
$jsonhash{"forcing.nam.backsite"} = $backsite;
$jsonhash{"forcing.nam.backdir"} = $backdir;
$jsonhash{$ncepcycles} = \@cyclesInRange;
&writeJSON;
# exit successfully
exit 0;

sub writeJSON () {
   my $lastupdatedref = $jsonhash{"lastupdated"};
   my @lastupdated = @$lastupdatedref;
   my $timestamp = &getTimeStamp;
   my $ts_ref = { $this => $timestamp };
   push(@lastupdated,$ts_ref);
   $jsonhash{"lastupdated"} = \@lastupdated;
   unless ( open(SJ,">$this.json") ) {
      &stderrMessage("ERROR","Could not open '$this.json' for writing: $!.");
      die;
   }
   my $json = JSON::PP->new->utf8->pretty->canonical->encode(\%jsonhash);
   print SJ $json;
   close(SJ);
}
#
# write a log message to stderr
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my $theTime = &getTimeStamp;
   printf STDERR "[$theTime] $level: $this: $message\n";
}
#
# write a log message to a log file dedicated to this script (typically debug messages)
sub appMessage () {
   my $level = shift;
   my $message = shift;
   my $theTime = &getTimeStamp;
   #
   # open an application log file
   unless ( open(APPLOGFILE,">>$this.log") ) {
      &stderrMessage("ERROR","Could not open $this.log for appending: $!.");
   }
   printf APPLOGFILE "[$theTime] $level: $this: $message\n";
   close(APPLOGFILE);
}

sub getTimeStamp () {
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "$year-$months[$month]-$dayOfMonth-T$hms";
   return $theTime;
}


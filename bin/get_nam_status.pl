#!/usr/bin/env perl
#--------------------------------------------------------------
# get_nam_status.pl: determines the latest available cycle(s)
# from NCEP NAM for ASGS nowcasts and forecasts
#--------------------------------------------------------------
# Copyright(C) 2022--2023 Jason Fleming
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
use strict;
use warnings;
use Net::FTP;
use HTTP::Tiny;
use Getopt::Long;
use JSON::PP;
use Cwd;
use ASGSUtil;
#
my $startcycle = "null";  # optional arg that indicates start of range of interest
my $backsite = "null";          # ncep ftp site for nam data
my $backdir = "null"; # dir on ncep ftp site
my @cyclerange; # if $startcycle was supplied this array will be populated with a range of cycles from startcycle or earliest available to the latest
#
my $ncepcycles = "cyclelist";
#
our @grib_fields = ( "PRMSL","UGRD:10 m above ground","VGRD:10 m above ground" );
#
GetOptions(
           "startcycle=s" => \$startcycle,
           "backsite=s" => \$backsite,
           "backdir=s" => \$backdir
          );

# initialize $ua only once and restrict direct access to it
{
    my $ua = HTTP::Tiny->new;

    # replaces the $ftp->ls for files, looks only for nam.* files
    sub http_ls {
        my $dir = shift;
        my $url         = sprintf( qq{https://nomads.ncep.noaa.gov%s/ls-l}, $dir );
        my $res         = $ua->get($url);
        my $raw_listing = $res->{content};
        my @files       = ( $raw_listing =~ m/ +(nam.+)\n/g );
        if ( not @files ) {
            warn "!! No files found via $url\n";
        }
        return @files;
    }

    # replaces the $ftp->ls for directory listings, extracts from the HTML listing
    sub http_dir {
        my $dir = shift;
        my $url         = sprintf( qq{https://nomads.ncep.noaa.gov%s}, $dir );
        my $res         = $ua->get($url);
        my $raw_listing = $res->{content};
        my @dirs        = ( $raw_listing =~ m/href="(nam\.\d{8}|\d\d)\/"/g );
        if ( not @dirs ) {
            warn "!! No directories found via $url\n";
        }
        return @dirs;
    }
}

#
# JSON request
my $file_content = do { local $/; <> };
# deserialize JSON
my $jshash_ref = JSON::PP->new->decode($file_content);
#
# grab config info and use it if it was not
# already provided on the command line
# also set reasonable defaults
ASGSUtil::setParameter( $jshash_ref, \$backsite,  "siteHost", "ftp.ncep.noaa.gov");
ASGSUtil::setParameter( $jshash_ref, \$backdir,   "siteDir",  "/pub/data/nccf/com/nam/prod");
# use values from JSON if they were provided
$jshash_ref->{"siteHost"} = $backsite;
$jshash_ref->{"siteDir"} = $backdir;
#
#  N O W C A S T   F R O M   F I L E S Y S T E M
#
if ( $backsite eq "filesystem" ) {
   ASGSUtil::appMessage( "INFO", "Constructing nowcast from grib2 files found in $backdir.");
   # check to see if the Operator-supplied directory exists
   if ( -d $backdir ) {
      ASGSUtil::appMessage( "INFO", "The directory $backdir was found.");
   } else {
      my $msg = qq{"The directory $backdir was not found."};
      ASGSUtil::appMessage( "INFO", $msg );
      die $msg;
   }
   my @grib2Files;
   my @grib2Dirs = glob( $backdir . "/erl.*" );
   my $numGrib2Dirs = @grib2Dirs;
   ASGSUtil::appMessage("INFO","There is/are $numGrib2Dirs directories to process." );
   if ( $numGrib2Dirs == 0 ) {
      my $msg = qq{There are no erl.* directories in $backdir.};
      ASGSUtil::appMessage( "ERROR", $msg );
      die $msg;
   }
   foreach my $dir (@grib2Dirs) {
      # e.g.:
      # $dataDir/erl.220123/nam.t18z.awip1200.tm00.grib2
      # $dataDir/erl.220124/nam.t00z.awip1200.tm00.grib2
      # $dataDir/erl.220124/nam.t06z.awip1200.tm00.grib2
      my @thisDirFiles = glob( $dir . "/nam.*awip1200*.grib2");
      push( @grib2Files, @thisDirFiles );
      my $numThisDirFiles = @thisDirFiles;
      ASGSUtil::appMessage("DEBUG","Found $numThisDirFiles grib2 file(s) in $dir." );
   }
   # make a list of the cycletimes associated with these files
   my @cyclesInRange;
   foreach my $file (@grib2Files) {
      $file =~ /erl.(\d{6})\/nam.t(\d{2})/;
      my $cycletime = "20$1$2";
      push( @cyclesInRange, $cycletime );
   }
   # add the parameters and the cycle list to the hash
   $jshash_ref->{"status"} = basename($0).".json";
   $jshash_ref->{"cyclelist"} = \@cyclesInRange;
   ASGSUtil::timestampJSON($jshash_ref);
   print JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
   exit;   # exit successfully
}
#
#  N O W C A S T   F R O M   N C E P
#
# if the startcycle was not provided, the script
# will return a list of all the cycles available
# from ncep
if ( $startcycle eq "null" && $jshash_ref ) {
   my $cyclelistref = $jshash_ref->{"cyclelist"};
   my @cyclelist = @$cyclelistref;
   if ( defined $cyclelist[0] ) {
      $startcycle = $cyclelist[0];
   }
}
#
ASGSUtil::appMessage( "DEBUG", "Connecting to $backsite:$backdir");
our $dl = 0;   # true if latest status was determined successfully
# open ftp connection
our $ftp = Net::FTP->new($backsite, Debug => 0, Passive => 1, Timeout => 120);
unless ( defined $ftp ) { ASGSUtil::stderrMessage("ERROR", "ftp: Cannot connect to $backsite: $@");
   exit 1;
}
my $ftpLoginSuccess = eval{ $ftp->login("anonymous",'-anonymous@') };
unless ( $ftpLoginSuccess ) {
   ASGSUtil::stderrMessage("ERROR", "ftp: Cannot login: " . $ftp->message);
   exit 1;
}
# switch to binary mode
$ftp->binary();
# cd to the directory containing the NAM files
my $hcDirSuccess = eval { $ftp->cwd($backdir) };
unless ( $hcDirSuccess ) {
   ASGSUtil::stderrMessage("ERROR", "ftp: Cannot change working directory to '$backdir': " .  $ftp->message);
   exit 1;
}
#
# now go to the ftp site and
# get the list of nam dates where data is available
# and report latest data available on the site
# directory entries are named e.g., nam.20220111

local $@;
my @ncepDirs = eval { http_dir($ftp->pwd) }; # gets all the current data dirs, incl. nam dirs
if ($@) {
   my $msg = ($@ =~ m/timeout/i) ? q{[Net::FTP] Timeout} : $@;
   ASGSUtil::stderrMessage("ERROR", q{ftp: Cannot list NCEP directories: } . $msg);
   exit 1;
}

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
   $hcDirSuccess = eval { $ftp->cwd("$backdir/$sortedNamDirs[0]") };
   unless ( $hcDirSuccess ) {
      ASGSUtil::stderrMessage( "ERROR", "ftp: Cannot change working directory to '$backdir/$sortedNamDirs[0]': " .  $ftp->message);
      exit 1;
   }
   #my @allFiles = http_ls($ftp->pwd);
   local $@;
   my @earliestFiles = eval { grep /awip1200.tm00/, http_ls($ftp->pwd) };
   if ($@) {
     my $msg = ($@ =~ m/timeout/i) ? q{[Net::FTP] Timeout} : $@;
     ASGSUtil::stderrMessage("ERROR", q{ftp: Cannot list "earliest" files: } . $msg);
     exit 1;
   }

   # now sort the NAM files from lowest to highest (it appears that ls() does
   # not automatically do this for us)
   my @sortedEarliestFiles = sort { lc($a) cmp lc($b) } @earliestFiles;
   $sortedEarliestFiles[0] =~ /nam.t(\d+)z.awip1200.tm00.grib2/;
   $startcycle = $1;
}
# sanity check
my $numSortedNamDirs = @sortedNamDirs;
if ( $numSortedNamDirs == 0 ) {
   ASGSUtil::stderrMessage( "WARNING", "Failed to find any NAM data directories.");
   exit 1;
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
   ASGSUtil::appMessage("DEBUG", "The cycledate is '$cycledate'.");
   # change to that directory and see if there are files in there
   $hcDirSuccess = eval { $ftp->cwd("$backdir/$targetDir") };
   unless ( $hcDirSuccess ) {
      ASGSUtil::stderrMessage("ERROR", "ftp: Cannot change working directory to '$backdir/$targetDir': " . $ftp->message);
      exit 1;
   }
   #my @allFiles = $ftp->ls();

   local $@;
   my @allFiles = eval { grep /awip1200.tm00/, http_ls($ftp->pwd) };
   if ($@) {
     my $msg = ($@ =~ m/timeout/i) ? q{[Net::FTP] Timeout} : $@;
     ASGSUtil::stderrMessage("ERROR", q{ftp: Cannot list "all" files: } . $msg);
     exit 1;
   }

   if (!@allFiles){
      #die "no awip1200 files yet in $targetDirs[-1]\n";
      ASGSUtil::stderrMessage("INFO", "No awip1200.tm00 files yet in $targetDir.");
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
   ASGSUtil::stderrMessage( "ERROR", "Could not find any NAM files in any NAM directory in the specified time range.");
   exit 1;
}
#
TODAYSFILES : foreach my $file (@sortedFiles) {
   if ( $file =~ /nam.t(\d+)z.awip1200.tm00.grib2/ ) {
      $cyclehour = $1; # find the last one that matches the pattern
   }
}
unless ( $cyclehour ne "null" ) {
   ASGSUtil::stderrMessage( "WARNING", "Could not download the list of NAM files from NCEP.");
   exit 1;
} else {
   #stderrMessage("DEBUG","The cyclehour is '$cyclehour'.");
   $cycletime = $cycledate . $cyclehour;
   ASGSUtil::appMessage( "DEBUG", "The cycletime is '$cycletime'.");
}
#
# write a JSON file
# that contains all the cycles available between the given starting
# date/time and the latest available cycle (inclusive)
my @cyclesInRange; # between startcycle and the latest
DIRECTORIES : foreach my $dir (@sortedNamDirs) {
   # cd to the directory containing the NAM directories
   my $hcDirSuccess = eval { $ftp->cwd("$backdir/$dir") };
   unless ( $hcDirSuccess ) {
      ASGSUtil::stderrMessage( "ERROR", "ftp: Cannot change working directory to '$backdir/$dir': " .
                $ftp->message);
      exit 1;
   }
   $dir =~ /nam.(\d+)/;
   my $thisdate = $1;

   local $@;
   my @allFiles = eval { grep /awip1200.tm00/, http_ls($ftp->pwd) };
   if ($@) {
     my $msg = ($@ =~ m/timeout/i) ? q{[Net::FTP] Timeout} : $@;
     ASGSUtil::stderrMessage("ERROR", q{ftp: Cannot list "all" files: } . $msg);
     exit 1;
   }

   @sortedFiles = sort { lc($a) cmp lc($b) } @allFiles;
   my $thishour = "null";
   my $thiscycle = "null";
   FILES : foreach my $file (@sortedFiles) {
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
$jshash_ref->{"status"} = basename($0).".json";
$jshash_ref->{"cyclelist"} = \@cyclesInRange;
ASGSUtil::timestampJSON($jshash_ref);
#ASGSUtil::writeJSON($jshash_ref);
print JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
# exit successfully
1;

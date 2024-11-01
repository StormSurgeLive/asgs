#!/usr/bin/env perl
#--------------------------------------------------------------
# get_gfs_status.pl: determines the latest available cycle(s)
# from NCEP GFS for ASGS nowcasts and forecasts
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
# FIXME: This is a cut-and-paste copy of get_nam_status.pl
# to allow it to be deployed without disturbing that script.
# Refactoring will be required to bring these codes together
# (probably in the offseason).
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
my $backsite = "null";    # ncep ftp site for gfs data
my $backdir = "null";     # dir on ncep ftp site
my @cyclerange;           # if $startcycle was supplied this array will be populated with a range of cycles from startcycle or earliest available to the latest
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

    # replaces the $ftp->ls for files, looks only for gfs.* files
    sub http_ls {
        my $dir = shift;
        $dir =~ s/\/pub\///g;
        my $url         = sprintf( qq{https://ftp.ncep.noaa.gov/%s/ls-l}, $dir );
        my $res         = $ua->get($url);
        my $raw_listing = $res->{content};
        my @files       = ( $raw_listing =~ m/ +(gfs.+)\n/g );
        if ( not @files ) {
            warn "!! No files found via $url\n";
        }
        return @files;
    }

    # replaces the $ftp->ls for directory listings, extracts from the HTML listing
    sub http_dir {
        my $dir = shift;
        $dir =~ s/\/pub\///g;
        my $url         = sprintf( qq{https://ftp.ncep.noaa.gov/%s}, $dir );
        my $res         = $ua->get($url);
        my $raw_listing = $res->{content};
        # Welcome to the perils of parsing HTML, the following match is set up
        # to work on the following 2 examples,
        # 1. <tr><td><a href="00/">00/</a></td><td align="right">31-Oct-2024 03:32
        # 2. <tr><td><a href="gfs.20241027/">gfs.20241027/</a></td><td align="right">27-Oct
        my @dirs        = ( $raw_listing =~ m/href="(gfs\.\d{8}|\d\d)\/"/g );
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
ASGSUtil::setParameter( $jshash_ref, \$backdir,   "siteDir",  "/pub/data/nccf/com/gfs/v16.2");
$jshash_ref->{"siteHost"} = $backsite;
$jshash_ref->{"siteDir"} = $backdir;
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
ASGSUtil::appMessage( "INFO", "Connecting to $backsite:$backdir");
our $dl = 0;   # true if latest status was determined successfully
# open ftp connection
our $ftp = Net::FTP->new($backsite, Debug => 0, Passive => 1, Timeout => 5);
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
# cd to the directory containing the GFS files
my $hcDirSuccess = eval { $ftp->cwd($backdir) };
unless ( $hcDirSuccess ) {
   ASGSUtil::stderrMessage("ERROR", "ftp: Cannot change working directory to '$backdir': " .  $ftp->message);
   exit 1;
}
#
# now go to the ftp site and
# get the list of gfs dates where data is available
# and report latest data available on the site
# directory entries are named e.g., gfs.20220111

local $@;
my @ncepDirs = eval { http_dir($ftp->pwd) }; # gets all the current data dirs, incl. gfs dirs
if ($@) {
   my $msg = ($@ =~ m/timeout/i) ? q{[Net::FTP] Timeout} : $@;
   ASGSUtil::stderrMessage("ERROR", q{ftp: Cannot list NCEP directories: } . $msg);
   exit 1;
}
# e.g. /pub/data/nccf/com/gfs/v16.2/gfs.20220527/00/atmos
my @gfsDirs;
foreach my $dir (@ncepDirs) {
   if ( $dir =~ /gfs.\d+/ ) { # filter out non-gfs dirs
      push(@gfsDirs,$dir);
   }
}
# now sort the GFS dirs from lowest to highest (it appears that ls() does
# not automatically do this for us)
my @sortedGfsDirs = sort { lc($a) cmp lc($b) } @gfsDirs;
# if the $startcycle was provided, remove any directories that
# are before the date of the $startcycle
my $startdate = "null";
if ( $startcycle ne "null" ) {
   $startcycle =~ /(\d{8})/;
   $startdate = $1;
   my $numbefore = 0; # number of directories prior to the startcycle directory
   DIRECTORIES : foreach my $dir (@sortedGfsDirs) {
      $dir =~ /gfs.(\d{8})/;
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
      shift(@sortedGfsDirs);
   }
} else {
   # startcycle was not specified, so find the
   # first date and time that data are available
   $sortedGfsDirs[0] =~ /gfs.(\d{8})/;
   $startdate = $1;
   # change to that directory and see what cycle subdirectories are in there
   $hcDirSuccess = eval { $ftp->cwd("$backdir/$sortedGfsDirs[0]") };
   unless ( $hcDirSuccess ) {
      ASGSUtil::stderrMessage( "ERROR", "ftp: Cannot change working directory to '$backdir/$sortedGfsDirs[0]': " .  $ftp->message);
      exit 1;
   }
   local $@;
   my @earliestGfsCycles = eval { http_dir($ftp->pwd) };
   if ($@) {
     my $msg = ($@) ? q{[HTTP::Tiny] } : $@;
     ASGSUtil::stderrMessage("ERROR", q{ftp: Cannot list "earliest" GFS cycle subdirectories: } . $msg);
     exit 1;
   }

   # now sort the GFS cycle subdirectories from lowest to highest (it appears that ls() does
   # not automatically do this for us)
   my @sortedEarliestGfsCycles = sort { lc($a) cmp lc($b) } @earliestGfsCycles;
   $startcycle = $sortedEarliestGfsCycles[0];
}
# sanity check
my $numSortedGfsDirs = @sortedGfsDirs;
if ( $numSortedGfsDirs == 0 ) {
   ASGSUtil::stderrMessage( "WARNING", "Failed to find any GFS data directories.");
   exit 1;
}
# determine the latest GFS directory that has data in it
# (a new directory may exist and be empty for some period
#  of time, so cannot be counted on to contain the latest data)
my $targetDir = "null";   # latest date directory e.g. gfs.20220527 that is not empty
my $targetCycle = "null"; # latest cycle directory e.g. gfs.20220527/00 that is not empty
my $targetDirFound = 0;   # true if the latest directory that is not empty has been found
my $targetCycleFound = 0; # true if the latest cycle directory that is not empty has been found
my @sortedFiles;          # data files in the most recent nam directory
my $cycletime = "null";   # date and hour latest cycle
my $cycledate = "null";   # date of latest cycle
my $cyclehour = "null";   # hour of latest cycle
LATESTDATEDIR : while ( ! $targetDirFound && ! $targetCycleFound && scalar(@sortedGfsDirs) != 0 ) {
   $targetDir = $sortedGfsDirs[-1];
   #ASGSUtil::stderrMessage("DEBUG", "LATESTDATEDIR: targetDir is $targetDir");
   # determine the most recent date/hour ... this is the latest gfs cycle time
   $targetDir =~ /gfs.(\d+)/;
   $cycledate = $1;
   #ASGSUtil::appMessage("DEBUG", "The cycledate is '$cycledate'.");
   # change to that directory and see if there are cycles in there
   $hcDirSuccess = eval { $ftp->cwd("$backdir/$targetDir") };
   unless ( $hcDirSuccess ) {
      ASGSUtil::stderrMessage("ERROR", "ftp: Cannot change working directory to '$backdir/$targetDir': " . $ftp->message);
      exit 1;
   }

   local $@;
   my @latestCycles = eval { http_dir($ftp->pwd) };
   if ($@) {
     my $msg = ($@) ? $@ : "";
     ASGSUtil::stderrMessage("ERROR", q{HTTP::Tiny: Cannot list latest GFS cycles in '$backdir/$targetDir': } . $msg);
     exit 1;
   }
   # now sort the GFS cycles from lowest to highest (it appears that ls() does
   # not automatically do this for us)
   my @sortedLatestCycles = sort { lc($a) cmp lc($b) } @latestCycles;

   # latestCycles will look like /pub/data/nccf/com/gfs/v16.2/gfs.20220527/00 etc
   LATESTCYCLEDIR: while ( ! $targetCycleFound && scalar(@sortedLatestCycles) != 0 ) {

      $targetCycle = $sortedLatestCycles[-1];
      #ASGSUtil::stderrMessage("DEBUG", "LATESTCYCLEDIR: targetCycle is $targetCycle");
      # change to that cycle directory and see if there are files in there
      $hcDirSuccess = eval { $ftp->cwd("$backdir/$targetDir/$targetCycle/atmos") };
      unless ( $hcDirSuccess ) {
         ASGSUtil::stderrMessage("ERROR", "ftp: Cannot change working directory to '$backdir/$targetDir/$targetCycle/atmos': " . $ftp->message);
         exit 1;
      }

      # looking for files like gfs.t00z.pgrb2.0p25.f000
      local $@;
      my @allFiles = eval { grep /gfs.t\d{2}z.pgrb2b.0p25.f\d{3}$/, http_ls($ftp->pwd) };
      if ($@) {
         my $msg = ($@) ? qq{[HTTP::Tiny] $@} : "";
         ASGSUtil::stderrMessage("ERROR", q{HTTP::Tiny: Cannot list "all" files: } . $msg);
         exit 1;
      }

      if (!@allFiles){
         ASGSUtil::stderrMessage("INFO", "No gfs.tNNz.pgrb2b.0p25.fNN files yet in '$targetDir/$targetCycle/atmos'.");
      } else {
         $targetCycleFound = 1;
         $targetDirFound = 1;
         $cyclehour = $targetCycle;
         # now sort the GFS files from lowest to highest (it appears that ls() does
         # not automatically do this for us)
         @sortedFiles = sort { lc($a) cmp lc($b) } @allFiles;
      }
      if ( ! $targetCycleFound ) {
        pop(@sortedLatestCycles);
      }
   }
   if ( ! $targetDirFound ) {
      pop(@sortedGfsDirs);
   }
}
unless ( $targetDirFound && $targetCycleFound && scalar(@sortedFiles) ) {
   ASGSUtil::stderrMessage( "ERROR", "Could not find any GFS files in any GFS directory in the specified time range.");
   exit 1;
}
#
unless ( $cyclehour ne "null" ) {
   ASGSUtil::stderrMessage( "WARNING", "Could not download the list of GFS files from NCEP.");
   exit 1;
} else {
   $cycletime = $cycledate . $cyclehour;
   #ASGSUtil::appMessage( "DEBUG", "The cycletime is '$cycletime'.");
}
#
# write a JSON file
# that contains all the cycles available between the given starting
# date/time and the latest available cycle (inclusive)
my @cyclesInRange; # between startcycle and the latest
DIRECTORIES : foreach my $dir (@sortedGfsDirs) {
   # cd to the directory containing the GFS directories
   #ASGSUtil::stderrMessage("DEBUG", "DIRECTORIES: dir is $dir");
   my $hcDirSuccess = eval { $ftp->cwd("$backdir/$dir") };
   unless ( $hcDirSuccess ) {
      ASGSUtil::stderrMessage( "ERROR", "ftp: Cannot change working directory to '$backdir/$dir': " .
         $ftp->message);
      exit 1;
   }
   $dir =~ /gfs.(\d+)/;
   my $thisdate = $1;

   # see what cycle directories are available for this date
   local $@;
   my @cycles = eval { http_dir($ftp->pwd) };
   if ($@) {
     my $msg = ($@) ? $@ : "";
     ASGSUtil::stderrMessage("ERROR", q{HTTP::Tiny: Cannot list the GFS cycles in '$backdir/$targetDir': } . $msg);
     exit 1;
   }
   my @sortedCycles = sort { lc($a) cmp lc($b) } @cycles;

   CYCLES: foreach my $cycle (@sortedCycles) {
      #ASGSUtil::stderrMessage("DEBUG", "CYCLES: cycle is $cycle");
      # cd to the directory containing the GFS files
      my $hcDirSuccess = eval { $ftp->cwd("$backdir/$dir/$cycle/atmos") };
      unless ( $hcDirSuccess ) {
         ASGSUtil::stderrMessage( "ERROR", "ftp: Cannot change working directory to '$backdir/$dir/$cycle/atmos': " .
            $ftp->message);
         exit 1;
      }
      my $thishour = $cycle;
      my $thiscycle = $thisdate . $thishour;
      if ( $thiscycle >= $startcycle && $thiscycle <= $cycletime ) {
         push(@cyclesInRange,$thiscycle);
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

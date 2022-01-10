#!/usr/bin/env perl
#--------------------------------------------------------------
# get_nam_status.pl: determines the latest available data
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
# ref: http://www.cpc.ncep.noaa.gov/products/wesley/fast_downloading_grib.html
#--------------------------------------------------------------
$^W++;
use strict;
use Net::FTP;
use Getopt::Long;
use Date::Calc;
use Cwd;
#
our $rundir;   # directory where the ASGS is running
my $backsite; # ncep ftp site for nam data
my $backdir;  # dir on ncep ftp site
my @altnamdirs; # alternate directories to look in for NAM data
#
my $date;     # date (UTC) corresponding to current ADCIRC time
my $hour;     # hour (UTC) corresponding to current ADCIRC time
our $this = "get_nam_status.pl";
#
our @grib_fields = ( "PRMSL","UGRD:10 m above ground","VGRD:10 m above ground" );
#
GetOptions(
           "rundir=s" => \$rundir,
           "backsite=s" => \$backsite,
           "backdir=s" => \$backdir,
           "altnamdirs=s" => \@altnamdirs,
          );
#
# create a hash of properties from run.properties
our %properties;
our $have_properties = 1;
# open the run.properties file : it will be in $rundir on a nowcast
my $proppath = $rundir; # we don't have the latest data yet, so we don't know what cycle we are on
unless (open(RUNPROP,"<$proppath/run.properties")) {
   &stderrMessage("WARNING","Failed to open $proppath/run.properties: $!.");
   &appMessage("WARNING","Failed to open $proppath/run.properties: $!.");
   $have_properties = 0;
} else {
   &appMessage("INFO","Opened $proppath/run.properties.");
   while (<RUNPROP>) {
      my @fields = split ':',$_, 2 ;
      # strip leading and trailing spaces and tabs
      $fields[0] =~ s/^\s|\s+$//g ;
      $fields[1] =~ s/^\s|\s+$//g ;
      $properties{$fields[0]} = $fields[1];
   }
   close(RUNPROP);
   &appMessage("INFO","Closed $proppath/run.properties.");
}
#
&appMessage("DEBUG","hstime is $hstime");
&appMessage("DEBUG","Connecting to $backsite:$backdir");
our $dl = 0;   # true if latest status was determined successfully
# open ftp connection
our $ftp = Net::FTP->new($backsite, Debug => 0, Passive => 1);
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
#
# everything below is designed to determine what is the
# latest cycle available from NCEP (or locally?)
#
# if alternate (local) directories for NAM data were supplied, then remove the
# commas from these directories
if ( @altnamdirs ) {
   @altnamdirs = split(/,/,join(',',@altnamdirs));
}
#
# now go to the ftp site and 
# get the list of nam dates where data is available
# and report latest data available on the site
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
# getting the directory listing with NAM data directories in it;
# if so, it is generally harmles because the asgs will just respawn get_nam_status.pl
# (sanity check)
my $numSortedNamDirs = @sortedNamDirs;
if ( $numSortedNamDirs == 0 ) {
   stderrMessage("INFO","Failed to find any NAM data directories. This script will be respawned.");
   printf STDOUT $dl;
   exit;
}
# take the last one; this is the latest
my $targetDir = $sortedNamDirs[-1];
#
# determine the most recent date/hour ... this is the latest nam cycle time
$targetDir =~ /nam.(\d+)/;
my $cycledate = $1;
&appMessage("DEBUG","The cycledate is '$cycledate'.");
#
$hcDirSuccess = $ftp->cwd($targetDir);
unless ( $hcDirSuccess ) {
   stderrMessage("ERROR","ftp: Cannot change working directory to '$targetDir': " . $ftp->message);
   printf STDOUT $dl;
   exit;
}
my $cyclehour;
#my @allFiles = $ftp->ls();
my @allFiles = grep /awip1200.tm00/, $ftp->ls();
if (!@allFiles){
   #die "no awip1200 files yet in $targetDirs[-1]\n";
   stderrMessage("ERROR","No awip1200.tm00 files yet in $targetDir.");
}
# now sort the NAM files from lowest to highest (it appears that ls() does
# not automatically do this for us)
my @sortedFiles = sort { lc($a) cmp lc($b) } @allFiles;
#
my $cycletime;
TODAYSFILES : foreach my $file (@sortedFiles) {
   if ( $file =~ /nam.t(\d+)z.awip1200.tm00.grib2/ ) {
      $cyclehour = $1;
   }
}
unless (defined $cyclehour ) {
   stderrMessage("WARNING","Could not download the list of NAM files from NCEP.");
   printf STDOUT $dl;
   exit;
} else {
   stderrMessage("DEBUG","The cyclehour is '$cyclehour'.");
   $cycletime = $cycledate . $cyclehour;
}
stderrMessage("DEBUG","The cycletime is '$cycletime'.");
printf STDOUT $dl;
exit;
#
# write a log message to stderr
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: $this: $message\n";
}
#
# write a log message to a log file dedicated to this script (typically debug messages)
sub appMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   #
   # open an application log file
   unless ( open(APPLOGFILE,">>$rundir/$this.log") ) {
      &stderrMessage("ERROR","Could not open $rundir/$this.log for appending: $!.");
   }
   printf APPLOGFILE "$theTime $level: $this: $message\n";
   close(APPLOGFILE);
}

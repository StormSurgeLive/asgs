#!/usr/bin/env perl
#--------------------------------------------------------------
# get_nam_data.pl: downloads background meteorology data from NCEP
# for ASGS nowcasts and forecasts
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
#--------------------------------------------------------------
# ref: http://www.cpc.ncep.noaa.gov/products/wesley/fast_downloading_grib.html
#--------------------------------------------------------------
# sample lines to test this script:
#
## supply starting and ending nowcast cycle:
#
# perl get_nam_data.pl \
#                 --startcycle 2005082900      \
#                 --finishcycle 2005082906     \
#                 --stage nowcast              \
#
# -- or --
#
## supply JSON file with a list of nowcast cycles to download:
#
# perl get_nam_data.pl \
#                 --selectfile select_nam_nowcast.pl.json
#                 --stage nowcast
#
# == or ==
#
## ask a specific forecast cycle:
#
# perl get_nam_data.pl \
#                 --stage forecast \
#                 --startcycle 2005082900      
#--------------------------------------------------------------
$^W++;
use strict;
use Net::FTP;
use JSON::PP;
use Getopt::Long;
use Date::Calc;
use File::Copy 'move';
use File::Basename;
use File::Path 'make_path';
use Cwd;
our $startcycle = "null"; # most recent cycle for which a nowcast was completed
my $finishcycle = "null"; # nowcast end (meaningless in the forecast stage)
my $selectfile = "null";  # array of cycles to download
my $stage = "null";       # nowcast | forecast
our $forecastcycle = "00,06,12,18";   # nam cycles to run a forecast
my $backsite = "ftp.ncep.noaa.gov";   # ncep ftp site for nam data
my $backdir = "/pub/data/nccf/com/nam/prod";    # dir on ncep ftp site
my $localbasedir = cwd();    # main directory to download NAM data to
our $scriptdir = dirname(__FILE__); # directory where this script is installed
our $forecastlength = 84; # standard length of a NAM forecast
our $max_retries = 20; # max number of times to attempt download of forecast file
our $num_retries = 0;
our $had_enough = 0;
my @nowcasts_downloaded;  # list of nowcast files that were successfully downloaded
#
our $this = "get_nam_data.pl";
my $ncepcycles = "forcing.nam.ncep.cyclelist";
#
our @grib_fields = ( "PRMSL","UGRD:10 m above ground","VGRD:10 m above ground" );
#
GetOptions(
           "startcycle=s" => \$startcycle,
           "finishcycle=s" => \$finishcycle,  
           "selectfile=s" => \$selectfile,
           "stage=s" => \$stage,
           "forecastcycle=s" => \$forecastcycle,
           "backsite=s" => \$backsite,
           "backdir=s" => \$backdir
);
# load the cycle list from the selectfile if it was specified
my @cyclelist;
if ( $selectfile ne "null" ) {
   unless ( open(SF,"<$selectfile") ) {
      stderrMessage("ERROR","Could not open '$selectfile' for writing: $!.");
      die;
   }
   # slurp the file contents into a scalar variable
   my $file_content = do { local $/; <SF> };
   close(SF);
   # deserialize JSON
   my $ref = JSON::PP->new->decode($file_content);
   my %cyclehash = %$ref;
   # grab the list of cycles out of the hash
   my $cyclelistref = $cyclehash{$ncepcycles};
   @cyclelist = @$cyclelistref;
}
# establish connection to the ftp site
&appMessage("DEBUG","Connecting to $backsite:$backdir");
our $dl = 0;   # true if we were able to download the file(s) successfully
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
#
# if this is a forecast, jump to the sub to get the
# forecast data for this cycle
# 
#    J U M P   T O   F O R E C A S T
if ( $stage eq "forecast" ) {
   if ( $selectfile ne "null" ) {
      # this is the cycle we nowcasted to 
      # ... we want our forecast to start here
      $startcycle = $cyclelist[-1];
   } else {
      # make sure that the $startcycle was specified,
      # or else we don't know what forecast cycle
      # to download
      if ( $startcycle eq "null" ) {
         stderrMessage("ERROR","The --startcycle argument was not specified for the forecast.");
         die;
      }
   }
   &getForecastData();
}
#
#    N O W C A S T    D A T A 
#
# create or load the list of cycletimes that are required

if ( $startcycle ne "null" && $finishcycle ne "null") {
   push(@cyclelist,$startcycle);
   my $thiscycle = $startcycle;
   while ( $thiscycle < $finishcycle ) {
      # now add six hours to determine the next cycle time
      $thiscycle =~ /(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
      my $yn = $1;
      my $mn = $2;
      my $dn = $3;
      my $hn = $4;
      my ($ty, $tm, $td, $th, $tmin, $ts); # targeted nowcast time
      # now add 6 hours
      ($ty,$tm,$td,$th,$tmin,$ts) =
          Date::Calc::Add_Delta_DHMS($yn,$mn,$dn,$hn,0,0,0,6,0,0);
      # form the date and hour of the next nowcast data needed
      my $date_needed = sprintf("%4d%02d%02d",$ty ,$tm, $td);
      my $hour_needed = sprintf("%02d",$th);
      $thiscycle = $date_needed.$hour_needed;
      push(@cyclelist,$thiscycle);
   }
} else {
   # this is the cycle we are nowcasting from
   $startcycle = $cyclelist[0];      
   # this is the cycle we are nowcasting to
   $finishcycle = $cyclelist[-1];   
}
unless ( @cyclelist ) {
   stderrMessage("ERROR","Could not create list of NAM cycles to download. Arguments --startcycle '$startcycle' and --finishcycle $finishcycle were provided.");   
   die;
} 
# loop over cycles, grabbing all files relevant to a nowcast
foreach my $cycle (@cyclelist) {
   $cycle =~ /(\d{8})/;
   my $dirDate = $1;
   my $remoteDir = "nam.$dirDate";
   stderrMessage("INFO","Downloading from directory '$backdir/$remoteDir'.");
   my $hcDirSuccess = $ftp->cwd("$backdir/$remoteDir");
   unless ( $hcDirSuccess ) {
      stderrMessage("ERROR",
         "ftp: Cannot change working directory to '$backdir/$remoteDir': " . $ftp->message);
      die;
   }
   # create the directory local to this ASGS instance where the
   # files will be stored (if it does not already exist)
   # 
   # the NAM nowcast file names do not indicate the date, only the hour
   # that the forecast was issued (and the hour from the issue time 
   # that the forecast is valid, e.g., 
   # /pub/data/nccf/com/nam/prod/nam.20220123/nam.t18z.awip1200.tm00.grib2
   # NAMtoOWIRamp.pl is hardcoded to look in e.g. the erl.220123 dir for the 20220123 cycles
   my $localDir = $localbasedir."/".$finishcycle."/erl.".substr($dirDate,2);
   unless ( -e $localDir ) {
      unless ( make_path($localDir) ) {
         stderrMessage("ERROR","Could not make the directory '$localDir': $!");
         die;
      }
   }
   # form the filename of the file to be downloaded
   $cycle =~ /\d{8}(\d{2})/;
   my $hourString = $1;
   my $fbase = "nam.t".$hourString."z.awip1200.tm00";
   my $f = $fbase . ".grib2";
   my $idxfile = $f . ".idx";
   #my $success = $ftp->get($f,$localDir."/".$f);
   my $err = &partialGribDownload($dirDate, $f, $idxfile, $localDir);
   if ( $err == 0 ) {
      stderrMessage("INFO","Download complete.");
      push(@nowcasts_downloaded,$dirDate.$hourString);
      #stderrMessage("DEBUG","Now have data for $dirDate$hourString.");
      # perform a smoke test on the file we found to check that it is
      # not corrupted (not a definitive test but better than nothing)
      if ( `$scriptdir/wgrib2 $localDir/$f -match PRMSL -inv - -text /dev/null` =~ /PRMSL/ ) {
         $dl++;
      } else {
         stderrMessage("ERROR","The file '$localDir/$f' appears to be corrupted and will not be used.");
         unless ( move($localDir/$f,"$localDir/$f.error") ) {
            stderrMessage("ERROR","Could not rename the file '$localDir/$f' to $localDir/$f.error: $!");
         }
      }
   } else {
      stderrMessage("ERROR","Failed to download '$f'.");
   }
}
# if we found at least two files, we assume have enough for the next advisory
if ( $dl >= 2 ) {
   printf STDOUT $finishcycle;
} else {
   printf STDOUT "0";
   die;
}
1;
#
#-----------------------------------------------------------
# F O R E C A S T
#-----------------------------------------------------------
# now download all the files that are relevant to a forecast
sub getForecastData() {
   my @targetFiles="";
   # create a local directory for the data if it does
   # not already exist
   my $localDir = $localbasedir."/".$startcycle."/erl.".substr($startcycle,2,6);
   unless ( -e $localDir ) {
      unless ( make_path($localDir) ) {
         stderrMessage("ERROR","Could not make the directory '$localDir': $!");
         die;
      }
   }
   # write a properties file to document when the forecast starts and ends
   unless ( open(FP,">$localbasedir/$startcycle/forecast.properties") ) {
      stderrMessage("ERROR","Could not open '$startcycle/forecast.properties' for writing: $!.");
      die;
   }
   printf FP "forecastValidStart : $startcycle" . "0000\n";
   #
   # download the forecast files
   $startcycle =~ /(\d{8})(\d{2})/;
   my $dirDate = $1;
   my $cyclehour = $2;
   my $remoteDir = "nam.$dirDate";
   appMessage("INFO","Downloading from directory '$backdir/$remoteDir'.");
   my $hcDirSuccess = $ftp->cwd("$backdir/$remoteDir");
   unless ( $hcDirSuccess ) {
      stderrMessage("ERROR",
         "ftp: Cannot change working directory to '$backdir/$remoteDir': " . $ftp->message);
      die;
   }
   # forecast files are the list of files to retrieve
   for (my $i=0; $i<=$forecastlength; $i+=3 ) {
      my $hourString = sprintf("%02d",$cyclehour);
      my $f = "nam.t".$hourString."z.awip12".sprintf("%02d",$i).".tm00.grib2";
      # sometimes an error occurs in Net::FTP causing this script to bomb out;
      # the asgs will retry, but we don't want it to re-download stuff that it
      # already has
      if ( -e $localDir."/".$f ) {
         # perform a smoke test on the file we found to check that it is
         # not corrupted (not a definitive test but better than nothing)
         if ( `$scriptdir/wgrib2 $localDir/$f -match PRMSL -inv - -text /dev/null` =~ /PRMSL/ ) {
            stderrMessage("INFO","'$f' has already been downloaded to '$localDir'.");
            $dl++;
            next;
         } else {
            stderrMessage("INFO","The file '$localDir/$f' appears to be corrupted and will not be used.");
            unless ( move($localDir/$f,"$localDir/$f.error") ) {
               stderrMessage("ERROR","Could not rename the file '$localDir/$f' to $localDir/$f.error: $!");
            }
         }
      }
      stderrMessage("INFO","Downloading '$f' to '$localDir'.");
      my $success = 0;
      $num_retries = 1;
      my $idxfile = $f . ".idx";
      while ( $success == 0 && $num_retries < $max_retries ) {
         my $stat = &partialGribDownload($dirDate, $f, $idxfile, $localDir);
         # my $stat = $ftp->get($f,$localDir."/".$f);
         if ( $stat == 0 ) {
            $dl++;
            $success = 1;
            stderrMessage("INFO","Downloaded in $num_retries attempt(s).");
         } else {                       
            stderrMessage("INFO","ftp: Get '$f' failed: " . $ftp->message);
            $num_retries++;
            #stderrMessage("DEBUG","num_retries is $num_retries");
            sleep 60;
         }
      }
      if ( $num_retries >= $max_retries ) {
         $had_enough = 1;
         stderrMessage("INFO","Retried download more than $max_retries times. Giving up on downloading $f.");
         last;  # if we tried 10 times and couldn't get it, the files are
                # probably not there at all, so don't spend time trying to
                # get the rest of them
      }
   }
   # determine the end date of the forecast for the forecast.properties file
   $startcycle =~ /(\d\d\d\d)(\d\d)(\d\d)/;
   my $cdy = $1;
   my $cdm = $2;
   my $cdd = $3;
   my $cmin = 0;
   my $cs = 0;
   my ($ey,$em,$ed,$eh,$emin,$es) =
         Date::Calc::Add_Delta_DHMS($cdy,$cdm,$cdd,$cyclehour,$cmin,$cs,0,$dl*3,0,0);
                        #  yyyy mm  dd  hh
   my $end_date = sprintf("%04d%02d%02d%02d",$ey,$em,$ed,$eh);
   printf FP "forecastValidEnd : $end_date" . "0000\n";
   close(FP);
   #
   # if we found at least two files, we assume have enough for
   # the next advisory
   if ( $dl >= 2 ) {
      # write out the datetime when the forecast ends
      printf STDOUT $end_date;
      exit;
   } else {
      printf STDOUT "0";
      die;
   }
}
#
# perform partial grib download using curl
# only gets the U, V, P at mean sea level
sub partialGribDownload () {
   my $dirDate = shift;
   my $f = shift;
   my $idxfile = shift;
   my $localDir = shift;
   #--------------------------------------------------------
   #    G R I B   I N V E N T O R Y  A N D   R A N G E S
   #--------------------------------------------------------
   # FIXME: undo this hardcode for downloading files with curl using
   # https://nomads rather than $backsite
   my $idx = "https://nomads.ncep.noaa.gov$backdir/nam.$dirDate/$idxfile";
   # jgfdebug: save a local copy of the inventory file
   appMessage("DEBUG","Downloading '$idx' with the command 'curl -f -s $idx -o $localDir/$idxfile'.");
   my $err=system("curl -f -s $idx -o $localDir/$idxfile");
   if ( $err != 0 ) {
      stderrMessage("INFO","curl: Get '$idx' failed.");
      unlink("$localDir/$idxfile");
      return $err;
   }
   # download directly into list
   #stderrMessage("INFO","Downloading '$idx' with the command 'curl -f -s $idx'.");
   #my @gribInventoryLines = `curl -f -s $idx`; # the grib inventory file from the ftp site
   my @rangeLines;    # inventory with computed ranges
   my $last = 0;      # number of immediately preceding lines with same starting byte index
   my $lastnum = -1;  # starting byte range of previous line (or lines if there are repeats)
   my @old_lines;     # contiguous lines in inventory with same starting byte
   my $has_range = 0; # set to 1 if the inventory already has a range field
   #
   # open index file for this time period
   appMessage("INFO","Parsing '$idx' to determine byte ranges of U, V, and P.");
   unless ( open(GRIBINVENTORY,"<$localDir/$idxfile") ) {
      stderrMessage("ERROR","Could not open '$localDir/$idxfile' for appending: $!.");
      return 1;
   }
   while(<GRIBINVENTORY>) {
      chomp($_);
      #stderrMessage("INFO","$li");
      # check to see if this is grib2 inventory that already has a range field
      # if so, don't need to calculate the range
      if ($_ =~ /:range=/) {
         $has_range = 1;
         push(@rangeLines,"$_\n");
      } else {
         # grib1/2 inventory,
         #    c o m p u t e   r a n g e   f i e l d
         # inventory line format without range field e.g.:
         # 1:0:d=2021030106:PRMSL:mean sea level:anl:
         # 2:233889:d=2021030106:PRES:1 hybrid level:anl:
         # 3:476054:d=2021030106:RWMR:1 hybrid level:anl:
         my ($f1,$startingByteIndex,$rest) = split(/:/,$_,3);
         # see if the starting byte index is different on this line
         # compared to the previous one (and this is not the first line)
         if ($lastnum != $startingByteIndex && $last != 0) {
            # compute the end of the byte range for the previous line
            my $previousEndingByteIndex = $startingByteIndex - 1;
            # add this byte range to all the old_lines we've stored due to their
            # repeated starting byte index
            foreach my $ol (@old_lines) {
               $ol = "$ol:range=$lastnum-$previousEndingByteIndex\n";
            }
            # now add these old lines to the list of lines with our newly computed ranges
            @rangeLines = (@rangeLines,@old_lines);
            @old_lines = ();
            $last = 1;
         }  else {
            $last++;
         }
         push(@old_lines,$_);
         $lastnum = $startingByteIndex;
      }
   }
   close(GRIBINVENTORY);
   if ( $has_range == 0 ) {
      foreach my $ol (@old_lines) {
         $ol = "$ol:range=$lastnum\n";
      }
      @rangeLines = (@rangeLines,@old_lines);
   }
   #   r a n g e   f i e l d s   h a v e   n o w   b e e n   c o m p u t e d   o r   p r o v i d e d
   #
   # download and concatenate grib2 byte ranges
   # jgfdebug
   #foreach my $li (@rangeLines) {
   #   print $li;
   #}
   # now iterate through them and collect the relevant byte ranges
   my @ranges;      # byte ranges to download
   foreach my $li (@rangeLines) {
      my $match = 0;
      # check to see if the line matches one of the fields of interest
      foreach my $gf (@grib_fields) {
         if ( $li =~ /$gf/ ) {
            #print "$li matches $gf\n";
            # want to download this field
            chomp($li);
            $li =~ /:range=([0-9]*)-([0-9]*)/;
            my $newrange = $1 . "-" . $2;
            # don't request the same range twice in a row (e.g., U and V will have the same range)
            unless ( @ranges > 0 && $newrange eq $ranges[-1] ) {
               push(@ranges,$newrange);
            }
         }
      }
   }
   # now join selected ranges and actually download the specified data
   my $range=join(",",@ranges);
   appMessage("INFO","Downloading '$f' to '$localDir' with curl -f -s -r \"$range\" https://nomads.ncep.noaa.gov$backdir/nam.$dirDate/$f -o $localDir/$f.");
   my $err=system("curl -f -s -r \"$range\" https://nomads.ncep.noaa.gov$backdir/nam.$dirDate/$f -o $localDir/$f");
   if ( $err == 0 ) {
      #stderrMessage("INFO","Download complete.");
      return 0;
      #stderrMessage("DEBUG","Now have data for $dirDate$hourString.");
   } else {
      stderrMessage("INFO","curl: Get '$f' failed.");
      unlink("$localDir/$f");
      return 1;
   }
}
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
   unless ( open(APPLOGFILE,">>$this.log") ) {
      &stderrMessage("ERROR","Could not open $this.log for appending: $!.");
   }
   printf APPLOGFILE "$theTime $level: $this: $message\n";
   close(APPLOGFILE);
}

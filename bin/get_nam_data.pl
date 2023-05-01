#!/usr/bin/env perl
#--------------------------------------------------------------
# get_nam_data.pl: downloads background meteorology data from NCEP
# for ASGS nowcasts and forecasts
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
#--------------------------------------------------------------
# ref: http://www.cpc.ncep.noaa.gov/products/wesley/fast_downloading_grib.html
#--------------------------------------------------------------
use strict;
use warnings;
use Net::FTP;
use JSON::PP;
use Getopt::Long;
use Date::Calc;
use File::Basename;
use File::Copy 'move';
use File::Copy 'copy';
use File::Path 'make_path';
use Cwd;
use ASGSUtil;
my $startcycle = "null"; # most recent cycle for which a nowcast was completed
my $finishcycle = "null"; # nowcast end (meaningless in the forecast stage)
my $selectfile = "null";  # array of cycles to download
my $stage = "null";       # nowcast | forecast
my $max_retries = 20; # max number of times to attempt download of forecast file
my %jsonhash;   # for deserializing and serializing json
my $backsite = "null";   # ncep ftp site for nam data
my $backdir = "null";    # dir on ncep ftp site
my $namdatadir = "null";    # main directory to download NAM data to
my $scriptdir = "null"; # directory where this script is installed
my $forecastlength = 84; # standard length of a NAM forecast
my $num_retries = 0;
my $had_enough = 0;
my @nowcasts_downloaded;  # nowcast dates and times that were successfully downloaded
my @files_downloaded; # list of grib2 files that were sucessfully downloaded
my @files_found; # list of grib2 files that were already locally available
my $ncepcycles = "forcing.nam.ncep.cyclelist";
my @grib_fields = ( "PRMSL","UGRD:10 m above ground","VGRD:10 m above ground" );
my @cyclelist;
#
GetOptions(
           "stage=s" => \$stage,
           "startcycle=s" => \$startcycle,
           "finishcycle=s" => \$finishcycle,
           "selectfile=s" => \$selectfile,
           "backsite=s" => \$backsite,
           "backdir=s" => \$backdir
);

# slurp STDIN JSON request into a scalar variable
my $file_content = do { local $/; <> };
# deserialize JSON
my $jshash_ref = JSON::PP->new->decode($file_content);
# grab the list of cycles out of the hash
my $cyclelistref = $jshash_ref->{"cyclelist"};
my @json_cyclelist = @$cyclelistref;

# grab config info and use it if it was not
# already provided on the command line
# also set reasonable defaults
ASGSUtil::setParameter( $jshash_ref, \$backsite,
                        "siteHost", "ftp.ncep.noaa.gov");
ASGSUtil::setParameter( $jshash_ref, \$backdir,
                        "siteDir", "/pub/data/nccf/com/nam/prod");
ASGSUtil::setParameter( $jshash_ref, \$stage,
                        "stage", "null");
ASGSUtil::setParameter( $jshash_ref, \$namdatadir,
                        "localDataDir", cwd() );
ASGSUtil::setParameter( $jshash_ref, \$scriptdir,
                        "scriptDir", dirname(__FILE__) );
#ASGSUtil::writeJSON($jshash_ref);

# the startcycle is a required parameter; if
# the finishcycle is not given, and this is
# a nowcast, this script will necessarily fail;
# if a start and end cycle are provided, but they are
# more than 6 hours apart, the cycles
# in between will be filled in, assuming they are 6
# hours apart
if ( $startcycle eq "null" ) {
   unless ( defined $json_cyclelist[0] ) {
      ASGSUtil::stderrMessage(
                "ERROR",
                "The cycle list was not specified.");
      die;
   }
}
#
#  N O W C A S T   D A T A   F R O M   F I L E S Y S T E M
#
if ( $backsite eq "filesystem" ) {
   ASGSUtil::appMessage( "INFO", "Copying nowcast grib2 files found in $backdir.");
   # check to see if the Operator-supplied directory exists
   if ( -d $backdir ) {
      ASGSUtil::appMessage( "INFO", "The directory $backdir was found.");
   } else {
      ASGSUtil::appMessage( "ERROR", "The directory $backdir was not found.");
      die;
   }
   foreach my $cycle (@json_cyclelist) {
      $cycle =~ /(\d{2})(\d{6})(\d{2})/; # 20 230429 00
      ASGSUtil::appMessage( "DEBUG", "$cycle $1 $2 $3");
      # e.g.:
      # $backdir/erl.220123/nam.t18z.awip1200.tm00.grib2
      my $file = "$backdir/erl.$2/nam.t$3z.awip1200.tm00.grib2";
      if ( -e $file ) {
         ASGSUtil::appMessage( "INFO", "The file $file was found.");
         if ( ! -d "$namdatadir/erl.$2" ) {
            make_path("$namdatadir/erl.$2")
               || ASGSUtil::appMessage("ERROR", "Could not create the path $namdatadir/erl.$2 : $!")
               && die;
         }
         copy($file,"$namdatadir/erl.$2/nam.t$3z.awip1200.tm00.grib2")
            || ASGSUtil::appMessage("ERROR", "Could not copy grib2 files: $!")
            && die;
      } else {
         ASGSUtil::appMessage( "ERROR", "The file $file was not found.");
         die;
      }
   }
   # add the parameters and the cycle list to the hash
   $jshash_ref->{"status"} = basename($0).".json";
   ASGSUtil::timestampJSON($jshash_ref);
   print JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
   exit;   # exit successfully
}
#
#  N O W C A S T   D A T A   F R O M   N C E P
#
# establish connection to the ftp site
ASGSUtil::appMessage(
          "DEBUG",
          "Connecting to $backsite:$backdir");
my $dl = 0;   # number of files successfully downloaded
# open ftp connection
my $ftp = Net::FTP->new($backsite, Debug => 0, Passive => 1);
unless ( defined $ftp ) {
   ASGSUtil::stderrMessage(
             "ERROR",
             "ftp: Cannot connect to $backsite: $@");
   die;
}
my $ftpLoginSuccess = $ftp->login("anonymous",'-anonymous@');
unless ( $ftpLoginSuccess ) {
   ASGSUtil::stderrMessage(
             "ERROR",
             "ftp: Cannot login: " . $ftp->message);
   die;
}
# switch to binary mode
$ftp->binary();
#
#    N O W C A S T    D A T A
#
# the Operator can provide a date range on the command
# line, and we can fill in the intervening cycles here
if ( $stage eq "NOWCAST" ) {
   # this is the cycle we are nowcasting from
   if ( $startcycle eq "null" ) {
      $startcycle = $json_cyclelist[0];
   }
   # this is the cycle we are nowcasting to
   if ( $finishcycle eq "null" ) {
      if ( defined $json_cyclelist[-1] ) {
         $finishcycle = $json_cyclelist[-1];
      } else {
         ASGSUtil::stderrMessage(
                   "ERROR",
                   "This is a nowcast but the finishcycle was not specified.");
         die;
      }
   }
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
   }
   unless ( @cyclelist ) {
      ASGSUtil::stderrMessage(
                "ERROR",
                "Could not create list of NAM cycles to download. " .
                "Arguments --startcycle '$startcycle' and " .
                "--finishcycle $finishcycle were provided.");
      die;
   }
   # loop over cycles, grabbing all files relevant to a nowcast
   foreach my $cycle (@cyclelist) {
      $cycle =~ /(\d{8})/;
      my $dirDate = $1;
      my $remoteDir = "nam.$dirDate";
      ASGSUtil::stderrMessage(
                "INFO",
                "Downloading from directory '$backdir/$remoteDir'.");
      my $hcDirSuccess = $ftp->cwd("$backdir/$remoteDir");
      unless ( $hcDirSuccess ) {
         ASGSUtil::stderrMessage(
                   "ERROR",
                   "ftp: Cannot change working directory to '$backdir/$remoteDir': " .
                   $ftp->message);
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
      my $localDir = $namdatadir."/erl.".substr($dirDate,2);
      unless ( -e $localDir ) {
         unless ( make_path($localDir) ) {
            ASGSUtil::stderrMessage(
                      "ERROR",
                      "Could not make the directory '$localDir': $!");
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
      # don't want to re-download stuff
      if ( -e $localDir."/".$f ) {
         # perform a smoke test on the file we found to check that it is
         # not corrupted (not a definitive test but better than nothing)
         if ( `$scriptdir/bin/wgrib2 $localDir/$f -match PRMSL -inv - -text /dev/null` =~ /PRMSL/ ) {
            ASGSUtil::stderrMessage(
                      "INFO",
                      "'$f' has already been downloaded to '$localDir'.");
            $dl++;
            push(@files_found,"$localDir/$f");
            next;
         } else {
            ASGSUtil::stderrMessage(
                      "INFO",
                      "The file '$localDir/$f' appears to be corrupted and will not be used.");
            unless ( move($localDir/$f,"$localDir/$f.error") ) {
               ASGSUtil::stderrMessage(
                         "ERROR",
                         "Could not rename the file '$localDir/$f' " .
                         " to $localDir/$f.error: $!");
               die;
            }
         }
      }
      my $err = partialGribDownload($dirDate, $f, $idxfile, $localDir);
      if ( $err == 0 ) {
         ASGSUtil::stderrMessage(
                   "INFO",
                   "Download complete.");
         push(@nowcasts_downloaded,$dirDate.$hourString);
         #ASGSUtil::stderrMessage("DEBUG","Now have data for $dirDate$hourString.");
         # perform a smoke test on the file we found to check that it is
         # not corrupted (not a definitive test but better than nothing)
         if ( `$scriptdir/bin/wgrib2 $localDir/$f -match PRMSL -inv - -text /dev/null` =~ /PRMSL/ ) {
            $dl++;
            push(@files_downloaded,"$localDir/$f");
         } else {
            ASGSUtil::stderrMessage(
                      "ERROR",
                      "The file '$localDir/$f' appears to be corrupted and will not be used.");
            unless ( move($localDir/$f,"$localDir/$f.error") ) {
               ASGSUtil::stderrMessage(
                         "ERROR",
                         "Could not rename the file '$localDir/$f' to $localDir/$f.error: $!");
            }
         }
      } else {
         ASGSUtil::stderrMessage(
                   "ERROR",
                   "Failed to download '$f'.");
      }
   }
   # if we found at least two files, we assume have enough for the next advisory
   if ( $dl >= 2 ) {
      $jshash_ref->{"get"} = basename($0).".json";
      $jshash_ref->{"filesDownloaded"} = \@files_downloaded;
      $jshash_ref->{"filesFromCache"} = \@files_found;
      # write json response to file
      #ASGSUtil::writeJSON($jshash_ref);
      ASGSUtil::timestampJSON($jshash_ref);
      # write json response to STDOUT
      print JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
   } else {
      die;
   }
}
#
#-----------------------------------------------------------
# F O R E C A S T
#-----------------------------------------------------------
# download all the files that are relevant to a forecast
if ( $stage eq "FORECAST" ) {
   # this is the cycle we are forecasting
   if ( $startcycle eq "null" ) {
      $startcycle = $json_cyclelist[-1];
   }
   my @targetFiles="";
   # create a local directory for the data if it does
   # not already exist ... in the forecast context,
   # $localbasedir is in the existing advisory directory
   my $localDir = $namdatadir."/erl.".substr($startcycle,2,6);
   unless ( -e $localDir ) {
      unless ( make_path($localDir) ) {
         ASGSUtil::stderrMessage(
                   "ERROR",
                   "Could not make the directory '$localDir': $!");
         die;
      }
   }
   # write a properties file to document when the forecast starts and ends
   unless ( open(FP,">forecast.properties") ) {
      ASGSUtil::stderrMessage(
                "ERROR",
                "Could not open 'forecast.properties' for writing: $!.");
      die;
   }
   printf FP "forecastValidStart : $startcycle" . "0000\n";
   #
   # download the forecast files
   $startcycle =~ /(\d{8})(\d{2})/;
   my $dirDate = $1;
   my $cyclehour = $2;
   my $remoteDir = "nam.$dirDate";
   ASGSUtil::appMessage(
             "INFO",
             "Downloading from directory '$backdir/$remoteDir'.");
   my $hcDirSuccess = $ftp->cwd("$backdir/$remoteDir");
   unless ( $hcDirSuccess ) {
      ASGSUtil::stderrMessage(
                "ERROR",
                "ftp: Cannot change working directory to '$backdir/$remoteDir': " .
                $ftp->message);
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
         if ( `$scriptdir/bin/wgrib2 $localDir/$f -match PRMSL -inv - -text /dev/null` =~ /PRMSL/ ) {
            ASGSUtil::stderrMessage(
                      "INFO",
                      "'$f' has already been downloaded to '$localDir'.");
            push(@files_found,"$localDir/$f");
            $dl++;
            next;
         } else {
            ASGSUtil::stderrMessage(
                      "INFO",
                      "The file '$localDir/$f' appears to be corrupted and will not be used.");
            unless ( move($localDir/$f,"$localDir/$f.error") ) {
               ASGSUtil::stderrMessage(
                         "ERROR",
                         "Could not rename the file '$localDir/$f' to $localDir/$f.error: $!");
               die;
            }
         }
      }
      ASGSUtil::stderrMessage(
                "INFO",
                "Downloading '$f' to '$localDir'.");
      my $success = 0;
      $num_retries = 1;
      my $idxfile = $f . ".idx";
      while ( $success == 0 && $num_retries < $max_retries ) {
         my $stat = partialGribDownload($dirDate, $f, $idxfile, $localDir);
         # my $stat = $ftp->get($f,$localDir."/".$f);
         if ( $stat == 0 ) {
            push(@files_downloaded,"$localDir/$f");
            $dl++;
            $success = 1;
            ASGSUtil::stderrMessage(
                      "INFO",
                      "Downloaded in $num_retries attempt(s).");
         } else {
            ASGSUtil::stderrMessage(
                      "INFO",
                      "ftp: Get '$f' failed: " .
                      $ftp->message);
            $num_retries++;
            #ASGSUtil::stderrMessage("DEBUG","num_retries is $num_retries");
            sleep 60;
         }
      }
      if ( $num_retries >= $max_retries ) {
         $had_enough = 1;
         ASGSUtil::stderrMessage(
                   "INFO",
                   "Retried download more than $max_retries times. Giving up on downloading $f.");
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
      $jshash_ref->{"get"} = basename($0);
      $jshash_ref->{"filesDownloaded"} = \@files_downloaded;
      $jshash_ref->{"filesFromCache"} = \@files_found;
      #ASGSUtil::writeJSON($jshash_ref);
      ASGSUtil::timestampJSON($jshash_ref);
      # write json response to STDOUT
      print JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
      exit;
   } else {
      die;
   }
}
1;
#
# perform partial grib download using curl
# only gets the U, V, P at mean sea level
sub partialGribDownload {
   my ( $dirDate, $f, $idxfile, $localDir ) = @_;
   #--------------------------------------------------------
   #    G R I B   I N V E N T O R Y  A N D   R A N G E S
   #--------------------------------------------------------
   # FIXME: undo this hardcode for downloading files with curl using
   # https://nomads rather than $backsite
   my $idx = "https://nomads.ncep.noaa.gov$backdir/nam.$dirDate/$idxfile";
   # jgfdebug: save a local copy of the inventory file
   ASGSUtil::appMessage(
             "DEBUG",
             "Downloading '$idx' with the command 'curl -f -s $idx -o $localDir/$idxfile'.");
   my $err=system("curl -f -s $idx -o $localDir/$idxfile");
   if ( $err != 0 ) {
      ASGSUtil::stderrMessage(
                "INFO",
                "curl: Get '$idx' failed.");
      unlink("$localDir/$idxfile");
      return $err;
   }
   # download directly into list
   #ASGSUtil::stderrMessage("INFO","Downloading '$idx' with the command 'curl -f -s $idx'.");
   #my @gribInventoryLines = `curl -f -s $idx`; # the grib inventory file from the ftp site
   my @rangeLines;    # inventory with computed ranges
   my $last = 0;      # number of immediately preceding lines with same starting byte index
   my $lastnum = -1;  # starting byte range of previous line (or lines if there are repeats)
   my @old_lines;     # contiguous lines in inventory with same starting byte
   my $has_range = 0; # set to 1 if the inventory already has a range field
   #
   # open index file for this time period
   ASGSUtil::appMessage(
             "INFO",
             "Parsing '$idx' to determine byte ranges of U, V, and P.");
   unless ( open(GRIBINVENTORY,"<$localDir/$idxfile") ) {
      ASGSUtil::stderrMessage(
                "ERROR",
                "Could not open '$localDir/$idxfile' for appending: $!.");
      return 1;
   }
   while(<GRIBINVENTORY>) {
      chomp($_);
      #ASGSUtil::stderrMessage("INFO","$li");
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
   #
   #   r a n g e   f i e l d s   h a v e   n o w   b e e n
   #
   #       c o m p u t e d   o r   p r o v i d e d
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
   ASGSUtil::appMessage(
             "INFO",
             "Downloading '$f' to '$localDir' with curl -f -s -r \"$range\" https://nomads.ncep.noaa.gov$backdir/nam.$dirDate/$f -o $localDir/$f.");
   $err=system("curl -f -s -r \"$range\" https://nomads.ncep.noaa.gov$backdir/nam.$dirDate/$f -o $localDir/$f");
   if ( $err == 0 ) {
      #ASGSUtil::stderrMessage("INFO","Download complete.");
      return 0;
      #ASGSUtil::stderrMessage("DEBUG","Now have data for $dirDate$hourString.");
   } else {
      ASGSUtil::stderrMessage(
                "INFO",
                "curl: Get '$f' failed.");
      unlink("$localDir/$f");
      return 1;
   }
   return;
}

#!/usr/bin/env perl
#----------------------------------------------------------------
# nco_ncrcat.pl
#
# concatenate fort.61.nc files at various time spans to
# facilitate validation
#----------------------------------------------------------------
# Copyright(C) 2021--2022 Jason Fleming
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
#----------------------------------------------------------------
# This script needs a "now" date in yyyymmddhh24 format and expects
# to find directories and files arranged in the following way for
# files posted to an opendap server:
# $root_data_dir/year/metmodel/yyyymmddhh24/mesh/hpc/asgs_instance/scenario/file
# and this way for files that are still in-situ in the directories
# of an asgs instance on the hpc machine:
# $root_data_dir/yyyymmddhh24/scenario/file
# For example:
#./2021/nam/2021012918/hsofs/hatteras.renci.org/hsofs-nam-bob-2021/nowcast/fort.61.nc
# or
# /work/jgflemin/asgs45806/2021042400/nowcast/fort.61.nc
# Then the script concatenates the output files over different time
# periods as follows (in days):
# 0.25 0.5 1.0 2.0 4.0 8.0 16.0 32.0
# with "now" date given as the final date in the file.
#----------------------------------------------------------------
# Assumptions :
#   1. all files have the same cold start date (so the time in
#      seconds is all that is needed)
#   2. if the target date is not given, the script uses the latest
#      target date available
#   3. the sort used in the find command assumes that the only
#      difference in the paths is in the dates ; if there are
#      other differences (e.g., hpcenv) the sort command will fail
#----------------------------------------------------------------
# Dependencies :
#   ncdump (installed with main netcdf library)
#   ncrcat (part of nco)
#   ncks   (part of nco?)
#----------------------------------------------------------------
# Sample commands to build up the file list JSON:
# path="/mnt/nas-storage/Operations/fortytwo.cct.lsu.edu/2022/nam/202204????/HSOFS/qbc.loni.org/HSOFS_nam_akheir/nowcast"
# fileList=$(printf "\"%s\",\n" $(ls -d $path | sort))
# printf "{ \"fort.61.nc\" : [ ${fileList::-1} ] }" | json_pp > fileList.json
#----------------------------------------------------------------
use strict;
use warnings;
use Getopt::Long;
use JSON::PP;
use Cwd;
use Util::H2O;
use ASGSUtil;
#
my $root_data_dir;             # directory containing subdirectories with data of interest
my $datafile = "fort.61.nc";   # file containing data at each station
my $target_end_sec = "null";   # the end of the target data file in seconds
my $target_date = "null";      # yyyymmddhh24 UTC to count back from
my $csy; my $csm; my $csd; my $csh; # cold start date components
my @time_periods = qw( 1 2 4 7 14 21 30 ); # days back from the target date
my $jshash_ref;                # list of files to be processed
my @paths_of_interest;
my @files_of_interest;
my @time_period_prefixes;

#
GetOptions(
           "root-data-dir=s" => \$root_data_dir,
           "target-date=s" => \$target_date
          );
#
# slurp the JSON request contents into a scalar variable
my $file_content = do { local $/; <> };
$jshash_ref = JSON::PP->new->decode($file_content);
# grab the list of paths out of the hash
my $pathlistref = $jshash_ref->{$datafile};
my @pathList = @$pathlistref;
if ( ! @pathList ) {
   ASGSUtil::stderrMessage("WARNING","The JSON did not contain any paths to '$datafile' files.");
   if ( $root_data_dir ) {
      # find the paths to the files ... the find command does not seem to return them
      # in ascending order so they are sorted ... only want the nowcast or
      # hindcast scenarios for use in validation
      my @pathList = `find $root_data_dir -name $datafile -print | grep -E 'nowcast|hindcast' | sort`;
      foreach my $p (@pathList) {
         ASGSUtil::stderrMessage("DEBUG","$p");
      }
   } else {
      ASGSUtil::stderrMessage("ERROR","The --root-data-dir command line option was not provided.");
      die;
   }
}
#
# check to see if the initialize/hindcast scenario is present; if so,
# sort will make it the "latest" scenario but we want it to be earliest
my @path_parts = split("/",$pathList[-1]);
if ( $path_parts[-2] eq "hindcast" ) {
   my $hindcast_scenario = pop(@pathList);
   unshift(@pathList,$hindcast_scenario);
}
# get rid of directories that have been moved out of the way, like 2021072206.old
my @filtered_paths;
PATHS: foreach my $fullpath (@pathList) {
   @path_parts = split("/",$fullpath);
   PARTS: foreach my $p (@path_parts) {
      if ( $p =~ /\.old/ ) {
         ASGSUtil::stderrMessage("INFO","Skipping the directory '$p'.");
         last PARTS;
      }
   }
   push(@filtered_paths,$fullpath);
}
#
# now reverse the order so it is reverse chronological
my @reverse_paths = reverse(@filtered_paths);
my @runStartTimes;
my @runEndTimes;
my @coldStartDates;
foreach my $p (@reverse_paths) {
   #ASGSUtil::stderrMessage("DEBUG","$p");
   # grab run start times and run end times from the metadata
   my %runProp;
   ASGSUtil::readProperties(\%runProp, "$p/run.properties");
   push(@runStartTimes,$runProp{'RunStartTime'});
   push(@runEndTimes,$runProp{'RunEndTime'});
   push(@coldStartDates,$runProp{'adcirc.time.coldstartdate'});
}
#
# find the final or target date if it was not given
if ( $target_date eq "null" ) {
   $target_date = $runEndTimes[0];
}
ASGSUtil::stderrMessage("DEBUG","target_date is $target_date");
ASGSUtil::stderrMessage("DEBUG","int(target_date) is ".int($target_date));
$target_date =~ /(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $ty = $1;
my $tm = $2;
my $td = $3;
my $th = $4;
my $tmin = 0;
my $tsec = 0;
#
# determine the yyyymmddHH24 date corresponding to the start of each of the
# time periods of interest
my @periodStarts;
foreach my $d (@time_periods) {
   my ($sy,$sm,$sd,$sh,$smin,$ss) =
      Date::Calc::Add_Delta_DHMS($ty,$tm,$td,$th,$tmin,$tsec,-1*$d,0,0,0);
   push(@periodStarts,sprintf("%04d%02d%02d%02d",$sy,$sm,$sd,$sh));
}
my $dataSetCount = 0; # counting data sets
my $pathCount = 0;    # counting paths
my $startDataSet = 0; # starting data set in the cat command
my $endDataSet = 0;   # last data set in the cat command
my @times;
 #
foreach my $p (@reverse_paths) {
   # add this file to the list of files of interest (this list is in chronological order)
   unshift(@paths_of_interest,$p);
   $pathCount++;
   # see if the end time of this file is after the target date, and if so,
   # don't include the datasets after the target date
#   if ( $runEndTimes[0] > $target_date ) {
      # grab the yyymmmddhh24 of each of the datasets in the file
      # use netcdf kitchen sink utility to get the final times in seconds
      # {
      #   "dimensions": {
      #     "time": 72
      #   },
      #   "variables": {
      #     "time": {
      #       "shape": ["time"],
      #       "type": "double",
      #       "attributes": {
      #         "long_name": "model time",
      #         "standard_name": "time",
      #         "units": "seconds since 2021-12-15 00:00:00",
      #         "base_date": "2021-12-15 00:00:00"
      #       },
      #       "data": [11405100, 11405400, 11405700, 11406000, 11406300, 11406600, 11406900, 11407200, 11407500, 11407800, 11408100, 11408400, 11408700, 11409000, 11409300, 11409600, 11409900, 11410200, 11410500, 11410800, 11411100, 11411400, 11411700, 11412000, 11412300, 11412600, 11412900, 11413200, 11413500, 11413800, 11414100, 11414400, 11414700, 11415000, 11415300, 11415600, 11415900, 11416200, 11416500, 11416800, 11417100, 11417400, 11417700, 11418000, 11418300, 11418600, 11418900, 11419200, 11419500, 11419800, 11420100, 11420400, 11420700, 11421000, 11421300, 11421600, 11421900, 11422200, 11422500, 11422800, 11423100, 11423400, 11423700, 11424000, 11424300, 11424600, 11424900, 11425200, 11425500, 11425800, 11426100, 11426400]
      #     }
      #   }
      # }
      my $timeJSON = `ncks --json -v time "$p/$datafile"`;
      my $jsTime_hash = h2o -recurse, JSON::PP->new->decode($timeJSON);
      my @times = @{$jsTime_hash->variables->time->data};
      foreach my $t (@times) {
         print "$t ";
      }

      #my $jsTime_hash = h2o -recurse, JSON::PP->new->decode($timeJSON);
      #my $data_ref = $jsTime_hash->variables->time->data;
      #my @times = @$data_ref;
      #foreach my $t (@times) {
      #   print "$t ";
      #}

      #my $jsTime_ref = JSON::PP->new->decode($timeJSON);
      #my $jsTime_hash = h2o -recurse, $jsTime_ref;
      #my $data_ref = $jsTime_hash->variables->time->data;
      #my @times = @$data_ref;
      #foreach my $t (@times) {
      #   print "$t ";
      #}

      #my $data_ref = @$time_ref{data};
      # grab the list of paths out of the hash
      #my $vars_ref = %$jsTime_ref{variables};
      #print keys %$vars_ref;
      #my $time_ref = %$vars_ref{time};
      #print keys %$time_ref;

      #@times = @$data_ref;
      #foreach my $t (@times) {
      #   print "$t ";
      #}

 #  }
   #  if ( $runStartTimes[0] < $times[99] ) {
   #      my $period_file_list = join(" ",@files_of_interest);
   #      #stderrMessage("DEBUG","period_file_list $period_file_list");
   #      my $period_file_name = $time_period_prefixes[0] . "_fort.61.nc";
   #      # -O will overwrite an existing file
   #      # -D 0 will set the debug level to 0 and hopefully suppress spurious warnings
   #      system("ncrcat -O -D 0 $period_file_list $period_file_name");
   #      #stderrMessage("DEBUG","system(ncrcat $period_file_list $period_file_name)");
   #      # go to the next period
   #      shift(@time_periods);
   #      shift(@time_period_prefixes);
   #  }
   #  # end when we run out of time periods to write values for
   #  if ( @time_periods == 0 ) {
   #      last;
   #  }
}
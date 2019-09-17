#!/usr/bin/env perl
#--------------------------------------------------------------
# get_flux.pl: downloads fort.20 files representing variable river
# flux data for ASGS nowcasts and forecasts.
#--------------------------------------------------------------
# Copyright(C) 2011--2016 Jason Fleming
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
# If forecast data is requested, the script will grab the 
# forecast data corresponding to the current ADCIRC time.
#
# jgf20150218: The script expects to find directories containing 
# fort.20 files. It expects these directories to be named according
# to the date, e.g., 20150218. Within these directories, it expects
# to find hindcast and forecast data with filenames formatted as
# follows:
#
# RHLRv9-OKU_20150218T0000_20150218T0000_20150218T0000_00_Zfort.20HC.txt
# RHLRv9-OKU_20150218T0000_20150218T0000_20150225T0000_00_Zfort.20FC.txt
#
#
#--------------------------------------------------------------
$^W++;
use strict;
use Net::FTP;
use Getopt::Long;
use Date::Calc;
use Cwd;
#
our $advisdir;   # directory where the ASGS is running
my $riversite; # site for variable flux b.c. data
my $riverdir;  # dir on site for variable flux b.c. data
our $enstorm;  # hindcast, nowcast, or other
my $csdate;   # UTC date and hour (YYYYMMDDHH) of ADCIRC cold start
my $hstime;   # hotstart time, i.e., time since ADCIRC cold start (in seconds)
my @altnamdirs; # alternate directories to look in for flux b.c. data 
our $archivedruns; # path to previously conducted and archived files
our @forecastcycle; # nam cycles to run a forecast (not just nowcast)
my $scriptDir;  # directory where the executable scripts are found
#
my @targetDirs; # directories to download data from 
my @nowcasts_downloaded;  # list of nowcast files that were 
                          # successfully downloaded
our @flux_data; # the data that will be written to the fort.20 file
my $varflux_nodes = 0; # number of nodes in fort.14 for var flux
our $now = 0; # the yyyyMMddhh24mmss start time of the ADCIRC run
our $end = 0; # the yyyyMMddhh24mmss end time of the ADCIRC run
my $meshfile = "./fort.14"; # adcirc mesh that the flux data apply to
my $inc; # flux time increment in seconds
our $defaultfile; # file containing the default fluxes if none can be downloaded
our $lastDateNeeded; # date of last flux data required to fully cover the run
our $startingPointFound = 0;
our $enough_data = 0; 
my $rdp = "scp"; # river data protocol: scp, ftp, or filesystem 
my $riveruser = "ldm"; # username on river data machine when using scp
#
GetOptions(
           "advisdir=s" => \$advisdir,
           "riversite=s" => \$riversite,
           "riverdir=s" => \$riverdir,
           "riveruser=s" => \$riveruser,
           "riverdataprotocol=s" => \$rdp,
           "defaultfile=s" => \$defaultfile,
           "meshfile=s" => \$meshfile,
           "enstorm=s" => \$enstorm,
           "csdate=s" => \$csdate,
           "hstime=s" => \$hstime,
           "archivedruns=s" => \$archivedruns,
           "forecastcycle=s" => \@forecastcycle,
           "scriptdir=s" => \$scriptDir
          );
#
# jgf20120706: The default protocol is now scp rather than ftp. 
#
our $dl;
our $ftp;
#
# establish an ftp connection if we are supposed to be using ftp
if ( $rdp eq "ftp" ) {
   $dl = 0;   # true if we were able to download the file(s) successfully
   $ftp = Net::FTP->new($riversite, Debug => 0, Passive => 1); 
   unless ( defined $ftp ) {
      stderrMessage("ERROR","ftp: Cannot connect to $riversite: $@");
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
   # cd to the directory containing the fort.20 variable river flux files
   my $dirSuccess = $ftp->cwd($riverdir);
   unless ( $dirSuccess ) {
      stderrMessage("ERROR",
          "ftp: Cannot change working directory to '$riverdir': " . $ftp->message);
      # TODO: error message so the ASGS can retry?
      die;
   }
}
#
# determine date and hour corresponding to current ADCIRC time
# ... we need to include seconds because the time increment of the 
# flux data will be specified in seconds
my $csdatetime = $csdate."00"."00"; # include minutes and seconds
if ( defined $hstime && $hstime != 0 ) {
   # add the hotstart seconds
   $now = &incDate($csdatetime,$hstime);
} else {
   # the hotstart time was not provided, or it was provided and is equal to 0
   # therefore the current ADCIRC time is the cold start time, t=0
   $now = $csdatetime;
}
#
# Open the mesh file to see how many flux boundary nodes we have ... this
# info is required for parsing the fort.20 file(s) we'll download and 
# for determining the time span of the data within them
unless ( open(MESH,"<$meshfile") ) { 
  stderrMessage("ERROR","Could not open '$meshfile' for reading: $!.");
  die;
}
# parse adcirc mesh file, skipping unneeded lines
my $line = <MESH>;  # header line
$line = <MESH>;     # number of elements and number of nodes
my @fields = split(' ',$line);
my $ne = $fields[0]; # number of elements 
my $np = $fields[1]; # number of nodes
for ( my $i=0; $i<$np; $i++ ) {
   $line = <MESH>; # skip over the list of nodes
}
for ( my $i=0; $i<$ne; $i++ ) {
   $line = <MESH>; # skip over the list of elements
}
$line = <MESH>; # NOPE, number of elevation specified boundary segments
@fields = split(' ',$line);
my $nope = $fields[0];
$line = <MESH>; # skip NETA, number of elevation specified boundary nodes
for ( my $i=0; $i<$nope; $i++ ) {
   $line = <MESH>; # NVDLL, number of nodes in the ith segment
   @fields = split(' ',$line);
   my $nvdll = $fields[0];
   for ( my $j=0; $j<$nvdll; $j++) {
      $line = <MESH>; # skip NBDV, node numbers in the ith segment     
   }
}
$line = <MESH>; # NBOU, number of normal flux specified boundary segments
@fields = split(' ',$line);
my $nbou = $fields[0];
$line = <MESH>; # skip NVEL, number of normal flux specified boundary nodes
for ( my $i=0; $i<$nbou; $i++ ) {
   $line = <MESH>; # NVELL  IBTYPE, number/type of nodes in the ith segment
   @fields = split(' ',$line);
   my $nvell = $fields[0];
   my $ibtype = $fields[1];
   if ( $ibtype == 22 ) {
      $varflux_nodes+=$nvell;   
   }
   for ( my $j=0; $j<$nvell; $j++) {
      $line = <MESH>; # skip NBVV, node numbers in the ith segment     
   }
}
close(MESH);
#
#
# open the run.properties file to determine the end date so that we know 
# how much time we have to have coverage for
unless ( open(PROPERTIES,"<$advisdir/$enstorm/run.properties") ) { 
   stderrMessage("ERROR","Could not open '$advisdir/$enstorm/run.properties' file for reading: $!.");
   die;
}
while(<PROPERTIES>) {
   @fields = split(":",$_);
   if ($fields[0] =~ /RunEndTime/) {
      $fields[1] =~ /(\d{10})/;
      $end = $1."00"."00"; # include minutes and seconds
      last;
   }
}
close(PROPERTIES);
if ( $end == 0 ) {
   stderrMessage("ERROR","Could not find the RunEndTime in the '$advisdir/run.properties' file.");
   die;
}
#
# get all the current data file names
my @fluxFiles;
my @allFiles;
if ( $rdp eq "ftp" ) {
   @allFiles = $ftp->ls();
} elsif ( $rdp eq "scp" ) {
   @allFiles = `ssh -l $riveruser $riversite "find $riverdir"`;
} elsif ( $rdp eq "filesystem" ) {
   @allFiles = `find $riverdir`;
} else {
   stderrMessage("ERROR","The river data protocol was specified as $rdp but this is not a valid protocol for collecting river data. The ASGS supports scp, ftp, and data collection from the local filesystem.");
   die;
}
# get a list of actual files, get rid of empty directories etc
for (my $i=0; $i<@allFiles; $i++) {
   #stderrMessage("DEBUG","allFile: $allFiles[$i]");
   unless ( $allFiles[$i] =~ /OKU_(\d+)T(\d\d)/ ) {
      next;
   } else {
      push(@fluxFiles,$allFiles[$i]);
   }
}
chomp(@fluxFiles);
#for (my $i=0; $i<@fluxFiles; $i++) {
#   stderrMessage("DEBUG","fluxFile: $fluxFiles[$i]");
#}
# now sort the files from earliest to most recent (it appears that ls() does
# not automatically do this for us)
my @sortedFluxFiles;
my $numFluxFiles = @fluxFiles;
if ( $numFluxFiles > 0 ) {
   @sortedFluxFiles = sort by_start_date @fluxFiles;
   # determine the most recent date/hour ... this is the cycle time
   $sortedFluxFiles[-1] =~ /OKU_(\d+)T(\d\d)/;
   my $cycledate = $1; 
   my $cyclehour = $2;
   # grab the date portion from the start of the adcirc run date/time
   $now =~ /(\d{8})/; 
   stderrMessage("INFO","The most recent available river flux cycle date and hour is '$cycledate$cyclehour'.");
} else {
   stderrMessage("WARNING","There are no river flux boundary condition files on the $rdp site.");
}
#if ( $cycledate < $1 ) { 
#   # something strange has happened here ... the most recent files
#   # available on the ftp site actually earlier than the current state
#   # of the ADCIRC run ... an ftp error, most likely
#   stderrMessage("ERROR","The flux cycledate is '$cycledate' but the ADCIRC hotstart date is '$now'; therefore an error has occurred. get_flux.pl is halting this attempted download.");
#   die;
#}
# separate the available files into nowcast and forecast
my @nowcastFluxFiles;
my @forecastFluxFiles;
foreach my $file (@sortedFluxFiles) {
   if ( $file =~ /HC.txt/ ) {
      push(@nowcastFluxFiles,$file);
   }
   if ( $file =~ /FC.txt/ ) {
      push(@forecastFluxFiles,$file);
   }
}
my $numNowcast = @nowcastFluxFiles;
my $numForecast = @forecastFluxFiles;
stderrMessage("INFO","There are '$numNowcast' nowcast fort.20 files and '$numForecast' forecast fort.20 files on the $rdp site.");
#for (my $i=0; $i<$numNowcast; $i++) {
#   stderrMessage("DEBUG",$nowcastFluxFiles[$i]);
#}
our $dateNeeded = $now;
our $dataDate = $dateNeeded; # initialize to a reasonable value
#
# If this is a nowcast, and there are nowcast files on the site, 
# construct a fort.20 file using the nowcast ("*HC*") data
# files from the site. Also use nowcast data to construct a fort.20 file
# if this is a forecast but there isn't any forecast data (seems unlikely).  
if ( ($enstorm eq "nowcast") || 
        (($enstorm ne "nowcast") && ($numForecast == 0)) ) {
   if ($numNowcast != 0) {
      $enough_data = &getFluxData(@nowcastFluxFiles);
   }
}
#
# If this is some sort of forecast, construct a fort.20 file using the 
# forecast ("*FC*") data from the site ... also, if this is a nowcast
# but there wasn't enough nowcast data, use forecast data to fill out the 
# remainder of the required data ... only do this if there is forecast data
# on the site. 
if ( ($enstorm ne "nowcast") || ($enough_data == 0) ) {
   if ($numForecast != 0) {
      $enough_data = &getFluxData(@forecastFluxFiles);
   }
}
#
# If there weren't any data on the site at all, use the default values
if ( $numNowcast == 0 && $numForecast == 0 ) {
   unless (open(FLUX,"<$defaultfile") ) { 
      stderrMessage("ERROR","Could not open '$defaultfile' for reading: $!.");
      die;
   }
   # read the time increment from the file
   $line = <FLUX>; 
   @fields = split(" ",$line);
   $inc = $fields[0]; # this should never change
   stderrMessage("INFO","The time increment in the default fort.20 file is $inc seconds.");
   # use the increment to calculate the last date we will need data 
   # for, which is one time increment beyond the end date of the 
   # simulation
   $lastDateNeeded = &incDate($end,$inc);
   # read the flux data from the file
   @flux_data = <FLUX>;
   close(FLUX);
   $enough_data = 1; # we've made sure the default data file is really long
} elsif ($enough_data == 0) {
   # If there were data on the site, but there wasn't enough nowcast and
   # forecast data in the file(s) to cover all the required run time, then 
   # persist the last available flux data
   my @flux_persist; 
   # grab the last dataset that was read from a file
   for (my $i=-$varflux_nodes; $i<=-1; $i++) {
      $flux_persist[$i+$varflux_nodes] = $flux_data[$i]; # read this dataset
   }
   while ($dataDate <= $lastDateNeeded) {
      push(@flux_data,@flux_persist);
      # increment the time that the persisted data correspond to   
      $dataDate = &incDate($dataDate,$inc);
   }
   $enough_data = 1; 
}
#
if ( $enough_data == 1 ) {
   unless ( open(OUT,">$advisdir/$enstorm/fort.20") ) { 
      stderrMessage("ERROR","Could not open '$advisdir/$enstorm/fort.20' for writing: $!.");
      die;
   }
   printf OUT "$inc     ! flux time increment (sec); there are $varflux_nodes flux nodes in the mesh\n";
   my $num_flux_values = @flux_data;
   stderrMessage("INFO","Writing '$num_flux_values' lines to the fort.20 (variable flux) file.");
   for (my $i=0; $i<$num_flux_values; $i++) {
      print OUT $flux_data[$i];
   }
   close(OUT);
} else {
   # should not happen
   stderrMessage("ERROR","There was not enough nowcast data on the ftp site to provide full coverage for the simulation run."); 
   die;
}
#
# jgf20150416: getFluxData Compares the time period of the available
# river boundary condition data with the required time range of this 
# simulation. It then loads fort.20 data of the proper type (nowcast or
# forecast) into the @flux_data array to cover the required time period.
#
# It contains logic to cover gaps and issues with the available data as
# follows:
#
# a. All available river b.c. data are later than the time period of
# interest. In this case, our first set of flux data comes from the 
# default river flux file b.c. file (whose name is set in the asgs 
# config file), and then we cover the gap between the starting time and
# the first available set of river b.c. data by generating data at the 
# correct time increment by linearly interpolating in time to the the first
# available set of river b.c. data. 
#
# b. The required time period of the simulation is within the start and
# end dates of the available river b.c. data. The available river flux data
# in each file may cover a longer period of time than required, or may not
# fully cover the time period. Or there may be gaps in time during the required
# period. There is logic to only select the portions of the available
# b.c. files that are pertinent, and to linearly interpolate in time over
# gaps in the data. 
#
# c. jgf20150416 adding capability to handle the case where the time 
# period of the available data is completely before or completely after
# the time period of interest, and simply using the default flux file in 
# this case. 
sub getFluxData() {
   my $numFiles = @_;
   # Check to see if the time period of the ADCIRC run is wholly
   # outside the time period of the available data. Start by
   # finding the date of the last available data. 
   # If the files are forecast files, the last date encoded in the
   # filename is the end date of the data.
   $_[-1] =~ /OKU_\d{8}T\d{4}_\d{8}T\d{4}_(\d{8})T(\d{4})/;
   my $dataEndDate = $1.$2."00"; # include seconds
   # However, if these files are "HC" files, used for a nowcast,
   # then all the dates encoded in the file name will be the same, and we
   # have to actually open the file and parse to find out when the data
   # end. 
   if ( $_[-1] =~ /HC/ ) {
      &getFluxFile($_[-1], $rdp); 
      #unless (open(FLUX,"<$_[-1]") ) { 
      #   stderrMessage("ERROR","Could not open '$_[-1]' for reading: $!.");
      #   die;
      #}
      my $baseFileName = `basename $_[-1]`;
      unless (open(FLUX,"<$advisdir/$enstorm/$baseFileName") ) { 
         stderrMessage("ERROR","Could not open '$advisdir/$enstorm/$baseFileName' for reading: $!.");
         die;
      }
      #stderrMessage("DEBUG","Opened $_[-1]");
      $line = <FLUX>; # read the first line
      @fields = split(" ",$line);
      $inc = $fields[0];
      #stderrMessage("DEBUG","inc is $inc");
      @flux_data = <FLUX>;
      close(FLUX);
      my $numLines = @flux_data;
      #stderrMessage("DEBUG","numLines is $numLines");
      my $numIncs = $numLines / $varflux_nodes;
      #stderrMessage("DEBUG","numIncs is $numIncs");
      my $numSecs = $numIncs * $inc;
      #stderrMessage("DEBUG","numSecs is $numSecs");
      $dataEndDate = &incDate($dataEndDate,$numSecs);
   }
   # now find the start date of the first available data
   $_[0] =~ /OKU_(\d{8})T(\d{4})/;
   my $dataStartDate = $1.$2."00"; # include seconds
   # if the data end before the simulation starts or the data start
   # after the simulation ends, then just use the default flux
   if ( $dataEndDate < $dateNeeded || $end < $dataStartDate ) {
      stderrMessage("WARNING","The available river boundary condition flux files all pertain to a time period that is not within the time period of the ADCIRC run. The available river boundary condition flux data start at $dataStartDate and end at $dataEndDate while the ADCIRC simulation starts at $dateNeeded and ends at $end. As a result, the default river boundary flux data from the file $defaultfile will be used for this simulation.");
      unless (open(FLUX,"<$defaultfile") ) { 
         stderrMessage("ERROR","Could not open '$defaultfile' for reading: $!.");
         die;
      }
      # read the time increment from the file
      $line = <FLUX>; 
      @fields = split(" ",$line);
      $inc = $fields[0]; # this should never change
      stderrMessage("INFO","The time increment in the default fort.20 file is $inc seconds.");
      # use the increment to calculate the last date we will need data 
      # for, which is one time increment beyond the end date of the 
      # simulation
      $lastDateNeeded = &incDate($end,$inc);
      # read the flux data from the file
      @flux_data = <FLUX>;
      close(FLUX);
      $enough_data = 1; # we've made sure the default data file is really long
      return $enough_data; # ************EARLY RETURN************
   }
   # Find a starting point to construct a fort.20 file.
   my $startingPoint = 0;  # index of the file name to start with
   # establish a starting point: find a file that starts on or before
   # the date needed, working backward from the latest available
   for (my $i=-1; $i>=-$numFiles; $i--) {
      $_[$i] =~ /OKU_(\d{8})T(\d{4})/;
      $dataDate = $1.$2."00"; # include seconds
      if ($dataDate <= $dateNeeded) {
         $startingPointFound = 1;
         $startingPoint = $i;
         last;
      }
   }
   # could not find a file to start with ... all the files on the ftp site
   # are later than the time we are interested in ... this could happen if
   # we have an outage and the file(s) we need get too stale and are removed
   # from the site before we have a chance to download them 
   if ($startingPointFound == 0) {
      stderrMessage("WARNING","All the river flux boundary condition data files are later than the start time of the simulation. The simulation will start with a default flux value, then linearly interpolate to the first available flux value.");
      # open up the file containing default flux values
      unless (open(FLUX,"<$defaultfile") ) { 
         stderrMessage("ERROR","Could not open '$defaultfile' for reading: $!.");
         die;
      }
      # read the time increment from the file
      $line = <FLUX>; 
      @fields = split(" ",$line);
      $inc = $fields[0]; # this should never change
      # use the increment to calculate the last date we will need data 
      # for, which is one time increment beyond the end date of the 
      # simulation
      $lastDateNeeded = &incDate($end,$inc);
      stderrMessage("INFO","The time increment in the default fort.20 file is $inc seconds.");
      # read the first set of flux data from the file
      for (my $i=0; $i<$varflux_nodes; $i++) {
         $line = <FLUX>; 
         push(@flux_data,$line);
      }
      close(FLUX);
      $dateNeeded = &incDate($dateNeeded,$inc);
      $startingPointFound = 1; # data will be interpolated later
      $startingPoint = -$numFiles; # now we can start from the first file in the list 
   } 
   if ($startingPointFound == 1) {
      # we have a file to start from, open it up and check to see if it
      # has enough data, if not, loop through the remaining files in the list
      # until the whole time period has been covered
      for (my $i=$startingPoint; $i<=-1; $i++) {
         stderrMessage("INFO","Downloading '$_[$i]' to '$advisdir'.");
         if ( $rdp eq "ftp" ) {
            my $success = $ftp->get($_[$i],$advisdir."/".$enstorm."/".$_[$i]);
            unless ( $success ) {
               stderrMessage("INFO","ftp: Get '$_[$i]' failed: " . $ftp->message);
               my $num_flux = @flux_data;
               if ( $num_flux == 0 ) { # we failed to download the very first file
                  last;
               } else { # a gap in the data that we can interpolate over
                  next;
               }
            } else {
               stderrMessage("INFO","Download complete.");
            }
         } elsif ( $rdp eq "scp" ) {
            my $scp_command="scp $riveruser\@$riversite:$_[$i] $advisdir/$enstorm";
            stderrMessage("INFO","Downloading file with $scp_command");
            my $status = `$scp_command`;
         } else {  # $rdp eq filesystem
            my $cp_command = `cp $_[$i] $advisdir/$enstorm`;
            stderrMessage("INFO","Copying file from filesystem.");
            my $status = `$cp_command`; 
         }
         my $baseFileName = `basename $_[$i]`;
         unless (open(FLUX,"<$advisdir/$enstorm/$baseFileName") ) { 
            stderrMessage("ERROR","Could not open '$advisdir/$enstorm/$baseFileName' for reading: $!.");
            die;
         }
         # determine the start date of the data in this file
         $_[$i] =~ /OKU_(\d+)T(\d\d)(\d\d)/;
         my $thisStartDate = $1.$2.$3."00";
         # read the time increment (seconds) of the data in this file
         $line = <FLUX>; 
         @fields = split(" ",$line);
         $inc = $fields[0]; # this should never change
         stderrMessage("INFO","The start date for this dataset is '$thisStartDate'.\n");
         stderrMessage("INFO","The time increment in the fort.20 file is '$inc' seconds.");
         # use the increment to calculate the last date we will need data 
         # for, which is one time increment beyond the end date of the 
         # simulation
         $lastDateNeeded = &incDate($end,$inc);
         #
         # read the data in the file, keeping it if needed, and skipping it 
         # if not 
         $dataDate = $thisStartDate;
         while(<FLUX>) {
            # stderrMessage("DEBUG","Reading a line from the file '$nowcastFluxFiles[$i]'.");
            # compare the date/time of the data with the date/time of the
            # data we are looking for
            #
            # the dataset in the fort.20 file is earlier than the date
            # we are looking for
            if ( $dataDate < $dateNeeded ) {
               #stderrMessage("DEBUG","This data set is earlier than the data required. Skipping this dataset.");
               for (my $i=0; $i<$varflux_nodes-1; $i++) {
                  $line = <FLUX>; # skip this dataset
               }
            #
            # the dataset in the fort.20 file is the same as the date 
            # we are looking for
            } elsif ( $dataDate == $dateNeeded ) {
               # stderrMessage("DEBUG","This data set corresponds to '$dataDate', which is the same as the date of interest, '$dateNeeded'. Loading data.");
               # this is the data we need, load it up
               push(@flux_data,$_);
               for (my $i=0; $i<$varflux_nodes-1; $i++) {
                  $line = <FLUX>; 
                  push(@flux_data,$line);
               }
               # increment the date of interest
               $dateNeeded = &incDate($dateNeeded,$inc);
            #
            # the dataset in the fort.20 file is later than the date
            # we are looking for
            } elsif ( $dataDate > $dateNeeded) {
               # there must be some data missing -- linearly interpolate it
               # form the date and hour of the next nowcast data needed
               stderrMessage("WARNING","Looking for data corresponding to '$dateNeeded' but found data for '$dataDate'. Linearly interpolating between datasets."); 
               # determine the amount of time and number of time increments 
               # required to cover the missing period
               my ($ney,$nem,$ned,$neh, $nemin, $nes); # date/time components
               my ($dy, $dm, $dd, $dh, $dmin, $ds);    # date/time components
               $dateNeeded =~ /(\d{4})(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/; 
               $ney = $1;
               $nem = $2;
               $ned = $3;
               $neh = $4;
               $nemin = $5;
               $nes = $6;
               $dataDate =~  /(\d{4})(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/; 
               $dy = $1;
               $dm = $2;
               $dd = $3;
               $dh = $4;
               $dmin = $5;
               $ds = $6;
               (my $ddays, my $dhrs, my $dmin, my $dsec) =
                  Date::Calc::Delta_DHMS($ney,$nem,$ned,$neh, $nemin, $nes,
                   $dy, $dm, $dd, $dh, $dmin, $ds);
               my $num_incs = 
                 int(($ddays*86400.0 + $dhrs*3600.0 + $dmin*60 + $dsec)/$inc);
               stderrMessage("INFO","Linearly interpolating over $ddays days, $dhrs hours, $dmin minutes, and $dsec seconds ($num_incs flux time increments).");
               # start interpolating with the last data we read
               my @startingNodalFlux;
               for (my $i=-$varflux_nodes; $i<=-1; $i++) {
                  $startingNodalFlux[$i+$varflux_nodes] = $flux_data[$i];
               }
               # now read the data from file (which represents the data 
               # we are interpolating to)    
               my @endingNodalFlux;
               $endingNodalFlux[0] = $_;
               for (my $i=1; $i<$varflux_nodes; $i++) {
                  $endingNodalFlux[$i] = <FLUX>;
               }
               #stderrMessage("DEBUG","Starting nodal flux:");
               #for (my $j=0; $j<$varflux_nodes; $j++ ) {
               #   stderrMessage("DEBUG","j=$j flux=$startingNodalFlux[$j]");
               #}
               #stderrMessage("DEBUG","Ending nodal flux:");
               #for (my $j=0; $j<$varflux_nodes; $j++ ) {
               #   stderrMessage("DEBUG","j=$j flux=$endingNodalFlux[$j]");
               #}
               # now fill in the data for each missing time increment
               for (my $i=1; $i<=$num_incs; $i++) {
                  for (my $j=0; $j<$varflux_nodes; $j++) {
                     my $flux = ($i/($num_incs+1))
                      *($endingNodalFlux[$j] - $startingNodalFlux[$j])
                      +$startingNodalFlux[$j];
               #      stderrMessage("DEBUG","j=$j flux=$flux");
                     push(@flux_data,$flux."\n");
                  }
               }
               # push the ending data into the results
               for (my $i=1; $i<$varflux_nodes; $i++) {
                  push(@flux_data,$endingNodalFlux[$i]);
               }
               # increment the date of interest
               $dateNeeded = &incDate($dateNeeded,($inc+($num_incs*$inc)));
            }   
            # increment the dataDate to correspond to the next dataset
            $dataDate = &incDate($dataDate,$inc);
            # check to see if the date (supposedly) needed is more than one
            # time increment beyond the end of the run, and if so, we have
            # enough data and can jump out of the loop
            if ($dataDate > $lastDateNeeded) {
               $enough_data = 1;
               last;
            }
         } # while(<FLUX>)
         stderrMessage("DEBUG","Finished loading $_[$i].");
         if ($enough_data == 1) {
            last; # don't read any more nowcast files
         }
      } # for (file list)
   } # if starting point found
   return $enough_data;
} 
#
# Takes a date string yyyyMMddhh24mmss, adds the number of seconds 
# specified in the argument, and returns the result as a new date
# string with the same format as the input
sub incDate () {
   my $oldDate = shift;
   my $incSecs = shift; 
   #             yyyy    MM    dd   hh24   mm    ss
   $oldDate =~ /(\d{4})(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/; 
   (my $ndy, my $ndm, my $ndd, my $ndh, my $ndmin, my $nds) =
         Date::Calc::Add_Delta_DHMS($1,$2,$3,$4,$5,$6,0,0,0,$incSecs); 
   return sprintf("%4d%02d%02d%02d%02d%02d",$ndy,$ndm,$ndd,$ndh,$ndmin,0);
}
#
#  Sorts a list of flux files from earliest starting date to latest starting
#  date ... ignores whether they are hindcast or forecast
sub by_start_date {
   $a =~ /OKU_(\d{8})T(\d{4})/;
   my $datetime_a = $1.$2;
   $b =~ /OKU_(\d{8})T(\d{4})/;
   my $datetime_b = $1.$2;
   $datetime_a <=> $datetime_b;
}

sub getFluxFile {
   my $fluxFileTarget = shift;
   my $protocol = shift;
   stderrMessage("INFO","Getting '$fluxFileTarget' via $protocol and writing to directory '$advisdir'.");
   if ( $protocol eq "ftp" ) {
      my $success = $ftp->get($fluxFileTarget,$advisdir."/".$enstorm."/".$fluxFileTarget);
      unless ( $success ) {
         stderrMessage("INFO","ftp: Get '$fluxFileTarget' failed: " . $ftp->message);
      } else {
         stderrMessage("INFO","Download complete.");
      }
   } elsif ( $protocol eq "scp" ) {
      my $scp_command="scp $riveruser\@$riversite:$fluxFileTarget $advisdir/$enstorm";
      stderrMessage("INFO","Downloading file with $scp_command");
      my $status = `$scp_command`;
   } else {  # $rdp eq filesystem
      my $cp_command = `cp $fluxFileTarget $advisdir/$enstorm`;
      stderrMessage("INFO","Copying $fluxFileTarget from filesystem.");
      my $status = `$cp_command`; 
   }
}

#
#  Prints a message to stderr, annotated with the date, the specified 
#  severity level, and the name of this script.
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: $enstorm: get_flux.pl: $message\n";
}

#!/usr/bin/env perl
#----------------------------------------------------------------
# advisory_xml_filler.pl
#
# Parses historical text forecast/advisories and optionally 
# best track files to create simulated real time files usable with ASGS. 
#
#----------------------------------------------------------------
#
# Copyright(C) 2015 Jason Fleming
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
#
# Parses historical text forecast/advisories from html files downloaded 
# from the National Hurricane Center and creates an xml file (using
# a template) to simulate the xml files generated in real time 
# for new forecast/advisories from the NHC. These can then be used
# in the ASGS to drive ADCIRC with the forecast/advisories for that
# storm.
#
# If the name of a best track file is optionally provided, this script 
# uses it to create a truncated best track file to accompany 
# the xml forecast/advisory file.  
#
#----------------------------------------------------------------
#
# Data needed from the forecast advisories to fill the XML template:
#
# %stormClass%
# %stormNameAllCaps%
# %advisoryNumber%
# %dateTimeIssued%
# %forecastAdvisoryText%
#
# %fileName% (either formed by this script or fed as an input param)
#
#----------------------------------------------------------------
#
# Sample shell script using this file:
#advisoryMin=1 ; advisoryMax=39 ; advisory=$advisoryMin ; while [[ $advisory -le $advisoryMax ]]; do advisoryNum=`printf "%03d\n" $advisory`; perl ~/asgs/2014stable/input/sample_advisories/advisory_xml_filler.pl --input al092012.fstadv.$advisoryNum.shtml --template ../template.index.xml --best bal092012.dat  ; advisory=`expr $advisory + 1` ; done

use strict;
use warnings;
use Date::Calc;
use Getopt::Long;
#
my $template; # the template xml file representing new forecast/advisory
my $input;    # name of input html file containing forecast advisory text
my $output;   # name of output xml file representing simulated RSS XML feed
my $best;     # name of a BEST track file from NHC for this storm
#
my $stormNumber; # number within the year 
my $stormYear;   # year that the event occurred
my $hour;         # hour that the forecast is effective
my $dayOfWeek;   # day of the week that the forecast was issued (all caps)
my $month;        # JUL, AUG, etc
my $day;          # two digit day of month, zero padded
my $issuedAt;    # used to fill in the description element
#
my $stormClass; # HURRICANE, TROPICAL STORM, TROPICAL DEPRESSION, etc
my $stormClassCase; # storm class with just the first letter capitalized
my $stormName; # KATRINA, RITA, KAREN, etc
my $advisoryNumber;
my $pubDate;    # formatted as follows: 
my $forecastAdvisoryText; # formatted as received, starts with ZCZC and ends with NNNN
my $fileName;   # of the output xml file, e.g., debby.01.index-at.xml
my $basin;      # EP (eastern pacific), AL (atlantic) etc
my $thisBest;   # name of the best track file being written
my @fields;      # fields in the line in the BEST track file 
my $fcstDate;   # 10 digit integer forecast date/time yyyymmddhh24
#
my %monthDowncase = (JAN=>'Jan',FEB=>'Feb',MAR=>'Mar',APR=>'Apr',MAY=>'May',JUN=>'Jun',JUL=>'Jul',AUG=>'Aug',SEP=>'Sep',OCT=>'Oct',NOV=>'Nov',DEC=>'Dec');
my %dayOfWeekDowncase = (MON=>'Mon',TUE=>'Tue',WED=>'Wed',THU=>'Thu',FRI=>'Fri',SAT=>'Sat',SUN=>'Sun');
my %monthLookup = (dummy => '00', JAN=>'01',FEB=>'02',MAR=>'03',APR=>'04',MAY=>'05',JUN=>'06',JUL=>'07',AUG=>'08',SEP=>'09',OCT=>'10',NOV=>'11',DEC=>'12');
#
GetOptions(
           "input=s" => \$input,
           "template=s" => \$template,
           "output=s" => \$output,
           "best=s" => \$best
           );
#
# open html file containing forecast advisory
unless (open(INPUT,"<$input")) {
   &stderrMessage("ERROR","Failed to open forecast advisory file $input for conversion to ATCF format: $!.");
   die; 
}
#
# For each line in the html file
# 
# 1. Read and toss html before the ZCZC that indicates the start of the
# forecast advisory text.
#
# 2. Grab the forecast advisory text, and parse parameters as required
#    to fill in the xml template. 
#
# 3. Once the NNNN that signals the last line of the forecast advisory
#    has been found, close the html file, fill in the xml template. 
#    
#
my $line;
my $foundAdvisory = 0; # set to 1 if the first line of the forecast advisory has been found 
my $finalLine = 0;     # set to 1 if the last line of the forecast advisory has been found 
while(<INPUT>) {
   # break the line on spaces into a list of fields
   my @fields = split(" ",$_);
   if ( $foundAdvisory == 0 ) {     
      # look for the AWIPS header or the WMO header
      # WTNT21-25 KNHC (MIATCMAT1-5) – Atlantic
      # WTPZ21-25 KNHC (MIATCMEP1-5) – E. Pacific
      unless ( $_ =~ /MIATCM(AT|EP)\d/ || $_ =~ /WT(NT|PZ)\d{2}\s+KNHC/ ) {
         next;
      } else {
         $foundAdvisory = 1;
      }
   }
   # if we've found the end of the advisory, we can end the script
   if ( $_ =~ /FORECASTER/ ) {
      $finalLine = 1;
      close(INPUT);
   }  
   #   
   # Get the year; this is not needed to fill the template, but it 
   # is needed to interpret the format of the forecast/advisory, 
   # because the format changed in 2005
   #
   # NWS TPC/NATIONAL HURRICANE CENTER MIAMI FL   AL172005
   if ( $_ =~ /([A-Z][A-Z])(\d{2})(\d{4})/ ) {   
      $basin = $1;
      $stormNumber = $2;
      $stormYear = $3;
      &stderrMessage("INFO","BASIN: $basin");
      &stderrMessage("INFO","STORM NUMBER: $stormNumber");
      &stderrMessage("INFO","STORM YEAR: $stormYear");
   }
   #
   # Capture the date/time the advisory was issued
   # 2005 and before:    1500Z THU SEP 02 2004
   # after 2005:         1500 UTC TUE JUL 18 2006
   # 
   #             hour                    dayOfWeek   month  dayOfMonth year
   if ( $_ =~ /(\d{4})(?:(?:Z)|(?: UTC)) ([A-Z]{3}) ([A-Z]{3}) (\d{2}) \d{4}/ ) {
      $issuedAt = $_;
      chomp($issuedAt);
      $hour = $1;
      $dayOfWeek = $2;
      $month = $3;
      $day = $4;
      &stderrMessage("INFO","Issued at: $issuedAt");
      #&stderrMessage("DEBUG","$_\n");
      #&stderrMessage("DEBUG","hour: $1\n");
      #&stderrMessage("DEBUG","dayOfWeek: $2\n");
      # 
      # construct a publication date to be the same as the effective
      # date and time that the forecast advisory was issued
      # <pubDate>Thu, 03 Jul 2014 02:46:44 GMT</pubDate>
      my $timeString = substr($hour,0,2) . ":" . substr($hour,2,2) . ":00"; 
      $pubDate = "$dayOfWeekDowncase{$dayOfWeek}, $day $monthDowncase{$month} $stormYear $timeString GMT";
   }
   # 
   # Grab storm class, storm name, and advisory number. 
   #
   # Examples:
   # HURRICANE FRANCES ADVISORY NUMBER  40
   # HURRICANE FRANCES FORECAST/ADVISORY NUMBER  37
   # HURRICANE FRANCES FORECAST/ADVISORY NUMBER  37...CORRECTED
   # HURRICANE FRANCES SPECIAL FORECAST/ADVISORY NUMBER  37
   # POTENTIAL TROPICAL CYCLONE THREE FORECAST/ADVISORY NUMBER   1
   if ( $_ =~ /^(.+)\s+FORECAST.+ADVISORY NUMBER\s+(\d{1,3})/ ) {
      $stormName = $1;
      $advisoryNumber  = $2;
      # get rid of the "SPECIAL" 
      $stormName =~ s/SPECIAL//;
      # break into individual words
      my @tmp = split(' ', $stormName);
      if ($tmp[0] eq 'HURRICANE'){
         $stormClass = $tmp[0];
         $stormName = $tmp[1];
      } elsif ($tmp[0] eq 'TROPICAL' or $tmp[0] eq 'SUBTROPICAL' or $tmp[0] eq 'REMNANTS' or $tmp[0] eq 'POST-TROPICAL') {
          # SUBTROPICAL is rare. see 2007 01
          $stormClass = "$tmp[0] $tmp[1]";
          $stormName = $tmp[2];
      } elsif ($tmp[0] eq 'POTENTIAL') {
         $stormClass = "$tmp[0] $tmp[1] $tmp[2]";       
         $stormName = $tmp[3]; 
      }
      $stormClassCase = $stormClass; 
      $stormClassCase =~ s/([\w']+)/\u\L$1/g;
   }
   #
   # add the full line to the text of the forecast advisory 
   $forecastAdvisoryText .= $_;
   if ( $finalLine == 1 ) {
      last;
   }
}
close(INPUT);
#
# open template file to be filled in
unless (open(TEMPLATE,"<$template")) { 
   &stderrMessage("ERROR","Failed to open template XML file $template : $!.");
   die;
}
# 
# form the name of the output file ... for the atlantic, the nhc posts
# index-at.xml files (whereas you might expect that the name of the
# file would be index-al.xml) ... for the pacific, the rss feed is named
# index-ep.xml
my $fileBasin = "at";
if ( $basin eq "EP" ) {
   $fileBasin = lc $basin;
}
$output = sprintf("%02d",$advisoryNumber) . "." . $stormNumber . $stormYear . ".index-" . $fileBasin . ".xml";
#
# open output file
unless (open(OUTPUT,">$output")) { 
   &stderrMessage("ERROR","Failed to open output file $output : $!.");
   die;
}
#
# Read the template file, fill in the fields, and write to output file.
while(<TEMPLATE>) {
    s/%pubDate%/$pubDate/;
    s/%issuedAt%/$issuedAt/;
    s/%stormClass%/$stormClass/;
    s/%stormClassCase%/$stormClassCase/;
    s/%stormName%/$stormName/;
    s/%advisoryNumber%/$advisoryNumber/;
    s/%year%/$stormYear/;
    s/%stormNumber%/$stormNumber/;
    s/%fileName%/$output/;  
    s/%forecastAdvisoryText%/$forecastAdvisoryText/;
    print OUTPUT $_;
}
#
close(TEMPLATE);
close(OUTPUT);
#
# If the name of a BEST track file has been provided, load it up and
# produce a truncated BEST track file to accompany this advisory. 
unless ( defined $best ) { 
   exit;
}
#
# open BEST track file
unless (open(BEST,"<$best")) { 
   &stderrMessage("ERROR","Failed to open BEST track file $best : $!.");
   die;
}
#
# open the file that will contain the truncated best track data for
# this advisory
$thisBest = sprintf("%02d.$best",$advisoryNumber);
unless (open(THISBEST,">$thisBest")) {
   &stderrMessage("ERROR","Failed to open truncated BEST track file for this advisory $thisBest for writing : $!.");
   die;
}   
#
# read each line of the file, perform quality control, and check the
# time to ensure it is prior to the forecast advisory being issued
while(<BEST>) {
   # split the line on commas 
   @fields = split(",",$_);
   $fields[10] =~ s/ //g; # remove spaces
   # get rid of lines at the beginning of the file that do not provide
   # enough information to produce a storm vortex, such as tropical 
   # disturbance ("DB") or low pressure system ("LO") or tropical wave ("WV")
   if ( $fields[10] eq "DB" || $fields[10] eq "LO" || $fields[10] eq "WV" ) {
      next;
   } 
   # check to see if the BEST track date is later than the time of the
   # forecast advisory, and if so, skip the line
   my $fcstDate = $stormYear . $monthLookup{$month} . $day . substr($hour,0,2);
   #&stderrMessage("DEBUG","$fcstDate $fields[2]");
   if ( $fields[2] > $fcstDate ) {
      next;
   }
   # Check to see if the BEST track line occurs at an hour other than
   # 00, 06, 12, 18. These lines correspond to "special advisories"
   # that actually just update the current position of the storm and 
   # provide nothing to the analyst. So we skip these lines
   $fields[2] =~ /\d{4}\d{2}\d{2}(\d{2})/;
   if ( $1 != 0 && $1 != 6 && $1 != 12 && $1 !=18 ) {
      next;
   }
   printf THISBEST $_;
}
close(BEST);
close(THISBEST);
#
# Write a log message to stderr. 
sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hour:$minute:$second]";
   printf STDERR "$theTime $level: advisory_xml_filler.pl: $message\n";
}



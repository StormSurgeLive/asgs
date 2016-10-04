#!/usr/bin/env perl
#######################################################################
# getCOOPSdata.pl
#
# a script get data from the NOAA CO-OPS API for data retrieval
#
# can be run interactively or with command line options
# 
#  e.g.
#
#  getCOOPSdata.pl  (and it will prompt you)
#
#  or  
#
#  getCOOPSdata.pl --station 8770613 --begin "20080824 01:30" --end "20080909 13:30" --product water_level --timezone GMT --datum MSL --units metric --format csv --outfile output.csv
# 
# It overcomes the one month limitation of the CO-OPs API
# by grabing data in month long chunks and appending them to he 
# output. Thus it is good for getting long records (e.g. 30 years)
# 
# Not all stations have all products, datums, etc.  Generally
# the output will return an error message if you request something 
# you can't get.
#
####################################################################### 
# Author: Nate Dill, natedill@gmail.com
#
# Copyright (C) 2015-2016 Nathan Dill
# 
# This program  is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of the 
# License, or (at your option) any later version. 
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software 
# Foundation, Inc., 59 Temple Place - Suite 330,Boston, MA  02111-1307,
# USA.
#                                       
#######################################################################
use strict;
use warnings;

use LWP;
use Getopt::Long;
use URI;

my $interactive=1;  # set to 1 if you want the script to prompt for missing input
                    # if set to zero, some missing inputs will be fatal errors.


# declare some variables
my $station; # CO-OPS station ID, e.g. 8443970 for Boston
my $beginDate; # e.g. yyyyMMdd, yyyyMMdd HH:mm, MM/dd/yyyy, or MM/dd/yyyy HH:mm 
my $endDate;   # e.g. yyyyMMdd, yyyyMMdd HH:mm, MM/dd/yyyy, or MM/dd/yyyy HH:mm 
my $range;     # in hours, use if only given beginDate or endDate
my $datum;    # MHHW, MHW, MTL, MSL, MLW, MLLW, NAVD, or STND
my $units;      # metric or english
my $timeZone; # gmt, lst, lst_ldt,
my $interval;  # "h" if you want hourly met or predictions
my $format;    # json, xml, or csv
my $bin;       # e.g. 4 if you want bin 4 for current data
my $application;# e.g. web_services, application name, 
my $product;   # see below

my @PRODUCTS=("water_level",       # list of products you might be able to get
              "combined_water_level",
              "air_temperature", 
	      "water_temperature",
	      "wind",
	      "air_pressure",
	      "air_gap",
	      "conductivity",
	      "visibility",
	      "humidity",
	      "salinity",
	      "hourly_height",
	      "high_low",
	      "daily_mean",
	      "monthly_mean",
	      "one_minute_water_level",
	      "predictions",
	      "datums",
	      "currents");

my @DATUMS=("MHHW","MHW","DTL","MTL","MSL","MLW","MLLW","GT","MN","DHQ","DLQ","NAVD");

my $url = URI->new ('http://tidesandcurrents.noaa.gov/api/datagetter');
my $data;
my $req;
my $res;
my $i;
my $outFileName;
my $line;
my $tomorrow;

# deal with the command line options

GetOptions('station:i'     => \$station,  
           'begin:s'       => \$beginDate,
           'end:s'         => \$endDate,
           'range:i'       => \$range,
	   'product:s'     => \$product,
           'datum:s'       => \$datum,
           'units:s'       => \$units,
           'timezone:s'    => \$timeZone,
           'outfile:s'     => \$outFileName,
           'interval:s'    => \$interval,
	   'format:s'      => \$format,
	   'bin:s'         => \$bin,
           'application:s' => \$application,
           'out:s'         => \$outFileName);

$application='RansomConsulting';

# now check the input
unless ($station) {
	die  "getCOOPSdata.pl: Fatal Error - no station id given\n" unless ($interactive); 
	print "** No STATION was specified (e.g. -station 8536110)\n";
	print "  please enter station number:  ";
        $station =<>;
	chomp($station);
	print "\n";
}

unless ($beginDate) {
   die " no beginDate given\n" unless($interactive);
   print "** No beginDate was specified (e.g. -begin 20070801)\n acceptable formats are: \n yyyyMMdd\n ";
   $beginDate=<>;
   chomp $beginDate;
}
unless ($endDate) {
   die " no endDate given\n" unless($interactive);
   print "** No endDate was specified (e.g. -begin 20070801)\n acceptable formats are: \n yyyyMMdd\n ";
   $endDate=<>;
   chomp $endDate;
}
unless ($product) {
   die " no product given\n" unless($interactive);
   print "** No product was specified (e.g. -product water_level)\n ";
   print "Possible available products are:\n";
   foreach my $p (@PRODUCTS){
       print "$p\n";
   }
   $product=<>;
   chomp $product;
}
unless ($datum) {
   die " no product given\n" unless($interactive);
   print "** No datum was specified (e.g. -datum NAVD)\n ";
   print "Possible available datums are:\n";
   foreach my $p (@DATUMS){
       print "$p\n";
   }
   $datum=<>;
   chomp $datum;
}

unless ($units) {
   die " no endDate given\n" unless($interactive);
   print "** No units was specified (e.g. -units metric)\ncan be eithe metric or english\n";
   $units=<>;
   chomp $units;
}
unless ($timeZone) {
   die " no timezone given\n" unless($interactive);
   print "** No timezone was specified (e.g. -timezone LST_LDT)\ncan be either GMT or LST_LDT\n";
   $timeZone=<>;
   chomp $timeZone;
}
unless ($format) {
   die " no format given\n" unless($interactive);
   print "** No format was specified (e.g. -format XML )\ncan be either CSV, JSON, or XML\n";
   $format=<>;
   chomp $format;
}
#need to add more checking here...

# build the form for the querystring
my %form;
$form {'station'}=$station;
$form {'begin_date'} = $beginDate if ($beginDate);
$form {'end_date'} = $endDate if ($endDate);
$form {'range'} = $range if ($range);
$form {'product'}=$product;
$form {'datum'}=$datum;
$form {'units'}=$units;
$form {'time_zone'}=$timeZone;
$form {'format'}=$format;
$form {'interval'}=$interval if ($interval);
$form {'bin'}=$bin if ($bin);
$form {'application'}=$application if ($application);


# now get the data
$outFileName="$station"."_"."$product-$beginDate-$endDate"."."."$format" unless (defined $outFileName);
$outFileName =~ s/://g;  # remove colons from time format since you cant use them in file names
$outFileName =~ s/\s//g;  # remove spaces too

open FILE, ">$outFileName" or die "can't open $outFileName\n";

my $bd=substr($beginDate,0,8);
my $beginTime=$beginDate;  
$beginTime =~ s/$bd//;         # will be HH:MM if it was included in the input
my $ed=substr($endDate,0,8);
my $endTime=$endDate;
$endTime =~ s/$ed//;
my $endDate_noTime=$ed;

my $nextMonth=advanceMonth($bd);
$ed=$nextMonth if ($nextMonth < $ed);

$beginTime=' 00:00' unless ($beginTime =~ m/\d\d:\d\d/);
$endTime=' 23:54' unless ($endTime =~ m/\d\d:\d\d/);  

my $cntnt='';
my $iter=1;
while (1==1){
  
  if ($iter==1){
     $form {'begin_date'} = "$bd"."$beginTime";
  }else{
     $form {'begin_date'} = "$bd"." 00:06";   # 2,3,4... iteration always starts after midnight
  }
  if ($endDate =~m/$ed/) { # this is last iter, go to endTime
     $form {'end_date'} = "$ed"."$endTime";
  }else{                   # there will be more iterations, just get zerotime, next iter will get rest of day
     $form {'end_date'} = "$ed"." 00:00";
  }

  $url->query_form(%form);


  my $ua = LWP::UserAgent->new;


  my $response=$ua->get( $url );  # a HTTP::Response object

 my $success=$response->is_success;
  unless ($success) {
    my $slept=0;
    while ($slept < 4){ 
       print "no success, taking 5";
       sleep 5;
       $success=$response->is_success;
       last if ($success);
       $slept++;
     }
   }
  unless ($success){
     print "slept 4 times without success, now dying\n";
     last;
  }

  my $statusLine=$response->status_line;
   
  my $contentType=$response->content_type; 
   
  my $content=$response->content;
  my @c=split(/\n/,$content);
  shift @c if ($iter !=1 and lc($format) eq 'csv');  # throws away the headerline when appending csv
  $content=join("\n",@c);
  #$cntnt.="$content\n";
  if ( uc($outFileName) eq 'STDOUT'){
     print "$content\n";
  }else{  
     print FILE "$content\n";  
  }

  $bd=$ed;
  $ed=advanceMonth($bd);
  $ed=$endDate_noTime if ($endDate_noTime < $ed);
  last if ($bd==$endDate_noTime);
  $iter++;
}


#print FILE "$cntnt";
close FILE;




#################################################
# subroutine to advance the date to the next day 
# input and output are both in yyyy/mm/dd format
#################################################
sub advanceDay { # adds one day to yyyy/mm/dd
 my $ymd=$_[0];
 my $yyyy=substr($ymd,0,4);
 my $mm=substr($ymd,4,2);
 my $dd=substr($ymd,6,2);
 my $febDays=28;
# is it a leap year?
 if (($yyyy-1900)/4 == int(($yyyy-1900)/4)){
     $febDays=29;
     }

 $dd=$dd+1;

 if (($mm == 1) && ($dd>31)){  # end of January
	 $mm=2;
	 $dd=1;
 }
 if (($mm == 2) && ($dd>$febDays)){ # end of Feb
	 $mm=3;
	 $dd=1;
 }
 if (($mm == 3) && ($dd>31)){
	 $mm=4;
	 $dd=1;
 }
 if (($mm == 4) && ($dd>30)){
	 $mm=5;
	 $dd=1;
 }
 if (($mm == 5) && ($dd>31)){
	 $mm=6;
	 $dd=1;
 }
 if (($mm == 5) && ($dd>31)){
	 $mm=6;
	 $dd=1;
 }
 if (($mm == 6) && ($dd>30)){
	 $mm=7;
	 $dd=1;
 }
 if (($mm == 7) && ($dd>31)){
	 $mm=8;
	 $dd=1;
 }
 if (($mm == 8) && ($dd>31)){
	 $mm=9;
	 $dd=1;
 }
 if (($mm == 9) && ($dd>30)){
	 $mm=10;
	 $dd=1;
 }
 if (($mm == 10) && ($dd>31)){
	 $mm=11;
	 $dd=1;
 }
 if (($mm == 11) && ($dd>30)){
	 $mm=12;
	 $dd=1;
 }
 if (($mm == 12) && ($dd>31)){
	 $mm=1;
	 $dd=1;
	 $yyyy=$yyyy+1;
 }

 my $output=sprintf("%04i%02i%02i",$yyyy,$mm,$dd);
}






#################################################
# subroutine to advance the date to the next month
# input and output are both in yyyymmdd format
#################################################
sub advanceMonth { # adds one Month to yyyymmdd 
 my $ymd=$_[0];
 my $yyyy=substr($ymd,0,4);
 my $mm=substr($ymd,4,2);
 my $dd=substr($ymd,6,2);
 $mm++;
 if ($mm==13){
  $yyyy++;
  $mm=1;
 }

 my $output=sprintf("%04i%02i%02i",$yyyy,$mm,$dd);
}

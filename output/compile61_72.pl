#!/usr/bin/env perl
#
# Copyright(C) 2006, 2007, 2008, 2009 Jason Fleming
# Copyright(C) 2006, 2007 Brett Estrade
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
$^W++;
use strict;
use Date::Calc;
use Getopt::Long;

my $cst='';      # cold start time/date
my $inputdir=''; # directory where the gnuplot template file is located
my $advisdir=''; # directory where the gnuplot script should be written
my $dt='';       # ADCIRC time step; used assuming output every 300 timesteps
my $enstorm='';  # storm ensemble member; indicates need to fake output 
                 # from other storms which have not run yet 

my $numstations = 23; # number of recording stations

GetOptions("cst=s" => \$cst,
           "inputdir=s" => \$inputdir,
           "advisdir=s" => \$advisdir,
           "dt=s" => \$dt,
           "enstorm=s" => \$enstorm);

# if this is being executed for storm1 only, then we need to create fake
# fort.61 and fort.72 files for the other storms ... these will be overwritten
# as those storms are run for real
if ( $enstorm eq "storm1" ) {
   my @elevs = qw(track_2.61 track_3.61 track_4.61 track_5.61);
   my @winds = qw(track_2.72 track_3.72 track_4.72 track_5.72);
   foreach my $file (@elevs) {
      open(CONSENSUS,"track_1.61") || die "Can't find track_1.61";
      open(FAKEELEVS,">".$file);
      while (<CONSENSUS>) {
         my @fields = split;
         for ( my $i=1; $i<=$numstations; ++$i ) {
            $fields[$i] = -99999;
	 }
         print FAKEELEVS "@fields\n";
      }
      close(CONSENSUS);
      close(FAKEELEVS);
   }
   foreach my $file (@winds) {
      open(CONSENSUS,"track_1.72") || die "Can't find track_1.72";
      open(FAKEWINDS,">".$file);
      while (<CONSENSUS>) {
         my @fields = split;
         for ( my $i=1; $i<=($numstations*3); ++$i ) {
            $fields[$i] = -99999;
         } 
         print FAKEWINDS "@fields\n";
      }
      close(CONSENSUS);
      close(FAKEWINDS);
   }
}
# if ADCIRC crashed during any of the forecast runs, pad out the short
# results file such that all the results files are the same length
#
# count lines in each file, determine which is the longest
my @file_lengths;
my $longest_length = 0;
my $file_index = 0;
my $longest_file = "";
my @filenames = qw(track_1.61 track_2.61 track_3.61 track_4.61 track_5.61 track_1.72 track_2.72 track_3.72 track_4.72 track_5.72);
foreach my $file (@filenames) {
   $file_lengths[$file_index] = `cat $file | wc -l`;
   if ( $file_lengths[$file_index] > $longest_length ) {
      $longest_length = $file_lengths[$file_index];
      $longest_file = $filenames[$file_index];
   }
   ++$file_index;
}
#
# pad out any file that is shorter than the longest file so that all
# files are the same length (i.e., as long as the longest)
my $line_padding;
$file_index = 0;
my @firstfields;
my @lastfields;
my $elev_padding;
my $wind_padding;
for ( my $i=1; $i<=$numstations; ++$i ) {
   $elev_padding .= " -99999 ";
}
for ( my $i=1; $i<=($numstations*3); ++$i ) {
   $wind_padding .= " -99999 ";
}
foreach my $file (@filenames) {
   if ( $file_lengths[$file_index] < $longest_length ) {
#      print STDERR "$file is shorter than $longest_file";
      $line_padding = $longest_length - $file_lengths[$file_index];
      # get time increment
      open(NEEDSPADDING,$file) || die "Can't find $file";
      my $line = <NEEDSPADDING>;
      @firstfields = split(" ",$line);
      # get last time point produced by ADCIRC
      while (<NEEDSPADDING>) {
          @lastfields = split;
      }
      close(NEEDSPADDING);
      # now add the padding
      open(NEEDSPADDING,">>".$file) || die "Can't find $file";
      for ( my $i=0; $i<$line_padding; ++$i ) {
          my $time = $lastfields[0] + ($i+1) * $firstfields[0];
          if ( $file_index < 5 ) {
             print NEEDSPADDING "$time  $elev_padding\n";
          } else {
             print NEEDSPADDING "$time  $wind_padding\n";
          }
      }
      close (NEEDSPADDING);
   } 
   ++$file_index;
}
#
# convert units for elevation data and determine the max and min
# values of water surface elevation
my $watermin=0.0; # minimum storm surge
my $watermax=0.0; # maximum storm surge
my @fields;
for ( my $i=0; $i<5; ++$i ) {
  open(ELEVFILE,$filenames[$i]) || die "Could not find $filenames[$i]";
  open(OUTFILE,">converted_".$filenames[$i]) 
                    || die "Could not find converted_$filenames[$i]";
  while(<ELEVFILE>) {
     @fields = split;
     for ( my $j=1; $j<=$numstations; ++$j ) {
        if ( $fields[$j] == "-0.9999900000E+05" ) {
           $fields[$j] = -99999;
	}
        if ( $fields[$j] != -99999 ) {
           if ( !($fields[$j] =~ /NaN/) ) { 
              # convert m to ft
              $fields[$j] = $fields[$j] * 100 / (2.54 * 12);
              if ( $fields[$j] < $watermin ) {
                 $watermin = $fields[$j];
              }
              if ( $fields[$j] > $watermax ) {
                 $watermax = $fields[$j]
              } 
           } else {
              $fields[$j] = -99999;
           }
        }
     }
     print OUTFILE "@fields\n";
  }
  close(ELEVFILE);
  close(OUTFILE);
}
#
# convert units for wind speed data and determine the max and min
# values of wind speed
my $windmin=0.0;  # minimum wind speed
my $windmax=0.0;  # maximum wind speed
my @fields;
for ( my $i=5; $i<10; ++$i ) {
  open(WINDFILE,$filenames[$i]);
  open(OUTFILE,">converted_".$filenames[$i]);
  while(<WINDFILE>) {
     @fields = split;
     for ( my $j=1; $j<=($numstations*3); ++$j ) {
        if ( $fields[$j] == "-0.9999900000E+05" ) {
           $fields[$j] = -99999;
	}
        if ( $fields[$j] != -99999 ) {
           if ( !($fields[$j] =~ /NaN/) ) {
              # convert from m/s to kt
              $fields[$j] = $fields[$j] / 0.51444444;
              # convert from 10 minute winds to 1 minute winds
              $fields[$j] = $fields[$j] * 1.136;
              if ( $fields[$j] < $windmin ) {
                 $windmin = $fields[$j];
              }
              if ( $fields[$j] > $windmax ) {
                 $windmax = $fields[$j]
              } 
           } else {
              $fields[$j] = -99999;
           }
        }
     }
     print OUTFILE "@fields\n";
  }
  close(WINDFILE);
  close(OUTFILE);
}
#
# Step through 10 files
open(FILE61_1,"converted_track_1.61") || die "Can't find converted_track_1.61"; 
open(FILE61_2,"converted_track_2.61") || die "Can't find converted_track_2.61";
open(FILE61_3,"converted_track_3.61") || die "Can't find converted_track_3.61";
open(FILE61_4,"converted_track_4.61") || die "Can't find converted_track_4.61";
open(FILE61_5,"converted_track_5.61") || die "Can't find converted_track_5.61";
open(FILE72_1,"converted_track_1.72") || die "Can't find converted_track_1.72";
open(FILE72_2,"converted_track_2.72") || die "Can't find converted_track_2.72";
open(FILE72_3,"converted_track_3.72") || die "Can't find converted_track_3.72";
open(FILE72_4,"converted_track_4.72") || die "Can't find converted_track_4.72";
open(FILE72_5,"converted_track_5.72") || die "Can't find converted_track_5.72";
#
my @station61_1;
my @station61_2;
my @station61_3;
my @station61_4;
my @station61_5;
my @station61_6;
my @station61_7;
my @station61_8;
my @station61_9;
my @station61_10;
my @station61_11;
my @station61_12;
my @station61_13;
my @station61_14;
my @station61_15;
my @station61_16;
my @station61_17;
my @station61_18;
my @station61_19;
my @station61_20;
my @station61_21;
my @station61_22;
my @station61_23;
#
my @station72_1;
my @station72_2;
my @station72_3;
my @station72_4;
my @station72_5;
my @station72_6;
my @station72_7;
my @station72_8;
my @station72_9;
my @station72_10;
my @station72_11;
my @station72_12;
my @station72_13;
my @station72_14;
my @station72_15;
my @station72_16;
my @station72_17;
my @station72_18;
my @station72_19;
my @station72_20;
my @station72_21;
my @station72_22;
my @station72_23;
#
my $time;
my $output_interval = $dt * 600; # in seconds, ASSUMES ADCIRC SET TO PRODUCE OUTPUT AT RECORDING STATIONS EVERY 600 TIMESTEPS
#
# get start and end dates out of fort.22 associated with run to generate 
# desired time conversions
my $startdate; # date and time at the start of the fort.22 file
my $enddate;   # date and time at the end of the fort.22 file (for graphs)
my $forecastdate; # date and time of the forecast
my $endforecast; # the length of the final forecast period, in hours
my $startgraph;  # the earliest date and time to plot on the graph
# get model start time from fort.22 - line 1, col 3
open(F22,"fort.22") || die "Can't find raw_fort.22";
my $line = <F22>;
my @tmp = split(',',$line);
$startdate = $tmp[2]; 
while(<F22>) {
   my @tmp = split(',',$_);
   $forecastdate = $tmp[2]; # will contain the value from last line in file 
   $endforecast = $tmp[5];  # will contain the value from last line in file
}
close(F22);
# use the value provided to this script as an argument rather than the
# value in the fort.22 file, if an argument was provided 
unless ( $cst eq "" ) {
   $startdate=$cst;
}
# determine the date and time corresponding to the end of the forecast
chomp($forecastdate);
$forecastdate =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $f_year = $1;
my $f_mon = $2;
my $f_day = $3;
my $f_hour = $4;
my $f_min = 0;
my $f_sec = 0;
# also convert to CDT (UTC-5)
my ($year,$month,$day,$hour,$min,$sec) = Date::Calc::Add_Delta_DHMS($f_year,$f_mon,$f_day,$f_hour,$f_min,$f_sec,0,($endforecast-5),0,0);
$endforecast = sprintf("%s%02s%02s %02s:%02s:%02s",$year,$month,$day,$hour,$min,$sec);
print STDERR "endforecast is $endforecast CDT\n";
# determine the earliest date and time to plot (two days prior 
# to forecast) ... also convert to CDT (UTC-5)
($year,$month,$day,$hour,$min,$sec) = Date::Calc::Add_Delta_DHMS($f_year,$f_mon,$f_day,$f_hour,$f_min,$f_sec,-2,-5,0,0);
$startgraph = sprintf("%s%02s%02s %02s:%02s:%02s",$year,$month,$day,$hour,$min,$sec);
print STDERR "startgraph is $startgraph CDT\n";
#
# generate gnuplot script
if ( $watermin > -1.0 && $watermax < 3.0 ) {
   $watermin = -1.0;
   $watermax = 3.0;
} else { # allow gnuplot to autorange
   $watermin = ""; 
   $watermax = "";
}
if ( $windmax < 10.0 ) {
   $windmax = 10.0;
} else { # allow gnuplot to autorange
   $windmin = "";
   $windmax = "";
}
open(TEMPLATE,"$inputdir/template.gp") || die "ERROR: Can't open template.gp file.";

open(GNUPLOTSCRIPT,">$advisdir/autoplot.gp") || die "ERROR: Can't open $advisdir/autoplot.gp for writing gnuplot script.";
while(<TEMPLATE>) {
    s/%watermin%/$watermin/;
    s/%watermax%/$watermax/;
    s/%windmin%/$windmin/;
    s/%windmax%/$windmax/;
    s/%forecastdate%/$forecastdate/;
    s/%startgraph%/$startgraph/;
    s/%endforecast%/$endforecast/;
    print GNUPLOTSCRIPT $_;
}
close(TEMPLATE);
close(GNUPLOTSCRIPT);
#
# Make allruns.out file with converted, date formatted data
#
# extract constituent parts of date string, YYYYMMDDHH
chomp($startdate);
$startdate =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $s_year = $1;
my $s_mon = $2;
my $s_day = $3;
my $s_hour = $4;
my $s_min = 0;
my $s_sec = 0;

# initial date calculation
while (1) {
  my $count = 0;
  # track0N.61 stations
  while (<FILE61_1>) {
    chomp($_);
    #m/^\s*([\.+E\d]*)\s*([\.+E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*([\.+-E\d]*)\s*/;
    my @track1_stations=split;
    # convert to CDT (UTC-5)
    #($year,$month,$day,$hour,$min,$sec) = Date::Calc::Add_Delta_DHMS($s_year,$s_mon,$s_day,$s_hour,$s_min,$s_sec,0,-5,0,sprintf("%f",$1));
    ($year,$month,$day,$hour,$min,$sec) = Date::Calc::Add_Delta_DHMS($s_year,$s_mon,$s_day,$s_hour,$s_min,$s_sec,0,-5,0,sprintf("%f",$track1_stations[0]));
    $time = sprintf("%s%02s%02s %02s:%02s:%02s",$year,$month,$day,$hour,$min,$sec);
#    print STDERR "$time\n";
    push(@station61_1,$track1_stations[1]);
    push(@station61_2,$track1_stations[2]);
    push(@station61_3,$track1_stations[3]);
    push(@station61_4,$track1_stations[4]);
    push(@station61_5,$track1_stations[5]);
    push(@station61_6,$track1_stations[6]);
    push(@station61_7,$track1_stations[7]);
    push(@station61_8,$track1_stations[8]);
    push(@station61_9,$track1_stations[9]);
    push(@station61_10,$track1_stations[10]);
    push(@station61_11,$track1_stations[11]);
    push(@station61_12,$track1_stations[12]);
    push(@station61_13,$track1_stations[13]);
    push(@station61_14,$track1_stations[14]);
    push(@station61_15,$track1_stations[15]);
    push(@station61_16,$track1_stations[16]);
    push(@station61_17,$track1_stations[17]);
    push(@station61_18,$track1_stations[18]);
    push(@station61_19,$track1_stations[19]);
    push(@station61_20,$track1_stations[20]);
    push(@station61_21,$track1_stations[21]);
    push(@station61_22,$track1_stations[22]);
    push(@station61_23,$track1_stations[23]);
    $count++;
    last;
  }
#  print $count;
  while (<FILE61_2>) {
    chomp($_);
    my @track2_stations=split;
    push(@station61_1,$track2_stations[1]);
    push(@station61_2,$track2_stations[2]);
    push(@station61_3,$track2_stations[3]);
    push(@station61_4,$track2_stations[4]);
    push(@station61_5,$track2_stations[5]);
    push(@station61_6,$track2_stations[6]);
    push(@station61_7,$track2_stations[7]);
    push(@station61_8,$track2_stations[8]);
    push(@station61_9,$track2_stations[9]);
    push(@station61_10,$track2_stations[10]);
    push(@station61_11,$track2_stations[11]);
    push(@station61_12,$track2_stations[12]);
    push(@station61_13,$track2_stations[13]);
    push(@station61_14,$track2_stations[14]);
    push(@station61_15,$track2_stations[15]);
    push(@station61_16,$track2_stations[16]);
    push(@station61_17,$track2_stations[17]);
    push(@station61_18,$track2_stations[18]);
    push(@station61_19,$track2_stations[19]);
    push(@station61_20,$track2_stations[20]);
    push(@station61_21,$track2_stations[21]);
    push(@station61_22,$track2_stations[22]);
    push(@station61_23,$track2_stations[23]);
    $count++;
    last;
  }
#  print $count;
  while (<FILE61_3>) {
    chomp($_);
    my @track3_stations=split;
    push(@station61_1,$track3_stations[1]);
    push(@station61_2,$track3_stations[2]);
    push(@station61_3,$track3_stations[3]);
    push(@station61_4,$track3_stations[4]);
    push(@station61_5,$track3_stations[5]);
    push(@station61_6,$track3_stations[6]);
    push(@station61_7,$track3_stations[7]);
    push(@station61_8,$track3_stations[8]);
    push(@station61_9,$track3_stations[9]);
    push(@station61_10,$track3_stations[10]);
    push(@station61_11,$track3_stations[11]);
    push(@station61_12,$track3_stations[12]);
    push(@station61_13,$track3_stations[13]);
    push(@station61_14,$track3_stations[14]);
    push(@station61_15,$track3_stations[15]);
    push(@station61_16,$track3_stations[16]);
    push(@station61_17,$track3_stations[17]);
    push(@station61_18,$track3_stations[18]);
    push(@station61_19,$track3_stations[19]);
    push(@station61_20,$track3_stations[20]);
    push(@station61_21,$track3_stations[21]);
    push(@station61_22,$track3_stations[22]);
    push(@station61_23,$track3_stations[23]);
    $count++;
    last;
  }
#  print $count;
  while (<FILE61_4>) {
    chomp($_);
    my @track4_stations=split;
    push(@station61_1,$track4_stations[1]);
    push(@station61_2,$track4_stations[2]);
    push(@station61_3,$track4_stations[3]);
    push(@station61_4,$track4_stations[4]);
    push(@station61_5,$track4_stations[5]);
    push(@station61_6,$track4_stations[6]);
    push(@station61_7,$track4_stations[7]);
    push(@station61_8,$track4_stations[8]);
    push(@station61_9,$track4_stations[9]);
    push(@station61_10,$track4_stations[10]);
    push(@station61_11,$track4_stations[11]);
    push(@station61_12,$track4_stations[12]);
    push(@station61_13,$track4_stations[13]);
    push(@station61_14,$track4_stations[14]);
    push(@station61_15,$track4_stations[15]);
    push(@station61_16,$track4_stations[16]);
    push(@station61_17,$track4_stations[17]);
    push(@station61_18,$track4_stations[18]);
    push(@station61_19,$track4_stations[19]);
    push(@station61_20,$track4_stations[20]);
    push(@station61_21,$track4_stations[21]);
    push(@station61_22,$track4_stations[22]);
    push(@station61_23,$track4_stations[23]);
    $count++;
    last;
  }
#  print $count;
  while (<FILE61_5>) {
    chomp($_);
    my @track5_stations=split;
    push(@station61_1,$track5_stations[1]);
    push(@station61_2,$track5_stations[2]);
    push(@station61_3,$track5_stations[3]);
    push(@station61_4,$track5_stations[4]);
    push(@station61_5,$track5_stations[5]);
    push(@station61_6,$track5_stations[6]);
    push(@station61_7,$track5_stations[7]);
    push(@station61_8,$track5_stations[8]);
    push(@station61_9,$track5_stations[9]);
    push(@station61_10,$track5_stations[10]);
    push(@station61_11,$track5_stations[11]);
    push(@station61_12,$track5_stations[12]);
    push(@station61_13,$track5_stations[13]);
    push(@station61_14,$track5_stations[14]);
    push(@station61_15,$track5_stations[15]);
    push(@station61_16,$track5_stations[16]);
    push(@station61_17,$track5_stations[17]);
    push(@station61_18,$track5_stations[18]);
    push(@station61_19,$track5_stations[19]);
    push(@station61_20,$track5_stations[20]);
    push(@station61_21,$track5_stations[21]);
    push(@station61_22,$track5_stations[22]);
    push(@station61_23,$track5_stations[23]);
    $count++;
    last;
  }
#  print $count;
  # track0N.72 stations
  while (<FILE72_1>) {
    chomp($_);
    m/^\s*([\.+E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*/;
    push(@station72_1,$2);
    push(@station72_2,$3);
    push(@station72_3,$4);
    push(@station72_4,$5);
    push(@station72_5,$6);
    push(@station72_6,$7);
    push(@station72_7,$8);
    push(@station72_8,$9);
    push(@station72_9,$10);
    push(@station72_10,$11);
    push(@station72_11,$12);
    push(@station72_12,$13);
    push(@station72_13,$14);
    push(@station72_14,$15);
    push(@station72_15,$16);
    push(@station72_16,$17);
    push(@station72_17,$18);
    push(@station72_18,$19);
    push(@station72_19,$20);
    push(@station72_20,$21);
    push(@station72_21,$22);
    push(@station72_22,$23);
    push(@station72_23,$24);
    $count++;
    last;
  }
  while (<FILE72_2>) {
    chomp($_);
    m/^\s*([\.+E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*/;
    push(@station72_1,$2);
    push(@station72_2,$3);
    push(@station72_3,$4);
    push(@station72_4,$5);
    push(@station72_5,$6);
    push(@station72_6,$7);
    push(@station72_7,$8);
    push(@station72_8,$9);
    push(@station72_9,$10);
    push(@station72_10,$11);
    push(@station72_11,$12);
    push(@station72_12,$13);
    push(@station72_13,$14);
    push(@station72_14,$15);
    push(@station72_15,$16);
    push(@station72_16,$17);
    push(@station72_17,$18);
    push(@station72_18,$19);
    push(@station72_19,$20);
    push(@station72_20,$21);
    push(@station72_21,$22);
    push(@station72_22,$23);
    push(@station72_23,$24);
    $count++;
    last;
  }
  while (<FILE72_3>) {
    chomp($_);
    m/^\s*([\.+E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*/;
    push(@station72_1,$2);
    push(@station72_2,$3);
    push(@station72_3,$4);
    push(@station72_4,$5);
    push(@station72_5,$6);
    push(@station72_6,$7);
    push(@station72_7,$8);
    push(@station72_8,$9);
    push(@station72_9,$10);
    push(@station72_10,$11);
    push(@station72_11,$12);
    push(@station72_12,$13);
    push(@station72_13,$14);
    push(@station72_14,$15);
    push(@station72_15,$16);
    push(@station72_16,$17);
    push(@station72_17,$18);
    push(@station72_18,$19);
    push(@station72_19,$20);
    push(@station72_20,$21);
    push(@station72_21,$22);
    push(@station72_22,$23);
    push(@station72_23,$24);
    $count++;
    last;
  }
  while (<FILE72_4>) {
    chomp($_);
    m/^\s*([\.+E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*/;
    push(@station72_1,$2);
    push(@station72_2,$3);
    push(@station72_3,$4);
    push(@station72_4,$5);
    push(@station72_5,$6);
    push(@station72_6,$7);
    push(@station72_7,$8);
    push(@station72_8,$9);
    push(@station72_9,$10);
    push(@station72_10,$11);
    push(@station72_11,$12);
    push(@station72_12,$13);
    push(@station72_13,$14);
    push(@station72_14,$15);
    push(@station72_15,$16);
    push(@station72_16,$17);
    push(@station72_17,$18);
    push(@station72_18,$19);
    push(@station72_19,$20);
    push(@station72_20,$21);
    push(@station72_21,$22);
    push(@station72_22,$23);
    push(@station72_23,$24);
    $count++;
    last;
  }
  while (<FILE72_5>) {
    chomp($_);
    m/^\s*([\.+E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*([\.+-E\d]*\s*[\.+-E\d]*\s*[\.+-E\d]*)\s*/;
    push(@station72_1,$2);
    push(@station72_2,$3);
    push(@station72_3,$4);
    push(@station72_4,$5);
    push(@station72_5,$6);
    push(@station72_6,$7);
    push(@station72_7,$8);
    push(@station72_8,$9);
    push(@station72_9,$10);
    push(@station72_10,$11);
    push(@station72_11,$12);
    push(@station72_12,$13);
    push(@station72_13,$14);
    push(@station72_14,$15);
    push(@station72_15,$16);
    push(@station72_16,$17);
    push(@station72_17,$18);
    push(@station72_18,$19);
    push(@station72_19,$20);
    push(@station72_20,$21);
    push(@station72_21,$22);
    push(@station72_22,$23);
    push(@station72_23,$24);
    $count++;
    last;
  }  
  if ($count != 10) {
    die "Count is not 10";
    last;
  } else {
    my @tmp = ();
    push (@tmp,
       join(' ',@station72_1),
       join(' ',@station72_2),
       join(' ',@station72_3),
       join(' ',@station72_4),
       join(' ',@station72_5),
       join(' ',@station72_6),
       join(' ',@station72_7),
       join(' ',@station72_8),
       join(' ',@station72_9),
       join(' ',@station72_10),
       join(' ',@station72_11),
       join(' ',@station72_12),
       join(' ',@station72_13),
       join(' ',@station72_14),
       join(' ',@station72_15),
       join(' ',@station72_16),
       join(' ',@station72_17),
       join(' ',@station72_18),
       join(' ',@station72_19),
       join(' ',@station72_20),
       join(' ',@station72_21),
       join(' ',@station72_22),
       join(' ',@station72_23),
       join(' ',@station61_1),
       join(' ',@station61_2),
       join(' ',@station61_3),
       join(' ',@station61_4),
       join(' ',@station61_5),
       join(' ',@station61_6),
       join(' ',@station61_7),
       join(' ',@station61_8),
       join(' ',@station61_9),
       join(' ',@station61_10),
       join(' ',@station61_11),
       join(' ',@station61_12),
       join(' ',@station61_13),
       join(' ',@station61_14),
       join(' ',@station61_15),
       join(' ',@station61_16),
       join(' ',@station61_17),
       join(' ',@station61_18),
       join(' ',@station61_19),
       join(' ',@station61_20),
       join(' ',@station61_21),
       join(' ',@station61_22),
       join(' ',@station61_23));
    @tmp = split(" ",join(' ',@tmp)),"\n";
    print  "$time  ";
    foreach (@tmp) {
#      my $a = @tmp;
#      print STDERR "$a\n";
      printf("%20s ",$_);
    }
    print "\n";
    @station61_1 = ();
    @station61_2 = ();
    @station61_3 = ();
    @station61_4 = ();
    @station61_5 = ();
    @station61_6 = ();
    @station61_7 = ();
    @station61_8 = ();
    @station61_9 = ();
    @station61_10 = ();
    @station61_11 = ();
    @station61_12 = ();
    @station61_13 = ();
    @station61_14 = ();
    @station61_15 = ();
    @station61_16 = ();
    @station61_17 = ();
    @station61_18 = ();
    @station61_19 = ();
    @station61_20 = ();
    @station61_21 = ();
    @station61_22 = ();
    @station61_23 = ();
    @station72_1 = ();
    @station72_2 = ();
    @station72_3 = ();
    @station72_4 = ();
    @station72_5 = ();
    @station72_6 = ();
    @station72_7 = ();
    @station72_8 = ();
    @station72_9 = ();
    @station72_10 = ();
    @station72_11 = ();
    @station72_12 = ();
    @station72_13 = ();
    @station72_14 = ();
    @station72_15 = ();
    @station72_16 = ();
    @station72_17 = ();
    @station72_18 = ();
    @station72_19 = ();
    @station72_20 = ();
    @station72_21 = ();
    @station72_22 = ();
    @station72_23 = ();
  }
}

close(FILE61_1);
close(FILE61_2);
close(FILE61_3);
close(FILE61_4);
close(FILE61_5);
close(FILE72_1);
close(FILE72_2);
close(FILE72_3);
close(FILE72_4);
close(FILE72_5);

1;

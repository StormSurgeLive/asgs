#!/usr/bin/perl
#----------------------------------------------------------------
# nhc_advisory_bot.pl
#
# Parses text advisories from the National Hurricane Center and converts
# them to ATCF format for use within ADCIRC.
#
# Based on code provided by the CERA program at LSU.
#----------------------------------------------------------------
# Copyright(C) 2009 Jason Fleming
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
use strict;
use warnings;

# parse rss reports from Nat'l Hurricane Center Atlantic Marine Forecast
# Advisory.  NHC Mail (Atlantic Marine)
# <mail-storm-atlan-marine@seahorse.nhc.noaa.gov> See:
# http://www.nhc.noaa.gov/signup.shtml and select the following list: Atlantic
# Marine (Forecast/Advisories and updates ONLY)

# BASIN,CY,YYYYMMDDHH,TECHNUM,TECH,TAU,LatN/S,LonE/W,VMAX,MSLP,TY,RAD,WINDCODE,RAD1,RAD2,RAD3,RAD4,RADP,RRP,MRD,GUSTS,EYE,SUBREGION,MAXSEAS,INITIALS,DIR,SPEED,STORMNAME,DEPTH,SEAS,SEASCODE,SEAS1,SEAS2,SEAS3,SEAS4
#
#                                                                                                    1         1         1         1         1         1         1         1         1         1
#          1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9
#01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
#AL, 01, 2009052912, 03, OFCL,   0, 393N,  649W,  30, 1006, TD,  34, NEQ,    0,    0,    0,    0,    0,    0,  40,  40,   0,    ,   0, TBK,  65,  17,           ,  , 12, NEQ,  60,  60,   0,   0
my $template = "AL, 01, 2009010100,   , OFCL,   0, 000N,  000W,  30, 1012,   ,  34, NEQ,    0,    0,    0,    0,    0,    0,  40,  40,   0,    ,   0, TBK,  65,  17,           ,  , 12, NEQ,  60,  60,   0,   0";
my %month_lookup = (dummy => '00', JAN=>'01',FEB=>'02',MAR=>'03',APR=>'04',MAY=>'05',JUN=>'06',JUL=>'07',AUG=>'08',SEP=>'09',OCT=>'10',NOV=>'11',DEC=>'12');
# These are the nhc storm number and url params.
my ($adv_str, $adv_url) = ('','');
# These are the data we are extracting
my ( $date_time, $lat, $lon, $pressure, $diameter, $wind_max_sust, $wind_gust, $move_dir, $move_speed )
  = ( '', -1, -1, -1, -1, -1, -1, -1, -1, -1, '' );
my $storm_class="";
my $storm_name;
my $storm_number; 
my $adv_num;
my $storm_year;
my $forecast_year;
my $forecast_month;
my $forecast_day;
my $forecast_hour;
my $nowcast_max_wind;
my $nowcast_central_pressure;
my $atcf_line = $template;
#
my @lines =(<>);
my $body_ref = \@lines;
my $cnt = @{$body_ref};
#
my @match = ();
# Get the NHC Number
# NWS TPC/NATIONAL HURRICANE CENTER MIAMI FL   AL172005
@match = grep /AL\d{2}\d{4}$/, @{$body_ref};
if (@match) {
   if ( $match[0] =~ /AL(\d{2})(\d{4})$/ ) {
      $storm_number = $1;
      $storm_year = $2;
      printf STDOUT "STORM NUMBER: $storm_number\n";
      printf STDOUT "STORM YEAR: $storm_year\n";
   } else {
      die "Exiting: NO NHC NUMBER/YEAR";
      exit;
   }
}
my $storm_number_str = sprintf( "%02d", $storm_number );
substr($atcf_line,4,2) = $storm_number_str; 
# Date format
# 1500Z THU SEP 02 2004
# July 18th TD 2. HAS CHANGED
# NOTE NOTE NOW! 1500 UTC TUE JUL 18 2006
if ($storm_year > 2005) {
   @match = grep /^\d{4} .+ \d{4}$/, @{$body_ref};
} else {
   @match = grep /^\d{4}Z .+ \d{4}$/, @{$body_ref};
}
#
if (@match) {
   $date_time = $match[0];
   chomp $date_time;
   my @vals = split( ' ', $date_time );
   $forecast_hour = substr( $vals[0], 0, 2 );
   if($storm_year > 2005) {
      $forecast_year = $vals[5];
      $forecast_month = $month_lookup{$vals[3]};
      $forecast_day = $vals[4];
   } else {
      $forecast_year = $vals[4];
      $forecast_month = $month_lookup{$vals[2]};
      $forecast_day = $vals[3];
   }
   $date_time = $forecast_year . $forecast_month . $forecast_day . $forecast_hour; 
   printf STDOUT "Time of forecast is $date_time\n";
   substr($atcf_line,8,10) = sprintf("%10d",$date_time);
}
# advisory number does not appear in the ATCF format
@match = grep /FORECAST.+ADVISORY NUMBER/, @{$body_ref};
# HURRICANE FRANCES ADVISORY NUMBER  40
# HURRICANE FRANCES FORECAST/ADVISORY NUMBER  37
# HURRICANE FRANCES FORECAST/ADVISORY NUMBER  37...CORRECTED
# HURRICANE FRANCES SPECIAL FORECAST/ADVISORY NUMBER  37
if (@match) {
   if ( $match[0] =~ /^(.+)\s+FORECAST.+ADVISORY NUMBER\s+(\d{1,3})/ ) {
      $storm_name = $1;
      $adv_num  = $2;
      $storm_name =~ s/SPECIAL//;
   }
} else {
   @match = grep /ADVISORY NUMBER/, @{$body_ref};
   if ( $match[0] =~ /^(.+)\s+ADVISORY NUMBER\s+(\d{1,3})/ ) {
      $storm_name = $1;
      $adv_num  = $2;
   }
}
my @tmp = split(' ', $storm_name);
if ($tmp[0] eq 'HURRICANE'){
   $storm_class = $tmp[0];
   $storm_name = $tmp[1];
} elsif ($tmp[0] eq 'TROPICAL' or $tmp[0] eq 'SUBTROPICAL') { 
    # SUBTROPICAL is rare. see 2007 01
    $storm_class = "$tmp[0] $tmp[1]";
    $storm_name = $tmp[2];
}
substr($atcf_line,148,10) = sprintf("%10s",$storm_name);
my $adv_num_str = sprintf( "%02d", $adv_num );
my $adv_num_url_str = sprintf( "%03d", $adv_num );
printf STDOUT "STORM NAME: $storm_name\n";
printf STDOUT "STORM CLASS: $storm_class\n";
printf STDOUT "ADVISORY NUMBER: $adv_num_str\n";
if ( $storm_year > 2005 ) {
   $adv_url = "http://www.nhc.noaa.gov/archive/$storm_year/al$storm_number_str/al$storm_number_str$storm_year.fstadv.$adv_num_url_str.shtml";
} else {
   # 2005 url
   $adv_url = "http://www.nhc.noaa.gov/archive/$storm_year/mar/al$storm_number_str$storm_year.fstadv.$adv_num_url_str.shtml";
}
    # HURRICANE CENTER LOCATED NEAR 23.4N  73.9W AT 02/1500Z
        # or
        #TROPICAL DEPRESSION DISSIPATING NEAR 29.0N  70.0W AT 24/2100Z
    # or
    # TROPICAL DEPRESSION CENTER LOCATED NEAR 11.2N  36.0W AT 25/0300Z
@match = grep /(CENTER LOCATED|DISSIPATING) NEAR/, @{$body_ref};
my $ns_hem = "N";
my $ew_hem = "W";
if (@match) {
   if ( $match[0] =~ /CENTER LOCATED NEAR\s+(\d{1,3}\.\d{1,2})([N|S])\s+(\d{1,3}\.\d{1,2})([E|W])\s+AT/) {
      $lat = $1;
      $ns_hem = $2;
      $lon = $3;
      $ew_hem = $4;
   }
   if ( $match[0] =~ /DISSIPATING NEAR\s+(\d{1,3}\.\d{1,2})([N|S])\s+(\d{1,3}\.\d{1,2})([E|W])\s+AT/) {
      $lat = $1;
      $ns_hem = $2;
      $lon = $3;
      $ew_hem = $4;
   }
}
my $nowcast_lat = sprintf("%4d$ns_hem", $lat * 10);
my $nowcast_lon = sprintf("%4d$ew_hem", $lon * 10);
substr($atcf_line,34,5) = sprintf("%5s", $nowcast_lat);
substr($atcf_line,41,5) = sprintf("%5s", $nowcast_lon);
printf STDOUT "Nowcast position is '$nowcast_lat' '$nowcast_lon'\n";
@match = grep /^ESTIMATED MINIMUM CENTRAL PRESSURE/, @{$body_ref};
if (@match) {
   if ( $match[0] =~ /^ESTIMATED MINIMUM CENTRAL PRESSURE\s+(.+)\s+MB/ ) {
      $pressure = $1;
   }
}
substr($atcf_line,53,4) = sprintf("%4d", $pressure);
printf STDOUT "nowcast central pressure is $pressure\n";
    #MAX SUSTAINED WINDS  25 KT WITH GUSTS TO  35 KT.
    #MAX SUSTAINED WINDS 125 KT WITH GUSTS TO 155 KT.

@match = grep /^MAX SUSTAINED WINDS/, @{$body_ref};

if (@match) {
   if ( $match[0] =~ /^MAX SUSTAINED WINDS\s+(\d{1,4}) KT WITH GUSTS TO\s+(\d{1,4})/ ) {
      $wind_max_sust = $1;
      $wind_gust     = $2;
   }
}
substr($atcf_line,47,4) = sprintf("%4d",$wind_max_sust); 
printf STDOUT "nowcast max wind is $wind_max_sust\n";
my $forecast_atcf_filename = lc($storm_name) . "_advisory_" . $adv_num . ".fst";
open(ATCF,">$forecast_atcf_filename") || die "ERROR: Could not open $forecast_atcf_filename: $!"; 
# collect nowcast wind radii, if any
my $isotachs_found = 0;
my @isotachs;
for my $i (0...$#{$body_ref}) {
   if ( @{$body_ref}[$i] =~ /^MAX SUSTAINED WINDS/) {
      #64 KT....... 45NE  30SE  20SW  30NW.
      #50 KT.......120NE  75SE  60SW  75NW.
      #34 KT.......175NE 120SE 120SW 120NW.
      $i++;

      if ( @{$body_ref}[$i] =~ /^(\d{1,2}) KT\.{7}\s+(\d{1,3})[N|S][E|W]/) {   
         printf STDOUT "Found an isotach\n";
      }
      while(1) { 
         printf STDOUT "line is " . @{$body_ref}[$i] . "\n";
         if ( @{$body_ref}[$i] =~ /^(\d{1,2}) KT\.{7}\s{0,}(\d{1,3})[N|S][E|W]\s+(\d{1,3})[N|S][E|W]\s+(\d{1,3})[N|S][E|W]\s+(\d{1,3})[N|S][E|W]/) {
            $isotachs_found++;
            my @wind_radii = ( $1, $2, $3, $4, $5 );
            push @isotachs, @wind_radii;
         } else {
            last;
         }
         $i++;
      }
      printf STDOUT "found $isotachs_found isotachs\n";
      for ( my $j=$isotachs_found; $j>0; $j-- ) {
         for ( my $k=0; $k<4; $k++ ) {
            my $starting_pos = 72 + ($k * 6);
            my $list_pos = 1 + $k + ( 5 * ($j-1) ); 
            # fill in wind radii
            substr($atcf_line,$starting_pos,5) = sprintf("%5d",$isotachs[$list_pos]);
            # fill in isotach
            substr($atcf_line,63,3) = sprintf("%3d",$isotachs[5*($j-1)]);
         }
         printf ATCF "$atcf_line\n"; 
      } 
      last;
   }
}
unless ( $isotachs_found ) {
   printf ATCF "$atcf_line\n"; 
}
printf STDOUT "nowcast isotachs and wind radii are as follows:\n";
while (@isotachs) {
   my @wind_radii = pop(@isotachs);   
   printf STDOUT "$wind_radii[0]\n";
} 
get_forecasts($body_ref, $date_time, $lat, $lon, $wind_max_sust, $storm_name);
close(ATCF);
# done with advisories for 1 storm
exit;
##############################################################

# FORECAST tracks and points
###############################################################

# FORECAST VALID 10/0000Z 21.5N  84.5W
# FORECASTs have a bizzare date format: dd/hhmm  BUT near end of the month say 31/0000 the forecast dates switch to
# 01/0000, 01/12000 so we need to check for this an increment the month.

sub get_forecasts{
   my($body_ref, $date_time, $lat, $lon, $wind_max_sust, $storm_name) = @_;
   my $adv_month="3";
   my $adv_year="2129";
   my $adv_day="32";
   my $time_label="UTZ";

   # first point for the forecast_tracks is the advisory itself
   my @f_points = ();
   my $rec = {};
   $rec->{date} = $date_time;
   $rec->{lat} = $lat;
   $rec->{lon} = $lon;
   $rec->{wind_max} = $wind_max_sust;
   $rec->{wind_gust} = $wind_gust;
   push @f_points, $rec;
   for my $i (0...$#{$body_ref}) {
      my $f_month = $adv_month;
      my $f_year = $adv_year;
      if ( @{$body_ref}[$i] =~ /^FORECAST VALID/) {
         my ($f_day, $f_hour, $f_lat, $f_lon, $f_wind_max, $f_wind_gust) = (-1,-1,-1,-1, -1, -1);
         my $line = @{$body_ref}[$i];
         chomp $line;
         if ( $line =~ /^FORECAST VALID\s+(\d{2})\/(\d{4})Z/ ) {
            $f_day = $1;
            $f_hour = substr( $2, 0, 2 );
         }
         if ( $line =~ /Z\s+(\d{1,2}\.\d{1,2})N\s+(\d{1,2}\.\d{1,2})W/) {
            $f_lat = $1;
            $f_lon = $2;
         }
         # Get the next line
         # MAX WIND  30 KT...GUSTS  40 KT.
         $i++;
         $line = @{$body_ref}[$i];
         chomp $line;
         if ($line =~ /^MAX WIND\s+(\d{1,4}) KT\.\.\.GUSTS\s+(\d{1,4}) KT\./ ) {
            $f_wind_max = $1;
            $f_wind_gust = $2;
         }
         #
         # we are into next month
         # OK. Forecast may go into next month and even next year in Dec. So increment f_month and f_year
         if ( $f_day < $adv_day ) {
            if ($adv_month == 12) {
               $f_month = 1;
               $f_year++;
            } else {
               $f_month++;
            }
         }
         $f_month = sprintf("%02d", $f_month);
         my $f_date_time = $f_year . '-' . $f_month .'-' . $f_day . 'T' . "$f_hour:00";
         #my $f_dt = DateTime::Format::ISO8601->parse_datetime($f_date_time);
         #$f_date_time = $f_dt->ymd . ' ' . $f_dt->hms . ' UTC';
         #my $time_label = $f_dt->strftime("%b %e, %l %p");
         next if ($f_lat == -1 or $f_lon == -1);
         printf STDOUT "Forecast: $f_date_time $f_lat $f_lon $f_wind_max $f_wind_gust\n";
         $rec = {};
         $rec->{lat} = $f_lat;
         $rec->{lon} = $f_lon;
         $rec->{date} = $f_date_time;
         $rec->{wind_max} = $f_wind_max;
         $rec->{wind_gust} = $f_wind_gust;
         $rec->{time_label} = $time_label;
         push @f_points, $rec;
      }
   } # end for
}

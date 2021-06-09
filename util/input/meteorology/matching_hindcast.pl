#!/usr/bin/env perl
#----------------------------------------------------------------
# matching_hindcast.pl
#
# Given an NHC forecast advisory in raw or ATCF format and a 
# hindcast in ATCF ("BEST track") format, this script will truncate
# the hindcast file just before (or right on) the nowcast time.
# This is used for making test data sets. 
#
#----------------------------------------------------------------
# Copyright(C) 2009--2015 Jason Fleming
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
use Getopt::Long;
#use Date::Calc;

# BASIN,CY,YYYYMMDDHH,TECHNUM,TECH,TAU,LatN/S,LonE/W,VMAX,MSLP,TY,RAD,WINDCODE,RAD1,RAD2,RAD3,RAD4,RADP,RRP,MRD,GUSTS,EYE,SUBREGION,MAXSEAS,INITIALS,DIR,SPEED,STORMNAME,DEPTH,SEAS,SEASCODE,SEAS1,SEAS2,SEAS3,SEAS4
#
#                                                                                                    1         1         1         1         1         1         1         1         1         1
#          1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9
#01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
#AL, 01, 2009052912, 03, OFCL,   0, 393N,  649W,  30, 1006, TD,  34, NEQ,    0,    0,    0,    0,    0,    0,  40,  40,   0,    ,   0, TBK,  65,  17,           ,  , 12, NEQ,  60,  60,   0,   0
my %month_lookup = (dummy => '00', JAN=>'01',FEB=>'02',MAR=>'03',APR=>'04',MAY=>'05',JUN=>'06',JUL=>'07',AUG=>'08',SEP=>'09',OCT=>'10',NOV=>'11',DEC=>'12');
my $hindcast;
my $atcf;
my $advisory;
my $line;
my $storm_year;
my $nowcast_year;
my $forecast_year;
my $nowcast_month;
my $forecast_month;
my $nowcast_day;
my $forecast_day;
my $nowcast_hour;
my $forecast_hour;
my $date_time;
my $forecast_date_time;
my $nowcast_date_time;
GetOptions( "hindcast=s" => \$hindcast,
            "atcf=s" => \$atcf,
            "advisory=s" => \$advisory );
#
# open the hindcast file
open(HINDCAST,"<$hindcast") || die "ERROR: Failed to open hindcast file named '$hindcast': $!.";

if ( $atcf ) { 
   # open ATCF formatted forecast file
   open(ATCF,"<$atcf") || die "ERROR: Failed to open forecast ATCF file named '$atcf': $!."; 
   # grab first line from file
   my $line = <ATCF>;
   # grab nowcast datetime 
   $nowcast_date_time = substr($line,8,10);
   close(ATCF);
} else {
   # open NHC text advisory file
   open(ADVISORY,"<$advisory") || die "ERROR: Failed to open NHC text advisory file named '$advisory': $!."; 
   my @lines =(<ADVISORY>);
   my $body_ref = \@lines;
   my $cnt = @{$body_ref};
   my @match = ();
   @match = grep /AL\d{2}\d{4}$/, @{$body_ref};
   if (@match) {
      if ( $match[0] =~ /AL(\d{2})(\d{4})$/ ) {
         $storm_year = $2;
      } else {
         die "Exiting: NO NHC NUMBER/YEAR";
         exit;
      }
   }
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
      my $date_time = $match[0];
      chomp $date_time;
      my @vals = split( ' ', $date_time );
      $nowcast_hour = substr( $vals[0], 0, 2 );
      if ($storm_year > 2005) {
         $nowcast_year = $vals[5];
         $nowcast_month = $month_lookup{$vals[3]};
         $nowcast_day = $vals[4];
      } else {
         $nowcast_year = $vals[4];
         $nowcast_month = $month_lookup{$vals[2]};
         $nowcast_day = $vals[3];
      }
      $nowcast_date_time = $nowcast_year . $nowcast_month . $nowcast_day . $nowcast_hour; 
   }

}
# now read through hindcast file, writing lines to stdout as long as they 
# have datetimes that are earlier or the same as the nowcast time
while (<HINDCAST>) {
   my $hindcast_date_time = substr($_,8,10);     
   unless ( $hindcast_date_time > $nowcast_date_time ) {
      print $_;
   }
}
close(HINDCAST);

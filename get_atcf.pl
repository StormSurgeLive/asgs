#!/usr/bin/env perl
#--------------------------------------------------------------
# get_ftp_atcf.pl:
#   This script downloads the latest hindcast and forecast from
#   the NHC ftp site.
#--------------------------------------------------------------
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
use Net::FTP;
use Getopt::Long;
#  Usage:
#   perl get_ftp_atcf.pl --site ftp.site --fdir /path/to/forecast --hdir /path/to/hindcast --storm 01 --year 2006
#
my $site;
my $fdir;
my $hdir;
my $storm;
my $year;
GetOptions(
           "site=s" => \$site,
           "fdir=s" => \$fdir,
           "hdir=s" => \$hdir,
           "storm=s" => \$storm,
           "year=s" => \$year
           );

my $hindcastfile="bal".$storm.$year.".dat";
my $forecastfile="al".$storm.$year.".fst";

my $dl = 0;   # true if we were able to download both hindcast and forecast
my $hcDl = 0; # true if the hindcast was downloaded successfully
my $fcDl = 0; # true if the forecast was downloaded successfully

while (!$dl) {
   
   $hcDl = 0;
   $fcDl = 0;
  
   my $ftp = Net::FTP->new($site, Debug => 0, Passive => 1)
     or die "Cannot connect to $site: $@";

   $ftp->login("anonymous",'-anonymous@')
     or die "Cannot login ", $ftp->message;

   # HINDCAST TRACK
   $ftp->cwd($hdir)
     or die "Cannot change working directory ", $ftp->message;

   $hcDl = $ftp->get($hindcastfile)
     or print STDERR "get failed ", $ftp->message;
  
   # FORECAST TRACK
   $ftp->cwd($fdir)
     or die "Cannot change working directory ", $ftp->message;

   $fcDl = $ftp->get($forecastfile)
     or print STDERR "get failed ", $ftp->message;

   # save the advisory number by parsing the name of the file that the 
   # file points to (a hack for our ftp test rig)
   if ( $site =~ /ftp.unc.edu/ ) {
      print STDOUT "getting advisory number";
      my @advisoryDir = $ftp->dir($fdir) 
        or print STDERR "get advisoryNumber failed ", $ftp->message;
      if ( @advisoryDir ) {
         foreach my $line (@advisoryDir) {
            if ( $line =~ /$forecastfile.*advisory_(\d{2}).fst/ ) {
               open(ADVNUM,">advisoryNumber") || die "ERROR: Could not open 'advisoryNumber' to write: $!\n";
               print ADVNUM "$1\n";
               close(ADVNUM);
            }
         }
      }
   }

   $ftp->quit;

   if ( $hcDl && $fcDl ) { 
      $dl=1;
   } else {
      sleep 60;
   }
}

1;

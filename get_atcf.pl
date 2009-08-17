#!/usr/bin/env perl
#--------------------------------------------------------------
# get_atcf.pl:
#   This script downloads the latest raw hindcast and forecast from
#   the NHC.
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
use Net::HTTP;
use Getopt::Long;
#  Usage Example:
#   perl get_atcf.pl --ftpsite ftp.tpc.ncep.noaa.gov --rsssite www.nhc.noaa.gov --fdir /ftp/path/to/forecast --hdir /ftp/path/to/hindcast --storm 01 --year 2006 --adv 05 --trigger rss
#
my $ftpsite;
my $rsssite;
my $fdir;
my $hdir;
my $storm;
my $year;
my $adv;     # formatted advisory number of the previous advisory, if any
my $trigger = "rss"; # the data source used to detect a new advisory
my $nhcName; # the name given by the NHC, e.g., TWO, GUSTAV, KATRINA, etc
GetOptions(
           "rsssite=s" => \$rsssite,
           "ftpsite=s" => \$ftpsite,
           "fdir=s" => \$fdir,
           "hdir=s" => \$hdir,
           "storm=s" => \$storm,
           "year=s" => \$year,
           "adv=s" => \$adv,
           "trigger=s" => \$trigger,
           "nhcName=s" => \$nhcName
           );

my $hindcastfile="bal".$storm.$year.".dat";
my $forecastfile="al".$storm.$year.".fst";

my $dl = 0;   # true if we were able to download both hindcast and forecast
my $hcDl = 0; # true if the hindcast was downloaded successfully
my $fcDl = 0; # true if the forecast was downloaded successfully

while (!$dl) {
   $hcDl = 0;
   $fcDl = 0;
   my $ftp = Net::FTP->new($ftpsite, Debug => 0, Passive => 1)
     or die "Cannot connect to $ftpsite: $@";
   $ftp->login("anonymous",'-anonymous@')
     or die "Cannot login ", $ftp->message;
   #
   # HINDCAST TRACK
   $ftp->cwd($hdir)
     or die "Cannot change working directory ", $ftp->message;
   $hcDl = $ftp->get($hindcastfile)
     or print STDERR "get failed ", $ftp->message;
   #   
   # FORECAST TRACK
   $ftp->cwd($fdir)
     or die "Cannot change working directory ", $ftp->message;
   $fcDl = $ftp->get($forecastfile)
     or print STDERR "get failed ", $ftp->message;
   # save the advisory number by parsing the name of the file that the 
   # file points to (a hack for our ftp test rig)
   if ( $trigger eq "ftp" ) {
      if ( $ftpsite =~ /ftp.unc.edu/ ) {
         my @advisoryDir = $ftp->dir($fdir) 
           or print STDERR "get advisoryNumber failed ", $ftp->message;
         if ( @advisoryDir ) {
            foreach my $line (@advisoryDir) {
               if ( $line =~ /$forecastfile.*advisory_(\d{2}).fst/ ) {
                  open(ADVNUM,">advisoryNumber") || die "ERROR: get_atcf.pl: Could not open 'advisoryNumber' to write: $!\n";
                  print ADVNUM "$1\n";
                  close(ADVNUM);
               }
            }
         }
      }
   }

   $ftp->quit;

   # grab the name of the storm from the hindcast, if it was not provided
   # in the command line parameters
   unless ( $nhcName ) {
      if ( $hcDl ) { 
         open(HINDCAST,"<$hindcastfile") || printf STDERR "WARNING: get_atcf.pl: Could not open hindcast file.";   
         # grab the last defined name in the hindcast 
         while(<HINDCAST>) {
            my @line = split(",",$_);
            foreach my $j (@line) {
               $j =~ s/\s*//g; # remove spaces
            }
            if ( defined $line[27] ) {
               $nhcName = $line[27];
            }
         }
         close(HINDCAST);
      } else { 
         printf STDERR "ERROR: get_atcf.pl: Could not get NHC Name from hindcast, and it was not provided in the command line parameters.";
      }
   }
   my $http = Net::HTTP->new(Host => $rsssite)
     or die "Cannot connect to $rsssite: $@";
   $http->write_request(GET => "/index-at.xml", 'User-Agent' => "Mozilla/5.0");
   my ($code, $mess, %h) = $http->read_response_headers();

   my $body;
   while(1) { 
      my $buf;
      my $n = $http->read_entity_body($buf,1024);
      die "read failed: $!" unless defined $n;
      last unless $n;
      $body.=$buf
   }
   open(INDEX,">index-at.xml") || die "ERROR: get_atcf.pl: Could not open index-at.xml for writing";
   print INDEX $body;
   close(INDEX);
   my @lines=split("\n",$body); # break text into an array of lines
   my $cnt=@lines;  # count them
   my $i=0;
   my $textAdvisoryHost;
   my $textAdvisoryPath;
   my $advNum;
   my $stormFound = 0;
   # printf STDERR "INFO: get_atcf.pl: The index-at.xml file contains $cnt lines.\n";
   # loop over the body of the index file, looking for our storm
   while($i<$cnt) {
      if ( $lines[$i] =~ /$nhcName Forecast\/Advisory Number (\d{1,2})/ ) {
         # we have found the entry containing info about the latest advisory
         # for our storm
         $stormFound = 1;
         $advNum = sprintf("%02d",$1);
         # printf STDERR "INFO: get_atcf.pl Advisory '$advNum' for storm $nhcName was found in the index-at.xml file.\n";
         # compare the advisory number in the index file with the current
         # advisory number on the command line, if any
         if ( defined $adv ) {
            if ( $advNum eq $adv ) {
                #printf STDERR "INFO: get_atcf.pl: advNum is '$advNum' and adv is '$adv': Not a new advisory.\n";
               last;    # this is not a new advisory
            } else {
               printf STDOUT "$advNum";
            }
         }
         # if new advisory, grab the link to the actual text of the advisory 
         while($i<$cnt) {
            $i++;
            if ( defined $lines[$i] ) {
               if ( $lines[$i] =~ /link/ ) {
                  $lines[$i] =~ /<link>http:\/\/(.*?)\/(.*)<\/link>/;
                  $textAdvisoryHost=$1;
                  $textAdvisoryPath=$2;
                  last;
               }
               if ( $lines[$i] =~ /item/ ) {
                  printf STDERR "ERROR: get_atcf.pl: The link to the text advisory was not found.\n";
                  last;
               }
            }
         }       
         last;   
      }   
      $i++;
   }
   unless ( $stormFound ) { 
      if ( $trigger eq "rss" ) {
         printf STDERR "WARNING: get_atcf.pl: The storm named '$nhcName' was not found in the RSS feed.\n";
      }
   }
   if ( defined $textAdvisoryHost and defined $textAdvisoryPath ) {
      my $advConnect = Net::HTTP->new(Host => $textAdvisoryHost)
        or die "Cannot connect to $textAdvisoryHost: $@";
      $advConnect->write_request(GET => "/$textAdvisoryPath", 'User-Agent' => "Mozilla/5.0");
      ($code, $mess, %h) = $advConnect->read_response_headers();
      $body="";
      while(1) { 
         my $buf;
         my $n = $advConnect->read_entity_body($buf,1024);
         die "read failed: $!" unless defined $n;
         last unless $n;
         $body.=$buf
      }
      my $textAdvFile = $forecastfile . ".html";
      open(TEXTFORECAST,">$textAdvFile") || die "ERROR: get_atcf.pl: Could not open $textAdvFile for writing.";
      print TEXTFORECAST $body;
      close(TEXTFORECAST);
      $fcDl=1;
   }

   if ( $hcDl && $fcDl ) { 
      $dl=1;
   } else {
      sleep 60;
   }
}

1;

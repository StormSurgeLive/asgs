#!/usr/bin/env perl
#--------------------------------------------------------------
# get_atcf.pl:
# This script downloads the latest raw BEST and OFCL track
# files from the NHC. Alternatively, it can load these
# files from the local file system.
#--------------------------------------------------------------
# Copyright(C) 2006--2026 Jason Fleming
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
#--------------------------------------------------------------
# Removed the option to download the OFCL files via ftp due to
# deprecated support.
$^W++;
use strict;
use Net::FTP;
#use Net::HTTP;
use HTTP::Tiny;
use IO::Socket::SSL;
use Net::SSLeay;
use Getopt::Long;
use ASGSUtil;
#
my $ftpsite; # hostname for hindcast, nowcast, and/or forecast data
             # $ftpsite can also be set to "filesystem" to pick up these
             # data from the local filesystem
my $rsssite; # hostname where the RSS feed (index-at.xml) is located
             # $rsssite can also be set to "filesystem" to pick up these
             # data from the local filesystem
my $fdir;    # directory on $ftpsite for ATCF formatted forecast data
my $hdir;    # directory on $ftpsite for ATCF formatted hindcast data
my $storm;   # two digit NHC storm number
my $year;    # four digit year of the storm
my $adv;     # formatted advisory number of the previous advisory, if any
my $trigger = "rss"; # the data source used to detect a new advisory
             # can also be set to "rssembedded" to get the advisory text
             # that is embedded in the RSS xml file, rather than following
             # the external link to the text of the forecast/advisory
my $nhcName; # the name given by the NHC, e.g., TWO, GUSTAV, KATRINA, etc
my $insecure;# if set, use "http" instead of "https"
my $body;    # text of the forecast/advisory
my $advNum;  # advisory number detected from the BEST/OFCL data feeds
my $newAdvisory = 0;
GetOptions(
           "rsssite=s" => \$rsssite,
           "ftpsite=s" => \$ftpsite,
           "fdir=s" => \$fdir,
           "hdir=s" => \$hdir,
           "storm=s" => \$storm,
           "year=s" => \$year,
           "adv=s" => \$adv,
           "trigger=s" => \$trigger,
           "nhcName=s" => \$nhcName,
           "insecure" => \$insecure,
           );
#
# zero-pad storm if single digit
$storm = sprintf qq{%02d}, $storm;
my $hindcastfile="bal".$storm.$year.".dat";
my $forecastfile="al".$storm.$year.".fst";
#
my $dl = 0;   # true if we were able to download both hindcast and forecast
my $hcDl = 0; # true if the hindcast was downloaded successfully
my $fcDl = 0; # true if the forecast was downloaded successfully

while (!$dl) {
   $hcDl = 0;
   $fcDl = 0;
   my $ftp;
   #
   # OPEN FTP SESSION (if needed)
   if ( $ftpsite ne "filesystem" ) {
      $ftp = Net::FTP->new($ftpsite, Debug => 1, Passive => 1);
      unless ( defined $ftp ) {
         ASGSUtil::stderrMessage("ERROR","ftp: Cannot connect to $ftpsite: $@");
         next;
      }
      my $ftpLoginSuccess = $ftp->login("anonymous",'-anonymous@');
      unless ( $ftpLoginSuccess ) {
         ASGSUtil::stderrMessage("ERROR","ftp: Cannot login: " . $ftp->message);
         next;
      }
   }
   #
   # Verify existence of hindcast directory and BEST track file
   if ( $ftpsite eq "filesystem" ) {
      # we are getting the hindcast from the local filesystem
      if ( -e "$hdir" && -d "$hdir" && -e "$hdir/$hindcastfile" ) {
         $hcDl = 1;
      } else {
         ASGSUtil::stderrMessage("ERROR","Get '$hdir/$hindcastfile' failed: "
            . " either the '$hindcastfile' file was not found, or the local directory '$hdir' does not exist or is not a directory.");
         last;
      }
   } else {
      # we are getting the hindcast from an ftp server
      my $hcDirSuccess = $ftp->cwd($hdir);
      unless ( $hcDirSuccess ) {
         ASGSUtil::stderrMessage("ERROR",
             "ftp: Cannot change working directory to '$hdir': " . $ftp->message);
         next;
      }
      $hcDl = $ftp->get($hindcastfile);
      unless ( $hcDl ) {
        ASGSUtil::stderrMessage("ERROR","ftp: Get '$hindcastfile' failed: " . $ftp->message);
        next;
      }

   }
   #
   # grab the name of the storm from the hindcast, if it was not provided
   # in the command line parameters
   unless ( $nhcName ) {
      if ( $hcDl ) {
         my $hcOpenSuccess;
         if ( $ftpsite eq "filesystem" ) {
            $hcOpenSuccess = open(HINDCAST,"<$hdir/$hindcastfile");
         } else {
            $hcOpenSuccess = open(HINDCAST,"<$hindcastfile");
         }
         unless ($hcOpenSuccess) {
            ASGSUtil::stderrMessage("ERROR","Could not open hindcast file '$hindcastfile'.");
            next;
         }
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
         ASGSUtil::stderrMessage("ERROR","Could not get NHC Name from BEST track file '$hindcastfile' " .
            "because the download of the hindcast file '$hindcastfile' " .
            "was not successful; " .
            "the NHC Name for the storm was also not provided " .
            "in the command line parameters.");
         next;
      }
   }
   #
   # FORECAST TRACK
   if ( $trigger eq "auto" ) {
      # there is no forecast track; the forecast track file will not be written
      $advNum = "00";
      $fcDl = 1;
      $newAdvisory = 1;
      ASGSUtil::stderrMessage("DEBUG","The new advisory number is $advNum.");
      printf STDOUT "$advNum";
   }
   if ( $trigger eq "ftp" ) {
      ASGSUtil::stderrMessage("ERROR","Cannot download OFCL track files via ftp.");
      die;
   }
   if ( $ftpsite ne "filesystem" ) {
      $ftp->quit;
   }
   #
   # Now load up the text of the forecast/advisory
   if ( $trigger eq "rss" || $trigger eq "rssembedded" ) {
      # pick up the RSS feed from the local filesystem
      if ( $rsssite eq "filesystem" ) {
         if ( -e "$fdir/index-at.xml" ) {
            unless (open(FORECAST,"<$fdir/index-at.xml")) {
               ASGSUtil::stderrMessage("ERROR","Cannot open the file $fdir/index-at.xml: $!");
               next;
            }
            # stuff the lines of the forecast into a string variable;
            # this mimics what happens if we download the RSS feed from
            # the web
            $body = '';
            while (<FORECAST>) {
               $body .= $_;
            }
            close(FORECAST);
         } else {
            if ( ! -d "$fdir" ) {
               ASGSUtil::stderrMessage("ERROR","Cannot find the directory '$fdir'.");
               last;
            } else {
               ASGSUtil::stderrMessage("ERROR","Cannot find the file '$fdir/index-at.xml'.");
               last;
            }
         }
      } else {
         # pick up the RSS feed from the web
         #(my $ok, my $why) = HTTP::Tiny->can_ssl;
         #(my $ok, my $why) = $http->can_ssl();
         #ASGSUtil::stderrMessage("DEBUG","ok is $ok");
         #ASGSUtil::stderrMessage("DEBUG","why is $why");
         my %attributes = ();

         # comment here just to remind us that we're not verifying SSL even
         # if 'https' is used in productin (NOAA's URL)
         #$attributes{'verify_SSL'} = 1;

         my $http = HTTP::Tiny->new(%attributes);
         # note: only use "https:" when NOAA's production hostname is being utilized
         # this is to make testing with 'replay' servers easier by not having to
         # create an https endpoint (doable but inconvenient for testing)
         my $protocol = ($insecure) ? q{http} : q{https};

         my $url = sprintf("%s://%s/index-at.xml", $protocol, $rsssite);
         my $response = $http->get($url);

         if ( $response->{status} == 599 ) {
            ASGSUtil::stderrMessage("ERROR","Failed to download forecast/advisory.");
            printf STDERR "content: ";
            print STDERR $response->{content};
            printf STDERR "status: ";
            print STDERR $response->{status} . "\n";
            printf STDERR "reason: ";
            print STDERR $response->{reason} . "\n";
         }
         #nld empty $body to clear any old advisory numbers from the xml
         $body="";
         $body = $response->{content};
         my $indexOpenSuccess = open(INDEX,">index-at.xml");
         unless ($indexOpenSuccess) {
            ASGSUtil::stderrMessage("ERROR","Could not open index-at.xml for writing.");
            next;
         }
         print INDEX $body;
         close(INDEX);
      }
      my @lines=split("\n",$body); # break text into an array of lines
      my $cnt=@lines;  # count them
      my $i=0;
      my $textAdvisoryHost;
      my $textAdvisoryPath;
      my $stormFound = 0;
      my $linkFound = 0;
      # printf STDERR "INFO: get_atcf.pl:
      # The index-at.xml file contains $cnt lines.\n";
      # Loop over the body of the index file, looking for our storm.
      #
      # jgf20140804: The storm name may have changed in the forecast, causing
      # the storm name in the best track file to be outdated and different
      # from the storm name found here. For example, in 2014, TD ONE changed
      # to TS ARTHUR, and TD TWO changed to TS BERTHA. Therefore, we must
      # look for the advisory by storm number, not name.
      while ($i<$cnt) {
         # TROPICAL STORM BERTHA FORECAST/ADVISORY NUMBER  22
         # NWS NATIONAL HURRICANE CENTER MIAMI FL       AL032014
         # pre-2006:
         # NWS TPC/NATIONAL HURRICANE CENTER MIAMI FL       AL182005
         if ( $lines[$i] =~ /NATIONAL HURRICANE CENTER MIAMI FL\s+AL(\d{2})(\d{4})/ ) {
            if ($1 == $storm && $2 == $year && $lines[$i-1] =~ /FORECAST.ADVISORY/ ) {
               # we have found the entry containing info about the
               # latest advisory for our storm
               $stormFound = 1;
               # get the advisory number from the previous line
               $lines[$i-1] =~ /([A-Z]+) FORECAST.ADVISORY NUMBER\s+(\d{1,2})/;
               $nhcName = $1;
               $advNum = sprintf("%02d",$2);
               printf STDERR "INFO: get_atcf.pl: Advisory '$advNum' for storm '$nhcName' was found in the index-at.xml file.\n";
               # compare the advisory number in the index file with the current
               # advisory number on the command line, if any
               if ( defined $adv ) {
                  unless ( $advNum le $adv ) {
                     $newAdvisory = 1;
                     ASGSUtil::stderrMessage("DEBUG","The new advisory number is $advNum.");
                     printf STDOUT "$advNum";
                  }
               } else {
                  $newAdvisory = 1;
                  ASGSUtil::stderrMessage("DEBUG","The previous advisory number was not provided; the new advisory number is $advNum.");
                  printf STDOUT "$advNum";
               }
               #
               # reset the line number to the beginning of the NHC
               # forecast/advisory text, and grab the whole of the advisory,
               # storing it in the $body variable
               $i -= 8;
               while ($i<$cnt) {
                  $i++;
                  if ( defined $lines[$i] ) {
                     # grab the actual text of the advisory from the RSS xml
                     if ( $trigger eq "rssembedded" ) {
                        if ( $lines[$i] =~ /description/ ) {
                           $body = "";
                           while ( $lines[$i] !~ m/\/pre.+\/description>/ ) {
                              $body .= $lines[$i] . "\n";
                              $i++;
                           }
                           last;
                        }
                     } else {
                        # just grab the link to the actual text of the advisory
                        # from a webserver
                        if ( $lines[$i] =~ /<link>http:\/\/(.*?)\/(.*)<\/link>/ ) {
                           $linkFound = 1;
                           $textAdvisoryHost=$1;
                           $textAdvisoryPath=$2;
                           last;
                        }
                        # grab the full path and file name of the file that
                        # contains the actual text of the advisory
                        if ( $lines[$i] =~ /<link>(\/.*)<\/link>/ ) {
                           $linkFound = 1;
                           $textAdvisoryPath=$1;
                           last;
                        }
                        if ( $lines[$i] =~ /item/ ) {
                           ASGSUtil::stderrMessage("ERROR",
                           "http: The link to the text advisory was not found in index-at.xml.");
                           last;
                        }
                     }
                  }
               }
               last;
            }
         }
         $i++;
      }
      unless ( $stormFound ) {
         ASGSUtil::stderrMessage("ERROR","http: The storm number $storm (named '$nhcName') of $year was not found in the RSS feed.");
         #ASGSUtil::stderrMessage("DEBUG","The body of the index-at.xml file was $body.");
         next;
      }
      # if we are supposed to get the text of the forecast file from
      # link in the RSS feed, and we find no such link, we are toast
      if ( $trigger eq "rss" && !$linkFound ) {
         ASGSUtil::stderrMessage("ERROR","http: The link to the Forecast/Advisory for the storm named '$nhcName' was not found in the index file of the RSS feed.");
         next;
      }
      unless ( $newAdvisory ) {
         #nld wait a bit here to slow down repeated checks to http site ASGSUtil::stderrMessage("INFO","Napping.");
         sleep 60;
         next;
      }
      # if we are supposed to follow a link and we have successfully
      # parsed out a hostname and path, follow the corresponding link
      # to the text of the advisory
      if ( defined $textAdvisoryHost and defined $textAdvisoryPath ) {
         my $advConnect = Net::HTTP->new(Host => $textAdvisoryHost);
         unless ($advConnect) {
            ASGSUtil::stderrMessage("ERROR","http: Cannot connect to $textAdvisoryHost: $@");
            next;
         }
         my $advReqSuccess = $advConnect->write_request(
            GET          => "/$textAdvisoryPath",
            'User-Agent' => "Mozilla/5.0");
         unless ($advReqSuccess) {
            ASGSUtil::stderrMessage("ERROR",
               "http: Request for /$textAdvisoryPath failed.");
            next;
         }
         my ($code, $mess, %h) = $advConnect->read_response_headers();
         $body="";
         while(1) {
            my $buf;
            my $n = $advConnect->read_entity_body($buf,1024);
            unless ( defined $n ) {
               ASGSUtil::stderrMessage("ERROR","http: buffer read failed: $!");
               last;
            }
            last unless $n;
            $body.=$buf;
         }
      }
      # if we are supposed to get the text of the forecast advisory from
      # the local file system, and we have successfully parsed out the
      # full path and file name of the file that contains the text of the
      # advsiory, then load it up in $body to mimic what would have happened
      # if we had downloaded it via http
      if ( $rsssite eq "filesystem" and defined $textAdvisoryPath and not defined $textAdvisoryHost ) {
	     unless ( -e $textAdvisoryPath ) {
            ASGSUtil::stderrMessage("ERROR","The file containing the full text of the forecast advisory ('$textAdvisoryPath', pulled from the link in the RSS feed) does not exist.");
			next;
		 }
         unless ( open(ADVTEXT,"<$textAdvisoryPath") ) {
         	ASGSUtil::stderrMessage("ERROR","Could not open '$textAdvisoryPath' to read: $!");
            next;
         }
		 while (<ADVTEXT>) {
		    $body .= $_;
		 }
		 close(ADVTEXT);
	  }
      # now write out the file, which may contain html or other extraneous
      # data, so that the relevant data can be parsed out
      # by nhc_advisory_bot.pl
      my $textAdvFile = $forecastfile . ".html";
      my $openTxtForecastSuccess = open(TEXTFORECAST,">$textAdvFile");
      unless ($openTxtForecastSuccess) {
         ASGSUtil::stderrMessage("ERROR","Could not open $textAdvFile for writing.");
         next;
      }
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




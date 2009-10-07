#!/usr/bin/env perl
# This script will revise the input file for 
# FigureGen26 in order to plot the output for the 5 tracks 
# run in the LPFS
# RJW:08/2008

# $^W++;
# use strict;
# use Net::FTP;
# use Net::HTTP;
use Getopt::Long;
#  Usage Example:
#   perl mak_JPG.pl --outputdir $POSTPROC_DIR --gmthome $GMTHOME --storm 01 --year 2006 --adv 05 --n 30.5 --s 28.5 --e -88.5 --w -90.5 --outputprefix 01_2006_nhcconsensus_05
#
my $gmthome;
my $storm;
my $year;
my $adv;   
my $north;
my $south;
my $east;
my $west;
my $outputprefix;
my $outputdir;
GetOptions(
           "outputdir=s" => \$outputdir,
           "gmthome=s" => \$gmthome,
           "storm=s" => \$storm,
           "year=s" => \$year,
           "adv=s" => \$adv,
           "n=s" => \$north,
           "s=s" => \$south,
           "e=s" => \$east,
           "w=s" => \$west,
           "outputprefix=s" => \$outputprefix
           );

# Open up the storm track name file and loop through each storm vriation in the file
# in the loop we will define variables and strings that will be used to 
# alter the input file to Figgen26.F90
# Now would be a good place to open up the input file for FigureGen26 
#
          $lineno = 0 ;
    open( FIGGENINPUT , "< $outputdir/FigGen/FG_lpfs.inp.orig" ) || die " No input file in this directory to modify \n";
# just write a new input file
    open( FIGGENOUTPUT, "> ./FG_lpfs.new.inp" ) || die " Could not open new inputgfile to write to \n";
      while(<FIGGENINPUT>) {
              chomp;
         $inputline = $_ ;
#    print $_ ," ", $inputline,":  this is what we read in\n";
          $lineno++ ;
               $inputline_new = $inputline ;
#    print $inputline , " ", $inputline_new, "\n";
            if ( $lineno == 4 ) { 
                 $inputline_new = "$gmthome/bin/" ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 5 ) { # Temp dir name
                $tempdir = "./Temp/"   ;
                 mkdir $tempdir  || die " No Temp directory to write working files, program will crash \n";
                 $inputline_new = $tempdir    ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
                       #     tried grabbing the desired text and replacing but do not know how
                       #    $inpline = substr($_,0,50) ;  #grab the first 50 columns of the line
                       #    $inpline = $inpline_new    ;
            }
            elsif ( $lineno == 6 ) { # Label for plot file names
                  $inputline_new = $outputprefix  ;
                  $fileheader = $inputline_new ;
#                  print $inputline_new, "\n"  ;
                  print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 7 ) { # Plot title label 
                  $inputline_new = "1,HWM (NAVD88): Storm ".$storm." ".$year."  Adv #".$adv ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }

            elsif ( $lineno == 9 ) { # rewrite Lat Lon depending on view
                 $inputline_new = $west  ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 10 ) { 
                 $inputline_new = $east ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 11 ) { 
                 $inputline_new = $south ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 12 ) { 
                 $inputline_new = $north ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 23 ) { 
                 $inputline_new = "Default31.pal" ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            else {
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
      }  # end of FigGen INPUT file loop
       close FIGGENINPUT;
       close FIGGENOUTPUT;
          rename("./FG_lpfs.new.inp", "./FG_lpfs.inp");
#
# The input file is ready now lets go get (symbolic link)
# the remaining files required to run the program
#

# we have the 
# maxelev.63
# input file
          
# use test to be sure grid file exists
#        if ( -e "./fort.14" ) {
#             print "grid file is ready \n";
#         } else {
#             die " problem with input grid: fort.14\n";
#         }

# now run figgen

#        system("./FigureGen26_wline.exe") ;


# end of storm track loop

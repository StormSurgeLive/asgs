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
#   perl mak_JPG.pl --outputdir $POSTPROC_DIR --gmthome $GMTHOME --gridfile $GRIDFILE --gshome $GSHOME --storm 01 --year 2006 --adv 05 --n 30.5 --s 28.5 --e -88.5 --w -90.5 --outputprefix Particle_Tracking --starttime $STARTTIME2
#
my $gmthome;
my $gshome;
my $gridfile;
my $storm;
my $year;
my $adv;   
my $north;
my $south;
my $east;
my $west;
my $outputprefix;
my $outputdir;
my $starttime;
GetOptions(
           "outputdir=s" => \$outputdir,
           "gmthome=s" => \$gmthome,
           "gridfile=s" => \$gridfile,
           "gshome=s" => \$gshome,
           "storm=s" => \$storm,
           "year=s" => \$year,
           "adv=s" => \$adv,
           "n=s" => \$north,
           "s=s" => \$south,
           "e=s" => \$east,
           "w=s" => \$west,
           "outputprefix=s" => \$outputprefix,
           "starttime=s" => \$starttime
           );

# Open up the storm track name file and loop through each storm vriation in the file
# in the loop we will define variables and strings that will be used to 
# alter the input file to Figgen26.F90
# Now would be a good place to open up the input file for FigureGen26 
#
          $lineno = 0 ;
   open( FIGGENINPUT, "< $outputdir/FG42_template.inp.orig" ) || die " No input file in this directory to modify \n";
# just write a new input file
    open( FIGGENOUTPUT, "> ./FG42_template.inp" ) || die " Could not open new inputfile to write to \n";
      while(<FIGGENINPUT>) {
              chomp;
         $inputline = $_ ;
#    print $_ ," ", $inputline,":  this is what we read in\n";
          $lineno++ ;
               $inputline_new = $inputline ;
#    print $inputline , " ", $inputline_new, "\n";
            if ( $lineno == 4 ) { 
                 $inputline_new = "$gmthome" ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 5 ) { # Temp dir name
                 $inputline_new = "$gshome" ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 6 ) { # Temp dir name
                $tempdir = "./Temp/"   ;
                 mkdir $tempdir  || die " No Temp directory to write working files, program will crash \n";
                 $inputline_new = $tempdir    ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
                       #     tried grabbing the desired text and replacing but do not know how
                       #    $inpline = substr($_,0,50) ;  #grab the first 50 columns of the line
                       #    $inpline = $inpline_new    ;
            }
            elsif ( $lineno == 7 ) { # Label for plot file names
                  $inputline_new = $outputprefix  ;
                  $fileheader = $inputline_new ;
#                  print $inputline_new, "\n"  ;
                  print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 8 ) { # Plot title label 
                  $inputline_new = "1,HWM (NAVD88): Storm ".$storm." ".$year."  Adv #".$adv ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }

            elsif ( $lineno == 10 ) { # rewrite Lat Lon depending on view
                 $inputline_new = $west  ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 11 ) { 
                 $inputline_new = $east ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 12 ) { 
                 $inputline_new = $south ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 13 ) { 
                 $inputline_new = $north ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 14 ) { 
                 $inputline_new = $gridfile ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 24 ) { 
                 $inputline_new = "Hue_standard_01.pal" ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            elsif ( $lineno == 63 ) { 
                 $inputline_new = "1,100,3,".$starttime ;
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
            else {
                 print FIGGENOUTPUT "$inputline_new\n" ;
            }
      }  # end of FigGen INPUT file loop
       close FIGGENINPUT;
       close FIGGENOUTPUT;
#
# The input file is ready now lets go get (symbolic link)
# the remaining files required to run the program
#
          

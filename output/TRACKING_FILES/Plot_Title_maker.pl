#!/usr/bin/env perl
$^W++;
use strict;
use Date::Calc;
 use Getopt::Long;
my $cst='';      # cold start time/date formatted as yyyymmddhh24mmss
my $gmtoffset=0; # difference between GMT and local time (hours) 
                 # (assuming the ADCIRC output is in GMT)
my $fortdate='fortdate';   # name of fort.61 file
my $storm='storm';   # name of storm for title
my $kind='TC'; # kind of forcing hurricane or nam
my $type='Part'; # type of plot Elev or Part
my $partinfo='48000_pts'; # type of plot Elev or Part


GetOptions("cst=s" => \$cst,
           "gmtoffset=i" => \$gmtoffset,
           "fortdate=s" => \$fortdate,
           "storm=s" => \$storm,
           "kind=s" => \$kind,
           "type=s" => \$type,
           "partinfo=s" => \$partinfo );
#if ( $#ARGV < 0 ) {
#   printf("Usage is as follows:\n");
#   printf("export PERL5LIB=/path/to/DateCalc.pm\n");
#   printf("perl rob_date_convert.pl --cst yyyymmddhh24hhmmss --gmtoffset -5 --fortdate my_fortdate\n");  
#die;
#}
my @Fld;         # target array for lines split into fields
#######################################################################
my $startdate='';
unless ( $cst eq "" ) {
   $startdate=$cst;
}
# extract constituent parts of date string, YYYYMMDDHHMMSS
chomp($startdate);
$startdate =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/;
my $s_year = $1;
my $s_mon = $2;
my $s_day = $3;
my $s_hour = $4;
my $s_min = $5;
my $s_sec = $6;
# also convert to whatever local time is desired
my ($year,$month,$day,$hour,$min,$sec) = Date::Calc::Add_Delta_DHMS($s_year,$s_mon,$s_day,$s_hour,$s_min,$s_sec,0,$gmtoffset,0,0);
#
#######################################################################
# convert seconds in ADCIRC output to date time 
#######################################################################
my $record_count = 0;
my $time = '';
my $time2 =  0;
my $LINE = '';
my $TimeStep = 0;
my $NumRecs = 0;
my $DefVal = 0;
open(DATEFILE,$fortdate) || die "ERROR: Could not open $fortdate";
open(DATEFILET,">".$storm."_".$type."_Title.txt") || die "ERROR: Could not open date_time_transpose.";
while (<DATEFILE>) {
      chomp;
      print $_ ."\n";
       ($time2, $TimeStep, $NumRecs, $DefVal) = split;

  #    m/^\s*([^\s]*)\s*([^\s]*)\s*$/;
       print $time2."\n";

      ($year,$month,$day,$hour,$min,$sec) 
         = Date::Calc::Add_Delta_DHMS($s_year,$s_mon,$s_day,
            $s_hour,$s_min,$s_sec,0,$gmtoffset,0,sprintf("%f",$time2));
     # $time = sprintf("%s %02s %02s %02s %02s %02s",
     #           $year,$month,$day,$hour,$min,$sec);
      $time = sprintf("%s/%02s/%02s  %02s:%02s:%02s",
                $year,$month,$day,$hour,$min,$sec);
      $record_count++;
    if ( $type eq "Elev"){
     printf DATEFILET $kind." ".$storm." : Water Surface Elevation & Wind Velocity ". $time . " UTC \n"; # 
}
    if ( $type eq "Part"){
     printf DATEFILET $kind." ".$storm." : Particle Tracking (".$partinfo.")  ". $time . " UTC \n"; # 
}   

}
print STDERR "Transposed $record_count date records.\n";
close(DATEFILE);
close(DATEFILET);

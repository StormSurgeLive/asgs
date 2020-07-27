#!/usr/bin/env perl
# #!/home/thorpe/local/bin/perl -W
#
# wrapper for pqinsert an adcirc solution file into local LDM product queue
# calls 
# pqinsert_adcirc_for_scoop_sub.pl fort.61
# pqinsert_adcirc_for_scoop_sub.pl fort.63
# pqinsert_adcirc_for_scoop_sub.pl fort.64
#
# Written for : NCSCOOP
# Date: 19 Sep, 2005
# Modified: 18 Jun, 2008
#
# jgf20110622: Refactored and rearranged for NCFS2011.
#
use strict;
use warnings;
use Getopt::Long;

my ($prodID,$HSprodID,$ADCIRCgrid);
my (%RP,$k,$v);
my ($windtag,$windsrc);
my ($model, $startDate, $cycle, $year, $mon, $mday, $fullDate);
my $stormnumber = "00"; # number of storm if NWS19 vortex forcing
my $status;
my $openDAPDirectory;
our $GRIDDIR = "/shared/apps/software-data/adcircRenderTools/grids";
our $PPDIR = "/shared/apps/software-data/RenciGETools/trunk/src";
my $cmdLinePPDIR;
my $envPPDIR = $ENV{'PPDIR'};
our $OPENDAPBASEDIR="/projects/ncfs/opendap/data";
my $pathonly='';     # if this is set, write opendappath to stdout and exit
my $remote='';       # if this is set, scp files to remote location
my $InsertFile=0; 
my $DeleteFiles=0;
my $SendNotification=1;
my $CopyFile=1;
sub stderrMessage($$);
my $opendaphost; # hostname for scp'ing results 
my $remoteppdir="/projects/ncfs/apps/asgs/trunk/output"; # netcdf post proc 
# set the OpenDAP prefix:  Change this if/when we change where opendap.renci.org points.
my $openDAPPrefix;
my $sshkey;

# the new name of the run.properties file as stored in OpenDap. Set below and 
# used in the call to sendEmailMessage
my $runPropertiesFileName;

# SET the email list!
my $toList="jason.fleming\@seahorsecoastal.com";

my %types=("fort.15"         => "fort15" , 
           "fort.61"         => "statelev", 
           "fort.63"         => "elev", 
           "fort.64"         => "dvel", 
           "fort.71"         => "statatmpress", 
           "fort.72"         => "statwvel", 
           "fort.73"         => "atmpress", 
           "fort.74"         => "wvel", 
           "fort.67"         => "hots67",
           "fort.68"         => "hots",
           "maxele.63"       => "maxelev",
           "maxrs.63"        => "maxradstress",
           "maxwvel.63"      => "maxwvel",
           "minpr.63"        => "minatmpress",
           "timeofmaxele.63" => "tmaxele",
           "swan_DIR.63"     => "dir",
           "swan_DIR_max.63" => "maxdir",
           "swan_TM01.63"    => "tm01",
           "swan_TM01_max.63" => "maxtm01",
           "swan_TMM10.63"    => "tmm10",
           "swan_TMM10_max.63" => "maxtmm10",
           "swan_TPS.63"    => "tps",
           "swan_TPS_max.63" => "maxtps",
           "swan_HS.63"      => "hsign",
           "swan_HS_max.63"  => "maxhsign",
           "run.properties"  => "run.properties",
           "stationOut.tar"  => "stations");
#
my @files = qw( fort.61 fort.63 fort.64 fort.71 fort.72 fort.73 fort.74 maxele.63 maxrs.63 maxwvel.63 minpr.63 swan_HS.63 swan_HS_max.63 swan_TMM10.63 swan_TMM10_max.63 swan_TPS.63 swan_TPS_max.63 swan_DIR.63 swan_DIR_max.63 timeofmaxele.63 );
my @compressionOnlyFiles = qw( fort.15 stationOut.tar );
#
#
GetOptions(
    "ppdir=s" => \$cmdLinePPDIR,
    "remoteppdir=s" => \$remoteppdir,
    "griddir=s" => \$GRIDDIR,
    "opendapbasedir=s" => \$OPENDAPBASEDIR,
    "opendaphost=s" => \$opendaphost,
    "sshkey=s" => \$sshkey,
    "pathonly" => \$pathonly,
    "remote" => \$remote,
    "tolist=s" => \$toList,
    "insertfile" => \$InsertFile, 
    "deletefiles" => \$DeleteFiles,
    "sendnotification" => \$SendNotification,
    "copyfile" => \$CopyFile   
          );
#
if (defined $cmdLinePPDIR) {
   $PPDIR = $cmdLinePPDIR;
} elsif (defined $envPPDIR) {
   $PPDIR = $envPPDIR;
} 

if (!-e "run.properties"){
   stderrMessage("ERROR","The run.properties file was not found.");
   die;
}
if (!-e "$PPDIR/convert_adc_native_2_netCDF") {
   stderrMessage("ERROR","The executable that converts ascii output from ADCIRC to NetCDF ('$PPDIR/convert_adc_native_2_netCDF') was not found.");
   die;
}

# read run.properties file as a hash, split on ":"
%RP=&ReadFileAsHash("run.properties",":");
stderrMessage("INFO","run.properties content is:");
&PrintHash(%RP);
#
$prodID=$RP{"prodID"};
$HSprodID=$RP{"HSprodID"};
$ADCIRCgrid=$RP{"ADCIRCgrid"};
$startDate=$RP{"currentdate"};
$cycle=$RP{"currentcycle"};
$model=$RP{"Model"};
$windtag=$RP{"WindModel"};
$stormnumber=$RP{"stormnumber"};
my $coldstarttime=$RP{"ColdStartTime"};
my $runstarttime=$RP{"RunStartTime"};
#
if (!defined $prodID) {
   stderrMessage("ERROR","prodID not found in run.properties file.");
   die; 
}
if (!defined $ADCIRCgrid) {
   stderrMessage("ERROR","ADCIRCgrid not found in run.properties file."); 
   die;
}
#
$year = "20" . substr $startDate,0,2;
$mon = substr $startDate,2,2;
$mday = substr $startDate,4,2;
$fullDate = "${year}${mon}${mday}${cycle}";
$runstarttime=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
my $rsy = $1;
my $rsm = $2;
my $rsd = $3;
my $rsh = $4;
$openDAPDirectory = "$OPENDAPBASEDIR/$model/$ADCIRCgrid/$windtag/$year/$mon/$mday/$cycle";
$openDAPPrefix="http://opendap.renci.org:1935/thredds/catalog/$model/$ADCIRCgrid";
#
# write the opendap directory that the files were copied to so that we 
# know where they went
unless ( open(OPENDAPPATH,">opendappath.log") ) {
   stderrMessage("ERROR","Could not open the file opendappath.log for writing: $!.");
   die;
}
printf OPENDAPPATH "$openDAPDirectory";
close(OPENDAPPATH);
if ( $pathonly ) {
   stderrMessage("INFO","The 'pathonly' option was selected; this script will now exit.");
   exit 0; 
}
#
my @filesProcessed;
#
# if this run corresponds to a particular tropical cyclone, we will want to 
# publish the ATCF formatted hindcast and forecast data
if ( $stormnumber ne "00" ) {
   my $forecast_atcf = "../al".$stormnumber.$year.".fst";
   my $hindcast_atcf = "../bal".$stormnumber.$year.".dat";
   push(@compressionOnlyFiles,$hindcast_atcf,$forecast_atcf);
   $types{$hindcast_atcf} = "atcfhindcast";
   $types{$forecast_atcf} = "atcfforecast";
}
#
# Process hotstart files, if applicable ... we just compress them
if (!defined $HSprodID){
   stderrMessage("INFO","This is not a hotstart run.");
} else {
   my @hsFiles = qw(fort.67 fort.68);
   foreach my $hsf (@hsFiles) {
      stderrMessage("INFO","Gzipping $hsf.");
      $status=`gzip --force $hsf`;
      if ( $status ne "" ) {
         stderrMessage("ERROR","Compression of $hsf failed: $status.");
      } else {
         my $myHSProductID = $HSprodID;
         my $myHSProductType = $types{$hsf};
         $myHSProductID=~s/<field>/$myHSProductType/;
         push(@filesProcessed,$hsf.".gz",$myHSProductID.".gz");
      }
   }
}
#
# Process the standard list of files
my $myProductID = $prodID;
my $myProductType;
foreach my $file (@files) {
   $status=0;
   stderrMessage("INFO","Processing $file.");
   if (!-e $file){
      stderrMessage("INFO","$file was not found. It will not be processed.");
      next;  
   }
#   if (-e "$file.nc.gz") {
#      stderrMessage("INFO","Deleting $file.nc.gz ...");
#      system("rm -rf $file.nc.gz"); 
#   }
   #stderrMessage("DEBUG","Converting $file to $file.nc (netCDF).");
#   $status=`$PPDIR/convert_adc_native_2_netCDF -y $rsy -m $rsm -d $rsd -h $rsh $file`;   
   #
   # Additional processing of maxele.63.nc file: append inundation masks,
   # calculate and append inundation data.
   if ($file eq "maxele.63" || $file eq "fort.63" ) {
      if ( -e $file && !$remote ) {
        $status = `ncks --quiet --append $PPDIR/nc_inundation_v6b_msl-inundation-masks.nc $file.nc`; # append depth data and inundation masks
        $status = `ncap2 --overwrite -S $PPDIR/produceInundationData.scr $file.nc $file.nc`;     # calculate inundation level     
      }
   }
   #
   # compress netcdf files
   #stderrMessage("DEBUG","Gzipping $file.nc ... ");
#   $status=`gzip --force $file.nc`; 
#   if ( $status ne "" ) {
#      stderrMessage("ERROR","Compression of $file.nc failed: $status. The file $file will not be processed.");
#      next; 
#   }
   $myProductID = $prodID;
   $myProductType = $types{$file};
   $myProductID=~s/<field>/$myProductType/;
   push(@filesProcessed,$file.".nc.gz",$myProductID);
   #
   my $shpPrefix = $myProductID;
   #
   # We may want to delete the files once they are processed. 
   if ($DeleteFiles) {
       #system("rm -rf $file"); 
      system("rm -rf $file.nc.gz"); 
   }
}
#
# Process files that just get compressed.
foreach my $file (@compressionOnlyFiles) {
   $status=0;
   stderrMessage("INFO","Processing $file.");
   if (!-e $file){
      stderrMessage("INFO","$file was not found. It will not be processed.");
      next;  
   }
   $myProductID = $prodID;
   $myProductType = $types{$file};
   $myProductID=~s/<field>/$myProductType/;
   $myProductID=~s/.nc.gz//;
   stderrMessage("INFO","Gzipping $file.");
   $status=`gzip --force $file`;
   if ( $status ne "" ) {
     stderrMessage("ERROR","Compression of $file failed: $status.");
   } else {
      push(@filesProcessed,"$file.gz",$myProductID.".gz");
   }
}
#
# process run.properties file: only on the forecast.
if (-e "run.properties") {
   $myProductID = $prodID;
   $myProductType = $types{"run.properties"};
   $myProductID=~s/<field>/_$myProductType/;
   $myProductID=~s/_Z.nc.gz//;
   push(@filesProcessed,"run.properties",$myProductID);
   # Save the name for run.properties, we will need it for the mail message.
   $runPropertiesFileName = $myProductID;
}
#
# now copy and/or insert processed files as requested
my $num_files = @filesProcessed;
if ($CopyFile) {
  stderrMessage("INFO","Copying files to '$openDAPDirectory'.");
}
for (my $i=0; $i<($num_files/2); $i++ ) {
   my $to = pop(@filesProcessed);
   my $from = pop(@filesProcessed);
   if ($CopyFile) {
      if ($remote) {
         system("scp $from $opendaphost:$openDAPDirectory/$to");
         stderrMessage("INFO","Copying '$from' to '$opendaphost:$openDAPDirectory/$to'.");
      if ($from eq "maxele.63.nc.gz" || $from eq "fort.63.nc.gz" ) {
         if ( -e $from ) {
            stderrMessage("INFO","Performing remote decompression on '$opendaphost:$openDAPDirectory/$to'.");
            system("ssh $opendaphost \"gunzip $openDAPDirectory/$to\""); # decompress the data
            my $basename =substr($to,0,-3);
            stderrMessage("INFO","Performing remote append of depth data and inundation masks on '$opendaphost:$openDAPDirectory/$basename'.");
            system("ssh $opendaphost \"ncks --quiet --append $remoteppdir/FEMA_R3_draft_20110303_MSL.nc $openDAPDirectory/$basename\""); # append depth data and inundation masks
            stderrMessage("INFO","Performing remote calculation of inundation on '$opendaphost:$openDAPDirectory/$basename'.");
            system("ssh $opendaphost \"ncap2 --overwrite -S $remoteppdir/produceInundationData.scr $openDAPDirectory/$basename $openDAPDirectory/$basename\"");     # calculate inundation level     
            stderrMessage("INFO","Performing remote compression on '$opendaphost:$openDAPDirectory/$basename'.");
            system("ssh $opendaphost \"gzip --force $openDAPDirectory/$basename\""); # recompress the data
      }
   }
      } else {
         stderrMessage("INFO","Copying '$from' as '$to'.");
         system("cp $from $openDAPDirectory/$to");
      }
   }
   if ($InsertFile) {
      stderrMessage("INFO","Inserting '$from' as '$to'.");
      # &pqi($from,$to);
   }
}


# Send the email message, if configured to do so
if ($SendNotification) {
   my $subject = "ADCIRC NCFS POSTED for $fullDate";
   if ( $windtag =~ /vortex/ ) {
      $subject .= " (TROPICAL CYCLONE)"; 
   }
   my $httpPathName=$openDAPPrefix;
   $httpPathName=~s/catalog/fileServer/;
   my $message = <<END;

The ADCIRC NCFS solutions for $fullDate have been posted to $openDAPPrefix/$windtag/$year/$mon/$mday/$cycle/

The run.properties file is : $httpPathName/$windtag/$year/$mon/$mday/$cycle/$runPropertiesFileName

or wget the file with the  following command

wget  $httpPathName/$windtag/$year/$mon/$mday/$cycle/$runPropertiesFileName
END

   open(MAIL, "|/usr/sbin/sendmail -t");

   ## Mail Header
   print MAIL "To: $toList\n";
   print MAIL "Subject: $subject\n\n";
   ## Mail Body
   print MAIL "$message\n";

   close(MAIL);
}
print 0;

##########
########## private subs
##########

#
#  Prints a message to stderr, annotated with the date, the specified 
#  severity level, and the name of this script.
sub stderrMessage($$) {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: asgsConvertR3ToNETCDF.pl: $message\n";
}
#
# ReadFileAsHash reads the contents of a file into
# a perl hash.  It assumes that there are 2 fields per 
# line in the file, separated by a character (like :).
# The line is split on this char, and the left and right
# fields are put into the key and value for the hash entry.
# BOB: 28 July 2006
# %H=&ReadFileAsHash($_[0]=<filename>,$_[1]=<separator>);
sub ReadFileAsHash
{
   my ($k,$v,%H);
   # read file as a hash, split on $_[1];
   my $fname = $_[0];
#   print "reading hash file $fname\n";
   open(FIL,"<$fname");
   while (<FIL>){
      ($k,$v)=split /$_[1]/;
      chomp($k);chomp($v);
      #remove leading and trailing whitespaces
      $k =~ s/\s+//g;
      $v =~ s/\s+//g;
      $H{$k}=$v;
   }
   close(FIL);
   return %H;
}

sub PrintHash
{
   my %h=@_;
   my ($k,$v);
   while(($k,$v)=each %h){
      print sprintf("%20s => %-s\n",$k,$v);
   }
}



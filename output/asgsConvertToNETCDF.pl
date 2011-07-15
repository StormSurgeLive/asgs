#!/usr/bin/perl -W
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
my $status;
my $openDAPDirectory;
our $GRIDDIR = "/shared/apps/software-data/adcircRenderTools/grids";
our $PPDIR = "/shared/apps/software-data/RenciGETools/trunk/src";
my $cmdLinePPDIR;
my $envPPDIR = $ENV{'PPDIR'};
our $OPENDAPBASEDIR="/projects/ncfs/opendap/data";
my $InsertFile=0;
my $DeleteFiles=0;
my $SendNotification=1;
my $CopyFile=1;
sub stderrMessage($$);
# set the OpenDAP prefix:  Change this if/when we change where opendap.renci.org points.
my $openDAPPrefix;

# the new name of the run.properties file as stored in OpenDap. Set below and 
# used in the call to sendEmailMessage
my $runPropertiesFileName;

# SET the email list!
my $toList="howard\@renci.org, jason.fleming\@seahorsecoastal.com, nc.cera.renci\@gmail.com";

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
#
#
GetOptions(
    "ppdir=s" => \$cmdLinePPDIR,
    "griddir=s" => \$GRIDDIR,
    "opendapbasedir=s" => \$OPENDAPBASEDIR,
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
#stderrMessage("DEBUG","Path to executables (PPDIR) is set to ".$PPDIR.".");
#stderrMessage("DEBUG","Path to mesh file (GRIDDIR) is set to '$GRIDDIR'.");
#stderrMessage("DEBUG","Path to OPenDAP data (OPENDAPBASEDIR) is set to '$OPENDAPBASEDIR'.");

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
my $coldstarttime=$RP{"ColdStartTime"};
my $runstarttime=$RP{"RunStartTime"};
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
#stderrMessage("DEBUG","openDAPDirectory: $openDAPDirectory");
#$openDAPPrefix="http://data.disaster.renci.org:1935/thredds/catalog/$model/$ADCIRCgrid";
$openDAPPrefix="http://opendap.renci.org:1935/thredds/catalog/$model/$ADCIRCgrid";
#stderrMessage("DEBUG","openDAPPrefix: $openDAPPrefix");

unless (-e $openDAPDirectory) {
   system("mkdir -p  $openDAPDirectory"); 
}

if (!defined $prodID) {
   stderrMessage("ERROR","prodID not found in run.properties file.");
   die; 
}
if (!defined $ADCIRCgrid) {
   stderrMessage("ERROR","ADCIRCgrid not found in run.properties file."); 
   die;
}
my @filesProcessed;
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
   if (-e "$file.nc.gz") {
      stderrMessage("INFO","Deleting $file.nc.gz ...");
      system("rm -rf $file.nc.gz"); 
   }
   #stderrMessage("DEBUG","Converting $file to $file.nc (netCDF).");
   $status=`$PPDIR/convert_adc_native_2_netCDF -y $rsy -m $rsm -d $rsd -h $rsh $file`;   
   # The conversion program writes a lot of info to stdout ... can't use this
   # as a status indicator
   #if ( $status == 1 ) {
   #   stderrMessage("ERROR","Conversion program '$PPDIR/convert_adc_native_2_netCDF' returned '$status'. The file $file will not be processed."); 
   #   next;
   #}                           
   # if this is a fort.63 || fort.64 file, 
   # append netcdf version of grid to .nc file
   #   print  "Appending netcdf grid file to $fnc ...\n";
   #   if ( $f eq "fort.63" || $f eq "fort.64" || $f eq "maxele.63" || $f eq "timeofmaxele.63" ||
   #       $f eq "swan_HS.63" || $f eq "swan_HS_max.63" || 
   #       $f eq "swan_TM01.63" || $f eq "swan_TM01_max.63" || 
   #       $f eq "swan_TPS.63" || $f eq "swan_TPS_max.63" || 
   #       $f eq "swan_DIR.63" || $f eq "swan_DIR_max.63"){
   #
   #      my $temp=$ADCIRCNCGDLOC;
   #      $temp=~s/<GRIDNAME>/$ADCIRCgrid/;  
   #      die "$temp not found." if (!-e $temp);
   #      $com="$NCODIR/ncks --quiet --append $temp $fnc >& /dev/null";
   #      print "$com\n";
   #      $status=`$com`;
   #   }


   #
   # Additional processing of maxele.63.nc file: append inundation masks,
   # calculate and append inundation data.
   if ($file eq "maxele.63") {
      if ( -e "maxele.63.nc" ) {
        $status = `ncks --quiet --append $PPDIR/nc_inundation_v6b_msl-inundation-masks.nc maxele.63.nc`; # append depth data and inundation masks
        $status = `ncap2 --overwrite -S $PPDIR/produceInundationData.scr maxele.63.nc maxele.63.nc`;     # calculate inundation level     
      }
   }
   #
   # compress netcdf files
   #stderrMessage("DEBUG","Gzipping $file.nc ... ");
   $status=`gzip --force $file.nc`; 
   if ( $status ne "" ) {
      stderrMessage("ERROR","Compression of $file.nc failed: $status. The file $file will not be processed.");
      next; 
   }
   $myProductID = $prodID;
   $myProductType = $types{$file};
   $myProductID=~s/<field>/$myProductType/;
   push(@filesProcessed,$file.".nc.gz",$myProductID);
   #
   my $shpPrefix = $myProductID;
   # Some special processing for the maxele files. We'd like to
   # run the shape file generator.
   #         if ($f eq "maxele.63") {
   #            $shpPrefix =~s/<field>/$t/;
   #            $shpPrefix =~s/Z.nc.gz/shp/;
   #            my $actoCommand = "$ACTOSHAPE --nowater $gridFile maxele.63 $shpPrefix";
   #            print "actoCommand: $actoCommand\n";
   #            my $result = `$ACTOSHAPE --nowater $gridFile maxele.63 $shpPrefix`;
   #             my $shpTarFile = "$shpPrefix.tar.gz";
   #             $result = `tar cfz $shpTarFile $shpPrefix.{shp,shx,prj,dbf} `;
   #            my $shpZipFile = "$shpPrefix.zip";
   #            $result = `zip -j $shpZipFile $shpPrefix.{shp,shx,prj,dbf} `;
   
   #            if ($InsertFile) {
   #               print "Inserting $shpZipFile as $shpZipFile ... \n";
   #               &pqi("$shpZipFile",$shpZipFile);
   #            }
   
   #            if ($DeleteFiles) {
   #               system("rm -rf $shpPrefix.{shp,shx,prj,dbf}"); 
   #            }
   #         }
   # We may want to delete the files once they are processed. 
   if ($DeleteFiles) {
       #system("rm -rf $file"); 
      system("rm -rf $file.nc.gz"); 
   }
}
#
# Process fort.15 file, if it exists. This just gets compressed.
if (-e "fort.15") {
   $myProductID = $prodID;
   $myProductType = $types{"fort.15"};
   $myProductID=~s/<field>/$myProductType/;
   $myProductID=~s/.nc.gz//;
   stderrMessage("INFO","Gzipping fort.15.");
   $status=`gzip --force fort.15`;
   if ( $status ne "" ) {
     stderrMessage("ERROR","Compression of fort.15 failed: $status.");
   } else {
      push(@filesProcessed,"fort.15.gz",$myProductID.".gz");
   }
}
#
# process stations out file, if it exists ... we just compress it
if (-e "stationOut.tar") {
   $myProductID = $prodID;
   $myProductType = $types{"stationOut.tar"};
   $myProductID=~s/<field>/$myProductType/;
   $myProductID=~s/nc.gz/tar/;
   stderrMessage("INFO","Gzipping 'stationOut.tar'.");
   $status=`gzip --force stationOut.tar`; 
   if ( $status ne "" ) {
      stderrMessage("ERROR","Compression of stationOut.tar failed: $status.");
   } else {
      push(@filesProcessed,"stationOut.tar.gz",$myProductID.".gz");
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
      stderrMessage("INFO","Copying '$from' as '$to'.");
      system("cp $from $openDAPDirectory/$to");
   }
   if ($InsertFile) {
      stderrMessage("INFO","Inserting '$from' as '$to'.");
      # &pqi($from,$to);
   }
}


# Send the email message, if configured to do so
if ($SendNotification) {
   my $subject = "ADCIRC NCFS POSTED for $fullDate";
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
   printf STDERR "$theTime $level: asgsConvertToNETCDF.pl: $message\n";
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



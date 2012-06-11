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
our %RP;
my ($k,$v);
my ($windtag,$windsrc);
my ($model, $startDate, $cycle, $year, $mon, $mday, $fullDate);
my $stormnumber = "00"; # number of storm if NWS19 vortex forcing
my $status;
my $openDAPDirectory;
our $GRIDDIR = "/shared/apps/software-data/adcircRenderTools/grids";
our $PPDIR = "/shared/apps/software-data/RenciGETools/trunk/src";
my $inundationmask = "nc_inundation_v6b_msl-inundation-masks.nc";
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
my $toList="howard\@renci.org, jason.fleming\@seahorsecoastal.com, nc.cera.renci\@gmail.com, xwang25\@uncc.edu, lyu8\@uncc.edu, aidong.lu\@uncc.edu";

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

my %ids_descs;
$ids_descs{"fort.61"} = "Water Surface Elevation Stations";
$ids_descs{"fort.62"} = "Water Current Velocity Stations";
$ids_descs{"fort.63"} = "Water Surface Elevation";
$ids_descs{"fort.64"} = "Water Current Velocity";
$ids_descs{"fort.71"} = "Barometric Pressure Stations";
$ids_descs{"fort.72"} = "Wind Velocity Stations";
$ids_descs{"fort.73"} = "Barometric Pressure";
$ids_descs{"fort.74"} = "Wind Velocity";
$ids_descs{"maxele.63"} = "Maximum Water Surface Elevation";
$ids_descs{"maxvel.63"} = "Maximum Current Speed";
$ids_descs{"maxwvel.63"} = "Maximum Wind Speed";
$ids_descs{"minpr.63"} = "Minimum Barometric Pressure";
$ids_descs{"maxrs.63"} = "Maximum Wave Radiation Stress";
$ids_descs{"swan_DIR.63"} = "Mean Wave Direction";
$ids_descs{"swan_DIR_max.63"} = "Maximum Mean Wave Direction";
$ids_descs{"swan_HS.63"} = "Significant Wave Height";
$ids_descs{"swan_HS_max.63"} = "Maximum Significant Wave Height";
$ids_descs{"swan_TMM10.63"} = "Mean Wave Period";
$ids_descs{"swan_TMM10_max.63"} = "Maximum Mean Wave Period";
$ids_descs{"swan_TPS.63"} = "Peak Wave Period";
$ids_descs{"swan_TPS_max.63"} = "Maximum Peak Wave Period";

#
my @files = qw( fort.61 fort.63 fort.64 fort.71 fort.72 fort.73 fort.74 maxele.63 maxrs.63 maxwvel.63 minpr.63 swan_HS.63 swan_HS_max.63 swan_TMM10.63 swan_TMM10_max.63 swan_TPS.63 swan_TPS_max.63 swan_DIR.63 swan_DIR_max.63 timeofmaxele.63 );
my @compressionOnlyFiles = qw( fort.15 stationOut.tar );
#
#
GetOptions(
    "ppdir=s" => \$cmdLinePPDIR,
    "griddir=s" => \$GRIDDIR,
    "opendapbasedir=s" => \$OPENDAPBASEDIR,
    "insertfile" => \$InsertFile,
    "deletefiles" => \$DeleteFiles,
    "sendnotification" => \$SendNotification,
    "inundationmask=s" => \$inundationmask,
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
$stormnumber=$RP{"stormnumber"};
my $coldstarttime=$RP{"ColdStartTime"};
my $runstarttime=$RP{"RunStartTime"};
my $instancename=$RP{"instance"};
#
if ( $instancename ne "1" ) { 
   $toList="jason.fleming\@seahorsecoastal.com, nc.cera.renci2\@gmail.com";
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
if ( defined $windtag ) {
   $openDAPDirectory = "$OPENDAPBASEDIR/$model/$ADCIRCgrid/$windtag/$year/$mon/$mday/$cycle";
	if ( $instancename ne "1" ) { 
       $openDAPDirectory = "$OPENDAPBASEDIR/blueridge.renci.org:2/$model/$ADCIRCgrid/$windtag/$year/$mon/$mday/$cycle";
	}
} else {
   $openDAPDirectory = "$OPENDAPBASEDIR/$model/$ADCIRCgrid/$year/$mon/$mday/$cycle";
}
#stderrMessage("DEBUG","openDAPDirectory: $openDAPDirectory");
#$openDAPPrefix="http://data.disaster.renci.org:1935/thredds/catalog/$model/$ADCIRCgrid";
$openDAPPrefix="http://opendap.renci.org:1935/thredds/catalog/$model/$ADCIRCgrid";
if ( $instancename ne "1" ) {
   $openDAPPrefix="http://opendap.renci.org:1935/thredds/catalog/blueridge.renci.org:2/$model/$ADCIRCgrid";
}
#stderrMessage("DEBUG","openDAPPrefix: $openDAPPrefix");

unless (-e $openDAPDirectory) {
   system("mkdir -p  $openDAPDirectory");
}
# write the opendap directory that the files were copied to so that we
# know where they went
unless ( open(OPENDAPPATH,">opendappath.log") ) {
   stderrMessage("ERROR","Could not open the file opendappath.log for writing: $!.");
   die;
}
printf OPENDAPPATH "$openDAPDirectory";
close(OPENDAPPATH);

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
   if ($file eq "maxele.63" || $file eq "fort.63" ) {
      if ( -e $file ) {
         # append depth data and inundation masks
         $status = `ncks --quiet --append $PPDIR/$inundationmask $file.nc`;
         # calculate inundation level
         $status = `ncap2 --overwrite -S $PPDIR/produceInundationData.scr $file.nc $file.nc`;
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
   # update the hash that represents the run.properties file to
   # show that the ascii file has been converted to renci-netcdf, and
   # the name of the new file
   my $filenamekey = $ids_descs{$file} . " File Name";
   $RP{$filenamekey} = $myProductID;
   my $fileformatkey = $ids_descs{$file} . " Format";
   $RP{$fileformatkey} = "renci-netcdf";
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
# write out the hash that represents the run.properties file
# with the updated names and formats
&writeHashToFile("run.properties",":");
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
   if ( $windtag =~ /vortex/ ) {
      $subject .= " (TROPICAL CYCLONE)";
   }
   my $httpPathName=$openDAPPrefix;
   $httpPathName=~s/catalog/fileServer/;
   my $path_suffix;
   if ( defined $windtag ) {
      $path_suffix =  "$windtag/$year/$mon/$mday/$cycle/";
   } else {
      $path_suffix =  "$year/$mon/$mday/$cycle/";
   }
   my $message = <<END;

The ADCIRC NCFS solutions for $fullDate have been posted to $openDAPPrefix/$path_suffix

The run.properties file is : $httpPathName/$path_suffix/$runPropertiesFileName

or wget the file with the following command

wget  $httpPathName/$path_suffix/$runPropertiesFileName
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
   unless ( open(FIL,"<$fname") ) {
      stderrMessage("ERROR","Could not open the file $fname for reading: $!.");
      die;
   }
   while (<FIL>){
      ($k,$v)=split /$_[1]/;
      chomp($k);
      chomp($v);
      #$k =~ s/\s+//g;
      # remove leading and trailing whitespaces from the key
      $k =~ s/^\s+//g;
      $k =~ s/\s+$//g; 
      # remove whitespace from the value
      $v =~ s/\s+//g;
      $H{$k}=$v;
   }
   close(FIL);
   return %H;
}

sub writeHashToFile
{
   my $fname = shift;
   my $separator = shift;
   unless ( open(FIL,">$fname") ) {
      stderrMessage("ERROR","Could not open the file $fname for writing: $!.");
      die;
   }
   foreach my $key (keys(%RP)) {
      printf FIL "$key $separator $RP{$key}\n";
   }
   close(FIL)
}

sub PrintHash
{
   my %h=@_;
   my ($k,$v);
   while(($k,$v)=each %h){
      print sprintf("%20s => %-s\n",$k,$v);
   }
}



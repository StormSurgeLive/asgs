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

use strict;
use warnings;

sub sendEmailMessage {
   # This is a simple function to send an email alert that the NCFS system has produced it's results.
   # the arguments are:
   #
   #    openDAPURLPrefix: the fixed portion of the URL to the file. For example 
   #
   #        http://data.disaster.renci.org:1935/thredds/catalog/PADCSWAN/nc6b
   #
   #    propertiesFile: the name of the run.properties file AS IT APPEARS THE
   #    OPENDAP Archive. For example:
   #
   #        SPDSnc6b-UNC_WNAMAW12-NCP_20110607T0600_20110607T0700_20110610T1800_00_run.properties
   #
   #    date: the 10 digit YYYYMMDDHH date of the run
   #
   #    to: the list of users to which to send the email.
   #   

   my ($openDAPURLPrefix, $propertiesFile, $date, $to) = @_;
   my ($subject, $yyyy, $mm, $dd, $hh);

   $subject = "ADCIRC NCFS POSTED for $date";
   my $httpPathName=$openDAPURLPrefix;
   $httpPathName=~s/catalog/fileServer/;

   $yyyy = substr $date,0,4;
   $mm = substr $date,4,2;
   $dd = substr $date,6,2;
   $hh = substr $date,8,2;

   my $message = <<END;

The ADCIRC NCFS solutions for $date have been posted to $openDAPURLPrefix/$yyyy/$mm/$dd/$hh/

The run.properties file is : $httpPathName/$yyyy/$mm/$dd/$hh/$propertiesFile

or wget the file with the  following command

wget  $httpPathName/$yyyy/$mm/$dd/$hh/$propertiesFile
END

   open(MAIL, "|/usr/sbin/sendmail -t");

   ## Mail Header
   print MAIL "To: $to\n";
   print MAIL "Subject: $subject\n\n";
   ## Mail Body
   print MAIL "$message\n";

   close(MAIL);
}

sub ReadFileAsHash
#
# ReadFileAsHash reads the contents of a file into
# a perl hash.  It assumes that there are 2 fields per 
# line in the file, separated by a character (like :).
# The line is split on this char, and the left and right
# fields are put into the key and value for the hash entry.
# BOB: 28 July 2006
# %H=&ReadFileAsHash($_[0]=<filename>,$_[1]=<separator>);
#
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


our $GRIDDIR;
our $PPDIR;
our $defLib;
our $OPENDAPBASEDIR="/projects/ncfs/opendap/data";

BEGIN {
   $PPDIR = $ENV{'PPDIR'};
   if (!defined($PPDIR) or !$PPDIR) {
      $PPDIR = "/shared/apps/software-data/RenciGETools/trunk/src";
      print "Using default PPDIR $PPDIR\n";
   } else {
      print "Using user PPDIR $PPDIR\n";
   }
   $GRIDDIR = "/shared/apps/software-data/adcircRenderTools/grids";
   print "Using GRIDDR $GRIDDIR\n";
}

# SET the email list!
my $toList="howard\@renci.org, jason.fleming\@seahorsecoastal.com, nc.cera.renci\@gmail.com";
# set the OpenDAP prefix:  Change this if/when we change where opendap.renci.org points.
my $openDAPPrefix="http://data.disaster.renci.org:1935/thredds/catalog/PADCSWAN/nc6b";

# the new name of the run.properties file as stored in OpenDap. Set below and 
# used in the call to sendEmailMessage
my $runPropertiesFileName;

my ($prodID,$HSprodID,$ADCIRCgrid);
my (%RP,$k,$v);
my ($windtag,$windsrc,$date1,$date2,$date3,$field);
my ($fname,$fname_no_nc,$fpath,$fsuffix,$fort,$fieldn,$Fsuffix);
my ($NDSETS, $NP, $DT_NSPOOLG, $NSPOOLG, $IRTYPE);
my ($dd,$mm,$yy,$dd2,$mm2,$yy2);
my ($model, $startDate, $cycle, $year, $mon, $mday, $fullDate);
my ($com,$status);
my (@files,@pids);
my $openDAPDirectory;

my %types=("fort.15"         => "fort15" , 
           "fort.61"         => "statelev", 
           "fort.63"         => "elev", 
           "fort.64"         => "dvel", 
           "fort.67"         => "hots67",
           "fort.68"         => "hots",
           "maxele.63"       => "maxelev",
           "timeofmaxele.63" => "tmaxele",
           "swan_DIR.63"     => "dir",
           "swan_DIR_max.63" => "maxdir",
           "swan_TM01.63"    => "tm01",
           "swan_TM01_max.63" => "maxtm01",
           "swan_TPS.63"    => "tps",
           "swan_TPS_max.63" => "maxtps",
           "swan_HS.63"      => "hsign",
           "swan_HS_max.63"  => "maxhsign",
           "run.properties"  => "run.properties",
           "stationOut.tar"  => "stations");
my $HSrun=1;
my $InsertFile=0;
my $DeleteFiles=0;
my $SendNotification=0;
my $CopyFile=1;

if (!-e "run.properties"){
    print  "run.properties file not found.  Cannot insert without it.\n";
    print 1;exit 1;}

# read run.properties file as a hash, split on :
%RP=&ReadFileAsHash("run.properties",":");
print "\nrun.properties Hash is: \n";
&PrintHash(%RP);

$prodID=$RP{"prodID"};
$HSprodID=$RP{"HSprodID"};
$ADCIRCgrid=$RP{"ADCIRCgrid"};
$startDate=$RP{"currentdate"};
$cycle=$RP{"currentcycle"};
$model=$RP{"Model"};
$year = "20" . substr $startDate,0,2;
$mon = substr $startDate,2,2;
$mday = substr $startDate,4,2;
$fullDate = "${year}${mon}${mday}${cycle}";
$openDAPDirectory = "$OPENDAPBASEDIR/$model/$ADCIRCgrid/$year/$mon/$mday/$cycle";
print "openDAPDirectory: $openDAPDirectory\n";

unless (-e $openDAPDirectory) {
   system("mkdir -p  $openDAPDirectory"); 
}

die "prodID not found in run.properties file."  if (!defined $prodID);
die "ADCIRCgrid not found in run.properties file."  if (!defined $ADCIRCgrid);

# this is the order in which the files will get processed/inserted
push(@files,"maxele.63");
push(@pids,$prodID);
push(@files,"timeofmaxele.63");
push(@pids,$prodID);
push(@files,"fort.15");
push(@pids,$prodID);
push(@files,"fort.61");
push(@pids,$prodID);
push(@files,"fort.63");
push(@pids,$prodID);
push(@files,"fort.64");
push(@pids,$prodID);
push(@files,"swan_HS.63");
push(@pids,$prodID);
push(@files,"swan_HS_max.63");
push(@pids,$prodID);
push(@files,"swan_TM01.63");
push(@pids,$prodID);
push(@files,"swan_TM01_max.63");
push(@pids,$prodID);
push(@files,"swan_TPS.63");
push(@pids,$prodID);
push(@files,"swan_TPS_max.63");
push(@pids,$prodID);
push(@files,"swan_DIR.63");
push(@pids,$prodID);
push(@files,"swan_DIR_max.63");
push(@pids,$prodID);
push(@files,"fort.67");
push(@pids,$HSprodID);
push(@files,"fort.68");
push(@pids,$HSprodID);
push(@files,"stationOut.tar");
push(@pids,$prodID);
push(@files,"run.properties");
push(@pids,$prodID);

if (!defined $HSprodID){
   print "This is not a hotstart run.\n" ;
   $HSrun=0;
}

my $pp;
for (my $i=0;$i<=$#files;$i++){
   $status=0;

   my $f=$files[$i];
   my $p=$pids[$i];
   my $t=$types{$f};
   
   print "\nProcessing $f ... \n";

   if ($f eq "fort.61" || 
       $f eq "fort.63" || 
       $f eq "fort.64" || 
       $f eq "maxele.63" || 
       $f eq "swan_HS.63" || 
       $f eq "swan_HS_max.63" || 
       $f eq "swan_TM01.63" || 
       $f eq "swan_TM01_max.63" || 
       $f eq "swan_TPS.63" || 
       $f eq "swan_TPS_max.63" || 
       $f eq "swan_DIR.63" || 
       $f eq "swan_DIR_max.63" || 
       $f eq "timeofmaxele.63" ){
      if (!-e $f){
         print "Skipping $f.  DNE.\n";
      }
      else{
         if (-e "$f.nc.gz"){
            print "Deleting $f.nc.gz ...\n";
            system("rm -rf $f.nc.gz"); 
         }

         print "Converting $f to $f.nc (netCDF) ...\n";
         &conv2netcdf($f);

         print "Gzipping $f.nc ... \n";
         $status=`gzip --force $f.nc`;

         $pp=$p;
         $pp=~s/<field>/$t/;
         if ($CopyFile) {
            print "copying $f.nc.gz as $pp ... \n";
            system("cp $f.nc.gz $openDAPDirectory/$pp");
         }

         if ($InsertFile) {
            print "Inserting $f.nc.gz as $pp ... \n";
         }
         my $shpPrefix = $p;
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
            system("rm -rf $f"); 
            system("rm -rf $f.nc.gz"); 
         }
      }
   } else {

       # insert hotstart file, if
#      if ($HSrun && ($f eq "fort.67" || $f eq "fort.68")){
       if ($HSrun && $f eq "fort.68"){
         print "Gzipping $f ... \n";
         $status=`gzip --force $f`;
         if ($InsertFile) {
            print "Inserting $f.gz as $p.gz ...\n";
#           &pqi("$f.gz","$p.gz");
         }

         if ($CopyFile) {
            print "copying $f.gz as $p.gz ... \n";
            system("cp $f.gz $openDAPDirectory/$p.gz");
         }
      }

      # insert fort.15 file, if it exists...
      if ($f eq "fort.15"){
         if (-e $f){
            $pp=$p;
            $pp=~s/<field>/$t/;
            $pp=~s/.nc.gz//;
            print "Gzipping $f ... \n";
            $status=`gzip --force $f`;
            if ($InsertFile) {
               print "Inserting $f.gz as $pp.gz ...\n";
#              &pqi("$f.gz","$pp.gz");
            }

            if ($CopyFile) {
               print "copying $f.gz as $pp.gz ... \n";
               system("cp $f.gz $openDAPDirectory/$pp.gz");
            }
         }
      }

      # insert stations out file, if it exists...
      if ($f eq "stationOut.tar"){
         if (-e $f){
            $pp=$p;
            print "Substituting into $pp ... \n";
            $pp=~s/<field>/$t/;
            $pp=~s/nc.gz/tar/;
            print "Gzipping $f ... \n";
            $status=`gzip --force $f`;
            if ($InsertFile) {
               print "Inserting $f.gz as $pp.gz ...\n";
#              &pqi("$f.gz","$pp.gz");
            }

            if ($CopyFile) {
               print "copying $f.gz as $pp.gz ... \n";
               system("cp $f.gz $openDAPDirectory/$pp.gz");
            }
         }
      }

      # run.properties file: only on the forecast.
      if ($f eq "run.properties"){
         if (-e $f){
            $pp=$p;
            print "Substituting into $pp ... \n";
            $pp=~s/<field>/_$t/;
            $pp=~s/_Z.nc.gz//;
            if ($InsertFile) {
               print "Inserting $f as $pp ...\n";
#              &pqi("$f","$pp");
            }

            if ($CopyFile) {
               print "copying $f as $pp ... \n";
               system("cp $f $openDAPDirectory/$pp");

               # Save the name for run.properties, we will need it for the mail message.
               $runPropertiesFileName = $pp;
            }
         }
      }
   }
}

# Send the email message
sendEmailMessage($openDAPPrefix, $runPropertiesFileName, $fullDate, $toList);

print 0;
exit 0;

##########
########## private subs
##########

sub conv2netcdf{
   my $f=$_[0];
   my $fnc=$f;
   $fnc.=".nc";
#   return;
   my $com="$PPDIR/convert_adc_native_2_netCDF";

   die "Cant find exec $com." if (!-e $com);   
   $status=`$com $f`;                               

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
}

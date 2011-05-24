#!/usr/bin/perl -w
################################################################################
#                          	netcdfNAMtoOWI                                 #
################################################################################
# This script is triggered when a netCDF NAM file is received by LDM.
# Its purpose is to convert the NAM data to OWI formatted data for use in
# ADCIRC.
################################################################################
#                   The packages used are the following:                       #
################################################################################
#NetCDF                                                                        #
#ArraySub								       #	
#Getopt									       #
################################################################################
#written by Eve-Marie Devaliere for UNC/FRF                                    #
#additions for grib2 input and command line arguments by Jason Fleming for ASGS#
#first written: 03/17/10                                                       #
#last updated: 03/31/10                                            	       #
################################################################################
#
# Example of usage for a set of grib2 files containing nowcast data:
#
# perl ~/asgs/NAMtoOWI.pl --ptFile ~/Ida/ptFile.txt --namFormat grib2 --namType nowcast --awipGridNumber 218 --dataDir ~/Ida --outDir ~/Ida/test/ --velocityMultiplier 0.9 --scriptDir ~/asgs
#
######################################################
#      Packages and exportation requirements         #
######################################################
use strict;
no strict 'refs';
#use NetCDF;
use ArraySub;
use Getopt::Long;
use Date::Pcalc;
######################################################
#             Variables declarations                 #
######################################################
our $dataDir="/data/renci/ADCIRC/wind/";    # path to NAM data
our $outDir='/data/renci/ADCIRC/wind/src/'; # path to NAM u,v,p text data 
our $outFilename='uvp.txt';                 # filename of NAM u,v,p text data
our $ptFile='ptFile.txt';                   # file name of output grid lat/lon
our $fort22='fort.22';			    # fort.22 path and filename
our $awipGridNumber=221;                    # code that describes the grid num
our $velocityMultiplier=1.0;                # multiplier for vels
our $pressureMultiplier=0.01;               # convert Pascals to mb by default
our $scriptDir=".";                        # path to executables
our ($wndFile,$presFile);                   # names of OWI wind/pre output files
our @namFormats = qw(grb grib2 netCDF);   # accceptable file types for NAMdata
our $namFormat = "netCDF";                  # default NAM format is netCDF
our $namType = "forecast";                  # expect forecast data by default
our ($nDims,$nVars,$nAtts,$recDim,$dimName,%varId,@dimIds,$name,$dataType,%data,%dimId,%nRec,$nRec,@ugrd,@vgrd,@atmp,@time,@OWI_wnd,@miniOWI_wnd,@OWI_pres,@miniOWI_pres,@zeroOffset,$geoHeader);
our ($OWItimeRef,$startTime,$endTime,$timeStep,$mainHeader,@miniUgrd,@miniVgrd,@miniAtmp,@OWItime,$recordLength);
# @ugrd holds the lambert gridded u wind velocity data, across all time steps 
# @vgrd holds the lambert gridded v wind velocity data, across all time steps 
# @atmp holds the lambert gridded atm. pressure data, across all time steps 
# $startTime and $endTime are date/time strings for main OWI header
# $timeStep is the met time increment (WTIMINC), in hours
# $mainHeader is at the top of the OWI files
# @OWItime is the time in the header of each met data set (i.e., incl. minutes)
# $recordLength is the number of grid points in the lambert gridded data
#
######################################################
#                    Main Program                    #
######################################################
# get command line options for variables
GetOptions(
          "dataDir=s" => \$dataDir,
          "outDir=s" => \$outDir,
          "outFilename=s" => \$outFilename,
          "ptFile=s" => \$ptFile,
          "namFormat=s" => \$namFormat,
          "namType=s" => \$namType,
          "awipGridNumber=s" => \$awipGridNumber,
          "velocityMultiplier=s" => \$velocityMultiplier,
          "pressureMultiplier=s" => \$pressureMultiplier,
          "scriptDir=s" => \$scriptDir
         );
&stderrMessage("INFO","Started processing NAM data.");
&stderrMessage("INFO","Started processing point file.");
$geoHeader=&processPtFile($ptFile);
# load NAM data
if ( ($namFormat eq "grib2") || ($namFormat eq "grb") ) 
	{
	&stderrMessage("INFO","Processing file(s).");
	&getGrib2($namType);
	&addToFort22();# have to add the record length to fort.22
	&stderrMessage("INFO","Rotate and format each time-step.");
	# loop through the time-steps to run awips_interp	
	&rotateAndFormat();
	}
elsif ( $namFormat eq "netCDF" )
	{
	#first get all the variables ids and dimensions ids from the netCDF file
	&stderrMessage("INFO","Process netCDF file.");
#	&getNetCDF();
	&addToFort22();# have to add the record length to fort.22
	&stderrMessage("INFO","Rotate and format each time-step.");
	# loop through the time-steps to run awips_interp	
	&rotateAndFormat();
	}
&stderrMessage("INFO","Print OWI files.");
&printOWIfiles();
&stderrMessage("INFO","Done processing NAM data.");
######################################################
#                    Subroutines                     #
######################################################


################################################################################
# NAME: &printDate
# CALL: &printDate($message)
# GOAL: print the given message along with the date/time
################################################################################
sub printDate()
{
	my $message=shift;
	my @time=localtime(time);
	my $second = $time[0];
	my $minute = sprintf "%2.2d",$time[1];
	my $hour = sprintf "%2.2d",$time[2];
	my $day = sprintf "%2.2d",$time[3];
	
	my $month = sprintf "%2.2d",(1+$time[4]);
	
	my $year = sprintf "%4.4d",($time[5]+1900);
	print "$message at $month/$day/$year $hour:$minute:$second \n";
}

################################################################################
# NAME: &processPtFile
# CALL: &processPtFile($file)
# GOAL: process the 'point file' aka 'lat/lon file' used by awips_interp to know
# where to output data by extracting grid information
################################################################################
sub processPtFile
{
	my $ptFile=shift;

	my (@lat,@lon,$null,@ary,$swLat,$swLon,@uniqLat,@uniqLon,$nLat,$nLon,$nPts,%seen,@uniqLatSorted,@uniqLonSorted,$dx,$dy);
        # check for existence of lat/lon file before attempting to open
        unless (-e $ptFile ) 
        { 
                 &stderrMessage("ERROR","Grid specification file '$ptFile' does not exist.");
                 die;
        }       
	open(PT,$ptFile);
	my @pt=<PT>;
	chomp(@pt);
	close (PT);
	$nPts=@pt;
	for my $i (1 .. $nPts-1)#starts at 1 to skip the first line (number of pts)
	{
		($null,$lon[$i-1],$lat[$i-1])=split/,/,$pt[$i];
	}
	# find SW lat and lon using min
	($swLat,$null)=&giveMinArray(\@lat);
	($swLon,$null)=&giveMinArray(\@lon);
	#get rid of remaining space (may be important for format of header line)
	$swLat=~m/(\S+)/;
	$swLat=sprintf("%3.4f",$1);# first to have the right float format
	$swLat=sprintf("%8s",$swLat);# then to have the right spacing in the file
	$swLon=~m/(\S+)/;
	$swLon=sprintf("%3.4f",$1);
	$swLon=sprintf("%8s",$swLon);
	# find the uniques lat and lon
	%seen=();
	foreach my $item (@lat)
	{
		push (@uniqLat,$item) unless $seen{$item}++;
	}
	$nLat=@uniqLat;
	%seen=();
	foreach my $item (@lon)
	{
		push (@uniqLon,$item) unless $seen{$item}++;
	}
	$nLon=@uniqLon;
	@uniqLatSorted=sort(@uniqLat);
	@uniqLonSorted=sort(@uniqLon);
	# assume a constant dx/dy - abs in case of negative values...
	$dx=sprintf("%1.4f",abs($uniqLonSorted[1]-$uniqLonSorted[0]));
	$dy=sprintf("%1.4f",abs($uniqLatSorted[1]-$uniqLatSorted[0]));
	$dx=sprintf("%6s",$dx);
	$dy=sprintf("%6s",$dy);
	$nLat=sprintf("%4s",$nLat);
	$nLon=sprintf("%4s",$nLon);
	my $headerLine="iLat=$nLat"."iLong=$nLon"."DX=$dx"."DY=$dy"."SWLat=$swLat"."SWLon=$swLon";
	#print "headerline=$headerLine\n";
	return $headerLine;
	
}
################################################################################
# NAME: &convertTime
# CALL: &convertTime(\@time);
# GOAL: convert the netCDF epoch time to the OWI format time requirement
################################################################################
sub convertTime
{
	my $timeRef=shift;
	my @time=@$timeRef;
	my $timeStep;
	my $nTimes=@time;
	my $count=0;
	my (@OWItime,$startTime,$endTime);
	foreach my $tim (@time)
	{
		 my($sec,$min,$hour,$mday,$mon,$year)=gmtime($tim);
		 $mon++; # because the range is 0->11
		$year=$year+1900; #because 1900 has been substracted before
		#to have the date returned formatted
		$sec=sprintf "%2.2d",$sec;
		$min=sprintf "%2.2d",$min;
		$hour=sprintf "%2.2d",$hour;
		$mday=sprintf "%2.2d",$mday;
		$mon=sprintf "%2.2d",$mon;
		$year=sprintf "%4.4d",$year;
		if ($count==0)
		{
			$startTime=$year.$mon.$mday.$hour;
		}
		if ($count==$nTimes-1)
		{
			$endTime=$year.$mon.$mday.$hour;
		}
		$OWItime[$count]=$year.$mon.$mday.$hour.$min;
		$count++;
	}
	# calculate time-step - assumed constant
	$timeStep=($OWItime[1]-$OWItime[0])/100;# in hours
	#print "timeSte==$timeStep \n";
	return (\@OWItime,$startTime,$endTime,$timeStep);
                                           
}
################################################################################
# NAME: &toOWIformat
# CALL: &toOWIformat($file,$header);
# GOAL: convert the file $file to OWI format (for one TS) with the header $header
################################################################################
sub toOWIformat
{
	my ($file,$header)=@_;
	push @OWI_wnd,$header;
	push @OWI_pres,$header;
        # check for existence of the data file before attempting to open
        unless (-e $file ) 
        { 
            &stderrMessage("ERROR","The data file '$file' does not exist.");
            die;
        }       
	open (FIL,$file);
	my @file=<FIL>;
	close(FIL);
	my (@ugrd,@vgrd,@atmp,@uLines,@vLines,@pLines,$uStr,$vStr,$pStr);
	undef ($uStr);
	undef ($vStr);
	undef ($pStr);
	my $count=0;
	my $null;
	foreach my $line (@file)
	{
		($null,$null,$ugrd[$count],$vgrd[$count],$atmp[$count])=split/\s+/,$line;#2 nulls bc starts with space and don't need index number
		#print "u,v,p=$ugrd[$count],$vgrd[$count],$atmp[$count]\n";
		$count++;
	}
	my $nTot=@ugrd-1;
	my $miniCount=0;
	for my $i (0 .. $nTot)# can do u, v and p at the same time
	{
		my $u=sprintf("% 10f",$ugrd[$i]);
		my $v=sprintf("% 10f",$vgrd[$i]);
                # change from Pascal to millibar while at it
                # needed to add sprintf %10s for pressure vals < 1000
                my $p=sprintf("%10s",
                    sprintf("% 4.4f",$atmp[$i]*$pressureMultiplier));
		if (defined($uStr))
		{
			$uStr=$uStr."$u";# concatenate values together
			$vStr=$vStr."$v";
			$pStr=$pStr."$p";
		}
		else
		{
			$uStr=$u;# concatenate values together
			$vStr=$v;
			$pStr=$p;
		}
		if (($miniCount==7) || ($i==$nTot))# 8 values per line or reach the end of the file
		{
			$miniCount=0;
			push @uLines,$uStr;
			push @vLines,$vStr;
			push @pLines,$pStr;
			undef ($uStr);
			undef ($vStr);
			undef ($pStr);
		}
		else
		{
			$miniCount++;
		}
	}
	# push the lines in the array representing each file
	push @OWI_wnd,@uLines;
	push @OWI_wnd,@vLines;
	push @OWI_pres,@pLines;
}
################################################################################
# NAME: &printOWIfiles
# CALL: &printOWIfiles
# GOAL: print the .wnd and .pre OWI files from their corresponding arrays
################################################################################
sub printOWIfiles
{
	open(WND,'>'.$wndFile);
	foreach my $line (@OWI_wnd)
	{
		print WND $line."\n";
	}
	close(WND);
	open(PRE,'>'.$presFile);
	foreach my $line (@OWI_pres)
	{
		print PRE $line."\n";
	}
	close(PRE);
}
################################################################################
# NAME: &getNetCDF
# CALL: &getNetCDF
# GOAL: get the u,v,p data and time info from the netCDF file
################################################################################
sub getNetCDF
{
	my $filename;
	 # check to be sure that the NetCDF file was specified on the command
        # line before attempting to open
        unless ( defined $ARGV[0] ) 
        {
            &stderrMessage("ERROR","NetCDF filename was not specified on command line.");
                 die;
        }
        # check for existence of netcdf file before attempting to open
        unless (-e $ARGV[0] ) 
        { 
                 &stderrMessage("ERROR","NetCDF file '$ARGV[0]' does not exist.");
                 die;
        } 
	if ($ARGV[0] =~/.gz$/)
	{
		`gunzip $ARGV[0]`;
		$filename=$`;
	}
	else
	{
		$filename=$ARGV[0];
	}
#	my $ncid = NetCDF::open($filename,NetCDF::NOWRITE) or die "can't open file $ARGV[0], error $! \n";
	#NetCDF::inquire($ncid,\$nDims,\$nVars,\$nAtts,\$recDim);
	#print "ndims=$nDims  nVar=$nVars, natt=$nAtts, recDim=$recDim\n";
	 for my $var (0 .. $nVars-1)# var ids are 0, 1 and 2 if we have 3 variables
	 {
	#	 NetCDF::varinq($ncid,$var,\$name,\$dataType,$nDims,\@dimIds,\$nAtts);
		 my $dimID=splice (@dimIds, 0, $nDims);
		# print "VAR: $var: NAME: $name, DATA TYPE: $dataType, NDIMS: $nDims, DIMIDS: $dimID, NATTS: $nAtts\n";
		 $varId{$name} = $var; # array of variable ID numbers as a function of variable name
		 $dimId{$name}=$dimID;
	 }
	# get x,y,time dimensions
	#NetCDF::diminq($ncid, $dimId{'x'},$dimName,$nRec); 
	$nRec{'x'}=$nRec;
	#NetCDF::diminq($ncid, $dimId{'y'},$dimName,$nRec);
	$nRec{'y'}=$nRec;
	#NetCDF::diminq($ncid, $dimId{'time'},$dimName,$nRec);
	$nRec{'time'}=$nRec;
	
	# get the time
	#NetCDF::varget($ncid, $varId{'time'},0,$nRec{'time'},\@time);
	($OWItimeRef,$startTime,$endTime,$timeStep)=&convertTime(\@time);
	#$mainHeader="Oceanweather WIN/PRE Format					$startTime   $endTime";
	$mainHeader="Oceanweather WIN/PRE Format                            $startTime     $startTime"; #jgf: Hey Eve, why is startTime here twice?
	push @OWI_wnd, $mainHeader;
	push @OWI_pres, $mainHeader;
	@OWItime=@$OWItimeRef;
	
	# build the filenames
	$wndFile='NAM_'.$startTime.'_'.$endTime.'.222';
	$presFile='NAM_'.$startTime.'_'.$endTime.'.221';
	# # get u,v,p values
	#NetCDF::varget($ncid, $varId{'velocity_we'},[0,0,0],[$nRec{'time'},$nRec{'y'},$nRec{'x'}],\@ugrd);
	my $nelems=@ugrd;
	#NetCDF::varget($ncid, $varId{'velocity_sn'},[0,0,0],[$nRec{'time'},$nRec{'y'},$nRec{'x'}],\@vgrd);
	#NetCDF::varget($ncid, $varId{'atm_pressure'},[0,0,0],[$nRec{'time'},$nRec{'y'},$nRec{'x'}],\@atmp);
	# close netCDF file 
	#NetCDF::close($ncid);
	# figure out each time-step record length
	$recordLength=$nelems/$nRec{'time'};
}
################################################################################
# NAME: &rotateAndFormat	
# CALL: &rotateAndFormat()
# GOAL: loop through time-steps to rotate and output at specific points with 
#	awips_lambert_interp - populate the OWI array with resulting data
################################################################################
sub rotateAndFormat
{
	for my $t (0 .. $nRec{'time'}-1)
		{
		#&stderrMessage("DEBUG","TS=$t");
		my $startInd=$t*$recordLength;
		my $stopInd=($t+1)*$recordLength-1;
		my @subset=($startInd .. $stopInd);
		# select subset of array corresponding at the particular time-step
		@miniUgrd=@ugrd[@subset];
		@miniVgrd=@vgrd[@subset];
		@miniAtmp=@atmp[@subset];
		# # print u,v,p file
		my $outFile=$outDir.$outFilename;
		 open (OUT,">$outFile") or die "Can't open output file ($outFile), error: $! \n";
		 for my $i (0 .. $recordLength-1)
		 {
			 print OUT "$miniUgrd[$i] \t $miniVgrd[$i] \t $miniAtmp[$i]\n";
		 }
		 close (OUT);
	
		# #run awis_interp
		 `$scriptDir/awip_lambert_interp.x $awipGridNumber 3 $outFile $ptFile rotatedNAM.txt velocity $velocityMultiplier`;

		 &toOWIformat('rotatedNAM.txt',$geoHeader."DT=".$OWItime[$t]);
		}
}
################################################################################
# NAME: &addToFort22	
# CALL: &addToFort22()
# GOAL: loop through time-steps to rotate and output at specific points with 
#	awips_lambert_interp - populate the OWI array with resulting data
################################################################################
sub addToFort22
{
	open(F22,">>$fort22") || die "ERROR: NAMtoOWI.pl: Failed to open OWI (NWS12) fort.22 file to append a comment line with the met time increment.";
	my $wtiminc=$timeStep*3600; # ts in seconds
        &stderrMessage("INFO","Appending the WTIMINC value of '$wtiminc' to the fort.22 file '$fort22'.");
	print F22 "# $wtiminc <-set WTIMINC to this value in ADCIRC fort.15\n";
	close(F22);
	
}

################################################################################
# NAME: &getGrib2
# CALL: &getGrib2
# GOAL: get the u,v,p data and time info from the grib2 files
################################################################################
sub getGrib2
{
        $fort22 = $outDir.$fort22;
        my @grib2Files;
        if ( $namType eq "nowcast" ) { 
           # if these are nowcast files, we'll assume that the data are 
           # six hours apart
           $timeStep = 6.0; # in hours
           # assume grib2 files are located in directories
           # called 'erl.yymmdd' where yymmdd is the year month day 
           my @grib2Dirs;
           if ( $namFormat eq "grib2" ) {
	      # assume that $dataDir points to a directory containing
	      # subdirectories named erl.*, e.g. erl.091108 (i.e., 8 November
	      # 2009) assume that each of these directories contain some grib2
	      # files that are named with the extension ".grib2"
              @grib2Dirs = glob($dataDir."/erl.*");
           }
           # assume grib files are all in the data directory
           if ( $namFormat eq "grb" ) {
              $grib2Dirs[0] = $dataDir;
           }
           my $numGrib2Dirs = @grib2Dirs;
           &stderrMessage("INFO","There is/are $numGrib2Dirs directories to process.");
           if ( $numGrib2Dirs == 0 ) {
              &stderrMessage("ERROR","There are no data directories to process.");
              die;
           }  
           foreach my $dir (@grib2Dirs) {
              push(@grib2Files,glob($dir."/*.".$namFormat));
           }
        } else { 
           # if these are forecast files, we'll assume that the data are
           # three hours apart, and that they are all located in the same
           # subdirectory 
           $timeStep = 3.0; # in hours 
           @grib2Files = glob($dataDir."/*.".$namFormat);
        }
        # grab the start time (YYYYMMDDHH) of the files from the
        # inventory in the first file ... this assumes that glob returns
        # the files in ascending order.
        if ( $namFormat eq "grb" ) {
           `$scriptDir/wgrib -v $grib2Files[0] | grep PRMSL` =~ m/:D=(\d+):PRMSL:/;
           $startTime = $1;  
        }
        if ( $namFormat eq "grib2" ) {
           `$scriptDir/wgrib2 $grib2Files[0] -match PRMSL` =~ m/d=(\d+)/;
           $startTime = $1;
        }
        &stderrMessage("DEBUG","The start time is '$startTime'.");
        $startTime =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
        my $sy = $1; # start year
        my $sm = $2; # start month
        my $sd = $3; # start day
        my $sh = $4; # start hour
        my ($ey, $em, $ed, $eh, $emin, $es);# end year, mon, day, hour, min, sec
        my ($fy, $fm, $fd, $fh, $fmin, $fs);# forecast yr, mon, day, hr, mn, sec
        my $numGrib2Files = 0;
        my $oldCycleHour = 0;
        my $oldEndTime = $startTime; 
        my @oldRawUVP;
        my $dhrs = 0;
        my ($oey, $oem, $oed, $oeh, $oemin, $oes); # old end time
        my ($ney, $nem, $ned, $neh, $nemin, $nes); # new end time
        foreach my $file (@grib2Files) {
           $numGrib2Files++;
           &stderrMessage("DEBUG","Starting work on '$file'.");
           my $cycleHour = "00";
           if ( $namFormat eq "grib2" ) {
              # grab the cycle hour from the filename itself
              $file =~ m/nam.t\d\dz.awip12(\d\d).tm00.grib2/;
              $cycleHour = $1;
           }
           if ( $namFormat eq "grb" ) {
              `$scriptDir/wgrib -v $file | grep PRMSL` =~ m/:D=(\d\d\d\d)(\d\d)(\d\d)(\d\d):PRMSL:/;
              $cycleHour = $4;  
           }
           &stderrMessage("DEBUG","The cycle hour is '$cycleHour'.");
           ($fy, $fm, $fd, $fh, $fmin, $fs) =
              Date::Pcalc::Add_Delta_DHMS($sy, $sm, $sd, $sh, 0, 0, 
                 0, $cycleHour, 0, 0);
           # calculate and save the end time ... last one will represent
           # end of the OWI file
           my $numInterp = 0;
           my @factors; 
           $factors[0] = 1.0;
           if ( $namType eq "nowcast" ) {
              if ( $namFormat eq "grib2" ) {
                 `$scriptDir/wgrib2 $file -match PRMSL` =~ m/d=(\d+)/;
                 $endTime = $1;
              } 
              if ( $namFormat eq "grb" ) {
                 `$scriptDir/wgrib -v $file | grep PRMSL` =~ m/:D=(\d+):PRMSL:/;
                 $endTime = $1;
              } 
              # check to see if there are any missing cycles ... if so, then
              # we will linearly interpolate from the previous cycle to the 
              # current one
              #
              # first, we compare the new end time and the old end time ...
              # if they are farther apart than a single time step, we must
              # interpolate the correct number of uvp data snaps in between
              $oldEndTime =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;       
              $oey = $1;
              $oem = $2;
              $oed = $3;
              $oeh = $4;
              $endTime =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;       
              $ney = $1;
              $nem = $2;
              $ned = $3;
              $neh = $4;
              (my $ddays,$dhrs, my $dsec) = Date::Pcalc::Delta_DHMS($oey,$oem,$oed,$oeh,0,0,$ney,$nem,$ned,$neh,0,0);
              $dhrs = $dhrs + $ddays * 24;
              #&stderrMessage("DEBUG","The dhrs is $dhrs."); 
              if ( $dhrs > $timeStep ) {
                 &stderrMessage("WARNING","The time difference between the files is greater than $timeStep hours. The intervening data will be linearly interpolated.");
                 $numInterp = $dhrs/$timeStep;
                 &stderrMessage("DEBUG","There are $numInterp time increments to interpolate."); 
                 # calculate interpolating factors
                 for (my $i=1; $i<=$numInterp; $i++ ) {
                    $factors[$i-1] = 1.0/$numInterp * $i; 
                 }
              } else {
                 $numInterp = 0;
                 $factors[0] = 1.0;
              }
           } else {
              ($ey, $em, $ed, $eh, $emin, $es) =
                 Date::Pcalc::Add_Delta_DHMS($sy, $sm, $sd, $sh, 0, 0, 
                    0, $cycleHour, 0, 0);
              $endTime = sprintf("%4d%02d%02d%02d",$ey ,$em, $ed, $eh);
           } 
           &stderrMessage("DEBUG","The end time is '$endTime'."); 
           push(@OWItime,$endTime."00"); # add the minutes columns
           #
           # now grab the u,v,p data from the file
           my @rawU;
           my @rawV;
           my @rawP;
           if ( $namFormat eq "grib2" ) { 
              # send accompanying inventory info (that would normally go to 
              # stdout also) to /dev/null 
              #
              # can't grab the U,V,P from the file all within one wgrib2 command
              # since wgrib2 will output them in the order in which they are 
              # found in the grib2 file ... PRMSL comes first in the grib2
              # file, so it would be first in the array ... need it to be last
              @rawU = `$scriptDir/wgrib2 $file -match "UGRD:10" -inv /dev/null -text -`;
              @rawV = `$scriptDir/wgrib2 $file -match "VGRD:10" -inv /dev/null -text -`;
              @rawP = `$scriptDir/wgrib2 $file -match "PRMSL" -inv /dev/null -text -`;
           }
           if ( $namFormat eq "grb" ) {
              #
              # get record number for wind velocity (u) at 10m
              `$scriptDir/wgrib -v $file | grep "UGRD:10 m above gnd"` =~ m/^(\d+):/;
              my $record_number = $1;
              # now decode the data for that record number to an external file
              system("wgrib -d $record_number -o ugrd.txt -text $file");
              # read in the data from the resulting file
              @rawU = `cat ugrd.txt`;
              #
              # get record number for wind velocity (v) at 10m
              `$scriptDir/wgrib -v $file | grep "VGRD:10 m above gnd"` =~ m/^(\d+):/;
              $record_number = $1;
              # now decode the data for that record number to an external file
              system("wgrib -d $record_number -o vgrd.txt -text $file");
              # read in the data from the resulting file
              @rawV = `cat vgrd.txt`;
              #
              # get record number for sea level barometric pressure
              `$scriptDir/wgrib -v $file | grep PRMSL` =~ m/^(\d+):/;
              $record_number = $1;
              # now decode the data for that record number to an external file
              system("wgrib -d $record_number -o prmsl.txt -text $file");
              # read in the data from the resulting file
              @rawP = `cat prmsl.txt`;
           }
           #
           # nlon and nlat are the first line in the output for each data set
           my @nxny = split(" ",shift(@rawU));
           shift(@rawV); # get rid of header line
           shift(@rawP); # get rid of header line
           #&stderrMessage("INFO","nlon is $nxny[0] nlat is $nxny[1].");
           $recordLength = $nxny[0] * $nxny[1];
           my @rawUVP = (@rawU,@rawV,@rawP);
           # interpolate if necessary
           if ( $numInterp > 0 ) {
              for ( my $i=1; $i<=$numInterp; $i++ ) {
                 (my $iy, my $im, my $iday, my $ih, my $imin, my $isec) =
                    Date::Pcalc::Add_Delta_DHMS($oey, $oem, $oed, $oeh, 0, 0, 
                    0, $i*$timeStep, 0, 0);
                 my $interpolatedTime = sprintf("%4d%02d%02d%02d",$iy ,$im, $iday, $ih);
                 &stderrMessage("WARNING","Interpolating data at time $interpolatedTime in the date range ($oldEndTime, $endTime) with the interpolating factor $factors[$i-1].");
                 my @interpUVP;
                 # create output data ... this will be linearly interpolated in
                 # time between two valid datasets
                 for ( my $uvp_index=0; $uvp_index<($recordLength*3); $uvp_index++ ) {
                    $interpUVP[$uvp_index] = ($rawUVP[$uvp_index] - $oldRawUVP[$uvp_index]) * $factors[$i-1] + $oldRawUVP[$uvp_index];
                 }   
                 foreach my $val (@interpUVP[(0 .. ($recordLength-1))]) {
                    push(@ugrd,$val);
                 }
                 foreach my $val (@interpUVP[($recordLength .. (2*$recordLength-1))]) {
                    push(@vgrd,$val);
                 }
                 foreach my $val (@interpUVP[(2*$recordLength .. (3*$recordLength-1))]){
                    push(@atmp,$val);
                 }
              }
           } else {
              # there wasn't any data missing between the last file and
              # the current one, so just push the data into the arrays
              foreach my $val (@rawUVP[(0 .. ($recordLength-1))]) {
                 push(@ugrd,$val);
              }
              foreach my $val (@rawUVP[($recordLength .. (2*$recordLength-1))]) {
                 push(@vgrd,$val);
              }
              foreach my $val (@rawUVP[(2*$recordLength .. (3*$recordLength-1))]){
                 push(@atmp,$val);
              }
           }              
           $oldEndTime = $endTime;
           $oldCycleHour = $cycleHour;
           @oldRawUVP = @rawUVP;
        }
        $mainHeader="Oceanweather WIN/PRE Format                            $startTime     $endTime";
        push @OWI_wnd, $mainHeader;
        push @OWI_pres, $mainHeader;
        # build the filenames
        $wndFile=$outDir.'NAM_'.$startTime.'_'.$endTime.'.222';
        $presFile=$outDir.'NAM_'.$startTime.'_'.$endTime.'.221';
        &stderrMessage("INFO","Processed $numGrib2Files file(s).");
	$nRec{'time'}=$numGrib2Files;
        if ( $numGrib2Files == 0 ) { 
           &stderrMessage("ERROR","There were no files to process.");
           die;
        } 
        
} 

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: NAMtoOWI.pl: $message\n";
   if ($level eq "ERROR") {
      sleep 60
   }
}


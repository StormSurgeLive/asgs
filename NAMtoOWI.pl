#!/usr/bin/perl -w
################################################################################
#                          	netcdfNAMtoOWI                                 #
################################################################################
# This script is triggered when a netCDF NAM file is received by LDM	       #
# 									       #
################################################################################
#                   The packages used are the following:                       #
################################################################################
#NetCDF                                                                        #
################################################################################
#written by Eve-Marie Devaliere for UNC/FRF                                    #
#first written: 03/17/10                                                       #
#last updated: 03/19/10                                            	       #
################################################################################

######################################################
#      Packages and exportation requirements         #
######################################################
use strict;
no strict 'refs';
use NetCDF;
use ArraySub;
######################################################
#             Variables declarations                 #
######################################################
our $dataDir="/data/renci/ADCIRC/wind/";
our $outDir='/data/renci/ADCIRC/wind/src/';
our $outFilename='uvp.txt';
our $ptFile='ptFile.txt';
our ($wndFile,$presFile);
our ($nDims,$nVars,$nAtts,$recDim,$dimName,%varId,@dimIds,$name,$dataType,%data,%dimId,%nRec,$nRec,@ugrd,@vgrd,@atmp,@time,@OWI_wnd,@miniOWI_wnd,@OWI_pres,@miniOWI_pres,@zeroOffset,$geoHeader);
our ($OWItimeRef,$startTime,$endTime,$timeStep,$wndHeader,$presHeader,@miniUgrd,@miniVgrd,@miniAtmp,@OWItime);
######################################################
#                    Main Program                    #
######################################################
#first get all the variables ids and dimensions ids from the netCDF file
&printDate("Start processing NAM Data");
&printDate("Process Point File ....");
$geoHeader=&processPtFile($ptFile);
&printDate("Process netCDF file ...");
my $ncid = NetCDF::open($ARGV[0],NetCDF::NOWRITE) or die "can't open file $ARGV[0], error $! \n";
NetCDF::inquire($ncid,\$nDims,\$nVars,\$nAtts,\$recDim);
#print "ndims=$nDims  nVar=$nVars, natt=$nAtts, recDim=$recDim\n";
 for my $var (0 .. $nVars-1)# var ids are 0, 1 and 2 if we have 3 variables
 {
	 NetCDF::varinq($ncid,$var,\$name,\$dataType,$nDims,\@dimIds,\$nAtts);
	 my $dimID=splice (@dimIds, 0, $nDims);
        # print "VAR: $var: NAME: $name, DATA TYPE: $dataType, NDIMS: $nDims, DIMIDS: $dimID, NATTS: $nAtts\n";
         $varId{$name} = $var; # array of variable ID numbers as a function of variable name
	 $dimId{$name}=$dimID;
 }
# get x,y,time dimensions
NetCDF::diminq($ncid, $dimId{'x'},$dimName,$nRec); 
$nRec{'x'}=$nRec;
NetCDF::diminq($ncid, $dimId{'y'},$dimName,$nRec);
$nRec{'y'}=$nRec;
NetCDF::diminq($ncid, $dimId{'time'},$dimName,$nRec);
$nRec{'time'}=$nRec;

# get the time
NetCDF::varget($ncid, $varId{'time'},0,$nRec{'time'},\@time);
($OWItimeRef,$startTime,$endTime,$timeStep)=&convertTime(\@time);
$wndHeader="OWI WWS Wind Output Ucomp,Vcomp in m/s		Start:$startTime End:$endTime";
$presHeader="OWI WWS Pressure Output Pressure in Pa		Start:$startTime End:$endTime";# pressure in Pa?
push @OWI_wnd, $wndHeader;
push @OWI_pres, $presHeader;
@OWItime=@$OWItimeRef;

# build the filenames
$wndFile='NAM_'.$startTime.'_'.$endTime.'.wnd';
$presFile='NAM_'.$startTime.'_'.$endTime.'.pre';
# # get u,v,p values
NetCDF::varget($ncid, $varId{'velocity_we'},[0,0,0],[$nRec{'time'},$nRec{'y'},$nRec{'x'}],\@ugrd);
my $nelems=@ugrd;
NetCDF::varget($ncid, $varId{'velocity_sn'},[0,0,0],[$nRec{'time'},$nRec{'y'},$nRec{'x'}],\@vgrd);
NetCDF::varget($ncid, $varId{'atm_pressure'},[0,0,0],[$nRec{'time'},$nRec{'y'},$nRec{'x'}],\@atmp);
# close netCDF file 
NetCDF::close($ncid);

# figure out each time-step record length
my $recordLength=$nelems/$nRec{'time'};
&printDate("Rotate and format each time-step ...");
# loop through the time-steps to run awips_interp	
for my $t (0 .. $nRec{'time'}-1)
{
	&printDate("TS=$t");
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
	 `./awip_lambert_interp.x 221 3 uvp.txt ptFile.txt rotatedNAM.txt`;
	 &toOWIformat('rotatedNAM.txt',$geoHeader."Dt=".$OWItime[$t]);
}

&printDate("Print OWI files...");
&printOWIfiles();
&printDate("Done processing NAM Data");
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
	$swLat=~m/(\d+.\d+)/;
	$swLat=$1;
	$swLon=~m/(\d+.\d+)/;
	$swLon=$1;
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
	$dx=abs($uniqLonSorted[1]-$uniqLonSorted[0]);
	$dy=abs($uniqLatSorted[1]-$uniqLatSorted[0]);
	my $headerLine="iLat= $nLat"."iLong= $nLon"."DX= $dx"."DY= $dy"."SWLat= $swLat"."SWLon= $swLon";
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
	open (FIL,$file);
	my @file=<FIL>;
	close(FIL);
	my (@ugrd,@vgrd,@atmp,@uLines,@vLines,@pLines,$uStr,$vStr,$pStr);
	($uStr,$vStr,$pStr)=('','','');
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
		my $u=sprintf("%1.5f",$ugrd[$i]);
		my $v=sprintf("%1.5f",$vgrd[$i]);
		my $p=sprintf("%6d",$atmp[$i]);
		$uStr=$uStr."   $u";# concatenate values together
		$vStr=$vStr."   $v";
		$pStr=$pStr."   $p";
		if (($miniCount==7) || ($i==$nTot))# 8 values per line or reach the end of the file
		{
			$miniCount=0;
			push @uLines,$uStr;
			push @vLines,$vStr;
			push @pLines,$pStr;
			($uStr,$vStr,$pStr)=('','','');
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

#!/usr/bin/env perl
#--------------------------------------------------------------------------
# NAMtoOWIRamp.pl
#--------------------------------------------------------------------------
# Copyright(C) 2019 Brett Estrade
# Copyright(C) 2011--2022 Jason Fleming
# Copyright(C) 2010--2011 Eve-Marie Devaliere
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
#--------------------------------------------------------------------------
################################################################################
#written by Eve-Marie Devaliere for UNC/FRF                                    #
#additions for grib2 input and command line arguments by Jason Fleming for ASGS#
#first written: 03/17/10                                                       #
#last updated: 03/31/10                                              	       #
################################################################################
#
######################################################
#      Packages and exportation requirements         #
######################################################
use strict;
use warnings;
use List::Util qw[min max];
use Getopt::Long;
use Date::Calc;
use JSON::PP;
use Storable;
use Cwd;
use ASGSUtil qw[ASGSUtil::setParameter];
######################################################
#             Variables declarations                 #
######################################################
my $dataDir = "null";      # path to NAM data
my $outDir = "null";       # path to NAM u,v,p text data
my $ptFile = "null";       # file name of output grid lat/lon
my $namFormat = "null";    # default NAM format is netCDF
my $stage = "null";        # expect forecast data by default
my $scriptDir = "null";    # path to executables
my $timeStep = "null";     # met time increment (WTIMINC), in hours
my $applyRamp = "null";    # whether or not to apply a spatial extrapolation ramp
my $rampDistance = "null"; # distance in lambert coords to ramp vals to zero
my $awipGridNumber = "null";     # integer that specifies the NAM domain and its grid
my $velocityMultiplier = "null"; # multiplier for vels
my $pressureMultiplier = "null"; # convert Pascals to mb by default
my @namFormats         = qw(grb grib2 grb2); # accceptable file types for NAMdata (grb, grib2, and grb2 also used as file extension)
# $startTime and $endTime are date/time strings for main OWI header and used to name the output files
my $startTime;
my $endTime;
my $this = "NAMtoOWIRamp.pl";
#
######################################################
#                    Main Program                    #
######################################################
# get command line options for variables
GetOptions(
    "dataDir=s"            => \$dataDir,
    "outDir=s"             => \$outDir,
    "ptFile=s"             => \$ptFile,
    "namFormat=s"          => \$namFormat,
    "stage=s"              => \$stage,
    "awipGridNumber=s"     => \$awipGridNumber,
    "velocityMultiplier=s" => \$velocityMultiplier,
    "pressureMultiplier=s" => \$pressureMultiplier,
    "applyRamp=s"          => \$applyRamp,
    "rampDistance=s"       => \$rampDistance,
    "scriptDir=s"          => \$scriptDir
);

# get JSON request from STDIN
my $file_content = do { local $/; <> };

# deserialize JSON
my $jshash_ref = JSON::PP->new->decode($file_content);
$jshash_ref->{"forcing.nam.ncep.file.json.reproject"} = "$this.json";
#
# FIXME: Use complex data structures to simplify this
ASGSUtil::setParameter( $this, $jshash_ref, \$dataDir,
              "forcing.nam.path.data", cwd() );
ASGSUtil::setParameter( $this, $jshash_ref, \$outDir,
              "forcing.nam.path.data.owi", cwd() );
ASGSUtil::setParameter( $this, $jshash_ref, \$namFormat,
              "forcing.nam.file.data.raw.format", "grib2" );
ASGSUtil::setParameter( $this, $jshash_ref, \$stage,
              "forcing.nam.stage", "null" );
ASGSUtil::setParameter( $this, $jshash_ref, \$awipGridNumber,
              "forcing.nam.awip.grid", 218 );
ASGSUtil::setParameter( $this, $jshash_ref, \$velocityMultiplier,
              "forcing.nam.data.owi.velocitymultiplier", 1.0 );
ASGSUtil::setParameter( $this, $jshash_ref, \$pressureMultiplier,
              "forcing.nam.data.owi.pressuremultiplier", 0.01 );
ASGSUtil::setParameter( $this, $jshash_ref, \$applyRamp,
              "forcing.nam.data.owi.applyramp", \0 ); # \0 creates JSON::false
ASGSUtil::setParameter( $this, $jshash_ref, \$rampDistance,
              "forcing.nam.data.owi.rampdistance", 1.0 );
ASGSUtil::setParameter( $this, $jshash_ref, \$scriptDir,
              "path.scriptdir", dirname(__FILE__) );
#
# the default ptFile uses scriptdir so it has to be set
# after scriptdir has been set
ASGSUtil::setParameter( $this, $jshash_ref, \$ptFile,
              "forcing.nam.data.owi.grid",
              $scriptDir."/input/ptFile_oneEighth.txt" );
#
# write out the initial JSON response file now for use in
# debugging if there is a failure later in this script
ASGSUtil::writeJSON($jshash_ref, $this);
#
ASGSUtil::stderrMessage("INFO",$this,"Started processing point file '$ptFile'." );
my $geoHeader = processPtFile($this, $ptFile, $jshash_ref);
my $stagelc = lc $stage;
$jshash_ref->{"forcing.nam.$stagelc.data.owi.header"} = $geoHeader;
#
# load NAM data
ASGSUtil::stderrMessage("INFO",$this,"Processing grib/grib2 file(s)." );
my ( @OWI_wnd, @OWI_pres, @OWItime, %nRec, $recordLength );
my ( $wndFile, $presFile ); # file names
# FIXME: use complex data structure to simplify this argument list
getGrib2(
         $this, $stage,
         $dataDir, $outDir, $namFormat,
         \$timeStep, \$startTime, \$endTime,
         \@OWI_wnd, \@OWI_pres, \@OWItime,
         \%nRec, \$recordLength,
         \$wndFile, \$presFile, $jshash_ref
        );
$jshash_ref->{"forcing.nam.$stagelc.file.owi.velocity"} = $wndFile;
$jshash_ref->{"forcing.nam.$stagelc.file.owi.pressure"} = $presFile;
my $wtiminc = $timeStep * 3600;    # ts in seconds
$jshash_ref->{"forcing.nam.$stagelc.data.owi.wtiminc.seconds"} = ($wtiminc * 1.0);
ASGSUtil::writeJSON($jshash_ref, $this);
# have to add the meteorology time increment to fort.22
ASGSUtil::stderrMessage("INFO", $this,
              "Adding WTIMINC value to fort.22 comment line." );
my $F22;
unless ( open( $F22, ">>", "fort.22" ) ) {
   ASGSUtil::stderrMessage("ERROR", $this,
                 "Failed to open OWI (NWS12) fort.22 file " .
                 "to append a comment line with the " .
                 "meteorological time increment.");
   die;
}
ASGSUtil::stderrMessage("INFO", $this,
              "Appending the WTIMINC value of '$wtiminc' to the fort.22 file." );
print $F22 "# $wtiminc <-set WTIMINC to this value in ADCIRC fort.15\n";
close($F22);
#
# loop through the time-steps to run awips_interp
ASGSUtil::stderrMessage("INFO", $this, "Rotate and format each time-step." );
my ( @ugrd_store_files, @vgrd_store_files, @atmp_store_files );
# FIXME: Use complex data structures to simplify this arg list
rotateAndFormat(
                $this, $applyRamp, $geoHeader,
                \@ugrd_store_files, \@vgrd_store_files, \@atmp_store_files,
                \@OWItime, \@OWI_wnd, \@OWI_pres,
                \%nRec, $recordLength, $jshash_ref
               );
$jshash_ref->{"forcing.nam.$stagelc.owi.pressure.nrec"} = $nRec{"time"};
$jshash_ref->{"forcing.nam.$stagelc.owi.pressure.recordlength"} = $recordLength;
$jshash_ref->{"forcing.nam.$stagelc.owi.times"} = \@OWItime;
#
# write out OWI files
ASGSUtil::stderrMessage("INFO", $this, "Print OWI files." );
my $OF;
#
# write wind file
unless ( open( $OF, '>', "$wndFile" ) ) {
ASGSUtil::stderrMessage("INFO", $this,
              "Could not open wind file '$wndFile': $!");
   die;
}
foreach my $line (@OWI_wnd) {
   print $OF $line . "\n";
}
close($OF);
#
# write pressure file
unless ( open( $OF, '>', "$presFile" ) ) {
   ASGSUtil::stderrMessage("INFO", $this,
              "Could not open wind file '$wndFile': $!");
   die;
}
foreach my $line (@OWI_pres) {
   print $OF $line . "\n";
}
close($OF);
ASGSUtil::stderrMessage("INFO", $this, "Done processing NAM data." );

# write json response to file
ASGSUtil::writeJSON($jshash_ref, $this);
# write json response to STDOUT
printf JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
1;

######################################################
#                    Subroutines                     #
######################################################

################################################################################
# NAME: processPtFile
# CALL: processPtFile($file)
# GOAL: process the 'point file' aka 'lat/lon file' used by awips_interp to know
# where to output data by extracting grid information
################################################################################
sub processPtFile {
    my ( $this, $ptFile, $jshash_ref ) = @_;

    my ( @lat, @lon, $null, @ary,
         $swLat, $swLon, @uniqLat, @uniqLon,
         $nLat, $nLon, $nPts, %seen,
         @uniqLatSorted, @uniqLonSorted,
         $dx, $dy );

    # check for existence of lat/lon file before attempting to open
    unless ( -e $ptFile ) {
        ASGSUtil::stderrMessage("ERROR", $this,
                       "Grid specification file '$ptFile' does not exist." );
        die;
    }
    open my $PT, '<', $ptFile || die $!;
    my $discard = <$PT>;    # skip first line
    my $i       = 0;
    while ( my $line = <$PT> ) {
        ( $null, $lon[$i], $lat[$i] ) = split /,/, $line;
        ++$i;
    }
    close $PT;

    # find SW lat and lon using min
    $swLat = min(@lat);
    $swLon = min(@lon);

    #get rid of remaining space (may be important for format of header line)
    $swLat =~ m/(\S+)/;
    $swLat = sprintf( "%3.4f", $1 );        # first to have the right float format
    $swLat = sprintf( "%8s",   $swLat );    # then to have the right spacing in the file
    $swLon =~ m/(\S+)/;
    $swLon = sprintf( "%3.4f", $1 );
    $swLon = sprintf( "%8s",   $swLon );

    # find the uniques lat and lon
    %seen = ();
    foreach my $item (@lat) {
        push( @uniqLat, $item ) unless $seen{$item}++;
    }
    $nLat = @uniqLat;
    %seen = ();
    foreach my $item (@lon) {
        push( @uniqLon, $item ) unless $seen{$item}++;
    }
    $nLon          = @uniqLon;
    @uniqLatSorted = sort(@uniqLat);
    @uniqLonSorted = sort(@uniqLon);

    # assume a constant dx/dy - abs in case of negative values...
    $dx   = sprintf( "%1.4f", abs( $uniqLonSorted[1] - $uniqLonSorted[0] ) );
    $dy   = sprintf( "%1.4f", abs( $uniqLatSorted[1] - $uniqLatSorted[0] ) );
    $dx   = sprintf( "%6s",   $dx );
    $dy   = sprintf( "%6s",   $dy );
    $nLat = sprintf( "%4s",   $nLat );
    $nLon = sprintf( "%4s",   $nLon );
    my $headerLine = "iLat=$nLat" . "iLong=$nLon" . "DX=$dx" . "DY=$dy" . "SWLat=$swLat" . "SWLon=$swLon";

    #print "headerline=$headerLine\n";
    return $headerLine;

}
################################################################################
# NAME: toOWIformat
# CALL: toOWIformat($file,$header);
# GOAL: convert the file $file to OWI format (for one TS) with the header $header
################################################################################
sub toOWIformat {
    my ( $file, $header, $OWI_wnd_ref, $OWI_pres_ref ) = @_;

    push @$OWI_wnd_ref,  $header;
    push @$OWI_pres_ref, $header;

    # check for existence of the data file before attempting to open
    unless ( -e $file ) {
        ASGSUtil::stderrMessage("ERROR",$this,"The data file '$file' does not exist." );
        printf STDOUT "0";
        die;
    }
    open my $FIL, '<', $file || die $!;
    my ( @ugrd, @vgrd, @atmp, @uLines, @vLines, @pLines, $uStr, $vStr, $pStr );
    undef($uStr);
    undef($vStr);
    undef($pStr);
    my $count = 0;
    my $null;

    while ( my $line = <$FIL> ) {
        ( $null, $null, $ugrd[$count], $vgrd[$count], $atmp[$count] ) =
          split /\s+/, $line;    #2 nulls bc starts with space and don't need index number
                                 #print "u,v,p=$ugrd[$count],$vgrd[$count],$atmp[$count]\n";
        $count++;
    }
    close $FIL;
    my $nTot      = @ugrd - 1;
    my $miniCount = 0;
    for my $i ( 0 .. $nTot ) {    # can do u, v and p at the same time

        my $u = sprintf( "% 10f", $ugrd[$i] );
        my $v = sprintf( "% 10f", $vgrd[$i] );

        # change from Pascal to millibar while at it
        # needed to add sprintf %10s for pressure vals < 1000
        my $p = sprintf( "%10s", sprintf( "% 4.4f", $atmp[$i] * $pressureMultiplier ) );
        if ( defined($uStr) ) {
            $uStr = $uStr . "$u";    # concatenate values together
            $vStr = $vStr . "$v";
            $pStr = $pStr . "$p";
        } else {
            $uStr = $u;              # concatenate values together
            $vStr = $v;
            $pStr = $p;
        }
        # 8 values per line or reach the end of the file
        if ( ( $miniCount == 7 ) || ( $i == $nTot ) ) {
            $miniCount = 0;
            push @uLines, $uStr;
            push @vLines, $vStr;
            push @pLines, $pStr;
            undef($uStr);
            undef($vStr);
            undef($pStr);
        } else {
            $miniCount++;
        }
    }
    # push the lines in the array representing each file
    push @$OWI_wnd_ref,  @uLines;
    push @$OWI_wnd_ref,  @vLines;
    push @$OWI_pres_ref, @pLines;
}

################################################################################
# NAME: rotateAndFormat
# CALL: rotateAndFormat()
# GOAL: loop through time-steps to rotate and output at specific points with
#	awips_lambert_interp - populate the OWI array with resulting data
################################################################################
sub rotateAndFormat {
    my ( $this, $applyRamp, $geoHeader,
         $ugrd_ref, $vgrd_ref, $atmp_ref,
         $OWItimeRef, $OWI_wnd_ref, $OWI_pres_ref,
         $nRec_ref, $recordLength, $jshash_ref
       ) = @_;

    for my $t ( 0 .. $nRec{'time'} - 1 ) {
        ASGSUtil::appMessage("DEBUG", $this, "TS=$t" );

        my $ugrd_file = $ugrd_ref->[$t];
        my $vgrd_file = $vgrd_ref->[$t];
        my $atmp_file = $atmp_ref->[$t];

        # select subset of array corresponding at the particular time-step
        my $miniUgrd_ref = retrieve($ugrd_file) or die $!;
        my $miniVgrd_ref = retrieve($vgrd_file) or die $!;
        my $miniAtmp_ref = retrieve($atmp_file) or die $!;

        # delete files as they're used
        unlink( $ugrd_file, $vgrd_file, $atmp_file );

        # # print u,v,p file
        my $uvpFile = $outDir . '/uvp.txt';
        open( my $OUT, '>', $uvpFile )
          or die "Can't open output file ($uvpFile), error: $! \n";
        for my $i ( 0 .. $recordLength - 1 ) {
            print $OUT "$miniUgrd_ref->[$i] \t $miniVgrd_ref->[$i] \t $miniAtmp_ref->[$i]\n";
        }
        close $OUT;

        # run awip_interp
        my $rampDistance = -99999.0;
        if ( $applyRamp ) {
           $rampDistance = $jshash_ref->{"forcing.nam.data.owi.rampdistance"};
        }
        # NAM pressure data are in Pa
        if ( -f "rotataedNAM.txt" ) {
           ASGSUtil::appMessage("DEBUG",$this,"Deleting old rotatedNAM.txt.");
           unlink "rotatedNAM.txt";
        }
        my $cmd = "$scriptDir/lambertInterpRamp.x " .
                  "--grid-number $awipGridNumber " .
                  "--num-columns 3 " .
                  "--lambert-data-inputfile $uvpFile " .
                  "--target-point-file $ptFile " .
                  "--geographic-data-outputfile rotatedNAM.txt " .
                  "--wind-units velocity " .
                  "--wind-multiplier $velocityMultiplier " .
                  "--ramp-distance $rampDistance " .
                  "--background-pressure 101300.0 " .
                  "--pressure-column 3 " .
                  ">> reproject.log 2>&1";
        ASGSUtil::appMessage("DEBUG", $this,
                   "1: Reprojecting Lambert Conformal NAM data ".
                   "with the following command: $cmd" );
        $jshash_ref->{"forcing.nam.data.owi.reprojection.cmd"} = "$cmd";
        my $res = `$cmd`;

        if ( !-f "rotatedNAM.txt" ) {
           die "\nrotatedNAM.txt DNE. on TS=$t\n";
        } else {
           ASGSUtil::appMessage("DEBUG", $this, "$res");
        }
        toOWIformat( 'rotatedNAM.txt', $geoHeader . "DT=" . $OWItimeRef->[$t], $OWI_wnd_ref, $OWI_pres_ref );
    }
}

################################################################################
# NAME: getGrib2
# CALL: getGrib2
# GOAL: get the u,v,p data and time info from the grib2 files
################################################################################
sub getGrib2 {
    my (
        $this, $stage,
        $dataDir, $outDir, $namFormat,
        $timeStep_ref, $startTime_ref, $endTime_ref,
        $OWI_wnd_ref, $OWI_pres_ref, $OWItime_ref,
        $nRec_ref, $recordLength_ref,
        $wndFile_ref, $presFile_ref, $jshash_ref
        )
        = @_;
    # grab the list of cycles out of the hash
    my $cyclelist_ref = $jshash_ref->{"forcing.nam.ncep.cyclelist"};
    #
    my $fort22 = $outDir . '/fort.22';
    my @grib2Files;

    if ( $stage eq "NOWCAST" ) {
        $$startTime_ref = $cyclelist_ref->[0];
        $$endTime_ref = $cyclelist_ref->[-1];
        #  N O W C A S T
        # if these are nowcast files, we'll assume that the data are
        # six hours apart
        $$timeStep_ref = 6.0;  # in hours
                               # assume grib2 files are located in directories
                               # called 'erl.yymmdd' where yymmdd is the year month day
        my @grib2Dirs;
        if ( $namFormat eq "grib2" ) {
            # assume that $dataDir points to a directory containing
            # subdirectories named erl.*, e.g. erl.091108 (i.e., 8 November
            # 2009) assume that each of these directories contain some grib2
            # files that are named with the extension ".grib2"
            @grib2Dirs = glob( $dataDir . "/erl.*" );
        }
        # assume grib files are all in the data directory
        if ( ($namFormat eq "grb") || ($namFormat eq "grb2") ) {
            $grib2Dirs[0] = $dataDir;
        }
        my $numGrib2Dirs = @grib2Dirs;
        ASGSUtil::stderrMessage("INFO",$this,"There is/are $numGrib2Dirs directories to process." );
        if ( $numGrib2Dirs == 0 ) {
            ASGSUtil::stderrMessage("ERROR",$this,"There are no data directories to process." );
            die;
        }
        foreach my $dir (@grib2Dirs) {
            # e.g.:
            # $dataDir/erl.220123/nam.t18z.awip1200.tm00.grib2
            # $dataDir/erl.220124/nam.t00z.awip1200.tm00.grib2
            # $dataDir/erl.220124/nam.t06z.awip1200.tm00.grib2
            #
            # eliminate any data file that is before or after
            # the time range of interest
            my @thisDirFiles = glob( $dir . "/nam.*awip1200*." . $namFormat );
            foreach my $file (@thisDirFiles) {
               $file =~ /erl.(\d\d)(\d\d)(\d\d)\/nam.t(\d\d)/;
               my $cycletime = "20$1$2$3$4";
               if ( $cycletime >= $$startTime_ref && $cycletime <= $$endTime_ref ) {
                  push( @grib2Files, $file );
               }
            }
        }
    } else {
        #  F O R E C A S T
        $$startTime_ref = $cyclelist_ref->[-1];
        $jshash_ref->{"forcing.nam.time.forecast.valid.start"} = "$$startTime_ref" . "0000";
        # if these are forecast files, we'll assume that the data are
        # three hours apart, and that they are all located in the same
        # subdirectory
        $$timeStep_ref = 3.0; # in hours
        # e.g.:
        # $dataDir/erl.220123/nam.t18z.awip1200.tm00.grib2
        # $dataDir/erl.220123/nam.t18z.awip1203.tm00.grib2
        # $dataDIr/erl.220123/nam.t18z.awip1206.tm00.grib2
        # ...
        # Determine the NAM cycle to convert
        $$startTime_ref =~ /\d{2}(\d{2})(\d{2})(\d{2})(\d{2})/;
        my $dir = "erl.$1$2$3";
        my $cycleHour = $4;
        @grib2Files = glob( "$dataDir/$dir/nam.t$cycleHour*." . $namFormat );
    }

    #
    # grab the start time (YYYYMMDDHH) of the files from the
    # inventory in the first file ... this assumes that glob returns
    # the files in ascending order.
    if ( $namFormat eq "grb" ) {
        `$scriptDir/wgrib -v $grib2Files[0] | grep PRMSL` =~ m/:D=(\d+):PRMSL:/;
        $$startTime_ref = $1;
    }
    if ( ($namFormat eq "grib2") || ($namFormat eq "grb2") ) {
        `$scriptDir/wgrib2 $grib2Files[0] -match PRMSL` =~ m/d=(\d+)/;
        $$startTime_ref = $1;
    }
    ASGSUtil::appMessage("DEBUG",$this,"The start time is '$$startTime_ref'." );
    $$startTime_ref =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
    my $sy = $1;    # start year
    my $sm = $2;    # start month
    my $sd = $3;    # start day
    my $sh = $4;    # start hour
    my ( $ey, $em, $ed, $eh, $emin, $es );    # end year, mon, day, hour, min, sec
    my ( $fy, $fm, $fd, $fh, $fmin, $fs );    # forecast yr, mon, day, hr, mn, sec
    my $numGrib2Files = 0;
    my $oldCycleHour  = 0;
    my $oldEndTime    = $$startTime_ref;
    my @oldRawUVP;
    my $dhrs = 0;
    my ( $oey, $oem, $oed, $oeh, $oemin, $oes );    # old end time
    my ( $ney, $nem, $ned, $neh, $nemin, $nes );    # new end time

    foreach my $file (@grib2Files) {
        $numGrib2Files++;
        ASGSUtil::appMessage("DEBUG",$this,"Starting work on '$file'." );
        my $cycleHour = "00";
        if ( $namFormat eq "grib2" ) {
            # grab the cycle hour from the filename itself
            $file =~ m/nam.t\d\dz.awip12(\d\d).tm00.grib2/;
            $cycleHour = $1;
        }
        if ( $namFormat eq "grb2" ) {
            `$scriptDir/wgrib2 -v $file | grep PRMSL` =~ m/:d=(\d\d\d\d)(\d\d)(\d\d)(\d\d):PRMSL/;
            $cycleHour = $4;
        }
        if ( $namFormat eq "grb" ) {
            `$scriptDir/wgrib -v $file | grep PRMSL` =~ m/:D=(\d\d\d\d)(\d\d)(\d\d)(\d\d):PRMSL:/;
            $cycleHour = $4;
        }
        ASGSUtil::appMessage("DEBUG",$this,"The cycle hour is '$cycleHour'." );
        ( $fy, $fm, $fd, $fh, $fmin, $fs ) = Date::Calc::Add_Delta_DHMS( $sy, $sm, $sd, $sh, 0, 0, 0, $cycleHour, 0, 0 );

        # calculate and save the end time ... last one will represent
        # end of the OWI file
        my $numInterp = 0;
        my @factors;
        $factors[0] = 1.0;
        $$endTime_ref = "";
        if ( $stage eq "NOWCAST" ) {
            my $temp = "";
            if ( ($namFormat eq "grib2") || ($namFormat eq "grb2") ) {
                my $com = "";
                $com  = "$scriptDir/wgrib2 $file -match PRMSL";
                $temp = `$com`;
                $temp =~ m/d=(\d+)/;
                $$endTime_ref = $1;
            }
            if ( $namFormat eq "grb" ) {
                `$scriptDir/wgrib -v $file | grep PRMSL` =~ m/:D=(\d+):PRMSL:/;
                $$endTime_ref = $1;
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
            $$endTime_ref =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
            $ney = $1;
            $nem = $2;
            $ned = $3;
            $neh = $4;
            ( my $ddays, $dhrs, my $dsec ) =
                Date::Calc::Delta_DHMS( $oey, $oem, $oed, $oeh, 0, 0,
                                        $ney, $nem, $ned, $neh, 0, 0 );
            $dhrs = $dhrs + $ddays * 24;

            #ASGSUtil::stderrMessage("DEBUG","The dhrs is $dhrs.");
            if ( $dhrs > $$timeStep_ref ) {
                ASGSUtil::stderrMessage("WARNING", $this,
                   "The time difference between the files " .
                   "is greater than $timeStep hours. " .
                   "The intervening data will be linearly interpolated.");
                $numInterp = $dhrs / $$timeStep_ref;
                ASGSUtil::appMessage("DEBUG", $this,
                   "There are $numInterp time increments to interpolate." );

                # calculate interpolating factors
                for ( my $i = 1; $i <= $numInterp; $i++ ) {
                    $factors[ $i - 1 ] = 1.0 / $numInterp * $i;
                }
            }
            else {
                $numInterp = 0;
                $factors[0] = 1.0;
            }
        }
        else {
            ( $ey, $em, $ed, $eh, $emin, $es ) =
               Date::Calc::Add_Delta_DHMS( $sy, $sm, $sd,        $sh, 0, 0,
                                                       0, $cycleHour, 0, 0 );
            $$endTime_ref = sprintf( "%4d%02d%02d%02d", $ey, $em, $ed, $eh );
        }
        ASGSUtil::appMessage("DEBUG", $this, "The end time is '$$endTime_ref'." );
        push( @OWItime, $$endTime_ref . "00" );    # add the minutes columns
                                              #
                                              # now grab the u,v,p data from the file
        my @rawU;
        my @rawV;
        my @rawP;

        if ( ($namFormat eq "grib2") || ($namFormat eq "grb2") ) {

            # send accompanying inventory info (that would normally go to
            # stdout also) to /dev/null
            #
            # can't grab the U,V,P from the file all within one wgrib2 command
            # since wgrib2 will output them in the order in which they are
            # found in the grib2 file ... PRMSL comes first in the grib2
            # file, so it would be first in the array ... need it to be last
            die "$file not found.\n" if ( !-f $file );
            my $com = "$scriptDir/wgrib2 $file -match \"UGRD:10\" -inv /dev/null -text -";
            @rawU = `$com`;
            die "rawU is empty, com=$com\n" unless (@rawU);
            $com  = "$scriptDir/wgrib2 $file -match \"VGRD:10\" -inv /dev/null -text -";
            @rawV = `$com`;
            die "rawV is empty, com=$com\n" unless (@rawV);
            $com  = "$scriptDir/wgrib2 $file -match \"PRMSL\" -inv /dev/null -text -";
            @rawP = `$com`;
            die "rawP is empty, com=$com\n" unless (@rawP);
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
        my @nxny = split( " ", shift(@rawU) );
        shift(@rawV);    # get rid of header line
        shift(@rawP);    # get rid of header line
                         #ASGSUtil::stderrMessage("INFO","nlon is $nxny[0] nlat is $nxny[1].");
        $$recordLength_ref = $nxny[0] * $nxny[1];
        my @rawUVP = ( @rawU, @rawV, @rawP );

        # interpolate if necessary
        my ( @tmp_ugrd, @tmp_vgrd, @tmp_atmp );
        my $ugrd_store_file = qq{ugrd.$numGrib2Files.tmp};
        my $vgrd_store_file = qq{vgrd.$numGrib2Files.tmp};
        my $atmp_store_file = qq{atmp.$numGrib2Files.tmp};

        push @ugrd_store_files, $ugrd_store_file;
        push @vgrd_store_files, $vgrd_store_file;
        push @atmp_store_files, $atmp_store_file;

        if ( $numInterp > 0 ) {
            for ( my $i = 1; $i <= $numInterp; $i++ ) {
                ( my $iy, my $im, my $iday, my $ih, my $imin, my $isec )
                   = Date::Calc::Add_Delta_DHMS( $oey, $oem, $oed, $oeh, 0, 0,
                                                      0, $i * $timeStep, 0, 0 );

                my $interpolatedTime = sprintf( "%4d%02d%02d%02d", $iy, $im, $iday, $ih );

                ASGSUtil::stderrMessage("WARNING", $this,
                   "Interpolating data at time $interpolatedTime " .
                   "in the date range ($oldEndTime, $endTime) " .
                   "with the interpolating factor $factors[$i-1].");

                # create output data ...
                # this will be linearly interpolated in
                # time between two valid datasets -
                # each 3rd of the interpolation is distributed
                # among @ugrd, @vgrd, and @atmp

                for ( my $uvp_index = 0;
                         $uvp_index < $$recordLength_ref;
                         $uvp_index++ ) {
                    my $val = ( $rawUVP[$uvp_index] - $oldRawUVP[$uvp_index] )
                              * $factors[ $i - 1 ] + $oldRawUVP[$uvp_index];
                    push( @tmp_ugrd, $val );
                }
                for ( my $uvp_index = $$recordLength_ref;
                         $uvp_index < 2 * $$recordLength_ref;
                         $uvp_index++ ) {
                    my $val = ( $rawUVP[$uvp_index] - $oldRawUVP[$uvp_index] )
                              * $factors[ $i - 1 ] + $oldRawUVP[$uvp_index];
                    push( @tmp_vgrd, $val );
                }
                for ( my $uvp_index = 2 * $$recordLength_ref;
                         $uvp_index < 3 * $$recordLength_ref;
                         $uvp_index++ ) {
                    my $val = ( $rawUVP[$uvp_index] - $oldRawUVP[$uvp_index] )
                              * $factors[ $i - 1 ] + $oldRawUVP[$uvp_index];
                    push( @tmp_atmp, $val );
                }
            }
        }
        else {
            # there wasn't any data missing between the last file and
            # the current one, so just push the data into the arrays
            foreach my $val ( @rawUVP[ ( 0 .. ( $$recordLength_ref - 1 ) ) ] ) {
                push( @tmp_ugrd, $val );
            }
            foreach my $val ( @rawUVP[ ( $$recordLength_ref .. ( 2 * $$recordLength_ref - 1 ) ) ] ) {
                push( @tmp_vgrd, $val );
            }
            foreach my $val ( @rawUVP[ ( 2 * $$recordLength_ref .. ( 3 * $$recordLength_ref - 1 ) ) ] ) {
                push( @tmp_atmp, $val );
            }
        }

        store \@tmp_ugrd, $ugrd_store_file;
        store \@tmp_vgrd, $vgrd_store_file;
        store \@tmp_atmp, $atmp_store_file;

        $oldEndTime   = $$endTime_ref;
        $oldCycleHour = $cycleHour;
        @oldRawUVP    = @rawUVP;
    }

    my $mainHeader = "Oceanweather WIN/PRE Format                            $startTime     $endTime";
    push @$OWI_wnd_ref,  $mainHeader;
    push @$OWI_pres_ref, $mainHeader;

    # build the filenames
    $$wndFile_ref  = "$outDir" . "/NAM_$stage" . "_$$startTime_ref" . "_$$endTime_ref" . ".222";
    $$presFile_ref = "$outDir" . "/NAM_$stage" . "_$$startTime_ref" . "_$$endTime_ref" . ".221";
    ASGSUtil::stderrMessage("INFO", $this,
                  "Processed $numGrib2Files file(s).");
    $$nRec_ref{'time'} = $numGrib2Files;
    if ( $numGrib2Files == 0 ) {
        ASGSUtil::stderrMessage("ERROR", $this, "There were no files to process.");
        die;
    }
    if ( $stage eq "FORECAST" ) {
        $jshash_ref->{"forcing.nam.time.forecast.valid.end"} = "$$endTime_ref" . "0000";
    }
}
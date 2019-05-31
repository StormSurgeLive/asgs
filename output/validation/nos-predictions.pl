#!/usr/bin/env perl

use strict;
use warnings;
use Exporter qw(import);
use Data::Dumper qw(Dumper);
use Getopt::Long;


#my $getlat=30.0;
#my $getlon=-90.0;
my $getlat=34.7458;
my $getlon=-76.5375;
my $maxdist=5.0;
#my $getyyyymmdd=20120829;
my $getyyyymmdd = 20150101;
my $event = "arthur_rtma";
my $startdate = 20140702;
my $enddate = 20140705;

my $nosproduct="predictions";

GetOptions(
           "getlat=f" => \$getlat,
           "getlon=f" => \$getlon,
           "maxdist=f" => \$maxdist,
           "event=s" => \$event,
           "startdate=s" => \$startdate,
           "enddate=s" => \$enddate
           );




open(my $noslist, '>',"$event/makenos.list");
open(my $predictlist, '>',"$event/makeoffset.list");
open(my $textlist, '>',"$event/noslist_$event.txt");
open(my $navdstations, '>','navd-stations.txt');
open(my $nonstations, '>','non-stations.txt');
#open(my $NCstations, '>','NC-fullinfo-stations.txt');
#open(my $LAstations, '>','LA-fullinfo-stations.txt');

my $stationfile = "stations_xml_active.txt";

open(SFILE,$stationfile);
my @stationdata;
#my $stationlines = `cat $stationfile | wc -l`;
my $stationlines = 4971;
my $linecount = 0;
my $stationname;
my $ID;
my $lat;
my $lon;
my $navdname;
while ($linecount < $stationlines)
{
	my $line = <SFILE>;
	chomp $line;
	if ( grep /station/, $line )
	{
		if ( grep /ID/, $line )
		{
#			print "$line\n";
			my @stationname = split('"',$line);
			$stationname = $stationname[1];
#			print "$stationname\n";
			$ID = $stationname[3];
			my $url = "tidesandcurrents.noaa.gov/api/datagetter?begin_date=20150217 10:00&end_date=20150218 12:24&station=$ID&product=$nosproduct&datum=navd&units=metric&time_zone=gmt&application=web_services&format=csv";
#			system("wget -O navdtest/$ID-navdtest.txt '$url'");


			my $navdlines = `cat navdtest/$ID-navdtest.txt | wc -l`;
			if ( $navdlines > 10 )
			{
				print $navdstations "$stationname, $ID\n";
				$navdname = "$stationname-NAVD88";
			}
			else
			{
				my $msllines = `cat navdtest/$ID-navdtest.txt | grep MSL | wc -l`;
				if ( $msllines > 0 )
				{
					print $nonstations "$stationname, $ID\n";
					$navdname = "$stationname-MSL";
				}
				else
				{
					my $mtllines = `cat navdtest/$ID-navdtest.txt | grep MTL | wc -l`;
					if ( $mtllines > 0 )
					{
						print $nonstations "$stationname, $ID\n";
						$navdname = "$stationname-MTL";
					}
					else
					{
						print $nonstations "$stationname, $ID\n";
						$navdname = $stationname;
					}
				}

#				my $url = "tidesandcurrents.noaa.gov/api/datagetter?begin_date=$startdate 00:00&end_date=$enddate 23:59&station=$ID&product=datum&time_zone=gmt&application=web_services&format=csv";
#				system("wget -O $event/$ID-datums.txt '$url'");
#				if ( grep /MSL/, $event/$ID-datums.txt )
#				{
#					print "MSL found in $ID datum TXT\n";
#					$navdname= "$stationname-MSL";
#				}
#				elsif ( grep /MTL/, $event/$ID-datums.txt )
#				{
#					print "MSL not found and MTL found in $ID datum TXT\n";
#					$navdname= "$stationname-MTL";
#				}
			}

# Get North Carolina Stations based on 4 states - with the unique first 3 digits to ID numbers
#			my $idstate = substr("$ID",0,3);
##			print "$ID, $idstate\n";
#			$idstate =~ s/^\s+|\s+$//g;
#			if ( ( $idstate == 865 ) or ( $idstate == 863 ) or ( $idstate == 857 ) or ( $idstate == 866 ) )
#			{
#				print $NCstations "$stationname, $ID\n";
#			}
			
		}			
	}
	
	
	if ( grep /lat/, $line )
	{
		if (grep /ID/, $line )
		{
			next;
		}
		else
		{	
			my @lats = split('>',$line);
			$lat = $lats[1];
			my @lat = split('<',$lat);
			$lat = $lat[0];
#			print "Latitude: $lat\n";
		}
	} 

	if ( grep /lon/, $line )
	{
		my @lons = split('>',$line);
		$lon = $lons[1];
		my @lon = split('<',$lon);
		$lon = $lon[0];
#		print "$stationname, $ID, $lat, $lon\n";
#		print "Longitude: $lon\n";
#		print " \n";


#			my $idstate = substr("$ID",0,3);
##			print "$ID, $idstate\n";
#			$idstate =~ s/^\s+|\s+$//g;
#			if ( ( $idstate == 865 ) or ( $idstate == 863 ) or ( $idstate == 857 ) or ( $idstate == 866 ) )
#			{
#				print $NCstations "$stationname, $ID, $lat, $lon\n";
#			}

	

#	if ( grep /"\/station"/, $line )
#	{
#		print "STATION DATA FILLED\n";
#		$stationname=;
#		$ID=;
#		$lat=;
#		$lon=;
#		print " \n";
#	}

# Using distance from a set lat/lon point, find all the stations that are within a certain lat/long radius of the
# focal point.  From there get data for a specified date range by building the URL, and wgetting from system call

		my $dist = sqrt((($getlat - $lat) * ($getlat - $lat))+(($getlon - $lon) * ($getlon - $lon)));
		if ( $dist < $maxdist )
		{
#			print $textlist "$navdname, $ID, $lat, $lon\n";
			if ( grep /NAVD88/, $navdname)
			{  
				my $url = "tidesandcurrents.noaa.gov/api/datagetter?begin_date=$startdate 00:00&end_date=$enddate 23:59&station=$ID&product=$nosproduct&datum=navd&units=metric&time_zone=gmt&application=web_services&format=csv";
				system("wget -O $event/$ID-$event-predictednos.txt '$url'");
				print $predictlist "$ID-$event-predictednos.txt\n";
				print $noslist "$ID-$event-predictednos.txt\n";
			}
			else 
			{
#				print $nonstations "$stationname, $ID\n";
#				$navdname = $stationname;
#				my $url = "tidesandcurrents.noaa.gov/api/datagetter?begin_date=$startdate 00:00&end_date=$enddate 23:59&station=$ID&product=datums&&units=metric&time_zone=gmt&application=web_services&format=csv";
#				system("wget -O $event/$ID-datums.txt '$url'");
				
				if ( grep /MSL/, $navdname )
				{
					my $url = "tidesandcurrents.noaa.gov/api/datagetter?begin_date=$startdate 00:00&end_date=$enddate 23:59&station=$ID&product=$nosproduct&datum=msl&units=metric&time_zone=gmt&application=web_services&format=csv";
					system("wget -O $event/$ID-$event-predictednos.txt '$url'");
					print $predictlist "$ID-$event-predictednos.txt\n";
					print $noslist "$ID-$event-predictednos.txt\n";
				}
				elsif ( grep /MTL/, $navdname )
				{
					my $url = "tidesandcurrents.noaa.gov/api/datagetter?begin_date=$startdate 00:00&end_date=$enddate 23:59&station=$ID&product=$nosproduct&datum=mtl&units=metric&time_zone=gmt&application=web_services&format=csv";
					system("wget -O $event/$ID-$event-predictednos.txt '$url'");
					print $predictlist "$ID-$event-predictednos.txt\n";
					print $noslist "$ID-$event-predictednos.txt\n";
				}							
			}
			print $textlist "$navdname, $ID, $lat, $lon\n";


# Get the Louisiana stations using a distance from the 30N 90W point. (Galveston to PanamaCity Beach)
#		my $lamaxdist = 5.0;
#		my $ladist = sqrt((($getlat - $lat) * ($getlat - $lat))+(($getlon - $lon) * ($getlon - $lon)));
#		if ( $ladist < $lamaxdist )
#		{
#			print $LAstations "$stationname, $ID, $lat, $lon\n";
##			my $url = "tidesandcurrents.noaa.gov/api/datagetter?begin_date=$startdate 10:00&end_date=$enddate 10:24&station=$ID&product=water_level&datum=msl&units=metric&time_zone=gmt&application=web_services&format=csv";
##			system("wget -O $ID-$event-nos.txt '$url'");

		}	
	}
	$linecount++;
}








exit;

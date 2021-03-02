#!/usr/bin/env perl
$curl="curl";

# get_inv.pl            wesley ebisuzaki
# v0.9  1/2005
#
# gets a wgrib inventory from the net
# adds cURL compatible "range" field (byte address of grib record)
#
# requires:
#   curl:   http://curl.haxx.se open source MIT/X derivate license
#   perl:   open source
#
# configuration:
#   line 1:  #!{location of perl} -w
#   line 2:  curl="{location of curl executable}";
#
# v0.9 1st version
# v0.9.3 2/2005
# v0.9.4 5/2006 grib2 support
# v0.9.5 7/2006 grib2 support - no need for -range option in inventory
# v0.9.6 1/2009 someone actually used the -range option in the inventory, remove error message
# v0.9.7 4/2017 allow URLs that start with https:
#
$version="get_inv.pl v0.9.7 4/2017 wesley ebisuzaki\n";
$file='';
foreach $_ (@ARGV) {
  SWITCH: {
    /^-v$|^--version$/ && do { print STDERR $version; exit 8; };
    /^ftp:|^http:|^https:/ && do {
	 if ($file eq '') { $file = $_; last SWITCH; }
	 else {
	   print STDERR "error: multiple URLs found\n";
	   exit 8;
	 }
      };
    /^-h$|^--help$/ && do {
        print STDERR "$0: gets wgrib inventory from net, adds range field\n";
        print STDERR "   usage: $0 URL-of-wgrib-inventory\n";
        print STDERR "   ex: $0 http://nomad3.ncep.noaa.gov/pub/gfs/rotating/gblav.t00z.pgrbf12.inv\n\n";
        print STDERR "This program is part of a package to select GRIB records (messages)\n";
        print STDERR "to download.  By selecting specific records, the download times can\n";
        print STDERR "be significantly reduced.  This program uses cURL and supports grib\n";
        print STDERR "data on http: (most) and ftp: servers that include wgrib inventories.\n";
        print STDERR "ref: http://www.cpc.ncep.noaa.gov/products/wesley/fast_downloading_grib.html\n";
        exit 8;
      };
    print STDERR "error: unknown parameter: $_\n";
    exit 8;
  }
}

if ($file eq '') {
  print STDERR $version;
  print STDERR "\n$0: gets wgrib inventory from net, adds range field\n";
  print STDERR "   usage: $0 URL-of-wgrib-inventory\n";
  exit 8;
}

open (In, "$curl -f -s $file |");

$last=0;
$lastnum = -1;
$has_range = 0;
while (<In>) {
  if (/:range=/) {
#    grib2 inventory has range field
     $has_range = 1;
     print $_;
  }
  else {
#    grib1/2 inventory, figure range field
     chomp;
     ($f1,$num,$rest) = split(/:/,$_,3);

#    check for missing file
     if (! defined($num) || "$num" eq "") {
        sleep(5);
        print STDERR "ERROR: Bad URL or not wgrib inventory: $file\n";
        sleep(3);
        exit 7;
     }

     if ($lastnum != $num && $last != 0) {
        $n = $num - 1;
        for ($i = 0; $i < $last; $i++) {
            print "$old_lines[$i]:range=$lastnum-$n\n";
        }
	      $last = 1;
        $old_lines[0] = $_;
     } else {
        $old_lines[$last++] = $_;
     }
   	$lastnum = $num;
  }
}


$firstCall = 1;
$repeated = 0;
$lastStartRange = -1;
$has_range = 0;
while (<In>) {
  if (/:range=/) {
#    grib2 inventory has range field
     # just pass the stdin to the stdout
     $has_range = 1;
     print $_;
  }
  else {
#    grib1/2 inventory, figure range field
     # e.g.: 
     # 1:0:d=2021030106:PRMSL:mean sea level:anl:
     # 2:233889:d=2021030106:PRES:1 hybrid level:anl:
     # 3:476054:d=2021030106:RWMR:1 hybrid level:anl:
     chomp;
     ($f1,$startRange,$rest) = split(/:/,$_,3);

#    check for missing file
     if (! defined($startRange) || "$startRange" eq "") {
        sleep(5);
        print STDERR "ERROR: Bad URL or not wgrib inventory: $file\n";
        sleep(3);
        exit 7;
     }
     # if the byte index for the start of the data has not 
     # changed from the previous line, and this is not the first
     # trip through the loop
     if ($lastStartRange != $startRange && $firstCall != 1) {
        $lastEndRange = $startRange - 1;
        for ($i = 0; $i < $repeated; $i++) {
            #print "$_\n"; # jgfdebug
            print "$old_lines[$i]:range=$lastStartRange-$lastEndRange\n";
        }
        $lastStartRange = $startRange;
	    $repeated = 1;
        $old_lines[0] = $_;
     }
     else {
        # byte index same as last line, just record stdin
        $old_lines[$repeated++] = $_;
	    $lastStartRange = $startRange;   
     }
  }
}

# finished dumping stdin to stdout for grib2 inventories that already have the range field in them
if ($has_range == 1) { exit 0; }

$f1="";
$rest="";
if ($repeated != 0) {
    for ($i = 0; $i < $repeated; $i++) {
        print "$old_lines[$i]:range=$lastStartRange\n";
    }
}
else {
  sleep(5);
  print STDERR "missing wgrib inventory\n";
  sleep(3);
  if (! -t STDIN) {
#   not a terminal .. sleep longer
    sleep(60);
  }
  exit 6;
}
exit 0;

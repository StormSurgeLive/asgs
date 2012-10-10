#!/usr/bin/env perl

$^W++;
use strict;
use Getopt::Long;

my $template;
my $tropcyc;
my $year;
my $storm;
my $advisory;
my $kmzflag;
my $datetime;

GetOptions(
           "template=s" => \$template,
           "tropcyc=i" => \$tropcyc,
           "year=s" => \$year,
           "storm=s" => \$storm,
           "advisory=s" => \$advisory,
           "kmzflag=i" => \$kmzflag,
);

if ( $tropcyc == 0 ) {
    $datetime = $advisory;
} else {
    open(WINDFILE,"../fort.22") || die "ERROR: Cannot open ../fort.22.";
    while(<WINDFILE>) {
        my($line) = $_;
        chomp($line);
        $datetime = substr($line,8,10);
    }
    close(WINDFILE);
}

open(TEMPLATE,"$template") || die "ERROR: Cannot open $template.";
while(<TEMPLATE>) {
    if ( $tropcyc == 0 ) {
        s/%plotlabel%/Adv $advisory/g;
    } else {
        s/%plotlabel%/$year Storm $storm Adv $advisory/g;
    }
    if ( $kmzflag == 0 ) {
        s/%al%/    /g;
        s/%kmzflag%/0/g;
        s/%ext%/eps  /g;
    } else {
        s/%al%/KMZ_/g;
        s/%kmzflag%/1/g;
        s/%ext%/png  /g;
    }
    s/%kmztime%/$datetime/g;
    print $_;
}
close(TEMPLATE);


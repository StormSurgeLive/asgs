#!/usr/bin/env perl

$^W++;
use strict;
use Getopt::Long;

my $template;
my $name;
my $filechoice;
my $shape;
my $vchoice;
my $domain;
my $l;
my $lonlatbuffer;

GetOptions(
           "template=s" => \$template,
           "name=s" => \$name,
           "filechoice=s" => \$filechoice,
           "shape=s" => \$shape,
           "vchoice=s" => \$vchoice,
           "domain=s" => \$domain,
           "l=s" => \$l,
	   "lonlatbuffer=s" => \$lonlatbuffer,
);

open(TEMPLATE,"$template") || die "ERROR: Cannot open $template.";
while(<TEMPLATE>) {
    s/%name%/$name/g;
    s/%filechoice%/$filechoice/g;
    s/%shape%/$shape/g;
    s/%vchoice%/$vchoice/g;
    s/%domain%/$domain/g;
    s/%l%/$l/g;
    s/%lonlatbuffer%/$lonlatbuffer/g;
    print $_;
}
close(TEMPLATE);


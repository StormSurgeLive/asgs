#!/usr/bin/env perl
use strict;
use warnings;
use Getopt::Long;

my $template;
my $postdir;
my $jobname;
my $account;
my $executable;
my $useremail;
my $walltime;
my $pbsscript;

GetOptions ("template=s" => \$template,
            "postdir=s" => \$postdir,
            "jobname=s" => \$jobname,
            "account=s" => \$account,
            "executable=s" => \$executable,
            "useremail=s" => \$useremail,
            "walltime=s" => \$walltime,
            "pbsscript=s" => \$pbsscript);

      
open IN, "<$template" or die "cant open $template\n";

# process the args that go into the executable
# @ARGV contains what is left after the "--" option after GetOptions does its thing
# we'll just pass  the left overs on to the executable down below 
my $args=join(' ',@ARGV); 

open OUT, ">$pbsscript" or die "cant open $pbsscript\n";

while (<IN>){
   chomp;
  
   $_ =~ s/%jobname%/$jobname/g;
   
   my $oefile="$postdir/$jobname.pbs.oe";
   $_ =~ s/%oefile%/$oefile/g;
  
   $_ =~ s/%postdir%/$postdir/g;
   $_ =~ s/%account%/$account/g;
   $_ =~ s/%useremail%/$useremail/g;
   $_ =~ s/%walltime%/$walltime/g;
   
   my $executeline="$executable $args";
   
   $_ =~ s/%executeline%/$executeline/g;
      
   print OUT "$_\n";
};







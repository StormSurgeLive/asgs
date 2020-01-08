#!/usr/bin/env perl

use strict;
use warnings;
use Test::More;

open my $fh, q{<}, q{./PERL-MODULES};
for my $module (<$fh>) {
  chomp $module;
  require_ok $module;
}

done_testing();

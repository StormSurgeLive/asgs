#!/usr/bin/env perl

use strict;
use warnings;
use Test::More;

open my $fh, q{<}, q{./PERL-MODULES};
for my $module (<$fh>) {
  chomp $module;
  require_ok $module;
}

# Testing list of modules that are not handled through the
# automated manifest, ./PERL-MODULES

require_ok q{AdcGrid};

done_testing();

#!/usr/bin/env perl
#
# Copyright(C) 2006, 2007, 2008, 2009 Jason Fleming
# Copyright(C) 2006, 2007 Brett Estrade
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
#
$^W++;
use strict;

# usage:
#   %perl parse72.pl < fort.72 > outfile.72
# Format of outfile.72:
#   time u@stat1 v@stat1 u@stat2 v@stat2 ... u@statN v@statN

# General subroutine used to test if an element is already in an array
sub is_member {
  my $test = shift;
  my $ret = 0;
  if (defined($test)) {
    # This way to test if something is a member is significantly faster
    # ..thanks, PM!
    if (grep {$_ eq $test} @_) {
      $ret++;
    }
  }
  return $ret;
}

# Main proceedural code

my @stations = qw/1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23/;
my $total_stations = 0;
my $record_count = 0;
my @values = ();

while (<>) {
  if ($. == 2) {
    m/^\s*(\d*)\s*(\d*)/;
    $total_stations = $2; # jgf1.2 this should work starting in adc46_08
    #$total_stations = 21; # hardcoded until input file is corrected
    print STDERR "Found $total_stations stations\n";
  } elsif ($. > 2 && ($.-3) % ($total_stations+1) == 0) {
    m/^\s*([^\s]*)\s*([^\s]*)\s*$/;
    my $time = $1;
    if (@values) {
      foreach (@values) {
        printf("%20s ",$_);
      }
      print "\n";
      @values = ();
    }
    push(@values,$time);
    $record_count++;
  } elsif ($. > 2) {
    m/^\s*([^\s]*)\s*([^\s]*)\s*([^\s]*)\s*$/;
    my $station = $1;
    my $u = $2;
    my $v = $3;
    my $mag = sqrt($u*$u+$v*$v);
    $mag = sprintf("%.8E",$mag);
    if (is_member($station,@stations)) {
      push(@values,$u,$v,$mag);
    }
  }
}

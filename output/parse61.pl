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
#   %perl parse61.pl < fort.61 > outfile.61
# Format of outfile.61:
#   time ele@stat1 ele@stat2 ... ele@statN

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
    m/^\s*([^\s]*)\s*([^\s]*)/;
    $total_stations = $2;
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
    m/^\s*(\d*)\s*(.*)\s*$/;
    my $station = $1;
    my $value = $2;
    if (is_member($station,@stations)) {
      push(@values,$value);
    }
  }
}

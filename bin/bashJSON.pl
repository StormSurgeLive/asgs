#!/usr/bin/env perl
#--------------------------------------------------------------
# bashJSON.pl: helper for JSON manipulation via bash script
#--------------------------------------------------------------
# Copyright(C) 2022 Jason Fleming
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
#--------------------------------------------------------------
use strict;
use warnings;
use Getopt::Long;
use JSON::PP;
use Scalar::Util qw( reftype );
use ASGSUtil;
#
my @keys;               # array for keys to values to read
my $keys_ref = \@keys;
my $key;                # single key for which value(s) should be returned
my $first;              # only return the first value of a JSON array
my $last;               # only return the last value of a JSON array
my %mapscalars;         # key/value pairs to write
my %maparrays;         # key/value pairs to write
my $file_content;       # entire file as slurped
#
my @values;  # to be returned
my $jshash_ref;
#
GetOptions(
           "keys=s{1,}" => \$keys_ref,
           "key=s" => \$key,
           "first" => \$first,
           "last" => \$last,
           "mapscalar=s" => \%mapscalars,
           "maparray=s" => \%maparrays
          );
#
#   r e a d   t h e   d a t a
#
# slurp the file contents into a scalar variable
$file_content = do { local $/; <> };
$jshash_ref = JSON::PP->new->decode($file_content);
#
#   r e t u r n   t h e   d a t a
#
if ( @keys ) {
    foreach my $k (@$keys_ref) {
        if ( exists($jshash_ref->{$k}) ) {
            push(@values,$jshash_ref->{$k});
        } else {
            push(@values,"null");
        }
    }
    print join(' ',@values);
    exit;
}
if ( defined $key ) {
    if ( exists($jshash_ref->{$key}) ) {
        if ( reftype($jshash_ref->{$key}) eq 'ARRAY' ) {
            my $listref = $jshash_ref->{$key};
            if ( defined $first ) {
                print $listref->[0];
            } elsif ( defined $last ) {
                print $listref->[-1];
            } else {
                print join(' ',@$listref);
            }
        } else {
            print $jshash_ref->{$key};
        }
    }
    exit;
}
#
#   w r i t e   t h e   d a t a
#
if ( %mapscalars ) {
    foreach my $k (keys(%mapscalars)) {
        $jshash_ref->{$k} = $mapscalars{$k};
    }
}
if ( %maparrays ) {
    foreach my $k (keys(%maparrays)) {
        my @arr = split(" ",$maparrays{$k});
        $jshash_ref->{$k} = \@arr;
    }
}
if ( %mapscalars || %maparrays ) {
    ASGSUtil::timestampJSON($jshash_ref);
    print JSON::PP->new->utf8->pretty->canonical->encode($jshash_ref);
    exit;
}
1;

__END__
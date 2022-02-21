#!/usr/bin/env perl
#--------------------------------------------------------------
# metadata.pl: i/o for json, yaml, and run.properties files
#--------------------------------------------------------------
# Copyright(C) 2021-2022 Jason Fleming
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
# This script accepts one or more keys whose values will be read
# from the specified metadata file; these will be returned as
# a set of key-value pairs.
#
# Alternatively, it will accept a string of key-value pairs to
# be added to the metadata file.
#
# The script determines the format of the metadata file
# automatically using the file extension (.json, .yaml, or
# .properties).
#--------------------------------------------------------------
use strict;
use warnings;
use Getopt::Long;
use JSON::PP;
use YAML::Tiny;
use Date::Calc;
use Cwd;
use File::Basename;
use ASGSUtil;
#
my @keys;                   # array for keys to values to read
my $keys_ref = \@keys;
my $key;                    # single key for which value should be returned
my $mapscalar = "null";     # key/value pairs to write
my $metadatafile = "null";  # file that holds the json or yaml data
my $file_content;           # entire file as slurped
my $yaml;                   # content as string
my $jsonify = 0;            # true if run.properties should be converted to scenario.json (overwriting any existing scenario.json)
my $redact = 0;             # true if email addresses should be removed
my $convertedFileName = "scenario.json"; # default name of converted file (without full path)
#
# the following property values are considered sensitve
my @redacted_properties = qw( notification.opendap.email.opendapnotify );
#
# the following properties from run.properties are deprecated and should
# not be used or stored in scenario.json
my @deprecated_properties = qw( forecast.ensemblesize asgs.path.fromdir
   asgs.path.lastsubdir asgs.enstorm enstorm hostname instance pseudostorm
   intendedAudience asgs.path.advisdir asgs.path.stormdir path.advisdir
   path.stormdir config.forcing.nam.schedule.forecast.forecastcycle
   config.forcing.nam.backsite config.forcing.nam.backdir config.forcing.nam.forecastlength
   config.forcing.nam.reprojection.ptfile config.forcing.nam.local.altnamdir storm
   stormnumber cpurequest ncpu numwriters h0 asgs prodID sea_surface_height_above_geoid
   wind year ADCIRCGrid ColdStartTime Model RunEndTime RunStartTime RunType WindModel
   currentcycle currentdate gusts
);
# the following properties from run.properties are array valued and designated in
# run.properties file with ( ) and should be stored as an array in json
# e.g.:
# post.executable.postprocess : (  createMaxCSV.sh includeWind10m.sh createOPeNDAPFileList.sh opendap_post.sh )
my @paren_properties;
# = qw ( post.executable.postprocess post.opendap.tds
#   hpc.job.padcswan.jobenv hpc.job.prep15.jobenv hpc.job.prep15.subshellpids
#   hpc.job.prep15.nodelist hpc.job.padcswan.jobenv hpc.job.padcswan.subshellpids
#   hpc.job.padcswan.nodelist post.opendap.files post.opendap.lsu_tds.linkablehosts
#   post.opendap.lsu_tds.copyablehosts
#);
# the following properties from run.properties are array valued and designated
# in run.properties as comma separated and should be stored as an array in json
my @comma_properties = qw( notification.email.activate_list
   notification.email.new_advisory_list notification.email.post_init_list
   notification.email.job_failed_list notification.hpc.email.notifyuser
   notification.opendap.email.opendapnotify
);
# the following properties from run.properties are stored as individual
# top level properties but should be considered a sublist of related
# metadata with a list of properties associated with them in turn
# (to start off with, " File Name" and " Format")
# adcirc.file.output.list
my @outputfile_properties = (
    "Water Surface Elevation Stations", "Water Surface Elevation",
    "Water Current Velocity", "Barometric Pressure Stations",
    "Wind Velocity Stations", "Barometric Pressure", "Wind Velocity",
    "Maximum Water Surface Elevation", "Maximum Current Speed",
    "Maximum Wind Speed", "Minimum Barometric Pressure",
    "Maximum Wave Radiation Stress", "Mean Wave Direction",
    "Maximum Mean Wave Direction", "Significant Wave Height",
    "Maximum Significant Wave Height", "Mean Wave Period",
    "Maximum Mean Wave Period", "Peak Wave Period",
    "Maximum Peak Wave Period", "Initially Dry", "Inundation Time",
    "Maximum Inundation Depth", "Ever Dried", "End Rising Inundation",
    "Wind Velocity 10m Stations", "Wind Velocity 10m",
    "Maximum Wind Speed 10m",
);
my @values;  # to be returned
my %properties;
my $jshash_ref;
#
GetOptions(
           "metadatafile=s" => \$metadatafile,
           "keys=s{1,}" => \$keys_ref,
           "key=s" => \$key,
           "mapscalar=s" => \$mapscalar,
           "converted-file-name=s" => \$convertedFileName,
           "jsonify" => \$jsonify,
           "redact" => \$redact
          );
#
#   r e a d   t h e   d a t a
#
my ($filename, $dirs, $suffix);
if ($metadatafile eq "null") {
    # read json from STDIN
    ASGSUtil::stderrMessage("INFO","A metadata file name was not provided; reading JSON from STDIN.");
    # slurp the file contents into a scalar variable
    $file_content = do { local $/; <> };
    $jshash_ref = JSON::PP->new->decode($file_content);
    $suffix = ".json";
} else {
    ($filename, $dirs, $suffix) = fileparse($metadatafile);
    #
    #  J S O N
    if ( $suffix eq ".json" ) {
        my $F;
        unless (open($F,"<$metadatafile")) {
            ASGSUtil::stderrMessage(
                      "ERROR",
                      "Failed to open '$metadatafile': $!.");
            die;
        }
        # slurp the file contents into a scalar variable
        $file_content = do { local $/; <$F> };
        close($F);
        $jshash_ref = JSON::PP->new->decode($file_content);
    #
    #  Y A M L
    } elsif ( $suffix eq ".yaml" ) {
        $yaml = YAML::Tiny->read($metadatafile);
    #
    #  P R O P E R T I E S
    } elsif ( $suffix eq ".properties" ) {
        my $RUNPROP;
        unless (open($RUNPROP,"<$metadatafile")) {
            ASGSUtil::stderrMessage(
                      "ERROR",
                      "Failed to open '$metadatafile': $!.");
            die;
        }
        while (<$RUNPROP>) {
            my @fields = split ':',$_, 2 ;
            # strip leading and trailing spaces and tabs
            $fields[0] =~ s/^\s|\s+$//g ;
            $fields[1] =~ s/^\s|\s+$//g ;
            $properties{$fields[0]} = $fields[1];
        }
        close($RUNPROP);
    #
    #  U N R E C O G N I Z E D
    } else {
        ASGSUtil::stderrMessage(
            "ERROR",
            "Unrecognized file suffix: '$suffix'. ".
            "Only .json, .yaml, and .properties are supported file types.");
        die;
    }
}
#
#   n o w   w r i t e   t h e   d a t a
#
if ( $suffix eq ".json" ) {

    if ( @keys ) {
        foreach my $k (@$keys_ref) {
            if ( exists($jshash_ref->{$k}) ) {
                push(@values,$jshash_ref->{$k});
            } else {
                push(@values,"null");
            }
        }
        print join(" ",@values);
        exit;
    }
    if ( defined $key ) {
        if ( exists($jshash_ref->{$key}) ) {
            print $jshash_ref->{$key};
        } else {
            print "null";
        }
        exit;
    }
} elsif ( $suffix eq ".yaml" ) {
    if ( $keys_ref ) {
        foreach my $k (@$keys_ref) {
            my $value = $yaml->[0]->{$k};
            if (defined $value) {
                push(@values,$value);
            } else {
                push(@values,"null");
            }
        }
        print join(" ",@values);
        exit;
    }
} else {
    #
    #  p r o p e r t i e s   f i l e
    #
    # filter out deprecated properties
    foreach my $dp (@deprecated_properties) {
        delete $properties{$dp};
    }
    # filter out redacted properties if requested
    if ( $redact ) {
        foreach my $rp (@redacted_properties) {
            if ( exists($properties{$rp}) ) {
                delete $properties{$rp};
            }
        }
    }
    # if a property value was requested, write the value to stdout and exit
    if ( $keys_ref ) {
        foreach my $k (@$keys_ref) {
            if ( exists($properties{$k})) {
                my $value = $properties{$k};
                push(@values,$value);
            } else {
                push(@values,"null");
            }
        }
        print join(" ",@values);
        exit;
    }
    # if there are leading and trailing parentheses, add this
    # to our list of paren properties
    foreach my $k (keys %properties) {
       if ( substr($properties{$k},0,1) eq "(" && substr($properties{$k},-1,1) eq ")" ) {
          push(@paren_properties,$k)
       }
    }
    # convert the properties file to json, creating json arrays
    # and subarrays in the appropriate places
    if ( $jsonify ) {
        foreach my $pp (@paren_properties) {
            if ( exists($properties{$pp})) {
                my @list_items = split(" ",$properties{$pp});
                # remove leading and trailing parentheses, which will be the first and last fields
                if ( $list_items[-1] eq ")" ) {
                    pop(@list_items);
                }
                if ( $list_items[0] eq "(" ) {
                    shift(@list_items);
                }
                # if the list is empty, add the list item "null"
                if ( @list_items == 0 ) {
                    push(@list_items,"null");
                }
                $properties{$pp} = \@list_items; # add list to hash to replace scalar representation of this list
            }
        }
        # turn property values that are meant to be a list (comma separated)
        # into a json array
        foreach my $cp (@comma_properties) {
            if ( exists($properties{$cp})) {
                my @list_items = split(",",$properties{$cp});
                $properties{$cp} = \@list_items; # add list to hash to replace scalar representation of this list
            }
        }
        # turn the output file properties into a sublist
        # with their own hashes for file name, file format, etc
        my @outputlist;
        foreach my $op (@outputfile_properties) {
            if ( exists($properties{"$op File Name"}) ) {
                # add the $op as the new key
                # populate it with a hash containing File Name and Format
                # delete the old "$op File Name" and "$op Format" keys
                my $fn = $properties{"$op File Name"};
                my $ff = $properties{"$op Format"};
                delete $properties{"$op File Name"};
                delete $properties{"$op Format"};
                my %fp = ( "description", $op, "name", $fn, "format", $ff );
                push(@outputlist,\%fp);
            }
        }
        $properties{"adcirc.files.output"} = \@outputlist;
        # now encode as json and write out
        unless ( open(SJ,">$dirs/$convertedFileName") ) {
            ASGSUtil::stderrMessage("ERROR","Could not open '$dirs/$convertedFileName' for writing: $!.");
        }
        my $json = JSON::PP->new->utf8->pretty->canonical->encode(\%properties);
        print SJ $json;
        close(SJ);
    }
}
1;

__END__
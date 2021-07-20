#!/usr/bin/env perl
#--------------------------------------------------------------
# metadata.pl: i/o for json, yaml, and run.properties files
#--------------------------------------------------------------
# Copyright(C) 2021 Jason Fleming
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
$^W++;
use strict;
use Getopt::Long;
use JSON::PP;
use YAML::Tiny;
use Date::Calc;
use Cwd;
#
my $keys = "null";          # keys for values to read FIXME: only works if a single key is provided
my $mapscalar = "null";     # key/value pairs to write
my $metadatafile = "null";  # file that holds the json or yaml data
my %mapping;                # deserialized hash 
my $file_content;           # entire file as slurped
my $yaml;                   # content as string
my $jsonify = 0;            # true if run.properties should be converted to scenario.json (overwriting any existing scenario.json)
my $convertedFileName = "scenario.json"; # default name of converted file (without full path)
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

our $this = "metadata.pl"; 
#
GetOptions(
           "metadatafile=s" => \$metadatafile,    
           "keys=s" => \$keys,
           "mapsacalar=s" => \$mapscalar,
           "converted-file-name=s" => \$convertedFileName,
           "jsonify" => \$jsonify           
          );
# open metadata file
if ($metadatafile eq "null") { 
   &stderrMessage("ERROR","Did not provide the name of the metadata file.");
   exit 1;
}    
# remove a trailing slash (if any, just in case)
if ( substr($metadatafile,-1,1) eq "/" ) {
    chop($metadatafile);
}
# determine the file type by grabbing all characters from last dot to end of line
my ($type) = $metadatafile =~ /(\.[^.]+)$/;
# determine path to the metadata file (if any was provided)
my $dirpath = "";
my $last_slash = rindex($metadatafile,"/");
if ( $last_slash != -1 ) {
    $dirpath = substr($metadatafile,0,$last_slash);
}
#
#  J S O N
if ( $type eq ".json" ) {
    unless (open(F,"<$metadatafile")) {
        &stderrMessage("ERROR","Failed to open '$metadatafile': $!.");
        die;
    }
    # slurp the file contents into a scalar variable
    $file_content = do { local $/; <F> };
    close(F);        
    my $ref = JSON::PP->new->decode($file_content);
    %mapping = %$ref;
    if ($keys ne "null" && exists($mapping{$keys})) {
        print $mapping{$keys};
    } else {
        print "null";
    }     
#
#  Y A M L   
} elsif ( $type eq ".yaml" ) {
    $yaml = YAML::Tiny->read($metadatafile);
    if ($keys ne "null") {
        my $value = $yaml->[0]->{$keys};
        if (defined $value) {
            print $value;
        } else {
            print "null";
        }
    }
#
#  P R O P E R T I E S
} elsif ( $type eq ".properties" ) {
    unless (open(RUNPROP,"<$metadatafile")) {
        &stderrMessage("ERROR","Failed to open '$metadatafile': $!.");
        die;
    }
    my %properties;
    while (<RUNPROP>) {
        my @fields = split ':',$_, 2 ;
        # strip leading and trailing spaces and tabs
        $fields[0] =~ s/^\s|\s+$//g ;
        $fields[1] =~ s/^\s|\s+$//g ;
        $properties{$fields[0]} = $fields[1];
    }
    close(RUNPROP);
    # filter out deprecated properties
    foreach my $dp (@deprecated_properties) {
        delete $properties{$dp};
    }
    # if a property value was requested, write the value to stdout and exit
    if ($keys ne "null") {
        if ( exists($properties{$keys})) {
            print $properties{$keys};
        } else {
            print "null";
        } 
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
        unless ( open(SJ,">$dirpath/$convertedFileName") ) {
            &stderrMessage("ERROR","Could not open '$dirpath/$convertedFileName' for writing: $!.");
        }
        my $json = JSON::PP->new->utf8->pretty->canonical->encode(\%properties);
        print SJ $json;
        close(SJ);
    }
} else {
    &stderrMessage("ERROR","File type was not recognized (requires either 'json' or 'yaml' or '.properties').");
}

1;

#
# write a log message to stderr
sub stderrMessage () {
    my $level = shift;
    my $message = shift;
    my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
       (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
    my $year = 1900 + $yearOffset;
    my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
    my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
    #printf STDERR "$theTime $level: $enstorm: get_nam.pl: $message\n";
    printf STDERR "$theTime $level: $this: $message\n";
    &appMessage($level,$message);
}
#
# write a log message to a log file dedicated to this script (typically debug messages)
sub appMessage () {
    my $level = shift;
    my $message = shift;
    unless ( open(APPLOGFILE,">>metadata.pl.log") ) { 
        &stderrMessage("ERROR","Could not open '$this.log' for appending: $!.");
        &appMessage("ERROR","Could not open '$this.log' for appending: $!.");
        return; 
    }
    my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
    (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
    my $year = 1900 + $yearOffset;
    my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
    my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
    #printf APPLOGFILE "$theTime $level: $enstorm: get_nam.pl: $message\n";
    printf APPLOGFILE "$theTime $level: $this: $message\n";
    close(APPLOGFILE);
}

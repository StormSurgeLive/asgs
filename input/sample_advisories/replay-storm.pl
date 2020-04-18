#!/usr/bin/env perl

package local::replayStorm;

use strict;
use warnings;
use Config::Tiny ();
use Getopt::Long ();
use Template     ();
use FindBin      ();
use File::Copy   ();
use XML::RSS     ();

use constant EXIT_ERROR   => 1;
use constant EXIT_SUCCESS => 0;

# modulino dispatcher, handles exceptions
if ( not caller ) {
    local $@;
    my $ret = eval { __PACKAGE__->replay( \@ARGV ) } or undef;
    if ( not $ret or $@ ) {
        warn $@ // q{Unknown error};
        exit EXIT_ERROR;
    }
    exit EXIT_SUCCESS;
}

sub replay {
    my $self     = shift;
    my $args_ref = shift // [];

    my $opts_ref = {};
    my @flags    = (
        qw/
          config=s
          /
    );
    my $ret = Getopt::Long::GetOptionsFromArray( $args_ref, $opts_ref, @flags );

    # validate required options (currently just accepts --config)
    if (   not exists $opts_ref->{config}
        or not $opts_ref->{config}
        or not -e $opts_ref->{config} ) {
        die "--config is required or must point to a valid file\n";
    }

    my $config = Config::Tiny->read( $opts_ref->{config} );

    # loop over each storm if listed in [storms] and set to a "truthy" value (like 1)
    my $tracking_ref;    # tracks current advisory for each storing being played
  INITIALIZE_COUNTS:
    foreach my $storm ( keys %{ $config->{storms} } ) {
        if ( $config->{storms}->{$storm} ) {
            $tracking_ref->{$storm} = {
                start   => $config->{$storm}->{start},
                end     => $config->{$storm}->{end},
                current => int $config->{$storm}->{start}
            };
        }
    }

    my @storms       = keys %{ $config->{storms} };
    my $finished_ref = { storms => [] };
  MAIN_LOOP:
    while (@storms) {

        # update index.html with summary in RSS dock root
        $self->update_index_html( $config, $tracking_ref, $finished_ref );

      ROTATE_STORMS:
        foreach my $storm ( keys %{ $config->{storms} } ) {

            next ROTATE_STORMS if not $config->{storms}->{$storm};
            # skip storm if ending advisory has already been reached
            if ( $tracking_ref->{$storm}->{current} >= $config->{$storm}->{end} ) {
                print qq{$storm has ended\n};

                # delete $storm from ROTATE_STORMS loop
                delete $config->{storms}->{$storm};
                push( @{ $finished_ref->{storms} }, $storm );

                # update for MAIN_LOOP
                @storms = keys %{ $config->{storms} };
                next ROTATE_STORMS;
            }

            # move data in place for RSS, updates $tracking_ref by reference
            $self->update_rss( $config, $storm, $tracking_ref->{$storm} );

            # move data in place for FTP, updates $tracking_ref by reference
            $self->update_ftp( $config, $storm, $tracking_ref->{$storm} );

            # update current storm number
            ++$tracking_ref->{$storm}->{current};
        }

        my $wait = $config->{global}->{ADVISORY_INTERVAL};
        print qq{Issuing next "official" advisory in $wait seconds ...\n};
        sleep $wait;
    }
}

sub update_index_html {
    my ( $self, $config, $tracking_ref, $finished_ref ) = @_;
    my $template_file = $config->{global}->{RSS_INDEX_TEMPLATE};
    my $tt            = Template->new( { INCLUDE_PATH => $FindBin::Bin, } );
    my $output;
    $tt->process( $template_file, { config => $config, tracking_ref => $tracking_ref, finished => $finished_ref }, \$output );
    my $index = sprintf( "%s/index.html", $config->{global}->{RSS_ROOT} );
    open my $fh, q{>}, $index || die $!;
    print $fh $output;
    close $fh;
    return 1;
}

sub update_rss {
    my ( $self, $config, $storm, $storm_count_info ) = @_;

    my $_index    = $storm_count_info->{current};
    my $_src_path = $config->{$storm}->{source};
    my $_storm    = $config->{$storm}->{storm};
    my $_year     = $config->{$storm}->{year};

    # source index-at.xml
    my $xml_src_file  = sprintf( "%s/%02d.%s%s.index-at.xml", $_src_path, $_index, $_storm, $_year );
    my $xml_dest_file = sprintf( "%s/index-at.xml",           $config->{global}->{RSS_ROOT} );

    # insert <item> from $src_file of this storm into the exposed index-at.xml
    # being served as the RSS XML via HTTP

    $self->_insert_rss( $config, $storm, $xml_src_file, $xml_dest_file );

    return;
}

sub _insert_rss {
    my ( $self, $config, $storm, $src_file, $dest_file ) = @_;

    # if $dest_file doesn't exist, copy the file directly
    if ( not -e $dest_file ) {
        File::Copy::copy $src_file, $dest_file;
        return 1;
    }

    # getting here means $dest_file exists already and we must
    # insert <item> from $src_file into $dest

    my $_basin = $config->{global}->{BASIN_PREFIX};
    my $_storm = $config->{$storm}->{storm};
    my $_year  = $config->{$storm}->{year};

    # string to look for in $item->{description} to determine it's the entry
    # for the storm we're updating
    my $storm_designation = sprintf( "%s%s%s", uc($_basin), $_storm, $_year );

    # open src via XML parser
    my $src_rss = XML::RSS->new;
    $src_rss->parsefile($src_file);

    # open dest (if it exists) via XML parser
    my $dest_rss = XML::RSS->new;
    $dest_rss->parsefile($dest_file);

    # find entry index for $storm if it's in the at-index.xml already
    my $storm_index = $self->_find_index( $storm_designation, $dest_rss );

    # entry found for this storm, so remove it
    if ( defined $storm_index ) {
        my $removed_item = splice( @{ $dest_rss->{items} }, $storm_index, 1 );
    }

    # insert new entry - assumed here to not be in conflict with anything that is
    # currently in $src_rss->{items}
    my $replacement_item = $src_rss->{items}->[0];
    push( @{ $dest_rss->{items} }, $replacement_item );

  REPLACE_CURRENT_XML:
    {
        # write index-at.xml to a temporary file, them perform an atomic mv
        my $tmp_file = qq{/tmp/$$.index-at.xml};
        $dest_rss->save($tmp_file);

        # perform atomic mv to overwrite existing file
        File::Copy::move( $tmp_file, $dest_file );
    }

    return;
}

sub _find_index {
    my ( $self, $storm_designation, $dest_rss ) = @_;
    my $index = 0;
    my $found;
  FIND_ENTRY:
    foreach my $item ( @{ $dest_rss->{items} } ) {
        if ( exists $item->{description} and $item->{description} =~ m/$storm_designation/ ) {
            $found = $index;
        }
        ++$index;
    }
    return $found;
}

sub update_ftp {
    my ( $self, $config, $storm, $storm_count_info ) = @_;

    my $_index    = $storm_count_info->{current};
    my $_src_path = $config->{$storm}->{source};
    my $_storm    = $config->{$storm}->{storm};
    my $_year     = $config->{$storm}->{year};
    my $_basin    = $config->{global}->{BASIN_PREFIX};

    # source index-at.xml
    my $xml_src_file = sprintf( "%s/%02d.%s%s.index-at.xml", $_src_path, $_index, $_storm, $_year );

    # source best track
    my $btk_src_file = sprintf( "%s/%02d.b%s%s%s.dat", $_src_path, $_index, $_basin, $_storm, $_year );

    # source forecast track - NOTE: right now we're using the bkt file
    my $fst_src_file = sprintf( "%s/%02d.b%s%s%s.dat", $_src_path, $_index, $_basin, $_storm, $_year );

    # merge the storm entry from $src_file into $xml_dest_file
    my $xml_dest_file_rss = sprintf( "%s/index-at.xml", $config->{global}->{RSS_ROOT} );
    my $xml_dest_file_ftp = sprintf( "%s%s/index-at.xml", $config->{global}->{FTP_ROOT}, $config->{global}->{FTP_FDIR} );

    # copy btk and fst to FTP
    my $_btk_root     = sprintf( "%s%s", $config->{global}->{FTP_ROOT}, $config->{global}->{FTP_HDIR} );
    my $btk_dest_file = sprintf( "%s/b%s%s%s.dat", $_btk_root, $_basin, $_storm, $_year );

    my $_fst_root     = sprintf( "%s%s", $config->{global}->{FTP_ROOT}, $config->{global}->{FTP_FDIR} );
    my $fst_dest_file = sprintf( "%s/%s%s%s.fst", $_fst_root, $_basin, $_storm, $_year );

    print qq{
    $xml_src_file (merge into) $xml_dest_file_rss
    $xml_src_file (merge into) $xml_dest_file_ftp
    $btk_src_file (replaces) $btk_dest_file
    $fst_src_file (replaces) $fst_dest_file
  };

    # insert
    $self->_insert_rss( $config, $storm, $xml_src_file, $xml_dest_file_rss );
    $self->_insert_rss( $config, $storm, $xml_src_file, $xml_dest_file_ftp );

    # replace
    File::Copy::copy $btk_src_file, $btk_dest_file;
    File::Copy::copy $fst_src_file, $fst_dest_file;

    return;
}

1

__END__

=head1 NAME

  replay-storm.pl

=head1 DESCRIPTION

The purpose of this script is to provide a firm foundation for replaying tropical storms using NHC
advisories and forecasts of real storms or synthetic storms (not generated by the NHC). It is meant
to be used in conjunction with a server that provides both FTP and RSS (over http) access set up to
emulate NHC's services.

=head2 Note on updating C<index-at.xml>

Since the purpose is to support running more than one storm at a time, this script updates the RSS
feed (C<index-at.xml>) but inserting/updating the storm information into the C<items> section rather than
overwriting the file that can be done easily with the C<best track> (e.g., C<bal142019>) and the
track forecasts (e.g., C<al052019>) since they are storm specific files and may reside next to
data for other storms. This behavior may change since it's not entirely clear how NHC represents
multiple storms in the C<index-at.xml>), but what's done here is a reasonable assumption.

=head1 OPTIONS

There is only single option, C<--config>, and this is meant to point to the configuration file that
describes the storms to replay and the parameters for each storm. Attempting to provide command
line flags meant to facilitate this would have resulted in an unusable utility. See the section below
on configuring storms for more information.

=head1 CONFIGURATION

There are various major sections that are fixed. Then their are sections that are required depending
on the value list of storms that are included in the C<[storms]> section.

=head2 C<[global]> Section

=over 3

=item C<FTP_ROOT>

Defines base directory that contain C<FTP_FDIR> and C<FTP_HDIR>.

=item C<FTP_FDIR>

Directory where forecasts data files are put, those that look like C<alXXYYYY.fst>.

=item C<FTP_HDIR>

Directory where best track data files are put, those that look like C<balXXYYYY.dat>.

=item C<RSS_ROOT>

Directory where the RSS file is placed, i.e., C<index-at.xml>.

=item C<RSS_INDEX_TEMPLATE>

Placed in the same directory as this script. The default should probably be used if there
is a desire to create one's own since it's a good example of the available variables.

=item C<BASIN_PREFIX>

Designate the ocean basin in which the storm appears. Generally for ASGS, this value is set
to C<al> (for the Atlantic basin).

=back

=head2 C<[status_page]> Section

=over 3

=item C<FTP_URL>

Used only in the default WWW index template that defines the FTP URL used in the link.

=back

=head2 C<[storms]> Section

This section defines all the storms that are being replayed. Each storm name forms the
left hand C<key>. A value of 1 will designate the storm as "activated", so one may keep
a list of storm names that may be toggled on/off.

Note: For each storm name, a subsequent section in the configuration file must be created
to define per-storm parameters.

=over 3

=item C<STORM_NAME>

For each storm you wish to replay, there must be an entry. The right hand side of the assignment
must be C<1> for the storm to be considered C<active>. See the Example Configuration below
for clarification.

=back

=head2 Per-Storm Configurations

Each storm named in the C<[storms]> section requires its own section. The following are the
expected per-storm parameters.

=over 3

=item C<source>

Absolute path of the directory containing all of the advisory files used for each
replay step of the storm.

=item C<storm>

This is the C<storm number> as designated by the NHC.

=item C<year>

This is the year that the storm occurred.

=item C<start>

The starting advisory number for the replay of this storm. In most cases, this is going
to just be C<1>.

=item C<end>

The ending advisory number for the storm. Once this advisory number is reached, the storm
will be considered to be C<finished>.

=item C<nowify>

This setting is not used, but will be once the feature to modify the dates in the advisories
to reflect the current time.

=back

=head2 Example Configuration

   ;;
   ;; Configuration file required for replaying storms
   ;;
   
   [global]
   ;; data directories (absolute paths)
   FTP_ROOT=/home/user/data
   ;; appended to FTP_ROOT
   FTP_FDIR=/atcf/afst
   FTP_HDIR=/atcf/btk
   RSS_ROOT=/home/user/rss-data
   ;; template used for index.html in RSS_ROOT (relative to location of replay-storm.pl) 
   RSS_INDEX_TEMPLATE=replay-storm.tpl
   ;; al -> "bal" for best track (.btk), "al" for forast track (.fst)
   BASIN_PREFIX=al
   ;; interval between advisories (doesn't affect intermediate/updates
   ;; to mimic NHC updates between advisories)
   ADVISORY_INTERVAL=21600
   
   [status_page]
   FTP_URL=ftp://142.93.48.99/atcf
   
   ;; Storms in run list
   [storms]
   DORIAN  = 1
   MELISSA = 1
   
   [DORIAN]
   source = /home/user/git/asgs/input/sample_advisories/2019/DORIAN
   storm  = 05
   year   = 2019
   start  = 1
   end    = 64
   nowify = no
   
   [MELISSA]
   source = /home/user/git/asgs/input/sample_advisories/2019/MELISSA
   storm  = 14
   year   = 2019
   start  = 1
   end    = 13
   nowify = no

=head1 LICENSE AND COPYRIGHT

This file is part of the ADCIRC Surge Guidance System (ASGS).  The ASGS is
free software: you can redistribute it and/or modify it under the terms of
the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

ASGS is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
the ASGS. If not, see <http://www.gnu.org/licenses/>.

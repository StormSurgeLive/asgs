#!/usr/bin/env perl

package local::nowify;

use strict;
use warnings;
use Getopt::Long ();
use XML::RSS     ();
use Date::Parse  ();
use File::Copy   ();

use constant EXIT_ERROR   => 1;
use constant EXIT_SUCCESS => 0;

{
    no warnings qw/redefine/;

    # monkey patch this method to control attributes in the <rss..> tag
    *XML::RSS::_get_default_modules = sub {
        return {
            'http://purl.org/dc/elements/1.1/' => 'dc',
        };
    };

    *XML::RSS::Private::Output::Base::_get_rdf_decl_open_tag = sub {
        return qq{<rss version="2.0" };
    };

    *XML::RSS::Private::Output::Base::_render_xmlns = sub {
        my ( $self, $prefix, $url ) = @_;
        my $pp = defined($prefix) ? ":$prefix" : "";
        return qq{ xmlns$pp="$url"};
    };

    *XML::RSS::Private::Output::Base::_get_rdf_decl = sub {
        my $self      = shift;
        my $base      = $self->_main()->{'xml:base'};
        my $base_decl = ( defined $base ) ? qq{ xml:base="$base"\n} : "";
        return $self->_get_rdf_decl_open_tag() . $base_decl . $self->_get_rdf_xmlnses() . ">\n";
    };

    *XML::RSS::Private::Output::Base::_output_xml_declaration = sub {
        my $self = shift;
        my $encoding =
          ( defined $self->_main->_encoding() ) ? ' encoding="' . $self->_main->_encoding() . '"' : "";
        $self->_out( '<?xml version="1.0"' . $encoding . '?>' . "\n" );
        if ( defined( my $stylesheet = $self->_main->_stylesheet ) ) {
            my $style_url = $self->_encode($stylesheet);
            $self->_out(qq{<?xml-stylesheet type="text/xsl" href="$style_url"?>\n});
        }
        return undef;
    };

}

# modulino dispatcher, handles exceptions
if ( not caller ) {
    local $@;
    my $ret = eval { __PACKAGE__->nowify( \@ARGV ) } or undef;
    if ( not $ret or $@ ) {
        warn $@ // q{Unknown error};
        exit EXIT_ERROR;
    }
    exit EXIT_SUCCESS;
}

sub nowify {
    my $self     = shift;
    my $args_ref = shift // [];

    my $opts_ref = {};
    my @flags    = (
        qw/
          adv-start=i
          adv-end=i
          best-track-basin=s
          forecast-basin=s
          new-dir=s
          new-start-YYYYDDMM=s
          new-storm=s
          new-name=s
          old-dir=s
          old-storm=s
          old-year=s
          /
    );
    my $ret = Getopt::Long::GetOptionsFromArray( $args_ref, $opts_ref, @flags );

    my $old_storm = $opts_ref->{'old-storm'};    # req
    my $old_year  = $opts_ref->{'old-year'};     # req
    my $old_dir   = $opts_ref->{'old-dir'};      # req

    my $new_storm = $opts_ref->{'new-storm'} // $old_storm;    # not req
    my $new_dir   = $opts_ref->{'new-dir'};                    # req

    my $fst_basin = $opts_ref->{'forecast-basin'} // q{at};    # because NHC is inconsistent about how
    $opts_ref->{'forecast-basin'} = $fst_basin;                # $opts_ref used in subroutine
    my $btk_basin = $opts_ref->{'best-track-basin'} // q{al};  # they designate ocean basins xD
    $opts_ref->{'best-track-basin'} = $btk_basin;              # $opts_ref used in subroutine

    die qq{--new-dir is not defined.\n} if not $new_dir;
    if ( not -d $new_dir ) {
        mkdir $new_dir;
    }

    # parse new-start-YYYYDDMM
    $opts_ref->{'new-start-YYYYDDMM'} =~ m/^(\d\d\d\d)(\d\d)(\d\d)(\d\d)?$/;    # req

    # check outcome of date regex
    die qq{--new-start-YYYYDDMM is not in the correct format: "YYYYDDMM" (same as what's in the best track file).\n} if not $1 or not $2 or not $3;

    # initialize new start date variables
    my ( $start_year, $start_day, $start_mon, $start_hour ) = ( $1,          $2,         $3,         $4 // undef );    # $start_hour defaults to hour in advisory 1 of old storm if not set
    my ( $new_year,   $new_day,   $new_mon,   $new_hour )   = ( $start_year, $start_day, $start_mon, $start_hour );

    $new_year -= 1900;                                                                                                 # internally represented as "years since 1900" by all Perl time functions
    $new_mon  -= 1;                                                                                                    # internally represented as "zero indexed"; e.g., January is 0, Feb is 1, ... Dec is 11

    # Pass 1 - generate mapping of dates/times (old -> new)
    my $_forecast_date_map = {};
    my ( $ss, $mm, $wday, $yday, $isdst );                                                                             # not used but needed for localtime capture
    for my $adv ( $opts_ref->{'adv-start'} .. $opts_ref->{'adv-end'} ) {
        my $rss_xml    = sprintf( "%s/%02d.%s%s.index-%s.xml", $old_dir, $adv, $old_storm, $old_year,  $fst_basin );
        my $best_track = sprintf( "%s/%02d.b%s%s%s.dat",       $old_dir, $adv, $btk_basin, $old_storm, $old_year );

        # fail if anything is missing, assumes 1:1 pair of best track and rss files
        die qq{Can't find $best_track or $rss_xml\n} if ( not -e $best_track or not -e $rss_xml );

        # open up the source index-at.xml file
        my $src_rss = XML::RSS->new;
        $src_rss->parsefile($rss_xml);
        my $pub_date = $src_rss->{channel}->{pubDate};

        my ( $_ss, $_mm, $_hh, $_day, $_month, $_year, $_zone ) = Date::Parse::strptime($pub_date);
        $new_hour //= $_hh;    # if $new_hour was not initialized using --start-YYYYDDMM it uses $_hh from advisory set with --adv-start
        $_forecast_date_map->{$pub_date} = {
            old_year => $_year,
            old_day  => $_day,
            old_mon  => $_month,
            old_hour => $_hh,
            old_tz   => $_zone,,
            new_year => $new_year,
            new_day  => $new_day,
            new_mon  => $new_mon,
            new_hour => $new_hour,
        };

        # first param of _move_date_by_hour is the number of hours; to adjust down, pass a negtive number
        ( $ss, $mm, $new_hour, $new_day, $new_mon, $new_year, $wday, $yday, $isdst ) = $self->_move_date_by_hour( 6, $new_hour, $new_day, $new_mon, $new_year );
    }

    # Pass 2 - modify RSS and best track using precomputed, $_forecast_date_map
    my $_btk_date_map_cache = {};
    for my $adv ( $opts_ref->{'adv-start'} .. $opts_ref->{'adv-end'} ) {
        my $rss_xml_file    = sprintf( "%02d.%s%s.index-%s.xml", $adv,     $old_storm, $old_year, $fst_basin );
        my $rss_xml_full    = sprintf( "%s/%s",                  $old_dir, $rss_xml_file );
        my $best_track_file = sprintf( "%02d.b%s%s%s.dat",       $adv,     $btk_basin, $old_storm, $old_year );
        my $best_track_full = sprintf( "%s/%s",                  $old_dir, $best_track_file );

        # fail if anything is missing, assumes 1:1 pair of best track and rss files
        die qq{Can't find $best_track_full or $rss_xml_full\n} if ( not -e $best_track_full or not -e $rss_xml_full );

        # open up the source index-at.xml file
        my $src_rss = XML::RSS->new;
        $src_rss->parsefile($rss_xml_full);
        my $old_pub_date = $src_rss->{channel}->{pubDate};

        # write updated data to new btk file
        $self->_update_best_track( $old_pub_date, $best_track_full, $_forecast_date_map, $_btk_date_map_cache, $adv, $opts_ref );
        $self->_update_rss( $rss_xml_full, $_forecast_date_map, $adv, $opts_ref );
    }

    return 1;
}

# Creates standard W3CDTF time stamp for use as RSS pubDate
sub _newdate_as_pubtime {
    my ( $self, $new_time ) = @_;

    # Sun, 18 Sep 2005 03:00:00 GMT
    #  %a, %d  %b   %Y %H:00:00 GMT
    my $W3CDTF = POSIX::strftime( qq{%a, %d %b %Y %H:00:00 GMT}, 0, 0, $new_time->{new_hour}, $new_time->{new_day}, $new_time->{new_mon}, $new_time->{new_year} );

    return $W3CDTF;
}

# update all dates in RSS fields and in the NHC forecast using the date map
sub _update_rss {
    my ( $self, $rss_xml_full, $_forecast_date_map, $adv, $opts_ref ) = @_;
    my $old_storm      = $opts_ref->{'old-storm'};
    my $old_year       = $opts_ref->{'old-year'};
    my $new_storm      = $opts_ref->{'new-storm'};
    my $new_storm_name = ( $opts_ref->{'new-name'} ) ? uc $opts_ref->{'new-name'} : undef;    # force --new-name to UPPER CASE
    my $new_dir        = $opts_ref->{'new-dir'};
    my $fst_basin      = $opts_ref->{'forecast-basin'};

    # open up the source index-at.xml file
    my $src_rss = XML::RSS->new;
    $src_rss->parsefile($rss_xml_full);
    my $channel_pub_date = $src_rss->{channel}->{pubDate};
    my $item_pub_date    = $src_rss->{items}->[0]->{pubDate};
    my $description      = $src_rss->{items}->[0]->{description};

    # update pubTime using W3CDTF format (standard for RSS fields)
    my $date_map    = $_forecast_date_map->{$item_pub_date};
    my $new_pubdate = $self->_newdate_as_pubtime($date_map);

    # update RSS fields for pubDate directly
    $src_rss->{channel}->{pubDate} = $new_pubdate;
    $src_rss->{items}->[0]->{pubDate} = $new_pubdate;

    # extract title of old storm and determine the storms name (and if it is a name or a number)
    my $old_full_title = $src_rss->{items}->[0]->{title};
    $old_full_title =~ m/^(.+) Forecast/;
    my $old_storm_title = $1;
    $old_storm_title =~ m/ ([^ ]+)$/;
    my $old_storm_name = $1;

    # if old storm name is still a NUMBER WORD, replace the old NUMBER WORD
    # with the new NUMBER WORD associated with $new_storm

    # if not set above (--new-name is optional), default to $old_storm_name
    $new_storm_name //= $old_storm_name;

    # if $old_storm_name is a number, use NUMBER WORD form of $new_storm, which
    # will be $old_storm if --new-storm is not specified
    # NOTE: a storm designated "INVEST" is not considered to have a NUMBER WORD name
    if ( $self->_storm_name_is_a_number($old_storm_name) ) {
        $new_storm_name = $self->_storm_number_to_word($new_storm);
    }

    # update <title></title> with new name/NUMBER WORD
    $src_rss->{items}->[0]->{title} =~ s/$old_storm_name/$new_storm_name/;

    # update <description></description>, which has an UPPER CASE
    # version of the $old_storm_title
    $old_storm_title = uc $old_storm_title;
    my $new_storm_title = $old_storm_title;
    $new_storm_title =~ s/$old_storm_name/$new_storm_name/;
    $description     =~ s/$old_storm_title/$new_storm_title/g;

    # determine new storm designation
    my $new_storm_id = sprintf( "AL%02d%s", $new_storm, $date_map->{new_year} + 1900 );
    my $old_storm_id = sprintf( "AL%02d%s", $old_storm, $old_year );

    # NWS TPC/NATIONAL HURRICANE CENTER MIAMI FL   AL182005
    $description =~ s/$old_storm_id/$new_storm_id/g;

    # 0300Z SUN SEP 18 2005 (line 5 of forecast text)
    # 'uc' forces all letters to "upper case"
    my $new_line5 = uc POSIX::strftime( qq{%H00Z %a %b %d %Y}, 0, 0, $date_map->{new_hour}, $date_map->{new_day}, $date_map->{new_mon}, $date_map->{new_year} );
    my $old_line5 = uc POSIX::strftime( qq{%H00Z %a %b %d %Y}, 0, 0, $date_map->{old_hour}, $date_map->{old_day}, $date_map->{old_mon}, $date_map->{old_year} );
    $description =~ s/$old_line5/$new_line5/g;

    # get all date strings of the form "DD/HH00Z" (not unique)
    my @day_slash_hourZ = ( $description =~ m/(\d\d\/\d\d\d\dZ)/g );
    my %_uniq_Z         = map { $_ => 1 } @day_slash_hourZ;

    # generate slash Z format to pub year look up (every hour for 7 days - starting 24 hours before old/new dates - just to be safe)
    my $_old_Z_to_new = {};
    my ( $_old_year, $_old_day, $_old_mon, $_old_hour ) = ( $date_map->{old_year}, $date_map->{old_day}, $date_map->{old_mon}, $date_map->{old_hour} - 24 );    # go back 24 hrs
    my ( $_new_year, $_new_day, $_new_mon, $_new_hour ) = ( $date_map->{new_year}, $date_map->{new_day}, $date_map->{new_mon}, $date_map->{new_hour} - 24 );    # go back 24 hrs
    for my $i ( 1 .. 24 * 7 ) {
        my $_old_Z = POSIX::strftime( qq{%d/%H00Z}, 0, 0, $_old_hour, $_old_day, $_old_mon, $_old_year );
        my $_new_Z = POSIX::strftime( qq{%d/%H00Z}, 0, 0, $_new_hour, $_new_day, $_new_mon, $_new_year );
        $_old_Z_to_new->{$_old_Z} = $_new_Z;
        ++$_old_hour;
        ++$_new_hour;
    }
    foreach my $_old_Z ( keys %_uniq_Z ) {
        my $_new_Z = $_old_Z_to_new->{$_old_Z};
        $description =~ s/$_old_Z/$_new_Z/;
    }

    # update RSS item object's <description></description>
    $description =~ s/<pre>/<![CDATA[<pre>/;    # begin CDATA with <pre> tag
    $description =~ s/<\/pre>/<\/pre>]]>/;      # end CDATA after </pre> tag
    $src_rss->{items}->[0]->{description} = $description;

    my $new_rss_file = sprintf( "%s/%02d.%02d%s.index-%s.xml", $new_dir, $adv, $new_storm, $date_map->{new_year} + 1900, $fst_basin );

    my $tmp_rss_file = sprintf( "%s.tmp", $new_rss_file );
    $src_rss->save($tmp_rss_file);
    return File::Copy::move $tmp_rss_file, $new_rss_file;
}

# update all dates in the best track CSV using the date map
sub _update_best_track {
    my ( $self, $pub_internal_date, $btk_file_full, $_forecast_date_map, $_btk_date_map_cache, $adv, $opts_ref ) = @_;
    my $new_storm      = $opts_ref->{'new-storm'};
    my $new_storm_name = ( $opts_ref->{'new-name'} ) ? uc $opts_ref->{'new-name'} : undef;    # force --new-name to UPPER CASE
    my $new_dir        = $opts_ref->{'new-dir'};
    my $btk_basin      = $opts_ref->{'best-track-basin'};

    my ( $ss, $mm, $hh, $day, $month, $year, $zone ) = Date::Parse::strptime($pub_internal_date);

    # get associated best track date from forecast date/time by looking 3 hours in the past
    my ( $btk_ss, $btk_mm, $btk_hh, $btk_day, $btk_month, $btk_year, $btk_wday, $btk_yday, $btk_isdst ) = $self->_move_date_by_hour( -3, $hh, $day, $month, $year );

    # similarly, adjust associated new date by -3 hours
    my $current = $_forecast_date_map->{$pub_internal_date};
    my ( $new_ss, $new_mm, $new_hh, $new_day, $new_month, $new_year, $new_wday, $new_yday, $new_isdst ) = $self->_move_date_by_hour( -3, $current->{new_hour}, $current->{new_day}, $current->{new_mon}, $current->{new_year} );

    my $btk_internal_datetime = sprintf( "%s%02d%02d%02d", $btk_year + 1900, $btk_month + 1, $btk_day, $btk_hh );
    my $new_internal_datetime = sprintf( "%s%02d%02d%02d", $new_year + 1900, $new_month + 1, $new_day, $new_hh );
    $_btk_date_map_cache->{$btk_internal_datetime} = $new_internal_datetime;

    # iterate over data in $btk_file_full, load data, replace old date with new date
    open my $btk_fh, q{<}, $btk_file_full or die $!;
    my $new_data_ref = [];
    while ( my $line = <$btk_fh> ) {
        chomp $line;
        my @data = split /, */, $line;
        my $date = $data[2];

        # directly replace
        $data[2] = $_btk_date_map_cache->{$date};

        my $old_storm_name = $data[27];
        if ( $self->_storm_name_is_a_number($old_storm_name) ) {
            $new_storm_name = $self->_storm_number_to_word($new_storm);
        }
        elsif ( $old_storm_name eq q{INVEST} ) {
            $new_storm_name = q{INVEST};
        }
        else {
            $new_storm_name = ( $opts_ref->{'new-name'} ) ? uc $opts_ref->{'new-name'} : $old_storm_name;    # force --new-name to UPPER CASE
        }
        $data[27] = $new_storm_name;

        push @$new_data_ref, \@data;
    }
    close $btk_fh;

    my $new_btk_file = sprintf( "%s/%02d.b%s%02d%s.dat", $new_dir, $adv, $btk_basin, $new_storm, $new_year + 1900 );
    my $tmp_btk_file = sprintf( "%s.tmp",                $new_btk_file );
    open my $tmp, q{>}, $tmp_btk_file;
    my $_content = q{};

    # some undef values appear in the data, so convert them to empty string - q{}
    foreach my $row (@$new_data_ref) {
        my @row = map { ( defined $_ ) ? $_ : q{} } @{$row};
        $_content .= sprintf( "%s\n", join( q{, }, @row ) );
    }
    print $tmp $_content;
    close $tmp;
    return File::Copy::move $tmp_btk_file, $new_btk_file;
}

# Note: POSIX::strftime happily rolls values greater than expected forward; e.g., it will
# adjust the time appropriately if "hour" >= 24 or "month" >= 12
sub _move_date_by_hour {
    my ( $self, $inc, $new_hour, $new_day, $new_mon, $new_year ) = @_;

    # POSIX::strftime creates correct epoch (unixtime); Perl built-in 'localtime' converts epoch to constituent time parts (sec, hour, etc)
    return localtime( POSIX::strftime( qq{%s}, 0, 0, $new_hour + $inc, $new_day, $new_mon, $new_year ) );
}

# translates storm number to the word for the number used in the forecast
sub _number_as_words {
    [
        qw/
          ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN ELEVEN TWELVE THIRTEEN FOURTEEN FIFTEEN
          SIXTEEN SEVENTEEN EIGHTEEN NINETEEN TWENTY TWENTYONE TWENTYTWO TWENTYTHREE TWENTYFOUR
          TWENTYFIVE TWENTYSIX TWENTYSEVEN TWENTYEIGHT TWENTYNINE THIRTY THIRTYONE THIRTYTWO THIRTYTHREE
          THIRTYFOUR THIRTYFIVE THIRTYSIX THIRTYSEVEN THIRTYEIGHT THIRTYNINE FORTY FORTYONE FORTYTWO
          FORTYTHREE FORTYFOUR FORTYFIVE
          /
    ];
}

sub _storm_name_is_a_number {
    my ( $self, $storm_name ) = @_;
    my $NUMBER_WORDS = $self->_number_as_words;
    return grep { /$storm_name/ } @$NUMBER_WORDS;
}

sub _storm_number_to_word {
    my ( $self, $storm ) = @_;
    return $storm if not int $storm;
    my $NUMBER_WORDS = $self->_number_as_words;
    return $NUMBER_WORDS->[ int $storm - 1 ];
}

1;

__END__

=head1 NAME

   advisory_nowify.pl

=head1 USAGE

   ./advisory_nowify.pl --adv-start <number> --adv-end <number> --old-storm <storm#> --old-year <storm YYYY> --old-dir ./input/dir --new-start-YYYYDDMM= <YYYYDDMM[HH]> --new-dir ./output/dir [--new-storm <number>] [--best-track-basin btk-basin] [--forecast-basin fst-basin]

Example,

   ./advisory_nowify.pl --adv-start 1 --adv-end 30 --old-storm 18 --old-year 2005 --old-dir ./rita --new-start-YYYYDDMM=20202704 --new-dir ./new-rita --new-storm 7 --best-track-basin al --forecast-basin at 

=head1 DESCRIPTION

This script is used take an existing set of advisories (e.g., Rita, AL182005) and
update all time references in the best track and the forecast RSS XML so that
they are relative to a new date passed in using the C<--new-start-YYYYDDMM> flag.

The primary purpose is to facilitated ASGS testing and readiness drills using
selected historical tracks, but using current meteorological data. The preparation
workflow entails running this script first to generate the modified best track
and forecast files in a new directory. Then the <replay-storm.pl> is run using
a configuration file that contains the details of the new storm, including the
new storm number and year (if options are set).

=head1 REQUIRED OPTIONS

=over 3

=item C<--adv-start> number

Designates the first NHC advisory number to process from the old
storm. All storms from C<--adv-start> to C<--adv-end> will be transformed.

=item C<--adv-end> number

Designates the last NHC advisory number to process from the old
storm. All storms from C<--adv-start> to C<--adv-end> will be transformed.

=item C<--new-dir> path/to/new-dir

Designates the directory destination of the generated best track and
forecast files. If this directory doesn't exist, it will be created.

=item C<--new-start-YYYYDDMM> YYYYDDMM[HH]

Defines the new start date in the prescribed format, C<YYYYDDMM>. The
values passed using this flag optionally accepts a two-digit number
representing the hour. Please note the format expected. If the created
advisories have unexpected dates, check to make sure you have not accidentally
swapped C<DD> and C<MM>. The internal date computations will gladly accept
these values.

Example,

    --new-start=20181504   # April 15, 2018 00Z
    
    --new-start=2018150403 # April 15, 2018 03Z

The starting hour, if provided via C<HH>, map directly to the starting hour
of the very first advisory for the old storm. If not set, C<HH>, defaults to
the starting hour of the avisory designated using C<--adv-start>.

Note, there is no default value for this and it is required. Despite the name
of the script contains the word C<nowify>, it is not meant to imply that the
new start literally defaults to C<now()> if not set as an argument.

=item C<--old-dir> path/to/old-storm

Denotes the directory that contains the C<old> best track and forecast files.
Assumes a name convention identical to that used by C<replay-storm.pl> and
used throughout the ASGS C<./input/sample_advisories> directory.

=item C<--old-storm> number

Specifies the NHC storm number of the old storm.

=item C<--old-year> number

Specifies the year in which the old storm occurred.

=back

=head1 NON-REQUIRED OPTIONS

=over 3

=item C<--best-track-basin> btk-basin

Defaults to C<al> (Atlantic Basin), but may be set to something else. Used
to name the best track file, which looks like C<ADVISORY.bBASIN.STORMYEAR.dat>.

=item C<--forecast-basin> fst-basin

Defaults to C<at> (Atlantic Basin), but may be set to something else. Used
to name the forecast file, which looks like C<ADVISORY.index-BASIN.xml>.

=item C<--new-storm> number

This option allows the user to designate a different NHC storm number. If
not set, the new storm number will be the same as the C<--old-storm>. This
is useful if issuing both old and new storms in the same C<replay> ensemble.

=item C<--new-name> new-name

Specify the new storm name as if it was issued by the NHC. The script can detect
when the olds storm is still known as it's NUMBER WORD (Nth storm of the season),
and if this is detected the NUMBER WORD version of C<--new-storm> number will be
substituted in it's place.  Forecast files (.xml) don't seem to have C<INVEST>
storms, but if this is detected, then this will be used in the new forecast. This
also applies to best track files, which do seem to have C<INVEST> as part of their
range of values for the storm name field.

=back

=head1 ASSUMPTIONS

To make a script such as this easier to create and maintain, the following
assumptions are made:

=over 3

=item * For each advisory to be converted, there are corresponding C<best track> and
C<forecast> files.

=item * The best track and forecast files are already consistent. This means that the
forecast file has been issued 3 hours after the last record in the best track file.

=item * The number of records contained in a best track file correspond to the number
of previous forecasts issued. This means for each current forecast, the best track
contains the latest storm track position and metric.

=item * Forecasts are issued every 6 hours.

=back

There are likely other hidden assumptions, but the enumerated ones above should
sufficiently explain any unexpected changes made in the new advisory files. They
are almost sure the result of the data that is contained in the source files
themselves (but maybe it's a bug:)).

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

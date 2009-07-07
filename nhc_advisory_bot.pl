#!/usr/bin/perl
use strict;
use warnings;

# parse rss reports from Nat'l Hurricane Center Atlantic Marine Forecast Advisory.
# NHC Mail (Atlantic Marine) <mail-storm-atlan-marine@seahorse.nhc.noaa.gov>
# See: http://www.nhc.noaa.gov/signup.shtml
# and select the following list: Atlantic Marine (Forecast/Advisories and updates ONLY)

use DateTime::Format::ISO8601;

my %month_lookup = (dummy => '00', JAN=>'01',FEB=>'02',MAR=>'03',APR=>'04',MAY=>'05',JUN=>'06',JUL=>'07',AUG=>'08',SEP=>'09',OCT=>'10',NOV=>'11',DEC=>'12');

# These are the nhc storm number and url params.
my ( $nhc_year, $nhc_number, $nhc_number_str, $nhc_name, $storm_name, $adv_num ) =
  ( -1, 0, '0', '','', 0 );

my ($adv_str, $adv_url) = ('','');

# These are the datum we are extracting
my (
    $date_time,     $lat,       $lon,      $pressure, $diameter,
    $wind_max_sust, $wind_gust, $move_dir, $move_speed, $dt
  )
  = ( '', -1, -1, -1, -1, -1, -1, -1, -1, -1, '', -1 );


    # PARSE EMAIL FOR INFO
    # It doesn't have to be email, we don't care about the headers anyway.
    #
    # This is historical. Originally this was a mail bot script
    #my $email = new Mail::Internet( \*INPUT );
        my @lines =(<>);

    my $body_ref = \@lines;

    my $cnt = @{$body_ref};

    if ( !$cnt ) {
        error_yak ("$0 Exiting: NO EMAIL BODY");
        exit;
    }

    my @match = ();

        # Get the NHC Number
        # NWS TPC/NATIONAL HURRICANE CENTER MIAMI FL   AL172005
    @match = grep /AL\d{2}\d{4}$/, @{$body_ref};
        if(@match){
        if ( $match[0] =~ /AL(\d{2})(\d{4})$/ ) {
                        $nhc_number = $1;
                        $nhc_year = $2;
                        yak( "NUMBER: $nhc_number YEAR: $nhc_year");
                }else{
                        #
                        error_yak("$0 Exiting: NO NHC NUMBER/YEAR");
                exit;
                }
        }

        # now we can set these
        $nhc_number_str = sprintf( "%02d", $nhc_number );


    # Date format
    # 1500Z THU SEP 02 2004
        # July 18th TD 2. HAS CHANGED
        # NOTE NOTE NOW! 1500 UTC TUE JUL 18 2006

        if($nhc_year > 2005){
        @match = grep /^\d{4} .+ \d{4}$/, @{$body_ref};
        }else{
        @match = grep /^\d{4}Z .+ \d{4}$/, @{$body_ref};
        }

    if (@match) {
        $date_time = $match[0];
        chomp $date_time;

        yak("date_time orig: " . $date_time);

        my @vals = split( ' ', $date_time );
        my $hrs = substr( $vals[0], 0, 2 );

                if($nhc_year > 2005) {
                $date_time = "$vals[5]-" . $month_lookup{$vals[3]} . "-$vals[4]T$hrs:00";
                }else{
                $date_time = "$vals[4]-" . $month_lookup{$vals[2]} . "-$vals[3]T$hrs:00";
                }

                $dt = DateTime::Format::ISO8601->parse_datetime($date_time);
                # < 2006 vals[1] is not the timezone. Is there any reason to save it.
                # We know it's UTC
                #$dt->set_time_zone($vals[1]);
                $dt->set_time_zone('UTC');
                $date_time = $dt->ymd . ' ' . $dt->hms . ' UTC';

    }

    @match = grep /FORECAST.+ADVISORY NUMBER/, @{$body_ref};

    # HURRICANE FRANCES ADVISORY NUMBER  40
    # HURRICANE FRANCES FORECAST/ADVISORY NUMBER  37
    # HURRICANE FRANCES FORECAST/ADVISORY NUMBER  37...CORRECTED
    # HURRICANE FRANCES SPECIAL FORECAST/ADVISORY NUMBER  37
    if (@match) {
        if ( $match[0] =~ /^(.+)\s+FORECAST.+ADVISORY NUMBER\s+(\d{1,3})/ ) {
            $nhc_name = $1;
            $adv_num  = $2;
            $nhc_name =~ s/SPECIAL//;
        }
    } else {
        @match = grep /ADVISORY NUMBER/, @{$body_ref};
        if ( $match[0] =~ /^(.+)\s+ADVISORY NUMBER\s+(\d{1,3})/ ) {
            $nhc_name = $1;
            $adv_num  = $2;
        }
    }
        yak( "ADVISORY: $adv_num $nhc_name");
        # Short version of Storm Name
        # This changes when the storm gets named,if it gets named so
        # we DB update this so that all records have the same storm_name for select lists.

        $storm_name = $nhc_name;

        my @tmp = split(' ', $nhc_name);
        if ($tmp[0] eq 'HURRICANE'){
                $storm_name = $tmp[1];
        }
        elsif ($tmp[0] eq 'TROPICAL' or $tmp[0] eq 'SUBTROPICAL'){  # SUBTROPICAL is rare. see 2007 01
                #if($tmp[1] eq 'DEPRESSION'){
                #   This looks shitty but no time to find a better TD solution. So just us TWO as storm name
                #       $storm_name = 'TD_' . $tmp[2];
                #}else{
                        $storm_name = $tmp[2];
                #}
        }

        $adv_str = sprintf( "%03d", $adv_num );
        if($nhc_year > 2005){
        $adv_url = "http://www.nhc.noaa.gov/archive/$nhc_year/al$nhc_number_str/al$nhc_number_str$nhc_year.fstadv.$adv_str.shtml";
        }else{
                # 2005 url
                $adv_url = "http://www.nhc.noaa.gov/archive/$nhc_year/mar/al$nhc_number_str$nhc_year.fstadv.$adv_str.shtml";
        }


    # HURRICANE CENTER LOCATED NEAR 23.4N  73.9W AT 02/1500Z
        # or
        #TROPICAL DEPRESSION DISSIPATING NEAR 29.0N  70.0W AT 24/2100Z
    # or
    # TROPICAL DEPRESSION CENTER LOCATED NEAR 11.2N  36.0W AT 25/0300Z
    @match = grep /(CENTER LOCATED|DISSIPATING) NEAR/, @{$body_ref};

    if (@match) {
        if ( $match[0] =~ /CENTER LOCATED NEAR\s+(\d{1,3}\.\d{1,2})N\s+(\d{1,3}\.\d{1,2})W\s+AT/)
        {
            $lat = $1;
            $lon = -$2;
        }
        if ( $match[0] =~ /DISSIPATING NEAR\s+(\d{1,3}\.\d{1,2})N\s+(\d{1,3}\.\d{1,2})W\s+AT/)
        {
            $lat = $1;
            $lon = -$2;
        }

    }

    @match = grep /^ESTIMATED MINIMUM CENTRAL PRESSURE/, @{$body_ref};

    if (@match) {
        if ( $match[0] =~ /^ESTIMATED MINIMUM CENTRAL PRESSURE\s+(.+)\s+MB/ ) {
            $pressure = $1;
        }
    }

    @match = grep /^EYE DIAMETER/, @{$body_ref};

    # EYE DIAMETER  25 NM
    # tropical depression's have no eyes.
    if (@match) {
        if ( $match[0] =~ /^EYE DIAMETER\s+(\d{1,3})/ ) {
            $diameter = $1;
        }
    }

    #MAX SUSTAINED WINDS  25 KT WITH GUSTS TO  35 KT.
    #MAX SUSTAINED WINDS 125 KT WITH GUSTS TO 155 KT.

    @match = grep /^MAX SUSTAINED WINDS/, @{$body_ref};

    if (@match) {
        if ( $match[0] =~
            /^MAX SUSTAINED WINDS\s+(\d{1,4}) KT WITH GUSTS TO\s+(\d{1,4})/ )
        {
            $wind_max_sust = $1;
            $wind_gust     = $2;
        }
    }

    #PRESENT MOVEMENT TOWARD THE WEST-NORTHWEST OR 295 DEGREES AT  11 KT
    #PRESENT MOVEMENT TOWARD THE WEST OR 280 DEGREES AT  15 KT
    @match = grep /^PRESENT MOVEMENT TOWARD THE/, @{$body_ref};
    if (@match) {
        if ( $match[0] =~ /(\d{1,3}) DEGREES/ ) {
            $move_dir = $1;
        }
        if ( $match[0] =~ /(\d{1,3}) KT$/ ) {
            $move_speed = $1;
        }

    }

    yak( "$adv_url");
    yak(
"$nhc_year, $storm_name, $nhc_number, $adv_num, $nhc_name, $date_time, $lat, $lon, $pressure, $diameter, $wind_max_sust, $wind_gust,  $move_dir, $move_speed\n");

        # Future Forecasts: ##########################
        get_forecasts($body_ref, $date_time, $lat, $lon, $wind_max_sust, $wind_gust, $dt, $storm_name);

# done with advisories for 1 storm
exit;

##########################################################################################

# debug-level messages
sub yak {
    foreach (@_) {
        print "$_\n";
   }
}

# debug-level messages
sub error_yak {
    foreach (@_) {
        print STDERR "$_\n";
    }
}

##############################################################
# FORECAST tracks and points
###############################################################

# FORECAST VALID 10/0000Z 21.5N  84.5W
# FORECASTs have a bizzare date format: dd/hhmm  BUT near end of the month say 31/0000 the forecast dates switch to
# 01/0000, 01/12000 so we need to check for this an increment the month.

sub get_forecasts{
        my($body_ref, $date_time, $lat, $lon, $wind_max_sust, $wind_gust, $dt, $storm_name) = @_;

        # Get the current advisory month and day. We calculate forecast time based on this.
        my $adv_month = $dt->month;
        my $adv_day = $dt->day;
        my $adv_year = $dt->year;

        # first point for the forecast_tracks is the advisory itself
        my @f_points = ();
        my $rec = {};
        $rec->{date} = $date_time;
        $rec->{lat} = $lat;
        $rec->{lon} = $lon;
        $rec->{wind_max} = $wind_max_sust;
        $rec->{wind_gust} = $wind_gust;
        $rec->{time_label} = $dt->strftime("%l:00 %p %a");
        push @f_points, $rec;
    for my $i (0...$#{$body_ref})
        {
                my $f_month = $adv_month;
                my $f_year = $adv_year;
        if( @{$body_ref}[$i] =~ /^FORECAST VALID/){
                        my ($f_day, $f_hour, $f_lat, $f_lon, $f_wind_max, $f_wind_gust) = (-1,-1,-1,-1, -1, -1);
                        my $line = @{$body_ref}[$i];
                        chomp $line;
                if ( $line =~ /^FORECAST VALID\s+(\d{2})\/(\d{4})Z/ )
                        {
                                $f_day = $1;
                        $f_hour = substr( $2, 0, 2 );
                        }
                if ( $line =~ /Z\s+(\d{1,2}\.\d{1,2})N\s+(\d{1,2}\.\d{1,2})W/)
                        {
                                $f_lat = $1;
                                $f_lon = -$2;
                }
                        # Get the next line
                        # MAX WIND  30 KT...GUSTS  40 KT.
                        $i++;
                        $line = @{$body_ref}[$i];
                        chomp $line;
            if ($line =~ /^MAX WIND\s+(\d{1,4}) KT\.\.\.GUSTS\s+(\d{1,4}) KT\./ )
                        {
                                $f_wind_max = $1;
                                $f_wind_gust = $2;
                        }
                        #

                        # we are into next month
                        # OK. Forecast may go into next month and even next year in Dec. So increment f_month and f_year
                        if($f_day < $adv_day){
                                if($adv_month == 12){
                                        $f_month = 1;
                                        $f_year++;
                                }else{
                                        $f_month++;
                                }
                        }

                        $f_month = sprintf("%02d", $f_month);
                my $f_date_time = $f_year . '-' . $f_month .'-' . $f_day . 'T' . "$f_hour:00";

                        my $f_dt = DateTime::Format::ISO8601->parse_datetime($f_date_time);

                        $f_date_time = $f_dt->ymd . ' ' . $f_dt->hms . ' UTC';

                        my $time_label = $f_dt->strftime("%b %e, %l %p");

                        next if ($f_lat == -1 or $f_lon == -1);

                        yak ("Forecast: $f_date_time $f_lat $f_lon $f_wind_max $f_wind_gust");
                        $rec = {};
                        $rec->{lat} = $f_lat;
                        $rec->{lon} = $f_lon;
                        $rec->{date} = $f_date_time;
                        $rec->{wind_max} = $f_wind_max;
                        $rec->{wind_gust} = $f_wind_gust;
                        $rec->{time_label} = $time_label;
                        push @f_points, $rec;
                }
        } # end for

}

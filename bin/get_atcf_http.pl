#!/usr/bin/env perl
# Generic HTTP(S) acquisition adapter for the ASGS get_atcf.pl interface.
#
# Fetches the NHC RSS feed and BEST track over HTTP(S), stages them locally,
# then delegates advisory parsing to the stock ASGS get_atcf.pl in filesystem
# mode.  The same adapter can be used with production NHC or StormReplay.

use strict;
use warnings;
use FindBin;
use Getopt::Long qw(GetOptions);
use HTTP::Tiny;
use File::Basename qw(dirname);
use File::Copy qw(move);
use File::Path qw(make_path);
use File::Spec;

my ($rsssite, $ftpsite, $atcfsite, $fdir, $hdir, $storm, $year, $adv, $trigger, $nhc_name);
my ($rssprotocol, $rssport, $atcfprotocol, $atcfport, $poll_seconds, $stage_dir, $stock);
my ($insecure, $test, $once, $print_urls);

$rssprotocol  = $ENV{GET_ATCF_HTTP_RSS_PROTOCOL}  || 'https';
$atcfprotocol = $ENV{GET_ATCF_HTTP_ATCF_PROTOCOL} || 'https';
$rssport      = $ENV{GET_ATCF_HTTP_RSS_PORT};
$atcfport     = $ENV{GET_ATCF_HTTP_ATCF_PORT};
$poll_seconds = $ENV{GET_ATCF_HTTP_POLL_SECONDS} || 60;
$stage_dir    = $ENV{GET_ATCF_HTTP_STAGE} || '.get_atcf_http';
$stock        = $ENV{GET_ATCF_HTTP_STOCK};

GetOptions(
    'rsssite=s'       => \$rsssite,
    'ftpsite=s'       => \$ftpsite,       # ASGS compatibility name
    'atcfsite=s'      => \$atcfsite,      # clearer alias for new use
    'fdir=s'          => \$fdir,          # retained for ASGS CLI compatibility
    'hdir=s'          => \$hdir,          # remote BEST-track path
    'storm=s'         => \$storm,
    'year=s'          => \$year,
    'adv=s'           => \$adv,
    'trigger=s'       => \$trigger,
    'nhcName=s'       => \$nhc_name,
    'rssprotocol=s'   => \$rssprotocol,
    'rssport=i'       => \$rssport,
    'atcfprotocol=s'  => \$atcfprotocol,
    'atcfport=i'      => \$atcfport,
    'poll-seconds=i'  => \$poll_seconds,
    'stage-dir=s'     => \$stage_dir,
    'stock=s'         => \$stock,
    'insecure'        => \$insecure,
    'test'            => \$test,
    'once'            => \$once,
    'print-urls'      => \$print_urls,
) or die "Invalid options\n";

die "--rsssite is required\n" unless defined($rsssite) && length($rsssite);
die "--storm is required\n" unless defined($storm) && $storm =~ /^\d{1,2}$/;
die "--year is required\n" unless defined($year) && $year =~ /^\d{4}$/;

$atcfsite = $ftpsite unless defined($atcfsite) && length($atcfsite);
die "--atcfsite/--ftpsite is required\n" unless defined($atcfsite) && length($atcfsite);

$trigger ||= 'rssembedded';
die "get_atcf_http.pl supports rss/rssembedded triggers only\n"
    unless $trigger eq 'rss' || $trigger eq 'rssembedded';

if ($insecure) {
    $rssprotocol = 'http';
    $atcfprotocol = 'http';
}
for my $pair (['rssprotocol', $rssprotocol], ['atcfprotocol', $atcfprotocol]) {
    die "--$pair->[0] must be http or https\n" unless $pair->[1] =~ /^https?$/;
}
$rssport  = _default_port($rssprotocol)  unless defined $rssport;
$atcfport = _default_port($atcfprotocol) unless defined $atcfport;
die "--rssport must be 1..65535\n"  unless $rssport  >= 1 && $rssport  <= 65535;
die "--atcfport must be 1..65535\n" unless $atcfport >= 1 && $atcfport <= 65535;
die "--poll-seconds must be >= 1\n" unless $poll_seconds >= 1;

$storm = sprintf('%02d', $storm);
$hdir = '/atcf/btk' unless defined($hdir) && length($hdir);
my $best_name = "bal${storm}${year}.dat";
my $rss_url   = _join_url(_base_url($rsssite, $rssprotocol, $rssport), 'index-at.xml');
my $best_url  = _join_url(_base_url($atcfsite, $atcfprotocol, $atcfport), $hdir, $best_name);

if ($print_urls) {
    print "RSS=$rss_url\nBEST=$best_url\n";
    exit 0;
}

$stock ||= File::Spec->catfile($FindBin::Bin, '..', 'get_atcf.pl');
die "Stock ASGS get_atcf.pl not found at '$stock'\n" unless -f $stock;

my $rss_stage = File::Spec->catdir($stage_dir, 'rss');
my $btk_stage = File::Spec->catdir($stage_dir, 'btk');
make_path($rss_stage, $btk_stage);

my $http = HTTP::Tiny->new(
    timeout    => 30,
    verify_SSL => 1,
    agent      => 'ASGS-get_atcf_http/1.0',
);

POLL:
while (1) {
    my $rss  = _get($http, $rss_url);
    my $best = _get($http, $best_url);

    unless (defined($rss) && defined($best)) {
        exit 2 if $once;
        sleep $poll_seconds;
        next POLL;
    }

    _atomic_write(File::Spec->catfile($rss_stage, 'index-at.xml'), $rss);
    _atomic_write(File::Spec->catfile($btk_stage, $best_name), $best);

    # Match stock get_atcf.pl's remote-download side effect: leave the BEST
    # file in the ASGS run directory as well as in our private staging area.
    _atomic_write($best_name, $best);

    my $current = _forecast_advisory_number($rss, $storm, $year);
    unless (defined $current) {
        warn "get_atcf_http.pl: AL${storm}${year} forecast/advisory not found in RSS\n";
        exit 3 if $once;
        sleep $poll_seconds;
        next POLL;
    }

    my $previous = defined($adv) && $adv =~ /(\d+)/ ? 0 + $1 : undef;
    if (!defined($previous) || $current > $previous) {
        my @cmd = (
            $^X, $stock,
            '--storm', $storm,
            '--year', $year,
            '--ftpsite', 'filesystem',
            '--fdir', $rss_stage,
            '--hdir', $btk_stage,
            '--rsssite', 'filesystem',
            '--trigger', $trigger,
        );
        push @cmd, ('--adv', $adv) if defined($adv) && length($adv);
        push @cmd, ('--nhcName', $nhc_name) if defined($nhc_name) && length($nhc_name);
        push @cmd, '--test' if $test;

        exec @cmd;
        die "Cannot exec stock get_atcf.pl: $!\n";
    }

    exit 0 if $once;
    sleep $poll_seconds;
}

sub _get {
    my ($http, $url) = @_;
    my $response = $http->get($url);
    if (!$response->{success}) {
        warn sprintf("get_atcf_http.pl: GET %s failed: HTTP %s %s\n",
            $url, $response->{status} // '?', $response->{reason} // '');
        return undef;
    }
    return $response->{content};
}

sub _atomic_write {
    my ($path, $content) = @_;
    my $dir = dirname($path);
    make_path($dir) if defined($dir) && length($dir) && $dir ne '.' && !-d $dir;
    my $tmp = "$path.http.$$";
    open my $fh, '>', $tmp or die "Cannot write '$tmp': $!\n";
    binmode $fh;
    print {$fh} $content;
    close $fh or die "Cannot close '$tmp': $!\n";
    move($tmp, $path) or die "Cannot replace '$path': $!\n";
}

sub _forecast_advisory_number {
    my ($xml, $number, $yr) = @_;
    my @lines = split /\n/, $xml;
    for my $i (1 .. $#lines) {
        next unless $lines[$i] =~ /NATIONAL HURRICANE CENTER MIAMI FL\s+AL(\d{2})(\d{4})/;
        next unless $1 == $number && $2 == $yr;
        my $prev = $lines[$i - 1] // '';
        next unless $prev =~ /FORECAST.ADVISORY NUMBER\s+(\d{1,3})/;
        return 0 + $1;
    }
    return undef;
}

sub _default_port {
    my ($protocol) = @_;
    return $protocol eq 'https' ? 443 : 80;
}

sub _base_url {
    my ($site, $protocol, $port) = @_;
    $site =~ s{/$}{};
    if ($site =~ m{^(https?)://(.+)$}i) {
        # A fully-qualified site is authoritative for its scheme.  If it also
        # contains a port, preserve it; otherwise apply the requested port.
        my ($site_protocol, $rest) = (lc($1), $2);
        my ($authority, $path) = $rest =~ m{^([^/]+)(/.*)?$};
        $path ||= '';
        return "$site_protocol://$authority$path" if $authority =~ /:\d+$/;
        my $effective_port = $port;
        return "$site_protocol://$authority$path"
            if $effective_port == _default_port($site_protocol);
        return "$site_protocol://$authority:$effective_port$path";
    }

    my ($authority, $path) = $site =~ m{^([^/]+)(/.*)?$};
    $path ||= '';
    return "$protocol://$authority$path" if $authority =~ /:\d+$/;
    return "$protocol://$authority$path" if $port == _default_port($protocol);
    return "$protocol://$authority:$port$path";
}

sub _join_url {
    my ($base, @parts) = @_;
    $base =~ s{/+$}{};
    for my $part (@parts) {
        next unless defined($part) && length($part);
        $part =~ s{^/+}{};
        $part =~ s{/+$}{};
        $base .= "/$part" if length $part;
    }
    return $base;
}

#!/usr/bin/env perl

use strict;
use warnings;
use utf8;
use feature qw(say);

use Util::H2O::More qw(Getopt2h2o d2o);
use Template;
use JSON::XS qw(decode_json encode_json);
use POSIX qw(strftime);
use URI::Escape qw(uri_escape_utf8);
use File::Basename qw(basename);
use File::Spec;
use FindBin qw/$Bin/;
use Time::Piece;

my $cli = Getopt2h2o(
    \@ARGV,
    {
        host               => undef,
        path               => undef,
        local_dir          => undef,
        template           => "$Bin/../output/asgs-run-index.tt",
        config             => "$ENV{HOME}/asgs-global.conf",
        base_url           => undef,
        title              => undef,
        brand_text         => undef,
        brand_link         => undef,
        output             => undef,
        deploy             => 0,
        verbose            => 0,
        links              => 0,
        header_image_url   => undef,
        header_image_link  => undef,
        header_image_alt   => undef,
        header_image_width => undef,
    },
    qw(
      host=s
      path=s
      local_dir=s
      template|f=s
      config=s
      base_url=s
      title=s
      brand_text=s
      brand_link=s
      output|o=s
      deploy!
      verbose!
      links!
      header_image_url=s
      header_image_link=s
      header_image_alt=s
      header_image_width=i
    )
);

my $cfg = read_ini_section($cli->config, 'run_index');

my %opt_hash = (
    host               => $cli->host,
    path               => $cli->path,
    local_dir          => $cli->local_dir,
    template           => $cli->template,
    config             => $cli->config,
    base_url           => defined($cli->base_url)   ? $cli->base_url   : ($cfg->{base_url}   // undef),
    title              => defined($cli->title)      ? $cli->title      : ($cfg->{title}      // 'ADCIRC/ASGS Run Directory'),
    brand_text         => defined($cli->brand_text) ? $cli->brand_text : ($cfg->{brand_text} // 'ASGS on GitHub'),
    brand_link         => defined($cli->brand_link) ? $cli->brand_link : ($cfg->{brand_link} // 'https://github.com/StormSurgeLive/asgs'),
    output             => defined($cli->output)     ? $cli->output     : ($cfg->{output}     // 'index.html'),
    deploy             => $cli->deploy ? 1 : 0,
    verbose            => $cli->verbose ? 1 : 0,
    links              => defined($cli->links) ? ($cli->links ? 1 : 0) : (($cfg->{links} // 0) ? 1 : 0),
    header_image_url   => defined($cli->header_image_url)
                            ? $cli->header_image_url
                            : ($cfg->{header_image_url}
                               // 'https://avatars.githubusercontent.com/u/78486559?s=400&u=1da856ed6f8760a76bbfc4768fdb3348ec0a6e2b&v=4'),
    header_image_link  => defined($cli->header_image_link)
                            ? $cli->header_image_link
                            : ($cfg->{header_image_link} // 'https://github.com/StormSurgeLive/asgs'),
    header_image_alt   => defined($cli->header_image_alt)
                            ? $cli->header_image_alt
                            : ($cfg->{header_image_alt} // 'ASGS'),
    header_image_width => defined($cli->header_image_width)
                            ? $cli->header_image_width
                            : ($cfg->{header_image_width} // 68),
);

my $opt = d2o(\%opt_hash);

usage() unless $opt->host && $opt->path && $opt->template;

my $remote_path = $opt->path;
$remote_path =~ s{/*$}{};

my $effective_base_url = normalize_base_url(
    base_url => $opt->base_url,
    path     => $remote_path,
);

my $remote_listing = get_remote_listing(
    host    => $opt->host,
    path    => $remote_path,
    verbose => $opt->verbose,
);

my %present_remote = map { $_->{name} => 1 } @$remote_listing;

my $local_dir = $opt->local_dir;

if ($local_dir && -d $local_dir) {
    warn "[localdir] using provided local_dir: $local_dir\n" if $opt->verbose;
}
elsif ($local_dir) {
    warn "[localdir] provided local_dir does not exist: $local_dir\n" if $opt->verbose;
    $local_dir = undef;
}

my $run_properties = {};
my $fort14         = {};
my $fort15         = {};
my $scenario_json  = {};
my $buildinfo_json = {};
my $asgs_config    = {};

if ($present_remote{'run.properties'}) {
    my $text = ssh_cat($opt->host, "$remote_path/run.properties");
    $run_properties = parse_run_properties($text);
    warn "[run.properties] using remote copy: $remote_path/run.properties\n" if $opt->verbose;
}
elsif ($local_dir && -f "$local_dir/run.properties") {
    my $text = slurp_local_text("$local_dir/run.properties", 150_000);
    $run_properties = parse_run_properties($text);
    warn "[run.properties] remote copy missing; using local copy: $local_dir/run.properties\n"
        if $opt->verbose;
}
else {
    die "run.properties not found remotely in $remote_path and no local fallback was found\n";
}

if (!$local_dir) {
    $local_dir = find_local_run_dir($run_properties);
}

if ($local_dir && -d $local_dir) {
    warn "[localdir] $local_dir\n" if $opt->verbose;
} else {
    warn "[localdir] no valid local scenario directory found; continuing in remote-only mode\n"
        if $opt->verbose;
    $local_dir = undef;
}

if ($present_remote{'scenario.status.json'}) {
    my $text = ssh_cat($opt->host, "$remote_path/scenario.status.json");
    eval {
        my $obj = decode_json($text);
        $scenario_json = ref($obj) eq 'HASH' ? $obj : { value => $obj };
        1;
    } or do {
        $scenario_json = { _error => "Could not parse scenario.status.json: $@" };
    };
}

my $buildinfo_name = 'adcirc.bin.buildinfo.json';
if ($run_properties->{all_keys} && $run_properties->{all_keys}{'adcirc.file.metadata.build'}) {
    $buildinfo_name = basename($run_properties->{all_keys}{'adcirc.file.metadata.build'});
}

if ($present_remote{$buildinfo_name}) {
    my $text = ssh_cat($opt->host, "$remote_path/$buildinfo_name");
    eval {
        my $obj = decode_json($text);
        $buildinfo_json = ref($obj) eq 'HASH' ? $obj : { value => $obj };
        1;
    } or do {
        $buildinfo_json = { _error => "Could not parse $buildinfo_name: $@" };
    };
}

if ($local_dir && -f "$local_dir/fort.14") {
    my $text = slurp_local_text("$local_dir/fort.14", 50_000);
    $fort14 = parse_fort14($text);
}

if ($local_dir && -f "$local_dir/fort.15") {
    my $text = slurp_local_text("$local_dir/fort.15", 250_000);
    $fort15 = parse_fort15($text);
}

if ($ENV{ASGS_CONFIG} && -f $ENV{ASGS_CONFIG}) {
    my $text = slurp_local_text($ENV{ASGS_CONFIG}, 150_000);
    $asgs_config = parse_shell_config($text, $ENV{ASGS_CONFIG});
}
elsif ($run_properties->{all_keys} && $run_properties->{all_keys}{'config.file'}) {
    my $cfg_name = basename($run_properties->{all_keys}{'config.file'});
    if ($local_dir && -f "$local_dir/$cfg_name") {
        my $text = slurp_local_text("$local_dir/$cfg_name", 150_000);
        $asgs_config = parse_shell_config($text, "$local_dir/$cfg_name");
    }
    elsif ($present_remote{$cfg_name}) {
        my $text = ssh_head($opt->host, "$remote_path/$cfg_name", 150_000);
        $asgs_config = parse_shell_config($text, $cfg_name);
    }
}

my $files = build_file_objects(
    remote_listing => $remote_listing,
    local_dir      => $local_dir,
    base_url       => $effective_base_url,
    run_properties => $run_properties,
    verbose        => $opt->verbose,
    enable_links   => $opt->links,
);

my $expected_records = expected_netcdf_records_from_run_properties($run_properties);
my $expected_outputs = expected_output_files_from_run_properties($run_properties);

my %published_files = map { $_->{name} => 1 } @$files;

for my $f (@$files) {
    my $name = $f->{name};

    if (exists $expected_outputs->{$name}) {
        $f->{is_expected_output} = 1;
    }

    next unless $f->{is_netcdf};

    if (exists $expected_records->{$name}) {
        $f->{expected_records} = $expected_records->{$name};

        if ($f->{nc} && !exists $f->{nc}{error} && defined $f->{nc}{record_count}) {
            $f->{record_check} = ($f->{nc}{record_count} == $expected_records->{$name}) ? 'ok' : 'mismatch';
        } else {
            $f->{record_check} = 'unknown';
        }
    }
}

my @missing_expected = map {
    {
        name             => $_,
        expected_records => $expected_records->{$_},
        reason           => (
            exists $expected_records->{$_}
            ? 'Configured in run.properties but not present in published directory; expected record count exists'
            : 'Configured in run.properties but not present in published directory'
        ),
    }
} sort grep { !$published_files{$_} } keys %$expected_outputs;

my @unexpected_outputs = grep {
       $_->{kind} eq 'adcirc_output'
    && !$expected_outputs->{ $_->{name} }
} @$files;

my $grouped = group_files($files);

my @record_mismatches = grep {
       defined $_->{record_check}
    && $_->{record_check} eq 'mismatch'
} @$files;

my @record_unknown = grep {
       defined $_->{record_check}
    && $_->{record_check} eq 'unknown'
} @$files;

my $job_durations = compute_hpc_job_durations($scenario_json);
my $job_metrics   = summarize_hpc_job_metrics($job_durations);

my $totals = {
    file_count                  => scalar(@$files),
    bytes_total                 => sum(map { $_->{size_bytes} || 0 } @$files),
    bytes_total_human           => human_size(sum(map { $_->{size_bytes} || 0 } @$files)),
    input_count                 => scalar(@{ $grouped->{adcirc_input} }),
    output_count                => scalar(@{ $grouped->{adcirc_output} }),
    other_count                 => scalar(@{ $grouped->{other} }),
    netcdf_count                => scalar(grep { $_->{is_netcdf} } @$files),
    netcdf_record_total         => sum(
        map { ($_->{nc} && defined $_->{nc}{record_count}) ? $_->{nc}{record_count} : 0 }
        grep { $_->{is_netcdf} } @$files
    ),
    record_check_ok_count       => scalar(grep { ($_->{record_check} // '') eq 'ok' } @$files),
    record_check_mismatch_count => scalar(@record_mismatches),
    record_check_unknown_count  => scalar(@record_unknown),
    expected_output_count       => scalar(keys %$expected_outputs),
    published_expected_count    => scalar(grep { $_->{is_expected_output} } @$files),
    missing_expected_count      => scalar(@missing_expected),
    unexpected_output_count     => scalar(@unexpected_outputs),
};

my $scenario_status_summary = summarize_scenario_status($scenario_json);

my $health = build_health_summary(
    totals           => $totals,
    scenario_summary => $scenario_status_summary,
    job_metrics      => $job_metrics,
    has_run_props    => ($run_properties && $run_properties->{key_count}) ? 1 : 0,
    has_fort14       => ($fort14 && ((defined $fort14->{title} && length $fort14->{title}) || (defined $fort14->{raw_header2} && length $fort14->{raw_header2}))) ? 1 : 0,
    has_fort15       => ($fort15 && $fort15->{line_count}) ? 1 : 0,
    has_buildinfo    => ($buildinfo_json && ref($buildinfo_json) eq 'HASH' && keys %$buildinfo_json && !$buildinfo_json->{_error}) ? 1 : 0,
);

my $scenario_status_json_pretty = '';
eval {
    $scenario_status_json_pretty = encode_json_pretty(anonymize_structure($scenario_json));
    1;
} or do {
    $scenario_status_json_pretty = '';
};

my $stash = {
    generated_at => strftime('%Y-%m-%d %H:%M:%S %z', localtime),
    title        => $opt->title,
    brand        => {
        text => $opt->brand_text,
        link => $opt->brand_link,
    },
    source => {
        ssh_host      => anonymize_path($opt->host),
        remote_path   => anonymize_path($remote_path),
        local_dir     => $local_dir ? anonymize_path($local_dir) : undef,
        base_url      => $effective_base_url,
        deploy_target => anonymize_path("$remote_path/" . $opt->output),
        mode          => ($local_dir ? 'hybrid-local-remote' : 'remote-only'),
    },
    ui => {
        links_enabled => $opt->links ? 1 : 0,
        header_image  => {
            url   => $opt->header_image_url,
            link  => $opt->header_image_link,
            alt   => $opt->header_image_alt,
            width => int($opt->header_image_width || 68),
        },
    },
    totals                  => $totals,
    health                  => $health,
    files                   => anonymize_files_for_stash($files),
    grouped                 => anonymize_grouped_for_stash($grouped),
    run_properties          => $run_properties,
    fort14                  => $fort14,
    fort15                  => $fort15,
    scenario_status         => anonymize_structure($scenario_json),
    scenario_status_summary => $scenario_status_summary,
    scenario_status_json    => $scenario_status_json_pretty,
    buildinfo               => anonymize_structure($buildinfo_json),
    buildinfo_name          => anonymize_path($buildinfo_name),
    asgs_config             => $asgs_config,
    record_mismatches       => anonymize_files_for_stash(\@record_mismatches),
    record_unknown          => anonymize_files_for_stash(\@record_unknown),
    missing_expected        => anonymize_structure(\@missing_expected),
    unexpected_outputs      => anonymize_files_for_stash(\@unexpected_outputs),
    expected_outputs        => anonymize_structure([ sort keys %$expected_outputs ]),
    job_durations           => anonymize_structure($job_durations),
    job_metrics             => anonymize_structure($job_metrics),
};

my $tt = Template->new({
    ABSOLUTE => 1,
    RELATIVE => 1,
    TRIM     => 1,
    LTRIM    => 1,
    RTRIM    => 1,
    ENCODING => 'utf8',
});

open my $out_fh, '>:encoding(UTF-8)', $opt->output
    or die "Cannot open output '$opt->output': $!";

$tt->process($opt->template, $stash, $out_fh)
    or die "Template processing failed: " . $tt->error . "\n";

close $out_fh
    or die "Cannot close output '$opt->output': $!";

say "Wrote local output: " . $opt->output;

if ($opt->deploy) {
    deploy_file(
        host        => $opt->host,
        local_file  => $opt->output,
        remote_file => "$remote_path/" . $opt->output,
    );
    say "Deployed to " . anonymize_path($opt->host . ":$remote_path/" . $opt->output);
}

exit 0;

sub usage {
    die <<"USAGE";
Usage:
  perl $0 --host SSH_ALIAS --path REMOTE_DIR --local_dir LOCAL_DIR -f TEMPLATE [options]

Required:
  --host         SSH host alias
  --path         Remote published directory to inspect
  --template,-f  Template Toolkit file

Recommended:
  --local_dir    Local scenario directory containing fort.14, fort.15, .nc files

Optional:
  --config               INI file (default: ~/asgs-global.conf)
  --base_url             Override base URL from config
  --title                Override title
  --brand_text           Override branding text
  --brand_link           Override branding URL
  --output,-o            Override output filename
  --deploy               Copy generated output back to remote path
  --verbose              Verbose progress
  --links / --nolinks    Enable published-file hyperlinks (default: off)
  --header_image_url     Header/report image URL
  --header_image_link    Link target for header/report image
  --header_image_alt     Alt text for header/report image
  --header_image_width   Width in pixels for header/report image
USAGE
}

sub read_ini_section {
    my ($file, $want_section) = @_;
    return {} unless defined $file && -f $file;

    open my $fh, '<:encoding(UTF-8)', $file or die "Could not open config '$file': $!\n";

    my %data;
    my $section = '';

    while (my $line = <$fh>) {
        chomp $line;
        $line =~ s/\r$//;
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;

        next if $line eq '';
        next if $line =~ /^[#;]/;

        if ($line =~ /^\[(.+?)\]$/) {
            $section = $1;
            next;
        }

        next unless $section eq $want_section;

        if ($line =~ /^([^=]+?)\s*=\s*(.*)$/) {
            my ($k, $v) = ($1, $2);
            $k =~ s/\s+$//;
            $v =~ s/^\s+//;
            $data{$k} = $v;
        }
    }

    close $fh;
    return \%data;
}

sub expand_path_vars {
    my ($s) = @_;
    return $s unless defined $s;

    my %vars = (
        USER        => ($ENV{USER}        // ''),
        RUNDIR      => ($ENV{RUNDIR}      // ''),
        ASGS_CONFIG => ($ENV{ASGS_CONFIG} // ''),
        STATEFILE   => ($ENV{STATEFILE}   // ''),
        SYSLOG      => ($ENV{SYSLOG}      // ''),
    );

    $s =~ s/\$\{(\w+)\}/exists $vars{$1} ? $vars{$1} : "\${$1}"/ge;
    $s =~ s/\$(\w+)/exists $vars{$1} ? $vars{$1} : "\$$1"/ge;

    return $s;
}

sub anonymize_path {
    my ($s) = @_;
    return $s unless defined $s;

    my %vars = (
        USER        => ($ENV{USER}        // ''),
        RUNDIR      => ($ENV{RUNDIR}      // ''),
        ASGS_CONFIG => ($ENV{ASGS_CONFIG} // ''),
        STATEFILE   => ($ENV{STATEFILE}   // ''),
        SYSLOG      => ($ENV{SYSLOG}      // ''),
    );

    for my $name (qw(ASGS_CONFIG STATEFILE SYSLOG RUNDIR USER)) {
        my $val = $vars{$name};
        next unless defined $val && length $val;
        $s =~ s/\Q$val\E/\$$name/g;
    }

    if ($vars{USER}) {
        my $u = $vars{USER};
        $s =~ s{(/work/)\Q$u\E(/)}{$1\$USER$2}g;
        $s =~ s{(/home/)\Q$u\E(/)}{$1\$USER$2}g;
    }

    return $s;
}

sub anonymize_structure {
    my ($x) = @_;

    if (!ref $x) {
        return anonymize_path($x);
    }
    elsif (ref($x) eq 'ARRAY') {
        return [ map { anonymize_structure($_) } @$x ];
    }
    elsif (ref($x) eq 'HASH') {
        my %h;
        for my $k (keys %$x) {
            $h{$k} = anonymize_structure($x->{$k});
        }
        return \%h;
    }

    return $x;
}

sub anonymize_files_for_stash {
    my ($files) = @_;
    my @out;

    for my $f (@$files) {
        my %copy = %$f;
        $copy{local_file} = anonymize_path($copy{local_file}) if exists $copy{local_file} && defined $copy{local_file};

        if ($copy{nc} && ref($copy{nc}) eq 'HASH') {
            my %nc = %{ $copy{nc} };
            $nc{local_file} = anonymize_path($nc{local_file}) if exists $nc{local_file} && defined $nc{local_file};
            $nc{error}      = anonymize_path($nc{error})      if exists $nc{error};
            $copy{nc} = \%nc;
        }

        push @out, \%copy;
    }

    return \@out;
}

sub anonymize_grouped_for_stash {
    my ($grouped) = @_;
    return {
        adcirc_input  => anonymize_files_for_stash($grouped->{adcirc_input}  || []),
        adcirc_output => anonymize_files_for_stash($grouped->{adcirc_output} || []),
        other         => anonymize_files_for_stash($grouped->{other}         || []),
    };
}

sub normalize_base_url {
    my (%args) = @_;
    my $base_url = $args{base_url} // die "No base_url provided and none found in config\n";
    my $path     = $args{path}     // die "normalize_base_url missing path\n";

    $base_url =~ s{/*$}{};

    return $base_url if $base_url =~ m{/thredds/fileServer/.+};

    my $suffix = $path;
    $suffix =~ s{^/opendap/}{};
    $suffix =~ s{^/data/opendap/}{};

    return $base_url . '/' . $suffix;
}

sub ssh_capture {
    my (%args) = @_;
    my $host = $args{host} or die "ssh_capture: missing host\n";
    my $cmd  = $args{cmd}  or die "ssh_capture: missing cmd\n";

    my $full = sprintf(
        q{ssh -o BatchMode=yes -o ConnectTimeout=10 %s %s},
        shell_quote($host),
        shell_quote($cmd),
    );

    my $out = qx{$full 2>&1};
    my $rc  = $? >> 8;

    die "SSH command failed (rc=$rc): $cmd\n$out\n" if $rc != 0;
    return $out;
}

sub ssh_cat {
    my ($host, $file) = @_;
    return ssh_capture(
        host => $host,
        cmd  => "cat " . shell_quote($file),
    );
}

sub ssh_head {
    my ($host, $file, $bytes) = @_;
    $bytes ||= 100_000;
    return ssh_capture(
        host => $host,
        cmd  => "head -c " . int($bytes) . " " . shell_quote($file),
    );
}

sub get_remote_listing {
    my (%args) = @_;
    my $host    = $args{host};
    my $path    = $args{path};
    my $verbose = $args{verbose};

    my $cmd = join ' ',
        'find', shell_quote($path),
        '-mindepth', '1',
        '-maxdepth', '1',
        '-printf', shell_quote('%f' . "\t" . '%s' . "\t" . '%T@' . "\t" . '%y' . "\n"),
        '| sort';

    my $out = ssh_capture(host => $host, cmd => $cmd);

    warn "[remote listing raw]\n$out\n" if $verbose;

    my @rows;
    for my $line (split /\n/, $out) {
        next unless length $line;

        my ($name, $size, $mtime_epoch, $typechar) = split /\t/, $line, 4;
        next unless defined $name && defined $size && defined $mtime_epoch && defined $typechar;
        next if $name eq '.' || $name eq '..';

        push @rows, {
            name               => $name,
            remote_size_bytes  => $size + 0,
            remote_mtime_epoch => $mtime_epoch + 0,
            remote_typechar    => $typechar || '',
        };
    }

    return \@rows;
}

sub find_local_run_dir {
    my ($rp) = @_;

    my @candidates;

    if ($rp && $rp->{all_keys}) {
        push @candidates,
            map { $rp->{all_keys}{$_} }
            grep { exists $rp->{all_keys}{$_} }
            qw(path.scenariodir path.stormdir asgs.path.stormdir path.rundir);
    }

    if ($ENV{RUNDIR}) {
        push @candidates, $ENV{RUNDIR};

        if ($rp && $rp->{all_keys} && $rp->{all_keys}{"scenario"}) {
            push @candidates, "$ENV{RUNDIR}/" . $rp->{all_keys}{"scenario"};
        }
        if ($rp && $rp->{all_keys} && $rp->{all_keys}{"asgs.enstorm"}) {
            push @candidates, "$ENV{RUNDIR}/" . $rp->{all_keys}{"asgs.enstorm"};
        }
        if ($rp && $rp->{all_keys} && $rp->{all_keys}{"adcirc.gridname"}) {
            push @candidates, "$ENV{RUNDIR}/" . $rp->{all_keys}{"adcirc.gridname"};
        }
    }

    @candidates = map { expand_path_vars($_) } @candidates;
    @candidates = grep { defined && $_ ne '' && $_ ne 'null' } @candidates;

    my %seen;
    @candidates = grep { !$seen{$_}++ } @candidates;

    for my $dir (@candidates) {
        next unless -d $dir;
        return $dir if (
               -f "$dir/fort.15"
            || -f "$dir/run.properties"
            || scalar(glob("$dir/*.nc"))
            || -f "$dir/fort.14"
        );
    }

    return undef;
}

sub slurp_local_text {
    my ($file, $max_bytes) = @_;
    $max_bytes ||= 0;

    open my $fh, '<:encoding(UTF-8)', $file or die "Could not open local text file '$file': $!\n";

    my $buf = '';
    if ($max_bytes && $max_bytes > 0) {
        read($fh, $buf, $max_bytes);
    } else {
        local $/;
        $buf = <$fh>;
    }

    close $fh;
    return $buf;
}

sub build_file_objects {
    my (%args) = @_;
    my $remote_listing = $args{remote_listing} || [];
    my $local_dir      = $args{local_dir};
    my $base_url       = $args{base_url};
    my $verbose        = $args{verbose};
    my $run_properties = $args{run_properties};
    my $enable_links   = $args{enable_links} ? 1 : 0;

    my @files;

    for my $row (@$remote_listing) {
        my $name = $row->{name};
        my $local_file = defined($local_dir) ? File::Spec->catfile($local_dir, $name) : undef;

        my ($size_bytes, $mtime_epoch, $typechar, $exists_local);

        if (defined($local_file) && -e $local_file) {
            $exists_local = 1;
            my @st = stat($local_file);
            $size_bytes  = $st[7];
            $mtime_epoch = $st[9];
            $typechar    = (-d _ ? 'd' : 'f');
        } else {
            $exists_local = 0;
            $size_bytes   = $row->{remote_size_bytes};
            $mtime_epoch  = $row->{remote_mtime_epoch};
            $typechar     = $row->{remote_typechar};
        }

        my $nc_summary;
        if ($name =~ /\.nc$/i && defined($local_file) && -f $local_file) {
            warn "[local ncdump] $local_file\n" if $verbose;
            $nc_summary = get_local_netcdf_summary($local_file);
            if ($nc_summary && ref($nc_summary) eq 'HASH') {
                $nc_summary->{source} = 'local';
                $nc_summary->{local_file} = $local_file;
            }
        }

        push @files, {
            name               => $name,
            local_file         => $local_file,
            exists_local       => $exists_local,
            size_bytes         => $size_bytes,
            mtime_epoch        => $mtime_epoch,
            typechar           => $typechar,
            size_human         => human_size($size_bytes),
            mtime_iso          => epoch_to_iso($mtime_epoch),
            url                => $enable_links ? build_url($base_url, $name) : undef,
            ext                => file_ext($name),
            kind               => classify_file($name, $run_properties),
            is_netcdf          => ($name =~ /\.nc$/i ? 1 : 0),
            is_expected_output => 0,
            nc                 => $nc_summary,
            expected_records   => undef,
            record_check       => undef,
        };
    }

    @files = sort {
           ($a->{kind} cmp $b->{kind})
        || ($a->{name} cmp $b->{name})
    } @files;

    return \@files;
}

sub parse_run_properties {
    my ($text) = @_;
    my %p;

    for my $line (split /\n/, $text) {
        $line =~ s/\r$//;
        next if $line =~ /^\s*#/;
        next if $line =~ /^\s*!/;
        next if $line =~ /^\s*$/;

        if ($line =~ /^\s*([^=:\s][^=:]*?)\s*[:=]\s*(.*?)\s*$/) {
            my ($k, $v) = ($1, $2);
            $p{$k} = expand_path_vars($v);
        }
    }

    my @interesting = qw(
      instancename
      scenario
      asgs.enstorm
      forcing.backgroundmet
      forcing.tropicalcyclone
      adcirc.gridname
      adcirc.version
      config.file
      adcirc.file.metadata.build
      hpc.hpcenv
      path.rundir
      path.scenariodir
      path.stormdir
      asgs.path.stormdir
      post.opendap.lsu_tds.downloadprefix
      post.opendap.lsu_tds.opendapdir
      post.opendap.lsu_tds.opendapsuffix
      downloadurl
      adcirc.control.physics.rnday
      adcirc.timestepsize
      coupling.waves
      Model
      RunStartTime
      RunEndTime
      ColdStartTime
      ncpu
    );

    my %summary;
    for my $k (@interesting) {
        $summary{$k} = anonymize_path($p{$k}) if exists $p{$k};
    }

    my %anon_all = map { $_ => anonymize_path($p{$_}) } keys %p;

    my $config_basename = exists $p{'config.file'} ? basename($p{'config.file'}) : undef;
    my $build_basename  = exists $p{'adcirc.file.metadata.build'} ? basename($p{'adcirc.file.metadata.build'}) : undef;

    my @output_keys = sort grep { /^adcirc\.file\.output\./ } keys %p;

    return {
        all_keys        => \%p,
        all_keys_anon   => \%anon_all,
        summary         => \%summary,
        key_count       => scalar(keys %p),
        sorted_keys     => [ sort keys %p ],
        output_keys     => \@output_keys,
        config_basename => $config_basename,
        build_basename  => $build_basename,
    };
}

sub expected_netcdf_records_from_run_properties {
    my ($rp) = @_;
    my %expected;

    return \%expected unless $rp && $rp->{all_keys};

    for my $k (keys %{ $rp->{all_keys} }) {
        next unless $k =~ /^adcirc\.file\.output\.(.+)\.numdatasets$/;
        my $fname = $1;
        my $v = $rp->{all_keys}{$k};
        next unless defined $v && $v =~ /^\d+$/;
        $expected{$fname} = $v + 0;
    }

    return \%expected;
}

sub expected_output_files_from_run_properties {
    my ($rp) = @_;
    my %expected;

    return \%expected unless $rp && $rp->{all_keys};

    for my $k (keys %{ $rp->{all_keys} }) {
        next unless $k =~ /^adcirc\.file\.output\.(.+?)\.(?:numdatasets|enable|format|name)$/;
        my $fname = $1;
        next unless defined $fname && length $fname;
        $expected{$fname} = 1;
    }

    return \%expected;
}

sub parse_shell_config {
    my ($text, $filename) = @_;

    my @lines = split /\n/, $text;
    my @preview = grep { /\S/ && $_ !~ /^\s*#/ } @lines;
    splice @preview, 12 if @preview > 12;

    my %vars;
    for my $line (@lines) {
        next if $line =~ /^\s*#/;
        if ($line =~ /^\s*(?:export\s+)?([A-Za-z_]\w*)=(.*)$/) {
            my ($k, $v) = ($1, $2);
            $v =~ s/^\s+//;
            $v =~ s/\s+$//;
            $v =~ s/^"(.*)"$/$1/;
            $v =~ s/^'(.*)'$/$1/;
            $vars{$k} = expand_path_vars($v);
        }
    }

    my @interesting = qw(
      CONFIG
      ADCIRC_PROFILE_NAME
      ASGS_MACHINE_NAME
      QUEUESYS
      INSTANCENAME
      GRIDNAME
      BACKGROUNDMET
      ENSTORM
      HPCENV
      ACCOUNT
    );

    my %summary;
    for my $k (@interesting) {
        $summary{$k} = anonymize_path($vars{$k}) if exists $vars{$k};
    }

    my %anon_vars = map { $_ => anonymize_path($vars{$_}) } keys %vars;

    return {
        filename   => anonymize_path($filename),
        line_count => scalar(@lines),
        preview    => [ map { anonymize_path($_) } @preview ],
        vars       => \%anon_vars,
        summary    => \%summary,
    };
}

sub parse_fort14 {
    my ($text) = @_;
    my @lines = grep { /\S/ } split /\n/, $text;

    my %s = (
        title       => $lines[0] // '',
        raw_header2 => $lines[1] // '',
    );

    if (($lines[1] // '') =~ /^\s*(\d+)\s+(\d+)\s*$/) {
        $s{ne} = $1 + 0;
        $s{np} = $2 + 0;
    }

    return \%s;
}

sub parse_fort15 {
    my ($text) = @_;
    my @lines = split /\n/, $text;

    my @markers = qw(
      RNDAY DT STATIM REFTIM
      NOUTGE NOUTGV NOUTE NOUTV
      NWS NRAMP IHOT ICS
      NFOVER NABOUT NSCREEN
    );

    my %found;
    for my $line (@lines) {
        for my $m (@markers) {
            if ($line =~ /\b\Q$m\E\b/i) {
                push @{ $found{$m} }, $line;
            }
        }
    }

    my @first_nonempty = grep { /\S/ } @lines;
    splice @first_nonempty, 10 if @first_nonempty > 10;

    return {
        line_count    => scalar(@lines),
        marker_lines  => \%found,
        preview_lines => \@first_nonempty,
    };
}

sub get_local_netcdf_summary {
    my ($file) = @_;

    my $cmd = 'ncdump -h ' . shell_quote($file) . ' 2>&1';
    my $txt = qx{$cmd};
    my $rc  = $? >> 8;

    if ($rc != 0) {
        chomp $txt;
        return { error => ($txt || "local ncdump failed for $file") };
    }

    return parse_ncdump_header($txt);
}

sub parse_ncdump_header {
    my ($txt) = @_;

    my %nc = (
        dimensions      => {},
        variables       => [],
        record_count    => undef,
        time_dim        => undef,
        dimension_count => 0,
        variable_count  => 0,
    );

    my $in_dims = 0;
    my $in_vars = 0;

    for my $line (split /\n/, $txt) {
        $line =~ s/\r$//;

        if ($line =~ /^\s*dimensions:\s*$/) {
            $in_dims = 1;
            $in_vars = 0;
            next;
        }
        if ($line =~ /^\s*variables:\s*$/) {
            $in_dims = 0;
            $in_vars = 1;
            next;
        }
        if ($line =~ /^\s*data:\s*$/) {
            last;
        }

        if ($in_dims) {
            if ($line =~ /^\s*([A-Za-z0-9_]+)\s*=\s*UNLIMITED\s*;\s*\/\/\s*\((\d+)\s+currently\)\s*$/) {
                $nc{dimensions}{$1} = 'UNLIMITED';
                $nc{record_count}   = $2 + 0;
                $nc{time_dim}       = $1;
                next;
            }
            if ($line =~ /^\s*([A-Za-z0-9_]+)\s*=\s*(\d+)\s*;\s*$/) {
                $nc{dimensions}{$1} = $2 + 0;
                next;
            }
        }

        if ($in_vars) {
            if ($line =~ /^\s*(?:byte|char|short|int|float|double|ubyte|ushort|uint|int64|uint64|string)\s+([A-Za-z0-9_]+)\s*\((.*?)\)\s*;\s*$/) {
                push @{ $nc{variables} }, {
                    name => $1,
                    dims => $2,
                };
                next;
            }
        }
    }

    $nc{dimension_count} = scalar(keys %{ $nc{dimensions} });
    $nc{variable_count}  = scalar(@{ $nc{variables} });

    if (!defined $nc{record_count}) {
        for my $dim (qw(time Time timestep timesteps record records)) {
            if (exists $nc{dimensions}{$dim} && $nc{dimensions}{$dim} ne 'UNLIMITED') {
                $nc{record_count} = $nc{dimensions}{$dim};
                $nc{time_dim}     = $dim;
                last;
            }
        }
    }

    return \%nc;
}

sub summarize_scenario_status {
    my ($obj) = @_;

    return {
        present      => 0,
        class        => 'neutral',
        label        => 'not available',
        summary      => {},
        scalar_pairs => [],
    } unless $obj && ref($obj) eq 'HASH' && keys %$obj;

    if ($obj->{_error}) {
        return {
            present      => 1,
            class        => 'bad',
            label        => 'parse error',
            summary      => {},
            scalar_pairs => [],
            error        => $obj->{_error},
        };
    }

    my @preferred = qw(
      status state phase stage condition health outcome result
      advisory cycle advisoryId advisory_id instance scenario
      progress progressPct progress_pct percent_complete completion
      start_time started_at end_time ended_at elapsed duration
      message summary detail
      error errors error_count warning warnings warning_count
    );

    my %flat;
    _flatten_json_scalars('', $obj, \%flat, 2);

    my @scalar_pairs;
    for my $k (@preferred) {
        next unless exists $flat{$k};
        push @scalar_pairs, { key => $k, value => $flat{$k} };
    }

    for my $k (sort keys %flat) {
        next if grep { $_->{key} eq $k } @scalar_pairs;
        push @scalar_pairs, { key => $k, value => $flat{$k} };
    }

    my $status_text = join ' ',
        map { defined($_->{value}) ? lc($_->{value}) : '' }
        grep { $_->{key} =~ /^(status|state|phase|stage|condition|health|outcome|result|message|summary|detail)$/ }
        @scalar_pairs;

    my $errorish = _first_numeric($flat{error_count}, $flat{errors}, $flat{failed}, $flat{failure_count});
    my $warnish  = _first_numeric($flat{warning_count}, $flat{warnings});

    my ($class, $label) = ('neutral', 'unknown');

    if ((defined $errorish && $errorish > 0) || $status_text =~ /\b(fail|failed|error|fatal|abort|aborted|stalled)\b/) {
        ($class, $label) = ('bad', 'problem');
    }
    elsif ((defined $warnish && $warnish > 0) || $status_text =~ /\b(warn|warning|partial|degraded|pending)\b/) {
        ($class, $label) = ('warn', 'warning');
    }
    elsif ($status_text =~ /\b(success|complete|completed|finished|done|published|ready|ok|healthy)\b/) {
        ($class, $label) = ('ok', 'good');
    }
    elsif ($status_text =~ /\b(run|running|active|processing|building)\b/) {
        ($class, $label) = ('warn', 'in progress');
    }

    my %summary;
    for my $k (qw(status state phase stage progress progressPct progress_pct cycle advisory advisory_id advisoryId start_time started_at end_time ended_at error_count errors warning_count warnings message summary)) {
        $summary{$k} = $flat{$k} if exists $flat{$k};
    }

    splice @scalar_pairs, 16 if @scalar_pairs > 16;

    return {
        present      => 1,
        class        => $class,
        label        => $label,
        summary      => \%summary,
        scalar_pairs => \@scalar_pairs,
    };
}

sub _flatten_json_scalars {
    my ($prefix, $value, $flat, $max_depth, $depth) = @_;
    $depth //= 0;
    return if $depth > $max_depth;

    if (!ref $value) {
        my $k = $prefix;
        $k =~ s/^\.//;
        return if !defined $k || $k eq '';
        $flat->{$k} = $value;
        my ($leaf) = $k =~ /([^.]+)$/;
        $flat->{$leaf} //= $value if defined $leaf;
        return;
    }

    if (ref($value) eq 'HASH') {
        for my $k (sort keys %$value) {
            _flatten_json_scalars("$prefix.$k", $value->{$k}, $flat, $max_depth, $depth + 1);
        }
        return;
    }

    if (ref($value) eq 'ARRAY') {
        my $count = 0;
        for my $item (@$value) {
            last if $count >= 6;
            _flatten_json_scalars("$prefix.$count", $item, $flat, $max_depth, $depth + 1);
            $count++;
        }
        return;
    }
}

sub _first_numeric {
    for my $v (@_) {
        next if !defined $v;
        return $v + 0 if $v =~ /^-?\d+(?:\.\d+)?$/;
    }
    return undef;
}

sub compute_hpc_job_durations {
    my ($scenario) = @_;

    return [] unless $scenario
        && ref($scenario) eq 'HASH'
        && ref($scenario->{'jobs.status'}) eq 'ARRAY';

    my @results;

    for my $job (@{ $scenario->{'jobs.status'} }) {
        next unless ref($job) eq 'HASH';

        my $jobid   = defined $job->{jobid}   ? "$job->{jobid}"   : '';
        my $jobtype = defined $job->{jobtype} ? "$job->{jobtype}" : '';

        my $t_submit = _parse_time($job->{'time.submit'});
        my $t_start  = _parse_time($job->{'time.start'});
        my $t_finish = _parse_time($job->{'time.finish'});

        my $wait  = _duration($t_submit, $t_start);
        my $run   = _duration($t_start,  $t_finish);
        my $total = _duration($t_submit, $t_finish);

        push @results, {
            jobid          => $jobid,
            jobtype        => $jobtype,
            submit         => $job->{'time.submit'},
            start          => $job->{'time.start'},
            finish         => $job->{'time.finish'},
            error          => $job->{'time.error'},
            wait_seconds   => $wait,
            run_seconds    => $run,
            total_seconds  => $total,
            wait_human     => _fmt_duration($wait),
            run_human      => _fmt_duration($run),
            total_human    => _fmt_duration($total),
            has_submit     => defined $job->{'time.submit'} ? 1 : 0,
            has_start      => defined $job->{'time.start'}  ? 1 : 0,
            has_finish     => defined $job->{'time.finish'} ? 1 : 0,
        };
    }

    return \@results;
}

sub summarize_hpc_job_metrics {
    my ($jobs) = @_;
    $jobs ||= [];

    my $count = scalar @$jobs;

    my $longest_run_job;
    my $longest_wait_job;
    my $first_submit;
    my $last_finish;

    my $sum_wait  = 0;
    my $sum_run   = 0;
    my $sum_total = 0;

    my $have_wait  = 0;
    my $have_run   = 0;
    my $have_total = 0;

    for my $j (@$jobs) {
        if (defined $j->{wait_seconds}) {
            $sum_wait += $j->{wait_seconds};
            $have_wait++;
            if (!$longest_wait_job || $j->{wait_seconds} > ($longest_wait_job->{wait_seconds} // -1)) {
                $longest_wait_job = $j;
            }
        }

        if (defined $j->{run_seconds}) {
            $sum_run += $j->{run_seconds};
            $have_run++;
            if (!$longest_run_job || $j->{run_seconds} > ($longest_run_job->{run_seconds} // -1)) {
                $longest_run_job = $j;
            }
        }

        if (defined $j->{total_seconds}) {
            $sum_total += $j->{total_seconds};
            $have_total++;
        }

        my $submit_epoch = _epoch_if_parseable($j->{submit});
        my $finish_epoch = _epoch_if_parseable($j->{finish});

        $first_submit = $submit_epoch
            if defined $submit_epoch && (!defined $first_submit || $submit_epoch < $first_submit);

        $last_finish = $finish_epoch
            if defined $finish_epoch && (!defined $last_finish || $finish_epoch > $last_finish);
    }

    my $workflow_span = (defined $first_submit && defined $last_finish)
        ? ($last_finish - $first_submit)
        : undef;

    return {
        job_count               => $count,
        jobs_with_wait          => $have_wait,
        jobs_with_run           => $have_run,
        jobs_with_total         => $have_total,
        sum_wait_seconds        => $sum_wait,
        sum_run_seconds         => $sum_run,
        sum_total_seconds       => $sum_total,
        sum_wait_human          => _fmt_duration($sum_wait),
        sum_run_human           => _fmt_duration($sum_run),
        sum_total_human         => _fmt_duration($sum_total),
        workflow_span_seconds   => $workflow_span,
        workflow_span_human     => _fmt_duration($workflow_span),
        longest_run_jobid       => $longest_run_job ? $longest_run_job->{jobid} : '',
        longest_run_jobtype     => $longest_run_job ? $longest_run_job->{jobtype} : '',
        longest_run_seconds     => $longest_run_job ? $longest_run_job->{run_seconds} : undef,
        longest_run_human       => $longest_run_job ? $longest_run_job->{run_human} : '',
        longest_wait_jobid      => $longest_wait_job ? $longest_wait_job->{jobid} : '',
        longest_wait_jobtype    => $longest_wait_job ? $longest_wait_job->{jobtype} : '',
        longest_wait_seconds    => $longest_wait_job ? $longest_wait_job->{wait_seconds} : undef,
        longest_wait_human      => $longest_wait_job ? $longest_wait_job->{wait_human} : '',
        queue_warning_count     => scalar(grep { defined($_->{wait_seconds}) && $_->{wait_seconds} > 60 } @$jobs),
        runtime_warning_count   => scalar(grep { defined($_->{run_seconds})  && $_->{run_seconds}  > 600 } @$jobs),
        total_warning_count     => scalar(grep { defined($_->{total_seconds}) && $_->{total_seconds} > 600 } @$jobs),
    };
}

sub _parse_time {
    my ($str) = @_;
    return undef unless defined $str && length $str;
    my $t;
    eval {
        $t = Time::Piece->strptime($str, "%Y-%b-%d-T%H:%M:%S%z");
        1;
    } or do {
        return undef;
    };
    return $t;
}

sub _epoch_if_parseable {
    my ($str) = @_;
    my $t = _parse_time($str);
    return defined $t ? $t->epoch : undef;
}

sub _duration {
    my ($a, $b) = @_;
    return undef unless $a && $b;
    return $b->epoch - $a->epoch;
}

sub _fmt_duration {
    my ($s) = @_;
    return '' unless defined $s;

    my $sign = $s < 0 ? '-' : '';
    $s = abs($s);

    my $h   = int($s / 3600);
    my $m   = int(($s % 3600) / 60);
    my $sec = $s % 60;

    return sprintf("%s%dh %dm %ds", $sign, $h, $m, $sec) if $h;
    return sprintf("%s%dm %ds", $sign, $m, $sec) if $m;
    return sprintf("%s%ds", $sign, $sec);
}

sub encode_json_pretty {
    my ($obj) = @_;
    return JSON::XS->new->canonical(1)->pretty(1)->encode($obj);
}

sub build_health_summary {
    my (%args) = @_;
    my $totals           = $args{totals} || {};
    my $scenario_summary = $args{scenario_summary} || {};
    my $job_metrics      = $args{job_metrics} || {};

    my $published_expected = $totals->{published_expected_count} || 0;
    my $expected_total     = $totals->{expected_output_count} || 0;
    my $coverage_pct = $expected_total ? sprintf('%.0f', 100 * $published_expected / $expected_total) : '0';

    my $overall = 'ok';
    $overall = 'bad'  if ($totals->{missing_expected_count} || 0) > 0 || ($totals->{record_check_mismatch_count} || 0) > 0;
    $overall = 'bad'  if ($scenario_summary->{class} || '') eq 'bad';
    $overall = 'warn' if $overall eq 'ok' && (
        ($totals->{record_check_unknown_count} || 0) > 0 ||
        ($totals->{unexpected_output_count} || 0) > 0 ||
        (($scenario_summary->{class} || '') eq 'warn') ||
        (($job_metrics->{queue_warning_count} || 0) > 0)
    );

    return {
        overall_class               => $overall,
        expected_coverage_pct       => $coverage_pct,
        missing_expected_count      => $totals->{missing_expected_count} || 0,
        record_check_ok_count       => $totals->{record_check_ok_count} || 0,
        record_check_mismatch_count => $totals->{record_check_mismatch_count} || 0,
        record_check_unknown_count  => $totals->{record_check_unknown_count} || 0,
        unexpected_output_count     => $totals->{unexpected_output_count} || 0,
        has_run_props               => $args{has_run_props} ? 1 : 0,
        has_fort14                  => $args{has_fort14} ? 1 : 0,
        has_fort15                  => $args{has_fort15} ? 1 : 0,
        has_buildinfo               => $args{has_buildinfo} ? 1 : 0,
        scenario                    => $scenario_summary,
        jobs                        => $job_metrics,
    };
}

sub classify_file {
    my ($name, $run_properties) = @_;

    my $buildinfo_name = 'adcirc.bin.buildinfo.json';
    if ($run_properties && $run_properties->{all_keys} && $run_properties->{all_keys}{'adcirc.file.metadata.build'}) {
        $buildinfo_name = basename($run_properties->{all_keys}{'adcirc.file.metadata.build'});
    }

    return 'adcirc_input' if $name =~ /^(fort\.1[45]|fort\.22|run\.properties|.*\.asgs-.*|.*\.sh|cycle\.log|scenario\.log|scenario\.status\.json)$/i;
    return 'adcirc_input' if lc($name) eq lc($buildinfo_name);
    return 'adcirc_output' if $name =~ /\.nc$/i;
    return 'adcirc_output' if $name =~ /^(max|min|initially|everdried|endrising|inundation|swan_|wind10m\.)/i;
    return 'other';
}

sub group_files {
    my ($files) = @_;

    my %g = (
        adcirc_input  => [],
        adcirc_output => [],
        other         => [],
    );

    for my $f (@$files) {
        if ($f->{kind} eq 'adcirc_input') {
            push @{ $g{adcirc_input} }, $f;
        }
        elsif ($f->{kind} eq 'adcirc_output') {
            push @{ $g{adcirc_output} }, $f;
        }
        else {
            push @{ $g{other} }, $f;
        }
    }

    return \%g;
}

sub deploy_file {
    my (%args) = @_;
    my $host        = $args{host};
    my $local_file  = $args{local_file};
    my $remote_file = $args{remote_file};

    my $cmd = sprintf(
        q{scp -q %s %s:%s},
        shell_quote($local_file),
        shell_quote($host),
        shell_quote($remote_file),
    );

    system($cmd) == 0 or die "Failed to deploy with scp: $cmd\n";
}

sub build_url {
    my ($base, $name) = @_;
    my @parts = split m{/+}, $name;
    return $base . '/' . join('/', map { uri_escape_utf8($_) } @parts);
}

sub file_ext {
    my ($name) = @_;
    return lc($1) if $name =~ /\.([^.]+)$/;
    return '';
}

sub shell_quote {
    my ($s) = @_;
    $s //= '';
    $s =~ s/'/'"'"'/g;
    return "'$s'";
}

sub epoch_to_iso {
    my ($epoch) = @_;
    return strftime('%Y-%m-%d %H:%M:%S', localtime(int($epoch)));
}

sub human_size {
    my ($bytes) = @_;
    $bytes ||= 0;

    my @u = qw(B KB MB GB TB PB);
    my $i = 0;
    my $v = $bytes + 0.0;

    while ($v >= 1024 && $i < $#u) {
        $v /= 1024;
        $i++;
    }

    return $i == 0 ? sprintf('%d %s', $bytes, $u[$i]) : sprintf('%.1f %s', $v, $u[$i]);
}

sub sum {
    my $x = 0;
    $x += $_ for @_;
    return $x;
}

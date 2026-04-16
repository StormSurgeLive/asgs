#!/usr/bin/env perl

use strict;
use warnings;
use utf8;
use feature qw(say);

use Util::H2O::More qw(Getopt2h2o d2o);
use Template;
use JSON::XS qw(decode_json);
use POSIX qw(strftime);
use URI::Escape qw(uri_escape_utf8);
use File::Basename qw(basename);
use File::Spec;

my $cli = Getopt2h2o(
    \@ARGV,
    {
        host       => undef,
        path       => undef,
        local_dir  => undef,
        template   => undef,
        config     => "$ENV{HOME}/asgs-global.conf",
        base_url   => undef,
        title      => undef,
        brand_text => undef,
        brand_link => undef,
        output     => undef,
        deploy     => 0,
        verbose    => 0,
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
    )
);

my $cfg = read_ini_section($cli->config, 'run_index');

my %opt_hash = (
    host       => $cli->host,
    path       => $cli->path,
    local_dir  => $cli->local_dir,
    template   => $cli->template,
    config     => $cli->config,
    base_url   => defined($cli->base_url)   ? $cli->base_url   : ($cfg->{base_url}   // undef),
    title      => defined($cli->title)      ? $cli->title      : ($cfg->{title}      // 'ADCIRC/ASGS Run Directory'),
    brand_text => defined($cli->brand_text) ? $cli->brand_text : ($cfg->{brand_text} // 'ASGS on GitHub'),
    brand_link => defined($cli->brand_link) ? $cli->brand_link : ($cfg->{brand_link} // 'https://github.com/StormSurgeLive/asgs'),
    output     => defined($cli->output)     ? $cli->output     : ($cfg->{output}     // 'index.html'),
    deploy     => $cli->deploy ? 1 : 0,
    verbose    => $cli->verbose ? 1 : 0,
);

my $opt = d2o(\%opt_hash);

usage() unless $opt->host && $opt->path && $opt->template;

my $remote_path = $opt->path;
$remote_path =~ s{/*$}{};

my $effective_base_url = normalize_base_url(
    base_url => $opt->base_url,
    path     => $remote_path,
);

# ------------------------------------------------------------
# 1) Remote published listing only
# ------------------------------------------------------------

my $remote_listing = get_remote_listing(
    host => $opt->host,
    path => $remote_path,
);

my %present_remote = map { $_->{name} => 1 } @$remote_listing;

# ------------------------------------------------------------
# 2) Remote small metadata files
# ------------------------------------------------------------

my $run_properties = {};
my $fort14         = {};
my $fort15         = {};
my $scenario_json  = {};
my $buildinfo_json = {};
my $asgs_config    = {};

if ($present_remote{'run.properties'}) {
    my $text = ssh_cat($opt->host, "$remote_path/run.properties");
    $run_properties = parse_run_properties($text);
} else {
    die "run.properties not found in remote directory listing for $remote_path\n";
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

# ------------------------------------------------------------
# 3) Local source directory
# ------------------------------------------------------------

my $local_dir = $opt->local_dir;

if (!$local_dir) {
    $local_dir = find_local_run_dir($run_properties);
}

die "Could not determine a valid local scenario directory; pass --local_dir\n"
    unless $local_dir && -d $local_dir;

warn "[localdir] $local_dir\n" if $opt->verbose;

# ------------------------------------------------------------
# 4) Local fort.14 / fort.15
# ------------------------------------------------------------

if (-f "$local_dir/fort.14") {
    my $text = slurp_local("$local_dir/fort.14", 50_000);
    $fort14 = parse_fort14($text);
}

if (-f "$local_dir/fort.15") {
    my $text = slurp_local("$local_dir/fort.15", 250_000);
    $fort15 = parse_fort15($text);
}

# ------------------------------------------------------------
# 5) Prefer local ASGS config if available
# ------------------------------------------------------------

if ($ENV{ASGS_CONFIG} && -f $ENV{ASGS_CONFIG}) {
    my $text = slurp_local($ENV{ASGS_CONFIG}, 150_000);
    $asgs_config = parse_shell_config($text, $ENV{ASGS_CONFIG});
}
elsif ($run_properties->{all_keys} && $run_properties->{all_keys}{'config.file'}) {
    my $cfg_name = basename($run_properties->{all_keys}{'config.file'});
    if (-f "$local_dir/$cfg_name") {
        my $text = slurp_local("$local_dir/$cfg_name", 150_000);
        $asgs_config = parse_shell_config($text, "$local_dir/$cfg_name");
    }
    elsif ($present_remote{$cfg_name}) {
        my $text = ssh_head($opt->host, "$remote_path/$cfg_name", 150_000);
        $asgs_config = parse_shell_config($text, $cfg_name);
    }
}

# ------------------------------------------------------------
# 6) Build file objects from remote published names, local inspection
# ------------------------------------------------------------

my $files = build_file_objects(
    remote_listing => $remote_listing,
    local_dir      => $local_dir,
    base_url       => $effective_base_url,
    verbose        => $opt->verbose,
);

# ------------------------------------------------------------
# 7) Expected output counts from run.properties
# ------------------------------------------------------------

my $expected_records = expected_netcdf_records_from_run_properties($run_properties);

for my $f (@$files) {
    next unless $f->{is_netcdf};

    my $name = $f->{name};
    if (exists $expected_records->{$name}) {
        $f->{expected_records} = $expected_records->{$name};

        if ($f->{nc} && !exists $f->{nc}{error} && defined $f->{nc}{record_count}) {
            $f->{record_check} = ($f->{nc}{record_count} == $expected_records->{$name}) ? 'ok' : 'mismatch';
        } else {
            $f->{record_check} = 'unknown';
        }
    }
}

# ------------------------------------------------------------
# 8) Grouping and summaries
# ------------------------------------------------------------

my $grouped = group_files($files);

my @record_mismatches = grep {
       defined $_->{record_check}
    && $_->{record_check} eq 'mismatch'
} @$files;

my @record_unknown = grep {
       defined $_->{record_check}
    && $_->{record_check} eq 'unknown'
} @$files;

my $totals = {
    file_count          => scalar(@$files),
    bytes_total         => sum(map { $_->{size_bytes} || 0 } @$files),
    bytes_total_human   => human_size(sum(map { $_->{size_bytes} || 0 } @$files)),
    input_count         => scalar(@{ $grouped->{adcirc_input} }),
    output_count        => scalar(@{ $grouped->{adcirc_output} }),
    other_count         => scalar(@{ $grouped->{other} }),
    netcdf_count        => scalar(grep { $_->{is_netcdf} } @$files),
    netcdf_record_total => sum(
        map { ($_->{nc} && defined $_->{nc}{record_count}) ? $_->{nc}{record_count} : 0 }
        grep { $_->{is_netcdf} } @$files
    ),
    record_check_ok_count => scalar(
        grep { ($_->{record_check} // '') eq 'ok' } @$files
    ),
    record_check_mismatch_count => scalar(@record_mismatches),
    record_check_unknown_count  => scalar(@record_unknown),
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
        local_dir     => anonymize_path($local_dir),
        base_url      => $effective_base_url,
        deploy_target => anonymize_path("$remote_path/" . $opt->output),
    },
    totals            => $totals,
    files             => anonymize_files_for_stash($files),
    grouped           => anonymize_grouped_for_stash($grouped),
    run_properties    => $run_properties,
    fort14            => $fort14,
    fort15            => $fort15,
    scenario_status   => anonymize_structure($scenario_json),
    buildinfo         => anonymize_structure($buildinfo_json),
    asgs_config       => $asgs_config,
    record_mismatches => anonymize_files_for_stash(\@record_mismatches),
    record_unknown    => anonymize_files_for_stash(\@record_unknown),
};

my $tt = Template->new({
    ABSOLUTE => 1,
    RELATIVE => 1,
    TRIM     => 1,
    LTRIM    => 1,
    RTRIM    => 1,
    ENCODING => 'utf8',
});

$tt->process($opt->template, $stash, $opt->output)
    or die "Template processing failed: " . $tt->error . "\n";

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
  --config       INI file (default: ~/asgs-global.conf)
  --base_url     Override base URL from config
  --title        Override title
  --brand_text   Override branding text
  --brand_link   Override branding URL
  --output,-o    Override output filename
  --deploy       Copy generated output back to remote path
  --verbose      Verbose progress
USAGE
}

sub read_ini_section {
    my ($file, $want_section) = @_;
    return {} unless defined $file && -f $file;

    open my $fh, '<', $file or die "Could not open config '$file': $!\n";

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

    # Replace concrete env values back to their literal placeholders
    for my $name (qw(ASGS_CONFIG STATEFILE SYSLOG RUNDIR USER)) {
        my $val = $vars{$name};
        next unless defined $val && length $val;
        $s =~ s/\Q$val\E/\$$name/g;
    }

    # Extra safety: replace /work/<user>/ path segment to /work/$USER/
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
        $copy{local_file} = anonymize_path($copy{local_file}) if exists $copy{local_file};

        if ($copy{nc} && ref($copy{nc}) eq 'HASH') {
            my %nc = %{ $copy{nc} };
            $nc{local_file} = anonymize_path($nc{local_file}) if exists $nc{local_file};
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
    my $host = $args{host};
    my $path = $args{path};

    my $cmd = join ' ',
        'find', shell_quote($path),
        '-mindepth', '1',
        '-maxdepth', '1',
        '-printf', shell_quote('%f' . "\t" . '%s' . "\t" . '%T@' . "\t" . '%y' . "\n"),
        '| sort';

    my $out = ssh_capture(host => $host, cmd => $cmd);

    my @rows;
    for my $line (split /\n/, $out) {
        next unless length $line;
        my ($name, $size, $mtime_epoch, $typechar) = split /\t/, $line, 4;
        next if !defined $name || $name eq '';
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

        if ($rp && $rp->{all_keys} && $rp->{all_keys}{scenario}) {
            push @candidates, "$ENV{RUNDIR}/" . $rp->{all_keys}{scenario};
        }
        if ($rp && $rp->{all_keys} && $rp->{all_keys}{'asgs.enstorm'}) {
            push @candidates, "$ENV{RUNDIR}/" . $rp->{all_keys}{'asgs.enstorm'};
        }
    }

    @candidates = map { expand_path_vars($_) } @candidates;
    @candidates = grep { defined && $_ ne '' && $_ ne 'null' } @candidates;

    my %seen;
    @candidates = grep { !$seen{$_}++ } @candidates;

    for my $dir (@candidates) {
        return $dir if -d $dir && (-f "$dir/fort.15" || -f "$dir/run.properties" || scalar(glob("$dir/*.nc")));
    }

    return undef;
}

sub slurp_local {
    my ($file, $max_bytes) = @_;
    $max_bytes ||= 0;

    open my $fh, '<', $file or die "Could not open local file '$file': $!\n";
    binmode($fh);

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
    my $local_dir      = $args{local_dir}      || die "build_file_objects: missing local_dir\n";
    my $base_url       = $args{base_url};
    my $verbose        = $args{verbose};

    my @files;

    for my $row (@$remote_listing) {
        my $name = $row->{name};
        my $local_file = File::Spec->catfile($local_dir, $name);

        my ($size_bytes, $mtime_epoch, $typechar, $exists_local);

        if (-e $local_file) {
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
        if ($name =~ /\.nc$/i && -f $local_file) {
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
            url                => build_url($base_url, $name),
            ext                => file_ext($name),
            kind               => classify_file($name),
            is_netcdf          => ($name =~ /\.nc$/i ? 1 : 0),
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
      adcirc.file.output.fort.63.nc.numdatasets
      adcirc.file.output.fort.64.nc.numdatasets
      adcirc.file.output.fort.74.nc.numdatasets
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

    return {
        all_keys        => \%p,
        all_keys_anon   => \%anon_all,
        summary         => \%summary,
        key_count       => scalar(keys %p),
        sorted_keys     => [ sort keys %p ],
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
        dimensions   => {},
        variables    => [],
        record_count => undef,
        time_dim     => undef,
    );

    while ($txt =~ /^\s*([A-Za-z0-9_]+)\s*=\s*UNLIMITED\s*;\s*\/\/\s*\((\d+)\s+currently\)\s*$/mg) {
        my ($dim, $n) = ($1, $2);
        $nc{dimensions}{$dim} = {
            unlimited => 1,
            size      => $n + 0,
        };
        $nc{record_count} = $n + 0 if !defined $nc{record_count};
        $nc{time_dim}     = $dim if !defined $nc{time_dim} && $dim =~ /time/i;
    }

    while ($txt =~ /^\s*([A-Za-z0-9_]+)\s*=\s*(\d+)\s*;\s*$/mg) {
        my ($dim, $n) = ($1, $2);
        next if exists $nc{dimensions}{$dim};
        $nc{dimensions}{$dim} = {
            unlimited => 0,
            size      => $n + 0,
        };
    }

    while ($txt =~ /^\s*(?:byte|char|short|int|float|double)\s+([A-Za-z0-9_]+)\s*\((.*?)\)\s*;/mg) {
        my ($var, $dims) = ($1, $2);
        push @{ $nc{variables} }, {
            name => $var,
            dims => [ map { s/^\s+|\s+$//gr } split /,/, $dims ],
        };
    }

    if (!defined $nc{record_count}) {
        for my $dim (sort keys %{ $nc{dimensions} }) {
            if ($dim =~ /time/i) {
                $nc{record_count} = $nc{dimensions}{$dim}{size};
                $nc{time_dim}     = $dim;
                last;
            }
        }
    }

    $nc{variable_count}  = scalar @{ $nc{variables} };
    $nc{dimension_count} = scalar keys %{ $nc{dimensions} };

    if (!defined $nc{record_count} && !%{ $nc{dimensions} } && !@{ $nc{variables} }) {
        return { error => "ncdump returned no parseable header output" };
    }

    return \%nc;
}

sub classify_file {
    my ($name) = @_;

    return 'adcirc_input'  if $name =~ /^(fort\.1[45]|fort\.22|run\.properties|.*\.asgs-.*|.*\.sh|cycle\.log|scenario\.log|scenario\.status\.json|adcirc\.bin\.buildinfo\.json)$/i;
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

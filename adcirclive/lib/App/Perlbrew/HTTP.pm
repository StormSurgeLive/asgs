package App::Perlbrew::HTTP;
use strict;
use warnings;
use 5.008;
 
use Exporter 'import';
our @EXPORT_OK = qw/http_user_agent_program http_user_agent_command http_get http_download http_postJSON http_getJSON/;
 
our $HTTP_VERBOSE = 0;
our $HTTP_USER_AGENT_PROGRAM;
 
my %commands = (
    curl => {
        test     => '--version >/dev/null 2>&1',
        get      => '--insecure --silent --location --fail -o - {url}',
        download => '--insecure --silent --location --fail -o {output} {url}',
        postJSON => "--insecure --silent --location --fail -o - -X POST -H 'Content-Type: application/json' {headerstr} -d '{json}' {url}",
        getJSON  => "--insecure --silent --location --fail -o - -X GET {headerstr} {url}",
        order    => 1,
 
        # Exit code is 22 on 404s etc
        die_on_error => sub { die 'Page not retrieved; HTTP error code 400 or above.' if ($_[ 0 ] >> 8 == 22); },
    },
);
 
sub http_user_agent_program {
    $HTTP_USER_AGENT_PROGRAM ||= do {
        my $program;
 
        for my $p (sort {$commands{$a}{order}<=>$commands{$b}{order}} keys %commands) {
            my $code = system("$p $commands{$p}->{test}") >> 8;
            if ($code != 127) {
                $program = $p;
                last;
            }
        }
 
        unless ($program) {
            die "[ERROR] Cannot find a proper http user agent program. Please install curl or wget.\n";
        }
 
        $program;
    };
 
    die "[ERROR] Unrecognized http user agent program: $HTTP_USER_AGENT_PROGRAM. It can only be one of: ".join(",", keys %commands)."\n" unless $commands{$HTTP_USER_AGENT_PROGRAM};
 
    return $HTTP_USER_AGENT_PROGRAM;
}
 
sub http_user_agent_command {
    my ($purpose, $params) = @_;
    my $ua = http_user_agent_program;
    my $cmd = $commands{ $ua }->{ $purpose };
    for (keys %$params) {
        $cmd =~ s/{$_}/$params->{$_}/g;
    }
 
    if ($HTTP_VERBOSE) {
        unless ($ua eq "fetch") {
            $cmd =~ s/(silent|quiet)/verbose/;
        }
    }
 
    $cmd = $ua . " " . $cmd;

    return ($ua, $cmd) if wantarray;
    return $cmd;
}
 
sub http_download {
    my ($url, $path, $FORCE) = @_;
 
    if (-e $path) {
        if ($FORCE) {
          unlink $path; 
        }
        else {
          die "ERROR: The download target < $path > already exists.\n";
        }
    }
 
    my $partial = 0;
    local $SIG{TERM} = local $SIG{INT} = sub { $partial++ };
 
    my $download_command = http_user_agent_command(download => { url => $url, output => $path });
 
    my $status = system($download_command);
    if ($partial) {
        $path->unlink;
        return "ERROR: Interrupted.";
    }
    unless ($status == 0) {
        $path->unlink;
        if ($? == -1) {
            return "ERROR: Failed to execute the command\n\n\t$download_command\n\nReason:\n\n\t$!";
        }
        elsif ($? & 127) {
            return "ERROR: The command died with signal " . ($? & 127) . "\n\n\t$download_command\n\n";
        }
        else {
            return "ERROR: The command finished with error\n\n\t$download_command\n\nExit code:\n\n\t" . ($? >> 8);
        }
    }
    return 0;
}
 
sub http_get {
    my ($url, $header, $cb) = @_;
 
    if (ref($header) eq 'CODE') {
        $cb = $header;
        $header = undef;
    }
 
    my ($program, $command) = http_user_agent_command(get => { url =>  $url });
 
    open my $fh, '-|', $command
    or die "open() pipe for '$command': $!";
 
    local $/;
    my $body = <$fh>;
    close $fh;
 
    # check if the download has failed and die automatically
    $commands{ $program }{ die_on_error }->($?);
 
    return $cb ? $cb->($body) : $body;
}

sub http_postJSON {
    my ($url, $JSON, $extra_headers, $cb) = @_;

    my @extra_headers = ();
    foreach my $header (keys %$extra_headers) {
      push @extra_headers, sprintf(qq{-H '%s: %s'}, $header, $extra_headers->{$header});
    }
 
    my ($program, $command) = http_user_agent_command(postJSON => { url =>  $url, json => $JSON, headerstr => join(q{ }, @extra_headers) });
 
    open my $fh, '-|', $command
    or die "open() pipe for '$command': $!";
 
    local $/;
    my $body = <$fh>;
    close $fh;
 
    # check if the download has failed and die automatically
    $commands{ $program }{ die_on_error }->($?);
 
    return $cb ? $cb->($body) : $body;
}

sub http_getJSON {
    my ($url, $extra_headers, $cb) = @_;

    my @extra_headers = ();
    foreach my $header (keys %$extra_headers) {
      push @extra_headers, sprintf(qq{-H '%s: %s'}, $header, $extra_headers->{$header});
    }
 
    my ($program, $command) = http_user_agent_command(getJSON => { url =>  $url, headerstr => join(q{ }, @extra_headers) });
 
    open my $fh, '-|', $command
    or die "open() pipe for '$command': $!";
 
    local $/;
    my $body = <$fh>;
    close $fh;
 
    # check if the download has failed and die automatically
    $commands{ $program }{ die_on_error }->($?);
 
    return $cb ? $cb->($body) : $body;
}
 
1;

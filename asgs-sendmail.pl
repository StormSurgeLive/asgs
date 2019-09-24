#!/usr/bin/env perl
#--------------------------------------------------------------------------
# asgs-sendmail.pl
#--------------------------------------------------------------------------
# Copyright(C) 2019 Brett Estrade
# Copyright(C) 2019 Jason Fleming
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
#--------------------------------------------------------------------------
package util::asgs_sendmail;

use strict;
use warnings;

use Email::Sender::Simple qw(sendmail);  # main email capability (receives TLS obj)
use Email::Sender::Transport::SMTP::TLS; # creates transport object
use Email::Simple::Creator;  # generates message itself (converts header and body from hash to text email)
use Config::Tiny; # reads ini format ... converts to a perl hash
use Try::Tiny;    # exception handling (alternative to eval)
use Getopt::Long;
use constant EXIT_SUCCESS => 0;

my $subject     = q{no subject};
my $HOME        = ( getpwuid($<) )[7];
my $config_file = qq{$HOME/asgs-global.conf};
my $to;

GetOptions(
    "c|config=s"  => \$config_file,
    "h|help"      => \&show_help,
    "s|subject=s" => \$subject,
    "t|to=s"      => \$to,
);

# verify recipient email has been set via --to
die qq{a recipient is required to be defined via --to\n} unless defined $to;

# make sure that $to is a comma delimited list of email addresses, accept a
# space delimted list, but reform it to use commas

$to =~ s/,/ /g;          # turn all commas to spaces
$to =~ s/ +/ /g;         # reduce all contiguous spaces of 2 or more, down to 1 space
my @to = split / /, $to; # split on single space
$to = join q{,}, @to;    # recreate list of email addresses and delimit them by commas

exit run();

sub run {
    my @content = <STDIN>;
    my $content;
    if (@content) {
        # don't need to add a newline between records since each already
        # ends in a newline
        $content = join( qq{}, @content );
    }
    else {
        $content = q{empty message};
    }

    my $config = Config::Tiny->new();
    $config = Config::Tiny->read($config_file);

    my $transport = Email::Sender::Transport::SMTP::TLS->new(
        host     => $config->{email}->{smtp_host},
        port     => $config->{email}->{smtp_port} // 587,    # defaults to TLS if not set
        username => $config->{email}->{smtp_username},
        password => $config->{email}->{smtp_password},
        helo     => 'HELO',
    );

    my $message = Email::Simple->create(
        header => [
            'Reply-To' => $config->{email}->{reply_to_address} // $config->{email}->{from_address},
            From       => $config->{email}->{from_address},
            To         => $to,
            Subject    => $subject,
        ],
        body => $content,
    );

    try {
        sendmail( $message, { transport => $transport } );
    }
    catch {
        die "Error sending email: $_";
    };
    return EXIT_SUCCESS;
}

sub show_help {
    print qq{$0 --subject "subject line" --to recipient\@email.tdl < body.txt\n};
    exit EXIT_SUCCESS;
}

1;

__END__

=head1 NAME

  asgs-sendmail.pl

=head1 USAGE 

  # piped STDIN
  cat body.txt | asgs-sendmail.pl [-c override/default/conf] --subject "subject line" --to recipient@email.tld
  some-utility | asgs-sendmail.pl [-c override/default/conf] --subject "subject line" --to recipient@email.tld

  # directed file contents in using '<', typical unix syntax
  asgs-sendmail.pl [-c override/default/conf] --subject "subject line" --to recipient@email.tld < body.txt

=head2 Multiple Recipients

The C<--to> options accepts a comma delimited list of email addresses:

  some-utility | asgs-sendmail.pl [-c override/default/conf] --subject "subject line" --to "recipient1@email.tld, recipient2@email.tld, ..., recipientN@email.tld"

A list of email addresses that are separated by spaces (like the actual sendmail utility) is also supported

  some-utility | asgs-sendmail.pl [-c override/default/conf] --subject "subject line" --to "recipient1@email.tld recipient2@email.tld ... recipientN@email.tld"

=head1 DESCRIPTION

This utility is used to send emails using an external SMTP server.

=head1 OPTIONS

=head2 Required

=over4

=item C<--to>

One or more recipients must be defined. If more than one, pass in as a comma delimted string. See perldoc for more information (POD at the end of this file).

=back

=head2 Optional

=over4

=item C<--c>

Define a non-default configuration file. See below for the format involved in creating the configuration file used by this script.

=item C<--subject>

Defines subject thread. There is a default, but it's meaningless.

=back
=head1 CONFIGURATION

By default, a configuration file located at $HOME/asgs-global.conf is
required. This file contains the necessary authoriziation information
required for sending emails through an external SMTP server gateway.
TLS (port 587) is a preferred transport for security reasons, but the
underlying module supports SSL and unecrypted (port 25).

  [email]
  from_address=info@stormsurge.email
  mail_reply_to=info@stormsurge.email
  smtp_host=smtp.host.name
  smtp_port=587 
  smtp_username=smtp-username
  smtp_password=smtp-password

=head1 LIMITATIONS

Currently, the content of the body of the message must be provided
via STDIN, this means it must be piped in via an external source.
This source could be another program or an existing file.

This document will be expanded, but the necessary step is to
create a file in $HOME called $HOME/asgs-global.conf that
contains the AWS SES (Simple Email Service) credentials. See
the asgs-global.conf.sample file in the ASGS repository as
a template.

NOTE: Lead operator will need to provide you with the required
SMTP authentication information, which may change time to time.

For help on the tool, 

1. Usage:

  ./asgs-sendmail.pl --help

2. Details

  perldoc asgs-sendmail.pl

3. Note, default $HOME/asgs-global.conf

   ;;; this file contains information that should not
   ;;; be stored in a git repository
   
   ;;; note, this "[email]" is a section header for the config file,
   ;;; do not replace it with an email address =)
   [email]

   from_address=info@stormsurge.email
   reply_to_address=info@stormsurge.email
   smtp_host=email-smtp.us-east-1.amazonaws.com
   smtp_port=587
   smtp_username=(redacted)
   smtp_password=(redacted)

   ;;; note, "from" addresses must be verified via Amazon's SES console
   ;;; before deviating from 'info@stormsurge.email' 
   ;;; note, from_address is not what it may look like it came from,
   ;;; currently it may be slightly different because this part of it
   ;;; is actually managed by AWS on their end 

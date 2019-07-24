#!/bin/env python

# Adapted from http://snippets.dzone.com/posts/show/757 By Phil Bordelon @ LONI 
#
# usage:
#  % python emailattach.py "comma, delimited, list, of, addresses" "subject" "body text file" "from" "mailserver" "space delimited list of attachments ad infinitum..."
#

import os
import sys
import smtplib
import ConfigParser
import os.path
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email.Utils import COMMASPACE, formatdate
from email import Encoders
from os.path import expanduser

def sendMail(to, subject, text, fro, server, files=[]):
    assert type(to)==list
    assert type(files)==list

    msg = MIMEMultipart()
    msg['From'] = fro 
    msg['To'] = COMMASPACE.join(to)
    msg['Date'] = formatdate(localtime=True)
    msg['Subject'] = subject

    msg.attach( MIMEText(text) )

    # look for SMTP config file
    home = expanduser("~")
    config_file = home + '/asgs-global.conf'
    config = ConfigParser.ConfigParser()
    config.read(config_file)
    dConfig = config.__dict__['_sections'].copy()

    for filein in files:
        part = MIMEBase('application', "octet-stream")
        part.set_payload( open(filein,"rb").read() )
        Encoders.encode_base64(part)
        part.add_header('Content-Disposition', 'attachment; filename="%s"'
                       % os.path.basename(filein))
        msg.attach(part)

    if os.path.isfile(config_file): 
        # 'server' and 'fro' params passed in are ignored if config_file is detected
        smtp = smtplib.SMTP(dConfig['email']['smtp_host'])
        smtp.starttls()
        smtp.ehlo(dConfig['email']['smtp_host']);
        smtp.login(dConfig['email']['smtp_username'],dConfig['email']['smtp_password']); 
        smtp.sendmail(dConfig['email']['from_address'], to, msg.as_string() )
        smtp.close()
    else:
        smtp = smtplib.SMTP(server) # for when smtp is not external (need a check)
        smtp.sendmail(fro, to, msg.as_string() )
        smtp.close()

if __name__ == "__main__":
    addresslist = sys.argv[1].split(",")
    subject = sys.argv[2]
    bodyfile = sys.argv[3]
    fro = sys.argv[4]
    smtpserver = sys.argv[5]
    attachmentlist = sys.argv[6:]
    
    bodytext = []
    for line in open(bodyfile,"r").xreadlines():
       bodytext.append(line)

    bodystring = "\n".join(bodytext)
       
    sendMail(
        addresslist,
        subject,
        bodystring,
        fro,
        smtpserver,
        attachmentlist 
       )

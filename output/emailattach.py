#!/bin/env python

# Adapted from http://snippets.dzone.com/posts/show/757 By Phil Bordelon @ LONI 
#
# usage:
#  % python emailattach.py "comma, delimited, list, of, addresses" "subject" "body text file" "from" "mailserver" "space delimited list of attachments ad infinitum..."
#

import smtplib
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email.Utils import COMMASPACE, formatdate
from email import Encoders
import os
import sys

def sendMail(to, subject, text, fro, server, files=[]):
    assert type(to)==list
    assert type(files)==list

    msg = MIMEMultipart()
    msg['From'] = fro 
    msg['To'] = COMMASPACE.join(to)
    msg['Date'] = formatdate(localtime=True)
    msg['Subject'] = subject

    msg.attach( MIMEText(text) )

    for filein in files:
        part = MIMEBase('application', "octet-stream")
        part.set_payload( open(filein,"rb").read() )
        Encoders.encode_base64(part)
        part.add_header('Content-Disposition', 'attachment; filename="%s"'
                       % os.path.basename(filein))
        msg.attach(part)

    smtp = smtplib.SMTP(server)
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

#!/usr/bin/env python3

import smtplib
import email.mime.text
import syslog

#from subprocess import call
import os

syslog.openlog('[UPS]')
def log(msg):
    syslog.syslog(str(msg))

GMAIL_ADDRESS = '<sending address here>'
GMAIL_PASSWORD = '<password here>'

from_email = GMAIL_ADDRESS
to_emails = ["<addresses to send to>"]

msg_text = os.popen("ssh -p 2525 fortytwo.cct.lsu.edu \"df | grep scratch; du -sh /scratch/opendap/tc/*\"").read()

print("msg_text: ")
print(msg_text)

space_left = int(msg_text.split("\n")[0].split()[3])

print("space left: ", space_left)
min_space = 104857600 # This is 100 GB

# If less than 100GB of space is left, send an email
if space_left < min_space:
    print("space is less than 100GB!")

    msg_subject = "fortytwo.cct.lsu.edu: "+msg_text.split("\n")[0]

    print(msg_subject)

    log(msg_subject)

    msg = email.mime.text.MIMEText(msg_text)
    msg['Subject'] = msg_subject
    msg['From'] = from_email
    msg['To'] = ", ".join(to_emails)
    s = smtplib.SMTP_SSL('smtp.gmail.com', '465')
    s.login(GMAIL_ADDRESS, GMAIL_PASSWORD)
    s.sendmail(from_email, to_emails, msg.as_string())
    s.quit()

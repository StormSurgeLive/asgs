#!/bin/bash

# for FTP
docker run -d -p 20:20 -p 21:21 -p 21100-21110:21100-21110 --restart always -v /home/user/data:/var/ftp:ro vsftpd/anonymous 

#!/bin/sh

docker run -p 80:80 -p 443:443 -v /home/user/nginx/etc/ssl:/etc/nginx/certs:ro -v /home/user/rss-data:/usr/share/nginx/html:ro --restart always -d nginx-ssl

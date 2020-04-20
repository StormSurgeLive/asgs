#!/bin/sh

docker -v /home/user/data/rss-data:/usr/share/nginx/html:ro --restart always -d nginx

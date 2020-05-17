#!/bin/bash

SSL_DIR=$HOME/nginx/etc/ssl
FQDN=$(hostname --long)
mkdir -p $SSL_DIR
openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout $SSL_DIR/nginx.key -out $SSL_DIR/nginx.crt

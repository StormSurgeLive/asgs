#!/usr/bin/env bash

THREDDSHOST=159.89.232.221 # WWW hostname for emailed links

OPENDAPINDEX=""             # if set to something other than "catalog.html", DOWNLOADPREFIX is used
                            #  by opendap_post2.sh to generate the http link address to the directory
			    #  listing

OPENDAPHOST=seahorse1_tds   # alias in $HOME/.ssh/config
OPENDAPPORT=":80"           # ':80' can be an empty string, but for clarity it's here
OPENDAPPROTOCOL="http"
DOWNLOADPREFIX=/fileServer
CATALOGPREFIX=/fileServer
OPENDAPBASEDIR=/mnt/volume_nyc1_01/fileServer
echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
echo "post.opendap.${SERVER}.copyablehosts : ( null )" >> $RUNPROPERTIES

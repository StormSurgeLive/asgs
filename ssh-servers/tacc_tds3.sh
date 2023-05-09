#!/usr/bin/env bash

THREDDSHOST=chg-1.oden.tacc.utexas.edu # WWW hostname for emailed links

OPENDAPINDEX=catalog.html   # if set to something other than "catalog.html", DOWNLOADPREFIX is used
                            #  by opendap_post2.sh to generate the http link address to the directory
			    #  listing

OPENDAPHOST=tacc_tds3       # alias in $HOME/.ssh/config
OPENDAPPORT=":80"           # ':80' can be an empty string, but for clarity it's here
OPENDAPPROTOCOL="http"
DOWNLOADPREFIX=/asgs
CATALOGPREFIX=/asgs
OPENDAPBASEDIR=/hurricane
echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
echo "post.opendap.${SERVER}.copyablehosts : ( null )" >> $RUNPROPERTIES

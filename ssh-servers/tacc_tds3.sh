#!/usr/bin/env bash

THREDDSHOST=chg-1.oden.tacc.utexas.edu # WWW hostname for emailed links
OPENDAPHOST=tacc_tds3                  # alias in $HOME/.ssh/config
OPENDAPPORT=":80"                      # ':80' can be an empty string, but for clarity it's here
OPENDAPPROTOCOL="http"
DOWNLOADPREFIX=/asgs
CATALOGPREFIX=/asgs
OPENDAPBASEDIR=/hurricane
echo "post.opendap.${SERVER}.linkablehosts : ( null )" >> $RUNPROPERTIES
echo "post.opendap.${SERVER}.copyablehosts : ( null )" >> $RUNPROPERTIES

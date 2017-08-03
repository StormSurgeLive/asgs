#!/bin/bash
#
# fort.63
ts=1; while [[ $ts -lt 85 ]]; do fnum=`printf "%02d" $ts`;  line=`expr $ts - 1` ; line="$line       python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t $ts -s -o elev${fnum}" ; echo $line >> elev_shape.conf ; ts=`expr $ts + 1`; done
#
# fort.74
ts=1; while [[ $ts -lt 85 ]]; do fnum=`printf "%02d" $ts`; line=`expr $ts - 1` ; line="$line       python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t $ts -s -o wind${fnum}" ; echo $line >> wind_shape.conf ; ts=`expr $ts + 1`; done

#!/usr/bin/bash

adv=0

if [[ -L "index-at.xml" ]] ; then
	# get adv num
	adv=`readlink index-at.xml`
	adv=`echo $adv | awk -F . '{print $1}'`
	adv=`echo $adv | bc`
fi

adv=`echo "$adv+1" | bc`
echo $adv

abase=`ls -1 ??.*.index-at.xml | head -n 1 |awk -F . '{print $2}'`
bbase=`ls -1 ??.bal* | head -n 1 |awk -F . '{print $2}'`
echo $abase, $bbase

# make links to a,b files
aname=`printf '%02d.%s.index-at.xml' $adv $abase`
bname=`printf '%02d.%s.dat' $adv $bbase`
echo $aname, $bname

ln -sf  "$bname" "$bbase.dat"
sleep 5

ln -sf  "$aname" "index-at.xml"

#!/bin/bash
ADVISDIR=$1
STORM=$2
KIND=$3
ADVISORY=$4
HOSTNAME=$5
ENSTORM=$6
OUTPUTPREFIX=$7
POST_LIST=$8

#
PTDIR=$ADVISDIR/TACKING
POSTDIR=/corral/hurricane/asgs_output
ANIMPOSTDIR=/corral/hurricane/asgs_output/movies

cat <<END > $ADVISDIR/post_notify.txt 
ASGS Oil Spill results available for $KIND $STORM advisory $ADVISORY 
and initial particle location ${PARTICLEFILE} on $HOSTNAME

The final output products (animations/movies) are located on Corral:

$ANIMPOSTDIR/$OUTPUTPREFIX.gif
$ANIMPOSTDIR/$OUTPUTPREFIX.avi
$ANIMPOSTDIR/$OUTPUTPREFIX.mp4

The particle position file can be found on Corral:

$ADVISDIR/$OUTPUTPREFIX.pth

Along with all of the individual still images and all of the processing files and 
programs found in :

$ADVISDIR/
$PTDIR/
$ADVISDIR/MONTAGE


END
#
cat $ADVISDIR/post_notify.txt | asgs-sendmail --subject "ASGS Oil Spill results available for $KIND $STORM advisory $ADVISORY  Initial Particle location ${PARTICLEFILE} on $HOSTNAME" --to "$POST_LIST"







#!/bin/bash
# postProcessFlux.sh
#
# An ASGS post processing script to calculate flux from the ADCIRC model
# results this script utilizes in its companion fluxAcrossNodestrings.pl
# Perl script to do the flux calculation.  For details about the 
# required input (nodestringfile) see comments within the 
# fluxAcrossNodestring.pl script.
#
# It also requires that you have the netcdf2adcirc utility program
#
# This script first will download or copy the netcdf versions of the 
# fulldomain time series water surface elevation and depth averaged 
# current velocity output from ADCIRC (i.e. the fort.63.nc and 
# fort.64.nc files) to a directory you specify below (postdir)
# (note: this would not be necessary if this script is run in within
# the ASGS. In that case you could use symlinks or just do the post
# processing in the directory that ADCIRC ran in.)
#
# Then it uses the netcdf2adcirc utility to convert these to ascii.
# (note for future work: this would be unnecessary if we could find a 
# good Perl module for reading netdcf directly)
#
# Then it runs the flux calculator script. 
#
# the fluxcalculator and and netcdf2adcirc jobs are submitted and
# run as single processor jobs to the hpc system. This requires the 
# postProcessFlux_templatefiller.pl Perl script and 
# postProcessFlux.pbs.template pbs script template to put together
# and the job submission scripts. This template and template filler
# may need to be modified to accommodate different hpc systems
# As it is, this works just fine with the pbs submission system
# on superMikeII (mike.hpc.lsu.edu), it probably will work on 
# queenbeeII (qb2.loni.org) without too much tweaking.   
#
# It is suggested, but not required that the nodestringsfile, which is
# input to the fluxcalculator be kept in the asgs /input/meshes/
# subdirectory that contains the grid file and other adcirc model 
# specific input files since this file is grid specific.
#
# example usage:
# bash /home/nate/cera/asgs/2014stable/output/postProcessFlux/postProcessFlux.sh 2016010918 cpra2017_v11k-CurrentConditions_chk queenbee.loni.org nowflowopen namforecast
# 
# The input agruments above are essentially the names of the 
# subdirectories that contain the run output on the opendap server
#
#######################################################################
# Copyright(C) 2016 Nathan Dill
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#####################################################################




#######################################################################
# configuration
#######################################################################
# ! note: I figure for running in the asgs, some of this configuration
#       information needed below could be read from the run.properties
#       or just passed as input arguments from asgs_main.

#-----
# Info needed to retrive the required input files
# i.e. it is everything afterh the 'nam' directory in the following example url 
#  http://opendap.renci.org:1935/thredds/fileServer/tc/nam/20160107006/cpra2017_v11k-CurrentConditions_chk/queenbee.loni.org/nowflowopen/namforecast/fort.63.nc"
#-----
rundate=$1 # the forecast data (same as on opendap server)
#rundate=2016010818 # the forecast data (same as on opendap server)
gridname=$2 # same as used by asgs from opendap path
#gridname=cpra2017_v11k-CurrentConditions_chk # same as used by asgs from opendap path
asgshost=$3 # same as used by asgs from opendap path
#asgshost=queenbee.loni.org # same as used by asgs from opendap path
instance=$4   # name of the asgs instance from opendap path
#instance=nowflowopen   # name of the asgs instance from opendap path
ensemblemember=$5 # same as used on opendap
#ensemblemember=namforecast # same as used on opendap



#-----
# Info for the pbs system
#-----
account=hpc_cera_2015 # user account for hpc system
walltime=01:00:00  #why is netcdf2adcirc so slow?
useremail=natedill@gmail.com # email address that goes in PBS script

#----
# Important paths
#----
postdir=/work/nate/MissRiverFlooding2016/$rundate/$gridname/$asgshost/$instance/$ensemblemember # the dir we're post processin in
postscriptdir=/home/nate/cera/asgs/2014stable/output/postProcessFlux # the dir that contains the post processing scripts etc.
# ncdatadir below is the dir that has the netcdf adcirc output.
# For now, while running on another server, we are copying the 
# files to to the postdir and using postdir as the ncdatadir.
# But, if the post processing is run on the same server, 
# ncdatadir should probably be the directory that adcirc ran in
# In the later case there would be no need to copy the netcdf 
# data files from another 
ncdatadir=$postdir

#----
#location of the netcdf2adcirc executable
#----
# We wouldn't have to use this if the flux calculator could
# just read netcdf files directly. That would probably 
# save some time too.
netcdf2adcirc=/home/nate/cera/asgs/2014stable/output/netcdf2adcirc.x


#----
# pbs script template and filler
#----
templatefiller=$postscriptdir/postProcessFlux_templateFiller.pl # script to fill in the pbs template
pbstemplate=$postscriptdir/postProcessFlux.pbs.template # pbs template

#----
# flux calculator
#----
fluxcalculator=$postscriptdir/fluxAcrossNodestrings.pl # script to calculate flux
kmloutput=$postdir/Flux-$rundate-$instance.kml # output file for flux calculatorS

meshdir=/home/nate/cera/asgs/2014stable/input/meshes/cpra2017_v11k-CurrentConditions

gridfile=$meshdir/cpra2017_v11k-CurrentConditions_chk.grd # full path to the grid file
nodestringfile=$meshdir/cpra2017_v11k-CurrentConditions_FluxNodestrings.txt  # input file for flux calculator

######################################################
# end configuration section
######################################################





#############################################
# make the dir for post processing and go 
# there
#############################################
echo "making the directory for postprocessing fluxcalculator:"
echo "         $postdir"
mkdir -p $postdir 
#cd $postdir   # maybe we really don't have to go there?

##############################################
# Get the output files 
##############################################

# scp from fortytwo
opendappath=/scratch/opendap/tc/nam/$rundate/$gridname/$asgshost/$instance/$ensemblemember
echo "getting files from fortytwo"
echo "scp -P 2525 nate@fortytwo.cct.lsu.edu:$opendappath/fort.63.nc $postdir"
scp -P 2525 nate@fortytwo.cct.lsu.edu:$opendappath/fort.63.nc $postdir
echo "scp -P 2525 nate@fortytwo.cct.lsu.edu:$opendappath/fort.64.nc $postdir"
scp -P 2525 nate@fortytwo.cct.lsu.edu:$opendappath/fort.64.nc $postdir

# wget from renci
# echo "getting files from renci"
# echo "wget --directory-prefix=$postdir http://opendap.renci.org:1935/thredds/fileServer/tc/nam/$rundate/$gridname/$asgshost/$instance/$ensemblemember/fort.63.nc"
# wget --directory-prefix=$postdir http://opendap.renci.org:1935/thredds/fileServer/tc/nam/$rundate/$gridname/$asgshost/$instance/$ensemblemember/fort.63.nc
# echo "wget --directory-prefix=$postdir http://opendap.renci.org:1935/thredds/fileServer/tc/nam/$rundate/$gridname/$asgshost/$instance/$ensemblemember/fort.64.nc"
# wget --directory-prefix=$postdir http://opendap.renci.org:1935/thredds/fileServer/tc/nam/$rundate/$gridname/$asgshost/$instance/$ensemblemember/fort.64.nc

###############################################
# convert the files to ascii
###############################################
# a list of files to process
datafiles=(fort.63.nc fort.64.nc)
jobnames=($RANDOM.n2a63 $RANDOM.n2a64) # need unique jobnames cause we're checking them in qstat down below
#loop over the datafiles
i=0 
for datafile in "${datafiles[@]}"
do 
  echo "converting $datafile to ascii"
  pbsscript="$postdir/$datafile.pbs"
  # fill in the template
  jobname=${jobnames[i]}
  args="--datafile $ncdatadir/$datafile"  # note: this gets passed after the -- so GetOptions carries it in in @ARGV
  opts="--template=$pbstemplate --pbsscript=$pbsscript --useremail=$useremail --account=$account --walltime=$walltime --jobname=$jobname --postdir=$postdir --executable=$netcdf2adcirc -- $args"
  echo "calling $templatefiller with options:"
  echo $opts 
  perl $templatefiller $opts

  # submit the job
  qsub $pbsscript
  let i=i+1
done


# check to see if the jobs are all done
# data file is used in the jobname,
# so if it is not in the qstat output
# and the file without .nc exists
# assume its done
count=0
while true 
do 
  echo "checked qstat $count times"
  if [ "$count" -gt 60 ]
  then 
     echo "netcdf2adcirc has taken too long"
     exit 1
  fi
  if [ "$count" -gt 4 ]; then 
     echo "why is netcdf2adcirc so slow?"
     if [ "$count" -gt 9 ]; then 
        echo " ... like molasses in February"
     fi
  fi
  stillrunning=1
  for jobname in "${jobnames[@]}" 
  do
     stillrunning=`qstat | grep  $USER | grep $jobname | wc -l`
     if [ "$stillrunning" -gt 0 ]
     then
        echo "netcdf2adcirc $datafile is still running"
        break
     fi
  done
  if  [ "$stillrunning" -gt 0 ]
  then 
     sleep 60
     ((count++))
     continue
  fi
  # here if the results of qstat show its done
  # now check if the finish file is written
  found=1
  for jobname in "${jobnames[@]}"
  do 
     file2="$jobname.finish"
     if [ ! -f $postdir/$file2 ]
     then 
        found=0
     fi
  done
  if [ "$found" -eq 1 ]
  then
     echo "looks like netcdf2adcirc jobs are all done"
     break
  else 
     echo "it appears that the netcdf2adcirc jobs are no longer running, but did not finish properly"
     exit 2
  fi
done  


##################################################
# Now that we've got all the data files converted
# to ascii, run the flux calculator.
#################################################

# check that we have the files
fileExists()  # function to check file existance
{
   file=$1
   exitcode=$2
   if [ ! -f $file ]
   then
      echo "cant find file: $file"
      exit $exitcode
   fi
}

fileExists $gridfile 3
fileExists $postdir/fort.63 4
fileExists $postdir/fort.64 5
fileExists $nodestringfile 6

# make a link to the grid file
ln -s $gridfile $postdir/fort.14 

# run the flux calculator

# fill in the template
jobname="${instance}Flx"
pbsscript="$postdir/${jobname}.pbs"
args="$fluxcalculator --nodestrings $nodestringfile --kmloutput $kmloutput --gridfile fort.14 --fort63 $postdir/fort.63 --fort64 $postdir/fort.64" # gets passsed after the -- so that GetOptions in the template filler puts it into ARGV for later passing to the executable

options="--template=$pbstemplate --useremail=$useremail --account=$account --walltime=$walltime --jobname=$jobname --postdir=$postdir --executable=perl --pbsscript=$pbsscript -- $args"

echo "calling $templatefiller with options:"
echo "$options"
perl $templatefiller $options

# submit the job
qsub $pbsscript

# check to see if the fluxcalculator is done

# do something with the output?


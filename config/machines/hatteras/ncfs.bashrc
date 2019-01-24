# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
#module load intelc/11.1_046
#module load intelfort/11.1_046
#module load mvapich2/1.9_intel-11.1_046
#module load mvapich2/2.0_intel-14.0.3_nemesis
#module load mvapich2/2.0_intel-14.0.3_ch3
# deprecated 20180102
#module load intelc/14.0.3
#module load intelfort/14.0.3
#module load netcdf/4.1.3_intel-14.0.3
#module load mvapich2/2.0_intel-14.0.3_ch3_ofed-3.1
# load netcdf libraries for use in netcdf utilities
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/projects/ncfs/apps/croatan/netcdf/lib
#
# new on 20180102
module load hdf5/1.10.1_intel-18.0.0
module load intelc/18.0.0
module load intelfort/18.0.0
# new on 20190124
module load mvapich2/2.3b_intel-18.0.0_ch3_ofed-4.1-test
module load netcdf-C/4.5.0_intel-18.0.0
module load netcdf-Fortran/4.4.0_intel-18.0.0
module load zlib/1.2.11_intel-18.0.0
#
# for gmt and gdal support
GDAL_HOME=/home/ncfs/asgs/gdal
export PATH=${GDAL_HOME}/bin:$PATH
export LD_LIBRARY_PATH=${GDAL_HOME}/lib:$LD_LIBRARY_PATH
export GDAL_DATA=${GDAL_HOME}/share/gdal
#
#
GMT_HOME=/home/ncfs/asgs/gmt/gmt-4.5.18
export PATH=${GMT_HOME}/bin:$PATH
export LD_LIBRARY_PATH=${GMT_HOME}/lib:$LD_LIBRARY_PATH
#
# for fftw
FFTW_HOME=/home/ncfs/asgs/fftw
export PATH=${FFTW_HOME}/bin:$PATH
export LD_LIBRARY_PATH=${FFTW_HOME}/lib:$LD_LIBRARY_PATH
#
#
# jgf20160215: Prevent git push from opening up a graphical dialog box to
# ask for a password; it will interactively ask for a password instead
unset SSH_ASKPASS

#Set PS1
PS1='\[\e[0;33m\][\u@\h \t \W]$\[\e[m\] '

alias llr='ls -alrt'

oldestFile()
{
find $1 -wholename "*" -type f -printf "%TY%Tm%Td %h/%f\n" | \
   awk 'BEGIN {cont=0; oldd=strftime("%Y%m%d"); } \
                 { if ($1 < oldd) { oldd=$1; oldf=$2; for(i=3; i<=NF; i++) oldf=oldf " " $i; }; count++; } \
                         END { print oldd; }'
}


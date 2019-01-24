# -*- shell-script -*-
# TACC startup script: ~/.bashrc version 2.1 -- 10.02.2015

# This file is NOT automatically sourced for login shells.
# Your ~/.profile can and should "source" this file.

# Note neither ~/.profile nor ~/.bashrc are sourced automatically by
# bash scripts. However, a script inherits the environment variables
# from its parent shell.  Both of these facts are standard bash
# behavior.
#
# In a parallel mpi job, this file (~/.bashrc) is sourced on every 
# node so it is important that actions here not tax the file system.
# Each nodes' environment during an MPI job has ENVIRONMENT set to
# "BATCH" and the prompt variable PS1 empty.

#################################################################
# Optional Startup Script tracking. Normally DBG_ECHO does nothing
if [ -n "$SHELL_STARTUP_DEBUG" ]; then
  DBG_ECHO "${DBG_INDENT}~/.bashrc{"
fi

############
# SECTION 1
#
# There are three independent and safe ways to modify the standard
# module setup. Below are three ways from the simplest to hardest.
#   a) Use "module save"  (see "module help" for details).
#   b) Place module commands in ~/.modules
#   c) Place module commands in this file inside the if block below.
#
# Note that you should only do one of the above.  You do not want
# to override the inherited module environment by having module
# commands outside of the if block[3].

if [ -z "$__BASHRC_SOURCED__" -a "$ENVIRONMENT" != BATCH ]; then
  export __BASHRC_SOURCED__=1

  ##################################################################
  # **** PLACE MODULE COMMANDS HERE and ONLY HERE.              ****
  ##################################################################

  # module load git

fi

############
# SECTION 2
#
# Please set or modify any environment variables inside the if block
# below.  For example, modifying PATH or other path like variables
# (e.g LD_LIBRARY_PATH), the guard variable (__PERSONAL_PATH___) 
# prevents your PATH from having duplicate directories on sub-shells.

if [ -z "$__PERSONAL_PATH__" ]; then
  export __PERSONAL_PATH__=1

  ###################################################################
  # **** PLACE Environment Variables including PATH here.        ****
  ###################################################################

  # export PATH=$HOME/bin:$PATH

fi

# alias m="more"
# alias bls='/bin/ls'   # handy alias for listing a large directory.

##########
# Umask
#
# If you are in a group that wishes to share files you can use 
# "umask". to make your files be group readable.  Placing umask here 
# is the only reliable place for bash and will insure that it is set 
# in all types of bash shells.

# umask 022

###################################
# Optional Startup Script tracking 

if [ -n "$SHELL_STARTUP_DEBUG" ]; then
  DBG_ECHO "${DBG_INDENT}}"
fi

#
# 
# gdal
GDAL_PATH="$WORK/asgs/gdal/bin"
GDAL_LD_LIBRARY_PATH="$WORK/asgs/gdal/lib"
export GDAL_DATA="$WORK/asgs/gdal/lib"
#
export PATH=$GDAL_PATH:$PATH
export LD_LIBRARY_PATH=$GDAL_LD_LIBRARY_PATH:$LD_LIBRARY_PATH
#
# gmt
export PATH=/work/00976/jgflemin/lonestar/asgs/gmt/gmt-4.5.18/bin:$PATH
#
# local perl modules
PATH="/home1/00976/jgflemin/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home1/00976/jgflemin/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home1/00976/jgflemin/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home1/00976/jgflemin/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home1/00976/jgflemin/perl5"; export PERL_MM_OPT;
#
# openssl
export PATH=/work/00976/jgflemin/lonestar/asgs/openssl/bin:$PATH
export LD_LIBRARY_PATH=/work/00976/jgflemin/lonestar/asgs/openssl/lib:$PATH
#
#
# @jasonfleming 20190122 : needed to compile adcirc
ml reset
ml netcdf nco

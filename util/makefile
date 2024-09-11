#--------------------------------------------------------------------------
# makefile
#
# A system to build ASGS-related utilities for processing ADCIRC output.
#
#--------------------------------------------------------------------------
# Copyright(C) 2014--2019 Jason Fleming
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
#--------------------------------------------------------------------------
#
# specify compiler=gfortran on the make command line
ifeq ($(compiler),gfortran)
   FC := gfortran
   FFLAGS := -cpp -ffree-line-length-none
   ifeq ($(DEBUG),full)
      FFLAGS := -cpp -ffree-line-length-none -g -O0 -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal #-Wall
   endif
endif
ifeq ($(compiler),intel)
   FC := ifort
   FFLAGS := -cpp -shared-intel
   ifeq ($(DEBUG),trace)
      FFLAGS := -g -O0 -fpp -traceback 
   endif
   ifeq ($(DEBUG),full)
      FFLAGS := -cpp -g -O0 -traceback -debug all -check all -ftrapuv -fpe0 -shared-intel -DDEBUGSEGFAULT
   endif
   # jgf20150616: Added to implement nld's -heap-arrays fix.
   ifeq ($(HEAP_ARRAYS),fix)
      FFLAGS := $(FFLAGS) -heap-arrays
   endif
endif
ifeq ($(compiler),intel-oneapi)
   FC := ifx
   FFLAGS := -cpp -shared-intel -fpp
   ifeq ($(DEBUG),trace)
      FFLAGS := -g -O0 -fpp -traceback 
   endif
   ifeq ($(DEBUG),full)
      FFLAGS := -cpp -g -O0 -traceback -debug all -check all -ftrapuv -fpe0 -shared-intel -DDEBUGSEGFAULT
   endif
   # jgf20150616: Added to implement nld's -heap-arrays fix.
   ifeq ($(HEAP_ARRAYS),fix)
      FFLAGS := $(FFLAGS) -heap-arrays
   endif
endif
ifeq ($(compiler),pgf90)
   FC := pgf90
   FFLAGS := -Mpreprocess -Mextend -Minform,inform -tp=shanghai-64 -fast
   ifeq ($(DEBUG),full)
      FFLAGS := -Mextend -g -O0 -traceback -Mbounds -Mchkfpstk -Mchkptr -Mchkstk -DALL_TRACE -DFLUSH_MESSAGES -DFULL_STACK -tp=shanghai-64 -DDEBUGSEGFAULT
   endif
endif
#
OUTPUTDIR := ../output
INCLUDES := -I. -I$(OUTPUTDIR)
LIBS := -L$(OUTPUTDIR)
VPATH = $(OUTPUTDIR)
#
# if netcdf libraries are available on the target platform, 
# use NETCDF=enable on the make command line
ifeq ($(NETCDF),enable)
   FFLAGS      := $(FFLAGS) -DASGSNETCDF
   NETCDFPATH  := /usr
   INCLUDES    := $(INCLUDES) -I. -I$(NETCDFPATH)/include
   LIBS        := $(LIBS) -L$(NETCDFPATH)/lib
   LDFLAGS     := $(LDFLAGS) -lnetcdff
endif
ifeq ($(NETCDF4),enable)
   FFLAGS      := $(FFLAGS) -DHAVE_NETCDF4
endif
ifeq ($(NETCDF4_COMPRESSION),enable)
   FFLAGS      := $(FFLAGS) -DNETCDF_CAN_DEFLATE
endif
#
#
OBJ := $(OUTPUTDIR)/adcmesh.o $(OUTPUTDIR)/asgsio.o $(OUTPUTDIR)/logging.o
MODS := adcmesh.mod asgsio.mod logging.mod
#
# targets
all : makeMax
#
clean :
	rm -f *.o *.mod *.x
#
makeMax : $(MODS) makeMax.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) $(LDFLAGS)
#
logging.mod : $(OUTPUTDIR)/logging.f90 
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c $(OUTPUTDIR)/logging.f90 $(LDFLAGS)
#
adcmesh.mod : $(OUTPUTDIR)/adcmesh.f90 asgsio.mod
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c $(OUTPUTDIR)/adcmesh.f90 $(LDFLAGS)
#
asgsio.mod : $(OUTPUTDIR)/asgsio.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c $(OUTPUTDIR)/asgsio.f90 $(LDFLAGS)

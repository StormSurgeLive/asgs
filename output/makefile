#--------------------------------------------------------------------------
# makefile
#
# A system to build ASGS-related utilities for processing ADCIRC output.
#
#--------------------------------------------------------------------------
# Copyright(C) 2014 Jason Fleming
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
#
# specify compiler=intel on the make command line
ifeq ($(compiler),intel)
   FC := ifort
   FFLAGS := -fpp
   ifeq ($(DEBUG),full)
      FFLAGS := -g -O0 -fpp -traceback -debug -check all
   endif
endif
#
INCLUDES := -I. 
#
# if netcdf libraries are available on the target platform, 
# use NETCDF=enable on the make command line
ifeq ($(NETCDF),enable)
   FFLAGS      := $(FFLAGS) -DASGSNETCDF
   LDFLAGS     := -lnetcdff
   ifeq ($(MACHINENAME),jason-desktop)
      NETCDFPATH  := /usr
   endif
   ifeq ($(MACHINENAME),hatteras)
      NETCDFPATH  := /usr/share/Modules/software/RHEL-6.5/netcdf/netcdf-4.1.3_intel-14.0.3
   endif
   FFLAGS      := $(FFLAGS) -DASGSNETCDF
   LDFLAGS     := -lnetcdff
   INCLUDES    := -I. -I$(NETCDFPATH)/include
   LIBS        := -L$(NETCDFPATH)/lib
endif
ifeq ($(NETCDF4),enable)
   FFLAGS      := $(FFLAGS) -DHAVE_NETCDF4
endif
ifeq ($(NETCDF4_COMPRESSION),enable)
   FFLAGS      := $(FFLAGS) -DNETCDF_CAN_DEFLATE
endif
#
#
OBJ := adcmesh.o asgsio.o adcircdata.o
MODS := adcmesh.mod asgsio.mod adcircdata.mod
#
# targets
all : adcirc2netcdf netcdf2adcirc makeMax generateXDMF pullStationTimeSeries 
#
clean :
	rm -f *.o *.mod *.x
#
adcirc2netcdf : $(MODS) adcirc2netcdf.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) $(LDFLAGS)
#
netcdf2adcirc : $(MODS) netcdf2adcirc.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) $(LDFLAGS)
#
asgsio.mod : asgsio.f90 
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c asgsio.f90 $(LDFLAGS)
#
adcmesh.mod : adcmesh.f90 asgsio.mod
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c adcmesh.f90 $(LDFLAGS)
#
adcircdata.mod : adcircdata.f90 adcmesh.mod asgsio.mod 
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c adcircdata.f90 $(LDFLAGS)
# 
makeMax : asgsio.mod adcmesh.mod adcircdata.mod ../util/makeMax.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o ../util/$@.x ../util/$@.f90 $(OBJ) $(LDFLAGS)
#
generateXDMF : adcmesh.mod asgsio.mod generateXDMF.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 adcmesh.o asgsio.o $(LDFLAGS)
#
pullStationTimeSeries : asgsio.mod adcmesh.mod adcircdata.mod pullStationTimeSeries.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) $(LDFLAGS)
#
wetDryCheck : adcmesh.mod asgsio.mod wetDryCheck.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 adcmesh.o asgsio.o $(LDFLAGS)

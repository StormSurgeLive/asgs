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
OUTPUTDIR := ../../output
INCLUDES := -I. -I$(OUTPUTDIR)
#
# if netcdf libraries are available on the target platform, 
# use NETCDF=enable on the make command line
ifeq ($(NETCDF),enable)
   FFLAGS      := $(FFLAGS) -DASGSNETCDF
   NETCDFPATH  := /usr
   #
   #   # jgf20150616: Added netcdf paths for stampede at TACC.
   ifeq ($(MACHINENAME),stampede) 
      NETCDFPATH := /opt/apps/intel13/netcdf/4.3.2/x86_64
   endif
   # jgf20150716: Added compiler flags for hatteras at RENCI.
   ifeq ($(MACHINENAME),hatteras)
       NETCDFPATH  := /usr/share/Modules/software/RHEL-6.5/netcdf/netcdf-4.1.3_intel-14.0.3
   endif
   # jgf20150720: Added compiler flags for queenbee at LONI.
   ifeq ($(MACHINENAME),queenbee)
      NETCDFPATH  := /usr/local/packages/netcdf/4.2.1.1/INTEL-140-MVAPICH2-2.0
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
OBJ := adcmesh.o asgsio.o landuse.o
MODS := adcmesh.mod asgsio.mod landuse.mod

#
# targets
all : tau0_gen mannings_n_finder surface_roughness submergence submergence_eddyvis convertna
#
clean :
	rm -f *.o *.mod *.x
#
nodalattr.mod : $(MODS) nodalattr.f90 asgsio.mod
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c nodalattr.f90 $(LDFLAGS)
#
convertna : $(MODS) convertna.f90 nodalattr.mod
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) nodalattr.o $(LDFLAGS)
#
submergence_eddyvis : $(MODS) submergence_eddyvis.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) $(LDFLAGS)
#
submergence : $(MODS) submergence.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) $(LDFLAGS)
#
surface_roughness : $(MODS) surface_roughness.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) $(LDFLAGS)
#
mannings_n_finder : $(MODS) mannings_n_finder.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) $(LDFLAGS)
#
tau0_gen : $(MODS) tau0_gen.f90
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o $@.x $@.f90 $(OBJ) $(LDFLAGS)
#
adcmesh.mod : $(OUTPUTDIR)/adcmesh.f90 asgsio.mod
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c $(OUTPUTDIR)/adcmesh.f90 $(LDFLAGS)
#
asgsio.mod : $(OUTPUTDIR)/asgsio.f90 
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c $(OUTPUTDIR)/asgsio.f90 $(LDFLAGS)
#
landuse.mod : landuse.f90 asgsio.mod
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -c landuse.f90 $(LDFLAGS)

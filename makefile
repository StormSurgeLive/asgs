#--------------------------------------------------------------------------
# makefile
#
# A system to build ASGS-related utilities.
#
#--------------------------------------------------------------------------
# Copyright(C) 2019 Jason Fleming
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

$(info Compiler: $(compiler))

ifeq ($(compiler),gfortran)
   override FC := gfortran
   override CC := gcc
   override COMP_SYS := gnu_linux
   FFLAGS := -ffree-line-length-none -ffixed-line-length-none
   ifeq ($(DEBUG),full)
      FFLAGS := -cpp -ffree-line-length-none -g -O0 -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal #-Wall
   endif
endif
#
# specify compiler=intel on the make command line
ifeq ($(compiler),intel)
   override FC := ifort
   override CC := icc
   override COMP_SYS := intel_linux
   FFLAGS := -132
   ifeq ($(DEBUG),full)
      FFLAGS := -g -O0 -fpp -traceback -debug -check all
   endif
endif
# specify compiler=intel on the make command line
ifeq ($(compiler),intel-oneapi)
   override FC := ifx
   override CC := icx
   override COMP_SYS := oneapi_linux
   FFLAGS := -132
   ifeq ($(DEBUG),full)
      FFLAGS := -g -O0 -fpp -traceback -debug -check all
   endif
endif

$(info FC: $(FC))

#
INCLUDES :=
#
OBJ :=
MODS :=
#
# targets
all : tide_fac.x awip_lambert_interp.x lambertInterpRamp.x wgrib2
#
clean : cleano cleanx
#
cleanx:
	rm -rf *.x tides/*.x bin/wgrib2 input/*.x ./grib2
#
cleano :
	rm -rf *.o *.mod tides/*.o tides/*.mod bin/wgrib2 input/*.o input/*.mod
#
tide_fac.x : tides/tide_fac.f
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o tides/tide_fac.x tides/tide_fac.f $(OBJ) $(LDFLAGS)
#
awip_lambert_interp.x : input/awip_lambert_interp.F
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o awip_lambert_interp.x input/awip_lambert_interp.F $(LDFLAGS)
#
lambertInterpRamp.x : input/lambertInterpRamp.f
	$(FC) $(FFLAGS) $(INCLUDES) $(LIBS) -o lambertInterpRamp.x input/lambertInterpRamp.f $(LDFLAGS)

wgrib2 : cleano
	cd grib2
	$(MAKE) -C grib2
	cp grib2/wgrib2/wgrib2 ./bin

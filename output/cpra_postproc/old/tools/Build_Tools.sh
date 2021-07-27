#!/bin/bash

NETCDFHOME=/usr/local/packages/netcdf/4.2.1.1/INTEL-140-MVAPICH2-2.0

echo "Building libraries..."
ifort -O2 -assume byterecl -D_NETCDF -I$NETCDFHOME/include -L$NETCDFHOME/lib -lnetcdf -lnetcdff genericModule.F90 -c
ifort -O2 -assume byterecl -D_NETCDF -I$NETCDFHOME/include -L$NETCDFHOME/lib -lnetcdf -lnetcdff kdtree2Module.F90 -c
ifort -O2 -assume byterecl -D_NETCDF -I$NETCDFHOME/include -L$NETCDFHOME/lib -lnetcdf -lnetcdff adcircModule.F90 -c
ifort -O2 -assume byterecl -D_NETCDF -I$NETCDFHOME/include -L$NETCDFHOME/lib -lnetcdf -lnetcdff imedsModule.F90 -c
ifort -O2 -assume byterecl -D_NETCDF -I$NETCDFHOME/include -L$NETCDFHOME/lib -lnetcdf -lnetcdff dateModule.F90 -c
ifort -O2 -assume byterecl -I$NETCDFHOME/include -L$NETCDFHOME/lib -lnetcdf -lnetcdff adcmesh.F90 -c

echo "Building netcdf2adcirc..."
ifort -O2 -D_NETCDF netcdf2adcirc.F90 adcmesh.o genericModule.o kdtree2Module.o adcircModule.o dateModule.o imedsModule.o -I$NETCDFHOME/include -L$NETCDFHOME/lib -lnetcdf -lnetcdff -o netcdf2adcirc.exe

echo "Building Fort61ToIMEDS..."
ifort -O2 Fort61ToIMEDS.F90 genericModule.o kdtree2Module.o adcircModule.o dateModule.o imedsModule.o -I$NETCDFHOME/include -L$NETCDFHOME/lib -lnetcdf -lnetcdff -o Fort61ToIMEDS.exe

!--------------------------------------------------------------------------
! adcircMaxColumnar.f90
!
! A program to reformat ADCIRC min/max data into columns with long, 
! lat, max, total depth.
!
!--------------------------------------------------------------------------
! Copyright(C) 2012 Jason Fleming
!
! This file is part of the ADCIRC Surge Guidance System (ASGS).
!
! The ASGS is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! ASGS is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
!--------------------------------------------------------------------------

! adcircMaxColumnar.f90
! 
! Copyright(C) 2012 Jason Fleming
!
! Example of compiling adcircMaxColumnar.f90 with gfortran:
! gfortran -o adcircMaxColumnar.x -cpp -ffree-form -ffree-line-length-none -I. adcircMaxColumnar.f90 
! 
! Example of compiling adcircMaxColumnar.f90 with ifort on blueridge:
! ifort -o adcircMaxColumnar.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -i-dynamic -I/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.1.2-gcc4.1-ifort/include -L/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.1.2-gcc4.1-ifort/lib adcircMaxColumnar.f90 -lnetcdf -lnetcdff -lz

include 'adcmesh.f90'

   program adcircMaxColumnar

      USE adcmesh
      USE netcdf
      IMPLICIT NONE
      CHARACTER(120)                :: InputFile
      character(1000)               :: Line
      DOUBLE PRECISION              :: DefaultValue, FillValue=-99999.0d0
      DOUBLE PRECISION, ALLOCATABLE :: Global1(:), Global2(:), Global3(:)
      integer                       :: natt, i, j, k, N, SS, NumNodes, NumSnaps
      integer                       :: nopt         ! actual number of files to convert
      integer                       :: menuOpt   ! user's selection
      integer                       :: argcount  ! number of command line arguments
      integer                       :: iargc     ! function to return command line arguments
      character(2048)               :: cmdlineopt ! command line option
      character(2048)               :: cmdlinearg ! content of command line argument
                                                  ! files formatted in hdf5 format
      character(80) :: dummy
      real(8) :: dataval
      integer :: nodenum
      !
      ! initializations   
      meshFileName = "null"
      !
      ! Report netcdf version
      write(6,*) "INFO: adcircMaxColumnar was compiled with the following netcdf library: ",trim(nf90_inq_libvers())

      argcount = iargc() ! count up command line options
      if (argcount.gt.0) then
         i=0
         do while (i.lt.argcount)
            i = i + 1
            call getarg(i, cmdlineopt)
            select case(trim(cmdlineopt))
               case("--meshfile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  meshFileName = trim(cmdlinearg)
               case("--datafile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  InputFile = trim(cmdlinearg)
               case default
                  write(6,*) "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
            end select
         end do
      end if

      ! Load fort.14
      call read14()

      ! load ascii maxele.63
      call openFileForRead(63, trim(InputFile))
      read(63,*) dummy  ! header 1
      read(63,*) dummy  ! header 2
      read(63,*) dummy  ! time and timestep
      allocate(global1(np))
      do i=1,np
         read(63,*) nodeNum, dataval
         global1(nodeNum) = dataval
      end do
      open(11,file='maxele.txt',status='replace',action='write')
      write(11,*) '-----------------------------------------------------'
      write(11,*) 'Longitude    Latitude    Max Water     Total Water'
      write(11,*) ' (decimal     (decimal    Elevation     Depth'
      write(11,*) '  degrees)    degrees)    (ft NAVD88)   (ft NAVD88)'
      write(11,*) '-----------------------------------------------------'
      do i=1,np
         if (global1(i).gt.-9999.d0) then
            write(11,11) xyd(1,i),xyd(2,i),global1(i)*3.2808399,(global1(i)+xyd(3,i))*3.2808399
         endif
      end do
11    format(2(F10.6,'  '),2(E10.5,'  '))
      close(11)
      stop
      end program adcircMaxColumnar

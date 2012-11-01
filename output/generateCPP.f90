!--------------------------------------------------------------------------
! generateCPP.f90
!
! A program to precompute the CPP (carte parallelogrammatique projection)
! and adding it to a NetCDF data file that contains an ADCIRC mesh.
! This projection is useful in visualization.
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
! Example of compiling this code with g95:
!
! g95 -o generateCPP.x -cpp -ffree-form -ffree-line-length-huge -I/usr/local/netcdf/netcdf-4.1.1/f90 generateCPP.f90 -L/usr/local/hdf5/hdf5-1.8.8/hdf5/lib -lnetcdf -lhdf5_hl -lhdf5 -lhdf5_fortran -lz
!
! Example of compiling generateCPP.f90 with pgf90:
! pgf90 -o generateCPP.x -Mpreprocess -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/opt/cray/netcdf/4.1.3/pgi/109/include generateCPP.f90 -L/opt/cray/netcdf/4.1.3/pgi/109/lib  -lnetcdf -lnetcdff
!
! Example of compiling with gfortran:
! gfortran -cpp -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -o generateCPP.x -ffree-form -ffree-line-length-none -I/usr/include generateCPP.f90 -lnetcdf -lnetcdff -lz

      include 'adcmesh.f90'

      program generateCPP
      use netcdf
      use adcmesh
      implicit none

      integer :: iargc
      integer :: nc_id ! netcdf ID of the open file containing the mesh
      integer :: NC_Count(2)
      integer :: NC_Start(2)
      integer :: NC_VarID_x_cpp
      integer :: NC_VarID_y_cpp
      integer :: nvar ! number of variables in the netcdf file
      real(8), allocatable :: x_cpp(:)
      real(8), allocatable :: y_cpp(:)
      character(NF90_MAX_NAME) :: varname
      logical :: foundCPP
      integer argcount
      character(1024) :: datafile
      character(1024) :: cmdlineopt
      character(1024) :: cmdlinearg
      logical :: fileFound
      integer :: i  ! loop counter

      deg2rad = 2*pi/360.0
      fileFound = .false.
      argcount = iargc() ! count up command line options
      if (argcount.gt.0) then
         i=0
         do while (i.lt.argcount)
            i = i + 1
            call getarg(i, cmdlineopt)
            select case(trim(cmdlineopt))
               case("--datafile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  datafile = trim(cmdlinearg)
               case("--cpp")
                  write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
                  projectCPP = .true.
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  read(cmdlinearg,*) slam0
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  read(cmdlinearg,*) sfea0
                  write(6,*) "INFO: slam0=",slam0," and sfea0=",sfea0,"."
               case default
                  write(6,*) "WARNING: The command line option ",trim(cmdlineopt)," was not recognized."
            end select
         end do
      end if
!
!     Check to see if file exists
      inquire(FILE=trim(datafile),EXIST=fileFound)
      if (fileFound.eqv..false.) then
         write(6,'("ERROR: The file ",A," was not found.")') trim(datafile)
         stop
      else
         ! netcdf file exists; open it
         call check(nf90_open(trim(datafile), NF90_WRITE, nc_id))
      endif

      ! determine the number of nodes
      call check(nf90_inq_dimid(nc_id, "node", NC_DimID_node))
      call check(nf90_inquire_dimension(nc_id, NC_DimID_node, len=np))
      allocate(xyd(3,np))
      ! get the existing x and y coordinates of the nodes (lon and lat degrees)
      call check(nf90_inq_varid(nc_id, "x", NC_VarID_x))
      call check(nf90_inq_varid(nc_id, "y", NC_VarID_y))
      !NC_Count = (/ np, 1 /)
      !NC_Start = (/ 1, 1 /)
      call check(nf90_get_var(nc_id, NC_VarID_x, xyd(1,:)))
      call check(nf90_get_var(nc_id, NC_VarID_y, xyd(2,:)))
      !
      ! check to see if we have already created netcdf variables for the
      ! CPP coordinates
      call check(nf90_inquire(nc_id,nVariables=nvar))
      foundCPP = .false.
      do i=1,nvar
         call check(nf90_inquire_variable(nc_id, i, name=varname))
         if ( trim(varname).eq."x_cpp" ) then
            foundCPP = .true.
            write(6,'("INFO: CPP coordinates are already present in the file. They will be updated.")')
            NC_VarID_x_cpp = i
            call check(nf90_inq_varid(nc_id, "y_cpp", NC_VarID_y_cpp))
            exit
         endif
      end do
      ! if we didn't find the cpp coordinate variables, create them
      if ( foundCPP.eqv..false. ) then
         write(6,'("INFO: CPP coordinates were not present in the file. They will be created.")')
         call check(nf90_redef(nc_id))
         call check(nf90_def_var(nc_id, "x_cpp", NF90_DOUBLE, NC_DimID_node, NC_VarID_x_cpp))
         call check(nf90_def_var(nc_id, "y_cpp", NF90_DOUBLE, NC_DimID_node, NC_VarID_y_cpp))
#ifdef HAVE_NETCDF4
#ifdef NETCDF_CAN_DEFLATE
         call check(nf90_def_var_deflate(nc_id, NC_VarID_x_cpp, 1, 1, 2))
         call check(nf90_def_var_deflate(nc_id, NC_VarID_y_cpp, 1, 1, 2))
#endif
#endif
         call check(nf90_enddef(nc_id))
      endif
      !
      ! compute the CPP
      allocate(x_cpp(np),y_cpp(np))
      write(6,'("INFO: Generating CPP coordinates and adding them to the NetCDF file.")')
      x_cpp = R * (xyd(1,:)*deg2rad - slam0*deg2rad) * cos(sfea0*deg2rad)
      y_cpp = xyd(2,:)*deg2rad * R
      !
      ! write the projected coordinates to the file
      call check(nf90_put_var(nc_id, NC_VarID_x_cpp, x_cpp))
      call check(nf90_put_var(nc_id, NC_VarID_y_cpp, y_cpp))
      !
      ! clean up
      deallocate(x_cpp, y_cpp)
      write(6,'("INFO: Finished generating CPP coordinates. Variable names are x_cpp and y_cpp.")')
!----------------------------------------------------------------------
      end program generateCPP
!----------------------------------------------------------------------

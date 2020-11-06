!--------------------------------------------------------------------------
! generateCPP.f90
!
! A program to precompute the CPP (carte parallelogrammatique projection)
! and adding it to a NetCDF data file that contains an ADCIRC mesh.
! This projection is useful in visualization.
!
!--------------------------------------------------------------------------
! Copyright(C) 2012--2017 Jason Fleming
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
program generateCPP
use netcdf
use adcmesh
use asgsio
use ioutil
use logging
implicit none
type(mesh_t) :: m
type(meshNetCDF_t) :: n
integer :: NC_Count(2)
integer :: NC_Start(2)
character(len=NF90_MAX_NAME) :: varname
type(fileMetaData_t) :: fn ! netcdf file 
logical :: foundCPP
logical :: projectCPP
logical :: fileFound
integer :: errorIO
integer :: i  ! loop counter
!
! initializations
if (loggingInitialized.eqv..false.) then
   call initLogging(availableUnitNumber(),'generateCPP.f90')
endif

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
            fn%dataFileName = trim(cmdlinearg)
         case("--cpp")
            write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
            projectCPP = .true.
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) m%slam0
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) m%sfea0
            write(6,*) "INFO: slam0=",m%slam0," and sfea0=",m%sfea0,"."
         case default
            write(6,*) "WARNING: The command line option ",trim(cmdlineopt)," was not recognized."
      end select
   end do
end if
!
! Check to see if file exists
call checkFileExistence(fn%dataFileName, errorIO)
! netcdf file exists
call determineNetCDFFileCharacteristics(fn, m, n)
m%meshFileName = trim(adjustl(fn%dataFileName))
fn%dataFileFormat = NETCDF4
call findMeshDimsNetCDF(m, n)
call readMeshNetCDF(m, n)
!
! check to see if we have already created netcdf variables for the
! CPP coordinates
call check(nf90_open(trim(fn%dataFileName), NF90_WRITE, fn%nc_id))
call check(nf90_inquire(fn%nc_id,nVariables=fn%nvar))
foundCPP = .false.
do i=1,fn%nvar
   call check(nf90_inquire_variable(fn%nc_id, i, name=varname))
   if ( trim(varname).eq."x_cpp" ) then
      foundCPP = .true.
      write(6,'("INFO: CPP coordinates are already present in the file. They will be updated.")')
      n%NC_VarID_x_cpp = i
      call check(nf90_inq_varid(fn%nc_id, "y_cpp", n%NC_VarID_y_cpp))
      exit
   endif
end do
! if we didn't find the cpp coordinate variables, create them
if ( foundCPP.eqv..false. ) then
   write(6,'("INFO: CPP coordinates were not present in the file. They will be created.")')
   call check(nf90_redef(fn%nc_id))
   call check(nf90_def_var(fn%nc_id, "x_cpp", NF90_DOUBLE, n%NC_DimID_node, n%NC_VarID_x_cpp))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_x_cpp,'long_name','longitude'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_x_cpp,'standard_name','longitude'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_x_cpp,'units','cpp'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_x_cpp,'positive','east'))
   call check(nf90_def_var(fn%nc_id, "y_cpp", NF90_DOUBLE, n%NC_DimID_node, n%NC_VarID_y_cpp))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_y_cpp,'long_name','latitude'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_y_cpp,'standard_name','latitude'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_y_cpp,'units','cpp'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_y_cpp,'positive','north'))
#ifdef HAVE_NETCDF4
#ifdef NETCDF_CAN_DEFLATE
         call check(nf90_def_var_deflate(fn%nc_id, n%NC_VarID_x_cpp, 1, 1, 2))
         call check(nf90_def_var_deflate(fn%nc_id, n%NC_VarID_y_cpp, 1, 1, 2))
#endif
#endif
   call check(nf90_enddef(fn%nc_id))
endif
!
! compute the projection to cartesian coordinates
call computeCPP(m)
!
! write the projected coordinates to the file
write(6,'("INFO: Adding CPP coordinates to the NetCDF file.")')
call check(nf90_put_var(fn%nc_id, n%NC_VarID_x_cpp, m%x_cpp))
call check(nf90_put_var(fn%nc_id, n%NC_VarID_y_cpp, m%y_cpp))
!
! clean up
deallocate(m%x_cpp, m%y_cpp)
write(6,'("INFO: Finished generating CPP coordinates. Variable names are x_cpp and y_cpp.")')
!----------------------------------------------------------------------
end program generateCPP
!----------------------------------------------------------------------

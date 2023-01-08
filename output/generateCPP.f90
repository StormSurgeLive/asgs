!--------------------------------------------------------------------------
! generateCPP.f90
!
! Computes coordinates for points in geographic projection in
! netcdf file. The CPP (carte parallelogrammatique projection)
! and cartesian unit sphere are both available.
! These coordinates are useful for visualization.
!
!--------------------------------------------------------------------------
! Copyright(C) 2012--2022 Jason Fleming
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
logical :: foundCartesianSphere
logical :: projectCartesianSphere
logical :: fileFound
integer :: errorIO
integer :: i  ! loop counter
!
! initializations
if (loggingInitialized.eqv..false.) then
   call initLogging(availableUnitNumber(),'generateCPP.f90')
endif
projectCPP = .false.
projectCartesianSphere = .false.
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
         case("--cartesian-sphere")
            write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
            projectCartesianSphere = .true.
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
! CPP and cartesian sphere coordinates
call check(nf90_open(trim(fn%dataFileName), NF90_WRITE, fn%nc_id))
call check(nf90_inquire(fn%nc_id,nVariables=fn%nvar))
foundCPP = .false.
foundCartesianSphere = .false.
do i=1,fn%nvar
   call check(nf90_inquire_variable(fn%nc_id, i, name=varname))
   if ( trim(varname).eq."x_cpp" ) then
      foundCPP = .true.
      write(6,'("INFO: CPP coordinates are already present in the file.")')
      n%NC_VarID_x_cpp = i
      call check(nf90_inq_varid(fn%nc_id, "y_cpp", n%NC_VarID_y_cpp))
   endif
   if ( trim(varname).eq."x-cartesian-sphere" ) then
      foundCartesianSphere = .true.
      write(6,'("INFO: Cartesian sphere coordinates are already present in the file.")')
      n%NC_VarID_x_cartesian_sphere = i
      call check(nf90_inq_varid(fn%nc_id, "y-cartesian-sphere", n%NC_VarID_y_cartesian_sphere))
      call check(nf90_inq_varid(fn%nc_id, "z-cartesian-sphere", n%NC_VarID_z_cartesian_sphere))
   endif
end do
! if we didn't find the CPP coordinate variables, create them
if ( (foundCPP.eqv..false.) .and. (projectCPP.eqv..true.) ) then
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
! if we didn't find the cartesian sphere coordinate variables, create them
if ( (foundCartesianSphere.eqv..false.) .and. (projectCartesianSphere.eqv..true.) ) then
   write(6,'("INFO: Cartesian sphere coordinates were not present in the file. They will be created.")')
   call check(nf90_redef(fn%nc_id))
   call check(nf90_def_var(fn%nc_id, "x-cartesian-sphere", NF90_DOUBLE, n%NC_DimID_node, n%NC_VarID_x_cartesian_sphere))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_x_cartesian_sphere,'long_name','x cartesian coordinate on a unit sphere'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_x_cartesian_sphere,'standard_name','x'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_x_cartesian_sphere,'units','earth radius normalized to unity'))
   call check(nf90_def_var(fn%nc_id, "y-cartesian-sphere", NF90_DOUBLE, n%NC_DimID_node, n%NC_VarID_y_cartesian_sphere))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_y_cartesian_sphere,'long_name','y cartesian coordinate on a unit sphere'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_y_cartesian_sphere,'standard_name','y'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_y_cartesian_sphere,'units','earth radius normalized to unity'))
   call check(nf90_def_var(fn%nc_id, "z-cartesian-sphere", NF90_DOUBLE, n%NC_DimID_node, n%NC_VarID_z_cartesian_sphere))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_z_cartesian_sphere,'long_name','z cartesian coordinate on a unit sphere'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_z_cartesian_sphere,'standard_name','z'))
   call check(nf90_put_att(fn%nc_id,n%nc_varid_z_cartesian_sphere,'units','earth radius normalized to unity'))
#ifdef HAVE_NETCDF4
#ifdef NETCDF_CAN_DEFLATE
   call check(nf90_def_var_deflate(fn%nc_id, n%NC_VarID_x_cartesian_sphere, 1, 1, 2))
   call check(nf90_def_var_deflate(fn%nc_id, n%NC_VarID_y_cartesian_sphere, 1, 1, 2))
   call check(nf90_def_var_deflate(fn%nc_id, n%NC_VarID_z_cartesian_sphere, 1, 1, 2))
#endif
#endif
   call check(nf90_enddef(fn%nc_id))
endif
!
if ( projectCPP.eqv..true. ) then
   !
   ! compute the projection to cpp coordinates
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
endif
!
if ( projectCartesianSphere.eqv..true. ) then
   !
   ! compute the projection to unit sphere in cartesian coordinates
   call computeCartesianSphere(m)
   !
   ! write the projected coordinates to the file
   write(6,'("INFO: Adding cartesian unit sphere coordinates to the NetCDF file.")')
   call check(nf90_put_var(fn%nc_id, n%NC_VarID_x_cartesian_sphere, m%x_cartesian_sphere))
   call check(nf90_put_var(fn%nc_id, n%NC_VarID_y_cartesian_sphere, m%y_cartesian_sphere))
   call check(nf90_put_var(fn%nc_id, n%NC_VarID_z_cartesian_sphere, m%z_cartesian_sphere))
   !
   ! clean up
   deallocate(m%x_cartesian_sphere, m%y_cartesian_sphere, m%z_cartesian_sphere)
   write(6,'("INFO: Finished generating CPP coordinates. Variable names are x_cartesian_sphere, y_cartesian_sphere, and z_cartesian_sphere.")')
endif


!----------------------------------------------------------------------
end program generateCPP
!----------------------------------------------------------------------

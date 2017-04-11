!--------------------------------------------------------------------------
! diffAdcircNetcdf.f90
!
! A program to find the numerical difference(s) between two ADCIRC 
! netCDF output files and writing the result to a third netCDF file. 
! jgf20150723: Initially limited to finding the absolute differences
! between two maxele.63.nc files.
!--------------------------------------------------------------------------
! Copyright(C) 2015 Jason Fleming
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
! Compile with accompanying makefile.
!--------------------------------------------------------------------------
!
program diffAdcircNetCDF
use netcdf
use asgsio
use adcmesh
use adcircdata
implicit none
integer :: ag
integer :: agnew
integer :: agold
integer :: i, j, k, m, n
integer :: ncstatus
character(len=1024) :: goldfile ! first file; possibly the expected output
character(len=1024) :: testfile ! second file; possibly the actual output
character(len=1024) :: resultfile ! difference between testfile and goldfile

! result = testfile - goldfile so that the result will be negative 
! where the testfile is smaller than the goldfile.  

! TODO: Implement the finding of relative differences, using the gold
! file for scaling.

stationfile = .false.
extremesWithTime = .false.
agrid = 'null'

write(6,'(a)') 'INFO: diffAdcircNetCDF.x compiled with the following netcdf library: '//trim(nf90_inq_libvers())

argcount = iargc() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
         case("--goldfile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ", &
               trim(cmdlinearg),"."
            goldfile = trim(cmdlinearg)
         case("--testfile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ", &
               trim(cmdlinearg),"."
            testfile = trim(cmdlinearg)
         case("--resultfile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ", &
               trim(cmdlinearg),"."
            resultfile = trim(cmdlinearg)
         case default
            write(6,'(a,a,a)') "WARNING: Command line option '", &
               TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
end if
!
! jgf20150723: UNFORTUNATELY ASSUMING WE HAVE TWO MAXELE.63.NC FILES !FIXME
! 
! Determine the mesh dimensions in the gold file.
call findMeshDimsNetCDF(goldfile)
call readMeshNetCDF(goldfile)
!
! open the gold netcdf file and determine data characteristics
call determineNetCDFFileCharacteristics(goldfile)
! load the gold data
allocate(gold_data(np,num_components))
nc_start = (/ 1, 1 /)
nc_count = (/ np, 1 /)
call check(nf90_get_var(nc_id,nc_varid(1),gold_data(:,1),nc_start,nc_count))
! close the gold file
call check(nf90_close(nc_id))
!
! open the test netcdf file
call determineNetCDFFileCharacteristics(testfile)
! load the test data
allocate(test_data(np,num_components))
call check(nf90_get_var(nc_id,nc_varid(1),test_data(:,1),nc_start,nc_count))
! close the test file
call check(nf90_close(nc_id))
!
! Calculate the result
allocate(result_data(np,num_components))
result_data = test_data - gold_data
!
! Create a new netcdf file and write the result array. 
!
! create netcdf file
write(6,'(a,a,a)') "INFO: Creating NetCDF file '"//trim(resultfile)//"'."
ncFileType = NF90_CLOBBER ! netcdf3 format, netcdf classic model
#ifdef HAVE_NETCDF4
if (useNetCDF4.eqv..true.) then
   ncFileType = ior(NF90_HDF5,NF90_CLASSIC_MODEL) ! netcdf4 (i.e., hdf5) format, netcdf classic model
endif
#endif
CALL Check(nf90_create(trim(resultfile),ncFileType,nc_id))
!
! create time dimension and create global attributes
CALL Check(NF90_DEF_DIM(NC_ID,'time',NF90_UNLIMITED,NC_DimID_time))
CALL Check(NF90_DEF_VAR(NC_ID,'time',NF90_DOUBLE,NC_DimID_time,NC_VarID_time))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'long_name','model time'))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'standard_name','time'))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'units',trim(datenum)))
!
! write the mesh definitions to the netcdf file 
call writeMeshDefinitionsToNetCDF(nc_id, useNetCDF4)
! create adcirc output variables and associated attributes
NC_DimID = (/ NC_DimID_node, NC_DimID_Time /)
!
! !FIXME! should change the name of the variable to reflect the 
! fact that the quantity represents a difference
CALL Check(NF90_DEF_VAR(NC_ID,'zeta_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_maxele))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'_FillValue',FillValue))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'long_name','maximum sea surface elevation above datum'))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'standard_name','maximum_sea_surface_elevation_above_datum'))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'coordinates','y x'))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'location','node'))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'mesh','adcirc_mesh'))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'units','m'))
!
! end variable and attributes definitions
CALL Check(NF90_ENDDEF(NC_ID))
!
! write the result data
call check(nf90_put_var(nc_id,nc_varid_maxele,result_data,nc_start,nc_count))
! close the file
call check(nf90_close(nc_id))

!---------------------------------------------------------------------
end program diffAdcircNetCDF
!---------------------------------------------------------------------

!----------------------------------------------------------------------
!  CHECK
!---------------------------------------------------------------------
SUBROUTINE Check(ncStatus)

USE netcdf
IMPLICIT NONE
INTEGER,INTENT(IN) :: ncStatus
IF(ncStatus.NE.NF90_NOERR)THEN
   WRITE(*,'(A,A)') "ERROR: NetCDF: ",TRIM(NF90_STRERROR(ncStatus))
   STOP
ENDIF
!---------------------------------------------------------------------
END SUBROUTINE check
!---------------------------------------------------------------------



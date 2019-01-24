!--------------------------------------------------------------------------
! collectMinMax.f90
!
! A program to collect min/max values from a source file and update
! a destination file to reflect the overall min or max from both.
!--------------------------------------------------------------------------
! Copyright(C) 2017 Jason Fleming
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
program collectMinMax
use netcdf
use asgsio
use adcmesh
use logging
use ioutil
implicit none
logical :: meshonly
integer :: ncstatus
type(mesh_t) :: sm ! source mesh to operate on, must be same as dest mesh
type(mesh_t) :: dm ! dest mesh to operate on, must be same as source mesh
type(meshNetCDF_t) :: sn ! source mesh netcdf IDs
type(meshNetCDF_t) :: dn ! dest mesh netcdf IDs
type(fileMetaData_t) :: sf ! source netcdf file to read data from 
type(fileMetaData_t) :: df ! destination netcdf file to be updated
integer :: ssnapi ! time step 
integer :: dsnapi ! time step 
real(8) :: ssnapr ! time (s)
real(8) :: dsnapr ! time (s)
integer :: lineNum
integer :: i
integer :: l ! ascii line number, not used
integer :: nc_start(2)
integer :: nc_count(2)

call initLogging(availableUnitNumber(),'collectMinMax.f90')
call allmessage(INFO,'Compiled with netcdf library version '//trim(nf90_inq_libvers())//'.')
!
! initializations
dm%is3D = .false.
sm%is3D = .false.
!
argcount = command_argument_count() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
         case("--source")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ", &
               trim(cmdlinearg),"."
            sf%dataFileName = trim(cmdlinearg)
         case("--destination")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ", &
               trim(cmdlinearg),"."
            df%dataFileName = trim(cmdlinearg)
         case default
            write(6,'(a,a,a)') "WARNING: Command line option '", &
               TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
end if
!
! determine type of data, etc 
call determineNetCDFFileCharacteristics(df, dm, dn)
call determineNetCDFFileCharacteristics(sf, sm, sn)
!
! allocate memory to hold single dataset from each netcdf file
sf%dataFileFormat = NETCDFG
call allocateDataSetMemory(sf, sm)
df%dataFileFormat = NETCDFG
call allocateDataSetMemory(df, dm)
!
! open the source netcdf file
call check(nf90_open(trim(sf%dataFileName), NF90_NOWRITE, sf%nc_id))
! open the destination netcdf file
call check(nf90_open(trim(df%dataFileName), NF90_WRITE, df%nc_id))
!
! read the data set from each file
call allMessage(INFO,'Reading one dataset from source file.')
call readOneDataSet(sf, sm, 1, l, ssnapr, ssnapi)
call allMessage(INFO,'Reading one dataset from destination file.')
call readOneDataSet(df, dm, 1, l, dsnapr, dsnapi)
select case (trim(df%defaultFileName))
case('minpr.63')
   do i=1, sm%np
      if ( sf%ncds(1)%rdata(i).lt.df%ncds(1)%rdata(i) ) then
         df%ncds(1)%rdata(i) = sf%ncds(1)%rdata(i)
      endif
   end do
case default  
   do i=1, sm%np
      if ( sf%ncds(1)%rdata(i).gt.df%ncds(1)%rdata(i) ) then
         df%ncds(1)%rdata(i) = sf%ncds(1)%rdata(i)
      endif
   end do
end select

nc_count = (/ df%numValuesPerDataset, 1 /)
nc_start = (/ 1, 1 /) 

call check(nf90_put_var(df%nc_id,df%ncds(1)%nc_varID,df%ncds(1)%rdata,nc_start,nc_count))


!---------------------------------------------------------------------
end program collectMinMax
!---------------------------------------------------------------------


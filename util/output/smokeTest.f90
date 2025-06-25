!-----------------------------------------------------------------------
! smokeTest: Make basic sanity checks on netcdf output files to look
! for obvious issues
!-----------------------------------------------------------------------
! Copyright(C) 2025 Jason Fleming
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
!-----------------------------------------------------------------------
program smokeTest
use iso_fortran_env, only : output_unit, error_unit
use asgsio
use adcmesh
use logging
use ioutil
implicit none
type(mesh_t) :: m
type(meshNetCDF_t) :: n
type(fileMetaData_t) :: ft ! netcdf file to be smoke tested
real(8) :: overallMin
real(8) :: overallMax
real(8) :: overallMean
real(8) :: totalMissing
real(8), allocatable :: arrayMins(:)
real(8), allocatable :: arrayMaxes(:)
real(8), allocatable :: arrayMeans(:)
real(8), allocatable :: arrayStandardDeviations(:)
integer, allocatable :: arrayNumMissing(:)
real(8), allocatable :: adcirc_data(:)
real(8), allocatable :: squares(:)
character(NF90_MAX_NAME) :: varname
logical :: countNegativeTopo = .false. ! .true. if only the number of nodes with negative topo should be counted
integer :: negativeTopoCount = 0
real(8) :: sum_squares   ! sum of the squares of the absolute differences for use in stdev calc
integer :: nc_start(2)
integer :: nc_count(2)
integer :: topo_start(1)
integer :: topo_count(1)
integer :: varid
integer :: errorIO
integer :: i, j, k  ! loop counters
!
! process command line options
if (loggingInitialized.eqv..false.) then
    call initLogging(availableUnitNumber(),'smokeTest.f90')
endif
argcount = command_argument_count() ! count up command line options
write(error_unit,'(a,i0,a)') 'INFO: There are ',argcount,' command line options.'
ft%dataFileName = "null"
varname = "null"
i=0
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--datafile")
      i = i + 1
      call getarg(i, cmdlinearg)
      ft%dataFileName = trim(cmdlinearg)
   case("--varname")
      i = i + 1
      call getarg(i, cmdlinearg)
      varname = trim(cmdlinearg)
   case("--count-negative-topo")
      countNegativeTopo = .true.
   case default
      write(error_unit,*) "ERROR: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
      error stop
   end select
end do
!
!
if ( trim(ft%dataFileName).eq.'null') then
    write(error_unit,*) "ERROR: The --datafile must be specified on the command line."
    error stop
endif
!
! open time varying data file and extract info
call determineNetCDFFileCharacteristics(ft, m, n)
allocate(adcirc_data(m%np))
!
write(error_unit,'(a,i0,a)') 'INFO: There are ',ft%nSnaps,' datasets in the file.'
call check(nf90_open(trim(ft%dataFileName), NF90_NOWRITE, ft%nc_id))
!
if (countNegativeTopo.eqv..true.) then
    write(error_unit,'(a,i0,a)') 'INFO: Counting the number of nodes with negative topo values.'
    varname = "depth"
    topo_start = (/ 1 /)
    topo_count = (/ m%np /)
    call check(nf90_inq_varid(ft%nc_id, trim(varname), varid))
    call check(nf90_get_var(ft%nc_id, varid, adcirc_data, topo_start, topo_count))
    negativeTopoCount = count(adcirc_data.lt.0.d0)
    write(output_unit,'(i0)') negativeTopoCount
    write(error_unit,'(a,i0,a)') 'INFO: Finished counting the number of nodes with negative topo values.'
    stop
endif
!
!
if ( trim(varname).eq.'null' ) then
    write(error_unit,*) "ERROR: The --varname must be specified on the command line."
    error stop
endif
!
allocate(arrayMins(ft%nSnaps))
allocate(arrayMaxes(ft%nSnaps))
allocate(arrayMeans(ft%nSnaps))
allocate(arrayStandardDeviations(ft%nSnaps))
allocate(arrayNumMissing(ft%nSnaps))
allocate(squares(m%np))
!
write(error_unit,'(a)') 'INFO: Compiling a record of values across all data sets.'
call check(nf90_inq_varid(ft%nc_id, trim(varname), varid))
nc_start = (/ 1, i /)
nc_count = (/ m%np, 1 /)
! loop over datasets
do i=1,ft%nSnaps
    !
    ! read the dataset from netcdf
     call check(nf90_get_var(ft%nc_id, varid, adcirc_data, nc_start, nc_count))
    ! check to see if each value exceeds the recorded extreme value
    ! at that node
    arrayMins(i) = minval(adcirc_data,adcirc_data.gt.-99998.d0)
    arrayMaxes(i) = maxval(adcirc_data)
    arrayNumMissing(i) = count(adcirc_data.lt.-99998.d0)
    ! compute array mean (may be less accurate for large arrays)
    arrayMeans(i) = sum(adcirc_data,adcirc_data.gt.-99998.d0) / dble(m%np - arrayNumMissing(i))
    ! compute array standard deviation
    arrayStandardDeviations(i) = 0.d0
    where (adcirc_data.gt.-99998.d0)
        squares = ( abs(adcirc_data - arrayMeans(i)) )**2
    end where
    sum_squares = sum(squares)
    arrayStandardDeviations(i) = sqrt( sum_squares / dble(m%np - arrayNumMissing(i)) )
end do
call check(nf90_close(ft%nc_id))
write(error_unit,'(/,a)') 'INFO: Finished building min/max/missing arrays dataset.'
!
overallMin = minval(arrayMins)
overallMax = maxval(arrayMaxes)
overallMean = sum(arrayMeans) / dble(ft%nSnaps)
totalMissing = sum(arrayNumMissing)
!
! write summary statistics to stdout
do i=1,ft%nSnaps
    write(output_unit,'(f15.7,1x,f15.7,1x,f15.7,1x,f15.7,1x,i0)') arrayMins(i),arrayMaxes(i),arrayMeans(i),arrayStandardDeviations(i),arrayNumMissing(i)
end do
write(error_unit,'(/,a)') 'INFO: Finished writing min/max/missing arrays dataset to stdout.'
!-----------------------------------------------------------------------
end program smokeTest
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! makeMax: takes a time dependent adcirc ascii output file and 
! determines the max value at every node ... e.g., takes a fort.63
! and generates a file like maxele.63.
!-----------------------------------------------------------------------
! Copyright(C) 2014--2016 Jason Fleming
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
program makeMax
use asgsio
use adcmesh
use logging
use ioutil
implicit none
type(mesh_t) :: m
type(meshNetCDF_t) :: n
type(fileMetaData_t) :: ft ! time varying fulldomain ascii adcirc output file
type(fileMetaData_t) :: fm ! full domain ascii min/max file
logical :: findMin
logical :: writeMaxTimes 
character(len=80) :: line
integer :: i, j, node
real(8) :: temp1, temp2
integer :: ss ! dataset counter
real(8), allocatable :: extremes(:)
real(8), allocatable :: extremeTimes(:)
real(8), allocatable :: dataValues(:)
real(8), allocatable :: adcirc_data(:,:)
real(8) :: snapr ! time (s) associated with dataset
integer :: snapi ! time step associated with dataset
integer :: numNodesNonDefault
integer :: nc_start(2)
integer :: nc_count(2)
integer :: errorIO
!
! initializations
writeMaxTimes = .false.
findMin = .false.
ft%dataFileFormat = ASCIIG
fm%dataFileFormat = ASCIIG
!
! process command line options
argcount = command_argument_count() ! count up command line options
write(6,'(a,i0,a)') 'INFO: There are ',argcount,' command line options.'
i=0
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--datafile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      ft%dataFileName = trim(cmdlinearg)
   case("--write-maxtimes")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      writeMaxTimes = .true.
   case("--findmin")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      findMin = .true.
   case("--netcdf-timeseries")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      ft%dataFileFormat = NETCDFG
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
!
! open time varying data file and extract info
select case(ft%dataFileFormat)
   case(NETCDFG)
      call determineNetCDFFileCharacteristics(ft, m, n)
      write(6,'(a,i0,a)') 'INFO: There are ',ft%nSnaps,' datasets in the file.'

   case(ASCIIG)
      ft%fun = availableUnitNumber()
      call determineASCIIFileCharacteristics(ft)
   case default
      write(6,'(a)') 'ERROR: The data file format option is not valid.'
end select
!
allocate(extremes(m%np))
allocate(extremeTimes(m%np))
allocate(dataValues(m%np))
allocate(adcirc_data(m%np,2))

if (findMin.eqv..false.) then
   extremes = -99999.
else
   extremes = 1.e6
endif
extremeTimes = 0.d0
fm%nSnaps = 1
if ( writeMaxTimes.eqv..true.) then
   fm%nSnaps = 2
endif
!
! open data file; find and compute extremes
select case(ft%dataFileFormat)
case(ASCIIG)
   SS=1  ! jgf: initialize the dataset counter
   !
   ! jgf: loop until we run out of data
   do    
      write(6,'(i0,1x)',advance='no') ss    ! update progress bar
      read(ft%fun,'(a80)',END=123,ERR=123) Line
      read(line,*) SnapR, SnapI
      read(line,*,ERR=907,END=907) SnapR, SnapI, NumNodesNonDefault, ft%defaultValue
      goto 908  ! jgf: this file is sparse ascii
 907  numNodesNonDefault = ft%numValuesPerDataset !jgf: this file is full ascii
         ft%defaultValue = -99999.
 908  dataValues = ft%defaultValue
      select case(ft%irtype)
      case(1) ! scalar data
         do node=1,numNodesNonDefault
            read(ft%fun,*) j, temp1
            dataValues(j) = temp1
         end do
      case(2) ! 2D vector data
         do node=1,numNodesNonDefault
            read(ft%fun,*) j, temp1, temp2
            dataValues(j) = sqrt(temp1**2+temp2**2)         
         end do     
      end select
      ! check to see if each value exceeds the recorded extreme value
      ! at that node
      if (findMin.eqv..false.) then
         do node=1,m%np
            if (dataValues(node).gt.extremes(node)) then
               extremes(node) = dataValues(node)
               extremeTimes(node) = snapR
            endif
         end do
      else
         do node=1,m%np
            if (dataValues(node).lt.extremes(node)) then
               extremes(node) = dataValues(node)
               extremeTimes(node) = snapR
            endif
         end do
      endif
      ss = ss + 1
   end do
123 close(ft%fun) ! jgf: When we've run out of datasets in the current file,
                  ! we jump to here.
case(NETCDFG)
   write(6,'(a)') 'INFO: Compiling a record of extreme values across all data sets.'
   ! loop over datasets   
   do i=1,ft%nSnaps
      write(6,advance='no',fmt='(i0,1x)') i  ! update progress bar
      !
      ! read the dataset from netcdf
      do j=1,ft%irtype
         nc_start = (/ 1, i /)
         nc_count = (/ m%np, 1 /)
         call check(nf90_get_var(ft%nc_id,ft%ncds(j)%nc_varid,adcirc_data(:,j),nc_start,nc_count))
      end do
      ! check to see if each value exceeds the recorded extreme value
      ! at that node
      if (findMin.eqv..false.) then
         if (ft%irtype.eq.2) then
            dataValues = sqrt(adcirc_data(:,1)**2+adcirc_data(:,2)**2)
         endif
         ! find max 
         do node=1,m%np
            if (adcirc_data(node,1).gt.extremes(node)) then
               extremes(node) = adcirc_data(node,1)
               extremeTimes(node) = snapR
            endif
         end do
      else
         ! find min
         do node=1,m%np
            if (adcirc_data(node,1).lt.extremes(node)) then
               extremes(node) = adcirc_data(node,1)
               extremeTimes(node) = snapR
            endif
         end do
      endif
   end do
   call check(nf90_close(ft%nc_id))
case default
   write(6,'(a)') 'ERROR: The data file format option is not valid.'
end select
write(6,'(/,a)') 'INFO: Finished building min/max dataset.'
!
! we need to form the name of the max file based on the name of the
! time series data file
call formMaxFileName(ft%dataFileName, fm%dataFileName)
write(6,'(a,a,a)') 'INFO: Writing min/max data to ',trim(fm%dataFileName),'.'
!
! open file to write maxes
fm%fun = availableUnitNumber()
open(unit=fm%fun, file=trim(adjustl(fm%dataFileName)), status='replace', action='write')
write(fm%fun,'(a,1x,a,1x,a)') trim(rundes), trim(runid), trim(m%agrid)
write(fm%fun,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') fm%nSnaps, ft%numValuesPerDataSet, ft%time_increment, ft%nspool, ft%irtype
!
! write extreme values to output file             
!
! instead of time and time step, write the extreme of the extremes and the node where it occurs
if (findMin.eqv..false.) then
   snapR = maxval(extremes)
   snapI = maxloc(extremes,1)
else
   snapR = minval(extremes)
   snapI = minloc(extremes,1)
endif
write(fm%fun,'(f15.7,2x,i0)') snapR, snapI
do node=1,m%np
   write(fm%fun,'(i0,2x,f15.7)') node, extremes(node)
end do
!
! write the times that the extreme values occurred if specified
if (writeMaxTimes.eqv..true.) then
   ! instead of the time and timestep, write the time that the max of the maxes occurred
   ! as well as the location where the most recent max was set  
   if (findMin.eqv..false.) then
      snapR = extremeTimes(maxloc(extremes,1))
      snapI = maxloc(extremeTimes,1)
   else
      snapR = extremeTimes(minloc(extremes,1))
      snapI = minloc(extremeTimes,1)
   endif
   write(fm%fun,'(f15.7,2x,i0)') snapR, snapI
   do node=1,m%np
      write(fm%fun,'(i0,2x,f15.7)') node, extremeTimes(node)
   end do
endif
close(fm%fun)
!-----------------------------------------------------------------------      
end program makeMax
!-----------------------------------------------------------------------

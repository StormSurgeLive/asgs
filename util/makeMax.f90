!-----------------------------------------------------------------------
! makeMax: takes a time dependent adcirc ascii output file and 
! determines the max value at every node ... e.g., takes a fort.63
! and generates a file like maxele.63.
!-----------------------------------------------------------------------
!
! Example of compilation with gfortran:
! gfortran -ffree-line-length-none -o makeMax.x makeMax.f90
!
! Example of compiling with gfortran with debugging turned on:
! gfortran -g -O0 -Wall -ffree-line-length-none -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal -o makeMax.x makeMax.f90
!
! Example of compiling with gfortran with profiling and test coverage turned on:
! gfortran -pg -O0 -fprofile-arcs -ftest-coverage -Wall -ffree-line-length-none -o makeMax.x makeMax.f90
!-----------------------------------------------------------------------
program makeMax
use asgsio
use adcmesh
use adcircdata
implicit none
character(len=1024) :: maxFileName
character(len=80) :: line
integer :: dataFileFormat
integer :: i, j, n
real(8) :: temp1, temp2
integer :: ndsetMax
integer :: ss ! dataset counter
!
! initializations
writeMaxTimes = .false.
findMin = .false.
dataFileFormat = ASCIIG
!
! process command line options
argcount = iargc() ! count up command line options
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
      datafile = trim(cmdlinearg)
   case("--write-maxtimes")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      writeMaxTimes = .true.
   case("--findmin")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      findMin = .true.
   case("--netcdf-timeseries")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      dataFileFormat = NETCDFG
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
!
! open time varying data file and extract info
select case(dataFileFormat)
   case(NETCDFG)
      call determineNetCDFFileCharacteristics(datafile)
      write(6,'(a,i0,a)') 'INFO: There are ',ndset,' datasets in the file.'
      write(header1,'(a,1x,a,1x,a)') trim(rundes), trim(runid), trim(agrid)
   case(ASCIIG)
      call openFileForRead(63,datafile)
      read(63,'(a80)') header1                ! 1st header line
      read(63,'(a80)') header2                ! 2nd header line
      ! can't trust the number of datasets listed in the header as being
      ! accurate; it will be incorrect after a hotstart
      read(header2,*) ndset, numNodes, tInterval, Interval, num_components
      write(*,'(a,i0,a)') 'INFO: There are ',numNodes,' in the associated mesh.'
      np = numNodes
   case default
      write(6,'(a)') 'ERROR: The data file format option is not valid.'
end select
!
allocate(extremes(np))
allocate(extremeTimes(np))
allocate(dataValues(np))
allocate(adcirc_data(np,2))

if (findMin.eqv..false.) then
   extremes = -99999.
else
   extremes = 1.e6
endif
extremeTimes = 0.d0
ndsetMax = 1
if ( writeMaxTimes.eqv..true.) then
   ndsetMax = 2
endif
!
! open data file; find and compute extremes
select case(dataFileFormat)
case(ASCIIG)
   SS=1  ! jgf: initialize the dataset counter
   !
   ! jgf: loop until we run out of data
   do    
      write(6,'(i0,1x)',advance='no') ss    ! update progress bar
      read(63,'(a80)',END=123,ERR=123) Line
      read(line,*) SnapR, SnapI
      read(line,*,ERR=907,END=907) SnapR, SnapI, NumNodesNonDefault, defaultValue
      goto 908  ! jgf: this file is sparse ascii
 907  NumNodesNonDefault = numNodes !jgf: this file is full ascii
         defaultValue = -99999.
 908  dataValues = defaultValue
      select case(num_components)
      case(1) ! scalar data
         do n=1,numNodesNonDefault
            read(63,*) j, temp1
            dataValues(j) = temp1
         end do
      case(2) ! 2D vector data
         do n=1,numNodesNonDefault
            read(63,*) j, temp1, temp2
            dataValues(j) = sqrt(temp1**2+temp2**2)         
         end do     
      end select
      ! check to see if each value exceeds the recorded extreme value
      ! at that node
      if (findMin.eqv..false.) then
         do n=1,np
            if (dataValues(n).gt.extremes(n)) then
               extremes(n) = dataValues(n)
               extremeTimes(n) = snapR
            endif
         end do
      else
         do n=1,np
            if (dataValues(n).lt.extremes(n)) then
               extremes(n) = dataValues(n)
               extremeTimes(n) = snapR
            endif
         end do
      endif
      ss = ss + 1
   end do
123 close(63) ! jgf: When we've run out of datasets in the current file,
                  ! we jump to here.
case(NETCDFG)
   ! get netcdf variable IDs for the the data 
   do j=1,num_components
      call check(nf90_inq_varid(nc_id, trim(adjustl(varname(j))), nc_varid(j)))
   end do   
   write(6,'(a)') 'INFO: Compiling a record of extreme values across all data sets.'
   ! loop over datasets   
   do i=1,ndset
      write(6,advance='no',fmt='(i0,1x)') i  ! update progress bar
      !
      ! read the dataset from netcdf
      do j=1,num_components
         nc_start = (/ 1, i /)
         nc_count = (/ np, 1 /)
         call check(nf90_get_var(nc_id,nc_varid(j),adcirc_data(:,j),nc_start,nc_count))
      end do
      ! check to see if each value exceeds the recorded extreme value
      ! at that node
      if (findMin.eqv..false.) then
         if (num_components.eq.2) then
            dataValues = sqrt(adcirc_data(:,1)**2+adcirc_data(:,2)**2)
         endif
         ! find max 
         do n=1,np
            if (adcirc_data(n,1).gt.extremes(n)) then
               extremes(n) = adcirc_data(n,1)
               extremeTimes(n) = snapR
            endif
         end do
      else
         ! find min
         do n=1,np
            if (adcirc_data(n,1).lt.extremes(n)) then
               extremes(n) = adcirc_data(n,1)
               extremeTimes(n) = snapR
            endif
         end do
      endif
   end do
   call check(nf90_close(nc_id))
case default
   write(6,'(a)') 'ERROR: The data file format option is not valid.'
end select
write(6,'(/,a)') 'INFO: Finished building min/max dataset.'
!
! we need to form the name of the max file based on the name of the
! time series data file
call formMaxFileName(datafile, maxFileName)
write(6,'(a,a,a)') 'INFO: Writing min/max data to ',trim(maxFileName),'.'
!
! open file to write maxes
open(unit=363, file=trim(adjustl(maxFileName)), status='replace', action='write')
write(363,'(a)') trim(adjustl(header1))
write(363,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') ndset, np, tInterval, Interval, nCol
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
write(363,'(f15.7,2x,i0)') snapR, snapI
do n=1,np
   write(363,'(i0,2x,f15.7)') n, extremes(n)
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
   write(363,'(f15.7,2x,i0)') snapR, snapI
   do n=1,np
      write(363,'(i0,2x,f15.7)') n, extremeTimes(n)
   end do
endif
close(363)
!-----------------------------------------------------------------------      
end program makeMax
!-----------------------------------------------------------------------

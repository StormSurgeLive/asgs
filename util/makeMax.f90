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
implicit none
character(len=1024) :: outputfile
character(len=1024) :: datafile
real(8), allocatable :: extremes(:)
real(8), allocatable :: extremeTimes(:)
real(8), allocatable :: dataValues(:)
real(8) :: temp1, temp2 ! raw data from the file
real(8) :: defaultValue
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
character(len=80) :: line ! line from the data file
integer :: numNodes
integer :: numSnaps
integer :: numNodesNonDefault ! number of nodes in a snap that have non default values (for sparse format)
real(8) :: snapR ! time in seconds
integer :: snapI ! time step
integer :: nCol ! number of columns of data
real(8) :: tinterval ! spooling interval in seconds
integer :: interval ! spooling interval in timesteps
logical :: writeMaxTimes !.true. if the time of maximum occurrence should be written
logical :: findMin ! .true. if the min should be found instead of the max
integer :: errorIO ! i/o status
integer :: argcount
logical :: fileFound !.true. if the file was found
integer :: i, j, n
integer :: ss ! dataset counter
!
! initializations
writeMaxTimes = .false.
findMin = .false.
!
! process command line options
argcount = iargc() ! count up command line options
write(6,*) 'There are ',argcount,' command line options.'
i=0
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--outputfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      outputfile = trim(cmdlinearg)
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
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
!
! open time varying data file
fileFound = .false.
inquire(file=trim(adjustl(datafile)),exist=fileFound)
if (fileFound.eqv..false.) then
   write(6,'(a)') 'ERROR: The file "',trim(adjustl(datafile)),'" does not exist.'
   stop 1
endif
errorIO = 0
open(unit=63, file=trim(adjustl(datafile)), status='old', action='read', iostat=errorIO)
if (errorIO.ne.0) then
   write(6,'(a)') 'ERROR: The file "',trim(adjustl(datafile)),'" could not be opened.'
   stop 1
endif
!
! open file to write maxes
open(unit=363, file=trim(adjustl(outputfile)), status='replace', action='write')
!
read(63,'(a80)') line                 ! 1st header line
write(363,'(a)') trim(adjustl(line))
read(63,'(a80)') line                 ! 2nd header line
write(363,'(a)') trim(adjustl(line))
read(line,*) numSnaps, numNodes, tInterval, Interval, nCol
allocate(extremes(numNodes))
allocate(extremeTimes(numNodes))
allocate(dataValues(numNodes))
if (findMin.eqv..false.) then
   extremes = -99999.
else
   extremes = 1.e6
endif
extremeTimes = 0.d0
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
   select case(nCol)
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
      do n=1,numNodes
         if (dataValues(n).gt.extremes(n)) then
            extremes(n) = dataValues(n)
            extremeTimes(n) = snapR
         endif
      end do
   else
      do n=1,numNodes
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
!
! write extreme values to output file             
if (writeMaxTimes.eqv..false.) then
   ! instead of time and time step, write the extreme of the extremes and the node where it occurs
   if (findMin.eqv..false.) then
      snapR = maxval(extremes)
      snapI = maxloc(extremes,1)
   else
      snapR = minval(extremes)
      snapI = minloc(extremes,1)
   endif
   write(363,'(f15.7,2x,i0)') snapR, snapI
   do n=1,numNodes
      write(363,'(i0,2x,f15.7)') n, extremes(n)
   end do
else
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
   do n=1,numNodes
      write(363,'(i0,2x,f15.7)') n, extremeTimes(n)
   end do
endif
close(363)
!-----------------------------------------------------------------------      
end program makeMax
!-----------------------------------------------------------------------

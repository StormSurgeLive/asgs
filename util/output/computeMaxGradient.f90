!-----------------------------------------------------------------------
! computeMaxGradients : compute element elevation gradients from a time
! varying water surface elevation output file and find the maximum values
!-----------------------------------------------------------------------
!
! Example of compilation with gfortran:
! gfortran -ffree-line-length-none -o computeMaxGradients.x -I/home/jason/asgs/trunk/output -I/usr/include  computeMaxGradients.f90  -lnetcdff
!
! Example of compiling with gfortran with debugging turned on:
! gfortran -g -O0 -Wall -ffree-line-length-none -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal -o computeMaxGradients.x -I/home/jason/asgs/trunk/output -I/usr/include  computeMaxGradients.f90  -lnetcdff
!
! Example of compiling with gfortran with profiling and test coverage turned on:
! gfortran -pg -O0 -fprofile-arcs -ftest-coverage -Wall -ffree-line-length-none -o computeMaxGradients.x -I/home/jason/asgs/trunk/output -I/usr/include computeMaxGradients.f90 -lnetcdff
!-----------------------------------------------------------------------
include 'adcmesh.f90'
program computeMaxGradients
use adcmesh
implicit none
character(len=1024) :: gradientfile
character(len=1024) :: maxfile
character(len=1024) :: datafile
character(len=1024) :: outputformat
real(8) :: WarnElevGrad
logical :: threshold
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
integer :: argcount
integer :: i, j, jn
logical :: useMax !.true. if a maxele file is used as input
!
! initializations
useMax = .false.
threshold = .false.
WarnElevGrad = 0.02d0
meshFileName = 'fort.14'
datafile = 'fort.63'
gradientfile = 'grad_warn.out'
!
! process command line options
argcount = iargc() ! count up command line options
write(6,*) 'There are ',argcount,' command line options.'
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
   case("--maxfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      datafile = trim(cmdlinearg)
      useMax = .true.
   case("--datafile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      datafile = trim(cmdlinearg)
   case("--gradientfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      gradientfile = trim(cmdlinearg)
   case("--outputformat")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      outputformat = trim(cmdlinearg)  
   case("--threshold")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) WarnElevGrad 
      threshold = .true.       
   case("--cpp")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      i = i + 1
      call getarg(i, cmdlinearg)
      read(cmdlinearg,*) slam0
      i = i + 1
      call getarg(i, cmdlinearg)
      read(cmdlinearg,*) sfea0
      write(6,*) "INFO: slam0=",slam0," and sfea0=",sfea0,"."
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
!
call read14()     
call computeCPP()      
call compute2xAreas()
call computeWeightingCoefficients()
call computeElementCentroids()
!
allocate ( eta2(np) )
allocate ( nodecode(np) )
! 
! open gradient file for writing data
open(unit=363,file=trim(adjustl(gradientfile)),status='old',action='write')
! open data file for reading
call openFileForRead(63,datafile)
read(63,'(a80)') line                 ! 1st header line
read(63,'(a80)') line                 ! 2nd header line
read(line,*) numSnaps, numNodes, tInterval, Interval, nCol
ss=1
! jgf: loop until we run out of data
do    
   write(6,'(i0,1x)',advance='no') ss    ! update progress bar
   read(63,'(a80)',END=123,ERR=123) Line
   read(line,*) SnapR, SnapI
   read(line,*,ERR=907,END=907) SnapR, SnapI, NumNodesNonDefault, DefaultValue
   goto 908  ! jgf: this file is sparse ascii
907  NumNodesNonDefault = NumNodes !jgf: this file is full ascii
      defaultValue = -99999.d0
908  eta2 = defaultValue
   do n=1,numNodesNonDefault
      read(63,*) j, temp1
      eta2(j) = temp1    
   end do
   ! compute wet/dry state
   nodecode = 0
   do i=1,np
      if ( (dp(n)+eta2(n)).gt.0.d0 ) then
         nodecode(i) = 1
      endif
   end do
   ! loop over elements and compute gradients
   do i=1,ne
      myNodes(1:3) = nm(i,1:3)
      ncele = product(nodecode(myNodes(1:3)))
      if(ncele.eq.0) then
         dEta2Mag=-9999.
      else
         eta2n(1:3) = eta2(myNodes(1:3))
         dEta2Dx = dot_product(eta2n,fdx(:,i))/Areas(i)
         dEta2Dy = dot_product(eta2n,fdy(:,i))/Areas(i) 
         dEta2Mag = sqrt(dEta2Dx*dEta2Dx+dEta2Dy*dEta2Dy)
      endif
      if ( (threshold.eqv..true.).and. &
         (trim(adjustl(outputformat)).eqv."elements").and. &
         (dEta2Mag.GE.WarnElevGrad) then
         write(16,*) IE,dEta2Mag
      endif
   enddo
   
   ss = ss + 1
   if ((ss.gt.1).and.(useMax.eqv..true.)) then
      exit ! kick out of loop after 1 dataset if this is a max file
   endif
end do
123   continue


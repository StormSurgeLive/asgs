! example of compiling with gfortran with optimization turned on
! gfortran -O2 -ffree-line-length-none -o totalWaterDepthGradient.x -I/home/jason/asgs/trunk/output -I/usr/include  totalWaterDepthGradient.f90
!
program totalWaterDepthGradient
use adcmesh
implicit none
character(1024) :: outputfile
real(8) :: startingWaterLevel ! water surface elevation (m) to start
real(8) :: endingWaterLevel   ! water surface elevation (m) at end
real(8) :: waterLevelIncrement ! step size 
real(8) :: dx ! distance from node to its neighbor (m)
real(8) :: totWaterDepthGradient ! at a node at a particular water level, unitless
real(8) :: maxTotalWaterDepthGradient ! at a node at a particular water level, unitless
real(8) :: myTotalWaterDepth ! (m) at node at a particular water level
real(8) :: neighborTotalWaterDepth ! (m) at for each neighbor of a node at a particular water level
real(8) :: waterLevel ! (m) water surface elevation used for total water depth computation
integer :: numWaterLevels ! number of datasets to be produced
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
integer :: argcount
integer :: i, j, k
!
! initializations
i=0
startingWaterLevel = 0.d0
endingWaterLevel = 1.0d0
waterLevelIncrement = 0.1d0
outputfile = 'twdg.63' ! twdg stands for total water depth gradient
!
! command line options
argcount = iargc() ! count up command line options
write(6,'(a,i0,a)') 'There are ',argcount,' command line options.'
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--verbose")
      write(6,'(3(a))') "INFO: Processing ",trim(cmdlineopt),"."
      verbose = .true.
   case("--meshfile")
      i = i + 1
      call getarg(i, meshFileName)
      write(6,'(3(a))') "INFO: Processing ",trim(cmdlineopt)," ",trim(meshFileName),"."
   case("--starting-water-level")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,'(a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) startingWaterLevel  
   case("--ending-water-level")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,'(a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) endingWaterLevel  
   case("--water-level-increment")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,'(a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) waterLevelIncrement  
   case("--outputfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,'(a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      outputfile = trim(cmdlinearg)
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
      write(6,'(a,i0,99x(a))') "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
!
! determine number of water levels to consider
numWaterLevels = abs((endingWaterLevel-startingWaterLevel)/waterLevelIncrement)
if ( (startingWaterLevel.gt.endingWaterLevel).and.(waterLevelIncrement.ge.0) ) then
   write(6,'(a)') 'ERROR: The starting water level is greater than the ending water level and yet the water level increment is positive.'
   stop
endif
write(6,'(a,i0,a)') 'INFO: There will be ',numWaterLevels,' water levels represented in the output file.'
!
! Load fort.14
call read14()
call computeNeighborTable()
!
! open a file that looks like an ADCIRC time varying full domain water surface
! elevation (fort.63) file 
open(63,file=trim(adjustl(outputfile)),status='replace',action='write')
! copy the comment line from the top of the mesh file into the output
write(63,'(a)') trim(adjustl(agrid)) ! RUNDES, RUNID, AGRID
! NDSETSE, NP, DTDP*NSPOOLGE, NSPOOLGE, IRTYPE
write(63,'(i0,1x,i0,1x,f15.8,1x,i0,1x,i0)') numWaterLevels, np, waterLevelIncrement, 1, 1
!
! loop over water specified water levels
do j=0, numWaterLevels-1
   waterLevel = startingWaterLevel + j*waterLevelIncrement
   write(63,'(f15.8,1x,i0)') waterLevel, j+1  ! TIME, IT
   ! loop over nodes; at each node:
   !   a. use the given water surface elevation to compute total water depth at that node and neighbors
   !   b. for each wet neighbor, compute total water depth
   !   c. compute total water depth gradient between node and each neigbor
   !   d. storm maximum total water depth gradient at that node and at that water depth 
   do i=1, np  
      myTotalWaterDepth = xyd(3,i) + waterLevel
      maxTotalWaterDepthGradient = -99999.0
      ! loop over neighbors
      do k=2, nneigh(i) ! k=1 is this node itself, k=2 is 1st neighbor...
         neighborTotalWaterDepth = xyd(3,NeiTab(i,k)) + waterLevel
         dx = sqrt( (x_cpp(i)-x_cpp(NeiTab(i,k)))**2 + (y_cpp(i)-y_cpp(NeiTab(i,k)))**2 ) 
         !write(*,*) x_cpp(i), y_cpp(i), x_cpp(NeiTab(i,k)), y_cpp(NeiTab(i,k))
         !write(*,*) dx
         if ( neighborTotalWaterDepth.ge.0.d0 .and. myTotalWaterDepth.ge.0.d0 ) then
            totWaterDepthGradient = abs(myTotalWaterDepth - neighborTotalWaterDepth)/dx
         else
            totWaterDepthGradient = -99999.0
         endif
         maxTotalWaterDepthGradient = max(maxTotalWaterDepthGradient,totWaterDepthGradient)
      end do
      ! record the max gradient for that node to the output file
      write(63,'(i0,1x,f15.8)') i, maxTotalWaterDepthGradient
   end do
end do
close(63)
!-------------------------------------------------------------------
end program totalWaterDepthGradient
!-------------------------------------------------------------------

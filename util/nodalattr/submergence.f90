! Example of compiling with gfortran with debugging turned on:
! gfortran -g -O0 -Wall -ffree-line-length-none -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal -o submergence.x -I/home/jason/asgs/trunk/output -I/usr/include  submergence.f90  -lnetcdff
!
! Example of compiling with gfortran with optimization turned on:
! gfortran -O3 -ffree-line-length-none -o submergence.x -I/home/jason/asgs/trunk/output -I/usr/include  submergence.f90  -lnetcdff
!
!
program submergence
use adcmesh
use asgsio, only : openFileForRead
implicit none
character(len=1024) :: outputfile
character(len=1024) :: seedfile
character(len=1024) :: nodal_attr_name  
integer :: nseed ! number of seed locations
integer, allocatable :: seednod(:) ! (nseed) nodes neighboring seed nodes
real(8), allocatable :: seedx(:) ! (nseed) x coordinate locations of seed nodes
real(8), allocatable :: seedy(:) ! (nseed) x coordinate locations of seed nodes
real(8), allocatable :: localDryElevation(:) ! (m) (+upward) in viscinity of seed 
real(8), allocatable :: dist(:) ! (np) distance (m) from seed to each node 
integer, allocatable :: frontNodes(:) ! (np) node numbers along the wet front
integer, allocatable :: newFrontNodes(:) ! (np) newly discovered wet nodes numbers
integer :: numFrontNodes ! number of nodes along the wet front
integer :: numNewFrontNodes ! number of newly discovered wet nodes
integer :: neighborNode ! node connected to thisNode
integer :: thisNode     ! wet node whose neighbors are currently under consideration
logical, allocatable :: wet(:) ! (np) .true. for nodes found to be wet in the current round
logical, allocatable :: startdry(:) ! (np) .true. for nodes that are to start dry (including those that would do so purely as a result of topography)
real(8) :: dryElevationAnyway ! (m) threshold elevation that forces nodes dry (+upward) 
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
integer :: argcount
integer :: i, j, k
!
! initializations
dryElevationAnyway = 0.d0
!
! process command line options
argcount = iargc() ! count up command line options
write(6,*) 'There are ',argcount,' command line options.'
i=0
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--verbose")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      verbose = .true.
   case("--meshfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      meshFileName = trim(cmdlinearg)
   case("--outputfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      outputfile = trim(cmdlinearg)
   case("--seedfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      seedfile = trim(cmdlinearg)
   case("--dry-elevation")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) dryElevationAnyway
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
!
! Open and read the mesh file.
write(6,*) 'INFO: Reading mesh file.'
call read14()
write(6,*) 'INFO: Finished reading mesh file.'
!
! proceed to create nodal attributes as directed by command line options
!
! surface submergence state
nodal_attr_name =  'surface_submergence_state'
if (neighborTableComputed.eqv..false.) then
   call computeNeighborTable()
endif
!
! read seed file containing x/y coordinates and wet limits of seed locations
write(6,*) 'INFO: Loading seed coordinates file.'
call openFileForRead(15,seedfile)
read(15,*) nseed
allocate(seedx(nseed))
allocate(seedy(nseed))
allocate(localDryElevation(nseed))
! the seed file coordinates must be in the same projection as ADCIRC mesh, 
! that is, geographic degrees east and north 
do i=1,nseed
   read(15,*) seedx(i), seedy(i), localDryElevation(i)
enddo
close(15)
write(6,*) 'INFO: Finished loading seed coordinates file.'
allocate(seednod(nseed))
!
! search for node nearest to each seed
write(6,*) 'INFO: Using seeds to find starting nodes.'
allocate(dist(np))
dist = huge(1.d0)
do i=1,nseed
   do j=1,np
      dist(j) = sqrt((seedx(i)-xyd(1,j))**2+(seedy(i)-xyd(2,j))**2)
   end do
   seednod(i)=minloc(dist,1)
enddo
!
! Find connected wet nodes to seed node(s)
write(6,*) 'INFO: Finding connected wet nodes.'
! initialize the start dry value
allocate(startdry(np))
allocate(wet(np))
allocate(frontNodes(np))
allocate(newFrontNodes(np))
startdry=.true. 
do i=1,nseed
   wet = .false.      
   ! check the seed node to see if it is on dry land (relative to the local
   ! indicator of dry land)
   if (xyd(3,seednod(i)).lt.-localDryElevation(i)) then
      startdry(seednod(i))=.false.
      write(6,'("WARNING: The seed number ",i0," at lon=",F15.7," lat=",F15.7," is in a dry area according to the local dry elevation of ",F15.7,". Please place by a wet node.")') i, seedx(i), seedy(i),localDryElevation(i)
      cycle ! just go to the next seed
   endif
   numNewFrontNodes = 1
   newFrontNodes(1)=seednod(i)
   ! repeat this loop until no new wet nodes are identified
   do 
      numFrontNodes = numNewFrontNodes
      frontNodes = newFrontNodes
      numNewFrontNodes = 0
      newFrontNodes = 0
      do j=1,numFrontNodes
         ! loop over their neighbors (start with k=2 b/c the node itself
         ! is listed as index 1)
         thisNode = frontNodes(j)
         do k=2,nneigh(thisNode)
            neighborNode = neitab(thisNode,k) 
            ! if the neighbor is not marked wet (yet)
            if (wet(neighborNode).eqv..false.) then
               ! check to see if the neighbor is below the local depth limit
               if (xyd(3,neighborNode).ge.-localDryElevation(i)) then
                  wet(neighborNode) = .true.
                  numNewFrontNodes = numNewFrontNodes + 1
                  newFrontNodes(numNewFrontNodes) = neighborNode
               endif
            endif
         enddo
      end do
      if (numNewFrontNodes.eq.0) then
         exit ! found them all
      endif
   end do
   where (wet.eqv..true.)
      startdry = .false.
   end where
end do
write(6,*) 'INFO: Finished finding connected wet nodes.'
!
! turn off the startdry in places where it was marked .true. but local
! mesh depth indicates that the location will be considered dry by ADCIRC 
! in any case (mesh depth is zero at the water surface and is positive 
! downward, so negative depths are actually out of the water) 
where (startdry.eqv..true.)
   where (xyd(3,:).lt.-dryElevationAnyway)
      startdry = .false.
   end where
end where
!
! write out the nodes that are deep enough for ADCIRC to assume they are
! wet, but are not hydrologically connected to a seeded basin (e.g., a polder)
write(6,*) 'INFO: Writing surface submergence nodal attribute.'
open(17,file='submergence.'//trim(outputfile),action='write',status='replace')
write(17,'(A)') trim(nodal_attr_name)
! write the number of nondefault values
write(17,*) count(startDry.eqv..true.)
do i=1, np
   if (startDry(i).eqv..true.) then
      write(17,'(i0," ",f3.1)') i, 1.0
   endif
end do
close(17)
write(6,*) 'INFO: Finished writing surface submergence nodal attribute.'
!-------------------------------------------------------------------
end program submergence
!-------------------------------------------------------------------

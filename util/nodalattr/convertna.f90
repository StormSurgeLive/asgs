!
! Example of compilation with gfortran:
! gfortran -ffree-line-length-none -o convertna.x -I/home/jason/asgs/trunk/output -I/usr/include  convertna.f90  -lnetcdff
!
! Example of compiling with gfortran with debugging turned on:
! gfortran -g -O0 -Wall -ffree-line-length-none -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal -o convertna.x -I/home/jason/asgs/trunk/output -I/usr/include  convertna.f90  -lnetcdff
!
! Example of compiling with gfortran with profiling and test coverage turned on:
! gfortran -pg -O0 -fprofile-arcs -ftest-coverage -Wall -ffree-line-length-none -o convertna.x -I/home/jason/asgs/trunk/output -I/usr/include convertna.f90 -lnetcdff
!-----------------------------------------------------------------------
program convertna
use adcmesh
use asgsio, only : openFileForRead
use nodalattr
implicit none
character(len=1024) :: outputfile
character(len=1024) :: targetMeshFileName
character(len=1024) :: naName
character(len=1024) :: targetAgrid ! comment line from the target mesh file
integer :: targetNE ! number of elements in target mesh
integer :: targetNP ! number of nodes in target mesh
real(8), allocatable :: targetXYD(:,:) ! node table for the target mesh
real(8), allocatable :: targetXCPP(:) ! (m) x coords of target mesh nodes in CPP
real(8), allocatable :: targetYCPP(:) ! (m) y coords of targen mesh nodes in CPP
real(8), allocatable :: targetNodalAttribute(:,:) ! (numVals,targetNP)
real(8), allocatable :: sourceNodalAttribute(:,:) ! (numVals, np)
real(8), allocatable :: dist(:) ! (np) distance (deg) from target node to each source node
logical, allocatable :: areDefaultValues(:) !.true. if the target node has all default values
integer :: sourceNodeNumber ! node number in source mesh closest to this node in target mesh
real(8) :: proximity ! distance (deg) from target node to nearest source node
real(8) :: closestSourceNeighbor! distance (deg) from source node to nearest source neighbor
logical :: interpolate ! .true. if we should interpolate nodat attributes from one mesh to another
logical :: write63 ! .true. if the n.a. data should be written to ascii fort.63 format
integer :: chunk ! reporting 10% for progress bar
integer :: step  ! reporting 3.33% for progress bar
integer :: progress ! percent progress reading file
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
integer :: argcount
integer :: i, j, jn
!
! initializations
write63 = .false.
interpolate = .false.
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
   case("--nodal-attributes-file")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      nodalAttributesFile = trim(cmdlinearg)
   case("--outputfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      outputfile = trim(cmdlinearg)
   case("--nodal-attribute")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      naName = trim(cmdlinearg)
   case("--target-meshfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      targetMeshFileName = trim(cmdlinearg)
   case("--write63")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      write63 = .true.
   case("--interpolate")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      interpolate = .true.      
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
! load the specified nodal attribute
call loadNodalAttribute(naName)
!
! write in fort.63 format if specified to do so
if (write63.eqv..true.) then
   call writeNodalAttribute63(naName, outputfile)
   stop
endif
!
! interpolate nodal attribute from one mesh to another if specified
if (interpolate.eqv..true.) then
   ! Open and read the mesh file.
   write(6,'(a)') 'INFO: Reading source mesh file.'
   call read14()
   write(6,'(a)') 'INFO: Finished reading source mesh file.'
   write(6,'(a)') 'INFO: Computing neighbor table for source mesh file.'
   call computeNeighborTable()
   write(6,'(a)') 'INFO: Finished computing neighbor table for source mesh file.'
   write(6,'(a)') 'INFO: Computing neighbor edge length table for source mesh file.'
   call computeNeighborEdgeLengthTable()
   write(6,'(a)') 'INFO: Finished computing neighbor edge length table for source mesh file.'
   ! read the node table from the target mesh file so we can use the node
   ! locations for interpolations
   write(6,*) 'INFO: Reading target mesh file.'
   call openFileForRead(144,targetMeshFileName)
   read(144,*) targetAgrid
   read(144,*) targetNE, targetNP
   allocate(targetXYD(3,targetNP))
   do i=1, targetNP
      read(144,*) jn, (targetXYD(j,i), j=1,3)
   end do
   close(144)
   write(6,*) 'INFO: Finished reading target mesh file.'
   ! compute the cpp projection
   allocate(targetXCPP(targetNP))
   allocate(targetYCPP(targetNP))
   targetXCPP = R * (targetXYD(1,:)*deg2rad - slam0*deg2rad) * cos(sfea0*deg2rad)
   targetYCPP = targetXYD(2,:)*deg2rad * R
   !
   ! now go through the target nodes and match the nodal attributes
   ! to the ones from the source mesh
   allocate(targetNodalAttribute(na(1)%numVals,targetNP))
   allocate(sourceNodalAttribute(na(1)%numVals,np))
   ! populate full array of source nodal attribute so we can grab data
   ! from it at any location
   write(6,*) 'INFO: Fully populating source nodal attribute array.'
   do i=1,np
      sourceNodalAttribute(:,i) = na(1)%defaultVals(:)
   end do
   do i=1,na(1)%numNodesNotDefault
      sourceNodalAttribute(:,na(1)%nonDefaultNodes(i)) = na(1)%nonDefaultVals(:,i)
   end do
   allocate(dist(na(1)%numNodesNotDefault))
   write(6,*) 'INFO: Assigning target nodal attributes.'
   ! compute parameters related to printing a progress bar
   progress=10
   chunk=targetNP/10
   step=targetNP/30
   do i=1,targetNP
      ! update progress bar
      !write(*,*) i
      if (mod(i,chunk).eq.0) then
         write(6,'(i0,"%")',advance='no') progress
         progress = progress + 10
      elseif (mod(i,step).eq.0) then
         write(6,'(a1)',advance='no') '.'     
      endif
      ! calculate the distance between this node and each node in the source
      ! mesh that does not have a default nodal attribute value
      do j=1,na(1)%numNodesNotDefault
         dist(j) = sqrt( (targetXCPP(i)-x_cpp(na(1)%nonDefaultNodes(j)))**2 & 
           + (targetYCPP(i)-y_cpp(na(1)%nonDefaultNodes(j)))**2 )
      end do
      ! find the node number of the closest node
      sourceNodeNumber = na(1)%nonDefaultNodes(minloc(dist,1))
      proximity = minval(dist)
      ! if the target node is closer to the source node than half the distance
      ! to the source node's nearest neighbor, then the target node has
      ! the same nodal attribute value as the source node; otherwise the
      ! target node has the default value
      closestSourceNeighbor = minval(neighborEdgeLengthTable(sourceNodeNumber,2:nneigh(sourceNodeNumber)))
      if (proximity.lt.closestSourceNeighbor) then
         targetNodalAttribute(:,i) = sourceNodalAttribute(:,sourceNodeNumber)
      else 
         targetNodalAttribute(:,i) = na(1)%defaultVals(:)
      endif
   end do
   write(6,*) 'INFO: Finished assigning target nodal attributes.'
   write(6,*) 'INFO: Writing target nodal attributes.'
   ! count the nondefault values
   allocate(areDefaultValues(np))
   areDefaultValues = .true.
   do i=1,np  
      do j=1,na(1)%numVals
        ! if any of the nodal attribute values at this node are different
        ! from the default value(s), then this is a non default node
        if ( abs(targetNodalAttribute(j,i)-na(1)%defaultVals(j)).gt.1.d-6 ) then
            areDefaultValues(i) = .false.
            exit
         endif
      end do
   enddo
   open(12,file=trim(outputfile),action='write',status='replace')
   write(12,'(A)') trim(naName)
   ! write the number of nondefault values
   write(12,*) count(areDefaultValues.eqv..false.)
   do i=1,np
      if (areDefaultValues(i).eqv..false.) then
         write(12,130) i,(targetNodalAttribute(j,i), j=1,na(1)%numVals) 
      endif
   end do
   close(12)
   write(6,*) 'INFO: Finished writing nondefault surface roughness values.'
   130   format(i0,12(2x,f15.7))
endif
!-----------------------------------------------------------------------      
end program convertna
!-----------------------------------------------------------------------

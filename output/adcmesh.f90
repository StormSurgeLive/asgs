!-----+---------+---------+---------+---------+---------+---------+
!
! adcmesh.f90
! This is a module for storing and manipulating data for ADCIRC meshes;
! it is based on code written by Corbitt Kerr.
!
!-----+---------+---------+---------+---------+---------+---------+
module adcmesh
!-----+---------+---------+---------+---------+---------+---------+
character(1024) :: meshFileName ! full pathname of file
double precision, parameter :: R = 6378206.4d0 ! radius of the earth
double precision, parameter :: pi = 3.141592653589793d0
double precision, parameter :: deg2rad = pi/180.d0
double precision, parameter :: rad2deg = 180.d0/pi
logical                         :: verbose
double precision, allocatable, target :: xyd(:,:), bar(:,:,:)
!
! parameters related to carte parallelogrammatique projection (CPP)
logical                          :: cppComputed = .false.
double precision, allocatable :: x_cpp(:)
double precision, allocatable :: y_cpp(:)
!
! parameters related to Albers Equal Area Conic projection
logical :: albersComputed = .false.
real(8), allocatable :: xalbers(:)
real(8), allocatable :: yalbers(:)
!
! parameters related to the neighbor edge length table (np,neimax)
logical :: neighborEdgeLengthTableComputed = .false. ! .true. when mem is allocated for this
real(8), allocatable :: neighborEdgeLengthTable(:,:)
!
real(8), allocatable          :: sigma(:)
character(80)                 :: agrid
integer                       :: ne, np
integer                       :: nfen
integer                       :: mnei = 15  ! maximum number of neighbors for a node
integer                       :: neta_count ! count of open boundary nodes
integer                       :: nvel_count ! count of land boundary nodes
integer                       :: nope, neta
integer                       :: nbou, nvel
integer,          allocatable :: nm(:,:)   ! element table (ne,3)
integer,          allocatable :: nvdll(:)  ! number of nodes on each open boundary
integer,          allocatable :: nbdv(:,:) ! node numbers on each open boundary
integer,          allocatable :: nvell(:)  ! number of nodes on each flux boundary
integer,          allocatable :: ibtype(:) ! boundary type of each flux boundary
integer,          allocatable :: nbvv(:,:) ! node numbers on each flux boundary
integer,          allocatable :: lbcodei(:) ! bound. type array for flux boundaries 
integer                       :: nvdll_max  ! longest elevation boundary
integer                       :: nvell_max  ! longest flux boundary     
logical                       :: neighborTableComputed = .false.
logical                       :: allLeveesOK ! .false. if there are any issues
integer                       :: NEIMIN
integer                       :: NEIMAX 
integer,         allocatable :: NNeigh(:)
integer,         allocatable :: NeiTab(:,:)
integer,         allocatable :: NeiTabEle(:,:)
!
integer                       :: NC_DimID_node
integer                       :: NC_DimID_vnode
integer                       :: NC_DimID_nele
integer                       :: NC_DimID_nvertex
integer                       :: NC_DimID_nope
integer                       :: NC_DimID_max_nvdll
integer                       :: NC_DimID_nbou
integer                       :: NC_DimID_neta
integer                       :: NC_DimID_nvel
integer                       :: NC_DimID_max_nvell
!
integer                       :: NC_VarID_Mesh
integer                       :: NC_VarID_x
integer                       :: NC_VarID_y
integer                       :: NC_VarID_sigma
integer                       :: NC_VarID_element
integer                       :: NC_VarID_neta
integer                       :: NC_VarID_nvdll
integer                       :: NC_VarID_max_nvdll
integer                       :: NC_VarID_ibtypee
integer                       :: NC_VarID_nbdv
integer                       :: NC_VarID_nvel
integer                       :: NC_VarID_nope 
integer                       :: NC_VarID_nvell
integer                       :: NC_VarID_max_nvell
integer                       :: NC_VarID_ibtype
integer                       :: NC_VarID_nbvv
integer                       :: NC_VarID_depth

logical                       :: projectCPP ! .true. if user wants to project mesh coordinates with CPP to aid in visualization
logical                       :: cppUpdated ! .true. if we've already computed/written CPP on this execution
double precision            :: slam0  ! longitude on which cpp projection is centered
double precision            :: sfea0  ! latitude on which cpp projection is centered
real(8) :: lonmin   ! domain extents (degrees)
real(8) :: lonmax
real(8) :: latmin
real(8) :: latmax 
!
! elevation boundaries and flux boundaries where
! ibtype = 0,1,2,10,11,12,20,21,22,30,52
type simpleBoundary_t
   integer :: indexNum               ! order within the fort.14 file
   integer, allocatable :: nodes(:) ! node numbers on boundary
end type simpleBoundary_t
! variable holding elevation boundaries
type(simpleBoundary_t), allocatable :: elevationBoundaries(:)

! variable holding flux boundaries with ibtype = 0, 1, 2, 10, 11, 12, 20, 21, 22, 30, 52
type(simpleBoundary_t), allocatable :: simpleFluxBoundaries(:)
integer :: numSimpleFluxBoundaries ! for memory allocation
integer :: sfCount   ! index into the simpleFluxBoundaries array

! flux boundaries where ibtype = 3, 13, 23
type externalFluxBoundary_t
   integer :: indexNum               ! order within the fort.14 file
   integer, allocatable :: nodes(:)
   real, allocatable :: barlanht(:)
   real, allocatable :: barlancfsp(:)
end type externalFluxBoundary_t
type(externalFluxBoundary_t), allocatable :: externalFluxBoundaries(:)
integer :: numExternalFluxBoundaries 
integer :: efCount   ! index into the externalFluxBoundaries array

! flux boundaries where ibtype = 4, 24
type internalFluxBoundary_t
   integer :: indexNum               ! order within the fort.14 file
   integer, allocatable :: nodes(:)
   integer, allocatable :: ibconn(:)
   real, allocatable :: barinht(:)
   real, allocatable :: barincfsb(:)
   real, allocatable :: barincfsp(:)         
end type internalFluxBoundary_t
type(internalFluxBoundary_t), allocatable :: internalFluxBoundaries(:)
integer :: numInternalFluxBoundaries    
integer :: ifCount   ! index into the internalFluxBoundaries array

! flux boundaries where ibtype = 5, 25
type internalFluxBoundaryWithPipes_t
   integer :: indexNum               ! order within the fort.14 file
   integer, allocatable :: nodes(:)
   integer, allocatable :: ibconnr(:)
   real, allocatable :: barinhtr(:)
   real, allocatable :: barincfsbr(:)
   real, allocatable :: barincfspr(:)
   real, allocatable :: pipehtr(:)
   real, allocatable :: pipecoefr(:)
   real, allocatable :: pipediamr(:)
end type internalFluxBoundaryWithPipes_t      
type(internalFluxBoundaryWithPipes_t), allocatable :: internalFluxBoundariesWithPipes(:)
integer :: numInternalFluxBoundariesWithPipes
integer :: ifwpCount ! index into the internalFluxBoundariesWithPipes array

integer, parameter :: specifiedFluxBoundaryTypes(5) = (/ 2, 12, 22, 32, 52 /)
integer :: nfluxf ! =1 if there are any specified flux boundaries in the mesh
   
!-----+---------+---------+---------+---------+---------+---------+
contains
!-----+---------+---------+---------+---------+---------+---------+  
  

!-----+---------+---------+---------+---------+---------+---------+
!  READ14_FindDims
!-----+---------+---------+---------+---------+---------+---------+
subroutine read14_findDims ()
implicit none
integer :: ios ! i/o status 
integer :: lineNum ! line number currently being read
integer :: i, j, k
integer, parameter :: iunit = 14
! initializations
lineNum = 1
numSimpleFluxBoundaries = 0
numExternalFluxBoundaries = 0 
numInternalFluxBoundaries = 0 
numInternalFluxBoundariesWithPipes = 0

read(iunit,'(A80)',err=10,end=20,iostat=ios) agrid
lineNum = lineNum + 1
write(6,'(A)') "INFO: Mesh file comment line: "//trim(agrid)
write(6,'(A)') "INFO: Reading mesh file dimensions."
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) ne, np
do k = 1, np
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) i
   lineNum = lineNum + 1
   if (i.ne.k) then
      write(6,'("ERROR: Attempted to read node number ",i0," but found node number ",i0," instead.")') k, i
      stop
   endif
enddo
do k = 1, ne
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) i
   lineNum = lineNum + 1
   if (i.ne.k) then
      write(6,'("ERROR: Attempted to read element number ",i0," but found element number ",i0," instead.")') k, i
      stop
   endif 
enddo
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nope  ! total number of elevation boundaries
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) neta  ! total number of nodes on elevation boundaries
lineNum = lineNum + 1 
neta_count = 0
allocate(nvdll(nope))
nvdll_max = 0
do k = 1, nope         
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvdll(k) ! number of nodes on the kth elevation boundary segment
   lineNum = lineNum + 1
   nvdll_max = max(nvdll_max,nvdll(k))
   do j = 1, nvdll(k)
      read(unit=iunit,fmt=*,err=10,end=20,iostat=ios)
      lineNum = lineNum + 1
      neta_count = neta_count + 1
   enddo
enddo
if ( neta_count.ne.neta ) then
   write(6,'("WARNING: Number of open boundary nodes was set to ",I6," but ",I6," were found.")') neta, neta_count
endif
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nbou ! total number of flux boundaries
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvel ! total number of nodes on flux boundaries
lineNum = lineNum + 1
nvel_count = 0
nvell_max = 0
allocate(nvell(nbou))
allocate(ibtype(nbou))
do k = 1, nbou
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvell(k), ibtype(k)  ! number of nodes and type of kth flux boundary 
   lineNum = lineNum + 1
   nvell_max = max(nvell_max,nvell(k))
   do j = 1, nvell(k)
      read(unit=iunit,fmt=*,err=10,end=20,iostat=ios)
      lineNum = lineNum + 1
      nvel_count = nvel_count + 1
   enddo
   ! count the total number of each type of boundary for later
   ! use in memory allocation
   select case(ibtype(k))
   case(0,1,2,10,11,12,20,21,22,30,52)
       numSimpleFluxBoundaries = numSimpleFluxBoundaries + 1
   case(3,13,23)
       numExternalFluxBoundaries = numExternalFluxBoundaries + 1 
   case(4,24)
       numInternalFluxBoundaries = numInternalFluxBoundaries + 1 
   case(5,25)
       numInternalFluxBoundariesWithPipes = numInternalFluxBoundariesWithPipes + 1
   case default
       write(6,'("ERROR: The boundary type ",I3," was found in the files but is not valid.")')
       stop
   end select
enddo
if ( nvel_count.ne.nvel) then
   write(6,'("WARNING: Number of land boundary nodes was set to ",I6," but ",I6," were found.")') nvel, nvel_count
   if (verbose.eqv..true.) then
      write(6,*) 'WARNING: Here is the summary of land boundary node information:'
      write(6,'("NVEL (specified number of land boundary nodes) = ",i0,".")') nvel
      write(6,'("Counted number of land boundary nodes = ",i0,".")') nvel_count
      do k=1,nbou
         write(6,'("ibtype(",i0,")=",i0,", nvell(",i0,")=",i0,", total=",i0,".")') k, ibtype(k), k, nvell(k), sum(nvell(1:k))
      end do
   endif
endif
rewind(iunit)
write(6,'(A)') 'INFO: Finished reading mesh file dimensions.'
return
   !
   ! jump to here on error and end conditions during read   
10 write(6,'("ERROR: Reading line ",i0," gave the following error code: ",i0,".")')         lineNum, ios
   close(iunit)
   stop
20 write(6,'("ERROR: Reached premature end of file on line ",i0,".")') lineNum
   close(iunit)
   stop
!-----+---------+---------+---------+---------+---------+---------+
end subroutine read14_findDims
!-----+---------+---------+---------+---------+---------+---------+

!-----+---------+---------+---------+---------+---------+---------+
! READ14
!-----+---------+---------+---------+---------+---------+---------+
subroutine read14 ()
implicit none
integer :: i, j, k, jn, je, nhy
integer, parameter :: iunit = 14
integer :: ios     ! i/o status
integer :: lineNum ! line number currently being read
!
! initialization
nfluxf = 0 
!
if (trim(meshFileName).eq."null") then
   write(6,'(a)',advance='no') "Enter name of the fort.14 file: "
   read(5,'(A)') meshFileName
endif
call openFileForRead(iunit,trim(meshFileName))
call read14_findDims ()
allocate(xyd(3,np)) ! node table
allocate(nm(ne,3))  ! element table
if (verbose.eqv..true.) then 
   write(6,'("Number of elevation specified boundaries (NOPE): ",i0,".")') nope
   write(6,'("Number of simple flux specified boundaries (0,1,2,etc): ",i0,".")') numSimpleFluxBoundaries
   write(6,'("Number of external flux boundaries (3,etc): ",i0,".")') numExternalFluxBoundaries         
   write(6,'("Number of internal flux boundaries (4,etc): ",i0,".")') numInternalFluxBoundaries
   write(6,'("Number of internal flux boundaries with pipes (5,etc): ",i0,".")') numInternalFluxBoundariesWithPipes
endif
allocate(elevationBoundaries(nope))
do i=1,nope
   allocate(elevationBoundaries(i)%nodes(nvdll(i)))
end do   
allocate(simpleFluxBoundaries(numSimpleFluxBoundaries))
allocate(externalFluxBoundaries(numExternalFluxBoundaries))
allocate(internalFluxBoundaries(numInternalFluxBoundaries))
allocate(internalFluxBoundariesWithPipes(numInternalFluxBoundariesWithPipes))
sfCount = 1
efCount = 1
ifCount = 1
ifwpCount = 1      
do i=1,nbou
   if (verbose.eqv..true.) then
      write(6,'("i=",i0)') i
   endif
   select case(ibtype(i))
   case(0,1,2,10,11,12,20,21,22,30,52)
      allocate(simpleFluxBoundaries(sfCount)%nodes(nvell(i)))
      sfCount = sfCount + 1
   case(3,13,23)
      allocate(externalFluxBoundaries(efCount)%nodes(nvell(i)))
      allocate(externalFluxBoundaries(efCount)%barlanht(nvell(i)))
      allocate(externalFluxBoundaries(efCount)%barlancfsp(nvell(i)))
      efCount = efCount + 1
   case(4,24)        
      allocate(internalFluxBoundaries(ifCount)%nodes(nvell(i)))
      allocate(internalFluxBoundaries(ifCount)%ibconn(nvell(i)))
      allocate(internalFluxBoundaries(ifCount)%barinht(nvell(i)))
      allocate(internalFluxBoundaries(ifCount)%barincfsb(nvell(i)))
      allocate(internalFluxBoundaries(ifCount)%barincfsp(nvell(i)))
      ifCount = ifCount + 1
   case(5,25)
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%nodes(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%ibconnr(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%barinhtr(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%barincfsbr(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%barincfspr(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%pipehtr(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%pipecoefr(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%pipediamr(nvell(i)))
      ifwpCount = ifwpCount + 1            
   case default
       write(6,'("ERROR: The boundary type ",I3," was found in the files but is not valid.")')
       stop
   end select
end do
write(6,'(A)') 'INFO: Reading mesh file coordinates, connectivity, and boundary data.'
lineNum = 1
read(unit=iunit,fmt='(a80)',err=10,end=20,iostat=ios) agrid
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) ne, np
lineNum = lineNum + 1
do k = 1, np
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) jn, (xyd(j,k), j=1,3)
   lineNum = lineNum + 1
enddo
do k = 1, ne
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) je, nhy, ( nm(k,j), j = 1, 3 )
   lineNum = lineNum + 1
enddo
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nope
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) neta
lineNum = lineNum + 1
do k = 1, nope
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvdll(k)
   lineNum = lineNum + 1
   elevationBoundaries(k)%indexNum = k
   do j = 1, nvdll(k)
      read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) elevationBoundaries(k)%nodes(j)
      lineNum = lineNum + 1
   enddo
enddo
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nbou
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvel
lineNum = lineNum + 1
sfCount = 1
efCount = 1
ifCount = 1
ifwpCount = 1      
do k = 1, nbou
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvell(k), ibtype(k)
   lineNum = lineNum + 1
   select case(ibtype(k))
   case(0,1,2,10,11,12,20,21,22,30,52)
      simpleFluxBoundaries(sfCount)%indexNum = k
      do j = 1, nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios)  &
            simpleFluxBoundaries(sfCount)%nodes(j)
         lineNum = lineNum + 1
      end do
      sfCount = sfCount + 1
   case(3,13,23)
      externalFluxBoundaries(efCount)%indexNum = k         
      do j = 1, nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) & 
                       externalFluxBoundaries(efCount)%nodes(j), &
                       externalFluxBoundaries(efCount)%barlanht(j), &
                       externalFluxBoundaries(efCount)%barlancfsp(j)
         lineNum = lineNum + 1
      end do
      efCount = efCount + 1
   case(4,24)
      internalFluxBoundaries(ifCount)%indexNum = k
      do j = 1, nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) &
                       internalFluxBoundaries(ifCount)%nodes(j), &
                       internalFluxBoundaries(ifCount)%ibconn(j), &
                       internalFluxBoundaries(ifCount)%barinht(j), &
                       internalFluxBoundaries(ifCount)%barincfsb(j), &
                       internalFluxBoundaries(ifCount)%barincfsp(j)
         lineNum = lineNum + 1
      end do
      ifCount = ifCount + 1
   case(5,25)
      internalFluxBoundaries(ifwpCount)%indexNum = k
      do j = 1, nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) &
                       internalFluxBoundariesWithPipes(ifCount)%nodes(j), &
                       internalFluxBoundariesWithPipes(ifCount)%ibconnr(j), &
                       internalFluxBoundariesWithPipes(ifCount)%barinhtr(j), &
                       internalFluxBoundariesWithPipes(ifCount)%barincfsbr(j), &
                       internalFluxBoundariesWithPipes(ifCount)%barincfspr(j), &
                       internalFluxBoundariesWithPipes(ifCount)%pipehtr(j), &
                       internalFluxBoundariesWithPipes(ifCount)%pipecoefr(j), &
                       internalFluxBoundariesWithPipes(ifCount)%pipediamr(j)
         lineNum = lineNum + 1                                           
      end do
      ifwpCount = ifwpCount + 1
   case default
      write(6,*) 'ERROR: IBTYPE ',ibtype(k),' is not allowed.'
      stop
   end select
end do
close(14)
write(6,'(A)') 'INFO: Finished reading mesh file coordinates, connectivity, and boundary data.'
return
      !
      ! jump to here on error and end conditions during read   
10    write(6,'("ERROR: Reading line ",i0," gave the following error code: ",i0,".")')         lineNum, ios
      close(iunit)
      stop
20    write(6,'("ERROR: Reached premature end of file on line ",i0,".")') lineNum
      close(iunit)
      stop
!-----+---------+---------+---------+---------+---------+---------+
end subroutine read14
!-----+---------+---------+---------+---------+---------+---------+

!-----+---------+---------+---------+---------+---------+---------+
!                   S U B R O U T I N E     
!  C O N S T R U C T   F L U X   B O U N D A R I E S   A R R A Y
!-----+---------+---------+---------+---------+---------+---------+
subroutine constructFluxBoundaryTypesArray()
implicit none
integer :: i
integer :: j
integer :: k
integer :: jgw
!
jgw = 1
allocate(lbcodei(nvel))
do k=1,nbou
   do j=1,nvell(k)
      lbcodei(jgw) = ibtype(k)
      jgw = jgw + 1
   end do
end do
! determine if there are any specified flux boundaries in the mesh
do i=1,size(specifiedFluxBoundaryTypes)
   if (any(lbcodei.eq.specifiedFluxBoundaryTypes(i))) then
      nfluxf = 1 ! => must find b.c.s in fort.15 or fort.20
      exit
   endif
end do
!-----+---------+---------+---------+---------+---------+---------+
end subroutine constructFluxBoundaryTypesArray
!-----+---------+---------+---------+---------+---------+---------+


!******************************************************************************
!                                                                             *
!      Subroutine to generate neighbor tables from a connectivity table.      *
!                                                                             *
!      NOTES                                                                  *
!      a node neighbor table is generated with the node itself is listed as   *
!         neighbor #1 and all other neighbors are sorted and placed in cw     *
!         order from east                                                     *
!      a neighbor element table is generated with:                            *
!         entry 1 = element # defined by neighbors 1,2,3                      *
!         entry 2 = element # defined by neighbors 1,3,4                      *
!         entry 3 = element # defined by neighbors 1,4,5                      *
!          .......                                                            *
!         entry last = element # defined by neighbors 1,nneigh,2              *
!         a zero area means that the defined triangle lies outside the domain *
!                                                                             *
!                                                                             *
!    v1.0   R.L.   6/29/99  used in 3D code                                   *
!    v2.0   R.L.   5/23/02  adapted to provide neighbor el table              *
!******************************************************************************
!                                                                             *
!     -  PARAMETERS WHICH MUST BE SET TO CONTROL THE DIMENSIONING OF ARRAYS   *
!           ARE AS FOLLOWS:                                                   *
!                                                                             *
!          MNP = MAXIMUM NUMBER OF NODAL POINTS                               *
!          MNE = MAXIMUM NUMBER OF ELEMENTS                                   *
!          MNEI= 1+MAXIMUM NUMBER OF NODES CONNECTED TO ANY ONE NODE IN THE   *
!                  FINITE ELEMENT GRID                                        *
!                                                                             *
!******************************************************************************
!                                                                             *
!    VARIABLE DEFINITIONS:                                                    *
!       NE - NUMBER OF ELEMENTS                                               *
!       NP - NUMBER OF NODES                                                  *
!       NM(MNE,3) - NODE NUMBERS ASSOCIATED WITH EACH ELEMENT                 *
!       NNeigh(MNP) NUMBER OF NEIGHBORS FOR EACH NODE                         *
!       NeiTab(MNP,NEIMAX) 2D ARRAY OF NEIGHBORS FOR EACH NODE                *
!       NeiTabEle(MNP,NEIMAX) 2D ARRAY OF NEIGHBOR ELEMENTS FOR EACH NODE     *
!       NEIMIN - 1+MINIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!       NEIMAX - 1+MAXIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!                                                                             *
!******************************************************************************
subroutine computeNeighborTable()
implicit none
double precision, allocatable :: angle(:)
integer, allocatable :: neitem(:)
integer, allocatable :: nneighele(:)
double precision :: anglelow
double precision :: anglemore
double precision :: delx
double precision :: dely
double precision :: dist
integer :: nn1, nn2, nn3 ! node numbers around an element
integer :: ne1, ne2, ne3 ! element numbers
integer :: i, j, jj, jlow, k, n  ! loop counters
!
! Initialization
if (cppComputed.eqv..false.) then
   call computeCPP()
endif
jlow = 0
ne2 = 0
ne3 = 0
!
! For interior nodes, the number of neighbor nodes around any node is
! equal to the number of elements that contain that node. Boundary nodes
! will have one additional neighboring node.
allocate(nneigh(np))
nneigh = 0
do i=1,ne
   do j=1,3
      ! count the number of elements that include each node
      nneigh(nm(i,j)) = nneigh(nm(i,j)) + 1 
   end do
end do
mnei = maxval(nneigh)
mnei = mnei + 2 ! +1 to include the node itself, +1 in case its a boundary node
!
allocate(neitab(np,mnei))
allocate(neitabele(np,mnei))
allocate(angle(mnei))
allocate(neitem(np))
allocate(nneighele(np))
neighborTableComputed = .true.
! initialize neighbor table to zeroes
NNeigh=0
NNeighEle=0
NeiTab=0
NeiTabEle=0
DO 10 N=1,NE
   NN1 = NM(N,1)
   NN2 = NM(N,2)
   NN3 = NM(N,3)
   NNeighEle(NN1)=NNeighEle(NN1)+1
   NNeighEle(NN2)=NNeighEle(NN2)+1
   NNeighEle(NN3)=NNeighEle(NN3)+1
   NeiTabEle(NN1,NNeighEle(NN1))=N
   NeiTabEle(NN2,NNeighEle(NN2))=N
   NeiTabEle(NN3,NNeighEle(NN3))=N

   DO J=1,NNeigh(NN1)
      IF(NN2.EQ.NeiTab(NN1,J)) GOTO 25
   END DO
   NNeigh(NN1)=NNeigh(NN1)+1
   NNeigh(NN2)=NNeigh(NN2)+1
   IF((NNeigh(NN1).GT.MNEI-1).OR.(NNeigh(NN2).GT.MNEI-1)) GOTO 999
   NeiTab(NN1,NNeigh(NN1))=NN2
   NeiTab(NN2,NNeigh(NN2))=NN1

25      CONTINUE
   DO J=1,NNeigh(NN1)
      IF(NN3.EQ.NeiTab(NN1,J)) GOTO 35
   END DO
   NNeigh(NN1)=NNeigh(NN1)+1
   NNeigh(NN3)=NNeigh(NN3)+1
   IF((NNeigh(NN1).GT.MNEI-1).OR.(NNeigh(NN3).GT.MNEI-1)) GOTO 999
   NeiTab(NN1,NNeigh(NN1))=NN3
   NeiTab(NN3,NNeigh(NN3))=NN1

35      CONTINUE
   DO J=1,NNeigh(NN2)
      IF(NN3.EQ.NeiTab(NN2,J)) GOTO 10
   END DO
   NNeigh(NN2)=NNeigh(NN2)+1
   NNeigh(NN3)=NNeigh(NN3)+1
   IF((NNeigh(NN2).GT.MNEI-1).OR.(NNeigh(NN3).GT.MNEI-1)) GOTO 999
   NeiTab(NN2,NNeigh(NN2))=NN3
   NeiTab(NN3,NNeigh(NN3))=NN2

10   CONTINUE
!
!     INSERT NODE ITSELF IN PLACE #1 and SORT other NEIGHBORS by
!     increasing cw angle from East
!
DO I=1,NP
   DO J=1,NNeigh(I)
      NEITEM(J)=NeiTab(I,J)
      DELX=x_cpp(NEITEM(J))-x_cpp(I)
      DELY=y_cpp(NEITEM(J))-y_cpp(I)
      DIST=SQRT(DELX*DELX+DELY*DELY)
      IF(DIST.EQ.0.0d0) GOTO 998
      IF(DELY.NE.0.0d0) THEN
         ANGLE(J)=RAD2DEG*ACOS(DELX/DIST)
         IF(DELY.GT.0.0) ANGLE(J)=360.0d0-ANGLE(J)
      ENDIF
      IF(DELY.EQ.0.0d0) THEN
         IF(DELX.GT.0.0d0) ANGLE(J)=0.0d0
         IF(DELX.LT.0.d0) ANGLE(J)=180.0d0
      ENDIF
   END DO
   ANGLEMORE=-1.d0
   DO JJ=1,NNeigh(I)
      ANGLELOW=400.d0
      DO J=1,NNeigh(I)
         IF((ANGLE(J).LT.ANGLELOW).AND.(ANGLE(J).GT.ANGLEMORE)) THEN
            ANGLELOW=ANGLE(J)
            JLOW=J
         ENDIF
      END DO
      NeiTab(I,JJ+1)=NEITEM(JLOW)
      ANGLEMORE=ANGLELOW
   END DO
   NeiTab(I,1)=I
   NNeigh(I)=NNeigh(I)+1
ENDDO
!
!     MATCH EACH SET OF 3 NODES WITH CORRESPONDING ELEMENT AND REORDER
!     ELEMENTS ACCORDINGLY
!
DO I=1,NP
   DO K=1,NNeighEle(I)
      NEITEM(K)=NeiTabEle(I,K)
      NeiTabEle(I,K)=0
   END DO
   DO J=2,NNeigh(I)
      NN1=NeiTab(I,1)
      NN3=NeiTab(I,J)
      IF(J.NE.NNeigh(I)) NN2=NeiTab(I,J+1)
      IF(J.EQ.NNeigh(I)) NN2=NeiTab(I,2)
      DO K=1,NNeighEle(I)
         IF(NEITEM(K).NE.0) THEN
            IF(NM(NEITEM(K),1).EQ.NN1) THEN
               NE1=NM(NEITEM(K),1)
               NE2=NM(NEITEM(K),2)
               NE3=NM(NEITEM(K),3)
            ENDIF
            IF(NM(NEITEM(K),2).EQ.NN1) THEN
               NE1=NM(NEITEM(K),2)
               NE2=NM(NEITEM(K),3)
               NE3=NM(NEITEM(K),1)
            ENDIF
            IF(NM(NEITEM(K),3).EQ.NN1) THEN
               NE1=NM(NEITEM(K),3)
               NE2=NM(NEITEM(K),1)
               NE3=NM(NEITEM(K),2)
            ENDIF
            IF((NE2.EQ.NN2).AND.(NE3.EQ.NN3)) THEN
               NeiTabEle(I,J-1)=NEITEM(K)
               NEITEM(K)=0
            ENDIF
         ENDIF
      END DO
   END DO
END DO
!
!  DETERMINE THE MAXIMUM AND MINIMUM NUMBER OF NEIGHBORS
NEIMAX = maxval(NNeigh)
NEIMIN = minval(NNeigh)
!  Deallocate local work arrays
DEALLOCATE ( ANGLE )
DEALLOCATE ( NEITEM )
DEALLOCATE ( NNEIGHELE )
RETURN

999  CONTINUE
WRITE(6,*) 'ERROR: Computation of neighbor table failed.'
STOP
998  CONTINUE
WRITE(6,*) 'ERROR: Nodes ',I,' and ',NEITEM(J),' have the same coordinates.'
STOP
!-----------------------------------------------------------------------
END SUBROUTINE computeNeighborTable
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                         S U B R O U T I N E   
!   C O M P U T E   N E I G H B O R   E D G E   L E N G T H   T A B L E 
!-----------------------------------------------------------------------
!     jgf: Very short subroutine to compute the length of each edge
!     attached to a node. 
!-----------------------------------------------------------------------
subroutine computeNeighborEdgeLengthTable()
implicit none
integer i,j
allocate(neighborEdgeLengthTable(np,neimax))
neighborEdgeLengthTableComputed = .true.
do i=1,np
   neighborEdgeLengthTable(i,1) = 0.d0 ! distance from this node to itself...
   do j=2,nneigh(i)
      neighborEdgeLengthTable(i,j) = sqrt( (x_cpp(i)-x_cpp(NeiTab(i,j)))**2 &
                  + (y_cpp(i)-y_cpp(NeiTab(i,j)))**2 )
   end do
end do
!-----------------------------------------------------------------------
end subroutine computeNeighborEdgeLengthTable
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                         S U B R O U T I N E   
!     C O M P U T E  A L B E R S   E Q U A L   A R E A   C O N I C
!-----------------------------------------------------------------------
!     jgf: Very short subroutine to compute the albers equal area
!     conic projection of the mesh node locations, allocating memory 
!     in the process if necessary, and not overwriting the 
!     original lat/lon data.
!-----------------------------------------------------------------------
SUBROUTINE computeAlbersEqualAreaConic()
IMPLICIT NONE
real(8), parameter :: PHI1 = 29.5d0
real(8), parameter :: PHI2 = 45.5d0       
real(8), parameter :: LON0 = -96.0d0                 
real(8), parameter :: LAT0 = 23.0d0                 
real(8), parameter :: EE = 0.0066943800229d0
real(8), parameter :: E = 0.0818191910428d0 
! 
! variables used in conversion of adcirc mesh point locations
! (in geographic projection) to Albers Equal Area Conic
real(8) :: alpha
real(8) :: alpha0
real(8) :: alpha1
real(8) :: alpha2
real(8) :: alphaa
real(8) :: alphab
real(8) :: alphac
real(8) :: c
real(8) :: e1
real(8) :: m1
real(8) :: m2
real(8) :: o
real(8) :: p
real(8) :: rho
real(8) :: rho0
real(8) :: theta
real(8) :: n
integer :: i ! loop counter
!
if (albersComputed.eqv..false.) then
   allocate(xalbers(np),yalbers(np))
   albersComputed = .true.
endif
write(6,'("INFO: Generating Albers Equal Area Conic coordinates.")')
e1 = 1.d0-ee                                            
o = (0.5d0/e)*e1
m1 = cos(phi1*deg2rad)/(1.d0-ee*sin(phi1*deg2rad)**2)**0.5d0
m2 = cos(phi2*deg2rad)/(1.d0-ee*sin(phi2*deg2rad)**2)**0.5d0
alphaa = (0.5d0/e)*log((1.d0-e*sin(lat0*deg2rad))/(1.d0+e*sin(lat0*deg2rad)))
alpha0 = e1*sin(lat0*deg2rad)/(1.d0-ee*dsin(lat0*deg2rad)**2)-e1*alphaa 
alphab = (0.5d0/e)*log((1.d0-e*sin(phi1*deg2rad))/(1.d0+e*sin(phi1*deg2rad)))
alpha1 = e1*sin(phi1*deg2rad)/(1.d0-ee*dsin(phi1*deg2rad)**2)-e1*alphab
alphac = (0.5d0/e)*log((1.d0-e*sin(phi2*deg2rad))/(1.d0+e*sin(phi2*deg2rad)))
alpha2 = e1*sin(phi2*deg2rad)/(1.d0-ee*dsin(phi2*deg2rad)**2)-e1*alphac
n = (m1**2-m2**2)/(alpha2-alpha1)
c = m1**2+n*alpha1
rho0 = r*((c-n*alpha0)**(0.5d0))/n
do i=1,np
   ! project mesh node locations into albers equal area conic
   P = O*DLOG((1.d0-E*DSIN(xyd(2,i)*deg2rad))/(1.d0+E*DSIN(xyd(2,i)*deg2rad)))
   ALPHA = E1*DSIN(xyd(2,i)*deg2rad)/(1.d0-EE*DSIN(xyd(2,i)*deg2rad)**2)-P      
   THETA = N*(xyd(1,i)-LON0) 
   RHO = R*(sqrt(C-N*ALPHA))/N
   xalbers(i) = RHO*DSIN(THETA*deg2rad)
   yalbers(i) = RHO0-RHO*DCOS(THETA*deg2rad)
end do
write(6,'("INFO: Finished generating Albers Equal Area Conic coordinates.")')
return
!-----------------------------------------------------------------------
END SUBROUTINE computeAlbersEqualAreaConic
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     S U B R O U T I N E   C O M P U T E  C P P 
!-----------------------------------------------------------------------
!     jgf: Very short subroutine to compute the CPP projection, 
!     allocating memory in the process, and not overwriting the 
!     original lat/lon data.
!-----------------------------------------------------------------------
      SUBROUTINE computeCPP()
      IMPLICIT NONE
      if (cppComputed.eqv..false.) then
         allocate(x_cpp(np),y_cpp(np))
         cppComputed = .true.
      endif
      write(6,'("INFO: Generating CPP coordinates.")')
      x_cpp = R * (xyd(1,:)*deg2rad - slam0*deg2rad) * cos(sfea0*deg2rad)
      y_cpp = xyd(2,:)*deg2rad * R
      return
!-----------------------------------------------------------------------
      END SUBROUTINE computeCPP
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     S U B R O U T I N E   O P E N  F I L E  F O R  R E A D
!-----------------------------------------------------------------------
!     jgf: Added general subroutine for opening an existing
!     file for reading. Includes error checking.
!-----------------------------------------------------------------------
   SUBROUTINE openFileForRead(lun, filename)
      IMPLICIT NONE
      INTEGER, intent(in) :: lun   ! fortran logical unit number
      CHARACTER(*), intent(in) :: filename ! full pathname of file
      INTEGER :: errorIO  ! zero if the file opened successfully
       errorIO = 0
!
!     Check to see if file exists
      call checkFileExistence(filename, errorIO)
      if ( errorIO.ne.0) then
         stop
      endif
!
!     Open existing file
      OPEN(lun,FILE=trim(filename),STATUS='OLD',ACTION='READ',IOSTAT=errorIO)
      if (errorIO.ne.0) then
          write(6,'("ERROR: Could not open the file ",A,".")') trim(filename)
          stop
      else
         write(6,'("INFO: The file ",A," was opened successfully.")') trim(filename)
      endif
      return
!-----------------------------------------------------------------------
   END SUBROUTINE openFileForRead
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!     S U B R O U T I N E   C H E C K   F I L E   E X I S T E N C E
!-----------------------------------------------------------------------
!     jgf: Just check for the existence of a file. I separated this 
!     from openFileForRead so that I could use it on NetCDF files 
!     as well.  
!-----------------------------------------------------------------------
   SUBROUTINE checkFileExistence(filename, errorIO)
      IMPLICIT NONE
      CHARACTER(*), intent(in) :: filename ! full pathname of file
      INTEGER, intent(out) :: errorIO  ! zero if the file opened successfully
      LOGICAL :: fileFound    ! .true. if the file is present
      errorIO = 0
!
!     Check to see if file exists
      write(6,'("INFO: Searching for file ",A," ...")') trim(filename)
      inquire(FILE=filename,EXIST=fileFound)
      if (fileFound.eqv..false.) then
         write(6,'("ERROR: The file ",A," was not found.")') trim(filename)
      else
         write(6,'("INFO: The file ",A," was found.")') trim(filename)
      endif
      return
!-----------------------------------------------------------------------
   END SUBROUTINE checkFileExistence
!-----------------------------------------------------------------------


!----------------------------------------------------------------------
!  CHECK
!---------------------------------------------------------------------
   SUBROUTINE Check(ncStatus)
      USE netcdf
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: ncStatus
      IF(ncStatus.NE.NF90_NOERR)THEN
         WRITE(*,'(A,A)') "ERROR: ",TRIM(NF90_STRERROR(ncStatus))
         ERROR STOP 1
      ENDIF
   END SUBROUTINE check
!-----+---------+---------+---------+---------+---------+---------+
   end module adcmesh
!-----+---------+---------+---------+---------+---------+---------+

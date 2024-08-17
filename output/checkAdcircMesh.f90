!--------------------------------------------------------------------------
! checkAdcircMesh.f90
!
! A program to find the following ADCIRC mesh anomalies:
!    (a) boundaries that are only two nodes long
!    (b) levees that are below the height of the local topography
!    (c) overlapping elements
!    (d) disjoint nodes
!    (e) nodes that have too many or too few connected elements for SWAN
!    (f) nodes repeated within a boundary (unless it is a closed boundary
!        and last node is the same as first)
!    (g) compute max time step assuming 1m total water depth in normally
!        dry areas
!
!--------------------------------------------------------------------------
! Copyright(C) 2013--2016 Jason Fleming
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

!-----+---------+---------+---------+---------+---------+---------+
!
!   P R O G R A M    C H E C K   A D C I R C   M E S H
!
!-----+---------+---------+---------+---------+---------+---------+
program checkAdcircMesh
use adcmesh
use ioutil
use logging
use netcdf
implicit none
type(mesh_t) :: m
type(meshNetCDF_t) :: n
integer :: nodeNumber
logical :: ok  ! .true. if a test passes
real :: leveeHeight
logical :: fileFound ! .true. if the file is already present
integer :: nodeNumbers(2)
logical :: shortBoundary
!
real(8) :: dx_crit ! used to create a bounding box around a high resolution area
logical :: negativeDepthsFound ! true if the mesh contains dry land areas
!
! The following are used to keep track of nodal connectedness to elements
! as it relates to SWAN's requirements.
!
! number of nodes with too few connected elements to meet SWAN's criterion
integer :: lowConnectedNodes
!
! number of nodes with too many connected elements to meet SWAN's criterion
integer :: highConnectedNodes
!
! set .true. if there are nodes with too many or not enough connected
! elements for SWAN; this will cause
logical :: writeNNeighEle
!
! The following is used to find disjoint nodes.
logical, allocatable :: used(:) ! (np) .true. if a node is resident on any element
real(8), allocatable :: nodeDistSquared(:,:) ! distance between any two nodes in the mesh (m)
real(8) :: minEdgeLengthSquared
!
! .true. if element number found in common between two nodes more than twice
logical, allocatable :: overlappingElements(:)
!
! .true. if the as-generated and after-sorting neighbor tables should be
! written out to ascii text files
logical :: writeNeighborTables
!
logical :: neitabContainsUninitializedValues ! .true. if erroneous -99 or 0 is found
logical :: neiTabEleContainsUninitializedValues ! .true. if erroneous -99 or 0 is found
!
! .true. if the node IDs should be written to a file that looks like
! a fort.63; this is valuable for visualization with ParaView because
! ParaView assumes 0-index ordering which is clumsy for labelling
! node and cell IDs
logical :: writeNodeIDs
!
! .true. if the cell IDs should be written to a file that looks like
! a noff.100 (like fort.63 but for elements); this is valuable for
! visualization with ParaView because ParaView assumes 0-index ordering
! which is clumsy for labelling node and cell IDs
logical :: writeElementIDs
!
! .true. if the areas of the elements in m^2 should be written
! to a file formatted like a noff.100
logical :: writeElementAreas
!
! .true. if the largest element area gradient (between that
! element and its neighbors) should be written
! to a file formatted like a noff.100
logical :: writeElementAreaGradients
!
! .true. if the edge length gradient (between that element and
! its neighboring elements) should be written
! to a file formatted like a noff.100
logical :: writeElementEdgeLengthGradients
!
! .true. if the minimum edge length around a node should
! be written (in m) in a file formatted like a fort.63
logical :: writeMinEdgeLengths
!
! .true. if the edge length gradient around a node should
! be written (in m) in a file formatted like a fort.63
logical :: writeEdgeLengthGradients
!
logical :: computeMaxTimestepSizes ! true to compute max time step at different levels of inundation
real(8), allocatable :: maxTimeStepSizes(:,:) ! (np,nincr) maximum time step at each node (seconds) for different levels of inundation
real(8) :: minTimeStepSize ! smallest time step constraint (s) at a particular level of inundation
real(8) :: effectiveDepth ! total water column depth when checking courant number
real(8) :: minEdge ! shortest edge a node is connected to (m)
real(8) :: inundationDepth ! total water column depth (m) used to check courant number in normally dry areas
real(8) :: inundationDepthIncrement ! how much depth (m) to add each time courant number is checked
real(8) :: maxInundationDepth ! greatest inundation (m) to consider for courant number check
logical :: inundationAboveLocalGround ! true if the inundation used to check courant number is relative to local ground level
!
logical :: checkDryElementArea ! true if the area of elements above msl should be checked
real(8) :: dt  ! used to compute the constraint on element size for elements above msl
real(8) :: tau0  ! used to compute the constraint on element size for elements above msl
real(8) :: minArea ! minimum area for a dry element at this topographic height
real(8) :: MsFacLOnDiag ! term in equation for minimum dry element area
real(8) :: onDiag ! multiplier for fully consistent diagonal terms in GWCE LHS
real(8) :: areaIE ! element area (m2)
real(8) :: areaIE4 ! 4x element area (m2)
real(8) :: dpAvg ! average bathymetric depth (m) of an element
real(8) :: g ! gravitational acceleration (m/s2)
real(8) :: a00 ! adcirc time weighting coefficient
real(8) :: GA00DPAvgOAreaIE4 ! term in equation for minimum dry element area
real(8) :: pMinArea(3) ! minimum element area (m2) constraint considering each of 3 nodes
integer, allocatable :: areaOK(:) ! 1 if dry element area is ok, 0 if not, -99999 if element is normally wet
!
logical :: computeImplicitDiagonalCoefficient ! true if coef(:,:) should be computed
real(8), allocatable :: coef(:,:) ! adcirc implicit lhs matrix
real(8) :: ep ! scaling factor for gwce
!
integer :: residentNodes(4) ! node numbers around an element, indices wrap around
integer :: e ! element counter
integer :: l ! counter for nodes around an element
integer :: i1, i2 ! counters for elements around a node
integer :: e1, e2 ! element number neighboring a node
integer :: icount ! counter for number of times two nodes have an element in common
!
integer :: i, j, k
integer :: ie ! element loop counter
!
! initializations
if (loggingInitialized.eqv..false.) then
   call initLogging(availableUnitNumber(),'checkAdcircMesh.f90')
endif
verbose = .false.
writeNNeighEle = .false.
writeNeighborTables = .false.
writeNodeIDs = .false.
writeMinEdgeLengths = .false.
writeEdgeLengthGradients = .false.
writeElementIDs = .false.
writeElementAreas = .false.
writeElementAreaGradients = .false.
writeElementEdgeLengthGradients = .false.
computeMaxTimestepSizes = .false.
computeImplicitDiagonalCoefficient = .false.
inundationAboveLocalGround = .false.
checkDryElementArea = .false.
dx_crit = 1750.d0
dt = 1.d0
tau0 = 0.03d0
m%sfea0 = 30.d0
a00 = 0.35d0  ! implicit mode
onDiag = 2.d0 ! fully consistent LHS
g = 9.81d0    ! gravitational acceleration
!
! Report netcdf version
write(6,*) "INFO: checkAdcircMesh was compiled with the following netcdf library: ", trim(nf90_inq_libvers())

! Process command line options
argcount = iargc() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
         case("--meshfile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            m%meshFileName = trim(cmdlinearg)
         case("--dx-crit")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) dx_crit
         case("--dt")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) dt
         case("--tau0")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) tau0
         case("--sfea0")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) m%sfea0
         case("--slam0")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) m%slam0
         case("--write-neighbor-tables")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeNeighborTables = .true.
         case("--write-element-ids")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeElementIDs = .true.
         case("--write-node-ids")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeNodeIDs = .true.
         case("--write-min-edge-lengths")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeMinEdgeLengths = .true.
         case("--write-edge-length-gradients")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeEdgeLengthGradients = .true.
         case("--write-element-areas")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeElementAreas = .true.
         case("--write-element-area-gradients")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeElementAreaGradients = .true.
         case("--write-element-edge-length-gradients")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeElementEdgeLengthGradients = .true.
         case("--write-nneighele")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeNNeighEle = .true.
         case("--check-dry-element-area")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            checkDryElementArea = .true.
         case("--compute-max-timestep-sizes")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            computeMaxTimestepSizes = .true.
         case("--compute-implicit-diagonal-coefficient")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            computeImplicitDiagonalCoefficient = .true.
         case("--verbose")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            verbose = .true.
         case default
            write(6,'(a,a,a)') 'WARNING: Command line option "',TRIM(cmdlineopt),'" was not recognized.'
      end select
   end do
end if
!
! Load fort.14
call read14(m)
write(6,'("INFO: Computing nodal and elemental neighbor tables.")')
call computeNeighborTable(m)
write(6,'("INFO: Computing the neighbor edge length table and finding the minimum edge length.")')
call computeNeighborEdgeLengthTable(m)
!
!              W R I T E   N O D E   I D S
!
! Writing node IDs is useful for labelling IDs in ParaView b/c
! ParaView assumes 0-indexing and ADCIRC uses 1-indexing
if (writeNodeIDs.eqv..true.) then
   write(6,'("INFO: Writing nodeids.63 file.")')
   open(11,file='nodeids.63',status='replace',action='write')
   ! write header info
   write(11,'(a)') trim(m%agrid)
   write(11,1010) 1, m%np, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   ! write node IDs
   do i=1,m%np
      write(11,'(20(i0,2x))') i, i
   end do
   close(11)
endif
!
!             W R I T E   E L E M E N T   I D S
!
! Writing element IDs is useful for labelling IDs in ParaView b/c
! ParaView assumes 0-indexing and ADCIRC uses 1-indexing
if (writeElementIDs.eqv..true.) then
   write(6,'("INFO: Writing elementids.100 file.")')
   open(11,file='elementids.100',status='replace',action='write')
   ! write header info
   write(11,'(a)') trim(m%agrid)
   write(11,1010) 1, m%ne, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   ! write element IDs
   do i=1,m%ne
      write(11,'(20(i0,2x))') i, i
   end do
   close(11)
endif
!
!         W R I T E   M I N   E D G E   L E N G T H S
!
if (writeMinEdgeLengths.eqv..true.) then
   write(6,'("INFO: Writing minedgelengths.63 file.")')
   call computeNeighborEdgeLengthTable(m)
   open(11,file='minedgelengths.63',status='replace',action='write')
   ! write header info
   write(11,'(a)') trim(m%agrid)
   write(11,1010) 1, m%np, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   ! write min edge length
   do i=1,m%np
      write(11,'(i0,1x,f15.7)') i, m%minEdgeLengths(i)
   end do
   close(11)
endif
!
!     W R I T E   E D G E   L E N G T H   G R A D I E N T S
!
if (writeEdgeLengthGradients.eqv..true.) then
   write(6,'("INFO: Writing edgelengthgradients.63 file.")')
   call computeNeighborEdgeLengthTable(m)
   open(11,file='edgelengthgradients.63',status='replace',action='write')
   ! write header info
   write(11,'(a)') trim(m%agrid)
   write(11,1010) 1, m%np, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   ! write min edge length
   do i=1,m%np
      write(11,'(i0,1x,f15.7)') i, m%edgeLengthGradients(i)
   end do
   close(11)
endif
!
!        W R I T E   E L E M E N T   A R E A S
!
if (writeElementAreas.eqv..true.) then
   write(6,'("INFO: Writing elementareas.100 file.")')
   call compute2xAreas(m)
   open(11,file='elementareas.100',status='replace',action='write')
   ! write header info
   write(11,'(a)') trim(m%agrid)
   write(11,1010) 1, m%ne, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   ! write element areas
   do ie=1,m%ne
      write(11,'(i0,1x,f15.7)') ie, 0.5d0*m%areas(ie)
   end do
   close(11)
endif
!
!    C R E A T E   M E S H   P R O P E R T I E S   F I L E
!
! Create mesh.properties file and bounding box properties
write(6,'("INFO: Creating mesh.properties file to record the properties of this mesh.")')
inquire(file=trim(adjustl(m%meshfileName))//'.properties',exist=fileFound)
if (fileFound.eqv..true.) then
   open(12,file=trim(adjustl(m%meshfileName))//'.properties',status='old',position='append',action='write')
else
   open(12,file=trim(adjustl(m%meshfileName))//'.properties',status='new',action='write')
endif
write(6,'("INFO: Recording full domain extents.")')
! report the domain extents
write(12,'("fullDomainMinimumLongitudeDegrees : ",f15.7)') minval(m%xyd(1,:))
write(12,'("fullDomainMaximumLongitudeDegrees : ",f15.7)') maxval(m%xyd(1,:))
write(12,'("fullDomainMinimumLatitudeDegrees : ",f15.7)') minval(m%xyd(2,:))
write(12,'("fullDomainMaximumLatitudeDegrees : ",f15.7)') maxval(m%xyd(2,:))
! report the lat and lon boundaries of the negative depths
m%lonmin = huge(1.d0)
m%lonmax = tiny(1.d0)
m%latmin = huge(1.d0)
m%latmax = tiny(1.d0)
negativeDepthsFound = .false.
do i=1,m%np
   if (m%xyd(3,i).lt.0.d0) then
      negativeDepthsFound = .true.
      m%lonmin = min(m%lonmin,m%xyd(1,i))
      m%lonmax = max(m%lonmax,m%xyd(1,i))
      m%latmin = min(m%latmin,m%xyd(2,i))
      m%latmax = max(m%latmax,m%xyd(2,i))
   endif
end do
if (negativeDepthsFound.eqv..false.) then
   m%lonmin = -99999.d0
   m%lonmax = -99999.d0
   m%latmin = -99999.d0
   m%latmax = -99999.d0
endif
write(12,'("fullDomainNegativeDepthMinimumLongitudeDegrees : ",f20.7)') m%lonmin
write(12,'("fullDomainNegativeDepthMaximumLongitudeDegrees : ",f20.7)') m%lonmax
write(12,'("fullDomainNegativeDepthMinimumLatitudeDegrees : ",f20.7)') m%latmin
write(12,'("fullDomainNegativeDepthMaximumLatitudeDegrees : ",f20.7)') m%latmax
!
write(6,'("INFO: Recording bounding box for high resolution area.")')
write(12,'("highResolutionRegionMaximumEdgeLengthMeters : ",f15.7)') dx_crit
m%lonmin = huge(1.d0)
m%lonmax = tiny(1.d0)
m%latmin = huge(1.d0)
m%latmax = tiny(1.d0)
do i=1,m%np
   do j=2,m%nneigh(i)
      if ( m%edgeLengthTable(i,j).lt.dx_crit ) then
         m%lonmin = min(m%lonmin,m%xyd(1,i))
         m%lonmax = max(m%lonmax,m%xyd(1,i))
         m%latmin = min(m%latmin,m%xyd(2,i))
         m%latmax = max(m%latmax,m%xyd(2,i))
         exit
      endif
   end do
end do
write(12,'("fullDomainHighResolutionRegionMinimumLongitudeDegrees : ",f15.7)') m%lonmin
write(12,'("fullDomainHighResolutionRegionMaximumLongitudeDegrees : ",f15.7)') m%lonmax
write(12,'("fullDomainHighResolutionRegionMinimumLatitudeDegrees : ",f15.7)') m%latmin
write(12,'("fullDomainHighResolutionRegionMaximumLatitudeDegrees : ",f15.7)') m%latmax
!
write(6,'("INFO: The minimum edge length is ",f15.7," meters at node ",i0,".")') m%minEdgeLength,m%minEdgeLengthNode
write(12,'("minimumEdgeLengthMeters : ",f15.7)') m%minEdgeLength
write(6,'("INFO: The maximum edge length is ",f15.7," meters at node ",i0,".")') m%maxEdgeLength,m%maxEdgeLengthNode
write(12,'("maximumEdgeLengthMeters : ",f15.7)') m%maxEdgeLength
! leave unit 12 open so we can write more mesh properties to it...
!
!         W R I T E   N E I G H B O R   T A B L E S
!
if (writeNeighborTables.eqv..true.) then
   write(6,'("INFO: Writing neitab.generated file.")')
   open(11,file='neitab.generated',status='replace')
   do i=1,m%np
      write(11,'(20(i0,2x))') i, (m%neiTabGenerated(i,j),j=1,m%nneigh(i))
   end do
   close(11)
   write(6,'("INFO: Writing neitab.sorted file.")')
   open(11,file='neitab.sorted',status='replace')
   do i=1,m%np
      write(11,'(20(i0,2x))') i, (m%neiTab(i,j),j=1,m%nneigh(i))
   end do
   close(11)
   write(6,'("INFO: Writing neitabele.generated file.")')
   open(11,file='neitabele.generated',status='replace')
   do i=1,m%np
      write(11,'(20(i0,2x))') i, (m%neiTabEleGenerated(i,j),j=1,m%nneighele(i))
   end do
   close(11)
   write(6,'("INFO: Writing neitabele.sorted file.")')
   open(11,file='neitabele.sorted',status='replace')
   do i=1,m%np
      write(11,'(20(i0,2x))') i, (m%neiTabEle(i,j),j=1,m%nneighele(i))
   end do
   close(11)
endif

!
!    R E P O R T   N O D E S   W I T H   L E S S   T H A N   4
! O R   M O R E   T H A N   1 0   C O N N E C T E D   E L E M E N T S
!                  ( B A D   F O R   S W A N  )
!
lowConnectedNodes = count(m%nNeighEle.lt.4)
highConnectedNodes = count(m%nNeighEle.gt.10)
if (lowConnectedNodes.gt.0) then
   write(6,'("WARNING: There are ",i0," nodes with less than 4 connected elements. This will cause SWAN to generate an error messaage.")') lowConnectedNodes
   writeNNeighEle = .true.
endif
if (highConnectedNodes.gt.0) then
   write(6,'("ERROR: There are ",i0," nodes with more than 10 connected elements. This will cause SWAN to generate an error message and will also cause internal memory corruption in SWAN.")') highConnectedNodes
   writeNNeighEle = .true.
endif
if (writeNNeighEle.eqv..true.) then
   write(6,'("INFO: Writing nneighele.63 file to record the number of neighboring elements around each node.")')
   ! open the ascii adcirc file that will hold the data
   open(11,file='nneighele.63',status='replace',action='write')
   ! write header info
   write(11,'(a)') trim(m%agrid)
   write(11,1010) 1, m%np, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   do i=1,m%np
      write(11,2452) i,m%nNeighEle(i)
   end do
else
   write(6,'("INFO: All nodes have at least 4 and no more than 10 connected elements. These are important criteria for SWAN.")')
endif
close(11)
 1010 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,'FileFmtVersion: ',I10)
 2120 FORMAT(2X,1pE20.10E3,5X,I10)
 2453 FORMAT(2x, i8, 2x, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3)
 2452 FORMAT(2x, i8, 2x, i0, 5x, i0, 5x, i0, 5x, i0)
!
!   C H E C K   L E V E E   H E I G H T S
!
m%allLeveesOK = .true.
do k=1,m%numExternalFluxBoundaries
   do j=1,size(m%externalFluxBoundaries(k)%nodes)
      nodeNumber = m%externalFluxBoundaries(k)%nodes(j)
      leveeHeight = m%externalFluxBoundaries(k)%barlanht(j)
      call checkLeveeHeightAtNode(m, nodeNumber, leveeHeight, ok)
      if (ok.eqv..false.) then
         write(6,'("There is an error on land boundary number ",i0," (external flux boundary).")') m%externalFluxBoundaries(k)%indexNum
      endif
   end do
end do
do k=1,m%numInternalFluxBoundaries
   do j=1,size(m%internalFluxBoundaries(k)%nodes)
      nodeNumbers(1) = m%internalFluxBoundaries(k)%nodes(j)
      nodeNumbers(2) = m%internalFluxBoundaries(k)%ibconn(j)
      leveeHeight = m%internalFluxBoundaries(k)%barinht(j)
      do i=1,2
         call checkLeveeHeightAtNode(m, nodeNumbers(i), leveeHeight, ok)
         if (ok.eqv..false.) then
            write(6,'("There is an error on land boundary number ",i0," (internal flux boundary).")') m%internalFluxBoundaries(k)%indexNum
            write(6,'("Node ",i0," is paired with node ",i0,".")') nodeNumbers(1), nodeNumbers(2)
         endif
      end do
   end do
end do
do k=1,m%numInternalFluxBoundariesWithPipes
   do j=1,size(m%internalFluxBoundariesWithPipes(k)%nodes)
      nodeNumbers(1) = m%internalFluxBoundariesWithPipes(k)%nodes(j)
      nodeNumbers(2) = m%internalFluxBoundariesWithPipes(k)%ibconn(j)
      leveeHeight = m%internalFluxBoundariesWithPipes(k)%barinht(j)
      do i=1,2
         call checkLeveeHeightAtNode(m, nodeNumbers(i), leveeHeight, ok)
         if (ok.eqv..false.) then
            write(6,'("There is an error on land boundary number ",i0," (internal flux boundary with pipes).")') m%internalFluxBoundaries(k)%indexNum
            write(6,'("Node ",i0," is paired with node ",i0,".")') nodeNumbers(1), nodeNumbers(2)
         endif
      end do
   end do
end do
if (m%allLeveesOK.eqv..true.) then
   write(6,'("INFO: Successfully tested levee heights: all levee heights are above the bathy/topo elevation at that node, as expected.")')
endif
!
!  C H E C K   F O  R   S H O R T   B O U N D A R I E S
!
! check to see if any boundary is two nodes long, and if so,
! output the data for that boundary
if (any(m%nvdll.lt.3)) then
   write(6,'("WARNING: The mesh file contains ",i0," elevation specified boundaries that are less than three nodes long.")') count(m%nvdll.lt.3)
   write(6,'("INFO: Identifying information for these short boundaries will now be written to the short_elevation_boundaries.txt file.")')
   open(11,file='short_elevation_boundaries.txt',status='replace',action='write')
   do i=1, m%nope
      if ( m%nvdll(i).lt.3 ) then
         shortBoundary = .true.
         write(11,'("WARNING: The mesh file ",a," has an open boundary (number ",i0,") that consists of only 2 nodes:")') trim(m%meshFileName), i
         write(11,'(i0," ! nvdll, number of nodes on the boundary")') m%nvdll(i)
         do j=1, m%nvdll(i)
            write(11,'(i0," ! nbdv, node number")') m%nbdv(i,j)
         end do
      end if
   end do
   close(11)
endif
if (any(m%nvell.lt.3)) then
   write(6,'("WARNING: The mesh file contains ",i0," flux specified boundaries that are less than three nodes long.")') count(m%nvell.lt.3)
   write(6,'("INFO: Identifying information for these short boundaries will now be written to the short_flux_boundaries.txt file.")')
   open(11,file='short_flux_boundaries.txt',status='replace',action='write')
   do i=1, m%nbou
      if ( m%nvell(i).lt.3 ) then
         shortBoundary = .true.
         write(11,'("WARNING: The mesh file ",a," has a flux boundary (number ",i0,") that consists of less than 3 nodes:")') trim(m%meshFileName), i
         write(11,'(i0,2x,i0," ! nvell (number of nodes on the boundary) ibtype (boundary type)")') m%nvell(i), m%ibtype(i)
         do j=1, m%nvell(i)
            write(11,'(i0," ! nbvv, node number")') m%nbvv(i,j)
         end do
      end if
   end do
endif
if ( shortBoundary.eqv..false. ) then
   write(6,'("INFO: All boundaries are longer than two nodes.")')
endif
write(6,'("INFO: Finished checking boundary information.")')
!
!   C H E C K   F O R   D I S J O I N T   N O D E S
!
!minEdgeLengthSquared = minEdgeLength**2
!do i=1,np-1
!   do j=i+1,np
!      if ( ((x_cpp(i)-x_cpp(j))**2 + (y_cpp(i)-y_cpp(j))**2).lt.(minEdgeLengthSquared - 1.e-4) ) then
!         write(6,'("ERROR: Node ",i0," is only ",f15.7," meters away from node ",i0," which is   closer than the minimum edge length.")') i, sqrt((x_cpp(i)-x_cpp(j))**2 + (y_cpp(i)-y_cpp(j))**2), j
!      endif
!   end do
!end do
allocate(used(m%np))
used = .false.
do i=1,m%ne
   do j=1,3
      used(m%nm(i,j)) = .true.
   end do
end do
if (any(used.eqv..false.)) then
   do i=1,m%np
      if (used(i).eqv..false.) then
         write(6,'("ERROR: Node ",i0," is not resident on any element.")') i
      endif
   end do
else
   write(6,'("INFO: No disjoint nodes were found. All nodes are resident on at least one element.")')
endif
!
!   C H E C K   F O R   O V E R L A P P I N G   E L E M E N T S
!
allocate(overlappingElements(m%ne))
overlappingElements(:) = .false.
! loop over elements
do e=1, m%ne
   residentNodes(1:3) = m%nm(e,1:3)
   residentNodes(4) = residentNodes(1)
   do l=1, 3
      icount = 0
      ! loop over the elements around the first node
      do i1=1, m%nNeighEle(residentNodes(l))
         ! get the element number
         e1 = m%neiTabEle(residentNodes(l),i1)
         ! if there is not an element here, go to the next one
         if (e1.eq.-99) then
            cycle
         endif
         ! loop over the elements around the other node
         do i2 = 1, m%nNeighEle(residentNodes(l+1))
            ! get the element number
            e2 = m%neiTabEle(residentNodes(l+1),i2)
            ! if there is not an element here, go to the next one
            if (e2.eq.-99) then
               cycle
            endif
            ! we should only have two in common at most?
            if( e1.eq.e2 ) then
               icount = icount + 1
               !exit ! why are we exiting here?
            endif
         enddo
      enddo
      if ( icount.gt.2 ) then
!         write(6,'(/)')
         overlappingElements(e) = .true.
         icount=0
         ! now go back and figure out what element was overlapping it
         do i1=1, m%nNeighEle(residentNodes(l))
            ! get the element number
            e1 = m%neiTabEle(residentNodes(l),i1)
            ! if there is not an element here, go to the next one
            if (e1.eq.-99) then
               cycle
            endif
            ! loop over the elements around the other node
            do i2 = 1, m%nNeighEle(residentNodes(l+1))
               ! get the element number
               e2 = m%neiTabEle(residentNodes(l+1),i2)
               ! if there is not an element here, go to the next one
               if (e2.eq.-99) then
                  cycle
               endif
               ! we should only have two in common at most?
               if( e1.eq.e2 ) then
                  icount = icount + 1
                  write(6,'("e=",i0," l=",i0," residentNodes(l)=",i0," residentNodes(l+1)=",i0," icount=",i0," i1=",i0," i2=",i0," m2=",i0)') e, l, residentNodes(l), residentNodes(l+1),icount, i1, i2, e2
                  write(6,'("node residentNodes(l)=",i0," has ",i0," neighboring elements: ")') residentNodes(l), m%nNeighEle(residentNodes(l))
                  do k=1, m%nNeighEle(residentNodes(l))
                     write(6,'(20(i0,2x))') m%neiTabEle(residentNodes(l),k)
                  end do
                  !exit ! why are we exiting here?
               endif
            enddo
         enddo
     endif
   enddo
enddo
!
if( any(overlappingElements) ) then
   write(6,'("ERROR: There are ",i0," overlapping elements.")') count(overlappingElements)
   do e=1, m%ne
      if( overlappingElements(e).eqv..true. ) then
         !write(6,'("ERROR: Element ",i0," is overlapping. Its resident node numbers are ",i0," ",i0," ",i0,".")') m, nm(m,1), nm(m,2), nm(m,3)
      endif
   enddo
else
   write(6,'("INFO: There are no overlapping elements.")')
endif
!------------------------------------------------------------------------
!       C H E C K   F O R   R E P E A T E D   N O D E S
!              W I T H I N   A   B O U N D A R Y
!------------------------------------------------------------------------
! land, island, and river boundaries
do k=1,m%numSimpleFluxBoundaries
   do j=1,size(m%simpleFluxBoundaries(k)%nodes)
      nodeNumber = m%simpleFluxBoundaries(k)%nodes(j)
      do i=j+1,size(m%simpleFluxBoundaries(k)%nodes)
         if ((nodeNumber.eq.m%simpleFluxBoundaries(k)%nodes(i))) then
            if ((j.eq.1).and.(i.eq.size(m%simpleFluxBoundaries(k)%nodes))) then
               ! first node matches the last node, this is a closed boundary
               ! which is ok
               cycle
            endif
            write(6,'("The node number ",i0," is erroneously repeated on flux boundary number ",i0," (ibtype=",i0,", contains ",i0," nodes).")') nodeNumber, m%simpleFluxBoundaries(k)%indexNum, m%ibtype(m%simpleFluxBoundaries(k)%indexNum),size(m%simpleFluxBoundaries(k)%nodes)
         endif
      end do
   end do
end do
! elevation specified boundaries
do k=1,m%nope
   do j=1,size(m%elevationBoundaries(k)%nodes)
      nodeNumber = m%elevationBoundaries(k)%nodes(j)
      do i=j+1,size(m%elevationBoundaries(k)%nodes)
         if ((nodeNumber.eq.m%elevationBoundaries(k)%nodes(i))) then
            if ((j.eq.1).and.(i.eq.size(m%elevationBoundaries(k)%nodes))) then
               ! first node matches the last node, this is a closed boundary
               ! which is ok
               cycle
            endif
            write(6,'("The node number ",i0," is erroneously repeated on elevation boundary number ",i0," (ibtypee=",i0,", contains ",i0," nodes).")') nodeNumber, m%elevationBoundaries(k)%indexNum, m%ibtypee(m%elevationBoundaries(k)%indexNum),size(m%elevationBoundaries(k)%nodes)
         endif
      end do
   end do
end do
!------------------------------------------------------------------------
!         C O M P U T E   M A X   T I M E   S T E P   S I Z E S
!------------------------------------------------------------------------
! The courant number C = u dt/dx and assuming
!    1. C can be no larger than unity
!    2. u is shallow water wave speed sqrt(gh)
!    3. we can apply a range of inundation depths to see how the max time
!       step varies as the water gets deeper
! then dt = C dx/u = 1 * dx / sqrt(gh)  where e.g. h = 0.0, 0.5, ..., 10.0m
!------------------------------------------------------------------------
if (computeMaxTimestepSizes.eqv..true.) then
   maxInundationDepth = 10.d0
   inundationDepthIncrement = 0.5d0
   inundationDepth = 0.d0
   j = 1
   allocate(maxTimeStepSizes(m%np,int(maxInundationDepth/inundationDepthIncrement)+1))
   maxTimeStepSizes = 99999.d0
   open(11,file='maxtimestepsizes.63',status='replace',action='write')
   ! write header info
   write(11,'(a)') trim(m%agrid)
   write(11,1010) 1, m%np, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   do while(inundationDepth.le.maxInundationDepth)
      do i=1,m%np
         ! at each node, determine min connected edge length
         minEdge = minval(m%edgeLengthTable(i,2:m%nneigh(i)))
         effectiveDepth = m%xyd(3,i) + inundationDepth ! inundation depth above msl
         if (inundationAboveLocalGround.eqv..true.) then
            ! if the node is normally dry, use the inundation depth directly
            if (m%xyd(3,i).lt.0.d0) then
               effectiveDepth = inundationDepth
            endif
         endif
         if (effectiveDepth.gt.0.d0) then
            maxTimeStepSizes(i,j) = minEdge / sqrt(9.81d0*effectiveDepth)
         endif
      end do
      write(6,'("INFO: At an inundation depth of ",f19.15,"m, the smallest time step is ",1pE20.10E3," seconds.")') inundationDepth, minval(maxTimeStepSizes(:,j))
      ! write max time step sizes
      write(11,2120) inundationDepth, j
      do i=1,m%np
         write(11,2453) i, maxTimeStepSizes(i,j)
      end do
      j = j + 1
      inundationDepth = inundationDepth + inundationDepthIncrement
   end do
   close(11)
endif
!------------------------------------------------------------------------
!    C O M P U T E   I M P L I C I T   D R Y   E L E M E N T
!                  A R E A   C O N S T R A I N T
!------------------------------------------------------------------------
! When running ADCIRC in implicit mode, there is a minimum element area
! for elements with negative average depths (i.e., out of the water or
! normally dry).
!------------------------------------------------------------------------
if (checkDryElementArea.eqv..true.) then
   ! need to compute the weighting coefficients for the basis function
   ! and the coversion for spherical coordinates
   call computeWeightingCoefficients(m)
   call compute2xAreas(m)
   allocate(areaOK(m%ne))
   allocate(coef(m%np,1))
   coef = 0.d0
   areaOK = -99999
   do ie=1,m%ne
      areaIE = 0.5d0 * m%areas(ie)
      areaIE4 = 2.d0 * m%areas(ie)
      dpAvg = sum(m%xyd(3,m%nm(ie,:)))/3.d0
      if (dpAvg.lt.0.d0) then
         GA00DPAvgOAreaIE4 = G * A00 * DPAvg / AreaIE4
         MsFacLOnDiag = OnDiag * AreaIE * (1.d0/DT+Tau0/2.d0)/DT/12.d0
         areaOK(ie) = 1
         do j=1,3
            coef(m%nm(ie,j),1) = coef(m%nm(ie,j),1) + MsFacLOnDiag  &
               + GA00DPAvgOAreaIE4 * (m%fdx(j,ie)**2 + m%fdy(j,ie)**2)
            if ( coef(m%nm(ie,j),1).lt.0.d0 ) then
               areaOK(ie) = 0
            endif
         end do
      endif
   end do
   if (any(areaOK.eq.0)) then
      write(6,'("WARNING: One or more dry elements are too small for the time step size and topographic height where they are found.")')
   endif
   write(6,'("INFO: Writing dryelementareacheck.100 file assuming an implicit ADCIRC simulation with a time step dt=",f19.15," and tau0=",f19.15,".")') dt, tau0
   ! now write the file
   open(11,file='dryelementareacheck.100',status='replace',action='write')
   ! write header info
   write(11,'(a)') trim(m%agrid)
   write(11,1010) 1, m%ne, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   ! write element IDs
   do ie=1,m%ne
      write(11,'(20(i0,2x))') ie, areaOK(ie)
   end do
   close(11)
   deallocate(coef)
endif
!
!------------------------------------------------------------------------
!    C O M P U T E   D I A G O N A L   O F   A D C I R C
!          I M P L I C I T   L H S   M A T R I X
!------------------------------------------------------------------------
! When running ADCIRC in implicit mode, there may be an interest in
! visualizing the diagonal coefficients of the LHS matrix.
! This assumes the mesh is wet everywhere.
! TODO: This does not include boundary conditions.
!------------------------------------------------------------------------
if (computeImplicitDiagonalCoefficient.eqv..true.) then
   ! need to compute the weighting coefficients for the basis function
   ! and the coversion for spherical coordinates
   call computeWeightingCoefficients(m)
   call compute2xAreas(m)
   allocate(coef(m%np,1))
   coef = 0.d0
   do ie=1,m%ne
      areaIE = 0.5d0 * m%areas(ie)
      areaIE4 = 2.d0 * m%areas(ie)
      dpAvg = sum(m%xyd(3,m%nm(ie,:)))/3.d0

      GA00DPAvgOAreaIE4 = G * A00 * DPAvg / AreaIE4
      MsFacLOnDiag = OnDiag * AreaIE * (1.d0/DT+Tau0/2.d0)/DT/12.d0
      coef(m%nm(ie,1),1) = coef(m%nm(ie,1),1) + MsFacLOnDiag  &
         + GA00DPAvgOAreaIE4 * (m%fdx(1,ie)**2 + m%fdy(1,ie)**2)
      coef(m%nm(ie,2),1) = coef(m%nm(ie,2),1) + MsFacLOnDiag &
         + GA00DPAvgOAreaIE4 * (m%fdx(2,ie)**2 + m%fdy(2,ie)**2)
      coef(m%nm(ie,3),1) = coef(m%nm(ie,3),1) + MsFacLOnDiag &
         + GA00DPAvgOAreaIE4 * (m%fdx(3,ie)**2 + m%fdy(3,ie)**2)
   end do
   if (any(coef(:,1).lt.0.d0)) then
      write(6,'("WARNING: One or more nodes will have a negative value on the diagonal of the left hand side matrix in a fully consistent (implicit mode) ADCIRC simulation.")')
   endif
   ! compute scaling factor
   ! TODO: handle elevation specified boundaries
   ep = 0.0d0
   do i=1,m%np
      ep = ep + coef(i,1)**2
   enddo
   ep = sqrt(ep/m%np)
   open(11,file='coefdiagonal.63',status='replace',action='write')
   ! write header info
   write(11,'(a)') trim(m%agrid)
   write(11,1010) 1, m%np, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   do i=1,m%np
      write(11,2453) i, coef(i,1)
   end do
endif

!----------------------------------------------------------------------
end program checkAdcircMesh
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!                        S U B R O U T I N E
!        C H E C K   L E V E E   H E I G H T   A T   N O D E
!----------------------------------------------------------------------
! jgf: Checks the levee height against the bathy/topo at a particular
! node.
!----------------------------------------------------------------------
subroutine checkLeveeHeightAtNode(m, nodeNumber, leveeHeight, ok)
use adcmesh
implicit none
integer, intent(in) :: nodeNumber
real, intent(in) :: leveeHeight
type(mesh_t), intent(inout) :: m
logical, intent(out) :: ok
real :: topo    ! topo/bathy height
real(8) :: lat  ! latitude of the levee node
real(8) :: lon  ! longitude of the levee node
ok = .true.
! topography and levee height have opposite sign conventions
topo = - m%xyd(3,nodeNumber)
if (topo.gt.leveeHeight) then
   lat = m%xyd(2,nodeNumber)
   lon = m%xyd(1,nodeNumber)
   write(6,'("ERROR: The levee height is ",F15.7," at node ",i0," (lat=",F15.7," lon=",F15.7,"), which is below its bathy/topo value of ",F15.7,".")') leveeHeight, nodeNumber, lat, lon, -topo
   m%allLeveesOK = .false.
endif
!----------------------------------------------------------------------
end subroutine checkLeveeHeightAtNode
!----------------------------------------------------------------------

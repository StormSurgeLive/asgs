!--------------------------------------------------------------------------
! checkAdcircMesh.f90
!
! A program to find the following ADCIRC mesh anomalies: 
!    (a) boundaries that are only two nodes long
!    (b) levees that are below the height of the local topography 
!    (c) overlapping elements
!    (d) disjoint nodes
!    (e) nodes that have too many or too few connected elements for SWAN
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
use netcdf
implicit none
integer         :: argcount  ! number of command line arguments
integer         :: iargc     ! function to return command line arguments
character(2048) :: cmdlineopt ! command line option
character(2048) :: cmdlinearg ! content of command line argument

integer :: nodeNumber
logical :: ok  ! .true. if a test passes
real :: leveeHeight
logical :: fileFound ! .true. if the file is already present
integer :: nodeNumbers(2)
logical :: shortBoundary
!
real(8) :: dx_crit ! used to create a bounding box around a high resolution area
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
logical writeNNeighEle
                               
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

integer :: residentNodes(4) ! node numbers around an element, indices wrap around
integer :: m ! element counter
integer :: l ! counter for nodes around an element
integer :: i1, i2 ! counters for elements around a node
integer :: m1, m2 ! element number neighboring a node 
integer :: icount ! counter for number of times two nodes have an element in common
!
integer :: i, j, k
!
! initializations
verbose = .false.
writeNNeighEle = .false.
writeNeighborTables = .false.
writeNodeIDs = .false.
writeElementIDs = .false. 
neitabContainsUninitializedValues = .false.
neiTabEleContainsUninitializedValues = .false.
dx_crit = 1750.d0
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
            meshFileName = trim(cmdlinearg)
         case("--dx-crit")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) dx_crit   
         case("--write-neighbor-tables")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeNeighborTables = .true.                              
         case("--write-element-ids")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeElementIDs = .true.
         case("--write-node-ids")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeNodeIDs = .true.
         case("--write-nneighele")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            writeNNeighEle = .true.
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
call read14()
write(6,'("INFO: Computing nodal and elemental neighbor tables.")')
call computeNeighborTable() 
write(6,'("INFO: Computing the neighbor edge length table and finding the minimum edge length.")')
call computeNeighborEdgeLengthTable()
!
!              W R I T E   N O D E   I D S
! 
! Writing node IDs is useful for labelling IDs in ParaView b/c
! ParaView assumes 0-indexing and ADCIRC uses 1-indexing 
if (writeNodeIDs.eqv..true.) then
   write(6,'("INFO: Writing nodeids.63 file.")')
   open(11,file='nodeids.63',status='replace',action='write')
   ! write header info 
   write(11,'(a)') trim(agrid)
   write(11,1010) 1, np, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   ! write node IDs 
   do i=1,np
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
   write(11,'(a)') trim(agrid)
   write(11,1010) 1, ne, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   ! write element IDs 
   do i=1,ne
      write(11,'(20(i0,2x))') i, i
   end do
   close(11)
endif
!
!    C R E A T E   M E S H   P R O P E R T I E S   F I L E
!
! Create mesh.properties file and bounding box properties
write(6,'("INFO: Creating mesh.properties file to record the properties of this mesh.")')
inquire(file='mesh.properties',exist=fileFound)
if (fileFound.eqv..true.) then
   open(12,file='mesh.properties',status='old',position='append',action='write')
else
   open(12,file='mesh.properties',status='new',action='write')
endif
write(6,'("INFO: Recording full domain extents.")')
! report the domain extents
write(12,'("fullDomainMinimumLongitudeDegrees : ",f15.7)') minval(xyd(1,:))
write(12,'("fullDomainMaximumLongitudeDegrees : ",f15.7)') maxval(xyd(1,:)) 
write(12,'("fullDomainMinimumLatitudeDegrees : ",f15.7)') minval(xyd(2,:))
write(12,'("fullDomainMaximumLatitudeDegrees : ",f15.7)') maxval(xyd(2,:))
! report the lat and lon boundaries of the negative depths
lonmin = huge(1.d0)
lonmax = tiny(1.d0)
latmin = huge(1.d0)
latmax = tiny(1.d0)
do i=1,np
   if (xyd(3,i).lt.0.d0) then
      lonmin = min(lonmin,xyd(1,i))
      lonmax = max(lonmax,xyd(1,i))
      latmin = min(latmin,xyd(2,i))
      latmax = max(latmax,xyd(2,i))
   endif
end do
write(12,'("fullDomainNegativeDepthMinimumLongitudeDegrees : ",f15.7)') lonmin 
write(12,'("fullDomainNegativeDepthMaximumLongitudeDegrees : ",f15.7)') lonmax 
write(12,'("fullDomainNegativeDepthMinimumLatitudeDegrees : ",f15.7)') latmin
write(12,'("fullDomainNegativeDepthMaximumLatitudeDegrees : ",f15.7)') latmax
!
write(6,'("INFO: Recording bounding box for high resolution area.")')
write(12,'("highResolutionRegionMaximumEdgeLengthMeters : ",f15.7)') dx_crit
lonmin = huge(1.d0)
lonmax = tiny(1.d0)
latmin = huge(1.d0)
latmax = tiny(1.d0)
do i=1,np
   do j=2,nneigh(i)
      if ( neighborEdgeLengthTable(i,j).lt.dx_crit ) then
         lonmin = min(lonmin,xyd(1,i))
         lonmax = max(lonmax,xyd(1,i))
         latmin = min(latmin,xyd(2,i))
         latmax = max(latmax,xyd(2,i))
         exit
      endif
   end do
end do
write(12,'("fullDomainHighResolutionRegionMinimumLongitudeDegrees : ",f15.7)') lonmin 
write(12,'("fullDomainHighResolutionRegionMaximumLongitudeDegrees : ",f15.7)') lonmax 
write(12,'("fullDomainHighResolutionRegionMinimumLatitudeDegrees : ",f15.7)') latmin
write(12,'("fullDomainHighResolutionRegionMaximumLatitudeDegrees : ",f15.7)') latmax
!
write(6,'("INFO: The minimum edge length is ",f15.7," meters.")') minEdgeLength
write(12,'("minimumEdgeLengthMeters : ",f15.7)') minEdgeLength
write(6,'("INFO: The maximum edge length is ",f15.7," meters.")') maxEdgeLength
write(12,'("maximumEdgeLengthMeters : ",f15.7)') maxEdgeLength
! leave unit 12 open so we can write more mesh properties to it...
!
!
!     C H E C K   N E I G H B O R   T A B L E S   F O R   
!          U N I N I T I A L I Z E D   V A L U E S
! 
do i=1,np
   do j=1, nneigh(i)
      if ((neitab(i,j).eq.-99).or.(neitab(i,j).eq.0)) then
         neitabContainsUninitializedValues = .true.
      endif
   end do
end do
if (neitabContainsUninitializedValues.eqv..true.) then
   write(6,'("ERROR: The table of the nodal neighbors for each node contains uninitialized values.")')
   writeNeighborTables = .true.
endif
do i=1,np
   do j=1, nNeighEle(i)
      if ((neiTabEle(i,j).eq.-99).or.(neiTabEle(i,j).eq.0)) then
         neiTabEleContainsUninitializedValues = .true.
      endif
   end do
end do
if (neiTabEleContainsUninitializedValues.eqv..true.) then
   write(6,'("ERROR: The table of the elemental neighbors for each node contains uninitialized values.")')
   writeNeighborTables = .true.
endif
!
!         W R I T E   N E I G H B O R   T A B L E S 
!
if (writeNeighborTables.eqv..true.) then
   write(6,'("INFO: Writing neitab.generated file.")')
   open(11,file='neitab.generated',status='replace')
   do i=1,np
      write(11,'(20(i0,2x))') i, (neiTabGenerated(i,j),j=1,nneigh(i))
   end do
   close(11)
   write(6,'("INFO: Writing neitab.sorted file.")')
   open(11,file='neitab.sorted',status='replace')
   do i=1,np
      write(11,'(20(i0,2x))') i, (neiTab(i,j),j=1,nneigh(i))
   end do
   close(11)
   write(6,'("INFO: Writing neitabele.generated file.")')
   open(11,file='neitabele.generated',status='replace')
   do i=1,np
      write(11,'(20(i0,2x))') i, (neiTabEleGenerated(i,j),j=1,nneighele(i))
   end do
   close(11)
   write(6,'("INFO: Writing neitabele.sorted file.")')
   open(11,file='neitabele.sorted',status='replace')
   do i=1,np
      write(11,'(20(i0,2x))') i, (neiTabEle(i,j),j=1,nneighele(i))
   end do
   close(11)
endif

!
!    R E P O R T   N O D E S   W I T H   L E S S   T H A N   4   
! O R   M O R E   T H A N   1 0   C O N N E C T E D   E L E M E N T S
!                  ( B A D   F O R   S W A N  )
!
lowConnectedNodes = count(nNeighEle.lt.4)
highConnectedNodes = count(nNeighEle.gt.10)
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
   write(11,'(a)') trim(agrid)
   write(11,1010) 1, np, 0.d0, 1, 1
   write(11,2120) 0.d0, 0
   do i=1,np
      write(11,2452) i,nNeighEle(i)
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
allLeveesOK = .true.
do k=1,numExternalFluxBoundaries
   do j=1,size(externalFluxBoundaries(k)%nodes)
      nodeNumber = externalFluxBoundaries(k)%nodes(j)
      leveeHeight = externalFluxBoundaries(k)%barlanht(j)
      call checkLeveeHeightAtNode(nodeNumber, leveeHeight, ok)
      if (ok.eqv..false.) then
         write(6,'("There is an error on land boundary number ",i0," (external flux boundary).")') externalFluxBoundaries(k)%indexNum
      endif
   end do
end do
do k=1,numInternalFluxBoundaries
   do j=1,size(internalFluxBoundaries(k)%nodes)
      nodeNumbers(1) = internalFluxBoundaries(k)%nodes(j)
      nodeNumbers(2) = internalFluxBoundaries(k)%ibconn(j)
      leveeHeight = internalFluxBoundaries(k)%barinht(j)
      do i=1,2        
         call checkLeveeHeightAtNode(nodeNumbers(i), leveeHeight, ok)
         if (ok.eqv..false.) then
            write(6,'("There is an error on land boundary number ",i0," (internal flux boundary).")') internalFluxBoundaries(k)%indexNum
            write(6,'("Node ",i0," is paired with node ",i0,".")') nodeNumbers(1), nodeNumbers(2)
         endif            
      end do
   end do
end do
do k=1,numInternalFluxBoundariesWithPipes
   do j=1,size(internalFluxBoundariesWithPipes(k)%nodes)
      nodeNumbers(1) = internalFluxBoundariesWithPipes(k)%nodes(j)
      nodeNumbers(2) = internalFluxBoundariesWithPipes(k)%ibconn(j)
      leveeHeight = internalFluxBoundariesWithPipes(k)%barinht(j)
      do i=1,2        
         call checkLeveeHeightAtNode(nodeNumbers(i), leveeHeight, ok)
         if (ok.eqv..false.) then
            write(6,'("There is an error on land boundary number ",i0," (internal flux boundary with pipes).")') internalFluxBoundaries(k)%indexNum
            write(6,'("Node ",i0," is paired with node ",i0,".")') nodeNumbers(1), nodeNumbers(2)
         endif            
      end do
   end do
end do
if (allLeveesOK.eqv..true.) then
   write(6,'("INFO: Successfully tested levee heights: all levee heights are above the bathy/topo elevation at that node, as expected.")')
endif
!
!  C H E C K   F O  R   S H O R T   B O U N D A R I E S   
!
! check to see if any boundary is two nodes long, and if so, 
! output the data for that boundary
if (any(nvdll.lt.3)) then
   write(6,'("WARNING: The mesh file contains ",i0," elevation specified boundaries that are less than three nodes long.")') count(nvdll.lt.3)
   write(6,'("INFO: Identifying information for these short boundaries will now be written to the short_elevation_boundaries.txt file.")')
   open(11,file='short_elevation_boundaries.txt',status='replace',action='write')
   do i=1, nope
      if ( nvdll(i).lt.3 ) then
         shortBoundary = .true.
         write(11,'("WARNING: The mesh file ",a," has an open boundary (number ",i0,") that consists of only 2 nodes:")') trim(meshFileName), i
         write(11,'(i0," ! nvdll, number of nodes on the boundary")') nvdll(i)
         do j=1, nvdll(i)
            write(11,'(i0," ! nbdv, node number")') nbdv(i,j)
         end do
      end if 
   end do
   close(11)
endif
if (any(nvell.lt.3)) then
   write(6,'("WARNING: The mesh file contains ",i0," flux specified boundaries that are less than three nodes long.")') count(nvell.lt.3)
   write(6,'("INFO: Identifying information for these short boundaries will now be written to the short_flux_boundaries.txt file.")')
   open(11,file='short_flux_boundaries.txt',status='replace',action='write')
   do i=1, nbou
      if ( nvell(i).lt.3 ) then
         shortBoundary = .true.
         write(11,'("WARNING: The mesh file ",a," has a flux boundary (number ",i0,") that consists of less than 3 nodes:")') trim(meshFileName), i
         write(11,'(i0,2x,i0," ! nvell (number of nodes on the boundary) ibtype (boundary type)")') nvell(i), ibtype(i)
         do j=1, nvell(i)
            write(11,'(i0," ! nbvv, node number")') nbvv(i,j)
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
allocate(used(np))
used = .false.
do i=1,ne
   do j=1,3
      used(nm(i,j)) = .true.
   end do
end do 
if (any(used.eqv..false.)) then
   do i=1,np
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
allocate(overlappingElements(ne))
overlappingElements(:) = .false.
! loop over elements
do m=1, ne
   residentNodes(1:3) = nm(m,1:3)
   residentNodes(4) = residentNodes(1)
   do l=1, 3
      icount = 0
      ! loop over the elements around the first node
      do i1=1, nNeighEle(residentNodes(l))
         ! get the element number
         m1 = neiTabEle(residentNodes(l),i1)
         ! loop over the elements around the other node
         do i2 = 1, nNeighEle(residentNodes(l+1))
            ! get the element number
            m2 = neiTabEle(residentNodes(l+1),i2)
            ! we should only have two in common at most?
            if( m1.eq.m2 ) then
               icount = icount + 1
               !exit ! why are we exiting here?
            endif
         enddo
      enddo
      if ( icount.gt.2 ) then
!         write(6,'(/)') 
         overlappingElements(m) = .true.
!         icount=0
!         ! now go back and figure out what element was overlapping it
!         do i1=1, nNeighEle(residentNodes(l))
!            ! get the element number
!            m1 = neiTabEle(residentNodes(l),i1)
!            ! loop over the elements around the other node
!            do i2 = 1, nNeighEle(residentNodes(l+1))
!               ! get the element number
!               m2 = neiTabEle(residentNodes(l+1),i2)
!               ! we should only have two in common at most?
!               if( m1.eq.m2 ) then            
!                  icount = icount + 1                   
!                  write(6,'("m=",i0," l=",i0," residentNodes(l)=",i0," residentNodes(l+1)=",i0," icount=",i0," i1=",i0," i2=",i0," m2=",i0)') m, l, residentNodes(l), residentNodes(l+1),icount, i1, i2, m2
!                  write(6,'("node residentNodes(l)=",i0," has ",i0," neighboring elements: ")') residentNodes(l), nNeighEle(residentNodes(l))
!                  do k=1, nNeighEle(residentNodes(l))
!                     write(6,'(20(i0,2x))') neiTabEle(residentNodes(l),k)
!                  end do
                  !exit ! why are we exiting here?
!               endif
!            enddo
!         enddo
     endif
   enddo
enddo
!
if( any(overlappingElements) ) then
   write(6,'("ERROR: There are ",i0," overlapping elements.")') count(overlappingElements)
   do m=1, ne
      if( overlappingElements(m).eqv..true. ) then
         !write(6,'("ERROR: Element ",i0," is overlapping. Its resident node numbers are ",i0," ",i0," ",i0,".")') m, nm(m,1), nm(m,2), nm(m,3)            
      endif
   enddo
else 
   write(6,'("INFO: There are no overlapping elements.")')
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
subroutine checkLeveeHeightAtNode(nodeNumber, leveeHeight, ok)
use adcmesh
implicit none
integer, intent(in) :: nodeNumber
real, intent(in) :: leveeHeight
logical, intent(out) :: ok
real :: topo    ! topo/bathy height 
real(8) :: lat  ! latitude of the levee node
real(8) :: lon  ! longitude of the levee node
ok = .true.
! topography and levee height have opposite sign conventions
topo = -xyd(3,nodeNumber)
if (topo.gt.leveeHeight) then
   lat = xyd(2,nodeNumber)
   lon = xyd(1,nodeNumber)
   write(6,'("ERROR: The levee height is ",F15.7," at node ",i0," (lat=",F15.7," lon=",F15.7,"), which is below its bathy/topo value of ",F15.7,".")') leveeHeight, nodeNumber, lat, lon, -topo
   allLeveesOK = .false.
endif
!----------------------------------------------------------------------
end subroutine checkLeveeHeightAtNode
!----------------------------------------------------------------------




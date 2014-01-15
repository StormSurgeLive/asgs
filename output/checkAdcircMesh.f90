!--------------------------------------------------------------------------
! checkAdcircMesh.f90
!
! A program to find the following ADCIRC mesh anomalies: 
!    (a) boundaries that are only two nodes long
!    (b) levees that are below the height of the local topography 
!    (c) overlapping elements
!    (d) disjoint nodes
!
!--------------------------------------------------------------------------
! Copyright(C) 2013 Jason Fleming
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

! Example of how to compile with gfortran:
! gfortran -o checkAdcircMesh.x -ffree-form -ffree-line-length-none -I/usr/include checkAdcircMesh.f90 -lnetcdf -lnetcdff -lz
!
! Example of how to compile with gfortran with debugging:
! gfortran -g -O0 -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal -o checkAdcircMesh.x -ffree-form -ffree-line-length-none -I/usr/include checkAdcircMesh.f90 -lnetcdf -lnetcdff -lz
!
! Example for compiling with ifort on croatan at RENCI:
! ifort -o checkAdcircMesh.x -i-dynamic -cpp -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/projects/ncfs/apps/croatan/netcdf/include checkAdcircMesh.f90 -L/projects/ncfs/apps/croatan/netcdf/lib -lnetcdf -lnetcdff -lz
!
! Compiling with pgf90 on garnet at ERDC 20130926:
! pgf90 -o checkAdcircMesh.x -Mpreprocess -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/opt/cray/netcdf/4.3.0/pgi/121/include checkAdcircMesh.f90 -L/opt/cray/netcdf/4.3.0/pgi/121/lib -lnetcdf -lnetcdff

include 'adcmesh.f90'
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

logical :: shortBoundary
logical :: levee
logical :: overlap
logical :: disjoint

integer :: nodeNumber
logical :: ok  ! .true. if a test passes
real :: leveeHeight
integer :: nodeNumbers(2)

integer :: i, j, k

! initializations
verbose = .false.
shortBoundary = .false.
levee = .false.
overlap = .false.
disjoint = .false.
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
            write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            meshFileName = trim(cmdlinearg)
         case("--all")
            write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
            shortBoundary = .true.
            levee = .true.
            overlap =.true.
            disjoint = .true.
         case("--short-boundary")
            write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
            shortBoundary = .true.
         case("--levee")
            write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
            levee = .true.
         case("--overlap")
            write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
            overlap =.true.
         case("--disjoint")
            write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
            disjoint = .true.
         case("--verbose")
            write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
            verbose = .true.                              
         case default
            write(6,*) "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
end if    
!
! Load fort.14
call read14()
! 
! report the domain extents
latmin = xyd(2,1)
latmax = xyd(2,1)
lonmin = xyd(1,1)
lonmax = xyd(1,1)
! report the lat and lon boundaries of the normally-dry nodes region
do i=1,np
   if (xyd(3,i).lt.0.d0) then
      latmin = min(latmin,xyd(2,i))
      latmax = max(latmax,xyd(2,i))
      lonmin = min(lonmin,xyd(1,i))
      lonmax = max(lonmax,xyd(1,i))
   endif
end do
write(6,'("INFO: The extent of the normally dry region is from ",F15.7," to ",F15.7," degrees longitude to ",F15.7," to ",F15.7," degrees latitude.")') lonmin,lonmax,latmin,latmax
! report the domain extent
do i=1,np
   latmin = min(latmin,xyd(2,i))
   latmax = max(latmax,xyd(2,i))
   lonmin = min(lonmin,xyd(1,i))
   lonmin = min(lonmax,xyd(1,i))
end do
write(6,'("INFO: The extent of the full domain is from ",F15.7," to ",F15.7," degrees longitude to ",F15.7," to ",F15.7," degrees latitude.")') lonmin,lonmax,latmin,latmax
!
!   C H E C K   L E V E E   H E I G H T S   I F   R E Q U E S T E D
!
if (levee.eqv..true.) then
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
         nodeNumbers(2) = internalFluxBoundariesWithPipes(k)%ibconnr(j)
         leveeHeight = internalFluxBoundariesWithPipes(k)%barinhtr(j)
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
endif
!
!  C H E C K   F O  R   S H O R T   B O U N D A R I E S   I F   R E Q U E S T E D
if (shortBoundary.eqv..true.) then
   ! check to see if any boundary is two nodes long, and if so, 
   ! output the data for that boundary
   do i=1, nope
      if ( nvdll(i).eq.2 ) then
         shortBoundary = .true.
         write(6,'(A,I0,A)') "WARNING: The mesh file '" // trim(meshFileName) // &
            "' has an open boundary (number ",i,") that consists of only 2 nodes:"
         write(6,'(I0,A)') nvdll(i)," ! nvdll, number of nodes on the boundary"
         do j=1, nvdll(i)
            write(6,'(I0,A)') nbdv(i,j)," ! nbdv, node number"
         end do
      end if 
   end do
   do i=1, nbou
      if ( nvell(i).eq.2 ) then
         shortBoundary = .true.
         write(6,'(A,I0,A)') "WARNING: The mesh file '" // trim(meshFileName) // &
            "' has a flux boundary (number ",i,") that consists of only 2 nodes:"
         write(6,'(I0,A,I0,A)') nvell(i),"   ",ibtype(i), &
            " ! nvell  ibtype : number of nodes on the boundary and the boundary type"
         do j=1, nvell(i)
            write(6,'(I0,A)') nbvv(i,j)," ! nbvv: node number"
         end do
      end if
   end do
   if ( shortBoundary.eqv..false. ) then
      write(6,'(A)') "INFO: All boundaries were longer than two nodes."      
   endif
   write(6,'(A)') "INFO: Finished checking boundary information."
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




!--------------------------------------------------------------------------
! boundaryFinder.f90: Pulls boundaries out of ADCIRC mesh and optionally 
! computes flux per unit width if a total flux is given.
!--------------------------------------------------------------------------
! Copyright(C) 2015--2017 Jason Fleming
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
!--------------------------------------------------------------------------
program boundaryFinder
use adcmesh
implicit none
character(1024) :: outputfile
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
character(1024) :: boundaryType
type(mesh_t) :: mesh
real(8), allocatable :: dist(:)
real(8), allocatable :: dep(:)
real(8) :: nextdist    ! (m) from node under consideration to next node
real(8) :: prevdist    ! (m) from node under consideration to previous node
integer :: thisNodeNum ! node number under consideration
integer :: nextNodeNum ! next node along boundary
integer :: prevNodeNum ! prev node along boundary
real(8) :: sumDepths   ! (m) at each of the nodes along a boundary 
real(8) :: sumLengths  ! (m) between each node along a boundary (half at ends)
real(8) :: nominalWSE  ! (m) nominal water surface elevation at boundary for river boundaries above msl
logical :: withCoordinates  ! true if lon lat should be written for boundary nodes
logical :: xyz              ! true if lon lat depth should be written for boundary nodes
logical :: lengthsDepths    ! true if to write depths and lengths along the boundaries 
logical :: writeBoundary    ! true if the boundary was requested
logical :: foundBoundary    ! true if the requested boundary was found at least once
integer :: argcount
integer :: i, j, k, m, n
!
i=0
withCoordinates = .false.
xyz = .false.
lengthsDepths = .false.
foundBoundary = .false.
nominalWSE = 0.0d0  
outputfile = 'boundaries.txt'
boundaryType = 'inflow_flux'
!
argcount = iargc() ! count up command line options
write(6,'(a,i0,a)') 'INFO: boundaryFinder.x: There are ',argcount,' command line options.'
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case('--meshfile')
      i = i + 1
      call getarg(i, mesh % meshFileName)
      write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(mesh % meshFileName),'.'
   case("--outputfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
      outputfile = trim(cmdlinearg)
   case("--with-coordinates")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      withCoordinates = .true.
   case("--xyz")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      xyz = .true.
   case("--lengths-depths")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      lengthsDepths = .true.
   case("--cpp")
      i = i + 1
      call getarg(i, cmdlinearg)
      read(cmdlinearg,*) mesh % slam0
      i = i + 1
      call getarg(i, cmdlinearg)
      read(cmdlinearg,*) mesh % sfea0
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",mesh % slam0," ",mesh % sfea0,"."
   case("--nominal-wse")
      i = i + 1
      call getarg(i, cmdlinearg)
      read(cmdlinearg,*) nominalWSE
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",nominalWSE,"."
   case("--boundary-type")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      boundaryType = trim(cmdlinearg)
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
!
! Load fort.14
write(6,'(a,a,a)') 'INFO: boundaryFinder.x: The mesh file name is ',trim(mesh % meshfilename),'.'
call read14(mesh)
!
! if lengths between nodes are required, must convert lon/lat differences
! to distances in meters, which requires cpp reprojection
if (lengthsDepths.eqv..true.) then
   write(6,'("INFO: boundaryFinder.x: The center of the projection is lon=",f20.10," lat=",f20.10,".")') mesh % slam0, mesh % sfea0
   call computeCPP(mesh) ! get the mesh coords in the same sys adcirc uses
endif
!
! Check to see if an output format has been specified, and if not, 
! set the default to withCoordinates
if ((withCoordinates.eqv..false.).and.(xyz.eqv..false.).and.(lengthsDepths.eqv..false.)) then
   withCoordinates = .true.
endif
!
! open output file
open(unit=99, file=trim(adjustl(outputfile)), status='replace', action='write')

select case(trim(adjustl(boundaryType)))
case("inflow_flux","land","island")
   do i = 1, mesh % numSimpleFluxBoundaries
      k = mesh % simpleFluxBoundaries(i)%indexNum
      writeBoundary = .false.
      select case(mesh % ibtype_orig(k))
      case(2,12,22,52)
         if (trim(adjustl(boundaryType)).eq."inflow_flux") then
            writeBoundary = .true.
            foundBoundary = .true.
         endif
      case(0,20)
         if (trim(adjustl(boundaryType)).eq."land") then
            writeBoundary = .true.
            foundBoundary = .true.
         endif
      case(1,21)
         if (trim(adjustl(boundaryType)).eq."island") then
            writeBoundary = .true.
            foundBoundary = .true.
         endif
      case default
         ! ignore the other boundary types
      end select
      if (writeBoundary.eqv..true.) then
         write(99,'(i0,1x,i0)') mesh % nvell(k), mesh % ibtype_orig(k)
         if (withCoordinates.eqv..true.) then
            do j=1, mesh % nvell(k)
               n = mesh % simpleFluxBoundaries(i)%nodes(j)
               write(99,'(i0,1x,f19.15,1x, f19.15)') n, mesh % xyd(1,n), mesh % xyd(2,n)
            end do 
         else if (xyz.eqv..true.) then
            do j=1,mesh % nvell(k)
               n = mesh % simpleFluxBoundaries(i)%nodes(j)
               write(99,'(3(f19.15,1x))') (mesh % xyd(m,n), m=1,3)
            end do
         else if (lengthsDepths.eqv..true.) then
            !
            ! now find the location of the boundary nodes, how far apart they
            ! are, and the depth at each one, for use in calculating the flux
            ! per unit width
            allocate(dep(mesh % nvell(k)))
            allocate(dist(mesh % nvell(k)))                
            ! 
            ! handle first node; use only half the distance to the next node
            thisNodeNum = mesh % simpleFluxBoundaries(i)%nodes(1)
            nextNodeNum = mesh % simpleFluxBoundaries(i)%nodes(2)
            dist(1) = 0.5d0 &
               * sqrt( (mesh % x_cpp(nextNodeNum)-mesh % x_cpp(thisNodeNum))**2 + &
                        (mesh % y_cpp(nextNodeNum)-mesh % y_cpp(thisNodeNum))**2 )
            dep(1) = 0.5d0 * (nominalWSE +mesh %  xyd(3,thisNodeNum)) ! half the total water depth
            sumDepths = sumDepths + dep(1) ! accumulate total depth
            sumLengths = sumLengths + dist(1) ! accumulate total length
            do j=2,mesh %  nvell(k)-1
               prevNodeNum =mesh %  simpleFluxBoundaries(i)%nodes(j-1)
               thisNodeNum =mesh %  simpleFluxBoundaries(i)%nodes(j)
               nextNodeNum =mesh %  simpleFluxBoundaries(i)%nodes(j+1)
               prevdist = sqrt( (mesh % x_cpp(thisNodeNum)-mesh % x_cpp(prevNodeNum))**2 + &
                                 (mesh % y_cpp(thisNodeNum)-mesh % y_cpp(prevNodeNum))**2 )
               nextdist = sqrt( (mesh % x_cpp(nextNodeNum)-mesh % x_cpp(thisNodeNum))**2 + &
                                 (mesh % y_cpp(nextNodeNum)-mesh % y_cpp(thisNodeNum))**2 )
               dist(j) = 0.5d0 * prevdist + 0.5d0 * nextdist
               dep(j) = nominalWSE +mesh %  xyd(3,thisNodeNum) ! use the whole total water depth here
               sumDepths = sumDepths + dep(j) ! accumulate total depth
               sumLengths = sumLengths + dist(j) ! accumulate total length
            end do
            ! handle last node; use only half the distance from the previous node
            thisNodeNum =mesh %  simpleFluxBoundaries(i)%nodes(mesh % nvell(k))
            prevNodeNum =mesh %  simpleFluxBoundaries(i)%nodes(mesh % nvell(k)-1)
            dist(mesh % nvell(k)) = 0.5d0 * sqrt( (mesh % x_cpp(thisNodeNum)-mesh % x_cpp(prevNodeNum))**2 + &
                              (mesh % y_cpp(thisNodeNum)-mesh % y_cpp(prevNodeNum))**2 )
            dep(mesh % nvell(k)) = 0.5d0 * (nominalWSE +mesh %  xyd(3,thisNodeNum)) ! use only half the total water depth
            sumDepths = sumDepths + dep(mesh % nvell(k)) ! accumulate total depth
            sumLengths = sumLengths + dist(mesh % nvell(k)) ! accumulate total length
            write(99,'(f20.10,f20.10," # totalEffDepth(m) totalLength(m)")') sumDepths, sumLengths
            do j=1,mesh % nvell(k)
               write(99,'(f20.10,f20.10," # effDepth(m) effLength(m)")') dep(j), dist(j)
            end do
            deallocate(dep)
            deallocate(dist)
         else
            ! if format was not specified, just write out the node number
            write(99,'(i0)') n
         endif
      endif
   enddo
case("internal_barrier") ! ibtype 4, 24, 5, 25
   do i = 1, mesh % numInternalFluxBoundaries
      k = mesh % internalFluxBoundaries(i)%indexNum
      select case(mesh %ibtype_orig(k))
      case(4,24,5,25)
         if (withCoordinates.eqv..true.) then
            write(99,'(i0,1x,i0)') mesh %nvell(k), mesh %ibtype_orig(k)
            do j=1,mesh %nvell(k)
               n = mesh %internalFluxBoundaries(i)%nodes(j)
               write(99,'(i0,1x,f15.7,1x, f15.7)') n, mesh %xyd(1,n), mesh %xyd(2,n)
            end do
         else if (xyz.eqv..true.) then
            do j=1,mesh %nvell(k)
               n =mesh % internalFluxBoundaries(i)%nodes(j)
               write(99,'(3(f15.7,1x))') (mesh %xyd(m,n), m=1,3)
            end do
         else
            do j=1,mesh %nvell(k)
               n =mesh % internalFluxBoundaries(i)%nodes(j)
               write(99,'(i0)') n
            end do
         endif
      case default
         ! ignore the other boundary types
      end select
   enddo
case("external_overflow") ! ibtype 3, 13, 23
   do i = 1,mesh % numExternalFluxBoundaries
      k =mesh % externalFluxBoundaries(i)%indexNum
      select case(mesh %ibtype_orig(k))
      case(3,13,23)
         if (withCoordinates.eqv..true.) then
            write(99,'(i0,1x,i0)')mesh % nvell(k),mesh % ibtype_orig(k)
            do j=1,mesh %nvell(k)
               n =mesh % externalFluxBoundaries(i)%nodes(j)
               write(99,'(i0,1x,f15.7,1x, f15.7)') n,mesh % xyd(1,n),mesh % xyd(2,n)
            end do
         else if (xyz.eqv..true.) then
            do j=1,mesh %nvell(k)
               n =mesh % externalFluxBoundaries(i)%nodes(j)
               write(99,'(3(f15.7,1x))') (mesh %xyd(m,n), m=1,3)
            end do
         else
            do j=1,mesh %nvell(k)
               n =mesh % externalFluxBoundaries(i)%nodes(j)
               write(99,'(i0)') n
            end do
         endif
      case default
         ! ignore the other boundary types
      end select
   enddo
case default
   write(6,'(a,a,a)') 'WARNING: Did not recognize the flux boundary type ',trim(boundaryType),'.'
   stop
end select
if (foundBoundary.eqv..false.) then
   write(6,'(a,a,a)') 'INFO: There are no boundaries of type "',trim(boundaryType),'" in the mesh file.'
   write(99,'(a,a,a)') 'INFO: There are no boundaries of type "',trim(boundaryType),'" in the mesh file.'
endif
close(99)
!-------------------------------------------------------------------
end program boundaryFinder
!-------------------------------------------------------------------

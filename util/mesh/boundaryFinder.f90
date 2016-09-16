!--------------------------------------------------------------------------
! boundaryFinder.f90: Pulls boundaries out of ADCIRC mesh and optionally 
! computes flux per unit width if a total flux is given.
!--------------------------------------------------------------------------
! Copyright(C) 2015--2016 Jason Fleming
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
logical :: withCoordinates  ! true if lon lat should be written for boundary nodes
logical :: xyz              ! true if lon lat depth should be written for boundary nodes
logical :: writeBoundary    ! true if the boundary was requested
integer :: argcount
integer :: i, j, k, m, n
!
i=0
withCoordinates = .false.
xyz = .false.
outputfile = 'boundaries.txt'
!
argcount = iargc() ! count up command line options
write(6,'(a,i0,a)') 'There are ',argcount,' command line options.'
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--meshfile")
      i = i + 1
      call getarg(i, meshFileName)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(meshFileName),"."
   case("--outputfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      outputfile = trim(cmdlinearg)
   case("--withcoordinates")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      withCoordinates = .true.
   case("--xyz")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      xyz = .true.
   case("--boundarytype")
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
write(6,'(a,a,a)') 'The mesh file name is ',trim(meshfilename),'.'
call read14()
!
! Check to see if an output format has been specified, and if not, 
! set the default to withCoordinates
if ((withCoordinates.eqv..false.).and.(xyz.eqv..false.)) then
   withCoordinates = .true.
endif
!
! open output file
open(unit=99, file=trim(adjustl(outputfile)), status='replace', action='write')
select case(trim(adjustl(boundaryType)))
case("inflow_flux","land","island")
   do i = 1, numSimpleFluxBoundaries
      k = simpleFluxBoundaries(i)%indexNum
      writeBoundary = .false.
      select case(ibtype_orig(k))
      case(2,12,22,52)
         if (trim(adjustl(boundaryType)).eq."inflow_flux") then
            writeBoundary = .true.
         endif
      case(0,20)
         if (trim(adjustl(boundaryType)).eq."land") then
            writeBoundary = .true.
         endif
      case(1,21)
         if (trim(adjustl(boundaryType)).eq."island") then
            writeBoundary = .true.
         endif
      case default
         ! ignore the other boundary types
      end select
      if (writeBoundary.eqv..true.) then
         write(99,'(i0,1x,i0)') nvell(k), ibtype_orig(k)
         if (withCoordinates.eqv..true.) then
            do j=1,nvell(k)
               n = simpleFluxBoundaries(i)%nodes(j)
               write(99,'(i0,1x,f19.15,1x, f19.15)') n, xyd(1,n), xyd(2,n)
            end do 
         else if (xyz.eqv..true.) then
            do j=1,nvell(k)
               n = simpleFluxBoundaries(i)%nodes(j)
               write(99,'(3(f19.15,1x))') (xyd(m,n), m=1,3)
            end do
         else
            write(99,'(i0)') n
         endif
      endif
   enddo
case("internal_barrier") ! ibtype 4, 24, 5, 25
   do i = 1, numInternalFluxBoundaries
      k = internalFluxBoundaries(i)%indexNum
      select case(ibtype_orig(k))
      case(4,24,5,25)
         if (withCoordinates.eqv..true.) then
            write(99,'(i0,1x,i0)') nvell(k), ibtype_orig(k)
            do j=1,nvell(k)
               n = internalFluxBoundaries(i)%nodes(j)
               write(99,'(i0,1x,f15.7,1x, f15.7)') n, xyd(1,n), xyd(2,n)
            end do
         else if (xyz.eqv..true.) then
            do j=1,nvell(k)
               n = internalFluxBoundaries(i)%nodes(j)
               write(99,'(3(f15.7,1x))') (xyd(m,n), m=1,3)
            end do
         else
            do j=1,nvell(k)
               n = internalFluxBoundaries(i)%nodes(j)
               write(99,'(i0)') n
            end do
         endif
      case default
         ! ignore the other boundary types
      end select
   enddo
case("external_overflow") ! ibtype 3, 13, 23
   do i = 1, numExternalFluxBoundaries
      k = externalFluxBoundaries(i)%indexNum
      select case(ibtype_orig(k))
      case(3,13,23)
         if (withCoordinates.eqv..true.) then
            write(99,'(i0,1x,i0)') nvell(k), ibtype_orig(k)
            do j=1,nvell(k)
               n = externalFluxBoundaries(i)%nodes(j)
               write(99,'(i0,1x,f15.7,1x, f15.7)') n, xyd(1,n), xyd(2,n)
            end do
         else if (xyz.eqv..true.) then
            do j=1,nvell(k)
               n = externalFluxBoundaries(i)%nodes(j)
               write(99,'(3(f15.7,1x))') (xyd(m,n), m=1,3)
            end do
         else
            do j=1,nvell(k)
               n = externalFluxBoundaries(i)%nodes(j)
               write(99,'(i0)') n
            end do
         endif
      case default
         ! ignore the other boundary types
      end select
   enddo
case default
write(6,*) 'Did not recognize the flux boundary type ',trim(boundaryType),'.'
stop
end select
close(99)
!-------------------------------------------------------------------
end program boundaryFinder
!-------------------------------------------------------------------

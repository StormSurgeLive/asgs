! example of compiling with gfortran with optimization turned on
! gfortran -O3 -ffree-line-length-none -o boundaryFinder.x -I/home/jason/asgs/trunk/output -I/usr/include  boundaryFinder.f90  -lnetcdff
!
include 'adcmesh.f90'
program boundaryFinder
use adcmesh
implicit none
character(1024) :: outputfile
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
character(1024) :: boundaryType
logical :: withCoordinates
logical :: xyz
integer :: argcount
integer :: i, j, k, m, n
!
i=0
withCoordinates = .false.
xyz = .false.
!
argcount = iargc() ! count up command line options
write(6,*) 'There are ',argcount,' command line options.'
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
write(6,*) 'mesh file name is ',trim(meshfilename)
call read14()
!
! open output file
open(unit=99, file=trim(adjustl(outputfile)), status='replace', action='write')
!
select case(trim(adjustl(boundaryType)))
case("inflow_flux")  ! ibtype 2, 12, 22, 52
   do i = 1, numSimpleFluxBoundaries
      k = simpleFluxBoundaries(i)%indexNum
      select case(ibtype_orig(k))
      case(2,12,22,52)
         write(99,'(i0,1x,i0)') nvell(k), ibtype_orig(k)
         do j=1,nvell(k)
            n = simpleFluxBoundaries(i)%nodes(j)
            if (withCoordinates.eqv..true.) then
               write(99,'(i0,1x,f15.7,1x, f15.7)') n, xyd(1,n), xyd(2,n)
            else
               write(99,'(i0)') n
            endif
         end do
      case default
         ! ignore the other boundary types
      end select
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

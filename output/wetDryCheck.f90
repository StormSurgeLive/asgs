!-----------------------------------------------------------------------
! wetDryCheck.f90: Check the nodecode.63 and noff.100 files to determine
! whether there are elements that are designated dry (noff=0) even 
! though all three nodes are wet (nodecode=1).
!-----------------------------------------------------------------------
program wetDryCheck
use adcmesh, only : read14, meshFileName, np, ne, nm
use asgsio, only : openFileForRead
implicit none
integer, allocatable :: noff(:)
integer, allocatable :: nodecode(:)
real(8) :: timeoute
integer :: ite
integer :: i, j, k, n(3), idum
integer :: noffornot ! 3 for consistent wet dry states, -3 for inconsistent
character(len=80) :: header
integer :: argcount
character(len=2048) :: cmdlinearg
character(len=2048) :: cmdlineopt

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
         case default 
            write(6,'(a,a,a)') "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
      end select
   end do 
end if
!
! Read the mesh file to get the fulldomain node and element tables.
call read14()
allocate(noff(ne))
allocate(nodecode(np))
!
! open the nodecode.63 file and read the file header lines
call openFileForRead(63, "nodecode.63")
read(63,'(a80)') header
read(63,'(a80)') header
!
! open the noff.100 file
call openFileForRead(100,"noff.100")
read(100,'(a80)') header
read(100,'(a80)') header
!
! open the noffornot.100 file
open(110,file="noffornot.100",status="replace",action="write")
write(110,'(a80)') header
write(110,'(a80)') header
!
! loop over the datasets, looking for elements that are designated dry
! but have three wet nodes
i=1 ! initialize time increment counter
do 
   ! read the time and time step information
   read(63,fmt=2120,end=9999) timeoute,ite
   read(100,fmt=2120,end=9999) timeoute,ite
   ! write it into the noffornot.100 file
   write(110,'(e15.8,2x,i0)') timeoute,ite
2120 format(2x,e20.10,5x,i10)
   ! read the node wet/dry states
   do j=1,np
      read(63,*) idum, nodecode(j)
   end do
   ! read the element wet/dry states
   do k=1,ne
      read(100,*) idum, noff(k)
   end do 
   !
   ! loop over elements
   do k=1,ne
      ! check to see if the element is dry but all three nodes are wet
      noffornot = 3
      if (noff(k).eq.0) then
         n(1:3) = nodecode(nm(k,1:3))
         if ( all(n.eq.1) ) then
            write(6,'(a,i0)') 'e=',k,' noff=',noff(k),' n(1)=',n(1),' n(2)=',n(2),' n(3)=',n(3)
            noffornot = -3
         endif
      endif
      write(110,'(2x,i8,2x,i0)') k, noffornot
   end do
   i=1+1
end do
              
9999 close(63)
   close(100)
   close(110)

!-----------------------------------------------------------------------
end program wetDryCheck
!-----------------------------------------------------------------------
      

! example of compiling with gfortran with optimization turned on
! gfortran -O3 -ffree-line-length-none -o tau0_gen.x -I/home/jason/asgs/trunk/output -I/usr/include  tau0_gen.f90  -lnetcdff
!
program tau0_gen
use adcmesh
implicit none
character(1024) :: nodal_attr_name 
character(1024) :: outputfile 
double precision, allocatable ::  dx_avg(:)  !(np)
double precision, allocatable ::  tau0_min(:) !(nodes)
double precision :: dx_crit = 1750.d0
double precision :: h_break = 10.d0 
double precision :: tau0_default = 0.030d0
double precision :: tau0_break = 0.02d0
double precision :: tau0_deep = 0.005d0
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
integer :: argcount
integer :: i, j
!
i=0
outputfile = 'tau0_fort.13'
argcount = iargc() ! count up command line options
write(6,'(a,i0,a)') 'There are ',argcount,' command line options.'
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--verbose")
      write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//"."
      verbose = .true.
   case("--meshfile")
      i = i + 1
      call getarg(i, meshFileName)
      write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(meshFileName)//"."
   case("--outputfile")
      i = i + 1
      call getarg(i, outputfile)
      write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(outputfile)//"."
   case("--default-tau0")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
      read(cmdlinearg,*) tau0_default   
   case default
      write(6,'(a,i0,a)') "WARNING: Command line option ",i," '"//TRIM(cmdlineopt)//"' was not recognized."
   end select
end do
!
! initializations
nodal_attr_name = 'primitive_weighting_in_continuity_equation'
!
! Load fort.14
write(6,'(a)') 'INFO: Mesh file name is  "'//trim(meshfilename)//'".'
call read14()
CALL computeNeighborTable()
write(6,'(a)') 'INFO: Tau0 results file is named  "'//trim(outputfile)//'".'
!
! compute the distance from every node to each of its neighbors and then
! divide by the total number of neighbors for that node
allocate(dx_avg(np))
dx_avg(:) = 0.0d0
do i=1, np
   dx_avg(i) = sum(sqrt( (x_cpp(i)-x_cpp(NeiTab(i,2:nneigh(i))))**2 &
            + (y_cpp(i)-y_cpp(NeiTab(i,2:nneigh(i))))**2 ) )/nneigh(i)
enddo
!
! now we have average dx for each node, test to see if greater than dx_crit
! and write fort.13 accordingly
allocate(tau0_min(np))
tau0_min(:) = tau0_default
! where the mesh spacing is greater or equal to the critical value 
where ( dx_avg.ge.dx_crit) 
   ! where the depth is also less than the break depth 
   where ( xyd(3,:).le.h_break )
      tau0_min = tau0_break
   elsewhere
      ! where the depth is greater than the break depth
      tau0_min = tau0_deep
   end where
elsewhere
   ! where mesh spacing is less than the critical value
   tau0_min = tau0_default
end where

! write output in fort.13 format for one attribute

open(unit=13,file=trim(outputfile),status='unknown')
write(13,'(a)') trim(nodal_attr_name)
write(13,'(i0)') count(tau0_min.ne.tau0_default)
do i=1, np
   if ( tau0_min(i).ne.tau0_default ) then
      write(13,'(i8,f13.6)') i, tau0_min(i)
   endif
end do
if (verbose.eqv..true.) then
   write(13,'(a)') 'average_distance_to_neighbors'
   write(13,'(i0)') np
   do i=1, np
      write(13,'(i0,1x,f13.6)') i, dx_avg(i)
   end do
endif
close(13)
!-------------------------------------------------------------------
end program tau0_gen
!-------------------------------------------------------------------

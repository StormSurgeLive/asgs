! Example of compiling with gfortran with debugging turned on:
! gfortran -g -O0 -Wall -ffree-line-length-none -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal -o eddyvis.x -I/home/jason/asgs/trunk/output -I/usr/include  eddyvis.f90  -lnetcdff
!
! Example of compiling with gfortran with optimization turned on:
! gfortran -O3 -ffree-line-length-none -o submergence_eddyvis.x -I/home/jason/asgs/trunk/output -I/usr/include eddyvis.f90 -lnetcdff
!
include 'adcmesh.f90'
include 'nodalattr.f90'
program eddyvis
use adcmesh
use nodalattr
implicit none
character(len=1024) :: outputfile
character(len=1024) :: nodal_attr_name  
real(8), allocatable :: minEdgeLength(:) ! (np) distance (m) from a node to nearest neighbor  
real(8), allocatable :: eddyViscosity(:) ! the nodal attribute
real(8) :: dryElevationAnyway ! (m) threshold elevation that forces nodes dry (+upward) 
real(8) :: defaultEddyViscosity 
logical :: useStartDry ! .true. if tau0 should be adjusted by the startdry value 
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
integer :: argcount
integer :: i, j
!
! initializations
defaultEddyViscosity = 20.d0
dryElevationAnyway = 0.d0
!
! process command line options
argcount = iargc() ! count up command line options
write(6,'(a,i0,a)') 'There are ',argcount,' command line options.'
i=0
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--verbose")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      verbose = .true.
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
   case("--default-eddy-viscosity")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) defaultEddyViscosity   
   case("--dry-elevation")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) dryElevationAnyway
   case("--use-startdry")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      useStartDry = .true.
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
!
! Open and read the mesh file.
write(6,'(a)') 'INFO: Reading mesh file.'
call read14()
write(6,'(a)') 'INFO: Finished reading mesh file.'
!
if (neighborTableComputed.eqv..false.) then
   call computeNeighborTable()
endif
!
! initialize the start dry value
if (useStartDry.eqv..true.) then
   nodal_attr_name =  'surface_submergence_state'
   call loadNodalAttribute(nodal_attr_name)
endif
!
! now generate and write out the horizontal eddy viscosity
write(6,'(a)') 'INFO: Computing horizontal eddy viscosity.'
allocate(eddyViscosity(np))
eddyViscosity = defaultEddyViscosity

!jgfdebug
write(6,'(a,i0)') 'numdefault=',count(eddyViscosity.eq.defaultEddyViscosity)
write(6,'(a,i0)') 'num2=',count(eddyViscosity.eq.2.d0)
write(6,'(a,i0)') 'num1=',count(eddyViscosity.eq.1.d0)

! areas that are normally considered wet by ADCIRC are set to 2.0
where (xyd(3,:).gt.-dryElevationAnyway) 
   eddyViscosity = 2.d0
end where

!jgfdebug
write(6,'(a,i0)') 'numdefault=',count(eddyViscosity.eq.defaultEddyViscosity)
write(6,'(a,i0)') 'num2=',count(eddyViscosity.eq.2.d0)
write(6,'(a,i0)') 'num1=',count(eddyViscosity.eq.1.d0)

! areas that have been selected to start dry set to 20.0
if (useStartDry.eqv..true.) then
   j=1
   do i=1,np
      if (i.eq.na(1)%nonDefaultNodes(j)) then
         eddyViscosity(i) = 20.d0
         j=j+1
      endif
      if (j.gt.na(1)%numNodesNotDefault) then
         exit
      endif
   end do
endif
! compute the lengths of the edges that connect each node to its neighbor
call computeNeighborEdgeLengthTable()
! find the min edge length attached to each node
allocate(minEdgeLength(np))
do i=1,np
   minEdgeLength(i) = minval(neighborEdgeLengthTable(i,2:nneigh(i)))
end do
! areas with really small elements (<10-12m) set to 1.0 to prevent
! some instabilities related to eddy viscosity on small elements 
where (minEdgeLength.lt.10.d0)
   eddyViscosity = 1.d0
end where

!jgfdebug
write(6,'(a,i0)') 'numdefault=',count(eddyViscosity.eq.defaultEddyViscosity)
write(6,'(a,i0)') 'num2=',count(eddyViscosity.eq.2.d0)
write(6,'(a,i0)') 'num1=',count(eddyViscosity.eq.1.d0)

write(6,*) 'INFO: Finished computing horizontal eddy viscosity.'      
write(6,*) 'INFO: Writing eddy viscosity nodal attribute.'
nodal_attr_name = 'average_horizontal_eddy_viscosity_in_sea_water_wrt_depth'
open(17,file='eddy_viscosity.'//trim(outputfile),action='write',status='replace')
write(17,'(A)') trim(nodal_attr_name)
! write the number of nondefault values
write(17,*) count(eddyViscosity.ne.defaultEddyViscosity)
do i=1, np
   if (eddyViscosity(i).ne.defaultEddyViscosity) then
      write(17,*) i, eddyViscosity(i) 
   endif
end do
close(17)
write(6,*) 'INFO: Finished writing eddy viscosity nodal attribute.'
!-------------------------------------------------------------------
end program eddyvis
!-------------------------------------------------------------------

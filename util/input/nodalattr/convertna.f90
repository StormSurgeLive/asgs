!--------------------------------------------------------------------------
! convertna.f90
!
! A program to write a nodal attribute in fort.63 format, or to reinterpolate
! a nodal attribute from one mesh to another.
!
!--------------------------------------------------------------------------
! Copyright(C) 2012-2017 Jason Fleming
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
! Compile with accompanying makefile.
!--------------------------------------------------------------------------
program convertna
use adcmesh
use ioutil
use logging
use nodalattr
implicit none
type(mesh_t) :: tm  ! target mesh
type(mesh_t) :: sm  ! source mesh
type(nodalAttrFile_t) :: naFile
character(len=1024) :: outputfile
character(len=1024) :: naName
real(8), allocatable :: targetNodalAttribute(:,:) ! (numVals,targetNP)
real(8), allocatable :: sourceNodalAttribute(:,:) ! (numVals, np)
real(8), allocatable :: dist(:) ! (np) distance (deg) from target node to each source node
logical, allocatable :: areDefaultValues(:) !.true. if the target node has all default values
integer :: sourceNodeNumber ! node number in source mesh closest to this node in target mesh
real(8) :: proximity ! distance (deg) from target node to nearest source node
real(8) :: closestSourceNeighbor! distance (deg) from source node to nearest source neighbor
logical :: interpolate ! .true. if we should interpolate nodat attributes from one mesh to another
logical :: write63 ! .true. if the n.a. data should be written to ascii fort.63 format
integer :: chunk ! reporting 10% for progress bar
integer :: step  ! reporting 3.33% for progress bar
integer :: progress ! percent progress reading file
integer :: errorIO
integer :: naUnit
integer :: i, j, jn
!
! initializations
write63 = .false.
interpolate = .false.
!
! process command line options
argcount = iargc() ! count up command line options
write(6,*) 'There are ',argcount,' command line options.'
i=0
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--source-meshfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      sm%meshFileName = trim(cmdlinearg)
   case("--nodal-attributes-file")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      naFile%nodalAttributesFileName = trim(cmdlinearg)
   case("--outputfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      outputfile = trim(cmdlinearg)
   case("--nodal-attribute")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      naName = trim(cmdlinearg)
   case("--target-meshfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      tm%MeshFileName = trim(cmdlinearg)
   case("--write63")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      write63 = .true.
   case("--interpolate")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      interpolate = .true.
   case("--cpp")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      i = i + 1
      call getarg(i, cmdlinearg)
      read(cmdlinearg,*) sm%slam0
      tm%slam0 = sm%slam0
      i = i + 1
      call getarg(i, cmdlinearg)
      read(cmdlinearg,*) sm%sfea0
      tm%sfea0 = sm%sfea0
      write(6,*) "INFO: slam0=",sm%slam0," and sfea0=",sm%sfea0,"."
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
!
! load the specified nodal attribute
call loadNodalAttribute(naName, naFile)
!
! write in fort.63 format if specified to do so
if (write63.eqv..true.) then
   call writeNodalAttribute63(naName, outputfile, naFile)
   stop
endif
!
! interpolate nodal attribute from one mesh to another if specified
if (interpolate.eqv..true.) then
   ! Open and read the mesh file.
   write(6,'(a)') 'INFO: Reading source mesh file.'
   call read14(sm)
   write(6,'(a)') 'INFO: Finished reading source mesh file.'
   write(6,'(a)') 'INFO: Computing neighbor table for source mesh file.'
   call computeNeighborTable(sm)
   write(6,'(a)') 'INFO: Finished computing neighbor table for source mesh file.'
   write(6,'(a)') 'INFO: Computing neighbor edge length table for source mesh file.'
   call computeNeighborEdgeLengthTable(sm)
   write(6,'(a)') 'INFO: Finished computing neighbor edge length table for source mesh file.'
   ! read the node table from the target mesh file so we can use the node
   ! locations for interpolations
   write(6,*) 'INFO: Reading target mesh file.'
   call read14(tm)
   write(6,*) 'INFO: Finished reading target mesh file.'
   ! compute the cpp projection
   call computeCPP(tm)
   !
   ! now go through the target nodes and match the nodal attributes
   ! to the ones from the source mesh
   allocate(targetNodalAttribute(naFile%na(1)%numVals,tm%np))
   allocate(sourceNodalAttribute(naFile%na(1)%numVals,sm%np))
   ! populate full array of source nodal attribute so we can grab data
   ! from it at any location
   write(6,*) 'INFO: Fully populating source nodal attribute array.'
   do i=1,sm%np
      sourceNodalAttribute(:,i) = naFile%na(1)%defaultVals(:)
   end do
   do i=1,naFile%na(1)%numNodesNotDefault
      sourceNodalAttribute(:,naFile%na(1)%nonDefaultNodes(i)) = naFile%na(1)%nonDefaultVals(:,i)
   end do
   allocate(dist(naFile%na(1)%numNodesNotDefault))
   write(6,*) 'INFO: Assigning target nodal attributes.'
   ! compute parameters related to printing a progress bar
   progress=10
   chunk=tm%np/10
   step=tm%np/30
   do i=1,tm%np
      ! update progress bar
      !write(*,*) i
      if (mod(i,chunk).eq.0) then
         write(6,'(i0,"%")',advance='no') progress
         progress = progress + 10
      elseif (mod(i,step).eq.0) then
         write(6,'(a1)',advance='no') '.'
      endif
      ! calculate the distance between this node and each node in the source
      ! mesh that does not have a default nodal attribute value
      do j=1,naFile%na(1)%numNodesNotDefault
         dist(j) = sqrt( (tm%x_cpp(i)-sm%x_cpp(naFile%na(1)%nonDefaultNodes(j)))**2 &
           + (tm%y_cpp(i)-sm%y_cpp(naFile%na(1)%nonDefaultNodes(j)))**2 )
      end do
      ! find the node number of the closest node
      sourceNodeNumber = naFile%na(1)%nonDefaultNodes(minloc(dist,1))
      proximity = minval(dist)
      ! if the target node is closer to the source node than half the distance
      ! to the source node's nearest neighbor, then the target node has
      ! the same nodal attribute value as the source node; otherwise the
      ! target node has the default value
      closestSourceNeighbor = minval(sm%edgeLengthTable(sourceNodeNumber,2:sm%nneigh(sourceNodeNumber)))
      if (proximity.lt.closestSourceNeighbor) then
         targetNodalAttribute(:,i) = sourceNodalAttribute(:,sourceNodeNumber)
      else
         targetNodalAttribute(:,i) = naFile%na(1)%defaultVals(:)
      endif
   end do
   write(6,*) 'INFO: Finished assigning target nodal attributes.'
   write(6,*) 'INFO: Writing target nodal attributes.'
   ! count the nondefault values
   allocate(areDefaultValues(sm%np))
   areDefaultValues = .true.
   do i=1,sm%np
      do j=1,naFile%na(1)%numVals
        ! if any of the nodal attribute values at this node are different
        ! from the default value(s), then this is a non default node
        if ( abs(targetNodalAttribute(j,i)-naFile%na(1)%defaultVals(j)).gt.1.d-6 ) then
            areDefaultValues(i) = .false.
            exit
         endif
      end do
   enddo
   naUnit = availableUnitNumber()
   open(naUnit,file=trim(outputfile),action='write',status='replace')
   write(naUnit,'(A)') trim(naName)
   ! write the number of nondefault values
   write(naUnit,*) count(areDefaultValues.eqv..false.)
   do i=1,sm%np
      if (areDefaultValues(i).eqv..false.) then
         write(naUnit,130) i,(targetNodalAttribute(j,i), j=1,naFile%na(1)%numVals)
      endif
   end do
   close(naUnit)
   write(6,*) 'INFO: Finished writing nondefault surface roughness values.'
   130   format(i0,12(2x,f15.7))
endif
!-----------------------------------------------------------------------
end program convertna
!-----------------------------------------------------------------------

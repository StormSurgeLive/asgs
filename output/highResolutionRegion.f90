!----------------------------------------------------------------------
! highResolutionRegion.f90 : outputs mesh properties that delineate
! the subregion of a mesh that has high resolution
!----------------------------------------------------------------------
! Copyright(C) 2016 Jason Fleming
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
!----------------------------------------------------------------------
program highResolutionRegion
use adcmesh
implicit none
character(1024) :: outputfile
real(8) :: lowerLeftLon ! lowest longitude in bounding box (degrees West)
real(8) :: lowerLeftLat  ! lowest latitude in bounding box (degrees North)
real(8) :: upperRightLon ! highest longitude in bounding box (degrees West)
real(8) :: upperRightLat ! lowest latitude in bounding box (degrees North)
integer :: highestResNodeNumber(1) ! single element array to hold the node number where highest res occurs
real(8) :: highestResLon ! longitude of node with lowest avg edge lengths connected (degrees West)
real(8) :: highestResLat ! latitude of node with lowest avg edge lengths connected (degrees North)
real(8) :: furthestHighRes ! distance (m) from highest res location to furthest threshold location 
real(8) :: highResCircleDiameterDegrees ! 2x distance (degrees) from highest res location to furthest threshold location 
real(8) :: testDist  ! distance (m) from highest res location to another node
real(8), allocatable ::  dx_avg(:)  !(np) average edge length around a node (m)
real(8) :: resolutionThreshold ! highest mesh spacing within high resolution region (m)
logical :: firstNodeFound ! true if we found a node that should be in the bounding box
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
integer :: argcount
integer :: i, j
!
i=0
outputfile = 'highResolutionRegion.properties'
resolutionThreshold = 500.0 
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
   case("--resolutionthreshold")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
      read(cmdlinearg,*) resolutionThreshold   
   case default
      write(6,'(a,i0,a)') "WARNING: Command line option ",i," '"//TRIM(cmdlineopt)//"' was not recognized."
   end select
end do
!
! Load fort.14
write(6,'(a)') 'INFO: Mesh file name is  "'//trim(meshfilename)//'".'
call read14()
CALL computeNeighborTable()
write(6,'(a)') 'INFO: The high resolution region properties will be written to "'//trim(outputfile)//'".'
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
! Find the node in the mesh with the highest resolution (minimum
! mesh spacing) and the distance to the farthest node that has 
! a resolution above the resolution threshold
highestResNodeNumber(:) = minloc(dx_avg)
write(6,*) 'highestResNodeNumber is ',highestResNodeNumber(1) ! jgfdebug
highestResLon = xyd(1,highestResNodeNumber(1))
highestResLat = xyd(2,highestResNodeNumber(1))
write(6,*) 'highestResLon is ',highestResLon ! jgfdebug
write(6,*) 'highestResLat is ',highestResLat ! jgfdebug
furthestHighRes = 0.0d0
highResCircleDiameterDegrees = 0.d0
open(unit=12,file='stuff.log',status='replace') !jgfdebug
do i=1, np
   if ( dx_avg(i).lt.resolutionThreshold) then
   ! meters
      testDist = sqrt((x_cpp(highestResNodeNumber(1))-x_cpp(i))**2 + (y_cpp(highestResNodeNumber(1))-y_cpp(i))**2)
      furthestHighRes = max(testDist,furthestHighRes)
      ! degrees
      testDist = sqrt((highestResLon-xyd(1,i))**2 + (highestResLat-xyd(2,i))**2)
      write(12,*) 'testDist is ',testDist
      highResCircleDiameterDegrees = max(testDist,highResCircleDiameterDegrees)
   endif
enddo
close(12)
!
! now we have average dx for each node, test to see if greater than 
! resolutionThreshold
firstNodeFound = .false.
do i=1,np
   if (dx_avg(i).lt.resolutionThreshold) then
      if (firstNodeFound.eqv..false.) then
         lowerLeftLon = xyd(1,i) 
         lowerLeftLat = xyd(2,i)
         upperRightLon = xyd(1,i)
         upperRightLat = xyd(2,i)
         firstNodeFound = .true.
      else
         lowerLeftLon = min(xyd(1,i),lowerLeftLon)
         lowerLeftLat = min(xyd(2,i),lowerLeftLat)
         upperRightLon = max(xyd(1,i),upperRightLon)
         upperRightLat = max(xyd(2,i),upperRightLat)
      endif
   endif
end do
!
! Check to see if there are any element edges less than the threshold
if (firstNodeFound.eqv..false.) then
   write(6,'("ERROR: highResolutionRegion: The mesh does not contain any nodes whose average length of connected element edges is shorter than the specified threshold resolution.")')
   stop
endif
!
! write the output properties file with the high resolution region properties
open(unit=12,file=trim(outputfile),status='replace')
write(12,'("highResolutionBoundingBox : ",4(f15.7))') lowerLeftLon, lowerLeftLat, upperRightLon, upperRightLat
write(12,'("highResolutionLonLatDegrees : ",2(f15.7))') highestResLon, highestResLat
write(12,'("highResolutionCircleDiameterDegrees : ",f15.7)') highResCircleDiameterDegrees
write(12,'("distanceToThresholdResolutionMeters :",f18.7)') furthestHighRes
write(12,'("resolutionThresholdMeters : ",f15.7)') resolutionThreshold
close(12)
!-------------------------------------------------------------------
end program highResolutionRegion
!-------------------------------------------------------------------

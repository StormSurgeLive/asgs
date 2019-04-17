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
use ioutil
use logging
implicit none
type(mesh_t) :: m
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
      call getarg(i, m%meshFileName)
      write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(m%meshFileName)//"."
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
write(6,'(a)') 'INFO: Mesh file name is  "'//trim(m%meshfilename)//'".'
call read14(m)
CALL computeNeighborTable(m)
write(6,'(a)') 'INFO: The high resolution region properties will be written to "'//trim(outputfile)//'".'
!
! compute the distance from every node to each of its neighbors and then
! divide by the total number of neighbors for that node
allocate(dx_avg(m%np))
dx_avg(:) = 0.0d0
do i=1, m%np
   dx_avg(i) = sum(sqrt( (m%x_cpp(i)-m%x_cpp(m%NeiTab(i,2:m%nneigh(i))))**2 &
            + (m%y_cpp(i)-m%y_cpp(m%NeiTab(i,2:m%nneigh(i))))**2 ) )/m%nneigh(i)
enddo
!
! Find the node in the mesh with the highest resolution (minimum
! mesh spacing) and the distance to the farthest node that has 
! a resolution above the resolution threshold
highestResNodeNumber(:) = minloc(dx_avg)
highestResLon = m%xyd(1,highestResNodeNumber(1))
highestResLat = m%xyd(2,highestResNodeNumber(1))
furthestHighRes = 0.0d0
highResCircleDiameterDegrees = 0.d0
do i=1, m%np
   if ( dx_avg(i).lt.resolutionThreshold) then
   ! meters
      testDist = sqrt((m%x_cpp(highestResNodeNumber(1))-m%x_cpp(i))**2 + (m%y_cpp(highestResNodeNumber(1))-m%y_cpp(i))**2)
      furthestHighRes = max(testDist,furthestHighRes)
      ! degrees
      testDist = sqrt((highestResLon-m%xyd(1,i))**2 + (highestResLat-m%xyd(2,i))**2)
      highResCircleDiameterDegrees = max(testDist,highResCircleDiameterDegrees)
   endif
enddo
!
! now we have average dx for each node, test to see if greater than 
! resolutionThreshold
firstNodeFound = .false.
do i=1,m%np
   if (dx_avg(i).lt.resolutionThreshold) then
      if (firstNodeFound.eqv..false.) then
         lowerLeftLon = m%xyd(1,i) 
         lowerLeftLat = m%xyd(2,i)
         upperRightLon = m%xyd(1,i)
         upperRightLat = m%xyd(2,i)
         firstNodeFound = .true.
      else
         lowerLeftLon = min(m%xyd(1,i),lowerLeftLon)
         lowerLeftLat = min(m%xyd(2,i),lowerLeftLat)
         upperRightLon = max(m%xyd(1,i),upperRightLon)
         upperRightLat = max(m%xyd(2,i),upperRightLat)
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
write(12,'("highResolutionRectangleLowerLeftLonDegrees : ",f15.7)') lowerLeftLon
write(12,'("highResolutionRectangleLowerLeftLatDegrees : ",f15.7)') lowerLeftLat
write(12,'("highResolutionRectangleUpperRightLonDegrees : ",f15.7)') upperRightLon
write(12,'("highResolutionRectangleUpperRightLatDegrees : ",f15.7)') upperRightLat
write(12,'("highResolutionCenterLonDegrees : ",f15.7)') highestResLon
write(12,'("highResolutionCenterLatDegrees : ",f15.7)') highestResLat
write(12,'("highResolutionCircleDiameterDegrees : ",f15.7)') highResCircleDiameterDegrees
write(12,'("highResolutionCircleDiameterMeters :",f18.7)') furthestHighRes
write(12,'("highResolutionThresholdMeters : ",f15.7)') resolutionThreshold
close(12)
!-------------------------------------------------------------------
end program highResolutionRegion
!-------------------------------------------------------------------

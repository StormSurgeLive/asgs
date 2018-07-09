!----------------------------------------------------------------------
! modMeshData.f90 : Modify data on an ADCIRC mesh using polygon regions.
!----------------------------------------------------------------------
! Copyright(C) 2017 Jason Fleming
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
! Analyst supplies a mesh, two polygons (it is assumed that one is
! entirely inside the other), and optionally an offset file, 
! a multiplier and a default value. 
!
! This utility finds the nodes that are inside the inner polygon and 
! multiplies their offset values by the multiplier (sets them to the 
! multiplier if there are no offset values). 
!
! Nodes that are outside the outer polygon are set to
! the default value, or to zero if there is no default value.
!
! Nodes that are between the inner and outer polygons are linearly 
! ramped according to their distance from inner and outer polygons.
!
! TODO: This only handles water level offset files but this could 
! be improved so that it handles NWS4 files, fort.13 files, fort.63
! files, etc.
! TODO: netCDF support 
!----------------------------------------------------------------------
program modMeshData
!----------------------------------------------------------------------
use adcmesh
use asgsio
use logging
implicit none
character(2048) :: dataFileExtension ! output file name after . something like 13, 14, 15, 63, 222 etc
integer :: lastSlashPosition ! used for trimming full path from a filename
integer :: lastSlashPositionMesh ! used for trimming full path from a mesh filename
character(2048) :: meshFileBase ! mesh file name sans full path, if any
integer :: lastDotPosition   ! to determine file extension
character(80) :: dataFileCommentLine
integer :: unitnumber ! i/o unit for the fulldomain output file
integer :: numVertices ! number of vertices in the polygon file
real(8) :: circleCenterLatitude(2)
real(8) :: circleCenterLongitude(2)
real(8) :: circleDiameterDegrees(2)
real(8) :: upperRightLatitude(2)
real(8) :: upperRightLongitude(2)
real(8) :: lowerLeftLatitude(2)
real(8) :: lowerLeftLongitude(2)
!
meshFileName = "null"
!
!
argcount = command_argument_count() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
      case('--meshfile')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         meshFileName = trim(cmdlinearg)
      case('--offsetfile')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         offsetFile = trim(cmdlinearg)
         useOffset = .true.
      case('--modifiedfile')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         modifiedFile = trim(cmdlinearg)
      case('--defaultvalue')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         read(cmdlinearg,*) defaultValue
      case('--polyshape')
         i = i + 1
         call getarg(i, polyposition) ! either "inner" or "outer"
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         select case(trim(polyposition))
         case("inner","Inner","INNER")
            ipoly = 1
         case("outer","Outer","OUTER")
            ipoly = 2
         case default
            write(6,'(a,a,a,a,a)') 'ERROR: polyshape position "'//trim(cmdlinearg)//'" not recognized; must be either "inner" or "outer"'
            stop
         end select
         i = i + 1
         call getarg(i, polyshape(ipoly))  ! "circle", "rectangle", or "polygon"
         select case(trim(polyshape(ipoly)))
         case('circle')
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) circleCenterLongitude(ipoly)                    
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) circleCenterLatitude(ipoly)
            write(6,'("INFO: '//trim(polyposition)//'Circle is centered at ",f15.7," degrees west longitude ",f15.7," degrees north latitude.")') circleCenterLongitude(ipoly), circleCenterLatitude(ipoly)           
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) circleDiameterDegrees(ipoly)
            write(6,'("INFO: '//trim(polyposition)//' circle diameter is ",f15.7," degrees.")') circleDiameterDegrees(ipoly)
            call getarg(i, cmdlinearg)
         case('rectangle')
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) lowerLeftLongitude(ipoly)                  
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) lowerLeftLatitude(ipoly)
            write(6,'("INFO: '//trim(polyposition)//' rectangle lower left coordinates are ",f15.7," degrees west longitude ",f15.7," degrees north longitude.")') lowerLeftLongitude(ipoly), lowerLeftLatitude(ipoly)
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) upperRightLongitude(ipoly)                    
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) upperRightLatitude(ipoly)
            write(6,'("INFO: '//trim(polyposition)//' rectangle upper right coordinates are ",f15.7," degrees west longitude ",f15.7," degrees north latitude.")') upperRightLongitude(ipoly), upperRightLatitude(ipoly)
         case default
            write(6,'(a,a,a)') 'WARNING: Command line option "',trim(cmdlineopt),'" was not recognized.'
         end select      
      case default
         write(6,'(a,a,a)') 'WARNING: Command line option "',trim(cmdlineopt),'" was not recognized.'
      end select
   end do
end if
!
! trim off the full path so we just have the file name
lastSlashPosition = index(trim(offsetFile),"/",.true.) 
offsetFileBase = trim(offsetFile(lastSlashPosition+1:))
lastDotPosition = index(trim(offsetFileBase),'.',.true.)
offsetFileExtension = trim(dataFileBase(lastDotPosition+1:))
!
! get mesh file name without full path (if any)
lastSlashPositionMesh = index(trim(meshFileName),"/",.true.)
meshFileBase = trim(meshFileName(lastSlashPositionMesh+1:))
!
!
allocate(within(np,2))
allocate(withinOuterOnly(np))
allocate(outsideBoth(np))
within(:,:) = .false.
withinOuterOnly(:) = .false.
outsideBoth(:) = .false.
!
! read mesh
call read14()
! select nodes inside each poly shape
numInside(1:2) = 0
do ipoly=1,2
   select case(trim(polyshape(ipoly)))
   case('circle')
      do n=1, np 
         if ( sqrt((circleCenterLongitude(ipoly)-xyd(1,n))**2 + (circleCenterLatitude(ipoly)-xyd(2,n))**2).lt.(0.5*circleDiameterDegrees(ipoly)) ) then
            within(n,ipoly) = .true.
            numWithin(ipoly) = numWithin(ipoly) + 1
         end if
      end do
   case('rectangle')
      do n=1, np
         if ( xyd(2,n).le.upperRightLatitude(ipoly) ) then
            if ( xyd(2,n).ge.lowerLeftLatitude(ipoly) ) then
               if ( xyd(1,n).le.upperRightLongitude(ipoly) ) then      
                  if ( xyd(1,n).ge.lowerLeftLongitude(ipoly) ) then      
                     within(n,ipoly) = .true.
                     numWithin(ipoly) = numWithin(ipoly) + 1
                  endif
               endif
            endif
         endif
      end do
   case default
      write(6,'(a,a,a)') 'ERROR: The result shape "',trim(resultShape),'" was not recognized.'
      stop
   end select
end do
!
! make a list of nodes that are inside the inner shape
allocate(nodesWithinInner(numWithin(1)))
innerNodeCounter = 1
do n=1,np
   if ( within(n,1).eqv..true. ) then
      nodesWithinInner(innerNodeCounter) = n
      innerNodeCounter = innerNodeCounter + 1
   endif
end do 
!
! make a list of nodes that are between the inner and outer shapes
numBetween = numWithin(2) - numWithin(1)
allocate(nodesBetween(numBetween))
betweenNodeCounter = 1
do n=1,np
   if ( (within(n,1).eqv..false.).and.(within(n,2).eqv..true.) ) then
      nodesBetween(betweenNodeCounter) = n
      betweenNodeCounter = betweenNodeCounter + 1
   endif
end do    
!
! find nodes that are outside both polyshapes (assuming the
! inner shape is entirely inside the outer shape)
numOutside = np - numWithin(2)
allocate(nodesOutside(numOutside))
outsideNodeCounter = 1
do n=1,np
   if ( within(n,2).eqv..false.) then
      nodesOutside(numOutsideCounter) = .true.
      outsideNodeCounter = outsideNodeCounter + 1
   endif
end do
!
! for the nodes between inner and outer polys, compute a linear spatial
! ramp value based on the nearest node inside the inner shape and the 
! nearest node outside the outer shape
allocate(rampValue(numBetween)
do n=1, numBetween
   distanceToClosestInnerNode = huge(99999.0)
   ! find distance to closest inner node
   do k=1, numWithin(1)
      dist = sqrt( (xyd(1,nodesBetween(n))-xyd(1,nodesWithinInner(k)))**2 + (xyd(2,nodesBetween(n))-xyd(2,nodesWithinInner(k)))**2 )
      if ( dist.lt.distanceToClosestInnerNode) then
         distanceToClosestInnerNode = dist
      endif
   end do
   ! find distance to closest outer node
   distanceToClosestOuterNode = huge(99999.0)
   do k=1, numOutside
      dist = sqrt( (xyd(1,nodesBetween(n))-xyd(1,nodesOutside(k)))**2 + (xyd(2,nodesBetween(n))-xyd(2,nodesOutside(k)))**2 )
      if ( dist.lt.distanceToClosestOuterNode) then
         distanceToClosestOuterNode = dist
      endif
   end do
   rampValue(n) = distanceToClosestOuterNode / (distanceToClosestInnerNode + distanceToClosestOuterNode)
end do 
!
! Read the fulldomain output file to determine its properties
read(unitNumber,*,end=246,err=248,iostat=errorio) numSnaps, numValuesPerDataset, tInterval, Interval, nCol
lineNum=lineNum+1
if ( (np.ne.numValuesPerDataset).and.(trim(dataCenter).eq.'Node') ) then
   write(6,'(a,i0,a,i0,a)') 'ERROR: The output file contains ',numValuesPerDataset,        &
     ' nodes, but the mesh file contains ',np,' nodes.'
    write(6,'(a)') 'ERROR: The output file does not correspond to the mesh file.'
   close(UnitNumber)
   stop
endif
if ( (ne.ne.numValuesPerDataset).and.(trim(dataCenter).eq.'Cell') ) then
   write(6,'(a,i0,a,i0,a)') 'ERROR: The output file contains ',numValuesPerDataset,        &
     ' elements, but the mesh file contains ',ne,' elements.'
   write(6,'(a)') 'ERROR: The output file does not correspond to the mesh file.'
   close(UnitNumber)
   stop
endif
!
! Allocate arrays to hold the data from the fulldomain file
select case(netCDFDataType)
case(NF90_DOUBLE)
   allocate(adcirc_data(numValuesPerDataSet,nCol))
case(NF90_INT)
   allocate(adcirc_idata(numValuesPerDataSet,nCol))      
case default
   write(6,'(a)') 'ERROR: Unsupported data type.'
end select
!
! Set the number of components
select case(nCol)
case(1)
   dataRank = 'Scalar'
case(2)           
   dataRank = '2DVector'
case default 
   write(6,'("ERROR: resultScope.f90: ADCIRC output files with ",i0," columns are not supported.")') nCol
   stop
end select
!
! open the subdomain ascii adcirc file that will hold the data
resultShapeOutputFileName = 'sub-' // trim(resultShape) // '_' // trim(dataFile) 
open(11,file=trim(resultShapeOutputFileName),status='replace',action='write')
! write header info
write(11,'(a)') trim(agrid) // trim(resultShapeOutputFileName) // ' ' // trim(dataFileCommentLine)
! write the header data to the resultshape file
write(11,1010) numSnaps, numValuesPerSubset, tInterval, Interval, nCol
!
!  R E A D   I N   F U L L D O M A I N   D A T A   
!      A N D   W R I T E   O U T   R E S U L T  S H A P E   D A T A
!           
SS=1 ! jgf: initialize the dataset counter
lineNum = 1 ! initialize the line number counter
DO   ! jgf: loop until we run out of data 
   read(unitNumber,*,end=244,err=248,iostat=errorio) SnapR, SnapI
   j=0
   do n=1,numValuesPerDataSet
      select case(trim(dataRank))
      case("Scalar")                    ! scalar data
         if (netCDFDataType.eq.NF90_DOUBLE) then
            read(unitNumber,*,end=246,err=248,iostat=errorio) j,Temp1
            lineNum = lineNum + 1
            adcirc_data(j,1) = Temp1
         else
            read(unitNumber,*,end=246,err=248,iostat=errorio) j,adcirc_idata(n,nCol)
            lineNum = lineNum + 1
         endif
      case("2DVector")                  ! 2D vector data
         read(unitNumber,*,end=246,err=248,iostat=errorio) j,Temp1,Temp2
         lineNum = lineNum + 1
         adcirc_data(j,1) = Temp1
         adcirc_data(j,2) = Temp2
      case default
         write(6,'(a,a,a)') 'ERROR: resultScope.f90: ',trim(dataRank),' data rank is not supported.'
         stop
      end select
   enddo 
   !
   ! nonsparse ascii output to resultshape output file
   write(11,2120) SnapR, SnapI
   do k=1,numValuesPerDataSet
      if ( (trim(dataCenter).eq.'Node').and.(within(k).eqv..true.) ) then
         if (isInteger.eqv..true.) then
            write(11,2452) full2subNodes(k), (adcirc_idata(k,j),j=1,nCol)
         else
            write(11,2453) full2subNodes(k), (adcirc_data(k,j),j=1,nCol)
         endif
      end if
      if ( (trim(dataCenter).eq.'Cell').and.(elementWithin(k).eqv..true.) ) then
         if (isInteger.eqv..true.) then
            write(11,2452) full2subElements(k), (adcirc_idata(k,j),j=1,nCol)
         else
            write(11,2453) full2subElements(k), (adcirc_data(k,j),j=1,nCol)
         endif
      end if
   end do
   write(6,advance='no',fmt='(i4)') SS
   SS = SS + 1
end do
244 close(unitNumber)
close(11)

write(6,'(a)') 'INFO: resultScope.f90: Finished writing file.'
write(6,'(a,i0,a)') 'INFO: resultScope.f90: Wrote ',SS-1,' data sets.'
stop
 1010 format(1x,i10,1x,i10,1x,e15.7e3,1x,i8,1x,i5,1x,'FileFmtVersion: ',i10)
 1011 format(1x,i10,1x,i10,1x,e15.7e3,1x,i8,1x,i5,1x,i2,1x,'FileFmtVersion: ',i10)
 2120 format(2x,1pe20.10e3,5x,i10)
 2121 format(2x,1pe20.10e3,5x,i10,99(1pe20.10e3,2x))
 2452 format(2x, i8, 2x, i0, 5x, i0, 5x, i0, 5x, i0)
 2453 format(2x, i8, 2x, 1pe20.10e3, 1pe20.10e3, 1pe20.10e3, 1pe20.10e3)
 2454 format(2x, i8, 2x, 99(1pe20.10e3))
      ! We jump to this section if there was an error reading a file.
246   write(6,'(a)') 'ERROR: Unexpectedly reached end-of-file.' ! END jumps here
248   write(6,'(a)') 'ERROR: I/O error during file access.'     ! ERR jumps here
      write(6,'(a,i0,a,i0,a)') 'INFO: Attempted to read line ',lineNum,' in dataset ',SS,'.' ! ERR jumps here      
      write(6,'(a,i0,a)') 'The numerical code of the i/o error was ',errorio,'.'
!----------------------------------------------------------------------
end program resultScope
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! SUBROUTINE PNPOLY 
!----------------------------------------------------------------------
! Determine whether a point is inside a polygon. 
!
! A vertical line is drawn thru the point in question. if it 
! crosses the polygon an odd number of times, then the 
! point is inside of the polygon. 
!
! -1 if the point is outside of the polygon, 
! 0 if the point is on an edge or at a vertex, 
! 1 if the point is inside of the polygon. 
! 
! The vertices may be listed clockwise or anticlockwise. 
! the first may optionally be repeated, if so n may 
! optionally be increased by 1. 
! The input polygon may be a compound polygon consisting 
! of several separate subpolygons. If so, the first vertex 
! of each subpolygon must be repeated, and when calculating 
! n, these first vertices must be counted twice. 
! 
! written by Randolph Franklin, university of ottawa, 7/70.
!
! Copyright (c) 1970-2003, Wm. Randolph Franklin
!
! Permission is hereby granted, free of charge, to any person 
! obtaining a copy of this software and associated documentation files
! the "Software"), to deal in the Software without restriction, 
! including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software,
! and to permit persons to whom the Software is furnished to do so, 
! ubject to the following conditions:
!
! Redistributions of source code must retain the above copyright 
! notice, this list of conditions and the following disclaimers.
!
! Redistributions in binary form must reproduce the above copyright
! notice in the documentation and/or other materials provided with
! the distribution.
!
! The name of W. Randolph Franklin may not be used to endorse or 
! promote products derived from this Software without specific 
! prior written permission. 
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS 
! BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN 
! ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN 
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
! SOFTWARE. 
!
!----------------------------------------------------------------------
subroutine pnpoly(px,py,xx,yy,n,within) 
implicit none
real(8) :: px, py ! point coordinates
real(8), allocatable :: x(:),y(:) ! distances from point to each polygon vertex
real(8) :: xx(*),yy(*) ! polygon vertices 
real(8) :: condition
logical mx,my,nx,ny 
integer within
integer i,j,n
!
! determine the distance from the point to each vertex of the polygon
allocate(x(n),y(n))
do i=1,n 
   x(i)=xx(i)-px 
   y(i)=yy(i)-py
end do
within=-1 
do i=1,n 
   j=1+mod(i,n) 
   mx = .false.
   nx = .false.
   my = .false.
   ny = .false.
   if (x(i).ge.0.d0) mx = .true.
   if (x(j).ge.0.d0) nx = .true.
   if (y(i).ge.0.d0) my = .true.
   if (y(j).ge.0.d0) ny = .true.
   if(.not.((my.or.ny).and.(mx.or.nx)).or.(mx.and.nx)) then
      cycle
   endif
   if(.not.(my.and.ny.and.(mx.or.nx).and..not.(mx.and.nx))) then
      condition = (y(i)*x(j)-x(i)*y(j))/(x(j)-x(i))
      if (condition.lt.0.d0) cycle
      if (condition.eq.0.d0) then
         within=0
         return
      endif
      if (condition.gt.0) then
         within=-within
         cycle
      endif      
   endif
   within=-within 
   cycle
end do
deallocate(x)
deallocate(y)
!----------------------------------------------------------------------
end subroutine pnpoly
!----------------------------------------------------------------------

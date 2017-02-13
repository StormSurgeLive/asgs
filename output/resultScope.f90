!----------------------------------------------------------------------
! resultScope.f90 : Cut out solution files on an analyst defined
! subdomain. Useful when the fulldomain / full time series output 
! data are too large to feasibly analyze/visualize. 
!----------------------------------------------------------------------
! Copyright(C) 2009 Seizo Tanaka
! Copyright(C) 2016--2017 Jason Fleming
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
! Seizo Tanaka: 
! * System can't allow "CLOCK OPERATION" for node connection in element.
!   And Node number and element number must be sequence in grid data.
!    Release Note;;
!        Trial  2009. 7.21. Kick Off
!               2009. 7.10. Minimum Composition
!----------------------------------------------------------------------
program resultScope
!----------------------------------------------------------------------
use netcdf
use adcmesh
use ioutil
use asgsio
use logging
implicit none
type(mesh_t) :: fm ! full domain mesh
type(meshNetCDF_t) :: fn ! fulldomain mesh netcdf IDs
type(fileMetaData_t) :: fd   ! fulldomain data file
!
type(mesh_t) :: rm ! resultShape mesh 
type(meshNetCDF_t) :: rn ! resultShape mesh netcdf IDs
type(fileMetaData_t) :: rd   ! resultShape data file
!
real(8), allocatable :: adcirc_data(:,:)
integer, allocatable :: adcirc_idata(:,:)
integer :: snapi
real(8) :: snapr
character(120) :: dataRank
character(1000) :: Line
character(1000) :: polygonFile ! name of file with polygon vertices
character(1000) :: resultShape ! shape of extraction from full domain data
character(1000) :: resultShapeMeshFileName ! name of mesh file extracted full domain mesh
character(2048) :: dataFileBase ! output file name sans full path, if any
character(2048) :: dataFileExtension ! output file name after . something like 13, 14, 15, 63, 222 etc
integer :: lastSlashPosition ! used for trimming full path from a filename
integer :: lastSlashPositionMesh ! used for trimming full path from a mesh filename
character(2048) :: meshFileBase ! mesh file name sans full path, if any
integer :: lastDotPosition   ! to determine file extension
character(80) :: dataFileCommentLine
integer :: i, j, k, e, n, SS
logical :: meshonly    ! .true. if we are just subsetting the mesh
logical, allocatable :: within(:) ! (np) .true. if a node is within the resultshape
logical, allocatable :: elementWithin(:) ! (ne) .true. if an element is within the resultshape
integer inOnOut ! 1 if a node is within the polygon, 0 if it is on the polygon, and -1 if it is outside
integer, allocatable :: sub2fullNodes(:) ! (nps) subdomain -> fulldomain index number mapping of each resultshape node
integer, allocatable :: full2subNodes(:) ! (np) fulldomain -> subdomain index mapping of the resultshape nodes 
integer, allocatable :: sub2fullElements(:) ! (nes) subdomain -> fulldomain index number mapping of element in resultshape
integer, allocatable :: full2subElements(:) ! (ne) fulldomain -> subdomain index mapping
integer :: nps ! number of nodes in the resultshape
integer :: nes ! number of elements in the resultshape 
real(8), allocatable :: polygonx(:) ! x coordinates of polygon vertices
real(8), allocatable :: polygony(:) ! y coordinates of polygon vertices
integer :: numVertices ! number of vertices in the polygon file
real(8) :: circleCenterLatitude
real(8) :: circleCenterLongitude
real(8) :: circleDiameterDegrees
real(8) :: upperRightLatitude
real(8) :: upperRightLongitude
real(8) :: lowerLeftLatitude
real(8) :: lowerLeftLongitude
real(8) :: temp1, temp2
character(1) :: junkc
integer :: lineNum, vertex
integer :: pvUnit 
integer :: errorIO
!
fm%meshFileName = "null"
rm%meshFileName = "null"
meshonly = .false.
dataFileBase = "null"
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
         fm%meshFileName = trim(cmdlinearg)
      case('--datafile')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         fd%dataFileName = trim(cmdlinearg)
      case('--datafiletype')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         fd%dataFileType = trim(cmdlinearg)
      case('--submeshfilename')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         rm%meshFileName = trim(cmdlinearg)
      case('--meshonly')
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),'.'
         meshonly = .true.
      case('--datafileformat')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         select case(trim(downcase(cmdlinearg)))
         case("netcdf")
            fd%fileFormat = NETCDFG
         case("adcirc","ascii","text")
            fd%fileFormat = ASCII         
         case default
            call allMessage(WARNING,'Command line option "'//trim(cmdlineopt)//'" was not recognized.')
         end select
      case('--subresultfileformat')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         select case(trim(downcase(cmdlinearg)))
         case("netcdf")
            rd%fileFormat = NETCDFG
         case("adcirc","ascii","text")
            rd%fileFormat = ASCII         
         case default
            call allMessage(WARNING,'Command line option "'//trim(cmdlineopt)//'" was not recognized.')
         end select
      case('--resultshape')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         resultShape = trim(cmdlinearg)
         select case(trim(resultshape))
         case('circle')
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) circleCenterLongitude                    
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) circleCenterLatitude
            write(6,'("INFO: Result circle is centered at ",f15.7," degrees west longitude ",f15.7," degrees north latitude.")') circleCenterLongitude, circleCenterLatitude           
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) circleDiameterDegrees
            write(6,'("INFO: Result circle diameter is ",f15.7," degrees.")') circleDiameterDegrees
            call getarg(i, cmdlinearg)
         case('rectangle')
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) lowerLeftLongitude                    
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) lowerLeftLatitude
            write(6,'("INFO: Result rectangle lower left coordinates are ",f15.7," degrees west longitude ",f15.7," degrees north longitude.")') lowerLeftLongitude, lowerLeftLatitude
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) upperRightLongitude                    
            i = i + 1
            call getarg(i, cmdlinearg)
            read(cmdlinearg,*) upperRightLatitude
            write(6,'("INFO: Result rectangle upper right coordinates are ",f15.7," degrees west longitude ",f15.7," degrees north latitude.")') upperRightLongitude, upperRightLatitude
         case('polygon')
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
            polygonFile = trim(cmdlinearg)
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
lastSlashPosition = index(trim(fd%dataFileName),"/",.true.) 
if (meshonly.eqv..true.) then
   ! trim off the full path so we just have the file name
   lastSlashPosition = index(trim(fm%meshFileName),"/",.true.)
endif
dataFileBase = trim(fd%dataFileName(lastSlashPosition+1:))
lastDotPosition = index(trim(dataFileBase),'.',.true.)
dataFileExtension = trim(dataFileBase(lastDotPosition+1:))
!
! get mesh file name without full path (if any)
lastSlashPositionMesh = index(trim(fm%meshFileName),"/",.true.)
meshFileBase = trim(fm%meshFileName(lastSlashPositionMesh+1:))
!
! If the data file type was not supplied, and the file is ascii, 
! then use the file name as the file type.
if ( (fd%fileFormat.eq.ASCII).and.(trim(fd%dataFileType).eq.'null') ) then
   fd%dataFileType = trim(fd%dataFileName)
endif
!
! open and read mesh in appropriate format
select case(fd%fileFormat)
case(ASCII)
   call read14(fm)
case(NETCDFG)
   call findMeshDimsNetCDF(fm, fn)
   call readMeshNetCDF(fm, fn)
   call determineNetCDFFileCharacteristics(fd, fm, fn)
case default
   call allMessage(ERROR,'Cannot read mesh.')
end select
!   
allocate(within(fm%np))
allocate(elementWithin(fm%ne))
elementWithin(:) = .false.
within(:) = .false.
!
! Characteristics of output data
!
! Determine if the dataFile is time varying or not. 
!write(6,*) 'DEBUG: dataFile: ',trim(dataFile)
select case(trim(fd%dataFileType))
case('maxele.63','maxvel.63','maxwvel.63','maxrs.63','minpr.63','swan_HS_max.63','swan_TPS_max.63')
   fd%timeVarying = .false.
case default
   fd%timeVarying = .true.
end select
!
! Determine datatype
select case(trim(fd%dataFileType))
case('nodecode.63','noff.100')
   fd%netCDFDataType = NF90_INT
   fd%isInteger = .true.
case default
   fd%netCDFDataType = NF90_DOUBLE
   fd%isInteger = .false.
end select
!
! Select nodes inside the resultShape
select case(trim(resultShape))
case('circle')
   do n=1, fm%np 
      if ( sqrt((circleCenterLongitude-fm%xyd(1,n))**2 + (circleCenterLatitude-fm%xyd(2,n))**2).lt.(0.5*circleDiameterDegrees) ) then
         within(n) = .true.
      end if
   end do
case('rectangle')
   do n=1, fm%np
      if ( fm%xyd(2,n).le.upperRightLatitude ) then
         if ( fm%xyd(2,n).ge.lowerLeftLatitude ) then
            if ( fm%xyd(1,n).le.upperRightLongitude ) then      
               if ( fm%xyd(1,n).ge.lowerLeftLongitude ) then      
                  within(n) = .true.
               endif
            endif
         endif
      endif
   end do
case('polygon')
   pvUnit = availableUnitNumber()
   call openFileForRead(pvUnit,polygonFile,errorIO)
   if (errorIO.ne.0) then
      stop
   endif
   ! Count the number of vertices in the polygon file
   numVertices = 0
   lineNum = 1
   ss = 1
   do  ! loop until we run out of data
      read(pvUnit,fmt=*,end=500,err=248) temp1, temp2
      numVertices = numVertices + 1
      lineNum = lineNum + 1
   end do   
500 rewind(pvUnit)
   lineNum = 1
   allocate(polygonx(numVertices),polygony(numVertices))
   do vertex=1,numVertices ! loop until we run out of data
      read(pvUnit,fmt=*,end=246,err=248) polygonx(vertex), polygony(vertex)
      lineNum = lineNum + 1
   end do
   close(pvUnit)
   ! determine whether each mesh node is inside, on, or outside the polygon
   do n=1, fm%np
      call pnpoly(fm%xyd(1,n),fm%xyd(2,n),polygonx,polygony,numVertices,inOnOut)
      if (inOnOut.ge.0) then
         within(n) = .true.
      endif
   end do
case default
   write(6,'(a,a,a)') 'ERROR: The result shape "',trim(resultShape),'" was not recognized.'
   stop
end select
!
! Select elements inside the resultShape; first we need to count them
! so we can allocate an array of the proper size
rm%ne = 0                 ! counter for elements that are included in the resultShape
do e = 1, fm%ne
   if (any(within(fm%nm(e,:))).eqv..true.) then
      rm%ne = rm%ne + 1     ! increment the number of elements included in the resultShape
      elementWithin(e) = .true.
   endif 
enddo
! now allocate an array to hold the element numbers of the selected elements 
allocate(sub2fullElements(rm%ne))
allocate(full2subElements(fm%ne))
full2subElements(:) = 0
rm%ne = 0                 
do e = 1, fm%ne
   if (any(within(fm%nm(e,:)))) then
      rm%ne = rm%ne + 1     ! increment the number of elements included in the resultShape
      sub2fullElements(rm%ne) = e  ! record the element number mapping
      full2subElements(e) = rm%ne
   endif 
enddo
!
! For each selected element, make sure that all 3 nodes on that element
! have been selected, even if they did not originally fall in the shape specified
! by the analyst
do e = 1, rm%ne
   do i = 1, 3
      within(fm%nm(sub2fullElements(e),i)) = .true.
   enddo
enddo
!
! Count and record nodes on elements that have been selected into the
! resultShape
allocate(full2subNodes(fm%np))
full2subNodes(:) = 0
rm%np = 0  ! counter for nodes on elements that are included in the resultShape
do n = 1, fm%np
   if ( within(n).eqv..true. ) then
      rm%np = rm%np + 1   ! increment total number of nodes selected
      full2subNodes(n) = rm%np  ! record the index of node numbers that have been selected 
   endif
enddo
allocate(sub2fullNodes(rm%np))
!
! record the fulldomain node number of the sequential nodes in the selection
rm%np = 0
do n=1,fm%np
   if (within(n).eqv..true.) then
      rm%np = rm%np + 1
      sub2fullNodes(rm%np) = n ! record the node number of the selected node
   end if
enddo
!
! Output sub-mesh
if (trim(rm%meshFileName).eq."null") then
   rm%meshFileName = trim(meshFileBase) // '_' // trim(resultShape) // '-sub.14'
endif
call writeMesh(rm)
!
if (meshonly.eqv..true.) then
   write(6,'("INFO: The --meshonly command line option was specified; the subdomain mesh has been written and execution is complete.")')
   stop
endif
!
! set data centeredness for ascii fulldomain data files
if (fd%fileFormat.eq.ASCII) then
   allocate(fd%dataCenter(1)) 
   select case(trim(fd%dataFileType))
   case('noff.100')
      fd%dataCenter(1) = 'Cell'
      rd%numValuesPerDataset = rm%ne
   case default
      fd%dataCenter(1) = 'Node'
      rd%numValuesPerDataset = rm%np
   end select
endif
if (rd%fileFormat.eq.ASCII) then
   allocate(rd%dataCenter(1))
endif
rd%numValuesPerDataset = rm%np
if (trim(rd%dataCenter(1)).eq.'Cell') then
   rd%numValuesPerDataset = rm%ne
endif
fd%fun = availableUnitNumber()
call openFileForRead(fd%fun, trim(fd%dataFileName),errorIO)
read(fd%fun,'(A)',end=246,err=248,iostat=errorio) dataFileCommentLine
lineNum = lineNum + 1
!
! jgf: Can't rely on the NumSnaps value; in general, it will not
! actually reflect the number of datasets in the file.
!
! Examine the fulldomain output file to determine its properties
! FIXME: provide support for netcdf formatted full domain files
read(fd%fun,*,end=246,err=248,iostat=errorio) fd%nSnaps, fd%numValuesPerDataset, fd%time_increment, fd%nspool, fd%num_components
lineNum=lineNum+1
if ( (fm%np.ne.fd%numValuesPerDataset).and.(trim(fd%dataCenter(1)).eq.'Node') ) then
   write(6,'(a,i0,a,i0,a)') 'ERROR: The output file contains ',fd%numValuesPerDataset,        &
     ' nodes, but the mesh file contains ',fm%np,' nodes.'
    write(6,'(a)') 'ERROR: The output file does not correspond to the mesh file.'
   close(fd%fun)
   stop
endif
if ( (fm%ne.ne.fd%numValuesPerDataset).and.(trim(fd%dataCenter(1)).eq.'Cell') ) then
   write(6,'(a,i0,a,i0,a)') 'ERROR: The output file contains ',fd%numValuesPerDataset,        &
     ' elements, but the mesh file contains ',fm%ne,' elements.'
   write(6,'(a)') 'ERROR: The output file does not correspond to the mesh file.'
   close(fd%fun)
   stop
endif
!
! Allocate arrays to hold the data from the fulldomain file
select case(fd%netCDFDataType)
case(NF90_DOUBLE)
   allocate(adcirc_data(fd%numValuesPerDataSet,fd%num_components))
case(NF90_INT)
   allocate(adcirc_idata(fd%numValuesPerDataSet,fd%num_components))      
case default
   write(6,'(a)') 'ERROR: Unsupported data type.'
end select
!
! Set the number of components
select case(fd%num_components)
case(1)
   fd%dataRank = 'Scalar'
case(2)           
   fd%dataRank = '2DVector'
case default 
   write(6,'("ERROR: resultScope.f90: ADCIRC output files with ",i0," columns are not supported.")') fd%num_components
   stop
end select
!
! open the subdomain ascii adcirc file that will hold the data
rd%dataFileName = 'sub-' // trim(resultShape) // '_' // trim(fd%dataFileName) 
rd%fun = availableUnitNumber()
open(rd%fun,file=trim(rd%dataFileName),status='replace',action='write')
! write header info
write(rd%fun,'(a)') trim(rm%agrid) // trim(rd%dataFileName) // ' ' // trim(dataFileCommentLine)
! write the header data to the resultshape file
write(rd%fun,1010) fd%nSnaps, rd%numValuesPerDataSet, fd%time_increment, fd%nspool, fd%num_components
!
!  R E A D   I N   F U L L D O M A I N   D A T A   
!      A N D   W R I T E   O U T   R E S U L T  S H A P E   D A T A
!           
SS=1 ! jgf: initialize the dataset counter
lineNum = 1 ! initialize the line number counter
DO   ! jgf: loop until we run out of data 
   read(fd%fun,*,end=244,err=248,iostat=errorio) SnapR, SnapI
   j=0
   do n=1,fd%numValuesPerDataSet
      select case(trim(fd%dataRank))
      case("Scalar")                    ! scalar data
         if (fd%netCDFDataType.eq.NF90_DOUBLE) then
            read(fd%fun,*,end=246,err=248,iostat=errorio) j,Temp1
            lineNum = lineNum + 1
            adcirc_data(j,1) = Temp1
         else
            read(fd%fun,*,end=246,err=248,iostat=errorio) j,adcirc_idata(n,fd%num_components)
            lineNum = lineNum + 1
         endif
      case("2DVector")                  ! 2D vector data
         read(fd%fun,*,end=246,err=248,iostat=errorio) j,Temp1,Temp2
         lineNum = lineNum + 1
         adcirc_data(j,1) = Temp1
         adcirc_data(j,2) = Temp2
      case default
         write(6,'(a,a,a)') 'ERROR: resultScope.f90: ',trim(fd%dataRank),' data rank is not supported.'
         stop
      end select
   enddo 
   !
   ! nonsparse ascii output to resultshape output file
   write(rd%fun,2120) SnapR, SnapI
   do k=1,rd%numValuesPerDataSet
      if ( (trim(rd%dataCenter(1)).eq.'Node').and.(within(k).eqv..true.) ) then
         if (fd%isInteger.eqv..true.) then
            write(11,2452) full2subNodes(k), (adcirc_idata(k,j),j=1,fd%num_components)
         else
            write(11,2453) full2subNodes(k), (adcirc_data(k,j),j=1,fd%num_components)
         endif
      end if
      if ( (trim(rd%dataCenter(1)).eq.'Cell').and.(elementWithin(k).eqv..true.) ) then
         if (rd%isInteger.eqv..true.) then
            write(11,2452) full2subElements(k), (adcirc_idata(k,j),j=1,fd%num_components)
         else
            write(11,2453) full2subElements(k), (adcirc_data(k,j),j=1,fd%num_components)
         endif
      end if
   end do
   write(6,advance='no',fmt='(i4)') SS
   SS = SS + 1
end do
244 close(rd%fun)
close(fd%fun)

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

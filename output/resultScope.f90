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
use nodalattr
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
type(nodalAttrFile_t) :: fna ! full size nodal attributes file
type(nodalAttrFile_t) :: rna ! resultShape nodal attributes file
!
type(netCDFMetaDataFromExternalFile_t) :: a ! attribute data file
!
type(realVector1D_t) :: timesec
type(integerVector1D_t) :: it
!

integer :: snapi
real(8) :: snapr
integer :: nc_count(2)
integer :: nc_start(2)
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
integer :: i, j, k, c, e, n, SS
logical :: meshonly    ! .true. if we are just subsetting the mesh
logical, allocatable :: within(:) ! (np) .true. if a node is within the resultshape

integer inOnOut ! 1 if a node is within the polygon, 0 if it is on the polygon, and -1 if it is outside
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
logical :: wetonly ! .true. if the result mesh(es) should consist of wet nodes only
integer, allocatable :: w(:) ! 1 if node is wet, 0 if node is dry
integer :: wUnit ! to read wet dry file
real(8) :: temp1, temp2

character(1) :: junkc
integer :: lineNum, vertex
integer :: pvUnit
integer :: errorIO
integer :: h ! horizontal node counter
!
fm%meshFileName = "null"
rm%meshFileName = "null"
fd%dataFileFormat = ASCII
rd%dataFileFormat = ASCII
meshonly = .false.
wetonly = .false.
dataFileBase = "null"
fd%defaultFileName = "null"
!
call initLogging(availableUnitNumber(),'resultScope.f90')
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
         fd%defaultFileName = trim(cmdlinearg)
      case('--resultmeshfilename')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         rm%meshFileName = trim(cmdlinearg)
      case('--meshonly')
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),'.'
         meshonly = .true.
      case('--attfile')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         a%nmattFileName = trim(cmdlinearg)
      case('--datafileformat')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         call downcase(cmdlinearg)
         select case(trim(adjustl(cmdlinearg)))
         case("netcdf")
            fd%dataFileFormat = NETCDFG
         case("adcirc","ascii","text")
            fd%dataFileFormat = ASCII
         case default
            call allMessage(ERROR,'Command line argument "'//trim(adjustl(cmdlinearg))//'" was not recognized.')
            stop
         end select
      case('--resultfileformat')
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(a,a,a,a,a)') 'INFO: Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         call downcase(cmdlinearg)
         select case(trim(cmdlinearg))
         case("netcdf")
            rd%dataFileFormat = NETCDFG
         case("adcirc","ascii","text")
            rd%dataFileFormat = ASCII
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
            write(6,'("INFO: Result circle is centered at ",f15.7," degrees longitude ",f15.7," degrees north latitude.")') circleCenterLongitude, circleCenterLatitude
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
         case('wetonly')
            wetonly = .true.
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
if ( (fd%dataFileFormat.eq.ASCII).and.(trim(fd%defaultFileName).eq.'null') ) then
   fd%defaultFileName = trim(fd%dataFileName)
endif
!
! open and read mesh in appropriate format
select case(fd%dataFileFormat)
case(ASCII,ASCIIG)
   call read14(fm)
   if (meshonly.eqv..false.) then
      call determineASCIIFileCharacteristics(fd)
   endif
case(NETCDFG)
   fm%meshFileName = fd%dataFileName
   call findMeshDimsNetCDF(fm, fn)
   call readMeshNetCDF(fm, fn)
   if (meshonly.eqv..false.) then
      call determineNetCDFFileCharacteristics(fd, fm, fn)
   endif
case default
   call allMessage(ERROR,'Cannot read mesh.')
   stop
end select
call allMessage(INFO,'Finished reading mesh.')
!
allocate(within(fm%np))
within(:) = .false.
!
! Select nodes inside the resultShape
select case(trim(resultShape))
case('circle')
   do n=1, fm%np
      if ( sqrt((circleCenterLongitude-fm%xyd(1,n))**2 + (circleCenterLatitude-fm%xyd(2,n))**2).lt.(0.5*circleDiameterDegrees) ) then
         within(n) = .true.
      end if
   end do
   write(scratchMessage,'(a,i0,a)' ) 'There are ',count(within),' vertices in the circle.'
   call allMessage(INFO,trim(scratchMessage))
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
   write(scratchMessage,'(a,i0,a)' ) 'There are ',count(within),' vertices in the rectangle.'
   call allMessage(INFO,trim(scratchMessage))
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
   write(scratchMessage,'(a,i0,a)' ) 'There are ',count(within),' vertices in the polygon.'
   call allMessage(INFO,trim(scratchMessage))
case('wetonly')
   call allMessage(INFO,'The wet vertices will be determined by reading data on STDIN.')
   allocate(w(fm%np))
   do n=1, fm%np
      read(*,*) w(n)
   end do
   do n=1, fm%np
      if (w(n).eq.1) then
         within(n) = .true.
      endif
   end do
   deallocate(w)
   write(scratchMessage,'(a,i0,a)' ) 'There are ',count(within),' wet vertices in the result.'
   call allMessage(INFO,trim(scratchMessage))
case default
   write(6,'(a,a,a)') 'ERROR: The result shape "',trim(resultShape),'" was not recognized.'
   stop
end select
!
!   S U B S E T   T H E   M E S H
!
call allMessage(INFO,'Selecting nodes and elements in the result mesh.')
call subSetMesh(fm, rm, within, wetonly)
call allMessage(INFO,'Finished selecting nodes and elements in the result mesh.')
!
! Output sub-mesh as fort.14 if the output format is ascii
call get_command(cmdlinearg)
rm%agrid = trim(fm%agrid) // ' ! command line used to make this result mesh: ' // trim(cmdlinearg)
if (rd%dataFileFormat.eq.ASCII) then
   if (trim(rm%meshFileName).eq."null") then
      rm%meshFileName = trim(meshFileBase) // '_' // trim(resultShape) // '-sub.14'
   endif
   call writeMesh(rm)
endif
!
if (meshonly.eqv..true.) then
   write(6,'("INFO: The --meshonly command line option was specified; the subdomain mesh has been written and execution is complete.")')
   stop
endif
!
! if the data file is a nodal attributes file, process that separately
if (trim(fd%defaultFileName).eq.'fort.13') then
   fna%nodalAttributesFileName = fd%dataFileName
   call readNodalAttributesFile(fna)
   ! populate result result shape fort.13
   call allMessage(INFO,'Subsetting nodal attributes for subdomain.')
   rna%nodalAttributesFileName = 'subdomain_fort.13'
   rna%nodalAttributesComment = trim(fna%nodalAttributesComment) // ' ! command line used to make this subdomain nodal attributes file: ' // trim(cmdlinearg)
   rna%numMeshNodes = rm%np
   rna%numNodalAttributes = fna%numNodalAttributes
   allocate(rna%na(rna%numNodalAttributes))
   do i=1,rna%numNodalAttributes
      rna%na(i)%attrName = fna%na(i)%attrName
      rna%na(i)%units = fna%na(i)%units
      rna%na(i)%numVals = fna%na(i)%numVals
      rna%na(i)%numNodesNotDefault = 0  ! need to count these
      allocate(rna%na(i)%fillValue(rna%na(i)%numVals))
      rna%na(i)%fillValue(:) = -99999.d0
      allocate(rna%na(i)%defaultVals(rna%na(i)%numVals))
      rna%na(i)%defaultVals = fna%na(i)%defaultVals
      ! count non default values in the subdomain mesh
      do j=1,fna%na(i)%numNodesNotDefault
         h = full2subNodes(fna%na(i)%nonDefaultNodes(j))
         if (h.ne.-99999 ) then
            rna%na(i)%numNodesNotDefault = rna%na(i)%numNodesNotDefault + 1
         endif
      end do
      ! now allocate memory for these and populate the nondefault array
      allocate(rna%na(i)%nonDefaultNodes(rna%na(i)%numNodesNotDefault))
      allocate(rna%na(i)%nonDefaultVals(rna%na(i)%numVals,rna%na(i)%numNodesNotDefault))
      ! now populate the non default nodes and their values in the subdomain
      k=1
      do j=1,fna%na(i)%numNodesNotDefault
         h = full2subNodes(fna%na(i)%nonDefaultNodes(j))
         if (h.ne.-99999) then
            rna%na(i)%nonDefaultNodes(k) = h
            do n=1,rna%na(i)%numVals
               rna%na(i)%nonDefaultVals(n,k) = fna%na(i)%nonDefaultVals(n,j)
            end do
            k=k+1
         endif
      end do
   end do
   call writeNodalAttributesFile(rna)
   stop
else
   if ( trim(adjustl(fd%defaultFileName)).ne.'null' ) then
      call allMessage(INFO,'Not a nodal attributes file: '//trim(adjustl(fd%defaultFileName)))
   endif
endif
!
!  C R E A T E   R E S U L T   F I L E
!
call createResultFile(fm, fn, fd, rm, rn, rd, a, resultShape)
!
! ALLOCATE ARRAYS FOR TRANSFERRING DATA
!
! Allocate arrays to hold the data from the full domain file and the
! result shape data file
call allocateDataSetMemory(fd, fm)
call allocateDataSetMemory(rd, rm)
call initR1D(timesec)
call initI1D(it)
!
! set pointers to relevant mappings
select case(rd%dataFileFormat)
case(NETCDFG)
   do i=1,rd%numVarNetCDF
      ! establish mapping from subdomain to fulldomain
      if (rd%ncds(i)%isElemental.eqv..true.) then
         rd%ncds(i)%mapping => sub2fullElements
      else
         rd%ncds(i)%mapping => sub2fullNodes
      endif
   end do
case(ASCII)
   ! establish mapping from subdomain to fulldomain
   if (rd%isElemental.eqv..true.) then
      rd%mapping => sub2fullElements
   else
      rd%mapping => sub2fullNodes
   endif
case default
   ! should be unreacable
   call allMessage(ERROR,'Only NETCDF and ASCII full domain file formats are supported.')
end select
!
! open the fulldomain ascii data file for reading
if ( fd%dataFileFormat.eq.ASCII ) then
   lineNum = 1
   fd%fun = availableUnitNumber()
   call openFileForRead(fd%fun,fd%dataFileName,errorIO)
   read(fd%fun,*,end=246,err=248,iostat=errorio) junkc  ! comment line
   lineNum=lineNum+1
   read(fd%fun,*,end=246,err=248,iostat=errorio) junkc  ! nSnaps etc
   lineNum=lineNum+1
endif
!
!
!  R E A D   I N   F U L L D O M A I N   D A T A
!      A N D   W R I T E   O U T   R E S U L T  S H A P E   D A T A
!
SS=1 ! jgf: initialize the dataset counter
lineNum = 1 ! initialize the line number counter
call allMessage(INFO,'Begin reading full domain data and writing result data.')
DO   ! jgf: loop until we run out of data (can't trust the nSnaps at top of ascii file)
   !
   !  R E A D   I N   O N E   D A T A S E T
   !

!subroutine readOneDataSet(f, m, s, l, snapr, snapi)

   ! jgfdebug
   !write(*,*) 'call readOneDataset(fd, fm, ss, lineNum, snapr, snapi)'

   call readOneDataset(fd, fm, ss, lineNum, snapr, snapi)
   !write(*,*) 'call readOneDataset(fd, fm, ss, lineNum, snapr, snapi)'
   call appendR1D(timesec, snapr)
   call appendI1D(it, snapi)
   !call allMessage(INFO,"read one data file") !jgfdebug
   !
   !  P O P U L A T E   R E S U L T   A R R A Y ( S )
   !        F O R   T H I S   S I N G L E   D A T A S E T
   !
   ! ASCII FULLDOMAIN FILE and ASCII SUBDOMAIN FILE
   if ( (fd%dataFileFormat.eq.ASCII).and.(rd%dataFileFormat.eq.ASCII) ) then
      ! map 3D real data  ascii->ascii
      if ( (fm%is3D.eqv..true.).and.(fd%is3D.eqv..true.) ) then
         do h=1,rd%numValuesPerDataSet
            rd%rdata3D(:,h,:) = fd%rdata3D(:,rd%mapping(h),:)
         end do
      else
         ! map 2DDI integer data   ascii->ascii
         if (fd%isInteger.eqv..true.) then
            do h=1,rd%numValuesPerDataSet
               rd%idata(:,h) = fd%idata(:,rd%mapping(h))
            end do
         else
         ! map 2DDI real data   ascii->ascii
            do h=1,rd%numValuesPerDataSet
               rd%rdata(:,h) = fd%rdata(:,rd%mapping(h))
            end do
         endif
      endif
   endif
   ! NETCDF FULLDOMAIN FILE and NETCDF SUBDOMAIN FILE
   if ( (fd%dataFileFormat.eq.NETCDFG).and.(rd%dataFileFormat.eq.NETCDFG) ) then
      do i=1,fd%numVarNetCDF
         ! map 3D real data  netcdf->netcdf
         if ( (fm%is3D.eqv..true.).and.(fd%ncds(i)%is3D.eqv..true.) ) then
            do h=1,rd%ncds(i)%numValuesPerDataSet
               rd%ncds(i)%rdata3D(h,:) = fd%ncds(i)%rdata3D(rd%ncds(i)%mapping(h),:)
            end do
         else
            ! map 2DDI integer data   netcdf->netcdf
            if (fd%ncds(i)%isInteger.eqv..true.) then
               do h=1,rd%ncds(i)%numValuesPerDataSet
                  rd%ncds(i)%idata(h) = fd%ncds(i)%idata(rd%ncds(i)%mapping(h))
               end do
            else
            ! map 2DDI real data   netcdf->netcdf
               do h=1,rd%ncds(i)%numValuesPerDataSet
                  rd%ncds(i)%rdata(h) = fd%ncds(i)%rdata(rd%ncds(i)%mapping(h))
               end do
            endif
         endif
      end do
      rd%timesec(ss) = fd%timesec(ss)
   endif
   ! ASCII FULLDOMAIN FILE and NETCDF SUBDOMAIN FILE
   if ( (fd%dataFileFormat.eq.ASCIIG).and.(rd%dataFileFormat.eq.NETCDFG) ) then
      ! map 3D real data  ascii->netcdf
      if ( (fm%is3D.eqv..true.).and.(fd%is3D.eqv..true.) ) then
         do i=1,fd%irtype
            do h=1,rd%ncds(i)%numValuesPerDataSet
               rd%rdata3D(i,h,:) = fd%ncds(i)%rdata3D(rd%mapping(h),:)
            end do
         end do
      else
         ! map 2DDI integer data   ascii->netcdf
         if (fd%isInteger.eqv..true.) then
            do i=1,fd%irtype
               do h=1,rd%ncds(i)%numValuesPerDataSet
                  rd%idata(i,h) = fd%ncds(i)%idata(rd%ncds(i)%mapping(h))
               end do
            end do
         else
         ! map 2DDI real data   ascii->netcdf
            do i=1,fd%irtype
               do h=1,rd%ncds(i)%numValuesPerDataSet
                  rd%ncds(i)%rdata(h) = fd%rdata(i,rd%ncds(i)%mapping(h))
               end do
            end do
         endif
      endif
   endif
   ! NETCDF FULLDOMAIN FILE and ASCII SUBDOMAIN FILE
   if ( (fd%dataFileFormat.eq.NETCDFG).and.(rd%dataFileFormat.eq.ASCIIG) ) then
      snapr = fd%timesec(ss)
      snapi = fd%it(ss)
      do i=1,fd%numVarNetCDF
         ! map 3D real data  netcdf->ascii
         if ( (fm%is3D.eqv..true.).and.(fd%ncds(i)%is3D.eqv..true.) ) then
            do h=1,rd%numValuesPerDataSet
               rd%rdata3D(i,h,:) = fd%ncds(i)%rdata3D(rd%mapping(h),:)
            end do
         else
            ! map 2DDI integer data   netcdf->ascii
            if (fd%ncds(i)%isInteger.eqv..true.) then
               do h=1,rd%numValuesPerDataSet
                  rd%idata(i,h) = fd%ncds(i)%idata(rd%mapping(h))
               end do
            else
            ! map 2DDI real data   netcdf->ascii
               do h=1,rd%numValuesPerDataSet
                  rd%rdata(i,h) = fd%ncds(i)%rdata(rd%mapping(h))
               end do
            endif
         endif
      end do
   endif
   !
   !   W R I T E   O U T   O N E   R E S U L T   D A T A S E T
   !
   !call readOneDataset(fd, fm, ss, lineNum, snapr, snapi)

            !jgfdebug
            !write(*,*) rd%numValuesPerDataset

   call writeOneDataSet(rd, rm, ss, lineNum, snapr, snapi)
   !
   write(6,advance='no',fmt='(i6)') SS
   SS = SS + 1
end do
244 continue
!
!   F I N I S H E D   - -   C L O S E   F I L E S
!
call closeFile(rd)
call closeFile(fd)
!
write(6,'(a)') 'INFO: resultScope.f90: Finished writing file.'
write(6,'(a,i0,a)') 'INFO: resultScope.f90: Wrote ',SS-1,' data sets.'

stop

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
!   S U B R O U T I N E    S U B  S E T  M E S H
!----------------------------------------------------------------------
subroutine subSetMesh(fm, rm, within, wetonly)
use adcmesh
use ioutil
use asgsio
use logging
implicit none
type(mesh_t), intent(in)  :: fm ! full domain mesh
type(mesh_t), intent(out) :: rm ! resultShape mesh
logical, intent(inout) :: within(*)
logical, intent(in) :: wetonly
!
logical, allocatable :: elementWithin(:) ! (ne) .true. if an element is within the resultshape
integer :: e ! element counter
integer :: i, j, n
!
allocate(elementWithin(fm%ne))
elementWithin(:) = .false.
!
! Select elements inside the resultShape; first we need to count them
! so we can allocate an array of the proper size
rm%ne = 0                 ! counter for elements that are included in the resultShape
if ( wetonly.eqv..true.) then
   do e = 1, fm%ne
      if (all(within(fm%nm(e,:))).eqv..true.) then
         rm%ne = rm%ne + 1     ! increment the number of elements included in the resultShape
         elementWithin(e) = .true.
      endif
   enddo
   ! remake the "within" list so that nodes that aren't in any
   ! element are removed
   within(1:fm%np) = .false.
   do e = 1, rm%ne
      if (elementWithin(e).eqv..true.) then
         within(fm%nm(e,:)) = .true.
      endif
   enddo
else
   do e = 1, fm%ne
      if (any(within(fm%nm(e,:))).eqv..true.) then
         rm%ne = rm%ne + 1     ! increment the number of elements included in the resultShape
         elementWithin(e) = .true.
      endif
   enddo
endif
! now allocate an array to hold the element numbers of the selected elements
allocate(sub2fullElements(rm%ne))
allocate(full2subElements(fm%ne))
full2subElements(:) = 0
rm%ne = 0
if ( wetonly.eqv..true.) then
   do e = 1, fm%ne
      if (all(within(fm%nm(e,:)))) then
         rm%ne = rm%ne + 1     ! increment the number of elements included in the resultShape
         sub2fullElements(rm%ne) = e  ! record the element number mapping
         full2subElements(e) = rm%ne
      endif
   enddo

else
   do e = 1, fm%ne
      if (any(within(fm%nm(e,:)))) then
         rm%ne = rm%ne + 1     ! increment the number of elements included in the resultShape
         sub2fullElements(rm%ne) = e  ! record the element number mapping
         full2subElements(e) = rm%ne
      endif
   enddo
   ! For each selected element, make sure that all 3 nodes on that element
   ! have been selected, even if they did not originally fall in the shape specified
   ! by the analyst
   do e = 1, rm%ne
      do i = 1, 3
         within(fm%nm(sub2fullElements(e),i)) = .true.
      enddo
   enddo
endif
!
! Count and record nodes on elements that have been selected into the
! resultShape
allocate(full2subNodes(fm%np))
full2subNodes(:) = -99999
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

allocate(rm%xyd(3,rm%np))
! node table
do i=1,rm%np
   do j=1,3
      rm%xyd(j,i) = fm%xyd(j,sub2fullNodes(i))
   end do
end do
! element table
allocate(rm%nm(rm%ne,3))
do i=1,rm%ne
   do j=1,3
      rm%nm(i,j) = full2subNodes(fm%nm(sub2fullElements(i),j))
   end do
end do
!----------------------------------------------------------------------
end subroutine subSetMesh
!----------------------------------------------------------------------


!----------------------------------------------------------------------
!  S U B R O U T I N E   C R E A T E   R E S U L T   F I L E
!----------------------------------------------------------------------
subroutine createResultFile(fm, fn, fd, rm, rn, rd, a, resultShape)
use adcmesh
use ioutil
use asgsio
use logging
implicit none
type(mesh_t), intent(in) :: fm          ! fulldomain mesh
type(meshNetCDF_t), intent(in) :: fn    ! fulldomain mesh netcdf IDs
type(fileMetaData_t), intent(inout) :: fd  ! fulldomain data file
type(mesh_t), intent(inout) :: rm          ! resultShape mesh
type(meshNetCDF_t), intent(out) :: rn   ! resultShape mesh netcdf IDs
type(fileMetaData_t), intent(out) :: rd ! resultShape data file
type(netCDFMetaDataFromExternalFile_t), intent(in) :: a ! attribute data file
character(len=1000), intent(in) :: resultShape
!
logical :: deflate
integer :: attI  ! integer metadata attribute value
real(8) :: attR   ! real number metadata attribute value
character(len=NF90_MAX_NAME) :: attC ! character string metadata attribute value
integer :: i, j
!
deflate = .true.
#ifndef NETCDF_CAN_DEFLATE
deflate =.false.
#endif
!
! set up subdomain result file
if (rd%isElemental.eqv..true.) then
   rd%numValuesPerDataset = rm%ne
else
   rd%numValuesPerDataset = rm%np
endif
rd%irtype = fd%irtype
!
select case(rd%dataFileFormat)
!
! result file (subdomain) in ascii format
case(ASCII)
   ! open the subdomain ascii adcirc file that will hold the data and
   ! write the header
   rd%dataFileName = 'sub-' // trim(resultShape) // '_' // trim(fd%dataFileName)
   rd%fun = availableUnitNumber()
   open(rd%fun,file=trim(rd%dataFileName),status='replace',action='write')
   ! write header info
   write(rd%fun,'(a)') trim(rm%agrid) // trim(rd%dataFileName) // ' ' // trim(fd%agridRunIDRunDesLine)
   ! write the header data to the resultshape file
   write(rd%fun,1010) fd%nSnaps, rd%numValuesPerDataSet, fd%time_increment, fd%nspool, fd%irtype
!
! result file in netcdf format
case(NETCDFG)
   ! create netcdf file
   rd%dataFileName = 'sub-' // trim(resultShape) // '_' // trim(fd%dataFileName)
   write(6,'(a,a,a)') "INFO: Creating NetCDF file '"//trim(rd%dataFileName)//"'."
   rd%ncFileType = NF90_CLOBBER ! netcdf3 format, netcdf classic model
#ifdef HAVE_NETCDF4
   rd%ncFileType = ior(NF90_HDF5,NF90_CLASSIC_MODEL) ! netcdf4 (i.e., hdf5) format, netcdf classic model
#endif
   call check(nf90_create(trim(rd%dataFileName), rd%ncFileType, rd%nc_id))
   !
   ! add time variable and associated metadata
   if (fd%timeVarying.eqv..true.) then
      rd%timeVarying = .true.
      call check(nf90_def_dim(rd%nc_id,'time',nf90_unlimited,rd%nc_dimid_time))
      call check(nf90_def_var(rd%nc_id,'time',nf90_double,rd%nc_dimid_time,rd%nc_varid_time))
      call check(nf90_put_att(rd%nc_id,rd%nc_varid_time,'long_name','model time'))
      call check(nf90_put_att(rd%nc_id,rd%nc_varid_time,'standard_name','time'))
      ! time units added from external file or from fulldomain netcdf file below
   endif
   !
   ! add metadata to result (subdomain) file
   select case(fd%dataFileFormat)
   ! ascii full domain file
   case(ASCII)
      ! metadata from external file
      do i = 1,a%nmatt
         call check(nf90_put_att(rd%nc_id,nf90_global,a%matt(1,i),a%matt(2,i)))
      enddo
      call check(nf90_put_att(rd%nc_id,rd%nc_varid_time,'units',a%datenum))
   ! netcdf full domain file
   case(NETCDFG)
      ! read netcdf global metadata from fulldomain file and write to
      ! result file
      call check(nf90_open(trim(fd%dataFileName), NF90_NOWRITE, fd%nc_id))
      do i=1, fd%natt
         select case(fd%nc_attType(i))
         case(NF90_DOUBLE)
            call check(nf90_get_att(fd%nc_id,nf90_global,fd%nc_attName(i),attR))
            ! put key/value pair into subdomain file
            call check(nf90_put_att(rd%nc_id,nf90_global,fd%nc_attName(i),attR))
         case(NF90_INT)
            call check(nf90_get_att(fd%nc_id,nf90_global,fd%nc_attName(i),attI))
            ! put key/value pair into subdomain file
            call check(nf90_put_att(rd%nc_id,nf90_global,fd%nc_attName(i),attI))
         case(NF90_CHAR)
            call check(nf90_get_att(fd%nc_id,nf90_global,fd%nc_attName(i),attC))
            ! put key/value pair into subdomain file
            call check(nf90_put_att(rd%nc_id,nf90_global,fd%nc_attName(i),attC))
         case default
            call allMessage(ERROR,'Cannot store metadata attribute "'//trim(fd%nc_attName(i))//'" because its datatype is unsupported.')
         end select
      end do
      ! add time metadata from full domain file
      call check(nf90_put_att(rd%nc_id,rd%nc_varid_time,'units',fd%datenum))
      ! add mesh definitions
      call writeMeshDefinitionsToNetCDF(rm, rn, rd%nc_id, deflate)
      ! define variables and variable-associated metadata
      rd%dataFileCategory = fd%dataFileCategory
      call addDataAttributesNetCDF(rd, rm, rn)
      ! deflate arrays if applicable
#ifdef NETCDF_CAN_DEFLATE
      if (rd%dataFileFormat.eq.NETCDF4) then
         do j=1,rd%irtype
            call check(nf90_def_var_deflate(rd%nc_id, rd%ncds(j)%nc_varID, 1, 1, 2))
         enddo
      endif
#endif
   case default
      ! should be unreachable
      call allMessage(ERROR,'Only NETCDF and ASCII full domain file formats are supported.')
   end select
   !
   call check(nf90_enddef(rd%nc_id))
case default
   ! should not be reachable
   call allMessage(ERROR,'Cannot create result file.')
end select

1010 format(1x,i10,1x,i10,1x,e15.7e3,1x,i8,1x,i5,1x,'FileFmtVersion: ',i10)
!----------------------------------------------------------------------
end subroutine createResultFile
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

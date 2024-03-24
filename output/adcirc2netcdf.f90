!------------------------------------------------------------------
! adcirc2netcdf: Convert ADCIRC ascii output files to netcdf format.
!------------------------------------------------------------------
! Copyright(C) 2012 Patrick C. Kerr 
! Copyright(C) 2012--2017 Jason Fleming
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
!------------------------------------------------------------------
! Compile with accompanying makefile. 
!------------------------------------------------------------------

!-----+---------+---------+---------+---------+---------+---------+
!
!   P R O G R A M    A D C I R C  2  N E T C D F
!
!-----+---------+---------+---------+---------+---------+---------+
program adcirc2netcdf
use netcdf
use asgsio
use ioutil
use logging
use adcmesh
use nodalattr
implicit none
type(mesh_t) :: m
type(meshNetCDF_t) :: n
type(fileMetaData_t) :: f  ! adcirc file to be read and converted
type(netCDFMetaDataFromExternalFile_t) :: a
type(nodalAttrFile_t) :: naFile
character(2048) :: dataFileBase
character(len=1000) :: Line
character(1) :: JunkC, Tadj
real(8), allocatable :: owi1(:,:)
real(8), allocatable :: owi2(:,:)
integer :: yy, mo, dd, hh, mi
integer :: i, j, k, SS, node
logical :: meshonly   ! .true. if user just wants to convert the mesh
logical :: dataonly   ! .true. if user just wants to convert the data
logical :: noBoundaries   ! .true. if the mesh file does not contain a boundary table
character(len=2048) :: errorVar
integer, dimension(2) :: timeOfNC_Start
integer, parameter :: version = 4
integer :: lastSlashPosition ! used for trimming full path from a filename
integer :: lastDotPosition ! to determine file extension
character(2048) :: dataFileExtension ! something like 13, 14, 15, 63, 222 etc
integer :: lineNum
logical :: deflate ! true if compiled with support for internal file compression in netcdf files
integer :: c ! number of components
integer :: attUnit ! unit number for attributes file
integer :: snapi ! time step number associated with a particular dataset
real(8) :: snapr ! time (s) associated with a particular dataset
integer :: errorIO
integer :: specifiedFileFormat ! format of netcdf file to create
integer :: asciiFileFormat  ! format of source data (ascii or sparse ascii)
character(len=2048) :: adataFileName ! ascii file name, full path
character(len=2048) :: ndataFileName ! netcdf file name, full path
!
! initializations
if (loggingInitialized.eqv..false.) then
   call initLogging(availableUnitNumber(),'adcirc2netcdf.f90')
endif
m%meshFileName = "fort.14"
a%nmattFileName = "null"
f%dataFileFormat = ASCII
f%dataFileName = "null"
f%defaultFileName = "null"
specifiedFileFormat = NETCDF4
!
adataFileName = "null"
ndataFileName = "null"
dataFileBase = "null"
meshonly = .false.
dataonly = .false.
lineNum=1
SS=1
#ifdef NETCDF_CAN_DEFLATE
   deflate = .true.
#else
   deflate = .false.
#endif
!
!write(6,*) "INFO: adcirc2netcdf version ",version,"."
! Report netcdf version
write(6,'(a,a)') "INFO: adcirc2netcdf was compiled with the following netcdf library: ", &
   trim(nf90_inq_libvers())
! jgf: Process command line options; can be used along with menu choices;
! if command line options provide all needed input, menu will not
! be presented to user; programs with command line options are
! slightly easier to automate than menu-based programs
argcount = command_argument_count() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
         case("--netcdf4")
            specifiedFileFormat = NETCDF4
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--netcdf3")
            specifiedFileFormat = NETCDF3
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--meshonly")
            meshonly = .true.
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--dataonly")
            dataonly = .true.
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--no-boundaries")
            noBoundaries = .true.
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--meshfile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            m%meshFileName = trim(cmdlinearg)
         case("--attfile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            a%nmattFileName = trim(cmdlinearg)
         case("--datafile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            adataFileName = trim(cmdlinearg)
         case("--defaultfilename")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            f%defaultFileName = trim(cmdlinearg)
         case default
            write(6,'(a,a,a)') "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
end if
!
! notify mesh file reader that there is no boundary table in the
! mesh file
m%readBoundaryTable = .false.
!
! if there were no data files specified on the command line, just 
! convert the mesh
if ( trim(aDataFileName).eq.'null' ) then
   meshonly = .true.
   aDataFileName = m%meshFileName
endif
!
! trim off the full path so we just have the file name
lastSlashPosition = index(trim(adataFileName),"/",.true.) 
! now set NETCDF file name for files containing only one type of data
if (meshonly.eqv..true.) then
   ! trim off the full path so we just have the file name
   lastSlashPosition = index(trim(m%meshFileName),"/",.true.)
   ndataFileName = trim(m%meshFileName(lastSlashPosition+1:))//'.nc'
   write(6,'(a,a,a)') 'DEBUG: The name of the netCDF file will be ',trim(ndataFileName),'.'
else
   ndataFileName = trim(adataFileName(lastSlashPosition+1:))//'.nc'
endif
dataFileBase = trim(adataFileName(lastSlashPosition+1:))
lastDotPosition = index(trim(dataFileBase),'.',.true.)
dataFileExtension = trim(dataFileBase(lastDotPosition+1:))
!
! If the data file type was not supplied, then use the file name 
! as the default adcirc file name.
if ( trim(f%defaultFileName).eq.'null') then
   f%defaultFileName = dataFileBase
endif
!
! recognize nodal attributes files
if ( trim(dataFileExtension).eq.'13' ) then
   f%defaultFileName = 'fort.13'
endif
!
! set up basic characteristics based on canonical ascii file name
if ( meshonly.eqv..false.) then
   f%dataFileName = adataFileName
   call determineASCIIFileCharacteristics(f)
endif
!
! create netcdf file
write(6,'(a,a,a)') "INFO: Creating NetCDF file '"//trim(ndataFileName)//"'."
f%ncFileType = NF90_CLOBBER ! netcdf3 format, netcdf classic model
f%dataFileFormat = specifiedFileFormat

#ifdef HAVE_NETCDF4
if (specifiedFileFormat.eq.NETCDF4) then
   f%ncFileType = ior(NF90_HDF5,NF90_CLASSIC_MODEL) ! netcdf4 (i.e., hdf5) format, netcdf classic model
endif
#endif

f%dataFileName = ndataFileName
call check(nf90_create(trim(f%dataFileName), f%ncFileType, f%nc_id))
!
! add netcdf metadata from external file (if any) as global attributes
! if no external file was provided, dummy metadata will be added 
call loadNetCDFMetadataFromExternalFile(a)
do i = 1,a%nmatt
   call check(nf90_put_att(f%nc_id,nf90_global,a%matt(1,i),a%matt(2,i)))
enddo
!
! meshed data, the common case
if (f%isGridded.eqv..false.) then
   if (dataonly.eqv..true.) then
      call check(nf90_put_att(f%nc_id,nf90_global,'description',trim(JunkC)))
      call check(nf90_def_dim(f%nc_id,'node',m%np,n%NC_DimID_node))
   else       
      call read14(m)
      call writeMeshDefinitionsToNetCDF(m, n, f%nc_id, deflate)
   endif
endif
!
! if this is a nodal attributes file, then read it and convert it
! using subroutines from the nodal attributes module and then stop
if (f%dataFileCategory.eq.NODALATTRIBF) then
   naFile%nodalAttributesFileName = trim(adataFileName)
   call readNodalAttributesFile(naFile)
   call writeNodalAttributesFileNetCDF(naFile, f%nc_id, m, n, deflate)
   stop
endif
!
! Create time dimension and units attributes
if ((meshonly.eqv..false.).and.(f%timeVarying.eqv..true.)) then      
   call check(nf90_def_dim(f%nc_id,'time',nf90_unlimited,f%nc_dimid_time))
   call check(nf90_def_var(f%nc_id,'time',nf90_double,f%nc_dimid_time,f%nc_varid_time))
   call check(nf90_put_att(f%nc_id,f%nc_varid_time,'long_name','model time'))
   call check(nf90_put_att(f%nc_id,f%nc_varid_time,'standard_name','time'))
   call check(nf90_put_att(f%nc_id,f%nc_varid_time,'units',f%datenum))
endif

! now that the mesh has been read, add associated metadata to the new netcdf file
if (meshonly.eqv..false.) then
   call addDataAttributesNetCDF(f, m, n)
endif
!      
! create adcirc output variables and associated attributes
#ifdef NETCDF_CAN_DEFLATE
   if (meshonly.eqv..false.) then
      if (specifiedFileFormat.eq.NETCDF4) then
         do j=1,f%irtype
            call check(nf90_def_var_deflate(f%nc_id, f%ncds(j)%nc_varid, 1, 1, 2))
         enddo
      endif
   endif
#endif

!----------------------------------------------------------------
! end variable and attributes definitions
!----------------------------------------------------------------
call check(nf90_enddef(f%nc_id))
!----------------------------------------------------------------
!
! place mesh-related data into the file, unless this is a data 
! only file
if ( (dataonly.eqv..false.).and.(f%isGridded.eqv..false.) ) then
   call writeMeshDataToNetCDF(m, n, f%nc_id)
endif
!
! finish up if only mesh data are to be converted
if (meshonly.eqv..true.) then
   call check(nf90_close(f%nc_id))
   write(6,'(a)') 'INFO: Only mesh data were written.'
   stop
endif
!
! read ascii header
f%fun = availableUnitNumber()
call openFileForRead(f%fun, trim(adataFileName), errorIO)
! read and discard two header lines unless this is fort.88 initial 
! river elevation file (which has no header)
if (f%dataFileCategory.ne.INITRIVER) then
   read(f%fun,*) line
   read(f%fun,*) line
endif
!
! Allocate space to hold the data
!
! gridded
if (f%isGridded.eqv..true.) then
   ! y before x according to netcdf specification in fortran api
   allocate(owi1(1:f%iLatOWI,1:f%iLonOWI))
   if (f%irtype.eq.2) then
      ! y before x according to netcdf specification in fortran api
      allocate(owi2(1:f%iLatOWI,1:f%iLonOWI))
   endif
! meshed
else
   f%dataFileFormat = ASCIIG
   call allocateDataSetMemory(f, m)
endif
!
! Read ascii data and write to netcdf file
SS=1        ! initialize the dataset counter
lineNum = 1 ! initialize the line number counter
!
! gridded data
if (f%isGridded.eqv..true.) then
   do   ! loop until we run out of gridded data
      owi1(:,:) = -99999.d0
      owi2(:,:) = -99999.d0
      errorVar = "first component"
      read(f%fun,22,end=321,err=9999,iostat=errorIO) ((owi1(j,i),i=1,f%iLonOWI),j=1,f%iLatOWI)
      call checkErrOWI(errorIO,errorVar,f%defaultFileName)
      if (f%irtype.eq.2) then
         errorVar = "second component"
         read(f%fun,22,end=123,err=9999,iostat=errorIO) ((owi2(j,i),i=1,f%iLonOWI),j=1,f%iLatOWI)
         call checkErrOWI(errorIO,errorVar,f%defaultFileName)
      endif
      call check(nf90_put_var(f%nc_id,f%nc_varid_time,(/snapr/),(/ss/),(/1/)))
      f%NC_Count_OWI = (/ f%iLatOWI, f%iLonOWI, 1 /)
      f%NC_Start_OWI = (/ 1, 1, SS /)
      timeOfNC_Start = (/ 1, 1 /)
      ! write the dataset to the netcdf file
      ! TODO : finish implementing gridded data
22    format(8f10.0)
   end do
9999  call checkErrOWI(1,errorVar,f%defaultFileName) ! ERR during read jumps to here
321 continue  ! jgf: jump here when no data left in gridded ascii file
   write(6,'(/,a,i0,a)') 'INFO: Wrote ',ss-1,' dataset(s).'
   close(f%fun)
   call check(nf90_close(f%nc_id))
   write(6,'(a)') 'INFO: adcirc2netcdf.x: Finished writing gridded data to netcdf.'
   stop !
endif
!
! meshed data
ss=1 ! dataset counter
lineNum=1  ! ascii data file line counter
DO   ! jgf: loop until we run out of mesh data
   f%dataFileFormat = ASCIIG
   call readOneDataSet(f, m, ss, lineNum, snapr, snapi)
   ! break out of the loop if we've run out of ascii data
   if (lineNum.eq.-99) then
      exit
   endif
   f%dataFileFormat = NETCDFG
   call writeOneDataSet(f, m, ss, lineNum, snapr, snapi)
   !
   write(6,advance='no',fmt='(i6)') ss
   ss = ss + 1 ! jgf: Increment the dataset counter
ENDDO

123   CONTINUE  ! jgf: When we've run out of datasets in the current file,
                ! we jump to here.
write(6,'(/,a,i0,a)') 'INFO: Wrote ',ss-1,' dataset(s).'

close(f%fun)
call check(nf90_close(f%nc_id))
write(6,'(a)') 'INFO: adcirc2netcdf finished.'
stop
      ! We jump to this section if there was an error reading a file.
246   write(6,'(a)') 'ERROR: Unexpectedly reached end-of-file.' ! END jumps here
248   write(6,'(a)') 'ERROR: I/O error during file access.'     ! ERR jumps here
write(6,'(a,i0,a,i0,a)') 'INFO: Attempted to read line ',lineNum,' in dataset ',SS,'.' ! ERR jumps here      
write(6,'(a,i0,a)') 'The numerical code of the i/o error was ',errorio,'.'
stop
!----------------------------------------------------------------------
end program adcirc2netcdf
!----------------------------------------------------------------------



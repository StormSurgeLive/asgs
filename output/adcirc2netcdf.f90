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
use asgsio, only : fileMetaData_t, addDataAttributesNetCDF
use ioutil, only : ASCII, NETCDF3, NETCDF4, cmdlinearg, cmdlineopt, argcount, availableUnitNumber, check, openFileForRead
use adcmesh
use nodalattr
implicit none
character(2048) :: dataFileBase
character(2048) :: attFile
character(120), allocatable :: att(:,:)
character(1000) :: Line
character(1) :: JunkC, Tadj
real(8) :: temp1, temp2
! owi or gridded dataset associated variables
character(len=1000) :: owiheader
character(len=2048) :: errorVar
real(8), allocatable :: owi1(:,:)
real(8), allocatable :: owi2(:,:)
integer(8) :: date1
integer(8) :: date2
real(8) :: dxOWI
real(8) :: dyOWI
real(8) :: swLatOWI
real(8) :: swLonOWI
integer :: iLatOWI
integer :: iLonOWI
integer :: iMinOWI
integer :: iCYMDHOWI
integer :: NC_DimID_x
integer :: NC_DimID_y
integer :: NC_Start_OWI(3)
integer :: NC_Count_OWI(3)
integer :: NC_VarID_owibp
integer :: NC_VarID_owibvx
integer :: NC_VarID_owibvy
integer :: NC_VarID_owirp
integer :: NC_VarID_owirvx
integer :: NC_VarID_owirvy
! ^^^ end owi or gridded dataset variables ^^^ 
integer :: yy, mo, dd, hh, mi
integer :: i, j, k, N, SS
logical :: meshonly   ! .true. if user just wants to convert the mesh
logical :: dataonly   ! .true. if user just wants to convert the data
integer :: ncStartMinMax(1)
integer :: ncCountMinMax(1)
integer :: NC_DimID(2) = (/ -99, -99 /)
integer :: NC_DimID_grid(3) = (/ -99, -99, -99 /)
integer, dimension(2) :: timeOfNC_Start
integer, parameter :: version = 4
integer :: lastSlashPosition ! used for trimming full path from a filename
integer :: lastDotPosition ! to determine file extension
character(2048) :: dataFileExtension ! something like 13, 14, 15, 63, 222 etc
integer :: iret !jgfdebug
integer :: lineNum
type(fileMetaData_t) :: fa  ! adcirc file to be read and converted
type(fileMetaData_t) :: fn  ! corresponding netcdf file to be written
real(8), allocatable :: adcirc_data(:,:)
integer, allocatable :: adcirc_idata(:) 
logical :: deflate ! true if compiled with support for internal file compression in netcdf files
integer :: c ! number of components
integer :: attUnit ! unit number for attributes file
integer :: nc_count(2) ! number of values to read along each dimension of netcdf file data
integer :: nc_start(2) ! index to start reading along each dimension
integer :: snapi ! time step number associated with a particular dataset
integer :: snapr ! time (s) associated with a particular dataset
integer :: errorIO
!
! initializations
meshFileName = "fort.14"
attFile = "null"
!
fa%fileFormat = ASCII
fa%dataFileName = "null"
fa%dataFileType = "null"
fa%dataRank = "Scalar"
fa%timeVarying = .true.
fa%griddedData = .false.
fa%dataCenter = 'Node'
fa%num_components = 1
!
fn%fileFormat = NETCDF4
fn%fillValue = -99999.d0
fn%ifillValue = -99999
!
dataFileBase = "null"
meshonly = .false.
dataonly = .false.
lineNum=1
SS=1
#if NETCDF_CAN_DEFLATE
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
            fn%fileFormat = NETCDF4
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--netcdf3")
            fn%fileFormat = NETCDF3
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--meshonly")
            meshonly = .true.
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--dataonly")
            dataonly = .true.
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--meshfile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            meshFileName = trim(cmdlinearg)
         case("--attfile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            attFile = trim(cmdlinearg)
         case("--datafile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            fa%dataFileName = trim(cmdlinearg)
         case("--datafiletype")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            fa%dataFileType = trim(cmdlinearg)
         case default
            write(6,'(a,a,a)') "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
end if
!
! trim off the full path so we just have the file name
lastSlashPosition = index(trim(fa%dataFileName),"/",.true.) 
! now set NETCDF file name for files containing only one type of data
if (meshonly.eqv..true.) then
   ! trim off the full path so we just have the file name
   lastSlashPosition = index(trim(meshFileName),"/",.true.)
   fn%dataFileName = trim(meshFileName(lastSlashPosition+1:))//'.nc'
   write(6,'(a,a,a)') 'DEBUG: The name of the netCDF file will be ',trim(fn%dataFileName),'.'
else
   fn%dataFileName = trim(fa%dataFileName(lastSlashPosition+1:))//'.nc'
endif
dataFileBase = trim(fa%dataFileName(lastSlashPosition+1:))
lastDotPosition = index(trim(dataFileBase),'.',.true.)
dataFileExtension = trim(dataFileBase(lastDotPosition+1:))
!
! If the data file type was not supplied, then use the file name 
! as the file type.
if ( trim(fa%dataFileType).eq.'null') then
   fa%dataFileType = trim(dataFileBase)
   fn%dataFileType = fa%dataFileType
endif      
!
! Set characteristics based on file type of ascii data.
select case(trim(fa%dataFileType))
case('maxele.63','maxvel.63','maxwvel.63','maxrs.63','minpr.63','swan_HS_max.63','swan_TPS_max.63')
   fa%num_components = 1 ! set to default
   fn%num_components = 1 ! set to default
   ! determine whether 
case('fort.13','fort.88')
   fa%timeVarying = .false.
   fn%timeVarying = .false.
case('fort.221','fort.223')
   fa%griddedData = .true.
   fa%timeVarying = .true.
   fn%griddedData = .true.
   fn%timeVarying = .true.   
case('fort.222','fort.224')
   fa%griddedData = .true.
   fa%num_components = 2
   fn%griddedData = .true.
   fn%num_components = 2
case default
   fa%timeVarying = .true.
   fa%griddedData = .false.
   fn%timeVarying = .true.
   fn%griddedData = .false.
end select
!
! Load netCDF Attributes if they have been provided by an external file
if (trim(attfile).eq.'null') then
   ! set default netcdf metadata in case they were not provided
   write(6,'(a)') 'INFO: adcirc2netcdf.f90: Setting default netcdf metadata/attributes.'
   fn%natt = 10
   allocate(att(1:2,1:fn%natt))
   fn%datenum = 'seconds since 2008-07-31 12:00:00 +00:00'
   att(1:2,1) = 'NCPROJ'
   att(1:2,2) = 'NCINST' 
   att(1:2,3) = 'NCSOUR' 
   att(1:2,4) = 'NCHIST'  
   att(1:2,5) = 'NCREF' 
   att(1:2,6) = 'NCCOM' 
   att(1:2,7) = 'NCHOST'
   att(1:2,8) = 'NCCONV' 
   att(1:2,9) = 'NCCONT' 
   att(1:2,10) = 'NCDATE' 
   lineNum=1
else  
   write(6,'(a)') 'INFO: adcirc2netcdf.f90: Opening netcdf metadata/attributes file.'
   attUnit = availableUnitNumber()
   call openFileForRead(attUnit,attFile,errorIO)
   read(attUnit,*,end=246,err=248,iostat=errorio) fn%natt
   lineNum=lineNum+1
   allocate(att(1:2,1:fn%natt))
   read(attUnit,'(A)',end=246,err=248,iostat=errorio) fn%datenum !seconds since 2008-07-31 12:00:00 +00:00
   lineNum=lineNum+1
   do i = 1,fn%natt
      read(attUnit,*,end=246,err=248,iostat=errorio) att(1,i), att(2,i)
      lineNum=lineNum+1
   enddo
   close(attUnit)
   lineNum=1
   write(6,'(a)') "INFO: Finished reading metadata/attributes file."
endif
!
! create netcdf file
write(6,'(a,a,a)') "INFO: Creating NetCDF file '"//trim(fn%dataFileName)//"'."
fn%ncFileType = NF90_CLOBBER ! netcdf3 format, netcdf classic model

#ifdef HAVE_NETCDF4
if (fn%fileFormat.eq.NETCDF4) then
   fn%ncFileType = ior(NF90_HDF5,NF90_CLASSIC_MODEL) ! netcdf4 (i.e., hdf5) format, netcdf classic model
endif
#endif

call check(nf90_create(trim(fn%dataFileName), fn%ncFileType, fn%nc_id))
!
! add netcdf metadata as global attributes 
do i = 1,fn%natt
   call check(nf90_put_att(fn%nc_id,nf90_global,att(1,i),att(2,i)))
enddo
!
! write the mesh definitions to the netcdf file unless the 
! dataonly command line option was specified
if ( (meshonly.eqv..false.).and.(trim(dataFileExtension).ne.'88').and.(trim(dataFileExtension).ne.'13').and.(fa%griddedData.eqv..false.) ) then
   write(6,'(a)') 'INFO: Checking number of nodes in data file.' 
   call openFileForRead(20, trim(fa%dataFileName), errorIO)
   read(20,'(a)',end=246,err=248,iostat=errorio) JunkC
   lineNum=lineNum+1
   read(20,*,end=246,err=248,iostat=errorio) fa%nSnaps, fa%numValuesPerDataset, fa%time_increment, fa%nspool, fa%num_components
   lineNum=lineNum+1
   close(20)
   lineNum=1
   !
   ! for min/max files, we now know the number of components based on the
   ! numSnaps 
   select case(trim(fa%dataFileType))
   case('maxele.63','maxvel.63','maxwvel.63','maxrs.63','minpr.63','swan_HS_max.63','swan_TPS_max.63')
      if (fa%nSnaps.eq.2) then
         fa%timeOfOccurrence  = .true.
         fn%timeOfOccurrence = .true. 
         write(6,'(a)') 'INFO: adcirc2netcdf.f90: Time of occurrence data were found in this min/max file.'
      endif
   end select
endif
!
! Define spatial dimensions and write to netcdf
if (fa%griddedData.eqv..true.) then
   ! only owi gridded data currently supported
   select case(trim(fa%dataFileType)) 
   case('fort.221','fort.222','fort.223','fort.224')   
      ! open the file and read the header
      fa%fun = 22
      write(6,'(a)') 'INFO: adcirc2netcdf.f90: Opening data file "',trim(fa%dataFileName),'".' 
      call openFileForRead(fa%fun, trim(fa%dataFileName), errorIO)
      owiheader(:) = ' '  !set owiheader to blanks before read
      errorVar = "owiheader"
      read(fa%fun, fmt='(a80)',end=99998,err=99999,iostat=errorIO) owiheader
      call checkErrOWI(errorIO,errorVar,fa%dataFileType)
      errorVar = "start date"
      read(owiheader(56:65),'(i10)',end=99998,err=99999,iostat=errorIO) date1
      call checkErrOWI(errorIO,errorVar,fa%dataFileType)
      write(6,'("INFO: adcirc2netcdf.x: ",a," in ",a," is ",i10,".")') trim(errorVar), trim(fa%dataFileName), date1
      errorVar = "end date"
      read(owiheader(71:80),'(i10)',end=99998,err=99999,iostat=errorIO) date2
      call checkErrOWI(errorIO,errorVar,fa%dataFileType)
      write(6,'("INFO: adcirc2netcdf.x: ",a," in ",a," is ",i10,".")') trim(errorVar), trim(fa%dataFileName), date2 
      !     
      ! Read grid specifications/date 
      errorVar = "grid specifications/date"
      read (fa%fun,11,end=99998,err=99999,iostat=errorIO) iLatOWI,iLonOWI,dxOWI,dyOWI,swlatOWI,swlonOWI,iCYMDHOWI,iMinOWI
 11  format(t6,i4,t16,i4,t23,f6.0,t32,f6.0,t44,f8.0,t58,f8.0,t69,i10,i2)
      write(6,'("INFO: adicrc2netcdf.x: iLatOWI=",i0," iLonOWI=",i0" dxOWI=",f6.0," dyOWI=",f6.0," swlatOWI=",f8.0," swlonOWI=",f8.0," iCYMDHOWI=",i0," iMinOWI=",i0)') iLatOWI,iLonOWI,dxOWI,dyOWI,swlatOWI,swlonOWI,iCYMDHOWI,iMinOWI
      call check(nf90_def_dim(fn%nc_id,'lon',iLonOWI,NC_DimID_x))
      call check(nf90_def_dim(fn%nc_id,'lat',iLatOWI,NC_DimID_y))
         ! END jumps here
   99998 write(6,'("ERROR: adcirc2netcdf.x: Unexpectedly reached end-of-file.")')
         !  ERR jumps here
   99999 call checkErrOWI(1,errorVar,fa%dataFileType) 
   case default
      write(6,'(a,a,a)') 'ERROR: adcirc2netcdf.x: Data files of type "',trim(fa%dataFileType),'" are not supported.'
      stop
   end select

else
   ! meshed data, the common case
   if (dataonly.eqv..false.) then
      call read14()
      call writeMeshDefinitionsToNetCDF(fn%nc_id, deflate)
   else       
      np = fa%numValuesPerDataset
      call check(nf90_put_att(fn%nc_id,nf90_global,'description',trim(JunkC)))
      call check(nf90_def_dim(fn%nc_id,'node',np,NC_DimID_node))
   endif
endif
!
! if this is a nodal attributes file, then read it and convert it
! using subroutines from the nodal attributes module and then stop
if (trim(dataFileExtension).eq.'13') then
   call readNodalAttributesFile(fa%dataFileName)
   call writeNodalAttributesFileNetCDF(fn%nc_id, deflate)
   stop
endif
!
! Create time dimension and units attributes
if (fa%timeVarying.eqv..true.) then      
   fn%timeVarying = .true. 
   call check(nf90_def_dim(fn%nc_id,'time',nf90_unlimited,fn%nc_dimid_time))
   call check(nf90_def_var(fn%nc_id,'time',nf90_double,fn%nc_dimid_time,fn%nc_varid_time))
   call check(nf90_put_att(fn%nc_id,fn%nc_varid_time,'long_name','model time'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varid_time,'standard_name','time'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varid_time,'units',fn%datenum))
   !
   ! Create space dimensions and units attributes   
   if (fa%griddedData.eqv..true.) then
      fn%griddedData = .true.
      ! fortran's row major order in memory and netcdf's column major
      ! order on disk means that we have to re-order the y dimension 
      ! before the x dimension  so that the data are passed the way 
      ! netcdf expects
      NC_DimID_grid = (/ NC_DimID_y, NC_DimID_x, fn%NC_DimID_Time /)
   endif
endif
!
! add attributes for CF compliance
call addDataAttributesNetCDF(fn)
!      
! create adcirc output variables and associated attributes
fn%num_components = 1
#ifdef NETCDF_CAN_DEFLATE
      if (fn%fileFormat.eq.NETCDF4) then
         do j=1,fn%num_components
            call check(nf90_def_var_deflate(fn%nc_id, fn%nc_varid(j), 1, 1, 2))
         enddo
      endif
#endif

!----------------------------------------------------------------
! end variable and attributes definitions
!----------------------------------------------------------------
call check(nf90_enddef(fn%nc_id))

! place mesh-related data into the file, unless this is a data 
! only file
if ( (dataonly.eqv..false.).and.(fa%griddedData.eqv..false.) ) then
   call writeMeshDataToNetCDF(fn%nc_id)
endif
!
! finish up if only mesh data are to be converted
if (meshonly.eqv..true.) then
   call check(nf90_close(fn%nc_id))
   write(6,'(a)') 'INFO: Only mesh data were written.'
   stop
endif
!
! write grid coordinates if appropriate
if (fa%griddedData.eqv..true.) then

  ! <write grid coordinates>

endif
fa%fun = availableUnitNumber()
write(6,'(a,a,a)') 'INFO: adcirc2netcdf.f90: Opening data file "',trim(fa%dataFileName),'".'
call openFileForRead(fa%fun, trim(fa%dataFileName), errorIO)

select case(trim(fa%dataFileType))
case('fort.221','fort.222','fort.223','fort.224')
   fa%numValuesPerDataset = iLonOWI * iLatOWI 
case('fort.88') 
   fa%numValuesPerDataset = np
   fa%time_increment = -99999.d0
   fa%nspool = -99999 
   fa%num_components = 1
case default
   ! fort.63 etc
   READ(fa%fun,'(A)',end=246,err=248,iostat=errorio) JunkC
   lineNum=lineNum+1
   ! jgf: Can't rely on the NumSnaps value; in general, it will not
   ! actually reflect the number of datasets in the file.
   READ(fa%fun,*,end=246,err=248,iostat=errorio) fa%nSnaps, fa%numValuesPerDataset, fa%time_increment, fa%nspool, fa%num_components
   lineNum=lineNum+1
   if ( (np.ne.fa%numValuesPerDataset).and.(trim(fa%dataCenter(1)).eq.'Node') ) then
      write(6,'(a,i0,a,i0,a)') 'ERROR: The output file contains ',fa%numValuesPerDataset,        &
        ' nodes, but the mesh file contains ',np,' nodes.'
       write(6,'(a)') 'ERROR: The output file does not correspond to the mesh file.'
      close(fa%fun)
      stop
   endif
   if ( (ne.ne.fa%numValuesPerDataset).and.(trim(fa%dataCenter(1)).eq.'Cell') ) then
      write(6,'(a,i0,a,i0,a)') 'ERROR: The output file contains ',fa%numValuesPerDataset,        &
        ' elements, but the mesh file contains ',ne,' elements.'
       write(6,'(a)') 'ERROR: The output file does not correspond to the mesh file.'
      close(fa%fun)
      stop
   endif
end select
!
! Allocate space to hold the data
select case(fn%netCDFDataType)
case(NF90_DOUBLE)
   if (fa%griddedData.eqv..true.) then
      ! y before x according to netcdf specification in fortran api
      allocate(owi1(1:iLatOWI,1:iLonOWI))
      if (fa%num_components.eq.2) then
         ! y before x according to netcdf specification in fortran api
         allocate(owi2(1:iLatOWI,1:iLonOWI))
      endif
   else
      allocate(adcirc_data(1:2,fa%numvaluesperdataset))
   endif
case(NF90_INT)
   allocate(adcirc_idata(1:fa%numValuesPerDataset))      
case default
   write(6,'(a,i0)') 'ERROR: Unsupported data type: ',fn%netCDFDataType
end select
!
! Read ascii data and write to netcdf file
SS=1        ! initialize the dataset counter
lineNum = 1 ! initialize the line number counter
!
! gridded data
if (fa%griddedData.eqv..true.) then
   do   ! loop until we run out of gridded data
      owi1(:,:) = fa%fillValue
      owi2(:,:) = fa%fillValue
      errorVar = "first component"
      read(fa%fun,22,end=321,err=9999,iostat=errorIO) ((owi1(j,i),i=1,iLonOWI),j=1,iLatOWI)
      call checkErrOWI(errorIO,errorVar,fa%dataFileType)
      if (fa%num_components.eq.2) then
         errorVar = "second component"
         read(fa%fun,22,end=123,err=9999,iostat=errorIO) ((owi2(j,i),i=1,iLonOWI),j=1,iLatOWI)
         call checkErrOWI(errorIO,errorVar,fa%dataFileType)
      endif
      call check(nf90_put_var(fn%nc_id,fn%nc_varid_time,(/snapr/),(/ss/),(/1/)))
      NC_Count_OWI = (/ iLatOWI, iLonOWI, 1 /)
      NC_Start_OWI = (/ 1, 1, SS /)
      timeOfNC_Start = (/ 1, 1 /)
      ! write the dataset to the netcdf file
      select case(trim(fa%dataFileType))
         case('fort.63') !63
            call check(nf90_put_var(fn%nc_id,fn%nc_varid(1),adcirc_data(1,:),nc_start_owi,nc_count_owi))
         case default
      end select
22    format(8f10.0)
   end do
9999  call checkErrOWI(1,errorVar,fa%dataFileType) ! ERR during read jumps to here
321 continue  ! jgf: jump here when no data left in gridded ascii file
   write(6,'(/,a,i0,a)') 'INFO: Wrote ',ss-1,' dataset(s).'
   close(fa%fun)
   call check(nf90_close(fn%nc_id))
   write(6,'(a)') 'INFO: adcirc2netcdf.x: Finished writing gridded data to netcdf.'
   stop !
endif
!
! mesh data
DO   ! jgf: loop until we run out of mesh data
   if (trim(fa%dataFileType).ne.'fort.88') then
      read(fa%fun,'(a)',end=123,err=123) line
      lineNum = lineNum + 1
      read(Line,*,end=246,err=248,iostat=errorio) SnapR, SnapI
      read(line,*,err=907,end=907) snapr, snapi, fa%numNodesNonDefault, fa%defaultValue
      goto 908  ! jgf: this file is sparse ascii
   endif
907     fa%numNodesNonDefault = fa%numValuesPerDataset
   fa%defaultValue = -99999.0d0
908 if (fn%netCDFDataType.eq.NF90_DOUBLE) then
         adcirc_data(:,:) = fa%defaultValue
   endif
   j=0
   do n=1,fa%numNodesNonDefault
     select case(trim(fa%dataRank))
       case("Scalar")                    ! scalar data
         if (fn%netCDFDataType.eq.NF90_DOUBLE) then
            if (trim(fa%dataFileType).eq.'fort.88') then
               read(fa%fun,*,end=246,err=248,iostat=errorio) Temp1
               lineNum = lineNum + 1
               j = j + 1
            else                  
               read(fa%fun,*,end=246,err=248,iostat=errorio) j,Temp1
               lineNum = lineNum + 1
            endif
            adcirc_data(1,j) = Temp1
         else
            read(fa%fun,*,end=246,err=248,iostat=errorio) j,adcirc_idata(n)
            lineNum = lineNum + 1
         endif
       case("2DVector")                  ! 2D vector data
         read(fa%fun,*,end=246,err=248,iostat=errorio) j,Temp1,Temp2
         lineNum = lineNum + 1
         adcirc_data(1,j) = temp1
         adcirc_data(2,j) = temp2
      case default
         write(6,'(a,a,a)') 'ERROR: adcirc2netcdf: ',trim(fa%dataRank),' data rank is not supported.'
         stop
     end select
   enddo
   
   call check(nf90_put_var(fn%nc_id,fn%nc_varid_time,(/snapr/),(/ss/),(/1/)))
   NC_Count = (/ fa%numValuesPerDataset, 1 /)
   NC_Start = (/ 1, SS /)
   !
   ncStartMinMax = (/ 1 /)
   ncCountMinMax = (/ SS /)
   ! write the dataset to the netcdf file
   if (fn%isInteger.eqv..true.) then
      call check(nf90_put_var(fn%nc_id,fn%nc_varid(1),adcirc_idata,nc_start,nc_count))
   else
      do c=1,fa%num_components
         call check(nf90_put_var(fn%nc_id,fn%nc_varid(c),adcirc_data(c,:),nc_start,nc_count))
      end do
   endif
   write(6,advance='no',fmt='(i6)') ss
   SS = SS + 1 ! jgf: Increment the dataset counter
   !
   if (trim(fa%dataFileType).eq.'fort.88') then
      exit
   endif
ENDDO

123   CONTINUE  ! jgf: When we've run out of datasets in the current file,
                ! we jump to here.
write(6,'(/,a,i0,a)') 'INFO: Wrote ',ss-1,' dataset(s).'

close(fa%fun)
call check(nf90_close(fn%nc_id))
write(6,'(a)') 'INFO: adcirc2netcdf finished.'
stop
      
      
      ! We jump to this section if there was an error reading a file.
246   write(6,'(a)') 'ERROR: Unexpectedly reached end-of-file.' ! END jumps here
248   write(6,'(a)') 'ERROR: I/O error during file access.'     ! ERR jumps here
write(6,'(a,i0,a,i0,a)') 'INFO: Attempted to read line ',lineNum,' in dataset ',SS,'.' ! ERR jumps here      
write(6,'(a,i0,a)') 'The numerical code of the i/o error was ',errorio,'.'
          
!----------------------------------------------------------------------
end program adcirc2netcdf
!----------------------------------------------------------------------


!-----------------------------------------------------------------------
!  S U B R O U T I N E   C H E C K  E R R  O W I 
!-----------------------------------------------------------------------
! Checks the return value from subroutine calls; if there
! was an error, it writes a termination message and exits.
!-----------------------------------------------------------------------
subroutine checkErrOWI(iret,errorVar,dataFileType)
implicit none
integer, intent(in) :: iret
character(len=2048), intent(in) :: errorVar
character(len=20), intent(in) :: dataFileType

if (iret.ne.0) then
   if (trim(errorVar).ne."") then
      write(6,'("ERROR: adcirc2netcdf.x: Failed to read ",a," from ",a,".")') trim(errorVar), trim(dataFileType)
   else
      write(6,'("ERROR: adcirc2netcdf.x: Failed to read ",a,".")') trim(dataFileType)
      stop
   endif
endif
!-----------------------------------------------------------------------
end subroutine checkErrOWI
!-----------------------------------------------------------------------


!----------------------------------------------------------------------
!  GETMONTHDAY
!----------------------------------------------------------------------
subroutine getmonthday(dy,yy,mo,dd)

implicit none
integer, intent(out)           :: mo, dd
integer, intent(in)            :: yy, dy
integer                       :: i
integer, allocatable          :: cd(:)

allocate(cd(1:13))
cd = (/ 0,31,59,90,120,151,181,212,243,273,304,334,365 /)
if( mod(yy,4) == 0 ) then
   cd = (/ 0,31,60,91,121,152,182,213,244,274,305,335,366 /)
endif
do i = 1,12
   if (dy.gt.cd(i)) then
      mo = i
      dd = dy-cd(i)
   endif
enddo
!----------------------------------------------------------------------
end subroutine getmonthday
!----------------------------------------------------------------------

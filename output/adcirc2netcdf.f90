! This program will convert ADCIRC+SWAN ASCII Output to NETCDF
! Copyleft by Patrick C. Kerr
! University of Notre Dame
! pkerr@nd.edu

! 2012-01-12:   v1 - Original
! 2012-01-16:   v2 - Fixed Time Bug, Made CF Compliant
! 2012-01-17:   v3 - Added maxele
! 2012-02-27:   v4 ...
!------------------------------------------------------------------
! adcirc2netcdf: Convert ADCIRC ascii output files to netcdf format.
!------------------------------------------------------------------
! Copyright(C) 2012--2016 Jason Fleming
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
use adcmesh
use nodalattr
use adcircdata
implicit none
character(2048) :: dataFileBase
character(2048) :: dataFileType
integer :: convertedFileFormat ! netcdf3 or netcdf4
character(2048) :: netCDFFile, AttFile
character(120) :: dataRank
character(120), allocatable :: att(:,:)
character(1000) :: Line
character(1) :: JunkC, Tadj
real(8) :: temp1, temp2
integer :: lun
real(8), allocatable :: Global1(:), Global2(:)
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
integer, allocatable :: idata(:)
integer :: numValuesPerDataset
integer :: yy, mo, dd, hh, mi
integer :: i, j, k, N, SS
integer :: unitnumber
logical :: meshonly   ! .true. if user just wants to convert the mesh
logical :: dataonly   ! .true. if user just wants to convert the data
logical :: timeVarying ! .true. for time varying data
logical :: griddedData ! .true. if the data are on a regular grid (not a mesh) 
integer :: ncFileType
integer :: ncStartMinMax(1)
integer :: ncCountMinMax(1)
integer :: NC_DimID(2) = (/ -99, -99 /)
integer :: NC_DimID_grid(3) = (/ -99, -99, -99 /)
integer :: NC_DimID_single = -99
integer :: NC_VarID_zeta = -99
integer :: NC_VarID_tau0 = -99
integer :: NC_VarID_eta1 = -99
integer :: NC_VarID_eta2 = -99
integer :: NC_VarID_tk = -99
integer :: NC_VarID_offset = -99
integer :: NC_VarID_uu1_vel = -99
integer :: NC_VarID_vv1_vel = -99
integer :: NC_VarID_nodecode = -99
integer :: NC_VarID_noff = -99
integer :: NC_VarID_dryelementareacheck = -99
integer :: NC_VarID_coefdiagonal = -99
integer :: NC_VarID_coefele = -99                   
integer :: NC_VarID_nneighele = -99
integer :: NC_VarID_nodeids = -99
integer :: NC_VarID_elementids = -99         
integer :: NC_VarID_u_vel = -99
integer :: NC_VarID_v_vel = -99
integer :: NC_VarID_maxele = -99
integer :: NC_VarID_timeOfmaxele = -99
integer :: NC_VarID_minpr = -99
integer :: NC_VarID_timeOfminpr = -99
integer :: NC_VarID_maxwvel = -99
integer :: NC_VarID_timeOfmaxwvel = -99
integer :: NC_VarID_maxvel = -99
integer :: NC_VarID_timeOfmaxvel = -99
integer :: NC_VarID_p = -99
integer :: NC_VarID_windx = -99
integer :: NC_VarID_windy = -99
integer :: NC_VarID_dir = -99
integer :: NC_VarID_hs = -99
integer :: NC_VarID_tmm10 = -99
integer :: NC_VarID_tps = -99
integer :: NC_VarID_swantpsmax = -99
integer :: NC_VarID_timeOfswantpsmax = -99
integer :: NC_VarID_swanhsmax = -99
integer :: NC_VarID_timeOfswanhsmax = -99
integer :: NC_VarID_eslnodes = -99
integer, dimension(2) :: timeOfNC_Start
integer, parameter :: version = 4
integer :: varid(3) ! varids for netcdf4 compression
integer :: lastSlashPosition ! used for trimming full path from a filename
integer :: lastDotPosition ! to determine file extension
character(2048) :: dataFileExtension ! something like 13, 14, 15, 63, 222 etc
integer :: iret !jgfdebug
integer :: lineNum
! initializations
meshFileName = "fort.14"
attFile = "null"
dataFile = "null"
dataFileType = "null"
dataFileBase = "null"
dataRank = "Scalar"
convertedFileFormat = NETCDF4
meshonly = .false.
dataonly = .false.
timeVarying = .true.
griddedData = .false.
dataCenter = 'Node'
lineNum=1
SS=1
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
            convertedFileFormat = NETCDF4
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--netcdf3")
            convertedFileFormat = NETCDF3
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
            dataFile = trim(cmdlinearg)
         case("--datafiletype")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            dataFileType = trim(cmdlinearg)
         case default
            write(6,'(a,a,a)') "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
end if
!
! trim off the full path so we just have the file name
lastSlashPosition = index(trim(dataFile),"/",.true.) 
! now set NETCDF file name for files containing only one type of data
if (meshonly.eqv..true.) then
   ! trim off the full path so we just have the file name
   lastSlashPosition = index(trim(meshFileName),"/",.true.)
   netCDFFile = trim(meshFileName(lastSlashPosition+1:))//'.nc'
   write(6,'(a,a,a)') 'DEBUG: The name of the netCDF file will be ',trim(netCDFFile),'.'
else
   netCDFFile = trim(dataFile(lastSlashPosition+1:))//'.nc'
endif
dataFileBase = trim(dataFile(lastSlashPosition+1:))
lastDotPosition = index(trim(dataFileBase),'.',.true.)
dataFileExtension = trim(dataFileBase(lastDotPosition+1:))
!
! If the data file type was not supplied, then use the file name 
! as the file type.
if ( trim(dataFileType).eq.'null') then
   dataFileType = trim(dataFileBase)
endif      
!
! Set characteristics based on file type of ascii data.
select case(trim(dataFileType))
case('maxele.63','maxvel.63','maxwvel.63','maxrs.63','minpr.63','swan_HS_max.63','swan_TPS_max.63')
   num_components = 1 ! set to default
   ! determine whether 
case('fort.13','fort.88')
   timeVarying = .false.
case('fort.221','fort.223')
   griddedData = .true.
   timeVarying = .true.
   num_components = 1
case('fort.222','fort.224')
   griddedData = .true.
   timeVarying = .true.
   num_components = 2
case default
   timeVarying = .true.
   griddedData = .false.
end select
!
! Load netCDF Attributes if they have been provided by an external file
if (trim(attfile).eq.'null') then
   ! set default netcdf metadata in case they were not provided
   write(6,'(a)') 'INFO: adcirc2netcdf.f90: Setting default netcdf metadata/attributes.'
   natt = 10
   allocate(att(1:2,1:natt))
   datenum = 'seconds since 2008-07-31 12:00:00 +00:00'
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
   call openFileForRead(100,AttFile)
   read(100,*,end=246,err=248,iostat=errorio) natt
   lineNum=lineNum+1
   allocate(att(1:2,1:natt))
   read(100,'(A)',end=246,err=248,iostat=errorio) datenum !seconds since 2008-07-31 12:00:00 +00:00
   lineNum=lineNum+1
   do i = 1,natt
      read(100,*,end=246,err=248,iostat=errorio) att(1,i), att(2,i)
      lineNum=lineNum+1
   enddo
   close(100)
   lineNum=1
   write(6,'(a)') "INFO: Finished reading metadata/attributes file."
endif
!
! create netcdf file
write(6,'(a,a,a)') "INFO: Creating NetCDF file '"//trim(netCDFFile)//"'."
ncFileType = NF90_CLOBBER ! netcdf3 format, netcdf classic model

#ifdef HAVE_NETCDF4
if (convertedFileFormat.eq.NETCDF4) then
   ncFileType = ior(NF90_HDF5,NF90_CLASSIC_MODEL) ! netcdf4 (i.e., hdf5) format, netcdf classic model
endif
#endif

CALL Check(NF90_CREATE(TRIM(netCDFFile),ncFileType,NC_ID))
!
! add netcdf metadata as global attributes 
do i = 1,natt
   CALL Check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,att(1,i),att(2,i)))
enddo
!
! write the mesh definitions to the netcdf file unless the 
! dataonly command line option was specified
if ( (meshonly.eqv..false.).and.(trim(dataFileExtension).ne.'88').and.(trim(dataFileExtension).ne.'13').and.(griddedData.eqv..false.) ) then
   write(6,'(a)') 'INFO: Checking number of nodes in data file.' 
   call openFileForRead(20, trim(dataFile))
   read(20,'(a)',end=246,err=248,iostat=errorio) JunkC
   lineNum=lineNum+1
   read(20,*,end=246,err=248,iostat=errorio) numSnaps, numValuesPerDataset, tInterval, Interval, nCol
   lineNum=lineNum+1
   close(20)
   lineNum=1
   !
   ! for min/max files, we now know the number of components based on the
   ! numSnaps 
   select case(trim(dataFileType))
   case('maxele.63','maxvel.63','maxwvel.63','maxrs.63','minpr.63','swan_HS_max.63','swan_TPS_max.63')
      if (numSnaps.eq.2) then
         num_components = numSnaps 
         write(6,'(a)') 'INFO: adcirc2netcdf.f90: Time of occurrence data were found in this min/max file.'
      endif
   end select
endif


!
! Define spatial dimensions and write to netcdf
if (griddedData.eqv..true.) then
   ! only owi gridded data currently supported
   select case(trim(dataFileType)) 
   case('fort.221','fort.222','fort.223','fort.224')   
      ! open the file and read the header
      lun = 22
      write(6,'(a)') 'INFO: adcirc2netcdf.f90: Opening data file "',trim(dataFile),'".' 
      call openFileForRead(lun, trim(dataFile))
      owiheader(:) = ' '  !set owiheader to blanks before read
      errorVar = "owiheader"
      read(lun, fmt='(a80)',end=99998,err=99999,iostat=errorIO) owiheader
      call checkErrOWI(errorIO,errorVar,dataFileType)
      errorVar = "start date"
      read(owiheader(56:65),'(i10)',end=99998,err=99999,iostat=errorIO) date1
      call checkErrOWI(errorIO,errorVar,dataFileType)
      write(6,'("INFO: adcirc2netcdf.x: ",a," in ",a," is ",I10,".")') trim(errorVar), trim(datafile), date1
      errorVar = "end date"
      read(owiheader(71:80),'(i10)',end=99998,err=99999,iostat=errorIO) date2
      call checkErrOWI(errorIO,errorVar,dataFileType)
      write(6,'("INFO: adcirc2netcdf.x: ",a," in ",a," is ",i10,".")') trim(errorVar), trim(datafile), date2 
      !     
      ! Read grid specifications/date 
      errorVar = "grid specifications/date"
      read (lun,11,end=99998,err=99999,iostat=errorIO) iLatOWI,iLonOWI,dxOWI,dyOWI,swlatOWI,swlonOWI,iCYMDHOWI,iMinOWI
 11  format(t6,i4,t16,i4,t23,f6.0,t32,f6.0,t44,f8.0,t58,f8.0,t69,i10,i2)
      write(6,'("INFO: adicrc2netcdf.x: iLatOWI=",i0," iLonOWI=",i0" dxOWI=",f6.0," dyOWI=",f6.0," swlatOWI=",f8.0," swlonOWI=",f8.0," iCYMDHOWI=",i0," iMinOWI=",i0)') iLatOWI,iLonOWI,dxOWI,dyOWI,swlatOWI,swlonOWI,iCYMDHOWI,iMinOWI
      call check(NF90_DEF_DIM(NC_ID,'lon',iLonOWI,NC_DimID_x))
      call check(NF90_DEF_DIM(NC_ID,'lat',iLatOWI,NC_DimID_y))
         ! END jumps here
   99998 write(6,'("ERROR: adcirc2netcdf.x: Unexpectedly reached end-of-file.")')
         !  ERR jumps here
   99999 call checkErrOWI(1,errorVar,dataFileType) 
   case default
      write(6,'(a,a,a)') 'ERROR: adcirc2netcdf.x: Data files of type "',trim(dataFileType),'" are not supported.'
      stop
   end select

else
   ! meshed data, the common case
   if (dataonly.eqv..false.) then
      call read14()
      call writeMeshDefinitionsToNetCDF(NC_ID, convertedFileFormat)
   else       
      np = numValuesPerDataset
      call check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,'description',trim(JunkC)))
      call check(NF90_DEF_DIM(NC_ID,'node',np,NC_DimID_node))
   endif
endif

!
! if this is a nodal attributes file, then read it and convert it
! using subroutines from the nodal attributes module and then stop
if (trim(dataFileExtension).eq.'13') then
   call readNodalAttributesFile(dataFile)
   call writeNodalAttributesFileNetCDF(nc_id, convertedFileFormat)
   stop
endif

!
! Create time dimension and units attributes

if (timeVarying.eqv..true.) then       
   CALL Check(NF90_DEF_DIM(NC_ID,'time',NF90_UNLIMITED,NC_DimID_time))
   CALL Check(NF90_DEF_VAR(NC_ID,'time',NF90_DOUBLE,NC_DimID_time,NC_VarID_time))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'long_name','model time'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'standard_name','time'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'units',datenum))
   !
   ! Create space dimensions and units attributes   
   if (griddedData.eqv..true.) then
      ! fortran's row major order in memory and netcdf's column major
      ! order on disk means that we have to re-order the y dimension 
      ! before the x dimension  so that the data are passed the way 
      ! netcdf expects
      NC_DimID_grid = (/ NC_DimID_y, NC_DimID_x, NC_DimID_Time /)
   endif
endif
NC_DimID = (/ NC_DimID_node, NC_DimID_Time /)
!      
! create adcirc output variables and associated attributes
select case(trim(dataFileType))
case('fort.221') 
   CALL Check(NF90_DEF_VAR(NC_ID,'basinpressure',NF90_DOUBLE,NC_DimID_grid,NC_VarID_owibp))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibp,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibp,'long_name','air pressure at sea level on basin grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibp,'standard_name','air_pressure_basin_grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibp,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibp,'units','mbar'))
   num_components = 1
   varid(1) = NC_VarID_owibp
case('fort.223') 
   CALL Check(NF90_DEF_VAR(NC_ID,'regionpressure',NF90_DOUBLE,NC_DimID_grid,NC_VarID_owirp))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirp,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirp,'long_name','air pressure at sea level on region grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirp,'standard_name','air_pressure_region_grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirp,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirp,'units','mbar'))
   num_components = 1
   varid(1) = NC_VarID_owirp
case('fort.222') 
   CALL Check(NF90_DEF_VAR(NC_ID,'basinwindx',NF90_DOUBLE,NC_DimID_grid,NC_VarID_owibvx))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvx,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvx,'long_name','e/w wind velocity on basin grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvx,'standard_name','eastward_wind_basin_grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvx,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvx,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvx,'positive','east'))
   num_components = 2
   varid(1) = NC_VarID_owibvx
   CALL Check(NF90_DEF_VAR(NC_ID,'basinwindy',NF90_DOUBLE,NC_DimID_grid,NC_VarID_owibvy))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvy,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvy,'long_name','n/s wind velocity on basin grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvy,'standard_name','nortward_wind_basin_grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvy,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvy,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owibvy,'positive','north'))
   varid(2) = NC_VarID_owibvy
case('fort.224') 
   CALL Check(NF90_DEF_VAR(NC_ID,'regionwindx',NF90_DOUBLE,NC_DimID_grid,NC_VarID_owirvx))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvx,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvx,'long_name','e/w wind velocity on region grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvx,'standard_name','eastward_wind_region_grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvx,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvx,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvx,'positive','east'))
   num_components = 2
   varid(1) = NC_VarID_owirvx
   CALL Check(NF90_DEF_VAR(NC_ID,'regionwindy',NF90_DOUBLE,NC_DimID_grid,NC_VarID_owirvy))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvy,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvy,'long_name','n/s wind velocity on region grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvy,'standard_name','nortward_wind_region_grid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvy,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvy,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_owirvy,'positive','north'))
   varid(2) = NC_VarID_owirvy
case('fort.63') !63
   CALL Check(NF90_DEF_VAR(NC_ID,'zeta',NF90_DOUBLE,NC_DimID,NC_VarID_zeta))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'long_name','water surface elevation above geoid'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'standard_name','water_surface_elevation'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'units','m'))
   num_components = 1
   varid(1) = NC_VarID_zeta
   !          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'positive','up')) 'DO NOT USE'
case('eta1.63') 
   CALL Check(NF90_DEF_VAR(NC_ID,'eta1',NF90_DOUBLE,NC_DimID,NC_VarID_eta1))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta1,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta1,'long_name','water surface elevation above geoid at previous time step'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta1,'standard_name','water_surface_elevation_at_previous_timestep'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta1,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta1,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta1,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta1,'units','m'))
   num_components = 1
   varid(1) = NC_VarID_eta1
case('eta2.63') 
   CALL Check(NF90_DEF_VAR(NC_ID,'eta2',NF90_DOUBLE,NC_DimID,NC_VarID_eta2))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta2,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta2,'long_name','water surface elevation above geoid at current time step'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta2,'standard_name','water_surface_elevation_at_current_timestep'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta2,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta2,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta2,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eta2,'units','m'))
   num_components = 1
   varid(1) = NC_VarID_eta2
case('tk.63') 
   CALL Check(NF90_DEF_VAR(NC_ID,'tk',NF90_DOUBLE,NC_DimID,NC_VarID_tk))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tk,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tk,'long_name','bottom friction force'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tk,'standard_name','bottom_friction_force'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tk,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tk,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tk,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tk,'units','m'))
   num_components = 1
   varid(1) = NC_VarID_tk
case('fort.90') 
   CALL Check(NF90_DEF_VAR(NC_ID,'tau0',NF90_DOUBLE,NC_DimID,NC_VarID_tau0))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tau0,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tau0,'long_name','time varying tau0'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tau0,'standard_name','time_varying_tau0'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tau0,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tau0,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tau0,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tau0,'units','1'))
   num_components = 1
   varid(1) = NC_VarID_tau0
case('offset.63') !63
   CALL Check(NF90_DEF_VAR(NC_ID,'offset',NF90_DOUBLE,NC_DimID,NC_VarID_offset))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_offset,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_offset,'long_name','water level offset'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_offset,'standard_name','water_level_offset'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_offset,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_offset,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_offset,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_offset,'units','m H2O'))
   num_components = 1
   varid(1) = NC_VarID_offset
case('fort.64') 
   dataRank = "2DVector"
   CALL Check(NF90_DEF_VAR(NC_ID,'u-vel',NF90_DOUBLE,NC_DimID,NC_VarID_u_vel))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'long_name','water column vertically averaged east/west velocity'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'standard_name','eastward_water_velocity'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'positive','east'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'dry_Value',-99999.0d0))
   num_components = 2
   varid(1) = NC_VarID_u_vel
   CALL Check(NF90_DEF_VAR(NC_ID,'v-vel',NF90_DOUBLE,NC_DimID,NC_VarID_v_vel))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'long_name','water column vertically averaged north/south velocity'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'standard_name','northward_water_velocity'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'positive','north'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'dry_Value',-99999.0d0))
   varid(2) = NC_VarID_v_vel
case('uu1vv1.64') 
   dataRank = "2DVector"
   CALL Check(NF90_DEF_VAR(NC_ID,'uu1-vel',NF90_DOUBLE,NC_DimID,NC_VarID_uu1_vel))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_uu1_vel,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_uu1_vel,'long_name','water column vertically averaged east/west velocity at previous time step'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_uu1_vel,'standard_name','eastward_water_velocity_at_previous_time_step'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_uu1_vel,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_uu1_vel,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_uu1_vel,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_uu1_vel,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_uu1_vel,'positive','east'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_uu1_vel,'dry_Value',-99999.0d0))
   num_components = 2
   varid(1) = NC_VarID_uu1_vel
   CALL Check(NF90_DEF_VAR(NC_ID,'vv1-vel',NF90_DOUBLE,NC_DimID,NC_VarID_vv1_vel))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_vv1_vel,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_vv1_vel,'long_name','water column vertically averaged north/south velocity at previous time step'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_vv1_vel,'standard_name','northward_water_velocity_at_previous_time_step'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_vv1_vel,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_vv1_vel,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_vv1_vel,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_vv1_vel,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_vv1_vel,'positive','north'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_vv1_vel,'dry_Value',-99999.0d0))
   varid(2) = NC_VarID_vv1_vel
case('fort.73') !73
   CALL Check(NF90_DEF_VAR(NC_ID,'pressure',NF90_DOUBLE,NC_DimID,NC_VarID_p))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'long_name','air pressure at sea level'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'standard_name','air_pressure_at_sea_level'))            
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'units','meters of water'))
   num_components = 1
   varid(1) = NC_VarID_p
!          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'positive','up')) 'DO NOT USE'
case('fort.74') !74
   dataRank = "2DVector"
   CALL Check(NF90_DEF_VAR(NC_ID,'windx',NF90_DOUBLE,NC_DimID,NC_VarID_windx))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windx,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windx,'long_name','e/w wind velocity'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windx,'standard_name','eastward_wind'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windx,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windx,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windx,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windx,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windx,'positive','east'))
   num_components = 2
   varid(1) = NC_VarID_windx
   CALL Check(NF90_DEF_VAR(NC_ID,'windy',NF90_DOUBLE,NC_DimID,NC_VarID_windy))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'long_name','n/s wind velocity'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'standard_name','northward_wind'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'units','m s-1'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'positive','north'))
   varid(2) = NC_VarID_windy
case('maxele.63') !MAXELE
   CALL Check(NF90_DEF_VAR(NC_ID,'zeta_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_maxele))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'long_name','maximum sea surface elevation above datum'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'standard_name','maximum_sea_surface_elevation_above_datum'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'coordinates','y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'units','m'))
   varid(1) = NC_VarID_maxele
   if ( num_components.eq.2) then
      CALL Check(NF90_DEF_VAR(NC_ID,'time_of_zeta_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_timeOfMaxele))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxele,'_FillValue',FillValue))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxele,'long_name','time of maximum sea surface elevation above datum'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxele,'standard_name','time_of_maximum_sea_surface_elevation_above_datum'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxele,'coordinates','y x'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxele,'location','node'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxele,'mesh','adcirc_mesh'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxele,'units','s'))
   endif
   varid(2) = NC_VarID_timeOfmaxele

case('minpr.63') ! minimum barometric pressure
   CALL Check(NF90_DEF_VAR(NC_ID,'pressure_min',NF90_DOUBLE,NC_DimID_node,NC_VarID_minpr))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_minpr,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_minpr,'long_name','minimum air pressure at sea level'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_minpr,'standard_name','minimum_air_pressure_at_sea_level'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_minpr,'coordinates','y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_minpr,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_minpr,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_minpr,'units','meters of water'))
   varid(1) = NC_VarID_minpr
   if ( num_components.eq.2) then
      CALL Check(NF90_DEF_VAR(NC_ID,'time_of_pressure_min',NF90_DOUBLE,NC_DimID_node,NC_VarID_timeOfMinpr))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfminpr,'_FillValue',FillValue))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfminpr,'long_name','time of minimum air pressure at sea level'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfminpr,'standard_name','time_of_minimum_air_pressure_at_sea_level'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfminpr,'coordinates','y x'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfminpr,'location','node'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfminpr,'mesh','adcirc_mesh'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfminpr,'units','s'))
   endif
   varid(2) = NC_VarID_timeOfminpr

case('swan_DIR.63') !DIR
   CALL Check(NF90_DEF_VAR(NC_ID,'swan_DIR',NF90_DOUBLE,NC_DimID,NC_VarID_dir))         
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'long_name','wave direction'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'standard_name','sea_surface_wave_to_direction'))            
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'units','degrees_CW_from_East'))
   num_components = 1
   varid(1) = NC_VarID_dir
case('swan_HS.63') !HS
   CALL Check(NF90_DEF_VAR(NC_ID,'swan_HS',NF90_DOUBLE,NC_DimID,NC_VarID_hs))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'long_name','significant wave height'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'standard_name','sea_surface_wave_significant_height'))            
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'units','m'))
   num_components = 1
   varid(1) = NC_VarID_hs
case('swan_TMM10.63') !TMM10
   CALL Check(NF90_DEF_VAR(NC_ID,'swan_TMM10',NF90_DOUBLE,NC_DimID,NC_VarID_tmm10))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'long_name','Mean Period'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'standard_name','sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment'))                
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'units','s'))
   num_components = 1
   varid(1) = NC_VarID_tmm10
case('swan_TPS.63') !TPS
   CALL Check(NF90_DEF_VAR(NC_ID,'swan_TPS',NF90_DOUBLE,NC_DimID,NC_VarID_tps))         
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'long_name','Peak Period'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'standard_name','maximum_sea_surface_wave_period_at_variance_spectral_density_maximum'))            
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'units','s'))
   num_components = 1
   varid(1) = NC_VarID_tps
case('coefdiagonal.63') 
   CALL Check(NF90_DEF_VAR(NC_ID,'coefdiagonal',NF90_DOUBLE,NC_DimID,NC_VarID_coefdiagonal))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefdiagonal,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefdiagonal,'long_name','adcirc fully consistent left hand side matrix diagonal coefficients'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefdiagonal,'standard_name','adcirc_fully_consistent_lhs_diagonal '))            
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefdiagonal,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefdiagonal,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefdiagonal,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefdiagonal,'units','unitless'))
   num_components = 1
   varid(1) = NC_VarID_coefdiagonal
case('nodecode.63') 
   netCDFDataType = NF90_INT
   call check(NF90_DEF_VAR(NC_ID,'nodecode',netCDFDataType,NC_DimID,NC_VarID_nodecode))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodecode,'_FillValue',-99999))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodecode,'long_name','node wet or dry'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodecode,'standard_name','node_wet_or_dry'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodecode,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodecode,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodecode,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodecode,'units','unitless'))
   num_components = 1
   varid(1) = NC_VarID_nodecode
case('noff.100') 
   netCDFDataType = NF90_INT
   call check(NF90_DEF_VAR(NC_ID,'noff',netCDFDataType,(/ NC_DimID_nele, NC_DimID_Time /),NC_VarID_noff))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_noff,'_FillValue',-99999))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_noff,'long_name','element wet or dry'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_noff,'standard_name','element_wet_or_dry'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_noff,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_noff,'location','element'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_noff,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_noff,'units','unitless'))
   num_components = 1
   dataCenter = 'Element'
   varid(1) = NC_VarID_noff
case('dryelementareacheck.100') 
   netCDFDataType = NF90_INT
   call check(NF90_DEF_VAR(NC_ID,'dryelementareacheck',netCDFDataType,NC_DimID_nele,NC_VarID_dryelementareacheck))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dryelementareacheck,'_FillValue',-99999))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dryelementareacheck,'long_name','dry element area check'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dryelementareacheck,'standard_name','dry_element_area_check'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dryelementareacheck,'coordinates','y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dryelementareacheck,'location','element'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dryelementareacheck,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dryelementareacheck,'units','unitless'))
   num_components = 1
   dataCenter = 'Element'
   varid(1) = NC_VarID_dryelementareacheck
case('coefele.100') 
   call check(NF90_DEF_VAR(NC_ID,'coefele',netCDFDataType,(/ NC_DimID_nele, NC_DimID_Time /),NC_VarID_coefele))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefele,'_FillValue',-99999.d0))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefele,'long_name','element contribution to mass matrix diagonal'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefele,'standard_name','element_coef'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefele,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefele,'location','element'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefele,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_coefele,'units','unitless'))
   num_components = 1
   dataCenter = 'Element'
   varid(1) = NC_VarID_coefele
case('nneighele.63') 
   netCDFDataType = NF90_INT
   call check(NF90_DEF_VAR(NC_ID,'nneighele',netCDFDataType,NC_DimID,NC_VarID_nneighele))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nneighele,'_FillValue',-99999))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nneighele,'long_name','number of element neighbors for each node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nneighele,'standard_name','num_element_neighbors'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nneighele,'coordinates','y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nneighele,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nneighele,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nneighele,'units','unitless'))
   num_components = 1
   varid(1) = NC_VarID_nneighele
case('nodeids.63') 
   netCDFDataType = NF90_INT
   call check(NF90_DEF_VAR(NC_ID,'nodeids',netCDFDataType,NC_DimID,NC_VarID_nodeids))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodeids,'_FillValue',-99999))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodeids,'long_name','fortran indexed node ids'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodeids,'standard_name','fortran_indexed_node_ids'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodeids,'coordinates','y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodeids,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodeids,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nodeids,'units','unitless'))
   num_components = 1
   varid(1) = NC_VarID_nodeids
case('elementids.100') 
   netCDFDataType = NF90_INT
   call check(NF90_DEF_VAR(NC_ID,'elementids',netCDFDataType,NC_DimID_nele,NC_VarID_elementids))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_elementids,'_FillValue',-99999))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_elementids,'long_name','fortran indexed element ids'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_elementids,'standard_name','fortran_indexed_element_ids'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_elementids,'coordinates','y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_elementids,'location','element'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_elementids,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_elementids,'units','unitless'))
   num_components = 1
   dataCenter = 'Element'
   varid(1) = NC_VarID_elementids
case('null','none') ! just the mesh
   ! do nothing, data set meta data not required
   meshonly = .true.
case('maxwvel.63') ! maxwvel
   CALL Check(NF90_DEF_VAR(NC_ID,'wind_max',netCDFDataType,NC_DimID_node,NC_VarID_maxwvel))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'long_name','maximum wind speed at sea level'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'standard_name','maximum_wind_speed_at_sea_level'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'units','m s-1'))
   varid(1) = NC_VarID_maxwvel
   if ( num_components.eq.2) then
      CALL Check(NF90_DEF_VAR(NC_ID,'time_of_wind_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_timeOfMaxwvel))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxwvel,'_FillValue',FillValue))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxwvel,'long_name','time of maximum wind speed at sea level'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxwvel,'standard_name','time_of_maximum_wind_speed_at_sea_level'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxwvel,'coordinates','y x'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxwvel,'location','node'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxwvel,'mesh','adcirc_mesh'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxwvel,'units','s'))
   endif
   varid(2) = NC_VarID_timeOfmaxwvel
case('maxvel.63') ! max water current velocity 
   CALL Check(NF90_DEF_VAR(NC_ID,'vel_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_maxvel))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxvel,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxvel,'long_name','maximum water column vertically averaged velocity'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxvel,'standard_name','maximum_water column_vertically_averaged_velocity'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxvel,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxvel,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxvel,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxvel,'units','m s-1'))
   varid(1) = NC_VarID_maxvel
   if ( num_components.eq.2) then
      CALL Check(NF90_DEF_VAR(NC_ID,'time_of_vel_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_timeOfMaxvel))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxvel,'_FillValue',FillValue))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxvel,'long_name','time of maximum water column vertically averaged velocity'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxvel,'standard_name','time_of_maximum_water_column_vertically_averaged_velocity'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxvel,'coordinates','y x'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxvel,'location','node'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxvel,'mesh','adcirc_mesh'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfmaxvel,'units','s'))
   endif
   varid(2) = NC_VarID_timeOfmaxvel
case('swan_HS_max.63') ! swan_HS_max
   CALL Check(NF90_DEF_VAR(NC_ID,'swan_HS_max',NF90_DOUBLE,NC_DimID,NC_VarID_swanhsmax))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'long_name','maximum significant wave height'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'standard_name', & 
       'maximum_sea_surface_wave_significant_height'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'units','m'))
   varid(1) = NC_VarID_swanhsmax
   if ( num_components.eq.2) then
      CALL Check(NF90_DEF_VAR(NC_ID,'time_of_swan_HS_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_timeOfswanhsmax))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswanhsmax,'_FillValue',FillValue))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswanhsmax,'long_name','time of maximum significant wave height'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswanhsmax,'standard_name', & 
          'time_of_maximum_sea_surface_wave_significant_height'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswanhsmax,'coordinates','y x'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswanhsmax,'location','node'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswanhsmax,'mesh','adcirc_mesh'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswanhsmax,'units','s'))
   endif
   varid(2) = NC_VarID_timeOfswanhsmax
case('swan_TPS_max.63') ! swan_TPS_max
   CALL Check(NF90_DEF_VAR(NC_ID,'swan_TPS_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_swantpsmax))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'long_name','maximum smoothed peak period'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'standard_name', &
      'maximum_sea_surface_wave_period_at_variance_spectral_density_maximum'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'units','s'))
   varid(1) = NC_VarID_swantpsmax 
   if ( num_components.eq.2) then
      CALL Check(NF90_DEF_VAR(NC_ID,'swan_TPS_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_timeOfswantpsmax))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswantpsmax,'_FillValue',FillValue))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswantpsmax,'long_name','time of maximum smoothed peak period'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswantpsmax,'standard_name', &
         'time_of_maximum_sea_surface_wave_period_at_variance_spectral_density_maximum'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswantpsmax,'coordinates','y x'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswantpsmax,'location','node'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswantpsmax,'mesh','adcirc_mesh'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_timeOfswantpsmax,'units','s'))            
   endif
   varid(2) = NC_VarID_timeOfswantpsmax 
case('ESLNodes.63') ! ESLNodes.63
   CALL Check(NF90_DEF_VAR(NC_ID,'ESLNodes',NF90_DOUBLE,NC_DimID,NC_VarID_eslnodes))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eslnodes,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eslnodes,'long_name','elemental slope limiter active nodes'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eslnodes,'standard_name', &
      'elemental_slope_limiter_active_nodes'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eslnodes,'coordinates','time y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eslnodes,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eslnodes,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_eslnodes,'units','1'))
   num_components = 1
   varid(1) = NC_VarID_eslnodes                        

case('fort.88') 
   CALL Check(NF90_DEF_VAR(NC_ID,'initial_river_elevation',NF90_DOUBLE,NC_DimID_node,NC_VarID_maxele))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'_FillValue',FillValue))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'long_name','initial river elevation'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'standard_name','initial_river_elevation'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'coordinates','y x'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'location','node'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'mesh','adcirc_mesh'))
   CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'units','m'))
   varid(1) = NC_VarID_maxele

case default
   write(6,'(a)') 'ERROR: Unable to convert '//trim(dataFileType)//' files.'

end select
#ifdef NETCDF_CAN_DEFLATE
      if (convertedFileFormat.eq.NETCDF4) then
         do j=1,num_components
            call check(nf90_def_var_deflate(NC_ID, varid(j), 1, 1, 2))
         enddo
      endif
#endif

!----------------------------------------------------------------
! end variable and attributes definitions
!----------------------------------------------------------------
call check(nf90_enddef(nc_id))

! place mesh-related data into the file, unless this is a data 
! only file
if ( (dataonly.eqv..false.).and.(griddedData.eqv..false.) ) then
   call writeMeshDataToNetCDF(NC_ID)
endif
!
! finish up if only mesh data are to be converted
if (meshonly.eqv..true.) then
   call Check(NF90_CLOSE(NC_ID))
   write(6,'(a)') 'INFO: Only mesh data were written.'
   stop
endif
!
! write grid coordinates if appropriate
if (griddedData.eqv..true.) then

  ! <write grid coordinates>

endif


UnitNumber = 20
write(6,'(a,a,a)') 'INFO: adcirc2netcdf.f90: Opening data file "',trim(dataFile),'".'
call openFileForRead(UnitNumber, trim(dataFile))

select case(trim(dataFileType))
case('fort.221','fort.222','fort.223','fort.224')
   numValuesPerDataset = iLonOWI * iLatOWI 
case('fort.88') 
   numValuesPerDataset = np
   tInterval = -99999.d0
   Interval = -99999 
   nCol = 1
case default
   ! fort.63 etc
   READ(UnitNumber,'(A)',end=246,err=248,iostat=errorio) JunkC
   lineNum=lineNum+1
   ! jgf: Can't rely on the NumSnaps value; in general, it will not
   ! actually reflect the number of datasets in the file.
   READ(UnitNumber,*,end=246,err=248,iostat=errorio) NumSnaps, numValuesPerDataset, tInterval, Interval, nCol
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
end select
!
! Allocate space to hold the data
select case(netCDFDataType)
case(NF90_DOUBLE)
   if (griddedData.eqv..true.) then
      ! y before x according to netcdf specification in fortran api
      allocate(owi1(1:iLatOWI,1:iLonOWI))
      if (num_components.eq.2) then
         ! y before x according to netcdf specification in fortran api
         allocate(owi2(1:iLatOWI,1:iLonOWI))
      endif
   else
      ALLOCATE(Global1(1:numValuesPerDataset))
      ALLOCATE(Global2(1:numValuesPerDataset))
   endif
case(NF90_INT)
   allocate(idata(1:numValuesPerDataset))      
case default
   write(6,'(a,i0)') 'ERROR: Unsupported data type: ',netCDFDataType
end select
!
! Read ascii data and write to netcdf file
SS=1        ! initialize the dataset counter
lineNum = 1 ! initialize the line number counter
!
! gridded data
if (griddedData.eqv..true.) then
   do   ! loop until we run out of gridded data
      owi1(:,:) = fillValue
      owi2(:,:) = fillValue
      errorVar = "first component"
      read(lun,22,end=321,err=9999,iostat=errorIO) ((owi1(j,i),i=1,iLonOWI),j=1,iLatOWI)
      call checkErrOWI(errorIO,errorVar,dataFileType)
      if (num_components.eq.2) then
         errorVar = "second component"
         read(lun,22,end=123,err=9999,iostat=errorIO) ((owi2(j,i),i=1,iLonOWI),j=1,iLatOWI)
         call checkErrOWI(errorIO,errorVar,dataFileType)
      endif
      call check(nf90_put_var(nc_id,nc_varid_time,(/snapr/),(/ss/),(/1/)))
      NC_Count_OWI = (/ iLatOWI, iLonOWI, 1 /)
      NC_Start_OWI = (/ 1, 1, SS /)
      timeOfNC_Start = (/ 1, 1 /)
      ! write the dataset to the netcdf file
      select case(trim(dataFileType))
         case('fort.63') !63
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_zeta,Global1,NC_Start_OWI,NC_Count_OWI))

         case default
      
      end select
22    format(8f10.0)

   end do
9999  call checkErrOWI(1,errorVar,dataFileType) ! ERR during read jumps to here
321 continue  ! jgf: jump here when no data left in gridded ascii file
   write(6,'(/,a,i0,a)') 'INFO: Wrote ',ss-1,' dataset(s).'
   close(lun)
   call check(nf90_close(nc_id))
   write(6,'(a)') 'INFO: adcirc2netcdf.x: Finished writing gridded data to netcdf.'
   STOP !
endif
!
! mesh data
DO   ! jgf: loop until we run out of mesh data
   if (trim(dataFileType).ne.'fort.88') then
      read(UnitNumber,'(A)',END=123,ERR=123) Line
      lineNum = lineNum + 1
      read(Line,*,end=246,err=248,iostat=errorio) SnapR, SnapI
      read(Line,*,ERR=907,END=907) SnapR, SnapI, NumNodesNonDefault, DefaultValue
      goto 908  ! jgf: this file is sparse ascii
   endif
907     NumNodesNonDefault = numValuesPerDataset
   DefaultValue = -99999.0d0
908     if (netCDFDataType.eq.NF90_DOUBLE) then
      DO N=1,numValuesPerDataset
         Global1(N)=DefaultValue
         Global2(N)=DefaultValue
      ENDDO
   endif
   j=0
   do N=1,NumNodesNonDefault
     select case(trim(dataRank))
       case("Scalar")                    ! scalar data
         if (netCDFDataType.eq.NF90_DOUBLE) then
            if (trim(dataFileType).eq.'fort.88') then
               READ(UnitNumber,*,end=246,err=248,iostat=errorio) Temp1
               lineNum = lineNum + 1
               j = j + 1
            else                  
               READ(UnitNumber,*,end=246,err=248,iostat=errorio) j,Temp1
               lineNum = lineNum + 1
            endif
            Global1(j) = Temp1
         else
            READ(UnitNumber,*,end=246,err=248,iostat=errorio) j,idata(n)
            lineNum = lineNum + 1
         endif
       case("2DVector")                  ! 2D vector data
         READ(UnitNumber,*,end=246,err=248,iostat=errorio) j,Temp1,Temp2
         lineNum = lineNum + 1
         Global1(j) = Temp1
         Global2(j) = Temp2
      case default
         write(6,'(a,a,a)') 'ERROR: adcirc2netcdf: ',trim(dataRank),' data rank is not supported.'
         stop
     end select
   enddo
   
   CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_time,(/SnapR/),(/SS/),(/1/)))
   NC_Count = (/ numValuesPerDataset, 1 /)
   NC_Start = (/ 1, SS /)
   !
   ncStartMinMax = (/ 1 /)
   ncCountMinMax = (/ SS /)
   ! write the dataset to the netcdf file
   select case(trim(dataFileType))
      case('fort.63') !63
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_zeta,Global1,NC_Start,NC_Count))
      case('fort.90') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_tau0,Global1,NC_Start,NC_Count))
      case('eta1.63') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_eta1,Global1,NC_Start,NC_Count))
      case('eta2.63') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_eta2,Global1,NC_Start,NC_Count))
      case('tk.63') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_tk,Global1,NC_Start,NC_Count))
      case('offset.63') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_offset,Global1,NC_Start,NC_Count))         
      case('uu1vv1.64') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_uu1_vel,Global1,NC_Start,NC_Count))
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_vv1_vel,Global2,NC_Start,NC_Count))
      case('fort.64') !64
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_u_vel,Global1,NC_Start,NC_Count))
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_v_vel,Global2,NC_Start,NC_Count))
      case('fort.73') !73
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_P,Global1,NC_Start,NC_Count))
      case('fort.74') !74
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_windx,Global1,NC_Start,NC_Count))
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_windy,Global2,NC_Start,NC_Count))
      case('nodecode.63') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nodecode,idata,NC_Start,NC_Count))
      case('noff.100') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_noff,idata,NC_Start,NC_Count))
      case('dryelementareacheck.100') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_dryelementareacheck,idata,NC_Start,NC_Count))
      case('coefele.100') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_coefele,Global1,NC_Start,NC_Count))
      case('coefdiagonal.63') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_coefdiagonal,Global1,NC_Start,NC_Count))
      case('nneighele.63') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nneighele,idata,NC_Start,NC_Count))
      case('nodeids.63') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nodeids,idata,NC_Start,NC_Count))
      case('elementids.100') 
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_elementids,idata,NC_Start,NC_Count))           
      case('maxele.63') !MAXELE
         select case(ss)
         case(1)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_maxele,Global1,ncStartMinMax,ncCountMinMax))
         case(2)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_timeOfmaxele,Global1,ncStartMinMax,ncCountMinMax))
         end select
      case('minpr.63') ! minimum barometric pressure at sea level
         select case(ss)
         case(1)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_minpr,Global1,NC_Start,NC_Count))
         case(2)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_timeOfminpr,Global1,timeOfNC_Start,NC_Count))
         end select 
      case('fort.88') ! initial river elevation
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_maxele,Global1,NC_Start,NC_Count))
      case('swan_DIR.63') !DIR
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_dir,Global1,NC_Start,NC_Count))
      case('swan_HS.63') !HS
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_hs,Global1,NC_Start,NC_Count))
      case('swan_TMM10.63') !TMM10
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_tmm10,Global1,NC_Start,NC_Count))
      case('swan_TPS.63') !TPS
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_tps,Global1,NC_Start,NC_Count))
      case('maxwvel.63') ! maxwvel
         select case(ss)
         case(1)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_maxwvel,Global1,NC_Start,NC_Count))
         case(2)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_timeOfmaxwvel,Global1,timeOfNC_Start,NC_Count))               
         end select
      case('swan_HS_max.63') ! swan_HS_max
         select case(ss)
         case(1)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_swanhsmax,Global1,NC_Start,NC_Count))
         case(2)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_timeOfswanhsmax,Global1,timeOfNC_Start,NC_Count))
         end select
      case('swan_TPS_max.63') ! swan_TPS_max
         select case(ss)
         case(1)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_swantpsmax,Global1,NC_Start,NC_Count))
         case(2)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_timeOfswantpsmax,Global1,timeOfNC_Start,NC_Count))
         end select
      case('ESLNodes.63') ! ESLNodes
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_eslnodes,Global1,NC_Start,NC_Count))
      case default
         write(6,'(a,a,a)') 'ERROR: Unable to write netcdf files for ',trim(dataFileType),'.'
         stop
   end select
   write(6,advance='no',fmt='(i6)') ss
   SS = SS + 1 ! jgf: Increment the dataset counter
   !
   if (trim(dataFileType).eq.'fort.88') then
      exit
   endif
ENDDO

123   CONTINUE  ! jgf: When we've run out of datasets in the current file,
                ! we jump to here.
write(6,'(/,a,i0,a)') 'INFO: Wrote ',ss-1,' dataset(s).'

CLOSE(UnitNumber)

CALL Check(NF90_CLOSE(NC_ID))
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
character(len=2048), intent(in) :: dataFileType

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

end subroutine getmonthday

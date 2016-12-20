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
character(2048)               :: dataFileBase
character(2048)               :: dataFileType
integer :: convertedFileFormat ! netcdf3 or netcdf4
character(2048)               :: netCDFFile, AttFile
character(120)                :: dataRank
character(120),   allocatable :: att(:,:)
character(1000)               :: Line
character(1)                  :: JunkC, Tadj
real(8)                        :: temp1, temp2
real(8), ALLOCATABLE         :: Global1(:), Global2(:)
integer, allocatable         :: idata(:)
integer                       :: numValuesPerDataset
integer                       :: yy, mo, dd, hh, mi
integer                       :: i, j, k, N, SS
integer                       :: unitnumber
logical                       :: meshonly   ! .true. if user just wants to convert the mesh
logical                       :: dataonly   ! .true. if user just wants to convert the data
logical                       :: timeVarying ! .true. for time varying data
integer                       :: ncFileType
INTEGER                       :: NC_DimID(2) = (/ -99, -99 /)
integer                       :: NC_DimID_single = -99
integer                       :: NC_VarID_zeta = -99
integer                       :: NC_VarID_tau0 = -99
integer                       :: NC_VarID_eta1 = -99
integer                       :: NC_VarID_eta2 = -99
integer                       :: NC_VarID_tk = -99
integer                       :: NC_VarID_uu1_vel = -99
integer                       :: NC_VarID_vv1_vel = -99
integer                       :: NC_VarID_nodecode = -99
integer                       :: NC_VarID_noff = -99
integer                       :: NC_VarID_dryelementareacheck = -99
integer                       :: NC_VarID_coefdiagonal = -99
integer                       :: NC_VarID_coefele = -99                   
integer                       :: NC_VarID_nneighele = -99
integer                       :: NC_VarID_nodeids = -99
integer                       :: NC_VarID_elementids = -99         
integer                       :: NC_VarID_u_vel = -99
integer                       :: NC_VarID_v_vel = -99
integer                       :: NC_VarID_maxele = -99
integer                       :: NC_VarID_timeOfmaxele = -99
integer                       :: NC_VarID_minpr = -99
integer                       :: NC_VarID_timeOfminpr = -99
integer                       :: NC_VarID_maxwvel = -99
integer                       :: NC_VarID_timeOfmaxwvel = -99
integer                       :: NC_VarID_maxvel = -99
integer                       :: NC_VarID_timeOfmaxvel = -99
integer                       :: NC_VarID_p = -99
integer                       :: NC_VarID_windx = -99
integer                       :: NC_VarID_windy = -99
integer                       :: NC_VarID_dir = -99
integer                       :: NC_VarID_hs = -99
integer                       :: NC_VarID_tmm10 = -99
integer                       :: NC_VarID_tps = -99
integer                       :: NC_VarID_swantpsmax = -99
integer                       :: NC_VarID_timeOfswantpsmax = -99
integer                       :: NC_VarID_swanhsmax = -99
integer                       :: NC_VarID_timeOfswanhsmax = -99
integer                       :: NC_VarID_eslnodes = -99
integer, dimension(2)        :: timeOfNC_Start
integer, parameter            :: version = 4
integer                       :: varid(3) ! varids for netcdf4 compression
integer                       :: lastSlashPosition ! used for trimming full path from a filename
integer                       :: lastDotPosition ! to determine file extension
character(2048)               :: dataFileExtension ! something like 13, 14, 15, 63, 222 etc
integer                       :: iret !jgfdebug
integer                       :: lineNum
! initializations
meshFileName = "null"
attFile = "null"
dataFile = "null"
dataFileType = "null"
dataFileBase = "null"
dataRank = "Scalar"
convertedFileFormat = NETCDF4
meshonly = .false.
dataonly = .false.
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
! Load netCDF Attributes
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
!
! create netcdf file
write(6,'(a,a,a)') "INFO: Creating NetCDF file '"//trim(netCDFFile)//"'."
ncFileType = NF90_CLOBBER ! netcdf3 format, netcdf classic model

#ifdef HAVE_NETCDF4
if (convertedFileFormat.eq.NETCDF4) then
   ncFileType = ior(NF90_HDF5,NF90_CLASSIC_MODEL) ! netcdf4 (i.e., hdf5) format, netcdf classic model
endif
#endif

!
CALL Check(NF90_CREATE(TRIM(netCDFFile),ncFileType,NC_ID))
! create global attributes 
do i = 1,natt
  CALL Check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,att(1,i),att(2,i)))
enddo
!
! write the mesh definitions to the netcdf file unless the 
! dataonly command line option was specified
if ( (meshonly.eqv..false.).and.(trim(dataFileExtension).ne.'88').and.(trim(dataFileExtension).ne.'13')) then
   write(6,'(a)') 'INFO: Checking number of nodes in data file.' 
   call openFileForRead(20, trim(dataFile))
   read(20,'(a)',end=246,err=248,iostat=errorio) JunkC
   lineNum=lineNum+1
   read(20,*,end=246,err=248,iostat=errorio) numSnaps, numValuesPerDataset, tInterval, Interval, nCol
   lineNum=lineNum+1
   close(20)
   lineNum=1
endif
if (dataonly.eqv..false.) then
   call read14()
   call writeMeshDefinitionsToNetCDF(NC_ID, convertedFileFormat)
else       
   np = numValuesPerDataset
   call check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,'description',trim(JunkC)))
   call check(NF90_DEF_DIM(NC_ID,'node',np,NC_DimID_node))
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
CALL Check(NF90_DEF_DIM(NC_ID,'time',NF90_UNLIMITED,NC_DimID_time))
CALL Check(NF90_DEF_VAR(NC_ID,'time',NF90_DOUBLE,NC_DimID_time,NC_VarID_time))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'long_name','model time'))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'standard_name','time'))
CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'units',datenum))
!
! Determine if the dataFile is time varying or not. 
!write(6,*) 'DEBUG: dataFileBase: ',trim(dataFileBase)
select case(trim(dataFileType))
case('maxele.63','maxvel.63','maxwvel.63','maxrs.63','minpr.63','swan_HS_max.63','swan_TPS_max.63')
   timeVarying = .false.
   num_components = numSnaps
case default
   timeVarying = .true.
end select
!write(6,'(a,i0)') 'DEBUG: num_components=',num_components
!      
! create adcirc output variables and associated attributes
NC_DimID = (/ NC_DimID_node, NC_DimID_Time /)
select case(trim(dataFileType))
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
if (dataonly.eqv..false.) then
   call writeMeshDataToNetCDF(NC_ID)
endif
!
!
if (meshonly.eqv..true.) then
   call Check(NF90_CLOSE(NC_ID))
   write(6,'(a)') 'INFO: Only mesh data were written.'
   stop
endif

UnitNumber = 20
call openFileForRead(UnitNumber, trim(dataFile))
if (trim(dataFileType).ne.'fort.88') then
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
else
   numValuesPerDataset = np
   tInterval = -99999.d0
   Interval = -99999 
   nCol = 1
endif

select case(netCDFDataType)
case(NF90_DOUBLE)
   ALLOCATE(Global1(1:numValuesPerDataset))
   ALLOCATE(Global2(1:numValuesPerDataset))
case(NF90_INT)
   allocate(idata(1:numValuesPerDataset))      
case default
   write(6,'(a)') 'ERROR: Unsupported data type.'
end select
    
SS=1 ! jgf: initialize the dataset counter
lineNum = 1 ! initialize the line number counter
DO   ! jgf: loop until we run out of data
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
   timeOfNC_Start = (/ 1, 1 /)
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
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_maxele,Global1,NC_Start,NC_Count))
         case(2)
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_timeOfmaxele,Global1,timeOfNC_Start,NC_Count))
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
   write(6,advance='no',fmt='(i4)') ss
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

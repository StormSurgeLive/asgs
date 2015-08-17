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
! Copyright(C) 2012--2015 Jason Fleming
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
      use adcircdata
      IMPLICIT NONE
      CHARACTER(2048)               :: dataFileBase
      character(2048)               :: netCDFFile, AttFile
      character(120)                :: dataRank
      character(120),   allocatable :: att(:,:)
      character(1000)               :: Line
      character(1)                  :: JunkC, Tadj
      real(8)                        :: temp1, temp2
      real(8), ALLOCATABLE         :: Global1(:), Global2(:), Global3(:)
      integer                       :: yy, mo, dd, hh, mi
      integer                       :: i, j, k, N, SS
      integer                       :: unitnumber
      logical                       :: useNetCDF4 ! .true. if user wants netcdf classic model
                                                  ! files formatted in hdf5 format
      logical                       :: meshonly   ! .true. if user just wants to convert the mesh
      logical                       :: dataonly   ! .true. if user just wants to convert the data
      logical                       :: timeVarying ! .true. for time varying data
      integer                       :: ncFileType
      INTEGER                       :: NC_DimID(2)
      integer                       :: NC_DimID_single
      integer                       :: NC_VarID_zeta
      integer                       :: NC_VarID_u_vel
      integer                       :: NC_VarID_v_vel
      integer                       :: NC_VarID_maxele
      integer                       :: NC_VarID_timeOfmaxele
      integer                       :: NC_VarID_maxwvel
      integer                       :: NC_VarID_timeOfmaxwvel
      integer                       :: NC_VarID_maxvel
      integer                       :: NC_VarID_timeOfmaxvel
      integer                       :: NC_VarID_p
      integer                       :: NC_VarID_windx
      integer                       :: NC_VarID_windy
      integer                       :: NC_VarID_dir
      integer                       :: NC_VarID_hs
      integer                       :: NC_VarID_tmm10
      integer                       :: NC_VarID_tps
      integer                       :: NC_VarID_swantpsmax
      integer                       :: NC_VarID_timeOfswantpsmax
      integer                       :: NC_VarID_swanhsmax
      integer                       :: NC_VarID_timeOfswanhsmax
      integer                       :: NC_VarID_eslnodes
      integer, dimension(2)        :: timeOfNC_Start
      integer, parameter            :: version = 4
      integer                       :: varid(3) ! varids for netcdf4 compression
      integer                       :: lastSlashPosition ! used for trimming full path from a filename
      ! initializations
      meshFileName = "null"
      attFile = "null"
      dataFile = "null"
      dataFileBase = "null"
      dataRank = "Scalar"
      useNetCDF4 = .false.
      meshonly = .false.
      dataonly = .false.
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
                  useNetCDF4 = .true.
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
                  write(6,'(a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  attFile = trim(cmdlinearg)
               case("--datafile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  dataFile = trim(cmdlinearg)
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
      !
      ! Load netCDF Attributes
      call openFileForRead(100,AttFile)
      read(100,*) natt
      allocate(att(1:2,1:natt))
      read(100,'(A)') datenum !seconds since 2008-07-31 12:00:00 +00:00
      do i = 1,natt
        read(100,*) att(1,i), att(2,i)
      enddo
      close(100)
      write(6,'(a)') "INFO: Finished reading metadata/attributes file."
      !
      ! create netcdf file
      write(6,'(a,a,a)') "INFO: Creating NetCDF file '"//trim(netCDFFile)//"'."
      ncFileType = NF90_CLOBBER ! netcdf3 format, netcdf classic model
#ifdef HAVE_NETCDF4
      if (useNetCDF4.eqv..true.) then
         ncFileType = ior(NF90_HDF5,NF90_CLASSIC_MODEL) ! netcdf4 (i.e., hdf5) format, netcdf classic model
      endif
#endif
      !
      !
      CALL Check(NF90_CREATE(TRIM(netCDFFile),ncFileType,NC_ID))
      ! create time dimension and create global attributes
      CALL Check(NF90_DEF_DIM(NC_ID,'time',NF90_UNLIMITED,NC_DimID_time))
      CALL Check(NF90_DEF_VAR(NC_ID,'time',NF90_DOUBLE,NC_DimID_time,NC_VarID_time))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'long_name','model time'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'standard_name','time'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'units',datenum))
      do i = 1,natt
        CALL Check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,att(1,i),att(2,i)))
      enddo
      !
      ! write the mesh definitions to the netcdf file unless the 
      ! dataonly command line option was specified
      write(6,'(a)') 'INFO: Checking number of nodes in data file.' 
      call openFileForRead(20, trim(dataFile))
      read(20,'(a)') JunkC
      read(20,*) numSnaps, NumNodes, tInterval, Interval, nCol
      close(20)
      if (dataonly.eqv..false.) then
         call read14()
         call writeMeshDefinitionsToNetCDF(NC_ID, useNetCDF4)
      else       
         np = NumNodes
         call check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,'description',trim(JunkC)))
         call check(NF90_DEF_DIM(NC_ID,'node',np,NC_DimID_node))
      endif
      !
      ! Determine if the dataFile is time varying or not. 
      !write(6,*) 'DEBUG: dataFileBase: ',trim(dataFileBase)
      select case(trim(dataFileBase))
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

      select case(trim(dataFileBase))
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
      case('fort.64') !64
         dataRank = "2DVector"
         CALL Check(NF90_DEF_VAR(NC_ID,'u-vel',NF90_DOUBLE,NC_DimID,NC_VarID_u_vel))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'_FillValue',FillValue))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'long_name','water column vertically averaged east/west velocity'))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'standard_name','eastward_water_velocity'))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'coordinates','time y x'))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'location','node'))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_Vel,'mesh','adcirc_mesh'))
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
      case('null') ! just the mesh
         ! do nothing, data set meta data not required
      case('maxwvel.63') ! maxwvel
         CALL Check(NF90_DEF_VAR(NC_ID,'wind_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_maxwvel))
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
      end select
#ifdef NETCDF_CAN_DEFLATE
      if (useNetCDF4.eqv..true.) then
         do j=1,num_components
            call check(nf90_def_var_deflate(NC_ID, varid(j), 1, 1, 2))
         enddo
      endif
#endif

      !----------------------------------------------------------------
      ! end variable and attributes definitions
      !----------------------------------------------------------------
      CALL Check(NF90_ENDDEF(NC_ID))

      ! place mesh-related data into the file, unless this is a data 
      ! only file
      if (dataonly.eqv..false.) then
         call writeMeshDataToNetCDF(NC_ID)
      endif
      !
      !
      if (meshonly.eqv..true.) then
         call Check(NF90_CLOSE(NC_ID))
         write(6,'(a)') "INFO: The --meshonly option was specified; only mesh data were written."
         stop
      endif

      UnitNumber = 20
      call openFileForRead(UnitNumber, trim(dataFile))
      READ(UnitNumber,'(A)') JunkC
      ! jgf: Can't rely on the NumSnaps value; in general, it will not
      ! actually reflect the number of datasets in the file.
      READ(UnitNumber,*) NumSnaps, NumNodes, tInterval, Interval, nCol

      if (np.ne.NumNodes) then
         write(6,'(a,i0,a,i0,a)') 'ERROR: The output file contains ',NumNodes,        &
           ' nodes, but the mesh file contains ',np,' nodes.'
          write(6,'(a)') 'ERROR: The output file does not correspond to the mesh file.'
         close(UnitNumber)
         stop
      endif
      ALLOCATE(Global1(1:NumNodes))
      ALLOCATE(Global2(1:NumNodes))
      ALLOCATE(Global3(1:1))
      SS=1 ! jgf: initialize the dataset counter
      DO   ! jgf: loop until we run out of data
         read(UnitNumber,'(A)',END=123,ERR=123) Line
         read(Line,*) SnapR, SnapI
         read(Line,*,ERR=907,END=907) SnapR, SnapI, NumNodesNonDefault, DefaultValue
         goto 908  ! jgf: this file is sparse ascii
 907     NumNodesNonDefault = NumNodes
         DefaultValue = -99999.0d0
 908     DO N=1,NumNodes
            Global1(N)=DefaultValue
            Global2(N)=DefaultValue
         ENDDO
         do N=1,NumNodesNonDefault
           select case(trim(dataRank))
             case("Scalar")                    ! scalar data
               READ(UnitNumber,*) j,Temp1
               Global1(j) = Temp1
             case("2DVector")                  ! 2D vector data
               READ(UnitNumber,*) j,Temp1,Temp2
               Global1(j) = Temp1
               Global2(j) = Temp2
            case default
               write(6,'(a,a,a)') 'ERROR: adcirc2netcdf: ',trim(dataRank),' data rank is not supported.'
               stop
           end select
         enddo
         Global3(1) = SnapR
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_time,Global3,(/SS/),(/1/)))
         NC_Count = (/ NumNodes, 1 /)
         NC_Start = (/ 1, SS /)
         timeOfNC_Start = (/ 1, 1 /)
         ! write the dataset to the netcdf file
         select case(trim(dataFileBase))
            case('fort.63') !63
               CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_zeta,Global1,NC_Start,NC_Count))
            case('fort.64') !64
               CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_u_vel,Global1,NC_Start,NC_Count))
               CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_v_vel,Global2,NC_Start,NC_Count))
            case('fort.73') !73
               CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_P,Global1,NC_Start,NC_Count))
            case('fort.74') !74
               CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_windx,Global1,NC_Start,NC_Count))
               CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_windy,Global2,NC_Start,NC_Count))
            case('maxele.63') !MAXELE
               select case(ss)
               case(1)
                  CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_maxele,Global1,NC_Start,NC_Count))
               case(2)
                  CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_timeOfmaxele,Global1,timeOfNC_Start,NC_Count))
               end select
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
         end select
         SS = SS + 1 ! jgf: Increment the dataset counter
      ENDDO

123   CONTINUE  ! jgf: When we've run out of datasets in the current file,
                ! we jump to here.
      CLOSE(UnitNumber)
      DEALLOCATE(Global1,Global2,Global3)

      CALL Check(NF90_CLOSE(NC_ID))
      write(6,'(a)') 'INFO: adcirc2netcdf finished.'
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

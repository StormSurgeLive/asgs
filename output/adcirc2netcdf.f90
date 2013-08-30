! This program will convert ADCIRC+SWAN ASCII Output to NETCDF
! Copyleft by Patrick C. Kerr
! University of Notre Dame
! pkerr@nd.edu

! 2012-01-12:   v1 - Original
! 2012-01-16:   v2 - Fixed Time Bug, Made CF Compliant
! 2012-01-17:   v3 - Added maxele
! 2012-02-27:   v4 ...
!
! Example of compiling adcirc2netcdf.f90 with g95:
! g95 -o adcirc2netcdf.x -cpp -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -ffree-form -ffree-line-length-huge -I/usr/local/netcdf/netcdf-4.1.1/f90 adcirc2netcdf.f90 -L/usr/local/hdf5/hdf5-1.8.8/hdf5/lib  -lnetcdf -lhdf5_hl -lhdf5 -lhdf5_fortran -lz
!
! Example of compiling adcirc2netcdf.f90 with ifort on blueridge 20130516:
! ifort -o adcirc2netcdf.x -i-dynamic -cpp -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/projects/ncfs/apps/netcdf/netcdf-fortran-4.2/include adcirc2netcdf.f90 -L/projects/ncfs/apps/netcdf/netcdf-fortran-4.2/lib  -lnetcdf -lnetcdff -lz
!
! Example of compiling adcirc2netcdf.f90 with gfortran:
! gfortran -o adcirc2netcdf.x -cpp -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -ffree-form -ffree-line-length-none -I. -I/usr/include -L/usr/lib adcirc2netcdf.f90 -lnetcdf -lnetcdff -lz
!
! Example of compiling adcirc2netcdf.f90 with gfortran with debugging support:
! gfortran -g -O0 -fbacktrace -DDEBUG -o adcirc2netcdf.x -cpp -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -ffree-form -ffree-line-length-none -I. -I/usr/include -L/usr/lib adcirc2netcdf.f90 -lnetcdf -lnetcdff -lz
!
! Example of compiling adcirc2netcdf.f90 with pgf90:
! pgf90 -o adcirc2netcdf.x -Mpreprocess -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/opt/cray/netcdf/4.1.3/pgi/109/include adcirc2netcdf.f90 -L/opt/cray/netcdf/4.1.3/pgi/109/lib  -lnetcdf -lnetcdff
!
! Example using ifort on Diamond at ERDC 20130726:
! ifort -cpp -o adcirc2netcdf.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -i-dynamic -I/usr/local/usp/PETtools/CE/pkgs/netcdf-4.2.1.1-intel-serial/include -L/usr/local/usp/PETtools/CE/pkgs/netcdf-4.2.1.1-intel-serial/lib adcirc2netcdf.f90 -lnetcdf -lnetcdff -lz
!
! Example of usage with command line options:
! ~/asgs/trunk/output/adcirc2netcdf.x --netcdf4 --meshonly --meshfile fort.14 --attfile sl15_att.txt


include 'adcmesh.f90'

!-----+---------+---------+---------+---------+---------+---------+
!
!   P R O G R A M    A D C I R C  2  N E T C D F
!
!-----+---------+---------+---------+---------+---------+---------+
   program adcirc2netcdf

      USE netcdf
      USE adcmesh
      IMPLICIT NONE
      CHARACTER(120)                :: InputFile, OutputFile, AttFile
      character(120)                :: datenum
      character(120),   allocatable :: att(:,:)
      character(1000)               :: Line
      character(1)                  :: JunkC, Tadj
      DOUBLE PRECISION              :: DefaultValue, FillValue=-99999.0d0
      double precision              :: temp1, temp2, SnapR, Interval, time(1)
      DOUBLE PRECISION, ALLOCATABLE :: Global1(:), Global2(:), Global3(:)
      integer                       :: yy, mo, dd, hh, mi
      integer                       :: natt, i, j, k, N, SS, NumNodes, NumSnaps
      integer                       :: NumNodesNonDefault, SnapI
      integer                       :: unitnumber, nCol
      integer, allocatable          :: iopt(:)         ! files to convert
      integer                       :: nopt         ! actual number of files to convert
      integer                       :: menuOpt   ! user's selection
      integer                       :: argcount  ! number of command line arguments
      integer                       :: iargc     ! function to return command line arguments
      character(2048)               :: cmdlineopt ! command line option
      character(2048)               :: cmdlinearg ! content of command line argument
      logical                       :: useNetCDF4 ! .true. if user wants netcdf classic model
                                                  ! files formatted in hdf5 format
      logical                       :: meshonly   ! .true. if user just wants to convert the mesh
      logical                       :: dataonly   ! .true. if user just wants to convert the data
      integer                       :: ncFileType
      integer                       :: NC_ID
      INTEGER                       :: NC_DimID(2)
      INTEGER                       :: NC_Count(2)
      INTEGER                       :: NC_Start(2)

      integer                       :: NC_DimID_time
      integer                       :: NC_DimID_single

      integer                       :: NC_VarID_time
      integer                       :: NC_VarID_zeta
      integer                       :: NC_VarID_u_vel
      integer                       :: NC_VarID_v_vel
      integer                       :: NC_VarID_maxele
      integer                       :: NC_VarID_maxwvel
      integer                       :: NC_VarID_p
      integer                       :: NC_VarID_windx
      integer                       :: NC_VarID_windy
      integer                       :: NC_VarID_dir
      integer                       :: NC_VarID_hs
      integer                       :: NC_VarID_tmm10
      integer                       :: NC_VarID_tps
      integer                       :: NC_VarID_swantpsmax
      integer                       :: NC_VarID_swanhsmax
      integer, parameter            :: version = 4
      integer                       :: num_components ! variable components for netcdf4 compression
      integer                       :: varid(3) ! varids for netcdf4 compression
      integer                       :: lastSlashPosition ! used for trimming full path from a filename
      ! initializations
      deg2rad = 2.0d0*pi/360.d0
      meshFileName = "null"
      attFile = "null"
      menuOpt = 0
      useNetCDF4 = .false.
      meshonly = .false.
      dataonly = .false.
      !
      !write(6,*) "INFO: adcirc2netcdf version ",version,"."
      ! Report netcdf version
      write(6,*) "INFO: adcirc2netcdf was compiled with the following netcdf library: ", &
         trim(nf90_inq_libvers())

      ! jgf: Process command line options; can be used along with menu choices;
      ! if command line options provide all needed input, menu will not
      ! be presented to user; programs with command line options are
      ! slightly easier to automate than menu-based programs
      argcount = iargc() ! count up command line options
      if (argcount.gt.0) then
         i=0
         do while (i.lt.argcount)
            i = i + 1
            call getarg(i, cmdlineopt)
            select case(trim(cmdlineopt))
               case("--netcdf4")
                  useNetCDF4 = .true.
                  write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
               case("--meshonly")
                  meshonly = .true.
                  menuOpt = 14
                  write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
               case("--dataonly")
                  dataonly = .true.
                  write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
               case("--meshfile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  meshFileName = trim(cmdlinearg)
               case("--attfile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  attFile = trim(cmdlinearg)
               case("--datafile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  select case(trim(cmdlinearg))
                     case("fort.63")
                        menuOpt = 1
                     case("fort.64")
                        menuOpt = 2
                     case("fort.73")
                        menuOpt = 3
                     case("fort.74")
                        menuOpt = 4
                     case("maxele.63")
                        menuOpt = 5
                     case("swan_DIR.63")
                        menuOpt = 6
                     case("swan_HS.63")
                        menuOpt = 7
                     case("swan_TMM10.63")
                        menuOpt = 8
                     case("swan_TPS.63")
                        menuOpt = 9
                     case("adcirc")
                        menuOpt = 10
                     case("swan")
                        menuOpt = 11
                     case("adcirc_swan")
                        menuOpt = 12
                     case("fort.14")
                        menuOpt = 14
                     case("maxwvel.63")
                        menuOpt = 15
                     case("swan_HS_max.63")
                        menuOpt = 16
                     case("swan_TPS_max.63")
                        menuOpt = 17
                     case default
                        write(6,*) "WARNING: Command line argument '",TRIM(cmdlinearg),"' was not recognized."
                  end select
                  InputFile = trim(cmdlinearg)
               case default
                  write(6,*) "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
            end select
         end do
      end if
      if (meshonly.eqv..true.) then
         menuOpt = 14
      endif
      ! present file conversion menu to user if the file was not specified on the
      ! command line
  997 continue
      if (menuOpt.eq.0) then
         write(6,*)    '*************************************************'
         write(6,*)    '    Select ADCIRC Output to Convert to NETCDF    '
         write(6,*)    '*************************************************'
         write(6,*)    '  1. fort.63       '
         write(6,*)    '  2. fort.64       '
         write(6,*)    '  3. fort.73       '
         write(6,*)    '  4. fort.74       '
         write(6,*)    '  5. maxele.63     '
         write(6,*)    '  6. swan_DIR.63   '
         write(6,*)    '  7. swan_HS.63    '
         write(6,*)    '  8. swan_TMM10.63 '
         write(6,*)    '  9. swan_TPS.63   '
         write(6,*)    '  10. Options 1-5   '
         write(6,*)    '  11. Options 6-9  '
         write(6,*)    '  12. Options 1-9  '
         write(6,*)    '*************************************************'
         write(6,'(A)',ADVANCE="NO")    ' Select Option: '
         read(5,'(i10)',ERR=997) menuOpt
      endif
      !
      ! jgf: make a list of files to convert, based on use menu selection;
      ! set names of files that will contain more than one type of data
      select case(menuOpt)
         case(1,2,3,4,5,6,7,8,9,14,15,16,17)
            nopt = 1       ! only need to convert 1 file
            allocate(iopt(nopt))
            iopt(1) = menuOpt ! file to convert has been selected from the menu
         case(10)
            nopt = 5
            allocate(iopt(nopt))
            iopt = (/ 1, 2, 3, 4, 5 /)
            Outputfile = 'adcirc.nc'
         case(11)
            nopt = 4
            allocate(iopt(nopt))
            iopt = (/ 6, 7, 8, 9 /)
            Outputfile = 'swan.nc'
         case(12)
            nopt = 9
            allocate(iopt(nopt))
            iopt = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
            Outputfile = 'adcirc_swan.nc'
         case default
            write(6,*) 'ERROR: Your selection was invalid. Please try again.'
            goto 997
      end select
      ! trim off the full path so we just have the file name
      lastSlashPosition = index(trim(InputFile),"/",.true.) 
      ! now set NETCDF file name for files containing only one type of data
      select case(menuOpt)
         case(1) !63
            Outputfile = 'fort.63.nc'
         case(2) !64
            Outputfile = 'fort.64.nc'
         case(3) !73
            Outputfile = 'fort.73.nc'
         case(4) !74
            Outputfile = 'fort.74.nc'
         case(5) !MAXELE
            Outputfile = 'maxele.63.nc'
         case(6) !DIR
            Outputfile = 'swan_DIR.63.nc'
         case(7) !HS
            Outputfile = 'swan_HS.63.nc'
         case(8) !TMM10
            Outputfile = 'swan_TMM10.63.nc'
         case(9) !TPS
            Outputfile = 'swan_TPS.63.nc'
         case(14)
            ! trim off the full path so we just have the file name
            lastSlashPosition = index(trim(meshFileName),"/",.true.)
            Outputfile = trim(meshFileName(lastSlashPosition+1:))//'.nc'
            write(6,*) "DEBUG: The name of the output file is ",Outputfile
         case(15) ! mesh
            Outputfile = trim(InputFile(lastSlashPosition+1:))//'.nc'
         case(16) ! swan_HS_max.63
            Outputfile = trim(InputFile(lastSlashPosition+1:))//'.nc'
         case(17) ! swan_TPS_max.63
            Outputfile = trim(InputFile(lastSlashPosition+1:))//'.nc'
         case default
            ! 10, 11, and 12 were assigned previously
      end select

      ! Load Global Attributes
      write(6,*)    '*************************************************'
      if (trim(attFile).eq."null") then
         WRITE(6,'(A)',ADVANCE='NO') "Enter name of attribute file: "
         READ(5,'(A)') AttFile
      endif
      call openFileForRead(100,AttFile)
      read(100,*) natt
      allocate(att(1:2,1:natt))
      read(100,'(A)') datenum !seconds since 2008-07-31 12:00:00 +00:00
      do i = 1,natt
        read(100,*) att(1,i), att(2,i)
      enddo
      close(100)
      write(6,*) "INFO: Finished reading metadata/attributes file."

      ! create netcdf file
      write(6,*) "INFO: Creating NetCDF file '"//trim(OutputFile)//"'."
      ncFileType = NF90_CLOBBER ! netcdf3 format, netcdf classic model
#ifdef HAVE_NETCDF4
      if (useNetCDF4.eqv..true.) then
         ncFileType = ior(NF90_HDF5,NF90_CLASSIC_MODEL) ! netcdf4 (i.e., hdf5) format, netcdf classic model
      endif
#endif
      CALL Check(NF90_CREATE(TRIM(OutputFile),ncFileType,NC_ID))
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
      if (dataonly.eqv..false.) then
         call read14()
         call writeMeshDefinitionsToNetCDF(NC_ID, useNetCDF4)
      else       
         write(6,*) "INFO: Checking number of nodes in data file" 
         call openFileForRead(20, trim(InputFile))
         read(20,'(A)') JunkC
         read(20,*) NumSnaps, NumNodes, Interval, Interval, nCol
         close(20)
         np = NumNodes
         call check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,'description',trim(JunkC)))
         call check(NF90_DEF_DIM(NC_ID,'node',np,NC_DimID_node))
      endif
      !      
      ! create adcirc output variables and associated attributes
      NC_DimID = (/ NC_DimID_node, NC_DimID_Time /)
      do i=1,nopt
         select case(iopt(i))
         case(1) !63
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
         case(2) !64
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
         case(3) !73
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
         case(4) !74
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
         case(5) !MAXELE
            CALL Check(NF90_DEF_VAR(NC_ID,'zeta_max',NF90_DOUBLE,NC_DimID_node,NC_VarID_maxele))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'_FillValue',FillValue))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'long_name','maximum sea surface elevation above datum'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'standard_name','maximum_sea_surface_elevation_above_datum'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'coordinates','y x'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'location','node'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'mesh','adcirc_mesh'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'units','m'))
            num_components = 1
            varid(1) = NC_VarID_maxele
         case(6) !DIR
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
         case(7) !HS
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
         case(8) !TMM10
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
         case(9) !TPS
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
         case(14) ! just the mesh
            cycle
         case(15) ! maxwvel
            CALL Check(NF90_DEF_VAR(NC_ID,'wind_max',NF90_DOUBLE,NC_DimID,NC_VarID_maxwvel))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'_FillValue',FillValue))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'long_name','maximum wind speed at sea level'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'standard_name','maximum_wind_speed_at_sea_level'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'coordinates','time y x'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'location','node'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'mesh','adcirc_mesh'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxwvel,'units','m s-1'))
            num_components = 1
            varid(1) = NC_VarID_maxwvel
         case(16) ! swan_HS_max
            CALL Check(NF90_DEF_VAR(NC_ID,'swan_HS_max',NF90_DOUBLE,NC_DimID,NC_VarID_swanhsmax))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'_FillValue',FillValue))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'long_name','maximum significant wave height'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'standard_name', & 
                'maximum_sea_surface_wave_significant_height'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'coordinates','time y x'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'location','node'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'mesh','adcirc_mesh'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swanhsmax,'units','m'))
            num_components = 1
            varid(1) = NC_VarID_swanhsmax
         case(17) ! swan_TPS_max
            CALL Check(NF90_DEF_VAR(NC_ID,'swan_TPS_max',NF90_DOUBLE,NC_DimID,NC_VarID_swantpsmax))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'_FillValue',FillValue))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'long_name','maximum smoothed peak period'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'standard_name', &
               'maximum_sea_surface_wave_period_at_variance_spectral_density_maximum'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'coordinates','time y x'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'location','node'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'mesh','adcirc_mesh'))
            CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_swantpsmax,'units','s'))
            num_components = 1
            varid(1) = NC_VarID_swantpsmax          
         end select
#ifdef NETCDF_CAN_DEFLATE
         if (useNetCDF4.eqv..true.) then
            do j=1,num_components
               call check(nf90_def_var_deflate(NC_ID, varid(j), 1, 1, 2))
            enddo
         endif
#endif
      enddo
      !----------------------------------------------------------------
      ! end variable and attributes definitions
      !----------------------------------------------------------------
      CALL Check(NF90_ENDDEF(NC_ID))

      ! place mesh-related data into the file, unless this is a data 
      ! only file
      if (dataonly.eqv..false.) then
         call writeMeshDataToNetCDF(NC_ID)
      endif
      
      if (meshonly.eqv..true.) then
         call Check(NF90_CLOSE(NC_ID))
         write(6,*) "INFO: The --meshonly option was specified; only mesh data were written."
         stop
      endif
      !
      ! now moving on to the actual adcirc output data; first, set name
      ! of ascii output file to read, based on menu choice
      do i=1,nopt
         select case(iopt(i))
            case(1) !63
               Inputfile = 'fort.63'
            case(2) !64
               Inputfile = 'fort.64'
            case(3) !73
               Inputfile = 'fort.73'
            case(4) !74
               Inputfile = 'fort.74'
            case(5) !MAXELE
               Inputfile = 'maxele.63'
            case(6) !DIR
               Inputfile = 'swan_DIR.63'
            case(7) !HS
               Inputfile = 'swan_HS.63'
            case(8) !TMM10
               Inputfile = 'swan_TMM10.63'
            case(9) !TPS
               Inputfile = 'swan_TPS.63'
            case(14) ! just the mesh
               exit  ! jump out of this loop completely
            case(15) ! maxwvel
               Inputfile = 'maxwvel.63'
            case(16) ! swan_HS_max
               Inputfile = 'swan_HS_max.63'
            case(17) ! swan_TPS_max
               Inputfile = 'swan_TPS_max.63'
         end select
         UnitNumber = 100+iopt(i)
         call openFileForRead(UnitNumber, trim(InputFile))
         READ(UnitNumber,'(A)') JunkC
         ! jgf: Can't rely on the NumSnaps value; in general, it will not
         ! actually reflect the number of datasets in the file.
         READ(UnitNumber,*) NumSnaps, NumNodes, Interval, Interval, nCol
         if (np.ne.NumNodes) then
           write(6,*) 'ERROR: The output file contains ',NumNodes,        &
             ' nodes, but the mesh file contains ',np,' nodes.'
           write(6,*) 'ERROR: The output file does not correspond to the mesh file.'
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
   907      NumNodesNonDefault = NumNodes
            DefaultValue = -99999.0d0
   908      DO N=1,NumNodes
               Global1(N)=DefaultValue
               Global2(N)=DefaultValue
            ENDDO
            do N=1,NumNodesNonDefault
              select case(iopt(i))
                case(1,3,5,6,7,8,9,15,16,17) ! scalar data
                  READ(UnitNumber,*) j,Temp1
                  Global1(j) = Temp1
                case(2,4)           ! 2D vector data
                  READ(UnitNumber,*) j,Temp1,Temp2
                  Global1(j) = Temp1
                  Global2(j) = Temp2
              end select
            enddo
            Global3(1) = SnapR
            CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_time,Global3,(/SS/),(/1/)))
            NC_Count = (/ NumNodes, 1 /)
            NC_Start = (/ 1, SS /)
            ! write the dataset to the netcdf file
            select case(iopt(i))
              case(1) !63
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_zeta,Global1,NC_Start,NC_Count))
              case(2) !64
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_u_vel,Global1,NC_Start,NC_Count))
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_v_vel,Global2,NC_Start,NC_Count))
              case(3) !73
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_P,Global1,NC_Start,NC_Count))
              case(4) !74
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_windx,Global1,NC_Start,NC_Count))
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_windy,Global2,NC_Start,NC_Count))
              case(5) !MAXELE
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_maxele,Global1,NC_Start,NC_Count))
              case(6) !DIR
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_dir,Global1,NC_Start,NC_Count))
              case(7) !HS
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_hs,Global1,NC_Start,NC_Count))
              case(8) !TMM10
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_tmm10,Global1,NC_Start,NC_Count))
              case(9) !TPS
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_tps,Global1,NC_Start,NC_Count))
              case(15) ! maxwvel
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_maxwvel,Global1,NC_Start,NC_Count))
              case(16) ! swan_HS_max
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_swanhsmax,Global1,NC_Start,NC_Count))
              case(17) ! swan_TPS_max
                CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_swantpsmax,Global1,NC_Start,NC_Count))
            end select
            SS = SS + 1 ! jgf: Increment the dataset counter
         ENDDO

   123   CONTINUE  ! jgf: When we've run out of datasets in the current file,
                   ! we jump to here.
         CLOSE(UnitNumber)
         DEALLOCATE(Global1,Global2,Global3)
      END DO

      CALL Check(NF90_CLOSE(NC_ID))
      write(6,*) 'INFO: adcirc2netcdf finished.'
!----------------------------------------------------------------------
   end program adcirc2netcdf
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!                  S U B R O U T I N E   
!     W R I T E   M E S H   D E F I N I T I O N S  T O   N E T C D F 
!----------------------------------------------------------------------
!     This subroutine writes the mesh parameters to the netcdf file. 
!----------------------------------------------------------------------
      subroutine writeMeshDefinitionsToNetCDF(nc_id, useNetCDF4)
      use netcdf
      use adcmesh
      implicit none
      integer, intent(in) :: nc_id
      logical, intent(in) :: useNetCDF4
      integer              :: NC_DimID_single
      !
      ! create and store mesh dimensions 
      CALL Check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,'agrid',trim(agrid)))
      CALL Check(NF90_DEF_DIM(NC_ID,'node',np,NC_DimID_node))
      CALL Check(NF90_DEF_DIM(NC_ID,'nele',ne,NC_DimID_nele))
      CALL Check(NF90_DEF_DIM(NC_ID,'nvertex',3,NC_DimID_nvertex))
      CALL Check(NF90_DEF_DIM(NC_ID,'single',1,NC_DimID_single))

      if (nope.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'nope',nope,NC_DimID_nope))
      if (nvdl_max.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'max_nvdll',nvdl_max,NC_DimID_max_nvdll))
      if (neta.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'neta',neta,NC_DimID_neta))
      if (nbou.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'nbou',nbou,NC_DimID_nbou))
      if (nvel.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'nvel',nvel,NC_DimID_nvel))
      if (nvel_max.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'max_nvell',nvel_max,NC_DimID_max_nvell))
   
      ! ibtypee, ibconn, bars are ignored
      CALL Check(NF90_DEF_VAR(NC_ID,'x',NF90_DOUBLE,NC_DimID_node,NC_VarID_x))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_x,'long_name','longitude'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_x,'standard_name','longitude'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_x,'units','degrees_east'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_x,'positive','east'))

      CALL Check(NF90_DEF_VAR(NC_ID,'y',NF90_DOUBLE,NC_DimID_node,NC_VarID_y))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_y,'long_name','latitude'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_y,'standard_name','latitude'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_y,'units','degrees_north'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_y,'positive','north'))

      CALL Check(NF90_DEF_VAR(NC_ID,'element',NF90_int,(/NC_DimID_nvertex, NC_DimID_nele /),NC_VarID_element))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_element,'long_name','element'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_element,'standard_name','face_node_connectivity'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_element,'units','nondimensional'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_element,'start_index',1))

      if (nope.ne.0) then
         CALL Check(NF90_DEF_VAR(NC_ID,'nvdll',NF90_DOUBLE,NC_DimID_nope,NC_VarID_nvdll))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nvdll,'long_name','total number of nodes in each elevation specified & boundary segment'))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nvdll,'units','nondimensional'))

         CALL Check(NF90_DEF_VAR(NC_ID,'max_nvdll',NF90_int,NC_DimID_single,NC_VarID_max_nvdll))
         CALL Check(NF90_DEF_VAR(NC_ID,'max_nvell',NF90_int,NC_DimID_single,NC_VarID_max_nvell))      
         CALL Check(NF90_DEF_VAR(NC_ID,'neta',NF90_int,NC_DimID_single,NC_VarID_neta))
         CALL Check(NF90_DEF_VAR(NC_ID,'nope',NF90_int,NC_DimID_single,NC_VarID_nope))
         CALL Check(NF90_DEF_VAR(NC_ID,'nvel',NF90_int,NC_DimID_single,NC_VarID_nvel))

         CALL Check(NF90_DEF_VAR(NC_ID,'nbdv',NF90_DOUBLE,(/ NC_DimID_nope, NC_DimID_max_nvdll /),NC_VarID_nbdv))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nbdv,'long_name','node numbers on each elevation specified boundary & segment'))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nbdv,'units','nondimensional'))
      endif

      if (nbou.ne.0) then
         CALL Check(NF90_DEF_VAR(NC_ID,'nvell',NF90_DOUBLE,NC_DimID_nbou,NC_VarID_nvell))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nvell,'long_name','number of nodes in each normal flow specified boundary segment'))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nvell,'units','nondimensional'))

         CALL Check(NF90_DEF_VAR(NC_ID,'ibtype',NF90_DOUBLE,NC_DimID_nbou,NC_VarID_ibtype))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_ibtype,'long_name','type of normal flow (discharge) boundary'))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_ibtype,'units','nondimensional'))

         CALL Check(NF90_DEF_VAR(NC_ID,'nbvv',NF90_DOUBLE,(/ NC_DimID_nbou, NC_DimID_max_nvell /),NC_VarID_nbvv))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nbvv,'long_name','node numbers on normal flow boundary segment'))
         CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_nbvv,'units','nondimensional'))
      endif

      CALL Check(NF90_DEF_VAR(NC_ID,'depth',NF90_DOUBLE,NC_DimID_node,NC_VarID_depth))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_depth,'long_name','distance from geoid'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_depth,'standard_name','depth_below_geoid'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_depth,'coordinates','time y x'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_depth,'location','node'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_depth,'mesh','adcirc_mesh'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_depth,'units','m'))
!      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_depth,'positive','down')) !DO NOT USE?

      CALL Check(NF90_DEF_VAR(NC_ID,'adcirc_mesh',NF90_INT,NC_DimID_single,NC_VarID_mesh))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_mesh,'long_name','mesh topology'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_mesh,'standard_name','mesh_topology'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_mesh,'dimension',2))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_mesh,'node_coordinates','x y'))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_mesh,'face_node_connectivity','element'))
#ifdef NETCDF_CAN_DEFLATE
      if (useNetCDF4.eqv..true.) then
         if (nope.ne.0) then
            call check(nf90_def_var_deflate(NC_ID, NC_VarID_nvdll, 0, 1, 2))
            call check(nf90_def_var_deflate(NC_ID, NC_VarID_nbdv, 0, 1, 2))
         endif
         if (nbou.ne.0) then
            call check(nf90_def_var_deflate(NC_ID, NC_VarID_nvell, 0, 1, 2))
            call check(nf90_def_var_deflate(NC_ID, NC_VarID_ibtype, 0, 1, 2))
            call check(nf90_def_var_deflate(NC_ID, NC_VarID_nbvv, 0, 1, 2))
         endif
         call check(nf90_def_var_deflate(NC_ID, NC_VarID_x, 0, 1, 2))
         call check(nf90_def_var_deflate(NC_ID, NC_VarID_y, 0, 1, 2))
         call check(nf90_def_var_deflate(NC_ID, NC_VarID_element, 0, 1, 2))
         call check(nf90_def_var_deflate(NC_ID, NC_VarID_depth, 0, 1, 2))
         call check(nf90_def_var_deflate(NC_ID, NC_VarID_depth, 0, 1, 2))
         call check(nf90_def_var_deflate(NC_ID, NC_VarID_Mesh, 0, 1, 2))
      endif
#endif
!----------------------------------------------------------------------
      end subroutine writeMeshDefinitionsToNetCDF
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!                    S U B R O U T I N E   
!         W R I T E   M E S H   D A T A   T O   N E T C D F 
!----------------------------------------------------------------------
!     This subroutine writes the mesh parameters to the netcdf file. 
!----------------------------------------------------------------------
      subroutine writeMeshDataToNetCDF(nc_id)
      use netcdf
      use adcmesh
      implicit none
      integer, intent(in) :: nc_id
      INTEGER                       :: NC_Count(2)
      INTEGER                       :: NC_Start(2)
     
      ! place mesh-related data into the file
      NC_Count = (/ np, 1 /)
      NC_Start = (/ 1, 1 /)
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_x,xyd(1,1:np),NC_Start,NC_Count))
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_y,xyd(2,1:np),NC_Start,NC_Count))
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_depth,xyd(3,1:np),NC_Start,NC_Count))
      NC_Count = (/ 3, ne /)
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_element,nm,NC_Start,NC_Count))

      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_max_nvell,nvel_max))
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_max_nvdll,nvdl_max))
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nope,nope))
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_neta,neta))      
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nvel,nvel))            
      
      if (nope.ne.0) then
         NC_Count = (/ nope, 1 /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nvdll,nvdll,NC_Start,NC_Count))
         NC_Count = (/ nope, nvdl_max /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nbdv,nbdv,NC_Start,NC_Count))
      endif
      if (nbou.ne.0) then
         NC_Count = (/ nbou, 1 /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nvell,nvell,NC_Start,NC_Count))
         NC_Count = (/ nbou, 1 /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_ibtype,ibtype,NC_Start,NC_Count))
         NC_Count = (/ nbou, nvel_max /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nbvv,nbvv,NC_Start,NC_Count))
      end if

      write(6,*) 'INFO: Mesh has been written to NETCDF'
!----------------------------------------------------------------------
      end subroutine writeMeshDataToNetCDF
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

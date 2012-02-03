! This program will convert ADCIRC+SWAN ASCII Output to NETCDF
! Copyleft by Patrick C. Kerr
! University of Notre Dame
! pkerr@nd.edu

! 2012-01-12:   v1 - Original
! 2012-01-16:   v2 - Fixed Time Bug, Made CF Compliant
! 2012-01-17:   v3 - Added maxele

   program ToNETCDF      

      USE netcdf
      IMPLICIT NONE
      CHARACTER(120)                :: InputFile, OutputFile, fort14, AttFile
      character(120)                :: agrid, datenum
      character(120),   allocatable :: att(:,:)
      character(1000)               :: Line
      character(1)                  :: JunkC, Tadj
      DOUBLE PRECISION              :: DefaultValue, FillValue=-99999.0d0
      double precision              :: temp1, temp2, SnapR, Interval, time(1)
      DOUBLE PRECISION, ALLOCATABLE :: Global1(:), Global2(:), Global3(:)
      double precision, allocatable :: xyd(:,:), bar(:,:,:)
      integer                       :: yy, mo, dd, hh, mi
      integer                       :: ne, np
      integer                       :: nope, neta, nvdl_max
      integer                       :: nbou, nvel, nvel_max, nodemax            
      integer,          allocatable :: nm(:,:), nvdll(:), nbdv(:,:), nsequencer(:)      
      integer,          allocatable :: nvell(:), ibtype(:),  nbvv(:,:), ibconn(:,:)  
      integer                       :: natt, i, j, k, N, SS, NumNodes, NumSnaps
      integer                       :: NumNodesNonDefault, SnapI    
      integer                       :: iopt, iopt2, unitnumber, nCol

      integer                       :: NC_ID
      INTEGER                       :: NC_DimID(2)
      INTEGER                       :: NC_Count(2)
      INTEGER                       :: NC_Start(2)
      
      integer                       :: NC_DimID_time
      integer                       :: NC_DimID_node
      integer                       :: NC_DimID_nele
      integer                       :: NC_DimID_nvertex
      integer                       :: NC_DimID_nope
      integer                       :: NC_DimID_max_nvdll
      integer                       :: NC_DimID_nbou
      integer                       :: NC_DimID_neta      
      integer                       :: NC_DimID_nvel      
      integer                       :: NC_DimID_max_nvell
      integer                       :: NC_DimID_single

      integer                       :: NC_VarID_Mesh
      integer                       :: NC_VarID_time
      integer                       :: NC_VarID_x
      integer                       :: NC_VarID_y
      integer                       :: NC_VarID_element
      integer                       :: NC_VarID_neta
      integer                       :: NC_VarID_nvdll
      integer                       :: NC_VarID_max_nvdll
      integer                       :: NC_VarID_ibtypee
      integer                       :: NC_VarID_nbdv
      integer                       :: NC_VarID_nvel
      integer                       :: NC_VarID_nvell
      integer                       :: NC_VarID_max_nvell
      integer                       :: NC_VarID_ibtype
      integer                       :: NC_VarID_nbvv
      integer                       :: NC_VarID_depth
      integer                       :: NC_VarID_zeta
      integer                       :: NC_VarID_u_vel
      integer                       :: NC_VarID_v_vel
      integer                       :: NC_VarID_maxele      
      integer                       :: NC_VarID_p      
      integer                       :: NC_VarID_windx
      integer                       :: NC_VarID_windy
      integer                       :: NC_VarID_dir
      integer                       :: NC_VarID_hs
      integer                       :: NC_VarID_tmm10
      integer                       :: NC_VarID_tps

!Define File Conversion
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
997   write(6,'(A)',ADVANCE="NO")    ' Select Option: '
      read(5,'(i10)',ERR=997) iopt
      if ((iopt.gt.12).or.(iopt.lt.1)) goto 997

! Load fort.14      
      write(6,*)    '*************************************************'
      WRITE(6,'(A)',ADVANCE='NO') "Enter name of the fort.14 file: "
      read(5,'(A)') fort14
      write(6,*) 'START! READING of Global data:  ', fort14(1:len_trim(fort14))
      open(14,file=fort14,status='old',action='read')
         call read14_alloc ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax )
      close(14)
      allocate( xyd(3,np) )
      allocate( nm(3,ne) )
      allocate( nvdll(nope)  )
      allocate( nbdv(nope,nvdl_max) )
      allocate( nvell(nbou), ibtype(nbou)  )
      allocate( nbvv(nbou,nvel_max), ibconn(nbou,nvel_max), bar(3,nbou,nvel_max) )
      allocate( nsequencer(nodemax) )
      open(14,file=fort14,status='old',action='read')
         call read14 ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax, nsequencer,      &
                       agrid, xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, &
                       bar )
      close(14)
      write(6,*) 'Finished Reading'

! Create NETCDF File
      select case(iopt)
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
        case(10) !ALL ADCIRC
          Outputfile = 'adcirc.nc'          
        case(11) !ALL SWAN
          Outputfile = 'swan.nc'
        case(12) !ALL FILES
          Outputfile = 'adcirc_swan.nc'          
      end select      
      CALL Check(NF90_CREATE(TRIM(OutputFile),NF90_CLOBBER,NC_ID))


      CALL Check(NF90_DEF_DIM(NC_ID,'time',NF90_UNLIMITED,NC_DimID_time))
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
      CALL Check(NF90_DEF_VAR(NC_ID,'time',NF90_DOUBLE,NC_DimID_time,NC_VarID_time))
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'long_name','model time'))          
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'standard_name','time'))          

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

      NC_DimID = (/ NC_DimID_node, NC_DimID_Time /)

      iopt2 = iopt
      if (iopt2.eq.10) iopt2 = 1
      if (iopt2.eq.11) iopt2 = 6
      if (iopt2.eq.12) iopt2 = 1
993   select case(iopt2)
        case(1) !63
          CALL Check(NF90_DEF_VAR(NC_ID,'zeta',NF90_DOUBLE,NC_DimID,NC_VarID_zeta))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'_FillValue',FillValue))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'long_name','water surface elevation above geoid'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'standard_name','sea_surface_height_above_geoid'))                
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'mesh','adcirc_mesh'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'units','m'))       
          !          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'positive','up')) 'DO NOT USE'           
        case(2) !64
          CALL Check(NF90_DEF_VAR(NC_ID,'u-vel',NF90_DOUBLE,NC_DimID,NC_VarID_u_vel))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'_FillValue',FillValue))                    
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'long_name','water column vertically averaged east/west velocity'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'standard_name','barotropic_eastward_sea_water_velocity'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_Vel,'mesh','adcirc_mesh'))           
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'units','m s-1'))       
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'positive','east'))   
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_u_vel,'dry_Value',-99999.0d0))         
          CALL Check(NF90_DEF_VAR(NC_ID,'v-vel',NF90_DOUBLE,NC_DimID,NC_VarID_v_vel))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'_FillValue',FillValue))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'long_name','water column vertically averaged north/south velocity'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'standard_name','barotropic_northward_sea_water_velocity'))                
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'mesh','adcirc_mesh'))           
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'units','m s-1'))       
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'positive','north'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_v_vel,'dry_Value',-99999.0d0))                      
        case(3) !73
          CALL Check(NF90_DEF_VAR(NC_ID,'pressure',NF90_DOUBLE,NC_DimID,NC_VarID_p))                
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'_FillValue',FillValue))                    
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'long_name','air pressure at sea level'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'standard_name','air_pressure'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'mesh','adcirc_mesh'))           
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_p,'units','meters of water'))       
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
          CALL Check(NF90_DEF_VAR(NC_ID,'windy',NF90_DOUBLE,NC_DimID,NC_VarID_windy))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'_FillValue',FillValue))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'long_name','n/s wind velocity'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'standard_name','northward_wind'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'mesh','adcirc_mesh'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'units','m s-1'))       
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_windy,'positive','north')) 
        case(5) !MAXELE
          CALL Check(NF90_DEF_VAR(NC_ID,'maxele',NF90_DOUBLE,NC_DimID,NC_VarID_maxele))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'_FillValue',FillValue))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'long_name','maximum water surface elevation above geoid'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'standard_name','maximum_sea_surface_height_above_geoid'))                
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'mesh','adcirc_mesh'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_maxele,'units','m'))       
          !          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_zeta,'positive','up')) 'DO NOT USE'            
        case(6) !DIR 
          CALL Check(NF90_DEF_VAR(NC_ID,'dir',NF90_DOUBLE,NC_DimID,NC_VarID_dir))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'_FillValue',FillValue))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'long_name','wave direction'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'standard_name','wave_direction'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'mesh','adcirc_mesh'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_dir,'units','degrees_CW_from_East'))       
        case(7) !HS
          CALL Check(NF90_DEF_VAR(NC_ID,'hs',NF90_DOUBLE,NC_DimID,NC_VarID_hs))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'_FillValue',FillValue))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'long_name','significant wave height'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'standard_name','significant_wave_height'))                
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'mesh','adcirc_mesh'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_hs,'units','m'))                  
        case(8) !TMM10
          CALL Check(NF90_DEF_VAR(NC_ID,'tmm10',NF90_DOUBLE,NC_DimID,NC_VarID_tmm10))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'_FillValue',FillValue))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'long_name','Mean Period'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'standard_name','mean_period'))                
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'mesh','adcirc_mesh'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tmm10,'units','s'))            
        case(9) !TPS
          CALL Check(NF90_DEF_VAR(NC_ID,'tps',NF90_DOUBLE,NC_DimID,NC_VarID_tps))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'_FillValue',FillValue))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'long_name','Peak Period'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'standard_name','peak_period'))                
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'coordinates','time y x'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'location','node'))
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'mesh','adcirc_mesh'))          
          CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_tps,'units','s')) 
      end select

      if ((iopt.eq.10).and.(iopt2.ne.5)) then
        iopt2 = iopt2 + 1
        goto 993
      endif
      if ((iopt.eq.11).and.(iopt2.ne.9)) then
        iopt2 = iopt2 + 1
        goto 993
      endif
      if ((iopt.eq.12).and.(iopt2.ne.9)) then
        iopt2 = iopt2 + 1
        goto 993
      endif      

! Load & Write Global Attributes
      write(6,*)    '*************************************************'      
      WRITE(6,'(A)',ADVANCE='NO') "Enter name of attribute file: "
      READ(5,'(A)') AttFile
      OPEN(UNIT=100,FILE=TRIM(AttFile),Status='old',ACTION='READ')
      read(100,*) natt
      allocate(att(1:2,1:natt))
      read(100,'(A)') datenum !seconds since 2008-07-31 12:00:00 +00:00
      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_time,'units',datenum))        
      do i = 1,natt
        read(100,*) att(1,i), att(2,i)
        CALL Check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,att(1,i),att(2,i)))
      enddo
      CALL Check(NF90_ENDDEF(NC_ID))

 
      NC_Count = (/ np, 1 /)
      NC_Start = (/ 1, 1 /)
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_x,xyd(1,1:ne),NC_Start,NC_Count))
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_y,xyd(2,1:ne),NC_Start,NC_Count))      
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_depth,xyd(3,1:ne),NC_Start,NC_Count))            
      NC_Count = (/ 3, ne /)
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_element,nm,NC_Start,NC_Count))
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

      write(6,*)    '*************************************************'      
      write(6,*) 'Grid has been written to NETCDF'
      write(6,*)    '*************************************************'      
      write(6,*) 'Writing ...'
! Load Data & Write Output
     iopt2 = iopt
     if (iopt2.eq.10) iopt2 = 1
     if (iopt2.eq.11) iopt2 = 6
     if (iopt2.eq.12) iopt2 = 1     
995  select case(iopt2)
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
      end select

      write(6,*) Trim(Inputfile)

      UnitNumber = 100+iopt2
      OPEN(UNIT=UnitNumber,FILE=TRIM(InputFile),status='old',ACTION='READ')
      READ(UnitNumber,'(A)') JunkC
      READ(UnitNumber,*) NumSnaps, NumNodes, Interval, Interval, nCol
      if (np.ne.NumNodes) then
        write(6,*) 'Ouputfile does not correspond to grid file'
        stop
      endif
      ALLOCATE(Global1(1:NumNodes))
      ALLOCATE(Global2(1:NumNodes))
      ALLOCATE(Global3(1:1))

      DO SS=1,NumSnaps
         read(UnitNumber,'(A)') Line
         read(Line,*) SnapR, SnapI
         read(Line,*,ERR=907,END=907) SnapR, SnapI, NumNodesNonDefault, DefaultValue
         goto 908
907      NumNodesNonDefault = NumNodes
         DefaultValue = -99999.0d0
908      DO N=1,NumNodes
            Global1(N)=DefaultValue
            Global2(N)=DefaultValue
         ENDDO
         do N=1,NumNodesNonDefault
           select case(iopt2)
             case(1,3,5,6,7,8,9)
               READ(UnitNumber,*) j,Temp1               
               Global1(j) = Temp1
             case(2,4)
               READ(UnitNumber,*) j,Temp1,Temp2
               Global1(j) = Temp1
               Global2(j) = Temp2
           end select
         enddo
         Global3(1) = SnapR
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_time,Global3,(/SS/),(/1/)))
         NC_Count = (/ NumNodes, 1 /)
         NC_Start = (/ 1, SS /)         
         select case(iopt2)
           case(1) !63
             CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_zeta,Global1,NC_Start,NC_Count))
           case(2) !64
             CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_u_vel,Global1,NC_Start,NC_Count))
             CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_v_vel,Global1,NC_Start,NC_Count))             
           case(3) !73
             CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_P,Global1,NC_Start,NC_Count))
           case(4) !74
             CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_windx,Global1,NC_Start,NC_Count))
             CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_windy,Global1,NC_Start,NC_Count))             
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
         end select         
      ENDDO

      DEALLOCATE(Global1,Global2,Global3)

      if ((iopt.eq.10).and.(iopt2.ne.5)) then
        iopt2 = iopt2 + 1
        goto 995
      endif
      if ((iopt.eq.11).and.(iopt2.ne.9)) then
        iopt2 = iopt2 + 1
        goto 995
      endif
      if ((iopt.eq.12).and.(iopt2.ne.9)) then
        iopt2 = iopt2 + 1
        goto 995
      endif  

      CALL Check(NF90_CLOSE(NC_ID))

      write(6,*)    '*************************************************'      
      write(6,*) 'NETCDF Finished'
   end program
!----------------------------------------------------------------------
!  CHECK
!---------------------------------------------------------------------   
   SUBROUTINE Check(Status)
 !     USE DATA,ONLY: MyRank
      USE netcdf
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: Status
      IF(Status.NE.NF90_NOERR)THEN
         WRITE(*,'(A,A)') "FATAL ERROR from ",TRIM(NF90_STRERROR(Status))
         STOP
      ENDIF
   END SUBROUTINE check

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

!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!  READ14_ALLOC
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine read14_alloc ( iunit, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax )
      implicit none
      integer, intent(in) :: iunit
      integer, intent(out) :: ne, np, nope, nbou, nvdl_max, nvel_max
      integer, intent(out) :: nodemax ! for Sequencer of node
      integer :: i, j, k
!
      nvdl_max = 0
      nvel_max = 0
      nodemax = 0
         read(iunit,*)
         read(iunit,*) ne, np
         do k = 1, np
            read(iunit,*) i
            nodemax = max(i,nodemax)
         enddo
         do k = 1, ne
            read(iunit,*)
         enddo
         write(6,*) '  |'
         read(iunit,*) nope
         read(iunit,*) 
         do k = 1, nope
            read(iunit,*) i
            if( i >= nvdl_max ) nvdl_max = i
            do j = 1, i
               read(iunit,*)
            enddo
         enddo
         read(iunit,*) nbou
         read(iunit,*)
         do k = 1, nbou
            read(iunit,*) i
            if( i >= nvel_max ) nvel_max = i
            do j = 1, i
               read(iunit,*)
            enddo
         enddo
!
   end subroutine read14_alloc   

!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
! READ14
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine read14 ( iunit, ne, np, nope, nbou, nvdl_max, nvel_max,  nodemax, nsequencer,  &
                       agrid, xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, &
                       bar )
      implicit none
      integer, intent(in) :: iunit, nodemax
      integer, intent(inout) :: ne, np, nope, nbou, nvdl_max, nvel_max
      double precision, intent(out) :: xyd(3,np)
      integer, intent(out) :: nm(3,ne)
      character(120), intent(out) :: agrid
      integer, intent(out) :: neta, nvdll(nope), nbdv(nope,nvdl_max)
      integer, intent(out) :: nvel, nvell(nbou), nbvv(nbou,nvel_max), ibtype(nbou)
      integer, intent(out) :: ibconn(nbou,nvel_max)
      double precision, intent(out) :: bar(3,nbou,nvel_max)
!
      integer, intent(out) :: nsequencer(nodemax)
!
     integer :: i, j, k, jn, je, nhy
!
      nsequencer(:) = 0
      bar(:,:,:) = 0.0d0
      ibconn(:,:) = 0
!
        agrid = ' '
        read(iunit,*) 
         read(iunit,*) ne, np
         do k = 1, np
            read(iunit,*) jn, (xyd(j,k), j=1,3)
            nsequencer(jn) = k
         enddo
         write(6,*) '  + '
         do k = 1, ne
            read(iunit,*) je, nhy, ( nm(j,k), j = 1, 3 )
            do j = 1, 3
               if( nm(j,k) <= 0 ) write(6,*) k,j, nm(j,k)
               nm(j,k) = nsequencer(nm(j,k))
            enddo
         enddo
         read(iunit,*) nope
         read(iunit,*) neta
         do k = 1, nope
            read(iunit,*) nvdll(k)
            do j = 1, nvdll(k)
               read(iunit,*) nbdv(k,j)
               nbdv(k,j) = nsequencer(nbdv(k,j))
            enddo
         enddo
         read(iunit,*) nbou
         read(iunit,*) nvel
         do k = 1, nbou
            read(iunit,*) nvell(k), ibtype(k)
            select case(ibtype(k))
               case(0,1,2,10,11,12,20,21,22,30,52)
                  do j = 1, nvell(k)
                     read(iunit,*) nbvv(k,j)
                     nbvv(k,j) = nsequencer(nbvv(k,j))
                  enddo
               case(3, 13, 23)
                  do j = 1, nvell(k)
                     read(iunit,*) nbvv(k,j), (bar(i,k,j), i=1,2)
                     nbvv(k,j) = nsequencer(nbvv(k,j))
                  enddo
               case(4, 24)
                  do j = 1, nvell(k)
                     read(iunit,*) nbvv(k,j), ibconn(k,j), (bar(i,k,j), i=1,3)
                     nbvv(k,j) = nsequencer(nbvv(k,j))
                     ibconn(k,j) = nsequencer(ibconn(k,j))
                  enddo
               case default
                  write(6,*) ' IBTYPE is not allowed', ibtype(k)
                  write(6,*)
                  write(6,*) '**** Hit the Enter-Key to stop ****'
                  read(5,*)
                  stop
            end select
         enddo
!
!      deallocate( nsequencer )
!
   end subroutine read14

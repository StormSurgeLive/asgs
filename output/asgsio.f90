!--------------------------------------------------------------------------
! asgsio.f90
!
! A module that provides helper subroutines for opening and reading 
! ADCIRC files in ascii and netcdf format. 
!--------------------------------------------------------------------------
! Copyright(C) 2014--2016 Jason Fleming
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
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
module asgsio
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
use netcdf
implicit none
!
! This derived data type is used to map NetCDF4 variables in various
! ADCIRC files so that they can be represented in XDMF XML files. It
! is used in generateXDMF.f90.
type fileMetaData_t
   !
   ! state 
   logical :: initialized  ! .true. if memory has been allocated 
   !
   ! data characteristics
   logical :: timeVarying  ! .true. if we have datasets at different times
   logical :: useCPP  ! .true. if metadata should refer to CPP coordinates
   real(8), allocatable :: timesec(:)  ! time in seconds associated with each dataset
   integer :: num_components ! for ascii or netcdf files containing one data type
   real(8) :: time_increment  !  time (s) between datasets
   integer :: nspool         ! time steps between datasets
   integer, allocatable :: it(:) ! time step number associated with each dataset
   character(len=50), allocatable :: dataFileStationIDs(:) ! namelen from adcirc is 50
   logical :: isInteger     ! true for integer data
   logical :: griddedData   ! true if the data are defined on a regular grid
   !
   ! file characteristics
   character(len=2048) :: dataFileName ! full path
   character(len=20) :: dataFileType ! content: fort.13, fort.63, fort.67, maxele.63, noff.100 etc   
   character(len=1024) :: fileTypeDesc ! analyst-readable description
   integer :: fileFormat       ! ASCII, NETCDF4, XDMF etc parameters defined above
   integer :: nSnaps           ! number of datasets in the time varying file
   logical :: timeOfOccurrence ! .true. if min/max file has time of occurrence data
   integer :: fun ! file i/o unit number; only needed for ascii files
   logical :: isStationFile    ! .true. if the file represents adcirc station data
   integer :: nStations        ! number of stations in a station file
   real(8) :: defaultValue      ! missing data value for real data
   integer :: idefaultValue    ! missing data value for integer data
   logical :: isSparse           ! .true. if the file is sparse ascii
   integer :: numNodesNonDefault  ! for sparse ascii files
   integer :: numValuesPerDataset ! for ascii files, should equal np in associated mesh file
   !
   ! netcdf 
   integer :: nc_id        ! netcdf ID for the file
   integer :: ncFileType   ! e.g. NF90_NOCLOBBER etc
   integer :: numVarNetCDF ! number of variables targetted in NetCDF4 file
   integer :: netcdfDataType ! nf90_double, nf90_int, etc 
   integer, allocatable :: nc_varID(:) ! netcdf variable ID for targetted variables
   character(NF90_MAX_NAME), allocatable :: varNameNetCDF(:) ! variable names inside files   
   integer, allocatable :: nc_type(:) ! netcdf variable type for targetted variables
   integer :: nvar         ! number of variables in the netcdf file
   integer :: nc_dimid_time ! netcdf ID for the time dimension
   integer :: nc_varid_time ! netcdf ID for the time variable
   integer :: nc_dimid_station ! netcdf ID for the time dimension
   integer :: nc_dimid_namelen ! netcdf ID for the time dimension
   integer :: station_namelen  ! length of netcdf station name variable
   integer :: ndim          ! number of dimensions in the netcdf file
   integer :: natt          ! number of attributes in the netcdf file
   integer :: ncformat      ! netcdf3 or netcdf4
   character(len=120) :: datenum ! e.g. seconds since 2008-07-31 12:00:00 +00:00
   real(8) :: fillValue     ! missing float data value, usually -99999.d0
   integer :: ifillValue    ! missing integer data value, usually -99999
   !
   ! xdmf 
   character(len=2048) :: xmfFile ! name of XDMF XML file
   integer :: xmfUnit      ! logical unit number of XDMF XML file
   integer :: numVarXDMF   ! number of variables as represented in XDMF XML
   character(NF90_MAX_NAME), allocatable :: varNameXDMF(:)   ! as represented in XDMF XML
   ! the following refer to scalar or vector quantities in XDMF files
   character(len=20), allocatable :: dataCenter(:) ! "Node" or "Element" 
   character(len=20), allocatable :: dataRank ! e.g. "2DVector" 
   character(len=20), allocatable :: numberType(:)   ! "Int" or "Float"
   integer, allocatable :: numberPrecision(:)         ! 4 or 8   
   integer, allocatable :: numComponents(:) ! rank of the data array
   !
   ! particles
   integer :: maxParticles  ! max number of particles at any one time
   integer, allocatable :: numParticlesPerSnap(:) ! (nsnaps)num part in each snap 
end type fileMetaData_t

character(len=80) :: rundes  ! 1st line in adcirc fort.15 input file
character(len=80) :: runid   ! 2nd line in adcirc fort.15 input file


!-----------
!-----------
contains
!-----------
!-----------

!----------------------------------------------------------------------
!                  S U B R O U T I N E   
! D E T E R M I N E   N E T C D F   F I L E   C H A R A C T E R I S T I C S
!----------------------------------------------------------------------
! jgf: Determine type and contents of adcirc data (output) files.
!----------------------------------------------------------------------
subroutine determineNetCDFFileCharacteristics(f, m, n)
use adcmesh
use logging
use ioutil
implicit none
type(fileMetaData_t), intent(inout) :: f
type(mesh_t), intent(inout) :: m
type(meshNetCDF_t), intent(inout) :: n
character(len=NF90_MAX_NAME) :: thisVarName
integer :: i, j
integer :: errorIO
!
! open the netcdf file
call check(nf90_open(trim(f%dataFileName), NF90_NOWRITE, f%nc_id))
!
! determine the type of data stored in the file
call check(nf90_inquire(f%nc_id, f%ndim, f%nvar, f%natt, f%nc_dimid_time, f%ncformat))
if ( (f%ncformat.eq.nf90_format_netcdf4).or. &
   (f%ncformat.eq.nf90_format_netcdf4_classic) ) then
   call allMessage(INFO,'The data file uses netcdf4 formatting.')
endif
!
! determine the number of snapshots in the file
call check(nf90_inquire_dimension(f%nc_id,f%nc_dimid_time,len=f%nSnaps))
write(scratchMessage,'(a,i0,a)') 'There is/are ',f%nSnaps,' dataset(s) in the file.'
call allMessage(INFO,scratchMessage)
if (f%nSnaps.eq.0) then
   write(scratchMessage,'(a,a,a)') 'The file "',trim(f%dataFileName),'" does not contain any output data.'
   call allMessage(ERROR,scratchMessage)
   stop 1
endif
!
!  get time
!
! load up the time values (in seconds)
allocate(f%timesec(f%nSnaps))
allocate(f%it(f%nSnaps))
call check(nf90_inq_varid(f%nc_id, "time", f%NC_VarID_time))
call check(nf90_get_var(f%nc_id, f%NC_VarID_time, f%timesec, (/ 1 /), (/ f%nSnaps /) ))
call check(nf90_get_att(f%nc_id,f%nc_varid_time,'units',f%datenum))
!
! is it a station file?
f%isStationFile = .false. 
do i=1,f%nvar
   call check(nf90_inquire_variable(f%nc_id, i, thisVarName))
   if (trim(thisVarName).eq.'station_name') then
      f%isStationFile = .true.
      call check(nf90_inq_dimid(f%nc_id, "station", f%nc_dimid_station))
   endif 
end do
! if this is not a station file, find the mesh node dimension and
! comment 
if ( f%isStationfile.eqv..false.) then
   call readMeshCommentLineNetCDF(m, f%nc_id)
   ! determine the number of nodes
   call check(nf90_inq_dimid(f%nc_id, "node", n%nc_dimid_node))
endif
! 
! find the rundes and runid attributes in case they need to be written
! to ascii output
errorIO = nf90_get_att(f%nc_id,nf90_global,'rundes',rundes)
if ( errorIO.ne.NF90_NOERR ) then
   rundes = 'rundes' !TODO: make adcirc write this value to netcdf output files
endif
errorIO = nf90_get_att(f%nc_id,nf90_global,'runid',runid) 
if ( errorIO.ne.NF90_NOERR ) then
   runid = 'runid'   !TODO: make adcirc write this value to netcdf output files 
endif
!   
! determine the type of data in the netcdf file, and set the 
! file metadata accordingly
f%num_components = 1
do i=1,f%nvar
   call check(nf90_inquire_variable(f%nc_id, i, thisVarName))
   select case(trim(thisVarName))
   case("u-vel3D","v-vel3D","w-vel3D")
      f%fileTypeDesc = 'an ADCIRC 3D water current velocity file.'
      if ( f%isStationfile.eqv..true. ) then
         f%dataFileType = "fort.42"
      else
         f%dataFileType = "fort.45"
      endif
      f%num_components = 3
      call initFileMetaData(f, thisVarName, 3, 1)
      f%varNameNetCDF(2) = "v-vel3D"
      f%varNameNetCDF(3) = "w-vel3D"
      exit
   case("zeta")
      if ( f%isStationfile.eqv..true. ) then
         f%dataFileType = "fort.61"          
         f%fileTypeDesc = 'a time varying 2D ADCIRC water surface elevation station file (fort.61)'
      else 
         f%dataFileType = "fort.63"
         f%fileTypeDesc = 'a time varying 2D ADCIRC water surface elevation file (fort.63)'
      endif 
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("eta1")
      f % fileTypeDesc = 'a time varying 2D ADCIRC water surface elevation at previous time step file (eta1.63)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit         
   case("eta2")
      f % fileTypeDesc = 'a time varying 2D ADCIRC water surface elevation at current time step file (eta2.63)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("tk")
      f % fileTypeDesc = 'a time varying 2D bottom friction file (tk.63)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("offset")
      f % fileTypeDesc = 'a time varying water level offset file (offset.63)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("tau0")
      f % fileTypeDesc = 'a time varying tau0 file (fort.90)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("coefdiagonal")
      f % fileTypeDesc = 'a fully consistent ADCIRC LHS matrix diagonal file (coefdiagonal.63)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit         
   case("coefele")
      f % fileTypeDesc = 'an element mass matrix coefficient file (coefele.100)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      f % dataCenter(1) = 'Cell' ! noff
      exit
   case("nodecode")
      f % fileTypeDesc = 'a node wet/dry state file (nodecode.63)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit  
   case("noff")
      f % fileTypeDesc = 'an element wet/dry state file (noff.100)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      f % dataCenter(1) = 'Cell' ! noff
      exit          
   case("dryelementareacheck")
      f % fileTypeDesc = 'a dry element area check (dryelementareacheck.100)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      f % dataCenter(1) = 'Cell' ! noff
      exit
   case("nneighele")
      f % fileTypeDesc = 'a number of elemental neighbors attached to each node file (nneighele.63)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      f % timeVarying = .false.
      exit  
   case("nodeids")
      f % fileTypeDesc = 'a fortran indexed node IDs file (nodeids.63)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      f % timeVarying = .false.
      exit  
   case("elementids")
      f % fileTypeDesc = 'a fortran indexed element IDs file (elementids.100)'
      call initFileMetaData(f, thisVarName, 1, 1)     
      f % dataCenter(1) = 'Cell' ! element IDs
      f % timeVarying = .false.
      exit          
   case("zetad")
      f % fileTypeDesc = 'a 2D ADCIRC hotstart file (fort.67/fort.68)'
      f % timeVarying = .false. 
      call initFileMetaData(f, thisVarName, 7, 6)
      f % varNameNetCDF(1) = "zeta1"  ! eta1 
      f % varNameNetCDF(2) = "zeta2"  ! eta2 
      f % varNameNetCDF(3) = "zetad"  ! EtaDisc 
      f % varNameNetCDF(4) = "u-vel"  ! uu2 \_combine as vector in XDMF_
      f % varNameNetCDF(5) = "v-vel"  ! vv2 /
      f % varNameNetCDF(6) = "nodecode"  ! nodecode                   
      f % varNameNetCDF(7) = "noff"   ! noff<---element/cell centered
      !
      f % timeVarying = .false.
      exit
   case("u-vel","v-vel")
      if ( f%isStationfile.eqv..true. ) then
         f%dataFileType = "fort.62"
         f%fileTypeDesc = 'a 2D ADCIRC water current velocity station file (fort.62)'  
      else
         f%dataFileType = "fort.64"
         f%fileTypeDesc = 'a 2D ADCIRC water current velocity file (fort.64)'  
      endif  
      f%num_components = 2
      call initFileMetaData(f, thisVarName, 2, 1)
      f%varNameNetCDF(1) = "u-vel"  ! uu2 in ADCIRC
      f%varNameNetCDF(2) = "v-vel"  ! vv2 in ADCIRC
      exit
   case("uu1-vel","vv1-vel")
      f % fileTypeDesc = 'a 2D ADCIRC water current velocity at previous time step file (uu1vv1.64)'     
      call initFileMetaData(f, thisVarName, 2, 1)
      f % varNameNetCDF(1) = "uu1-vel"  ! uu1 in ADCIRC
      f % varNameNetCDF(2) = "vv1-vel"  ! vv1 in ADCIRC
      f % numComponents(1) = 2
      f % varNameXDMF(1) = 'water_current_velocity_at_previous_timestep'
      exit
   case("pressure")
      if ( f%isStationfile.eqv..true. ) then
         f%dataFileType = "fort.71"
         f%fileTypeDesc = "an ADCIRC barometric pressure station file (fort.71)"
      else
         f%dataFileType = "fort.73"
         f%fileTypeDesc = "an ADCIRC barometric pressure file (fort.73)"
      endif
      call initFileMetaData(f, thisVarName, 1, 1)
      exit 
   case("windx","windy")
      if ( f%isStationfile.eqv..true. ) then
          f%dataFileType = "fort.72"
          f%fileTypeDesc = "an ADCIRC wind velocity file (fort.72)"
      else
          f%dataFileType = "fort.74"
          f%fileTypeDesc = "an ADCIRC wind velocity file (fort.74)"
      endif
      f%num_components = 2
      call initFileMetaData(f, thisVarName, 2, 1)
      f % varNameNetCDF(1) = "windx"  
      f % varNameNetCDF(2) = "windy"  
      exit
   case("maxele","zeta_max")
      f % fileTypeDesc = "an ADCIRC maximum water surface elevation (maxele.63) file"
      ! Check to see if this is a newer-style min/max file that records
      ! the time of the min or max, and if so, prepare to convert the
      ! time of occurrence metadata as well.     
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit
   case("initial_river_elevation")
      f % fileTypeDesc = "an ADCIRC initial river elevation (fort.88) file"
      call initFileMetaData(f, thisVarName, 1, 1)
      exit    
   case("maxwvel","wind_max")
      f % fileTypeDesc = "an ADCIRC maximum wind speed (maxwvel.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit
   case("maxvel","vel_max")
      f % fileTypeDesc = "an ADCIRC maximum current speed (maxvel.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit
   case("maxrs","radstress_max")
      f % fileTypeDesc = "an ADCIRC maximum wave radiation stress gradient (maxrs.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit
   case("minpr","pressure_min")
      f % fileTypeDesc = "an ADCIRC minimum barometric pressure (minpr.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit
   case("endrisinginun")
      f % fileTypeDesc = "an ADCIRC nodes with inundation rising at end of simulation (endrisinginun.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit
   case("initiallydry")
      f % fileTypeDesc = "an ADCIRC dry nodes at cold start (initiallydry.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit 
   case("inundationmask")
      f % fileTypeDesc = "an ADCIRC inundation mask (inundationmask.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      f % timeVarying = .false.
      exit          
   case("inun_time")
      f % fileTypeDesc = "an ADCIRC total time inundated (inundationtime.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit
   case("everdried")
      f % fileTypeDesc = "an ADCIRC ever dried (everdried.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit   
   case("inun_max")
      f % fileTypeDesc = "an ADCIRC maximum inundation depth (maxinundepth.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit      
   case("radstress_x","radstress_y")
      f % fileTypeDesc = "an ADCIRC wave radiation stress gradient (rads.64) file"
      call initfileMetaData(f, thisVarName, 2, 1)
      f % varNameNetCDF(1) = "radstress_x"  
      f % varNameNetCDF(2) = "radstress_y"  
      f % numComponents(1) = 2
   case("swan_DIR")
      f % fileTypeDesc = "a SWAN wave direction (swan_DIR.63) file"
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit   
   case("swan_HS")
      f % fileTypeDesc = "a SWAN significant wave height (swan_HS.63) file"
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("swan_HS_max")
      f % fileTypeDesc = "a SWAN maximum significant wave height (swan_HS_max.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit
   case("swan_TMM10")
      f % fileTypeDesc = "a SWAN mean absolute wave period (swan_TMM10.63) file"
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("swan_TM01")
      f % fileTypeDesc = "SWAN mean absolute wave period (swan_TM01.63) file"
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("swan_TM02")
      f % fileTypeDesc = "a SWAN mean absolute zero crossing period (swan_TM02.63) file"
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("swan_TPS")
      f % fileTypeDesc = "a SWAN smoothed peak period (swan_TPS.63) file"
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("swan_TPS_max")
      f % fileTypeDesc = "a SWAN maximum smoothed peak period (swan_TPS_max.63) file"
      call initMinMaxFileMetaData(f, thisVarName, .true.)
      exit
   case("ESLNodes")
      f % fileTypeDesc = "an elemental slope limiter active nodes (ESLNodes.63) file"
      call initFileMetaData(f, thisVarName, 1, 1)     
      exit
   case("swan_windx","swan_windy")
      f % fileTypeDesc = "a SWAN wind velocity (swan_WIND.64) file"
      call initFileMetaData(f, thisVarName, 2, 1)
      f % varNameNetCDF(1) = "swan_windx"  
      f % varNameNetCDF(2) = "swan_windy"  
      f % numComponents(1) = 2
      exit
   case default
      cycle     ! did not recognize this variable name
   end select
end do
call check(nf90_close(f%nc_id))
!
! if this is not a station file, find the mesh node dimension and
! comment 
if ( f%isStationfile.eqv..false.) then
   ! determine the number of nodes
   call check(nf90_inq_dimid(f%nc_id, "node", n%nc_dimid_node))
   call check(nf90_inquire_dimension(f%nc_id, n%nc_dimid_node, len=m%np))
else
   ! determine the number of stations
   call check(nf90_inq_dimid(f%nc_id, "station", f%nc_dimid_station))
   call check(nf90_inquire_dimension(f%nc_id, f%nc_dimid_station, len=f%nStations))

   call check(nf90_inq_dimid(f%nc_id, "namelen", f%nc_dimid_namelen))
   call check(nf90_inquire_dimension(f%nc_id, f%nc_dimid_namelen, len=f%station_namelen))
endif                                                             


! determine time increment between output writes
if ( (f%nSnaps.gt.1).and.(f%timeOfOccurrence.eqv..false.) ) then
   f%time_increment = f%timesec(2) - f%timesec(1)
else
   f%time_increment = -99999.d0
endif
f%nspool = -99999
f%it(:) = -99999
f%defaultValue = -99999.d0
!
! get the variable id(s) of the data we want to convert
do i=1,f%num_components
   call check(nf90_inq_varid(f%nc_id, f%varNameNetCDF(i), f%nc_varid(i)))
end do

!----------------------------------------------------------------------
end subroutine determineNetCDFFileCharacteristics
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!                S U B R O U T I N E   
!    A D D   D A T A   A T T R I B U T E S   N E T C D F
!----------------------------------------------------------------------
! jgf : Adds attributes for netcdf cf compliance based on the type 
! of data to be written into the file. 
!----------------------------------------------------------------------
subroutine addDataAttributesNetCDF(fn, m, n)
use ioutil, only : check
use adcmesh
implicit none
type(fileMetaData_t), intent(inout) :: fn ! netcdf file to write metadata attributes to 
type(mesh_t), intent(inout) :: m
type(meshNetCDF_t), intent(inout) :: n
integer :: nc_dimID(2) = (/ -99, -99 /)
integer :: nc_dimid_grid(3) = (/ -99, -99, -99 /)
character(NF90_MAX_NAME) :: thisVarName
!
nc_dimid = (/ n%nc_dimid_node, fn%nc_dimID_Time /)
select case(trim(fn%dataFileType))
case('fort.221') 
   thisVarName = 'basinpressure'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,fn%varNameNetCDF(1),nf90_double,nc_dimid_grid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillvalue',fn%fillvalue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','air pressure at sea level on basin grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','air_pressure_basin_grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','mbar'))
case('fort.223') 
   thisVarName = 'regionpressure'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,fn%varNameNetCDF(1),nf90_double,nc_dimid_grid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillvalue',fn%fillvalue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','air pressure at sea level on region grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','air_pressure_region_grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','mbar'))
case('fort.222') 
   fn%num_components = 2
   thisVarName = 'basinwindx'
   call initFileMetaData(fn, thisVarName, 2, 1)
   call check(nf90_def_var(fn%nc_id,thisVarName,nf90_double,nc_dimid_grid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','e/w wind velocity on basin grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','eastward_wind_basin_grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'positive','east'))
   fn%varNameNetCDF(2) = 'basinwindy'
   call check(nf90_def_var(fn%nc_id,'basinwindy',nf90_double,nc_dimid_grid,fn%nc_varID(2)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','n/s wind velocity on basin grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name','nortward_wind_basin_grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'positive','north'))
case('fort.224') 
   fn%num_components = 2
   thisVarName = 'regionwindx'
   call initFileMetaData(fn, thisVarName, 2, 1)
   call check(nf90_def_var(fn%nc_id,'regionwindx',nf90_double,nc_dimid_grid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','e/w wind velocity on region grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','eastward_wind_region_grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'positive','east'))
   fn%varNameNetCDF(2) = 'regionwindy'
   call check(nf90_def_var(fn%nc_id,'regionwindy',nf90_double,nc_dimid_grid,fn%nc_varID(2)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','n/s wind velocity on region grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name','nortward_wind_region_grid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'positive','north'))
case('fort.63') !63
   thisVarName = 'zeta'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'zeta',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','water surface elevation above geoid'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','water_surface_elevation'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m'))
case('eta1.63') 
   thisVarName = 'eta1'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'eta1',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','water surface elevation above geoid at previous time step'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','water_surface_elevation_at_previous_timestep'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m'))
case('eta2.63') 
   thisVarName = 'eta2'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'eta2',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','water surface elevation above geoid at current time step'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','water_surface_elevation_at_current_timestep'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m'))
case('tk.63') 
   thisVarName = 'tk'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'tk',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','bottom friction force'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','bottom_friction_force'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m'))
case('fort.90') 
   thisVarName = 'tau0'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'tau0',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','time varying tau0'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','time_varying'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','1'))
case('offset.63') !63
   thisVarName = 'offset'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'offset',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','water level offset'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','water_level'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m h2o'))
case('fort.64') 
   fn%datarank = "2DVector"
   fn%num_components = 2
   thisVarName = 'u-vel'
   call initFileMetaData(fn, thisVarName, 2, 1)
   call check(nf90_def_var(fn%nc_id,'u-vel',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','water column vertically averaged east/west velocity'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','eastward_water_velocity'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'positive','east'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'dry_value',-99999.0d0))
   fn%varNameNetCDF(2) = 'v-vel'
   call check(nf90_def_var(fn%nc_id,'v-vel',nf90_double,nc_dimid,fn%nc_varID(2)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','water column vertically averaged north/south velocity'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name','northward_water_velocity'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'positive','north'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'dry_value',-99999.0d0))
case('uu1vv1.64') 
   fn%datarank = "2DVector"
   fn%num_components = 2
   thisVarName = 'uu1-vel'
   call initFileMetaData(fn, thisVarName, 2, 1)
   call check(nf90_def_var(fn%nc_id,'uu1-vel',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','water column vertically averaged east/west velocity at previous time step'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','eastward_water_velocity_at_previous_time_step'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'positive','east'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'dry_value',-99999.0d0))
   fn%varNameNetCDF(2) = 'vv1-vel'
   call check(nf90_def_var(fn%nc_id,'vv1-vel',nf90_double,nc_dimid,fn%nc_varID(2)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','water column vertically averaged north/south velocity at previous time step'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name','northward_water_velocity_at_previous_time_step'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'positive','north'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'dry_value',-99999.0d0))
case('fort.73') !73
   thisVarName = 'pressure'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'pressure',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','air pressure at sea level'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','airressure_at_sea_level'))            
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','meters of water'))
case('fort.74') !74
   fn%datarank = "2DVector"
   fn%num_components = 2
   thisVarName = 'windx'
   call initFileMetaData(fn, thisVarName, 2, 1)
   call check(nf90_def_var(fn%nc_id,'windx',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','e/w wind velocity'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','eastward_wind'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'positive','east'))
   fn%varNameNetCDF(2) = 'windy'
   call check(nf90_def_var(fn%nc_id,'windy',nf90_double,nc_dimid,fn%nc_varID(2)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','n/s wind velocity'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name','northward_wind'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','m s-1'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'positive','north'))
case('maxele.63') !maxele
   thisVarName = 'zeta_max'
   call initMinMaxFileMetaData(fn, thisVarName, .false.)
   call check(nf90_def_var(fn%nc_id,'zeta_max',nf90_double,n%nc_dimid_node,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','maximum sea surface elevation above datum'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','maximum_sea_surface_elevation_above_datum'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m'))
   if ( fn%timeOfOccurrence.eqv..true.) then
      call check(nf90_def_var(fn%nc_id,'time_of_zeta_max',nf90_double,n%nc_dimid_node,fn%nc_varID(2)))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','time of maximum sea surface elevation above datum'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name','time_of_maximum_sea_surface_elevation_above_datum'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','y x'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'location','node'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'mesh','adcirc_mesh'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','s'))
   endif
case('minpr.63') ! minimum barometric pressure
   thisVarName = 'pressure_min'
   call check(nf90_def_var(fn%nc_id,'pressure_min',nf90_double,n%nc_dimid_node,fn%nc_varID(1)))
   call initMinMaxFileMetaData(fn, thisVarName, .false.)
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','minimum air pressure at sea level'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','minimum_air_pressure_at_sea_level'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','meters of water'))
   if ( fn%timeOfOccurrence.eqv..true.) then
      call check(nf90_def_var(fn%nc_id,'time_of_pressure_min',nf90_double,n%nc_dimid_node,fn%nc_varID(2)))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','time of minimum air pressure at sea level'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name','time_of_minimum_air_pressure_at_sea_level'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','y x'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'location','node'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'mesh','adcirc_mesh'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','s'))
   endif
case('swan_dir.63') !dir
   thisVarName = 'swan_dir'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'swan',nf90_double,nc_dimid,fn%nc_varID(1)))         
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','wave direction'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','sea_surface_wave_direction'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','degrees_cw_from_east'))
case('swan_hs.63') !hs
   thisVarName = 'swan_hs'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'swan_hs',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','significant wave height'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','sea_surface_wave_significant_height'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m'))
case('swan_tmm10.63') !tmm10
   thisVarName = 'swan_tmm10'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'swan_tmm10',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','mean period'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','s'))
case('swan_tps.63') !tps
   thisVarName = 'swan_tps'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'swan_tps',nf90_double,nc_dimid,fn%nc_varID(1)))         
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','peak period'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','maximum_sea_surface_wave_period_at_variance_spectral_density_maximum'))            
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','s'))
case('coefdiagonal.63') 
   thisVarName = 'coefdiagonal'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'coefdiagonal',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','adcirc fully consistent left hand side matrix diagonal coefficients'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','adcirc_fully_consistent_lhs_diagonal '))            
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','unitless'))
case('nodecode.63') 
   thisVarName = 'nodecode'
   fn%netcdfdatatype = nf90_int
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'nodecode',fn%netcdfdatatype,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',-99999))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','node wet or dry'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','node_wet_or_dry'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','unitless'))
case('noff.100') 
   fn%netcdfdatatype = nf90_int
   fn%datacenter = 'element'
   thisVarName = 'noff'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'noff',fn%netcdfdatatype,(/ n%nc_dimid_nele, fn%nc_dimid_time /),fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',-99999))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','element wet or dry'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','element_wet_or_dry'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','element'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','unitless'))
case('dryelementareacheck.100') 
   fn%netcdfdatatype = nf90_int
   fn%datacenter = 'element'
   thisVarName = 'dryelementareacheck'
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'dryelementareacheck',fn%netcdfdatatype,n%nc_dimid_nele,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',-99999))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','dry element area check'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','dry_element_area_check'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','element'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','unitless'))
case('coefele.100') 
   fn%datacenter = 'element'
   thisVarName = 'coefele'   
   call initFileMetaData(fn, thisVarName, 1, 1)
   call check(nf90_def_var(fn%nc_id,'coefele',nf90_double,(/ n%nc_dimid_nele, fn%nc_dimid_time /),fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',-99999.d0))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','element contribution to mass matrix diagonal'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','element_coef'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','element'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','unitless'))
case('nneighele.63') 
   fn%netcdfdatatype = nf90_int
   thisVarName = 'nneighele'
   call initFileMetaData(fn, thisVarName, 1, 1)   
   call check(nf90_def_var(fn%nc_id,'nneighele',fn%netcdfdatatype,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',-99999))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','number of element neighbors for each node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','num_element_neighbors'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','unitless'))
case('nodeids.63') 
   thisVarName = 'nodeids'
   fn%netcdfdatatype = nf90_int
   call initFileMetaData(fn, thisVarName, 1, 1)   
   call check(nf90_def_var(fn%nc_id,'nodeids',fn%netcdfdatatype,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',-99999))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','fortran indexed node ids'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','fortran_indexed_node_ids'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','unitless'))
case('elementids.100') 
   fn%netcdfdatatype = nf90_int
   fn%datacenter = 'element'
   thisVarName = 'elementids'
   call initFileMetaData(fn, thisVarName, 1, 1)   
   call check(nf90_def_var(fn%nc_id,'elementids',fn%netcdfdatatype,n%nc_dimid_nele,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',-99999))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','fortran indexed element ids'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','fortran_indexed_element_ids'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','element'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','unitless'))
case('maxwvel.63') ! maxwvel
   thisVarName = 'wind_max'
   call initMinMaxFileMetaData(fn, thisVarName, .false.)
   call check(nf90_def_var(fn%nc_id,'wind_max',nf90_double,n%nc_dimid_node,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','maximum wind speed at sea level'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','maximum_wind_speed_at_sea_level'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m s-1'))
   if ( fn%timeOfOccurrence.eqv..true.) then
      call check(nf90_def_var(fn%nc_id,'time_of_wind_max',nf90_double,n%nc_dimid_node,fn%nc_varID(2)))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','time of maximum wind speed at sea level'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name','time_of_maximum_wind_speed_at_sea_level'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','y x'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'location','node'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'mesh','adcirc_mesh'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','s'))
   endif
case('maxvel.63') ! max water current velocity 
   thisVarName = 'vel_max'
   call initMinMaxFileMetaData(fn, thisVarName, .false.)
   call check(nf90_def_var(fn%nc_id,'vel_max',nf90_double,n%nc_dimid_node,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','maximum water column vertically averaged velocity'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','maximum_water column_vertically_averaged_velocity'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m s-1'))
   if (fn%timeOfOccurrence.eqv..true.) then
      call check(nf90_def_var(fn%nc_id,'time_of_vel_max',nf90_double,n%nc_dimid_node,fn%nc_varID(2)))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','time of maximum water column vertically averaged velocity'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name','time_of_maximum_water_column_vertically_averaged_velocity'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','y x'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'location','node'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'mesh','adcirc_mesh'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','s'))
   endif
case('swan_hs_max.63') ! swan_hs_max
   thisVarName = 'swan_hs_max'
   call initMinMaxFileMetaData(fn, thisVarName, .false.)
   call check(nf90_def_var(fn%nc_id,'swan_hs_max',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','maximum significant wave height'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name', & 
       'maximum_sea_surface_wave_significant_height'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m'))
   if (fn%timeOfOccurrence.eqv..true.) then
      call check(nf90_def_var(fn%nc_id,'time_of_swan_hs_max',nf90_double,n%nc_dimid_node,fn%nc_varID(2)))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','time of maximum significant wave height'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name', & 
          'time_of_maximum_sea_surface_wave_significant_height'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','y x'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'location','node'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'mesh','adcirc_mesh'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','s'))
   endif
case('swan_tps_max.63') ! swan_tps_max
   thisVarName = 'swan_tps_max'
   call initMinMaxFileMetaData(fn, thisVarName, .false.)
   call check(nf90_def_var(fn%nc_id,'swan_tps_max',nf90_double,n%nc_dimid_node,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','maximum smoothed peak period'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name', &
      'maximum_sea_surface_wave_period_at_variance_spectral_density_maximum'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','s'))
   if ( fn%timeOfOccurrence.eqv..true.) then
      call check(nf90_def_var(fn%nc_id,'swan_tps_max',nf90_double,n%nc_dimid_node,fn%nc_varID(2)))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'_fillValue',fn%fillValue))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'long_name','time of maximum smoothed peak period'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'standard_name', &
         'time_of_maximum_sea_surface_wave_period_at_variance_spectral_density_maximum'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'coordinates','y x'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'location','node'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'mesh','adcirc_mesh'))
      call check(nf90_put_att(fn%nc_id,fn%nc_varID(2),'units','s'))            
   endif
case('eslnodes.63') ! eslnodes.63
   thisVarName = 'eslnodes'
   call initFileMetaData(fn, thisVarName, 1, 1)   
   call check(nf90_def_var(fn%nc_id,'eslnodes',nf90_double,nc_dimid,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','elemental slope limiter active nodes'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name', &
      'elemental_slope_limiter_active_nodes'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','time y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','1'))
case('fort.88') 
   thisVarName = 'initial_river_elevation'
   call initFileMetaData(fn, thisVarName, 1, 1)   
   call check(nf90_def_var(fn%nc_id,'initial_river_elevation',nf90_double,n%nc_dimid_node,fn%nc_varID(1)))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'_fillValue',fn%fillValue))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'long_name','initial river elevation'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'standard_name','initial_river_elevation'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'coordinates','y x'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'location','node'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'mesh','adcirc_mesh'))
   call check(nf90_put_att(fn%nc_id,fn%nc_varID(1),'units','m'))
case default
   write(6,'(a)') 'ERROR: Unable to convert '//trim(fn%dataFileType)//' files.'

end select
!----------------------------------------------------------------------
end subroutine addDataAttributesNetCDF
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!    S U B R O U T I N E   F O R M   M A X   F I L E   N A M E 
!----------------------------------------------------------------------
! jgf: Form the name of the max (or min) file, given the name of the
! data file from which it will be derived. 
!----------------------------------------------------------------------
subroutine formMaxFileName(datafile, maxFileName)
implicit none
character(len=1024), intent(in) :: datafile
character(len=1024), intent(out) :: maxFileName
character(len=1024) :: fileNameBase
integer :: extIndex
!
! defaults
fileNameBase = datafile
!
! find and trim off the extension, if any
extIndex = 0
extIndex = index(datafile,'.nc')
if (extIndex.ne.0) then
   fileNameBase = datafile(:extIndex-1)
endif
!
! pick the corresponding max file name
select case(trim(adjustl(fileNameBase)))
   case("fort.63")
      maxFileName = 'maxele.63'
   case("fort.64")
      maxFileName = 'maxvel.63'
   case("fort.73")
      maxFileName = 'minpr.63'
   case("fort.74")
      maxFileName = 'maxwvel.63'
   case("swan_DIR.63")
      maxFileName = 'swan_DIR_max.63'
   case("swan_HS.63")
      maxFileName = 'swan_HS_max.63'
   case("swan_TMM10.63")
      maxFileName = 'swan_TMM10_max.63'
   case("swan_TPS.63")
      maxFileName = 'swan_TPS_max.63'
   case("fort.69")
      maxFileName = 'maxwarnelev.63'
   case default
      write(6,'(a,a,a)') 'WARNING: File name ',trim(adjustl(datafile)),' was not recognized.'
      maxFileName = trim(adjustl(fileNameBase)) // '_maxfile.63'
end select
write(6,'(a,a,a)') 'INFO: The max file name is set to ',trim(adjustl(maxFileName)),'.'
!----------------------------------------------------------------------
end subroutine formMaxFileName
!----------------------------------------------------------------------



!----------------------------------------------------------------------
!  S U B R O U T I N E     I N I T   F I L E   M E T A  D A T A 
!----------------------------------------------------------------------
! Allocate memory to hold variable names, variable IDs, etc for
! variables in the targetted NetCDF4 files so the metadata can be
! appropriately written to the XDMF XML. Also initialize the newly
! allocated variables to reasonable values. 
!----------------------------------------------------------------------
subroutine initFileMetaData(fmd, firstVarName, numNC, numXDMF)
use netcdf
implicit none
type(fileMetaData_t), intent(inout) :: fmd
character(NF90_MAX_NAME), intent(in) :: firstVarName
integer, intent(in) :: numNC
integer, intent(in) :: numXDMF
!
fmd % timeVarying = .true.       ! initialize to most common value
fmd % timeOfOccurrence = .false. ! only relevant to min/max files
!
! NetCDF
fmd % numVarNetCDF = numNC
allocate(fmd % varNameNetCDF(numNC))
fmd % varNameNetCDF(:) = 'error: not_set'
fmd % varNameNetCDF(1) = trim(firstVarName) ! initialize to most common value
allocate(fmd % nc_varID(numNC))
fmd % nc_varID(:) = -999
allocate(fmd % nc_type(numNC))
fmd % nc_type(:) = -999
!
! XDMF
fmd % numVarXDMF = numXDMF
allocate(fmd % varNameXDMF(numXDMF))
fmd % varNameXDMF(:) = 'error: not_set'
allocate(fmd % numComponents(numXDMF))
fmd % numComponents(:) = 1         ! initialize to most common value
allocate(fmd % dataCenter(numXDMF))
fmd%dataCenter(:) = 'Node'                 ! initialize to most common value
allocate(fmd % numberType(numXDMF))
fmd%numberType(:) = 'Float'                 ! initialize to most common value
allocate(fmd % numberPrecision(numXDMF))
fmd%numberPrecision(:) = 8                 ! initialize to most common value
fmd%initialized = .true. 

!----------------------------------------------------------------------
end subroutine initFileMetaData
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! S U B R O U T I N E   I N I T  M I N  M A X  F I L E  M E T A D A T A
!----------------------------------------------------------------------
! Checks for the existence of time of occurrence data in the min max
! file before initializing the file metadata. 
!----------------------------------------------------------------------
subroutine initMinMaxFileMetaData(fmd, someVarName, checkTimeOfOccurrence)
use netcdf
use logging, only : allMessage, INFO
use ioutil, only : check
implicit none
type(fileMetaData_t), intent(inout) :: fmd 
character(NF90_MAX_NAME), intent(in) :: someVarName 
logical, intent(in) :: checkTimeOfOccurrence ! true if unknown whether file contains time of occurrence data
!
character(NF90_MAX_NAME) timeOfVarName 
character(NF90_MAX_NAME) aVarName
integer :: j
!     
timeOfVarName = 'time_of_'//trim(someVarName)
if (trim(someVarName).eq."inun_time") then
   timeOfVarName = 'last_'//trim(someVarName)
endif
if ( checkTimeOfOccurrence.eqv..true. ) then
   do j=1,fmd%nvar
      call check(nf90_inquire_variable(fmd%nc_id, j, aVarName))
      if (trim(aVarName).eq.trim(timeOfVarName)) then
         call allMessage(INFO,'The file contains time of occurrence data.')
         fmd % timeOfOccurrence = .true.
         exit
      endif 
   end do
endif
if (fmd % timeOfOccurrence.eqv..true.) then
   call initFileMetaData(fmd, someVarName, 2, 2)
   fmd % varNameNetCDF(2) = trim(timeOfVarName)
   fmd % timeOfOccurrence = .true. ! was reset in initFileMetaData   
else
   call initFileMetaData(fmd, someVarName, 1, 1)
endif
fmd % timeVarying = .false. ! was reset in initFileMetaData     
!----------------------------------------------------------------------
end subroutine initMinMaxFileMetaData
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!----------------------------------------------------------------------
end module asgsio
!----------------------------------------------------------------------
!----------------------------------------------------------------------




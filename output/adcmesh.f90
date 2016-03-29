!-----+---------+---------+---------+---------+---------+---------+
!
! adcmesh.f90
! This is a module for storing and manipulating data for ADCIRC meshes;
! it is based on code originally written by Corbitt Kerr.
!
!-----+---------+---------+---------+---------+---------+---------+
module adcmesh
!-----+---------+---------+---------+---------+---------+---------+
character(1024) :: meshFileName ! full pathname of file
real(8), parameter :: R = 6378206.4d0 ! radius of the earth
real(8), parameter :: pi = 3.141592653589793d0
real(8), parameter :: deg2rad = pi/180.d0
real(8), parameter :: rad2deg = 180.d0/pi
real(8), parameter :: oneThird = 1.d0/3.d0
logical :: verbose
real(8), allocatable, target :: xyd(:,:), bar(:,:,:)
!
! parameters related to carte parallelogrammatique projection (CPP)
logical                          :: cppComputed = .false.
real(8), allocatable :: x_cpp(:)
real(8), allocatable :: y_cpp(:)
!
! parameters related to Albers Equal Area Conic projection
logical :: albersComputed = .false.
real(8), allocatable :: xalbers(:)
real(8), allocatable :: yalbers(:)
!
! parameters related to the neighbor edge length table (np,neimax)
logical :: neighborEdgeLengthTableComputed = .false. ! .true. when mem is allocated for this
real(8), allocatable :: neighborEdgeLengthTable(:,:)
real(8), allocatable :: areas(:) ! (ne) 2x element areas in square meters
real(8), allocatable :: sfac(:) ! (np)
real(8), allocatable :: sfacAvg(:) ! (ne)
real(8), allocatable :: fdx(:,:) ! (3,ne)
real(8), allocatable :: fdy(:,:) ! (3,ne)
real(8), allocatable :: centroids(:,:) ! (2,ne) x and y coordinates of the element centroids
!
real(8), allocatable          :: sigma(:)
character(80)                 :: agrid
integer                       :: ne, np
integer, allocatable         :: nmnc(:,:) ! element table in netcdf (3,ne)
integer, allocatable         :: nm(:,:)   ! element table in adcirc (ne,3)
integer                       :: nfen
integer                       :: mnei = 15  ! maximum number of neighbors for a node
integer                       :: neta_count ! count of open boundary nodes
integer                       :: nvel_count ! count of land boundary nodes
integer                       :: nope, neta
integer                       :: nbou, nvel

integer,          allocatable :: nvdll(:)  ! number of nodes on each open boundary
integer,          allocatable :: nbdv(:,:) ! node numbers on each open boundary
integer,          allocatable :: nvell(:)  ! number of nodes on each flux boundary
integer,          allocatable :: ibtype(:) ! boundary type of each flux boundary
integer,          allocatable :: ibtypee(:) ! boundary type of each elevation boundary
integer,          allocatable :: nbvv(:,:) ! node numbers on each flux boundary
integer,          allocatable :: lbcodei(:) ! bound. type array for flux boundaries 
integer                       :: nvdll_max  ! longest elevation boundary
integer                       :: nvell_max  ! longest flux boundary     
integer, allocatable :: nbd(:)
integer, allocatable :: nbv(:)
integer, allocatable :: ibconn(:,:)

integer, allocatable :: ibtype_orig(:)
integer, allocatable :: bcrnbvv(:,:)
integer, allocatable :: bcrnvell(:)
real(8), allocatable :: barlanht(:,:)
real(8), allocatable :: barinht(:,:)
real(8), allocatable :: pipeht(:,:)
real(8), allocatable :: barlancfsp(:,:)
real(8), allocatable :: barlancfsb(:,:)
real(8), allocatable :: barincfsb(:,:)
real(8), allocatable :: barincfsp(:,:)
real(8), allocatable :: pipediam(:,:)
real(8), allocatable :: pipecoef(:,:)


logical                       :: neighborTableComputed = .false.
logical                       :: allLeveesOK ! .false. if there are any issues
integer                       :: NEIMIN
integer                       :: NEIMAX 
integer,         allocatable :: NNeigh(:)
integer,         allocatable :: NeiTab(:,:)
integer,         allocatable :: NeiTabEle(:,:)
!
integer                       :: NC_DimID_node
integer                       :: NC_DimID_vnode
integer                       :: NC_DimID_nele
integer                       :: NC_DimID_nvertex
integer                       :: NC_DimID_nope
integer                       :: NC_DimID_max_nvdll
integer                       :: NC_DimID_nbou
integer                       :: NC_DimID_neta
integer                       :: NC_DimID_nvel
integer                       :: NC_DimID_max_nvell
!
integer                       :: NC_VarID_Mesh
integer                       :: NC_VarID_x
integer                       :: NC_VarID_y
integer                       :: NC_VarID_sigma
integer                       :: NC_VarID_element
integer                       :: NC_VarID_neta
integer                       :: NC_VarID_nvdll
integer                       :: NC_VarID_max_nvdll
integer                       :: NC_VarID_ibtypee
integer                       :: NC_VarID_nbdv
integer                       :: NC_VarID_nvel
integer                       :: NC_VarID_nope 
integer                       :: NC_VarID_nvell
integer                       :: NC_VarID_max_nvell
integer                       :: NC_VarID_ibtype
integer                       :: NC_VarID_nbvv
integer                       :: NC_VarID_depth

logical                       :: projectCPP ! .true. if user wants to project mesh coordinates with CPP to aid in visualization
logical                       :: cppUpdated ! .true. if we've already computed/written CPP on this execution
real(8) :: slam0  ! longitude on which cpp projection is centered (degrees)
real(8) :: sfea0  ! latitude on which cpp projection is centered (degrees)
real(8) :: lonmin   ! domain extents (degrees)
real(8) :: lonmax
real(8) :: latmin
real(8) :: latmax 
!
! elevation boundaries and flux boundaries where
! ibtype = 0,1,2,10,11,12,20,21,22,30,52
type simpleBoundary_t
   integer :: indexNum                ! order within the fort.14 file (used to get IBTYPE etc)
   integer :: informationID           ! xdmf ID for IBTYPEE or IBTYPE info
   integer :: setID                   ! xdmf ID for node numbers
   integer, allocatable :: nodes(:) ! node numbers on boundary
   real(8), allocatable :: bGeom(:)  ! coordinates for visualization
end type simpleBoundary_t
! variable holding elevation boundaries
type(simpleBoundary_t), allocatable :: elevationBoundaries(:)

! variable holding flux boundaries with ibtype = 0, 1, 2, 10, 11, 12, 20, 21, 22, 30, 52
type(simpleBoundary_t), allocatable :: simpleFluxBoundaries(:)
integer :: numSimpleFluxBoundaries ! for memory allocation
integer :: sfCount   ! index into the simpleFluxBoundaries array

! flux boundaries where ibtype = 3, 13, 23

type externalFluxBoundary_t
   integer :: indexNum               ! order within the fort.14 file
   integer :: informationID              ! xdmf ID for IBTYPE info
   integer :: setID                      ! xdmf ID for node numbers
   integer :: numAttributes = 2
   integer :: attributeIDs(2) ! xdmf IDs for parameters
   integer, allocatable :: nodes(:)
   real(8), allocatable :: barlanht(:)
   real(8), allocatable :: barlancfsp(:)
   real(8), allocatable :: leveeGeom(:)
end type externalFluxBoundary_t
type(externalFluxBoundary_t), allocatable :: externalFluxBoundaries(:)
integer :: numExternalFluxBoundaries 
integer :: efCount   ! index into the externalFluxBoundaries array

! flux boundaries where ibtype = 4, 24
type internalFluxBoundary_t
   integer :: indexNum               ! order within the fort.14 file
   integer :: informationID              ! xdmf ID for IBTYPE info
   integer :: setID                      ! xdmf ID for node numbers
   integer :: numAttributes = 4
   integer :: attributeIDs(4) ! xdmf IDs for parameters
   integer, allocatable :: nodes(:)
   integer, allocatable :: ibconn(:)
   real(8), allocatable :: barinht(:)
   real(8), allocatable :: barincfsb(:)
   real(8), allocatable :: barincfsp(:)
   real(8), allocatable :: leveeGeom(:)         
   integer, allocatable :: ibTypeAttribute(:) ! used for visualization
end type internalFluxBoundary_t
type(internalFluxBoundary_t), allocatable :: internalFluxBoundaries(:)
integer :: numInternalFluxBoundaries    
integer :: ifCount   ! index into the internalFluxBoundaries array

! flux boundaries where ibtype = 5, 25
type internalFluxBoundaryWithPipes_t
   integer :: indexNum               ! order within the fort.14 file
   integer :: informationID              ! xdmf ID for IBTYPE info
   integer :: setID                      ! xdmf ID for node numbers
   integer :: numAttributes = 7
   integer :: attributeIDs(7) ! xdmf IDs for parameters
   integer, allocatable :: nodes(:)
   integer, allocatable :: ibconn(:)
   real(8), allocatable :: barinht(:)
   real(8), allocatable :: barincfsb(:)
   real(8), allocatable :: barincfsp(:)
   real(8), allocatable :: pipeht(:)
   real(8), allocatable :: pipecoef(:)
   real(8), allocatable :: pipediam(:)
   real(8), allocatable :: leveeGeom(:)
end type internalFluxBoundaryWithPipes_t      
type(internalFluxBoundaryWithPipes_t), allocatable :: internalFluxBoundariesWithPipes(:)
integer :: numInternalFluxBoundariesWithPipes
integer :: ifwpCount ! index into the internalFluxBoundariesWithPipes array

integer, parameter :: specifiedFluxBoundaryTypes(5) = (/ 2, 12, 22, 32, 52 /)
integer :: nfluxf ! =1 if there are any specified flux boundaries in the mesh
! 
! all info needed for self describing dataset in XDMF
type xdmfMetaData_t
   logical :: createdIDs        ! .true. if infoIDs have been created 
   character(80) :: variable_name 
   integer :: variable_name_id
   character(80) :: long_name 
   integer :: long_name_id
   character(80) :: standard_name
   integer :: standard_name_id
   character(80) :: coordinates
   integer :: coordinates_id
   character(80) :: units
   integer :: units_id
   character(80) :: positive
   integer :: positive_id
   integer :: ndset ! number of data sets
end type xdmfMetaData_t
!
! info related to recording stations
type station_t
   real(8) :: lon             ! decimal degrees east 
   real(8) :: lat             ! decimal degrees north
   integer :: elementIndex   ! where station is located in a particular mesh
   integer :: nodeIndices(3) ! nodes that surround the station
   real(8) :: weights(3)     ! used to interpolate station values based on nodal values
   character(len=1024) :: stationID   ! generally a number assigned by govt agency 
   character(len=1025) :: description ! human readable 
end type station_t
      
!-----+---------+---------+---------+---------+---------+---------+
contains
!-----+---------+---------+---------+---------+---------+---------+  

!-----+---------+---------+---------+---------+---------+---------+
!  READ14_FindDims
!-----+---------+---------+---------+---------+---------+---------+
subroutine read14_findDims()
use asgsio
implicit none
integer :: ios ! i/o status 
integer :: lineNum ! line number currently being read
integer :: i, j, k
integer, parameter :: iunit = 14
! initializations
lineNum = 1
numSimpleFluxBoundaries = 0
numExternalFluxBoundaries = 0 
numInternalFluxBoundaries = 0 
numInternalFluxBoundariesWithPipes = 0
!
call openFileForRead(iunit, meshFileName)
read(iunit,'(A80)',err=10,end=20,iostat=ios) agrid
lineNum = lineNum + 1
write(6,'(A)') "INFO: Mesh file comment line: "//trim(agrid)
write(6,'(A)') "INFO: Reading mesh file dimensions."
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) ne, np
do k = 1, np
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) i
   lineNum = lineNum + 1
   if (i.ne.k) then
      write(6,'("ERROR: Attempted to read node number ",i0," but found node number ",i0," instead.")') k, i
      stop
   endif
enddo
do k = 1, ne
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) i
   lineNum = lineNum + 1
   if (i.ne.k) then
      write(6,'("ERROR: Attempted to read element number ",i0," but found element number ",i0," instead.")') k, i
      stop
   endif 
enddo
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nope  ! total number of elevation boundaries
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) neta  ! total number of nodes on elevation boundaries
lineNum = lineNum + 1
write(6,'(a)') 'INFO: Allocating memory for elevation specified boundaries.'
call allocateElevationBoundaryLengths()
call allocateAdcircElevationBoundaryArrays() 

neta_count = 0
nvdll_max = 0
do k = 1, nope         
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvdll(k) ! number of nodes on the kth elevation boundary segment
   lineNum = lineNum + 1
   nvdll_max = max(nvdll_max,nvdll(k))
   do j = 1, nvdll(k)
      read(unit=iunit,fmt=*,err=10,end=20,iostat=ios)
      lineNum = lineNum + 1
      neta_count = neta_count + 1
   enddo
enddo
if ( neta_count.ne.neta ) then
   write(6,'("WARNING: Number of open boundary nodes was set to ",i0," but ",i0," were found.")') neta, neta_count
endif
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nbou ! total number of flux boundaries
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvel ! total number of nodes on flux boundaries
lineNum = lineNum + 1
write(6,'(a)') 'INFO: Allocating memory for flux specified boundaries.'
call allocateFluxBoundaryLengths()
call allocateAdcircFluxBoundaryArrays()
nvel_count = 0
nvell_max = 0
do k = 1, nbou
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvell(k), ibtype_orig(k)  ! number of nodes and type of kth flux boundary 
   lineNum = lineNum + 1
   nvell_max = max(nvell_max,nvell(k))
   do j = 1, nvell(k)
      read(unit=iunit,fmt=*,err=10,end=20,iostat=ios)
      lineNum = lineNum + 1
      nvel_count = nvel_count + 1
   enddo
   ! count the total number of each type of boundary for later
   ! use in memory allocation
   select case(ibtype_orig(k))
   case(0,1,2,10,11,12,20,21,22,30,52)
       numSimpleFluxBoundaries = numSimpleFluxBoundaries + 1
   case(3,13,23)
       numExternalFluxBoundaries = numExternalFluxBoundaries + 1 
   case(4,24)
       numInternalFluxBoundaries = numInternalFluxBoundaries + 1 
   case(5,25)
       numInternalFluxBoundariesWithPipes = numInternalFluxBoundariesWithPipes + 1
   case default
       write(6,'("ERROR: The boundary type ",i0," was found in the file but is not valid.")') ibtype_orig(k)
       stop
   end select
enddo
if ( nvel_count.ne.nvel) then
   write(6,'("WARNING: Number of land boundary nodes was set to ",i0," but ",i0," were found.")') nvel, nvel_count
   if (verbose.eqv..true.) then
      write(6,*) 'WARNING: Here is the summary of land boundary node information:'
      write(6,'("NVEL (specified number of land boundary nodes) = ",i0,".")') nvel
      write(6,'("Counted number of land boundary nodes = ",i0,".")') nvel_count
      do k=1,nbou
         write(6,'("ibtype(",i0,")=",i0,", nvell(",i0,")=",i0,", total=",i0,".")') k, ibtype_orig(k), k, nvell(k), sum(nvell(1:k))
      end do
   endif
endif
rewind(iunit)
write(6,'(A)') 'INFO: Finished reading mesh file dimensions.'
return
   !
   ! jump to here on error and end conditions during read   
10 write(6,'("ERROR: Reading line ",i0," gave the following error code: ",i0,".")')         lineNum, ios
   close(iunit)
   stop
20 write(6,'("ERROR: Reached premature end of file on line ",i0,".")') lineNum
   close(iunit)
   stop
!-----+---------+---------+---------+---------+---------+---------+
end subroutine read14_findDims
!-----+---------+---------+---------+---------+---------+---------+


!------------------------------------------------------------------
!                   S U B R O U T I N E    
!        F I N D   M E S H   D I M S   N E T C D F
!------------------------------------------------------------------
! Unfinished routine to read the dimensions of mesh data from
! a netcdf file. See TODO comments at the end of the subroutine.
!------------------------------------------------------------------
subroutine findMeshDimsNetCDF(datafile)
use netcdf
use asgsio, only : nc_id, check
implicit none
character(len=1024), intent(in) :: datafile
integer :: dimPres ! return code from netcdf to determine if the dimension is present in the file
integer :: i
integer :: natt ! number of attributes in the netcdf file
integer :: nvar ! number of variables in the netcdf file
integer :: ndim ! number of dimensions in the netcdf file
integer :: nc_dimid_time ! id of the time dimension
integer :: ncformat ! netcdf3, netcdf4, netcdf4 classic model, etc
!
write(6,'(a)') 'INFO: Reading mesh dimensions from the netCDF file.'
!
! open the netcdf file
call check(nf90_open(trim(datafile), NF90_NOWRITE, nc_id))
!
! determine the type of data stored in the file
call check(nf90_inquire(nc_id, ndim, nvar, natt, &
                        nc_dimid_time, ncformat))
if ( (ncformat.eq.nf90_format_netcdf4).or. &
   (ncformat.eq.nf90_format_netcdf4_classic) ) then
   write(6,'(a)') 'INFO: The data file uses netcdf4 formatting.'
endif
!
call readMeshCommentLineNetCDF()
!
! read the lengths of the dimensions that will always be present in 
! an adcirc netcdf file that contains a mesh
call check(nf90_inq_dimid(nc_id, 'node', nc_dimid_node))
call check(nf90_inquire_dimension(nc_id, nc_dimid_node, len=np))
call check(nf90_inq_dimid(nc_id, 'nele', nc_dimid_nele))
call check(nf90_inquire_dimension(nc_id, nc_dimid_nele, len=ne))

!
! determine which other dimensions are present and find their lengths 
!
! open boundaries
write(6,'(a)') 'INFO: Reading boundary dimensions.'
dimPres = nf90_inq_dimid(nc_id, 'nope', nc_dimid_nope)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, nc_dimid_nope, len=nope))
else 
   nope = 0
endif
! total number of open boundary nodes
dimPres = nf90_inq_dimid(nc_id, 'neta', nc_dimid_neta)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, nc_dimid_neta, len=neta))
else
   neta = 0
endif
! longest open boundary segment
dimPres = nf90_inq_dimid(nc_id, 'max_nvdll', nc_dimid_max_nvdll)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, nc_dimid_max_nvdll, len=nvdll_max))
else
   nvdll_max = 0
endif
! allocate arrays to hold the data
call allocateElevationBoundaryLengths()
call allocateAdcircElevationBoundaryArrays() 
!
! now read in the lengths of the open boundary segments and the node
! numbers on each segment
if (nope.ne.0) then
   call check(nf90_inq_varid(nc_id, 'nvdll', nc_varid_nvdll))
   call check(nf90_get_var(nc_id, nc_varid_nvdll, nvdll, (/ 1 /), (/ nope /) ))
   call check(nf90_inq_varid(nc_id, 'nbdv', nc_varid_nbdv))
   do i=1, nope
      call check(nf90_get_var(nc_id, nc_varid_nbdv, nbdv(i,:), (/ i, 1 /), (/ 1, nvdll(i) /) ))  
   end do
endif
!
! flux boundaries
dimPres = nf90_inq_dimid(nc_id, 'nbou', nc_dimid_nbou)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, nc_dimid_nbou, len=nbou))
else
   nbou = 0
endif
! total number of flux boundary nodes
dimPres = nf90_inq_dimid(nc_id, 'nvel', nc_dimid_nvel)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, nc_dimid_nvel, len=nvel))
else 
   nvel = 0
endif
! longest flux boundary segment
dimPres = nf90_inq_dimid(nc_id, 'max_nvell', nc_dimid_max_nvell)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, nc_dimid_max_nvell, len=nvell_max))
else
   nvell_max = 0
endif
!
call allocateFluxBoundaryLengths()
call allocateAdcircFluxBoundaryArrays()
!
! now read in the lengths of the open boundary segments and the node
! numbers on each segment
if (nbou.ne.0) then
   call check(nf90_inq_varid(nc_id, 'nvell', nc_varid_nvell))
   call check(nf90_get_var(nc_id, nc_varid_nvell, nvell, (/ 1 /), (/ nbou /) ))
   call check(nf90_inq_varid(nc_id, 'ibtype', nc_varid_ibtype))
   call check(nf90_get_var(nc_id, nc_varid_ibtype, ibtype, (/ 1 /) , (/ nbou /) ))
   call check(nf90_inq_varid(nc_id, 'nbvv', nc_varid_nbvv))
   do i=1, nbou
      call check(nf90_get_var(nc_id, nc_varid_nbvv, nbvv(i,:), (/ i, 1 /), (/ 1, nvell(i) /) ))  
   end do
endif
!
! Close the file once the mesh dimensions have been determined and the 
! arrays have been allocated. The actual data are read in the
! subroutine readMeshNetCDF().
call check(nf90_close(nc_id))

write(6,'(a)') 'INFO: Finished reading mesh dimensions from the netCDF file.'

!----------------------------------------------------------------------
end subroutine findMeshDimsNetCDF
!----------------------------------------------------------------------

!------------------------------------------------------------------
!    S U B R O U T I N E     R E A D   M E S H   N E T C D F
!------------------------------------------------------------------
! Read the mesh data (coordinates, bathy/topo, element table).
! The subroutine findMeshDimsNetCDF() must be called before this 
! one. 
! TODO: The NetCDF files written by ADCIRC don't actually contain the
! levee heights, coefficients of sub/supercritical flow, etc. 
!------------------------------------------------------------------
subroutine readMeshNetCDF(datafile)
use netcdf
use asgsio, only : nc_id, check
implicit none
character(len=1024), intent(in) :: datafile
integer :: dimPres ! return code from netcdf to determine if the dimension is present in the file
integer :: i, j ! loop counter
integer :: natt ! number of attributes in the netcdf file
integer :: nvar ! number of variables in the netcdf file
integer :: ndim ! number of dimensions in the netcdf file
integer :: nc_dimid_time ! id of the time dimension
integer :: ncformat ! netcdf3, netcdf4, netcdf4 classic model, etc

integer :: nc_count(2)
integer :: nc_start(2)
!
write(6,'(a)') 'INFO: Reading mesh from the netCDF file.'
call allocateNodalAndElementalArrays()
!
! open the netcdf file
call check(nf90_open(trim(datafile), NF90_NOWRITE, nc_id))
!
! read mesh lon, lat, depth data from the file
nc_count = (/ np, 1 /)
nc_start = (/ 1, 1 /)
call check(nf90_inq_varid(nc_id, 'x', nc_varid_x))
call check(nf90_get_var(nc_id,nc_varid_x,xyd(1,1:np),nc_start,nc_count))     
call check(nf90_inq_varid(nc_id, 'y', nc_varid_y))
call check(nf90_get_var(nc_id,nc_varid_y,xyd(2,1:np),nc_start,nc_count))
call check(nf90_inq_varid(nc_id, 'depth', nc_varid_depth))
call check(nf90_get_var(nc_id,nc_varid_depth,xyd(3,1:np),nc_start,nc_count))
!
! element table
NC_Count = (/ 3, ne /)
call check(nf90_inq_varid(nc_id, 'element', nc_varid_element))
call check(nf90_get_var(nc_id,nc_varid_element,nmnc,nc_start,nc_count))
!
! populate the adcirc-style element table
!
! we must reverse the order of the dimensions because netcdf writes
! them to disk in column major order, according to the way C interprets
! the data, rather than row major order, the way Fortran would interpret
! the data
do i=1, ne
   do j=1, 3
      nm(i,j)= nmnc(j,i)
   end do
end do
!
! open (i.e., elevation specified) boundaries
if (nope.ne.0) then
   nc_count = (/ nope, 1 /)
   call check(nf90_get_var(nc_id,nc_varid_nvdll,nvdll,nc_start,nc_count))
   nc_count = (/ nope, nvdll_max /)   
   call check(nf90_get_var(nc_id,nc_varid_nbdv,nbdv,nc_start,nc_count))
endif
if (nbou.ne.0) then
   nc_count = (/ nbou, 1 /)
   call check(nf90_get_var(nc_id,nc_varid_nvell,nvell,nc_start,nc_count))  
   nc_count = (/ nbou, 1 /)
   call check(nf90_get_var(nc_id,nc_varid_ibtype,ibtype,nc_start,nc_count))     
   nc_count = (/ nbou, nvell_max /)
   call check(nf90_get_var(nc_id,nc_varid_nbvv,nbvv,nc_start,nc_count))     
end if
call check(nf90_close(nc_id))
write(6,'(a)') 'INFO: Mesh has been read from the netCDF file.'

!----------------------------------------------------------------------
end subroutine readMeshNetCDF
!----------------------------------------------------------------------


!------------------------------------------------------------------
!                   S U B R O U T I N E    
!   R E A D   M E S H   C O M M E N T   L I N E   N E T C D F
!------------------------------------------------------------------
! Reads the mesh comment line from the netcdf file, taking into 
! account the fact that this comment line has been written with
! two different variable names at different times by different 
! programs. 
!------------------------------------------------------------------
subroutine readMeshCommentLineNetCDF()
use netcdf
use asgsio, only : nc_id, check
implicit none
! return codes to determine which of the variable names were
! used in writing this file
integer :: agold
integer :: agnew
integer :: ag
agold = nf90_get_att(nc_id,nf90_global,'grid',agrid)
agnew = nf90_get_att(nc_id,nf90_global,'agrid',agrid)
if (agold.EQ.NF90_NOERR) then
   ag = nf90_get_att(nc_id,nf90_global,'grid',agrid)
elseif(agnew.EQ.NF90_NOERR) then
   ag = nf90_get_att(nc_id,nf90_global,'agrid',agrid)
else
   call check(ag)
endif
!----------------------------------------------------------------------
end subroutine readMeshCommentLineNetCDF
!----------------------------------------------------------------------

!-----+---------+---------+---------+---------+---------+---------+
! READ14
!-----+---------+---------+---------+---------+---------+---------+
subroutine read14()
use asgsio
implicit none
integer :: i, j, k, jn, je, nhy
integer, parameter :: iunit = 14
integer :: ios     ! i/o status
integer :: lineNum ! line number currently being read
!
! initialization
nfluxf = 0 
!
if (trim(meshFileName).eq."null") then
   write(6,'(a)',advance='no') "Enter name of the mesh file: "
   read(5,'(A)') meshFileName
endif
!
call read14_findDims()
!
call allocateNodalAndElementalArrays()
call allocateBoundaryArrays()
!
if (verbose.eqv..true.) then 
   write(6,'("Number of elevation specified boundaries (nope): ",i0,".")') nope
   write(6,'("Number of simple flux specified boundaries (0,1,2,etc): ",i0,".")') numSimpleFluxBoundaries
   write(6,'("Number of external flux boundaries (3,etc): ",i0,".")') numExternalFluxBoundaries         
   write(6,'("Number of internal flux boundaries (4,etc): ",i0,".")') numInternalFluxBoundaries
   write(6,'("Number of internal flux boundaries with pipes (5,etc): ",i0,".")') numInternalFluxBoundariesWithPipes
endif

write(6,'(A)') 'INFO: Reading mesh file coordinates, connectivity, and boundary data.'
lineNum = 1
read(unit=iunit,fmt='(a80)',err=10,end=20,iostat=ios) agrid
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) ne, np
lineNum = lineNum + 1
do k = 1, np
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) jn, (xyd(j,k), j=1,3)
   lineNum = lineNum + 1
enddo
do k = 1, ne
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) je, nhy, ( nm(k,j), j = 1, 3 )
   lineNum = lineNum + 1
enddo
!
! populate netcdf-style element table
do i=1, ne
   do j=1, 3
      nmnc(j,i) = nm(i,j)
   end do
end do

read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nope
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) neta
lineNum = lineNum + 1
do k = 1, nope
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvdll(k)
   lineNum = lineNum + 1
   elevationBoundaries(k)%indexNum = k
   do j = 1, nvdll(k)
      read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) elevationBoundaries(k)%nodes(j)
      lineNum = lineNum + 1
   enddo
enddo
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nbou
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvel
lineNum = lineNum + 1
sfCount = 1
efCount = 1
ifCount = 1
ifwpCount = 1      
do k = 1, nbou
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) nvell(k), ibtype_orig(k)
   lineNum = lineNum + 1
   select case(ibtype_orig(k))
   case(0,1,2,10,11,12,20,21,22,30,52)
      simpleFluxBoundaries(sfCount)%indexNum = k
      do j = 1, nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios)  &
            simpleFluxBoundaries(sfCount)%nodes(j)
         lineNum = lineNum + 1
      end do
      sfCount = sfCount + 1
   case(3,13,23)
      externalFluxBoundaries(efCount)%indexNum = k         
      do j = 1, nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) & 
                       externalFluxBoundaries(efCount)%nodes(j), &
                       externalFluxBoundaries(efCount)%barlanht(j), &
                       externalFluxBoundaries(efCount)%barlancfsp(j)
         lineNum = lineNum + 1
      end do
      efCount = efCount + 1
   case(4,24)
      internalFluxBoundaries(ifCount)%indexNum = k
      do j = 1, nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) &
                       internalFluxBoundaries(ifCount)%nodes(j), &
                       internalFluxBoundaries(ifCount)%ibconn(j), &
                       internalFluxBoundaries(ifCount)%barinht(j), &
                       internalFluxBoundaries(ifCount)%barincfsb(j), &
                       internalFluxBoundaries(ifCount)%barincfsp(j)
         lineNum = lineNum + 1
      end do
      ifCount = ifCount + 1
   case(5,25)
      internalFluxBoundaries(ifwpCount)%indexNum = k
      do j = 1, nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) &
                       internalFluxBoundariesWithPipes(ifCount)%nodes(j), &
                       internalFluxBoundariesWithPipes(ifCount)%ibconn(j), &
                       internalFluxBoundariesWithPipes(ifCount)%barinht(j), &
                       internalFluxBoundariesWithPipes(ifCount)%barincfsb(j), &
                       internalFluxBoundariesWithPipes(ifCount)%barincfsp(j), &
                       internalFluxBoundariesWithPipes(ifCount)%pipeht(j), &
                       internalFluxBoundariesWithPipes(ifCount)%pipecoef(j), &
                       internalFluxBoundariesWithPipes(ifCount)%pipediam(j)
         lineNum = lineNum + 1                                           
      end do
      ifwpCount = ifwpCount + 1
   case default
      write(6,*) 'ERROR: IBTYPE ',ibtype_orig(k),' is not allowed.'
      stop
   end select
end do
close(14)
! 
! initialize ibtype array
ibtype = ibtype_orig
!
write(6,'(A)') 'INFO: Finished reading mesh file coordinates, connectivity, and boundary data.'
return
      !
      ! jump to here on error and end conditions during read   
10    write(6,'("ERROR: Reading line ",i0," gave the following error code: ",i0,".")')         lineNum, ios
      close(iunit)
      stop
20    write(6,'("ERROR: Reached premature end of file on line ",i0,".")') lineNum
      close(iunit)
      stop
!-----+---------+---------+---------+---------+---------+---------+
end subroutine read14
!-----+---------+---------+---------+---------+---------+---------+

!------------------------------------------------------------------
!                      S U B R O U T I N E    
! A L L O C A T E   N O D A L   A N D   E L E M E N T A L   A R R A Y S
!------------------------------------------------------------------
! Mesh related memory allocation for any array that is
! dimensioned by the number of nodes in the mesh or the number of 
! elements in the mesh. Mirrors the subroutine of the same name in 
! the mesh module in adcirc.
!------------------------------------------------------------------
subroutine allocateNodalAndElementalArrays()
implicit none
allocate(xyd(3,np))
allocate(nm(ne,3))
allocate(nmnc(3,ne))
!
! initialize to something troublesome to make it easy to spot issues
xyd = -99999.d0
nm = 0
!------------------------------------------------------------------
end subroutine allocateNodalAndElementalArrays
!------------------------------------------------------------------

!------------------------------------------------------------------
!                   S U B R O U T I N E  
! A L L O C A T E   E L E V A T I O N   B O U N D A R Y   L E N G T H S
!------------------------------------------------------------------
! Allocate the arrays that hold the number of nodes on each elevation
! boundary segment
!------------------------------------------------------------------
subroutine allocateElevationBoundaryLengths()
implicit none
allocate(nvdll(nope)) ! number of nodes on each elevation boundary segment
allocate(ibtypee(nope)) ! type of each elevation boundary segment
!
! initialize to something troublesome to make it easy to spot issues
ibtypee = -99999
nvdll = -99999
!------------------------------------------------------------------
end subroutine allocateElevationBoundaryLengths
!------------------------------------------------------------------

!------------------------------------------------------------------
!                   S U B R O U T I N E  
!      A L L O C A T E  F L U X  B O U N D A R Y   L E N G T H S
!------------------------------------------------------------------
! Allocate the arrays that hold the number of nodes (primary nodes 
! in the case of paired node boundaries like levees) on each flux
! boundary segment
!------------------------------------------------------------------
subroutine allocateFluxBoundaryLengths()
implicit none

allocate(nvell(nbou)) ! number of nodes on each flux boundary segment
allocate(ibtype_orig(nbou))
allocate(ibtype(nbou))
!
! initialize to something troublesome to make it easy to spot issues

nvell = -99999
ibtype_orig = -99999
ibtype = -99999
!------------------------------------------------------------------
end subroutine allocateFluxBoundaryLengths
!------------------------------------------------------------------

!------------------------------------------------------------------
!                   S U B R O U T I N E  
!      A L L O C A T E   A D C I R C  E L E V A T I O N  
!               B O U N D A R Y  A R R A Y S
!------------------------------------------------------------------
! Allocate space for elevation boundary-related variables
!------------------------------------------------------------------
subroutine allocateAdcircElevationBoundaryArrays()
implicit none
allocate(nbdv(nope,nvdll_max))
allocate(nbd(neta))
!
! initialize to something troublesome to make it easy to spot issues
nbdv = -99999
nbd = -99999
!------------------------------------------------------------------
end subroutine allocateAdcircElevationBoundaryArrays
!------------------------------------------------------------------

!------------------------------------------------------------------
!                       S U B R O U T I N E  
!              A L L O C A T E  A D C I R C   F L U X  
!                   B O U N D A R Y  A R R A Y S
!------------------------------------------------------------------
!     jgf51.21.11 Allocate space for flux boundary-related variables
!------------------------------------------------------------------
subroutine allocateAdcircFluxBoundaryArrays()
implicit none
allocate ( nbv(nvel),lbcodei(nvel))
allocate ( nbvv(nbou,nvell_max))
!
! jgf20150723: These are never used so I commented them out because
! they are memory hogs.
!allocate ( barlanht(nbou,nvel),barlancfsp(nbou,nvel))
!allocate ( barinht(nbou,nvel),barincfsb(nbou,nvel),barincfsp(nbou,nvel))
!allocate ( pipeht(nbou,nvel),pipecoef(nbou,nvel),pipediam(nbou,nvel))
!allocate ( ibconn(nbou,nvel))
! kmd - added for rivers in baroclinic simulation
!allocate (bcrnbvv(nbou,0:nvel))
!allocate (bcrnvell(nbou))

!
! initialize to something troublesome to make it easy to spot issues
nbv = -99999
lbcodei = -99999
nbvv = -99999
!
! jgf20150723: These are never used so I commented them out because
! they are memory hogs.
!barlanht = -99999.d0
!barlancfsp = -99999.d0
!barinht = -99999.d0
!barincfsb = -99999.d0
!barincfsp = -99999.d0
!pipeht = -99999.d0
!pipecoef = -99999.d0
!pipediam = -99999.d0
!ibconn = -99999

!bcrnbvv = -99999
!bcrnvell = -99999
!------------------------------------------------------------------
end subroutine allocateAdcircFluxBoundaryArrays
!------------------------------------------------------------------

!------------------------------------------------------------------
!                   S U B R O U T I N E  
!        A L L O C A T E  B O U N D A R Y  A R R A Y S
!------------------------------------------------------------------
! Allocate space for boundary-related variables
!------------------------------------------------------------------
subroutine allocateBoundaryArrays()
implicit none
integer :: i
!
allocate(elevationBoundaries(nope))
do i=1,nope
   allocate(elevationBoundaries(i)%nodes(nvdll(i)))
end do   
allocate(simpleFluxBoundaries(numSimpleFluxBoundaries))
allocate(externalFluxBoundaries(numExternalFluxBoundaries))
allocate(internalFluxBoundaries(numInternalFluxBoundaries))
allocate(internalFluxBoundariesWithPipes(numInternalFluxBoundariesWithPipes))
sfCount = 1
efCount = 1
ifCount = 1
ifwpCount = 1      
do i=1,nbou
   if (verbose.eqv..true.) then
      write(6,'("i=",i0)') i
   endif
   select case(ibtype_orig(i))
   case(0,1,2,10,11,12,20,21,22,30,52)
      allocate(simpleFluxBoundaries(sfCount)%nodes(nvell(i)))
      sfCount = sfCount + 1
   case(3,13,23)
      allocate(externalFluxBoundaries(efCount)%nodes(nvell(i)))
      allocate(externalFluxBoundaries(efCount)%barlanht(nvell(i)))
      allocate(externalFluxBoundaries(efCount)%barlancfsp(nvell(i)))
      efCount = efCount + 1
   case(4,24)        
      allocate(internalFluxBoundaries(ifCount)%nodes(nvell(i)))
      allocate(internalFluxBoundaries(ifCount)%ibconn(nvell(i)))
      allocate(internalFluxBoundaries(ifCount)%barinht(nvell(i)))
      allocate(internalFluxBoundaries(ifCount)%barincfsb(nvell(i)))
      allocate(internalFluxBoundaries(ifCount)%barincfsp(nvell(i)))
      ifCount = ifCount + 1
   case(5,25)
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%nodes(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%ibconn(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%barinht(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%barincfsb(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%barincfsp(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%pipeht(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%pipecoef(nvell(i)))
      allocate(internalFluxBoundariesWithPipes(ifwpCount)%pipediam(nvell(i)))
      ifwpCount = ifwpCount + 1            
   case default
       write(6,'("ERROR: The boundary type ",i0," was found in the file but is not valid.")') ibtype_orig(i)
       stop
   end select
end do
! initialize to something troublesome to make it easy to spot issues
do i=1,nope
   elevationBoundaries(i)%nodes(:) = -99999
end do
do i=1,numSimpleFluxBoundaries
   simpleFluxBoundaries(i)%nodes(:) = -99999
end do
do i=1,numExternalFluxBoundaries
   externalFluxBoundaries(i)%nodes(:) = -99999
   externalFluxBoundaries(i)%barlanht(:) = -99999.d0
   externalFluxBoundaries(i)%barlancfsp(:) = -99999.d0
end do
do i=1,numInternalFluxBoundaries
   internalFluxBoundaries(i)%nodes(:) = -99999
   internalFluxBoundaries(i)%ibconn(:) = -99999
   internalFluxBoundaries(i)%barinht(:) = -99999.d0
   internalFluxBoundaries(i)%barincfsb(:) = -99999.d0
   internalFluxBoundaries(i)%barincfsp(:) = -99999.d0
end do
do i=1,numInternalFluxBoundariesWithPipes
   internalFluxBoundariesWithPipes(i)%nodes(:) = -99999
   internalFluxBoundariesWithPipes(i)%ibconn(:) = -99999
   internalFluxBoundariesWithPipes(i)%barinht(:) = -99999.d0
   internalFluxBoundariesWithPipes(i)%barincfsb(:) = -99999.d0
   internalFluxBoundariesWithPipes(i)%barincfsp(:) = -99999.d0
   internalFluxBoundariesWithPipes(i)%pipeht(:) = -99999.d0
   internalFluxBoundariesWithPipes(i)%pipecoef(:) = -99999.d0
   internalFluxBoundariesWithPipes(i)%pipediam(:) = -99999.d0
end do

!------------------------------------------------------------------
end subroutine allocateBoundaryArrays
!------------------------------------------------------------------

!-----+---------+---------+---------+---------+---------+---------+
!                   S U B R O U T I N E     
!  C O N S T R U C T   F L U X   B O U N D A R I E S   A R R A Y
!-----+---------+---------+---------+---------+---------+---------+
subroutine constructFluxBoundaryTypesArray()
implicit none
integer :: i
integer :: j
integer :: k
integer :: jgw
!
jgw = 1
do k=1,nbou
   do j=1,nvell(k)
      lbcodei(jgw) = ibtype(k)
      jgw = jgw + 1
   end do
end do
! determine if there are any specified flux boundaries in the mesh
do i=1,size(specifiedFluxBoundaryTypes)
   if (any(lbcodei.eq.specifiedFluxBoundaryTypes(i))) then
      nfluxf = 1 ! => must find b.c.s in fort.15 or fort.20
      exit
   endif
end do
!-----+---------+---------+---------+---------+---------+---------+
end subroutine constructFluxBoundaryTypesArray
!-----+---------+---------+---------+---------+---------+---------+

!------------------------------------------------------------------
!   S U B R O U T I N E   W R I T E   M E S H 
!------------------------------------------------------------------
!  Writes the mesh file data to adcirc ascii fort.14 format. 
!------------------------------------------------------------------
subroutine writeMesh()
implicit none
integer :: i, j, k, jn, je, nhy
integer, parameter :: iunit = 14
integer :: ios     ! i/o status
integer :: lineNum ! line number currently being read
!
! initialization
nfluxf = 0 
nhy = 3
!
if (trim(meshFileName).eq."null") then
   write(6,'(a)',advance='no') "Enter name of the mesh file: "
   read(5,'(a)') meshFileName
endif
open(unit=iunit,file=trim(meshFileName),status='replace',action='write')
if (verbose.eqv..true.) then 
   write(6,'("Number of elevation specified boundaries (NOPE): ",i0,".")') nope
   write(6,'("Number of simple flux specified boundaries (0,1,2,etc): ",i0,".")') numSimpleFluxBoundaries
   write(6,'("Number of external flux boundaries (3,etc): ",i0,".")') numExternalFluxBoundaries         
   write(6,'("Number of internal flux boundaries (4,etc): ",i0,".")') numInternalFluxBoundaries
   write(6,'("Number of internal flux boundaries with pipes (5,etc): ",i0,".")') numInternalFluxBoundariesWithPipes
endif
write(6,'(a)') 'INFO: Writing node table to "' // trim(meshFileName) // '".'
lineNum = 1
write(unit=iunit,fmt='(a80)',err=10,iostat=ios) agrid
lineNum = lineNum + 1
write(unit=iunit,fmt='(2(i0,1x),a)',err=10,iostat=ios) ne, np, &
  '! number of elements (ne), number of nodes (np)'
lineNum = lineNum + 1
write(unit=iunit,fmt='(i0,3(1x,f15.7),a)',err=10,iostat=ios) &
   1, (xyd(j,1), j=1,3), ' ! node table: node number, x, y, depth'
lineNum = lineNum + 1
do k = 2, np
   write(unit=iunit,fmt='(i0,3(1x,f15.7))',err=10,iostat=ios) &
      k, (xyd(j,k), j=1,3)
   lineNum = lineNum + 1
enddo
write(6,'(a)') 'INFO: Writing element table to ' // trim(meshFileName) // '".'
write(unit=iunit,fmt='(5(i0,2x),a)',err=10,iostat=ios) 1, nhy, ( nm(1,j), j = 1, 3 ), &
   '! element table : element number, number of nodes per element, node numbers counter clockwise around the element ' 
lineNum = lineNum + 1
do k = 2, ne
   write(unit=iunit,fmt='(5(i0,2x))',err=10,iostat=ios) k, nhy, ( nm(k,j), j = 1, 3 )
   lineNum = lineNum + 1
enddo
write(6,'(a)') 'INFO: Writing boundaries to '  // trim(meshFileName) // '".'
write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) nope, &
   ' ! total number of elevation specified boundary segments (nope)'  
lineNum = lineNum + 1
write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) neta, &
   ' ! total number of nodes on elevation specified boundaries (neta) (consistency check)' 
lineNum = lineNum + 1
do k = 1, nope
   write(unit=iunit,fmt='(i0,1x,i0,a,i0)',err=10,iostat=ios) nvdll(k), 0, &
   ' ! number of nodes and boundary type of elevation specified boundary (nvdll, ibtypee) segment number ', k 
   lineNum = lineNum + 1
   write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) elevationBoundaries(k)%nodes(1), &
      ' ! list of nodes on this elevation specified boundary segment (nbdv)'
   lineNum = lineNum + 1
   do j = 2, nvdll(k)
      write(unit=iunit,fmt='(i0)',err=10,iostat=ios) elevationBoundaries(k)%nodes(j)
      lineNum = lineNum + 1
   enddo
enddo
write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) nbou, &
   ' ! total number of flux boundary segments (nbou)'
lineNum = lineNum + 1
write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) nvel, &
   ' ! total number of nodes on flux boundaries (nvel) (consistency check)'
lineNum = lineNum + 1
sfCount = 1
efCount = 1
ifCount = 1
ifwpCount = 1      
do k = 1, nbou
   write(unit=iunit,fmt='(2(i0,2x),a)',err=10,iostat=ios) nvell(k), ibtype_orig(k), &
      '! number of nodes and boundary type of flux boundary (nvell, ibtype)'
   lineNum = lineNum + 1
   select case(ibtype_orig(k))
   case(0,1,2,10,11,12,20,21,22,30,52)
      write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios)  &
         simpleFluxBoundaries(sfCount)%nodes(1), ' ! nodes on this boundary (nbvv)' 
         lineNum = lineNum + 1
      do j = 2, nvell(k)
         write(unit=iunit,fmt='(i0)',err=10,iostat=ios)  &
            simpleFluxBoundaries(sfCount)%nodes(j)
         lineNum = lineNum + 1
      end do
      sfCount = sfCount + 1
   case(3,13,23)
      write(unit=iunit,fmt='(i0,1x,f8.3,1x,f8.3,a)',err=10,iostat=ios) & 
                       externalFluxBoundaries(efCount)%nodes(1), &
                       externalFluxBoundaries(efCount)%barlanht(1), &
                       externalFluxBoundaries(efCount)%barlancfsp(1), &
      ' ! boundary node (nbvv), barrier height (barlanht), and coef of. supercritical flow (barlancfsp)'
      lineNum = lineNum + 1
      do j = 2, nvell(k)
         write(unit=iunit,fmt='(i0,1x,f8.3,1x,f8.3)',err=10,iostat=ios) & 
                       externalFluxBoundaries(efCount)%nodes(j), &
                       externalFluxBoundaries(efCount)%barlanht(j), &
                       externalFluxBoundaries(efCount)%barlancfsp(j)
         lineNum = lineNum + 1
      end do
      efCount = efCount + 1
   case(4,24)
      write(unit=iunit,fmt=*,err=10,iostat=ios) &
                       internalFluxBoundaries(ifCount)%nodes(1), &
                       internalFluxBoundaries(ifCount)%ibconn(1), &
                       internalFluxBoundaries(ifCount)%barinht(1), &
                       internalFluxBoundaries(ifCount)%barincfsb(1), &
                       internalFluxBoundaries(ifCount)%barincfsp(1), &
      ' ! boundary node (nbvv), connected backface node (ibconn), ' // &
      'barrier height (barinht), coef. of subcrit. flow (barincfsb), ' // &
      'coef. of supercrit. flow (barincfsp) '
      lineNum = lineNum + 1
      do j = 2, nvell(k)
         write(unit=iunit,fmt='(2(i0,1x),3(f8.3,1x))',err=10,iostat=ios) &
                       internalFluxBoundaries(ifCount)%nodes(j), &
                       internalFluxBoundaries(ifCount)%ibconn(j), &
                       internalFluxBoundaries(ifCount)%barinht(j), &
                       internalFluxBoundaries(ifCount)%barincfsb(j), &
                       internalFluxBoundaries(ifCount)%barincfsp(j)
         lineNum = lineNum + 1
      end do
      ifCount = ifCount + 1
   case(5,25)
      write(unit=iunit,fmt='(2(i0,1x),6(f8.3),a)',err=10,iostat=ios) &
                    internalFluxBoundariesWithPipes(ifCount)%nodes(1), &
                    internalFluxBoundariesWithPipes(ifCount)%ibconn(1), &
                    internalFluxBoundariesWithPipes(ifCount)%barinht(1), &
                    internalFluxBoundariesWithPipes(ifCount)%barincfsb(1), &
                    internalFluxBoundariesWithPipes(ifCount)%barincfsp(1), &
                    internalFluxBoundariesWithPipes(ifCount)%pipeht(1), &
                    internalFluxBoundariesWithPipes(ifCount)%pipecoef(1), &
                    internalFluxBoundariesWithPipes(ifCount)%pipediam(1), &
      ' ! boundary node (nbvv), connected backface node (ibconn), ' // &
      'barrier height (barinht), coef. of subcrit. flow (barincfsb), ' // &
      'coef. of supercrit. flow (barincfsp), pipe height (pipeht), ' // &
      ' pipe coef. (pipecoef), pipediameter(pipediam)' 
      lineNum = lineNum + 1                                              
      do j = 2, nvell(k)
         write(unit=iunit,fmt='(2(i0,1x),6(f8.3))',err=10,iostat=ios) &
                       internalFluxBoundariesWithPipes(ifCount)%nodes(j), &
                       internalFluxBoundariesWithPipes(ifCount)%ibconn(j), &
                       internalFluxBoundariesWithPipes(ifCount)%barinht(j), &
                       internalFluxBoundariesWithPipes(ifCount)%barincfsb(j), &
                       internalFluxBoundariesWithPipes(ifCount)%barincfsp(j), &
                       internalFluxBoundariesWithPipes(ifCount)%pipeht(j), &
                       internalFluxBoundariesWithPipes(ifCount)%pipecoef(j), &
                       internalFluxBoundariesWithPipes(ifCount)%pipediam(j)
         lineNum = lineNum + 1                                           
      end do
      ifwpCount = ifwpCount + 1
   case default
      write(6,'("ERROR: IBTYPE ",i0," is not allowed.")') ibtype_orig(k)
      stop
   end select
end do
close(14)
write(6,'(a)') 'INFO: Finished writing ascii mesh file.'
return
      !
      ! jump to here on error and end conditions during read   
10    write(6,'("ERROR: The error code ",i0," occurred when writing line ",i0,".")')         ios, lineNum
      close(iunit)
      stop
!------------------------------------------------------------------
end subroutine writeMesh
!------------------------------------------------------------------

!----------------------------------------------------------------------
!                  S U B R O U T I N E   
!     W R I T E   M E S H   D E F I N I T I O N S  T O   N E T C D F 
!----------------------------------------------------------------------
!     This subroutine writes the mesh parameters to the netcdf file. 
!----------------------------------------------------------------------
subroutine writeMeshDefinitionsToNetCDF(nc_id, fileFormat)
use netcdf
use asgsio, only : check, NETCDF4
implicit none
integer, intent(in) :: nc_id
integer, intent(in) :: fileFormat
integer              :: NC_DimID_single
!
! create and store mesh dimensions 
CALL Check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,'agrid',trim(agrid)))
CALL Check(NF90_DEF_DIM(NC_ID,'node',np,NC_DimID_node))
CALL Check(NF90_DEF_DIM(NC_ID,'nele',ne,NC_DimID_nele))
CALL Check(NF90_DEF_DIM(NC_ID,'nvertex',3,NC_DimID_nvertex))
CALL Check(NF90_DEF_DIM(NC_ID,'single',1,NC_DimID_single))

if (nope.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'nope',nope,NC_DimID_nope))
if (nvdll_max.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'max_nvdll',nvdll_max,NC_DimID_max_nvdll))
if (neta.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'neta',neta,NC_DimID_neta))
if (nbou.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'nbou',nbou,NC_DimID_nbou))
if (nvel.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'nvel',nvel,NC_DimID_nvel))
if (nvell_max.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'max_nvell',nvell_max,NC_DimID_max_nvell))

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

! automatically turn on compression if it is available
if (fileFormat.eq.NETCDF4) then
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
      use asgsio, only : check
      implicit none
      integer, intent(in) :: nc_id
      integer :: nc_count(2)
      integer :: nc_start(2)
      integer :: i, j
     
      ! place mesh-related data into the file
      NC_Count = (/ np, 1 /)
      NC_Start = (/ 1, 1 /)
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_x,xyd(1,1:np),NC_Start,NC_Count))
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_y,xyd(2,1:np),NC_Start,NC_Count))
      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_depth,xyd(3,1:np),NC_Start,NC_Count))
      NC_Count = (/ 3, ne /)

      CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_element,nmnc,NC_Start,NC_Count))
      
      if (nope.ne.0) then
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nope,nope))
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_max_nvell,nvell_max))
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_max_nvdll,nvdll_max))
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_neta,neta))      
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nvel,nvel))            
         NC_Count = (/ nope, 1 /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nvdll,nvdll,NC_Start,NC_Count))
         NC_Count = (/ nope, nvdll_max /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nbdv,nbdv,NC_Start,NC_Count))
      endif
      if (nbou.ne.0) then
         NC_Count = (/ nbou, 1 /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nvell,nvell,NC_Start,NC_Count))
         NC_Count = (/ nbou, 1 /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_ibtype,ibtype,NC_Start,NC_Count))
         NC_Count = (/ nbou, nvell_max /)
         CALL Check(NF90_PUT_VAR(NC_ID,NC_VarID_nbvv,nbvv,NC_Start,NC_Count))
      end if

      write(6,'(a)') 'INFO: Mesh has been written to the netCDF file.'
!----------------------------------------------------------------------
      end subroutine writeMeshDataToNetCDF
!----------------------------------------------------------------------


!******************************************************************************
!                                                                             *
!      Subroutine to generate neighbor tables from a connectivity table.      *
!                                                                             *
!      NOTES                                                                  *
!      a node neighbor table is generated with the node itself is listed as   *
!         neighbor #1 and all other neighbors are sorted and placed in cw     *
!         order from east                                                     *
!      a neighbor element table is generated with:                            *
!         entry 1 = element # defined by neighbors 1,2,3                      *
!         entry 2 = element # defined by neighbors 1,3,4                      *
!         entry 3 = element # defined by neighbors 1,4,5                      *
!          .......                                                            *
!         entry last = element # defined by neighbors 1,nneigh,2              *
!         a zero area means that the defined triangle lies outside the domain *
!                                                                             *
!                                                                             *
!    v1.0   R.L.   6/29/99  used in 3D code                                   *
!    v2.0   R.L.   5/23/02  adapted to provide neighbor el table              *
!******************************************************************************
!                                                                             *
!     -  PARAMETERS WHICH MUST BE SET TO CONTROL THE DIMENSIONING OF ARRAYS   *
!           ARE AS FOLLOWS:                                                   *
!                                                                             *
!          MNP = MAXIMUM NUMBER OF NODAL POINTS                               *
!          MNE = MAXIMUM NUMBER OF ELEMENTS                                   *
!          MNEI= 1+MAXIMUM NUMBER OF NODES CONNECTED TO ANY ONE NODE IN THE   *
!                  FINITE ELEMENT GRID                                        *
!                                                                             *
!******************************************************************************
!                                                                             *
!    VARIABLE DEFINITIONS:                                                    *
!       NE - NUMBER OF ELEMENTS                                               *
!       NP - NUMBER OF NODES                                                  *
!       NM(MNE,3) - NODE NUMBERS ASSOCIATED WITH EACH ELEMENT                 *
!       NNeigh(MNP) NUMBER OF NEIGHBORS FOR EACH NODE                         *
!       NeiTab(MNP,NEIMAX) 2D ARRAY OF NEIGHBORS FOR EACH NODE                *
!       NeiTabEle(MNP,NEIMAX) 2D ARRAY OF NEIGHBOR ELEMENTS FOR EACH NODE     *
!       NEIMIN - 1+MINIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!       NEIMAX - 1+MAXIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!                                                                             *
!******************************************************************************
subroutine computeNeighborTable()
implicit none
double precision, allocatable :: angle(:)
integer, allocatable :: neitem(:)
integer, allocatable :: nneighele(:)
double precision :: anglelow
double precision :: anglemore
double precision :: delx
double precision :: dely
double precision :: dist
integer :: nn1, nn2, nn3 ! node numbers around an element
integer :: ne1, ne2, ne3 ! element numbers
integer :: i, j, jj, jlow, k, n  ! loop counters
!
! Initialization
if (cppComputed.eqv..false.) then
   call computeCPP()
endif
jlow = 0
ne2 = 0
ne3 = 0
!
! For interior nodes, the number of neighbor nodes around any node is
! equal to the number of elements that contain that node. Boundary nodes
! will have one additional neighboring node.
allocate(nneigh(np))
nneigh = 0
do i=1,ne
   do j=1,3
      ! count the number of elements that include each node
      nneigh(nm(i,j)) = nneigh(nm(i,j)) + 1 
   end do
end do
mnei = maxval(nneigh)
mnei = mnei + 2 ! +1 to include the node itself, +1 in case its a boundary node
!
allocate(neitab(np,mnei))
allocate(neitabele(np,mnei))
allocate(angle(mnei))
allocate(neitem(np))
allocate(nneighele(np))
neighborTableComputed = .true.
! initialize neighbor table to zeroes
NNeigh=0
NNeighEle=0
NeiTab=0
NeiTabEle=0
DO 10 N=1,NE
   NN1 = NM(N,1)
   NN2 = NM(N,2)
   NN3 = NM(N,3)
   NNeighEle(NN1)=NNeighEle(NN1)+1
   NNeighEle(NN2)=NNeighEle(NN2)+1
   NNeighEle(NN3)=NNeighEle(NN3)+1
   NeiTabEle(NN1,NNeighEle(NN1))=N
   NeiTabEle(NN2,NNeighEle(NN2))=N
   NeiTabEle(NN3,NNeighEle(NN3))=N

   DO J=1,NNeigh(NN1)
      IF(NN2.EQ.NeiTab(NN1,J)) GOTO 25
   END DO
   NNeigh(NN1)=NNeigh(NN1)+1
   NNeigh(NN2)=NNeigh(NN2)+1
   IF((NNeigh(NN1).GT.MNEI-1).OR.(NNeigh(NN2).GT.MNEI-1)) GOTO 999
   NeiTab(NN1,NNeigh(NN1))=NN2
   NeiTab(NN2,NNeigh(NN2))=NN1

25      CONTINUE
   DO J=1,NNeigh(NN1)
      IF(NN3.EQ.NeiTab(NN1,J)) GOTO 35
   END DO
   NNeigh(NN1)=NNeigh(NN1)+1
   NNeigh(NN3)=NNeigh(NN3)+1
   IF((NNeigh(NN1).GT.MNEI-1).OR.(NNeigh(NN3).GT.MNEI-1)) GOTO 999
   NeiTab(NN1,NNeigh(NN1))=NN3
   NeiTab(NN3,NNeigh(NN3))=NN1

35      CONTINUE
   DO J=1,NNeigh(NN2)
      IF(NN3.EQ.NeiTab(NN2,J)) GOTO 10
   END DO
   NNeigh(NN2)=NNeigh(NN2)+1
   NNeigh(NN3)=NNeigh(NN3)+1
   IF((NNeigh(NN2).GT.MNEI-1).OR.(NNeigh(NN3).GT.MNEI-1)) GOTO 999
   NeiTab(NN2,NNeigh(NN2))=NN3
   NeiTab(NN3,NNeigh(NN3))=NN2

10   CONTINUE
!
!     INSERT NODE ITSELF IN PLACE #1 and SORT other NEIGHBORS by
!     increasing cw angle from East
!
DO I=1,NP
   DO J=1,NNeigh(I)
      NEITEM(J)=NeiTab(I,J)
      DELX=x_cpp(NEITEM(J))-x_cpp(I)
      DELY=y_cpp(NEITEM(J))-y_cpp(I)
      DIST=SQRT(DELX*DELX+DELY*DELY)
      IF(DIST.EQ.0.0d0) GOTO 998
      IF(DELY.NE.0.0d0) THEN
         ANGLE(J)=RAD2DEG*ACOS(DELX/DIST)
         IF(DELY.GT.0.0) ANGLE(J)=360.0d0-ANGLE(J)
      ENDIF
      IF(DELY.EQ.0.0d0) THEN
         IF(DELX.GT.0.0d0) ANGLE(J)=0.0d0
         IF(DELX.LT.0.d0) ANGLE(J)=180.0d0
      ENDIF
   END DO
   ANGLEMORE=-1.d0
   DO JJ=1,NNeigh(I)
      ANGLELOW=400.d0
      DO J=1,NNeigh(I)
         IF((ANGLE(J).LT.ANGLELOW).AND.(ANGLE(J).GT.ANGLEMORE)) THEN
            ANGLELOW=ANGLE(J)
            JLOW=J
         ENDIF
      END DO
      NeiTab(I,JJ+1)=NEITEM(JLOW)
      ANGLEMORE=ANGLELOW
   END DO
   NeiTab(I,1)=I
   NNeigh(I)=NNeigh(I)+1
ENDDO
!
!     MATCH EACH SET OF 3 NODES WITH CORRESPONDING ELEMENT AND REORDER
!     ELEMENTS ACCORDINGLY
!
DO I=1,NP
   DO K=1,NNeighEle(I)
      NEITEM(K)=NeiTabEle(I,K)
      NeiTabEle(I,K)=0
   END DO
   DO J=2,NNeigh(I)
      NN1=NeiTab(I,1)
      NN3=NeiTab(I,J)
      IF(J.NE.NNeigh(I)) NN2=NeiTab(I,J+1)
      IF(J.EQ.NNeigh(I)) NN2=NeiTab(I,2)
      DO K=1,NNeighEle(I)
         IF(NEITEM(K).NE.0) THEN
            IF(NM(NEITEM(K),1).EQ.NN1) THEN
               NE1=NM(NEITEM(K),1)
               NE2=NM(NEITEM(K),2)
               NE3=NM(NEITEM(K),3)
            ENDIF
            IF(NM(NEITEM(K),2).EQ.NN1) THEN
               NE1=NM(NEITEM(K),2)
               NE2=NM(NEITEM(K),3)
               NE3=NM(NEITEM(K),1)
            ENDIF
            IF(NM(NEITEM(K),3).EQ.NN1) THEN
               NE1=NM(NEITEM(K),3)
               NE2=NM(NEITEM(K),1)
               NE3=NM(NEITEM(K),2)
            ENDIF
            IF((NE2.EQ.NN2).AND.(NE3.EQ.NN3)) THEN
               NeiTabEle(I,J-1)=NEITEM(K)
               NEITEM(K)=0
            ENDIF
         ENDIF
      END DO
   END DO
END DO
!
!  DETERMINE THE MAXIMUM AND MINIMUM NUMBER OF NEIGHBORS
NEIMAX = maxval(NNeigh)
NEIMIN = minval(NNeigh)
!  Deallocate local work arrays
DEALLOCATE ( ANGLE )
DEALLOCATE ( NEITEM )
DEALLOCATE ( NNEIGHELE )
RETURN

999  CONTINUE
WRITE(6,*) 'ERROR: Computation of neighbor table failed.'
STOP
998  CONTINUE
WRITE(6,*) 'ERROR: Nodes ',I,' and ',NEITEM(J),' have the same coordinates.'
STOP
!-----------------------------------------------------------------------
END SUBROUTINE computeNeighborTable
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                         S U B R O U T I N E   
!   C O M P U T E   N E I G H B O R   E D G E   L E N G T H   T A B L E 
!-----------------------------------------------------------------------
!     jgf: Very short subroutine to compute the length of each edge
!     attached to a node. 
!-----------------------------------------------------------------------
subroutine computeNeighborEdgeLengthTable()
implicit none
integer i,j
allocate(neighborEdgeLengthTable(np,neimax))
neighborEdgeLengthTableComputed = .true.
do i=1,np
   neighborEdgeLengthTable(i,1) = 0.d0 ! distance from this node to itself...
   do j=2,nneigh(i)
      neighborEdgeLengthTable(i,j) = sqrt( (x_cpp(i)-x_cpp(NeiTab(i,j)))**2 &
                  + (y_cpp(i)-y_cpp(NeiTab(i,j)))**2 )
   end do
end do
!-----------------------------------------------------------------------
end subroutine computeNeighborEdgeLengthTable
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                         S U B R O U T I N E   
!     C O M P U T E  A L B E R S   E Q U A L   A R E A   C O N I C
!-----------------------------------------------------------------------
!     jgf: Very short subroutine to compute the albers equal area
!     conic projection of the mesh node locations, allocating memory 
!     in the process if necessary, and not overwriting the 
!     original lat/lon data.
!-----------------------------------------------------------------------
SUBROUTINE computeAlbersEqualAreaConic()
IMPLICIT NONE
real(8), parameter :: PHI1 = 29.5d0
real(8), parameter :: PHI2 = 45.5d0       
real(8), parameter :: LON0 = -96.0d0                 
real(8), parameter :: LAT0 = 23.0d0                 
real(8), parameter :: EE = 0.0066943800229d0
real(8), parameter :: E = 0.0818191910428d0 
! 
! variables used in conversion of adcirc mesh point locations
! (in geographic projection) to Albers Equal Area Conic
real(8) :: alpha
real(8) :: alpha0
real(8) :: alpha1
real(8) :: alpha2
real(8) :: alphaa
real(8) :: alphab
real(8) :: alphac
real(8) :: c
real(8) :: e1
real(8) :: m1
real(8) :: m2
real(8) :: o
real(8) :: p
real(8) :: rho
real(8) :: rho0
real(8) :: theta
real(8) :: n
integer :: i ! loop counter
!
if (albersComputed.eqv..false.) then
   allocate(xalbers(np),yalbers(np))
   albersComputed = .true.
endif
write(6,'("INFO: Generating Albers Equal Area Conic coordinates.")')
e1 = 1.d0-ee                                            
o = (0.5d0/e)*e1
m1 = cos(phi1*deg2rad)/(1.d0-ee*sin(phi1*deg2rad)**2)**0.5d0
m2 = cos(phi2*deg2rad)/(1.d0-ee*sin(phi2*deg2rad)**2)**0.5d0
alphaa = (0.5d0/e)*log((1.d0-e*sin(lat0*deg2rad))/(1.d0+e*sin(lat0*deg2rad)))
alpha0 = e1*sin(lat0*deg2rad)/(1.d0-ee*dsin(lat0*deg2rad)**2)-e1*alphaa 
alphab = (0.5d0/e)*log((1.d0-e*sin(phi1*deg2rad))/(1.d0+e*sin(phi1*deg2rad)))
alpha1 = e1*sin(phi1*deg2rad)/(1.d0-ee*dsin(phi1*deg2rad)**2)-e1*alphab
alphac = (0.5d0/e)*log((1.d0-e*sin(phi2*deg2rad))/(1.d0+e*sin(phi2*deg2rad)))
alpha2 = e1*sin(phi2*deg2rad)/(1.d0-ee*dsin(phi2*deg2rad)**2)-e1*alphac
n = (m1**2-m2**2)/(alpha2-alpha1)
c = m1**2+n*alpha1
rho0 = r*((c-n*alpha0)**(0.5d0))/n
do i=1,np
   ! project mesh node locations into albers equal area conic
   P = O*DLOG((1.d0-E*DSIN(xyd(2,i)*deg2rad))/(1.d0+E*DSIN(xyd(2,i)*deg2rad)))
   ALPHA = E1*DSIN(xyd(2,i)*deg2rad)/(1.d0-EE*DSIN(xyd(2,i)*deg2rad)**2)-P      
   THETA = N*(xyd(1,i)-LON0) 
   RHO = R*(sqrt(C-N*ALPHA))/N
   xalbers(i) = RHO*DSIN(THETA*deg2rad)
   yalbers(i) = RHO0-RHO*DCOS(THETA*deg2rad)
end do
write(6,'("INFO: Finished generating Albers Equal Area Conic coordinates.")')
return
!-----------------------------------------------------------------------
END SUBROUTINE computeAlbersEqualAreaConic
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     S U B R O U T I N E   C O M P U T E  C P P 
!-----------------------------------------------------------------------
!     jgf: Very short subroutine to compute the CPP projection, 
!     allocating memory in the process, and not overwriting the 
!     original lat/lon data.
!-----------------------------------------------------------------------
SUBROUTINE computeCPP()
IMPLICIT NONE
if (cppComputed.eqv..false.) then
   allocate(x_cpp(np),y_cpp(np))
   cppComputed = .true.
endif
write(6,'("INFO: Generating CPP coordinates.")')
x_cpp = R * (xyd(1,:)*deg2rad - slam0*deg2rad) * cos(sfea0*deg2rad)
y_cpp = xyd(2,:)*deg2rad * R
return
!-----------------------------------------------------------------------
END SUBROUTINE computeCPP
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!     S U B R O U T I N E   C O M P U T E   2 X   A R E A S
!-----------------------------------------------------------------------
!     jgf: Compute 2x the elemental areas ... requires that the 
!     the CPP projection has already been computed.
!-----------------------------------------------------------------------
SUBROUTINE compute2xAreas()
IMPLICIT NONE
real(8) :: nx(3)
real(8) :: ny(3)
integer :: i, j
if (cppComputed.eqv..false.) then
   call computeCPP()
endif
write(6,'("INFO: Computing 2x the elemental areas.")')
do i=1,ne
   do j=1,3
      nx(j) = x_cpp(nm(i,j))
      ny(j) = y_cpp(nm(i,j))
   end do
   areas(i)=(nx(1)-nx(3))*(ny(2)-ny(3))+(nx(3)-nx(2))*(ny(1)-ny(3))
end do
write(6,'("INFO: Finished computing 2x the elemental areas.")')
!-----------------------------------------------------------------------
END SUBROUTINE compute2xAreas
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                    S U B R O U T I N E   
!     C O M P U T E   W E I G H T I N G   C O E F F I C I E N T S
!-----------------------------------------------------------------------
!     jgf: Compute the solution weighting coefficients.
!-----------------------------------------------------------------------
SUBROUTINE computeWeightingCoefficients()
IMPLICIT NONE
integer :: myNodes(0:4)
integer :: i, j
if (cppComputed.eqv..false.) then
   call computeCPP()
endif
allocate(sfac(np))
allocate(fdx(3,ne))
allocate(fdy(3,ne))
sfac(:)=cos(sfea0*deg2rad)/cos(xyd(2,:)*deg2rad)
write(6,'("INFO: Computing weighting coefficients.")')
do i=1,ne
   myNodes(1:3) = nm(i,1:3)
   ! wrap the values around so we can easily implement a loop 
   ! around the element
   myNodes(0) = myNodes(3)
   myNodes(4) = myNodes(1)
   sfacAvg(i) = oneThird * sum(sfac(myNodes(1:3)))
   ! loop over the nodes on this element
   do j=1,3
      fdy(j,i) = x_cpp(myNodes(j-1))-x_cpp(myNodes(j+1))         ! a1, a2, a3
      fdx(j,i) = ( y_cpp(myNodes(j+1))-y_cpp(myNodes(j-1)) ) * sFacAvg(i) ! b1, b2, b3
   end do        
end do
write(6,'("INFO: Finished computing weighting coefficients.")')
!-----------------------------------------------------------------------
      END SUBROUTINE computeWeightingCoefficients
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                     S U B R O U T I N E  
!         C O M P U T E   E L E M E N T   C E N T R O I D S
!-----------------------------------------------------------------------
!     jgf: Compute the solution weighting coefficients.
!-----------------------------------------------------------------------
SUBROUTINE computeElementCentroids()
IMPLICIT NONE
integer :: i
if (cppComputed.eqv..false.) then
   call computeCPP()
endif
allocate(centroids(2,ne))
write(6,'("INFO: Computing element centroids.")')
do i=1,ne
   centroids(1,i) = oneThird * sum(x_cpp(nm(i,1:3)))
   centroids(2,i) = oneThird * sum(y_cpp(nm(i,1:3)))
end do
write(6,'("INFO: Finished computing element centroids.")')
!-----------------------------------------------------------------------
      END SUBROUTINE computeElementCentroids
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  S U B R O U T I N E   C O M P U T E   S T A T I O N   W E I G H T S
!-----------------------------------------------------------------------
! jgf: Find the element in which a station is located and then compute
! the interpolation weights for evaluating the solution at the
! station location.
!-----------------------------------------------------------------------
subroutine computeStationWeights(station)
implicit none
type(station_t), intent(inout) :: station
!
real(8) :: x1, x2, x3 ! longitude temporary variables
real(8) :: y1, y2, y3 ! latitude temporary variables
real(8) :: subArea1, subArea2, subArea3, TotalArea
integer :: e

do e=1,ne
   X1 = station%lon
   X2 = xyd(1,nm(e,2))
   X3 = xyd(1,nm(e,3))
   Y1 = station%lat
   Y2 = xyd(2,nm(e,2))
   Y3 = xyd(2,nm(E,3))
   SubArea1 = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

   X1 = xyd(1,nm(e,1))
   X2 = station%lon
   X3 = xyd(1,nm(e,3))
   Y1 = xyd(2,nm(e,1))
   Y2 = station%lat
   Y3 = xyd(2,nm(e,3))
   SubArea2 = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

   X1 = xyd(1,nm(e,1))
   X2 = xyd(1,nm(e,2))
   X3 = station%lon
   Y1 = xyd(2,nm(e,1))
   Y2 = xyd(2,nm(e,2))
   Y3 = station%lat
   SubArea3 = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

   X1 = xyd(1,nm(e,1))
   X2 = xyd(1,nm(e,2))
   X3 = xyd(1,nm(e,3))
   Y1 = xyd(2,nm(e,1))
   Y2 = xyd(2,nm(e,2))
   Y3 = xyd(2,nm(e,3))
   TotalArea = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

   IF ((SubArea1+SubArea2+SubArea3).LE.(1.01*TotalArea))THEN
      station%elementIndex = e
      station%weights(1) = ( (station%lon-X3)*(Y2-Y3)+(X2-X3)*(Y3-station%lat) )/TotalArea
      station%weights(2) = ( (station%lon-X1)*(Y3-Y1)-(station%lat-Y1)*(X3-X1))/TotalArea
      station%weights(3) = (-(station%lon-X1)*(Y2-Y1)+(station%lat-Y1)*(X2-X1))/TotalArea
      exit
   else  
      station%elementIndex = 0
      station%weights = -99999.0
   endif    

enddo

!-----------------------------------------------------------------------
END SUBROUTINE computeStationWeights
!-----------------------------------------------------------------------


!-----+---------+---------+---------+---------+---------+---------+
   end module adcmesh
!-----+---------+---------+---------+---------+---------+---------+

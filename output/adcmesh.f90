!-----+---------+---------+---------+---------+---------+---------+
!
! adcmesh.f90
! This is a module for storing and manipulating data for ADCIRC meshes;
! it is based on code originally written by Corbitt Kerr.
!
!-----+---------+---------+---------+---------+---------+---------+
module adcmesh
!-----+---------+---------+---------+---------+---------+---------+
use netcdf, only : NF90_MAX_NAME
use kdtree2_module

real(8), parameter :: R = 6378206.4d0 ! radius of the earth
real(8), parameter :: pi = 3.141592653589793d0
real(8), parameter :: deg2rad = pi/180.d0
real(8), parameter :: rad2deg = 180.d0/pi
real(8), parameter :: oneThird = 1.d0/3.d0
logical :: verbose

!
! elevation boundaries and flux boundaries where
! ibtype = 0,1,2,10,11,12,20,21,22,30,52
type simpleBoundary_t
   integer :: ibtype                  ! boundary type
   integer :: indexNum                ! order within the fort.14 file (used to get IBTYPE etc)
   integer :: informationID           ! xdmf ID for IBTYPEE or IBTYPE info
   integer :: setID                   ! xdmf ID for node numbers
   integer, allocatable :: nodes(:)   ! node numbers on boundary
   real(8), allocatable :: bGeom(:)   ! coordinates for visualization
end type simpleBoundary_t
!
! flux boundaries where ibtype = 3, 13, 23
type externalFluxBoundary_t
   integer :: ibtype                  ! boundary type
   integer :: indexNum                ! order within the fort.14 file
   integer :: informationID           ! xdmf ID for IBTYPE info
   integer :: setID                   ! xdmf ID for node numbers
   integer :: numAttributes = 2
   integer :: attributeIDs(2) ! xdmf IDs for parameters
   integer, allocatable :: nodes(:)
   real(8), allocatable :: barlanht(:)
   real(8), allocatable :: barlancfsp(:)
   real(8), allocatable :: leveeGeom(:)
end type externalFluxBoundary_t
!
! flux boundaries where ibtype = 4, 24
type internalFluxBoundary_t
   integer :: ibtype                  ! boundary type
   integer :: indexNum                ! order within the fort.14 file
   integer :: informationID           ! xdmf ID for IBTYPE info
   integer :: setID                   ! xdmf ID for node numbers
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
!
! flux boundaries where ibtype = 5, 25
type internalFluxBoundaryWithPipes_t
   integer :: ibtype                 ! boundary type
   integer :: indexNum               ! order within the fort.14 file
   integer :: informationID          ! xdmf ID for IBTYPE info
   integer :: setID                  ! xdmf ID for node numbers
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
!
! integer codes for boundary categories, used for reverse lookup
! (given any random node number, tell what boundary type it is, what
! index that boundary is within that boundary type, and what index that
! node is on that boundary)
integer, parameter :: ELEV_B = 0      ! elevation specified (open) boundary
integer, parameter :: SFLUX_B = 1     ! simple (land, island, river) flux-specified boundary
integer, parameter :: EXTFLUX_B = 2   ! external overflow flux-specified boundary
integer, parameter :: INTFLUX_B = 3   ! internal flux-specified boundary (levee)
integer, parameter :: INTFLUXWP_B = 4 ! internal flux-specified boundary with cross-barrier pipes
!
!
type mesh_t
   !
   character(NF90_MAX_NAME) :: meshFileName ! full pathname of file
   !
   real(8), allocatable :: xyd(:,:)
   real(8), allocatable :: bar(:,:,:)
   !
   ! parameters related to carte parallelogrammatique projection (CPP)
   logical :: cppComputed = .false.
   real(8), allocatable :: x_cpp(:)
   real(8), allocatable :: y_cpp(:)
   !
   ! parameters related to cartesian unit sphere
   logical :: cartesianSphereComputed = .false.
   real(8), allocatable :: x_cartesian_sphere(:)
   real(8), allocatable :: y_cartesian_sphere(:)
   real(8), allocatable :: z_cartesian_sphere(:)
   !
   ! parameters related to Albers Equal Area Conic projection
   logical :: albersComputed = .false.
   real(8), allocatable :: xalbers(:)
   real(8), allocatable :: yalbers(:)
   !
   ! parameters related to the neighbor edge length table (np,neimax)
   logical :: edgeLengthTableComputed = .false. ! .true. when mem is allocated for this
   real(8), allocatable :: edgeLengthTable(:,:)
   real(8), allocatable :: edgeMidpointCoordinates(:,:,:)
   real(8), allocatable :: edgeLengthMinMax(:,:)
   real(8), allocatable :: edgeLengthGradients(:)
   real(8), allocatable :: minEdgeLengths(:)
   real(8) :: maxEdgeLengthGradient
   integer :: maxEdgeLengthGradientNode
   real(8) :: minEdgeLength ! shortest edge length in the whole mesh (m)
   real(8) :: maxEdgeLength ! longest edge length in the whole mesh (m)
   integer :: minEdgeLengthNode
   integer :: maxEdgeLengthNode
   real(8), allocatable :: areas(:) ! (ne) 2x element areas in square meters
   real(8), allocatable :: eledepths(:) ! (ne) element bathymetric depths (m)
   real(8), allocatable :: sfac(:) ! (np)
   real(8), allocatable :: sfacAvg(:) ! (ne)
   real(8), allocatable :: fdx(:,:) ! (3,ne)
   real(8), allocatable :: fdy(:,:) ! (3,ne)
   real(8), allocatable :: centroids(:,:) ! (2,ne) x and y cartesian coordinates of the element centroids
   integer :: mnei = 15  ! maximum number of neighbors for a node
   !
   character(2048) :: agrid
   integer :: ne   ! number of elements
   integer :: np   ! number of nodes
   integer, allocatable :: nmnc(:,:) ! element table in netcdf (3,ne)
   integer, allocatable :: nm(:,:)   ! element table in adcirc (ne,3)
   integer :: nfen
   real(8), allocatable :: sigma(:)
   logical :: is3D  ! true if the mesh is 3D
   ! boundary related parameters
   logical :: readBoundaryTable = .true.
   logical :: writeBoundaryTable = .true.
   integer :: neta_count ! count of open boundary nodes
   integer :: nvel_count ! count of land boundary nodes
   integer :: nope ! number of open (ocean) elevation specified boundaries
   integer :: neta ! total number of nodes on open (ocean) elevation specified boundary nodes
   integer :: nbou ! number of flux specified boundaries
   integer :: nvel ! total number of nodes on flux specified boundaries
   logical, allocatable :: isBoundaryNode(:) ! .true. if the node is on a boundary
   integer, allocatable :: nvdll(:)  ! number of nodes on each open boundary
   integer, allocatable :: nbdv(:,:) ! node numbers on each open boundary
   integer, allocatable :: nvell(:)  ! number of nodes on each flux boundary
   integer, allocatable :: ibtype(:) ! boundary type of each flux boundary
   integer, allocatable :: ibtypee(:) ! boundary type of each elevation boundary
   integer, allocatable :: nbvv(:,:) ! node numbers on each flux boundary
   integer, allocatable :: lbcodei(:) ! bound. type array for flux boundaries
   integer :: nvdll_max  ! longest elevation boundary
   integer :: nvell_max  ! longest flux boundary
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

   logical :: elementAreasComputed = .false.
   logical :: elementDepthsComputed = .false.
   logical :: elementCentroidsComputed = .false.
   logical :: kdtree2SearchTreeComputed = .false.
   logical :: weightingCoefficientsComputed = .false.
   logical :: neighborTableComputed = .false.
   logical :: allLeveesOK ! .false. if there are any issues
   integer :: NEIMIN
   integer :: NEIMAX
   integer, allocatable :: NNeigh(:)
   integer, allocatable :: NeiTab(:,:)
   integer, allocatable :: NeiTabGenerated(:,:)
   integer, allocatable :: NeiTabEle(:,:)
   integer, allocatable :: NeiTabEleGenerated(:,:)
   integer, allocatable :: nneighele(:)
   integer :: nLowConnected   ! number of nodes in the mesh with only 2 nodal neighbors
   real(8) :: slam0  ! longitude on which cpp projection is centered (degrees)
   real(8) :: sfea0  ! latitude on which cpp projection is centered (degrees)
   real(8) :: lonmin ! domain extents (degrees)
   real(8) :: lonmax
   real(8) :: latmin
   real(8) :: latmax
   logical :: useStationTolerance ! allow close stations to be associated with mesh with some tolerance
   real(8) :: stationTolerance    ! tolerance to use for stations just outside mesh
   !
   ! elevation boundaries
   type(simpleBoundary_t), allocatable :: elevationBoundaries(:)
   integer :: numElevationBoundaries ! for memory allocation
   !
   ! mainland and island boundaries
   type(simpleBoundary_t), allocatable :: simpleFluxBoundaries(:)
   integer :: numSimpleFluxBoundaries ! for memory allocation
   ! variable holding flux boundaries with ibtype = 0, 1, 2, 10, 11, 12, 20, 21, 22, 30, 52
   integer :: sfCount   ! index into the simpleFluxBoundaries array
   !
   ! external overflow boundary and river boundary
   type(externalFluxBoundary_t), allocatable :: externalFluxBoundaries(:)
   integer :: numExternalFluxBoundaries
   integer :: efCount   ! index into the externalFluxBoundaries array
   !
   ! levee boundaries
   type(internalFluxBoundary_t), allocatable :: internalFluxBoundaries(:)
   integer :: numInternalFluxBoundaries
   integer :: ifCount   ! index into the internalFluxBoundaries array
   !
   ! levees with cross barrier pipes
   type(internalFluxBoundaryWithPipes_t), allocatable :: internalFluxBoundariesWithPipes(:)
   integer :: numInternalFluxBoundariesWithPipes
   integer :: ifwpCount ! index into the internalFluxBoundariesWithPipes array

   integer, allocatable :: boundaryCategory(:)  ! (np) elevation, simpleFlux, externalFlux, etc
   integer, allocatable :: boundaryIndex(:)     ! (np) e.g., within elevationBoundaries(:) array
   integer, allocatable :: boundaryNodeIndex(:) ! (np) e.g., within elevationBoundaries(1)%nodes(:) array

   integer :: nfluxf ! =1 if there are any specified flux boundaries in the mesh
   !
   ! finding boundary nodes
   integer, allocatable :: tbnode(:,:)   ! size(mnp,2)
   integer :: nseg                       ! total number of element edges lying on boundaries
   !
   ! sorting boundary nodes
   integer, allocatable :: conbnode(:)   ! connected boundary node numbers on contiguous boundaries
   integer, allocatable :: nbn(:)        ! number of boundary nodes on each boundary
   integer :: numBoundFound              ! number of contiguous boundaries found through search
   integer :: externalBoundaryNodeHint   ! operator-specified node number that is on the external boundary
   integer :: nodeOnExternalBoundary     ! number of a node known to be on external boundary
   logical :: externalBoundaryFound      ! true if the external boundary has been identified
   integer :: longestBoundary(1)         ! index of the longest boundary
   integer, allocatable :: extbns(:)     ! node numbers in the external boundary (found with
                                         !  hint) with first node repeated as last node
   ! assigning boundary node types
   integer :: minElevationBoundaryLength = 10 ! operator-specified min number of nodes for an elevation specified boundary
   real :: minElevationBoundaryDepth = 2.d0   ! (m) operator specified min depth of elevation specified boundary
   logical :: oneElevationBoundary       ! .true. if there should be one elevation specified boundary in the mesh
   integer, allocatable :: ibbeg(:)      ! beginning node numbers of open boundaries
   integer, allocatable :: ibend(:)      ! ending node numbers of open boundaries
   integer :: nob                        ! number of open boundaries found in a mesh
   integer :: itotobn                    ! total number of open boundary nodes found
   integer, allocatable :: bsegbn(:)     ! beginning node numbers of flux boundaries
   integer, allocatable :: bsegen(:)     ! ending node numbers of flux boundaries
   integer :: nlb                        ! number of land boundaries found in a mesh
   integer :: itotlbn                    ! total number of land boundary nodes found
   integer :: nexlb
   !
   ! variables related to kdtree2 searches
   integer                      :: srchdp       ! number of elements to return from search
   type(kdtree2), pointer       :: tree         ! search tree
   type(kdtree2_result), allocatable :: kdresults(:) ! list of elements to check

end type mesh_t

type meshNetCDF_t
   integer :: NC_DimID_node = -99
   integer :: NC_DimID_nfen = -99
   integer :: NC_DimID_nele = -99
   integer :: NC_DimID_nvertex = -99
   integer :: NC_DimID_nope = -99
   integer :: NC_DimID_max_nvdll = -99
   integer :: NC_DimID_nbou = -99
   integer :: NC_DimID_neta = -99
   integer :: NC_DimID_nvel = -99
   integer :: NC_DimID_max_nvell = -99
   integer :: NC_VarID_Mesh = -99
   integer :: NC_VarID_x = -99
   integer :: NC_VarID_y = -99
   integer :: NC_VarID_sigma = -99
   integer :: NC_VarID_nfen = -99
   integer :: NC_VarID_element = -99
   integer :: NC_VarID_neta = -99
   integer :: NC_VarID_nvdll = -99
   integer :: NC_VarID_max_nvdll = -99
   integer :: NC_VarID_ibtypee = -99
   integer :: NC_VarID_nbdv = -99
   integer :: NC_VarID_nvel = -99
   integer :: NC_VarID_nope  = -99
   integer :: NC_VarID_nvell = -99
   integer :: NC_VarID_max_nvell = -99
   integer :: NC_VarID_ibtype = -99
   integer :: NC_VarID_nbvv = -99
   integer :: NC_VarID_depth = -99
   integer :: NC_VarID_x_cpp = -99
   integer :: NC_VarID_y_cpp = -99
   integer :: NC_VarID_x_cartesian_sphere = -99
   integer :: NC_VarID_y_cartesian_sphere = -99
   integer :: NC_VarID_z_cartesian_sphere = -99
   end type meshNetCDF_t

integer, parameter :: specifiedFluxBoundaryTypes(5) = (/ 2, 12, 22, 32, 52 /)
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
   real(8) :: lon            ! decimal degrees east
   real(8) :: lat            ! decimal degrees north
   real(8) :: x_cpp          ! m east after reprojection with CPP
   real(8) :: y_cpp          ! m north after reprojection with CPP
   real(8) :: z              ! vertical position (m) relative to mesh zero
   integer :: irtype         ! number of components; 1=scalar, 2=2D vector, 3=3D vector
   logical :: elementFound   ! true if the element number is known
   integer :: elementIndex   ! where station is located in a particular mesh element table array
   real(8) :: elementArea    ! total area in m^2
   logical :: outsideWithinTolerance  ! if the station is actually outside mesh but within tolerance (for meshes where this criterion is active)
   real(8) :: elementBathyDepth       ! spatially weighted interpolation of nodal depths (m)
   logical :: useBruteForceSearch     ! true if every element should be checked
   integer :: n(3)           ! nodes that surround the station
   real(8) :: w(3)           ! weights used to interpolate station values based on nodal values
   real(8), allocatable :: d(:,:)     ! station data (irtype, ntime)
   integer :: iID            ! simple numerical ID
   character(len=1024) :: stationID   ! generally a number assigned by govt agency
   character(len=1024) :: description ! human readable
   character(len=1024) :: agency      ! organization responsible for the station
   character(len=1024) :: datum       ! relevant vertical datum (MSL, NAVD88, etc)
end type station_t

!-----+---------+---------+---------+---------+---------+---------+
contains
!-----+---------+---------+---------+---------+---------+---------+

!-----+---------+---------+---------+---------+---------+---------+
!  READ14_FindDims
!-----+---------+---------+---------+---------+---------+---------+
subroutine read14_findDims(m)
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: ios ! i/o status
integer :: lineNum ! line number currently being read
integer :: i, j, k
integer :: iunit
integer :: errorIO

! initializations
lineNum = 1
m%numSimpleFluxBoundaries = 0
m%numExternalFluxBoundaries = 0
m%numInternalFluxBoundaries = 0
m%numInternalFluxBoundariesWithPipes = 0
m%is3D = .false.
!
iunit = availableUnitNumber()
call openFileForRead(iunit, m%meshFileName, errorIO)
read(iunit,'(a)',err=10,end=20,iostat=ios) m%agrid
lineNum = lineNum + 1
write(6,'(A)') "INFO: Mesh file comment line: "//trim(m%agrid)
write(6,'(A)') "INFO: Reading mesh file dimensions."

read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%ne, m%np
do k = 1, m%np
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) i
   lineNum = lineNum + 1
   if (i.ne.k) then
      write(6,'("ERROR: Attempted to read node number ",i0," but found node number ",i0," instead.")') k, i
      stop
   endif
enddo
do k = 1, m%ne
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) i
   lineNum = lineNum + 1
   if (i.ne.k) then
      write(6,'("ERROR: Attempted to read element number ",i0," but found element number ",i0," instead.")') k, i
      stop
   endif
enddo
!
! return if not reading boundary table
if (m%readBoundaryTable.eqv..false.) then
   close(iunit)
   write(6,'(A)') 'INFO: Finished reading mesh file dimensions.'
   return
endif
!
!
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nope  ! total number of elevation boundaries
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%neta  ! total number of nodes on elevation boundaries
lineNum = lineNum + 1
write(6,'(a)') 'INFO: Allocating memory for elevation specified boundaries.'
call allocateElevationBoundaryLengths(m)
m%neta_count = 0
m%nvdll_max = 0
do k = 1, m%nope
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nvdll(k) ! number of nodes on the kth elevation boundary segment
   lineNum = lineNum + 1
   m%nvdll_max = max(m%nvdll_max,m%nvdll(k))
   do j = 1, m%nvdll(k)
      read(unit=iunit,fmt=*,err=10,end=20,iostat=ios)
      lineNum = lineNum + 1
      m%neta_count = m%neta_count + 1
   enddo
enddo
! need nvdll_max to allocate nbdv
call allocateAdcircElevationBoundaryArrays(m)
if ( m%neta_count.ne.m%neta ) then
   write(6,'("WARNING: Number of open boundary nodes was set to ",i0," but ",i0," were found.")') m%neta, m%neta_count
endif
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nbou ! total number of flux boundaries
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nvel ! total number of nodes on flux boundaries
lineNum = lineNum + 1
write(6,'(a)') 'INFO: Allocating memory for flux specified boundaries.'
call allocateFluxBoundaryLengths(m)
m%nvel_count = 0
m%nvell_max = 0
do k = 1, m%nbou
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nvell(k), m%ibtype_orig(k)  ! number of nodes and type of kth flux boundary
   lineNum = lineNum + 1
   m%nvell_max = max(m%nvell_max,m%nvell(k))
   do j = 1, m%nvell(k)
      read(unit=iunit,fmt=*,err=10,end=20,iostat=ios)
      lineNum = lineNum + 1
   enddo
   ! count the total number of each type of boundary for later
   ! use in memory allocation
   select case(m%ibtype_orig(k))
   case(0,1,2,10,11,12,20,21,22,30,52)
       m%numSimpleFluxBoundaries = m%numSimpleFluxBoundaries + 1
       m%nvel_count = m%nvel_count + m%nvell(k)
   case(3,13,23)
       m%numExternalFluxBoundaries = m%numExternalFluxBoundaries + 1
       m%nvel_count = m%nvel_count + m%nvell(k)
   case(4,24)
       m%numInternalFluxBoundaries = m%numInternalFluxBoundaries + 1
       m%nvel_count = m%nvel_count + 2*m%nvell(k)
   case(5,25)
       m%numInternalFluxBoundariesWithPipes = m%numInternalFluxBoundariesWithPipes + 1
       m%nvel_count = m%nvel_count + 2*m%nvell(k)
   case default
       write(6,'("ERROR: The boundary type ",i0," was found in the file but is not valid.")') m%ibtype_orig(k)
       stop
   end select
enddo
! need nvell_max to allocate nbvv
call allocateAdcircFluxBoundaryArrays(m)
if ( m%nvel_count.ne.m%nvel) then
   write(6,'("WARNING: Number of flux boundary nodes was set to ",i0," but ",i0," were found.")') m%nvel, m%nvel_count
   if (verbose.eqv..true.) then
      write(6,*) 'WARNING: Here is the summary of land boundary node information:'
      write(6,'("NVEL (specified number of land boundary nodes) = ",i0,".")') m%nvel
      write(6,'("Counted number of land boundary nodes = ",i0,".")') m%nvel_count
      do k=1,m%nbou
         write(6,'("ibtype(",i0,")=",i0,", nvell(",i0,")=",i0,", total=",i0,".")') k, m%ibtype_orig(k), k, m%nvell(k), sum(m%nvell(1:k))
      end do
   endif
endif
close(iunit)

! allocate space for boundary reverse lookup (given a node number,
! return boundary info and references about that node)
allocate(m%isBoundaryNode(m%np))
allocate(m%boundaryCategory(m%np))
allocate(m%boundaryIndex(m%np))
allocate(m%boundaryNodeIndex(m%np))
m%isBoundaryNode(:) = .false.
m%boundaryCategory(:) = 0
m%boundaryIndex(:) = 0
m%boundaryNodeIndex(:) = 0

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
subroutine findMeshDimsNetCDF(m, n)
use netcdf
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
type(meshNetCDF_t), intent(inout) :: n ! netcdf IDs to operate on
integer :: nc_id  ! netcdf ID of the file to read from
integer :: dimPres ! return code from netcdf to determine if the dimension is present in the file
integer :: i
!
write(6,'(a)') 'INFO: Reading mesh dimensions from the netCDF file.'
!
! initializations
m%is3D = .false.
!
! open the netcdf file
call check(nf90_open(trim(m%meshFileName), NF90_NOWRITE, nc_id))
!
call readMeshCommentLineNetCDF(m, nc_id)
!
! read the lengths of the dimensions that will always be present in
! an adcirc netcdf file that contains a mesh
call check(nf90_inq_dimid(nc_id, 'node', n%nc_dimid_node))
call check(nf90_inquire_dimension(nc_id, n%nc_dimid_node, len=m%np))
call check(nf90_inq_dimid(nc_id, 'nele', n%nc_dimid_nele))
call check(nf90_inquire_dimension(nc_id, n%nc_dimid_nele, len=m%ne))
!
! determine which other dimensions are present and find their lengths
!
! open boundaries
write(6,'(a)') 'INFO: Reading boundary dimensions.'
dimPres = nf90_inq_dimid(nc_id, 'nope', n%nc_dimid_nope)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, n%nc_dimid_nope, len=m%nope))
else
   m%nope = 0
endif
! total number of open boundary nodes
dimPres = nf90_inq_dimid(nc_id, 'neta', n%nc_dimid_neta)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, n%nc_dimid_neta, len=m%neta))
else
   m%neta = 0
endif
! longest open boundary segment
dimPres = nf90_inq_dimid(nc_id, 'max_nvdll', n%nc_dimid_max_nvdll)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, n%nc_dimid_max_nvdll, len=m%nvdll_max))
else
   m%nvdll_max = 0
endif
! allocate arrays to hold the data
call allocateElevationBoundaryLengths(m)
call allocateAdcircElevationBoundaryArrays(m)
!
! now read in the lengths of the open boundary segments and the node
! numbers on each segment
if (m%nope.ne.0) then
   call check(nf90_inq_varid(nc_id, 'nvdll', n%nc_varid_nvdll))
   call check(nf90_get_var(nc_id, n%nc_varid_nvdll, m%nvdll, (/ 1 /), (/ m%nope /) ))
   call check(nf90_inq_varid(nc_id, 'nbdv', n%nc_varid_nbdv))
   do i=1, m%nope
      call check(nf90_get_var(nc_id, n%nc_varid_nbdv, m%nbdv(i,:), (/ i, 1 /), (/ 1, m%nvdll(i) /) ))
   end do
endif
!
! flux boundaries
dimPres = nf90_inq_dimid(nc_id, 'nbou', n%nc_dimid_nbou)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, n%nc_dimid_nbou, len=m%nbou))
else
   m%nbou = 0
endif
! total number of flux boundary nodes
dimPres = nf90_inq_dimid(nc_id, 'nvel', n%nc_dimid_nvel)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, n%nc_dimid_nvel, len=m%nvel))
else
   m%nvel = 0
endif
! longest flux boundary segment
dimPres = nf90_inq_dimid(nc_id, 'max_nvell', n%nc_dimid_max_nvell)
if (dimPres.eq.NF90_NOERR) then
   call check(nf90_inquire_dimension(nc_id, n%nc_dimid_max_nvell, len=m%nvell_max))
else
   m%nvell_max = 0
endif
!
call allocateFluxBoundaryLengths(m)
call allocateAdcircFluxBoundaryArrays(m)
!
! now read in the lengths of the open boundary segments and the node
! numbers on each segment
if (m%nbou.ne.0) then
   call check(nf90_inq_varid(nc_id, 'nvell', n%nc_varid_nvell))
   call check(nf90_get_var(nc_id, n%nc_varid_nvell, m%nvell, (/ 1 /), (/ m%nbou /) ))
   call check(nf90_inq_varid(nc_id, 'ibtype', n%nc_varid_ibtype))
   call check(nf90_get_var(nc_id, n%nc_varid_ibtype, m%ibtype, (/ 1 /) , (/ m%nbou /) ))
   call check(nf90_inq_varid(nc_id, 'nbvv', n%nc_varid_nbvv))
   do i=1, m%nbou
      call check(nf90_get_var(nc_id, n%nc_varid_nbvv, m%nbvv(i,:), (/ i, 1 /), (/ 1, m%nvell(i) /) ))
   end do
endif
! allocate space for boundary reverse lookup (given a node number,
! return boundary info and references about that node)
allocate(m%isBoundaryNode(m%np))
allocate(m%boundaryCategory(m%np))
allocate(m%boundaryIndex(m%np))
allocate(m%boundaryNodeIndex(m%np))
m%isBoundaryNode(:) = .false.
m%boundaryCategory(:) = 0
m%boundaryIndex(:) = 0
m%boundaryNodeIndex(:) = 0
!
! get 3D mesh dimensions if the data are 3D
i = nf90_inq_dimid(nc_id, "num_v_nodes", n%nc_dimid_nfen)
if (i.eq.NF90_NOERR) then
   m%is3D = .true.
   call check(nf90_inquire_dimension(nc_id, n%nc_dimid_nfen, len=m%nfen))
   call check(nf90_inq_varid(nc_id, "sigma", n%nc_varid_sigma))
   allocate(m%sigma(m%nfen))
   call check(nf90_get_var(nc_id, n%nc_varid_sigma, m%sigma))
endif
!
! close netcdf file
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
subroutine readMeshNetCDF(m, n)
use netcdf
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
type(meshNetCDF_t), intent(inout) :: n ! netcdf IDs to operate on
integer :: nc_id ! netcdf ID of the file to read from
integer :: i, j ! loop counter
!
integer :: nc_count(1) ! x, y, and depth are 1D
integer :: nc_start(1)
!
integer :: nc_ele_count(2) ! element table is 2D
integer :: nc_ele_start(2)
!
integer :: nc_boundmax_count(2) ! max boundary nodes table is 2D
integer :: nc_boundmax_start(2)
!
m%useStationTolerance = .false.
write(6,'(a)') 'INFO: Reading mesh from the netCDF file.'
call allocateNodalAndElementalArrays(m)
!
call check(nf90_open(trim(m%meshFileName), NF90_NOWRITE, nc_id))
!
! read mesh lon, lat, depth data from the file
nc_start = (/ 1 /)
nc_count = (/ m%np /)

call check(nf90_inq_varid(nc_id, 'x', n%nc_varid_x))
call check(nf90_get_var(nc_id,n%nc_varid_x,m%xyd(1,1:m%np),nc_start,nc_count))
call check(nf90_inq_varid(nc_id, 'y', n%nc_varid_y))
call check(nf90_get_var(nc_id,n%nc_varid_y,m%xyd(2,1:m%np),nc_start,nc_count))
call check(nf90_inq_varid(nc_id, 'depth', n%nc_varid_depth))
call check(nf90_get_var(nc_id,n%nc_varid_depth,m%xyd(3,1:m%np),nc_start,nc_count))
!
! element table
nc_ele_start = (/ 1, 1 /)
nc_ele_count = (/ 3, m%ne /)
call check(nf90_inq_varid(nc_id, 'element', n%nc_varid_element))
call check(nf90_get_var(nc_id,n%nc_varid_element,m%nmnc,nc_ele_start,nc_ele_count))
!
! populate the adcirc-style element table
!
! we must reverse the order of the dimensions because netcdf writes
! them to disk in column major order, according to the way C interprets
! the data, rather than row major order, the way Fortran would interpret
! the data
do i=1, m%ne
   do j=1, 3
      m%nm(i,j)= m%nmnc(j,i)
   end do
end do
!
! open (i.e., elevation specified) boundaries
if (m%nope.ne.0) then
   nc_count = (/ m%nope /)
   call check(nf90_get_var(nc_id,n%nc_varid_nvdll,m%nvdll,nc_start,nc_count))
   nc_boundmax_start = (/ 1, 1 /)
   nc_boundmax_count = (/ m%nope, m%nvdll_max /)
   call check(nf90_get_var(nc_id,n%nc_varid_nbdv,m%nbdv,nc_boundmax_start,nc_boundmax_count))
endif
if (m%nbou.ne.0) then
   nc_count = (/ m%nbou /)
   call check(nf90_get_var(nc_id,n%nc_varid_nvell,m%nvell,nc_start,nc_count))
   nc_count = (/ m%nbou /)
   call check(nf90_get_var(nc_id,n%nc_varid_ibtype,m%ibtype,nc_start,nc_count))
   nc_boundmax_start = (/ 1, 1 /)
   nc_boundmax_count = (/ m%nbou, m%nvell_max /)
   call check(nf90_get_var(nc_id,n%nc_varid_nbvv,m%nbvv,nc_boundmax_start,nc_boundmax_count))
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
subroutine readMeshCommentLineNetCDF(m, nc_id)
use netcdf
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer, intent(in) :: nc_id ! netcdf id of the file to read from
! return codes to determine which of the variable names were
! used in writing this file
integer :: agold
integer :: agnew

agnew = nf90_get_att(nc_id,nf90_global,'agrid',m%agrid)
if (agnew.EQ.NF90_NOERR) then
   return
endif
agold = nf90_get_att(nc_id,nf90_global,'grid',m%agrid)
if (agold.EQ.NF90_NOERR) then
   return
endif
! This can happen if a minimal hotstart file was created, e.g., by adcirpolate
write(6,'(a)') 'WARNING: Could not find the fort.14 comment line (grid or agrid attribute).'
m%agrid='WARNING: Could not find the fort.14 comment line (grid or agrid attribute).'

!----------------------------------------------------------------------
end subroutine readMeshCommentLineNetCDF
!----------------------------------------------------------------------

!-----+---------+---------+---------+---------+---------+---------+
! READ14
!-----+---------+---------+---------+---------+---------+---------+
subroutine read14(m)
use ioutil
implicit none
type(mesh_t), intent(inout) :: m
integer :: i, j, k, jn, je, nhy
integer :: iunit
integer :: ios     ! i/o status
integer :: lineNum ! line number currently being read
!
! initialization
m%nfluxf = 0
m%useStationTolerance = .false.
!
if (trim(m%meshFileName).eq."null") then
   write(6,'(a)',advance='no') "Enter name of the mesh file: "
   read(5,'(A)') m%meshFileName
endif
!
call read14_findDims(m)
!
call allocateNodalAndElementalArrays(m)
if (m%readBoundaryTable.eqv..true.) then
   call allocateBoundaryArrays(m)
   !
   if (verbose.eqv..true.) then
      write(6,'("Number of elevation specified boundaries (nope): ",i0,".")') m%nope
      write(6,'("Number of simple flux specified boundaries (0,1,2,etc): ",i0,".")') m%numSimpleFluxBoundaries
      write(6,'("Number of external flux boundaries (3,etc): ",i0,".")') m%numExternalFluxBoundaries
      write(6,'("Number of internal flux boundaries (4,etc): ",i0,".")') m%numInternalFluxBoundaries
      write(6,'("Number of internal flux boundaries with pipes (5,etc): ",i0,".")') m%numInternalFluxBoundariesWithPipes
   endif
endif
write(6,'(A)') 'INFO: Reading mesh file coordinates, connectivity, and boundary data.'
iunit = availableUnitNumber()
call openFileForRead(iunit, m%meshFileName, ios)
lineNum = 1
read(unit=iunit,fmt='(a)',err=10,end=20,iostat=ios) m%agrid
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%ne, m%np
lineNum = lineNum + 1
do k = 1, m%np
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) jn, (m%xyd(j,k), j=1,3)
   lineNum = lineNum + 1
enddo
do k = 1, m%ne
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) je, nhy, ( m%nm(k,j), j = 1, 3 )
   lineNum = lineNum + 1
enddo
!
! populate netcdf-style element table
do i=1, m%ne
   do j=1, 3
      m%nmnc(j,i) = m%nm(i,j)
   end do
end do
!
! return if not reading boundary table
if (m%readBoundaryTable.eqv..false.) then
   write(6,'(A)') 'INFO: Finished reading mesh file coordinates and connectivity.'
   return
endif
!
! read boundary table
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nope
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%neta
lineNum = lineNum + 1
do k = 1, m%nope
   ! not reading ibtypee, it may not be there and 0 is currently the only valid value anyway
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nvdll(k)
   lineNum = lineNum + 1
   m%elevationBoundaries(k)%indexNum = k
   m%elevationBoundaries(k)%ibtype = 0     ! hard coded b/c we don't have any other valid ibtype (for now)
   do j = 1, m%nvdll(k)
      read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) jn
      m%elevationBoundaries(k)%nodes(j) = jn
      m%nbdv(k,j) = jn
      m%isBoundaryNode(jn) = .true.
      m%boundaryCategory(jn) = ELEV_B
      m%boundaryIndex(jn) = k
      m%boundaryNodeIndex(jn) = j
      lineNum = lineNum + 1
   enddo
enddo
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nbou
lineNum = lineNum + 1
read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nvel
lineNum = lineNum + 1
m%sfCount = 1
m%efCount = 1
m%ifCount = 1
m%ifwpCount = 1
do k = 1, m%nbou
   read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) m%nvell(k), m%ibtype_orig(k)
   lineNum = lineNum + 1
   select case(m%ibtype_orig(k))
   case(0,1,2,10,11,12,20,21,22,30,52)
      m%simpleFluxBoundaries(m%sfCount)%indexNum = k
      m%simpleFluxBoundaries(m%sfCount)%ibtype = m%ibtype_orig(k)
      do j = 1, m%nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) jn
         m%simpleFluxBoundaries(m%sfCount)%nodes(j) = jn
         m%nbvv(k,j) = jn
         m%isBoundaryNode(jn) = .true.
         m%boundaryCategory(jn) = SFLUX_B
         m%boundaryIndex(jn) = m%sfCount
         m%boundaryNodeIndex(jn) = j
         lineNum = lineNum + 1
      end do
      m%sfCount = m%sfCount + 1
   case(3,13,23)
      m%externalFluxBoundaries(m%efCount)%indexNum = k
      m%externalFluxBoundaries(m%efCount)%ibtype = m%ibtype_orig(k)
      do j = 1, m%nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) &
                       m%externalFluxBoundaries(m%efCount)%nodes(j), &
                       m%externalFluxBoundaries(m%efCount)%barlanht(j), &
                       m%externalFluxBoundaries(m%efCount)%barlancfsp(j)
         m%nbvv(k,j) = m%externalFluxBoundaries(m%efCount)%nodes(j)
         jn = m%externalFluxBoundaries(m%efCount)%nodes(j)
         m%isBoundaryNode(jn) = .true.
         m%boundaryCategory(jn) = EXTFLUX_B
         m%boundaryIndex(jn) = m%efCount
         m%boundaryNodeIndex(jn) = j
         lineNum = lineNum + 1
      end do
      m%efCount = m%efCount + 1
   case(4,24)
      m%internalFluxBoundaries(m%ifCount)%indexNum = k
      m%internalFluxBoundaries(m%ifCount)%ibtype = m%ibtype_orig(k)
      do j = 1, m%nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) &
                       m%internalFluxBoundaries(m%ifCount)%nodes(j), &
                       m%internalFluxBoundaries(m%ifCount)%ibconn(j), &
                       m%internalFluxBoundaries(m%ifCount)%barinht(j), &
                       m%internalFluxBoundaries(m%ifCount)%barincfsb(j), &
                       m%internalFluxBoundaries(m%ifCount)%barincfsp(j)
         m%nbvv(k,j) = m%internalFluxBoundaries(m%ifCount)%nodes(j)
         jn = m%internalFluxBoundaries(m%ifCount)%nodes(j)
         m%isBoundaryNode(jn) = .true.
         m%boundaryCategory(jn) = INTFLUX_B
         m%boundaryIndex(jn) = m%ifCount
         m%boundaryNodeIndex(jn) = j
         lineNum = lineNum + 1
      end do
      m%ifCount = m%ifCount + 1
   case(5,25)
      m%internalFluxBoundaries(m%ifwpCount)%indexNum = k
      m%internalFluxBoundaries(m%ifwpCount)%ibtype = m%ibtype_orig(k)
      do j = 1, m%nvell(k)
         read(unit=iunit,fmt=*,err=10,end=20,iostat=ios) &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%nodes(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%ibconn(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%barinht(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%barincfsb(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%barincfsp(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipeht(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipecoef(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipediam(j)
         m%nbvv(k,j) = m%internalFluxBoundariesWithPipes(m%ifwpCount)%nodes(j)
         jn = m%internalFluxBoundariesWithPipes(m%ifwpCount)%nodes(j)
         m%isBoundaryNode(jn) = .true.
         m%boundaryCategory(jn) = INTFLUXWP_B
         m%boundaryIndex(jn) = m%ifwpCount
         m%boundaryNodeIndex(jn) = j
         lineNum = lineNum + 1
      end do
      m%ifwpCount = m%ifwpCount + 1
   case default
      write(6,*) 'ERROR: IBTYPE ',m%ibtype_orig(k),' is not allowed.'
      stop
   end select
end do
close(iunit)
!
! initialize ibtype array
m%ibtype = m%ibtype_orig
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
subroutine allocateNodalAndElementalArrays(m)
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
allocate(m%xyd(3,m%np))
allocate(m%nm(m%ne,3))
allocate(m%nmnc(3,m%ne))
!
! initialize to something troublesome to make it easy to spot issues
m%xyd = -99999.d0
m%nm = 0
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
subroutine allocateElevationBoundaryLengths(m)
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
allocate(m%nvdll(m%nope)) ! number of nodes on each elevation boundary segment
allocate(m%ibtypee(m%nope)) ! type of each elevation boundary segment
!
! initialize to something troublesome to make it easy to spot issues
m%ibtypee = -99999
m%nvdll = -99999
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
subroutine allocateFluxBoundaryLengths(m)
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
allocate(m%nvell(m%nbou)) ! number of nodes on each flux boundary segment
allocate(m%ibtype_orig(m%nbou))
allocate(m%ibtype(m%nbou))
!
! initialize to something troublesome to make it easy to spot issues
m%nvell = -99999
m%ibtype_orig = -99999
m%ibtype = -99999
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
subroutine allocateAdcircElevationBoundaryArrays(m)
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
allocate(m%nbdv(m%nope,m%nvdll_max))
allocate(m%nbd(m%neta))
!
! initialize to something troublesome to make it easy to spot issues
m%nbdv = -99999
m%nbd = -99999
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
subroutine allocateAdcircFluxBoundaryArrays(m)
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
allocate ( m%nbv(m%nvel),m%lbcodei(m%nvel))
allocate ( m%nbvv(m%nbou,m%nvell_max))
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
m%nbv = -99999
m%lbcodei = -99999
m%nbvv = -99999
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
subroutine allocateBoundaryArrays(m)
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: i
!
allocate(m%elevationBoundaries(m%nope))
do i=1,m%nope
   allocate(m%elevationBoundaries(i)%nodes(m%nvdll(i)))
end do
allocate(m%simpleFluxBoundaries(m%numSimpleFluxBoundaries))
allocate(m%externalFluxBoundaries(m%numExternalFluxBoundaries))
allocate(m%internalFluxBoundaries(m%numInternalFluxBoundaries))
allocate(m%internalFluxBoundariesWithPipes(m%numInternalFluxBoundariesWithPipes))
m%sfCount = 1
m%efCount = 1
m%ifCount = 1
m%ifwpCount = 1
do i=1,m%nbou
   if (verbose.eqv..true.) then
      write(6,'("i=",i0)') i
   endif
   select case(m%ibtype_orig(i))
   case(0,1,2,10,11,12,20,21,22,30,52)
      allocate(m%simpleFluxBoundaries(m%sfCount)%nodes(m%nvell(i)))
      m%sfCount = m%sfCount + 1
   case(3,13,23)
      allocate(m%externalFluxBoundaries(m%efCount)%nodes(m%nvell(i)))
      allocate(m%externalFluxBoundaries(m%efCount)%barlanht(m%nvell(i)))
      allocate(m%externalFluxBoundaries(m%efCount)%barlancfsp(m%nvell(i)))
      m%efCount = m%efCount + 1
   case(4,24)
      allocate(m%internalFluxBoundaries(m%ifCount)%nodes(m%nvell(i)))
      allocate(m%internalFluxBoundaries(m%ifCount)%ibconn(m%nvell(i)))
      allocate(m%internalFluxBoundaries(m%ifCount)%barinht(m%nvell(i)))
      allocate(m%internalFluxBoundaries(m%ifCount)%barincfsb(m%nvell(i)))
      allocate(m%internalFluxBoundaries(m%ifCount)%barincfsp(m%nvell(i)))
      m%ifCount = m%ifCount + 1
   case(5,25)
      allocate(m%internalFluxBoundariesWithPipes(m%ifwpCount)%nodes(m%nvell(i)))
      allocate(m%internalFluxBoundariesWithPipes(m%ifwpCount)%ibconn(m%nvell(i)))
      allocate(m%internalFluxBoundariesWithPipes(m%ifwpCount)%barinht(m%nvell(i)))
      allocate(m%internalFluxBoundariesWithPipes(m%ifwpCount)%barincfsb(m%nvell(i)))
      allocate(m%internalFluxBoundariesWithPipes(m%ifwpCount)%barincfsp(m%nvell(i)))
      allocate(m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipeht(m%nvell(i)))
      allocate(m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipecoef(m%nvell(i)))
      allocate(m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipediam(m%nvell(i)))
      m%ifwpCount = m%ifwpCount + 1
   case default
       write(6,'("ERROR: The boundary type ",i0," was found in the file but is not valid.")') m%ibtype_orig(i)
       stop
   end select
end do
! initialize to something troublesome to make it easy to spot issues
do i=1,m%nope
   m%elevationBoundaries(i)%nodes(:) = -99999
end do
do i=1,m%numSimpleFluxBoundaries
   m%simpleFluxBoundaries(i)%nodes(:) = -99999
end do
do i=1,m%numExternalFluxBoundaries
   m%externalFluxBoundaries(i)%nodes(:) = -99999
   m%externalFluxBoundaries(i)%barlanht(:) = -99999.d0
   m%externalFluxBoundaries(i)%barlancfsp(:) = -99999.d0
end do
do i=1,m%numInternalFluxBoundaries
   m%internalFluxBoundaries(i)%nodes(:) = -99999
   m%internalFluxBoundaries(i)%ibconn(:) = -99999
   m%internalFluxBoundaries(i)%barinht(:) = -99999.d0
   m%internalFluxBoundaries(i)%barincfsb(:) = -99999.d0
   m%internalFluxBoundaries(i)%barincfsp(:) = -99999.d0
end do
do i=1,m%numInternalFluxBoundariesWithPipes
   m%internalFluxBoundariesWithPipes(i)%nodes(:) = -99999
   m%internalFluxBoundariesWithPipes(i)%ibconn(:) = -99999
   m%internalFluxBoundariesWithPipes(i)%barinht(:) = -99999.d0
   m%internalFluxBoundariesWithPipes(i)%barincfsb(:) = -99999.d0
   m%internalFluxBoundariesWithPipes(i)%barincfsp(:) = -99999.d0
   m%internalFluxBoundariesWithPipes(i)%pipeht(:) = -99999.d0
   m%internalFluxBoundariesWithPipes(i)%pipecoef(:) = -99999.d0
   m%internalFluxBoundariesWithPipes(i)%pipediam(:) = -99999.d0
end do

!------------------------------------------------------------------
end subroutine allocateBoundaryArrays
!------------------------------------------------------------------

!-----+---------+---------+---------+---------+---------+---------+
!                   S U B R O U T I N E
!  C O N S T R U C T   F L U X   B O U N D A R I E S   A R R A Y
!-----+---------+---------+---------+---------+---------+---------+
subroutine constructFluxBoundaryTypesArray(m)
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: i
integer :: j
integer :: k
integer :: jgw
!
jgw = 1
do k=1,m%nbou
   do j=1,m%nvell(k)
      m%lbcodei(jgw) = m%ibtype(k)
      jgw = jgw + 1
   end do
end do
! determine if there are any specified flux boundaries in the mesh
do i=1,size(specifiedFluxBoundaryTypes)
   if (any(m%lbcodei.eq.specifiedFluxBoundaryTypes(i))) then
      m%nfluxf = 1 ! => must find b.c.s in fort.15 or fort.20
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
subroutine writeMesh(m)
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: i, j, k, jn, je, nhy
integer :: iunit
integer :: ios     ! i/o status
integer :: lineNum ! line number currently being read
!
! initialization
m%nfluxf = 0
nhy = 3
!
if (trim(m%meshFileName).eq."null") then
   write(6,'(a)',advance='no') "Enter name of the mesh file: "
   read(5,'(a)') m%meshFileName
endif
iunit = availableUnitNumber()
open(unit=iunit,file=trim(m%meshFileName),status='replace',action='write')
if (verbose.eqv..true.) then
   write(6,'("Number of elevation specified boundaries (m%nope): ",i0,".")') m%nope
   write(6,'("Number of simple flux specified boundaries (0,1,2,etc): ",i0,".")') m%numSimpleFluxBoundaries
   write(6,'("Number of external flux boundaries (3,etc): ",i0,".")') m%numExternalFluxBoundaries
   write(6,'("Number of internal flux boundaries (4,etc): ",i0,".")') m%numInternalFluxBoundaries
   write(6,'("Number of internal flux boundaries with pipes (5,etc): ",i0,".")') m%numInternalFluxBoundariesWithPipes
endif
write(6,'(a)') 'INFO: Writing node table to "' // trim(m%meshFileName) // '".'
lineNum = 1
write(unit=iunit,fmt='(a)',err=10,iostat=ios) trim(m%agrid)
lineNum = lineNum + 1
write(unit=iunit,fmt='(2(i0,1x),a)',err=10,iostat=ios) m%ne, m%np, &
  '! number of elements (ne), number of nodes (np)'
lineNum = lineNum + 1
write(unit=iunit,fmt='(i0,3(1x,f15.7),a)',err=10,iostat=ios) &
   1, (m%xyd(j,1), j=1,3), ' ! node table: node number, x, y, depth'
lineNum = lineNum + 1
do k = 2, m%np
   write(unit=iunit,fmt='(i0,3(1x,f15.7))',err=10,iostat=ios) &
      k, (m%xyd(j,k), j=1,3)
   lineNum = lineNum + 1
enddo
write(6,'(a)') 'INFO: Writing element table to ' // trim(m%meshFileName) // '".'
write(unit=iunit,fmt='(5(i0,2x),a)',err=10,iostat=ios) 1, nhy, ( m%nm(1,j), j = 1, 3 ), &
   '! element table : element number, number of nodes per element, node numbers counter clockwise around the element '
lineNum = lineNum + 1
do k = 2, m%ne
   write(unit=iunit,fmt='(5(i0,2x))',err=10,iostat=ios) k, nhy, ( m%nm(k,j), j = 1, 3 )
   lineNum = lineNum + 1
enddo
write(6,'(a)') 'INFO: Writing boundaries to '  // trim(m%meshFileName) // '".'
write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) m%nope, &
   ' ! total number of elevation specified boundary segments (nope)'
lineNum = lineNum + 1
write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) m%neta, &
   ' ! total number of nodes on elevation specified boundaries (neta) (consistency check)'
lineNum = lineNum + 1
do k = 1, m%nope
   write(unit=iunit,fmt='(i0,1x,i0,a,i0)',err=10,iostat=ios) m%nvdll(k), 0, &
   ' ! number of nodes and boundary type of elevation specified boundary (nvdll, ibtypee) segment number ', k
   lineNum = lineNum + 1
   write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) m%elevationBoundaries(k)%nodes(1), &
      ' ! list of nodes on this elevation specified boundary segment (nbdv)'
   lineNum = lineNum + 1
   do j = 2, m%nvdll(k)
      write(unit=iunit,fmt='(i0)',err=10,iostat=ios) m%elevationBoundaries(k)%nodes(j)
      lineNum = lineNum + 1
   enddo
enddo
write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) m%nbou, &
   ' ! total number of flux boundary segments (nbou)'
lineNum = lineNum + 1
write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios) m%nvel, &
   ' ! total number of nodes on flux boundaries (nvel) (consistency check)'
lineNum = lineNum + 1
m%sfCount = 1
m%efCount = 1
m%ifCount = 1
m%ifwpCount = 1
do k = 1, m%nbou
   write(unit=iunit,fmt='(2(i0,2x),a)',err=10,iostat=ios) m%nvell(k), m%ibtype_orig(k), &
      '! number of nodes and boundary type of flux boundary (m%nvell, ibtype)'
   lineNum = lineNum + 1
   select case(m%ibtype_orig(k))
   case(0,1,2,10,11,12,20,21,22,30,52)
      write(unit=iunit,fmt='(i0,a)',err=10,iostat=ios)  &
         m%simpleFluxBoundaries(m%sfCount)%nodes(1), ' ! nodes on this boundary (nbvv)'
         lineNum = lineNum + 1
      do j = 2, m%nvell(k)
         write(unit=iunit,fmt='(i0)',err=10,iostat=ios)  &
            m%simpleFluxBoundaries(m%sfCount)%nodes(j)
         lineNum = lineNum + 1
      end do
      m%sfCount = m%sfCount + 1
   case(3,13,23)
      write(unit=iunit,fmt='(i0,1x,f8.3,1x,f8.3,a)',err=10,iostat=ios) &
                       m%externalFluxBoundaries(m%efCount)%nodes(1), &
                       m%externalFluxBoundaries(m%efCount)%barlanht(1), &
                       m%externalFluxBoundaries(m%efCount)%barlancfsp(1), &
      ' ! boundary node (nbvv), barrier height (barlanht), and coef of. supercritical flow (barlancfsp)'
      lineNum = lineNum + 1
      do j = 2, m%nvell(k)
         write(unit=iunit,fmt='(i0,1x,f8.3,1x,f8.3)',err=10,iostat=ios) &
                       m%externalFluxBoundaries(m%efCount)%nodes(j), &
                       m%externalFluxBoundaries(m%efCount)%barlanht(j), &
                       m%externalFluxBoundaries(m%efCount)%barlancfsp(j)
         lineNum = lineNum + 1
      end do
      m%efCount = m%efCount + 1
   case(4,24)
      write(unit=iunit,fmt=*,err=10,iostat=ios) &
                       m%internalFluxBoundaries(m%ifCount)%nodes(1), &
                       m%internalFluxBoundaries(m%ifCount)%ibconn(1), &
                       m%internalFluxBoundaries(m%ifCount)%barinht(1), &
                       m%internalFluxBoundaries(m%ifCount)%barincfsb(1), &
                       m%internalFluxBoundaries(m%ifCount)%barincfsp(1), &
      ' ! boundary node (nbvv), connected backface node (ibconn), ' // &
      'barrier height (barinht), coef. of subcrit. flow (barincfsb), ' // &
      'coef. of supercrit. flow (barincfsp) '
      lineNum = lineNum + 1
      do j = 2, m%nvell(k)
         write(unit=iunit,fmt='(2(i0,1x),3(f8.3,1x))',err=10,iostat=ios) &
                       m%internalFluxBoundaries(m%ifCount)%nodes(j), &
                       m%internalFluxBoundaries(m%ifCount)%ibconn(j), &
                       m%internalFluxBoundaries(m%ifCount)%barinht(j), &
                       m%internalFluxBoundaries(m%ifCount)%barincfsb(j), &
                       m%internalFluxBoundaries(m%ifCount)%barincfsp(j)
         lineNum = lineNum + 1
      end do
      m%ifCount = m%ifCount + 1
   case(5,25)
      write(unit=iunit,fmt='(2(i0,1x),6(f8.3),a)',err=10,iostat=ios) &
                    m%internalFluxBoundariesWithPipes(m%ifwpCount)%nodes(1), &
                    m%internalFluxBoundariesWithPipes(m%ifwpCount)%ibconn(1), &
                    m%internalFluxBoundariesWithPipes(m%ifwpCount)%barinht(1), &
                    m%internalFluxBoundariesWithPipes(m%ifwpCount)%barincfsb(1), &
                    m%internalFluxBoundariesWithPipes(m%ifwpCount)%barincfsp(1), &
                    m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipeht(1), &
                    m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipecoef(1), &
                    m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipediam(1), &
      ' ! boundary node (nbvv), connected backface node (ibconn), ' // &
      'barrier height (barinht), coef. of subcrit. flow (barincfsb), ' // &
      'coef. of supercrit. flow (barincfsp), pipe height (pipeht), ' // &
      ' pipe coef. (pipecoef), pipediameter(pipediam)'
      lineNum = lineNum + 1
      do j = 2, m%nvell(k)
         write(unit=iunit,fmt='(2(i0,1x),6(f8.3))',err=10,iostat=ios) &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%nodes(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%ibconn(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%barinht(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%barincfsb(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%barincfsp(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipeht(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipecoef(j), &
                       m%internalFluxBoundariesWithPipes(m%ifwpCount)%pipediam(j)
         lineNum = lineNum + 1
      end do
      m%ifwpCount = m%ifwpCount + 1
   case default
      write(6,'("ERROR: IBTYPE ",i0," is not allowed.")') m%ibtype_orig(k)
      stop
   end select
end do
close(iunit)
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


!------------------------------------------------------------------
!   S U B R O U T I N E   W R I T E   M E S H
!------------------------------------------------------------------
!  Writes the mesh file header to adcirc ascii fort.14 format.
!------------------------------------------------------------------
subroutine writeMeshHeaderASCII(m)
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: iunit
integer :: ios
!
iunit = availableUnitNumber()
open(unit=iunit,file=trim(m%meshFileName)//'-with-boundaries.14',status='replace',action='write')
write(unit=iunit,fmt='(a)',iostat=ios) trim(m%agrid)
write(unit=iunit,fmt='(2(i0,1x),a)',iostat=ios) m%ne, m%np, &
   '! number of elements (ne), number of nodes (np)'
close(iunit)
!------------------------------------------------------------------
end subroutine writeMeshHeaderASCII
!------------------------------------------------------------------

!------------------------------------------------------------------
!                   S U B R O U T I N E
!  W R I T E   M E S H   N O D E   T A B L E   A S C I I
!------------------------------------------------------------------
!  Writes the mesh node table header to adcirc ascii fort.14 format.
!------------------------------------------------------------------
subroutine writeMeshNodeTableASCII(m)
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: i, j, k, jn, je, nhy
integer :: iunit
integer :: ios     ! i/o status
!
iunit = availableUnitNumber()
open(unit=iunit,file=trim(m%meshFileName)//'-with-boundaries.14',status='old',action='write')
write(6,'(a)') 'INFO: Writing node table to "' // trim(m%meshFileName) // '".'
write(unit=iunit,fmt='(i0,3(1x,f15.7),a)',iostat=ios) &
   1, (m%xyd(j,1), j=1,3), ' ! node table: node number, x, y, depth'
do k = 2, m%np
   write(unit=iunit,fmt='(i0,3(1x,f15.7))',iostat=ios) &
      k, (m%xyd(j,k), j=1,3)
enddo
close(iunit)
!------------------------------------------------------------------
end subroutine writeMeshNodeTableASCII
!------------------------------------------------------------------

!------------------------------------------------------------------
!                   S U B R O U T I N E
!  W R I T E   M E S H    E L E M E N T   T A B L E   A S C I I
!------------------------------------------------------------------
!  Writes the mesh element table header to adcirc ascii fort.14 format.
!------------------------------------------------------------------
subroutine writeMeshElementTableASCII(m)
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: iunit
integer :: i, j, k, jn, je
integer, parameter :: nhy = 3
integer :: ios     ! i/o status
!
iunit = availableUnitNumber()
open(unit=iunit,file=trim(m%meshFileName)//'-with-boundaries.14',status='old',action='write')
write(6,'(a)') 'INFO: Writing element table to ' // trim(m%meshFileName) // '".'
write(unit=iunit,fmt='(5(i0,2x),a)',iostat=ios) 1, nhy, ( m%nm(1,j), j = 1, 3 ), &
   '! element table : element number, number of nodes per element, node numbers counter clockwise around the element '
do k = 2, m%ne
   write(unit=iunit,fmt='(5(i0,2x))',iostat=ios) k, nhy, ( m%nm(k,j), j = 1, 3 )
enddo
close(iunit)
!------------------------------------------------------------------
end subroutine writeMeshElementTableASCII
!------------------------------------------------------------------


!------------------------------------------------------------------
!                   S U B R O U T I N E
!  W R I T E   M E S H    E L E V A T I O N   B O U N D A R Y   T A B L E   A S C I I
!------------------------------------------------------------------
! Writes the elevation specified boundary table in adcirc ascii
! format to the fort.14 file.
!
! Uses data related to boundary finding, rather than data read
! in from a fort.14 file.
!------------------------------------------------------------------
subroutine writeMeshElevationBoundaryTableASCII(m)
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: nobn    ! number of nodes on this open boundary
integer :: iunit
integer :: ios     ! i/o status
integer :: i, j, k
!
iunit = availableUnitNumber()
open(unit=iunit,file=trim(m%meshFileName)//'-with-boundaries.14',status='old',action='write')
write(6,'(a)') 'INFO: Writing elevation boundary table to ' // trim(m%meshFileName) // '".'

! Compute the total number of open boundary nodes
m%itotobn=0
do k=1,m%nob
   m%itotobn=m%itotobn+(m%bsegen(k)-m%bsegbn(k))+1
end do

! Write the open boundary information
write(iunit,*) m%nob
write(iunit,*) m%itotobn
do k=1,m%nob
   nobn=m%bsegen(k)-m%bsegbn(k)+1
   write(iunit,*) nobn
   write(iunit,*) m%extbns(m%bsegbn(k))
   do j=m%bsegbn(k)+1,m%bsegen(k)-1
      write(iunit,*) m%extbns(j)
      m%extbns(j)=0
   end do
   write(iunit,*) m%extbns(m%bsegen(k))
end do

close(iunit)
!------------------------------------------------------------------
end subroutine writeMeshElevationBoundaryTableASCII
!------------------------------------------------------------------

!------------------------------------------------------------------
!                   S U B R O U T I N E
!  W R I T E   M E S H    F L U X    B O U N D A R Y   T A B L E   A S C I I
!------------------------------------------------------------------
! Writes the flux specified boundary table in adcirc ascii
! format to the fort.14 file.
!
! Uses data related to boundary finding, rather than data read
! in from a fort.14 file.
!------------------------------------------------------------------
subroutine writeMeshFluxBoundaryTableASCII(m)
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: nlbn    ! number of nodes on this open boundary
integer :: iunit
integer :: ios     ! i/o status
integer :: i, j, k
!
iunit = availableUnitNumber()
open(unit=iunit,file=trim(m%meshFileName)//'-with-boundaries.14',status='old',action='write')
write(6,'(a)') 'INFO: Writing flux specified boundary table to ' // trim(m%meshFileName) // '".'

! Write the land boundary information

write(iunit,*) m%nlb
write(iunit,*) m%itotlbn
do k=1,m%nexlb
   nlbn=m%bsegen(k)-m%bsegbn(k)+1
   if((m%nob.eq.0).and.(m%nexlb.eq.1)) nlbn=nlbn+1
   write(iunit,*) nlbn,m%ibtype(k)
   write(iunit,*) m%extbns(m%bsegbn(k))
   do j=m%bsegbn(k)+1,m%bsegen(k)-1
      write(iunit,*) m%extbns(j)
      m%extbns(j)=0
   end do
   write(iunit,*) m%extbns(m%bsegen(k))
   end do
if((m%nob.eq.0).and.(m%nexlb.eq.1)) write(iunit,*) m%extbns(m%bsegen(k)+1)

close(iunit)
!------------------------------------------------------------------
end subroutine writeMeshFluxBoundaryTableASCII
!------------------------------------------------------------------


!----------------------------------------------------------------------
!                  S U B R O U T I N E
!     W R I T E   M E S H   D E F I N I T I O N S  T O   N E T C D F
!----------------------------------------------------------------------
!     This subroutine writes the mesh parameters to the netcdf file.
!----------------------------------------------------------------------
subroutine writeMeshDefinitionsToNetCDF(m, n, nc_id, deflate)
use netcdf
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
type(meshNetCDF_t), intent(inout) :: n ! netcdf IDs for the target file
integer, intent(in) :: nc_id
logical, intent(in) :: deflate
integer             :: NC_DimID_single
integer :: ncStatus
!
! create and store mesh dimensions
write(6,'(a)') 'INFO: Writing mesh definitions to netcdf.'
CALL Check(NF90_PUT_ATT(NC_ID,NF90_GLOBAL,'agrid',trim(m%agrid)))
! check to see if the node dimension is already there
ncStatus = nf90_inq_dimid(NC_ID,'node',n%NC_DimID_node)
if ( ncStatus.ne.NF90_NOERR ) then
   CALL Check(NF90_DEF_DIM(NC_ID,'node',m%np,n%NC_DimID_node))
endif
ncStatus = nf90_inq_dimid(NC_ID,'nele',n%NC_DimID_nele)
if ( ncStatus.ne.NF90_NOERR ) then
   CALL Check(NF90_DEF_DIM(NC_ID,'nele',m%ne,n%NC_DimID_nele))
endif
CALL Check(NF90_DEF_DIM(NC_ID,'nvertex',3,n%NC_DimID_nvertex))
CALL Check(NF90_DEF_DIM(NC_ID,'single',1,NC_DimID_single))
! boundary parameters
if (m%writeBoundaryTable.eqv..true.) then
   if (m%nope.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'m%nope',m%nope,n%NC_DimID_nope))
   if (m%nvdll_max.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'max_nvdll',m%nvdll_max,n%NC_DimID_max_nvdll))
   if (m%neta.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'neta',m%neta,n%NC_DimID_neta))
   if (m%nbou.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'nbou',m%nbou,n%NC_DimID_nbou))
   if (m%nvel.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'nvel',m%nvel,n%NC_DimID_nvel))
   if (m%nvell_max.ne.0) CALL Check(NF90_DEF_DIM(NC_ID,'max_m%nvell',m%nvell_max,n%NC_DimID_max_nvell))
endif
! ibtypee, ibconn, bars are ignored
CALL Check(NF90_DEF_VAR(NC_ID,'x',NF90_DOUBLE,n%NC_DimID_node,n%NC_VarID_x))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_x,'long_name','longitude'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_x,'standard_name','longitude'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_x,'units','degrees_east'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_x,'positive','east'))

CALL Check(NF90_DEF_VAR(NC_ID,'y',NF90_DOUBLE,n%NC_DimID_node,n%NC_VarID_y))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_y,'long_name','latitude'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_y,'standard_name','latitude'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_y,'units','degrees_north'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_y,'positive','north'))

CALL Check(NF90_DEF_VAR(NC_ID,'element',NF90_int,(/ n%NC_DimID_nvertex, n%NC_DimID_nele /), n%NC_VarID_element))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_element,'long_name','element'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_element,'standard_name','face_node_connectivity'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_element,'units','nondimensional'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_element,'start_index',1))

if (m%writeBoundaryTable.eqv..true.) then
   if (m%nope.ne.0) then
      CALL Check(NF90_DEF_VAR(NC_ID,'nvdll',NF90_DOUBLE,n%NC_DimID_nope,n%NC_VarID_nvdll))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_nvdll,'long_name','total number of nodes in each elevation specified & boundary segment'))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_nvdll,'units','nondimensional'))

      CALL Check(NF90_DEF_VAR(NC_ID,'max_nvdll',NF90_int,NC_DimID_single,n%NC_VarID_max_nvdll))
      CALL Check(NF90_DEF_VAR(NC_ID,'max_nvell',NF90_int,NC_DimID_single,n%NC_VarID_max_nvell))
      CALL Check(NF90_DEF_VAR(NC_ID,'neta',NF90_int,NC_DimID_single,n%NC_VarID_neta))
      CALL Check(NF90_DEF_VAR(NC_ID,'nope',NF90_int,NC_DimID_single,n%NC_VarID_nope))
      CALL Check(NF90_DEF_VAR(NC_ID,'nvel',NF90_int,NC_DimID_single,n%NC_VarID_nvel))

      CALL Check(NF90_DEF_VAR(NC_ID,'nbdv',NF90_DOUBLE,(/ n%NC_DimID_nope, n%NC_DimID_max_nvdll /), n%NC_VarID_nbdv))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_nbdv,'long_name','node numbers on each elevation specified boundary & segment'))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_nbdv,'units','nondimensional'))
   endif

   if (m%nbou.ne.0) then
      CALL Check(NF90_DEF_VAR(NC_ID,'nvell',NF90_DOUBLE,n%NC_DimID_nbou,n%NC_VarID_nvell))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_nvell,'long_name','number of nodes in each normal flow specified boundary segment'))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_nvell,'units','nondimensional'))

      CALL Check(NF90_DEF_VAR(NC_ID,'ibtype',NF90_DOUBLE,n%NC_DimID_nbou,n%NC_VarID_ibtype))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_ibtype,'long_name','type of normal flow (discharge) boundary'))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_ibtype,'units','nondimensional'))

      CALL Check(NF90_DEF_VAR(NC_ID,'nbvv',NF90_DOUBLE,(/ n%NC_DimID_nbou, n%NC_DimID_max_nvell /),n%NC_VarID_nbvv))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_nbvv,'long_name','node numbers on normal flow boundary segment'))
      CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_nbvv,'units','nondimensional'))
   endif
endif

CALL Check(NF90_DEF_VAR(NC_ID,'depth',NF90_DOUBLE,n%NC_DimID_node,n%NC_VarID_depth))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_depth,'long_name','distance from geoid'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_depth,'standard_name','depth_below_geoid'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_depth,'coordinates','time y x'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_depth,'location','node'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_depth,'mesh','adcirc_mesh'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_depth,'units','m'))
!      CALL Check(NF90_PUT_ATT(NC_ID,NC_VarID_depth,'positive','down')) !DO NOT USE?

CALL Check(NF90_DEF_VAR(NC_ID,'adcirc_mesh',NF90_INT,NC_DimID_single,n%NC_VarID_mesh))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_mesh,'long_name','mesh topology'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_mesh,'standard_name','mesh_topology'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_mesh,'dimension',2))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_mesh,'node_coordinates','x y'))
CALL Check(NF90_PUT_ATT(NC_ID,n%NC_VarID_mesh,'face_node_connectivity','element'))

#ifdef NETCDF_CAN_DEFLATE

! automatically turn on compression if it is available
if (deflate.eqv..true.) then
   if (m%readBoundaryTable.eqv..true.) then
      if (m%nope.ne.0) then
         call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_nvdll, 0, 1, 2))
         call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_nbdv, 0, 1, 2))
      endif
      if (m%nbou.ne.0) then
         call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_nvell, 0, 1, 2))
         call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_ibtype, 0, 1, 2))
         call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_nbvv, 0, 1, 2))
      endif
   endif
   call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_x, 0, 1, 2))
   call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_y, 0, 1, 2))
   call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_element, 0, 1, 2))
   call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_depth, 0, 1, 2))
   call check(nf90_def_var_deflate(NC_ID, n%NC_VarID_Mesh, 0, 1, 2))
endif

#endif

write(6,'(a)') 'INFO: Finished writing mesh definitions to netcdf.'
!----------------------------------------------------------------------
end subroutine writeMeshDefinitionsToNetCDF
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!                    S U B R O U T I N E
!         W R I T E   M E S H   D A T A   T O   N E T C D F
!----------------------------------------------------------------------
!     This subroutine writes the mesh parameters to the netcdf file.
!----------------------------------------------------------------------
subroutine writeMeshDataToNetCDF(m, n, nc_id)
use netcdf
use ioutil
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
type(meshNetCDF_t), intent(inout) :: n ! netcdf IDs to operate on
integer, intent(in) :: nc_id
integer :: nc_count(2)
integer :: nc_start(2)
integer :: i, j
write(6,'(a)') 'INFO: Writing mesh data to netcdf.'
! place mesh-related data into the file
NC_Count = (/ m%np, 1 /)
NC_Start = (/ 1, 1 /)
CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_x,m%xyd(1,1:m%np),NC_Start,NC_Count))
CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_y,m%xyd(2,1:m%np),NC_Start,NC_Count))
CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_depth,m%xyd(3,1:m%np),NC_Start,NC_Count))
NC_Count = (/ 3, m%ne /)
!
! populate netcdf-style element table
do i=1, m%ne
   do j=1, 3
      m%nmnc(j,i) = m%nm(i,j)
   end do
end do
CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_element,m%nmnc,NC_Start,NC_Count))
!
! boundary parameters
if (m%writeBoundaryTable.eqv..false.) then
   write(6,'(a)') 'INFO: Mesh has been written to the netCDF file.'
   return
endif
if (m%nope.ne.0) then
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_nope,m%nope))
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_max_nvell,m%nvell_max))
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_max_nvdll,m%nvdll_max))
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_neta,m%neta))
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_nvel,m%nvel))
   NC_Count = (/ m%nope, 1 /)
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_nvdll,m%nvdll,NC_Start,NC_Count))
   NC_Count = (/ m%nope, m%nvdll_max /)
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_nbdv,m%nbdv,NC_Start,NC_Count))
endif
if (m%nbou.ne.0) then
   NC_Count = (/ m%nbou, 1 /)
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_nvell,m%nvell,NC_Start,NC_Count))
   NC_Count = (/ m%nbou, 1 /)
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_ibtype,m%ibtype,NC_Start,NC_Count))
   NC_Count = (/ m%nbou, m%nvell_max /)
   CALL Check(NF90_PUT_VAR(NC_ID,n%NC_VarID_nbvv,m%nbvv,NC_Start,NC_Count))
end if

write(6,'(a)') 'INFO: Mesh data has been written to the netCDF file.'
!----------------------------------------------------------------------
end subroutine writeMeshDataToNetCDF
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! S U B R O U T I N E    C O M P U T E   N E I G H B O R   T A B L E
!-----------------------------------------------------------------------
!
!  Subroutine to generate neighbor tables from a connectivity table.
!
!  NOTES
!  a node neighbor table is generated with the node itself is listed as
!     neighbor #1 and all other neighbors are sorted and placed in cw
!     order from east
!  a neighbor element table is generated with:
!     entry 1 = element # defined by neighbors 1,2,3
!     entry 2 = element # defined by neighbors 1,3,4
!     entry 3 = element # defined by neighbors 1,4,5
!      .......
!     entry last = element # defined by neighbors 1,nneigh,2
!     a zero area means that the defined triangle lies outside the domain
!
!-----------------------------------------------------------------------
!
! VARIABLE DEFINITIONS:
!   NE - NUMBER OF ELEMENTS
!   NP - NUMBER OF NODES
!   NM(MNE,3) - NODE NUMBERS ASSOCIATED WITH EACH ELEMENT
!   NNeigh(MNP) NUMBER OF NEIGHBORS FOR EACH NODE
!   NeiTab(MNP,NEIMAX) 2D ARRAY OF NEIGHBORS FOR EACH NODE
!   NeiTabEle(MNP,NEIMAX) 2D ARRAY OF NEIGHBOR ELEMENTS FOR EACH NODE
!   nNeighEle(MNP) Number of neighbor elements at each node
!   NEIMIN - 1+MINIMUM NUMBER OF NEIGHBORS FOR ANY NODE
!   NEIMAX - 1+MAXIMUM NUMBER OF NEIGHBORS FOR ANY NODE
!
!-----------------------------------------------------------------------
subroutine computeNeighborTable(m)
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on
double precision, allocatable :: angle(:)
integer, allocatable :: neitem(:)
double precision :: anglelow
double precision :: anglemore
double precision :: delx
double precision :: dely
double precision :: dist
integer :: nn1, nn2, nn3 ! node numbers around an element
integer :: ne1, ne2, ne3 ! element numbers
integer :: i, j, jj, jlow, k, n, e  ! loop counters
!
! Initialization
if (m%cppComputed.eqv..false.) then
   call computeCPP(m)
endif
jlow = 0
ne2 = 0
ne3 = 0
!
! For interior nodes, the number of neighbor nodes around any node is
! equal to the number of elements that contain that node. Boundary nodes
! will have one additional neighboring node.
allocate(m%nneigh(m%np))
m%nneigh = 0
do e=1,m%ne
   do j=1,3
      ! increment the number of nodal neighbors this node has
      m%nneigh(m%nm(e,j)) = m%nneigh(m%nm(e,j)) + 1
   end do
end do
m%mnei = maxval(m%nneigh)
m%mnei = m%mnei + 2 ! +1 to include the node itself, +1 in case its a boundary node
!
allocate(m%neitab(m%np,m%mnei))
allocate(m%neitabele(m%np,m%mnei))
allocate(angle(m%mnei))
allocate(m%nneighele(m%np))
m%neighborTableComputed = .true.
allocate(m%neiTabGenerated(m%np,m%mnei))
allocate(m%neiTabEleGenerated(m%np,m%mnei))
!
! initialize neighbor tables to invalid values to make it easy to
! spot issues
m%NNeigh=0
m%NNeighEle=0
m%NeiTab=-99
m%NeiTabEle=-99
DO 10 e=1,m%NE
   NN1 = m%NM(e,1)
   NN2 = m%NM(e,2)
   NN3 = m%NM(e,3)
   m%NNeighEle(NN1)=m%NNeighEle(NN1)+1 ! increment the number of elements that neighbor this node
   m%NNeighEle(NN2)=m%NNeighEle(NN2)+1
   m%NNeighEle(NN3)=m%NNeighEle(NN3)+1
   m%NeiTabEle(NN1,m%NNeighEle(NN1))=e ! add element e to the neighboring elements list for this node
   m%NeiTabEle(NN2,m%NNeighEle(NN2))=e
   m%NeiTabEle(NN3,m%NNeighEle(NN3))=e
   !
   ! repeat for the number of nodal neighbors of node 1 on element n
   DO J=1,m%NNeigh(NN1)
      ! check to see if node 2 is already in node 1's neighbor list; if so skip adding it
      IF(NN2.EQ.m%NeiTab(NN1,J)) GOTO 25
   END DO
   m%NNeigh(NN1)=m%NNeigh(NN1)+1 ! increment the number of neighbors of node 1 on element n
   m%NNeigh(NN2)=m%NNeigh(NN2)+1 ! increment the number of neighbors of node 2 on element n
   ! if the number of neighbors on node 1 or node 2 of element n exceeds the limit, terminate
   IF((m%NNeigh(NN1).GT.m%MNEI-1).OR.(m%NNeigh(NN2).GT.m%MNEI-1)) GOTO 999
   ! add node 1 and node 2 to each other's neighbor lists
   m%NeiTab(NN1,m%NNeigh(NN1))=NN2
   m%NeiTab(NN2,m%NNeigh(NN2))=NN1

25      CONTINUE
   DO J=1,m%NNeigh(NN1)
      IF(NN3.EQ.m%NeiTab(NN1,J)) GOTO 35
   END DO
   m%NNeigh(NN1)=m%NNeigh(NN1)+1
   m%NNeigh(NN3)=m%NNeigh(NN3)+1
   IF((m%NNeigh(NN1).GT.m%MNEI-1).OR.(m%NNeigh(NN3).GT.m%MNEI-1)) GOTO 999
   m%NeiTab(NN1,m%NNeigh(NN1))=NN3
   m%NeiTab(NN3,m%NNeigh(NN3))=NN1

35      CONTINUE
   DO J=1,m%NNeigh(NN2)
      IF(NN3.EQ.m%NeiTab(NN2,J)) GOTO 10
   END DO
   m%NNeigh(NN2)=m%NNeigh(NN2)+1
   m%NNeigh(NN3)=m%NNeigh(NN3)+1
   IF((m%NNeigh(NN2).GT.m%MNEI-1).OR.(m%NNeigh(NN3).GT.m%MNEI-1)) GOTO 999
   m%NeiTab(NN2,m%NNeigh(NN2))=NN3
   m%NeiTab(NN3,m%NNeigh(NN3))=NN2

10   CONTINUE
m%neiTabGenerated = m%neiTab
m%neiTabEleGenerated = m%neiTabEle
!
!     INSERT NODE ITSELF IN PLACE #1 and SORT other NEIGHBORS by
!     increasing cw angle from East
!
! loop over nodes
allocate(neitem(m%mnei))
DO I=1,m%NP
   ! loop over nodal neighbors of node i
   DO J=1,m%NNeigh(I)
      ! save the node number of the jth neighbor of node i
      NEITEM(J)=m%NeiTab(I,J)
      ! compute the distance from the jth neighbor of node i to node i
      DELX=m%x_cpp(NEITEM(J))-m%x_cpp(I)
      DELY=m%y_cpp(NEITEM(J))-m%y_cpp(I)
      DIST=SQRT(DELX*DELX+DELY*DELY)
      ! check for identical coordinates
      IF(DIST.EQ.0.0d0) GOTO 998
      ! compute the trigonometric angle between node i and
      ! its jth neighbor in degrees
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
   ! repeat for the number of neighbors of node i
   DO JJ=1,m%NNeigh(I)
      ! initialize the value of the low angle???
      ANGLELOW=400.d0
      ! loop over the neighbors of node i
      DO J=1,m%NNeigh(I)
         IF((ANGLE(J).LT.ANGLELOW).AND.(ANGLE(J).GT.ANGLEMORE)) THEN
            ANGLELOW=ANGLE(J)
            JLOW=J
         ENDIF
      END DO
      m%NeiTab(I,JJ+1)=NEITEM(JLOW)
      ANGLEMORE=ANGLELOW
   END DO
   m%NeiTab(I,1)=I
   m%NNeigh(I)=m%NNeigh(I)+1
ENDDO
!
!     MATCH EACH SET OF 3 NODES WITH CORRESPONDING ELEMENT AND REORDER
!     ELEMENTS ACCORDINGLY
!
DO I=1,m%NP
   ! temporarily save the existing array of neighboring elements for this node
   neiTem(:)=-99
   DO K=1,m%NNeighEle(I)
      NEITEM(K)=m%NeiTabEle(I,K)
      m%NeiTabEle(I,K)=-99
   END DO
   ! loop over the nodal neighbors of node i
   DO J=2,m%NNeigh(I)
      NN1=m%NeiTab(I,1) ! node 1 is the node itself (node i)
      NN3=m%NeiTab(I,J) ! node 3 is the Jth nodal neighbor of this node
      !
      ! if j is not the last neighbor then node 2 is the J+1th nodal neighbor
      if (J.NE.m%NNeigh(I)) then
         NN2=m%NeiTab(I,J+1)
      endif
      !
      ! if j is the last neighbor, then node 2 is the first nodal neighbor
      if (J.EQ.m%NNeigh(I)) then
         NN2=m%NeiTab(I,2)
      endif
      !
      ! loop over the elemental neighbors of node i
      DO K=1,m%nNeighEle(I)
         ! if the neighboring element number is not uninitialized
         ! (why do we need to check this??)
         IF((NEITEM(K).NE.-99).and.(neitem(k).ne.0)) THEN
            ! if resident node 1 of the kth elemental neighbor is the same
            ! as node i, then set the node order 1, 2, 3
            IF(m%NM(NEITEM(K),1).EQ.NN1) THEN
               NE1=m%NM(NEITEM(K),1)
               NE2=m%NM(NEITEM(K),2)
               NE3=m%NM(NEITEM(K),3)
            ENDIF
            ! if resident node 2 of the kth elemental neighbor is the same
            ! as node i, then set the node order 2, 3, 1
            IF(m%NM(NEITEM(K),2).EQ.NN1) THEN
               NE1=m%NM(NEITEM(K),2)
               NE2=m%NM(NEITEM(K),3)
               NE3=m%NM(NEITEM(K),1)
            ENDIF
            ! if resident node 3 of the kth elemental neighbor is the same
            ! as node i, then set the node order 3, 1, 2
            IF(m%NM(NEITEM(K),3).EQ.NN1) THEN
               NE1=m%NM(NEITEM(K),3)
               NE2=m%NM(NEITEM(K),1)
               NE3=m%NM(NEITEM(K),2)
            ENDIF
            ! if
            IF((NE2.EQ.NN2).AND.(NE3.EQ.NN3)) THEN
               m%NeiTabEle(I,J-1)=NEITEM(K)
               NEITEM(K)=0
            ENDIF
         else
            ! there is no element corresponding to these nodes
         ENDIF
      END DO
   END DO
END DO
!
!  DETERMINE THE MAXIMUM AND MINIMUM NUMBER OF NEIGHBORS
m%NEIMAX = maxval(m%NNeigh)
m%NEIMIN = minval(m%NNeigh)
!  Deallocate local work arrays
deallocate ( angle )
deallocate ( neitem )
return

999  CONTINUE
WRITE(6,'("ERROR: Computation of neighbor table failed.")')
STOP
998  CONTINUE
WRITE(6,'("ERROR: Nodes ",i0," and ",i0," have the same coordinates.")') i, neiTem(j)
STOP
!-----------------------------------------------------------------------
END SUBROUTINE computeNeighborTable
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!                         S U B R O U T I N E
!   C O M P U T E   N E I G H B O R   E D G E   L E N G T H   T A B L E
!-----------------------------------------------------------------------
!     jgf: Very short subroutine to compute the length of each edge
!     attached to a node. Also finds the minimum edge length in the mesh,
!     the minimum and maximum edge length around a node, the midpoint
!     cpp coordinates of each edge, and the maximum edge length gradient
!     around each node.
!-----------------------------------------------------------------------
subroutine computeNeighborEdgeLengthTable(m)
implicit none
type(mesh_t), intent(inout) :: m  ! mesh to operate on
real(8) :: xmin, ymin, xmax, ymax
real(8) :: minLength, maxLength
integer :: minNeigh, maxNeigh
integer :: i,j
if ( m%edgeLengthTableComputed.eqv..true. ) then
   return
endif
allocate(m%edgeLengthTable(m%np,m%neimax))
allocate(m%edgeMidpointCoordinates(2,m%np,m%neimax))
allocate(m%edgeLengthGradients(m%np))
allocate(m%minEdgeLengths(m%np))
m%minEdgeLength = huge(1.d0)
m%maxEdgeLength = tiny(1.d0)
do i=1,m%np
   m%edgeLengthTable(i,1) = 0.d0 ! distance from this node to itself...
   minLength = huge(1.d0)
   maxLength = tiny(1.d0)
   do j=2,m%nneigh(i)
      ! compute edge length
      m%edgeLengthTable(i,j) = sqrt( (m%x_cpp(i)-m%x_cpp(m%NeiTab(i,j)))**2 &
                  + (m%y_cpp(i)-m%y_cpp(m%NeiTab(i,j)))**2 )
      ! compute midpoint of edge for use in computing edge length gradient at a node
      m%edgeMidpointCoordinates(1,i,j) = m%x_cpp(i)-m%x_cpp(m%NeiTab(i,j))
      m%edgeMidpointCoordinates(2,i,j) = m%y_cpp(i)-m%y_cpp(m%NeiTab(i,j))
      ! find the min and max edge lengths at this node
      ! set which neighbor nodes share the edges with min and max lengths
      if ( m%edgeLengthTable(i,j).lt.minLength ) then
         minLength = m%edgeLengthTable(i,j)
         minNeigh = j
      endif
      if ( m%edgeLengthTable(i,j).gt.maxLength ) then
         maxLength = m%edgeLengthTable(i,j)
         maxNeigh = j
      endif
   end do
   m%minEdgeLengths(i) = minLength
   ! check min and max edge lengths against the min and max
   ! edge lengths for the full domain
   if ( minLength.lt.m%minEdgeLength ) then
      m%minEdgeLength = minLength
      m%minEdgeLengthNode = i
   endif
   ! check min and max edge lengths against the min and max
   ! edge lengths for the full domain
   if ( maxLength.gt.m%maxEdgeLength ) then
      m%maxEdgeLength = maxLength
      m%maxEdgeLengthNode = i
   endif
   ! use the neighbor node numbers with min and max length to look up the midpoint
   ! coordinates
   xmin =  m%edgeMidpointCoordinates(1,i,minNeigh)
   ymin =  m%edgeMidpointCoordinates(2,i,minNeigh)
   xmax =  m%edgeMidpointCoordinates(1,i,maxNeigh)
   ymax =  m%edgeMidpointCoordinates(2,i,maxNeigh)
   ! compute the edge length gradient at this node
   m%edgeLengthGradients(i) = ( maxLength - minLength ) &
        /  sqrt( (xmax - xmin)**2  + (ymax - ymin)**2  )
end do
! max edge legnth gradient for the whole mesh
m%maxEdgeLengthGradient = maxval(m%edgeLengthGradients)
! node number where max edge length gradient occurs
m%maxEdgeLengthGradientNode = maxloc(m%edgeLengthGradients,1)
m%edgeLengthTableComputed = .true.
!-----------------------------------------------------------------------
end subroutine computeNeighborEdgeLengthTable
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                       S U B R O U T I N E
!              F I N D   B O U N D A R Y   E D G E S
!-----------------------------------------------------------------------
!              jgf: Find edges that are on boundaries.
!-----------------------------------------------------------------------
subroutine findBoundaryEdges(m)
use logging, only : scratchMessage, allMessage, INFO
implicit none
type(mesh_t), intent(inout) :: m  ! mesh to operate on
integer :: neinod1, neinod2, neinod3 ! three node numbers around an interior element
integer :: neinum        ! index into a neighbor table
integer :: neinump1      ! clockwise incremented index around a neighbor table
integer :: i, j, k
!
! Space for boundary edges
allocate(m%tbnode(m%np,2))
!
! On an interior element, we can can look for the next clockwise
! neighbor node in a chain of three neighbor tables and get back
! to the node we started on.
!
! A boundary edge is detected when the third neighbor table
! lookup does not return the original starting node.
!
m%nseg=0
! loop over all nodes in the mesh
do i=1,m%np
   ! loop clockwise over the neighbors of this node
   do j=2,m%NNeigh(i)
      ! record the node number of a neighbor
      neinod1=m%NeiTab(i,j)
      ! loop clockwise over that neighbor node's neighbors
      do k=2, m%NNeigh(neinod1)
         ! find and record this node's index in the neighbor's neighbor table
         if (m%NeiTab(neinod1,k).eq.i) then
            neinum=k
         endif
      end do
      ! set the index of the next clockwise neighbor in this neighbor table
      neinump1=neinum+1
      ! loop back to the beginning of the neighbor table if we went off the end
      if(neinum.eq.m%NNeigh(neinod1)) then
         neinump1=2
      endif
      !
      ! record the node number of the next clockwise neighbor
      neinod2=m%NeiTab(neinod1,neinump1)
      ! loop over the neighbors of the next clockwise neighbor
      do k=2,m%NNeigh(neinod2)
         ! find and record this node's index in the neighbor's neighbor table
         if(m%NeiTab(neinod2,k).eq.neinod1) then
            neinum=k
         endif
      end do
      ! set the index of the next clockwise neighbor in this neighbor table
      neinump1=neinum+1
      ! loop back to the beginning of the neighbor table if we went off the end
      if(neinum.eq.m%NNeigh(neinod2)) then
         neinump1=2
      endif
      !
      ! record the node number of the next clockwise neighbor
      neinod3=m%NeiTab(neinod2,neinump1)
      ! for interior nodes, this gets us back to the node we started with
      ! for boundary nodes, this will be some other node
      if(neinod3.ne.i) then
         ! increment the total number of boundary edges
         m%nseg=m%nseg+1
         ! record the node numbers of the nodes on this boundary edge
         m%tbnode(m%nseg,2)=i
         m%tbnode(m%nseg,1)=neinod1
      endif
   end do
end do
write(scratchMessage,'(i0,1x,a)') m%nseg,' boundary edges were found.'
call allMessage(INFO,trim(scratchMessage))
!-----------------------------------------------------------------------
end subroutine findBoundaryEdges
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                       S U B R O U T I N E
!              S O R T   B O U N D A R Y   E D G E S
!-----------------------------------------------------------------------
!   jgf: Sort edges that are on boundaries into contiguous groups.
!-----------------------------------------------------------------------
subroutine sortBoundaryEdges(m)
implicit none
type(mesh_t), intent(inout) :: m  ! mesh to operate on
integer :: npbn          ! total number of nodes on contiguous boundaries previous to the current one
integer :: n             ! total number of boundary nodes
integer :: i, j, k

allocate(m%conbnode(m%nseg))
allocate(m%nbn(m%nseg))
!
! All boundary node numbers appear twice in the list of
! boundary edges, once as regular node, and once as a
! "connected" node.
!
! The sorting algorithm starts with a node, then searches
! for the edge where this node is listed as a neighbor.
! The node on that edge is the next connected node
! in the boundary list. Then that edge is retired from
! the list by setting its neighbor node number to zero.
!
! If the neighbor node is found to be the same node number
! as the starting node number, the boundary is complete
! and sorting starts again with a new (unprocessed)
! boundary node.
!
n=0      ! total number of boundary nodes
npbn=0   ! total number of nodes on contiguous boundaries previous to the current one
m%numBoundFound=0   ! number of contiguous boundaries
9  continue
! loop over all boundary edges
do i=1,m%nseg
   ! skip over neighbor nodes that have already been processed
   if(m%tbnode(i,1).ne.0) then
      goto 10
   endif
end do
goto 99  ! exit once all boundary neighbor node numbers have been set to zero

10  j=i   ! save segment number
11  n=n+1 ! increment number of boundary nodes processed
!
! add the node number on segment to the list of nodes on this boundary
m%conbnode(n)=m%tbnode(j,2)
!
if(m%conbnode(n).eq.m%tbnode(i,1)) then
   !
   m%tbnode(i,1)=0     ! remove this neighbor from the list
   m%numBoundFound=m%numBoundFound+1       ! increment the number of boundaries
   m%nbn(m%numBoundFound)=n-npbn  ! set number of nodes on this boundary equal to current count minus current count at the end of the last boundary
   npbn=n
   goto 9            ! get the next segment
endif
! loop over the segments, searching for a neighbor with the
! same node number as the node being added to this boundary
do j=1,m%nseg
   if(m%tbnode(j,1).eq.m%conbnode(n)) then
      m%tbnode(j,1)=0   ! eliminate this neighbor from the list
      goto 11
   endif
end do
99 continue

!-----------------------------------------------------------------------
end subroutine sortBoundaryEdges
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                       S U B R O U T I N E
!              F I N D   E X T E R N A L   B O U N D A R Y
!-----------------------------------------------------------------------
! Before this subroutine is called, the following three subroutines must
! have already been called:
! 1. readMesh(m)
! 2. findBoundaryEdges(m)
! 3. sortBoundaryEdges(m)
!-----------------------------------------------------------------------
subroutine findExternalBoundary(m)
use logging, only : logMessage, ERROR, INFO
use ioutil
implicit none
type(mesh_t), intent(inout) :: m   ! mesh to operate on
integer :: b  ! index into list of already-specified boundaries
integer :: t  ! boundary type
integer :: i, j, k
!
! initialize whether the external boundary has been found
m%externalBoundaryFound = .false.
!
! if the operator provided a node number that falls on an external
! boundary, start with that
if (m%externalBoundaryNodeHint.ne.0) then
   do i=1,m%nseg
      if (m%conbnode(i).eq.m%externalBoundaryNodeHint) then
         m%externalBoundaryFound = .true.
         m%nodeOnExternalBoundary = m%externalBoundaryNodeHint
      endif
   end do
endif
! report whether the operator-specified node number was found on the external boundary
if (m%externalBoundaryNodeHint.ne.0) then
   if (m%externalBoundaryFound.eqv..false.) then
      call logMessage(ERROR,"The node number specified as being on the external boundary was not found there.")
   else
      call logMessage(INFO,"The node number specified as being on the external boundary was found there.")
   endif
endif
! if the external boundary was not found using a hint from the operator,
! then go looking for it using existing boundary type assignments
if (m%externalBoundaryFound.eqv..false.) then
   if (m%numElevationBoundaries.gt.0) then
      m%externalBoundaryFound = .true.
      m%nodeOnExternalBoundary = m%elevationBoundaries(1)%nodes(1)
   endif
   if ((m%numSimpleFluxBoundaries.gt.0.and.m%externalBoundaryFound.eqv..false.)) then
      do b=1, m%numSimpleFluxBoundaries
         t = m%simpleFluxBoundaries(b)%ibtype
         if ( (t.eq.0).or.(t.eq.20).or.(t.eq.2).or.(t.eq.22) ) then
            m%externalBoundaryFound = .true.
            m%nodeOnExternalBoundary = m%simpleFluxBoundaries(b)%nodes(1)
            exit
         endif
      end do
   endif
   if ((m%numExternalFluxBoundaries.gt.0.and.m%externalBoundaryFound.eqv..false.)) then
      m%externalBoundaryFound = .true.
      m%nodeOnExternalBoundary = m%externalFluxBoundaries(1)%nodes(1)
   endif
endif
! if the external boundary was not found using existing boundary type assignments
! (perhaps there aren't any) then assume the longest contiguous boundary segment is
! the external boundary
if (m%externalBoundaryFound.eqv..false.) then
   m%longestBoundary = maxloc(m%nbn)
   ! connected boundary node that is one node past the end
   ! of the boundaries preceding the longest boundary
   m%nodeOnExternalBoundary = m%conbnode(sum(m%nbn(1:m%longestBoundary(1)-1))+1)
   m%externalBoundaryFound = .true.
endif
!-----------------------------------------------------------------------
end subroutine findExternalBoundary
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!                       S U B R O U T I N E
!           E X T R A C T   E X T E R N A L   B O U N D A R Y
!-----------------------------------------------------------------------
! Before this subroutine is called, the following three subroutines must
! have already been called:
! 1. readMesh(m)
! 2. findBoundaryEdges(m)
! 3. sortBoundaryEdges(m)
! 4. findExternalBoundary(m)
!-----------------------------------------------------------------------
subroutine extractExternalBoundary(m)
use logging, only : logMessage, ERROR
use ioutil
implicit none
type(mesh_t), intent(inout) :: m   ! mesh to operate on
integer :: nref          ! node number of first node on a boundary
integer :: n             ! total number of boundary nodes
integer :: ncheck
!
! determining which boundary is the external one (using hint)
integer :: nbegnbnm1     ! the node index (minus 1) at the beginning of
                         !  the boundary on which the operator-specified
                         !  first open boundary node number is found
integer :: nbegm1        ! node index (minus 1) of the operator-specified
                         !  first open boundary node
integer :: ib            ! index of the contiguous boundary where the
                         !  first operator-specified open boundary node is found
integer :: i, j, k
!
n=0
! loop over all the contiguous boundaries
do i=1,m%numBoundFound
   ! store the node number of the first node on this boundary
   nref=n
   ! loop over all the nodes in this boundary
   do j=1,m%nbn(i)
      n=n+1
      ! check to see whether the operator-specified node for
      ! the start of the first open boundary is in the list of
      ! connected boundary nodes
      if (m%conbnode(n).eq.m%nodeOnExternalBoundary) then
         ! set the node index (minus 1) at the beginning of the boundary
         ! on which the external boundary node number is found
         nbegnbnm1=nref
         ! set the node index (minus 1) of the external boundary node
         nbegm1=n-1
         ! set the index of the contiguous boundary where the
         ! first operator-specified open boundary node is found
         ! i.e., the index of the external boundary
         ib=i
      endif
   end do
end do
!
! extract the external boundary into a single continuous array
n=nbegm1
! compute index at end of contiguous boundary where
! operator-specified first open boundary node is found
! ... this index may actually be out of range if the
! boundary runs off the end of the conbnode array
ncheck=nbegnbnm1+m%nbn(ib)
do j=1,m%nbn(ib)
   n=n+1
   if (n.gt.ncheck) then
   ! wrap back to beginning of the external boundary within the conbnode array
   n=n-m%nbn(ib)
   endif
   ! write the list of external boundary nodes into their own array
   m%extbns(j)=m%conbnode(n)
   ! remove the list of external boundary nodes from the full list of boundary nodes
   m%conbnode(n)=0
end do
! repeat the first node of the external boundary at the end
m%extbns(m%nbn(ib)+1)=m%extbns(1)

!-----------------------------------------------------------------------
end subroutine extractExternalBoundary
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!                       S U B R O U T I N E
! F I N D   E L E V A T I O N   S P E C I F I E D   B O U N D A R I E S
!-----------------------------------------------------------------------
! Before this subroutine is called, the following three subroutines must
! have already been called:
! 1. readMesh(m)
! 2. findBoundaryEdges(m)
! 3. sortBoundaryEdges(m)
! 4. findExternalBoundary(m)
! 5. extractExternalBoundary(m)
!
! If this is a subset mesh, it probably will not have any elevation
! specified boundaries (they will all have been left behind on the full
! domain mesh). In this case, in the absence of a hint, this subroutine
! will set the starting and ending nodes of elevation-specified
! boundaries to any string of external boundary nodes that exceed a
! specified threshold depth and specified theshold number of nodes
! in a contiguous string.
!-----------------------------------------------------------------------
subroutine findElevationSpecifiedBoundaries(m)
use logging, only : logMessage, ERROR
use ioutil
implicit none
type(mesh_t), intent(inout) :: m  ! mesh to operate on
type(integerVector1D_t) :: ibbeg  ! list of starting node numbers for elevation specified boundaries
type(integerVector1D_t) :: ibend  ! list of ending node numbers for elevation specified boundaries
type(integerVector1D_t) :: iblen  ! number of nodes on the ith elevation specified boundary
integer :: longestBoundary(1)
integer :: n
integer :: i, j, k
logical :: foundElevationSpecifiedBoundary ! true if any external boundary node string met or exceeded the thresholds
!
call initI1D(ibbeg)
call initI1D(ibend)
call initI1d(iblen)
foundElevationSpecifiedBoundary = .false.
!
! loop through the external boundary nodes looking for nodestrings that
! exceed the depth threshold over the length threshold
do i=1,size(m%extbns)
   if (m%xyd(m%extbns(i),3).ge.m%minElevationBoundaryDepth) then
      ! check the next nodes along the boundary to see if they
      ! also exceed the depth threshold and how many of them
      ! in a row do so
      do n=1,size(m%extbns)-i
         if (m%xyd(m%extbns(i+n),3).lt.m%minElevationBoundaryDepth) then
            exit
         endif
      end do
      if (n.ge.m%minElevationBoundaryLength) then
         call appendI1D(ibbeg,m%extbns(i))
         call appendI1D(ibend,m%extbns(i+n))
         call appendI1D(iblen,n)
         foundElevationSpecifiedBoundary = .true.
      endif
   endif
end do
! copy the beginning and ending node numbers for the open
! boundary into the corresponding mesh struct array
if (foundElevationSpecifiedBoundary.eqv..true.) then
   ! if there is only supposed to be one elevation specified boundary
   ! (most common case) then pick the longest one. There may be
   ! other boundaries that are in water (e.g., rivers) but these will
   ! have some sort of flux-specified boundary. And yet they may
   ! still meet the minimum criteria (length and depth) for an elevation
   ! specified boundary. Explicitly hinting that there is only one
   ! elevation specified boundary will help the code to avoid specifying
   ! elevation specified boundaries where they are not wanted.
   if (m%oneElevationBoundary.eqv..true.) then
      longestBoundary = maxloc(iblen%v)
      allocate(m%ibbeg(1))
      allocate(m%ibend(1))
      m%ibbeg(1) = ibbeg%v(longestBoundary(1))
      m%ibend(1) = ibend%v(longestBoundary(1))
   else
      allocate(m%ibbeg(ibbeg%n))
      allocate(m%ibend(ibend%n))
      do i=1,ibbeg%n
         m%ibbeg(i) = ibbeg%v(i)
         m%ibend(i) = ibend%v(i)
      end do
   endif
endif
!-----------------------------------------------------------------------
end subroutine findElevationSpecifiedBoundaries
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                       S U B R O U T I N E
! F I N D   F L U X   S P E C I F I E D   E X T E R N A L   B O U N D A R I E S
!-----------------------------------------------------------------------
! Before this subroutine is called, the following three subroutines must
! have already been called:
! 1. readMesh(m)
! 2. findBoundaryEdges(m)
! 3. sortBoundaryEdges(m)
! 4. findExternalBoundary(m)
! 5. extractExternalBoundary(m)
! 6. findElevationSpecifiedBoundaries(m)
!
! If this is a subset mesh, it probably will have some flux specified
! boundaries already (brought in from the full domain mesh) along
! with some unassigned boundary nodes that need to be assigned a flux
! boundary type. These may be entirely new boundary node strings,
! or they may need to be attached to existing boundary node strings
! (i.e., those boundary node strings will need to be extended).
!
!-----------------------------------------------------------------------
subroutine findFluxSpecifiedExternalBoundaries(m)
use logging, only : logMessage, ERROR
use ioutil
implicit none
type(mesh_t), intent(inout) :: m  ! mesh to operate on
type(integerVector1D_t) :: ibbeg  ! list of starting node numbers for elevation specified boundaries
type(integerVector1D_t) :: ibend  ! list of ending node numbers for elevation specified boundaries
logical :: foundElevationSpecifiedBoundary
integer :: n
integer :: i, j, k
!
call initI1D(ibbeg)
call initI1D(ibend)
foundElevationSpecifiedBoundary = .false.
!
! loop through the external boundary nodes looking for nodes that
! 1, exceed the depth threshold over the length threshold
do i=1,size(m%extbns)
   if (m%xyd(m%extbns(i),3).ge.m%minElevationBoundaryDepth) then
      ! check the next nodes along the boundary to see if they
      ! also exceed the depth threshold and how many of them
      ! in a row do so
      do n=1,size(m%extbns)-i
         if (m%xyd(m%extbns(i+n),3).lt.m%minElevationBoundaryDepth) then
            exit
         endif
      end do
      if (n.ge.m%minElevationBoundaryLength) then
         call appendI1D(ibbeg,m%extbns(i))
         call appendI1D(ibend,m%extbns(i+n))
         foundElevationSpecifiedBoundary = .true.
      endif
   endif
end do
if (foundElevationSpecifiedBoundary.eqv..true.) then
   m%nob = ibbeg%n
   allocate(m%ibbeg(m%nob))
   allocate(m%ibend(m%nob))
   do i=1,m%nob
      m%ibbeg(i) = ibbeg%v(i)
      m%ibend(i) = ibend%v(i)
   end do
endif

!-----------------------------------------------------------------------
end subroutine findFluxSpecifiedExternalBoundaries
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!                       S U B R O U T I N E
!           S E T   D E F A U L T   B O U N D A R Y   T Y P E S
!-----------------------------------------------------------------------
! Before this subroutine is called, the following three subroutines must
! have already been called:
! 1. readMesh(m)
! 2. findBoundaryEdges(m)
! 3. sortBoundaryEdges(m)
! 4. findExternalBoundary(m)
! 5. extractExternalBoundary(m)
!-----------------------------------------------------------------------
subroutine setDefaultBoundaryTypes(m)
use logging, only : logMessage, ERROR
use ioutil
implicit none
type(mesh_t), intent(inout) :: m   ! mesh to operate on
integer :: edgeNodeIndex ! index of node on edge found by findBoundaryNodes (m%conbnode)
integer :: streak ! number of nodes in a row on a boundary node string
logical :: land   ! .true. if the node should be assigned a no flux boundary
integer :: i, j, k
type(integerVector1D_t) :: bnodes  ! boundary nodes

logical, allocatable :: boundaryTypeAlreadyAssigned(:)
integer :: b  ! index into list of already-specified boundaries
integer :: bn ! index into list of nodes on an already-specified boundary
!
! initialize whether the connected boundary nodes already have a
! boundary type assigned to them
allocate(boundaryTypeAlreadyAssigned(m%nseg))
boundaryTypeAlreadyAssigned(:) = .false.
!
! search through the existing nodes that have boundary types
! (of any kind) already specified and remove those from our
! list of boundary edges
!
! the nodes on the remaining edges must have boundary types
! assigned to them
!
! node strings with positive bathymetric depth will automatically
! have elevation-specified boundary types specified;
! node strings with negative bathymetric depth will automatically
! have zero flux (land) boundary types assigned
!
! any nodes that are found on the external boundary but have
! island or weir boundary types will automatically be reset
! to land (zero flux) boundary types
!
if (m%externalBoundaryFound.eqv..false.) then
   ! loop over open boundaries
   do b=1, m%numElevationBoundaries
      ! loop over the nodes of this open boundary
      do bn=1, size(m%elevationBoundaries(b)%nodes)
         ! loop over the boundary nodes found by the
         ! findBoundaryNodes subroutine
         do edgeNodeIndex=1, m%nseg
            if (m%conbnode(edgeNodeIndex).eq.m%elevationBoundaries(b)%nodes(bn)) then
               boundaryTypeAlreadyAssigned(edgeNodeIndex) = .true.
            endif
         end do
      end do
   end do
   ! simple flux (land, island, river) boundaries
   do b=1, m%numSimpleFluxBoundaries
      do bn=1, size(m%simpleFluxBoundaries(b)%nodes)
         do edgeNodeIndex=1, m%nseg
            if (m%conbnode(edgeNodeIndex).eq.m%simpleFluxBoundaries(b)%nodes(bn)) then
               boundaryTypeAlreadyAssigned(edgeNodeIndex) = .true.
            endif
         end do
      end do
   end do
   ! external flux boundaries
   do b=1, m%numExternalFluxBoundaries
      do bn=1, size(m%externalFluxBoundaries(b)%nodes)
         do edgeNodeIndex=1, m%nseg
            if (m%conbnode(edgeNodeIndex).eq.m%externalFluxBoundaries(b)%nodes(bn)) then
               boundaryTypeAlreadyAssigned(edgeNodeIndex) = .true.
            endif
         end do
      end do
   end do
   ! internal flux boundaries
   do b=1, m%numInternalFluxBoundaries
      do bn=1, size(m%internalFluxBoundaries(b)%nodes)
         do edgeNodeIndex=1, m%nseg
            if (m%conbnode(edgeNodeIndex).eq.m%internalFluxBoundaries(b)%nodes(bn)) then
               boundaryTypeAlreadyAssigned(edgeNodeIndex) = .true.
            endif
         end do
      end do
   end do
   ! internal flux boundaries with cross barrier pipes
   do b=1, m%numInternalFluxBoundariesWithPipes
      do bn=1, size(m%internalFluxBoundariesWithPipes(b)%nodes)
         do edgeNodeIndex=1, m%nseg
            if (m%conbnode(edgeNodeIndex).eq.m%internalFluxBoundariesWithPipes(b)%nodes(bn)) then
               boundaryTypeAlreadyAssigned(edgeNodeIndex) = .true.
            endif
         end do
      end do
   end do
endif
!
! now make new boundaries with the nodes that do not have an
! assigned boundary type
!
! walk along the unassigned connected boundary nodes that were found
! during the search and make new boundaries with them according to
! the bathymetric depth (elevation-specified for node strings with
! positive bathy depth (ocean) and flux-specified (land) for node strings
! with negative bathy depth
!
streak=0
land=.true.
call initI1D(bnodes)
! loop over all the found boundary nodes
do edgeNodeIndex=1, m%nseg
   ! if a boundary type is already assigned, reset the
   ! streak counter to zero and go to the next node
   if (boundaryTypeAlreadyAssigned(edgeNodeIndex).eqv..true.) then
      if (streak.eq.1) then
         call logMessage(ERROR,"Created a boundary that is only one node long.")
      endif
      streak=0
      cycle
   endif
   streak=streak + 1
   if (m%xyd(m%conbnode(edgeNodeIndex),3).ge.0.d0) then
      land = .true.
   else
      land = .false.
   endif
   ! if this is the first node of the new boundary, create the
   ! new boundary type
   if (streak.eq.1) then
      if (land.eqv..true.) then
         m%numSimpleFluxBoundaries = m%numSimpleFluxBoundaries + 1
         m%nbou=m%nbou + 1
      endif
      if (land.eqv..false.) then
         m%numElevationBoundaries = m%numElevationBoundaries + 1
         m%nope=m%nope + 1
      endif
   endif
   !
   ! in order to get the boundaries in the right order, we are going
   ! to have to count them and figure out how many we have of each
   ! type, and what nodes are on them
   !
   ! and then either rearrange the
   ! existing boundaries, or make a whole new mesh and copy the
   ! boundaries over in the correct order
end do

!-----------------------------------------------------------------------
end subroutine setDefaultBoundaryTypes
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
SUBROUTINE computeAlbersEqualAreaConic(m)
IMPLICIT NONE
type(mesh_t), intent(inout) :: m ! mesh to operate on
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
if (m%albersComputed.eqv..false.) then
   allocate(m%xalbers(m%np),m%yalbers(m%np))
   m%albersComputed = .true.
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
do i=1,m%np
   ! project mesh node locations into albers equal area conic
   P = O*DLOG((1.d0-E*DSIN(m%xyd(2,i)*deg2rad))/(1.d0+E*DSIN(m%xyd(2,i)*deg2rad)))
   ALPHA = E1*DSIN(m%xyd(2,i)*deg2rad)/(1.d0-EE*DSIN(m%xyd(2,i)*deg2rad)**2)-P
   THETA = N*(m%xyd(1,i)-LON0)
   RHO = R*(sqrt(C-N*ALPHA))/N
   m%xalbers(i) = RHO*DSIN(THETA*deg2rad)
   m%yalbers(i) = RHO0-RHO*DCOS(THETA*deg2rad)
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
SUBROUTINE computeCPP(m)
IMPLICIT NONE
type(mesh_t), intent(inout) :: m  ! mesh to operate on
if (m%cppComputed.eqv..false.) then
   allocate(m%x_cpp(m%np),m%y_cpp(m%np))
   m%cppComputed = .true.
endif
write(6,'("INFO: Generating CPP coordinates.")')
m%x_cpp = R * (m%xyd(1,:)*deg2rad - m%slam0*deg2rad) * cos(m%sfea0*deg2rad)
m%y_cpp = m%xyd(2,:)*deg2rad * R
return
!-----------------------------------------------------------------------
END SUBROUTINE computeCPP
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     S U B R O U T I N E   C O M P U T E  C P P  S T A T I O N
!-----------------------------------------------------------------------
!     jgf: Compute the CPP projection for the lat/on of a station
!     without overwriting the original lat/lon data.
!-----------------------------------------------------------------------
SUBROUTINE computeCPPStation(station, slam0, sfea0)
IMPLICIT NONE
type(station_t), intent(inout) :: station      ! station to operate on
real(8), intent(in)            :: slam0        ! longitude center of cpp projection
real(8), intent(in)            :: sfea0        ! latitude center of cpp projection

station%x_cpp = R * (station%lon*deg2rad - slam0*deg2rad) * cos(sfea0*deg2rad)
station%y_cpp = station%lat*deg2rad * R

return
!-----------------------------------------------------------------------
END SUBROUTINE computeCPPStation
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  S U B R O U T I N E   C O M P U T E  C A R T E S I A N   S P H E R E
!-----------------------------------------------------------------------
!     jgf: Very short subroutine to compute the coordinate locations
!     on a sphere with a unit radius,
!     allocating memory in the process, and not overwriting the
!     original lat/lon data.
!-----------------------------------------------------------------------
SUBROUTINE computeCartesianSphere(m)
IMPLICIT NONE
type(mesh_t), intent(inout) :: m  ! mesh to operate on
if (m%cartesianSphereComputed.eqv..false.) then
   allocate(m%x_cartesian_sphere(m%np),m%y_cartesian_sphere(m%np),m%z_cartesian_sphere(m%np))
   m%cartesianSphereComputed = .true.
endif
write(6,'("INFO: Generating cartesian coordinates on a unit sphere.")')
m%x_cartesian_sphere = cos(m%xyd(1,:)*deg2rad) * cos(m%xyd(2,:)*deg2rad)
m%y_cartesian_sphere = sin(m%xyd(1,:)*deg2rad) * cos(m%xyd(2,:)*deg2rad)
m%z_cartesian_sphere = sin(m%xyd(2,:)*deg2rad)
return
!-----------------------------------------------------------------------
END SUBROUTINE computeCartesianSphere
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!     S U B R O U T I N E   C O M P U T E   2 X   A R E A S
!-----------------------------------------------------------------------
!     jgf: Compute 2x the elemental areas ... requires that the
!     the CPP projection has already been computed.
!-----------------------------------------------------------------------
SUBROUTINE compute2xAreas(m)
IMPLICIT NONE
type(mesh_t), intent(inout) :: m ! mesh to operate on
real(8) :: nx(3)
real(8) :: ny(3)
integer :: i, j
!
if (m%elementAreasComputed.eqv..true.) then
   return
endif
if (m%cppComputed.eqv..false.) then
   call computeCPP(m)
endif
allocate(m%areas(m%ne))
write(6,'("INFO: Computing 2x the elemental areas.")')
do i=1,m%ne
   do j=1,3
      nx(j) = m%x_cpp(m%nm(i,j))
      ny(j) = m%y_cpp(m%nm(i,j))
   end do
   m%areas(i)=(nx(1)-nx(3))*(ny(2)-ny(3))+(nx(3)-nx(2))*(ny(1)-ny(3))
end do
m%elementAreasComputed = .true.
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
SUBROUTINE computeWeightingCoefficients(m)
IMPLICIT NONE
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: myNodes(0:4)
integer :: i, j
if (m%weightingCoefficientsComputed.eqv..true.) then
   return
endif
if (m%cppComputed.eqv..false.) then
   call computeCPP(m)
endif
allocate(m%sfac(m%np))
allocate(m%sfacAvg(m%ne))
allocate(m%fdx(3,m%ne))
allocate(m%fdy(3,m%ne))
m%sfac(:)=cos(m%sfea0*deg2rad)/cos(m%xyd(2,:)*deg2rad)
write(6,'("INFO: Computing weighting coefficients.")')
do i=1,m%ne
   myNodes(1:3) = m%nm(i,1:3)
   ! wrap the values around so we can easily implement a loop
   ! around the element
   myNodes(0) = myNodes(3)
   myNodes(4) = myNodes(1)
   m%sfacAvg(i) = oneThird * sum(m%sfac(myNodes(1:3)))
   ! loop over the nodes on this element
   do j=1,3
      m%fdy(j,i) = m%x_cpp(myNodes(j-1))-m%x_cpp(myNodes(j+1))         ! a1, a2, a3
      m%fdx(j,i) = ( m%y_cpp(myNodes(j+1))-m%y_cpp(myNodes(j-1)) ) * m%sFacAvg(i) ! b1, b2, b3
   end do
end do
m%weightingCoefficientsComputed = .true.
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
SUBROUTINE computeElementCentroids(m)
IMPLICIT NONE
type(mesh_t), intent(inout) :: m ! mesh to operate on
integer :: i

if (m%elementCentroidsComputed.eqv..true.) then
   return
endif
if (m%cppComputed.eqv..false.) then
   call computeCPP(m)
endif
allocate(m%centroids(2,m%ne))
write(6,'("INFO: Computing element centroids.")')
do i=1,m%ne
   m%centroids(1,i) = oneThird * sum(m%x_cpp(m%nm(i,1:3)))
   m%centroids(2,i) = oneThird * sum(m%y_cpp(m%nm(i,1:3)))
end do
m%elementCentroidsComputed = .true.
write(6,'("INFO: Finished computing element centroids.")')
!-----------------------------------------------------------------------
      END SUBROUTINE computeElementCentroids
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                     S U B R O U T I N E
!         C O M P U T E   E L E M E N T   D E P T H S
!-----------------------------------------------------------------------
!     jgf: Compute the solution weighting coefficients.
!-----------------------------------------------------------------------
SUBROUTINE computeElementDepths(m)
IMPLICIT NONE
type(mesh_t), intent(inout) :: m ! mesh to operate on
real(8) :: x1, x2, x3 ! longitude temporary variables
real(8) :: y1, y2, y3 ! latitude temporary variables
real(8) :: w1, w2, w3 ! weighting coefficients of different nodes at the element centroid
real(8) :: TotalArea
integer :: e

if (m%elementDepthsComputed.eqv..true.) then
   return
endif

call computeElementCentroids(m)

allocate(m%eledepths(m%ne))
write(6,'("INFO: Computing element depths.")')
do e=1,m%ne
   X1 = m%x_cpp(m%nm(e,1))
   X2 = m%x_cpp(m%nm(e,2))
   X3 = m%x_cpp(m%nm(e,3))
   Y1 = m%y_cpp(m%nm(e,1))
   Y2 = m%y_cpp(m%nm(e,2))
   Y3 = m%y_cpp(m%nm(e,3))
   TotalArea = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))
   w1 = ( (m%centroids(1,e)-X3)*(Y2-Y3)+(X2-X3)*(Y3-m%centroids(2,e)) )/TotalArea
   w2 = ( (m%centroids(1,e)-X1)*(Y3-Y1)-(m%centroids(2,e)-Y1)*(X3-X1))/TotalArea
   w3 = (-(m%centroids(1,e)-X1)*(Y2-Y1)+(m%centroids(2,e)-Y1)*(X2-X1))/TotalArea
   m%eledepths(e) =                      &
      m%xyd(3,m%nm(e,1)) * w1 &
    + m%xyd(3,m%nm(e,2)) * w2 &
    + m%xyd(3,m%nm(e,3)) * w3
end do
m%elementDepthsComputed = .true.
write(6,'("INFO: Finished computing element depths.")')
!-----------------------------------------------------------------------
      END SUBROUTINE computeElementDepths
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  S U B R O U T I N E   C O M P U T E   S T A T I O N   W E I G H T S
!-----------------------------------------------------------------------
! jgf: Find the element in which a station is located and then compute
! the interpolation weights for evaluating the solution at the
! station location.
!-----------------------------------------------------------------------
subroutine computeStationWeights(station, m)
use kdtree2_module
implicit none
!
type(station_t), intent(inout) :: station
type(mesh_t), intent(inout) :: m ! mesh to operate on (may compute element areas as a side effect)
!
real(8) :: x1, x2, x3 ! longitude temporary variables
real(8) :: y1, y2, y3 ! latitude temporary variables
real(8) :: TotalArea
integer :: e          ! element loop counter
integer :: se         ! element search results loop counter
!
real(8) :: ds1(2),ds2(2),ds3(2)  ! vectors from element nodes to station
real(8) :: c1,c2,c3              ! cross products of distance vectors from nodes to station

call computeKdtree2SearchTree(m)

call computeCPPStation(station, m%slam0, m%sfea0)

! first try a kdtree2 search
if (station%elementFound.eqv..false.) then
   ! Find the element centroids that are closest to the station
   call kdtree2_n_nearest(tp=m%tree,qv=(/station%x_cpp,station%y_cpp/),nn=m%srchdp,results=m%kdresults)
   ! see if the particle is inside one of the elements in the list of results
   do se=1,m%srchdp
      ! distances from nodes of this element to the station
      e = m%kdresults(se)%idx
      ds1(1) = m%x_cpp(m%nm(e,1)) - station%x_cpp  ! vector 1
      ds1(2) = m%y_cpp(m%nm(e,1)) - station%y_cpp
      ds2(1) = m%x_cpp(m%nm(e,2)) - station%x_cpp  ! vector 2
      ds2(2) = m%y_cpp(m%nm(e,2)) - station%y_cpp
      ds3(1) = m%x_cpp(m%nm(e,3)) - station%x_cpp  ! vector 3
      ds3(2) = m%y_cpp(m%nm(e,3)) - station%y_cpp
      ! all positive cross products indicate the particle is within
      ! the element because element nodes are listed counter clockwise
      c1 = (ds1(1)*ds2(2))-(ds1(2)*ds2(1))
      c2 = (ds2(1)*ds3(2))-(ds2(2)*ds3(1))
      c3 = (ds3(1)*ds1(2))-(ds3(2)*ds1(1))

      if ((c1.ge.0.d0).and.(c2.ge.0.d0).and.(c3.gt.0.d0)) then
         station%elementIndex = e
         station%elementFound = .true.
         exit
      endif
   end do
endif

! now try a brute force search
if (station%useBruteForceSearch.eqv..true.) then
   if (station%elementFound.eqv..false.) then
      station%outsideWithinTolerance = .false.
      do e=1,m%ne
         call isStationWithinElement(station, e, m)
         if (station%elementFound.eqv..true.) Then
            exit
         endif
      end do
   endif
endif

! compute station weights if the element containing the station was found
! or set the weights to a missing value if the element was not found
if (station%elementFound.eqv..true.) then
   X1 = m%xyd(1,m%nm(station%elementIndex,1))
   X2 = m%xyd(1,m%nm(station%elementIndex,2))
   X3 = m%xyd(1,m%nm(station%elementIndex,3))
   Y1 = m%xyd(2,m%nm(station%elementIndex,1))
   Y2 = m%xyd(2,m%nm(station%elementIndex,2))
   Y3 = m%xyd(2,m%nm(station%elementIndex,3))
   TotalArea = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))
   station%w(1) = ( (station%lon-X3)*(Y2-Y3)+(X2-X3)*(Y3-station%lat) )/TotalArea
   station%w(2) = ( (station%lon-X1)*(Y3-Y1)-(station%lat-Y1)*(X3-X1))/TotalArea
   station%w(3) = (-(station%lon-X1)*(Y2-Y1)+(station%lat-Y1)*(X2-X1))/TotalArea
   if (m%elementAreasComputed.eqv..false.) then
      call compute2xAreas(m)
   endif
   station%elementArea = 0.5d0*m%areas(e)
else
   station%elementIndex = 0
   station%elementArea = -99999.d0
   station%w = -99999.0
endif
!-----------------------------------------------------------------------
END SUBROUTINE computeStationWeights
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  S U B R O U T I N E   I S   S T A T I O N   W I T H I N   E L E M E N T
!-----------------------------------------------------------------------
! jgf: Check a set of cartesian coordinates to see if it is inside
! a specified element.
!-----------------------------------------------------------------------
subroutine isStationWithinElement(station, e, m)
implicit none
type(station_t), intent(inout) :: station
type(mesh_t), intent(inout) :: m ! mesh to operate on (may compute element areas as a side effect)
integer, intent(in) :: e
!
real(8) :: x1, x2, x3 ! longitude temporary variables
real(8) :: y1, y2, y3 ! latitude temporary variables
real(8) :: subArea1, subArea2, subArea3, TotalArea

if (station%elementFound.eqv..false.) then

   station%outsideWithinTolerance = .false.

   X1 = station%lon
   X2 = m%xyd(1,m%nm(e,2))
   X3 = m%xyd(1,m%nm(e,3))
   Y1 = station%lat
   Y2 = m%xyd(2,m%nm(e,2))
   Y3 = m%xyd(2,m%nm(e,3))
   SubArea1 = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

   X1 = m%xyd(1,m%nm(e,1))
   X2 = station%lon
   X3 = m%xyd(1,m%nm(e,3))
   Y1 = m%xyd(2,m%nm(e,1))
   Y2 = station%lat
   Y3 = m%xyd(2,m%nm(e,3))
   SubArea2 = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

   X1 = m%xyd(1,m%nm(e,1))
   X2 = m%xyd(1,m%nm(e,2))
   X3 = station%lon
   Y1 = m%xyd(2,m%nm(e,1))
   Y2 = m%xyd(2,m%nm(e,2))
   Y3 = station%lat
   SubArea3 = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

   X1 = m%xyd(1,m%nm(e,1))
   X2 = m%xyd(1,m%nm(e,2))
   X3 = m%xyd(1,m%nm(e,3))
   Y1 = m%xyd(2,m%nm(e,1))
   Y2 = m%xyd(2,m%nm(e,2))
   Y3 = m%xyd(2,m%nm(e,3))
   TotalArea = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

   if ((SubArea1+SubArea2+SubArea3).LE.(1.01*TotalArea))THEN
      station%elementIndex = e
      station%elementFound = .true.
   endif
   if (m%useStationTolerance.and.(SubArea1+SubArea2+SubArea3).LE.((1.0+m%stationTolerance)*TotalArea))THEN
      station%elementIndex = e
      station%elementFound = .true.
      station%outsideWithinTolerance = .true.
   endif

endif

!-----------------------------------------------------------------------
END SUBROUTINE isStationWithinElement
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!      S U B R O U T I N E   C O M P U T E   K D T R E E 2
!                    S E A R C H   T R E E
!-----------------------------------------------------------------------
! jgf: Check a set of cartesian coordinates to see if it is inside
! a specified element.
!-----------------------------------------------------------------------
subroutine computeKdtree2SearchTree(m)
implicit none
type(mesh_t), intent(inout) :: m ! mesh to operate on (may compute element areas as a side effect)

if (m%kdtree2SearchTreeComputed.eqv..true.) then
   return
endif

call computeElementCentroids(m)

write(6,'("INFO: Computing kdtree2 search tree.")')

! set search depth (not to exceed number of elements)
m%srchdp = min(12,m%ne)
! allocate space for kdtree2 search and create the tree
m%tree => kdtree2_create(m%centroids,rearrange=.true.,sort=.true.)
! allocate space for search results from the tree
allocate(m%kdresults(m%srchdp))

m%kdtree2SearchTreeComputed = .true.

write(6,'("INFO: Finished computing kdtree2 search tree.")')

!-----------------------------------------------------------------------
END SUBROUTINE computeKdtree2SearchTree
!-----------------------------------------------------------------------

!-----+---------+---------+---------+---------+---------+---------+
   end module adcmesh
!-----+---------+---------+---------+---------+---------+---------+

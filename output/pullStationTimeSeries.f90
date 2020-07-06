!------------------------------------------------------------------
! pullStationTimeSeries.f90: Create ADCIRC ascii elevation station
! file (fort.61) from ADCIRC fort.63 file.
!------------------------------------------------------------------
! Copyright(C) 2015--2020 Jason Fleming
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
program pullStationTimeSeries
use asgsio
use adcmesh
use logging
use ioutil
implicit none
type(station_t), allocatable :: stations(:)
type(mesh_t) :: m
type(meshNetCDF_t) :: n
character(len=1024) :: dataSetHeaderLine
character(len=1024) :: headerLineOne
character(len=1024) :: stationFileName ! name of file containing list of stations
character(len=1024) :: nodeFileName    ! name of file containing list of node numbers
type(fileMetaData_t) :: ft ! full domain time series data file to pull from
type(fileMetaData_t) :: fs ! time series data file at stations
real(8), allocatable :: adcirc_data(:,:) ! (np,irtype)
real(8) :: stationVal, temp1, temp2
integer :: numStations
integer :: numNodeStations ! nodes where values should be output
integer, allocatable :: nodeStations(:) ! node numbers where values should be reported
logical, allocatable :: isNodeStationInMesh(:) ! in case node number is outside mesh range
integer :: nc_start(2)
integer :: nc_count(2)
integer :: sbtUnit
integer :: sfUnit
integer :: nfUnit
integer :: swUnit
integer :: snapi
real(8) :: snapr
integer :: numNodesNonDefault
integer :: i, j, node, ds, s, k
integer :: errorIO
logical :: zeroIndex ! true if stations should be numbered starting at zero 
integer :: stationStart ! station index to start on
integer :: stationEnd   ! station index to end on
logical :: useGivenNodeNumber ! .true. if given node numbers should be used in first column of output

! initializations
call initLogging(availableUnitNumber(),'pullStationTimeSeries.f90')
m%meshFileName = 'fort.14'
stationFileName = 'null'  ! e.g., stations.txt
nodeFileName = 'null'     ! e.g., nodes.txt
numStations = 0
numNodeStations = 0
fs%dataFileName = 'stations_timeseries.txt'
fs%dataFileFormat = ASCIIG
ft%dataFileFormat = ASCIIG
dataSetHeaderLine = "-99999.0 -99999"
zeroIndex = .false.
useGivenNodeNumber = .false.

argcount = command_argument_count() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
      case("--meshfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         m%meshFileName = trim(cmdlinearg)
      case("--datafile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         ft%dataFileName = trim(cmdlinearg)
      case("--netcdf")
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt),"."
         ft%dataFileFormat = NETCDFG
      case("--zero-index")
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt),"."
         zeroIndex = .true.
      case("--stationfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         stationFileName = trim(cmdlinearg)
      case("--nodefile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         nodeFileName = trim(cmdlinearg)
      case("--use-given-node-number")
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt),"."
         useGivenNodeNumber = .true.         
      case("--outputfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         fs%dataFileName = trim(cmdlinearg)
      case default
         write(6,'(99(a))') "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
else
   ! if there weren't any command line options, prompt interactively
   WRITE(*,*)'GRID FILE?'
   READ(*,*) m%meshFileName

   WRITE(*,*)'STATION X,Y FILE?'
   READ(*,*) stationFileName
end if    
!
! read in the mesh
if ( ft%dataFileFormat.eq.NETCDFG ) then
   m%meshFileName = ft%dataFileName
   call findMeshDimsNetCDF(m, n)
   call readMeshNetCDF(m, n)
else
   call read14(m)
endif
!
!  count the number of stations
if ( trim(adjustl(stationFileName)).ne.'null' ) then
   sfUnit = availableUnitNumber()
   call openFileForRead(sfUnit, stationFileName, errorIO)
   numStations = 0
   do
      read(sfUnit,*,end=7)
      numStations = numStations + 1
   end do
   7 write(6,'(a,i0,a,a,a)') 'INFO: There are ',numStations,' station(s) in ',trim(adjustl(stationFileName)), '.'
   rewind(sfUnit)
   stationStart = 1
   stationEnd = numStations
   if (zeroIndex.eqv..true.) then
      stationStart = 0
      stationEnd = numStations - 1 
   endif
   allocate(stations(stationStart:stationEnd))
   !
   ! read station file
   write(6,'(a)') 'INFO: Reading station file.'
   do i=stationStart, stationEnd
      read(sfUnit,*) stations(i)%lon, stations(i)%lat
   end do
   close(sfUnit)
   write(6,'(a)') 'INFO: Finished reading station file.'
endif
!
!  count the number of node stations (if any)
if ( trim(adjustl(nodeFileName)).ne.'null' ) then
   nfUnit = availableUnitNumber()
   call openFileForRead(nfUnit, nodeFileName, errorIO)
   numNodeStations = 0
   do
      read(nfUnit,*,end=8)
      numNodeStations = numNodeStations + 1
   end do
   8 write(6,'(a,i0,a,a,a)') 'INFO: There are ',numNodeStations,' node(s) in ',trim(nodeFileName),'.'
   rewind(nfUnit)
   allocate(nodeStations(numNodeStations))
   allocate(isNodeStationInMesh(numNodeStations))   
   isNodeStationInMesh(:) = .true. ! most likely case
   !
   ! read node station file for the node numbers 
   write(6,'(a)') 'INFO: Reading node file.'
   do i=1, numNodeStations
      read(nfUnit,*) nodeStations(i)
   end do
   close(nfUnit)
   write(6,'(a)') 'INFO: Finished reading node file.'
   write(6,'(a)') 'INFO: Checking list of nodes against mesh node number range.'
   do i=1,numNodeStations
      if ( nodeStations(i).lt.1 .or. nodeStations(i).gt.m%np ) then
         write(6,'(a,i0,a)') 'WARNING: Node ',nodeStations(i),' was not found in the mesh.'
         isNodeStationInMesh(i)=.false. ! use this later to avoid lookup errors
      endif
   end do
   write(6,'(a)') 'INFO: Finished checking list of nodes against mesh.'
endif
!
! bomb out if there are no locations to report data on
if ( numStations.eq.0 .and. numNodeStations.eq.0 ) then
   write(6,'(a)') 'WARNING: There are no stations or nodes. Exiting.'
   stop
endif
! 
! grab memory for holding a single data set for 1 or 2 component data
allocate(adcirc_data(m%np,2))
!jgfdebug
! write(*,*) 'm%np is ',m%np,' size(adcirc_data,1) is ',size(adcirc_data,1),'size(adcirc_data,2) is ',size(adcirc_data,2)
!
! loop over the stations, find the element that contains each station,
! and compute the interpolation weights
if ( numStations.ne.0 ) then
   write(6,'(a,i0,a)') 'INFO: Finding element containing each station and computing interpolation weights for ',numStations,' station(s).'
   do s=stationStart, stationEnd
      write(6,'(i0,1x)',advance='no') s    ! update progress bar
      stations(s)%elementFound = .false.
      call computeStationWeights(stations(s), m)
   end do
endif
!
! open the text file for writing time series data
fs%fun = availableUnitNumber()
open(unit=fs%fun,file=trim(fs%dataFileName),status='replace',action='write')
fs%defaultValue = -99999.
!
! open the data file (netcdf or ascii)
select case(ft%dataFileFormat)
case(ASCIIG)
   ft%fun = availableUnitNumber()
   call openFileForRead(ft%fun,ft%dataFileName,errorIO)
   !
   ! read header lines and write them to time series file
   read(ft%fun,'(a1024)') headerLineOne
   write(fs%fun,*) trim(adjustl(headerLineOne))
   read(ft%fun,*) ft%nSnaps, ft%numValuesPerDataset, ft%time_increment, ft%nspool, ft%irtype
   write(fs%fun,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') ft%nSnaps, (numStations+numNodeStations), ft%time_increment, ft%nspool, ft%irtype
   ds=1  ! jgf: initialize the dataset counter
   !
   ! jgf: loop until we run out of data
   do    
      write(6,'(i0,1x)',advance='no') ds    ! update progress bar
      read(ft%fun,'(a80)',END=123,ERR=123) dataSetHeaderLine
      ! jgfdebug 
      ! write(*,*) 'dataSetHeaderLine is ',trim(dataSetHeaderLine)
      read(dataSetHeaderLine,*) SnapR, SnapI
      read(dataSetHeaderLine,*,ERR=907,END=907) SnapR, SnapI, numNodesNonDefault, ft%defaultValue
      goto 908  ! jgf: this file is sparse ascii
 907  numNodesNonDefault = ft%numValuesPerDataset !jgf: this file is full ascii
      !jgfdebug
      ! write(*,*) 'nunNodesNonDefault = ',numNodesNonDefault
 908  adcirc_data(:,:) = ft%defaultValue
      select case(ft%irtype)
      case(1) ! scalar data
         do node=1,numNodesNonDefault
            read(ft%fun,*) j, temp1
            adcirc_data(j,1) = temp1
         end do
      case(2) ! 2D vector data
         adcirc_data = ft%defaultValue
         do node=1,numNodesNonDefault
            read(ft%fun,*) j, temp1, temp2
            adcirc_data(j,1) = temp1
            adcirc_data(j,2) = temp2
         end do     
      end select
      ! add dataset header
      write(fs%fun,*) snapR, snapI
      s=0
      ! write station values (if any) for this dataset
      if ( numStations.ne.0 ) then
         do s=stationStart, stationEnd
            call writeStationValue(adcirc_data, m, numNodesNonDefault, ft%irtype, stations(s), s, fs%fun)
         end do
      endif
      ! write nodal values (if any) for this dataset
      if ( numNodeStations.ne.0 ) then
         do k=1, numNodeStations
            call writeNodalValue(adcirc_data, m%np, ft%irtype, nodeStations(k), isNodeStationInMesh(k), (k+s), useGivenNodeNumber, fs%fun)
         end do
      endif              
      ds = ds + 1
   end do
123 close(ft%fun) ! jgf: When we've run out of datasets in the current file,
                  ! we jump to here.     
case(NETCDFG)
   call determineNetCDFFileCharacteristics(ft, m, n)
   headerLineOne = trim(rundes) // ' ' // trim(runid) // ' ' // trim(m%agrid)
   snapR = ft%time_increment
   write(fs%fun,*) trim(adjustl(headerLineOne))
   write(fs%fun,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') ft%nSnaps, (numStations+numNodeStations), ft%time_increment, ft%nspool, ft%irtype

   ! open the netcdf file
   call check(nf90_open(trim(ft%dataFileName), NF90_NOWRITE, ft%nc_id))   
   ! get netcdf variable IDs for the the data 
   do j=1,ft%irtype
      !write(6,'(a,i0,a,a,a,i0,a)') 'DEBUG: The variable name for component ',j,' is ',trim(varname(j)),' and the variable ID is ',nc_varid(j),'.'
      call check(nf90_inq_varid(ft%nc_id, trim(adjustl(ft%ncds(j)%varNameNetCDF)), ft%ncds(j)%nc_varid))
   end do   
   write(6,'(a)') 'INFO: Compiling a record of station values across all data sets.'
   ! loop over datasets   
   do i=1,ft%nSnaps
      write(6,advance='no',fmt='(i0,1x)') i  ! update progress bar
      !
      ! read the dataset from netcdf
      do j=1,ft%irtype
         nc_start = (/ 1, i /)
         nc_count = (/ m%np, 1 /)
         ! get data
         call check(nf90_get_var(ft%nc_id,ft%ncds(j)%nc_varID,adcirc_data(:,j),nc_start,nc_count))
      end do
      write(fs%fun,*) ft%timesec(i), ft%it(i)
      s=0
      ! write station values (if any) for this dataset
      if ( numStations.ne.0 ) then            
         do s=stationStart, stationEnd   
            call writeStationValue(adcirc_data, m, ft%numValuesPerDataSet, ft%irtype, stations(s), s, fs%fun)
         end do
      endif
      ! write nodal values (if any) for this dataset
      if ( numNodeStations.ne.0 ) then
         do k=1, numNodeStations
            call writeNodalValue(adcirc_data, m%np, ft%irtype, nodeStations(k), isNodeStationInMesh(k), (k+s), useGivenNodeNumber, fs%fun)
         end do
      endif              
   end do
   close(fs%fun)
   if (ft%dataFileCategory.eq.MINMAX) then
      ! open the text file for writing time of occurrence data
      fs%fun = availableUnitNumber()
      open(unit=fs%fun,file='time_'//trim(fs%dataFileName),status='replace',action='write')
      fs%defaultValue = -99999.
      write(fs%fun,*) trim(adjustl(headerLineOne))
      write(fs%fun,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') ft%nSnaps, numStations, ft%time_increment, ft%nspool, ft%irtype
      nc_start = (/ 1, 1 /)
      nc_count = (/ m%np, 1 /)
      ! get data
      call check(nf90_inq_varid(ft%nc_id, trim('time_of_' // adjustl(ft%ncds(1)%varNameNetCDF)), ft%ncds(1)%nc_varid))
      call check(nf90_get_var(ft%nc_id,ft%ncds(j)%nc_varID,adcirc_data(:,1),nc_start,nc_count))     
      write(fs%fun,*) ft%timesec(1), ft%it(1)      
      s=0
      ! write station values (if any) for this dataset
      if ( numStations.ne.0 ) then            
         do s=stationStart, stationEnd   
            call writeStationValue(adcirc_data, m, ft%numValuesPerDataset, ft%irtype, stations(s), s, fs%fun)
         end do
      endif
      close(fs%fun)
      ! write nodal values (if any) for this dataset
      if ( numNodeStations.ne.0 ) then
         do k=1, numNodeStations
            call writeNodalValue(adcirc_data, m%np, ft%irtype, nodeStations(k), isNodeStationInMesh(k), (k+s), useGivenNodeNumber, fs%fun)
         end do
      endif                    
   endif
   call check(nf90_close(ft%nc_id))
case default
   write(6,*) "ERROR: File format '",TRIM(cmdlineopt),"' was not recognized."
   stop
end select
close(61)
!
! write the station weights to a text file in fort.61 format 
! for reference or troubleshooting
! write station values (if any) for this dataset
if ( numStations.ne.0 ) then
   swUnit = availableUnitNumber()
   open(unit=swUnit,file='station_weights.61',status='replace',action='write')
   write(swUnit,*) trim(adjustl(headerLineOne))
   write(swUnit,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') ft%nSnaps, numStations, ft%time_increment, ft%nspool, 3
   write(swUnit,*) trim(adjustl(dataSetHeaderLine))
   do s=stationStart,stationEnd 
      write(swUnit,'(i10,2x,3(f15.7,3x))') s, (stations(s)%w(i),i=1,3)
   end do
   close(swUnit)
   write(6,'(/,a)') 'INFO: Finished finding element(s) and computing interpolation weights.'
endif
!
! use station weights to interpolate bathy/topo at station locations
! and write out bathy/topo elevations in fort.61 format
sbtUnit = availableUnitNumber()
open(unit=sbtUnit,file='station_bathytopo.61',status='replace',action='write')
write(sbtUnit,*) trim(adjustl(headerLineOne))
write(sbtUnit,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') ft%nSnaps, (numStations+numNodeStations), ft%time_increment, ft%nspool, 1
write(sbtUnit,*) trim(adjustl(dataSetHeaderLine))
s=0
if ( numStations.ne.0 ) then
   do s=stationStart,stationEnd
      if (stations(s)%elementIndex.eq.0) then
         stationVal = -99999.0
      else
         stationVal = m%xyd(3,m%nm(stations(s)%elementIndex,1)) * stations(s)%w(1) &
                    + m%xyd(3,m%nm(stations(s)%elementIndex,2)) * stations(s)%w(2) &
                    + m%xyd(3,m%nm(stations(s)%elementIndex,3)) * stations(s)%w(3) 
      endif
      write(sbtUnit,'(i10,2x,e17.10)') s, stationVal
   end do
endif
if ( numNodeStations.ne.0 ) then 
   ! write nodal values (if any) for this dataset
   do k=1, numNodeStations
      i = k + s ! index for first column
      if ( useGivenNodeNumber.eqv..true. ) then
         i = nodeStations(k)
      endif
      if (isNodeStationInMesh(k).eqv..true.) then 
         write(sbtUnit,'(i10,2x,e17.10,a,i0)') i, m%xyd(3,nodeStations(k)), ' ! node ',node
      else
         write(sbtUnit,'(i10,2x,e17.10,a,i0,a)') i, -99999.0, ' ! warning: node ',node,' was not found in the mesh. '               
      endif
   end do
endif
close(sbtUnit)
!
write(6,'(a)') 'INFO: Wrote station values successfully.'
!-----------------------------------------------------------------------
end program pullStationTimeSeries
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!    S U B R O U T I N E     W R I T E   S T A T I O N   V A L U E 
!-----------------------------------------------------------------------
! Writes the interpolated time series value at the station location,
! or -99999 if the station is outside the mesh.
!-----------------------------------------------------------------------
subroutine writeStationValue(adcirc_data, m, numValuesPerDataset, irtype, station, s, slun)
use adcmesh
implicit none
real(8), intent(in) :: adcirc_data(numValuesPerDataSet,irtype)
integer, intent(in) :: irtype ! number of vector components, 1 is scalar etc
integer, intent(in) :: numValuesPerDataSet ! number of values in a sparse dataset?
type(mesh_t), intent(inout) :: m 
type(station_t), intent(in) :: station
integer, intent(in) :: s    ! station index
integer, intent(in) :: slun ! logical unit number to write to
character(len=100) :: note  ! alert operator that station was not found
real(8) :: stationVal, temp1, temp2
logical :: dryNode
integer :: i
!
dryNode = .false.
note = ''
if (station%elementIndex.ne.0) then
   do i=1,3
      if (adcirc_data(m%nm(station%elementIndex,i),1).eq.-99999) then
         dryNode = .true.
      endif
   end do
else
   note = ' ! warning: this station is actually outside the mesh'
endif

if (irtype.eq.1) then
   if (station%elementIndex.eq.0 .or. dryNode.eqv..true.) then
      stationVal = -99999.0
   else
      stationVal = adcirc_data(m%nm(station%elementIndex,1),1) * station%w(1) &
                 + adcirc_data(m%nm(station%elementIndex,2),1) * station%w(2) &
                 + adcirc_data(m%nm(station%elementIndex,3),1) * station%w(3) 
   endif
   write(slun,'(i10,2x,e17.10,a)') s, stationVal, trim(note) 
else
   if (station%elementIndex.eq.0 .or. dryNode.eqv..true.) then
      temp1 = -99999.0
      temp2 = -99999.0
   else
      temp1 = adcirc_data(m%nm(station%elementIndex,1),1) * station%w(1) &
            + adcirc_data(m%nm(station%elementIndex,2),1) * station%w(2) &
            + adcirc_data(m%nm(station%elementIndex,3),1) * station%w(3) 
      temp2 = adcirc_data(m%nm(station%elementIndex,1),2) * station%w(1) &
            + adcirc_data(m%nm(station%elementIndex,2),2) * station%w(2) &
            + adcirc_data(m%nm(station%elementIndex,3),2) * station%w(3) 
   endif
   write(slun,'(i10,2(2x,e17.10),a)') s, temp1, temp2, trim(note)
endif
!-----------------------------------------------------------------------
end subroutine writeStationValue
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!    S U B R O U T I N E     W R I T E   N O D A L   V A L U E 
!-----------------------------------------------------------------------
! Writes the time series value at the node, or -99999 if the node number
! is not within the range 1:m%np.
!-----------------------------------------------------------------------
subroutine writeNodalValue(adcirc_data, nn, irtype, node, found, k, ugnn, slun)
implicit none
real(8), intent(in) :: adcirc_data(nn,irtype)
integer, intent(in) :: nn     ! number of nodes in mesh
integer, intent(in) :: irtype ! number of vector components, 1 is scalar etc
integer, intent(in) :: node   ! the node number of interest
logical, intent(in) :: found  ! true if node is in mesh node number range
integer, intent(in) :: k      ! index in total list of stations+nodes
logical, intent(in) :: ugnn   ! true if the original node number should appear in 1st column
integer, intent(in) :: slun   ! i/o unit number to write to

integer :: i                  ! 1st column index

i = k
if (ugnn.eqv..true.) then
   i = node
endif

if (irtype.eq.1) then
   if (found.eqv..true.) then 
      write(slun,'(i10,2x,e17.10,a,i0)') &
         i, adcirc_data(node,1), ' ! node ',node
   else
      write(slun,'(i10,2x,e17.10,a,i0,a)') &
         i, -99999.0, ' ! warning: node ',node,' was not found in the mesh. '               
   endif
endif

if (irtype.eq.2) then
   if (found.eqv..true.) then 
      write(slun,'(i10,2(2x,e17.10),a,i0)') &
         i, adcirc_data(node,1), adcirc_data(node,2),' ! node ',node
   else
      write(slun,'(i10,2(2x,e17.10),a,i0,a)') &
         i, -99999.0, -99999.0,' ! warning: node ',node,' was not found in the mesh. '               
   endif
endif
!-----------------------------------------------------------------------
end subroutine writeNodalValue
!-----------------------------------------------------------------------

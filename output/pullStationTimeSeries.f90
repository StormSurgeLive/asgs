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
real(8) :: temp1, temp2
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
logical :: zeroIndex     ! true if stations should be numbered starting at zero 
integer :: stationStart  ! station index to start on
integer :: stationEnd    ! station index to end on
logical :: useGivenNodeNumber ! .true. if given node numbers should be used in first column of output
logical :: peak = .false. ! .true. if a peak value should be generated for every station/element instead of time varying
integer, allocatable :: ipeakvalues(:)
real(8), allocatable :: peakvalues1(:)
real(8), allocatable :: peakvalues2(:)
character(len=2000) :: note     ! metadata for station file output 
character(len=2000) :: filetype ! timeseries or peak 
!---------------------------------------------------------------------------------
! M A U R E P A R T I C L E
!---------------------------------------------------------------------------------
! TODO: fix the maureparticle interface so it determines the number of datasets, time increment, and start time automatically 
integer :: numParticles = 0    ! number of particles in the file
integer :: numDatasets = 0     ! number of datasets in the file
real(8) :: timeIncrement = 0   ! time increment (sec) between datasets
real(8) :: startTime = 0       ! start time (sec) of datasets
logical :: toElement = .false. ! convert particle counts in each element to elemental output file (.100)
real(8), allocatable :: particlesXY(:,:)  ! (2,p) particle lon lat
integer, allocatable :: particles2elements(:)   ! mapping from particles to elements
integer, allocatable :: elementParticleCount(:) ! raw number of particles in each element
integer, allocatable :: particleCount(:)       ! raw number of particles at model location (numValuesPerDataset)
real(8), allocatable :: particlesPerArea(:)     ! p/m^2 at model location (numValuesPerDataset)
real(8), allocatable :: particlesPerVolume(:)   ! p/m^3 at model location (numValuesPerDataset)
integer, allocatable :: peakParticleCount(:)       ! raw number of particles at model location (numValuesPerDataset)
real(8), allocatable :: peakParticlesPerArea(:)     ! p/m^2 at model location (numValuesPerDataset)
real(8), allocatable :: peakParticlesPerVolume(:)   ! p/m^3 at model location (numValuesPerDataset)
real(8), allocatable :: peakTime(:)       ! p/m^3 (numValuesPerDataset)
logical :: duration                       ! true if time extent of nonzero particle count should be reported 
real(8), allocatable :: durationStart(:)  ! (numValuesPerDataset) (sec) when nonzero particle count was first detected
real(8), allocatable :: durationEnd(:)    ! (numValuesPerDataset) (sec) since nonzero particle count was found
type(fileMetaData_t), allocatable :: pd(:)      ! (3) time series of (a) particle count, (b) particles per m^2, and (c) particles per m^3
integer :: numValuesPerDataset  ! number of stations for station file or number of elements for .100 file   
integer :: f                    ! particle station file loop counter
integer :: p                    ! particle loop counter
integer :: e                    ! element loop counter
real(8) :: tempR1, tempR2       ! placeholder real variables for i/o
integer :: tempI1               ! placeholder integer variable for i/o
character(len=10) :: fileExtension ! .100 for elemental quantity, .200 for peak elemental quantity
!---------------------------------------------------------------------------------
!
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
peak = .false.
duration = .false.

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

      case("--maureparticlefile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         ft%dataFileName = trim(cmdlinearg)
         ft%dataFileCategory = MAUREPT

      case("--num-particles")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         read(cmdlinearg,*) numParticles

      case("--num-datasets")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         read(cmdlinearg,*) numDatasets

      case("--start-time")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         read(cmdlinearg,*) startTime
         
      case("--time-increment")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         read(cmdlinearg,*) timeIncrement         

      case("--to-element")
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt),"."
         toElement = .true. 

      case("--peak")
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt),"."
         peak = .true.
         
      case("--duration")
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt),"."
         duration = .true.         

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
!------------------------------------------------------------------------
!                  P R O C E S S 
!                 S T A T I O N S
!------------------------------------------------------------------------ 
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
! bomb out if there are no locations to report data on and this is not a conversion to an elemental file
if ( (numStations.eq.0) .and. (numNodeStations.eq.0) .and. (toElement.eqv..false.) ) then
   write(6,'(a)') 'WARNING: There are no stations or nodes and this is not a conversion to an elemental file. Exiting.'
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
! write the station weights, element indices, and element total areas to a text file in pseudo-fort.61 format 
! for reference or troubleshooting
! write station values (if any) for this dataset
if ( numStations.ne.0 ) then
   swUnit = availableUnitNumber()
   open(unit=swUnit,file='station_weights.61',status='replace',action='write')
   write(swUnit,*) trim(adjustl(headerLineOne))
   write(swUnit,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') ft%nSnaps, numStations, ft%time_increment, ft%nspool, 3
   write(swUnit,*) trim(adjustl(dataSetHeaderLine))
   do s=stationStart,stationEnd 
      write(swUnit,'(i10,2x,3(f15.7,3x),i10,2x,f15.7)') s, (stations(s)%w(i),i=1,3), stations(s)%elementIndex, stations(s)%elementArea
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
         stations(s)%elementBathyDepth = -99999.0
      else
         stations(s)%elementBathyDepth =                                           &
                      m%xyd(3,m%nm(stations(s)%elementIndex,1)) * stations(s)%w(1) &
                    + m%xyd(3,m%nm(stations(s)%elementIndex,2)) * stations(s)%w(2) &
                    + m%xyd(3,m%nm(stations(s)%elementIndex,3)) * stations(s)%w(3) 
      endif
      write(sbtUnit,'(i10,2x,e17.10)') s, stations(s)%elementBathyDepth
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
!------------------------------------------------------------------------
!    F I N I S H E D   P R O C E S S I N G   S T A T I O N S
!
!
!                 N O W   P R O C E S S   D A T A
!------------------------------------------------------------------------
!         M A U R E P A R T I C L E 
!          S T A T I O N   A N D  
!          E L E M E N T  D A T A
!------------------------------------------------------------------------ 
! maureparticle files are read to compute (a) number of particles in the 
! element where the station is located; (b) number of particles per square
! meter in the element; and (c) number of particles per cubic meter, 
! using the nominal water depth at the location
if (ft%dataFileCategory.eq.MAUREPT) then
   ! check for existence of command line options
   if ( numDatasets.eq.0 ) then
      call allMessage(WARNING,"The --num-datasets command line argument was not supplied. The value in the station file header will be set to -99.")
      stop
   endif
   if ( numParticles.eq.0 ) then
      call allMessage(ERROR,"The --num-particles command line argument was not supplied. Exiting.")
      stop
   endif   
   if ( timeIncrement.eq.0 ) then
      call allMessage(ERROR,"The --time-increment command line argument was not supplied. Exiting.")
      stop      
   endif
   allocate(particlesXY(2,numParticles),particles2elements(numParticles),elementParticleCount(m%ne))
   allocate(pd(3))
   filetype = 'timeseries'  
   fileExtension = '.61'         ! scalar station file
   snapR = startTime   
   snapI = 1
   ! open maureparticle file containing particle datasets including
   ! location and element index where they are found over time
   ! particles per element for each station
   if ( toElement.eqv..true. ) then
      fileExtension = '.100'     ! elemental output
      call computeElementDepths(m)
      call compute2xAreas(m)
      numValuesPerDataSet = m%ne
      if ( peak.eqv..true. ) then
         fileExtension = '.200'  ! element output (peak values)
      endif
   else 
      ! particle station file
      numValuesPerDataSet = numStations
      if (peak.eqv..true.) then
          fileExtension = '.161'  ! scalar station file (peak values)
      endif      
   endif
   allocate(particleCount(numValuesPerDataset),particlesPerArea(numValuesPerDataset),particlesPerVolume(numValuesPerDataset))
   if (duration.eqv..true.) then
      allocate(durationStart(numValuesPerDataset))
      allocate(durationEnd(numValuesPerDataset))
      durationStart(:) = -1.d0
      durationEnd(:) = -1.d0
   endif
   if (peak.eqv..true.) then   
      filetype = 'peak'      
      allocate(peakParticleCount(numValuesPerDataset))      
      allocate(peakParticlesPerArea(numValuesPerDataset))
      allocate(peakParticlesPerVolume(numValuesPerDataset))
      allocate(peakTime(numValuesPerDataset))      
      peakParticleCount(:) = 0
      peakParticlesPerArea(:) = 0.d0
      peakParticlesPerVolume(:) = 0.d0
      peakTime(:) = 0.d0
   endif
   if (duration.eqv..true.) then
      filetype = "peakandduration"
   endif
   pd(1)%dataFileName = "count_"     ! raw particle count
   pd(2)%dataFileName = "perarea_"   ! particles per square meter for each station
   pd(3)%dataFileName = "pervolume_" ! particles per cubic meter for each station
   ! open particle data station files 
   call get_command(cmdlinearg)
   do f=1,3
      pd(f)%dataFileName = "particles_" // trim(adjustl(pd(f)%dataFileName)) // trim(adjustl(filetype)) // trim(adjustl(fileExtension)) 
      pd(f)%nSnaps = 1 
      if (peak.eqv..true.) then
         pd(f)%nSnaps = snapI 
      else
         pd(f)%nSnaps = numDatasets
      endif
      pd(f)%time_increment = timeIncrement 
      pd(f)%nspool = -99 
      pd(f)%irtype = 1
      pd(f)%fun = availableUnitNumber()
      open(unit=pd(f)%fun,file=trim(pd(f)%dataFileName),status='replace',action='write')
      ! first header line (to make the data look like fort.61)    
      write(pd(f)%fun,'(a)') trim(m%agrid) // ' ! command line that created this station time series file: ' // trim(cmdlinearg)
      ! write 2nd header line to make these files look like an ascii adcirc fort.61 file (or adcirc .100 file)
      write(pd(f)%fun,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') pd(f)%nSnaps, numValuesPerDataset, pd(f)%time_increment, pd(f)%nspool, pd(f)%irtype
   enddo      
   ! open maureparticle file
   ft%fun = availableUnitNumber()
   call openFileForRead(ft%fun,ft%dataFileName,errorIO)
   ! loop until we run out of data
   do    
      write(6,'(i0,1x)',advance='no') snapI    ! update progress bar
      ! write station dataset headers for time seriest files
      if (peak.eqv..false.) then
         do f=1,3
            write(pd(f)%fun,*) snapR, snapI
         end do   
      endif
      elementParticleCount(:) = 0              ! reset particle counter for each element
      ! read one maureparticle dataset
      do p=1,numParticles
         ! maureparticle format : particleID, lon(deg), lat(deg), timestamp(sec), element index
         read(ft%fun,*,end=456) j, tempR1, tempR2, snapR, tempI1  
         particlesXY(1,j) = tempR1
         particlesXY(2,j) = tempR2
         particles2elements(j) = tempI1
         ! only add in the particle to the element if it was not marked lost
         if (tempI1.ne.0) then
            elementParticleCount(tempI1) = elementParticleCount(tempI1) + 1 
         endif 
      end do
      if ( toElement.eqv..true. ) then
         do e=1,m%ne
            particleCount(e) = elementParticleCount(e)
            particlesPerArea(e) = particleCount(e)/(0.5d0*m%areas(e))
            particlesPerVolume(e) = particleCount(e)/(0.5d0*m%areas(e)*m%eledepths(e))
         end do
      else
         ! station output
         do s=stationStart,stationEnd
            if (stations(s)%elementIndex.eq.0) then
               particleCount(s) = -99999
               particlesPerArea(s) = -99999.d0
               particlesPerVolume(s) = -99999.d0
             else
               particleCount(s) = elementParticleCount(stations(s)%elementIndex)
               particlesPerArea(s) = particleCount(s)/(0.5d0*m%areas(stations(s)%elementIndex))
               particlesPerVolume(s) = particleCount(s)/(0.5d0*m%areas(stations(s)%elementIndex)*stations(s)%elementBathyDepth)
            endif
            !write(*,*) 's=',s,'stationStart=',stationStart,' stationEnd=',stationEnd,'particleCount(s)=',particleCount(s)
         end do
      endif
      ! if duration of nonzero particle counts was requested, record it
      ! FIXME: this only measures the first instance of nonzero particle count
      if (duration.eqv..true.) then
         do j=1,numValuesPerDataSet
            if (particleCount(j).gt.0) then
               ! record the start time of nonzero particle count if it has not been recorded already
               if (durationStart(j).eq.-1.d0) then
                  durationStart(j) = snapR
               endif
            else
               ! record the end time of nonzero particle count if it has not been recorded already
               if ((durationStart(j).ne.-1.d0).and.(durationEnd(j).eq.-1.d0)) then
                  durationEnd(j) = snapR
               endif
            endif
         enddo           
      endif
      ! see if peak values have been exceeded, and if so, update the peak
      ! value and the time of peak occurrence
      if (peak.eqv..true.) then
         do j=1,numValuesPerDataset
            if ( particleCount(j).gt.peakParticleCount(j) ) then
               peakParticleCount(j) = particleCount(j)
               peakParticlesPerArea(j) = particlesPerArea(j)
               peakParticlesPerVolume(j) = particlesPerVolume(j)
               peakTime(j) = snapR
            endif
         enddo
      else
         ! write time series values for this dataset (if the peak values were not requested) 
         do j=1, numValuesPerDataset     
            write(pd(1)%fun,'(i10,2x,i10,a)') j, particleCount(j)
            write(pd(2)%fun,'(i10,2x,f15.7,a)') j, particlesPerArea(j)
            write(pd(3)%fun,'(i10,2x,f15.7,a)') j, particlesPerVolume(j)
         end do
      endif
      snapI = snapI + 1
      snapR = snapR + timeIncrement
   end do
456 close(ft%fun) ! jgf: When we've run out of datasets in the current file,
                  ! we jump to here.

    ! write peak values if requested
    if (peak.eqv..true.) then
      note = ''
      ! write peak values
      do f=1,3
         write(pd(f)%fun,*) snapR, snapI ! dataset header
      enddo
      if ( toElement.eqv..true. ) then
         do e=1,m%ne
            write(pd(1)%fun,'(i10,2x,i10,a)')   e, peakParticleCount(e)
            write(pd(2)%fun,'(i10,2x,f15.7,a)') e, peakParticlesPerArea(e)
            write(pd(3)%fun,'(i10,2x,f15.7,a)') e, peakParticlesPerVolume(e)                                       
         end do
      else
         ! station output
         do s=stationStart,stationEnd
            if (stations(s)%elementIndex.eq.0) then
               note = ' ! warning: this station is actually outside the mesh'
               write(pd(1)%fun,'(i10,2x,i10,a)')   s, -99999, trim(note)         
               write(pd(2)%fun,'(i10,2x,f15.7,a)') s, -99999.d0, trim(note)         
               write(pd(3)%fun,'(i10,2x,f15.7,a)') s, -99999.d0, trim(note)
               note = ''                  
            else
               write(pd(1)%fun,'(i10,2x,i10,a)')   s, peakParticleCount(s)
               write(pd(2)%fun,'(i10,2x,f15.7,a)') s, peakParticlesPerArea(s)
               write(pd(3)%fun,'(i10,2x,f15.7,a)') s, peakParticlesPerVolume(s)
            endif                    
            !write(*,*) 's=',s,'stationStart=',stationStart,' stationEnd=',stationEnd,'particleCount(s)=',particleCount(s)
         end do
      endif
      ! now write time of peak occurrence (or duration of first
      ! occurrence of nonzero particle count if it was requested)
      do f=1,3
         write(pd(f)%fun,*) snapR, snapI ! dataset header
      end do
      do j=1,numValuesPerDataSet
         if ( (toElement.eqv..false.).and.(stations(j)%elementIndex.eq.0) ) then
            note = ' ! warning: this station is actually outside the mesh'
            do f=1,3
               write(pd(f)%fun,'(i10,2x,f15.7,a)') j, -99999.d0, trim(note)
            end do         
            note = ''        
         else  
            if (duration.eqv..true.) then
               if ((durationStart(j).ne.-1.d0).and.(durationEnd(j).eq.-1.d0)) then
                  durationEnd(j) = snapR
               endif
               tempR1 = durationEnd(j) - durationStart(j)
               note = " ! duration (sec)"
            else
               tempR1 = peakTime(j)
               note = " ! peak time (sec)"
            endif
            do f=1,3
               write(pd(f)%fun,'(i10,2x,f15.7,a)') j, tempR1, trim(note)
            end do
         endif
      end do         
    endif
    ! close files
    do f=1,3 
      close(pd(f)%fun)
    end do
    ! end here
    stop
endif
!
!------------------------------------------------------------------------
!           A D C I R C 
!      S T A T I O N  D A T A
!------------------------------------------------------------------------ 
! open the text file for writing time series adcirc station data
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

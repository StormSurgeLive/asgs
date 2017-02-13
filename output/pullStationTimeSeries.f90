!------------------------------------------------------------------
! pullStationTimeSeries.f90: Create ADCIRC ascii elevation station
! file (fort.61) from ADCIRC fort.63 file.
!------------------------------------------------------------------
! Copyright(C) 2015--2017 Jason Fleming
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
use ioutil
implicit none
type(station_t), allocatable :: stations(:)
type(mesh_t) :: m
type(meshNetCDF_t) :: n
character(len=1024) :: line
character(len=1024) :: stationFileName ! name of file containing list of stations
type(fileMetaData_t) :: ft ! full domain time series data file to pull from
type(fileMetaData_t) :: fs ! time series data file at stations
real(8), allocatable :: adcirc_data(:,:) ! (np,num_components)
real(8) :: stationVal, temp1, temp2
integer :: numStations
integer :: nc_start(2)
integer :: nc_count(2)
integer :: sbtUnit
integer :: sfUnit
integer :: swUnit
integer :: snapi
integer :: snapr
integer :: numNodesNonDefault
integer :: i, j, node, ss, s
integer :: errorIO

! initializations
m%meshFileName = 'fort.14'
stationFileName = 'stations.txt'
fs%dataFileName = 'stations_timeseries.txt'
fs%fileFormat = ASCIIG

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
         ft%fileFormat = NETCDFG
      case("--stationfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         stationFileName = trim(cmdlinearg)
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
!  count the number of stations
sfUnit = availableUnitNumber()
call openFileForRead(sfUnit, stationFileName, errorIO)
numStations = 0
do
   read(sfUnit,*,end=7)
   numStations = numStations + 1
end do
7 write(6,'(a,i0,a,a)') 'INFO: There are ',numStations,' station(s) in ',trim(stationFileName),'.'
rewind(sfUnit)
allocate(stations(numStations))
!
! read station file
write(6,'(a)') 'INFO: Reading station file.'
do i=1, numStations
   read(sfUnit,*) stations(i)%lon, stations(i)%lat
end do
close(sfUnit)
write(6,'(a)') 'INFO: Finished reading station file.'
!
! read in the mesh
if ( ft%fileFormat.eq.NETCDFG ) then
   call findMeshDimsNetCDF(m, n)
   call readMeshNetCDF(m, n)
else
   call read14(m)
endif
! 
! grab memory for holding a single data set for 1 or 2 component data
allocate(adcirc_data(m%np,2))
!
! loop over the stations, find the element that contains each station,
! and compute the interpolation weights
write(6,'(a,i0,a)') 'INFO: Finding element containing each station and computing interpolation weights for ',numStations,' station(s).'
do s=1, numStations
   write(6,'(i0,1x)',advance='no') s    ! update progress bar
   stations(s)%elementFound = .false.
   call computeStationWeights(stations(s), m)
end do
! write the station weights to a text file for reference or troubleshooting
swUnit = availableUnitNumber()
open(unit=swUnit,file='station_weights.txt',status='replace',action='write')
do s=1,numStations 
   write(swUnit,'(3(f15.7,3x))') (stations(s)%w(i),i=1,3)
end do
close(swUnit)
write(6,'(/,a)') 'INFO: Finished finding element(s) and computing interpolation weights.'
!
! use station weights to interpolate bathy/topo at station locations
sbtUnit = availableUnitNumber()
open(unit=sbtUnit,file='station_bathytopo.txt',status='replace',action='write')
do s=1,numStations
   if (stations(s)%elementIndex.eq.0) then
      stationVal = -99999.0
   else
      stationVal = m%xyd(3,m%nm(stations(s)%elementIndex,1)) * stations(s)%w(1) &
                 + m%xyd(3,m%nm(stations(s)%elementIndex,2)) * stations(s)%w(2) &
                 + m%xyd(3,m%nm(stations(s)%elementIndex,3)) * stations(s)%w(3) 
   endif
   write(sbtUnit,'(i10,2x,e17.10)') s, stationVal
end do
close(sbtUnit)
!
! open the text file for writing time series data
fs%fun = availableUnitNumber()
open(unit=fs%fun,file=trim(fs%dataFileName),status='replace',action='write')
fs%defaultValue = -99999.
!
! open the data file (netcdf or ascii)
select case(ft%fileFormat)
case(ASCIIG)
   ft%fun = availableUnitNumber()
   call openFileForRead(ft%fun,ft%dataFileName,errorIO)
   !
   ! read header lines and write them to time series file
   read(ft%fun,'(a1024)') line
   write(fs%fun,*) trim(adjustl(line))
   read(ft%fun,*) ft%nSnaps, ft%numValuesPerDataSet, ft%time_increment, ft%nspool, ft%num_components
   write(61,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') ft%nSnaps, ft%nStations, ft%time_increment, ft%nspool, ft%num_components
   SS=1  ! jgf: initialize the dataset counter
   !
   ! jgf: loop until we run out of data
   do    
      write(6,'(i0,1x)',advance='no') ss    ! update progress bar
      read(ft%fun,'(a80)',END=123,ERR=123) Line
      read(line,*) SnapR, SnapI
      read(line,*,ERR=907,END=907) SnapR, SnapI, NumNodesNonDefault, ft%defaultValue
      goto 908  ! jgf: this file is sparse ascii
 907  NumNodesNonDefault = ft%numValuesPerDataset !jgf: this file is full ascii

 908  adcirc_data = ft%defaultValue
      select case(ft%num_components)
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
      write(fs%fun,*) snapR, snapI
      do s=1, numStations   
         call writeStationValue(adcirc_data, m, ft%numValuesPerDataset, ft%num_components, stations(s), s, fs%fun)
      end do
      ss = ss + 1
   end do
123 close(ft%fun) ! jgf: When we've run out of datasets in the current file,
                  ! we jump to here.     
case(NETCDFG)
   call determineNetCDFFileCharacteristics(ft, m, n)
   line = trim(rundes) // ' ' // trim(runid) // ' ' // trim(m%agrid)
   snapR = ft%time_increment
   write(fs%fun,*) trim(adjustl(line))
   write(fs%fun,'(i0,1x,i0,1x,f15.7,1x,i0,1x,i0)') ft%nSnaps, numStations, ft%time_increment, ft%nspool, ft%num_components
   ! get netcdf variable IDs for the the data 
   do j=1,ft%num_components
      !write(6,'(a,i0,a,a,a,i0,a)') 'DEBUG: The variable name for component ',j,' is ',trim(varname(j)),' and the variable ID is ',nc_varid(j),'.'
      call check(nf90_inq_varid(ft%nc_id, trim(adjustl(ft%varNameNetCDF(j))), ft%nc_varid(j)))
   end do   
   write(6,'(a)') 'INFO: Compiling a record of station values across all data sets.'
   ! loop over datasets   
   do i=1,ft%nSnaps
      write(6,advance='no',fmt='(i0,1x)') i  ! update progress bar
      !
      ! read the dataset from netcdf
      do j=1,ft%num_components
         nc_start = (/ 1, i /)
         nc_count = (/ m%np, 1 /)
         ! get data
         call check(nf90_get_var(ft%nc_id,ft%nc_varid(j),adcirc_data(:,j),nc_start,nc_count))
      end do
      write(fs%fun,*) ft%timesec(i), ft%it(i)      
      do s=1, numStations   
         call writeStationValue(adcirc_data, m, ft%numValuesPerDataset, ft%num_components, stations(s), s, fs%fun)
      end do
   end do
   call check(nf90_close(ft%nc_id))
case default
   write(6,*) "ERROR: File format '",TRIM(cmdlineopt),"' was not recognized."
   stop
end select
close(61)
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
subroutine writeStationValue(adcirc_data, m, numValuesPerDataset, num_components, station, s, slun)
use adcmesh
implicit none
real(8), intent(in) :: adcirc_data(numValuesPerDataSet,num_components)
integer, intent(in) :: num_components ! number of vector components, 1 is scalar etc
integer, intent(in) :: numValuesPerDataSet ! number of vector components, 1 is scalar etc
type(mesh_t), intent(inout) :: m 
type(station_t), intent(in) :: station
integer, intent(in) :: s   ! station index
integer, intent(in) :: slun ! logical unit number to write to
real(8) :: stationVal, temp1, temp2
logical :: dryNode
integer :: i
!
dryNode = .false.
if (station%elementIndex.ne.0) then
   do i=1,3
      if (adcirc_data(m%nm(station%elementIndex,i),1).eq.-99999) then
         dryNode = .true.
      endif
   end do
endif
if (num_components.eq.1) then
   if (station%elementIndex.eq.0 .or. dryNode.eqv..true.) then
      stationVal = -99999.0
   else
      stationVal = adcirc_data(m%nm(station%elementIndex,1),1) * station%w(1) &
                 + adcirc_data(m%nm(station%elementIndex,2),1) * station%w(2) &
                 + adcirc_data(m%nm(station%elementIndex,3),1) * station%w(3) 
   endif
   write(slun,'(i10,2x,e17.10)') s, stationVal 
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
      write(slun,'(i10,2(2x,e17.10))') s, temp1, temp2
   endif
endif
!-----------------------------------------------------------------------
end subroutine writeStationValue
!-----------------------------------------------------------------------

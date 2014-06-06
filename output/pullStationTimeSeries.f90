program pullStationTimeSeries
use asgsio
use adcmesh
use adcircdata
implicit none
type(station_t), allocatable :: stations(:)
character(len=1024) :: line
character(len=1024) :: stationFileName ! name of file containing list of stations
character(len=1024) :: outputfile ! time series data at stations 
real(8) :: stationVal, temp1, temp2
integer :: numStations
integer :: fileFormat
integer :: i, j, n, ss, s

! initializations
meshFileName = 'fort.14'
stationFileName = 'stations.txt'
outputFile = 'stations_timeseries.txt'
fileFormat = ASCIIG

argcount = iargc() ! count up command line options
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
         meshFileName = trim(cmdlinearg)
      case("--datafile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         dataFile = trim(cmdlinearg)
      case("--netcdf")
         write(6,'(99(a))') "INFO: processing ",trim(cmdlineopt),"."
         fileFormat = NETCDFG
      case("--stationfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         stationFileName = trim(cmdlinearg)
      case("--outputfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,'(99(a))') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         outputFile = trim(cmdlinearg)
      case default
         write(6,'(99(a))') "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
else
   ! if there weren't any command line options, prompt interactively
   WRITE(*,*)'GRID FILE?'
   READ(*,*) meshFileName

   WRITE(*,*)'STATION X,Y FILE?'
   READ(*,*) stationFileName
end if    
!
!  count the number of stations
call openFileForRead(12, stationFileName)
numStations = 0
do
   read(12,*,end=7)
   numStations = numStations + 1
end do
7 write(6,'(a,i0,a,a)') 'INFO: There are ',numStations,' station(s) in ',trim(stationFileName),'.'
rewind(12)
allocate(stations(numStations))
!
! read station file
write(6,'(a)') 'INFO: Reading station file.'
do i=1, numStations
   read(12,*) stations(i)%lon, stations(i)%lat
end do
close(12)
write(6,'(a)') 'INFO: Finished reading station file.'
!
! read in the mesh
call read14()
! 
! grab memory for holding a single data set for 1 or 2 component data
allocate(adcirc_data(np,2))
!
! loop over the stations, find the element that contains each station,
! and compute the interpolation weights
write(6,'(a,i0,a)') 'INFO: Finding element containing each station and computing interpolation weights for ',numStations,' station(s).'
do s=1, numStations
   write(6,'(i0,1x)',advance='no') s    ! update progress bar
   call computeStationWeights(stations(s))
end do
! write the station weights to a text file for reference or troubleshooting
open(unit=62,file='station_weights.txt',status='replace',action='write')
do s=1,numStations 
   write(62,'(3(f15.7,3x))') (stations(s)%weights(i),i=1,3)
end do
close(62)
write(6,'(/,a)') 'INFO: Finished finding element(s) and computing interpolation weights.'
!
! use station weights to interpolate bathy/topo at station locations
open(unit=14,file='station_bathytopo.txt',status='replace',action='write')
do s=1,numStations
   if (stations(s)%elementIndex.eq.0) then
      stationVal = -99999.0
   else
      stationVal = xyd(3,nm(stations(s)%elementIndex,1)) * stations(s)%weights(1) &
                 + xyd(3,nm(stations(s)%elementIndex,2)) * stations(s)%weights(2) &
                 + xyd(3,nm(stations(s)%elementIndex,3)) * stations(s)%weights(3) 
   endif
   write(14,'(i10,2x,e17.10)') s, stationVal
end do
close(14)
!
! open the text file for writing time series data
open(unit=61,file=trim(outputfile),status='replace',action='write')
!
! open the data file (netcdf or ascii)
select case(fileFormat)
case(ASCIIG)
   call openFileForRead(63,datafile)
   SS=1  ! jgf: initialize the dataset counter
   !
   ! jgf: loop until we run out of data
   do    
      write(6,'(i0,1x)',advance='no') ss    ! update progress bar
      read(63,'(a80)',END=123,ERR=123) Line
      read(line,*) SnapR, SnapI
      read(line,*,ERR=907,END=907) SnapR, SnapI, NumNodesNonDefault, defaultValue
      goto 908  ! jgf: this file is sparse ascii
 907  NumNodesNonDefault = numNodes !jgf: this file is full ascii
         defaultValue = -99999.
 908  dataValues = defaultValue
      select case(num_components)
      case(1) ! scalar data
         do n=1,numNodesNonDefault
            read(63,*) j, temp1
            adcirc_data(j,1) = temp1
         end do
      case(2) ! 2D vector data
         adcirc_data = defaultValue
         do n=1,numNodesNonDefault
            read(63,*) j, temp1, temp2
            adcirc_data(j,1) = temp1
            adcirc_data(j,2) = temp2
         end do     
      end select
      do s=1, numStations   
         call writeStationValue(stations(s), s)
      end do
      ss = ss + 1
   end do
123 close(63) ! jgf: When we've run out of datasets in the current file,
                  ! we jump to here.     
case(NETCDFG)
   call determineNetCDFFileCharacteristics(datafile)
   ! get netcdf variable IDs for the the data 
   do j=1,num_components
      !write(6,'(a,i0,a,a,a,i0,a)') 'DEBUG: The variable name for component ',j,' is ',trim(varname(j)),' and the variable ID is ',nc_varid(j),'.'
      call check(nf90_inq_varid(nc_id, trim(adjustl(varname(j))), nc_varid(j)))
   end do   
   write(6,'(a)') 'INFO: Compiling a record of station values across all data sets.'
   ! loop over datasets   
   do i=1,ndset
      write(6,advance='no',fmt='(i0,1x)') i  ! update progress bar
      !
      ! read the dataset from netcdf
      do j=1,num_components
         nc_start = (/ 1, i /)
         nc_count = (/ np, 1 /)
         call check(nf90_get_var(nc_id,nc_varid(j),adcirc_data(:,j),nc_start,nc_count))
      end do
      do s=1, numStations   
         call writeStationValue(stations(s), s)
      end do
   end do
   call check(nf90_close(nc_id))
case default
   write(6,*) "ERROR: File format '",TRIM(cmdlineopt),"' was not recognized."
   stop
end select
close(61)
write(6,'(/,a)') 'INFO: Wrote station values successfully.'
!-----------------------------------------------------------------------
end program pullStationTimeSeries
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!    S U B R O U T I N E     W R I T E   S T A T I O N   V A L U E 
!-----------------------------------------------------------------------
! Writes the interpolated time series value at the station location,
! or -99999 if the station is outside the mesh.
!-----------------------------------------------------------------------
subroutine writeStationValue(station, s)
use adcmesh
use adcircdata
implicit none
type(station_t), intent(in) :: station
integer, intent(in) :: s 
real(8) :: stationVal, temp1, temp2
logical :: dryNode
integer :: i
!
dryNode = .false.
if (station%elementIndex.ne.0) then
   do i=1,3
      if (adcirc_data(nm(station%elementIndex,i),1).eq.-99999) then
         dryNode = .true.
      endif
   end do
endif
if (num_components.eq.1) then
   if (station%elementIndex.eq.0 .or. dryNode.eqv..true.) then
      stationVal = -99999.0
   else
      stationVal = adcirc_data(nm(station%elementIndex,1),1) * station%weights(1) &
                 + adcirc_data(nm(station%elementIndex,2),1) * station%weights(2) &
                 + adcirc_data(nm(station%elementIndex,3),1) * station%weights(3) 
   endif
   write(61,'(i10,2x,e17.10)') s, stationVal 
else
   if (station%elementIndex.eq.0 .or. dryNode.eqv..true.) then
      temp1 = -99999.0
      temp2 = -99999.0
   else
      temp1 = adcirc_data(nm(station%elementIndex,1),1) * station%weights(1) &
            + adcirc_data(nm(station%elementIndex,2),1) * station%weights(2) &
            + adcirc_data(nm(station%elementIndex,3),1) * station%weights(3) 
      temp2 = adcirc_data(nm(station%elementIndex,1),2) * station%weights(1) &
            + adcirc_data(nm(station%elementIndex,2),2) * station%weights(2) &
            + adcirc_data(nm(station%elementIndex,3),2) * station%weights(3) 
      write(61,'(i10,2(2x,e17.10))') s, temp1, temp2
   endif
endif
!-----------------------------------------------------------------------
end subroutine writeStationValue
!-----------------------------------------------------------------------

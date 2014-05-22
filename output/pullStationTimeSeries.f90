include 'adcmesh.f90'
program pullStationTimeSeries
implicit none
type(station_t), allocatable :: stations(:)
character(len=1024)  :: line
character(len=1024)  :: outputFile
character(len=1024)  :: stationFile
character(len=1024)  :: dataFile
character(len=1024)  :: fileFormat
integer :: numSnaps
integer :: numStations
real(8)  :: defaultValue

! initializations
meshFile = 'fort.14'
stationFile = 'stations.txt'
outputFile = 'stations.kml'
fileFormat = 'ascii'

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
         write(6,*) "info: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         meshFile = trim(cmdlinearg)
      case("--datafile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,*) "info: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         dataFile = trim(cmdlinearg)

      case("--fileformat")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,*) "info: processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         fileFormat = trim(cmdlinearg)
      case("--stationfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         stationFile = trim(cmdlinearg)
      case("--outputfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
         outputFile = trim(cmdlinearg)
      case("--minmax")
         minMax = .true.
      case default
         write(6,*) "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
else
   ! if there weren't any command line options, prompt interactively
   WRITE(*,*)'GRID FILE?'
   READ(*,*) meshFile

   WRITE(*,*)'STATION X,Y FILE?'
   READ(*,*) stationFile      
end if    
!
!  count the number of stations
call openFileForRead(12, stationFile)
do
   read(12,*,end=7)
   numStations = numStations + 1
end do
write(*,'(a,i0,a,a)') 'INFO: There are ',numStations,' station(s) in ',trim(stationFile),'.'
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
! loop over the stations, find the element that contains each station,
! and compute the interpolation weights
do i=1, numStations
   call findStationElements(stations(i))
   call computeStationWeights(stations(i))
end do
!
! open the data file (netcdf or ascii)
select case(trim(adjustl(fileFormat))
case("ascii","Ascii","ASCII")
   call openFileForRead(12,datafile)
case("netcdf","Netcdf","NETCDF")

case default
   write(6,*) "ERROR: File format '",TRIM(cmdlineopt),"' was not recognized."
   stop
end select

!
! read the raw data, interpolate using the interpolation weights, 
! and write results (netcdf, compact ascii, full ascii)


!
! make note of stations not found in the mesh




end program pullStationTimeSeries


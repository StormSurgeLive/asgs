!------------------------------------------------------------------
! stationProcessor.f90: Reads ADCIRC netCDF station file(s) and 
! performs specified operation for given station list
!------------------------------------------------------------------
! Copyright(C) 2017 Jason Fleming
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
! FIXME: This code assumes that the station files are provided in 
! chronological order. 
!------------------------------------------------------------------
program stationProcessor
use asgsio
use adcmesh
use ioutil
use logging
implicit none
type(mesh_t) :: m
type(meshNetCDF_t) :: n
type(station_t), allocatable :: stations(:)
character(len=1024) :: metadataLine
character(len=20) :: operation  ! min, max, mean, median, range, maxtime, mintime, etc
real(8) :: timesecStart = -99999.d0  ! beginning of time range (s) to use from fort.61.nc file
real(8) :: timesecEnd = -99999.d0  ! end of time range (s) to use from fort.61.nc file 
character(len=1024) :: stationFileName ! name of file containing list of stations in standard metadata format
logical, allocatable :: outOfRange(:)      ! true if requested start or end times are out of data range in file
logical :: strictTimeRange ! true if output should not be written when data are not available for the full requested range
character(len=50), allocatable :: dataFileStationIDs(:) ! holder for netcdf IDs array
character(len=1024) :: outputfile      ! average data at stations 
integer :: numStationsInList ! number of stations in the list of interest (not the fort.61)
real(8), allocatable :: stationData(:) ! one component of one complete dataset from fort.61
real(8), allocatable :: resultVal(:,:) ! (irtype,numStationsInList)
integer, allocatable :: numObs(:,:) ! number of non-missing values at each station (irtype,numStationsInList)
logical, allocatable :: stationFound(:) ! true if a station in the specified list was found in the fort.61
type(characterVector1D_t) :: stationFileNames ! expandable list of station files to be processed
integer :: outu ! i/o unit number for results file
integer :: stu ! i/o unit number for specified station list file
integer :: bangCounter ! counts the record separators ("!") in the station metadata
integer :: oldBangPosition ! character index where previous record separator was found
integer :: lineNum ! counts line numbers for use in error messages
integer :: dsta ! station counter in the fort.61 datafile
integer :: i ! command line argument counter
integer :: ista ! station counter in the specified station list
integer :: j ! character index position counter
integer :: s ! station counter
integer :: t ! dataset counter
integer :: c ! station data component counter 
integer :: f ! data file counter
integer :: tdata ! dataset counter for indexing datasets in fort.61
integer :: lastDataSetInTimeRange ! index of last dataset from fort.61
integer :: firstDataSetInTimeRange ! index of first dataset from fort.61
integer :: numDataSetsInTimeRange ! number of datasets in time range from fort.61
integer :: numDataSetsBeforeStart  !  
integer :: numDataSetsAfterEnd     !
integer :: nc_start_station_names(2) ! index to start reading station name array from netcdf
integer :: nc_count_station_names(2) ! number of station name array items to read from netcdf
character(len=1024) :: line ! comment line at top of result file
character(len=40) :: coordString ! holds lon lat values for stations
type(fileMetaData_t), allocatable :: sf(:) ! file containing station data (e.g., fort.61)
integer :: errorIO
integer :: nc_start(2)
integer :: nc_count(2)
integer :: nc_varid_station_name
type(characterVector1D_t) :: dataFileNames
character(len=2000) :: tempName
! 
! initializations
stationFileName = 'stations.txt'
outputFile = 'station_averages.txt'
operation = 'mean'
strictTimeRange = .false.
call initC1D(stationFileNames)
call initC1D(dataFileNames)
call initLogging(availableUnitNumber(),'stationProcessor.f90')
!
argcount = command_argument_count() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
      case("--strict-time-range")
         write(scratchMessage,'(99(a))') 'Processing ',trim(cmdlineopt),'.'
         call allMessage(INFO,scratchMessage)
         strictTimeRange = .true.
      case("--datafile")
         i = i + 1
         call getarg(i, tempName)
         call appendC1D(dataFileNames,tempName)
         write(scratchMessage,'(99(a))') 'Processing ',trim(cmdlineopt),' ',trim(tempName),'.'
         call allMessage(INFO,scratchMessage)
      case("--stationfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(scratchMessage,'(99(a))') 'Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         call allMessage(INFO,scratchMessage)
         stationFileName = trim(cmdlinearg)
      case("--outputfile")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(scratchMessage,'(99(a))') 'Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         call allMessage(INFO,scratchMessage)
         outputFile = trim(cmdlinearg)
      case("--timesec-start")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(scratchMessage,'(99(a))') 'Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         call allMessage(INFO,scratchMessage)
         read(cmdlinearg,*) timesecStart
      case("--timesec-end")
         i = i + 1
         call getarg(i, cmdlinearg)
         write(scratchMessage,'(99(a))') 'Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         call allMessage(INFO,scratchMessage)
         read(cmdlinearg,*) timesecEnd
      case("--operation") 
         i = i + 1
         call getarg(i, cmdlinearg)
         write(scratchMessage,'(99(a))') 'Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
         call allMessage(INFO,scratchMessage)
         operation = trim(cmdlinearg)
      case default
         write(scratchMessage,'(99(a))') 'Command line option "',TRIM(cmdlineopt),'" was not recognized.'
         call allMessage(INFO,scratchMessage)
      end select
   end do
endif
!
!  count the number of stations
stu = availableUnitNumber()
call openFileForRead(stu, stationFileName, errorIO)
! count the stations
numStationsInList = 0
lineNum = 1
do
   read(unit=stu,fmt=*,end=7,err=321,iostat=errorIO)
   numStationsInList = numStationsInList + 1
   lineNum = lineNum + 1
end do
7  write(scratchMessage,'("There are ",i0," station(s) in ",a,".")') numStationsInList, trim(stationFileName)
call allMessage(INFO,scratchMessage)
rewind(stu)
allocate(stations(numStationsInList))
!
! read station file
call allMessage(INFO,'Reading station file using standard ADCIRC station metadata format.')
lineNum = 1
do s=1, numStationsInList
   read(unit=stu,fmt='(a1024)',err=321,end=432,iostat=errorIO) metadataLine
   ! find the positions of the exclamation points that we are using 
   ! as record separators and use them to 
   bangCounter = 0
   oldBangPosition = 0
   do j=1,len_trim(metadataLine)
      if (metaDataLine(j:j).eq.'!') then
         bangCounter = bangCounter + 1
         select case(bangCounter)
         case(1) ! station coordinates (lon deg E space lat deg N) 
            coordString = metadataLine(oldBangPosition+1:j-1)
            read(coordString,*) stations(s)%lon, stations(s)%lat 
         case(2) ! stationID
            stations(s)%stationID = metadataLine(oldBangPosition+1:j-1)
         case(3) ! agency
            stations(s)%agency = metadataLine(oldBangPosition+1:j-1)
         case(4) ! description 
            stations(s)%description = metadataLine(oldBangPosition+1:j-1)
         case(5) ! datum 
            stations(s)%datum = metadataLine(oldBangPosition+1:j-1)
         case default
            ! found too many bangs, perhaps an erroneously embedded one?
            write(scratchMessage,'("Found more than five record separators (!) in the stations metadata file on line ",i0," indicating that the stations were not parsed correctly.")') lineNum
            call allMessage(ERROR,scratchMessage)
            stop 
         end select
         oldBangPosition = j
      endif
   end do
   lineNum = lineNum + 1
end do
close(stu)
call allMessage(INFO,'Finished reading station file.')
!
! make a list of which array indices from fort.61 match each station in the station file
! pull the data for those stations and store in an array
! once all data has been pulled for those stations perform the specified operation on the stations
! report the results
!
! allocate space for holding and reading data file(s)
allocate(sf(dataFileNames%n))
! allocate space for recording which files have data in the specified time range
allocate(outOfRange(dataFileNames%n))
do f=1,dataFileNames%n
   sf(f)%dataFileName = dataFileNames%v(f)
end do
outOfRange(:) = .false.
!
! open station file, determine if it contains data in the time period of interest
numDataSetsInTimeRange = 0
do f=1, dataFileNames%n 
   ! assume the station files are all in netcdf format
   sf(f)%dataFileFormat = NETCDFG
   ! open the netcdf station file(s), get dimensions, etc
   call determineNetCDFFileCharacteristics(sf(f), m, n)
   ! check to see if data are available for the full requested time range
   if ( (timesecStart.gt.0).and.(timesecStart.gt. sf(f)%timesec(sf(f)%nSnaps) ) ) then
      write(scratchMessage,'("The data file ends at time t=",e17.8," (s) but the requested start time is t=",e17.8," (s).")') timesecStart, sf(f)%timesec(sf(f)%nSnaps)  
      call allMessage(INFO,scratchMessage)
      outOfRange(f) = .true.
   endif
   if ( (timesecEnd.gt.0).and.(timesecEnd.lt.sf(f)%timesec(1)) ) then
      write(scratchMessage,'("The data file starts at time t=",e17.8," (s) but the requested end time is t=",e17.8," (s).")') , timesecEnd, sf(f)%timesec(1)
      call allMessage(WARNING,scratchMessage)
      outOfRange(f) = .true.
   endif
   ! count the total number of datasets in the file within the specified time range
   if (outOfRange(f).eqv..false.) then
      do t=1,sf(f)%nSnaps
         if ( (timesecEnd.gt.0).and.(sf(f)%timesec(t).gt.timesecEnd) ) then
            cycle
         endif
         if ( (timesecStart.gt.0).and.(sf(f)%timesec(t).lt.timesecStart) ) then
            cycle
         endif
         numDataSetsInTimeRange = numDataSetsInTimeRange + 1
      end do
   endif   
end do
!
! allocate memory for holding data for each station
do ista=1,numStationsInList
   allocate(stations(ista)%d(sf(1)%irtype,numDataSetsInTimeRange))
end do
!
! for files that contain data in the time period of interest, check to 
! see if all the specified stations are present in the file
!
! allocate space to record which stations were found in all data files
allocate(stationFound(numStationsInList))
stationFound(:) = .true.
do f=1, dataFileNames%n 
   if (outOfRange(f).eqv..true.) then
      ! no data of interest in this file
      cycle
   endif
   write(scratchMessage,'("There are ",i0," stations in the file ",a,".")') sf(f)%nStations,trim(sf(f)%dataFileName)
   call allMessage(INFO,scratchMessage)
   !
   ! allocate space to record the station IDs in the station file 
   allocate(dataFileStationIDs(sf(f)%nStations)) 
   !
   ! read the station_name array (which actually contains the stationID instead
   ! of the description) from this netcdf file
   call check(nf90_open(trim(sf(f)%dataFileName), NF90_NOWRITE, sf(f)%nc_id))
   nc_start_station_names = (/ 1, 1 /)
   nc_count_station_names = (/ sf(f)%station_namelen, sf(f)%nStations /)
   call check(nf90_inq_varid(sf(f)%nc_id, "station_name", sf(f)%NC_VarID_station))
   call check(nf90_get_var(sf(f)%nc_id,sf(f)%nc_varid_station,dataFileStationIDs,nc_start_station_names,nc_count_station_names))
   !
   ! for each station in the list of interest, determine the array index in the station
   ! data file that corresponds to that station
   do ista=1,numStationsInList
      if (stationFound(ista).eqv..false.) then
         cycle ! if it was not found in a previous file, exclude it from consideration
      endif
      do dsta=1,sf(f)%nStations
         stationFound(ista)= .false.
         if (trim(adjustl(stations(ista)%stationID)).eq.trim(adjustl(dataFileStationIDs(dsta)))) then
            stations(ista)%iID = dsta
            stationFound(ista) = .true.
            exit
         endif
      end do
      if (stationFound(ista).eqv..false.) then
         write(scratchMessage,'("Station ID ",a," was not found in the station data file. All data for this station will be written as undefined values (-99999.0).")') trim(stations(ista)%stationID)
         call allMessage(WARNING,scratchMessage)
      endif
   end do
end do

do f=1, dataFileNames%n 
   if (outOfRange(f).eqv..true.) then
      ! no data of interest in this file
      cycle
   endif
   !
   ! determine which datasets fall in the specified time range
   firstDataSetInTimeRange = 0
   numDataSetsAfterEnd = 0
   numDataSetsBeforeStart = 0
   !
   ! check to see if data are available for the full requested time range
   if ( (timesecStart.gt.0).and.(timesecStart.lt.sf(f)%timesec(1)) ) then
      write(scratchMessage,'("The data file starts at time t=",e17.8," (s) before the requested start time t=",e17.8," (s).")') sf(f)%timesec(1), timesecStart 
      call allMessage(INFO,scratchMessage)
   endif
   if ( (timesecEnd.gt.0).and.(timesecEnd.gt.sf(f)%timesec(sf(f)%nSnaps)) ) then
      write(scratchMessage,'("The data file ends at time t=",e17.8," (s) after the requested end time t=",e17.8," (s).")') sf(f)%timesec(sf%nSnaps), timesecEnd
      call allMessage(WARNING,scratchMessage)
   endif
   do t=1,sf(f)%nSnaps
      ! exclude datasets after the specified end time (if any)
      if ( (timesecEnd.gt.0).and.(sf(f)%timesec(t).gt.timesecEnd) ) then
         numDataSetsAfterEnd = numDataSetsAfterEnd + 1
         cycle
      endif
      ! exclude datasets before the specified start time (if any)
      if ( (timesecStart.gt.0).and.(sf(f)%timesec(t).lt.timesecStart) ) then
         numDataSetsBeforeStart = numDataSetsBeforeStart + 1
         cycle
      endif
      if ( firstDataSetInTimeRange.eq.0 ) then
         firstDataSetInTimeRange = t
      endif
   end do 
   lastDataSetInTimeRange = firstDataSetInTimeRange + numDataSetsInTimeRange - 1 
   !
   ! loop over datasets in this file, loading data if they fall within the specified time range
   allocate(stationData(sf(f)%nStations))
   t=1
   do tdata=firstDataSetInTimeRange, lastDataSetInTimeRange
      write(6,advance='no',fmt='(i0,1x)') tdata  ! update progress bar
      !
      ! read one dataset from netcdf, one component at a time
      nc_start = (/ 1, tdata /)
      nc_count = (/ sf(f)%nStations, 1 /)
      do c=1,sf(f)%irtype
         ! get data
         call check(nf90_get_var(sf(f)%nc_id,sf(f)%ncds(c)%nc_varid,stationData,nc_start,nc_count))
         ! go through the specified list of stations and store the 
         ! values from the corresponding station index 
         do s=1,numStationsInList
            if ( (stations(s)%iID.ne.0).and.(stationFound(s).eqv..true.) ) then
               stations(s)%d(c,t) = stationData(stations(s)%iID)
            else
               stations(s)%d(c,t) = -99999.d0
            endif
         end do
      end do
      t = t + 1
   end do 
   call check(nf90_close(sf(f)%nc_id))
   deallocate(dataFileStationIDs) 
   if ( (outOfRange(f).eqv..true.).and.(strictTimeRange.eqv..true.) ) then
      call allMessage(INFO,'The file does not have data for the full time range, and the command line option --strict-time-range was set.')
      write(scratchMessage,'("The first data set in the time range is data set ",i0," and the last is data set ",i0,"; there are ",i0," data sets in the given time range.")') firstDataSetInTimeRange, lastDataSetInTimeRange, numDataSetsInTimeRange
      call allMessage(INFO,scratchMessage)
      call allMessage(INFO,'Output will not be written because of the unavailability of some of the data in the requested time range. Execution complete.')
      stop
   endif 
end do
!
! now perform the specified operation on the data obtained for the
! stations during the specified time interval
allocate(resultVal(sf(1)%irtype,numStationsInList))
! initialize values
select case(trim(operation))
case("min")
   resultVal = huge(0.d0)
case("max")
   resultVal = tiny(0.d0)
case("mean","average","avg")
   resultVal = 0.d0 
case default
   write(scratchMessage,'("The operation ",a," is not supported.")') trim(operation) 
   call allMessage(ERROR,scratchMessage)
end select
!
! perform the specified operation on each component of each specified station
! FIXME: this may not do the right thing for multicomponent (i.e., vector) quantities
allocate(numObs(sf(1)%irtype,numStationsInList))
numObs(:,:) = numDataSetsInTimeRange
do s=1, numStationsInList
   do c=1, sf(1)%irtype
      do t=1, numDataSetsInTimeRange
         ! avoid use of a missing value into the avg
         if ( stations(s)%d(c,t).lt.-9999.d0 ) then
            numObs(c,s) = numObs(c,s) - 1
            cycle
         endif
         select case(trim(operation))
         case("min")
            if ( resultVal(c,s).lt.stations(s)%d(c,t) ) then
               resultVal(c,s) = stations(s)%d(c,t)
            endif
         case("max")
            if ( resultVal(c,s).gt.stations(s)%d(c,t) ) then
               resultVal(c,s) = stations(s)%d(c,t)
            endif         
         case("mean","average","avg")
            resultVal(c,s) = resultVal(c,s) + stations(s)%d(c,t)
         case default
            write(scratchMessage,'("The operation ",a," is not supported.")') trim(operation) 
            call allMessage(ERROR,scratchMessage)
         end select
      end do
   end do
end do
! finish computing the mean
select case(trim(operation))
case("mean","average","avg")
   do s=1, numStationsInList
      do c=1, sf(1)%irtype ! iterate over components
         if ( numObs(c,s).ne.0 ) then
            resultVal(c,s) = resultVal(c,s) / dble(numObs(c,s))
         else
            resultVal(c,s) = -99999.d0
         endif
      end do
   end do
end select
!
! now write values to the output file
outu = availableUnitNumber()
open(unit=outu,file='processedStations.dat',status='replace',action='write')
line = 'rundes: '//trim(rundes)//' runid: '//trim(runid)//' agrid:'//trim(m%agrid)
write(outu,'("# ",a)') trim(line) ! comment line
write(outu,'(a)') '# stationID ! operationType ! timestart(s) ! timeend(s) ! (result ! numObservations (c=1,irtype))'
do s=1,numStationsInList
   write(outu,fmt='(a,1x,a,1x,2(f21.7,1x),3(f21.7,1x,i0,1x))') trim(stations(s)%stationID), trim(operation), timesecStart, timesecEnd, (resultVal(c,s), numObs(c,s), c=1,sf(1)%irtype)
end do

stop
! jump to here when encountering eof when reading file
432 write(scratchMessage,'("Attempted to read line ",i0," from the file ",a," when the end of the file was unexpectedly encountererd.")') lineNum, trim(stationFileName)

! jump to here when encountering i/o error when reading file
321 write(scratchMessage,'("Attempted to read line ",i0," from the file ",a," when an i/o error occurred. The Fortran error code was ",i0,".")') lineNum, trim(stationFileName), errorIO
call allMessage(ERROR,scratchMessage)
stop

!------------------------------------------------------------------------
end program stationProcessor
!------------------------------------------------------------------------

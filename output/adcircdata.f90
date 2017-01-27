!--------------------------------------------------------------------------
! adcircdata.f90
!
! A module that provides helper subroutines when dealing with ADCIRC 
! output files in ascii and netcdf format. 
!--------------------------------------------------------------------------
! Copyright(C) 2014--2015 Jason Fleming
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
module adcircdata
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
#ifdef ASGSNETCDF
use netcdf
#endif
implicit none

#ifdef ASGSNETCDF
character(NF90_MAX_NAME) :: varName(3)
character(NF90_MAX_NAME) :: thisVarName
character(NF90_MAX_NAME) :: aVarName
character(NF90_MAX_NAME) :: timeOfVarName
#endif

integer :: NC_VarID(3)
integer :: NC_DimID_time
integer :: NC_VarID_time

integer :: agold ! netcdf i/o status for old agrid attribute
integer :: agnew ! netcdf i/o status for new agrid attribute
integer :: nerr ! netcdf i/o status
integer :: nc_count(2)
integer :: nc_start(2)
integer :: nc_count3D(3)
integer :: nc_start3D(3)
integer :: numNodesNonDefault
integer :: ndim
integer :: nvar
integer :: natt
integer :: ncformat
integer :: ndset
integer :: num_components
real(8) :: time_increment
real(8) :: defaultValue
real(8) :: fillValue = -99999.d0
real(8), allocatable :: timesec(:)  ! time in seconds associated with each dataset
real(8), allocatable :: adcirc_data(:,:) ! generic holder for converted data
real(8), allocatable :: gold_data(:,:)   ! holder for expected outputd data
real(8), allocatable :: test_data(:,:)   ! generic holder for converted data
real(8), allocatable :: result_data(:,:) ! generic holder for converted data
integer, allocatable :: adcirc_idata(:,:) ! generic holder for converted integer data
real(8), allocatable :: adcirc_data3D(:,:,:) ! generic holder for converted data
character(len=50), allocatable :: dataFileStationIDs(:) ! namelen from adcirc is 50
character(len=120) :: datenum !seconds since 2008-07-31 12:00:00 +00:00
integer :: nspool
integer :: it
character(1024) :: datafile
character(1024) :: ascii_datafile_name
logical :: sparse
logical :: stationfile ! true if the data represent recording stations
logical :: extremesWithTime ! true if data are min/max with time of occurrence
real(8), allocatable :: extremes(:)
real(8), allocatable :: extremeTimes(:)
real(8), allocatable :: dataValues(:)
integer :: numNodes
integer :: nc_dimid_station
integer :: nc_varid_station_name
integer :: nc_dimid_namelen
integer :: station_namelen
integer :: nStations  ! number of stations in the data file
integer :: numSnaps
real(8) :: snapR ! time in seconds
integer :: snapI ! time step
integer :: nCol ! number of columns of data
real(8) :: tinterval ! spooling interval in seconds
integer :: interval ! spooling interval in timesteps
logical :: writeMaxTimes !.true. if the time of maximum occurrence should be written
logical :: findMin ! .true. if the min should be found instead of the max
character(len=1024) :: header1  ! 1st line in ascii adcirc output file
character(len=1024) :: header2  ! 2nd line in ascii adcirc output file
character(len=80) :: rundes  ! 1st line in adcirc fort.15 input file
character(len=80) :: runid   ! 2nd line in adcirc fort.15 input file
logical :: isInteger = .false.  ! .true. for integer data
character(len=20) :: dataCenter ! "Node" or "Element"

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
subroutine determineNetCDFFileCharacteristics(datafile)
use asgsio
use adcmesh, only : np, nc_dimid_node, agrid, readMeshCommentLineNetCDF
implicit none
character(len=1024), intent(in) :: datafile
!
integer :: i, j

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
! determine the number of snapshots in the file
call check(nf90_inquire_dimension(nc_id,nc_dimid_time,len=ndset))
write(6,'(a,i0,a)') 'INFO: There is/are ',ndset,' dataset(s) in the file.'
if (ndset.eq.0) then
   write(6,'(a,a,a)') 'ERROR: The file "',trim(datafile),'" does not contain any output data.'
   stop 1
endif
!
!  get time
!
! load up the time values (in seconds)
allocate(timesec(ndset))
call check(nf90_inq_varid(nc_id, "time", NC_VarID_time))
call check(nf90_get_var(nc_id, NC_VarID_time, timesec, (/ 1 /), (/ ndset /) ))
call check(nf90_get_att(nc_id,nc_varid_time,'units',datenum))
!
! determine the type of file that we have
do i=1,3
   varname(i) = "null"
end do
num_components = 1
! is it a station file?
do i=1,nvar
   call check(nf90_inquire_variable(nc_id, i, thisVarName))
   if (trim(thisVarName).eq.'station_name') then
      stationFile = .true.
      call check(nf90_inq_dimid(nc_id, "station", nc_dimid_station))
   endif 
end do
! if this is not a station file, find the mesh node dimension and
! comment 
if ( stationfile.eqv..false.) then
   call readMeshCommentLineNetCDF()
   ! determine the number of nodes
   call check(nf90_inq_dimid(nc_id, "node", nc_dimid_node))
endif
! 
! find the rundes and runid attributes in case they need to be written
! to ascii output
agnew = nf90_get_att(nc_id,nf90_global,'agrid',agrid)
if (agnew.ne.NF90_NOERR) then
   agold = nf90_get_att(nc_id,nf90_global,'grid',agrid)
   if (agold.ne.NF90_NOERR) then
      agrid = 'agrid_not_found'
   endif
endif
nerr = nf90_get_att(nc_id,nf90_global,'rundes',rundes)
if ( nerr.ne.NF90_NOERR ) then
   rundes = 'rundes' !TODO: make adcirc write this value to netcdf output files
endif
nerr = nf90_get_att(nc_id,nf90_global,'runid',runid) 
if ( nerr.ne.NF90_NOERR ) then
   runid = 'runid'   !TODO: make adcirc write this value to netcdf output files 
endif
!   
! determine the type of data in the file, and set the output
! filename accordingly
do i=1,nvar
   call check(nf90_inquire_variable(nc_id, i, thisVarName))
   select case(trim(thisVarName))
   case("u-vel3D","v-vel3D","w-vel3D")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC 3D " &
         // "water current velocity file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.42"
      else
         ascii_datafile_name = "fort.45"
      endif
      num_components = 3
      varname(1) = "u-vel3D"
      varname(2) = "v-vel3D"
      varname(3) = "w-vel3D"
      exit
   case("zeta")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC water " &
         // "surface elevation file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.61"          
      else 
         ascii_datafile_name = "fort.63"
      endif 
      varname(1) = "zeta"
      write(6,'(a,a,a)') 'INFO: The variable name is ',trim(adjustl(varname(1))),'.'
      exit
   case("u-vel","v-vel")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC water " &
         // "current velocity file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.62"
      else
         ascii_datafile_name = "fort.64"
      endif
      num_components = 2
      varname(1) = "u-vel"
      varname(2) = "v-vel"
      exit
   case("pressure")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC barometric " &
         // "pressure file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.71"
      else
         ascii_datafile_name = "fort.73"
      endif
      varname(1) = "pressure"
      exit
   case("windx","windy")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC wind " &
         // "velocity file."
      if ( stationfile.eqv..true. ) then
          ascii_datafile_name = "fort.72"
      else
          ascii_datafile_name = "fort.74"
      endif
      num_components = 2
      varname(1) = "windx"
      varname(2) = "windy"
      exit
   case("zeta_max")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC maximum " &
         // "water elevation file."
      ascii_datafile_name = "maxele.63"
      varname(1) = "zeta_max"
      num_components = 1
      ndset = 1
      timeOfVarName = 'time_of_'//trim(thisVarName)
      do j=1,nvar
         call check(nf90_inquire_variable(nc_id, j, aVarName))
         ! check to see if this is a new-style min/max file that records
         ! the time of the min or max, and if so, prepare to convert the
         ! time information as well
         if (trim(aVarName).eq.trim(timeOfVarName)) then
            write(6,'(a)') 'INFO: The file contains time of occurrence data.'
            extremesWithTime = .true.
            varname(2) = trim(timeOfVarName)
            num_components = 2
            ndset = 2
            exit
         endif 
      end do
      exit
   case("wind_max")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC maximum " &
         // "wind speed file."
      ascii_datafile_name = "maxwvel.63"
      ndset = 1
      num_components = 1
      varname(1) = "wind_max"
      ! check to see if this is a new-style min/max file that records
      ! the time of the min or max, and if so, prepare to convert the
      ! time information as well
      timeOfVarName = 'time_of_'//trim(thisVarName)
      do j=1,nvar
         call check(nf90_inquire_variable(nc_id, j, aVarName))
         if (trim(aVarName).eq.trim(timeOfVarName)) then
            write(6,'(a)') 'INFO: The file contains time of occurrence data.'
            extremesWithTime = .true.
            varname(2) = trim(timeOfVarName)
            num_components = 2
            ndset = 2
            exit
         endif 
      end do
      exit
   case("dir")
      write(6,'(a)') "INFO: Preparing to write a mean wave " &
         // "direction file."
      ascii_datafile_name = "swan_DIR.63"
      varname(1) = "dir"
      exit
   case("hs")
      write(6,'(a)') "INFO: Preparing to write a significant " &
          // "wave height file."
      ascii_datafile_name = "swan_HS.63"
      varname(1) = "hs"
      exit
   case("tmm10")
      write(6,'(a)') "INFO: Preparing to write a mean absolute " &
        // "wave period file."
      ascii_datafile_name = "swan_TMM10.63"
      varname(1) = "tmm10"
      exit
   case("tps")
      write(6,'(a)') "INFO: Preparing to write a relative peak " &
         // "period file."
      ascii_datafile_name = "swan_TPS.63"
      varname(1) = "tps"
      exit
   case("swan_HS_max")
      write(6,'(a)') "INFO: Preparing to write a maximum " &
         // "significant wave height file."
      ascii_datafile_name = "swan_HS_max.63"
      ndset = 1
      varname(1) = "swan_HS_max"
      exit
   case("swan_TPS_max")
      write(6,'(a)') "INFO: Preparing to write an maximum relative " &
        // "peak wave period file."
      ascii_datafile_name = "swan_TPS_max.63"
      ndset = 1
      varname(1) = "swan_TPS_max"
      exit                        
   case default
      !jgf this is tmi: write(6,*) "DEBUG: Did not recognize the variable name '"//trim(thisVarName)//"'."
      cycle     ! did not recognize this variable name
   end select
end do
!write(6,*) "INFO: " // trim(ascii_datafile_name)
! if this is not a station file, find the mesh node dimension and
! comment 
if ( stationfile.eqv..false.) then
   ! determine the number of nodes
   call check(nf90_inq_dimid(nc_id, "node", nc_dimid_node))
   call check(nf90_inquire_dimension(nc_id, nc_dimid_node, len=np))
else
   ! determine the number of stations
   call check(nf90_inq_dimid(nc_id, "station", nc_dimid_station))
   call check(nf90_inquire_dimension(nc_id, nc_dimid_station, len=nStations))

   call check(nf90_inq_dimid(nc_id, "namelen", nc_dimid_namelen))
   call check(nf90_inquire_dimension(nc_id, nc_dimid_namelen, len=station_namelen))
endif                                                             


! determine time increment between output writes
if ( (ndset.gt.1).and.(extremesWithTime.eqv..false.) ) then
   time_increment = timesec(2) - timesec(1)
else
   time_increment = -99999.d0
endif
nspool = -99999
it = -99999
defaultValue = -99999.d0
!
! get the variable id(s) of the data we want to convert
do i=1,num_components
   call check(nf90_inq_varid(nc_id, varname(i), nc_varid(i)))
end do

!----------------------------------------------------------------------
end subroutine determineNetCDFFileCharacteristics
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
!----------------------------------------------------------------------
end module adcircdata
!----------------------------------------------------------------------
!----------------------------------------------------------------------




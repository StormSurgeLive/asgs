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
integer :: argcount
character(1024) :: cmdlineopt
character(1024) :: cmdlinearg
integer, parameter :: minUnitNumber = 10 ! lowest fortran i/o unit number
integer, parameter :: maxUnitNumber = 999 ! highest fortran i/o unit number
!
! file format integers
integer, parameter :: OFF = 0
integer, parameter :: ASCII = 1     ! ADCIRC ASCII
integer, parameter :: SPARSE_ASCII = 4  ! ADCIRC sparse ASCII
integer, parameter :: NETCDF3 = 3
integer, parameter :: NETCDF4 = 5
integer, parameter :: XDMF = 7
integer, parameter :: NETCDFG = 35  ! either NETCDF3 or NETCDF4
integer, parameter :: ASCIIG = 14   ! either ASCII or SPARSE_ASCII
integer, parameter :: MAUREPT = 108 ! output from maureparticle
!
integer :: netCDFDataType = NF90_DOUBLE ! NF90_INT NF90_FLOAT 
!
integer :: nc_id ! netcdf id of file
integer :: errorIO  ! zero if the file opened or read successfully
!
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
   character(len=20), allocatable :: dataCenter(:) ! "Node" or "Element" 
   character(len=20), allocatable :: numberType(:)   ! "Int" or "Float"
   integer, allocatable :: numberPrecision(:)         ! 4 or 8   
   integer, allocatable :: numComponents(:) ! rank of the data array
   !
   ! file characteristics
   character(len=2048) :: dataFileName ! full path
   character(len=20) :: dataFileType ! content: fort.13, fort.63, fort.67, maxele.63, noff.100 etc   
   character(len=1024) :: fileTypeDesc ! analyst-readable description
   integer :: fileFormat       ! ASCII, NETCDF4, XDMF etc parameters defined above
   integer :: nSnaps           ! number of datasets in the time varying file
   logical :: timeOfOccurrence ! .true. if min/max file has time of occurrence data
   integer :: fun ! file i/o unit number; only needed for ascii files
   !
   ! netcdf 
   integer :: nc_id        ! netcdf ID for the file
   integer :: numVarNetCDF ! number of variables targetted in NetCDF4 file
   integer, allocatable :: nc_varID(:) ! netcdf variable ID for targetted variables
   character(NF90_MAX_NAME), allocatable :: varNameNetCDF(:) ! variable names inside files   
   integer, allocatable :: nc_type(:) ! netcdf variable type for targetted variables
   integer :: nvar         ! number of variables in the netcdf file
   integer :: nc_dimid_node ! netcdf ID for the dimension for number of nodes
   integer :: nc_dimid_nele ! netcdf ID for the dimension of the number of elements
   integer :: nc_dimid_time ! netcdf ID for the time dimension
   integer :: nc_varid_time ! netcdf ID for the time variable
   integer :: ndim          ! number of dimensions in the netcdf file
   integer :: natt          ! number of attributes in the netcdf file
   !
   ! xdmf 
   character(len=2048) :: xmfFile ! name of XDMF XML file
   integer :: xmfUnit      ! logical unit number of XDMF XML file
   integer :: numVarXDMF   ! number of variables as represented in XDMF XML
   character(NF90_MAX_NAME), allocatable :: varNameXDMF(:)   ! number of variables as represented in XDMF XML
   !
   ! particles
   integer :: maxParticles  ! max number of particles at any one time
   integer, allocatable :: numParticlesPerSnap(:) ! (nsnaps)num part in each snap 

end type fileMetaData_t

!-----------
!-----------
contains
!-----------
!-----------

!-----------------------------------------------------------------------
! S U B R O U T I N E   F I N D   U N U S E D   U N I T   N U M B E R
!-----------------------------------------------------------------------
! Dynamic unit numbers prevent collisions. 
!-----------------------------------------------------------------------
integer function availableUnitNumber()
implicit none
logical :: unusedUnitNumberFound ! .true. if an unused unit number is available
logical :: isOpen ! true if an i/o unit number is connected to an open file

do availableUnitNumber = minUnitNumber,maxUnitNumber
   inquire(unit=availableUnitNumber,opened=isOpen,iostat=errorIO)
   if (errorIO.eq.0) then
      if (isOpen.eqv..true.) then
         cycle
      else
         unusedUnitNumberFound = .true.
         exit
      endif
   else
      ! an error occurred
      write(6,'("ERROR: Could not determine which i/o unit numbers are unused. The Fortran i/o error code was ",i0,".")') errorIO
      stop
   endif
end do 
if (unusedUnitNumberFound.eqv..false.) then
   write(6,'("ERROR: Could not find an unused unit number.")')
   stop
endif
!-----------------------------------------------------------------------
end function availableUnitNumber
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     S U B R O U T I N E   C H E C K   F I L E   E X I S T E N C E
!-----------------------------------------------------------------------
!     jgf: Just check for the existence of a file. I separated this 
!     from openFileForRead so that I could use it on NetCDF files 
!     as well.  
!-----------------------------------------------------------------------
subroutine checkFileExistence(filename)
use logging
implicit none
character(*), intent(in) :: filename ! full pathname of file
logical :: fileFound    ! .true. if the file is present
errorIO = 0
!
! Check to see if file exists
write(scratchMessage,'("Searching for file ",a," ...")') trim(filename)
call allMessage(INFO,scratchMessage)
inquire(file=trim(filename),exist=fileFound,iostat=errorIO)
if (fileFound.eqv..false.) then
   write(scratchMessage,'("The file ",A," was not found.")') trim(filename)
   call allMessage(ERROR,scratchMessage)
else
   write(scratchMessage,'("The file ",A," was found.")') trim(filename)
   call allMessage(INFO,scratchMessage)
endif
!-----------------------------------------------------------------------
   END SUBROUTINE checkFileExistence
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     S U B R O U T I N E   O P E N  F I L E  F O R  R E A D
!-----------------------------------------------------------------------
!     jgf: Added general subroutine for opening an existing
!     file for reading. Includes error checking.
!-----------------------------------------------------------------------
subroutine openFileForRead(lun, filename)
use logging
implicit none
integer, intent(in) :: lun   ! fortran logical unit number
character(*), intent(in) :: filename ! full pathname of file
logical unitConnected !.true. if this lun is already being used
errorIO = 0
!
!  Check to see if file exists
call checkFileExistence(filename)
if (errorIO.ne.0) then
   return
endif
!
! Check to see if the unit number is already in use
inquire(unit=lun,opened=unitConnected)
if (unitConnected.eqv..true.) then
   write(scratchMessage,'("The i/o unit ",i0," is already connected.")') lun
   call allMessage(ERROR,scratchMessage)
   errorIO = 1
   return
endif
!
! Open existing file
OPEN(lun,FILE=trim(filename),STATUS='OLD',ACTION='READ',IOSTAT=errorIO)
if (errorIO.ne.0) then
   write(scratchMessage,'("Could not open the file ",A,".")') trim(filename)
   call allMessage(ERROR,scratchMessage) 
else
   write(scratchMessage,'("The file ",A," was opened successfully.")') trim(filename)
   call allMessage(INFO,scratchMessage)
endif
return
!-----------------------------------------------------------------------
   END SUBROUTINE openFileForRead
!-----------------------------------------------------------------------


!----------------------------------------------------------------------
!                  F U N C T I O N     I N D 
!----------------------------------------------------------------------
! returns a format string containing the right number of spaces for the
! specified indentation level
!----------------------------------------------------------------------
function ind(change)
implicit none
character(len=1), intent(in) :: change
character(len=4) :: ind
integer, save :: currentIndent = 0
!
select case(change)
case('+') ! increase indentation level
   currentIndent = currentIndent + 3
case('-') ! decrease indentation level
   currentIndent = currentIndent - 3
case default 
   ! keep indentation level the same
end select
write(ind,'(i3,"x")') currentIndent 
!----------------------------------------------------------------------
end function ind
!----------------------------------------------------------------------



!----------------------------------------------------------------------
!  CHECK
!---------------------------------------------------------------------
subroutine check(ncstatus)
use netcdf
implicit none
integer,intent(in) :: ncstatus
real(8), allocatable :: intentionalSegFault(:)
real(8) :: triggerSegFaultIntentionallyForStackTrace
if(ncstatus.ne.nf90_noerr)then
   write(*,'(a,a)') "ERROR: ",trim(nf90_strerror(ncstatus))

#ifdef DEBUGSEGFAULT
   triggerSegFaultIntentionallyForStackTrace = intentionalSegFault(1)
#endif

   stop 1
endif
!---------------------------------------------------------------------      
end subroutine check
!---------------------------------------------------------------------


!----------------------------------------------------------------------
!----------------------------------------------------------------------
end module asgsio
!----------------------------------------------------------------------
!----------------------------------------------------------------------




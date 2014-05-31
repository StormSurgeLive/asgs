!--------------------------------------------------------------------------
! asgsio.f90
!
! A module that provides helper subroutines for opening and reading 
! ADCIRC files in ascii and netcdf format. 
!--------------------------------------------------------------------------
! Copyright(C) 2014 Jason Fleming
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
implicit none
!
integer :: argcount
character(1024) :: cmdlineopt
character(1024) :: cmdlinearg
!
! file format integers
integer, parameter :: OFF = 0
integer, parameter :: ASCII = 1
integer, parameter :: SPARSE_ASCII = 4
integer, parameter :: NETCDF3 = 3
integer, parameter :: NETCDF4 = 5
integer, parameter :: XDMF = 7
integer, parameter :: NETCDFG = 35
integer, parameter :: ASCIIG = 14

integer :: nc_id ! netcdf id of file

!-----------
!-----------
contains
!-----------
!-----------

!-----------------------------------------------------------------------
!     S U B R O U T I N E   C H E C K   F I L E   E X I S T E N C E
!-----------------------------------------------------------------------
!     jgf: Just check for the existence of a file. I separated this 
!     from openFileForRead so that I could use it on NetCDF files 
!     as well.  
!-----------------------------------------------------------------------
SUBROUTINE checkFileExistence(filename, errorIO)
IMPLICIT NONE
CHARACTER(*), intent(in) :: filename ! full pathname of file
INTEGER, intent(out) :: errorIO  ! zero if the file opened successfully
LOGICAL :: fileFound    ! .true. if the file is present
errorIO = 0
!
!     Check to see if file exists
write(6,'("INFO: Searching for file ",A," ...")') trim(filename)
inquire(FILE=filename,EXIST=fileFound)
if (fileFound.eqv..false.) then
   write(6,'("ERROR: The file ",A," was not found.")') trim(filename)
else
   write(6,'("INFO: The file ",A," was found.")') trim(filename)
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
   SUBROUTINE openFileForRead(lun, filename)
      IMPLICIT NONE
      INTEGER, intent(in) :: lun   ! fortran logical unit number
      CHARACTER(*), intent(in) :: filename ! full pathname of file
      INTEGER :: errorIO  ! zero if the file opened successfully
       errorIO = 0
!
!     Check to see if file exists
      call checkFileExistence(filename, errorIO)
      if ( errorIO.ne.0) then
         stop
      endif
!
!     Open existing file
      OPEN(lun,FILE=trim(filename),STATUS='OLD',ACTION='READ',IOSTAT=errorIO)
      if (errorIO.ne.0) then
          write(6,'("ERROR: Could not open the file ",A,".")') trim(filename)
          stop
      else
         write(6,'("INFO: The file ",A," was opened successfully.")') trim(filename)
      endif
      return
!-----------------------------------------------------------------------
   END SUBROUTINE openFileForRead
!-----------------------------------------------------------------------

#ifdef ASGSNETCDF
!----------------------------------------------------------------------
!  CHECK
!---------------------------------------------------------------------
subroutine check(ncstatus)
use netcdf
implicit none
integer,intent(in) :: ncstatus
if(ncstatus.ne.nf90_noerr)then
   write(*,'(a,a)') "error: ",trim(nf90_strerror(ncstatus))
   stop 1
endif
!---------------------------------------------------------------------      
end subroutine check
!---------------------------------------------------------------------
#endif


!----------------------------------------------------------------------
!----------------------------------------------------------------------
end module asgsio
!----------------------------------------------------------------------
!----------------------------------------------------------------------




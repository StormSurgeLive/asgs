!--------------------------------------------------------------------------
! logging.f90
!--------------------------------------------------------------------------
!
! A module for writing log messages from fortran utility programs.
!
!--------------------------------------------------------------------------
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
!--------------------------------------------------------------------------
! Compile this program with the accompanying makefile.
!--------------------------------------------------------------------------
module logging
!
character(len=2048) :: logSource ! name of program doing the logging
!
! log file name, defaults to the name of the fortran program doing the
! writing appended with ".log"
character(len=2048) :: logFileName
integer :: loglun ! log file i/o unit number
logical :: loggingInitialized = .false. ! true if log files set up and logfile opened
!
! numbers representing log levels from least to most severe
integer, parameter :: DEBUG = -1
integer, parameter :: ECHO = 0
integer, parameter :: INFO = 1
integer, parameter :: WARNING = 2
integer, parameter :: ERROR = 3
!
! severity of messages to write to log file (this level and higher)
integer :: logLevel = INFO
!
! to hold character representation of logging levels
character(len=10) :: logLevelNames(-1:3)
!
character(len=2048) :: scratchMessage ! for forming log messages


contains

!--------------------------------------------------------------------
!     S U B R O U T I N E    S E T   L O G G I N G   L E V E L
!--------------------------------------------------------------------
! Set the level of severity for messages to be logged; log messages
! with a lower severity will be silently ignored.
!--------------------------------------------------------------------
subroutine setLoggingLevel(lvl)
implicit none
character(len=20), intent(in) :: lvl ! message severity
if (loggingInitialized.eqv..false.) then
   write(6,*) 'ERROR: Must call subroutine initLogging before using logging module.'
   stop
endif
select case(trim(lvl))
case('DEBUG','Debug','DeBug','debug')
   logLevel = DEBUG
case('ECHO','Echo','echo') 
   logLevel = ECHO
case('INFO','Info','info') 
   logLevel = INFO
case('WARNING','Warning','warning') 
   logLevel = WARNING   
case('ERROR','Error','error') 
   logLevel = ERROR
case default
   write(scratchMessage,'("Could not set logging level to ",a,".")') trim(lvl)
   call allMessage(ERROR,scratchMessage)
   stop
end select
!--------------------------------------------------------------------
end subroutine setLoggingLevel
!--------------------------------------------------------------------

!--------------------------------------------------------------------
!     S U B R O U T I N E    I N I T   L O G G I N G
!--------------------------------------------------------------------
!  Initialize the names for the logging levels and the counter
!  for the current subroutine.
!--------------------------------------------------------------------
subroutine initLogging(lun, progname,logname)
implicit none
character(*), intent(in) :: progname ! name of program writing log messages 
character(*), intent(in), optional :: logname ! to override default log name
integer :: lun ! i/o unit for log file, calling routine should use function availableUnitNumber to set this
integer :: errorIO ! positive if there was an i/o error; zero otherwise
logLevelNames(-1) = "DEBUG"
logLevelNames(0) = "ECHO"
logLevelNames(1) = "INFO"
logLevelNames(2) = "WARNING"
logLevelNames(3) = "ERROR"
logSource = trim(adjustl(progname))
if (present(logname)) then
   logFileName = trim(adjustl(logName))
else
   logFileName = trim(adjustl(logSource)) // '.log'
endif
! open the log file
loglun = lun
open(unit=loglun,file=trim(adjustl(logFileName)),action='write',status='replace',iostat=errorIO)
if (errorIO.gt.0) then
   write(6,'(a)') 'ERROR: Could not open log file for writing.'
   stop
endif
loggingInitialized = .true.
!--------------------------------------------------------------------
end subroutine initLogging
!--------------------------------------------------------------------

!--------------------------------------------------------------------
!     S U B R O U T I N E    S T D O U T 
!--------------------------------------------------------------------
!  Writes a log message to stdout (i.e., screen or console). 
!--------------------------------------------------------------------
subroutine stdout(level, message)
implicit none
integer, intent(in) :: level
character(*), intent(in) :: message
!
!if (loggingInitialized.eqv..false.) then
!   write(6,*) 'ERROR: Must call subroutine initLogging before using logging module.'
!   stop
!endif
if (level.ge.logLevel) then
   write(6,'(a,": ",a,": ",a)') trim(logLevelNames(level)),trim(adjustl(logSource)),trim(message)
endif
!--------------------------------------------------------------------
end subroutine stdout
!--------------------------------------------------------------------

!--------------------------------------------------------------------
!     S U B R O U T I N E    L O G   M E S S A G E
!--------------------------------------------------------------------
!  Writes a log message to log file. 
!--------------------------------------------------------------------
subroutine logMessage(level, message)
implicit none
integer, intent(in) :: level
character(*), intent(in) :: message
!
!if (loggingInitialized.eqv..false.) then
!   write(6,*) 'ERROR: Must call subroutine initLogging before using logging module.'
!   stop
!endif
if (level.ge.logLevel) then
   write(loglun,'(a,": ",a,": ",a)') trim(logLevelNames(level)),trim(adjustl(logSource)),trim(message)
endif
!--------------------------------------------------------------------
end subroutine logMessage
!--------------------------------------------------------------------


!--------------------------------------------------------------------
!     S U B R O U T I N E   A L L    M E S S A G E
!--------------------------------------------------------------------
!   Writes a message to log file and to the screen. 
!--------------------------------------------------------------------
subroutine allMessage(level, message)
implicit none
integer, intent(in) :: level
character(*), intent(in) :: message
!if (loggingInitialized.eqv..false.) then
!   write(6,*) 'ERROR: Must call subroutine initLogging before using logging module.'
!   stop
!endif
call stdout(level, message)
call logMessage(level, message)
!--------------------------------------------------------------------
end subroutine allMessage
!--------------------------------------------------------------------


end module logging

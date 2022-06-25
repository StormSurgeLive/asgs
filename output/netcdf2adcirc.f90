!--------------------------------------------------------------------------
! netcdf2adcirc.f90
!
! A program to convert adcirc files that are in netcdf format to
! adcirc ascii format.
!--------------------------------------------------------------------------
! Copyright(C) 2012--2017 Jason Fleming
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
! Compile with accompanying makefile.
!--------------------------------------------------------------------------
!
program netcdf2adcirc
use netcdf
use asgsio
use adcmesh
use logging
use ioutil
implicit none
logical :: meshonly
logical :: split   ! true if the time snaps should be written as separate files
integer :: ncstatus
type(mesh_t) :: m ! mesh to operate on
type(meshNetCDF_t) :: n ! mesh netcdf IDs
type(fileMetaData_t) :: fn ! netcdf file to be converted
character(len=1000) :: asciiFileName ! name of downstream ascii file
integer :: numAsciiFiles             ! number of downstream ascii files
integer :: snapsPerFile              ! number of data sets in each downstream ascii file
logical :: negative = .false. ! true if the data should be multiplied by -1
real(8) :: sense = 1.d0 ! -1.d0 if --negative was given on cmd line
integer :: snapi ! time step
real(8) :: snapr ! time (s)
integer :: lineNum
integer :: i
integer :: f     ! file counter
meshonly = .false.
split = .false.
call initLogging(availableUnitNumber(),'netcdf2adcirc.f90')
call allmessage(INFO,'Compiled with netcdf library version '//trim(nf90_inq_libvers())//'.')
!
! initializations
m%is3D = .false.
!
argcount = command_argument_count() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
         case("--meshonly")
            meshonly = .true.
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--split")
            split = .true.
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--sparse")
            fn%isSparse = .true.
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--negative")
            negative = .true.
            sense = -1.d0
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--datafile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ", &
               trim(cmdlinearg),"."
            fn%dataFileName = trim(cmdlinearg)
         case default
            write(6,'(a,a,a)') "WARNING: Command line option '", &
               TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
end if
!
! determine the number of snapshots in the file, type of data, etc etc
call determineNetCDFFileCharacteristics(fn, m, n)
if (fn%dataFileCategory.eq.OWI) then
   call allMessage(ERROR,'Unable to convert OWI files.')
   stop
endif
!
! allocate memory to hold single dataset from each netcdf variable
fn%dataFileFormat = NETCDFG
call allocateDataSetMemory(fn, m)
!
numAsciiFiles = 1
snapsPerFile = fn%nSnaps
if ( split.eqv..true. ) then
   numAsciiFiles = fn%nSnaps
   snapsPerFile = 1
endif
!
! loop over the number of output files
do f = 1,numAsciiFiles
   !
   ! open the ascii adcirc file that will hold the data and write header
   asciiFileName = fn%defaultFileName
   if ( split.eqv..true. ) then
      write(asciiFileName,'(i0.6,a,a)') f,'_',trim(fn%defaultFileName)
   endif
   fn%fun = availableUnitNumber()
   open(fn%fun,file=trim(asciiFileName),status='replace',action='write')
   ! write header info
   write(fn%fun,'(a)') trim(m%agrid)//' '//trim(rundes)//' '//trim(runid)
   if (m%is3D.eqv..true.) then
      ! 3D header
      write(fn%fun,1011) snapsPerFile, fn%numValuesPerDataSet, fn%time_increment, fn%nspool, m%nfen, fn%irtype
   else
      ! 2D header
      write(fn%fun,1010) snapsPerFile, fn%numValuesPerDataset, fn%time_increment, fn%nspool, fn%irtype
   endif
   !
   ! open the netcdf file
   call check(nf90_open(trim(fn%dataFileName), NF90_NOWRITE, fn%nc_id))
   !
   ! loop over netcdf datasets
   lineNum = 1
   do i=1,snapsPerFile
      !
      ! READ ONE COMPLETE DATASET FROM NETCDF
      !
      fn%dataFileFormat = NETCDFG
      call readOneDataset(fn, m, i, lineNum, snapr, snapi)
      !fn%
      !
      ! WRITE ONE COMPLETE DATASET TO ASCII
      !
      fn%dataFileFormat = ASCIIG
      call writeOneDataset(fn, m, i, lineNum, snapr, snapi)
      if ( split.eqv..false. ) then
         write(6,advance='no',fmt='(i6)') i
      endif

   end do
   if ( split.eqv..true. ) then
      close(fn%fun)
      write(6,advance='no',fmt='(i6)') f
   endif
end do
write(6,*)
call allMessage(INFO,'Finished writing file.')
write(scratchMessage,'(a,i0,a)') 'Wrote ',fn%nSnaps,' data sets.'
call allMessage(INFO,scratchMessage)
close(fn%fun)
call check(nf90_close(fn%nc_id))
!
 1010 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,'FileFmtVersion: ',I10)
 1011 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,I2,1X,'FileFmtVersion: ',I10)
!---------------------------------------------------------------------
end program netcdf2adcirc
!---------------------------------------------------------------------



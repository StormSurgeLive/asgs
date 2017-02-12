!--------------------------------------------------------------------------
! netcdf2adcirc.f90
!
! A program to convert adcirc files that are in netcdf format to
! adcirc ascii format.
!--------------------------------------------------------------------------
! Copyright(C) 2012--2015 Jason Fleming
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
integer :: ncstatus
type(fileMetaData_t) :: fn ! netcdf file to be converted
type(fileMetaData_t) :: fa ! ascii file to be created
integer :: nc_start(2)
integer :: nc_count(2)
integer :: nc_start3D(3)
integer :: nc_count3D(3)
real(8), allocatable :: adcirc_data(:,:)
real(8), allocatable :: adcirc_data3D(:,:,:)
integer, allocatable :: adcirc_idata(:)
integer :: i, j, k, m
meshonly = .false.
fa%isSparse = .false.
agrid = 'null'

write(6,'(a,a)') "INFO: adcirc2netcdf was compiled with the following " &
   // "netcdf library: ",trim(nf90_inq_libvers())

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
         case("--sparse")
            fa%isSparse = .true.
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
! determine the number of snapshots in the file
call determineNetCDFFileCharacteristics(fn)
!
write(6,'(a,i0,a)') 'INFO: The file contains ',fn%nSnaps,' datasets.'
write(6,'(a)') "INFO: Commence writing file ..."
!
! open the ascii adcirc file that will hold the data
fa%dataFileName = fn%dataFileType
fa%fun = availableUnitNumber()
open(fa%fun,file=trim(fa%dataFileName),status='replace',action='write')
! write header info
write(fa%fun,'(a)') trim(agrid)
!
nc_count = (/ np, 1 /)
!
! integer nodal data
! TODO: handle multicomponent integer data
if ( (fn%isInteger.eqv..true.).and.(trim(fn%dataCenter(1)).eq.'Node') ) then
   allocate(adcirc_idata(np))
   stop
endif
!
! allocate space to hold a single dataset
if (fn%isStationFile.eqv..true.) then
   fn%numValuesPerDataset = np
else   
   fn%numValuesPerDataset = fn%nStations
endif
allocate(adcirc_data(fn%numValuesPerDataset,fn%num_components))
!
! handle min/max files with time of occurrence   
if (fn%timeOfOccurrence.eqv..true.) then
   write(fa%fun,1010) fn%nSnaps, np, fn%time_increment, fn%nspool, 1
   write(fa%fun,2120) fn%timesec(1), fn%it(1)
   nc_start = (/ 1, 1 /)
   call check(nf90_get_var(fn%nc_id,fn%nc_varid(1),adcirc_data(:,1),nc_start,nc_count))
   do k=1,np
      write(fa%fun,2453) k,adcirc_data(k,1)
   end do
   ! time of occurrence data
   write(fa%fun,2120) fn%timesec(1), fn%it(1)
   nc_start = (/ 1, 2 /)      
   call check(nf90_get_var(fn%nc_id,fn%nc_varid(2),adcirc_data(:,2),nc_start,nc_count))
   do k=1,np
      write(fa%fun,2453) k,adcirc_data(k,2)
   end do
   stop
endif
!
! typical nodal or station data 
! TODO: handle elemental data 
write(fa%fun,1010) fn%nSnaps, fn%numValuesPerDataset, fn%time_increment, fn%nspool, fn%num_components
!
! loop over 2D datasets (either a time varying file or a min max file
! without time of occurrence information
if (fn%num_components.le.2) then
   do i=1,fn%nSnaps
      nc_start = (/ 1, i /)
      do j=1,fn%num_components
         ! read the dataset from netcdf
         call check(nf90_get_var(fn%nc_id,fn%nc_varid(j),adcirc_data(:,j),nc_start,nc_count))
      end do 
      ! write the dataset to ascii
      if ( fn%isSparse.eqv..true. ) then
         fn%numNodesNonDefault = count( adcirc_data(:,1).eq.fn%defaultValue )
         write(fa%fun,*) fn%timesec(i), fn%it(i), fn%numNodesNonDefault, fn%defaultValue
         do k=1,fn%numValuesPerDataset
            if ( adcirc_data(k,1).ne.fn%defaultValue) then
               write(fa%fun,*) k,(adcirc_data(k,j),j=1,fn%num_components)
            endif
         end do
      else
         ! nonsparse ascii output
         write(fa%fun,2120) fn%timesec(i), fn%it(i)
         do k=1,np
            write(fa%fun,2453) k,(adcirc_data(k,j),j=1,fn%num_components)
         end do
      endif
      write(6,advance='no',fmt='(i4)') i
   end do
   stop
endif
!
! loop over 3D datasets FIXME: this code is unfinished 
if (fn%num_components.eq.3) then
   deallocate(adcirc_data)
   call check(nf90_inq_dimid(fn%nc_id, "num_v_nodes", nc_dimid_vnode))
   call check(nf90_inquire_dimension(fn%nc_id, nc_dimid_vnode, len=nfen))
   nc_count3D = (/ np, nfen, 1 /)
   allocate(adcirc_data3D(np,nfen,fn%num_components))
   allocate(sigma(nfen))
   call check(nf90_inq_varid(fn%nc_id, "sigma", nc_varid_sigma))
   call check(nf90_get_var(fn%nc_id, nc_varid_sigma, sigma))
   write(11,1011) fn%nSnaps, np, fn%time_increment, fn%nspool, nfen, fn%num_components
   !
   ! loop over datasets   
   do i=1,fn%nSnaps
      !
      ! read 3D data from netcdf
      do j=1,fn%num_components
         nc_start3D = (/ 1, 1, i /)
         call check(nf90_get_var(fn%nc_id,fn%nc_varid(j),adcirc_data3D(:,:,j),nc_start3D,nc_count3D))   
      end do
      !
      ! write 3D data to ascii
      write(11,2121) fn%timesec(i), fn%it(i), (sigma(m),sigma(m),sigma(m),m=1,nfen-1),sigma(nfen),sigma(nfen)
      do k=1,np
         write(11,2454) k,(adcirc_data3D(k,j,1),adcirc_data3D(k,j,2),adcirc_data3D(k,j,3),j=1,nfen)
      end do
      write(6,advance='no',fmt='(i6)') i
   end do
endif

write(6,'(/,A)') "INFO: ... finished writing file."
write(6,'(a,i0,a)') "INFO: Wrote ",i-1," data sets."
close(11)
call check(nf90_close(fn%nc_id))
!
 1010 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,'FileFmtVersion: ',I10)
 1011 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,I2,1X,'FileFmtVersion: ',I10)
 2120 FORMAT(2X,1pE20.10E3,5X,I10)
 2121 FORMAT(2X,1pE20.10E3,5X,I10,99(1pE20.10E3,2X))
 2452 FORMAT(2x, i8, 2x, i0, 5x, i0, 5x, i0, 5x, i0)
 2453 FORMAT(2x, i8, 2x, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3)
 2454 FORMAT(2x, i8, 2x, 99(1pE20.10E3))
!---------------------------------------------------------------------
      end program netcdf2adcirc
!---------------------------------------------------------------------


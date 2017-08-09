!----------------------------------------------------------------------
! inundationMask.f90 : uses everdried.63.nc or initiallydry.63.nc 
! file to create an inundation mask for visualization.
!----------------------------------------------------------------------
! Copyright(C) 2016 Jason Fleming
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
!----------------------------------------------------------------------
program inundationMask
use adcmesh
use logging
use ioutil
use asgsio
use netcdf
implicit none
type(mesh_t) :: m
type(meshNetCDF_t) :: n
type(fileMetaData_t) :: im ! inundation mask file
type(fileMetaData_t) :: ed ! everdried or initially dry file
logical dataFound ! .true. if file contains either initiallydry or everdried data
integer, allocatable :: nnodecode(:) ! nodal wet/dry state, 0=dry, 1=wet
integer, allocatable :: newnodecode(:) ! nodal wet/dry state, 0=dry, 1=wet at end of expansion
integer :: numPasses ! number of times expand the mask to neighboring nodes 
integer :: i, j, k   ! loop counters
integer :: mask_start(1) ! array index to start writing inundation mask data
integer :: mask_count(1) ! number of inundation mask data to write
integer :: nc_varid_inundationmask
character(len=NF90_MAX_NAME) :: thisVarName
integer :: varid
integer, allocatable :: adcirc_idata(:,:)
real(8), allocatable :: adcirc_data(:,:)
integer :: nc_start(2)
integer :: nc_count(2)
integer :: imUnit
logical :: deflate
!
numPasses = 1
ed%dataFileName = 'null'
dataFound = .false.
im%dataFileFormat = NETCDF4
call initLogging(availableUnitNumber(),'inundationMask.f90')
!
! Report netcdf version
write(6,'(a,a)') 'INFO: inundationMask: compiled with the following netcdf library: ', trim(nf90_inq_libvers())

! Process command line options
argcount = iargc() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
         case("--filename")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(scratchMessage,'(a,a,a,a,a)') 'Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
            call allMessage(INFO,scratchMessage)
            ed%dataFileName = trim(cmdlinearg)
         case("--netcdf4")
             im%dataFileFormat = NETCDF4
             write(scratchMessage,'(a,a,a)') 'Processing "',trim(cmdlineopt),'".'            
            call allMessage(INFO,scratchMessage)
         case("--ascii")
             im%dataFileFormat = ASCII
             write(scratchMessage,'(a,a,a)') 'Processing "',trim(cmdlineopt),'".'
             call allMessage(INFO,scratchMessage)
         case("--numpasses")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(scratchMessage,'(a,a,a,a,a)') 'Processing ',trim(cmdlineopt),' ',trim(cmdlinearg),'.'
            call allMessage(INFO,scratchMessage)
            read(cmdlinearg,*) numPasses
         case default
            write(scratchMessage,'(a,a,a,a)') 'Command line option "',trim(cmdlineopt),'" was not recognized.'
            call allMessage(WARNING,scratchMessage)
      end select
   end do
end if
!
! quit if we don't have a filename to use    
if ( trim(ed%dataFileName).eq.'null' ) then
   write(scratchMessage,'(a,a,a,a)') 'Either an initiallydry.63.nc or everdried.63.nc file must be provided.'
   call allMessage(ERROR,scratchMessage)
   stop
else
   m%meshFileName = trim(ed%dataFileName)
endif
!
! open the file and read the mesh
call findMeshDimsNetCDF(m, n)
call readMeshNetCDF(m, n)
!jgfdebug write the element table as a check
!do i=1,10
!   write(6,'(i0,1x,i0,1x,i0,1x,i0)') i, (nmnc(j,i), j=1,3)
!end do
!stop
!
! compute the neighbor table
call computeNeighborTable(m)
!
! allocate the nnodecode array (nodal wet/dry state, 0=dry 1=wet)
allocate(nnodecode(m%np))
! initialize to all wet
nnodecode=1  
!
! load the wet/dry states of the nodes
!
call determineNetCDFFileCharacteristics(ed, m, n)
dataFound = .false.
! open the netcdf file so that we can read the data 
call check(nf90_open(trim(ed%dataFileName), NF90_NOWRITE, ed%nc_id))
! determine file type and load data 
do i=1,ed%nvar
   call check(nf90_inquire_variable(ed%nc_id, i, thisVarName))
   select case(trim(thisVarName))   
   case("initiallydry")
      write(scratchMessage,'(a)') 'Creating an inundation mask from an ADCIRC initially dry file.'
      call allMessage(INFO,scratchMessage)
      call check(nf90_inq_varid(ed%nc_id, thisVarName, varid))
      allocate(adcirc_idata(m%np,1))
      nc_start = (/ 1, 1 /)
      nc_count = (/ m%np, 1 /)      
      call check(nf90_get_var(ed%nc_id,varid,adcirc_idata(:,1),nc_start,nc_count))      
      do j=1,m%np
         if ( adcirc_idata(j,1).eq.1 ) then
            nnodecode(j) = 0
         endif
      end do
      dataFound = .true.
      deallocate(adcirc_idata)
      exit      
   case("everdried")
      write(scratchMessage,'(a)') 'Creating an inundation mask from an ADCIRC ever dried file.'
      call allMessage(INFO,scratchMessage)
      call check(nf90_inq_varid(ed%nc_id, thisVarName, varid))
      allocate(adcirc_data(m%np,1))
      nc_start = (/ 1, 1 /)
      nc_count = (/ m%np, 1 /)      
      call check(nf90_get_var(ed%nc_id,varid,adcirc_data(:,1),nc_start,nc_count))      
      do j=1,m%np
         if ( adcirc_data(j,1)-(-99999d0).lt.1.e-6 ) then
            nnodecode(j) = 0
         endif
      end do
      dataFound = .true.
      deallocate(adcirc_data)
      exit                 
   case default
      ! did not recognize this variable name
      cycle
   end select
end do
!
! make sure this is one of the files we know how to process
if ( dataFound.eqv..false. ) then
   write(scratchMessage,'(a,a,a,a)') 'Either an initiallydry.63.nc or everdried.63.nc file must be provided.'
   call allMessage(ERROR,scratchMessage)
   stop
endif
call check(nf90_close(ed%nc_id))
!
! now that the neighbor table and the data are loaded, expand the 
! mask by a single layer of elements for each of the passes specified
! in the command line options
allocate(newnodecode(m%np))
do i=1,numPasses
   write(scratchMessage,'(a,i0,a)') 'Pass number ',i,'.'
   call allMessage(INFO,scratchMessage)
   ! set the new value equal to the existing value
   newnodecode = nnodecode
   do j=1,m%np
      ! if node j is dry
      if ( nnodecode(j).eq.0 ) then    
         ! make all its neighbors dry too
         do k=2,m%nNeigh(j)
            newnodecode(m%neitab(j,k)) = 0
         end do
      end if
   end do
   ! set the existing value to the new set of values
   nnodecode = newnodecode
end do
!
! write the file in the specified format
deflate = .true.
#ifndef HAVE_NETCDF4
   deflate = .false.
   if (im%dataFileFormat.eq.NETCDF4) then
      write(scratchMessage,'(a)') 'This executable only supports NetCDF3, so the mask file will be written in NetCDF3 format.'
      call allMessage(WARNING,scratchMessage)
      im%dataFileFormat = NETCDF3
   endif
#endif
select case(im%dataFileFormat)
case(ASCII)
   write(scratchMessage,'(a,a,a)') 'Creating ascii file "inundationmask.63".'
   call allMessage(INFO,scratchMessage)
   ! write the inundation mask file with 0=dry 1=wet
   imUnit = availableUnitNumber()
   open(imUnit,file='inundationmask.63',status='replace',action='write')
   ! write header info
   write(imUnit,'(a)') trim(m%agrid)
   write(imUnit,1010) 1, m%np, -99999.0, -99999, 1 ! ndset, np, time_increment, nspool, itype
   write(imUnit,2120) -99999.0, -99999
   do i=1,m%np
      write(imUnit,2452) i, nnodecode(i)
   end do
   close(imUnit)
   1010 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,'FileFmtVersion: ',I10)
   2120 FORMAT(2X,1pE20.10E3,5X,I10)
   2452 FORMAT(2x, i8, 2x, i0, 5x, i0, 5x, i0, 5x, i0)
case(NETCDF3,NETCDF4)
   write(scratchMessage,'(a,a,a)') 'Creating NetCDF file "inundationmask.63.nc".'
   call allMessage(INFO, scratchMessage)
   im%ncFileType = ior(NF90_HDF5,NF90_CLASSIC_MODEL) ! netcdf4 (i.e., hdf5) format, netcdf
   if (im%dataFileFormat.eq.NETCDF3) then
      im%ncFileType = NF90_CLOBBER ! netcdf3 format, netcdf classic model
   endif
   call check(nf90_create('inundationmask.63.nc',im%ncFileType,im%NC_ID))
   call writeMeshDefinitionsToNetCDF(m, n, im%NC_ID, deflate)
   call check(nf90_def_var(im%nc_id,'inundationmask',nf90_int,n%nc_dimid_node,nc_varid_inundationmask))
   call check(nf90_put_att(im%nc_id,nc_varid_inundationmask,'_FillValue',-99999))
   call check(nf90_put_att(im%nc_id,nc_varid_inundationmask,'long_name','inundation mask for visualization purposes'))
   call check(nf90_put_att(im%nc_id,nc_varid_inundationmask,'standard_name','inundation_mask'))
   call check(nf90_put_att(im%nc_id,nc_varid_inundationmask,'coordinates','y x'))
   call check(nf90_put_att(im%nc_id,nc_varid_inundationmask,'location','node'))
   call check(nf90_put_att(im%nc_id,nc_varid_inundationmask,'mesh','adcirc_mesh'))
   call check(nf90_put_att(im%nc_id,nc_varid_inundationmask,'units','unitless'))
#ifdef NETCDF_CAN_DEFLATE
      if (im%dataFileFormat.eq.NETCDF4) then
         call check(nf90_def_var_deflate(im%NC_ID, nc_varid_inundationmask, 1, 1, 2))
      endif
#endif
   call check(nf90_enddef(im%nc_id))
   call writeMeshDataToNetCDF(m, n, im%nc_id)
   mask_start = (/ 1 /)
   mask_count = (/ m%np /)
   ! write the dataset to the netcdf file
   call check(nf90_put_var(im%nc_id,nc_varid_inundationmask,nnodecode,nc_start,nc_count))
   case default
      ! this should be impossible to reach
      write(scratchMessage,'(a)') 'Invalid file format specification.'
      call allMessage(ERROR,scratchMessage)
end select

!----------------------------------------------------------------------
end program inundationMask
!----------------------------------------------------------------------

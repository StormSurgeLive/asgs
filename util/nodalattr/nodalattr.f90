!-----------------------------------------------------------------------
!               M O D U L E    N O D A L   A T T R 
!-----------------------------------------------------------------------
! jgf: Module for handling nodal attribute data.
!-----------------------------------------------------------------------
module nodalattr
!-----------------------------------------------------------------------
!
character(len=1024) :: nodalAttributesFile    ! name of the file
character(len=1024) :: nodalAttributesComment ! comment line at the top
integer :: numMeshNodes ! expected to be the same as np
integer :: numNodalAttributes ! number of nodal attributes in the file
integer :: numLoadedAttributes ! number that we've loaded into memory
!
type nodalAttr_t
   character(len=1024) :: attrName ! name of the nodal attr 
   character(len=1024) :: units    ! physical units of the nodal attr
   integer :: numVals  ! number of values at each node for this nodal attr
   integer :: numNodesNotDefault ! number of nodes with values different from the default
   real(8), allocatable :: fillValue(:)  ! missing data value for each nodal attr 
   real(8), allocatable :: defaultVals(:) ! default value(s) for real valued attributes 
   real(8), allocatable :: nonDefaultVals(:,:) ! nondefault vals (numVals x numNodesNotDefault) 
   integer, allocatable :: nonDefaultNodes(:) ! node numbers where nondefault vals occur
   ! xdmf related
   real(8), allocatable :: xdmfArray(:)
   real(8), allocatable :: xdmfMatrix(:,:)
   ! netcdf related
   integer :: nc_dimid(2) ! dimensions of the full dataset (numNodes x numVals)
   integer :: nc_dimid_values_per_node ! dimensions of default values
   integer :: nc_varid    ! full nodal attribute variable id (numNodes x numVals)
   integer :: nc_varid_defaults ! just default values
   real(8), allocatable :: ncData(:,:)  ! full array of values in netcdf file
   
end type nodalAttr_t
! variable capable of holding all nodal attributes in the file
type(nodalAttr_t), allocatable :: na(:)
! a second variable for holding a modified attribute
type(nodalAttr_t), allocatable :: altNA(:)
!
!---------
contains
!---------
!
!-----------------------------------------------------------------------
!  S U B R O U T I N E    L O A D   N O D A L    A T T R I B U T E 
!-----------------------------------------------------------------------
! jgf: Loads up a single nodal attribute from an ascii nodal attributes 
! file. 
!-----------------------------------------------------------------------
subroutine loadNodalAttribute(naName)
use ioutil, only : openFileForRead
implicit none
character(len=1024), intent(in) :: naName   ! name of the nodal attribute
character(len=80) :: line    ! throwaway line
logical :: foundIt  ! .true. if the specified nodal attribute is in the file
integer :: nondef ! number of non default values to be skipped
integer :: errorIO
integer :: i, j, k
!
numLoadedAttributes = 1
allocate(na(1)) ! only allocate for the one we want
na(1)%attrName = trim(adjustl(naName))
!
foundIt = .false.
write(6,*) 'INFO: Reading nodal attribute.'
call openFileForRead(13,nodalAttributesFile, errorIO)
read(13,*) nodalAttributesComment
read(13,*) numMeshNodes
write(6,'("INFO: There are ",i0," nodes in the corresponding mesh.")') numMeshNodes 
read(13,*) numNodalAttributes
write(6,'("INFO: There are ",i0," nodal attributes in the file.")') numNodalAttributes 
! read remainder of header
do i=1,numNodalAttributes
   read(13,*) line
   ! see if this is the one we are interested in, and if so, load 
   ! up the relevant data; otherwise, skip past it
   if (trim(adjustl(line)).eq.trim(adjustl(naName))) then
      foundIt = .true.
      read(13,*) na(1)%units
      read(13,*) na(1)%numVals
      ! allocate memory for the default values and read them
      allocate(na(1)%defaultVals(na(1)%numVals))
      read(13,*) (na(1)%defaultVals(j), j=1,na(1)%numVals)
   else
      read(13,*) line ! units (skip it)
      read(13,*) line ! numVals (skip it)
      read(13,*) line ! default val (skip it)     
   endif
end do
if (foundIt.eqv..false.) then
   write(6,*) 'ERROR: The nodal attribute "',trim(naName),'" was not found in the file "',trim(nodalAttributesFile),'".'
   close(13)
   error stop 1 
endif
! finished reading header
!
! now read the body of the nodal attributes file to find the specified
! n.a. data
do i=1,numNodalAttributes
   read(13,*) line
   ! see if this is the one we are interested in, and if so, load 
   ! up the relevant data; otherwise, skip past it
   if (trim(adjustl(line)).eq.trim(adjustl(naName))) then
      read(13,*) na(1)%numNodesNotDefault
      write(6,'("INFO: There are ",i0," nodes with non-default values.")') na(1)%numNodesNotDefault 
      ! allocate memory for the node numbers
      allocate(na(1)%nonDefaultNodes(na(1)%numNodesNotDefault))
      ! allocate memory for the floating point data and then read it
      allocate(na(1)%nonDefaultVals(na(1)%numVals,na(1)%numNodesNotDefault))
      do j=1,na(1)%numNodesNotDefault
         read(13,*) na(1)%nonDefaultNodes(j), (na(1)%nonDefaultVals(k,j), k=1,na(1)%numVals)
      end do
      exit
   else
      ! this is not the specified nodal attribute; skip this data
      read(13,*) nondef ! num non default vals, skip it
      do j=1,nondef
         read(13,*) line ! non default vals, skip them
      end do
   endif
end do
close(13)
write(6,*) 'INFO: Finished reading nodal attribute data.'
!-----------------------------------------------------------------------
end subroutine loadNodalAttribute
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                   S U B R O U T I N E   
!   S E T   N O D A L   A T T R I B U T E S   F I L E   N A M E  
!-----------------------------------------------------------------------
subroutine setNodalAttributesFileName(asciiFile)
implicit none
character(len=1024), intent(in) :: asciiFile
nodalAttributesFile = trim(asciiFile)
!-----------------------------------------------------------------------
end subroutine setNodalAttributesFileName
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                    S U B R O U T I N E    
!    R E A D   N O D A L   A T T R I B U T E S   F I L E 
!-----------------------------------------------------------------------
! jgf: Reads all the nodal attribute data from an ascii adcirc nodal
! attributes file. 
!-----------------------------------------------------------------------
subroutine readNodalAttributesFile(asciiFile)
use ioutil, only : openFileForRead
implicit none
character(len=1024), intent(in) :: asciiFile  ! name of the nodal attributes file
character(len=1024) :: line
integer :: w ! array index of the nodal attribute we are to write
integer :: i, j, k, m
integer :: naIndex
logical :: foundIt
integer :: errorIO
!
call setNodalAttributesFileName(asciiFile)

write(6,'(a)') 'INFO: Reading nodal attributes from "' // trim(nodalAttributesFile) // '".'
call openFileForRead(13,nodalAttributesFile, errorIO)
read(13,'(a1024)') nodalAttributesComment
read(13,*) numMeshNodes
write(6,'("INFO: There are ",i0," nodes in the corresponding mesh.")') numMeshNodes 
read(13,*) numNodalAttributes
write(6,'("INFO: There are ",i0," nodal attributes in the file.")') numNodalAttributes 
allocate(na(numNodalAttributes))
! read remainder of header
do i=1,numNodalAttributes
   read(13,*) line
   na(i)%attrName = trim(adjustl(line))
   read(13,*) na(i)%units
   read(13,*) na(i)%numVals
   ! allocate memory for the default values and read them
   allocate(na(i)%defaultVals(na(i)%numVals))
   read(13,*) (na(i)%defaultVals(j), j=1,na(i)%numVals)
end do
!
! finished reading header
!
! now read the body of the nodal attributes file
do i=1,numNodalAttributes
   read(13,*) line
   ! the nodal attributes can be listed in the body of the ascii file in
   ! a different order than they were provided in the header; so we need
   ! to first figure out which of the attributes in the header correspond
   ! to these data 
   foundIt = .false.
   do j=1,numNodalAttributes
      if (trim(adjustl(line)).eq.trim(adjustl(na(j)%attrName))) then
         naIndex = j
         foundIt = .true.
         exit
      endif
   end do
   if (foundIt.eqv..false.) then
      !write(6,'(a)') 'ERROR: The nodal attribute ',trim(adjustl(line)), &
      !' was found in the body of the file but not the header. This file is not ' &
      !'properly formatted.'
      stop 
   endif
   ! read the number of nondefault nodes
   read(13,*) na(naIndex)%numNodesNotDefault
   ! allocate memory for the node numbers of the non default nodes
   allocate(na(naIndex)%nonDefaultNodes(na(naIndex)%numNodesNotDefault))
   ! allocate memory for the floating point data and then read it
   allocate(na(naIndex)%nonDefaultVals(na(naIndex)%numVals,na(naIndex)%numNodesNotDefault))
   do j=1,na(naIndex)%numNodesNotDefault
      read(13,*) na(naIndex)%nonDefaultNodes(j), &
         (na(naIndex)%nonDefaultVals(k,j), k=1,na(naIndex)%numVals)
   end do
end do
close(13)
write(6,'(a)') 'INFO: Finished reading nodal attribute data.'
!-----------------------------------------------------------------------
end subroutine readNodalAttributesFile
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                    S U B R O U T I N E    
!    W R I T E   N O D A L   A T T R I B U T E S   F I L E 
!-----------------------------------------------------------------------
! jgf: Writes all the nodal attribute data to an ascii adcirc nodal
! attributes file. 
!-----------------------------------------------------------------------
subroutine writeNodalAttributesFile(newFileName)
implicit none
character(len=1024), intent(in) :: newFileName  ! name of the nodal attributes file to write
integer :: i, j, k, m
!
write(6,'(a)') 'INFO: Writing nodal attributes to "'//trim(newFileName)//'".'
open(unit=13,file=trim(adjustl(newFileName)),status='new',action='write')
write(13,'(a)') trim(adjustl(nodalAttributesComment))
write(13,'(i0)') numMeshNodes
write(6,'("INFO: There are ",i0," nodes in the corresponding mesh.")') numMeshNodes 
write(13,'(i0)') numNodalAttributes
write(6,'("INFO: There are ",i0," nodal attributes in the file.")') numNodalAttributes 
! write remainder of header
do i=1,numNodalAttributes
   write(13,'(a)') trim(adjustl(na(i)%attrName))
   write(13,'(a)') trim(adjustl(na(i)%units))
   write(13,'(99(i0))') na(i)%numVals
   write(13,'(99(F15.7))') (na(i)%defaultVals(j), j=1,na(i)%numVals)
end do
!
! finished writing header
!
! now write the body of the nodal attributes file
do i=1,numNodalAttributes
   write(13,'(a)') trim(adjustl(na(i)%attrName))
   ! write the number of nondefault nodes
   write(13,'(i0)') na(i)%numNodesNotDefault
   ! write the node numbers and values for nodes whose value is not the default
   do j=1,na(i)%numNodesNotDefault
      write(13,'(i0,1x,99(F15.7))') na(i)%nonDefaultNodes(j), &
         (na(i)%nonDefaultVals(k,j), k=1,na(i)%numVals)
   end do
end do
close(13)
write(6,'(a)') 'INFO: Finished writing nodal attribute data.'
!-----------------------------------------------------------------------
end subroutine writeNodalAttributesFile
!-----------------------------------------------------------------------
!


!-----------------------------------------------------------------------
!  S U B R O U T I N E    W R I T E   N O D A L   A T T R I B U T E  6 3
!-----------------------------------------------------------------------
! jgf: Writes a single nodal attribute in the ascii adcirc fort.63 format
!-----------------------------------------------------------------------
subroutine writeNodalAttribute63(naName,fort63)
implicit none
character(len=1024), intent(in) :: naName   ! name of the nodal attribute
character(len=1024), intent(in) :: fort63   ! name of the file to write to
logical haveNodalAttribute ! .true. if we've loaded the one that we're writing
integer :: w ! array index of the nodal attribute we are to write
integer :: i, j, m
!
! determine which nodal attribute corresponds to the one being requested
w = 0
haveNodalAttribute = .false.
do i=1, numLoadedAttributes
   if (trim(na(i)%attrName).eq.trim(adjustl(naName))) then
      haveNodalAttribute = .true.
      w = i
      exit
   endif
end do
!
write(6,'(a)') 'INFO: Writing nodal attribute.'
open(63,file=trim(fort63),status='replace',action='write')
! RUNDES, RUNID, AGRID
write(63,*) trim(nodalAttributesComment) // " " // trim(naName) 
! NDSETSE, NP, DTDP*NSPOOLGE, NSPOOLGE, IRTYPE
write(63,'(i0," ",i0," ",f3.1," ",i0," ",i0)') na(w)%numVals, numMeshNodes, 1.0, 1, 1
do i=1,na(w)%numVals
   ! TIME, IT
   write(63,'(f4.1," ",i0)') 1.0*i, i
   m=1
   do j=1,numMeshNodes
      ! check to see if this node has a non default value
      if (j.eq.na(w)%nonDefaultNodes(m)) then        
         !write(*,*) 'node ',j,' has nondefault value ',na(w)%nonDefaultVals(1,m)
         write(63,'(i0,99(" ",f15.7))') j, na(w)%nonDefaultVals(i,m)
         if (m.lt.na(w)%numNodesNotDefault) then
            m = m + 1
         endif
      else
         ! if it doesn't then write the default value
         !write(*,*) 'node ',j,' has default value ',na(w)%defaultVals(1)
          write(63,'(i0,99(" ",f15.7))') j, na(w)%defaultVals(i)
      endif
   end do
end do
close(63)
write(6,'(a)') 'INFO: Finished writing nodal attribute data.'
!-----------------------------------------------------------------------
end subroutine writeNodalAttribute63
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!                    S U B R O U T I N E    
!   W R I T E   N O D A L   A T T R I B U T E S   F I L E   N E T C D F 
!-----------------------------------------------------------------------
! jgf: Writes all the nodal attribute data to a netcdf file.
!-----------------------------------------------------------------------
subroutine writeNodalAttributesFileNetCDF(ncid, deflate)
use netcdf
use adcmesh
use ioutil, only : check
implicit none
integer, intent(in) :: ncid ! netcdf id of the file to write
logical, intent(in) :: deflate ! turns on compression if compiled w/suitable libs 
integer :: nc_start(2) ! element of array where writing begins (each dimension)
integer :: nc_count(2) ! number of elements of array to write (each dimension)
character(len=2048) :: nameStr
integer :: i, j, k, m
!
write(6,'(a)') 'INFO: Writing nodal attributes to netCDF.'
call check(nf90_put_att(ncid,nf90_global,'nodalAttributesComment',trim(adjustl(nodalAttributesComment))))
write(6,'("INFO: There are ",i0," nodes in the corresponding mesh.")') np 
write(6,'("INFO: There are ",i0," nodal attributes in the file.")') numNodalAttributes 
!
! define dimensions, variables, and metadata for each nodal attribute
do i=1,numNodalAttributes
   ! number of values per node : dimension
   nameStr = trim(adjustl(na(i)%attrName))//'_valuesPerNode'
   call check(nf90_def_dim(ncid,trim(nameStr),na(i)%numVals,na(i)%nc_dimid_values_per_node))
   ! default value(s) : variable definition
   nameStr = trim(adjustl(na(i)%attrName))//'_defaultValues'
   call check(nf90_def_var(ncid,trim(nameStr),nf90_double,na(i)%nc_dimid_values_per_node,na(i)%nc_varid_defaults))
   ! nodal values : dimensions
   na(i)%nc_dimid(1) = nc_dimid_node
   na(i)%nc_dimid(2) = na(i)%nc_dimid_values_per_node  
   ! nodal values : variable definition   
   call check(nf90_def_var(ncid,trim(adjustl(na(i)%attrName)),nf90_double,na(i)%nc_dimid,na(i)%nc_varid))
   !
   ! netcdf metadata for each nodal attribute
   call check(nf90_put_att(ncid,na(i)%nc_varid,'_FillValue',na(i)%fillvalue))
   call check(nf90_put_att(ncid,na(i)%nc_varid,'long_name',trim(adjustl(na(i)%attrName))))
   call check(nf90_put_att(ncid,na(i)%nc_varid,'standard_name',trim(adjustl(na(i)%attrName))))
   call check(nf90_put_att(ncid,na(i)%nc_varid,'coordinates','y x'))
   call check(nf90_put_att(ncid,na(i)%nc_varid,'location','node'))
   call check(nf90_put_att(ncid,na(i)%nc_varid,'mesh','adcirc_mesh'))
   call check(nf90_put_att(ncid,na(i)%nc_varid,'units',trim(adjustl(na(i)%units))))
   call check(nf90_put_att(ncid,na(i)%nc_varid,'valuesPerNode',na(i)%numVals))

#ifdef NETCDF_CAN_DEFLATE
   if (deflate.eqv..true.) then
      call check(nf90_def_var_deflate(ncid, na(i)%nc_varid, 1, 1, 2))
   endif
#endif

end do
! end definitions mode of netcdf
call check(nf90_enddef(ncid))
! write mesh data to netcdf; mesh definitions were written by the calling routine
call writeMeshDataToNetCDF(ncid)
!
! now populate default value(s) and value(s) at each node
do i=1,numNodalAttributes
   ! default values
   call check(nf90_put_var(ncid,na(i)%nc_varid_defaults,na(i)%defaultVals,(/ 1 /),(/ na(i)%numVals /) ))
   !
   ! nodal attribute values
   allocate(na(i)%ncData(np, na(i)%numVals))
   ! set default values throughout
   do j=1,np
      na(i)%ncData(j,:) = na(i)%defaultVals(:)
   end do
   ! selectively set nondefault values    
   do j=1,na(i)%numNodesNotDefault
      do k=1,na(i)%numVals
         na(i)%ncData( na(i)%nonDefaultNodes(j),k ) = na(i)%nonDefaultVals(k,j) 
      end do
   end do
   nc_start = (/ 1, 1 /)
   nc_count = (/ np, na(i)%numVals /)
   ! write nodal values to netcdf
   call check(nf90_put_var(ncid,na(i)%nc_varid,na(i)%ncData,nc_start,nc_count))
end do
call check(nf90_close(ncid))
write(6,'(a)') 'INFO: Finished writing nodal attribute data to netcdf.'
!-----------------------------------------------------------------------
end subroutine writeNodalAttributesFileNetCDF
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
end module nodalattr
!-----------------------------------------------------------------------

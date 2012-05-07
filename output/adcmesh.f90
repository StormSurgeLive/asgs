!-----+---------+---------+---------+---------+---------+---------+
!
! adcmesh.f90
! This is a module for storing and manipulating data for ADCIRC meshes;
! it is based on code written by Corbitt Kerr.
!
!-----+---------+---------+---------+---------+---------+---------+
   module adcmesh
!-----+---------+---------+---------+---------+---------+---------+
      character(120) :: meshFileName ! full pathname of file
      real, parameter :: R = 6378206.4 ! radius of the earth
      real, parameter :: pi = 3.141592653589793
      real :: deg2rad

      double precision, allocatable :: xyd(:,:), bar(:,:,:)
      real(8), allocatable          :: sigma(:)
      character(120)                :: agrid
      integer                       :: ne, np
      integer                       :: nfen
      integer                       :: neta_count ! count of open boundary nodes
      integer                       :: nvel_count ! count of land boundary nodes
      integer                       :: nope, neta, nvdl_max
      integer                       :: nbou, nvel, nvel_max, nodemax
      integer,          allocatable :: nm(:,:), nvdll(:), nbdv(:,:), nsequencer(:)
      integer,          allocatable :: nvell(:), ibtype(:),  nbvv(:,:), ibconn(:,:)
      !
      integer                       :: NC_DimID_node
      integer                       :: NC_DimID_vnode
      integer                       :: NC_DimID_nele
      integer                       :: NC_DimID_nvertex
      integer                       :: NC_DimID_nope
      integer                       :: NC_DimID_max_nvdll
      integer                       :: NC_DimID_nbou
      integer                       :: NC_DimID_neta
      integer                       :: NC_DimID_nvel
      integer                       :: NC_DimID_max_nvell
      !
      integer                       :: NC_VarID_Mesh
      integer                       :: NC_VarID_x
      integer                       :: NC_VarID_y
      integer                       :: NC_VarID_sigma
      integer                       :: NC_VarID_element
      integer                       :: NC_VarID_neta
      integer                       :: NC_VarID_nvdll
      integer                       :: NC_VarID_max_nvdll
      integer                       :: NC_VarID_ibtypee
      integer                       :: NC_VarID_nbdv
      integer                       :: NC_VarID_nvel
      integer                       :: NC_VarID_nvell
      integer                       :: NC_VarID_max_nvell
      integer                       :: NC_VarID_ibtype
      integer                       :: NC_VarID_nbvv
      integer                       :: NC_VarID_depth

      logical                       :: projectCPP ! .true. if user wants to project mesh coordinates with CPP to aid in visualization
      logical                       :: cppUpdated ! .true. if we've already computed/written CPP on this execution
      real                          :: slam0  ! longitude on which cpp projection is centered
      real                          :: sfea0  ! latitude on which cpp projection is centered

   contains

   !-----+---------+---------+---------+---------+---------+---------+
   !  READ14_ALLOC
   !-----+---------+---------+---------+---------+---------+---------+
   subroutine read14_alloc ()
      implicit none
      integer :: i, j, k
      integer, parameter :: iunit = 14
!
      write(6,*) "INFO: Reading mesh file dimensions."
      nvdl_max = 0
      nvel_max = 0
      nodemax = 0
      read(iunit,*)
      read(iunit,*) ne, np
      do k = 1, np
         read(iunit,*) i
         nodemax = max(i,nodemax)
      enddo
      do k = 1, ne
         read(iunit,*)
      enddo
      write(6,*) '  |'
      read(iunit,*) nope
      read(iunit,*) neta ! total number of open boundary nodes
      neta_count = 0
      do k = 1, nope
         read(iunit,*) i ! number of nodes on the kth open boundary segment
         if( i >= nvdl_max ) nvdl_max = i
         do j = 1, i
            read(iunit,*)
            neta_count = neta_count + 1
         enddo
      enddo
      if ( neta_count.ne.neta ) then
         write(6,'("WARNING: Number of open boundary nodes was set to ",I6," but ",I6," were found.")') neta, neta_count
      endif
      read(iunit,*) nbou
      !write(6,'("DEBUG: There are ",I6," land boundary segments in the file.")') nbou
      read(iunit,*) nvel ! total number of land boundary nodes
      !write(6,'("DEBUG: There are ",I6," land boundary nodes in the file.")') nvel
      nvel_count = 0
      do k = 1, nbou
         read(iunit,*) i  ! number of nodes on the kth land boundary segment
         !write(6,'("DEBUG: There are ",I6," land boundary nodes in segment ",I6,".")') i, k
         if( i >= nvel_max ) nvel_max = i
         do j = 1, i
            read(iunit,*)
            nvel_count = nvel_count + 1
         enddo
      enddo
      if ( nvel_count.ne.nvel) then
         !write(6,'("WARNING: Number of land boundary nodes was set to ",I6," but ",I6," were found.")') nvel, nvel_count
      endif

      rewind(iunit)
      write(6,*) "INFO: Finished reading mesh file dimensions."
   !-----+---------+---------+---------+---------+---------+---------+
   end subroutine read14_alloc
   !-----+---------+---------+---------+---------+---------+---------+

   !-----+---------+---------+---------+---------+---------+---------+
   ! READ14
   !-----+---------+---------+---------+---------+---------+---------+
   subroutine read14 ()
      implicit none

      integer :: i, j, k, jn, je, nhy
      integer, parameter :: iunit = 14

      if (trim(meshFileName).eq."null") then
         write(6,*)    '*************************************************'
         WRITE(6,'(A)',ADVANCE='NO') "Enter name of the fort.14 file: "
         read(5,'(A)') meshFileName
      endif
      call openFileForRead(iunit,trim(meshFileName))
      call read14_alloc ()

      allocate( xyd(3,np) )
      allocate( nm(3,ne) )
      allocate( nvdll(nope)  )
      allocate( nbdv(nope,nvdl_max) )
      allocate( nvell(nbou), ibtype(nbou)  )
      allocate( nbvv(nbou,nvel_max), ibconn(nbou,nvel_max), bar(3,nbou,nvel_max) )
      allocate( nsequencer(nodemax) )

      nsequencer(:) = 0
      bar(:,:,:) = 0.0d0
      ibconn(:,:) = 0
      agrid = ' '
      write(6,*) "INFO: Reading mesh file coordinates, connectivity, and boundary data."
      read(iunit,*) agrid
      read(iunit,*) ne, np
      do k = 1, np
         read(iunit,*) jn, (xyd(j,k), j=1,3)
         nsequencer(jn) = k
      enddo
      write(6,*) '  + '
      do k = 1, ne
         read(iunit,*) je, nhy, ( nm(j,k), j = 1, 3 )
         do j = 1, 3
            if( nm(j,k) <= 0 ) write(6,*) k,j, nm(j,k)
            nm(j,k) = nsequencer(nm(j,k))
         enddo
      enddo
      read(iunit,*) nope
      read(iunit,*) neta
      do k = 1, nope
         read(iunit,*) nvdll(k)
         do j = 1, nvdll(k)
            read(iunit,*) nbdv(k,j)
            nbdv(k,j) = nsequencer(nbdv(k,j))
         enddo
      enddo
      read(iunit,*) nbou
      read(iunit,*) nvel
      do k = 1, nbou
         read(iunit,*) nvell(k), ibtype(k)
         select case(ibtype(k))
            case(0,1,2,10,11,12,20,21,22,30,52)
               do j = 1, nvell(k)
                  read(iunit,*) nbvv(k,j)
                  nbvv(k,j) = nsequencer(nbvv(k,j))
               enddo
            case(3, 13, 23)
               do j = 1, nvell(k)
                  read(iunit,*) nbvv(k,j), (bar(i,k,j), i=1,2)
                  nbvv(k,j) = nsequencer(nbvv(k,j))
               enddo
            case(4, 24)
               do j = 1, nvell(k)
                  read(iunit,*) nbvv(k,j), ibconn(k,j), (bar(i,k,j), i=1,3)
                  nbvv(k,j) = nsequencer(nbvv(k,j))
                  ibconn(k,j) = nsequencer(ibconn(k,j))
               enddo
            case default
               write(6,*) 'ERROR: IBTYPE ',ibtype(k),' is not allowed.'
               stop
         end select
      enddo
      close(14)
      write(6,*) "INFO: Finished reading mesh file coordinates, connectivity, and boundary data."
!     deallocate( nsequencer )
   !-----+---------+---------+---------+---------+---------+---------+
   end subroutine read14
   !-----+---------+---------+---------+---------+---------+---------+

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
      LOGICAL :: fileFound    ! .true. if the file is present
      errorIO = 0
!
!     Check to see if file exists
      write(6,'("INFO: Searching for file to open on unit ",I5,"...")') lun
      inquire(FILE=trim(filename),EXIST=fileFound)
      if (fileFound.eqv..false.) then
         write(6,'("ERROR: The file ",A," was not found.")') trim(filename)
         stop
      else
         write(6,'("INFO: The file ",A," was found. The file will be opened.")') &
            trim(filename)
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

!----------------------------------------------------------------------
!  CHECK
!---------------------------------------------------------------------
   SUBROUTINE Check(ncStatus)
 !     USE DATA,ONLY: MyRank
      USE netcdf
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: ncStatus
      IF(ncStatus.NE.NF90_NOERR)THEN
         WRITE(*,'(A,A)') "ERROR: NetCDF: ",TRIM(NF90_STRERROR(ncStatus))
         STOP
      ENDIF
   END SUBROUTINE check

!-----+---------+---------+---------+---------+---------+---------+
   end module adcmesh
!-----+---------+---------+---------+---------+---------+---------+

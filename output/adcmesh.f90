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
      double precision, parameter :: R = 6378206.4d0 ! radius of the earth
      double precision, parameter :: pi = 3.141592653589793d0
      double precision :: deg2rad
      double precision :: rad2deg

      double precision, allocatable :: xyd(:,:), bar(:,:,:)
      !
      logical                          :: cppComputed = .false.
      double precision, allocatable :: x_cpp(:)
      double precision, allocatable :: y_cpp(:)
      !
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
      logical                       :: neighborTableInitialized = .false.
      integer                       :: NEIMIN
      integer                       :: NEIMAX 
      integer,         allocatable :: NNeigh(:)
      integer,         allocatable :: NeiTab(:,:)
      integer,         allocatable :: NeiTabEle(:,:)
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
      integer                       :: NC_VarID_nope 
      integer                       :: NC_VarID_nvell
      integer                       :: NC_VarID_max_nvell
      integer                       :: NC_VarID_ibtype
      integer                       :: NC_VarID_nbvv
      integer                       :: NC_VarID_depth

      logical                       :: projectCPP ! .true. if user wants to project mesh coordinates with CPP to aid in visualization
      logical                       :: cppUpdated ! .true. if we've already computed/written CPP on this execution
      double precision            :: slam0  ! longitude on which cpp projection is centered
      double precision            :: sfea0  ! latitude on which cpp projection is centered

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

!******************************************************************************
!                                                                             *
!      Subroutine to generate neighbor tables from a connectivity table.      *
!                                                                             *
!      NOTES                                                                  *
!      a node neighbor table is generated with the node itself is listed as   *
!         neighbor #1 and all other neighbors are sorted and placed in cw     *
!         order from east                                                     *
!      a neighbor element table is generated with:                            *
!         entry 1 = element # defined by neighbors 1,2,3                      *
!         entry 2 = element # defined by neighbors 1,3,4                      *
!         entry 3 = element # defined by neighbors 1,4,5                      *
!          .......                                                            *
!         entry last = element # defined by neighbors 1,nneigh,2              *
!         a zero area means that the defined triangle lies outside the domain *
!                                                                             *
!                                                                             *
!    v1.0   R.L.   6/29/99  used in 3D code                                   *
!    v2.0   R.L.   5/23/02  adapted to provide neighbor el table              *
!******************************************************************************
!                                                                             *
!     -  PARAMETERS WHICH MUST BE SET TO CONTROL THE DIMENSIONING OF ARRAYS   *
!           ARE AS FOLLOWS:                                                   *
!                                                                             *
!          MNP = MAXIMUM NUMBER OF NODAL POINTS                               *
!          MNE = MAXIMUM NUMBER OF ELEMENTS                                   *
!          MNEI= 1+MAXIMUM NUMBER OF NODES CONNECTED TO ANY ONE NODE IN THE   *
!                  FINITE ELEMENT GRID                                        *
!                                                                             *
!******************************************************************************
!                                                                             *
!    VARIABLE DEFINITIONS:                                                    *
!       NE - NUMBER OF ELEMENTS                                               *
!       NP - NUMBER OF NODES                                                  *
!       NM(MNE,3) - NODE NUMBERS ASSOCIATED WITH EACH ELEMENT                 *
!       NNeigh(MNP) NUMBER OF NEIGHBORS FOR EACH NODE                         *
!       NeiTab(MNP,NEIMAX) 2D ARRAY OF NEIGHBORS FOR EACH NODE                *
!       NeiTabEle(MNP,NEIMAX) 2D ARRAY OF NEIGHBOR ELEMENTS FOR EACH NODE     *
!       NEIMIN - 1+MINIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!       NEIMAX - 1+MAXIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!                                                                             *
!******************************************************************************
      SUBROUTINE computeNeighborTable()
      IMPLICIT NONE

      INTEGER, PARAMETER :: MNEI = 15
      DOUBLE PRECISION, ALLOCATABLE :: ANGLE(:)
      INTEGER, ALLOCATABLE :: NEITEM(:)
      INTEGER, ALLOCATABLE :: NNeighEle(:)
      DOUBLE PRECISION :: ANGLELOW
      DOUBLE PRECISION :: ANGLEMORE
      DOUBLE PRECISION :: DELX
      DOUBLE PRECISION :: DELY
      DOUBLE PRECISION :: DIST
      INTEGER :: NN1, NN2, NN3 ! node numbers around an element
      INTEGER :: NE1, NE2, NE3 ! element numbers
      INTEGER :: I, J, JJ, JLOW, K, N  ! loop counters
      !
      ! Initialization
      ALLOCATE ( ANGLE(MNEI) )
      ALLOCATE ( NEITEM(NP) )
      ALLOCATE ( NNeighEle(NP) )
      deg2rad = 2.d0*pi/360.d0
      rad2deg = 1.d0/deg2rad
      ! initialize neighbor table to zeroes
      NNeigh(:)=0
      NNeighEle(:)=0
      NeiTab(:,:)=0
      NeiTabEle(:,:)=0
      DO 10 N=1,NE
         NN1 = NM(N,1)
         NN2 = NM(N,2)
         NN3 = NM(N,3)
         NNeighEle(NN1)=NNeighEle(NN1)+1
         NNeighEle(NN2)=NNeighEle(NN2)+1
         NNeighEle(NN3)=NNeighEle(NN3)+1
         NeiTabEle(NN1,NNeighEle(NN1))=N
         NeiTabEle(NN2,NNeighEle(NN2))=N
         NeiTabEle(NN3,NNeighEle(NN3))=N

         DO J=1,NNeigh(NN1)
            IF(NN2.EQ.NeiTab(NN1,J)) GOTO 25
         END DO
         NNeigh(NN1)=NNeigh(NN1)+1
         NNeigh(NN2)=NNeigh(NN2)+1
         IF((NNeigh(NN1).GT.MNEI-1).OR.(NNeigh(NN2).GT.MNEI-1)) GOTO 999
         NeiTab(NN1,NNeigh(NN1))=NN2
         NeiTab(NN2,NNeigh(NN2))=NN1

 25      CONTINUE
         DO J=1,NNeigh(NN1)
            IF(NN3.EQ.NeiTab(NN1,J)) GOTO 35
         END DO
         NNeigh(NN1)=NNeigh(NN1)+1
         NNeigh(NN3)=NNeigh(NN3)+1
         IF((NNeigh(NN1).GT.MNEI-1).OR.(NNeigh(NN3).GT.MNEI-1)) GOTO 999
         NeiTab(NN1,NNeigh(NN1))=NN3
         NeiTab(NN3,NNeigh(NN3))=NN1

 35      CONTINUE
         DO J=1,NNeigh(NN2)
            IF(NN3.EQ.NeiTab(NN2,J)) GOTO 10
         END DO
         NNeigh(NN2)=NNeigh(NN2)+1
         NNeigh(NN3)=NNeigh(NN3)+1
         IF((NNeigh(NN2).GT.MNEI-1).OR.(NNeigh(NN3).GT.MNEI-1)) GOTO 999
         NeiTab(NN2,NNeigh(NN2))=NN3
         NeiTab(NN3,NNeigh(NN3))=NN2

 10   CONTINUE
!
!     INSERT NODE ITSELF IN PLACE #1 and SORT other NEIGHBORS by
!     increasing cw angle from East
!
      DO I=1,NP
         DO J=1,NNeigh(I)
            NEITEM(J)=NeiTab(I,J)
            DELX=x_cpp(NEITEM(J))-x_cpp(I)
            DELY=y_cpp(NEITEM(J))-y_cpp(I)
            DIST=SQRT(DELX*DELX+DELY*DELY)
            IF(DIST.EQ.0.0d0) GOTO 998
            IF(DELY.NE.0.0d0) THEN
               ANGLE(J)=RAD2DEG*ACOS(DELX/DIST)
               IF(DELY.GT.0.0) ANGLE(J)=360.0d0-ANGLE(J)
            ENDIF
            IF(DELY.EQ.0.0d0) THEN
               IF(DELX.GT.0.0d0) ANGLE(J)=0.0d0
               IF(DELX.LT.0.d0) ANGLE(J)=180.0d0
            ENDIF
         END DO
         ANGLEMORE=-1.d0
         DO JJ=1,NNeigh(I)
            ANGLELOW=400.d0
            DO J=1,NNeigh(I)
               IF((ANGLE(J).LT.ANGLELOW).AND.(ANGLE(J).GT.ANGLEMORE)) THEN
                  ANGLELOW=ANGLE(J)
                  JLOW=J
               ENDIF
            END DO
            NeiTab(I,JJ+1)=NEITEM(JLOW)
            ANGLEMORE=ANGLELOW
         END DO
         NeiTab(I,1)=I
         NNeigh(I)=NNeigh(I)+1
      ENDDO
!
!     MATCH EACH SET OF 3 NODES WITH CORRESPONDING ELEMENT AND REORDER
!     ELEMENTS ACCORDINGLY
!
      DO I=1,NP
         DO K=1,NNeighEle(I)
            NEITEM(K)=NeiTabEle(I,K)
            NeiTabEle(I,K)=0
         END DO
         DO J=2,NNeigh(I)
            NN1=NeiTab(I,1)
            NN3=NeiTab(I,J)
            IF(J.NE.NNeigh(I)) NN2=NeiTab(I,J+1)
            IF(J.EQ.NNeigh(I)) NN2=NeiTab(I,2)
            DO K=1,NNeighEle(I)
               IF(NEITEM(K).NE.0) THEN
                  IF(NM(NEITEM(K),1).EQ.NN1) THEN
                     NE1=NM(NEITEM(K),1)
                     NE2=NM(NEITEM(K),2)
                     NE3=NM(NEITEM(K),3)
                  ENDIF
                  IF(NM(NEITEM(K),2).EQ.NN1) THEN
                     NE1=NM(NEITEM(K),2)
                     NE2=NM(NEITEM(K),3)
                     NE3=NM(NEITEM(K),1)
                  ENDIF
                  IF(NM(NEITEM(K),3).EQ.NN1) THEN
                     NE1=NM(NEITEM(K),3)
                     NE2=NM(NEITEM(K),1)
                     NE3=NM(NEITEM(K),2)
                  ENDIF
                  IF((NE2.EQ.NN2).AND.(NE3.EQ.NN3)) THEN
                     NeiTabEle(I,J-1)=NEITEM(K)
                     NEITEM(K)=0
                  ENDIF
               ENDIF
            END DO
         END DO
      END DO
      !
      !  DETERMINE THE MAXIMUM AND MINIMUM NUMBER OF NEIGHBORS
      NEIMAX = maxval(NNeigh)
      NEIMIN = minval(NNeigh)
      !  Deallocate local work arrays
      DEALLOCATE ( ANGLE )
      DEALLOCATE ( NEITEM )
      DEALLOCATE ( NNEIGHELE )
      RETURN

 999  CONTINUE
      WRITE(6,*) 'ERROR: Computation of neighbor table failed.'
      STOP
 998  CONTINUE
      WRITE(6,*) 'ERROR: Nodes ',I,' and ',NEITEM(J),' have the same coordinates.'
      STOP
!-----------------------------------------------------------------------
      END SUBROUTINE computeNeighborTable
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     S U B R O U T I N E   C O M P U T E  C P P 
!-----------------------------------------------------------------------
!     jgf: Very short subroutine to compute the CPP projection, 
!     allocating memory in the process, and not overwriting the 
!     original lat/lon data.
!-----------------------------------------------------------------------
      SUBROUTINE computeCPP()
      IMPLICIT NONE
      deg2rad = 2.d0*pi/360.d0
      if (cppComputed.eqv..false.) then
         allocate(x_cpp(np),y_cpp(np))
         cppComputed = .true.
      endif
      write(6,'("INFO: Generating CPP coordinates.")')
      x_cpp = R * (xyd(1,:)*deg2rad - slam0*deg2rad) * cos(sfea0*deg2rad)
      y_cpp = xyd(2,:)*deg2rad * R
      return
!-----------------------------------------------------------------------
      END SUBROUTINE computeCPP
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

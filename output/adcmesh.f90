!-----+---------+---------+---------+---------+---------+---------+
!
! adcmesh.f90
! This is a module for storing and manipulating data for ADCIRC meshes;
! it is based on code written by Corbitt Kerr.
!
!-----+---------+---------+---------+---------+---------+---------+
   module adcmesh
!-----+---------+---------+---------+---------+---------+---------+
      character(1024) :: meshFileName ! full pathname of file
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
      integer                       :: nope, neta
      integer                       :: nbou, nvel
      integer,          allocatable :: nm(:,:)   ! element table (ne,3)
      integer,          allocatable :: nvdll(:)  ! number of nodes on each open boundary
      integer,          allocatable :: nbdv(:,:) ! node numbers on each open boundary
      integer,          allocatable :: nvell(:)  ! number of nodes on each flux boundary
      integer,          allocatable :: ibtype(:) ! boundary type of each flux boundary
      integer,          allocatable :: nbvv(:,:) ! node numbers on each flux boundary 
      integer                       :: nvdll_max  ! longest elevation boundary
      integer                       :: nvell_max  ! longest flux boundary     
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
      !
      ! elevation boundaries and flux boundaries where
      ! ibtype = 0,1,2,10,11,12,20,21,22,30,52
      type simpleBoundary_t
         integer, allocatable :: nodes(:) ! node numbers on boundary
      end type simpleBoundary_t
      ! variable holding elevation boundaries
      type(simpleBoundary_t), allocatable :: elevationBoundaries(:)

      ! variable holding flux boundaries with ibtype = 0, 1, 2, 10, 11, 12, 20, 21, 22, 30, 52
      type(simpleBoundary_t), allocatable :: simpleFluxBoundaries(:)
      integer :: numSimpleFluxBoundaries ! for memory allocation
      integer :: sfCount   ! index into the simpleFluxBoundaries array

      ! flux boundaries where ibtype = 3, 13, 23
      type externalFluxBoundary_t
         integer, allocatable :: nodes(:)
         integer, allocatable :: barlanht(:)
         integer, allocatable :: barlancfsp(:)
      end type externalFluxBoundary_t
      type(externalFluxBoundary_t), allocatable :: externalFluxBoundaries(:)
      integer :: numExternalFluxBoundaries 
      integer :: efCount   ! index into the externalFluxBoundaries array
      
      ! flux boundaries where ibtype = 4, 24
      type internalFluxBoundary_t
         integer, allocatable :: nodes(:)
         integer, allocatable :: ibconn(:)
         integer, allocatable :: barinht(:)
         integer, allocatable :: barincfsb(:)
         integer, allocatable :: barincfsp(:)         
      end type internalFluxBoundary_t
      type(internalFluxBoundary_t), allocatable :: internalFluxBoundaries(:)
      integer :: numInternalFluxBoundaries    
      integer :: ifCount   ! index into the internalFluxBoundaries array
      
      ! flux boundaries where ibtype = 5, 25
      type internalFluxBoundaryWithPipes_t
         integer, allocatable :: nodes(:)
         integer, allocatable :: ibconnr(:)
         integer, allocatable :: barinhtr(:)
         integer, allocatable :: barincfsbr(:)
         integer, allocatable :: barincfspr(:)
         integer, allocatable :: pipehtr(:)
         integer, allocatable :: pipecoefr(:)
         integer, allocatable :: pipediamr(:)
      end type internalFluxBoundaryWithPipes_t      
      type(internalFluxBoundaryWithPipes_t), allocatable :: internalFluxBoundariesWithPipes(:)
      integer :: numInternalFluxBoundariesWithPipes
      integer :: ifwpCount ! index into the internalFluxBoundariesWithPipes array

      
   contains

   !-----+---------+---------+---------+---------+---------+---------+
   !  READ14_FindDims
   !-----+---------+---------+---------+---------+---------+---------+
   subroutine read14_findDims ()
      implicit none
      integer :: i, j, k
      integer, parameter :: iunit = 14
!
      read(iunit,'(A32)') agrid 
      write(6,'(A)') "INFO: Mesh file comment line: "//trim(agrid)
      write(6,'(A)') "INFO: Reading mesh file dimensions."
      read(iunit,*) ne, np
      do k = 1, np
         read(iunit,*) i
      enddo
      do k = 1, ne
         read(iunit,*) i 
      enddo
      read(iunit,*) nope  ! total number of elevation boundaries
      read(iunit,*) neta  ! total number of nodes on elevation boundaries 
      neta_count = 0
      allocate(nvdll(nope))
      nvdll_max = 0
      do k = 1, nope         
         read(iunit,*) nvdll(k) ! number of nodes on the kth elevation boundary segment
         nvdll_max = max(nvdll_max,nvdll(k))
         do j = 1, nvdll(k)
            read(iunit,*)
            neta_count = neta_count + 1
         enddo
      enddo
      if ( neta_count.ne.neta ) then
         write(6,'("WARNING: Number of open boundary nodes was set to ",I6," but ",I6," were found.")') neta, neta_count
      endif
      read(iunit,*) nbou ! total number of flux boundaries
      read(iunit,*) nvel ! total number of nodes on flux boundaries
      nvel_count = 0
      nvell_max = 0
      allocate(nvell(nbou))
      allocate(ibtype(nbou))
      do k = 1, nbou
         read(iunit,*) nvell(k), ibtype(k)  ! number of nodes and type of kth flux boundary 
         nvell_max = max(nvell_max,nvell(k))
         do j = 1, nvell(k)
            read(iunit,*)
            nvel_count = nvel_count + 1
         enddo
         ! count the total number of each type of boundary for later
         ! use in memory allocation
         select case(ibtype(k))
         case(0,1,2,10,11,12,20,21,22,30,52)
             numSimpleFluxBoundaries = numSimpleFluxBoundaries + 1
         case(3,13,23)
             numExternalFluxBoundaries = numExternalFluxBoundaries + 1 
         case(4,24)
             numInternalFluxBoundaries = numInternalFluxBoundaries + 1 
         case(5,25)
             numInternalFluxBoundariesWithPipes = numInternalFluxBoundariesWithPipes + 1
         case default
             write(6,'("ERROR: The boundary type ",I3," was found in the files but is not valid.")')
             stop
         end select
      enddo
      if ( nvel_count.ne.nvel) then
         write(6,'("WARNING: Number of land boundary nodes was set to ",I6," but ",I6," were found.")') nvel, nvel_count
      endif

      rewind(iunit)
      write(6,'(A)') 'INFO: Finished reading mesh file dimensions.'
   !-----+---------+---------+---------+---------+---------+---------+
   end subroutine read14_findDims
   !-----+---------+---------+---------+---------+---------+---------+

   !-----+---------+---------+---------+---------+---------+---------+
   ! READ14
   !-----+---------+---------+---------+---------+---------+---------+
   subroutine read14 ()
      implicit none

      integer :: i, j, k, jn, je, nhy
      integer, parameter :: iunit = 14

      if (trim(meshFileName).eq."null") then
         WRITE(6,'(A)',ADVANCE='NO') "Enter name of the fort.14 file: "
         read(5,'(A)') meshFileName
      endif
      call openFileForRead(iunit,trim(meshFileName))
      call read14_findDims ()
      allocate(xyd(3,np)) ! node table
      allocate(nm(3,ne))  ! element table
      allocate(elevationBoundaries(nope))
      do i=1,nope
         allocate(elevationBoundaries(i)%nodes(nvdll(i)))
      end do   
      allocate(simpleFluxBoundaries(numSimpleFluxBoundaries))
      allocate(externalFluxBoundaries(numExternalFluxBoundaries))
      allocate(internalFluxBoundaries(numInternalFluxBoundaries))
      allocate(internalFluxBoundariesWithPipes(numInternalFluxBoundariesWithPipes))
      do i=1,nbou
         select case(ibtype(i))
         case(0,1,2,10,11,12,20,21,22,30,52)
            allocate(simpleFluxBoundaries(i)%nodes(nvell(i)))
         case(3,13,23)
            allocate(externalFluxBoundaries(i)%nodes(nvell(i)))
            allocate(externalFluxBoundaries(i)%barlanht(nvell(i)))
            allocate(externalFluxBoundaries(i)%barlancfsp(nvell(i)))
         case(4,24)
            allocate(internalFluxBoundaries(i)%nodes(nvell(i)))
            allocate(internalFluxBoundaries(i)%ibconn(nvell(i)))
            allocate(internalFluxBoundaries(i)%barinht(nvell(i)))
            allocate(internalFluxBoundaries(i)%barincfsb(nvell(i)))
            allocate(internalFluxBoundaries(i)%barincfsp(nvell(i)))
         case(5,25)
            allocate(internalFluxBoundariesWithPipes(i)%nodes(nvell(i)))
            allocate(internalFluxBoundariesWithPipes(i)%ibconnr(nvell(i)))
            allocate(internalFluxBoundariesWithPipes(i)%barinhtr(nvell(i)))
            allocate(internalFluxBoundariesWithPipes(i)%barincfsbr(nvell(i)))
            allocate(internalFluxBoundariesWithPipes(i)%barincfspr(nvell(i)))         
            allocate(internalFluxBoundariesWithPipes(i)%pipehtr(nvell(i)))
            allocate(internalFluxBoundariesWithPipes(i)%pipecoefr(nvell(i)))
            allocate(internalFluxBoundariesWithPipes(i)%pipediamr(nvell(i)))
         case default
             write(6,'("ERROR: The boundary type ",I3," was found in the files but is not valid.")')
             stop
         end select
      end do
      write(6,'(A)') 'INFO: Reading mesh file coordinates, connectivity, and boundary data.'
      read(iunit,*) agrid
      read(iunit,*) ne, np
      do k = 1, np
         read(iunit,*) jn, (xyd(j,k), j=1,3)
      enddo
      do k = 1, ne
         read(iunit,*) je, nhy, ( nm(j,k), j = 1, 3 )
      enddo
      read(iunit,*) nope
      read(iunit,*) neta
      do k = 1, nope
         read(iunit,*) nvdll(k)
         do j = 1, nvdll(k)
            read(iunit,*) elevationBoundaries(k)%nodes(j)
         enddo
      enddo
      read(iunit,*) nbou
      read(iunit,*) nvel
      sfCount = 1
      efCount = 1
      ifCount = 1
      ifwpCount = 1      
      do k = 1, nbou
         read(iunit,*) nvell(k), ibtype(k)
         select case(ibtype(k))
         case(0,1,2,10,11,12,20,21,22,30,52)
            do j = 1, nvell(k)
               read(iunit,*) simpleFluxBoundaries(sfCount)%nodes(j)
            end do
            sfCount = sfCount + 1
         case(3,13,23)
            do j = 1, nvell(k)
               read(iunit,*) externalFluxBoundaries(efCount)%nodes(j), &
                             externalFluxBoundaries(efCount)%barlanht(j), &
                             externalFluxBoundaries(efCount)%barlancfsp(j)
            end do
            efCount = efCount + 1
         case(4,24)
            do j = 1, nvell(k)
               read(iunit,*) internalFluxBoundaries(ifCount)%nodes(j), &
                             internalFluxBoundaries(ifCount)%ibconn(j), &
                             internalFluxBoundaries(ifCount)%barinht(j), &
                             internalFluxBoundaries(ifCount)%barincfsb(j), &
                             internalFluxBoundaries(ifCount)%barincfsp(j)
            end do
            ifCount = ifCount + 1
         case(5,25)
            do j = 1, nvell(k)
               read(iunit,*) internalFluxBoundariesWithPipes(ifCount)%nodes(j), &
                             internalFluxBoundariesWithPipes(ifCount)%ibconnr(j), &
                             internalFluxBoundariesWithPipes(ifCount)%barinhtr(j), &
                             internalFluxBoundariesWithPipes(ifCount)%barincfsbr(j), &
                             internalFluxBoundariesWithPipes(ifCount)%barincfspr(j), &
                             internalFluxBoundariesWithPipes(ifCount)%pipehtr(j), &
                             internalFluxBoundariesWithPipes(ifCount)%pipecoefr(j), &
                             internalFluxBoundariesWithPipes(ifCount)%pipediamr(j)              
            end do
            ifwpCount = ifwpCount + 1
         case default
            write(6,*) 'ERROR: IBTYPE ',ibtype(k),' is not allowed.'
            stop
         end select
      end do
      close(14)
      write(6,'(A)') 'INFO: Finished reading mesh file coordinates, connectivity, and boundary data.'
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
      inquire(FILE=trim(filename),EXIST=fileFound)
      if (fileFound.eqv..false.) then
         write(6,'("ERROR: The file ",A," was not found.")') trim(filename)
      else
         write(6,'("INFO: The file ",A," was found.")') trim(filename)
      endif
      return
!-----------------------------------------------------------------------
   END SUBROUTINE checkFileExistence
!-----------------------------------------------------------------------


!----------------------------------------------------------------------
!  CHECK
!---------------------------------------------------------------------
   SUBROUTINE Check(ncStatus)
      USE netcdf
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: ncStatus
      INTEGER, ALLOCATABLE :: dummy(:)
      IF(ncStatus.NE.NF90_NOERR)THEN
         WRITE(*,'(A,A)') "ERROR: ",TRIM(NF90_STRERROR(ncStatus))
         ! if the program was compiled with debug support, generate an
         ! intentional segmentation fault so that the executable will
         ! dump a stack trace and we can find the line number where
         ! this netcdf error was generated
         WRITE(*,*) dummy(1)
         STOP
      ENDIF
   END SUBROUTINE check
!-----+---------+---------+---------+---------+---------+---------+
   end module adcmesh
!-----+---------+---------+---------+---------+---------+---------+

!-----------------------------------------------------------------------
!
!       DROGUES  -  ***   DROGUES.F   ***  HARMONIC VELOCITY FIELDS
!              	    ***   DISTRIBUTION VERSION 1991, BRIAN O. BLANTON 
!                   ***   LAST MODIFICATION MAY 2010
!
!-----------------------------------------------------------------------
!
! ORIGINAL AUTHORS: ANTONIO M. BAPTISTA, MIT, OGC
!                   DOUGLAS COSLER, MIT
!                   PAUL J. TURNER, OGC
!                   E. ERIC ADAMS, MIT
!                   RICHARD KOSSICK, MIT
!
! MODIFICATIONS BY: BRIAN O. BLANTON, UNC
!                   RICK LUETTICH, UNC       
!                   ROBERT J WEAVER, UNC
!-----------------------------------------------------------------------
!
!   THE ORIGINAL CODE IS A PART OF THE COMPUTATIONAL STRUCTURE, ACE, FOR 
!   ANALYSIS OF FLOW, HYDRAULIC AND SEDIMENT TRANSPORT, AND WATER 
!   QUALITY IN ESTUARIES AND COASTS.
!
!-----------------------------------------------------------------------
!
!       REVISION HISTORY
!
!       AMB APRIL 1988   - REMOVED TRACKING ALGORITHM FROM ELA, AND
!                          IMPLEMENTED IT AS SEPARATE CODE, DROGUES
!
!       PJT JULY 1988    - REMOVED VARIABLES DECLARED BUT NOT USED,
!                          REMOVED SUBROUTINES NOT USED,
!                          CHANGED INTEGER*2 DECLARATIONS TO INTEGER,
!                          PORTED TO CRAY XMP, ALLIANT FX/8,
!                          GENERAL TIDYING UP
!
!       PJT AUG 1988     - ADDED SUBROUTINE BELELINIT TO INITIALIZE
!                          DATA FOR BELEL. REMOVED THE LOOP IN BELEL.
!                          CHANGED THE WRITE STATEMENT IN TRACK THAT
!                          OUTPUTS THE RESULTS TO A FORMATTED WRITE
!
!       AMB/PJT DEC 1988 - CHANGED FORMAT FOR GRID FROM QUADRATIC 
!                          GRID TO LINEAR GRID. MORE CLEANUPS 
!                          AND DEAD CODE ELIMINATION.
!
!       DKS MARCH 1990   - ADAPTED DROGUES.F FOR TIME-DEPENDENT 
!                          VELOCITY FIELDS.
!
!       BOB JAN 1991     - ADDED FLEXIBILITY IN UNITS SPECIFICATION. 
!                          ONCE A DROGUE ENCOUNTERS A BOUNDARY,
!                          IT STAYS THERE.  USERS MANUAL WRITTEN.
!
!       RAL MAY 1997     - MODIFIED ROUTINES TO DEAL WITH DROGUE BEHAVIOR
!                          NEAR BOUNDARIES.  REMOVED PREVIOUS "STICKY ZONE"
!                          APPROACH AND REPLACED THIS WITH PROVISIONS FOR
!                          DROGUES TO MOVE ALONG A BOUNDARY SEGMENT USING THE 
!                          VELOCITY COMPONENT TANGENTIAL TO THAT SEGMENT.
!
!       CWF AUG 2002     - ADDED AN OPTION TO READ IN A FORT.64 FILE OR
!                          HARMONIC FILES AS INPUT.  THE FORT.64 DOES NOT NEED
!                          TO BE CHANGED EXCEPT TO MAKE IT A FILE THAT ENDS IN
!                          .V2C
!
!       RJW MAY 2010     - MODIFIED TO USE FOR TRACKING OIL SLICK
!                          Removed all harmonic velocity capabilities 
!                          added an imrproved searching algorithm to find starting elements 
!                          for each particle.  ALtered input method, input file names now
!                          read in from drogue_input_1.  
!                          ADDED ability to handle full global output velocity files OR
!                          sparse global output velocity files.
!                          Modified input and output format to work with ASGS tracking system
!                          Included a Z, variable (flag) for each particle 
!                          
!       ST3 JUN 2010     - TRANSPORT TO Fortran90 FORMAT
!                          Cancellation of COMMON, GOTO, Static memory allocation..., and clean-up
!                          ADDED Fail-safe in searching part
!                          Implement OpenMP parallelization for tracking part
!-----------------------------------------------------------------------
 MODULE COMMONS
  IMPLICIT NONE
!  !U and V velocities read from ?????.VEL (a fort.64 file)
     DOUBLE PRECISION, ALLOCATABLE :: UNEW(:),VNEW(:)                         !/VEL/
!  !COORDINATES OF THE NODES - READ FROM ?????.GR3
     DOUBLE PRECISION, ALLOCATABLE :: X(:),Y(:)                               !/COORDS/
!  !SHAPE FUNCTION INFORMATION
     DOUBLE PRECISION, ALLOCATABLE :: A(:,:),B(:,:),A0(:,:)                   !/ABA0/
!  !TABLE OF ELEMENTS
     INTEGER,ALLOCATABLE           :: ELEMS(:,:)                              !/ELEMS/
!  !CONNECTIVITY MATRICES - ELEMENT-ELEMENT & NODE-ELEMENT
     INTEGER,ALLOCATABLE           :: ICEE(:,:),ICNE(:,:)                     !/CONEC/
!  !AREAS OF ELEMENTS
     DOUBLE PRECISION, ALLOCATABLE :: AR(:)                                   !/ELAREAS/
     DOUBLE PRECISION, ALLOCATABLE :: T(:,:)                                  !/ARBEL/
!  !TOLERANCE ERROR PARAMETERS
     DOUBLE PRECISION              :: EPS,DTMIN                               !/ERRORS/
!  !NUMBER OF NODES, NUMBER OF ELEMENTS
     INTEGER                       :: NMND, NMEL, NTINT                       !/NUMBER/
!  !BOUNDARY SEGMENTS
     INTEGER,ALLOCATABLE           :: IBSEG(:,:),ISEGP(:),ISEGF(:),IBSEGEL(:) !/BSEG/
     INTEGER                       :: NSEG                                    !/BSEG/
!  !NUMBER VARIABLES
     INTEGER                       :: NTIME                                   !/NUMS/
!  !TOTAL NUMBER OF PARTICLES
     INTEGER                       :: NDR
     INTEGER, ALLOCATABLE          :: KODE(:), ICURBS(:)
 END MODULE COMMONS
 MODULE LATTICE_TABLE
! !!st3 SEARCHING TABLE         
  IMPLICIT NONE
     INTEGER,PARAMETER   :: NDIV=1000
     INTEGER             :: NE_PIECE_INDEX(1:NDIV,1:NDIV), NE_PIECE(0:NDIV+1,0:NDIV+1)
     INTEGER,ALLOCATABLE :: NE_PIECE_LIST(:) !NE_PIECE_LIST(NNE*2) <= this is safety allocation
     DOUBLE PRECISION    :: XMIN(1:2), DX(1:2)
 END MODULE LATTICE_TABLE

 PROGRAM MAIN
!     SET MAXIMUM DIMENSIONING OF ARRAYS
      USE COMMONS
      USE LATTICE_TABLE
!$    USE OMP_LIB
      IMPLICIT NONE
!-----------------------------------------------------------------------
!***********************************************************************
      DOUBLE PRECISION, PARAMETER   ::REARTH=6.3675D6
!   !U and V velocities read from ?????.VEL (a fort.64 file)
      DOUBLE PRECISION, ALLOCATABLE ::  US(:,:),VS(:,:) !US(ND,2),VS(ND,2)
!   !BOUNDARY SEGMENTS
      INTEGER ::  IL(3,2)
      INTEGER ::  ISEG,IPROD,IEAD,K
!   !LOCAL VARIABLES - DT1, ARRAY OF STEP SIZES FOR INTEGRATION
      DOUBLE PRECISION, ALLOCATABLE ::  DT1(:) !DT1(MXDRG)
!   !XDR,YDR - INITIAL DROGUE POSITIONS AND AFTER EACH TRACKING SET,
!   !THE NEW LOCATION.
      DOUBLE PRECISION, ALLOCATABLE :: XDR(:),YDR(:),ZDR(:) !XDR(MXDRG),YDR(MXDRG),ZDR(MXDRG)
      INTEGER,ALLOCATABLE           :: IDR(:),JJDR(:)       !IDR(MXDRG),JJDR(MXDRG)
!   !CHARACTER VARIABLES FOR FILENAMES AND MISC
      CHARACTER(LEN=80)   :: GRID,HEADER
      CHARACTER(LEN=72),ALLOCATABLE :: VLIST(:) !VLIST(NFR)
      CHARACTER(LEN=72)   :: CASE0
      CHARACTER(LEN=20)   :: JunkC
      CHARACTER(LEN=100)  :: LINE
      
      DOUBLE PRECISION, ALLOCATABLE :: SCAMPU(:),SCPHAU(:) !SCAMPU(NFR),SCPHAU(NFR)
      DOUBLE PRECISION, ALLOCATABLE :: SCAMPV(:),SCPHAV(:) !SCAMPV(NFR),SCPHAV(NFR)
      INTEGER,ALLOCATABLE           :: ICOMP(:)            !ICOMP(NFR)
      DOUBLE PRECISION    :: LONG,LAT,DEGRAD
      DOUBLE PRECISION    :: XO,YO
      DOUBLE PRECISION    :: PI,TPATH,TIME
      DOUBLE PRECISION    :: SCNDX,SCNDY,SCDRX,SCDRY,XD,YD,ZD
      DOUBLE PRECISION    :: AX,PX,AY,PY,XSTART,YSTART,STEPP,t1,t2,COSPHI
      INTEGER             :: N1,N2,N3,NNO,NFREQT,III,NOTFND,IND,ii,jj,JNEW
      INTEGER             :: NFREQ, LEGNO,NPER,J
      INTEGER             :: IPRINT,I,N,L
      DOUBLE PRECISION    :: XTEST,YTEST
      INTEGER             :: IHOUR,IH
      INTEGER             :: NUMNODES, TIMESTEP, NODE, NUMTIMES, NUMSTOP
      INTEGER             :: OUTPUTTIMESTEP
      INTEGER             :: JunkI
      REAL                :: JunkR
      REAL,   ALLOCATABLE :: FILETIME(:) !FILETIME(MXTIME)
      REAL                :: TIMEINC
      REAL                :: TIMEDIFF,UDIFF,VDIFF
      INTEGER             :: NumNonDefaultNodes!, IntegerTimeStep
      INTEGER             :: Sparse
      REAL                :: DefaultValue
      INTEGER             :: IX, IY, ICHECK       !st3
      DOUBLE PRECISION    :: UNEWS, VNEWS, DDT    !st3
      INTEGER             :: NLOOP
      INTEGER             :: NTHREADS=1, IMAP
      INTEGER,ALLOCATABLE :: NOMP_MAP(:)
!
!
!$ double precision:: TIME_ALL, TIME_TC

!
! *** BEGIN EXECUTION 
!   DEFINE PI.
      PI=2.D0*DASIN(1.D0)
      DEGRAD=PI/180.D0
      
      OPEN(UNIT=90,FILE='drogue_input_1',STATUS='OLD')
!     PRINT*, 'ENTER .din NAME (no suffix)'
       READ(90,'(A)') CASE0   !I/O FILE NAME
       READ(90,'(A)') GRID    !GRID FILE NAME
      CLOSE (90)

! OPEN FILES
      OPEN (UNIT=11,FILE=TRIM(ADJUSTL(CASE0))//'.din') !Particle Conditions (I)
      OPEN (UNIT=12,FILE=TRIM(ADJUSTL(CASE0))//'.pth') !Particle Path       (O)
!
! *** READS CHARACTERISTICS OF THE RUN FROM FILE ?????.DIN
! *** TWO ALPHANUMERICS
      READ(11,'(A)') HEADER
      write(*,*) header
      READ(11,*) LEGNO,TPATH,TIME,NTINT,NPER
        ALLOCATE( VLIST(1:NPER), ICOMP(1:NPER) )
        ALLOCATE( SCAMPU(1:NPER),SCPHAU(1:NPER),SCAMPV(1:NPER),SCPHAV(1:NPER) )
      READ(11,'(A)')(VLIST(J),J=1,NPER)
      READ(11,*)(ICOMP(J),SCAMPU(J),SCPHAU(J),SCAMPV(J),SCPHAV(J),J=1,NPER)
      READ(11,'(A)') HEADER
      READ(11,'(A)') HEADER
      READ(11,*) IPRINT
      READ(11,'(A)') HEADER
      READ(11,*) SCNDX,SCNDY
      READ(11,'(A)') HEADER
      READ(11,*) EPS,DTMIN
      READ(11,'(A)') HEADER
      READ(11,*) SCDRX,SCDRY
!
! ***     TOTAL NUMBER OF PARTICLES AT START
      READ(11,'(A)') HEADER
      READ(11,*) NDR, JunkC
        WRITE(*,*) NDR, JunkC
        ALLOCATE( XDR(1:NDR),YDR(1:NDR),ZDR(1:NDR),IDR(1:NDR),JJDR(1:NDR),DT1(1:NDR) )
        ALLOCATE( KODE(1:NDR), ICURBS(1:NDR) )
!
! *** READ IN DROGUE COORDINATES
      READ(11,'(A)') HEADER
      WRITE(*,*) HEADER
      DO I = 1,NDR
        READ(11,*) JunkI, XD, YD, ZD
        XDR(I)=XD*REARTH*DEGRAD*SCDRX
        YDR(I)=YD*REARTH*DEGRAD*SCDRY
        ZDR(I)=ZD
      ENDDO
!
!
! OPEN GRID FILES
!      write(*,*)'DomainName=',GRID(:INDEX(GRID,' ')-1)//'.gr2'
      write(*,*)'GridName= ',TRIM(ADJUSTL(GRID))
      open(9,file=TRIM(ADJUSTL(GRID)),status='old')
!
! *** READS GRID DATA FROM FILE ?????.GR2 !st3 Change to Origial ADCIRC GRID FORMAT
      READ (9,*) 
      READ (9,*) NMEL,NMND
      WRITE(*,*) NMEL,NMND
        ALLOCATE( X(1:NMND),Y(1:NMND) )
        ALLOCATE( ELEMS(1:NMEL,1:3) )
!    !READ COORDINATES OF NODES
      DO I = 1,NMND
        READ(9,*)N,LONG,LAT
        LONG=LONG*DEGRAD
        LAT=LAT*DEGRAD
        X(I)=LONG*REARTH*SCNDX
        Y(I)= LAT*REARTH*SCNDY
      ENDDO
!    !READ THE TABLE OF ELEMENTS
      DO I = 1,NMEL
        READ (9,*) N, JunkI,(ELEMS(N,J),J=1,3)
!        READ (9,*) N,(ELEMS(N,J),J=1,3)
      ENDDO
!
      WRITE( 6,*) ' GRID-INPUT COMPLETE'
      CALL MAK_NEINFO
!
!!   ELEMENT-ELEMENT ADJACENCY INFORMATION
!      DO I = 1,NMEL
!        READ (9,*) N, (ICEE(N,J),J=1,3)
!      ENDDO
!!
!!   NODE-ELEMENT ADJACENCY INFORMATION
!!
!      DO I = 1,NMND
!        READ (9,*) N, ICNE(N,1), (ICNE(N,J+1),J=1,ICNE(N,1))
!      ENDDO
!
!
! Open and start reading the time-series velocity file
      ALLOCATE( US(1:NMND,1:2),VS(1:NMND,1:2) )
      ALLOCATE( UNEW(1:NMND),VNEW(1:NMND) )
      Sparse = 100
      open(10,file=vlist(1)(:INDEX(VLIST(1),' ')-1)//'.v2c',status='old')
        read(10,*) ! HEADER
        read(10,*) numtimes,numnodes, JunkR, JunkI, JunkI
        IF(NUMNODES /= NMND ) THEN
        WRITE(6,*) "Diff.tot num nodes are in velocity and grid file"
          STOP
        ENDIF
        ALLOCATE(FILETIME(1:numtimes))
        READ(UNIT=10,FMT='(A)',END=9000,ERR=9000) LINE
        READ(UNIT=LINE,FMT=*,END=9001, ERR=9001) filetime(1),timestep,NumNonDefaultNodes,DefaultValue
        Sparse=1
        WRITE(*,*) "Sparse file: SPARSE = ", Sparse 
        write(*,*) NumNonDefaultNodes
 9001   READ(UNIT=LINE,FMT=*) filetime(1),timestep
        IF( Sparse /= 1 ) then
          Sparse=0
          WRITE(*,*) "regular file: SPARSE = ", Sparse 
        ENDIF

        k=1
        SELECT CASE(Sparse)
          CASE(1)
            us(1:NMND,k)=DefaultValue
            vs(1:NMND,k)=DefaultValue
            DO l=1,NumNonDefaultNodes
              read(10,*,end=9000,err=9000) node, us(node,k), vs(node,k)
            ENDDO
          CASE(0)
            DO l=1,NumNodes
              read(10,*,end=9000,err=9000) node, us(node,k), vs(node,k)
            ENDDO
          CASE DEFAULT
            WRITE(*,*) "Bad Sparse Value,Error reading vel input file"
            STOP   
        END SELECT
!

      WRITE (6,*) ' VELOCITY INPUT COMPLETE: NO ERROR '
!
! NFREQ = THE NUMBER OF FREQUENCIES ACTUALLY USED
!      NFREQ = NFREQT - 1
! *** CLOSE INPUT FILES
!
      CLOSE (UNIT=9)
      CLOSE (UNIT=11)
!
      PRINT *,' LENGTH OF TRACKING (HOURS)      : ',TPATH
      PRINT *,' TIME AT START OF TRACING (HOURS): ',TIME
      PRINT *,' NUMBER OF TRACKING INTERVALS    : ',NTINT
      PRINT *,' OUTPUT TIME-STEP IS (MINUTES)   : ',(TPATH*IPRINT/NTINT)*60.D0
      PRINT *,' MINIMUM TIME-STEP  IS (MINUTES) : ',DTMIN*60.D0
      OUTPUTTIMESTEP= (TPATH*IPRINT/NTINT)*3600  ! output time step in seconds
!
! *** TIME CONVERSION TO SECONDS
      TPATH = TPATH*3600.D0
      TIME  = TIME *3600.D0
      DTMIN = DTMIN*3600.D0
!
! *** COMPUTE AREA COORDINATES FOR ELEMENT INTERPOLATION FUNCTIONS
      ALLOCATE( A(1:NMEL,1:3), B(1:NMEL,1:3), A0(1:NMEL,1:2) )
      ALLOCATE( AR(1:NMEL), T(1:NMEL,1:3) )
      DO J = 1,NMEL
        N1 = ELEMS(J,1)
        N2 = ELEMS(J,2)
        N3 = ELEMS(J,3)
        A(J,1) = X(N3) - X(N2)
        A(J,2) = X(N1) - X(N3)
        A(J,3) = X(N2) - X(N1)
        B(J,1) = Y(N2) - Y(N3)
        B(J,2) = Y(N3) - Y(N1)
        B(J,3) = Y(N1) - Y(N2)
        A0(J,1) = 0.5* (X(N2)*Y(N3)-X(N3)*Y(N2))
        A0(J,2) = 0.5* (X(N3)*Y(N1)-X(N1)*Y(N3))
        AR(J) = 0.5* (A(J,2)*B(J,1)-A(J,1)*B(J,2))
        T(J,1) = A0(J,1)*2.E0
        T(J,2) = A0(J,2)*2.E0
        T(J,3) = (2.E0*AR(J)-T(J,1)-T(J,2))
!
! *** CHECK FOR NON-POSITIVE ELEMENT AREAS
        IF (AR(J).LE.0) THEN
          PRINT *,'NON-POSITIVE AREA AT ELEM ',J
          STOP
        END IF
      ENDDO
      PRINT *,' ELEMENT AREAS COMPUTED '

      STEPP = TPATH/float(NTINT)

!
! *** DEFINE ELEMENT INTERPOLATION FUNCTIONS FOR VELOCITY
!
!!st3 *** MAKE SEARCHING TABLE
      CALL MAKE_STAB( NMEL, ELEMS, NMND, X, Y )
!
! *** FIND STARTING ELEMENT FOR EACH DROGUE
      print*,'Locating initial drogue positions',NDR
      NOTFND=0
      DO III = 1, NDR
        XSTART = XDR(III)
        YSTART = YDR(III)
        IF (NOTFND.GT.0) THEN
          XDR(III-NOTFND)=XDR(III)
          YDR(III-NOTFND)=YDR(III)
        ENDIF
        IX = INT( (XSTART-XMIN(1))/DX(1) ) + 1
        IY = INT( (YSTART-XMIN(2))/DX(2) ) + 1
        IX = MAX(0,IX); IX = MIN(NDIV+1,IX)
        IY = MAX(0,IY); IY = MIN(NDIV+1,IY)
        ICHECK = 0
        DO J = 1, NE_PIECE(IX,IY)
          I = NE_PIECE_LIST(J+NE_PIECE_INDEX(IX,IY))
          CALL BELEL(I,XSTART,YSTART,IND)
          IF (IND.EQ.1) THEN
            JJDR(III-NOTFND) = I
            ICHECK = 1
            EXIT
          END IF
        ENDDO
        IF( ICHECK == 1 ) CYCLE
        PRINT *,'*** COULD NOT FIND STARTING ELEMENT FOR DROGUE ',III
        NOTFND=NOTFND+1
      ENDDO

      NDR=NDR-NOTFND
      WRITE(*,*) '# DROGUES = ',NDR
      KODE(1:NDR)   = 0
      ICURBS(1:NDR) = 0
!
! ESTABLISH DEFAULT STEP SIZE
      DO III=1,NDR
        DT1(III) = DABS(STEPP/10.d0)
      END DO
!
! LIMITS OF INTEGRATION - TIME IS THE STARTING TIME
      T1 = TIME
      T2 = TIME + STEPP
!
! WRITE TO THE RESULTS FILE - CASE(XX).PTH
!
! *** WRITE OUT HEADERS AND INITIAL DROGUE POSITIONS
!     TO ACE/vis OUTPUT FILE

      WRITE(12,'(a)') GRID
      WRITE(12,*) NTINT/IPRINT, NDR, OUTPUTTIMESTEP
      WRITE(12,*) T1,NDR
      DO III = 1, NDR
        IDR(III) = III 
        WRITE (12,172) III,XDR(III)/REARTH/DEGRAD, &
     &                     YDR(III)/REARTH/DEGRAD, &
     &                     nint(ZDR(III))
      END DO

!
! *** HARD CODING FOR STAGGERED RELEASE AT BI
! *** BEGIN TRACKING OF PARTICLES
!
      print*,' '
      print*,'Begin Tracking...'
!$    NTHREADS=omp_get_max_threads()
      ALLOCATE( NOMP_MAP(1:NDR) )
      J=0
      DO I = 1, NTHREADS
        DO N = 1, INT(NDR/NTHREADS)
          J = J + 1
          NOMP_MAP(J) = (N-1)*NTHREADS+I
        ENDDO
      ENDDO
      DO I = INT(NDR/NTHREADS)*NTHREADS+1, NDR
        J = J + 1
        NOMP_MAP(I) = I
      ENDDO
      WRITE(6,*) NDR, J
!$ write(*,*) 'OpenMP mode',NTHREADS,'CPUs'
!
!!$ TIME_ALL = - omp_get_wtime()
!!$ TIME_TC = 0.0d0
! LOOP OVER EACH TIME STEP
       TIMESTEP_LOOP:DO I = 1,NTINT
          WRITE(*,*) I
! *** Find each timestep in the fort.64 file
         IF (I.eq.1) then
           LOOP_NUMTIMES1:DO j = 2,numtimes
             IF ( Sparse .EQ. 1 ) then
                READ(UNIT=10,FMT=*,END=9003, ERR=9003) filetime(j),timestep,NumNonDefaultNodes,DefaultValue
               write(*,*) NumNonDefaultNodes
             ELSEIF (Sparse .EQ. 0 ) then
               READ(UNIT=10,FMT=*,END=9003, ERR=9003) filetime(j),timestep
             ENDIF
             IF ((T1.ge.filetime(j-1)).and.(T1.lt.filetime(j))) then
               timeinc=filetime(j)-filetime(j-1)
               timediff=filetime(j)-T1

               IF ( Sparse .EQ. 1 ) then
                 write(*,*) "Sparse = 1 ",Sparse
                 DO l=1,NumNodes
                   us(l,2)=0.0d0  !DefaultValue
                   vs(l,2)=0.0d0  !DefaultValue
                 ENDDO
                 NLOOP = NumNonDefaultNodes
               ELSEIF (Sparse .EQ. 0 ) then
                 write(*,*) "Sparse = 0 ",Sparse
                 NLOOP = NumNodes
               ENDIF

               DO l=1, NLOOP
                 read(10,*,end=9003,err=9003) node, us(node,2), vs(node,2)
               ENDDO
!$OMP        PARALLEL DO DEFAULT (NONE) PRIVATE(L,UDIFF,VDIFF) &
!$OMP&                   SHARED(US,VS,UNEW, VNEW, NUMNODES,TIMEDIFF,TIMEINC)
               DO l=1,NumNodes
                 udiff=us(l,2)-us(l,1)
                 unew(l)=us(l,2)-((timediff*udiff)/timeinc)
                 vdiff=vs(l,2)-vs(l,1)
                 vnew(l)=vs(l,2)-((timediff*vdiff)/timeinc)
               ENDDO
!$OMP        END PARALLEL DO

               numstop=j
               EXIT !goto 150
             ELSE
               IF ( Sparse .EQ. 1 ) then
                 write(*,*) "Sparse = 1 ",Sparse
!$OMP          PARALLEL DO DEFAULT (NONE) PRIVATE(L) SHARED(US,VS,NUMNODES)
                 DO l=1,NumNodes
                   us(l,1)=0.0d0  !DefaultValue
                   vs(l,1)=0.0d0  !DefaultValue
                 ENDDO
!$OMP          END PARALLEL DO
                 NLOOP = NumNonDefaultNodes
               ELSEIF (Sparse .EQ. 0 ) then
                 NLOOP = NumNodes
               ENDIF

               DO l=1, NLOOP
                 read(10,*,end=9004,err=9004) node, us(node,1), vs(node,1)
               ENDDO

             ENDIF ! T1 time conditional
           ENDDO LOOP_NUMTIMES1  ! numtimes
         ELSE ! I not equal to 1
           LOOP_NUMTIMES2:DO j= numstop,numtimes
             IF (j.ne.numstop) THEN
               IF ( Sparse .EQ. 1 ) then
                 READ(UNIT=10,FMT=*,END=9005, ERR=9005) filetime(j),timestep,NumNonDefaultNodes,DefaultValue
                 write(*,*) NumNonDefaultNodes
               ELSEIF (Sparse .EQ. 0 ) then
                 READ(UNIT=10,FMT=*,END=9005, ERR=9005) filetime(j),timestep
               ENDIF
             ENDIF
             if ((T1.ge.filetime(j-1)).and.(T1.le.filetime(j))) then
               timeinc=filetime(j)-filetime(j-1)
               timediff=filetime(j)-T1
               if (j.ne.numstop) then
                 IF ( Sparse .EQ. 1 ) then
                   write(*,*) "Sparse = 1 ",Sparse
!$OMP            PARALLEL DO DEFAULT (NONE) PRIVATE(L) SHARED(US,VS,NUMNODES)
                   DO l=1,NumNodes
                     us(l,2)=0.0d0  !DefaultValue
                     vs(l,2)=0.0d0  !DefaultValue
                   ENDDO
!$OMP            END PARALLEL DO
                   NLOOP = NumNonDefaultNodes
                 ELSEIF (Sparse .EQ. 0 ) then
                   write(*,*) "Sparse = 0 ",Sparse
                   NLOOP = NumNodes
                 ENDIF
                   DO l=1,NLOOP
                     read(10,*,end=9003,err=9003) node, us(node,2), vs(node,2)
                   ENDDO
               endif ! numstop conditional
!$OMP        PARALLEL DO DEFAULT (NONE) PRIVATE(L,UDIFF,VDIFF) &
!$OMP&                   SHARED(US,VS,UNEW, VNEW, NUMNODES,TIMEDIFF,TIMEINC)
               DO l=1,NumNodes
                 udiff=us(l,2)-us(l,1)
                 unew(l)=us(l,2)-((timediff*udiff)/timeinc)
                 vdiff=vs(l,2)-vs(l,1)
                 vnew(l)=vs(l,2)-((timediff*vdiff)/timeinc)
               ENDDO
!$OMP        END PARALLEL DO
               numstop=j
               EXIT !goto 150
             else
!$OMP        PARALLEL DO DEFAULT (NONE) PRIVATE(L) SHARED(US,VS,NUMNODES)
               do l=1,numnodes
                 us(l,1)=us(l,2)
                 vs(l,1)=vs(l,2)
               enddo
!$OMP        END PARALLEL DO
             endif
           enddo LOOP_NUMTIMES2
         endif
!
! LOOP OVER EACH DROGUE
         Write(*,*)" loop through particles and track"
         NTIME = I
!!!$ TIME_TC = TIME_TC - omp_get_wtime()
!$OMP PARALLEL DEFAULT (SHARED) PRIVATE(IMAP,II,XO,YO,JJ,UNEWS,VNEWS,JNEW,DDT)
!$OMP  DO
         DO IMAP = 1,NDR
           II = NOMP_MAP(IMAP)
         !IF THIS ELEMENT WAS ELIMINATED ON A PREVIOUS STEP THEN SKIP
           IF (IDR(II).EQ.0) CYCLE
           !XO, YO ARE THE STARTING POSITIONS
             XO = XDR(II)
             YO = YDR(II)
             JJ = JJDR(II)
             DDT = DT1(II)
           !GET THE COMPONENTS OF FLOW AT XO, YO
             CALL VELS(JJ,XO,YO,UNEWS,VNEWS,T1)
           !TRACK THIS PARTICLE FROM TIME T1 TO T2
             CALL TRACK(JJ,JNEW,XO,YO,UNEWS,VNEWS,T1,T2,DDT,II)
             XDR(II) = XO
             YDR(II) = YO
             JJDR(II) = JNEW
             DT1(II) = DDT
         ENDDO
!$OMP  END DO
!$OMP END PARALLEL
!!!$ TIME_TC = TIME_TC + omp_get_wtime()
         Write(*,*)" done looping through particles and tracking"
!
! *** WRITE OUT THE POSITIONS AT THIS TIME STEP
! *** Section to print output in ACE/vis format
         write(*,*) "write output for this time"

         IF(MOD(I,IPRINT).EQ.0)THEN
           WRITE (12,*) T1,NDR
           DO II = 1,NDR
             WRITE (12,172) II,XDR(II)/REARTH/DEGRAD, &
     &                      YDR(II)/REARTH/DEGRAD,    &
     &                      nint(ZDR(II))
           ENDDO
         ENDIF
         write(*,*) "wrote output for this time"
!
      ! INCREMENT THE LIMITS OF INTEGRATION
         T1 = T1 + STEPP
         T2 = T2 + STEPP
      ENDDO TIMESTEP_LOOP
!!!$ TIME_ALL = TIME_ALL + omp_get_wtime()
!!!$    WRITE(6,*) 'TIME_ALL:', TIME_ALL
!!!$    WRITE(6,*) 'TIME_TC: ', TIME_TC
 

  171 FORMAT ( 2(1x,1e18.9), 1x, i8 )
  172 FORMAT (I8,2x,2(1x,1e18.9),2x,I6)


! *** CLOSE OUTPUT FILE
      CLOSE (UNIT=12)

 9000   WRITE(*,*) "Error reading or end of file while reading velocity"
 9003   write(*,*) "error reading in velocity values"
 9004   write(*,*) "error reading in velocity values"
 9005   write(*,*) "error reading in velocity values"

 END PROGRAM MAIN
!
!***********************************************************************
      SUBROUTINE DEBLNK(STRING,ISTART,IEND)  !st3 never used
!-----------------------------------------------------------------------
! PURPOSE: THIS SUBROUTINE FINDS THE FIRST NONBLANK SPACE (ISTART) IN A 
!            ARBITRARY CHARACTER STRING AND THE FIRST SUBSEQUENT BLANK
!            SPACE (IEND).  THE RANGE OF STRING BETWEEN ISTART AND IEND
!            CAN THEN BE SEPARATED FROM THE REST OF STRING BY SPECIFYING
!            STRING(ISTART:IEND)
!
!
! INPUTS:  STRING - A CHARACTER STRING OF LENGTH .LE. 72
!
! OUTPUTS: ISTART - FIRST NONBLANK CHARACTER IN STRING
!          IEND   - FIRST BLANK CHARACTER IN STRING AFTER ISTART
!
! HISTORY:  WRITTEN BY CHRISTOPHER E. NAIMIE
!           DARTMOUTH COLLEGE
!           12 JUNE 1992
!
!           NAME CHANGED TO DEBLNK BY BRIAN BLANTON, UNC-CH
!           INCLUDED IN OPNML LIBRABY AND TRACKING ALGORITHMS
!           19 JUNE 1995
!-----------------------------------------------------------------------
      IMPLICIT NONE
!
! ARGUMENTS
      CHARACTER*72 STRING
      INTEGER ISTART,IEND,I
!
! START OF EXECUTABLE CODE
!
! FIND BEGINNING OF STRING
      DO I=1,72
         ISTART=I
         IF(STRING(I:I).NE.' ') EXIT
      ENDDO
!
! FIND END OF STRING
      DO I=ISTART,72
         IEND=I-1
          IF(STRING(I:I).EQ.' ') EXIT
      ENDDO
!
! END OF ROUTINE
      RETURN
      END
!
!***********************************************************************
      SUBROUTINE BELEL(J,XP,YP,NFLAG)
!***********************************************************************
!
! *** DETERMINE WHETHER THE POINT (XP,YP) IS WITHIN ELEMENT "J"
! *** (OR ON ITS BOUNDARIES).  IF SO, IND=1; IF NOT, IND=0
!
! *** THIS IS THE CROSS-PRODUCT METHOD OF CHRISTOPHER NAIMIE,
! *** NUMERICAL METHODS LABORATORY, DARTMOUTH COLLEGE, HANOVER NH
!
! PURPOSE: THIS SUBROUTINE DETERMINES IF A POINT (XVAL,YVAL) IS ON A 
!            LOCAL 2-D TRIANGULAR LINEAR ELEMENT
!    
! RESTRICTIONS: APPLICABLE ONLY FOR  2-D TRIANGULAR LINEAR ELEMENT
!
! INPUTS:  J         - CURRENT ELEMENT BEING TESTED
!          XVAL,YVAL - LOCATION OF THE POINT 
!
! OUTPUTS: NFLAG IS SET EQUAL TO 1 IF THE POINT (XVAL,YVAL) IS IN/ON 
!            THE LOCAL ELEMENT J.
!          NFLAG IS SET EQUAL TO 0 IF THE POINT (XVAL,YVAL) IS NOT ON 
!            THE LOCAL ELEMENT J.
!
! HISTORY:  WRITTEN BY CHRISTOPHER E. NAIMIE
!           DARTMOUTH COLLEGE
!           22 APRIL 1992
!
!           IMPLEMENTED IN DROG3DDT BY BRIAN BLANTON
!           UNC-CH
!           10 Aug 1995
!
!-----------------------------------------------------------------------
      USE COMMONS, ONLY: X, Y, ELEMS
      IMPLICIT NONE
!-----------------------------------------------------------------------
!
! *** INCOMING ARGUMENTS
      DOUBLE PRECISION, INTENT(IN) :: XP,YP
      INTEGER,INTENT(IN) :: J
      INTEGER,INTENT(OUT):: NFLAG
!
! *** LOCAL DECLARATIONS
      INTEGER I,K
      DOUBLE PRECISION XLOCAL(3),YLOCAL(3),CROSSPROD
      DOUBLE PRECISION VX(3),VY(3),DELX,DELY,D,THETA
!
! *** EXTRACT LOCAL NODE COORDINATES
      XLOCAL(1)=X(ELEMS(J,1))
      XLOCAL(2)=X(ELEMS(J,2))
      XLOCAL(3)=X(ELEMS(J,3))
      YLOCAL(1)=Y(ELEMS(J,1))
      YLOCAL(2)=Y(ELEMS(J,2))
      YLOCAL(3)=Y(ELEMS(J,3))
!
!  CALCULATE THE X AND Y COMPONENTS OF VECTORS POINTING FROM (XP,YP)
!  TO EACH NODE ON THE ELEMENT
      DO I=1,3
        DELX=XLOCAL(I)-XP
        DELY=YLOCAL(I)-YP
        D=DSQRT(DELX**2.0+DELY**2.0)
        THETA=DATAN2(DELY,DELX)
        VX(I)=D*DCOS(THETA)
        VY(I)=D*DSIN(THETA)
      ENDDO
!
!  DETERMINE IF THE POINT IS ON THE ELEMENT BY CALCULATING THE
!    CROSSPRODUCTS OF NEIGHBORING VECTORS IN A DIRECTION
!    WHICH WILL YIELD ALL NON-NEGATIVE NUMBERS IF X,Y IS ON THE ELEMENT.
!    (IE: V1XV2>OR=0, V2XV3>OR=0, AND V3XV1>OR=0 => X,Y IS ON ELEMENT)
      NFLAG=1
      DO I=1,3
        K=I+1
        IF(I.EQ.3)K=1
        CROSSPROD=VX(I)*VY(K)-VY(I)*VX(K)
        IF(CROSSPROD.LT.0.D0)THEN
          NFLAG=0
          EXIT
        ENDIF
      ENDDO
!
! END OF ROUTINE
      RETURN
      END
!
!***********************************************************************
      SUBROUTINE FNDELE(J,JJ,XX,YY,INDD)
!***********************************************************************
!
! *** FINDS NEW ELEMENT "JJ" WHICH CONTAINS THE POINT (XX,YY).  "J" IS T
! *** OLD ELEMENT.  INDD=1 IF THE NEW ELEMENT IS FOUND; INDD=0 IF IT
! *** CANNOT BE FOUND.
!
!-----------------------------------------------------------------------
      USE COMMONS, ONLY: NMEL, ICEE, ICNE, ELEMS
      IMPLICIT NONE
!-----------------------------------------------------------------------
      DOUBLE PRECISION, INTENT(IN) :: XX,YY
      INTEGER,INTENT(IN) :: J
      INTEGER,INTENT(INOUT) :: JJ
      INTEGER,INTENT(OUT):: INDD
      INTEGER  :: NEC(1000), NECN(1000), NDONE(1000) 
!
      INTEGER idone,i,n,ind,isum,l1,l2,kk,icount,k,nl
!
      INDD = 0

      IDONE = 1
      NDONE(IDONE) = J
!
! *** USE THE LOGIC DESCRIBED IN SUBROUTINE "LOCATE"
! *** NEXT, CHECK NEIGHBORING ELEMENTS
      DO I = 1,3
        N = ICEE(J,I)
        IF (N.EQ.0) CYCLE
        CALL BELEL(N,XX,YY,IND)
        IF (IND.EQ.1) THEN
          JJ = N
          INDD = 1
          RETURN !GO TO 130
        END IF
        IDONE = IDONE + 1
        NDONE(IDONE) = N
      ENDDO
      ISUM = 0
      DO L1 = 1,3
        L2 = ELEMS(J,L1)
        KK_LOOP:DO KK = 1,ICNE(L2,1)
          N = ICNE(L2,KK+1)
          DO I = 1,IDONE
            IF (N.EQ.NDONE(I)) CYCLE KK_LOOP
          ENDDO
          ISUM = ISUM + 1
          IDONE = IDONE + 1
          NDONE(IDONE) = N
          NEC(ISUM) = N
        ENDDO KK_LOOP
      ENDDO
!
! *** PROGRESSIVELY LOOP OVER MORE AND MORE ELEMENTS IN SEARCH OF THE EL
! *** MENT CONTAINING (XX,YY)
      ICOUNT = ISUM
      ISUM = 0
      IF (ICOUNT.EQ.0) THEN
        DO I = 2,IDONE
          ICOUNT = ICOUNT + 1
          NEC(ICOUNT) = NDONE(I)
        ENDDO
      END IF

   INFINI_LOOP: DO
      DO K = 1,ICOUNT
        N = NEC(K)
        CALL BELEL(N,XX,YY,IND)
        IF (IND.EQ.1) THEN
          JJ = N
          INDD = 1
          RETURN !GO TO 130
        END IF
      ENDDO
      IF (IDONE.GT.70) THEN
        INDD = 0
        RETURN !GO TO 130
      END IF
!
! *** ACCUMULATE LIST OF NEW ELEMENTS TO BE CHECKED ON NEXT LOOP
      DO K = 1,ICOUNT
        N = NEC(K)
        DO L1 = 1,3
          L2 = ELEMS(N,L1)
          KK_LOOP2:DO KK = 1,ICNE(L2,1)
            NL = ICNE(L2,KK+1)
            DO I = 1,IDONE
              IF (NL.EQ.NDONE(I)) CYCLE KK_LOOP2
            ENDDO
            ISUM = ISUM + 1
            IDONE = IDONE + 1
            NDONE(IDONE) = NL
            NECN(ISUM) = NL
          ENDDO KK_LOOP2
        ENDDO
      ENDDO

      ICOUNT = ISUM
      IF (ICOUNT.EQ.0) THEN
        INDD = 0
        RETURN !GO TO 130
      END IF

      NEC(1:ICOUNT) = NECN(1:ICOUNT)

      ISUM = 0
   ENDDO INFINI_LOOP

      RETURN
      END

!
!***********************************************************************
      SUBROUTINE RK4(J,JOUT,XX,YY,U,V,T,DT,XOUT,YOUT,KODE)
!***********************************************************************
!
! *** USES 4TH ORDER RUNGE-KUTTA SCHEME TO ADVANCE SOLUTION OVER A TIME
! *** INTERVAL "DT" AND RETURNS THE RESULTING POINT (XOUT,YOUT) AS THE
! *** COORDINATE OF THE ENDPOINT OF THE INTEGRATION STEP.
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER J,JOUT
      DOUBLE PRECISION XX,YY,U,V,T,DT,XOUT,YOUT
      INTEGER indd,jj,KODE,jt,ind
      DOUBLE PRECISION dth,dt6,th,xt,yt,ut,vt
      DOUBLE PRECISION um,vm,u4,v4
!
      KODE = 0
      DTH = DT/2.D0
      DT6 = DT/6.D0
      TH = T + DTH
      JT = J
!
! *** FIRST STEP
      XT = XX + DTH*U
      YT = YY + DTH*V
!
! *** CHECK WHETHER POINT STILL LIES IN ELEMENT "J";  IF NOT, FIND NEW ELEMENT
      CALL BELEL(JT,XT,YT,IND)
      IF (IND.EQ.0) THEN
        CALL FNDELE(JT,JJ,XT,YT,INDD)
        CALL FNDELEls(INDD, JJ, XT, YT)
        IF (INDD.EQ.0) THEN          
          KODE = 1
          XOUT=XT
          YOUT=YT
          RETURN
        END IF
        JT = JJ
      END IF
!
! *** SECOND STEP
      CALL VELS(JT,XT,YT,UT,VT,TH)
      XT = XX + DTH*UT
      YT = YY + DTH*VT
      CALL BELEL(JT,XT,YT,IND)
      IF (IND.EQ.0) THEN
        CALL FNDELE(JT,JJ,XT,YT,INDD)
        CALL FNDELEls(INDD, JJ, XT, YT)
        IF (INDD.EQ.0) THEN
          KODE = 2
          RETURN
        END IF
        JT = JJ
      END IF
!
! *** THIRD STEP
      CALL VELS(JT,XT,YT,UM,VM,TH)
      XT = XX + DT*UM
      YT = YY + DT*VM
      CALL BELEL(JT,XT,YT,IND)
      IF (IND.EQ.0) THEN
        CALL FNDELE(JT,JJ,XT,YT,INDD)
        CALL FNDELEls(INDD, JJ, XT, YT)
        IF (INDD.EQ.0) THEN
          KODE = 3
          RETURN
        END IF
        JT = JJ
      END IF
!
! *** FOURTH STEP
      CALL VELS(JT,XT,YT,U4,V4,T+DT)
!
! *** ACCUMULATE INCREMENTS WITH PROPER WEIGHTS
      XOUT = XX + (U+2* (UT+UM)+U4)*DT6
      YOUT = YY + (V+2* (VT+VM)+V4)*DT6
      CALL BELEL(JT,XOUT,YOUT,IND)
      IF (IND.EQ.0) THEN
        CALL FNDELE(JT,JJ,XOUT,YOUT,INDD)
        CALL FNDELEls(INDD, JJ, XT, YT)
        IF (INDD.EQ.0) THEN
          KODE = 4
          RETURN
        END IF
        JT = JJ
      END IF

      JOUT = JT

      RETURN
      END

!
!***********************************************************************
      SUBROUTINE RKQC(JINOUT,XX,YY,U,V,T,DTTRY,DTNEXT,KODE,ICURBS)
!***********************************************************************
!
!
! *** 5TH ORDER RUNGE-KUTTA STEP WITH MONITORING OF LOCAL 4TH ORDER TRUN
! *** ATION ERROR TO ENSURE ACCURACY AND ADJUST STEP SIZE.  "DTTRY" IS T
! *** STEP SIZE TO BE ATTEMPTED AND "EPS" IS THE REQUIRED ACCURACY.
! *** UPON FINISHING THE INTEGRATION STEP, "XX" AND "YY" CONTAIN THE FIN
! *** TRACKED LOCATION.  "DTNEXT" IS THE ESTIMATED NEXT STEP SIZE.
!
! *** SET MAXIMUM DIMENSIONS HERE.
!-----------------------------------------------------------------------
      USE COMMONS, ONLY:NSEG,IBSEG,IBSEGEL,X,Y,EPS,DTMIN
      IMPLICIT NONE
!-----------------------------------------------------------------------
      DOUBLE PRECISION PGROW,PSHRNK,FCOR,ONE,SAFETY,ERRCON
      PARAMETER (PGROW=-0.2D0,PSHRNK=-0.25D0,FCOR=1.D0/15.D0,ONE=1.D0, &
     &           SAFETY=0.9D0,ERRCON=6.0D-04)
!
! *** NOTE:  ERRCON = (4/SAFETY)**(1/PGROW)
      INTEGER J1HS,J2HS,JBS,JINOUT,JSAV,KODESAV,ICURBS
      DOUBLE PRECISION XX,YY,U,V,T,TH,DTTRY,DTDID,DTNEXT
      DOUBLE PRECISION tsav,usav,vsav,xsav,ysav,dt,dth
      DOUBLE PRECISION xtemp,ytemp,XXHS,YYHS,error
      DOUBLE PRECISION XBSMIN,XBSMAX,XTRKMIN,XTRKMAX,YTRKMIN,YTRKMAX,XI,YI
      DOUBLE PRECISION XB,YB,XE,YE,XIMIN,YIMIN,DIST,DISTMIN
      DOUBLE PRECISION TRKSLOPE,TRKINT,BNDSLOPE,BNDINT
      INTEGER ind,KODE,I,ITRKSLOPEF,IBNDSLOPEF,IELMIN,IMIN

      INTEGER :: icheck

!
! *** SAVE INITIAL VALUES
      JSAV = JINOUT
      TSAV = T
      USAV = U
      VSAV = V
      XSAV = XX
      YSAV = YY
      KODESAV = KODE
!      WRITE(45,*) ' Particle starts at position ',XX,YY
!      WRITE(45,*) ' In element ',JINOUT,' with KODE = ',KODE

!
! *** SET STEP SIZE TO INITIAL TRY VALUE
      DT = DTTRY

!
! *** TAKE TWO HALF STEPS
   INFINI_LOOP: DO   ! 10 CONTINUE

! *** TAKE FIRST HALF STEP
      DTH = DT/2.d0
      CALL RK4(JSAV,J1HS,XSAV,YSAV,USAV,VSAV,TSAV,DTH,XTEMP,YTEMP,KODE)

! *** IF KODE <> 0, TRACKING HAS LEFT THE GRID DURING THE FIRST HALF OF DT.
! *** IF KODE = 1, TRACKING HAS LEFT THE GRID ON THE FIRST STEP. IN THIS CASE 
! ***              DEPENDING ON KODESAV, MOVE TO THE BOUNDARY OR JUST TRACK
! ***              ALONG THE BOUNDARY.
! *** IF KODESAV=-1, PARTICLE IS ALREADY ON THE BOUNDARY, JUST BOUNDARY TRACK.
! *** IF KODESAV=0, PARTICLE IS COMING FROM THE INTERIOR OF THE DOMAIN.  THEREFORE
! ***               MOVE IT TO THE BOUNDARY AND THEN BOUNDARY TRACK.
! *** THE FOLLOWING STEPS ARE USED TO MOVE A PARTICLE TO THE BOUNDARY:
! ***   1. Check to see which boundary segment(s) was (were) crossed
! ***   2. Determine intersection point(s)
! ***   3. If more than one boundary segment was crossed, determine the closest
! ***   4. Put particle at intersection point with closest boundary
! ***   5. Figure out how long it took to get to the boundary and therefore 
! ***      how much of DT is left.  This is done using USAV,VSAV.

      IF((KODE.EQ.1).AND.(KODESAV.EQ.-1)) THEN
!        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
!        WRITE(45,*) ' Particle on boundary left domain in tracking',
!     &              ' segment 1'
        RETURN
      ENDIF
      IMIN = 0
      icheck = 0
      IF((KODE.EQ.1).AND.(KODESAV.NE.-1)) THEN
!        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
!        WRITE(45,*) ' Particle left domain in tracking segment 1'
        DISTMIN=999999999.D0
        XTRKMIN=MIN(XSAV,XTEMP)
        XTRKMAX=MAX(XSAV,XTEMP)
        YTRKMIN=MIN(YSAV,YTEMP)
        YTRKMAX=MAX(YSAV,YTEMP)
        IF((XTEMP-XSAV).NE.0.D0) THEN
          TRKSLOPE=(YTEMP-YSAV)/(XTEMP-XSAV)
          TRKINT=YSAV-TRKSLOPE*XSAV
          ITRKSLOPEF=0
        ELSE
          ITRKSLOPEF=1
        ENDIF

        DO I=1,NSEG
          XB=X(IBSEG(I,1))
          YB=Y(IBSEG(I,1))
          XE=X(IBSEG(I,2))
          YE=Y(IBSEG(I,2))

!         initially check if particle is near the boundary segment
          IF((XB.LT.XTRKMIN).AND.(XE.LT.XTRKMIN)) CYCLE
          IF((XB.GT.XTRKMAX).AND.(XE.GT.XTRKMAX)) CYCLE
          IF((YB.LT.YTRKMIN).AND.(YE.LT.YTRKMIN)) CYCLE
          IF((YB.GT.YTRKMAX).AND.(YE.GT.YTRKMAX)) CYCLE

!         slope and intercept of boundary segment
          IF((XE-XB).NE.(0.D0)) THEN
            BNDSLOPE=(YE-YB)/(XE-XB)
            BNDINT=YB-BNDSLOPE*XB
            IBNDSLOPEF=0
          ELSE
            IBNDSLOPEF=1
          ENDIF

!         track and boundary segment parallel
          IF(TRKSLOPE.EQ.BNDSLOPE) CYCLE
!         compute intersection point of track line and boundary segment
!         and check if it falls on both the track line and boundary segment
!         keep track of minimum along track distance to the boundary.
          IF(TRKSLOPE.NE.BNDSLOPE) THEN
            IF((ITRKSLOPEF.NE.1).AND.(IBNDSLOPEF.NE.1)) THEN
              XI=(BNDINT-TRKINT)/(TRKSLOPE-BNDSLOPE)
              YI=BNDSLOPE*XI+BNDINT
            ENDIF
            IF(ITRKSLOPEF.EQ.1) THEN
              XI=XSAV
              YI=BNDSLOPE*XI+BNDINT
            ENDIF
            IF(IBNDSLOPEF.EQ.1) THEN
              XI=XB
              YI=TRKSLOPE*XI+TRKINT
            ENDIF
            XBSMIN=MIN(XB,XE)
            XBSMAX=MAX(XB,XE)
            IF((XI.GE.XBSMIN).AND.(XI.LE.XBSMAX).AND.(XI.GE.XTRKMIN)   &
     &                       .AND.(XI.LE.XTRKMAX)) THEN   
              DIST=SQRT((XI-XSAV)*(XI-XSAV)+(YI-YSAV)*(YI-YSAV))
              icheck = icheck + 1
              IF(DIST.LT.DISTMIN) THEN
                XIMIN=XI
                YIMIN=YI
                DISTMIN=DIST
                IELMIN=IBSEGEL(I)
                IMIN=I
              ENDIF
            ENDIF
          ENDIF
        ENDDO

        IF( IMIN == 0 ) then !CHECK NOT FOUND BOUNDARY SEGMENT
          IELMIN = 0
          write(6,*) "TAIHEN", I, NSEG, JINOUT
        ENDIF
 

!       move particle to boundary
        XX = XIMIN
        YY = YIMIN
        JINOUT=IELMIN
        ICURBS=IMIN

!       compute time it took particle to get there assuming velocity is
!       stationary in time and space over this interval
        DTDID = DISTMIN/sqrt(USAV*USAV+VSAV*VSAV)
        DTNEXT = DT
        T=TSAV+DTDID

!        WRITE(45,*) ' Particle stops at position ',XX,YY
!        WRITE(45,*) ' In element ',JINOUT
!        WRITE(45,*) ' Time advanced to ',T
!        WRITE(45,*) ' '
        RETURN
      END IF !from IF((KODE.EQ.1).AND.(KODESAV.NE.-1)) THEN

! *** IF KODE = 2, TRACKING HAS LEFT THE GRID ON THE SECOND STEP. IN THIS CASE 
! *** CUT DOWN THE TRACKING TIME STEP SIZE.
      IF (KODE.EQ.2) THEN
!        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
!        WRITE(45,*) ' Particle left domain in tracking segment 2'
!        WRITE(45,*) ' '
        DT=DT/4.d0                                               
        CYCLE INFINI_LOOP  !GO TO 10
      END IF

! *** IF KODE = 3 or 4, TRACKING HAS LEFT THE GRID ON THE 3rd or 4th STEP.
! *** CUT DOWN THE TRACKING TIME STEP SIZE.
      IF (KODE.EQ.3 .OR. KODE.EQ.4) THEN
!        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
!        WRITE(45,*) ' Particle left domain in tracking segment 3 or 4'
!        WRITE(45,*) ' '
        DT=DT/2.d0
        CYCLE INFINI_LOOP  !GO TO 10
      END IF
  
! *** TRACKING REMAINED IN THE GRID OVER THE FIRST HALF STEP, NOW TAKE
! *** SECOND HALF STEP.
!      WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
!      WRITE(45,*) ' First half step succeeded'
!      WRITE(45,*) ' '

      TH=TSAV+DTH
      CALL VELS(J1HS,XTEMP,YTEMP,U,V,TH)
      CALL RK4(J1HS,J2HS,XTEMP,YTEMP,U,V,TH,DTH,XXHS,YYHS,KODE)

! *** IF KODE <> 0, TRACKING HAS LEFT THE GRID DURING THE SECOND HALF OF DT.
! *** CUT DOWN THE TRACKING STEP SIZE.
      IF (KODE.NE.0) THEN
!        WRITE(45,*) ' At time ',T,' with time step size ',DT
!        WRITE(45,*) ' Particle left domain in 2nd half step'
!        WRITE(45,*) ' '
        DT = DT/2.d0
        CYCLE INFINI_LOOP  !GO TO 10
      END IF

! *** TRACKING REMAINED IN THE GRID OVER BOTH HALF STEPS. NOW TAKE 
! *** THE LARGE STEP
!      WRITE(45,*) ' At time ',T,' with time step size ',DT
!      WRITE(45,*) ' Second half step succeeded'
!      WRITE(45,*) ' '
      CALL RK4(JSAV,JBS,XSAV,YSAV,USAV,VSAV,TSAV,DT,XTEMP,YTEMP,KODE)

! *** IF KODE <> 0, TRACKING HAS LEFT THE GRID DURING THE LARGE STEP.
! *** CUT DOWN THE TRACKING STEP SIZE.
      IF (KODE.NE.0) THEN
!        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
!        WRITE(45,*) ' Particle left domain in big step'
!        WRITE(45,*) ' '
        DT = DT/2.d0
        CYCLE INFINI_LOOP  !GO TO 10
      END IF

!      WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
!      WRITE(45,*) ' Big step succeeded'
!      WRITE(45,*) ' '

      ERROR = DSQRT((YYHS-YTEMP)**2+ (XXHS-XTEMP)**2)
!
! *** SCALE ERROR RELATIVE TO REQUIRED TOLERANCE
      ERROR = ERROR/EPS

      IF (ERROR.GT.ONE .AND. ABS(DT).GE.DTMIN) THEN
!
! ***   TRUNCATION ERROR TOO LARGE, REDUCE STEP SIZE AND TRY AGAIN
        DT = SAFETY*DT* (ERROR**PSHRNK)
!        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
!        WRITE(45,*) ' Truncation error too large'
!        WRITE(45,*) ' '
        CYCLE INFINI_LOOP  !GO TO 10
      ELSE
!
! ***   STEP SUCCEEDED;  COMPUTE (ESTIMATE) SIZE OF NEXT STEP
        T = TSAV + DT
!        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
!        WRITE(45,*) ' Step succeeded'

        IF (ERROR.GT.ERRCON) THEN
          DTNEXT = SAFETY*DT* (ERROR**PGROW)
        ELSE
          DTNEXT = 4.0*DT
        END IF
        IF(DT.LT.DTMIN) DT=DTMIN
      END IF
      EXIT
   ENDDO INFINI_LOOP

      XX=XXHS
      YY=YYHS

! *** ADD IN TRUNCATION ERROR TO INCREASE 4TH ORDER TO 5TH ORDER ACCURACY
      XX = XX + (XX-XTEMP)*FCOR
      YY = YY + (YY-YTEMP)*FCOR

! *** MAKE SURE THAT THE POINT (XX,YY) IS STILL IN ELEMENT "J2HS".  IF NOT,
! *** DONT INCREASE TO 5TH ORDER.
      CALL BELEL(J2HS,XX,YY,IND)
      IF (IND.EQ.0) THEN
        XX = XXHS
        YY = YYHS
      END IF

      JINOUT=J2HS
      
!      WRITE(45,*) ' Particle stops at position ',XX,YY
!      WRITE(45,*) ' In element ',JINOUT
!      WRITE(45,*) ' Time advanced to ',T
!      WRITE(45,*) ' '

      RETURN
      END

!
!***********************************************************************
      SUBROUTINE BOUNTRK(JINOUT,XX,YY,U,V,T,T2,KODE,ICURBS)
!***********************************************************************
!
! *** TRACK A PARTICLE ALONG A BOUNDARY SEGMENT.
! *** "T2-T" IS THE TIME STEP SIZE TO BE ATTEMPTED.
! *** UPON FINISHING THE INTEGRATION STEP, "XX" AND "YY" CONTAIN THE 
! *** FINAL TRACKED LOCATION.
! ***
! *** THIS TRACKING IS DONE BY:
! *** 1. Computing the tangential velocity at the particle position
! *** 2. Determining which boundary node is "downstream"
! *** 3. Using average of velocities computed in 1. and 2., determine whether 
! ***    particle would make it to the node.
! *** 4. IF the answer in 3. is no, move particle along boundary until DT
! ***    is used up.  Remember that particle is on boundary and return.
! *** 5. IF the answer in 3. is yes, put particle at downstream boundary 
! ***    node and then figure how much of DT is left.  Then return and 
! ***    see if particle will leave boundary or continue to move along the
! ***    next boundary segment.

!
! *** SET MAXIMUM DIMENSIONS HERE.
!-----------------------------------------------------------------------
      USE COMMONS, ONLY:IBSEG,ISEGP,ISEGF,IBSEGEL,X,Y
      IMPLICIT NONE
!-----------------------------------------------------------------------

      INTEGER JINOUT,JSAV
      DOUBLE PRECISION XX,YY,U,V,T,DTDID,DTTRY,T2
      DOUBLE PRECISION tsav,usav,vsav,xsav,ysav
      DOUBLE PRECISION DELXE,DELYE,DELXB,DELYB,DELTRK,XM,YM,UM,VM
      DOUBLE PRECISION XB,YB,XE,YE,DISTSEG,DIST,COSTHETA,SINTHETA
      DOUBLE PRECISION UTANXX,VTANYY,UTANXM,VTANYM,UTANAVG,VTANAVG
      DOUBLE PRECISION SPDTANXX,SPDTANXM
      INTEGER KODE,ICURBS
!
!
! *** SAVE INITIAL VALUES
      JSAV = JINOUT
      TSAV = T
      USAV = U
      VSAV = V
      XSAV = XX
      YSAV = YY
      DTTRY=T2-T
!      WRITE(45,*) ' In subroutine BOUNTRK '
!      WRITE(45,*) ' Particle starts at position ',XX,YY
!      WRITE(45,*) ' In element ',JINOUT

!
! *** SET UP BOUNDARY SEGMENT
      XB=X(IBSEG(ICURBS,1))
      YB=Y(IBSEG(ICURBS,1))
      XE=X(IBSEG(ICURBS,2))
      YE=Y(IBSEG(ICURBS,2))
      DELXE=XE-XX
      DELYE=YE-YY
      DELXB=XB-XX
      DELYB=YB-YY
!
! *** COMPUTE VELOCITY TANGENTIAL TO BOUNDARY AT XX,YY AND TIME T
      DISTSEG=SQRT((XE-XB)*(XE-XB)+(YE-YB)*(YE-YB))
      COSTHETA=(XE-XB)/DISTSEG
      SINTHETA=(YE-YB)/DISTSEG
      UTANXX=USAV*COSTHETA*COSTHETA+VSAV*SINTHETA*COSTHETA
      VTANYY=USAV*COSTHETA*SINTHETA+VSAV*SINTHETA*SINTHETA
      SPDTANXX=SQRT(UTANXX*UTANXX+VTANYY*VTANYY)

! *** If this velocity = 0, particle does not move.
      IF((UTANXX.EQ.0.D0).AND.(VTANYY.EQ.0.D0)) THEN
        T=T2
!        WRITE(45,*) ' UTANXX and VTANYY = 0 '
!        WRITE(45,*) ' Particle stuck at boundary node'
!        WRITE(45,*) ' Time advanced to ',T
        RETURN
      ENDIF
!      
! *** DETERMINE WHICH END OF BOUNDARY PARTICLE IS MOVING TOWARD
      XM=0.D0
      YM=0.D0

! *** If at one end of boundary segment (XE,YE), first check to see if 
! *** particle is moving toward the other end (XB,YB).  IF not, check to 
! *** see if particle is moving to other end of adjacent boundary segment.
! *** If it is moving in neither direction, it is temporarily stuck in 
! *** present position.
      IF((DELXE.EQ.0.D0).AND.(DELYE.EQ.0.D0)) THEN
        IF((DELXB*UTANXX.GE.0.D0).AND.(DELYB*VTANYY.GE.0.D0)) THEN
          XM=XB
          YM=YB
          GOTO 10
        ENDIF
        XE=X(IBSEG(ISEGF(ICURBS),2))
        YE=Y(IBSEG(ISEGF(ICURBS),2))
        DELXE=XE-XX
        DELYE=YE-YY
        DISTSEG=SQRT(DELXE*DELXE+DELYE*DELYE)
        COSTHETA=DELXE/DISTSEG
        SINTHETA=DELYE/DISTSEG
        UTANXX=USAV*COSTHETA*COSTHETA+VSAV*SINTHETA*COSTHETA
        VTANYY=USAV*COSTHETA*SINTHETA+VSAV*SINTHETA*SINTHETA
        SPDTANXX=SQRT(UTANXX*UTANXX+VTANYY*VTANYY)
        IF((UTANXX.EQ.0.D0).AND.(VTANYY.EQ.0.D0)) THEN
          T=T2
!         WRITE(45,*) ' UTANXX and VTANYY = 0 '
!         WRITE(45,*) ' Particle stuck at boundary node'
!         WRITE(45,*) ' Time advanced to ',T
          RETURN
        ENDIF
        IF((DELXE*UTANXX.GE.0.D0).AND.(DELYE*VTANYY.GE.0.D0)) THEN
          XM=XE
          YM=YE
          ICURBS=ISEGF(ICURBS)
          JINOUT=IBSEGEL(ICURBS)
          GOTO 10
        ENDIF
        T=T2
!        WRITE(45,*) ' Particle stuck at boundary node'
!        WRITE(45,*) ' Time advanced to ',T
        RETURN
      ENDIF

! *** If at one end of boundary segment (XB,YB), first check to see if 
! *** particle is moving toward the other end (XE,YE).  IF not, check to 
! *** see if particle is moving to other end of adjacent boundary segment.
! *** If it is moving in neither direction, it is temporarily stuck in 
! *** present position.
      IF((DELXB.EQ.0.D0).AND.(DELYB.EQ.0.D0)) THEN
        IF((DELXE*UTANXX.GE.0.D0).AND.(DELYE*VTANYY.GE.0.D0)) THEN
          XM=XE
          YM=YE
          GOTO 10
        ENDIF
        XB=X(IBSEG(ISEGP(ICURBS),1))
        YB=Y(IBSEG(ISEGP(ICURBS),1))
        DELXB=XB-XX
        DELYB=YB-YY
        DISTSEG=SQRT(DELXB*DELXB+DELYB*DELYB)
        COSTHETA=DELXB/DISTSEG
        SINTHETA=DELYB/DISTSEG
        UTANXX=USAV*COSTHETA*COSTHETA+VSAV*SINTHETA*COSTHETA
        VTANYY=USAV*COSTHETA*SINTHETA+VSAV*SINTHETA*SINTHETA
        SPDTANXX=SQRT(UTANXX*UTANXX+VTANYY*VTANYY)
        IF((UTANXX.EQ.0.D0).AND.(VTANYY.EQ.0.D0)) THEN
          T=T2
!          WRITE(45,*) ' UTANXX and VTANYY = 0 '
!          WRITE(45,*) ' Particle stuck at boundary node'
!          WRITE(45,*) ' Time advanced to ',T
          RETURN
        ENDIF
        IF((DELXB*UTANXX.GE.0.D0).AND.(DELYB*VTANYY.GE.0.D0)) THEN
          XM=XB
          YM=YB
          ICURBS=ISEGP(ICURBS)
          JINOUT=IBSEGEL(ICURBS)
          GOTO 10
        ENDIF
        T=T2
!        WRITE(45,*) ' Particle stuck at boundary node'
!        WRITE(45,*) ' Time advanced to ',T
        RETURN
      ENDIF

! *** If particle is at neither end of the boundary segment, check to
! *** see if it is moving toward XE,YE.
      IF((DELXE*UTANXX.GE.0.D0).AND.(DELYE*VTANYY.GE.0.D0)) THEN
        XM=XE
        YM=YE
        GOTO 10
      ENDIF

! *** If particle is at neither end of the boundary segment, check to
! *** see if it is moving toward XB,YB.
      IF((DELXB*UTANXX.GE.0.D0).AND.(DELYB*VTANYY.GE.0.D0)) THEN
        XM=XB
        YM=YB
      ENDIF

  10  CONTINUE 

! *** If XM,YM have not been set, could not figure which direction particle
! *** is moving.
      IF((XM.EQ.0.D0).AND.(YM.EQ.0.D0)) THEN
        T=T2
!        WRITE(45,*) ' COULD NOT FIGURE WHICH WAY PARTICLE IS MOVING',
!     &               ' IN BOUNTRK'
!        WRITE(45,*) ' Particle stuck '
!        WRITE(45,*) ' UTANXX,VTANYY = ',UTANXX,VTANYY
!        WRITE(45,*) ' Time advanced to ',T
        RETURN
      ENDIF
!
! *** DETERMINE VELOCITY AT END OF BOUNDARY SEGMENT PARTICLE IS MOVING TOWARD
      CALL VELS(JINOUT,XM,YM,UM,VM,T)
      UTANXM=UM*COSTHETA*COSTHETA+VM*SINTHETA*COSTHETA
      VTANYM=UM*COSTHETA*SINTHETA+VM*SINTHETA*SINTHETA
      SPDTANXM=SQRT(UTANXM*UTANXM+VTANYM*VTANYM)
!
! *** ASSUMING VELOCITY IS STATIONARY IN TIME BUT VARYING IN SPACE, COMPUTE 
! *** TRACK LENGTH
      DIST=SQRT((XM-XX)*(XM-XX)+(YM-YY)*(YM-YY))
      DELTRK=2.D0*SPDTANXX*DTTRY*DIST/(2.D0*DIST-DTTRY*(SPDTANXM-SPDTANXX))
!
! *** IF PARTICLE OVER SHOOTS END OF BOUNDARY SEGMENT, PUT IT AT THE END
      IF((DELTRK.GT.DIST).OR.(DIST.EQ.0.D0)) THEN
        XX=XM
        YY=YM
        DTDID=DTTRY*DIST/DELTRK
        T=TSAV+DTDID
        KODE=-1
!        WRITE(45,*) ' Particle moved along boundary to node ',XX,YY
!        WRITE(45,*) ' In element ',JINOUT,' KODE= ',KODE
!        WRITE(45,*) ' Time advanced to ',T
        RETURN
      ENDIF
!
! *** IF PARTICLE DIDNT OVER SHOOT THE END OF THE BOUNDARY SEGMENT, MOVE IT
! *** THE APPROPRIATE DISTANCE ALONG THE BOUNDARY
      UTANAVG=(DELTRK*(UTANXM-UTANXX)/DIST + 2.D0*UTANXX)/2.D0
      VTANAVG=(DELTRK*(VTANYM-VTANYY)/DIST + 2.D0*VTANYY)/2.D0
      XX=DTTRY*UTANAVG+XSAV
      YY=DTTRY*VTANAVG+YSAV
      T=TSAV+DTTRY
      KODE=-1
!      WRITE(45,*) ' Particle moved along boundary to position ',XX,YY
!      WRITE(45,*) ' In element ',JINOUT,' KODE= ',KODE
!      WRITE(45,*) ' Time advanced to ',T
!      WRITE(45,*) ' '

      RETURN
      END
!***********************************************************************
!
!***********************************************************************
      SUBROUTINE TRACK(JEL,J,XSTART,YSTART,USTART,VSTART,T1,T2,DT1,IPN)
!***********************************************************************
!
!
! *** 5TH ORDER RUNGE-KUTTA TRACKING OF "X" AND "Y" OVER THE TIME INTERV
! *** [T1 TO T2] USING ADAPTIVE (VARIABLE) TRACKING TIME STEP SIZE.  FOR
! *** FURTHER DETAILS ON THE GENERAL METHODOLOGY USED IN THIS ALGORITHM,
! *** REFER TO SECTIONS 15.1 AND 15.2 OF "NUMERICAL RECIPES", BY PRESS,
! *** ET. AL.  X AND Y ARE INTEGRATED FROM "XSTART" AND "YSTART", RESPEC
! *** WITH A USER SPECIFIED ACCURACY, "EPS".  "DT1" IS THE GUESSED FIRST
! *** STEPSIZE (MAGNITUDE) AND "DTMIN" IS THE MINIMUM ALLOWED STEPSIZE
! *** (THIS CAN EQUAL ZERO).  UPON COMPLETION OF THE INTEGRATIONS, "XSTART" 
! *** AND "YSTART" ARE REPLACED WITH THE FINAL
! *** VALUES FOR "X" AND "Y".  SUBROUTINE "RKQC" IS THE STEPPER ROUTINE.
! *** NOTE:  T2 > T1  AND  T2 < T1  ARE ALLOWED (I.E., FORWARD AND BACKW
! *** TRACKING).
!
! *** SET MAXIMUM DIMENSIONS HERE.
!-----------------------------------------------------------------------
      USE COMMONS, ONLY:DTMIN,KODE,ICURBS
      IMPLICIT NONE
!-----------------------------------------------------------------------

      INTEGER MAXSTP
      DOUBLE PRECISION ZERO,TEST
      PARAMETER (MAXSTP=1000,ZERO=0.D0)
      PARAMETER (TEST=1.0D-03)
      INTEGER JEL,J,IPN
      DOUBLE PRECISION XSTART,YSTART,USTART,VSTART,T1,T2,DT1
      DOUBLE PRECISION xx,yy,t,dt,u0,v0,dtnext
      INTEGER icount,nstp
!

      XX = XSTART
      YY = YSTART
      T = T1
      DT = SIGN(DT1,T2-T1)
      J = JEL
      ICOUNT = 0
      U0 = USTART
      V0 = VSTART

      DO NSTP = 1,MAXSTP
        IF (NSTP.NE.1) CALL VELS(J,XX,YY,U0,V0,T)
!
! ***   CHECK WHETHER STEP SIZE WILL RESULT IN OVERSHOOTING END OF
! ***   INTEGRATION INTERVAL (T2).  IF SO, CUT DOWN STEP SIZE.
        IF ((T+DT-T2)* (T+DT-T1).GT.ZERO) DT = T2 - T
!
! ***   IF PARTICLE IS IN THE INTERIOR OF THE DOMAIN PERFORM A 5TH
! ***   ORDER RUNGE-KUTTA STEP.
! ***   IF PARTICLE IS ON THE BOUNDARY, TRACK IT THERE
        IF(KODE(IPN).LE.0) THEN
          CALL RKQC(J,XX,YY,U0,V0,T,DT,DTNEXT,KODE(IPN),ICURBS(IPN))
        ELSE
          CALL BOUNTRK(J,XX,YY,U0,V0,T,T2,KODE(IPN),ICURBS(IPN))
        ENDIF

! ***   IF INTEGRATION IS COMPLETE, SAVE POSITION AND SAVE RECENT
! ***   STEP SIZE AS ESTIMATE FOR NEXT TRACKING
        IF ((T-T2)* (T2-T1).GE.ZERO) THEN
          XSTART = XX
          YSTART = YY
          DT1 = DTNEXT
          RETURN
        END IF

        IF (ABS(DTNEXT).LT.DTMIN) THEN
          DTNEXT = SIGN(DTMIN,T2-T1)
        END IF
!
! ***   SET NEXT STEP SIZE
          DT = DTNEXT
      ENDDO

      PRINT *,'TOO MANY TRACKING STEPS; ',MAXSTP,' TAKEN AT TIME = ',T
      PRINT *,'** PARTICLE ELIMINATED **'

      RETURN
      END
!
!************************************************************************
      SUBROUTINE VELS(J,X,Y,UF,VF,T)
!************************************************************************
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: J
      DOUBLE PRECISION, INTENT(IN) :: X, Y, T
      DOUBLE PRECISION, INTENT(INOUT):: UF, VF
      DOUBLE PRECISION               :: UFUNTIME, VFUNTIME

      UF = UFUNTIME(J,X,Y,T)
      VF = VFUNTIME(J,X,Y,T)

      RETURN
      END

!
!***********************************************************************
       DOUBLE PRECISION FUNCTION UFUNTIME(J,X,Y,T)
!     THIS FUNCTION IS USED IF TIME-SERIES VELOCITY FILES ARE USED
!***********************************************************************
!
!       DETERMINES X COMPONENT OF VELOCITY AT (X,Y)
!
! ***   SET MAXIMUM ARRAY DIMENSIONS
!-----------------------------------------------------------------------
      USE COMMONS, ONLY:ELEMS,A,B,A0,AR,UNEW
      IMPLICIT NONE
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION A03,ARI
      INTEGER J
      INTEGER N1,N2,N3
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,F1,F2,F3,F4,F5,F6
      DOUBLE PRECISION X,Y,T

      UFUNTIME = 0.D0

      N1 = ELEMS(J,1)
      N2 = ELEMS(J,2)
      N3 = ELEMS(J,3)
      A03 = AR(J) - A0(J,1) - A0(J,2)
      ARI = 0.5d0/AR(J)
      U1 = UNEW(N1)
      U2 = UNEW(N2)
      U3 = UNEW(N3)
      U4 = UNEW(N1)
      U5 = UNEW(N2)
      U6 = UNEW(N3)
      F1 = ARI* (B(J,1)*U1+B(J,2)*U2+B(J,3)*U3)
      F4 = ARI* (B(J,1)*U4+B(J,2)*U5+B(J,3)*U6)
      F2 = ARI* (A(J,1)*U1+A(J,2)*U2+A(J,3)*U3)
      F5 = ARI* (A(J,1)*U4+A(J,2)*U5+A(J,3)*U6)
      F3 = 2*ARI* (A0(J,1)*U1+A0(J,2)*U2+A03*U3)
      F6 = 2*ARI* (A0(J,1)*U4+A0(J,2)*U5+A03*U6)

      UFUNTIME = UFUNTIME + (F1*X+F2*Y+F3)
      UFUNTIME = UFUNTIME + (F4*X+F5*Y+F6)

      RETURN
      END

!
!***********************************************************************
      DOUBLE PRECISION FUNCTION VFUNTIME(J,X,Y,T)
!     THIS FUNCTION IS USED IF TIME-SERIES VELOCITY FILES ARE USED
!***********************************************************************
!
!       COMPUTES Y COMPONENT OF VELOCITY AT (X,Y)
!
! ***   SET MAXIMUM ARRAY DIMENSIONS
!-----------------------------------------------------------------------
      USE COMMONS, ONLY:ELEMS,A,B,A0,AR,VNEW
      IMPLICIT NONE
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION A03,ARI
      INTEGER J
      INTEGER N1,N2,N3
      DOUBLE PRECISION V1,V2,V3,V4,V5,V6,G1,G2,G3,G4,G5,G6
      DOUBLE PRECISION X,Y,T

      VFUNTIME = 0.D0

      N1 = ELEMS(J,1)
      N2 = ELEMS(J,2)
      N3 = ELEMS(J,3)
      A03 = AR(J) - A0(J,1) - A0(J,2)
      ARI = 0.5d0/AR(J)
      V1 = VNEW(N1)
      V2 = VNEW(N2)
      V3 = VNEW(N3)
      V4 = VNEW(N1)
      V5 = VNEW(N2)
      V6 = VNEW(N3)
      G1 = ARI* (B(J,1)*V1+B(J,2)*V2+B(J,3)*V3)
      G4 = ARI* (B(J,1)*V4+B(J,2)*V5+B(J,3)*V6)
      G2 = ARI* (A(J,1)*V1+A(J,2)*V2+A(J,3)*V3)
      G5 = ARI* (A(J,1)*V4+A(J,2)*V5+A(J,3)*V6)
      G3 = 2*ARI* (A0(J,1)*V1+A0(J,2)*V2+A03*V3)
      G6 = 2*ARI* (A0(J,1)*V4+A0(J,2)*V5+A03*V6)

      VFUNTIME = VFUNTIME + (G1*X+G2*Y+G3)
      VFUNTIME = VFUNTIME + (G4*X+G5*Y+G6)

      RETURN
      END

!!st3*******************************************************************
      SUBROUTINE MAKE_STAB( NMEL, ELEMS, NMND, X, Y)
!     MAKE SEARCHING TABLE
!!st3*******************************************************************
!-----------------------------------------------------------------------
      USE LATTICE_TABLE
!-----------------------------------------------------------------------
      INTEGER, INTENT(IN) :: NMEL, ELEMS(NMEL,3), NMND
      DOUBLE PRECISION,  INTENT(IN) :: X(NMND), Y(NMND)
!
!     NE_PIECE(:,:) :      Total number of elements in PIECE(IX,IY)
!     NE_PIECE_LIST(:):    Element number list of PEICE(IX,IY)
!     NE_PIECE_INDEX(:,:): Index of NE_PIECE_LIST for PIECE(IX,IY)
!          *This index is required for Commpressing of Strage of LIST
!
      INTEGER :: N, M, I, J, IX(3), IY(3), ISTART
      DOUBLE PRECISION  :: XMAX(2)
!
      ALLOCATE( NE_PIECE_LIST(NMEL*2) )
!
      XMAX(1) = MAXVAL(X(1:NMND))
      XMIN(1) = MINVAL(X(1:NMND))
      XMAX(2) = MAXVAL(Y(1:NMND))
      XMIN(2) = MINVAL(Y(1:NMND))
      DO I = 1, 2
        XMAX(I) = XMAX(I) + 1.0d0
        DX(I) = ( XMAX(I) - XMIN(I) ) / DBLE(NDIV)
      ENDDO
!
! SEARCH PIECE INDEX
      NE_PIECE(0:NDIV+1,0:NDIV+1) = 0
      DO M = 1, NMEL
        DO J = 1, 3
          N = ELEMS(M,J)
          IX(J) = INT( (X(N)-XMIN(1)) / DX(1) ) + 1
          IY(J) = INT( (Y(N)-XMIN(2)) / DX(2) ) + 1
        ENDDO
        DO I = MINVAL(IX(1:3)), MAXVAL(IX(1:3))
          DO J = MINVAL(IY(1:3)), MAXVAL(IY(1:3))
            NE_PIECE(I,J) = NE_PIECE(I,J) + 1
          ENDDO
        ENDDO
      ENDDO
      ISTART = 0
      DO I = 1, NDIV
        DO J = 1, NDIV
          NE_PIECE_INDEX(I,J) = ISTART
          ISTART = ISTART + NE_PIECE(I,J)
        ENDDO
      ENDDO
!
! MAKE PIECE TABLE
      NE_PIECE(1:NDIV,1:NDIV) = 0
      DO M = 1, NMEL
        DO J = 1, 3
          N = ELEMS(M,J)
          IX(J) = INT( (X(N)-XMIN(1)) / DX(1) ) + 1
          IY(J) = INT( (Y(N)-XMIN(2)) / DX(2) ) + 1
        ENDDO
        DO I = MINVAL(IX(1:3)), MAXVAL(IX(1:3))
          DO J = MINVAL(IY(1:3)), MAXVAL(IY(1:3))
!           Count Total number of elements in PIECE(I,J)
            NE_PIECE(I,J) = NE_PIECE(I,J) + 1
!           Store Element number in PIECE(I,J)
            NE_PIECE_LIST(NE_PIECE(I,J)+NE_PIECE_INDEX(I,J)) = M
          ENDDO
        ENDDO
      ENDDO
!
      RETURN
      END
!--------------------------------------------------------------------------------
      SUBROUTINE FNDELEls(INDD, JJ, XT, YT)
! *** FINDS NEW ELEMENT "JJ" WHICH CONTAINS THE POINT (XX,YY). USING Lattice Search
!--------------------------------------------------------------------------------
      USE LATTICE_TABLE
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: INDD, JJ
      DOUBLE PRECISION,  intent(in):: XT, YT
      INTEGER :: IX, IY, J, I, IND
!
! Quick TURN 
      IF( INDD == 1 ) RETURN  !IF JJ was found in FNDELE

      IX = INT( (XT-XMIN(1))/DX(1) ) + 1
      IY = INT( (YT-XMIN(2))/DX(2) ) + 1
      IX = MAX(0,IX); IX = MIN(NDIV+1,IX)
      IY = MAX(0,IY); IY = MIN(NDIV+1,IY)
      DO J = 1, NE_PIECE(IX,IY)
        I = NE_PIECE_LIST(J+NE_PIECE_INDEX(IX,IY))
        CALL BELEL(I,XT,YT,IND)
        IF (IND.EQ.1) THEN
          JJ = I
          INDD = 1
          EXIT
        END IF
      ENDDO

      RETURN
      END
!--------------------------------------------------------------------------------
!     MAKE Node-Element Information Table
      SUBROUTINE MAK_NEINFO
!--------------------------------------------------------------------------------
      USE COMMONS, ONLY: NMEL, NMND, ELEMS, ICEE, ICNE, &
     &                   NSEG, IBSEG, ISEGP, ISEGF, IBSEGEL
      IMPLICIT NONE
      INTEGER:: I, J, M, N, I1, I2, M1, M2, N1, N2, N3, ISEG, maxne
      INTEGER,ALLOCATABLE :: NPROP(:)
      INTEGER, PARAMETER:: MAP(4) = (/1, 2, 3, 1 /)
!
      ALLOCATE(NPROP(1:NMND))
!
!!   NODE-ELEMENT ADJACENCY INFORMATION
      NPROP(1:NMND) = 0
      DO M = 1, NMND
        DO J = 1, 3
          NPROP(ELEMS(M,J)) = NPROP(ELEMS(M,J)) + 1
        ENDDO
      ENDDO
      maxne = maxval(NPROP(1:NMND)) + 1
      ALLOCATE( ICEE(1:NMEL,1:3), ICNE(1:NMND,1:maxne) )
      ICNE(1:NMND,1:maxne) = 0
      DO M = 1, NMEL
        DO J = 1, 3
          ICNE(ELEMS(M,J),1) = ICNE(ELEMS(M,J),1) + 1
          ICNE(ELEMS(M,J),ICNE(ELEMS(M,J),1)+1) = M
        ENDDO
      ENDDO
!
!!   ELEMENT-ELEMENT ADJACENCY INFORMATION
!       1
!       |\
!    (1)| \(3)
!       |__\
!      2 (2)3
      ICEE(1:NMEL,1:3) = 0
      DO M = 1, NMEL
        NM_LOOP:DO J = 1, 3
          DO I1 = 1, ICNE(ELEMS(M,MAP(J)),1)
            M1 = ICNE(ELEMS(M,MAP(J)),I1+1)
            IF( M1 == M ) CYCLE
            DO I2 = 1, ICNE(ELEMS(M,MAP(J+1)),1)
              M2 = ICNE(ELEMS(M,MAP(J+1)),I2+1)
              IF( M1 == M2 ) THEN
                ICEE(M,J) = M1
                CYCLE NM_LOOP
              ENDIF
            ENDDO
          ENDDO
        ENDDO NM_LOOP
      ENDDO
!
      NPROP(1:NMND)=0
      DO I = 1, NMEL
        N1 = ELEMS(I,1)
        N2 = ELEMS(I,2)
        N3 = ELEMS(I,3)
        NPROP(N1) = NPROP(N1) + N2 - N3
        NPROP(N2) = NPROP(N2) + N3 - N1
        NPROP(N3) = NPROP(N3) + N1 - N2
      ENDDO
      I = 0
      DO N = 1, NMND  !!!! COUNT TOTAL# of NODES ON BOUNDARY
        IF( NPROP(N) == 0 ) CYCLE
        I = I +1
      ENDDO
      ALLOCATE( IBSEG(1:I*2,1:2),IBSEGEL(1:I*2) )
!
! COMPUTE BOUNDARY LEGS. THESE OCCUR IN AN ELEMENT THAT 
! HAS A 0 IN THE ELEMENT ADJACENCY TABLE.  THEY ARE 
! DETERMINED BY MATCHING LEGS WITH OTHER ADJACENT ELEMENTS.
      ISEG = 0
      DO M = 1, NMEL
        DO J = 1, 3
          IF( ICEE(M,J) == 0 ) then
            ISEG = ISEG + 1
            IBSEG(ISEG,1) = ELEMS(M,MAP(J))
            IBSEG(ISEG,2) = ELEMS(M,MAP(J+1))
            IBSEGEL(ISEG) = M
          ENDIF
        ENDDO
      ENDDO
      NSEG = ISEG
      ALLOCATE( ISEGP(1:NSEG),ISEGF(1:NSEG) )
      ISEGP(:) = 0
      ISEGF(:) = 0
      DO I = 1, NSEG
        DO J = I+1, NSEG
          IF( IBSEG(I,1) == IBSEG(J,2) ) then
             ISEGP(I) = J    !boundary segment preceeding segment I
             ISEGF(J) = I    !boundary segment preceeding segment I
          ENDIF
          IF( IBSEG(I,2) == IBSEG(J,1) ) then
             ISEGF(I) = J    !boundary segment following  segment I
             ISEGP(J) = I    !boundary segment following  segment I
          ENDIF
        ENDDO
      ENDDO
!
      DEALLOCATE(NPROP)
      END SUBROUTINE MAK_NEINFO

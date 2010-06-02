C-----------------------------------------------------------------------
C
C       DROGUES  -  ***   DROGUES.F   ***  HARMONIC VELOCITY FIELDS
C              	    ***   DISTRIBUTION VERSION 1991, BRIAN O. BLANTON 
C                   ***   LAST MODIFICATION MAY 2010
C
C-----------------------------------------------------------------------
C
C ORIGINAL AUTHORS: ANTONIO M. BAPTISTA, MIT, OGC
C                   DOUGLAS COSLER, MIT
C                   PAUL J. TURNER, OGC
C                   E. ERIC ADAMS, MIT
C                   RICHARD KOSSICK, MIT
C
C MODIFICATIONS BY: BRIAN O. BLANTON, UNC
C                   RICK LUETTICH, UNC       
C                   ROBERT J WEAVER, UNC
C-----------------------------------------------------------------------
C
C   THE ORIGINAL CODE IS A PART OF THE COMPUTATIONAL STRUCTURE, ACE, FOR 
C   ANALYSIS OF FLOW, HYDRAULIC AND SEDIMENT TRANSPORT, AND WATER 
C   QUALITY IN ESTUARIES AND COASTS.
C
C-----------------------------------------------------------------------
C
C       REVISION HISTORY
C
C       AMB APRIL 1988   - REMOVED TRACKING ALGORITHM FROM ELA, AND
C                          IMPLEMENTED IT AS SEPARATE CODE, DROGUES
C
C       PJT JULY 1988    - REMOVED VARIABLES DECLARED BUT NOT USED,
C                          REMOVED SUBROUTINES NOT USED,
C                          CHANGED INTEGER*2 DECLARATIONS TO INTEGER,
C                          PORTED TO CRAY XMP, ALLIANT FX/8,
C                          GENERAL TIDYING UP
C
C       PJT AUG 1988     - ADDED SUBROUTINE BELELINIT TO INITIALIZE
C                          DATA FOR BELEL. REMOVED THE LOOP IN BELEL.
C                          CHANGED THE WRITE STATEMENT IN TRACK THAT
C                          OUTPUTS THE RESULTS TO A FORMATTED WRITE
C
C       AMB/PJT DEC 1988 - CHANGED FORMAT FOR GRID FROM QUADRATIC 
C                          GRID TO LINEAR GRID. MORE CLEANUPS 
C                          AND DEAD CODE ELIMINATION.
C
C       DKS MARCH 1990   - ADAPTED DROGUES.F FOR TIME-DEPENDENT 
C                          VELOCITY FIELDS.
C
C       BOB JAN 1991     - ADDED FLEXIBILITY IN UNITS SPECIFICATION. 
C                          ONCE A DROGUE ENCOUNTERS A BOUNDARY,
C                          IT STAYS THERE.  USERS MANUAL WRITTEN.
C
C       RAL MAY 1997     - MODIFIED ROUTINES TO DEAL WITH DROGUE BEHAVIOR
C                          NEAR BOUNDARIES.  REMOVED PREVIOUS "STICKY ZONE"
C                          APPROACH AND REPLACED THIS WITH PROVISIONS FOR
C                          DROGUES TO MOVE ALONG A BOUNDARY SEGMENT USING THE 
C                          VELOCITY COMPONENT TANGENTIAL TO THAT SEGMENT.
C
C       CWF AUG 2002     - ADDED AN OPTION TO READ IN A FORT.64 FILE OR
C                          HARMONIC FILES AS INPUT.  THE FORT.64 DOES NOT NEED
C                          TO BE CHANGED EXCEPT TO MAKE IT A FILE THAT ENDS IN
C                          .V2C
C
C       RJW MAY 2010     - MODIFIED TO USE FOR TRACKING OIL SLICK
C                          Removed all harmonic velocity capabilities 
C                          added an imrproved searching algorithm to find starting elements 
C                          for each particle.  ALtered input method, input file names now
C                          read in from drogue_input_1.  
C                          ADDED ability to handle full global output velocity files OR
C                          sparse global output velocity files.
C                          Modified input and output format to work with ASGS tracking system
C                          Included a Z, variable (flag) for each particle 
C                          
C-----------------------------------------------------------------------
C
C     SET MAXIMUM DIMENSIONING OF ARRAYS
C
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------
C
C***********************************************************************
C
C        WHERE     ND         = MAX. # OF NODES
C                  NNE        = MAX. # OF ELEMENTS
C                  NFR        = MAX. NO. OF FREQUENCIES
C                  MXDRG      = MAX. NO. OF DROGUES
C                  MXTIME     = MAX. NO. OF TIMES OUTPUT FROM DROGUE PROGRAM
C
C NOTE:  SUBROUTINES MUST ALSO BE REDIMENSIONED WHEN REDIMENSIONING
C        MAIN PROGRAM
C
C***********************************************************************
C
c     INTEGER :: ND, NNE, NFR
c     INTEGER :: MXDRG, MXTIME
c     REAL :: TOL, REARTH
C
C AMPLITUDES AND PHASES FOR EACH COORDINATE DIRECTION READ FROM ?????.VEL 
C
C
C FREQUENCIES AND THE NUMBER OF FREQUENCIES - READ FROM ?????.VEL
C
      COMMON /FREQCO/FREQ,NFREQ
      REAL*8 ::  FREQ(NFR)

C U and V velocities read from ?????.VEL (a fort.64 file)
      COMMON /VEL/UNEW,VNEW
      REAL*8 ::  UNEW(ND),VNEW(ND)
      REAL*8 ::  US(ND,2),VS(ND,2)

      COMMON /IOPT/NUMFILE
      INTEGER ::  NUMFILE
C
C COORDINATES OF THE NODES - READ FROM ?????.GR3
C
      COMMON /COORDS/X,Y
      REAL*8 ::  X(ND),Y(ND)
C
C SHAPE FUNCTION INFORMATION
C
      COMMON /ABA0/A,B,A0
      REAL*8 ::  A(NNE,3),B(NNE,3),A0(NNE,2)
C
C TABLE OF ELEMENTS
C
      COMMON /ELEMS/ELEMS
      INTEGER ::  ELEMS(NNE,3)
C
C CONNECTIVITY MATRICES - ELEMENT-ELEMENT & NODE-ELEMENT
C
      COMMON /CONEC/ICEE,ICNE
      INTEGER ::  ICEE(NNE,3),ICNE(ND,12)
C
C AREAS OF ELEMENTS
C
      COMMON /ELAREAS/AR
      REAL*8 ::  AR(NNE)
      COMMON /ARBEL/T
      REAL*8 ::  T(NNE,3)
C
C TOLERANCE ERROR PARAMETERS
C
      COMMON /ERRORS/EPS,DTMIN
C
C NUMBER OF NODES, NUMBER OF ELEMENTS
C
      COMMON /NUMBER/NMND,NMEL,NTINT
      INTEGER ::  NTINT
C
C BOUNDARY SEGMENTS
C
      COMMON /BSEGS/NSEG,IBSEG,ISEGP,ISEGF,IBSEGEL
      INTEGER ::  IBSEG(ND,2),ISEGP(ND),ISEGF(ND),IBSEGEL(ND)
      INTEGER ::  NSEG
      INTEGER ::  IL(3,2)
      INTEGER ::  ISEG,IPROD,IEAD,K

C 
C NUMBER VARIABLES
C
      COMMON /NUMS/NTIME
      INTEGER ::  NTIME
C
C LOCAL VARIABLES - DT1, ARRAY OF STEP SIZES FOR INTEGRATION
C
      REAL*8 ::  DT1(MXDRG)
C
C XDR,YDR - INITIAL DROGUE POSITIONS AND AFTER EACH TRACKING SET,
C THE NEW LOCATION.
C
      REAL*8 :: XDR(MXDRG),YDR(MXDRG),ZDR(MXDRG)
      INTEGER :: IDR(MXDRG),JJDR(MXDRG)
C
C CHARACTER VARIABLES FOR FILENAMES AND MISC
C
      CHARACTER(LEN=80) :: GRID,HEADER
      CHARACTER(LEN=72) :: VLIST(NFR)
      CHARACTER(LEN=72) :: CASE,CASE3,CASE4
      CHARACTER(LEN=20) :: JunkC
      CHARACTER(LEN=100) :: LINE
      
      REAL*8  :: SCAMPU(NFR),SCPHAU(NFR)
      REAL*8  :: SCAMPV(NFR),SCPHAV(NFR)
      INTEGER :: ICOMP(NFR)
      REAL*8  :: LONG,LAT,DEGRAD
      REAL*8  :: XO,YO
      REAL*8  :: EPS,DTMIN,PI,TPATH,TIME
      REAL*8  :: SCNDX,SCNDY,SCDRX,SCDRY,XD,YD,ZD
      REAL*8  :: AX,PX,AY,PY,XSTART,YSTART,STEPP,t1,t2,COSPHI
      INTEGER :: N1,N2,N3,NNO,NFREQT,III,NOTFND,IND,ii,jj,JNEW
      INTEGER :: NFREQ,NMND,NMEL,LEGNO,NPER,J
      INTEGER :: IPRINT,I,N,NDR,L
      REAL*8  :: XTEST,YTEST
      INTEGER :: IHOUR,IH
      INTEGER :: NUMNODES, TIMESTEP, NODE, NUMTIMES, NUMSTOP
      INTEGER :: OUTPUTTIMESTEP
      INTEGER :: Drnum,JunkI
      REAL    :: Junk,JunkR
      REAL    :: FILETIME(MXTIME)
      REAL    :: TIMEINC
      REAL    :: TIMEDIFF,UDIFF,VDIFF
      INTEGER :: NumNonDefaultNodes, IntegerTimeStep
      INTEGER :: Sparse
      REAL    :: DefaultValue
      REAL    :: RealTime
      INTEGER :: PEI,NL
C
C *** BEGIN EXECUTION 
C
C   DEFINE PI.
C
      PI=2.D0*DASIN(1.D0)
      DEGRAD=PI/180.D0
      
      OPEN(UNIT=90,FILE='drogue_input_1',STATUS='OLD')
     
C     PRINT*, 'ENTER .din NAME (no suffix)'
C     READ (5,6006) CASE
      READ (90,6006) CASE
      
      CASE3=CASE(:INDEX(CASE,' ')-1)//'.din'
      CASE4=CASE(:INDEX(CASE,' ')-1)//'.pth'
      
C     write(*,*) 'Enter grid name (no suffix)'
      READ(90,9020) GRID

C     write(*,*) 'Are the .v2c files harmonic or time-series output?'//
C    &            ' (1=harmonic, 2=time-series)'
C     read(*,*) numfile
      numfile=2

C OPEN FILES
      OPEN (UNIT=11,FILE=CASE3)
      OPEN (UNIT=12,FILE=CASE4)
C
C *** READS CHARACTERISTICS OF THE RUN FROM FILE ?????.DIN
C
C *** TWO ALPHANUMERICS
C
      READ(11,9020) HEADER
      write(*,*) header
      READ(11,*) LEGNO,TPATH,TIME,NTINT,NPER
      READ(11,6006)(VLIST(J),J=1,NPER)
      READ(11,*)(ICOMP(J),
     +           SCAMPU(J),SCPHAU(J),SCAMPV(J),
     +           SCPHAV(J),J=1,NPER)
      READ(11,9020) HEADER
      READ(11,9020) HEADER
      READ(11,*) IPRINT
      READ(11,9020) HEADER
      READ(11,*) SCNDX,SCNDY
      READ(11,9020) HEADER
      READ(11,*) EPS,DTMIN
      READ(11,9020) HEADER
      READ(11,*) SCDRX,SCDRY
C
C ***     TOTAL NUMBER OF PARTICLES AT START
C
      READ(11,9020) HEADER
      READ(11,*) NDR, JunkC
        WRITE(*,*) NDR, JunkC
C
C *** READ IN DROGUE COORDINATES
C
      READ(11,9020) HEADER
      WRITE(*,*) HEADER
      DO 90 I = 1,NDR
          READ(11,*) XD, YD, ZD
          XDR(I)=XD*REARTH*DEGRAD*SCDRX
          YDR(I)=YD*REARTH*DEGRAD*SCDRY
          ZDR(I)=ZD
   90 CONTINUE
C
C
C OPEN GRID FILES
C
      write(*,*)'DomainName=',GRID(:INDEX(GRID,' ')-1)//'.gr2'
      open(9,file=GRID(:INDEX(GRID,' ')-1)//'.gr2',status='old')
C
C *** READS GRID DATA FROM FILE ?????.GR2
C
      READ (9,*) NMEL,NMND
      WRITE(*,*) NMEL,NMND

      IF (NMEL.GT.NNE) THEN
          PRINT *,NMEL,' IS TOO MANY ELEMENTS. MAXIMUM =',NNE
          PRINT *, 'CHECK PARAMETER STATEMENTS'
          STOP
      END IF

      IF (NMND.GT.ND) THEN
          PRINT *,' IS TOO MANY NODES. MAXIMUM =',ND
          PRINT *, 'CHECK PARAMETER STATEMENTS'
          STOP
      END IF
C
C READ COORDINATES OF NODES
C
      DO 10 I = 1,NMND
         READ(9,*)N,LONG,LAT
         LONG=LONG*DEGRAD
         LAT=LAT*DEGRAD
         X(I)=LONG*REARTH*SCNDX
         Y(I)= LAT*REARTH*SCNDY
   10 CONTINUE
C
C READ THE TABLE OF ELEMENTS
C
      DO 20 I = 1,NMEL
          READ (9,*) N, (ELEMS(N,J),J=1,3)
   20 CONTINUE
C
C ELEMENT-ELEMENT ADJACENCY INFORMATION
C
      DO 30 I = 1,NMEL
          READ (9,*) N, (ICEE(N,J),J=1,3)
   30 CONTINUE
C
C NODE-ELEMENT ADJACENCY INFORMATION
C
      DO 40 I = 1,NMND
          READ (9,*) N,ICNE(N,1), (ICNE(N,J+1),J=1,ICNE(N,1))
   40 CONTINUE
      WRITE( 6,*) ' GRID-INPUT COMPLETE'
C
C COMPUTE BOUNDARY LEGS. THESE OCCUR IN AN ELEMENT THAT 
C HAS A 0 IN THE ELEMENT ADJACENCY TABLE.  THEY ARE 
C DETERMINED BY MATCHING LEGS WITH OTHER ADJACENT ELEMENTS.
C
      ISEG=0
      DO I=1,NMEL
        IPROD=ICEE(I,1)*ICEE(I,2)*ICEE(I,3)
        IF(IPROD.EQ.0) THEN
          IL(1,1)=ELEMS(I,1)
          IL(1,2)=ELEMS(I,3)
          IL(2,1)=ELEMS(I,3)
          IL(2,2)=ELEMS(I,2)
          IL(3,1)=ELEMS(I,2)
          IL(3,2)=ELEMS(I,1)
          DO J=1,3
            IEAD=ICEE(I,J)
            IF(IEAD.NE.0) THEN
              DO K=1,3
                IF((IL(K,1).EQ.ELEMS(IEAD,1)).AND.
     &             (IL(K,2).EQ.ELEMS(IEAD,2))) THEN
                  IL(K,1)=0
                  IL(K,2)=0
                  ENDIF
                IF((IL(K,1).EQ.ELEMS(IEAD,2)).AND.
     &             (IL(K,2).EQ.ELEMS(IEAD,3))) THEN
                  IL(K,1)=0
                  IL(K,2)=0
                  ENDIF
                IF((IL(K,1).EQ.ELEMS(IEAD,3)).AND.
     &             (IL(K,2).EQ.ELEMS(IEAD,1))) THEN
                  IL(K,1)=0
                  IL(K,2)=0
                  ENDIF
                END DO
              ENDIF
            END DO
          DO K=1,3
            IF(IL(K,1).NE.0) THEN
              ISEG=ISEG+1
              IBSEG(ISEG,1)=IL(K,2)   !beginning node on boundary segment iseg
              IBSEG(ISEG,2)=IL(K,1)   !ending node on boundary segment iseg
              IBSEGEL(ISEG)=I         !element containing boundary segment iseg
              ENDIF
            END DO
         ENDIF
       END DO                        
      NSEG=ISEG                       !total number of boundary segments
      DO I=1,NSEG
        DO J=1,NSEG
          IF(IBSEG(J,2).EQ.IBSEG(I,1)) ISEGP(I)=J !boundary segment preceeding segment I
          IF(IBSEG(J,1).EQ.IBSEG(I,2)) ISEGF(I)=J !boundary segment following segment I
          END DO
        END DO
C
C Open and start reading the time-series velocity file
C
C
      IF (NUMFILE.EQ.2) then

         open(10,
     +     file=vlist(1)(:INDEX(VLIST(1),' ')-1)//'.v2c',
     +     status='old')
          read(10,*) ! HEADER
          read(10,*) numtimes,numnodes, JunkR, JunkI, JunkI
C               numtimes=numtimes-1
       READ(UNIT=10,FMT='(A)',END=9000,ERR=9000) LINE
       READ(UNIT=LINE,FMT=*,END=9001, ERR=9001) filetime(1),timestep,
     &                                NumNonDefaultNodes,DefaultValue
       Sparse=1
         WRITE(*,*) "Sparse file: SPARSE = ", Sparse 
         write(*,*) NumNonDefaultNodes
       GOTO 9002
9001   READ(UNIT=LINE,FMT=*) filetime(1),timestep
       Sparse=0
         WRITE(*,*) "regular file: SPARSE = ", Sparse 
9002   CONTINUE
         
           k=1
           IF ( Sparse .EQ. 1 ) then        
                DO l=1,NumNodes
                  us(l,k)=DefaultValue
                  vs(l,k)=DefaultValue
                 ENDDO

              DO l=1,NumNonDefaultNodes
               read(10,*,end=9000,err=9000) node, us(node,k), vs(node,k)
              ENDDO

           ELSEIF (Sparse .EQ. 0 ) then 
           
              DO l=1,NumNodes
               read(10,*,end=9000,err=9000) node, us(node,k), vs(node,k)
              ENDDO

           ELSE 
        WRITE(*,*) "Bad Sparse Value,Error reading vel input file"
9000    WRITE(*,*)"Error reading or end of file while reading velocity"
           STOP   
           ENDIF
C
C NFREQT KEEPS COUNT OF THE FREQUENCIES ACTUALLY USED
C INDPER=0 INDICATES THAT THIS FREQUENCY IS NOT USED
C
      ELSE
        WRITE(*,*) "numfile not recognized"
        STOP
      ENDIF

         WRITE (6,*) ' VELOCITY INPUT COMPLETE: NO ERROR '
C
C NFREQ = THE NUMBER OF FREQUENCIES ACTUALLY USED
C
c      NFREQ = NFREQT - 1
C
C *** CLOSE INPUT FILES
C
      CLOSE (UNIT=9)
      CLOSE (UNIT=11)
C
      PRINT *,' LENGTH OF TRACKING (HOURS)      : ',TPATH
      PRINT *,' TIME AT START OF TRACING (HOURS): ',TIME
      PRINT *,' NUMBER OF TRACKING INTERVALS    : ',NTINT
      PRINT *,' OUTPUT TIME-STEP IS (MINUTES)   : ',
     +        (TPATH*IPRINT/NTINT)*60.D0
      PRINT *,' NUMBER OF FREQUENCIES USED      : ',NFREQ
      PRINT *,' MINIMUM TIME-STEP  IS (MINUTES) : ',DTMIN*60.D0
       OUTPUTTIMESTEP= (TPATH*IPRINT/NTINT)*3600  ! output time step in seconds
C
C *** TIME CONVERSION TO SECONDS
C
      TPATH = TPATH*3600.D0
      TIME = TIME*3600.D0
      DTMIN = DTMIN*3600.D0
C
C
C *** COMPUTE AREA COORDINATES FOR ELEMENT INTERPOLATION FUNCTIONS
C
      DO 100 J = 1,NMEL
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
C
C *** CHECK FOR NON-POSITIVE ELEMENT AREAS
C
         IF (AR(J).LE.0) THEN
             PRINT *,'NON-POSITIVE AREA AT ELEM ',J
             STOP
         END IF
  100 CONTINUE
      PRINT *,' ELEMENT AREAS COMPUTED '

      STEPP = TPATH/float(NTINT)

C
C *** DEFINE ELEMENT INTERPOLATION FUNCTIONS FOR VELOCITY
C


C
C *** FIND STARTING ELEMENT FOR EACH DROGUE
C
      print*,'Locating initial drogue positions',NDR
      NOTFND=0
      DO 130 III = 1,NDR
          XSTART = XDR(III)
          YSTART = YDR(III)
          IF (NOTFND.GT.0) THEN
            XDR(III-NOTFND)=XDR(III)
            YDR(III-NOTFND)=YDR(III)
          ENDIF
C new search algorithm
          IF (III .EQ. 1) THEN  
C first search all elelments for home of first particle
           DO 120 I = 1,NMEL
                 
              CALL BELEL(I,XSTART,YSTART,IND)
              IF (IND.EQ.1) THEN
                  JJDR(III-NOTFND) = I
C   PEI = particle element index
                  PEI=I
                WRite(*,*) III, PEI
                  GO TO 130
              END IF
  120     CONTINUE
         ELSE ! node loop 1 conditional
C  now search neighboring elements first for locations of subsequesnt particles
C   PEI = particle element index
                  NL = PEI
              CALL BELEL(NL,XSTART,YSTART,IND)
              IF (IND.EQ.1) THEN
                  JJDR(III-NOTFND) = NL
                   PEI=NL
                WRite(*,*) III, PEI
                  GO TO 130
              END IF
          DO 121 I = 2,NMEL
                  NL = PEI + (I-1)
                  IF (NL .GT. NMEL) NL = NL-NMEL
              CALL BELEL(NL,XSTART,YSTART,IND)
              IF (IND.EQ.1) THEN
                  JJDR(III-NOTFND) = NL
                   PEI=NL
                WRite(*,*) III, PEI
                  GO TO 130
              END IF
                  NL = PEI - (I-1)
                  IF (NL .LT. 1) NL = NL+NMEL
              CALL BELEL(NL,XSTART,YSTART,IND)
              IF (IND.EQ.1) THEN
                  JJDR(III-NOTFND) = NL
                   PEI=NL
                WRite(*,*) III, PEI
                  GO TO 130
              END IF

  121     CONTINUE

         ENDIF ! node loop 1 conditional
          PRINT *,'*** COULD NOT FIND STARTING ELEMENT FOR DROGUE ',III
          NOTFND=NOTFND+1

  130 CONTINUE

      NDR=NDR-NOTFND
      WRITE(*,*) '# DROGUES = ',NDR
c      DO  III = 1,NDR
c         write(45,*)XDR(III),YDR(III),JJDR(iii)
c      END DO

C
C ESTABLISH DEFAULT STEP SIZE
C
      DO III=1,NDR
        DT1(III) = DABS(STEPP/10.d0)
        END DO

   
C
C LIMITS OF INTEGRATION - TIME IS THE STARTING TIME
C
      T1 = TIME
      T2 = TIME + STEPP

C
C WRITE TO THE RESULTS FILE - CASE(XX).PTH
C
C *** WRITE OUT HEADERS AND INITIAL DROGUE POSITIONS
C     TO ACE/vis OUTPUT FILE
C

      WRITE(12,'(a)') GRID
      WRITE(12,*) NTINT/IPRINT, NDR, OUTPUTTIMESTEP
      WRITE(12,*) T1,NDR
      DO III = 1, NDR
         IDR(III) = III 
         WRITE (12,172) III,XDR(III)/REARTH/DEGRAD,
     +                      YDR(III)/REARTH/DEGRAD,
     +                      ZDR(III)
      END DO


C *** HARD CODING FOR STAGGERED RELEASE AT BI
C

C
C *** BEGIN TRACKING OF PARTICLES
C
      print*,' '
      print*,'Begin Tracking...'
C
C LOOP OVER EACH TIME STEP
C
       DO 180 I = 1,NTINT
          WRITE(*,*) I
C *** Find each timestep in the fort.64 file

      IF (NUMFILE.EQ.2) then
         IF (I.eq.1) then
            DO j = 2,numtimes

           IF ( Sparse .EQ. 1 ) then
       READ(UNIT=10,FMT=*,END=9003, ERR=9003) filetime(j),timestep,
     &                                NumNonDefaultNodes,DefaultValue
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
                      us(l,2)=DefaultValue
                      vs(l,2)=DefaultValue
                      udiff=us(l,2)-us(l,1)
                      unew(l)=us(l,2)-((timediff*udiff)/timeinc)
                      vdiff=vs(l,2)-vs(l,1)
                      vnew(l)=vs(l,2)-((timediff*vdiff)/timeinc)
                    ENDDO
                     DO l=1,NumNonDefaultNodes
               read(10,*,end=9003,err=9003) node, us(node,2), vs(node,2)
                     udiff=us(l,2)-us(l,1)
                     unew(l)=us(l,2)-((timediff*udiff)/timeinc)
                     vdiff=vs(l,2)-vs(l,1)
                     vnew(l)=vs(l,2)-((timediff*vdiff)/timeinc)
                     ENDDO
                  ELSEIF (Sparse .EQ. 0 ) then
                   write(*,*) "Sparse = 0 ",Sparse  
                     DO l=1,NumNodes
               read(10,*,end=9003,err=9003) node, us(node,2), vs(node,2)
                     udiff=us(l,2)-us(l,1)
                     unew(l)=us(l,2)-((timediff*udiff)/timeinc)
                     vdiff=vs(l,2)-vs(l,1)
                     vnew(l)=vs(l,2)-((timediff*vdiff)/timeinc)
                    ENDDO
                   ELSE
        WRITE(*,*) "Bad Sparse Value,Error reading vel input file"
9003    WRITE(*,*)"Error reading or end of file while reading velocity"
                    STOP
                  ENDIF
                  numstop=j
                 GOTO 150
             ELSE 
                   IF ( Sparse .EQ. 1 ) then
                   write(*,*) "Sparse = 1 ",Sparse
                    DO l=1,NumNodes
                      us(l,1)=DefaultValue
                      vs(l,1)=DefaultValue
                    ENDDO
                     DO l=1,NumNonDefaultNodes
               read(10,*,end=9004,err=9004) node, us(node,1), vs(node,1)
                     ENDDO
                   ELSEIF (Sparse .EQ. 0 ) then
                     DO l=1,NumNodes
               read(10,*,end=9004,err=9004) node, us(node,1), vs(node,1)
                    ENDDO
                   ELSE
        WRITE(*,*) "Bad Sparse Value,Error reading vel input file"
9004    WRITE(*,*)"Error reading or end of file while reading velocity"
                    STOP
                  ENDIF

             ENDIF ! T1 time conditional
           ENDDO  ! numtimes
         ELSE ! I not equal to 1
              DO j= numstop,numtimes
                 IF (j.ne.numstop) THEN
           IF ( Sparse .EQ. 1 ) then
       READ(UNIT=10,FMT=*,END=9005, ERR=9005) filetime(j),timestep,
     &                                NumNonDefaultNodes,DefaultValue
        write(*,*) NumNonDefaultNodes
           ELSEIF (Sparse .EQ. 0 ) then
       READ(UNIT=10,FMT=*,END=9005, ERR=9005) filetime(j),timestep
           ENDIF
                ENDIF
                   write(20,*) filetime(j),numstop,j

                 if ((T1.ge.filetime(j-1)).and.(T1.le.filetime(j))) then
                  timeinc=filetime(j)-filetime(j-1)
                  timediff=filetime(j)-T1
                    if (j.ne.numstop) then

                  IF ( Sparse .EQ. 1 ) then
                    DO l=1,NumNodes
                      us(l,2)=DefaultValue
                      vs(l,2)=DefaultValue
                      udiff=us(l,2)-us(l,1)
                      unew(l)=us(l,2)-((timediff*udiff)/timeinc)
                      vdiff=vs(l,2)-vs(l,1)
                      vnew(l)=vs(l,2)-((timediff*vdiff)/timeinc)
                    ENDDO
                     DO l=1,NumNonDefaultNodes
               read(10,*,end=9005,err=9005) node, us(node,2), vs(node,2)
                     udiff=us(l,2)-us(l,1)
                     unew(l)=us(l,2)-((timediff*udiff)/timeinc)
                     vdiff=vs(l,2)-vs(l,1)
                     vnew(l)=vs(l,2)-((timediff*vdiff)/timeinc)
                     ENDDO
                   ELSEIF (Sparse .EQ. 0 ) then
                     DO l=1,NumNodes
               read(10,*,end=9005,err=9005) node, us(node,2), vs(node,2)
                     udiff=us(l,2)-us(l,1)
                     unew(l)=us(l,2)-((timediff*udiff)/timeinc)
                     vdiff=vs(l,2)-vs(l,1)
                     vnew(l)=vs(l,2)-((timediff*vdiff)/timeinc)
                    ENDDO
                   ELSE
        WRITE(*,*) "Bad Sparse Value,Error reading vel input file"
9005    WRITE(*,*)"Error reading or end of file while reading velocity"
                    STOP
                  ENDIF

                  else !numstop conditional
                       do l=1,numnodes
                        udiff=us(l,2)-us(l,1)
                        unew(l)=us(l,2)-((timediff*udiff)/timeinc)
                        vdiff=vs(l,2)-vs(l,1)
                        vnew(l)=vs(l,2)-((timediff*vdiff)/timeinc)
                       enddo
                   endif
                     numstop=j
                     goto 150
                else 
                   do l=1,numnodes
                     us(l,1)=us(l,2)
                     vs(l,1)=vs(l,2)
                   enddo
                endif
            enddo
         endif
       endif   
150      continue
  
     
C
C LOOP OVER EACH DROGUE
C
         Write(*,*)" loop through particles and track"
          DO 160 II = 1,NDR
C
C IF THIS ELEMENT WAS ELIMINATED ON A PREVIOUS STEP THEN SKIP
C
              IF (IDR(II).EQ.0) GO TO 160
C
C XO, YO ARE THE STARTING POSITIONS
C
              XO = XDR(II)
              YO = YDR(II)
              JJ = JJDR(II)
             NTIME = I

C
C GET THE COMPONENTS OF FLOW AT XO, YO
C
              CALL VELS(JJ,XO,YO,UNEW,VNEW,T1,NUMFILE)
C
C
C TRACK THIS PARTICLE FROM TIME T1 TO T2
C
c              WRITE(45,*) '********************************************'
c              WRITE(45,*) ' PROCESSING DROGUE # ',II
c              WRITE(45,*) ' FROM TIME ',T1,' to ',T2
c              WRITE(45,*) ' '
              CALL TRACK(JJ,JNEW,XO,YO,UNEW,VNEW,T1,T2,DT1(II),II)

              XDR(II) = XO
              YDR(II) = YO
              JJDR(II) = JNEW

  160     CONTINUE
         Write(*,*)" done looping through particles and tracking"
C
C *** WRITE OUT THE POSITIONS AT THIS TIME STEP
C

C *** Section to print output in ACE/vis format
C
       write(*,*) "write output for this time"

          IF(MOD(I,IPRINT).EQ.0)THEN
            WRITE (12,*) T1,NDR
            DO 170 II = 1,NDR
               WRITE (12,172) II,XDR(II)/REARTH/DEGRAD,
     +                        YDR(II)/REARTH/DEGRAD,
     +                        ZDR(II)
  170       CONTINUE
            ENDIF
          write(*,*) "wrote output for this time"

  171     FORMAT ( 2(1x,1e18.9), 1x, i8 )
  172     FORMAT (I8,2x,2(1x,1e18.9),2x,I6)
C
C
C INCREMENT THE LIMITS OF INTEGRATION
C
          T1 = T1 + STEPP
          T2 = T2 + STEPP


180       CONTINUE


C
C *** CLOSE OUTPUT FILE
C
      CLOSE (UNIT=12)
c      CLOSE (UNIT=45)

      STOP

 6006 FORMAT(A)
 9020 FORMAT (A)
      END
C
C***********************************************************************
      SUBROUTINE DEBLNK(STRING,ISTART,IEND)
C-----------------------------------------------------------------------
C PURPOSE: THIS SUBROUTINE FINDS THE FIRST NONBLANK SPACE (ISTART) IN A 
C            ARBITRARY CHARACTER STRING AND THE FIRST SUBSEQUENT BLANK
C            SPACE (IEND).  THE RANGE OF STRING BETWEEN ISTART AND IEND
C            CAN THEN BE SEPARATED FROM THE REST OF STRING BY SPECIFYING
C            STRING(ISTART:IEND)
C
C
C INPUTS:  STRING - A CHARACTER STRING OF LENGTH .LE. 72
C
C OUTPUTS: ISTART - FIRST NONBLANK CHARACTER IN STRING
C          IEND   - FIRST BLANK CHARACTER IN STRING AFTER ISTART
C
C HISTORY:  WRITTEN BY CHRISTOPHER E. NAIMIE
C           DARTMOUTH COLLEGE
C           12 JUNE 1992
C
C           NAME CHANGED TO DEBLNK BY BRIAN BLANTON, UNC-CH
C           INCLUDED IN OPNML LIBRABY AND TRACKING ALGORITHMS
C           19 JUNE 1995
C-----------------------------------------------------------------------
C
C ARGUMENTS
      CHARACTER*72 STRING
      INTEGER ISTART,IEND,I
C
C START OF EXECUTABLE CODE
C
C FIND BEGINNING OF STRING
      DO 10 I=1,72
         ISTART=I
        IF(STRING(I:I).NE.' ')GO TO 11
 10   CONTINUE
C
C FIND END OF STRING
 11   DO 20 I=ISTART,72
         IEND=I-1
          IF(STRING(I:I).EQ.' ')GO TO 21
 20   CONTINUE
C
C END OF ROUTINE
 21   RETURN
      END
C
C***********************************************************************
      SUBROUTINE BELEL(J,XP,YP,NFLAG)
C***********************************************************************
C
C *** DETERMINE WHETHER THE POINT (XP,YP) IS WITHIN ELEMENT "J"
C *** (OR ON ITS BOUNDARIES).  IF SO, IND=1; IF NOT, IND=0
C
C *** THIS IS THE CROSS-PRODUCT METHOD OF CHRISTOPHER NAIMIE,
C *** NUMERICAL METHODS LABORATORY, DARTMOUTH COLLEGE, HANOVER NH
C
C PURPOSE: THIS SUBROUTINE DETERMINES IF A POINT (XVAL,YVAL) IS ON A 
C            LOCAL 2-D TRIANGULAR LINEAR ELEMENT
C    
C RESTRICTIONS: APPLICABLE ONLY FOR  2-D TRIANGULAR LINEAR ELEMENT
C
C INPUTS:  J         - CURRENT ELEMENT BEING TESTED
C          XVAL,YVAL - LOCATION OF THE POINT 
C
C OUTPUTS: NFLAG IS SET EQUAL TO 1 IF THE POINT (XVAL,YVAL) IS IN/ON 
C            THE LOCAL ELEMENT J.
C          NFLAG IS SET EQUAL TO 0 IF THE POINT (XVAL,YVAL) IS NOT ON 
C            THE LOCAL ELEMENT J.
C
C HISTORY:  WRITTEN BY CHRISTOPHER E. NAIMIE
C           DARTMOUTH COLLEGE
C           22 APRIL 1992
C
C           IMPLEMENTED IN DROG3DDT BY BRIAN BLANTON
C           UNC-CH
C           10 Aug 1995
C
C-----------------------------------------------------------------------
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------
C
      COMMON /COORDS/X,Y
      REAL*8  X(ND),Y(ND)
      COMMON /ELEMS/ELEMS
      INTEGER ELEMS(NNE,3)
C
C *** INCOMING ARGUMENTS
C
      REAL*8 XP,YP
      INTEGER J,NFLAG
C
C *** LOCAL DECLARATIONS
C
      INTEGER I,K
      REAL*8 XLOCAL(3),YLOCAL(3),CROSSPROD
      REAL*8 VX(3),VY(3),DELX,DELY,D,THETA
C
C *** EXTRACT LOCAL NODE COORDINATES
C
      XLOCAL(1)=X(ELEMS(J,1))
      XLOCAL(2)=X(ELEMS(J,2))
      XLOCAL(3)=X(ELEMS(J,3))
      YLOCAL(1)=Y(ELEMS(J,1))
      YLOCAL(2)=Y(ELEMS(J,2))
      YLOCAL(3)=Y(ELEMS(J,3))
C
C  CALCULATE THE X AND Y COMPONENTS OF VECTORS POINTING FROM (XP,YP)
C  TO EACH NODE ON THE ELEMENT
C
      DO 10 I=1,3
         DELX=XLOCAL(I)-XP
         DELY=YLOCAL(I)-YP
         D=DSQRT(DELX**2.0+DELY**2.0)
         THETA=DATAN2(DELY,DELX)
         VX(I)=D*DCOS(THETA)
         VY(I)=D*DSIN(THETA)
C         WRITE(*,*) VX(I), VY(I)
 10   CONTINUE
C
C  DETERMINE IF THE POINT IS ON THE ELEMENT BY CALCULATING THE
C    CROSSPRODUCTS OF NEIGHBORING VECTORS IN A DIRECTION
C    WHICH WILL YIELD ALL NON-NEGATIVE NUMBERS IF X,Y IS ON THE ELEMENT.
C    (IE: V1XV2>OR=0, V2XV3>OR=0, AND V3XV1>OR=0 => X,Y IS ON ELEMENT)
      DO 20 I=1,3
         K=I+1
         IF(I.EQ.3)K=1
         CROSSPROD=VX(I)*VY(K)-VY(I)*VX(K)
         IF(CROSSPROD.LT.0.D0)THEN
            NFLAG=0
            GO TO 21
         ENDIF
 20   CONTINUE
      NFLAG=1
 21   CONTINUE
C
C END OF ROUTINE
      RETURN
      END
C
C***********************************************************************
      SUBROUTINE FNDELE(J,JJ,XX,YY,INDD)
C***********************************************************************
C
C *** FINDS NEW ELEMENT "JJ" WHICH CONTAINS THE POINT (XX,YY).  "J" IS T
C *** OLD ELEMENT.  INDD=1 IF THE NEW ELEMENT IS FOUND; INDD=0 IF IT
C *** CANNOT BE FOUND.
C
C-----------------------------------------------------------------------
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------
      REAL*8 XX,YY
      INTEGER J,JJ,INDD
      INTEGER ICEE(NNE,3),ICNE(ND,12),ELEMS(NNE,3)
      COMMON /CONEC/ICEE,ICNE
      COMMON /ELEMS/ELEMS
      INTEGER NEC(NNE),NECN(NNE),NDONE(NNE)
      INTEGER idone,i,n,ind,isum,l1,l2,kk,icount,k,nl

      IDONE = 1
      NDONE(IDONE) = J
C
C *** USE THE LOGIC DESCRIBED IN SUBROUTINE "LOCATE"
C *** NEXT, CHECK NEIGHBORING ELEMENTS
C
      DO 10 I = 1,3
          N = ICEE(J,I)
          IF (N.EQ.0) GO TO 10
          CALL BELEL(N,XX,YY,IND)
          IF (IND.EQ.1) THEN
              JJ = N
              INDD = 1
              GO TO 130
          END IF

          IDONE = IDONE + 1
          NDONE(IDONE) = N
   10 CONTINUE
      ISUM = 0
      DO 40 L1 = 1,3
          L2 = ELEMS(J,L1)
          DO 30 KK = 1,ICNE(L2,1)
              N = ICNE(L2,KK+1)
              DO 20 I = 1,IDONE
                  IF (N.EQ.NDONE(I)) GO TO 30
   20         CONTINUE
              ISUM = ISUM + 1
              IDONE = IDONE + 1
              NDONE(IDONE) = N
              NEC(ISUM) = N
   30     CONTINUE
   40 CONTINUE
C
C *** PROGRESSIVELY LOOP OVER MORE AND MORE ELEMENTS IN SEARCH OF THE EL
C *** MENT CONTAINING (XX,YY)
C
      ICOUNT = ISUM
      ISUM = 0
      IF (ICOUNT.EQ.0) THEN
          DO 50 I = 2,IDONE
              ICOUNT = ICOUNT + 1
              NEC(ICOUNT) = NDONE(I)
   50     CONTINUE
      END IF

   60 CONTINUE
      DO 70 K = 1,ICOUNT
          N = NEC(K)
          CALL BELEL(N,XX,YY,IND)
          IF (IND.EQ.1) THEN
              JJ = N
              INDD = 1
              GO TO 130
          END IF

   70 CONTINUE
      IF (IDONE.GT.70) THEN
          INDD = 0
          GO TO 130
      END IF
C
C *** ACCUMULATE LIST OF NEW ELEMENTS TO BE CHECKED ON NEXT LOOP
C
      DO 110 K = 1,ICOUNT
          N = NEC(K)
          DO 100 L1 = 1,3
              L2 = ELEMS(N,L1)
              DO 90 KK = 1,ICNE(L2,1)
                  NL = ICNE(L2,KK+1)
                  DO 80 I = 1,IDONE
                      IF (NL.EQ.NDONE(I)) GO TO 90
   80             CONTINUE
                  ISUM = ISUM + 1
                  IDONE = IDONE + 1
                  NDONE(IDONE) = NL
                  NECN(ISUM) = NL
   90         CONTINUE
  100     CONTINUE
  110 CONTINUE

      ICOUNT = ISUM
      IF (ICOUNT.EQ.0) THEN
          INDD = 0
          GO TO 130
      END IF

      DO 120 I = 1,ICOUNT
          NEC(I) = NECN(I)
  120 CONTINUE

      ISUM = 0
      GO TO 60

  130 CONTINUE
      RETURN

      END

C
C***********************************************************************
      SUBROUTINE RK4(J,JOUT,XX,YY,U,V,T,DT,XOUT,YOUT,KODE)
C***********************************************************************
C
C *** USES 4TH ORDER RUNGE-KUTTA SCHEME TO ADVANCE SOLUTION OVER A TIME
C *** INTERVAL "DT" AND RETURNS THE RESULTING POINT (XOUT,YOUT) AS THE
C *** COORDINATE OF THE ENDPOINT OF THE INTEGRATION STEP.
C
C-----------------------------------------------------------------------
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------
C
      COMMON /IOPT/NUMFILE
      INTEGER NUMFILE
      INTEGER J,JOUT
      REAL*8 XX,YY,U,V,T,DT,XOUT,YOUT
      INTEGER indd,jj,KODE,jt,ind
      REAL*8 dth,dt6,th,xt,yt,ut,vt
      REAL*8 um,vm,u4,v4
      
      KODE = 0
      DTH = DT/2.D0
      DT6 = DT/6.D0
      TH = T + DTH
      JT = J
C
C *** FIRST STEP
C
      XT = XX + DTH*U
      YT = YY + DTH*V
C
C *** CHECK WHETHER POINT STILL LIES IN ELEMENT "J";  IF NOT, FIND NEW ELEMENT
C
      CALL BELEL(JT,XT,YT,IND)
      IF (IND.EQ.0) THEN
          CALL FNDELE(JT,JJ,XT,YT,INDD)
          IF (INDD.EQ.0) THEN          
              KODE = 1
              XOUT=XT
              YOUT=YT
              RETURN
          END IF

   10     CONTINUE
          JT = JJ
      END IF
C
C *** SECOND STEP
C
      CALL VELS(JT,XT,YT,UT,VT,TH,NUMFILE)
      XT = XX + DTH*UT
      YT = YY + DTH*VT
      CALL BELEL(JT,XT,YT,IND)
      IF (IND.EQ.0) THEN
          CALL FNDELE(JT,JJ,XT,YT,INDD)
          IF (INDD.EQ.0) THEN
              KODE = 2
              RETURN
          END IF

   20     CONTINUE
          JT = JJ
      END IF
C
C *** THIRD STEP
C
      CALL VELS(JT,XT,YT,UM,VM,TH,NUMFILE)
      XT = XX + DT*UM
      YT = YY + DT*VM
      CALL BELEL(JT,XT,YT,IND)
      IF (IND.EQ.0) THEN
          CALL FNDELE(JT,JJ,XT,YT,INDD)
          IF (INDD.EQ.0) THEN
              KODE = 3
              RETURN
          END IF

   30     CONTINUE
          JT = JJ
      END IF
C
C *** FOURTH STEP
C
      CALL VELS(JT,XT,YT,U4,V4,T+DT,NUMFILE)
C
C *** ACCUMULATE INCREMENTS WITH PROPER WEIGHTS
C
      XOUT = XX + (U+2* (UT+UM)+U4)*DT6
      YOUT = YY + (V+2* (VT+VM)+V4)*DT6
      CALL BELEL(JT,XOUT,YOUT,IND)
      IF (IND.EQ.0) THEN
          CALL FNDELE(JT,JJ,XOUT,YOUT,INDD)
          IF (INDD.EQ.0) THEN
              KODE = 4
              RETURN
          END IF

          JT = JJ
      END IF

      JOUT = JT

      RETURN

      END

C
C***********************************************************************
      SUBROUTINE RKQC(JINOUT,XX,YY,U,V,T,DTTRY,DTNEXT,KODE,ICURBS)
C***********************************************************************
C
C
C *** 5TH ORDER RUNGE-KUTTA STEP WITH MONITORING OF LOCAL 4TH ORDER TRUN
C *** ATION ERROR TO ENSURE ACCURACY AND ADJUST STEP SIZE.  "DTTRY" IS T
C *** STEP SIZE TO BE ATTEMPTED AND "EPS" IS THE REQUIRED ACCURACY.
C *** UPON FINISHING THE INTEGRATION STEP, "XX" AND "YY" CONTAIN THE FIN
C *** TRACKED LOCATION.  "DTNEXT" IS THE ESTIMATED NEXT STEP SIZE.
C
C *** SET MAXIMUM DIMENSIONS HERE.
C-----------------------------------------------------------------------
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------
      COMMON /IOPT/NUMFILE
      INTEGER NUMFILE
      REAL*8 PGROW,PSHRNK,FCOR,ONE,SAFETY,ERRCON
      PARAMETER (PGROW=-0.2D0,PSHRNK=-0.25D0,FCOR=1.D0/15.D0,ONE=1.D0,
     +          SAFETY=0.9D0,ERRCON=6.0D-04)
C
C *** NOTE:  ERRCON = (4/SAFETY)**(1/PGROW)
C
      COMMON /BSEGS/NSEG,IBSEG,ISEGP,ISEGF,IBSEGEL
      INTEGER IBSEG(ND,2),ISEGP(ND),ISEGF(ND),IBSEGEL(ND),NSEG
      COMMON /COORDS/X,Y
      REAL*8 X(ND),Y(ND)
      COMMON /ERRORS/EPS,DTMIN
      INTEGER J1HS,J2HS,JBS,JINOUT,JSAV,KODESAV,ICURBS
      REAL*8 XX,YY,U,V,T,TH,DTTRY,DTDID,DTNEXT
      REAL*8 eps,dtmin,tsav,usav,vsav,xsav,ysav,dt,dth
      REAL*8 xtemp,ytemp,XXHS,YYHS,error
      REAL*8 XBSMIN,XBSMAX,XTRKMIN,XTRKMAX,YTRKMIN,YTRKMAX,XI,YI
      REAL*8 XB,YB,XE,YE,XIMIN,YIMIN,DIST,DISTMIN
      REAL*8 TRKSLOPE,TRKINT,BNDSLOPE,BNDINT
      INTEGER ind,KODE,I,ITRKSLOPEF,IBNDSLOPEF,IELMIN,IMIN

C
C *** SAVE INITIAL VALUES
C
      JSAV = JINOUT
      TSAV = T
      USAV = U
      VSAV = V
      XSAV = XX
      YSAV = YY
      KODESAV = KODE
c      WRITE(45,*) ' Particle starts at position ',XX,YY
c      WRITE(45,*) ' In element ',JINOUT,' with KODE = ',KODE

C
C *** SET STEP SIZE TO INITIAL TRY VALUE
C
      DT = DTTRY

C
C *** TAKE TWO HALF STEPS
C
   10 CONTINUE

C *** TAKE FIRST HALF STEP

      DTH = DT/2.d0
      CALL RK4(JSAV,J1HS,XSAV,YSAV,USAV,VSAV,TSAV,DTH,XTEMP,YTEMP,
     &         KODE)

C *** IF KODE <> 0, TRACKING HAS LEFT THE GRID DURING THE FIRST HALF OF DT.
C *** IF KODE = 1, TRACKING HAS LEFT THE GRID ON THE FIRST STEP. IN THIS CASE 
C ***              DEPENDING ON KODESAV, MOVE TO THE BOUNDARY OR JUST TRACK
C ***              ALONG THE BOUNDARY.
C *** IF KODESAV=-1, PARTICLE IS ALREADY ON THE BOUNDARY, JUST BOUNDARY TRACK.
C *** IF KODESAV=0, PARTICLE IS COMING FROM THE INTERIOR OF THE DOMAIN.  THEREFORE
C ***               MOVE IT TO THE BOUNDARY AND THEN BOUNDARY TRACK.
C *** THE FOLLOWING STEPS ARE USED TO MOVE A PARTICLE TO THE BOUNDARY:
C ***   1. Check to see which boundary segment(s) was (were) crossed
C ***   2. Determine intersection point(s)
C ***   3. If more than one boundary segment was crossed, determine the closest
C ***   4. Put particle at intersection point with closest boundary
C ***   5. Figure out how long it took to get to the boundary and therefore 
C ***      how much of DT is left.  This is done using USAV,VSAV.

        IF((KODE.EQ.1).AND.(KODESAV.EQ.-1)) THEN
c        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
c        WRITE(45,*) ' Particle on boundary left domain in tracking',
c     &              ' segment 1'
        RETURN
        ENDIF

      IF((KODE.EQ.1).AND.(KODESAV.NE.-1)) THEN
c        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
c        WRITE(45,*) ' Particle left domain in tracking segment 1'
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

        DO 90 I=1,NSEG
          XB=X(IBSEG(I,1))
          YB=Y(IBSEG(I,1))
          XE=X(IBSEG(I,2))
          YE=Y(IBSEG(I,2))

c         initially check if particle is near the boundary segment

          IF((XB.LT.XTRKMIN).AND.(XE.LT.XTRKMIN)) GOTO 90
          IF((XB.GT.XTRKMAX).AND.(XE.GT.XTRKMAX)) GOTO 90
          IF((YB.LT.YTRKMIN).AND.(YE.LT.YTRKMIN)) GOTO 90
          IF((YB.GT.YTRKMAX).AND.(YE.GT.YTRKMAX)) GOTO 90

c         slope and intercept of boundary segment

          IF((XE-XB).NE.(0.D0)) THEN
            BNDSLOPE=(YE-YB)/(XE-XB)
            BNDINT=YB-BNDSLOPE*XB
            IBNDSLOPEF=0
            ELSE
            IBNDSLOPEF=1
            ENDIF

c         track and boundary segment parallel

          IF(TRKSLOPE.EQ.BNDSLOPE) GOTO 90
 
c         compute intersection point of track line and boundary segment
c         and check if it falls on both the track line and boundary segment
c         keep track of minimum along track distance to the boundary.

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
c            WRITE(45,*) XI,YI
            XBSMIN=MIN(XB,XE)
            XBSMAX=MAX(XB,XE)
            IF((XI.GE.XBSMIN).AND.(XI.LE.XBSMAX).AND.(XI.GE.XTRKMIN)
     &                       .AND.(XI.LE.XTRKMAX)) THEN   
              DIST=SQRT((XI-XSAV)*(XI-XSAV)+(YI-YSAV)*(YI-YSAV))
              IF(DIST.LT.DISTMIN) THEN
                XIMIN=XI
                YIMIN=YI
                DISTMIN=DIST
                IELMIN=IBSEGEL(I)
                IMIN=I
                ENDIF
              ENDIF
            ENDIF
          
90        CONTINUE

c       move particle to boundary

        XX = XIMIN
        YY = YIMIN
        JINOUT=IELMIN
        ICURBS=IMIN

c       compute time it took particle to get there assuming velocity is
c       stationary in time and space over this interval

        DTDID = DISTMIN/sqrt(USAV*USAV+VSAV*VSAV)
        DTNEXT = DT
        T=TSAV+DTDID

c        WRITE(45,*) ' Particle stops at position ',XX,YY
c        WRITE(45,*) ' In element ',JINOUT
c        WRITE(45,*) ' Time advanced to ',T
c        WRITE(45,*) ' '

        RETURN
        END IF

C *** IF KODE = 2, TRACKING HAS LEFT THE GRID ON THE SECOND STEP. IN THIS CASE 
C *** CUT DOWN THE TRACKING TIME STEP SIZE.
   
      IF (KODE.EQ.2) THEN
c        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
c        WRITE(45,*) ' Particle left domain in tracking segment 2'
c        WRITE(45,*) ' '
        DT=DT/4.d0                                               
        GO TO 10
        END IF

C *** IF KODE = 3 or 4, TRACKING HAS LEFT THE GRID ON THE 3rd or 4th STEP.
C *** CUT DOWN THE TRACKING TIME STEP SIZE.
 
      IF (KODE.EQ.3 .OR. KODE.EQ.4) THEN
c        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
c        WRITE(45,*) ' Particle left domain in tracking segment 3 or 4'
c        WRITE(45,*) ' '
        DT=DT/2.d0
        GO TO 10
        END IF
  
C *** TRACKING REMAINED IN THE GRID OVER THE FIRST HALF STEP, NOW TAKE
C *** SECOND HALF STEP.

c      WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
c      WRITE(45,*) ' First half step succeeded'
c      WRITE(45,*) ' '

      TH=TSAV+DTH
      CALL VELS(J1HS,XTEMP,YTEMP,U,V,TH,NUMFILE)
      CALL RK4(J1HS,J2HS,XTEMP,YTEMP,U,V,TH,DTH,XXHS,YYHS,KODE)

C *** IF KODE <> 0, TRACKING HAS LEFT THE GRID DURING THE SECOND HALF OF DT.
C *** CUT DOWN THE TRACKING STEP SIZE.

      IF (KODE.NE.0) THEN
c        WRITE(45,*) ' At time ',T,' with time step size ',DT
c        WRITE(45,*) ' Particle left domain in 2nd half step'
c        WRITE(45,*) ' '
        DT = DT/2.d0
        GO TO 10
        END IF

C *** TRACKING REMAINED IN THE GRID OVER BOTH HALF STEPS. NOW TAKE 
C *** THE LARGE STEP

c      WRITE(45,*) ' At time ',T,' with time step size ',DT
c      WRITE(45,*) ' Second half step succeeded'
c      WRITE(45,*) ' '

      CALL RK4(JSAV,JBS,XSAV,YSAV,USAV,VSAV,TSAV,DT,XTEMP,YTEMP,KODE)

C *** IF KODE <> 0, TRACKING HAS LEFT THE GRID DURING THE LARGE STEP.
C *** CUT DOWN THE TRACKING STEP SIZE.

      IF (KODE.NE.0) THEN
c        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
c        WRITE(45,*) ' Particle left domain in big step'
c        WRITE(45,*) ' '
        DT = DT/2.d0
        GO TO 10
        END IF

c      WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
c      WRITE(45,*) ' Big step succeeded'
c      WRITE(45,*) ' '

      ERROR = DSQRT((YYHS-YTEMP)**2+ (XXHS-XTEMP)**2)
C
C *** SCALE ERROR RELATIVE TO REQUIRED TOLERANCE
C
      ERROR = ERROR/EPS

      IF (ERROR.GT.ONE .AND. ABS(DT).GE.DTMIN) THEN
C
C ***   TRUNCATION ERROR TOO LARGE, REDUCE STEP SIZE AND TRY AGAIN
C
        DT = SAFETY*DT* (ERROR**PSHRNK)
c        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
c        WRITE(45,*) ' Truncation error too large'
c        WRITE(45,*) ' '
        GO TO 10                       

        ELSE
C
C ***   STEP SUCCEEDED;  COMPUTE (ESTIMATE) SIZE OF NEXT STEP
C
        T = TSAV + DT
c        WRITE(45,*) ' At time ',TSAV,' with time step size ',DT
c        WRITE(45,*) ' Step succeeded'

        IF (ERROR.GT.ERRCON) THEN
          DTNEXT = SAFETY*DT* (ERROR**PGROW)
          ELSE
          DTNEXT = 4.0*DT
          END IF
        IF(DT.LT.DTMIN) DT=DTMIN
        END IF

      XX=XXHS
      YY=YYHS

C *** ADD IN TRUNCATION ERROR TO INCREASE 4TH ORDER TO 5TH ORDER ACCURACY

      XX = XX + (XX-XTEMP)*FCOR
      YY = YY + (YY-YTEMP)*FCOR

C *** MAKE SURE THAT THE POINT (XX,YY) IS STILL IN ELEMENT "J2HS".  IF NOT,
C *** DONT INCREASE TO 5TH ORDER.

      CALL BELEL(J2HS,XX,YY,IND)
      IF (IND.EQ.0) THEN
        XX = XXHS
        YY = YYHS
        END IF

      JINOUT=J2HS
      
c      WRITE(45,*) ' Particle stops at position ',XX,YY
c      WRITE(45,*) ' In element ',JINOUT
c      WRITE(45,*) ' Time advanced to ',T
c      WRITE(45,*) ' '

      RETURN

      END

C
C***********************************************************************
      SUBROUTINE BOUNTRK(JINOUT,XX,YY,U,V,T,T2,KODE,ICURBS)
C***********************************************************************
C
C *** TRACK A PARTICLE ALONG A BOUNDARY SEGMENT.
C *** "T2-T" IS THE TIME STEP SIZE TO BE ATTEMPTED.
C *** UPON FINISHING THE INTEGRATION STEP, "XX" AND "YY" CONTAIN THE 
C *** FINAL TRACKED LOCATION.
C ***
C *** THIS TRACKING IS DONE BY:
C *** 1. Computing the tangential velocity at the particle position
C *** 2. Determining which boundary node is "downstream"
C *** 3. Using average of velocities computed in 1. and 2., determine whether 
C ***    particle would make it to the node.
C *** 4. IF the answer in 3. is no, move particle along boundary until DT
C ***    is used up.  Remember that particle is on boundary and return.
C *** 5. IF the answer in 3. is yes, put particle at downstream boundary 
C ***    node and then figure how much of DT is left.  Then return and 
C ***    see if particle will leave boundary or continue to move along the
C ***    next boundary segment.

C
C *** SET MAXIMUM DIMENSIONS HERE.
C-----------------------------------------------------------------------
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------

      COMMON /IOPT/NUMFILE
      INTEGER NUMFILE
      COMMON /BSEGS/NSEG,IBSEG,ISEGP,ISEGF,IBSEGEL
      INTEGER IBSEG(ND,2),ISEGP(ND),ISEGF(ND),IBSEGEL(ND),NSEG
      COMMON /COORDS/X,Y
      REAL*8 X(ND),Y(ND)
      INTEGER JINOUT,JSAV, I
      REAL*8 XX,YY,U,V,T,DTDID,DTTRY,T2
      REAL*8 tsav,usav,vsav,xsav,ysav
      REAL*8 DELXE,DELYE,DELXB,DELYB,DELTRK,XM,YM,UM,VM
      REAL*8 XB,YB,XE,YE,DISTSEG,DIST,COSTHETA,SINTHETA
      REAL*8 UTANXX,VTANYY,UTANXM,VTANYM,UTANAVG,VTANAVG
      REAL*8 SPDTANXX,SPDTANXM
      INTEGER KODE,ICURBS
C
C
C *** SAVE INITIAL VALUES
C
      JSAV = JINOUT
      TSAV = T
      USAV = U
      VSAV = V
      XSAV = XX
      YSAV = YY
      DTTRY=T2-T
c      WRITE(45,*) ' In subroutine BOUNTRK '
c      WRITE(45,*) ' Particle starts at position ',XX,YY
c      WRITE(45,*) ' In element ',JINOUT

C
C *** SET UP BOUNDARY SEGMENT
C
      XB=X(IBSEG(ICURBS,1))
      YB=Y(IBSEG(ICURBS,1))
      XE=X(IBSEG(ICURBS,2))
      YE=Y(IBSEG(ICURBS,2))
      DELXE=XE-XX
      DELYE=YE-YY
      DELXB=XB-XX
      DELYB=YB-YY
C
C *** COMPUTE VELOCITY TANGENTIAL TO BOUNDARY AT XX,YY AND TIME T
C
      DISTSEG=SQRT((XE-XB)*(XE-XB)+(YE-YB)*(YE-YB))
      COSTHETA=(XE-XB)/DISTSEG
      SINTHETA=(YE-YB)/DISTSEG
      UTANXX=USAV*COSTHETA*COSTHETA+VSAV*SINTHETA*COSTHETA
      VTANYY=USAV*COSTHETA*SINTHETA+VSAV*SINTHETA*SINTHETA
      SPDTANXX=SQRT(UTANXX*UTANXX+VTANYY*VTANYY)

c *** If this velocity = 0, particle does not move.
      
      IF((UTANXX.EQ.0.D0).AND.(VTANYY.EQ.0.D0)) THEN
        T=T2
c        WRITE(45,*) ' UTANXX and VTANYY = 0 '
c        WRITE(45,*) ' Particle stuck at boundary node'
c        WRITE(45,*) ' Time advanced to ',T
        RETURN
        ENDIF
C      
C *** DETERMINE WHICH END OF BOUNDARY PARTICLE IS MOVING TOWARD
C
      XM=0.D0
      YM=0.D0

c *** If at one end of boundary segment (XE,YE), first check to see if 
c *** particle is moving toward the other end (XB,YB).  IF not, check to 
c *** see if particle is moving to other end of adjacent boundary segment.
c *** If it is moving in neither direction, it is temporarily stuck in 
c *** present position.

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
c          WRITE(45,*) ' UTANXX and VTANYY = 0 '
c          WRITE(45,*) ' Particle stuck at boundary node'
c          WRITE(45,*) ' Time advanced to ',T
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
c        WRITE(45,*) ' Particle stuck at boundary node'
c        WRITE(45,*) ' Time advanced to ',T
        RETURN
        ENDIF

c *** If at one end of boundary segment (XB,YB), first check to see if 
c *** particle is moving toward the other end (XE,YE).  IF not, check to 
c *** see if particle is moving to other end of adjacent boundary segment.
c *** If it is moving in neither direction, it is temporarily stuck in 
c *** present position.

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
c          WRITE(45,*) ' UTANXX and VTANYY = 0 '
c          WRITE(45,*) ' Particle stuck at boundary node'
c          WRITE(45,*) ' Time advanced to ',T
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
c        WRITE(45,*) ' Particle stuck at boundary node'
c        WRITE(45,*) ' Time advanced to ',T
        RETURN
        ENDIF

c *** If particle is at neither end of the boundary segment, check to
c *** see if it is moving toward XE,YE.

      IF((DELXE*UTANXX.GE.0.D0).AND.(DELYE*VTANYY.GE.0.D0)) THEN
        XM=XE
        YM=YE
        GOTO 10
        ENDIF

c *** If particle is at neither end of the boundary segment, check to
c *** see if it is moving toward XB,YB.

      IF((DELXB*UTANXX.GE.0.D0).AND.(DELYB*VTANYY.GE.0.D0)) THEN
        XM=XB
        YM=YB
        ENDIF

  10  CONTINUE 

c *** If XM,YM have not been set, could not figure which direction particle
c *** is moving.

      IF((XM.EQ.0.D0).AND.(YM.EQ.0.D0)) THEN
        T=T2
c        WRITE(45,*) ' COULD NOT FIGURE WHICH WAY PARTICLE IS MOVING',
c     &               ' IN BOUNTRK'
c        WRITE(45,*) ' Particle stuck '
c        WRITE(45,*) ' UTANXX,VTANYY = ',UTANXX,VTANYY
c        WRITE(45,*) ' Time advanced to ',T
        RETURN
        ENDIF
C
C *** DETERMINE VELOCITY AT END OF BOUNDARY SEGMENT PARTICLE IS MOVING TOWARD
C
      CALL VELS(JINOUT,XM,YM,UM,VM,T,NUMFILE)
      UTANXM=UM*COSTHETA*COSTHETA+VM*SINTHETA*COSTHETA
      VTANYM=UM*COSTHETA*SINTHETA+VM*SINTHETA*SINTHETA
      SPDTANXM=SQRT(UTANXM*UTANXM+VTANYM*VTANYM)
C
C *** ASSUMING VELOCITY IS STATIONARY IN TIME BUT VARYING IN SPACE, COMPUTE 
C *** TRACK LENGTH
C
      DIST=SQRT((XM-XX)*(XM-XX)+(YM-YY)*(YM-YY))
      DELTRK=2.D0*SPDTANXX*DTTRY*DIST/
     &      (2.D0*DIST-DTTRY*(SPDTANXM-SPDTANXX))
C
C *** IF PARTICLE OVER SHOOTS END OF BOUNDARY SEGMENT, PUT IT AT THE END
C  
      IF((DELTRK.GT.DIST).OR.(DIST.EQ.0.D0)) THEN
        XX=XM
        YY=YM
        DTDID=DTTRY*DIST/DELTRK
        T=TSAV+DTDID
        KODE=-1
c        WRITE(45,*) ' Particle moved along boundary to node ',XX,YY
c        WRITE(45,*) ' In element ',JINOUT,' KODE= ',KODE
c        WRITE(45,*) ' Time advanced to ',T
        RETURN
        ENDIF
C
C *** IF PARTICLE DIDNT OVER SHOOT THE END OF THE BOUNDARY SEGMENT, MOVE IT
C *** THE APPROPRIATE DISTANCE ALONG THE BOUNDARY
C      
      UTANAVG=(DELTRK*(UTANXM-UTANXX)/DIST + 2.D0*UTANXX)/2.D0
      VTANAVG=(DELTRK*(VTANYM-VTANYY)/DIST + 2.D0*VTANYY)/2.D0
      XX=DTTRY*UTANAVG+XSAV
      YY=DTTRY*VTANAVG+YSAV
      T=TSAV+DTTRY
           KODE=-1
c      WRITE(45,*) ' Particle moved along boundary to position ',XX,YY
c      WRITE(45,*) ' In element ',JINOUT,' KODE= ',KODE
c      WRITE(45,*) ' Time advanced to ',T
c      WRITE(45,*) ' '

      RETURN

      END

C***********************************************************************

C
C***********************************************************************
      SUBROUTINE TRACK(JEL,J,XSTART,YSTART,USTART,VSTART,T1,T2,DT1,IPN)
C***********************************************************************
C
C
C *** 5TH ORDER RUNGE-KUTTA TRACKING OF "X" AND "Y" OVER THE TIME INTERV
C *** [T1 TO T2] USING ADAPTIVE (VARIABLE) TRACKING TIME STEP SIZE.  FOR
C *** FURTHER DETAILS ON THE GENERAL METHODOLOGY USED IN THIS ALGORITHM,
C *** REFER TO SECTIONS 15.1 AND 15.2 OF "NUMERICAL RECIPES", BY PRESS,
C *** ET. AL.  X AND Y ARE INTEGRATED FROM "XSTART" AND "YSTART", RESPEC
C *** WITH A USER SPECIFIED ACCURACY, "EPS".  "DT1" IS THE GUESSED FIRST
C *** STEPSIZE (MAGNITUDE) AND "DTMIN" IS THE MINIMUM ALLOWED STEPSIZE
C *** (THIS CAN EQUAL ZERO).  UPON COMPLETION OF THE INTEGRATIONS, "XSTART" 
C *** AND "YSTART" ARE REPLACED WITH THE FINAL
C *** VALUES FOR "X" AND "Y".  SUBROUTINE "RKQC" IS THE STEPPER ROUTINE.
C *** NOTE:  T2 > T1  AND  T2 < T1  ARE ALLOWED (I.E., FORWARD AND BACKW
C *** TRACKING).
C
C *** SET MAXIMUM DIMENSIONS HERE.
C-----------------------------------------------------------------------
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------

      COMMON /IOPT/NUMFILE
      INTEGER NUMFILE
      INTEGER MAXSTP
      REAL*8 ZERO,TEST
      PARAMETER (MAXSTP=1000,ZERO=0.D0)
      PARAMETER (TEST=1.0D-03)
      REAL*8 EPS,DTMIN
      COMMON /ELAREAS/AR
      REAL*8 AR(NNE)
      COMMON /ERRORS/EPS,DTMIN
      INTEGER JEL,J,IPN,KODE(MXDRG),ICURBS(MXDRG)
      COMMON /NUMS/NTIME
      INTEGER NTIME
      REAL*8 XSTART,YSTART,USTART,VSTART,T1,T2,DT1
      REAL*8 xx,yy,t,dt,u0,v0,dtnext
      INTEGER icount,nstp

      XX = XSTART
      YY = YSTART
      T = T1
      DT = SIGN(DT1,T2-T1)
      J = JEL
      ICOUNT = 0
      U0 = USTART
      V0 = VSTART

      DO 10 NSTP = 1,MAXSTP

        IF (NSTP.NE.1) CALL VELS(J,XX,YY,U0,V0,T,NUMFILE)
C
C ***   CHECK WHETHER STEP SIZE WILL RESULT IN OVERSHOOTING END OF
C ***   INTEGRATION INTERVAL (T2).  IF SO, CUT DOWN STEP SIZE.
C
        IF ((T+DT-T2)* (T+DT-T1).GT.ZERO) DT = T2 - T
C
C ***   IF PARTICLE IS IN THE INTERIOR OF THE DOMAIN PERFORM A 5TH
C ***   ORDER RUNGE-KUTTA STEP.
C ***   IF PARTICLE IS ON THE BOUNDARY, TRACK IT THERE
C
        IF(KODE(IPN).LE.0) THEN
          CALL RKQC(J,XX,YY,U0,V0,T,DT,DTNEXT,KODE(IPN),
     &                                            ICURBS(IPN))
          ELSE
          CALL BOUNTRK(J,XX,YY,U0,V0,T,T2,KODE(IPN),ICURBS(IPN))
          ENDIF

C ***   IF INTEGRATION IS COMPLETE, SAVE POSITION AND SAVE RECENT
C ***   STEP SIZE AS ESTIMATE FOR NEXT TRACKING

        IF ((T-T2)* (T2-T1).GE.ZERO) THEN
          XSTART = XX
          YSTART = YY
          DT1 = DTNEXT
          RETURN
          END IF

        IF (ABS(DTNEXT).LT.DTMIN) THEN
          DTNEXT = SIGN(DTMIN,T2-T1)
          END IF
C
C ***   SET NEXT STEP SIZE
C
          DT = DTNEXT
   10     CONTINUE

      PRINT *,'TOO MANY TRACKING STEPS; ',MAXSTP,' TAKEN AT TIME = ',T
      PRINT *,'** PARTICLE ELIMINATED **'
      RETURN

      END
C
C************************************************************************
      SUBROUTINE VELS(J,X,Y,UF,VF,T,NUMFILE)
C************************************************************************
C
C-----------------------------------------------------------------------
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------
C
      COMMON /NUMS/NTIME
      INTEGER NTIME
      INTEGER J
      INTEGER NUMFILE
      REAL*8 UF, VF, X, Y, T
      REAL*8 UFUN, VFUN, UFUNTIME, VFUNTIME

      IF (NUMFILE.EQ.2) THEN
          UF = UFUNTIME(J,X,Y,T)
          VF = VFUNTIME(J,X,Y,T)
      ENDIF
      RETURN
      END


C
C***********************************************************************
       REAL*8 FUNCTION UFUNTIME(J,X,Y,T)
C     THIS FUNCTION IS USED IF TIME-SERIES VELOCITY FILES ARE USED
C***********************************************************************
C
C       DETERMINES X COMPONENT OF VELOCITY AT (X,Y)
C
C ***   SET MAXIMUM ARRAY DIMENSIONS
C-----------------------------------------------------------------------
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------
C
      COMMON /ELEMS/ELEMS
      INTEGER ELEMS(NNE,3)
      COMMON /NUMS/NTIME
      INTEGER NTIME
      COMMON /ABA0/A,B,A0
       REAL*8 A(NNE,3),B(NNE,3),A0(NNE,2)
      COMMON /ELAREAS/AR
       REAL*8 AR(NNE)
      COMMON /VEL/UNEW,VNEW
       REAL*8 UNEW(ND),VNEW(ND)
       REAL*8 A03,ARI
      INTEGER J
      INTEGER N1,N2,N3
       REAL*8 U1,U2,U3,U4,U5,U6,F1,F2,F3,F4,F5,F6
      REAL*8 X,Y,T

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
   10 CONTINUE
      RETURN
      END

C
C***********************************************************************
      REAL*8 FUNCTION VFUNTIME(J,X,Y,T)
C     THIS FUNCTION IS USED IF TIME-SERIES VELOCITY FILES ARE USED
C***********************************************************************
C
C       COMPUTES Y COMPONENT OF VELOCITY AT (X,Y)
C
C ***   SET MAXIMUM ARRAY DIMENSIONS
C-----------------------------------------------------------------------
      INCLUDE 'CB_2D.h'
C-----------------------------------------------------------------------
C
      COMMON /ELEMS/ELEMS
      INTEGER ELEMS(NNE,3)
      COMMON /NUMS/NTIME
      INTEGER NTIME
      COMMON /ABA0/A,B,A0
      REAL*8 A(NNE,3),B(NNE,3),A0(NNE,2)
      COMMON /ELAREAS/AR
      REAL*8 AR(NNE)
      REAL*8 A03,ARI
      COMMON /VEL/UNEW,VNEW
      REAL*8 UNEW(ND),VNEW(ND)
      INTEGER J
      INTEGER N1,N2,N3
       REAL*8 V1,V2,V3,V4,V5,V6,G1,G2,G3,G4,G5,G6
      REAL*8 X,Y,T

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
   10 CONTINUE
      RETURN
      END


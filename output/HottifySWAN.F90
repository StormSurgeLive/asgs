MODULE DATA

      IMPLICIT NONE

      INTEGER,ALLOCATABLE :: G2L(:,:)
      INTEGER             :: IERR
      INTEGER             :: MyProc
      INTEGER             :: NumCores
      INTEGER             :: NumDirs
      INTEGER             :: NumFreqs
      INTEGER             :: NumGlobalVerts
      INTEGER             :: NumProcs
      INTEGER             :: UnitNumber

      REAL(8),ALLOCATABLE :: Lat(:)
      REAL(8),ALLOCATABLE :: Lon(:)

      TYPE Connectivity
         INTEGER,ALLOCATABLE :: Conn(:)
      END TYPE
      TYPE(Connectivity),ALLOCATABLE :: L2G(:)

END MODULE



PROGRAM HottifySWAN

      USE DATA, ONLY: IERR,MyProc,NumProcs,UnitNumber

      IMPLICIT NONE

#ifdef MPI
      INCLUDE 'mpif.h'
#endif

      CHARACTER(LEN=1) :: UserSel
      INTEGER :: ARGCOUNT ! number of command line arguments
      INTEGER :: I        ! counter for command line arguments
      CHARACTER(2048) :: CMDLINEARG ! a command line argument
#ifdef MPI
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,MyProc,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NumProcs,IERR)
#else
      MyProc = 0
      NumProcs = 1
#endif
      UnitNumber = 0
      IF(MyProc.EQ.0)THEN
         ARGCOUNT=IARGC()
         IF (ARGCOUNT.ne.0) THEN
            DO WHILE (I.LT.ARGCOUNT) 
               I=I+1
               CALL GETARG(I,CMDLINEARG)  
               SELECT CASE(CMDLINEARG(1:2))
               CASE("-g") ! globalize
                  UserSel = "1"
               CASE("-l") ! localize
                  UserSel = "2"
               CASE("-u") ! unit number
                  I=I+1
                  CALL GETARG(I,CMDLINEARG) 
                  READ(CMDLINEARG,*) UnitNumber
               CASE DEFAULT
                  WRITE(*,'(A)',ADVANCE='YES')            &
                  "Hottify: The command line option '",   &
                  trim(CMDLINEARG),                       &
                  "' was not recognized."                 
               END SELECT
            END DO
         ELSE        
            WRITE(*,'(A)',ADVANCE='YES') " "
            WRITE(*,'(A)',ADVANCE='YES') "This program will globalize or localize a set of SWAN hot-start files."
            WRITE(*,'(A)',ADVANCE='YES') "... Do you want to:"
            WRITE(*,'(A)',ADVANCE='YES') "...... 1. Create a global file from an existing set of local files."
            WRITE(*,'(A)',ADVANCE='YES') "...... 2. Create a set of local files from an existing global file."
            WRITE(*,'(A)',ADVANCE='NO') "... Please enter your selection (1/2): "
            READ(*,'(A)') UserSel
         ENDIF
      ENDIF

#ifdef MPI
      CALL MPI_BCAST(UserSel,1,MPI_CHARACTER,0,MPI_COMM_WORLD,IERR)
#endif

      IF(UserSel.EQ."1")THEN

         IF(MyProc.EQ.0)THEN

            WRITE(*,'(A)',ADVANCE='YES') " "
            WRITE(*,'(A)',ADVANCE='YES') "You have chosen to GLOBALIZE."

            CALL SLEEP(10)

            CALL GlobalToLocal
            CALL WriteHeader
            CALL Globalize

         ELSE

            IF(MyProc.EQ.1)THEN

               CALL SLEEP(5)

               WRITE(*,'(A)',ADVANCE='YES') "... WARNING: Globalization can only be done in serial."
               WRITE(*,'(A)',ADVANCE='YES') "...... Some resources will be wasted."

            ENDIF

         ENDIF

      ELSEIF(UserSel.EQ."2")THEN

         IF(MyProc.EQ.0)THEN
            WRITE(*,'(A)',ADVANCE='YES') " "
            WRITE(*,'(A)',ADVANCE='YES') "You have chosen to LOCALIZE."
         ENDIF

         CALL LocalToGlobal
         CALL Localize

      ELSE

         IF(MyProc.EQ.0)THEN
            WRITE(*,'(A)',ADVANCE='YES') " "
            WRITE(*,'(A)',ADVANCE='YES') "Your selection is not valid."
         ENDIF

      ENDIF

      IF(MyProc.EQ.0)THEN
         WRITE(*,'(A)',ADVANCE='YES') " "
      ENDIF

#ifdef MPI
      CALL MPI_FINALIZE(IERR)
#endif

END PROGRAM



SUBROUTINE Globalize

      USE DATA

      IMPLICIT NONE

      CHARACTER(LEN=1)          :: JunkC
      CHARACTER(LEN=5*NumDirs)  :: Line
      CHARACTER(LEN=50)         :: SwanFile

      INTEGER                   :: Core
      INTEGER                   :: IC
      INTEGER                   :: ID
      INTEGER                   :: IS
      INTEGER                   :: IV
      INTEGER                   :: LocalVert
      INTEGER                   :: NumLocalVerts

      TYPE ActionDensity
         CHARACTER(LEN=6) :: Type
         CHARACTER(LEN=24) :: Factor
         INTEGER,ALLOCATABLE :: Values(:,:)
      END TYPE
      TYPE LocalInfo
         TYPE(ActionDensity),ALLOCATABLE :: Ac(:)
      END TYPE
      TYPE(LocalInfo),ALLOCATABLE :: Local(:)

      WRITE(*,'(A)',ADVANCE='YES') " "
      WRITE(*,'(A)',ADVANCE='YES') "Globalizing the local SWAN hot-start files ..."
      WRITE(*,'(A,A,A)',ADVANCE='YES') "... Building the array with the local action densities."
      WRITE(*,'(A)',ADVANCE='NO') "... Progress: +"
      !FLUSH(6)

      ALLOCATE(Local(1:NumCores))

      DO IC=1,NumCores

         CALL ShowBar(IC,NumCores)

         WRITE(SwanFile,'(A,I4.4,A,I2.2)') "PE",IC-1,"/swan.",UnitNumber
         OPEN(UNIT=99,FILE=TRIM(SwanFile),ACTION='READ')

         ! SWAN standard file, with version
         READ(99,'(A)') Line

         ! time-dependent data
         READ(99,'(A)') Line

         ! time coding option
         READ(99,'(A)') LIne

         ! locations on the globe
         READ(99,'(A)') Line

         ! number of locations
         READ(99,'(A)') Line
         READ(Line,*) NumLocalVerts
         DO IV=1,NumLocalVerts
            READ(99,'(A)') JunkC
         ENDDO

         ALLOCATE(Local(IC)%Ac(1:NumLocalVerts))

         ! relative frequencies in Hz
         READ(99,'(A)') Line

         ! number of frequencies
         READ(99,'(A)') Line
         DO IS=1,NumFreqs
            READ(99,'(A)') Line
         ENDDO

         ! spectral Cartesian directions in degr
         READ(99,'(A)') Line

         ! number of directions
         READ(99,'(A)') Line
         READ(Line,*) NumDirs
         DO ID=1,NumDirs
            READ(99,'(A)') Line
         ENDDO

         ! QUANT
         READ(99,'(A)') Line

         ! number of quantities in table
         READ(99,'(A)') Line

         ! action densities
         READ(99,'(A)') Line

         ! unit
         READ(99,'(A)') Line

         ! exception value
         READ(99,'(A)') Line

         ! date and time
         READ(99,'(A)') Line

         DO IV=1,NumLocalVerts

            READ(99,'(A)') Line
            WRITE(Local(IC)%Ac(IV)%Type,'(A)') TRIM(Line)

            IF(INDEX(TRIM(Line),"FACTOR").GT.0)THEN

               ALLOCATE(Local(IC)%Ac(IV)%Values(1:NumFreqs,1:NumDirs))

               READ(99,'(A)') Line
               WRITE(Local(IC)%Ac(IV)%Factor,'(A)') TRIM(Line)

               DO IS=1,NumFreqs
                  READ(99,*) Local(IC)%Ac(IV)%Values(IS,:)
               ENDDO

            ENDIF

         ENDDO

         CLOSE(UNIT=99,STATUS='KEEP')

      ENDDO

      WRITE(*,'(A)',ADVANCE='YES')
      WRITE(*,'(A)',ADVANCE='YES') "... Done."

      WRITE(*,'(A)',ADVANCE='YES')
      WRITE(*,'(A)',ADVANCE='YES') "Writing the global SWAN hot-start file ..."

      WRITE(SwanFile,'(A,I2.2)') "swan.",UnitNumber
      OPEN(UNIT=UnitNumber,FILE=TRIM(SwanFile),ACTION='WRITE',POSITION='APPEND')

      WRITE(*,'(A,A,A)',ADVANCE='YES') "... The hot-start information is being written to the ",TRIM(SwanFile)," file."
      WRITE(*,'(A)',ADVANCE='NO') "... Progress: +"
      !FLUSH(6)

      DO IV=1,NumGlobalVerts

         CALL ShowBar(IV,NumGlobalVerts)

         Core      = G2L(IV,1)
         LocalVert = G2L(IV,2)

         WRITE(UnitNumber,'(A)') TRIM(Local(Core)%Ac(LocalVert)%Type)

         IF(INDEX(Local(Core)%Ac(LocalVert)%Type,"FACTOR").GT.0)THEN

            WRITE(UnitNumber,'(A)') TRIM(Local(Core)%Ac(LocalVert)%Factor)
            DO IS=1,NumFreqs
               WRITE(UnitNumber,'(200(1X,I4))') Local(Core)%Ac(LocalVert)%Values(IS,:)
            ENDDO

         ENDIF

      ENDDO

      WRITE(*,'(A)',ADVANCE='YES')
      WRITE(*,'(A)',ADVANCE='YES') "... Done."

      CLOSE(UNIT=UnitNumber,STATUS='KEEP')

      RETURN

END SUBROUTINE



SUBROUTINE GlobalToLocal

      USE DATA

      IMPLICIT NONE

      CHARACTER(LEN=50) :: Fort18File
      CHARACTER(LEN=1)  :: JunkC

      INTEGER           :: Core
      INTEGER           :: IC
      INTEGER           :: IE
      INTEGER           :: IV
      INTEGER           :: JunkI
      INTEGER           :: NumLocalElems
      INTEGER           :: NumLocalVerts
      INTEGER           :: Vert

      WRITE(*,'(A)',ADVANCE='YES') " "
      WRITE(*,'(A)',ADVANCE='YES') "Developing the global-to-local connectivity ..."

      OPEN(UNIT=14,FILE='fort.14',ACTION='READ')

      READ(14,'(A)') JunkC
      READ(14,*) JunkI,NumGlobalVerts

      ALLOCATE(Lon(1:NumGlobalVerts))
      ALLOCATE(Lat(1:NumGlobalVerts))

      DO IV=1,NumGlobalVerts
         READ(14,*) JunkI,Lon(IV),Lat(IV)
      ENDDO

      CLOSE(UNIT=14,STATUS='KEEP')

      WRITE(*,'(A,I8.8,A)',ADVANCE='YES') "... The global mesh has ",NumGlobalVerts," vertices."

      ALLOCATE(G2L(1:NumGlobalVerts,1:2))

      OPEN(UNIT=99,FILE='partmesh.txt',ACTION='READ')

      NumCores = 0
      DO IV=1,NumGlobalVerts
         READ(99,*) Core
         IF(Core.GT.NumCores) NumCores = Core
         G2L(IV,1) = Core
      ENDDO

      CLOSE(UNIT=99,STATUS='KEEP')

      WRITE(*,'(A,I8.8,A)',ADVANCE='YES') "... It has been decomposed on ",NumCores," computational cores."
      WRITE(*,'(A)',ADVANCE='NO') "... Progress: +"

      DO IC=1,NumCores

         CALL ShowBar(IC,NumCores)

         WRITE(Fort18File,'(A,I4.4,A)') "PE",IC-1,"/fort.18"

         OPEN(UNIT=18,FILE=TRIM(Fort18File),ACTION='READ')

         READ(18,'(A)') JunkC
         READ(18,'(A8,I12,I12,I12)') JunkC,JunkI,JunkI,NumLocalElems

         DO IE=1,NumLocalElems
            READ(18,'(A)') JunkC
         ENDDO

         READ(18,'(A8,I12,I12,I12)') JunkC,JunkI,JunkI,NumLocalVerts

         DO IV=1,NumLocalVerts
            READ(18,*) Vert
            IF(Vert.GT.0)THEN
               G2L(Vert,2) = IV
               !WRITE(*,*) IV,NumLocalVerts  ! jgfdebug
            ENDIF
         ENDDO

         CLOSE(UNIT=18,STATUS='KEEP')

      ENDDO

      WRITE(*,'(A)',ADVANCE='YES')
      WRITE(*,'(A)',ADVANCE='YES') "... Done."

      RETURN

END SUBROUTINE



SUBROUTINE Localize

      USE DATA

      IMPLICIT NONE

#ifdef MPI
      INCLUDE 'mpif.h'
#endif

      CHARACTER(LEN=1000) :: Line
      CHARACTER(LEN=50)   :: SwanFile

      INTEGER             :: GlobalVert
      INTEGER             :: IC
      INTEGER             :: ID
      INTEGER             :: IS
      INTEGER             :: IV
      INTEGER             :: LoopEnd
      INTEGER             :: LoopStart
      INTEGER             :: NumLocalVerts

      TYPE ActionDensity
         CHARACTER(LEN=6) :: Type
         CHARACTER(LEN=24) :: Factor
         INTEGER,ALLOCATABLE :: Values(:,:)
      END TYPE
      TYPE(ActionDensity),ALLOCATABLE :: Ac(:)

      If(MyProc.EQ.0)THEN
         WRITE(*,'(A)',ADVANCE='YES')
         WRITE(*,'(A)',ADVANCE='YES') "Reading the global SWAN hot-start file ..."
         IF (UnitNumber.eq.0) THEN
            WRITE(*,'(A)',ADVANCE='NO') "... Please enter the unit number of the global file (67/68): "
            READ(*,*) UnitNumber
         ENDIF
      ENDIF

#ifdef MPI
      CALL MPI_BCAST(UnitNumber,1,MPI_INTEGER,0,MPI_COMM_WORLD,MPI_STATUS_IGNORE,IERR)
#endif

      WRITE(SwanFile,'(A,I2.2)') "swan.",UnitNumber

      IF(MyProc.EQ.0)THEN
         WRITE(*,'(A,A,A)',ADVANCE='YES') "... Now processing the ",TRIM(SwanFile)," file."
         WRITE(*,'(A)',ADVANCE='NO') "... Progress: +"
      ENDIF

      OPEN(UNIT=UnitNumber,FILE=TRIM(SwanFile),ACTION='READ')
 
      ! SWAN standard file, with version
      READ(UnitNumber,'(A)') Line

      ! time-dependent data
      READ(UnitNumber,'(A)') Line

      ! time coding option
      READ(UnitNumber,'(A)') Line

      ! locations on the globe
      READ(UnitNumber,'(A)') Line

      ! number of locations
      READ(UnitNumber,'(A)') Line
      READ(Line,*) NumGlobalVerts
      DO IV=1,NumGlobalVerts
         READ(UnitNumber,'(A)') Line
      ENDDO

      ALLOCATE(Ac(1:NumGlobalVerts))

      ! relative frequencies in Hz
      READ(UnitNumber,'(A)') Line

      ! number of frequencies
      READ(UnitNumber,'(A)') Line
      READ(Line,*) NumFreqs
      DO IS=1,NumFreqs
         READ(UnitNumber,'(A)') Line
      ENDDO

      ! spectral Cartesian directions in degr
      READ(UnitNumber,'(A)') Line

      ! number of directions
      READ(UnitNumber,'(A)') Line
      READ(Line,*) NumDirs
      DO ID=1,NumDirs
         READ(UnitNumber,'(A)') Line
      ENDDO

      ! QUANT
      READ(UnitNumber,'(A)') Line

      ! number of quantities in table
      READ(UnitNumber,'(A)') Line

      ! action densities
      READ(UnitNumber,'(A)') Line

      ! unit
      READ(UnitNumber,'(A)') Line

      ! exception value
      READ(UnitNumber,'(A)') Line

      ! date and time
      READ(UnitNumber,'(A)') Line

      DO IV=1,NumGlobalVerts

         IF(MyProc.EQ.0)THEN
            CALL ShowBar(IV,NumGlobalVerts)
         ENDIF

         READ(UnitNumber,'(A)') Line
         WRITE(Ac(IV)%Type,'(A)') TRIM(Line)

         IF(INDEX(TRIM(Line),"FACTOR").GT.0)THEN

            ALLOCATE(Ac(IV)%Values(1:NumFreqs,1:NumDirs))

            READ(UnitNumber,'(A)') Line
            WRITE(Ac(IV)%Factor,'(A)') TRIM(Line)

            DO IS=1,NumFreqs
               READ(UnitNumber,*) Ac(IV)%Values(IS,:)
            ENDDO

         ENDIF

      ENDDO

      CLOSE(UNIT=UnitNumber,STATUS='KEEP')

      IF(MyProc.EQ.0)THEN
         WRITE(*,'(A)',ADVANCE='YES')
         WRITE(*,'(A)',ADVANCE='YES') "... Done."
         WRITE(*,'(A)',ADVANCE='YES')
         WRITE(*,'(A)',ADVANCE='YES') "Writing the local SWAN hot-start files ..."
         WRITE(*,'(A)',ADVANCE='NO') "... Progress: +"
      ENDIF

#ifdef MPI
      LoopStart = MyProc*(NumCores/NumProcs)
      IF(MyProc.EQ.0) LoopStart = 1
      IF(LoopStart.LT.1) LoopStart = 1
      LoopEnd = (MyProc+1)*(NumCores/NumProcs)
      IF(MyProc.EQ.(NumProcs-1)) LoopEnd = NumCores
      IF(LoopEnd.GT.NumCores) LoopEnd = NumCores
#else
      LoopStart = 1
      LoopEnd = NumCores
#endif

      DO IC=LoopStart,LoopEnd

         IF(MyProc.EQ.0)THEN
            CALL ShowBar(IC,LoopEnd-LoopStart+1)
         ENDIF

         WRITE(SwanFile,'(A,I2.2)') "swan.",UnitNumber
         OPEN(UNIT=UnitNumber,FILE=TRIM(SwanFile),ACTION='READ')

         WRITE(SwanFile,'(A,I4.4,A,I2.2)') "PE",IC-1,"/swan.",UnitNumber
         OPEN(UNIT=99,FILE=TRIM(SwanFile),ACTION='WRITE')

         ! SWAN standard file, with version
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! time-dependent data
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! time coding option
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! locations on the globe
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! number of locations
         READ(UnitNumber,'(A)') Line
         DO IV=1,NumGlobalVerts
            READ(UnitNumber,'(A)') Line
         ENDDO
         NumLocalVerts = SIZE(L2G(IC)%Conn)
         WRITE(99,*) NumLocalVerts
         DO IV=1,NumLocalVerts
            GlobalVert = ABS(L2G(IC)%Conn(IV))
            WRITE(99,'(1X,F11.6,1X,F11.6)') Lon(GlobalVert),Lat(GlobalVert)
         ENDDO

         ! relative frequencies in Hz
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! number of frequencies
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)
         READ(Line,*) NumFreqs
         DO IS=1,NumFreqs
            READ(UnitNumber,'(A)') Line
            WRITE(99,'(A)') TRIM(Line)
         ENDDO

         ! spectral Cartesian directions in degr
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! number of directions
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)
         READ(Line,*) NumDirs
         DO ID=1,NumDirs
            READ(UnitNumber,'(A)') Line
            WRITE(99,'(A)') TRIM(Line)
         ENDDO

         ! QUANT
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! number of quantities in table
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! action densities
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! unit
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! exception value
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         ! date and time
         READ(UnitNumber,'(A)') Line
         WRITE(99,'(A)') TRIM(Line)

         CLOSE(UNIT=UnitNumber,STATUS='KEEP')

         DO IV=1,NumLocalVerts

            GlobalVert = ABS(L2G(IC)%Conn(IV))

            WRITE(99,'(A)') TRIM(Ac(GlobalVert)%Type)

            IF(INDEX(Ac(GlobalVert)%Type,"FACTOR").GT.0)THEN

               WRITE(99,'(A)') TRIM(Ac(GlobalVert)%Factor)
               DO IS=1,NumFreqs
                  WRITE(99,'(200(1X,I4))') Ac(GlobalVert)%Values(IS,:)
               ENDDO

            ENDIF

         ENDDO

         CLOSE(UNIT=99,STATUS='KEEP')

      ENDDO

      IF(MyProc.EQ.0)THEN
         WRITE(*,'(A)',ADVANCE='YES')
         WRITE(*,'(A)',ADVANCE='YES') "... Done."
      ENDIF

      RETURN

END SUBROUTINE



SUBROUTINE LocalToGlobal

      USE DATA

      IMPLICIT NONE

      CHARACTER(LEN=50) :: Fort18File
      CHARACTER(LEN=1)  :: JunkC

      INTEGER           :: Core
      INTEGER           :: IC
      INTEGER           :: IE
      INTEGER           :: IV
      INTEGER           :: JunkI
      INTEGER           :: NumLocalElems
      INTEGER           :: NumLocalVerts
      INTEGER           :: Vert

      IF(MyProc.EQ.0)THEN
         WRITE(*,'(A)',ADVANCE='YES') " "
         WRITE(*,'(A)',ADVANCE='YES') "Developing the local-to-global connectivity ..."
      ENDIF

      OPEN(UNIT=14,FILE='fort.14',ACTION='READ')

      READ(14,'(A)') JunkC
      READ(14,*) JunkI,NumGlobalVerts

      ALLOCATE(Lon(1:NumGlobalVerts))
      ALLOCATE(Lat(1:NumGlobalVerts))

      DO IV=1,NumGlobalVerts
         READ(14,*) JunkI,Lon(IV),Lat(IV)
      ENDDO

      CLOSE(UNIT=14,STATUS='KEEP')

      IF(MyProc.EQ.0)THEN
         WRITE(*,'(A,I8.8,A)',ADVANCE='YES') "... The global mesh has ",NumGlobalVerts," vertices."
      ENDIF

      OPEN(UNIT=99,FILE='partmesh.txt',ACTION='READ')

      NumCores = 0
      DO IV=1,NumGlobalVerts
         READ(99,*) Core
         IF(Core.GT.NumCores) NumCores = Core
      ENDDO

      CLOSE(UNIT=99,STATUS='KEEP')

      ALLOCATE(L2G(1:NumCores))

      IF(MyProc.EQ.0)THEN
         WRITE(*,'(A,I8.8,A)',ADVANCE='YES') "... It has been decomposed on ",NumCores," computational cores."
         WRITE(*,'(A)',ADVANCE='NO') "... Progress: +"
      ENDIF

      DO IC=1,NumCores

         IF(MyProc.EQ.0)THEN
            CALL ShowBar(IC,NumCores)
         ENDIF

         WRITE(Fort18File,'(A,I4.4,A)') "PE",IC-1,"/fort.18"

         OPEN(UNIT=18,FILE=TRIM(Fort18File),ACTION='READ')

         READ(18,'(A)') JunkC
         READ(18,'(A8,I12,I12,I12)') JunkC,JunkI,JunkI,NumLocalElems

         DO IE=1,NumLocalElems
            READ(18,'(A)') JunkC
         ENDDO

         READ(18,'(A8,I8,I8,I8)') JunkC,JunkI,JunkI,NumLocalVerts

         ALLOCATE(L2G(IC)%Conn(1:NumLocalVerts))

         DO IV=1,NumLocalVerts
            READ(18,*) Vert
            L2G(IC)%Conn(IV) = Vert
         ENDDO

         CLOSE(UNIT=18,STATUS='KEEP')

      ENDDO

      IF(MyProc.EQ.0)THEN
         WRITE(*,'(A)',ADVANCE='YES')
         WRITE(*,'(A)',ADVANCE='YES') "... Done."
      ENDIF

      RETURN

END SUBROUTINE



SUBROUTINE WriteHeader

      USE DATA

      IMPLICIT NONE

      CHARACTER(LEN=15)   :: CurrentTime
      CHARACTER(LEN=1)    :: JunkC
      CHARACTER(LEN=1000) :: Line
      CHARACTER(LEN=50)   :: SwanFile

      INTEGER             :: IC
      INTEGER             :: ID
      INTEGER             :: IS
      INTEGER             :: IV
      INTEGER             :: NumLocalVerts

      WRITE(*,'(A)',ADVANCE='YES') " "
      WRITE(*,'(A)',ADVANCE='YES') "Writing the header information ..."
      IF (UnitNumber.eq.0) THEN
         WRITE(*,'(A)',ADVANCE='NO') "... Enter the unit number for the file (67/68): "
         READ(*,*) UnitNumber
      ENDIF

      WRITE(SwanFile,'(A,I2.2)') "PE0000/swan.",UnitNumber
      OPEN(UNIT=99,FILE=TRIM(SwanFile),ACTION='READ')

      WRITE(SwanFile,'(A,I2.2)') "swan.",UnitNumber
      OPEN(UNIT=UnitNumber,FILE=TRIM(SwanFile),ACTION='WRITE')

      ! SWAN standard file, with version
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! time-dependent data
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! time coding option
      READ(99,'(A)') LIne
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! locations on the globe
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! number of locations
      READ(99,'(A)') Line
      READ(Line,*) NumLocalVerts
      DO IV=1,NumLocalVerts
         READ(99,'(A)') JunkC
      ENDDO
      WRITE(UnitNumber,*) NumGlobalVerts
      DO IV=1,NumGlobalVerts
         WRITE(UnitNumber,'(F12.6,F12.6)') Lon(IV),Lat(IV)
      ENDDO

      ! relative frequencies in Hz
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! number of frequencies
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)
      READ(Line,*) NumFreqs
      WRITE(*,'(A,I2.2,A)',ADVANCE='YES') "... These files contain ",NumFreqs," frequency bins."
      DO IS=1,NumFreqs
         READ(99,'(A)') Line
         WRITE(UnitNumber,'(A)') TRIM(Line)
      ENDDO

      ! spectral Cartesian directions in degr
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! number of directions
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)
      READ(Line,*) NumDirs
      WRITE(*,'(A,I2.2,A)',ADVANCE='YES') "... These files contain ",NumDirs," directional bins."
      DO ID=1,NumDirs
         READ(99,'(A)') Line
         WRITE(UnitNumber,'(A)') TRIM(Line)
      ENDDO

      ! QUANT
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! number of quantities in table
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! action densities
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! unit
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! exception value
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)

      ! date and time
      READ(99,'(A)') Line
      WRITE(UnitNumber,'(A)') TRIM(Line)
      READ(Line,'(A15)') CurrentTime
      WRITE(*,'(A,A,A)',ADVANCE='YES') "... These files are time-stamped to ",TRIM(CurrentTime),"."

      CLOSE(UNIT=99,STATUS='KEEP')

      CLOSE(UNIT=UnitNumber,STATUS='KEEP')

      WRITE(*,'(A)',ADVANCE='YES') "... Done."

      RETURN

END SUBROUTINE



SUBROUTINE ShowBar(Now,Total)

       IMPLICIT NONE

       INTEGER,INTENT(IN) :: Now
       INTEGER,INTENT(IN) :: Total

       INTEGER            :: N

       outer: DO N=1,20
          IF(Now.EQ.CEILING(N*0.05*REAL(Total)))THEN
             IF(MOD(N,5).EQ.0)THEN
                WRITE(*,'(A)',ADVANCE='NO') "+"
             ELSE
                WRITE(*,'(A)',ADVANCE='NO') "-"
             ENDIF
             !FLUSH(6)
             EXIT outer
          ENDIF
       ENDDO outer

END SUBROUTINE

MODULE DATA

        CHARACTER(LEN=50)                :: AlphaLabel
        CHARACTER(LEN=50)                :: BoundariesColor
        CHARACTER(LEN=50)                :: CoastlineColor
        CHARACTER(LEN=50)                :: CoastlineFile
        CHARACTER(LEN=40)                :: ColorLines
        CHARACTER(LEN=50)                :: ContourFile1
        CHARACTER(LEN=50)                :: ContourFile2
        CHARACTER(LEN=50)                :: ContourFileFormat
        CHARACTER(LEN=40)                :: ContourLabelRotation
        CHARACTER(LEN=40)                :: ContourUnits
        CHARACTER(LEN=50)                :: ContourXYZFile
        CHARACTER(LEN=40)                :: DiffContoursFile
        CHARACTER(LEN=40)                :: Fort14File
        CHARACTER(LEN=100)               :: InputFile
        CHARACTER(LEN=40)                :: LabelsColor
        CHARACTER(LEN=40)                :: LabelsFile
        CHARACTER(LEN=50)                :: Palette
        CHARACTER(LEN=50)                :: Path
        CHARACTER(LEN=50)                :: PlotLabel
        CHARACTER(LEN=50)                :: PlotLabelFile
        CHARACTER(LEN=40)                :: SMSPalette
        CHARACTER(LEN=60)                :: TempLabelsFile
        CHARACTER(LEN=60)                :: TempMapFile1
        CHARACTER(LEN=60)                :: TempMapFile2
        CHARACTER(LEN=50)                :: TempPath
        CHARACTER(LEN=50)                :: TimeCurrentFile
        CHARACTER(LEN=50)                :: TimeCurrentTextFile
        CHARACTER(LEN=50)                :: TimeMaxFile
        CHARACTER(LEN=40)                :: VectorFile
        CHARACTER(LEN=50)                :: VectorFileFormat
        CHARACTER(LEN=40)                :: VectorScaleFile
        CHARACTER(LEN=40)                :: VectorTextFile
        CHARACTER(LEN=40)                :: VectorUnits
        CHARACTER(LEN=50)                :: VectorUFile
        CHARACTER(LEN=50)                :: VectorVFile

        INTEGER                          :: ContourFileType
        INTEGER                          :: ContourLabelEvery
        INTEGER                          :: ContourLabelSize
        INTEGER                          :: FindContourRange
        INTEGER                          :: FindVectorScale
        INTEGER                          :: IERR
        INTEGER                          :: IfAddPlotLabel
        INTEGER                          :: IfAddTimeBar
        INTEGER                          :: IfPlotBoundaries
        INTEGER                          :: IfPlotCoastline
        INTEGER                          :: IfPlotContourLines
        INTEGER                          :: IfPlotFilledContours
        INTEGER                          :: IfPlotGrid
        INTEGER                          :: IfPlotLabels
        INTEGER                          :: IfPlotVectors
        INTEGER                          :: LargeJPGResolution
        INTEGER                          :: MyRank
        INTEGER                          :: NumNodesGlobal
        INTEGER                          :: NumNodesLocal
        INTEGER                          :: NumProcs
        INTEGER                          :: NumRecords
        INTEGER                          :: NumRecs
        INTEGER,DIMENSION(:),ALLOCATABLE :: RecordsList
        INTEGER                          :: RemoveFiles = 1
        INTEGER                          :: SmallJPGWidth
        INTEGER                          :: SplitBy
        INTEGER                          :: VectorFileType
        INTEGER                          :: Verbose
        INTEGER,DIMENSION(:),ALLOCATABLE :: XYZNodes

        REAL(8)                          :: BorderIncrementMajor
        REAL(8)                          :: BorderIncrementMinor
        REAL(8)                          :: Buffer
        REAL(8)                          :: ContourConversionFactor
        REAL(8)                          :: ContourInterval
        REAL(8)                          :: ContourLabelMinDist
        REAL(8)                          :: ContourMax
        REAL(8)                          :: ContourMin
        REAL(8)                          :: CurrentTime
        REAL(8)                          :: LatLonBuffer = 0.25
        REAL(8)                          :: LatN
        REAL(8)                          :: LatS
        REAL(8)                          :: LongE
        REAL(8)                          :: LongW
        REAL(8)                          :: ScaleHeight
        REAL(8)                          :: ScaleLabelEvery
        REAL(8)                          :: ScaleWidth
        REAL(8)                          :: VectorConversionFactor
        REAL(8)                          :: VectorMag
        REAL(8)                          :: VectorHeadLength
        REAL(8)                          :: VectorHeadWidth
        REAL(8)                          :: VectorScaleMag
        REAL(8)                          :: VectorSpacing
        REAL(8)                          :: VectorTailWidth
        REAL(8)                          :: Width
        REAL(8),DIMENSION(:),ALLOCATABLE :: X(:)
        REAL(8),DIMENSION(:),ALLOCATABLE :: Y(:)
        REAL(8),DIMENSION(:),ALLOCATABLE :: Z(:)

END MODULE



PROGRAM FigureGen

        USE DATA

        IMPLICIT NONE

#ifdef CMPI
        INCLUDE 'mpif.h'
        INTEGER                          :: Counter
        INTEGER,DIMENSION(:),ALLOCATABLE :: MPIRequests
        INTEGER                          :: RecordsFinished
        INTEGER,DIMENSION(:),ALLOCATABLE :: RecordsOnProcs
#endif

        CHARACTER(LEN=1)                 :: JunkC

        INTEGER                          :: I
        INTEGER                          :: JunkI
        INTEGER                          :: WorkingRecord

        REAL(8)                          :: JunkR

#ifdef CMPI
        CALL MPI_INIT(IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD, MyRank, IERR)
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NumProcs, IERR)
#else
        MyRank = 0
#endif

        IF(MyRank.EQ.0)THEN

            WRITE(*,'(A)',ADVANCE="YES") " "
            WRITE(*,'(A)',ADVANCE="YES") "---------------------------------------------"
            WRITE(*,'(A)',ADVANCE="YES") "FigureGen                          2008/08/15"
            WRITE(*,'(A)',ADVANCE="YES") " "
            WRITE(*,'(A)',ADVANCE="YES") "This program reads raw ADCIRC output files"
            WRITE(*,'(A)',ADVANCE="YES") "and uses GMT to generate a figure with"
            WRITE(*,'(A)',ADVANCE="YES") "contours and vectors plotted within a"
            WRITE(*,'(A)',ADVANCE="YES") "specified lat/lon box."
            WRITE(*,'(A)',ADVANCE="YES") "---------------------------------------------"

            WRITE(*,'(A)',ADVANCE="YES") " "
!            WRITE(*,'(A)',ADVANCE="NO") "Enter name of input file: "
!            READ(*,'(A)') InputFile
              InputFile = 'FG_lpfs.inp'

        ENDIF

#ifdef CMPI
        CALL MPI_BCAST(InputFile, 100, MPI_CHARACTER, 0, MPI_COMM_WORLD, IERR)
#endif

        CALL ReadInputFile

        IF(MyRank.EQ.0)THEN

            IF(Verbose.GE.2)THEN
                WRITE(*,'(A)',ADVANCE="YES") " "
                WRITE(*,'(A)',ADVANCE="YES") "PRE-PROCESSING:"
                WRITE(*,'(A)',ADVANCE="YES") " "
            ENDIF

            CALL ProcessFort14File

            IF((TRIM(ContourFileFormat).EQ."OUTPUT-FULL").OR.(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE"))THEN
                OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                READ(UNIT=19,FMT='(A)') JunkC
                READ(UNIT=19,FMT=*) NumRecs, JunkI, JunkR, JunkI, ContourFileType
                CLOSE(UNIT=19,STATUS="KEEP")
            ENDIF

            IF(NumRecords.EQ.0)THEN
                NumRecords = NumRecs
                ALLOCATE(RecordsList(1:NumRecords))
                DO I=1,NumRecords
                    RecordsList(I) = I
                ENDDO
            ENDIF

            IF(FindContourRange.EQ.1)THEN
                CALL FindContourMinMax
            ENDIF
            IF(FindVectorScale.EQ.1)THEN
                CALL FindVectorScaleMag
            ENDIF

            CALL CreateCPTFiles

        ENDIF

#ifdef CMPI
        IF(FindContourRange.EQ.1)THEN
            CALL MPI_BCAST(ContourMax, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, IERR)
            CALL MPI_BCAST(ContourMin, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, IERR)
        ENDIF
        IF(FindVectorScale.EQ.1)THEN
            CALL MPI_BCAST(VectorScaleMag, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, IERR)
        ENDIF
        CALL MPI_BCAST(NumNodesGlobal, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
        CALL MPI_BCAST(NumNodesLocal, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
#endif

        IF(MyRank.NE.0)THEN
            ALLOCATE(XYZNodes(1:NumNodesLocal))
            ALLOCATE(X(1:NumNodesLocal))
            ALLOCATE(Y(1:NumNodesLocal))
            ALLOCATE(Z(1:NumNodesLocal))
        ENDIF

#ifdef CMPI
        CALL MPI_BCAST(XYZNodes, NumNodesLocal, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
        CALL MPI_BCAST(X, NumNodesLocal, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, IERR)
        CALL MPI_BCAST(Y, NumNodesLocal, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, IERR)
        CALL MPI_BCAST(Z, NumNodesLocal, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, IERR)
#endif

        IF(MyRank.EQ.0)THEN

            IF(Verbose.GE.2)THEN
                WRITE(*,'(A)') " "
                WRITE(*,'(A)') "GENERATING IMAGES:"
                WRITE(*,'(A)') " "
            ENDIF

#ifdef CMPI
            ALLOCATE(MPIRequests(1:(NumProcs-1)))
            ALLOCATE(RecordsOnProcs(1:(NumProcs-1)))
            Counter = 0
            JunkI = 0
            RecordsFinished = 0

! Send the initial batch of records to the processors.

            IF((NumProcs-1).GT.NumRecords)THEN
                IF(Verbose.GE.1)THEN
                    WRITE(*,'(A)') "WARNING: There are more processors to use than time snaps to assign."// &
                                   "  Some resources will be wasted."
                ENDIF
                NumProcs = NumRecords + 1
            ENDIF

            DO I=1,NumProcs-1
                Counter = Counter + 1
                WorkingRecord = RecordsList(Counter)
                RecordsOnProcs(I) = WorkingRecord
                CALL MPI_SEND(WorkingRecord, 1, MPI_INTEGER, I, 1, MPI_COMM_WORLD, IERR)
                CALL MPI_IRECV(JunkI, 1, MPI_INTEGER, I, RecordsOnProcs(I), &
                               MPI_COMM_WORLD, MPIRequests(I), IERR)
            ENDDO

! Loop continuously over the processors, testing to see if any are finished.  When a processor finishes,
! assign the next record to it.

            outer1: DO

                IF(RecordsFinished.GE.NumRecords)THEN

                    EXIT outer1

                ENDIF

                CALL MPI_WAITANY(NumProcs-1, MPIRequests, I, MPI_STATUS_IGNORE, IERR)

                RecordsFinished = RecordsFinished + 1
                IF(Verbose.GE.4)THEN
                    WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," received the all-clear"//  &
                                                 " from processor ",I,"."
                ENDIF

                IF(Counter.LT.NumRecords)THEN

                    Counter = Counter + 1
                    WorkingRecord = RecordsList(Counter)
                    RecordsOnProcs(I) = WorkingRecord
                    CALL MPI_SEND(WorkingRecord, 1, MPI_INTEGER, I, 1, &
                                  MPI_COMM_WORLD, IERR)
                    CALL MPI_IRECV(JunkI, 1, MPI_INTEGER, I, RecordsOnProcs(I), &
                                  MPI_COMM_WORLD, MPIRequests(I), IERR)

                ELSE

                    WorkingRecord = 0
                    CALL MPI_SEND(WorkingRecord, 1, MPI_INTEGER, I, 1, MPI_COMM_WORLD, IERR)

                ENDIF

            ENDDO outer1

        ELSE

            outer2: DO
#else
            outer2: DO I=1,NumRecords
#endif

#ifdef CMPI
                CALL MPI_RECV(WorkingRecord, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, IERR)
#else
                WorkingRecord = RecordsList(I)
#endif

                IF(WorkingRecord.GT.0)THEN

                    IF(Verbose.GE.3)THEN
                        WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," started record ",WorkingRecord,"."
                    ENDIF

                    CALL WriteXYZFiles(WorkingRecord)
                    CALL WritePSImage(WorkingRecord)

                    IF(Verbose.GE.2)THEN
                        WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," completed record ",WorkingRecord,"."
                    ENDIF

#ifdef CMPI
                    CALL MPI_SEND(JunkI, 1, MPI_INTEGER, 0, WorkingRecord, MPI_COMM_WORLD, IERR)
#endif

                    IF(RemoveFiles.EQ.1)THEN
                        CALL Finisher(1)
                    ENDIF

#ifdef CMPI
                ELSE

                    EXIT outer2
#endif

                ENDIF

            ENDDO outer2

        ENDIF

#ifdef CMPI
        CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
#endif

        IF(RemoveFiles.EQ.1)THEN
            CALL Finisher(0)
        ENDIF

#ifdef CMPI
        IF(ALLOCATED(RecordsOnProcs))   DEALLOCATE(RecordsOnProcs)
        CALL MPI_FINALIZE(IERR)
#endif

END PROGRAM



REAL(8) FUNCTION ComputeDistance(Lon1, Lat1, Lon2, Lat2)

        IMPLICIT NONE

        INTRINSIC :: COS
        INTRINSIC :: SQRT

        REAL(8) :: AdjLat1
        REAL(8) :: AdjLat2
        REAL(8) :: AdjLon1
        REAL(8) :: AdjLon2
        REAL(8) :: Deg2Rad = 0.01745329252
        REAL(8) :: EarthRad = 6378206.4
        REAL(8) :: Lat1
        REAL(8) :: Lat2
        REAL(8) :: Lon1
        REAL(8) :: Lon2
        REAL(8) :: SFEA0
        REAL(8) :: SLAM0

        Deg2Rad = 0.01745329252
        EarthRad = 6378206.4
        SLAM0 = 265.5
        SFEA0 = 29.0

        SLAM0 = Deg2Rad * SLAM0
        SFEA0 = Deg2Rad * SFEA0

        AdjLon1 = Deg2Rad * Lon1
        AdjLat1 = Deg2Rad * Lat1
        AdjLon2 = Deg2Rad * Lon2
        AdjLat2 = Deg2Rad * Lat2

        AdjLon1 = EarthRad * (AdjLon1 - SLAM0) * COS(SFEA0)
        AdjLat1 = EarthRad * AdjLat1
        AdjLon2 = EarthRad * (AdjLon2 - SLAM0) * COS(SFEA0)
        AdjLat2 = EarthRad * AdjLat2

        ComputeDistance = SQRT((AdjLon1-AdjLon2)**2.0+(AdjLat1-AdjLat2)**2.0)

END FUNCTION



SUBROUTINE CreateCPTFiles

        USE DATA

        IMPLICIT NONE

        INTRINSIC                                  :: ABS
        INTRINSIC                                  :: MOD
        INTRINSIC                                  :: NINT
        INTRINSIC                                  :: TRIM

        CHARACTER(LEN=1)                           :: JunkC

        INTEGER                                    :: I
        INTEGER                                    :: J
        INTEGER                                    :: JunkI
        INTEGER                                    :: K
        INTEGER                                    :: L
        INTEGER,DIMENSION(:),ALLOCATABLE           :: MakeGray
        INTEGER                                    :: NumColors
        INTEGER                                    :: NumDiffContours
        INTEGER                                    :: TempIndex
        INTEGER                                    :: TempSplitBy

        REAL(8)                                    :: CurrentContour
        REAL(8),DIMENSION(:),ALLOCATABLE           :: DiffContours

        TYPE ColorData 
            REAL(8) Value1
            REAL(8) Red1
            REAL(8) Green1
            REAL(8) Blue1
            REAL(8) Value2
            REAL(8) Red2
            REAL(8) Green2
            REAL(8) Blue2
        END TYPE
        TYPE(ColorData), ALLOCATABLE, DIMENSION(:) :: GMTColors
        TYPE(ColorData), ALLOCATABLE, DIMENSION(:) :: SMSColors

        IF(TRIM(Palette).EQ."SMS+INTERVALS")THEN

            OPEN(UNIT=24,FILE=TRIM(DiffContoursFile),ACTION="READ")

            READ(UNIT=24,FMT=*) NumDiffContours

            ALLOCATE(DiffContours(1:NumDiffContours))
            ALLOCATE(MakeGray(1:NumDiffContours-1))

            DO I=1,NumDiffContours-1
                READ(UNIT=24,FMT=*) DiffContours(I), MakeGray(I)
            ENDDO
            READ(UNIT=24,FMT=*) DiffContours(NumDiffContours)

            CLOSE(UNIT=24,STATUS="KEEP")

        ENDIF

        IF((TRIM(Palette).EQ."SMS").OR.(TRIM(Palette).EQ."SMS+INTERVALS"))THEN

            OPEN(UNIT=14,FILE=SMSPalette,ACTION="READ")

            READ(UNIT=14,FMT='(A)') JunkC
            READ(UNIT=14,FMT='(A)') JunkC
            READ(UNIT=14,FMT=*)     JunkC, NumColors
            READ(UNIT=14,FMT='(A)') JunkC

            ALLOCATE(SMSColors(1:NumColors))

            DO I=1,NumColors

                READ(UNIT=14,FMT=*) SMSColors(I)%Value1, &
                                    SMSColors(I)%Red1,   &
                                    SMSColors(I)%Green1, &
                                    SMSColors(I)%Blue1

            ENDDO

            CLOSE(UNIT=14,STATUS="KEEP")

        ELSE

            NumColors = 3

            ALLOCATE(SMSColors(1:NumColors))

            SMSColors(1)%Value1 = 0.0
            SMSColors(1)%Red1   = 0
            SMSColors(1)%Green1 = 0
            SMSColors(1)%Blue1  = 255

            SMSColors(2)%Value1 = 0.5
            SMSColors(2)%Red1   = 0
            SMSColors(2)%Green1 = 255
            SMSColors(2)%Blue1  = 0

            SMSColors(3)%Value1 = 1.0
            SMSColors(3)%Red1   = 255 
            SMSColors(3)%Green1 = 0
            SMSColors(3)%Blue1  = 0

        ENDIF

        DO K=1,3

            IF(TRIM(Palette).EQ."SMS+INTERVALS")THEN

                IF(K.EQ.1)THEN

                    TempSplitBy = SplitBy

                ELSEIF(K.EQ.2)THEN

                    TempSplitBy = 1

                ELSEIF(K.EQ.3)THEN

                    TempSplitBy = 1

                ENDIF

                ALLOCATE(GMTColors(1:(NumDiffContours-1)*TempSplitBy))

                DO I=1,(NumDiffContours-1)*TempSplitBy
                    GMTColors(I)%Red1   = 0
                    GMTColors(I)%Green1 = 0
                    GMTColors(I)%Blue1  = 0
                    GMTColors(I)%Red2   = 0
                    GMTColors(I)%Green2 = 0
                    GMTColors(I)%Blue2  = 0
                ENDDO

                GMTColors(1)%Value1 = DiffContours(1)
                GMTColors(1)%Value2 = DiffContours(1) + &
                        (DiffContours(2)-DiffContours(1))/TempSplitBy

                DO I=1,NumDiffContours-1

                    DO J=1,TempSplitBy

                        TempIndex = (I-1)*TempSplitBy + J

                        GMTColors(TempIndex)%Value1 = DiffContours(I) + (J-1)* &
                                (DiffContours(I+1)-DiffContours(I))/TempSplitBy
                        GMTColors(TempIndex)%Value2 = DiffContours(I) + (J  )* &
                                (DiffContours(I+1)-DiffContours(I))/TempSplitBy
                                
                        IF(MakeGray(I).EQ.1)THEN

                            GMTColors(TempIndex)%Red1   = 225
                            GMTColors(TempIndex)%Green1 = 225
                            GMTColors(TempIndex)%Blue1  = 225
                            GMTColors(TempIndex)%Red2   = 225
                            GMTColors(TempIndex)%Green2 = 225
                            GMTColors(TempIndex)%Blue2  = 225

                        ELSE

                            CurrentContour = (REAL(TempIndex) - 1.0)/ &
                                    ((REAL(NumDiffContours)-1.0)*REAL(TempSplitBy)-1.0)

                            IF(SMSColors(1)%Value1.GT.CurrentContour)THEN

                                GMTColors(TempIndex)%Red1   = SMSColors(1)%Red1
                                GMTColors(TempIndex)%Green1 = SMSColors(1)%Green1
                                GMTColors(TempIndex)%Blue1  = SMSColors(1)%Blue1
                                GMTColors(TempIndex)%Red2   = SMSColors(1)%Red1
                                GMTColors(TempIndex)%Green2 = SMSColors(1)%Green1
                                GMTColors(TempIndex)%Blue2  = SMSColors(1)%Blue1

                            ELSEIF(SMSColors(NumColors)%Value1.LT.CurrentContour)THEN

                                GMTColors(TempIndex)%Red1   = SMSColors(NumColors)%Red1
                                GMTColors(TempIndex)%Green1 = SMSColors(NumColors)%Green1
                                GMTColors(TempIndex)%Blue1  = SMSColors(NumColors)%Blue1
                                GMTColors(TempIndex)%Red2   = SMSColors(NumColors)%Red1
                                GMTColors(TempIndex)%Green2 = SMSColors(NumColors)%Green1
                                GMTColors(TempIndex)%Blue2  = SMSColors(NumColors)%Blue1

                            ELSE

                                DO L=2,NumColors

                                    IF((SMSColors(L-1)%Value1.LE.CurrentContour).AND.        &
                                           (SMSColors(L)%Value1.GE.CurrentContour))THEN

                                        GMTColors(TempIndex)%Red1   = SMSColors(l-1)%Red1+   &
                                                (CurrentContour-SMSColors(L-1)%Value1)/      &
                                                (SMSColors(L)%Value1-SMSColors(L-1)%Value1)* &
                                                (SMSColors(L)%Red1-SMSColors(L-1)%Red1)
                                        GMTColors(TempIndex)%Green1 = SMSColors(L-1)%Green1+ &
                                                (CurrentContour-SMSColors(L-1)%Value1)/      &
                                                (SMSColors(L)%Value1-SMSColors(L-1)%Value1)* &
                                                (SMSColors(L)%Green1-SMSColors(L-1)%Green1)
                                        GMTColors(TempIndex)%Blue1  = SMSColors(L-1)%Blue1+  &
                                                (CurrentContour-SMSColors(L-1)%Value1)/      &
                                                (SMSColors(L)%Value1-SMSColors(L-1)%Value1)* &
                                                (SMSColors(L)%Blue1-SMSColors(L-1)%Blue1)
                                        GMTColors(TempIndex)%Red2   = SMSColors(L-1)%Red1+   &
                                                (CurrentContour-SMSColors(L-1)%Value1)/      &
                                                (SMSColors(L)%Value1-SMSColors(L-1)%Value1)* &
                                                (SMSColors(L)%Red1-SMSColors(L-1)%Red1)
                                        GMTColors(TempIndex)%Green2 = SMSColors(L-1)%Green1+ &
                                                (CurrentContour-SMSColors(L-1)%Value1)/      &
                                                (SMSColors(L)%Value1-SMSColors(L-1)%Value1)* &
                                                (SMSColors(L)%Green1-SMSColors(L-1)%Green1)
                                        GMTColors(TempIndex)%Blue2  = SMSColors(L-1)%Blue1+  &
                                                (CurrentContour-SMSColors(L-1)%Value1)/      &
                                                (SMSColors(L)%Value1-SMSColors(L-1)%Value1)* &
                                                (SMSColors(L)%Blue1-SMSColors(L-1)%Blue1)

                                    ENDIF

                                ENDDO

                            ENDIF

                        ENDIF

                    ENDDO

                ENDDO

                IF(K.EQ.1)THEN
                    
                    OPEN(UNIT=15,FILE=TRIM(TempPath)//"ContourPalette.cpt",ACTION="WRITE")

                ELSEIF(K.EQ.2)THEN

                    OPEN(UNIT=15,FILE=TRIM(TempPath)//"LabelPalette.cpt",ACTION="WRITE")

                ELSEIF(K.EQ.3)THEN

                    OPEN(UNIT=15,FILE=TRIM(TempPath)//"ScalePalette.cpt",ACTION="WRITE")

                ENDIF

                WRITE(UNIT=15,FMT='(A)') "#"
                WRITE(UNIT=15,FMT='(A)') "#"
                WRITE(UNIT=15,FMT='(A)') "#"

                IF((K.EQ.1).AND.((ContourFileType.EQ.1).OR.(TRIM(ContourFileFormat).EQ."GRID-BATH").OR. &
                   (TRIM(ContourFileFormat).EQ."GRID-SIZE")))THEN

                    WRITE(UNIT=15,FMT='(A,2X,I3,2X,I3,2X,I3,2X,F16.8,2X,I3,2X,I3,2X,I3)')    &
                                        "-99900.00000000",                                   &
                                        NINT(GMTColors(1)%Red1),                             &
                                        NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1), &
                                        GMTColors(1)%Value1,                                 &
                                        NINT(GMTColors(1)%Red1),                             &
                                        NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1)

                ENDIF

                DO I=1,(NumDiffContours-1)*TempSplitBy
           
                    WRITE(UNIT=15,FMT='(2(F16.8,2X,I3,2X,I3,2X,I3,2X))')                     &
                                        GMTColors(I)%Value1,                                 &
                                        NINT(GMTColors(I)%Red1),                             &
                                        NINT(GMTColors(I)%Green1), NINT(GMTColors(I)%Blue1), &
                                        GMTColors(I)%Value2,                                 &
                                        NINT(GMTColors(I)%Red2),                             &
                                        NINT(GMTColors(I)%Green2), NINT(GMTColors(I)%Blue2)

                ENDDO

                IF((K.EQ.1).AND.((ContourFileType.EQ.1).OR.(TRIM(ContourFileFormat).EQ."GRID-BATH").OR. &
                   (TRIM(ContourFileFormat).EQ."GRID-SIZE")))THEN

                    WRITE(UNIT=15,FMT='(F16.8,2X,I3,2X,I3,2X,I3,2X,A,2X,I3,2X,I3,2X,I3)')        &
                                        GMTColors((NumDiffContours-1)*TempSplitBy)%Value2,       &
                                        NINT(GMTColors((NumDiffContours-1)*TempSplitBy)%Red2),   &
                                        NINT(GMTColors((NumDiffContours-1)*TempSplitBy)%Green2), &
                                        NINT(GMTColors((NumDiffContours-1)*TempSplitBy)%Blue2),  &
                                        "99900.00000000",                                        &
                                        NINT(GMTColors((NumDiffContours-1)*TempSplitBy)%Red2),   &
                                        NINT(GMTColors((NumDiffContours-1)*TempSplitBy)%Green2), &
                                        NINT(GMTColors((NumDiffContours-1)*TempSplitBy)%Blue2)

                ENDIF

                WRITE(UNIT=15,FMT='(A)') "B  215  215  215"
                WRITE(UNIT=15,FMT='(A,I3,A,I3,A,I3)') "F  ",                                          &
                                        NINT(GMTColors((NumDiffContours-1)*TempSplitBy)%Red2),"  ",   &
                                        NINT(GMTColors((NumDiffContours-1)*TempSplitBy)%Green2),"  ", &
                                        NINT(GMTColors((NumDiffContours-1)*TempSplitBy)%Blue2)
                WRITE(UNIT=15,FMT='(A)') "N  255  255  255"

                CLOSE(UNIT=15,STATUS="KEEP")

            ELSE

                IF(K.EQ.1)THEN

                    TempSplitBy = SplitBy

                ELSEIF(K.EQ.2)THEN

                    TempSplitBy = 1

                ELSEIF(K.EQ.3)THEN

                    TempSplitBy = SplitBy

                ENDIF

                ALLOCATE(GMTColors(1:NINT((ContourMax-ContourMin)/ContourInterval)*TempSplitBy))

                DO I=1,NINT((ContourMax-ContourMin)/ContourInterval)*TempSplitBy

                    CurrentContour = ((I-0.5)*ContourInterval/TempSplitBy)/(ContourMax-ContourMin)

                    IF(SMSColors(1)%Value1.GT.CurrentContour)THEN

                        GMTColors(I)%Value1 = ContourMin+(I-1)*ContourInterval/TempSplitBy
                        GMTColors(I)%Red1   = SMSColors(1)%Red1
                        GMTColors(I)%Green1 = SMSColors(1)%Green1
                        GMTColors(I)%Blue1  = SMSColors(1)%Blue1
                        GMTColors(I)%Value2 = ContourMin+(I)*ContourInterval/TempSplitBy
                        GMTColors(I)%Red2   = SMSColors(1)%Red1
                        GMTColors(I)%Green2 = SMSColors(1)%Green1
                        GMTColors(I)%Blue2  = SMSColors(1)%Blue1

                    ELSEIF(SMSColors(NumColors)%Value1.LT.CurrentContour)THEN

                        GMTColors(I)%Value1 = ContourMin+(I-1)*ContourInterval/TempSplitBy
                        GMTColors(I)%Red1   = SMSColors(NumColors)%Red1
                        GMTColors(I)%Green1 = SMSColors(NumColors)%Green1
                        GMTColors(I)%Blue1  = SMSColors(NumColors)%Blue1
                        GMTColors(I)%Value2 = ContourMin+(I)*ContourInterval/TempSplitBy
                        GMTColors(I)%Red2   = SMSColors(NumColors)%Red1
                        GMTColors(I)%Green2 = SMSColors(NumColors)%Green1
                        GMTColors(I)%Blue2  = SMSColors(NumColors)%Blue1

                    ELSE

                        DO J=2,NumColors

                            IF((SMSColors(J-1)%Value1.LE.CurrentContour).AND.        &
                                   (SMSColors(J)%Value1.GE.CurrentContour))THEN

                                GMTColors(I)%Value1 = ContourMin+(I-1)*ContourInterval/TempSplitBy
                                GMTColors(I)%Red1   = SMSColors(J-1)%Red1+           &
                                        (CurrentContour-SMSColors(J-1)%Value1)/      &
                                        (SMSColors(J)%Value1-SMSColors(J-1)%Value1)* &
                                        (SMSColors(J)%Red1-SMSColors(J-1)%Red1)
                                GMTColors(I)%Green1 = SMSColors(J-1)%Green1+         &
                                        (CurrentContour-SMSColors(J-1)%Value1)/      &
                                        (SMSColors(J)%Value1-SMSColors(J-1)%Value1)* &
                                        (SMSColors(J)%Green1-SMSColors(J-1)%Green1)
                                GMTColors(I)%Blue1  = SMSColors(J-1)%Blue1+          &
                                        (CurrentContour-SMSColors(J-1)%Value1)/      &
                                        (SMSColors(J)%Value1-SMSColors(J-1)%Value1)* &
                                        (SMSColors(J)%Blue1-SMSColors(J-1)%Blue1)
                                GMTColors(I)%Value2 = ContourMin+(I)*ContourInterval/TempSplitBy
                                GMTColors(I)%Red2   = SMSColors(J-1)%Red1+           &
                                        (CurrentContour-SMSColors(J-1)%Value1)/      &
                                        (SMSColors(J)%Value1-SMSColors(J-1)%Value1)* &
                                        (SMSColors(J)%Red1-SMSColors(J-1)%Red1)
                                GMTColors(I)%Green2 = SMSColors(J-1)%Green1+         &
                                        (CurrentContour-SMSColors(J-1)%Value1)/      &
                                        (SMSColors(J)%Value1-SMSColors(J-1)%Value1)* &
                                        (SMSColors(J)%Green1-SMSColors(J-1)%Green1)
                                GMTColors(I)%Blue2  = SMSColors(J-1)%Blue1+          &
                                        (CurrentContour-SMSColors(J-1)%Value1)/      &
                                        (SMSColors(J)%Value1-SMSColors(J-1)%Value1)* &
                                        (SMSColors(J)%Blue1-SMSColors(J-1)%Blue1)

                            ENDIF

                        ENDDO

                    ENDIF

                ENDDO

                IF(K.EQ.1)THEN
                    
                    OPEN(UNIT=15,FILE=TRIM(TempPath)//"ContourPalette.cpt",ACTION="WRITE")

                ELSEIF(K.EQ.2)THEN

                    OPEN(UNIT=15,FILE=TRIM(TempPath)//"LabelPalette.cpt",ACTION="WRITE")

                ELSEIF(K.EQ.3)THEN

                    OPEN(UNIT=15,FILE=TRIM(TempPath)//"ScalePalette.cpt",ACTION="WRITE")

                ENDIF

                WRITE(UNIT=15,FMT='(A)') "#"
                WRITE(UNIT=15,FMT='(A)') "#"
                WRITE(UNIT=15,FMT='(A)') "#"

                IF((K.EQ.1).AND.((ContourFileType.EQ.1).OR.(TRIM(ContourFileFormat).EQ."GRID-BATH").OR. &
                   (TRIM(ContourFileFormat).EQ."GRID-SIZE")))THEN

                    WRITE(UNIT=15,FMT='(A,2X,I3,2X,I3,2X,I3,2X,F16.8,2X,I3,2X,I3,2X,I3)')    &
                                        "-99900.00000000",                                   &
                                        NINT(GMTColors(1)%Red1),                             &
                                        NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1), &
                                        GMTColors(1)%Value1,                                 &
                                        NINT(GMTColors(1)%Red1),                             &
                                        NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1)

                ENDIF

                JunkI = NINT((ContourMax-ContourMin)/ContourInterval)*TempSplitBy

                DO I=1,JunkI
           
                    WRITE(UNIT=15,FMT='(2(F16.8,2X,I3,2X,I3,2X,I3,2X))')                     &
                                        GMTColors(I)%Value1,                                 &
                                        NINT(GMTColors(I)%Red1),                             &
                                        NINT(GMTColors(I)%Green1), NINT(GMTColors(I)%Blue1), &
                                        GMTColors(I)%Value2,                                 &
                                        NINT(GMTColors(I)%Red2),                             &
                                        NINT(GMTColors(I)%Green2), NINT(GMTColors(I)%Blue2)

                ENDDO

                WRITE(UNIT=15,FMT='(A)') "B  215  215  215"
                WRITE(UNIT=15,FMT='(A,I3,A,I3,A,I3)') "F  ",NINT(GMTColors(JunkI)%Red2),"  ",    &
                                        NINT(GMTColors(JunkI)%Green2),"  ",                     &
                                        NINT(GMTColors(JunkI)%Blue2)
                WRITE(UNIT=15,FMT='(A)') "N  255  255  255"

                CLOSE(UNIT=15,STATUS="KEEP")

            ENDIF

            IF(ALLOCATED(GMTColors)) DEALLOCATE(GMTColors)

        ENDDO

        IF(ALLOCATED(DiffContours)) DEALLOCATE(DiffContours)        
        IF(ALLOCATED(MakeGray)) DEALLOCATE(MakeGray)        
        IF(ALLOCATED(SMSColors)) DEALLOCATE(SMSColors)

        IF(Verbose.GE.3)THEN
            WRITE(*,'(A,I4.4,A)') "Processor ", MyRank, " created the contour palettes."
        ENDIF

END SUBROUTINE



SUBROUTINE FindContourMinMax

        USE DATA

        IMPLICIT NONE

        INTRINSIC                          :: CEILING
        INTRINSIC                          :: FLOOR
        INTRINSIC                          :: INDEX
        INTRINSIC                          :: TRIM

        CHARACTER(LEN=1)                   :: JunkC

        INTEGER                            :: CounterLocal
        INTEGER                            :: I
        INTEGER                            :: J
        INTEGER                            :: JunkI
        INTEGER,ALLOCATABLE,DIMENSION(:,:) :: NC
        INTEGER                            :: NumElemsGlobal
        INTEGER                            :: NumNodes1
        INTEGER                            :: NumNodes2
        INTEGER                            :: NumRecsLocal

        REAL(8),ALLOCATABLE,DIMENSION(:)   :: Bath1
        REAL(8),ALLOCATABLE,DIMENSION(:)   :: Bath2
        REAL(8)                            :: ComputeDistance
        REAL(8)                            :: DefaultValue
        REAL(8)                            :: Dist
        REAL(8)                            :: JunkR
        REAL(8),ALLOCATABLE,DIMENSION(:)   :: Lat
        REAL(8),ALLOCATABLE,DIMENSION(:)   :: Lon
        REAL(8)                            :: Max
        REAL(8)                            :: Min
        REAL(8),ALLOCATABLE,DIMENSION(:)   :: U1
        REAL(8),ALLOCATABLE,DIMENSION(:)   :: U2
        REAL(8),ALLOCATABLE,DIMENSION(:)   :: V1
        REAL(8),ALLOCATABLE,DIMENSION(:)   :: V2
        REAL(8),ALLOCATABLE,DIMENSION(:)   :: Vels1
        REAL(8),ALLOCATABLE,DIMENSION(:)   :: Vels2

        CounterLocal = 1
        Max = -9999.0
        Min = 9999.0

        IF(IfPlotFilledContours.EQ.1)THEN

            IF((TRIM(ContourFileFormat).EQ."OUTPUT-FULL").OR.(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE"))THEN

                OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                READ(UNIT=19,FMT='(A)') JunkC
                READ(UNIT=19,FMT=*) NumRecsLocal, NumNodesGlobal, JunkR, JunkI, ContourFileType

                ALLOCATE(U1(1:NumNodesGlobal))
                ALLOCATE(V1(1:NumNodesGlobal))
                ALLOCATE(Vels1(1:NumNodesGlobal))

                loopminmax1: DO J=1,NumRecsLocal 

                    IF(J.LT.RecordsList(CounterLocal))THEN
                   
                        IF(TRIM(ContourFileFormat).EQ."OUTPUT-FULL")THEN

                            READ(UNIT=19,FMT=*) JunkR, JunkI
                            NumNodes1 = NumNodesGlobal
                            DefaultValue = 0.0

                        ELSEIF(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE")THEN
                    
                            READ(UNIT=19,FMT=*) JunkR, JunkI, NumNodes1, DefaultValue

                        ENDIF

                        DO I=1,NumNodes1
                            READ(UNIT=19,FMT=*) JunkI
                        ENDDO

                    ELSE

                        CounterLocal = CounterLocal + 1

                        IF(TRIM(ContourFileFormat).EQ."OUTPUT-FULL")THEN

                            READ(UNIT=19,FMT=*) JunkR,JunkI
                            NumNodes1 = NumNodesGlobal
                            DefaultValue = 0.0

                        ELSEIF(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE")THEN

                            READ(UNIT=19,FMT=*) JunkR,JunkI,NumNodes1,DefaultValue

                        ENDIF

                        IF(INDEX(TRIM(ContourFile1),"64").GT.0)THEN
                            DefaultValue = -99999.0
                        ENDIF

                        IF(DefaultValue.GT.-99998.0)THEN
                            DefaultValue = DefaultValue * ContourConversionFactor
                        ENDIF

                        DO I=1,NumNodesGlobal
                            U1(I) = DefaultValue
                            V1(I) = DefaultValue
                            Vels1(I) = DefaultValue
                        ENDDO

                        DO I=1,NumNodes1

                            IF(ContourFileType.EQ.1)THEN

                                READ(UNIT=19,FMT=*) JunkI, U1(JunkI)
                                IF(U1(JunkI).GT.-99998.0)THEN
                                    U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                ENDIF
                                Vels1(JunkI) = U1(JunkI)

                            ELSEIF(ContourFileType.EQ.2)THEN

                                READ(UNIT=19,FMT=*) JunkI, U1(JunkI), V1(JunkI)
                                U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                V1(JunkI) = V1(JunkI) * ContourConversionFactor
                                Vels1(JunkI) = SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))

                            ENDIF

                        ENDDO

                        DO I=1,NumNodesLocal

                            IF(Vels1(XYZNodes(I)).GT.-99998.0)THEN

                                IF(Vels1(XYZNodes(I)).LT.Min)THEN
                                    Min = Vels1(XYZNodes(I))
                                ENDIF
                                IF(Vels1(XYZNodes(I)).GT.Max)THEN
                                   Max = Vels1(XYZNodes(I))
                                ENDIF

                            ENDIF

                        ENDDO

                    ENDIF

                    IF(CounterLocal.GT.NumRecords)THEN
                        EXIT loopminmax1
                    ENDIF

                ENDDO loopminmax1

                IF(ALLOCATED(U1)) DEALLOCATE(U1)
                IF(ALLOCATED(V1)) DEALLOCATE(V1)
                IF(ALLOCATED(Vels1)) DEALLOCATE(Vels1)

                CLOSE(UNIT=19,STATUS="KEEP")

            ELSEIF((TRIM(ContourFileFormat).EQ."GRID-BATH").OR.(TRIM(ContourFileFormat).EQ."GRID-SIZE"))THEN

                OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                READ(UNIT=19,FMT='(A)') JunkC
                READ(UNIT=19,FMT=*) NumElemsGlobal, NumNodesGlobal, JunkR, JunkI, ContourFileType

                IF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN
                    ALLOCATE(Bath1(1:NumNodesGlobal))
                ELSEIF(TRIM(ContourFileFormat).EQ."GRID-SIZE")THEN
                    ALLOCATE(Lat(1:NumNodesGlobal))
                    ALLOCATE(Lon(1:NumNodesGlobal))
                ENDIF

                DO I=1,NumNodesGlobal

                    IF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN
                        READ(UNIT=19,FMT=*) JunkI, JunkR, JunkR, Bath1(I)
                        Bath1(I) = Bath1(I) * ContourConversionFactor
                    ELSEIF(TRIM(ContourFileFormat).EQ."GRID-SIZE")THEN
                        READ(UNIT=19,FMT=*) JunkI, Lon(I), Lat(I), JunkR
                    ENDIF

                ENDDO

                IF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN

                    DO I=1,NumNodesLocal
                        IF(Bath1(XYZNodes(I)).LT.Min)THEN
                            Min = Bath1(XYZNodes(I))
                        ENDIF
                        IF(Bath1(XYZNodes(I)).GT.Max)THEN
                            Max = Bath1(XYZNodes(I))
                        ENDIF
                    ENDDO

                ELSEIF(TRIM(ContourFileFormat).EQ."GRID-SIZE")THEN

                    ALLOCATE(NC(1:NumElemsGlobal,1:3))

                    DO I=1,NumElemsGlobal

                        READ(UNIT=19,FMT=*) JunkI, JunkI, NC(I,1), NC(I,2), NC(I,3)

                        DO J=1,NumNodesLocal

                            IF(NC(I,1).EQ.XYZNodes(I))THEN

                                Dist = ComputeDistance(Lon(NC(I,1)),Lat(NC(I,1)),Lon(NC(I,2)),Lat(NC(I,2)))
                                IF(Dist.LT.Min)THEN
                                    Min = Dist
                                ENDIF
                                IF(Dist.GT.Max)THEN
                                    Max = Dist
                                ENDIF

                                Dist = ComputeDistance(Lon(NC(I,1)),Lat(NC(I,1)),Lon(NC(I,3)),Lat(NC(I,3)))
                                IF(Dist.LT.Min)THEN
                                    Min = Dist
                                ENDIF
                                IF(Dist.GT.Max)THEN
                                    Max = Dist
                                ENDIF

                            ELSEIF(NC(I,2).EQ.XYZNodes(I))THEN

                                Dist = ComputeDistance(Lon(NC(I,2)),Lat(NC(I,2)),Lon(NC(I,1)),Lat(NC(I,1)))
                                IF(Dist.LT.Min)THEN
                                    Min = Dist
                                ENDIF
                                IF(Dist.GT.Max)THEN
                                    Max = Dist
                                ENDIF

                                Dist = ComputeDistance(Lon(NC(I,2)),Lat(NC(I,2)),Lon(NC(I,3)),Lat(NC(I,3)))
                                IF(Dist.LT.Min)THEN
                                    Min = Dist
                                ENDIF
                                IF(Dist.GT.Max)THEN
                                    Max = Dist
                                ENDIF

                            ELSEIF(NC(I,3).EQ.XYZNodes(I))THEN

                                Dist = ComputeDistance(Lon(NC(I,3)),Lat(NC(I,3)),Lon(NC(I,1)),Lat(NC(I,1)))
                                IF(Dist.LT.Min)THEN
                                    Min = Dist
                                ENDIF
                                IF(Dist.GT.Max)THEN
                                    Max = Dist
                                ENDIF

                                Dist = ComputeDistance(Lon(NC(I,3)),Lat(NC(I,3)),Lon(NC(I,2)),Lat(NC(I,2)))
                                IF(Dist.LT.Min)THEN
                                    Min = Dist
                                ENDIF
                                IF(Dist.GT.Max)THEN
                                    Max = Dist
                                ENDIF

                            ENDIF

                        ENDDO

                    ENDDO

                ENDIF

                IF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN
                    IF(ALLOCATED(Bath1)) DEALLOCATE(Bath1)
                ELSEIF(TRIM(ContourFileFormat).EQ."GRID-SIZE")THEN
                    IF(ALLOCATED(Lat)) DEALLOCATE(Lat)
                    IF(ALLOCATED(Lon)) DEALLOCATE(Lon)
                    IF(ALLOCATED(NC))  DEALLOCATE(NC)
                ENDIF

                CLOSE(UNIT=19,STATUS="KEEP")

            ENDIF

            ContourMin = FLOOR(Min)
            ContourMax = CEILING(Max)

        ELSEIF(IfPlotFilledContours.EQ.2)THEN

            IF((TRIM(ContourFileFormat).EQ."OUTPUT-FULL").OR.(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE"))THEN

                OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                READ(UNIT=19,FMT='(A)') JunkC
                READ(UNIT=19,FMT=*) NumRecsLocal, NumNodes1, JunkR, JunkI, ContourFileType
                OPEN(UNIT=23,FILE=TRIM(ContourFile2),ACTION="READ")
                READ(UNIT=23,FMT='(A)') JunkC
                READ(UNIT=23,FMT=*) NumRecsLocal, NumNodes2, JunkR, JunkI, ContourFileType

                IF(NumNodes1.NE.NumNodes2)THEN
                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: The contour files should have the same number of nodes."
#ifdef CMPI
                    CALL MPI_FINALIZE(IERR)
#endif
                    STOP
                ENDIF

                NumNodesGlobal = NumNodes1

                ALLOCATE(U1(1:NumNodesGlobal))
                ALLOCATE(U2(1:NumNodesGlobal))
                ALLOCATE(V1(1:NumNodesGlobal))
                ALLOCATE(V2(1:NumNodesGlobal))
                ALLOCATE(Vels1(1:NumNodesGlobal))
                ALLOCATE(Vels2(1:NumNodesGlobal))

                loopminmax2: DO J=1,NumRecsLocal 

                    IF(J.LT.RecordsList(CounterLocal))THEN

                        IF(TRIM(ContourFileFormat).EQ."OUTPUT-FULL")THEN

                            READ(UNIT=19,FMT=*) JunkR, JunkI
                            READ(UNIT=23,FMT=*) JunkR, JunkI
                            NumNodes1 = NumNodesGlobal
                            NumNodes2 = NumNodesGlobal
                            DefaultValue = 0.0

                        ELSEIF(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE")THEN
                    
                            READ(UNIT=19,FMT=*) JunkR, JunkI, NumNodes1, DefaultValue
                            READ(UNIT=23,FMT=*) JunkR, JunkI, NumNodes2, DefaultValue

                        ENDIF

                        DO I=1,NumNodes1
                            READ(UNIT=19,FMT=*) JunkI
                        ENDDO

                        DO I=1,NumNodes2
                            READ(UNIT=23,FMT=*) JunkI
                        ENDDO

                    ELSE

                        CounterLocal = CounterLocal + 1

                        IF(TRIM(ContourFileFormat).EQ."OUTPUT-FULL")THEN

                            READ(UNIT=19,FMT=*) JunkR,JunkI
                            READ(UNIT=23,FMT=*) JunkR,JunkI
                            NumNodes1 = NumNodesGlobal
                            NumNodes2 = NumNodesGlobal
                            DefaultValue = 0.0

                        ELSEIF(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE")THEN

                            READ(UNIT=19,FMT=*) JunkR,JunkI,NumNodes1,DefaultValue
                            READ(UNIT=23,FMT=*) JunkR,JunkI,NumNodes2,DefaultValue

                        ENDIF

                        IF(INDEX(TRIM(ContourFile1),"64").GT.0)THEN
                            DefaultValue = -99999.0
                        ENDIF

                        IF(DefaultValue.GT.-99998.0)THEN
                            DefaultValue = DefaultValue * ContourConversionFactor
                        ENDIF

                        DO I=1,NumNodesGlobal

                            U1(I) = DefaultValue
                            U2(I) = DefaultValue
                            V1(I) = DefaultValue
                            V2(I) = DefaultValue
                            Vels1(I) = DefaultValue
                            Vels2(I) = DefaultValue

                        ENDDO

                        DO I=1,NumNodes1

                            IF(ContourFileType.EQ.1)THEN

                                READ(UNIT=19,FMT=*) JunkI, U1(JunkI)
                                IF(U1(JunkI).GT.-99998.0)THEN
                                    U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                ENDIF
                                Vels1(JunkI) = U1(JunkI)

                            ELSEIF(ContourFileType.EQ.2)THEN

                                READ(UNIT=19,FMT=*) JunkI, U1(JunkI), V1(JunkI)
                                U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                V1(JunkI) = V1(JunkI) * ContourConversionFactor
                                Vels1(JunkI) = SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))

                            ENDIF

                        ENDDO

                        DO I=1,NumNodes2

                            IF(ContourFileType.EQ.1)THEN

                                READ(UNIT=23,FMT=*) JunkI, U2(JunkI)
                                IF(U1(JunkI).GT.-99998.0)THEN
                                    U2(JunkI) = U2(JunkI) * ContourConversionFactor
                                ENDIF
                                Vels2(JunkI) = U2(JunkI)

                            ELSEIF(ContourFileType.EQ.2)THEN

                                READ(UNIT=23,FMT=*) JunkI, U2(JunkI), V2(JunkI)
                                U2(JunkI) = U2(JunkI) * ContourConversionFactor
                                V2(JunkI) = V2(JunkI) * ContourConversionFactor
                                Vels2(JunkI) = SQRT(U2(JunkI)*U2(JunkI)+V2(JunkI)*V2(JunkI))

                            ENDIF

                        ENDDO

                        DO I=1,NumNodesLocal

                            IF((Vels1(XYZNodes(I)).GT.-99998.0).AND. &
                               (Vels2(XYZNodes(I)).GT.-99998.0))THEN

                                IF((Vels1(XYZNodes(I))-Vels2(XYZNodes(I))).LT.Min)THEN
                                    Min = Vels1(XYZNodes(I))-Vels2(XYZNodes(I))
                                ENDIF
                                IF((Vels1(XYZNodes(I))-Vels2(XYZNodes(I))).GT.Max)THEN
                                    Max = Vels1(XYZNodes(I))-Vels2(XYZNodes(I))
                                ENDIF

                            ENDIF

                        ENDDO

                    ENDIF

                    IF(CounterLocal.GT.NumRecords)THEN

                        EXIT loopminmax2

                    ENDIF

                ENDDO loopminmax2

                IF(ALLOCATED(U1)) DEALLOCATE(U1)
                IF(ALLOCATED(U2)) DEALLOCATE(U2)
                IF(ALLOCATED(V1)) DEALLOCATE(V1)
                IF(ALLOCATED(V2)) DEALLOCATE(V2)
                IF(ALLOCATED(Vels1)) DEALLOCATE(Vels1)
                IF(ALLOCATED(Vels2)) DEALLOCATE(Vels2)

                CLOSE(UNIT=19,STATUS="KEEP")
                CLOSE(UNIT=23,STATUS="KEEP")

            ELSEIF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN

                OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                READ(UNIT=19,FMT='(A)') JunkC
                READ(UNIT=19,FMT=*) JunkI, NumNodes1
                OPEN(UNIT=23,FILE=TRIM(ContourFile2),ACTION="READ")
                READ(UNIT=23,FMT='(A)') JunkC
                READ(UNIT=23,FMT=*) JunkI, NumNodes2

                IF(NumNodes1.NE.NumNodes2)THEN
                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: The grids should have the same number of nodes."
#ifdef CMPI
                    CALL MPI_FINALIZE(IERR)
#endif
                    STOP
                ENDIF

                ALLOCATE(Bath1(1:NumNodes1))
                ALLOCATE(Bath2(1:NumNodes2))

                DO I=1,NumNodes1
                    READ(UNIT=19,FMT=*) JunkI, JunkR, JunkR, Bath1(I)
                    Bath1(I) = Bath1(I) * ContourConversionFactor
                ENDDO

                DO I=1,NumNodes2
                    READ(UNIT=23,FMT=*) JunkI, JunkR, JunkR, Bath2(I)
                    Bath2(I) = Bath2(I) * ContourConversionFactor
                ENDDO

                DO I=1,NumNodesLocal

                    IF((Bath1(XYZNodes(I))-Bath2(XYZNodes(I))).LT.Min)THEN
                        Min = Bath1(XYZNodes(I))-Bath2(XYZNodes(I))
                    ENDIF
                    IF((Bath1(XYZNodes(I))-Bath2(XYZNodes(I))).GT.Max)THEN
                        Max = Bath1(XYZNodes(I))-Bath2(XYZNodes(I))
                    ENDIF

                ENDDO

                IF(ALLOCATED(Bath1)) DEALLOCATE(Bath1)
                IF(ALLOCATED(Bath2)) DEALLOCATE(Bath2)

                CLOSE(UNIT=19,STATUS="KEEP")
                CLOSE(UNIT=23,STATUS="KEEP")

            ENDIF

            ContourMin = FLOOR(Min)
            ContourMax = CEILING(Max)

        ENDIF

        IF(Verbose.GE.3)THEN
            WRITE(*,'(A,I4.4,A,F7.2,A,A,F7.2,A,A)')                              &
                         "Processor ", MyRank, " computed the contour range ..." &
                         //" Minimum ~ ",ContourMin,TRIM(ContourUnits),          &
                         ", Maximum ~ ",ContourMax,TRIM(ContourUnits),"."
        ENDIF

END SUBROUTINE



SUBROUTINE FindVectorScaleMag

        USE DATA

        IMPLICIT NONE

        INTRINSIC                        :: CEILING

        CHARACTER(LEN=1)                 :: JunkC

        INTEGER                          :: CounterLocal
        INTEGER                          :: I
        INTEGER                          :: J
        INTEGER                          :: JunkI
        INTEGER                          :: NumNodes1
        INTEGER                          :: NumRecsLocal

        REAL(8)                          :: DefaultValue
        REAL(8)                          :: JunkR
        REAL(8)                          :: Max
        REAL(8),ALLOCATABLE,DIMENSION(:) :: U1
        REAL(8),ALLOCATABLE,DIMENSION(:) :: V1
        REAL(8),ALLOCATABLE,DIMENSION(:) :: Vels1

        CounterLocal = 1
        Max = -9999.0

        OPEN(UNIT=20,FILE=TRIM(VectorFile),ACTION="READ")
        READ(UNIT=20,FMT='(A)') JunkC
        READ(UNIT=20,FMT=*) NumRecsLocal, NumNodesGlobal, JunkR, JunkI, VectorFileType

        ALLOCATE(U1(1:NumNodesGlobal))
        ALLOCATE(V1(1:NumNodesGlobal))
        ALLOCATE(Vels1(1:NumNodesGlobal))

        loopvectorscale: DO J=1,NumRecsLocal 

            IF(J.LT.RecordsList(CounterLocal))THEN
                   
                IF(TRIM(VectorFileFormat).EQ."OUTPUT-FULL")THEN

                    READ(UNIT=20,FMT=*) JunkR, JunkI
                    NumNodes1 = NumNodesGlobal
                    DefaultValue = 0.0

                ELSEIF(TRIM(VectorFileFormat).EQ."OUTPUT-SPARSE")THEN
                    
                    READ(UNIT=20,FMT=*) JunkR, JunkI, NumNodes1, DefaultValue

                ENDIF

                DO I=1,NumNodes1

                    READ(UNIT=20,FMT=*) JunkI

                ENDDO

            ELSE

                CounterLocal = CounterLocal + 1

                IF(TRIM(VectorFileFormat).EQ."OUTPUT-FULL")THEN

                    READ(UNIT=20,FMT=*) JunkR,JunkI
                    NumNodes1 = NumNodesGlobal
                    DefaultValue = 0.0

                ELSEIF(TRIM(VectorFileFormat).EQ."OUTPUT-SPARSE")THEN

                    READ(UNIT=20,FMT=*) JunkR,JunkI,NumNodes1,DefaultValue

                ENDIF

                IF(INDEX(TRIM(VectorFile),"64").GT.0)THEN

                    DefaultValue = -99999.0

                ENDIF

                IF(DefaultValue.GT.-99998.0)THEN

                    DefaultValue = DefaultValue * VectorConversionFactor

                ENDIF

                DO I=1,NumNodesGlobal

                    U1(I) = DefaultValue
                    V1(I) = DefaultValue
                    Vels1(I) = DefaultValue

                ENDDO

                DO I=1,NumNodes1

                    IF(VectorFileType.EQ.2)THEN

                        READ(UNIT=20,FMT=*) JunkI, U1(JunkI), V1(JunkI)

                        U1(JunkI) = U1(JunkI) * ContourConversionFactor
                        V1(JunkI) = V1(JunkI) * ContourConversionFactor

                        Vels1(JunkI) = SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))

                    ENDIF

                    IF(Vels1(JunkI).GT.Max)THEN
                        Max = Vels1(JunkI)
                    ENDIF

                ENDDO

            ENDIF

            IF(CounterLocal.GT.NumRecords)THEN

                EXIT loopvectorscale

            ENDIF

        ENDDO loopvectorscale

        IF(ALLOCATED(U1)) DEALLOCATE(U1)
        IF(ALLOCATED(V1)) DEALLOCATE(V1)
        IF(ALLOCATED(Vels1)) DEALLOCATE(Vels1)

        CLOSE(UNIT=20,STATUS="KEEP")

        VectorScaleMag = CEILING(Max)

        IF(Verbose.GE.3)THEN
            WRITE(*,'(A,I4.4,A,F7.2,A,A)')                                       &
                         "Processor ", MyRank, " computed the vector scale ..."  &
                         //" Maximum ~ ",VectorScaleMag,TRIM(VectorUnits),"."
        ENDIF

END SUBROUTINE



SUBROUTINE ProcessFort14File

        USE DATA

        IMPLICIT NONE

        INTRINSIC                            :: NINT

        CHARACTER(LEN=1)                     :: JunkC

        INTEGER                              :: Counter
        INTEGER                              :: I
        INTEGER                              :: J
        INTEGER                              :: JunkI
        INTEGER                              :: K
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: NC
        INTEGER, DIMENSION(:), ALLOCATABLE   :: NEList
        INTEGER, DIMENSION(:), ALLOCATABLE   :: NET
        INTEGER, DIMENSION(:), ALLOCATABLE   :: NPList
        INTEGER, DIMENSION(:), ALLOCATABLE   :: NPT
        INTEGER                              :: NumElemsGlobal
        INTEGER                              :: NumElemsLocal
        INTEGER                              :: NumLandBoundaries
        INTEGER                              :: NumOpenBoundaries
        INTEGER, DIMENSION(:), ALLOCATABLE   :: OldToNew
        INTEGER                              :: TotNumLandBoundaryNodes
        INTEGER                              :: TotNumOpenBoundaryNodes

        REAL(8), DIMENSION(:) ,ALLOCATABLE   :: Bath
        REAL(8)                              :: ComputeDistance
        REAL(8), DIMENSION(:) ,ALLOCATABLE   :: Lat
        REAL(8), DIMENSION(:) ,ALLOCATABLE   :: Long
        REAL(8)                              :: ZVal

        TYPE BoundaryListing
            INTEGER                                       :: NumNodes
            INTEGER                                       :: Code
            CHARACTER(LEN=100)                            :: Header
            DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: Entry1
            DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: Entry2
            DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: Entry3
            DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: Entry4
            DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: Entry5
        END TYPE
        TYPE (BoundaryListing), ALLOCATABLE, DIMENSION(:) :: LandBoundaries 
        TYPE (BoundaryListing), ALLOCATABLE, DIMENSION(:) :: OpenBoundaries 

       OPEN(UNIT=24,FILE=TRIM(Fort14File),ACTION="READ")
       OPEN(UNIT=13,FILE=TRIM(TempPath)//TRIM(Fort14File)//".tri",ACTION="WRITE")
       OPEN(UNIT=16,FILE=TRIM(TempPath)//TRIM(Fort14File)//".bnd.xy",ACTION="WRITE")
       IF(IfPlotGrid.EQ.1)THEN
           OPEN(UNIT=31,FILE=TRIM(TempPath)//TRIM(Fort14File)//".edges.xy",ACTION="WRITE")
       ENDIF

        READ(UNIT=24,FMT='(A)') JunkC
        READ(UNIT=24,FMT=*) NumElemsGlobal, NumNodesGlobal

        ALLOCATE(Bath(NumNodesGlobal))
        ALLOCATE(Lat(NumNodesGlobal))
        ALLOCATE(Long(NumNodesGlobal))
        ALLOCATE(NC(NumElemsGlobal,3))
        ALLOCATE(NET(NumElemsGlobal))
        ALLOCATE(NPT(NumNodesGlobal))
        ALLOCATE(OldToNew(NumNodesGlobal))

        OldToNew = 0
        NPT = 0
        NET = 1

        DO I=1,NumNodesGlobal

            READ(24,*) JunkI, Long(I), Lat(I), Bath(I)

            IF(Long(I).GT.(LongE+LatLonBuffer))CYCLE
            IF(Long(I).LT.(LongW-LatLonBuffer))CYCLE
            IF(Lat(I).GT.(LatN+LatLonBuffer))CYCLE
            IF(Lat(I).LT.(LatS-LatLonBuffer))CYCLE

            NPT(I) = 1

        ENDDO

        NumNodesLocal = SUM(NPT)

        ALLOCATE(NPList(NumNodesLocal))

        K = 0

        DO I=1,NumNodesGlobal

            IF(NPT(I).EQ.1)THEN

                K = K + 1
                NPList(K) = I
                OldToNew(I) = K

            ENDIF

        ENDDO

        DO I=1,NumElemsGlobal

            READ(24,*) JunkI, K, NC(JunkI,1), NC(JunkI,2), NC(JunkI,3)

            IF(JunkI.NE.I) STOP 'FATAL ERROR: Elements out of order in grid file.'

            DO K=1,3

                IF(NPT(NC(JunkI,K)).EQ.0)THEN

                    NET(JunkI) = 0
                    EXIT

                ENDIF

            ENDDO

        ENDDO

        NumElemsLocal = SUM(NET)

        ALLOCATE(NEList(NumElemsLocal))

        K = 0

        DO I=1,NumElemsGlobal

            IF(NET(I).EQ.1)THEN

                K = K + 1
                NEList(K) = I

            ENDIF

        ENDDO

        ALLOCATE(XYZNodes(1:NumNodesLocal))
        ALLOCATE(X(1:NumNodesLocal))
        ALLOCATE(Y(1:NumNodesLocal))
        ALLOCATE(Z(1:NumNodesLocal))

        DO I=1,NumNodesLocal

            XYZNodes(I) = NPList(I)
            X(I)        = Long(NPList(I))
            Y(I)        = Lat(NPList(I))

        ENDDO

        Counter = 0
!!!!!!
!        goto 99999

        DO I=1,NumElemsLocal

            WRITE(UNIT=13,FMT=*) OldToNew(NC(NEList(I),1))-1, &
                                 OldToNew(NC(NEList(I),2))-1, &
                                 OldToNew(NC(NEList(I),3))-1

            IF(IfPlotGrid.EQ.1)THEN

                IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
                    ZVal = 0.5D0*(Bath(NC(NEList(I),1))+Bath(NC(NEList(I),2)))
                    ZVal = ZVal * ContourConversionFactor
                    WRITE(UNIT=31,FMT='(A,F11.6)') "> -Z",ZVal
                ELSEIF(TRIM(ColorLines).EQ."GRID-SIZE")THEN
                    ZVal = ComputeDistance( Long(NC(NEList(I),1)), &
                                            Lat( NC(NEList(I),1)), &
                                            Long(NC(NEList(I),2)), &
                                            Lat( NC(NEList(I),2))  )
                    ZVal = ZVal * ContourConversionFactor
                    WRITE(UNIT=31,FMT='(A,F11.6)') "> -Z",ZVal
                ELSE
                    WRITE(UNIT=31,FMT='(A)') ">"
                ENDIF
                WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                    Long(NC(NEList(I),1)),    &
                                    Lat(NC(NEList(I),1))
                WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                    Long(NC(NEList(I),2)),    &
                                    Lat(NC(NEList(I),2))
                Counter = Counter + 1

                IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
                    ZVal = 0.5D0*(Bath(NC(NEList(I),2))+Bath(NC(NEList(I),3)))
                    ZVal = ZVal * ContourConversionFactor
                    WRITE(UNIT=31,FMT='(A,F11.6)') "> -Z",ZVal
                ELSEIF(TRIM(ColorLines).EQ."GRID-SIZE")THEN
                    ZVal = ComputeDistance( Long(NC(NEList(I),2)), &
                                            Lat( NC(NEList(I),2)), &
                                            Long(NC(NEList(I),3)), &
                                            Lat( NC(NEList(I),3))  )
                    ZVal = ZVal * ContourConversionFactor
                    WRITE(UNIT=31,FMT='(A,F11.6)') "> -Z",ZVal
                ELSE
                    WRITE(UNIT=31,FMT='(A)') ">"
                ENDIF
                WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                    Long(NC(NEList(I),2)),    &
                                    Lat(NC(NEList(I),2))
                WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                    Long(NC(NEList(I),3)),    &
                                    Lat(NC(NEList(I),3))
                Counter = Counter + 1

                IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
                    ZVal = 0.5D0*(Bath(NC(NEList(I),3))+Bath(NC(NEList(I),1)))
                    ZVal = ZVal * ContourConversionFactor
                    WRITE(UNIT=31,FMT='(A,F11.6)') "> -Z",ZVal
                ELSEIF(TRIM(ColorLines).EQ."GRID-SIZE")THEN
                    ZVal = ComputeDistance( Long(NC(NEList(I),3)), &
                                            Lat( NC(NEList(I),3)), &
                                            Long(NC(NEList(I),1)), &
                                            Lat( NC(NEList(I),1))  )
                    ZVal = ZVal * ContourConversionFactor
                    WRITE(UNIT=31,FMT='(A,F11.6)') "> -Z",ZVal
                ELSE
                    WRITE(UNIT=31,FMT='(A)') ">"
                ENDIF
                WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                    Long(NC(NEList(I),3)),    &
                                    Lat(NC(NEList(I),3))
                WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                    Long(NC(NEList(I),1)),    &
                                    Lat(NC(NEList(I),1))
                Counter = Counter + 1

            ENDIF

        ENDDO

!99999   continue

        Counter = 0
!        goto 99990

        IF(IfPlotBoundaries.GE.1)THEN
                
            READ(24,*) NumOpenBoundaries
            READ(24,*) TotNumOpenBoundaryNodes

            ALLOCATE(OpenBoundaries(1:NumOpenBoundaries))

            DO J=1,NumOpenBoundaries

                READ(24,*) OpenBoundaries(J)%NumNodes

                ALLOCATE(OpenBoundaries(J)%Entry1(1:OpenBoundaries(J)%NumNodes)) 

                DO I=1,OpenBoundaries(J)%NumNodes

                    READ(24,*) OpenBoundaries(J)%Entry1(I)

                    IF((I.GT.1).AND. &
                      (((Long(NINT(OpenBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                        (Long(NINT(OpenBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                        (Lat(NINT(OpenBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                        (Lat(NINT(OpenBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                       ((Long(NINT(OpenBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                        (Long(NINT(OpenBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                        (Lat(NINT(OpenBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                        (Lat(NINT(OpenBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                        WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                        WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                            Long(NINT(OpenBoundaries(J)%Entry1(I-1))), &
                                            Lat(NINT(OpenBoundaries(J)%Entry1(I-1)))
                        WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                            Long(NINT(OpenBoundaries(J)%Entry1(I))),   &
                                            Lat(NINT(OpenBoundaries(J)%Entry1(I)))
                        Counter = Counter + 1

                    ENDIF

                ENDDO

            ENDDO

            READ(24,*) NumLandBoundaries
            READ(24,*) TotNumLandBoundaryNodes

            ALLOCATE(LandBoundaries(1:NumLandBoundaries))

            DO J=1,NumLandBoundaries

                READ(24,*) LandBoundaries(J)%NumNodes, LandBoundaries(J)%Code

                IF(LandBoundaries(J)%Code.EQ.0)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I)

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.1)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I)

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.10)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I)

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.11)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I)

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.12)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I)

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.13)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))
                    ALLOCATE(LandBoundaries(J)%Entry2(1:LandBoundaries(J)%NumNodes))
                    ALLOCATE(LandBoundaries(J)%Entry3(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I), &
                                   LandBoundaries(J)%Entry2(I), & 
                                   LandBoundaries(J)%Entry3(I) 

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.20)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I)

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.21)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I)

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.22)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I)
 
                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.23)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))
                    ALLOCATE(LandBoundaries(J)%Entry2(1:LandBoundaries(J)%NumNodes))
                    ALLOCATE(LandBoundaries(J)%Entry3(1:LandBoundaries(J)%NumNodes))
 
                    DO I=1,LandBoundaries(J)%NumNodes
 
                        READ(24,*) LandBoundaries(J)%Entry1(I), &
                                   LandBoundaries(J)%Entry2(I), & 
                                   LandBoundaries(J)%Entry3(I) 

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.24)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))
                    ALLOCATE(LandBoundaries(J)%Entry2(1:LandBoundaries(J)%NumNodes))
                    ALLOCATE(LandBoundaries(J)%Entry3(1:LandBoundaries(J)%NumNodes))
                    ALLOCATE(LandBoundaries(J)%Entry4(1:LandBoundaries(J)%NumNodes))
                    ALLOCATE(LandBoundaries(J)%Entry5(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I), &
                                   LandBoundaries(J)%Entry2(I), & 
                                   LandBoundaries(J)%Entry3(I), & 
                                   LandBoundaries(J)%Entry4(I), & 
                                   LandBoundaries(J)%Entry5(I) 

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                    DO I=1,LandBoundaries(J)%NumNodes

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry2(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry2(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry2(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry2(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry2(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry2(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry2(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry2(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry2(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry2(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry2(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry2(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSEIF(LandBoundaries(J)%Code.EQ.52)THEN

                    ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

                    DO I=1,LandBoundaries(J)%NumNodes

                        READ(24,*) LandBoundaries(J)%Entry1(I)

                        IF((I.GT.1).AND. &
                          (((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                            (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                           ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                            (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                            (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer)))))THEN

                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                Lat(NINT(LandBoundaries(J)%Entry1(I)))
                            Counter = Counter + 1

                        ENDIF

                    ENDDO

                ELSE

                    WRITE(*,'(A)',ADVANCE="YES") " "
                    WRITE(*,'(A)',ADVANCE="YES") "ERROR!"
                    WRITE(*,'(A,I4)',ADVANCE="YES") "J = ", J
                    WRITE(*,'(A,I10)',ADVANCE="YES") "LandBoundaries(J)%Code = ", LandBoundaries(J)%Code 

                ENDIF
            ENDDO



        ENDIF

!99990     continue
            CLOSE(UNIT=16,STATUS="KEEP")
        CLOSE(UNIT=24,STATUS="KEEP")
        CLOSE(UNIT=13,STATUS="KEEP")
        CLOSE(UNIT=31,STATUS="KEEP")

        IF(ALLOCATED(Bath))           DEALLOCATE(Bath)
        IF(ALLOCATED(LandBoundaries)) DEALLOCATE(LandBoundaries)
        IF(ALLOCATED(Lat))            DEALLOCATE(Lat)
        IF(ALLOCATED(Long))           DEALLOCATE(Long)
        IF(ALLOCATED(NC))             DEALLOCATE(NC)
        IF(ALLOCATED(NET))            DEALLOCATE(NET)
        IF(ALLOCATED(NPT))            DEALLOCATE(NPT)
        IF(ALLOCATED(OldToNew))       DEALLOCATE(OldToNew)
        IF(ALLOCATED(OpenBoundaries)) DEALLOCATE(OpenBoundaries)

        IF(Verbose.GE.3)THEN
            WRITE(*,'(A,I4.4,A)',ADVANCE="YES") "Processor ",MyRank," processed the grid file."
        ENDIF

END SUBROUTINE



SUBROUTINE ReadInputFile

        USE DATA

        IMPLICIT NONE

#ifdef CMPI
        INCLUDE 'mpif.h'
#endif

        INTRINSIC                                :: INDEX
        INTRINSIC                                :: LEN_TRIM
        INTRINSIC                                :: TRIM

        CHARACTER(LEN=1)                         :: JunkC
        CHARACTER(LEN=50)                        :: TempC
        CHARACTER(LEN=50)                        :: TempC2

        INTEGER                                  :: I
        INTEGER                                  :: RecordsBegin
        INTEGER                                  :: RecordsEnd

        REAL(8)                                  :: TempR

        OPEN(UNIT=11,FILE=TRIM(InputFile),ACTION="READ")

        READ(UNIT=11,FMT='(A1)')  JunkC ! FOR BEST RESULTS ...
        READ(UNIT=11,FMT='(A1)')  JunkC ! 01234567890 ...

        READ(UNIT=11,FMT=*)       Verbose 
        READ(UNIT=11,FMT='(A50)') Path
        READ(UNIT=11,FMT='(A50)') TempPath
        READ(UNIT=11,FMT='(A50)') AlphaLabel
        READ(UNIT=11,FMT='(A50)') TempC
        IF(TempC(1:1).EQ."1")THEN
            TempC2 = TempC(1:INDEX(TempC,",")-1)
            READ(UNIT=TempC2,FMT=*) IfAddPlotLabel
            PlotLabel = TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC))
        ENDIF
        READ(UNIT=11,FMT=*)       IfAddTimeBar

        READ(UNIT=11,FMT=*) LongW
        READ(UNIT=11,FMT=*) LongE
        READ(UNIT=11,FMT=*) LatS
        READ(UNIT=11,FMT=*) LatN

        IF(LongW.GE.LongE)THEN
            TempR = LongW
            LongW = LongE
            LongE = TempR
        ENDIF
        IF(LatS.GE.LatN)THEN
            TempR = LatS
            LatS = LatN
            LatN = TempR
        ENDIF

        READ(UNIT=11,FMT='(A40)') Fort14File
        READ(UNIT=11,FMT=*)       IfPlotGrid

        READ(UNIT=11,FMT='(A1)')  JunkC ! PARAMETERS FOR CONTOURS
        
        READ(UNIT=11,FMT=*)       IfPlotFilledContours
        READ(UNIT=11,FMT=*)       IfPlotContourLines
        IF((IfPlotFilledContours.EQ.1).AND.(IfPlotContourLines.EQ.2))THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: IfPlotFilledContours = 1, then IfPlotContourLines = 0 or 1."
            ENDIF
#ifdef CMPI
            CALL MPI_FINALIZE(IERR)
#endif
            STOP
        ENDIF
        IF((IfPlotFilledContours.EQ.2).AND.(IfPlotContourLines.EQ.1))THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: IfPlotFilledContours = 2, then IfPlotContourLines = 0 or 2."
            ENDIF
#ifdef CMPI
            CALL MPI_FINALIZE(IERR)
#endif
            STOP
        ENDIF
        READ(UNIT=11,FMT='(A50)') TempC
        IF(IfPlotFilledContours.LE.1)THEN
            IF(INDEX(TempC,",").GT.0)THEN
                IF(MyRank.EQ.0)THEN
                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: IfPlotFilledContours = 0 or 1, then list only one contour file name."
                ENDIF
#ifdef CMPI
                CALL MPI_FINALIZE(IERR)
#endif
                STOP
            ENDIF
            ContourFile1 = TRIM(TempC)
        ELSE
            IF(INDEX(TempC,",").LE.0)THEN
                IF(MyRank.EQ.0)THEN
                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: IfPlotFilledContours = 2, then list two contour file names."
                ENDIF
#ifdef CMPI
                CALL MPI_FINALIZE(IERR)
#endif
                STOP
            ENDIF
            ContourFile1 = TempC(1:INDEX(TempC,",")-1)
            ContourFile2 = TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC))
        ENDIF
        READ(UNIT=11,FMT='(A)')   ContourFileFormat
        IF(TRIM(ContourFileFormat).EQ."0")THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileFormat = 0 and reset to OUTPUT-FULL."
            ENDIF
            ContourFileFormat = "OUTPUT-FULL"
        ENDIF
        IF(TRIM(ContourFileFormat).EQ."1")THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileFormat = 1 and reset to OUTPUT-SPARSE."
            ENDIF
            ContourFileFormat = "OUTPUT-SPARSE"
        ENDIF
        IF(TRIM(ContourFileFormat).EQ."2")THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileFormat = 2 and reset to GRID-BATH."
            ENDIF
            ContourFileFormat = "GRID-BATH"
        ENDIF
        IF((TRIM(ContourFileFormat).EQ."GRID-SIZE").AND.(IfPlotFilledContours.EQ.2))THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ContourFileFormat = GRID-SIZE, but IfPlotFilledContours = 2."
            ENDIF
#ifdef CMPI
            CALL MPI_FINALIZE(IERR)
#endif
            STOP
        ENDIF
        IF((TRIM(ContourFileFormat).EQ."GRID-SIZE").AND.(IfPlotContourLines.EQ.2))THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ContourFileFormat = GRID-SIZE, but IfPlotContourLines = 2."
            ENDIF
#ifdef CMPI
            CALL MPI_FINALIZE(IERR)
#endif
            STOP
        ENDIF
        READ(UNIT=11,FMT=*)       ContourConversionFactor
        READ(UNIT=11,FMT='(A40)') ContourUnits
        READ(UNIT=11,FMT='(A)')   Palette
        IF(TRIM(Palette).EQ."0")THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "WARNING: Palette = 0 and reset to RGB."
            ENDIF
            Palette = "RGB"
        ENDIF
        IF(TRIM(Palette).EQ."1")THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "WARNING: Palette = 1 and reset to SMS."
            ENDIF
            Palette = "SMS"
        ENDIF
        IF(TRIM(Palette).EQ."2")THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "WARNING: Palette = 2 and reset to SMS+INTERVALS."
            ENDIF
            Palette = "SMS+INTERVALS"
        ENDIF
        READ(UNIT=11,FMT='(A50)') TempC
        IF((TRIM(Palette).EQ."RGB").OR.(TRIM(Palette).EQ."SMS"))THEN
            IF(INDEX(TempC,",").GT.0)THEN
                IF(MyRank.EQ.0)THEN
                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: Palette = RGB or SMS, then list only one file name."
                ENDIF
#ifdef CMPI
                CALL MPI_FINALIZE(IERR)
#endif
                STOP
            ENDIF
            SMSPalette = TRIM(TempC)
        ELSE
            IF(INDEX(TempC,",").LE.0)THEN
                IF(MyRank.EQ.0)THEN
                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: Palette = SMS+INTERVALS, then list two file names."
                ENDIF
#ifdef CMPI
                CALL MPI_FINALIZE(IERR)
#endif
                STOP
            ENDIF
            SMSPalette = TempC(1:INDEX(TempC,",")-1)
            DiffContoursFile = TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC))
        ENDIF
        READ(UNIT=11,FMT='(A40)') ColorLines
        IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
            IF(IfPlotGrid.EQ.0)THEN
                IF(MyRank.EQ.0)THEN
                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ColorLines = GRID-BATH, then IfPlotGrid = 1."
                ENDIF
#ifdef CMPI
                CALL MPI_FINALIZE
#endif
                STOP
            ENDIF
        ELSEIF(TRIM(ColorLines).EQ."GRID-SIZE")THEN
            IF(IfPlotGrid.EQ.0)THEN
                IF(MyRank.EQ.0)THEN
                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ColorLines = GRID-SIZE, then IfPlotGrid = 1."
                ENDIF
#ifdef CMPI
                CALL MPI_FINALIZE
#endif
                STOP
            ENDIF
        ELSEIF(TRIM(ColorLines).EQ."CONTOUR-LINES")THEN
            IF(IfPlotContourLines.EQ.0)THEN
                IF(MyRank.EQ.0)THEN
                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ColorLines = CONTOUR-LINES, then IfPlotContourLines > 0."
                ENDIF
#ifdef CMPI
                CALL MPI_FINALIZE
#endif
                STOP
            ENDIF
        ELSE
            ColorLines = "DEFAULT"
        ENDIF 
        READ(UNIT=11,FMT='(A50)') TempC
        IF((TRIM(Palette).EQ."RGB").OR.(TRIM(Palette).EQ."SMS"))THEN
            IF(TempC(1:1).EQ."F")THEN
                IF((IfPlotFilledContours.GT.0).OR.(IfPlotContourLines.GT.0))THEN
                    FindContourRange = 1
                ELSE
                    FindContourRange = 0
                ENDIF
            ELSE
                IF(INDEX(TempC,",").LE.0)THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: If not using FIND to compute min/max for contour range," &
                                            //" then list them on the same line in the input file."
                    ENDIF
#ifdef CMPI
                    CALL MPI_FINALIZE(IERR)
#endif
                    STOP
                ENDIF
                FindContourRange = 0
                TempC2 = TempC(1:INDEX(TempC,",")-1)
                READ(UNIT=TempC2,FMT=*) ContourMin
                TempC2 = TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC))
                READ(UNIT=TempC2,FMT=*) ContourMax
            ENDIF
        ELSE
            FindContourRange = 0
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A,A,A)') "WARNING: Contour range superseded by contours specified in ", &
                                            TRIM(DiffContoursFile),"."
            ENDIF
        ENDIF
        READ(UNIT=11,FMT=*)       ContourInterval
        READ(UNIT=11,FMT=*)       SplitBy
        READ(UNIT=11,FMT=*)       ContourLabelEvery
        READ(UNIT=11,FMT=*)       ContourLabelMinDist
        READ(UNIT=11,FMT='(A40)') ContourLabelRotation
        READ(UNIT=11,FMT=*)       ContourLabelSize
        READ(UNIT=11,FMT=*)       ScaleLabelEvery
        READ(UNIT=11,FMT=*)       ScaleWidth

        READ(UNIT=11,FMT='(A1)')  JunkC ! PARAMETERS FOR VECTORS

        READ(UNIT=11,FMT=*)       IfPlotVectors
        READ(UNIT=11,FMT='(A40)') VectorFile
        READ(UNIT=11,FMT='(A)')   VectorFileFormat
        IF(TRIM(VectorFileFormat).EQ."0")THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "WARNING: VectorFileFormat = 0 and reset to OUTPUT-FULL."
            ENDIF
            VectorFileFormat = "OUTPUT-FULL"
        ENDIF
        IF(TRIM(VectorFileFormat).EQ."1")THEN
            IF(MyRank.EQ.0)THEN
                WRITE(UNIT=*,FMT='(A)') "WARNING: VectorFileFormat = 1 and reset to OUTPUT-SPARSE."
            ENDIF
            VectorFileFormat = "OUTPUT-SPARSE"
        ENDIF
        READ(UNIT=11,FMT=*)       VectorConversionFactor
        READ(UNIT=11,FMT='(A40)') VectorUnits
        READ(UNIT=11,FMT=*)       VectorMag
        READ(UNIT=11,FMT=*)       VectorSpacing
        READ(UNIT=11,FMT=*)       VectorHeadLength
        READ(UNIT=11,FMT=*)       VectorHeadWidth
        READ(UNIT=11,FMT=*)       VectorTailWidth
        READ(UNIT=11,FMT='(A50)') TempC
        IF(TempC(1:1).EQ."F")THEN
            IF(IfPlotVectors.GT.0)THEN
                FindVectorScale = 1
            ELSE
                FindVectorScale = 0
                VectorScaleMag = 0.0
            ENDIF
        ELSE
            FindVectorScale = 0
            READ(UNIT=TempC,FMT=*) VectorScaleMag
        ENDIF

        READ(UNIT=11,FMT='(A1)')  JunkC ! PARAMETERS FOR OVERALL PLOT

        READ(UNIT=11,FMT='(A50)') TempC
        IF(TempC(1:1).EQ."1")THEN
            IfPlotBoundaries = 1
            BoundariesColor = TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC))
        ELSE
            IfPlotBoundaries = 0
        ENDIF
        READ(UNIT=11,FMT='(A50)') TempC
        IF(TempC(1:1).EQ."1")THEN
            IfPlotCoastline = 1
            DO I=3,LEN_TRIM(TempC)
                IF(TempC(i:i).EQ.",")THEN
                    CoastlineColor = TempC(3:i-1)
                ENDIF
            ENDDO
            CoastlineFile = TempC(4+LEN_TRIM(CoastlineColor):LEN_TRIM(TempC))
        ELSE
            IfPlotCoastline = 0
        ENDIF
        READ(UNIT=11,FMT='(A50)') TempC
        IF(TempC(1:1).EQ."1")THEN
            IfPlotLabels = 1
            DO I=3,LEN_TRIM(TempC)
                IF(TempC(i:i).EQ.",")THEN
                    LabelsColor = TempC(3:i-1)
                ENDIF
            ENDDO
            LabelsFile = TempC(4+LEN_TRIM(LabelsColor):LEN_TRIM(TempC))
        ELSE
            IfPlotLabels = 0
        ENDIF
        READ(UNIT=11,FMT=*)       Width
        READ(UNIT=11,FMT=*)       Buffer
        READ(UNIT=11,FMT=*)       BorderIncrementMinor
        READ(UNIT=11,FMT=*)       BorderIncrementMajor
        READ(UNIT=11,FMT=*)       LargeJPGResolution
        READ(UNIT=11,FMT=*)       SmallJPGWidth
        READ(UNIT=11,FMT=*)       NumRecords
        IF((TRIM(ContourFileFormat).EQ."GRID-BATH").OR.(TRIM(ContourFileFormat).EQ."GRID-SIZE"))THEN
            IF(MyRank.EQ.0)THEN
                WRITE(*,'(A)') "WARNING: Record information ignored at end of input file; "// &
                               "one record will be generated from grid file."
            ENDIF
            NumRecords = 1
            ALLOCATE(RecordsList(1:NumRecords))
            DO I=1,NumRecords
                RecordsList(I) = 1
            ENDDO
        ELSEIF(INDEX(ContourFileFormat,"13-").GT.0)THEN
            IF(INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0)THEN
                IF(TRIM(ContourFileFormat).EQ."13-WIND-REDUCTION")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(*,'(A)') "WARNING: Record information ignored at end of input file; "// &
                                       "twelve records will be generated from fort.13 file."
                    ENDIF
                    NumRecords = 12
                    ALLOCATE(RecordsList(1:NumRecords))
                    DO I=1,NumRecords
                        RecordsList(I) = I
                    ENDDO
                ELSE
                    IF(MyRank.EQ.0)THEN
                        WRITE(*,'(A)') "WARNING: Record information ignored at end of input file; "// &
                                       "one record will be generated from fort.13 file."
                    ENDIF
                    NumRecords = 1
                    ALLOCATE(RecordsList(1:NumRecords))
                    DO I=1,NumRecords
                        READ(UNIT=ContourFileFormat,FMT='(18X,I2)') RecordsList(I)
                    ENDDO
                ENDIF
            ELSE
                IF(MyRank.EQ.0)THEN
                    WRITE(*,'(A)') "WARNING: Record information ignored at end of input file; "// &
                                   "one record will be generated from fort.13 file."
                ENDIF
                NumRecords = 1
                ALLOCATE(RecordsList(1:NumRecords))
                DO I=1,NumRecords
                    RecordsList(I) = 1
                ENDDO
            ENDIF
        ELSE
            IF(NumRecords.GT.0)THEN
                ALLOCATE(RecordsList(1:NumRecords))
                DO I=1,NumRecords
                    READ(UNIT=11,FMT=*) RecordsList(I)
                ENDDO
            ELSEIF(NumRecords.EQ.-1)THEN
                READ(UNIT=11,FMT=*) RecordsBegin
                READ(UNIT=11,FMT=*) RecordsEnd
                NumRecords = RecordsEnd - RecordsBegin + 1
                ALLOCATE(RecordsList(1:NumRecords))
                DO I=1,NumRecords
                    RecordsList(I) = RecordsBegin - 1 + I
                ENDDO
            ENDIF
        ENDIF

        CLOSE(UNIT=11,STATUS="KEEP")

        IF(Verbose.GE.3)THEN
            WRITE(*,'(A,I4.4,A)') "Processor ",MyRank," read the input file."
        ENDIF

END SUBROUTINE



SUBROUTINE WritePSImage(Record)

        USE DATA

        IMPLICIT NONE

        INTRINSIC         :: ABS
        INTRINSIC         :: LEN_TRIM
        INTRINSIC         :: NINT
        INTRINSIC         :: TRIM

        CHARACTER(LEN=8)  :: BorderIncrementMajorC
        CHARACTER(LEN=8)  :: BorderIncrementMinorC
        CHARACTER(LEN=8)  :: ContourLabelEveryC
        CHARACTER(LEN=8)  :: ContourLabelMinDistC
        CHARACTER(LEN=8)  :: ContourLabelSizeC
        CHARACTER(LEN=8)  :: ContourScaleYC
        CHARACTER(LEN=40) :: LabelAlign
        CHARACTER(LEN=40) :: LabelC
        CHARACTER(LEN=40) :: LabelLat
        CHARACTER(LEN=40) :: LabelLon
        CHARACTER(LEN=40) :: LabelX
        CHARACTER(LEN=40) :: LabelY
        CHARACTER(LEN=8)  :: LargeJPGResolutionC
        CHARACTER(LEN=500):: Line
        CHARACTER(LEN=60) :: PlotName
        CHARACTER(LEN=8)  :: PlotLabelXAdjustC
        CHARACTER(LEN=8)  :: PlotLabelYAdjustC
        CHARACTER(LEN=8)  :: ScaleLabelEveryC
        CHARACTER(LEN=8)  :: ScaleHeightC
        CHARACTER(LEN=8)  :: ScaleWidthC
        CHARACTER(LEN=8)  :: SideBarXC
        CHARACTER(LEN=8)  :: SmallJPGWidthC
        CHARACTER(LEN=8)  :: TimeScaleYC
        CHARACTER(LEN=8)  :: TimeScaleTextYC
        CHARACTER(LEN=8)  :: VectorHeadLengthC
        CHARACTER(LEN=8)  :: VectorHeadWidthC
        CHARACTER(LEN=8)  :: VectorMagC
        CHARACTER(LEN=8)  :: VectorScaleXC
        CHARACTER(LEN=8)  :: VectorScaleYC
        CHARACTER(LEN=8)  :: VectorSpacingC
        CHARACTER(LEN=8)  :: VectorTailWidthC
        CHARACTER(LEN=8)  :: WidthC
        CHARACTER(LEN=8)  :: XMax
        CHARACTER(LEN=8)  :: XMaxBuf
        CHARACTER(LEN=8)  :: XMin
        CHARACTER(LEN=8)  :: XMinBuf
        CHARACTER(LEN=8)  :: YMax
        CHARACTER(LEN=8)  :: YMaxBuf
        CHARACTER(LEN=8)  :: YMin
        CHARACTER(LEN=8)  :: YMinBuf

        INTEGER           :: Found
        INTEGER           :: I
        INTEGER           :: J
        INTEGER           :: IfStarted
        INTEGER           :: NumLabels
        INTEGER           :: Record

        REAL(8)           :: ContourScaleY
        REAL(8)           :: Height
        REAL(8)           :: JunkR
        REAL(8)           :: PlotLabelXAdjust
        REAL(8)           :: PlotLabelYAdjust
        REAL(8)           :: SideBarX
        REAL(8)           :: TimeScaleY
        REAL(8)           :: TimeScaleTextY
        REAL(8)           :: VectorScaleX
        REAL(8)           :: VectorScaleY

        IfStarted = 0

        CALL SYSTEM(TRIM(Path)//"gmtset PAPER_MEDIA letter")
        CALL SYSTEM(TRIM(Path)//"gmtset PLOT_DEGREE_FORMAT -D")
        CALL SYSTEM(TRIM(Path)//"gmtset OUTPUT_DEGREE_FORMAT -D")
        CALL SYSTEM(TRIM(Path)//"gmtset BASEMAP_TYPE fancy")
        CALL SYSTEM(TRIM(Path)//"gmtset D_FORMAT %lg")

        IF(Width<10.0)THEN
            WRITE(UNIT=WidthC,FMT='(F4.2)') Width
        ELSE
            WRITE(UNIT=WidthC,FMT='(F5.2)') Width
        ENDIF

        IF(ABS(LongW)<100.0)THEN
            WRITE(UNIT=XMin,FMT='(F6.2)') LongW
        ELSE
            WRITE(UNIT=XMin,FMT='(F7.2)') LongW
        ENDIF
        IF(ABS(LongE)<100.0)THEN
            WRITE(UNIT=XMax,FMT='(F6.2)') LongE
        ELSE
            WRITE(UNIT=XMax,FMT='(F7.2)') LongE
        ENDIF
        WRITE(UNIT=YMin,FMT='(F5.2)') LatS
        WRITE(UNIT=YMax,FMT='(F5.2)') LatN

        WRITE(UNIT=TempMapFile1,FMT='(A,A,I4.4,A)') TRIM(TempPath), "mapproject.", Record, ".inp"
        WRITE(UNIT=TempMapFile2,FMT='(A,A,I4.4,A)') TRIM(TempPath), "mapproject.", Record, ".out"
        OPEN(UNIT=29,FILE=TRIM(TempMapFile1),ACTION="WRITE")
        WRITE(UNIT=29,FMT=*) LongE,LatN
        CLOSE(UNIT=29,STATUS="KEEP")
        CALL SYSTEM(TRIM(Path)//"mapproject "//TRIM(TempMapFile1)               &
                    //" -Di -JM"//TRIM(WidthC)//"i -R"//TRIM(XMin)//"/"         &
                    //TRIM(XMax)//"/"//TRIM(YMin)//"/"//TRIM(YMax)              &
                    //" > "//TRIM(TempMapFile2))
        OPEN(UNIT=30,FILE=TRIM(TempMapFile2),ACTION="READ")
        READ(UNIT=30,FMT=*) JunkR, Height
        CLOSE(UNIT=30,STATUS="KEEP")

!        WRITE(UNIT=PlotName,FMT='(A,I4.4)') TRIM(AlphaLabel), Record
        WRITE(UNIT=PlotName,FMT='(A)') TRIM(AlphaLabel)

        IF((IfAddTimeBar.EQ.0).AND.(IfPlotVectors.EQ.0))THEN 
            ScaleHeight = Height - 0.3
        ELSEIF((IfAddTimeBar.EQ.1).AND.(IfPlotVectors.EQ.0))THEN
            ScaleHeight = Height - 1.2
        ELSEIF((IfAddTimeBar.EQ.0).AND.(IfPlotVectors.EQ.1))THEN
            ScaleHeight = Height - 0.8
        ELSE
            ScaleHeight = Height - 1.6
        ENDIF

        IF(ScaleHeight<10.0)THEN
            WRITE(UNIT=ScaleHeightC,FMT='(F4.2)') ScaleHeight
        ELSEIF(ScaleHeight<100.0)THEN
            WRITE(UNIT=ScaleHeightC,FMT='(F5.2)') ScaleHeight
        ELSE 
            WRITE(UNIT=ScaleHeightC,FMT='(F6.2)') ScaleHeight
        ENDIF

        IF(BorderIncrementMajor<10.0)THEN
            WRITE(UNIT=BorderIncrementMajorC,FMT='(F4.2)') BorderIncrementMajor
        ELSEIF(BorderIncrementMajor<100.0)THEN
            WRITE(UNIT=BorderIncrementMajorC,FMT='(F5.2)') BorderIncrementMajor
        ELSE 
            WRITE(UNIT=BorderIncrementMajorC,FMT='(F6.2)') BorderIncrementMajor
        ENDIF

        IF(BorderIncrementMinor<10.0)THEN
            WRITE(UNIT=BorderIncrementMinorC,FMT='(F4.2)') BorderIncrementMinor
        ELSEIF(BorderIncrementMinor<100.0)THEN
            WRITE(UNIT=BorderIncrementMinorC,FMT='(F5.2)') BorderIncrementMinor
        ELSE 
            WRITE(UNIT=BorderIncrementMinorC,FMT='(F6.2)') BorderIncrementMinor
        ENDIF

        WRITE(UNIT=ContourLabelEveryC,FMT='(I4.4)') ContourLabelEvery

        IF(ContourLabelMinDist<10.0)THEN
            WRITE(UNIT=ContourLabelMinDistC,FMT='(F4.2)') ContourLabelMinDist
        ELSEIF(ContourLabelMinDist<100.0)THEN
            WRITE(UNIT=ContourLabelMinDistC,FMT='(F5.2)') ContourLabelMinDist
        ELSE 
            WRITE(UNIT=ContourLabelMinDistC,FMT='(F6.2)') ContourLabelMinDist
        ENDIF

        WRITE(UNIT=ContourLabelSizeC,FMT='(I4.4)') ContourLabelSize

        ContourScaleY = 0.5 * ScaleHeight
        IF(ContourScaleY<10.0)THEN
            WRITE(UNIT=ContourScaleYC,FMT='(F4.2)') ContourScaleY
        ELSEIF(ContourScaleY<100.0)THEN
            WRITE(UNIT=ContourScaleYC,FMT='(F5.2)') ContourScaleY
        ELSE 
            WRITE(UNIT=ContourScaleYC,FMT='(F6.2)') ContourScaleY
        ENDIF

        WRITE(UNIT=LargeJPGResolutionC,FMT='(I4.4)') LargeJPGResolution

        PlotLabelXAdjust = 0.5 * Width
        IF(PlotLabelXAdjust<10.0)THEN
            WRITE(UNIT=PlotLabelXAdjustC,FMT='(F4.2)') PlotLabelXAdjust
        ELSE
            WRITE(UNIT=PlotLabelXAdjustC,FMT='(F5.2)') PlotLabelXAdjust
        ENDIF

        PlotLabelYAdjust = Height + Buffer
        IF(PlotLabelYAdjust<10.0)THEN
            WRITE(UNIT=PlotLabelYAdjustC,FMT='(F4.2)') PlotLabelYAdjust
        ELSE
            WRITE(UNIT=PlotLabelYAdjustC,FMT='(F5.2)') PlotLabelYAdjust
        ENDIF

        IF(ScaleLabelEvery<10.D0)THEN
            WRITE(UNIT=ScaleLabelEveryC,FMT='(F4.2)') ScaleLabelEvery
        ELSEIF(ScaleLabelEvery<100.D0)THEN
            WRITE(UNIT=ScaleLabelEveryC,FMT='(F5.2)') ScaleLabelEvery
        ELSEIF(ScaleLabelEvery<1000.D0)THEN
            WRITE(UNIT=ScaleLabelEveryC,FMT='(F6.3)') ScaleLabelEvery
        ELSE
            WRITE(UNIT=ScaleLabelEveryC,FMT='(F7.4)') ScaleLabelEvery
        ENDIF

        IF(ScaleWidth<10.0)THEN
            WRITE(UNIT=ScaleWidthC,FMT='(F4.2)') ScaleWidth
        ELSEIF(ScaleWidth<100.0)THEN
            WRITE(UNIT=ScaleWidthC,FMT='(F5.2)') ScaleWidth
        ELSE 
            WRITE(UNIT=ScaleWidthC,FMT='(F6.2)') ScaleWidth
        ENDIF

        SideBarX = Width + Buffer
        IF(SideBarX<10.0)THEN
            WRITE(UNIT=SideBarXC,FMT='(F4.2)') SideBarX
        ELSE
            WRITE(UNIT=SideBarXC,FMT='(F5.2)') SideBarX
        ENDIF

        WRITE(UNIT=SmallJPGWidthC,FMT='(I4.4)') SmallJPGWidth

        TimeScaleY = Height - 0.35
        IF(TimeScaleY<10.0)THEN
            WRITE(UNIT=TimeScaleYC,FMT='(F4.2)') TimeScaleY
        ELSEIF(TimeScaleY<100.0)THEN
            WRITE(UNIT=TimeScaleYC,FMT='(F5.2)') TimeScaleY
        ELSE
            WRITE(UNIT=TimeScaleYC,FMT='(F6.2)') TimeScaleY
        ENDIF

        TimeScaleTextY = Height
        IF(TimeScaleTextY<10.0)THEN
            WRITE(UNIT=TimeScaleTextYC,FMT='(F4.2)') TimeScaleTextY
        ELSEIF(TimeScaleTextY<100.0)THEN
            WRITE(UNIT=TimeScaleTextYC,FMT='(F5.2)') TimeScaleTextY
        ELSE
            WRITE(UNIT=TimeScaleTextYC,FMT='(F6.2)') TimeScaleTextY
        ENDIF

        IF(VectorHeadLength<10.0)THEN
            WRITE(UNIT=VectorHeadLengthC,FMT='(F4.2)') VectorHeadLength
        ELSEIF(VectorHeadLength<100.0)THEN
            WRITE(UNIT=VectorHeadLengthC,FMT='(F5.2)') VectorHeadLength
        ELSE
            WRITE(UNIT=VectorHeadLengthC,FMT='(F6.2)') VectorHeadLength
        ENDIF

        IF(VectorHeadWidth<10.0)THEN
            WRITE(UNIT=VectorHeadWidthC,FMT='(F4.2)') VectorHeadWidth
        ELSEIF(VectorHeadWidth<100.0)THEN
            WRITE(UNIT=VectorHeadWidthC,FMT='(F5.2)') VectorHeadWidth
        ELSE
            WRITE(UNIT=VectorHeadWidthC,FMT='(F6.2)') VectorHeadWidth
        ENDIF

        IF(VectorMag<10.0)THEN
            WRITE(UNIT=VectorMagC,FMT='(F4.2)') VectorMag
        ELSEIF(VectorMag<100.0)THEN
            WRITE(UNIT=VectorMagC,FMT='(F5.2)') VectorMag
        ELSE
            WRITE(UNIT=VectorMagC,FMT='(F6.2)') VectorMag
        ENDIF

        VectorScaleX = Width + Buffer
        IF(VectorScaleX<10.0)THEN
            WRITE(UNIT=VectorScaleXC,FMT='(F4.2)') VectorScaleX
        ELSE
            WRITE(UNIT=VectorScaleXC,FMT='(F5.2)') VectorScaleX
        ENDIF

        IF(VectorSpacing<10.0)THEN
            WRITE(UNIT=VectorSpacingC,FMT='(F4.2)') VectorSpacing
        ELSE
            WRITE(UNIT=VectorSpacingC,FMT='(F5.2)') VectorSpacing
        ENDIF

        IF(VectorTailWidth<10.0)THEN
            WRITE(UNIT=VectorTailWidthC,FMT='(F4.2)') VectorTailWidth
        ELSEIF(VectorTailWidth<100.0)THEN
            WRITE(UNIT=VectorTailWidthC,FMT='(F5.2)') VectorTailWidth
        ELSE
            WRITE(UNIT=VectorTailWidthC,FMT='(F6.2)') VectorTailWidth
        ENDIF

        IF(IfPlotFilledContours.GE.1)THEN

            Line = ""
            Line = TRIM(Line)//TRIM(Path)//"pscontour"
            Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(ContourXYZFile)
            Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"ContourPalette.cpt"
            Line = TRIM(Line)//" "//"-JM"//TRIM(WidthC)//"i"
            Line = TRIM(Line)//" "//"-R"//TRIM(XMin)//"/"//TRIM(XMax)//"/"//TRIM(YMin)//"/"//TRIM(YMax)
            Line = TRIM(Line)//" "//"-Bp"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)// &
                               "/s"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)//"WeSn"
            Line = TRIM(Line)//" "//"-I"
            Line = TRIM(Line)//" "//"-T"//TRIM(TempPath)//TRIM(Fort14File)//".tri"
            Line = TRIM(Line)//" "//"-K"
            IF(IfStarted.EQ.0)THEN
                Line = TRIM(Line)//" "//">"
                IfStarted = 1
            ELSE
                Line = TRIM(Line)//" "//"-O >>"
            ENDIF
            Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"

!             write(*,*) TRIM(Line)

            CALL SYSTEM(TRIM(Line))

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the filled contours for record ",Record,"."
            ENDIF

        ENDIF

        IF(IfPlotGrid.EQ.1)THEN

            Line = ""
            Line = TRIM(Line)//TRIM(Path)//"psxy"
            Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(Fort14File)//".edges.xy"
            Line = TRIM(Line)//" "//"-JM"//TRIM(WidthC)//"i"
            Line = TRIM(Line)//" "//"-R"//TRIM(XMin)//"/"//TRIM(XMax)//"/"//TRIM(YMin)//"/"//TRIM(YMax)
            IF(IfPlotFilledContours.EQ.0)THEN
                Line = TRIM(Line)//" "//"-Bp"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)// &
                                   "/s"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)//"WeSn"
            ENDIF
            Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"ContourPalette.cpt"
            Line = TRIM(Line)//" "//"-M"
            IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
                Line = TRIM(Line)//" "//"-W+"
            ELSEIF(TRIM(ColorLines).EQ."GRID-SIZE")THEN
                Line = TRIM(Line)//" "//"-W+"
            ELSE
                Line = TRIM(Line)//" "//"-W1,Black"
            ENDIF
            Line = TRIM(Line)//" "//"-K"
            IF(IfStarted.EQ.0)THEN
                Line = TRIM(Line)//" "//">"
                IfStarted = 1
            ELSE
                Line = TRIM(Line)//" "//"-O >>"
            ENDIF
            Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
            CALL SYSTEM(TRIM(Line))

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the underlying mesh for record ",Record,"."
            ENDIF

        ENDIF

        IF(IfPlotContourLines.GE.1)THEN

            Line = ""
            Line = TRIM(Line)//TRIM(Path)//"pscontour"
            Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(ContourXYZFile)
            Line = TRIM(Line)//" "//"-JM"//TRIM(WidthC)//"i"
            Line = TRIM(Line)//" "//"-R"//TRIM(XMin)//"/"//TRIM(XMax)//"/"//TRIM(YMin)//"/"//TRIM(YMax)
            IF((IfPlotFilledContours.EQ.0).AND.(IfPlotGrid.EQ.0))THEN
                Line = TRIM(Line)//" "//"-Bp"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)// &
                                   "/s"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)//"WeSn"
            ENDIF
            Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"LabelPalette.cpt"
            IF(ContourLabelEvery.EQ.0)THEN
                Line = TRIM(Line)//" "//"-A-"
            ELSE
                Line = TRIM(Line)//" "//"-A+a"//TRIM(ContourLabelRotation)//"+s"//TRIM(ContourLabelSizeC)
                Line = TRIM(Line)//" "//"-Gd"//TRIM(ContourLabelEveryC)//"i:"//TRIM(ContourLabelMinDistC)//"i"
            ENDIF
            IF(TRIM(ColorLines).EQ."CONTOUR-LINES")THEN
                Line = TRIM(Line)//" "//"-W+"
            ELSE
                Line = TRIM(Line)//" "//"-W"
            ENDIF
            Line = TRIM(Line)//" "//"-T"//TRIM(TempPath)//TRIM(Fort14File)//".tri"
            Line = TRIM(Line)//" "//"-K"
            IF(IfStarted.EQ.0)THEN
                Line = TRIM(Line)//" "//">"
                IfStarted = 1
            ELSE
                Line = TRIM(Line)//" "//"-O >>"
            ENDIF
            Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"

!             write(*,*) TRIM(Line)

            CALL SYSTEM(TRIM(Line))

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the contour lines for record ",Record,"."
            ENDIF

        ENDIF

        IF((IfPlotFilledContours.GE.1).OR.(TRIM(ColorLines).NE."DEFAULT"))THEN

            Line = ""
            Line = TRIM(Line)//TRIM(Path)//"psscale"
            Line = TRIM(Line)//" "//"-D"//TRIM(SideBarXC)//"i/"//TRIM(ContourScaleYC)//"i/" &
                             //TRIM(ScaleHeightC)//"i/"//TRIM(ScaleWidthC)//"i"
            Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"ScalePalette.cpt"
            IF(TRIM(Palette).EQ."SMS")THEN
                Line = TRIM(Line)//" "//"-B"//TRIM(ScaleLabelEveryC)//"::/:"//TRIM(ContourUnits)//":"
            ELSEIF(TRIM(Palette).EQ."SMS+INTERVALS")THEN
                Line = TRIM(Line)//" "//"-B"//"::/:"//TRIM(ContourUnits)//":"
                Line = TRIM(Line)//" "//"-L"
            ENDIF
            Line = TRIM(Line)//" "//"-K"
            Line = TRIM(Line)//" "//"-O >>"
            Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
            CALL SYSTEM(TRIM(Line))

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the scale for record ",Record,"."
            ENDIF

        ENDIF

        IF(IfPlotBoundaries.EQ.1)THEN

            Line = ""
            Line = TRIM(Line)//TRIM(Path)//"psxy"
            Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(Fort14File)//".bnd.xy"
            Line = TRIM(Line)//" "//"-JM"//TRIM(WidthC)//"i"
            Line = TRIM(Line)//" "//"-R"//TRIM(XMin)//"/"//TRIM(XMax)//"/"//TRIM(YMin)//"/"//TRIM(YMax)
            IF((IfPlotFilledContours.EQ.0).AND.(IfPlotGrid.EQ.0).AND.(IfPlotContourLines.EQ.0))THEN
                Line = TRIM(Line)//" "//"-Bp"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)// &
                                   "/s"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)//"WeSn"
            ENDIF
            Line = TRIM(Line)//" "//"-M"
            Line = TRIM(Line)//" "//"-W2,"//TRIM(BoundariesColor)
            Line = TRIM(Line)//" "//"-K"
            IF(IfStarted.EQ.0)THEN
                Line = TRIM(Line)//" "//">"
                IfStarted = 1
            ELSE
                Line = TRIM(Line)//" "//"-O >>"
            ENDIF
            Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
            CALL SYSTEM(TRIM(Line))

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the levee/road boundaries for record ",Record,"."
            ENDIF

        ENDIF

        IF(IfPlotCoastline.EQ.1)THEN

            Line = ""
            Line = TRIM(Line)//TRIM(Path)//"psxy"
            Line = TRIM(Line)//" "//TRIM(CoastlineFile)
            Line = TRIM(Line)//" "//"-JM"//TRIM(WidthC)//"i"
            Line = TRIM(Line)//" "//"-R"//TRIM(XMin)//"/"//TRIM(XMax)//"/"//TRIM(YMin)//"/"//TRIM(YMax)
            Line = TRIM(Line)//" "//"-G"//TRIM(CoastlineColor)
            Line = TRIM(Line)//" "//"-Sc2p"
            Line = TRIM(Line)//" "//"-K"
            Line = TRIM(Line)//" "//"-O >>"
            Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
            CALL SYSTEM(TRIM(Line))

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the coastline for record ",Record,"."
            ENDIF

        ENDIF

        IF(IfPlotLabels.EQ.1)THEN

            CALL SYSTEM(TRIM(Path)//"gmtset ANNOT_FONT_PRIMARY Helvetica-Bold")

            OPEN(UNIT=32,FILE=TRIM(LabelsFile),ACTION="READ")

            WRITE(UNIT=TempLabelsFile,FMT='(A,A,I4.4,A)') TRIM(TempPath), "Labels.", Record, ".txt"

            READ(UNIT=32,FMT=*) NumLabels

            DO I=1,NumLabels

                READ(UNIT=32,FMT='(A100)') Line
                Found = 0
                DO J=1,LEN_TRIM(Line)
                    IF((Line(J:J).EQ.",").AND.(Found.EQ.0))THEN
                        LabelC = Line(1:J-1)
                        Found = 1
                    ENDIF
                ENDDO
                Found = 0
                DO J=LEN_TRIM(LabelC)+2,LEN_TRIM(Line)
                    IF((Line(J:J).EQ.",").AND.(Found.EQ.0))THEN
                        LabelLon = Line(LEN_TRIM(LabelC)+2:J-1)
                        Found = 1
                    ENDIF
                ENDDO
                Found = 0
                DO J=LEN_TRIM(LabelC)+LEN_TRIM(LabelLon)+3,LEN_TRIM(Line)
                    IF((Line(J:J).EQ.",").AND.(Found.EQ.0))THEN
                        LabelLat = Line(LEN_TRIM(LabelC)+LEN_TRIM(LabelLon)+3:J-1)
                        Found = 1
                    ENDIF
                ENDDO
                LabelAlign = Line(LEN_TRIM(LabelC)+LEN_TRIM(LabelLon)+LEN_TRIM(LabelLat)+4:LEN_TRIM(Line))

                IF(INDEX(LabelAlign,"L").GT.0)THEN
                    LabelX = "3p"
                ELSEIF(INDEX(LabelAlign,"C").GT.0)THEN
                    LabelX = "0p"
                ELSEIF(INDEX(LabelAlign,"R").GT.0)THEN
                    LabelX = "-3p"
                ENDIF
                IF(INDEX(LabelAlign,"T").GT.0)THEN
                    LabelY = "-3p"
                ELSEIF(INDEX(LabelAlign,"M").GT.0)THEN
                    LabelY = "0p"
                ELSEIF(INDEX(LabelAlign,"B").GT.0)THEN
                    LabelY = "3p"
                ENDIF

                OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                WRITE(UNIT=33,FMT='(A10,2X,A10)') LabelLon, LabelLat
                CLOSE(UNIT=33,STATUS="KEEP")

                Line = ""
                Line = TRIM(Line)//TRIM(Path)//"psxy"
                Line = TRIM(Line)//" "//TRIM(TempLabelsFile)
                Line = TRIM(Line)//" "//"-JM"//TRIM(WidthC)//"i"
                Line = TRIM(Line)//" "//"-R"//TRIM(Xmin)//"/"//TRIM(Xmax)//"/"//TRIM(YMin)//"/"//TRIM(Ymax)
                Line = TRIM(Line)//" "//"-G"//TRIM(LabelsColor)
                Line = TRIM(Line)//" "//"-Sc5p"
                Line = TRIM(Line)//" "//"-K"
                Line = TRIM(Line)//" "//"-O >>"
                Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                CALL SYSTEM(TRIM(Line))

                OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                WRITE(UNIT=33,FMT='(A,2X,A,2X,I2,2X,I2,2X,I2,2X,A,2X,A)') TRIM(LabelLon), TRIM(LabelLat), 14, 0, 1, &
                                                                          TRIM(LabelAlign), TRIM(LabelC)
                CLOSE(UNIT=33,STATUS="KEEP")

                Line = ""
                Line = TRIM(Line)//TRIM(Path)//"pstext"
                Line = TRIM(Line)//" "//TRIM(TempLabelsFile)
                Line = TRIM(Line)//" "//"-JM"//TRIM(WidthC)//"i"
                Line = TRIM(Line)//" "//"-R"//TRIM(Xmin)//"/"//TRIM(Xmax)//"/"//TRIM(YMin)//"/"//TRIM(Ymax)
                Line = TRIM(Line)//" "//"-D"//TRIM(LabelX)//"/"//TRIM(LabelY)
                Line = TRIM(Line)//" "//"-G"//TRIM(LabelsColor)
!               Line = TRIM(Line)//" "//"-Sl14p/"//TRIM(LabelC)
                Line = TRIM(Line)//" "//"-K"
                Line = TRIM(Line)//" "//"-O >>"
                Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                CALL SYSTEM(TRIM(Line))

            ENDDO

            CLOSE(UNIT=32,STATUS="KEEP")

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the labels for record ",Record,"."
            ENDIF

            CALL SYSTEM(TRIM(Path)//"gmtset ANNOT_FONT_PRIMARY Helvetica")

        ENDIF

        IF(IfPlotVectors.EQ.1)THEN

            IF(ABS(LongW-LatLonBuffer)<100.0)THEN
                WRITE(UNIT=XMinBuf,FMT='(F6.2)') LongW-LatLonBuffer
            ELSE
                WRITE(UNIT=XMinBuf,FMT='(F7.2)') LongW-LatLonBuffer
            ENDIF
            IF(ABS(LongE+LatLonBuffer)<100.0)THEN
                WRITE(UNIT=XMaxBuf,FMT='(F6.2)') LongE+LatLonBuffer
            ELSE
                WRITE(UNIT=XMaxBuf,FMT='(F7.2)') LongE+LatLonBuffer
            ENDIF
            WRITE(UNIT=YMinBuf,FMT='(F5.2)') LatS-LatLonBuffer
            WRITE(UNIT=YMaxBuf,FMT='(F5.2)') LatN+LatLonBuffer

            CALL SYSTEM(TRIM(Path)//"xyz2grd "//TRIM(TempPath)//TRIM(VectorUFile)//".xyz"//         &
                    " -G"//TRIM(TempPath)//TRIM(VectorUFile)//".grd -I"//TRIM(VectorSpacingC)//     &
                    "= -R"//TRIM(XMinBuf)//"/"//TRIM(XMaxBuf)//"/"//                                &
                    TRIM(YMinBuf)//"/"//TRIM(YMaxBuf)//                                             &
                    " -N0.0")

            CALL SYSTEM(TRIM(Path)//"xyz2grd "//TRIM(TempPath)//TRIM(VectorVFile)//".xyz"//         &
                    " -G"//TRIM(TempPath)//TRIM(VectorVFile)//".grd -I"//TRIM(VectorSpacingC)//     &
                    "= -R"//TRIM(XMinBuf)//"/"//TRIM(XMaxBuf)//"/"//                &
                    TRIM(YMinBuf)//"/"//TRIM(YMaxBuf)//                             &
                    " -N0.0")

            Line = ""
            Line = TRIM(Line)//TRIM(Path)//"grdvector"
            Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(VectorUFile)//".grd"
            Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(VectorVFile)//".grd"
            Line = TRIM(Line)//" "//"-JM"//TRIM(WidthC)//"i"
            Line = TRIM(Line)//" "//"-GBlack"
            Line = TRIM(Line)//" "//"-Q"//TRIM(VectorTailWidthC)//"i/"//TRIM(VectorHeadLengthC)//"i/" &
                             //TRIM(VectorHeadWidthC)//"i"
            Line = TRIM(Line)//" "//"-R"//TRIM(XMin)//"/"//TRIM(XMax)//"/"//TRIM(YMin)//"/"//TRIM(YMax)
            IF((IfPlotFilledContours.EQ.0).AND.(IfPlotGrid.EQ.0).AND.(IfPlotContourLines.EQ.0).AND.(IfPlotBoundaries.EQ.0))THEN
                Line = TRIM(Line)//" "//"-Bp"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)// &
                                   "/s"//TRIM(BorderIncrementMajorC)//"f"//TRIM(BorderIncrementMinorC)//"WeSn"
            ENDIF
            Line = TRIM(Line)//" "//"-S"//TRIM(VectorMagC)//"i"
            Line = TRIM(Line)//" "//"-K"
            IF(IfStarted.EQ.0)THEN
                Line = TRIM(Line)//" "//">"
                IfStarted = 1
            ELSE
                Line = TRIM(Line)//" "//"-O >>"
            ENDIF
            Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
            CALL SYSTEM(TRIM(Line))

            WRITE(UNIT=VectorScaleFile,FMT='(A,I4.4,A)') "vectorscale.",Record,".txt"

            OPEN(UNIT=21,FILE=TRIM(TempPath)//TRIM(VectorScaleFile),ACTION="WRITE")
            WRITE(UNIT=21,FMT='(A,F5.2)') "0.0 0.0 0 ",VectorScaleMag/VectorMag
            CLOSE(UNIT=21,STATUS="KEEP")

            IF(IfAddTimeBar.EQ.0)THEN
                VectorScaleY = Height - 0.3
            ELSE
                VectorScaleY = Height - 1.1
            ENDIF

            IF(VectorScaleY<10.0)THEN
                WRITE(UNIT=VectorScaleYC,FMT='(F4.2)') VectorScaleY
            ELSE
                WRITE(UNIT=VectorScaleYC,FMT='(F5.2)') VectorScaleY
            ENDIF

            CALL SYSTEM(TRIM(Path)//"psxy "//TRIM(TempPath)//TRIM(VectorScaleFile)// &
                    " -Jx1"//                                                        &
                    " -R0/1/0/1 -GBlack -N -O"//                                     &
                    " -Sv"//TRIM(VectorTailWidthC)//"i/"//TRIM(VectorHeadLengthC)//  &
                    "i/"//TRIM(VectorHeadWidthC)//"i -Xa"//TRIM(VectorScaleXC)//     &
                    "i -Ya"//TRIM(VectorScaleYC)//"i -K >> "//                       &
                    TRIM(PlotName)//".ps")

            WRITE(UNIT=VectorTextFile,FMT='(A,I4.4,A)') "vectortext.",Record,".txt"

            OPEN(UNIT=22,FILE=TRIM(TempPath)//TRIM(VectorTextFile),ACTION="WRITE")
            WRITE(UNIT=22,FMT='(A,I3,A)') "0.0 0.0 14 0 0 LT ",                     &
                    NINT(VectorScaleMag)," "//TRIM(VectorUnits)
            CLOSE(UNIT=22,STATUS="KEEP")

            IF(IfAddTimeBar.EQ.0)THEN
                VectorScaleY = Height
            ELSE
                VectorScaleY = Height - 0.8
            ENDIF

            IF(VectorScaleY<10.0)THEN
                WRITE(UNIT=VectorScaleYC,FMT='(F4.2)') VectorScaleY
            ELSE
                WRITE(UNIT=VectorScaleYC,FMT='(F5.2)') VectorScaleY
            ENDIF

            CALL SYSTEM(TRIM(Path)//"pstext "//TRIM(TempPath)//TRIM(VectorTextFile)// &
                    " -Jx1 -K"//                                                      &
                    " -R0/1/0/1 -GBlack -N -O -Xa"//TRIM(SideBarXC)//"i"//            &
                    " -Ya"//TRIM(VectorScaleYC)//"i >> "//                            &
                    TRIM(PlotName)//".ps")

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the vectors for record ",Record,"."
            ENDIF

        ENDIF

        IF(IfAddPlotLabel.EQ.1)THEN

            WRITE(UNIT=PlotLabelFile,FMT='(A,I4.4,A)') "plotlabel.",Record,".txt"

            OPEN(UNIT=28,FILE=TRIM(TempPath)//TRIM(PlotLabelFile),ACTION="WRITE")
            WRITE(UNIT=28,FMT='(A,A)') "0.0 0.0 14 0 0 BC ",TRIM(PlotLabel)
            CLOSE(UNIT=28,STATUS="KEEP")

            CALL SYSTEM(TRIM(Path)//"pstext "//TRIM(TempPath)//TRIM(PlotLabelFile)  &
                        //" -JX1i -R0/8/0/1 -Xa"//TRIM(PlotLabelXAdjustC)           &
                        //"i -Ya"//TRIM(PlotLabelYAdjustC)                          &
                        //"i -K -N -O >>"//Trim(PlotName)//".ps")

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the plot label for record ",Record,"."
            ENDIF

        ENDIF

        IF(IfAddTimeBar.EQ.1)THEN

            WRITE(UNIT=TimeMaxFile,FMT='(A,I4.4,A)') "timemax.",Record,".txt"
            OPEN(UNIT=25,FILE=TRIM(TempPath)//TRIM(TimeMaxFile),ACTION="WRITE")
            WRITE(UNIT=25,FMT='(A)') "0 0 0 1.0"
            CLOSE(UNIT=25,STATUS="KEEP")

            CALL SYSTEM(TRIM(Path)//"psxy "//TRIM(TempPath)//TRIM(TimeMaxFile)      &
                        //" -JX1i -R0/2/0/2 -N -O -Sv0.2i/0.0i/0.01"                &
                        //" -Xa"//TRIM(SideBarXC)//"i"                              &
                        //" -Ya"//TRIM(TimeScaleYC)//"i -K >> "                     &
                        //TRIM(PlotName)//".ps")

            WRITE(UNIT=TimeCurrentFile,FMT='(A,I4.4,A)') "timecurrent.",Record,".txt"
            OPEN(UNIT=26,FILE=TRIM(TempPath)//TRIM(TimeCurrentFile),ACTION="WRITE")
            IF(NumRecs.NE.0)THEN
                WRITE(UNIT=26,FMT='(A,F4.2)') "0 0 0 ",1.0*Record/NumRecs
            ELSE
                WRITE(UNIT=26,FMT='(A)') "0 0 0 0.0"
            ENDIF
            CLOSE(UNIT=26,STATUS="KEEP")

            CALL SYSTEM(TRIM(Path)//"psxy "//TRIM(TempPath)//TRIM(TimeCurrentFile)  &
                        //" -JX1i -R0/2/0/2 -GBlack -N -O -Sv0.2i/0.0i/0.0i"        &
                        //" -Xa"//TRIM(SideBarXC)//"i"                              &
                        //" -Ya"//TRIM(TimeScaleYC)//"i -K >> "                     &
                        //TRIM(PlotName)//".ps")

            WRITE(UNIT=TimeCurrentTextFile,FMT='(A,I4.4,A)') "timecurrenttext.",Record,".txt"
            OPEN(UNIT=27,FILE=TRIM(TempPath)//TRIM(TimeCurrentTextFile),ACTION="WRITE")
            WRITE(UNIT=27,FMT='(A,F5.2,A)') "0 0 14 0 0 LT ",CurrentTime/86400.0," days"
            CLOSE(UNIT=27,STATUS="KEEP")

            CALL SYSTEM(TRIM(Path)//"pstext "//TRIM(TempPath)                       &
                        //TRIM(TimeCurrentTextFile)//" -JX1i -R0/2/0/2"             &
                        //" -Xa"//TRIM(SideBarXC)//"i"                              &
                        //" -Ya"//TRIM(TimeScaleTextYC)//"i -K -N -O >> "           &
                        //TRIM(PlotName)//".ps")

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the time bar for record ",Record,"."
            ENDIF

        ENDIF
        CALL SYSTEM(TRIM(Path)//"psxy track_point.dat -: -JM"//TRIM(WidthC)//"i -R"//TRIM(XMin)//"/" &
                    //TRIM(XMax)//"/"//TRIM(YMin)//"/"//TRIM(YMax)              &
                    //" -O -K -P -G0 -Skhurricane  >> "//TRIM(PlotName)//".ps")

        CALL SYSTEM(TRIM(Path)//"psxy track_line.dat -: -JM"//TRIM(WidthC)//"i -R"//TRIM(XMin)//"/" &
                   //TRIM(XMax)//"/"//TRIM(YMin)//"/"//TRIM(YMax)              &
                   //" -O -K -P -W5.0  >> "//TRIM(PlotName)//".ps")


! generate jpg from ps using convert
!          write(*,*) "first make a large high res image"
! first make a large high res image
        CALL SYSTEM("/usr/bin/convert -rotate 90 -trim -border 100 -bordercolor white"//     &
                " -density "//TRIM(LargeJPGResolutionC)//" "//                      &
                TRIM(PlotName)//".ps "//TRIM(PlotName)//"full.jpg")

!          write(*,*) "add ADCIRC image"
! now drop in the adcirc logo in the upper left hand corner
         CALL SYSTEM("/usr/bin/convert -draw 'image Over 360,339 0,0 adcirc_logo_white.jpg' "// &
            TRIM(PlotName)//"full.jpg "//TRIM(PlotName)//"full.jpg")
!
!          write(*,*) "now resize thew image to a smaller jpg"
!now resize thew image to a smaller jpg
        CALL SYSTEM("/usr/bin/convert -resize "// &
                TRIM(SmallJPGWidthC)//" "//TRIM(PlotName)// &
                "full.jpg "// TRIM(PlotName)//".jpg")

!and create the png
!        CALL SYSTEM("/usr/bin/convert -transparent White -resize "// &
!                TRIM(SmallJPGWidthC)//" "//TRIM(PlotName)// &
!                ".jpg "// TRIM(PlotName)//"s.png")

END SUBROUTINE



SUBROUTINE WriteXYZFiles(Record)

        USE DATA
        
        IMPLICIT NONE

        INTRINSIC                                :: INDEX
        INTRINSIC                                :: SQRT
        INTRINSIC                                :: TRIM

        CHARACTER(LEN=50)                        :: AttributeLabel
        CHARACTER(LEN=1)                         :: JunkC

        INTEGER, DIMENSION(:), ALLOCATABLE       :: Count
        INTEGER                                  :: I
        INTEGER                                  :: J
        INTEGER                                  :: JunkI
        INTEGER, DIMENSION(:,:), ALLOCATABLE     :: NC
        INTEGER                                  :: NumAttributes1
        INTEGER                                  :: NumAttributes2
        INTEGER                                  :: NumElems
        INTEGER                                  :: NumNodes1
        INTEGER                                  :: NumNodes2
        INTEGER                                  :: NumNonDefault
        INTEGER                                  :: Record

        REAL(8), DIMENSION(:), ALLOCATABLE       :: Attributes1
        REAL(8), DIMENSION(:), ALLOCATABLE       :: Attributes2
        REAL(8)                                  :: AttributeDefault
        REAL(8), DIMENSION(:), ALLOCATABLE       :: AttrWR
        REAL(8), DIMENSION(:), ALLOCATABLE       :: Bath1
        REAL(8), DIMENSION(:), ALLOCATABLE       :: Bath2
        REAL(8)                                  :: ComputeDistance
        REAL(8)                                  :: DefaultValue
        REAL(8)                                  :: Dist
        REAL(8)                                  :: JunkR
        REAL(8), DIMENSION(:), ALLOCATABLE       :: Lat
        REAL(8), DIMENSION(:), ALLOCATABLE       :: Lon
        REAL(8), DIMENSION(:), ALLOCATABLE       :: Sizes
        REAL(8), DIMENSION(:), ALLOCATABLE       :: U1
        REAL(8), DIMENSION(:), ALLOCATABLE       :: U2
        REAL(8), DIMENSION(:), ALLOCATABLE       :: V1
        REAL(8), DIMENSION(:), ALLOCATABLE       :: V2
        REAL(8), DIMENSION(:), ALLOCATABLE       :: Vels1
        REAL(8), DIMENSION(:), ALLOCATABLE       :: Vels2

        IF((IfPlotFilledContours.GE.1).OR.(IfPlotContourLines.GE.1))THEN

            IF((IfPlotFilledContours.EQ.1).OR.(IfPlotContourLines.EQ.1))THEN

                IF((TRIM(ContourFileFormat).EQ."OUTPUT-FULL").OR.(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE"))THEN

                    WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"

                    OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                    OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                    READ(UNIT=19,FMT='(A)') JunkC
                    READ(UNIT=19,FMT=*) NumRecs, JunkI, JunkR, JunkI, ContourFileType

                    IF(Record.GT.1)THEN
                    
                        DO J=1,Record-1

                            IF(TRIM(ContourFileFormat).EQ."OUTPUT-FULL")THEN

                                READ(UNIT=19,FMT=*) JunkR, CurrentTime
                                NumNodes1 = NumNodesGlobal
                                DefaultValue = 0.0

                            ELSEIF(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE")THEN
                    
                                READ(UNIT=19,FMT=*) JunkR, CurrentTime, NumNodes1, DefaultValue

                            ENDIF

                            DO I=1,NumNodes1

                                READ(UNIT=19,FMT=*) JunkI

                            ENDDO

                        ENDDO

                    ENDIF

                    IF(TRIM(ContourFileFormat).EQ."OUTPUT-FULL")THEN

                        READ(UNIT=19,FMT=*) JunkR, CurrentTime
                        NumNodes1 = NumNodesGlobal
                        DefaultValue = 0.0

                    ELSEIF(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE")THEN
            
                        READ(UNIT=19,FMT=*) JunkR, CurrentTime, NumNodes1, DefaultValue

                    ENDIF

                    IF(INDEX(TRIM(ContourFile1),"64").GT.0)THEN

                        DefaultValue = -99999.0

                    ENDIF

                    IF(DefaultValue.GT.-99998.0)THEN

                        DefaultValue = DefaultValue * ContourConversionFactor

                    ENDIF

                    ALLOCATE(U1(1:NumNodesGlobal))
                    ALLOCATE(V1(1:NumNodesGlobal))
                    ALLOCATE(Vels1(1:NumNodesGlobal))

                    DO I=1,NumNodesGlobal

                        U1(I) = DefaultValue
                        V1(I) = DefaultValue
                        Vels1(I) = DefaultValue

                    ENDDO

                    DO I=1,NumNodes1

                        IF(ContourFileType.EQ.1)THEN

                            READ(UNIT=19,FMT=*) JunkI, U1(JunkI)

                            IF(U1(JunkI).GT.-99998.0)THEN

                                U1(JunkI) = U1(JunkI) * ContourConversionFactor

                            ENDIF

                            Vels1(JunkI) = U1(JunkI)

                        ELSEIF(ContourFileType.EQ.2)THEN
                
                            READ(UNIT=19,FMT=*) JunkI, U1(JunkI), V1(JunkI)

                            U1(JunkI) = U1(JunkI) * ContourConversionFactor
                            V1(JunkI) = V1(JunkI) * ContourConversionFactor

                            Vels1(JunkI) = SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))

                        ENDIF

                    ENDDO

                    DO I=1,NumNodesLocal

                        Z(I) = Vels1(XYZNodes(I))

                        WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                    ENDDO

                    IF(ALLOCATED(U1)) DEALLOCATE(U1)
                    IF(ALLOCATED(V1)) DEALLOCATE(V1)
                    IF(ALLOCATED(Vels1)) DEALLOCATE(Vels1)

                    CLOSE(UNIT=12,STATUS="KEEP")
                    CLOSE(UNIT=19,STATUS="KEEP")

                ELSEIF((TRIM(ContourFileFormat).EQ."GRID-BATH").OR.(TRIM(ContourFileFormat).EQ."GRID-SIZE"))THEN

                    WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"

                    OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                    OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")

                    READ(UNIT=19,FMT='(A)') JunkC
                    READ(UNIT=19,FMT=*) NumElems, NumNodes1

                    IF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN
                        ALLOCATE(Bath1(1:NumNodes1))
                    ELSEIF(TRIM(ContourFileFormat).EQ."GRID-SIZE")THEN
                        ALLOCATE(Lat(1:NumNodes1))
                        ALLOCATE(Lon(1:NumNodes1))
                    ENDIF

                    DO I=1,NumNodes1

                        IF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN
                            READ(UNIT=19,FMT=*) JunkI, JunkR, JunkR, Bath1(I)
                        ELSEIF(TRIM(ContourFileFormat).EQ."GRID-SIZE")THEN
                            READ(UNIT=19,FMT=*) JunkI, Lon(I), Lat(I), JunkR
                        ENDIF

                    ENDDO

                    IF(TRIM(ContourFileFormat).EQ."GRID-SIZE")THEN

                        ALLOCATE(NC(1:NumElems,1:3))
                        ALLOCATE(Sizes(1:NumNodes1))
                        ALLOCATE(Count(1:NumNodes1))

                        DO I=1,NumNodes1
                            Sizes(I) = 0.D0
                            Count(I) = 0
                        ENDDO

                        DO I=1,NumElems

                            READ(UNIT=19,FMT=*) JunkI, JunkI, NC(I,1), NC(I,2), NC(I,3)

                            Dist = ComputeDistance(Lon(NC(I,1)),Lat(NC(I,1)),Lon(NC(I,2)),Lat(NC(I,2)))
                            Sizes(NC(I,1)) = Sizes(NC(I,1)) + Dist
                            Sizes(NC(I,2)) = Sizes(NC(I,2)) + Dist
                            Count(NC(I,1)) = Count(NC(I,1)) + 1
                            Count(NC(I,2)) = Count(NC(I,2)) + 1

                            Dist = ComputeDistance(Lon(NC(I,2)),Lat(NC(I,2)),Lon(NC(I,3)),Lat(NC(I,3)))
                            Sizes(NC(I,2)) = Sizes(NC(I,2)) + Dist
                            Sizes(NC(I,3)) = Sizes(NC(I,3)) + Dist
                            Count(NC(I,2)) = Count(NC(I,2)) + 1
                            Count(NC(I,3)) = Count(NC(I,3)) + 1

                            Dist = ComputeDistance(Lon(NC(I,3)),Lat(NC(I,3)),Lon(NC(I,1)),Lat(NC(I,1)))
                            Sizes(NC(I,3)) = Sizes(NC(I,3)) + Dist
                            Sizes(NC(I,1)) = Sizes(NC(I,1)) + Dist
                            Count(NC(I,3)) = Count(NC(I,3)) + 1
                            Count(NC(I,1)) = Count(NC(I,1)) + 1

                        ENDDO

                    ENDIF

                    DO I=1,NumNodesLocal

                        IF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN
                            Z(I) = Bath1(XYZNodes(I)) * ContourConversionFactor
                        ELSEIF(TRIM(ContourFileFormat).EQ."GRID-SIZE")THEN
                            Z(I) = (Sizes(XYZNodes(I)) / Count(XYZNodes(I))) * ContourConversionFactor
                        ENDIF

                        WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                    ENDDO

                    IF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN
                        IF(ALLOCATED(Bath1)) DEALLOCATE(Bath1)
                    ELSEIF(TRIM(ContourFileFormat).EQ."GRID-SIZE")THEN
                        IF(ALLOCATED(Lat)) DEALLOCATE(Lat)
                        IF(ALLOCATED(Lon)) DEALLOCATE(Lon)
                        IF(ALLOCATED(NC)) DEALLOCATE(NC)
                        IF(ALLOCATED(Sizes)) DEALLOCATE(Sizes)
                        IF(ALLOCATED(Count)) DEALLOCATE(Count)
                    ENDIF

                    CLOSE(UNIT=12,STATUS="KEEP")
                    CLOSE(UNIT=19,STATUS="KEEP")

                ELSEIF((TRIM(ContourFileFormat).EQ."13-MANNING").OR.(INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0).OR. &
                       (TRIM(ContourFileFormat).EQ."13-CANOPY"))THEN

                    IF(INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0)THEN
                        ALLOCATE(AttrWR(1:12))
                    ENDIF

                    WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"

                    OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                    OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")

                    READ(UNIT=19,FMT='(A)') JunkC
                    READ(UNIT=19,FMT=*) NumNodes1
                    READ(UNIT=19,FMT=*) NumAttributes1

                    DO I=1,NumAttributes1

                        READ(UNIT=19,FMT='(A)') AttributeLabel

                        IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-MANNING"))THEN

                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) AttributeDefault 

                        ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-CANOPY"))THEN

                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) AttributeDefault 

                        ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                               (INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0))THEN

                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), &
                                                AttrWR(5), AttrWR(6), AttrWR(7), AttrWR(8), &
                                                AttrWR(9), AttrWR(10), AttrWR(11), AttrWR(12)

                            AttributeDefault = AttrWR(Record)

                        ELSE

                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC

                        ENDIF

                    ENDDO

                    ALLOCATE(Attributes1(1:NumNodes1))
                    DO I=1,NumNodes1
                        Attributes1(I) = AttributeDefault
                    ENDDO

                    DO I=1,NumAttributes1

                        READ(UNIT=19,FMT='(A)') AttributeLabel

                        IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-MANNING"))THEN

                            READ(UNIT=19,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=19,FMT=*) JunkI, JunkR
                                Attributes1(JunkI) = JunkR

                            ENDDO

                        ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-CANOPY"))THEN

                            READ(UNIT=19,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=19,FMT=*) JunkI, JunkR
                                Attributes1(JunkI) = JunkR

                            ENDDO

                        ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                               (INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0))THEN

                            READ(UNIT=19,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=19,FMT=*) JunkI, AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), AttrWR(5), AttrWR(6), &
                                                    AttrWR(7), AttrWR(8), AttrWR(9), AttrWR(10), AttrWR(11), AttrWR(12)
                                Attributes1(JunkI) = AttrWR(Record)

                            ENDDO

                        ELSE

                            READ(UNIT=19,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=19,FMT='(A)') JunkC

                            ENDDO

                        ENDIF

                    ENDDO

                    DO I=1,NumNodesLocal

                        Z(I) = Attributes1(XYZNodes(I)) * ContourConversionFactor
                        WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                    ENDDO

                    IF(ALLOCATED(Attributes1)) DEALLOCATE(Attributes1)                    

                    IF(INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0)THEN
                        IF(ALLOCATED(AttrWR)) DEALLOCATE(AttrWR)
                    ENDIF

                    CLOSE(UNIT=12,STATUS="KEEP")
                    CLOSE(UNIT=19,STATUS="KEEP")

                ENDIF

            ELSEIF((IfPlotFilledContours.EQ.2).OR.(IfPlotContourLines.EQ.2))THEN

                IF((TRIM(ContourFileFormat).EQ."OUTPUT-FULL").OR.(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE"))THEN

                    WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"

                    OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                    OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                    READ(UNIT=19,FMT='(A)') JunkC
                    READ(UNIT=19,FMT=*) NumRecs, JunkI, JunkR, JunkI, ContourFileType
                    OPEN(UNIT=23,FILE=TRIM(ContourFile2),ACTION="READ")
                    READ(UNIT=23,FMT='(A)') JunkC
                    READ(UNIT=23,FMT=*) NumRecs, JunkI, JunkR, JunkI, ContourFileType

                    IF(Record.GT.1)THEN
                    
                        DO J=1,Record-1

                            IF(TRIM(ContourFileFormat).EQ."OUTPUT-FULL")THEN

                                READ(UNIT=19,FMT=*) JunkR, CurrentTime
                                READ(UNIT=23,FMT=*) JunkR, CurrentTime
                                NumNodes1 = NumNodesGlobal
                                DefaultValue = 0.0

                            ELSEIF(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE")THEN
                    
                                READ(UNIT=19,FMT=*) JunkR, CurrentTime, NumNodes1, DefaultValue
                                READ(UNIT=23,FMT=*) JunkR, CurrentTime, NumNodes2, DefaultValue

                            ENDIF

                            DO I=1,NumNodes1

                                READ(UNIT=19,FMT=*) JunkI

                            ENDDO

                            DO I=1,NumNodes2

                                READ(UNIT=23,FMT=*) JunkI

                            ENDDO

                        ENDDO

                    ENDIF

                    IF(TRIM(ContourFileFormat).EQ."OUTPUT-FULL")THEN

                        READ(UNIT=19,FMT=*) JunkR, CurrentTime
                        READ(UNIT=23,FMT=*) JunkR, CurrentTime
                        NumNodes1 = NumNodesGlobal
                        NumNodes2 = NumNodesGlobal
                        DefaultValue = 0.0

                    ELSEIF(TRIM(ContourFileFormat).EQ."OUTPUT-SPARSE")THEN

                        READ(UNIT=19,FMT=*) JunkR, CurrentTime, NumNodes1, DefaultValue
                        READ(UNIT=23,FMT=*) JunkR, CurrentTime, NumNodes2, DefaultValue

                    ENDIF

                    IF(INDEX(TRIM(ContourFile1),"64").GT.0)THEN

                        DefaultValue = -99999.0

                    ENDIF

                    IF(DefaultValue.GT.-99998.0)THEN

                        DefaultValue = DefaultValue * ContourConversionFactor

                    ENDIF

                    ALLOCATE(U1(1:NumNodesGlobal))
                    ALLOCATE(U2(1:NumNodesGlobal))
                    ALLOCATE(V1(1:NumNodesGlobal))
                    ALLOCATE(V2(1:NumNodesGlobal))
                    ALLOCATE(Vels1(1:NumNodesGlobal))
                    ALLOCATE(Vels2(1:NumNodesGlobal))

                    DO I=1,NumNodesGlobal

                        U1(I) = DefaultValue
                        U2(I) = DefaultValue
                        V1(I) = DefaultValue
                        V2(I) = DefaultValue
                        Vels1(I) = DefaultValue
                        Vels2(I) = DefaultValue

                    ENDDO

                    DO I=1,NumNodes1

                        IF(ContourFileType.EQ.1)THEN

                            READ(UNIT=19,FMT=*) JunkI, U1(JunkI)

                            IF(U1(JunkI).GT.-99998.0)THEN

                                U1(JunkI) = U1(JunkI) * ContourConversionFactor

                            ENDIF

                            Vels1(JunkI) = U1(JunkI)

                        ELSEIF(ContourFileType.EQ.2)THEN
                
                            READ(UNIT=19,FMT=*) JunkI, U1(JunkI), V1(JunkI)

                            U1(JunkI) = U1(JunkI) * ContourConversionFactor
                            V1(JunkI) = V1(JunkI) * ContourConversionFactor

                            Vels1(JunkI) = SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))

                        ENDIF

                    ENDDO

                    DO I=1,NumNodes2

                        IF(ContourFileType.EQ.1)THEN

                            READ(UNIT=23,FMT=*) JunkI, U2(JunkI)

                            IF(U2(JunkI).GT.-99998.0)THEN

                                U2(JunkI) = U2(JunkI) * ContourConversionFactor

                            ENDIF

                            Vels2(JunkI) = U2(JunkI)

                        ELSEIF(ContourFileType.EQ.2)THEN
                
                            READ(UNIT=23,FMT=*) JunkI, U2(JunkI), V2(JunkI)

                            U2(JunkI) = U2(JunkI) * ContourConversionFactor
                            V2(JunkI) = V2(JunkI) * ContourConversionFactor

                            Vels2(JunkI) = SQRT(U2(JunkI)*U2(JunkI)+V2(JunkI)*V2(JunkI))

                        ENDIF

                    ENDDO

                    DO I=1,NumNodesLocal

                        Z(I) = Vels1(XYZNodes(I)) - Vels2(XYZNodes(I))

                        WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                    ENDDO

                    IF(ALLOCATED(U1)) DEALLOCATE(U1)
                    IF(ALLOCATED(U2)) DEALLOCATE(U2)
                    IF(ALLOCATED(V1)) DEALLOCATE(V1)
                    IF(ALLOCATED(V2)) DEALLOCATE(V2)
                    IF(ALLOCATED(Vels1)) DEALLOCATE(Vels1)
                    IF(ALLOCATED(Vels2)) DEALLOCATE(Vels2)

                    CLOSE(UNIT=12,STATUS="KEEP")
                    CLOSE(UNIT=19,STATUS="KEEP")
                    CLOSE(UNIT=23,STATUS="KEEP")

                ELSEIF(TRIM(ContourFileFormat).EQ."GRID-BATH")THEN

                    WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"

                    OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                    OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                    OPEN(UNIT=23,FILE=TRIM(ContourFile2),ACTION="READ")

                    READ(UNIT=19,FMT='(A)') JunkC
                    READ(UNIT=19,FMT=*) JunkI, NumNodes1
                    READ(UNIT=23,FMT='(A)') JunkC
                    READ(UNIT=23,FMT=*) JunkI, NumNodes2

                    IF(NumNodes1.NE.NumNodes1)THEN
                        STOP "FATAL ERROR: The number of nodes should be the same in the two grids."
                    ENDIF

                    ALLOCATE(Bath1(1:NumNodes1))
                    ALLOCATE(Bath2(1:NumNodes2))

                    DO I=1,NumNodes1

                        READ(UNIT=19,FMT=*) JunkI, JunkR, JunkR, Bath1(I)
                        READ(UNIT=23,FMT=*) JunkI, JunkR, JunkR, Bath2(I)

                    ENDDO

                    DO I=1,NumNodesLocal

                        Z(I) = (Bath1(XYZNodes(I)) - Bath2(XYZNodes(I))) * ContourConversionFactor

                        WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                    ENDDO

                    IF(ALLOCATED(Bath1)) DEALLOCATE(Bath1)
                    IF(ALLOCATED(Bath2)) DEALLOCATE(Bath2)

                    CLOSE(UNIT=12,STATUS="KEEP")
                    CLOSE(UNIT=19,STATUS="KEEP")
                    CLOSE(UNIT=23,STATUS="KEEP")

                ELSEIF((TRIM(ContourFileFormat).EQ."13-MANNING").OR.(INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0).OR. &
                       (TRIM(ContourFileFormat).EQ."13-CANOPY"))THEN

                    IF(INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0)THEN
                        ALLOCATE(AttrWR(1:12))
                    ENDIF

                    WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"

                    OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                    OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                    OPEN(UNIT=23,FILE=TRIM(ContourFile2),ACTION="READ")

                    READ(UNIT=19,FMT='(A)') JunkC
                    READ(UNIT=19,FMT=*) NumNodes1
                    READ(UNIT=19,FMT=*) NumAttributes1
                    READ(UNIT=23,FMT='(A)') JunkC
                    READ(UNIT=23,FMT=*) NumNodes2
                    READ(UNIT=23,FMT=*) NumAttributes2

                    IF(NumNodes1.NE.NumNodes2)THEN
                        WRITE(*,'(A)') "FATAL ERROR: The 13 files do not have the same number of nodes."
                        STOP
                    ENDIF

                    DO I=1,NumAttributes1

                        READ(UNIT=19,FMT='(A)') AttributeLabel

                        IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-MANNING"))THEN

                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) AttributeDefault

                        ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-CANOPY"))THEN

                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) AttributeDefault

                        ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                               (INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0))THEN

                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), &
                                                AttrWR(5), AttrWR(6), AttrWR(7), AttrWR(8), &
                                                AttrWR(9), AttrWR(10), AttrWR(11), AttrWR(12)

                            AttributeDefault = AttrWR(Record)

                        ELSE

                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT='(A)') JunkC

                        ENDIF

                    ENDDO

                    ALLOCATE(Attributes1(1:NumNodes1))
                    DO I=1,NumNodes1
                        Attributes1(I) = AttributeDefault
                    ENDDO

                    DO I=1,NumAttributes2

                        READ(UNIT=23,FMT='(A)') AttributeLabel

                        IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-MANNING"))THEN

                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT=*) AttributeDefault

                        ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-CANOPY"))THEN

                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT=*) AttributeDefault 

                        ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                               (INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0))THEN

                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT=*) AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), &
                                                AttrWR(5), AttrWR(6), AttrWR(7), AttrWR(8), &
                                                AttrWR(9), AttrWR(10), AttrWR(11), AttrWR(12)

                            AttributeDefault = AttrWR(Record)

                        ELSE

                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT='(A)') JunkC

                        ENDIF

                    ENDDO

                    ALLOCATE(Attributes2(1:NumNodes2))
                    DO I=1,NumNodes2
                        Attributes2(I) = AttributeDefault
                    ENDDO

                    DO I=1,NumAttributes1

                        READ(UNIT=19,FMT='(A)') AttributeLabel

                        IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-MANNING"))THEN

                            READ(UNIT=19,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=19,FMT=*) JunkI, JunkR
                                Attributes1(JunkI) = JunkR

                            ENDDO

                        ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-CANOPY"))THEN

                            READ(UNIT=19,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=19,FMT=*) JunkI, JunkR
                                Attributes1(JunkI) = JunkR

                            ENDDO

                        ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                               (INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0))THEN

                            READ(UNIT=19,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=19,FMT=*) JunkI, AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), AttrWR(5), AttrWR(6), &
                                                    AttrWR(7), AttrWR(8), AttrWR(9), AttrWR(10), AttrWR(11), AttrWR(12)
                                Attributes1(JunkI) = AttrWR(Record)

                            ENDDO

                        ELSE

                            READ(UNIT=19,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=19,FMT='(A)') JunkC

                            ENDDO

                        ENDIF

                    ENDDO

                    DO I=1,NumAttributes2

                        READ(UNIT=23,FMT='(A)') AttributeLabel

                        IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-MANNING"))THEN

                            READ(UNIT=23,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=23,FMT=*) JunkI, JunkR
                                Attributes2(JunkI) = JunkR

                            ENDDO

                        ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileFormat).EQ."13-CANOPY"))THEN

                            READ(UNIT=23,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=23,FMT=*) JunkI, JunkR
                                Attributes2(JunkI) = JunkR

                            ENDDO

                        ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                               (INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0))THEN

                            READ(UNIT=23,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=23,FMT=*) JunkI, AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), AttrWR(5), AttrWR(6), &
                                                    AttrWR(7), AttrWR(8), AttrWR(9), AttrWR(10), AttrWR(11), AttrWR(12)
                                Attributes2(JunkI) = AttrWR(Record)

                            ENDDO

                        ELSE

                            READ(UNIT=23,FMT=*) NumNonDefault

                            DO J=1,NumNonDefault

                                READ(UNIT=23,FMT='(A)') JunkC

                            ENDDO

                        ENDIF

                    ENDDO

                    DO I=1,NumNodesLocal

                        Z(I) = (Attributes1(XYZNodes(I))-Attributes2(XYZNodes(I))) * ContourConversionFactor
                        WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                    ENDDO

                    IF(ALLOCATED(Attributes1)) DEALLOCATE(Attributes1)
                    IF(ALLOCATED(Attributes2)) DEALLOCATE(Attributes2)

                    IF(INDEX(ContourFileFormat,"13-WIND-REDUCTION").GT.0)THEN
                        IF(ALLOCATED(AttrWR)) DEALLOCATE(AttrWR)
                    ENDIF

                    CLOSE(UNIT=12,STATUS="KEEP")
                    CLOSE(UNIT=19,STATUS="KEEP")
                    CLOSE(UNIT=23,STATUS="KEEP")

                ENDIF

            ENDIF

        ENDIF

        IF(IfPlotVectors.EQ.1)THEN

            WRITE(UNIT=VectorUFile,FMT='(A,A,I4.4,A)') TRIM(VectorFile), ".", Record, ".u"
            WRITE(UNIT=VectorVFile,FMT='(A,A,I4.4,A)') TRIM(VectorFile), ".", Record, ".v"

            OPEN(UNIT=17,FILE=TRIM(TempPath)//TRIM(VectorUFile)//".xyz",ACTION="WRITE")
            OPEN(UNIT=18,FILE=TRIM(TempPath)//TRIM(VectorVFile)//".xyz",ACTION="WRITE")
            OPEN(UNIT=20,FILE=TRIM(VectorFile),ACTION="READ")
            READ(UNIT=20,FMT='(A)') JunkC
            READ(UNIT=20,FMT=*) NumRecs, JunkI, JunkR, JunkI, VectorFileType

            IF(Record.GT.1)THEN
                    
                DO J=1,Record-1

                    IF(TRIM(VectorFileFormat).EQ."OUTPUT-FULL")THEN

                        READ(UNIT=20,FMT=*) JunkR, JunkI
                        NumNodes1 = NumNodesGlobal
                        DefaultValue = 0.0

                    ELSEIF(TRIM(VectorFileFormat).EQ."OUTPUT-SPARSE")THEN
                    
                        READ(UNIT=20,FMT=*) JunkR, JunkI, NumNodes1, DefaultValue

                    ENDIF

                    DO I=1,NumNodes1

                        READ(UNIT=20,FMT=*) JunkI

                    ENDDO

                ENDDO

            ENDIF

            IF(TRIM(VectorFileFormat).EQ."OUTPUT-FULL")THEN

                READ(UNIT=20,FMT=*) JunkR, JunkI
                NumNodes1 = NumNodesGlobal
                DefaultValue = 0.0

            ELSEIF(TRIM(VectorFileFormat).EQ."OUTPUT-SPARSE")THEN
            
                READ(UNIT=20,FMT=*) JunkR, JunkI, NumNodes1, DefaultValue

            ENDIF

            DefaultValue = DefaultValue * VectorConversionFactor

            ALLOCATE(U1(1:NumNodesGlobal))
            ALLOCATE(V1(1:NumNodesGlobal))
            ALLOCATE(Vels1(1:NumNodesGlobal))

            DO I=1,NumNodesGlobal

                U1(I) = DefaultValue
                V1(I) = DefaultValue
                Vels1(I) = DefaultValue

            ENDDO

            DO I=1,NumNodes1

                READ(UNIT=20,FMT=*) JunkI, U1(JunkI), V1(JunkI)

                U1(JunkI) = U1(JunkI) * VectorConversionFactor
                V1(JunkI) = V1(JunkI) * VectorConversionFactor

                Vels1(JunkI) = SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))

            ENDDO

            DO I=1,NumNodesLocal

                WRITE(UNIT=17,FMT='(3(2X,F16.8))') X(I), Y(I), U1(XYZNodes(I))
                WRITE(UNIT=18,FMT='(3(2X,F16.8))') X(I), Y(I), V1(XYZNodes(I))

            ENDDO

            IF(ALLOCATED(U1)) DEALLOCATE(U1)
            IF(ALLOCATED(V1)) DEALLOCATE(V1)
            IF(ALLOCATED(Vels1)) DEALLOCATE(Vels1)

            CLOSE(UNIT=17,STATUS="KEEP")
            CLOSE(UNIT=18,STATUS="KEEP")
            CLOSE(UNIT=20,STATUS="KEEP")

        ENDIF

        IF(Verbose.GE.3)THEN
            WRITE(*,'(A,I4.4,A,I4.4,A)') "Processor ",MyRank," wrote the XYZ files for record ",Record,"."
        ENDIF

END SUBROUTINE 



SUBROUTINE Finisher(Flag)

        USE DATA

        IMPLICIT NONE

        INTEGER :: Flag

#ifdef CMPI
        IF(MyRank.EQ.0)THEN
#endif

            IF(Flag.EQ.0)THEN

               OPEN(UNIT=13,FILE=TRIM(TempPath)//TRIM(Fort14File)//".tri",ACTION="WRITE")
               CLOSE(UNIT=13,STATUS="DELETE")

               OPEN(UNIT=15,FILE=TRIM(TempPath)//"ContourPalette.cpt",ACTION="WRITE")
               CLOSE(UNIT=15,STATUS="DELETE")

               OPEN(UNIT=15,FILE=TRIM(TempPath)//"LabelPalette.cpt",ACTION="WRITE")
               CLOSE(UNIT=15,STATUS="DELETE")

               OPEN(UNIT=15,FILE=TRIM(TempPath)//"ScalePalette.cpt",ACTION="WRITE")
               CLOSE(UNIT=15,STATUS="DELETE")

               OPEN(UNIT=16,FILE=TRIM(TempPath)//TRIM(Fort14File)//".bnd.xy",ACTION="WRITE")
               CLOSE(UNIT=16,STATUS="DELETE")

                IF(IfPlotGrid.EQ.1)THEN

                   OPEN(UNIT=31,FILE=TRIM(TempPath)//TRIM(Fort14File)//".edges.xy",ACTION="WRITE")
                   CLOSE(UNIT=31,STATUS="DELETE")

                ENDIF

#ifdef CMPI
                IF(ALLOCATED(RecordsList))  DEALLOCATE(RecordsList)
#endif
                IF(ALLOCATED(XYZNodes))     DEALLOCATE(XYZNodes)
                IF(ALLOCATED(X))            DEALLOCATE(X)
                IF(ALLOCATED(Y))            DEALLOCATE(Y)
                IF(ALLOCATED(Z))            DEALLOCATE(Z)

            ENDIF

#ifdef CMPI
        ELSEIF(MyRank.NE.0)THEN
#endif

            IF(Flag.EQ.1)THEN

                IF((IfPlotFilledContours.GE.1).OR.(IfPlotContourLines.GE.1))THEN

                   OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                   CLOSE(UNIT=12,STATUS="DELETE")

                ENDIF

                IF(IfPlotLabels.EQ.1)THEN

                   OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                   CLOSE(UNIT=33,STATUS="DELETE")

                ENDIF

                IF(IfPlotVectors.EQ.1)THEN

                   OPEN(UNIT=17,FILE=TRIM(TempPath)//TRIM(VectorUFile)//".xyz",ACTION="WRITE")
                   CLOSE(UNIT=17,STATUS="DELETE")

                   OPEN(UNIT=17,FILE=TRIM(TempPath)//TRIM(VectorUFile)//".grd",ACTION="WRITE")
                   CLOSE(UNIT=17,STATUS="DELETE")

                   OPEN(UNIT=18,FILE=TRIM(TempPath)//TRIM(VectorVFile)//".xyz",ACTION="WRITE")
                   CLOSE(UNIT=18,STATUS="DELETE")

                   OPEN(UNIT=18,FILE=TRIM(TempPath)//TRIM(VectorVFile)//".grd",ACTION="WRITE")
                   CLOSE(UNIT=18,STATUS="DELETE")

                   OPEN(UNIT=21,FILE=TRIM(TempPath)//TRIM(VectorScaleFile),ACTION="WRITE")
                   CLOSE(UNIT=21,STATUS="DELETE")

                   OPEN(UNIT=22,FILE=TRIM(TempPath)//TRIM(VectorTextFile),ACTION="WRITE")
                   CLOSE(UNIT=22,STATUS="DELETE")

                ENDIF

                IF(IfAddPlotLabel.EQ.1)THEN

                   OPEN(UNIT=28,FILE=TRIM(TempPath)//TRIM(PlotLabelFile),ACTION="WRITE")
                   CLOSE(UNIT=28,STATUS="DELETE")

                   OPEN(UNIT=29,FILE=TRIM(TempMapFile1),ACTION="WRITE")
                   CLOSE(UNIT=29,STATUS="DELETE")

                   OPEN(UNIT=30,FILE=TRIM(TempMapFile2),ACTION="WRITE")
                   CLOSE(UNIT=30,STATUS="DELETE")

               ENDIF

                IF(IfAddTimeBar.EQ.1)THEN

                   OPEN(UNIT=25,FILE=TRIM(TempPath)//TRIM(TimeMaxFile),ACTION="WRITE")
                   CLOSE(UNIT=25,STATUS="DELETE")

                   OPEN(UNIT=26,FILE=TRIM(TempPath)//TRIM(TimeCurrentFile),ACTION="WRITE")
                   CLOSE(UNIT=26,STATUS="DELETE")

                   OPEN(UNIT=27,FILE=TRIM(TempPath)//TRIM(TimeCurrentTextFile),ACTION="WRITE")
                   CLOSE(UNIT=27,STATUS="DELETE")

                ENDIF

            ENDIF

#ifdef CMPI
        ELSEIF(MyRank.NE.0)THEN
#endif

            IF(Flag.EQ.0)THEN

#ifdef CMPI
                IF(ALLOCATED(RecordsList))  DEALLOCATE(RecordsList)
#endif
                IF(ALLOCATED(XYZNodes))     DEALLOCATE(XYZNodes)
                IF(ALLOCATED(X))            DEALLOCATE(X)
                IF(ALLOCATED(Y))            DEALLOCATE(Y)
                IF(ALLOCATED(Z))            DEALLOCATE(Z)

            ENDIF

#ifdef CMPI
        ENDIF
#endif

END SUBROUTINE




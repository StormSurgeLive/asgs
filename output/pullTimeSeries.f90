PROGRAM PullTimeSeries

      IMPLICIT NONE

      INTRINSIC           :: ABS
      INTRINSIC           :: TRIM

      CHARACTER(LEN=100)  :: GridFile
      CHARACTER(LEN=100)  :: GlobalFile
      CHARACTER(LEN=1)    :: JunkC
      CHARACTER(LEN=500)  :: Line
      CHARACTER(LEN=100)  :: OutputFile
      CHARACTER(LEN=100)  :: StationsFile

      INTEGER,ALLOCATABLE :: Conn(:,:)
      INTEGER             :: E
      INTEGER             :: JunkI
      INTEGER             :: N
      INTEGER             :: NumElems
      INTEGER             :: NumNodes
      INTEGER             :: NumNodesNonDefault
      INTEGER             :: NumSnaps
      INTEGER             :: NumStations
      INTEGER             :: S
      INTEGER             :: SS
      INTEGER,ALLOCATABLE :: StationsElem(:)
      INTEGER             :: UserSel

      REAL(8)            :: DefaultValue
      REAL(8),ALLOCATABLE :: Global(:)
      REAL(8),ALLOCATABLE :: GridLat(:)
      REAL(8),ALLOCATABLE :: GridLon(:)
      REAL(8)             :: JunkR
      REAL(8),ALLOCATABLE :: StationsLat(:)
      REAL(8),ALLOCATABLE :: StationsLon(:)
      REAL(8)             :: StationsTemp
      REAL(8)             :: SubArea1
      REAL(8)             :: SubArea2
      REAL(8)             :: SubArea3
      REAL(8)             :: TotalArea
      REAL(8),ALLOCATABLE :: Weight(:,:)
      REAL(8)             :: X1
      REAL(8)             :: X2
      REAL(8)             :: X3
      REAL(8)             :: Y1
      REAL(8)             :: Y2
      REAL(8)             :: Y3

      WRITE(*,'(A)',ADVANCE="YES") " "
      WRITE(*,'(A)',ADVANCE="NO") "Enter name of file with lat/lon for stations: "
      READ(*,'(A)') StationsFile

      OPEN(UNIT=11,FILE=TRIM(StationsFile),ACTION="READ")

      READ(11,*) NumStations

      ALLOCATE(StationsLon(1:NumStations))
      ALLOCATE(StationsLat(1:NumStations))

      DO S=1,NumStations
         READ(11,*) JunkI, StationsLon(S), StationsLat(S)
      ENDDO

      CLOSE(UNIT=11,STATUS="KEEP")

      WRITE(*,'(A)',ADVANCE="YES") "Stations information was read successfully!"

      WRITE(*,'(A)',ADVANCE="YES") " "
      WRITE(*,'(A)',ADVANCE="NO") "Enter name of ADCIRC grid file: "
      READ(*,'(A)') GridFile

      OPEN(UNIT=14,FILE=TRIM(GridFile),ACTION="READ")
      OPEN(UNIT=19,FILE='Stn_Tmp.txt',ACTION="WRITE")   !!! Adonahue:  Text file listing stn #'s w/in mesh

      READ(14,'(A)') JunkC
      READ(14,*) NumElems, NumNodes

      ALLOCATE(GridLon(1:NumNodes))
      ALLOCATE(GridLat(1:NumNodes))

      DO N=1,NumNodes
         READ(14,*) JunkI, GridLon(N), GridLat(N)
      ENDDO

      ALLOCATE(Conn(1:NumElems,1:3))

      DO E=1,NumElems
         READ(14,*) JunkI, JunkI, Conn(E,1), Conn(E,2), Conn(E,3)
      ENDDO

      CLOSE(UNIT=14,STATUS="KEEP")

      ALLOCATE(StationsElem(1:NumStations))
      ALLOCATE(Weight(1:NumStations,1:3))


      DO S=1,NumStations

         inner: DO E=1,NumElems

            X1 = StationsLon(S)
            X2 = GridLon(Conn(E,2))
            X3 = GridLon(Conn(E,3))
            Y1 = StationsLat(S)
            Y2 = GridLat(Conn(E,2))
            Y3 = GridLat(Conn(E,3))
            SubArea1 = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

            X1 = GridLon(Conn(E,1))
            X2 = StationsLon(S)
            X3 = GridLon(Conn(E,3))
            Y1 = GridLat(Conn(E,1))
            Y2 = StationsLat(S)
            Y3 = GridLat(Conn(E,3))
            SubArea2 = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

            X1 = GridLon(Conn(E,1))
            X2 = GridLon(Conn(E,2))
            X3 = StationsLon(S)
            Y1 = GridLat(Conn(E,1))
            Y2 = GridLat(Conn(E,2))
            Y3 = StationsLat(S)
            SubArea3 = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

            X1 = GridLon(Conn(E,1))
            X2 = GridLon(Conn(E,2))
            X3 = GridLon(Conn(E,3))
            Y1 = GridLat(Conn(E,1))
            Y2 = GridLat(Conn(E,2))
            Y3 = GridLat(Conn(E,3))
            TotalArea = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))

            IF((SubArea1+SubArea2+SubArea3).LE.(1.01*TotalArea))THEN

               StationsElem(S) = E
               Weight(S,1) = ( (StationsLon(S)-X3)*(Y2-Y3)+(X2-X3)*(Y3-StationsLat(S)))/TotalArea
               Weight(S,2) = ( (StationsLon(S)-X1)*(Y3-Y1)-(StationsLat(S)-Y1)*(X3-X1))/TotalArea
               Weight(S,3) = (-(StationsLon(S)-X1)*(Y2-Y1)+(StationsLat(S)-Y1)*(X2-X1))/TotalArea

               PRINT*, Weight(S,1), Weight(S,2), Weight(S,3), S

	       WRITE(19,'(I5)') S	!!! Adonahue: Determine which stations are in Mesh

               EXIT inner

	    ELSE  !!! Adonahue Locate stations outside of Mesh

	       StationsElem(S) = 0

            ENDIF    

         ENDDO inner

      ENDDO

      WRITE(*,'(A)',ADVANCE="YES") "ADCIRC grid file was read successfully!"

      WRITE(*,'(A)',ADVANCE="YES") " "
      WRITE(*,'(A)',ADVANCE="NO") "Enter name of file containing global values: "
      READ(*,'(A)') GlobalFile

      OPEN(UNIT=12,FILE=TRIM(GlobalFile),ACTION="READ")

      READ(12,'(A)') JunkC
      READ(12,'(I11,I11,A)') NumSnaps,JunkI,Line
      WRITE(*,'(A)',ADVANCE="YES") " "
      WRITE(*,'(A)',ADVANCE="NO") "Enter name of output file: "
      READ(*,'(A)') OutputFile

      OPEN(UNIT=14,FILE=TRIM(OutputFile),ACTION="WRITE")

      WRITE(14,'(A)') "Interpolated values at stations"
      WRITE(14,'(I11,I11,A)') NumSnaps,NumStations,TRIM(Line)

      WRITE(*,'(A)',ADVANCE="YES") " "

      DO SS=1,NumSnaps

	 WRITE(*,'(A,I3.3,A,I3.3,A)',ADVANCE="YES") "Processing snap #",SS," of ",NumSnaps,"."

         ALLOCATE(Global(1:NumNodes))

         READ(12,'(A)',ERR=999,END=999) Line
         READ(Line,*) JunkR, JunkI, NumNodesNonDefault, DefaultValue
!        NumNodesNonDefault = NumNodes
!        DefaultValue = -99999.D0

         WRITE(14,'(A)') TRIM(Line)

         DO N=1,NumNodes
            Global(N) = DefaultValue
         ENDDO

         DO N=1,NumNodesNonDefault
            READ(12,*,ERR=999,END=999) JunkI, StationsTemp
            Global(JunkI) = StationsTemp
         ENDDO

         DO S=1,NumStations

	    IF(StationsElem(S).eq.0)THEN   !!! Adonahue: Flag stations not in mesh
		StationsTemp = -9999 
	    ELSE
                StationsTemp = Global(Conn(StationsElem(S),1)) * Weight(S,1) &
                         + Global(Conn(StationsElem(S),2)) * Weight(S,2) &
                         + Global(Conn(StationsElem(S),3)) * Weight(S,3)
	    ENDIF

            WRITE(14,'(I10,5X,E17.10)') S, StationsTemp

         ENDDO

         DEALLOCATE(Global)

      ENDDO

 999  CONTINUE

      CLOSE(UNIT=12,STATUS="KEEP")
      CLOSE(UNIT=19,STATUS="KEEP")

      WRITE(*,'(A)',ADVANCE="YES") "Significant wave heights were processed successfully!"

      CLOSE(UNIT=14,STATUS="KEEP")

      DEALLOCATE(Conn)
      DEALLOCATE(StationsElem)
      DEALLOCATE(StationsLat)
      DEALLOCATE(StationsLon)
      DEALLOCATE(Weight)

END PROGRAM


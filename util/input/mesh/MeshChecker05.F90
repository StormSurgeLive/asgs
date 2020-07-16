!
! Generalized diagnostics to look for some common errors in Adcirc meshes:
!
!      Sequential node and element numbering
!      Disjoint nodes and duplicate elements
!      Unrealistic topo heights
!      Ordering of river BC should be east -> west
!      Minimum element size reporting
!      Unrealistic default -8888 levee crest heights
!      Comparison of levee crest with prevailing topo
!
!
! Original LeveeChecker6.f90 coded by Casey Dietrich @ ND
!
! Additional checks coded by Atkinson 303-544-0043 ext 20
!
! Additional checks for Overlapping Elements coded by Seizo
! Additional checks for Boundary Edge        coded by Seizo
!
! Added OpenMP to LeveeChecking Portion of code, Zach Cobell [ v04 ]
!   Under gfortran compile with flag "-fopenmp" to activate parallel code.
! Other bug fixes, Zach Cobell
!
! Added Check for overlapping boundary conditions, Zach Cobell [ v05 ]
!
!

MODULE DATA

    IMPLICIT NONE

    CHARACTER(LEN=100)                 :: Header
    CHARACTER(LEN=100)                 :: NumGenBoundariesC
    CHARACTER(LEN=100)                 :: NumLandBoundariesC
    CHARACTER(LEN=100)                 :: NumOpenBoundariesC
    CHARACTER(LEN=100)                 :: TempC
    CHARACTER(LEN=100)                 :: TotNumGenBoundaryNodesC
    CHARACTER(LEN=100)                 :: TotNumLandBoundaryNodesC
    CHARACTER(LEN=100)                 :: TotNumOpenBoundaryNodesC

    INTEGER                            :: Counter
    INTEGER                            :: FoundJunction
    INTEGER                            :: NumElems
    INTEGER                            :: NumGenBoundaries
    INTEGER                            :: NumLandBoundaries
    INTEGER                            :: NumNodes
    INTEGER                            :: NumOpenBoundaries
    INTEGER                            :: StringLen
    INTEGER                            :: TotNumGenBoundaryNodes
    INTEGER                            :: TotNumLandBoundaryNodes
    INTEGER                            :: TotNumOpenBoundaryNodes

    DOUBLE PRECISION                   :: MaxIncrease=0.D0
    DOUBLE PRECISION                   :: MaxJunctionHeight
    
    TYPE GridListing1
        INTEGER :: Node
        DOUBLE PRECISION :: XCoord
        DOUBLE PRECISION :: YCoord
        DOUBLE PRECISION :: Bath
    END TYPE
    TYPE (GridListing1), ALLOCATABLE, DIMENSION(:) :: GridInfo1

    TYPE GridListing2
        INTEGER :: Element
        INTEGER :: NodeCount
        INTEGER, DIMENSION(3) :: Nodes
    END TYPE
    TYPE (GridListing2), ALLOCATABLE, DIMENSION(:) :: GridInfo2
    
    LOGICAL                            :: debug = .FALSE.

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
    TYPE (BoundaryListing), ALLOCATABLE, DIMENSION(:) :: GenBoundaries
    TYPE (BoundaryListing), ALLOCATABLE, DIMENSION(:) :: LandBoundaries 
    TYPE (BoundaryListing), ALLOCATABLE, DIMENSION(:) :: OpenBoundaries 

    INTEGER, ALLOCATABLE :: nean(:,:), numean(:)

END MODULE



PROGRAM LeveeChecker

    USE DATA

    IMPLICIT NONE

    CALL ReadGrid
    CALL ElementCheck          ! Check for Overlapping Elements !S.Tanaka, 2008.08.28
    CALL BoundaryCheck         ! Check for Boundary Condition   !S.Tanaka, 2008.09.01
    CALL CheckOverlappingBCs   ! Check for overlapping boundary condition ! Z. Cobell, 2011.5.25
    CALL AdjustHeights
    CALL WriteGrid

    DEALLOCATE(GridInfo1)
    DEALLOCATE(GridInfo2)
    DEALLOCATE(OpenBoundaries)
    DEALLOCATE(LandBoundaries)

END PROGRAM



SUBROUTINE AdjustHeights

!$  USE OMP_LIB  !ZCobell

    USE DATA

    IMPLICIT NONE

    INTRINSIC        :: INT
    INTRINSIC        :: LEN

    INTEGER          :: I
    INTEGER          :: J
    INTEGER          :: RaiseCriterion
    INTEGER          :: NumCores
    
    REAL             :: start
    REAL             :: finish

    DOUBLE PRECISION :: MaxGroundHeight
    DOUBLE PRECISION :: MinHeight
    DOUBLE PRECISION :: Tolerance

    WRITE(*,'(A)',ADVANCE="YES") " "
    WRITE(*,'(A)',ADVANCE="YES") "Do you want to:"
    WRITE(*,'(A)',ADVANCE="YES") "    1. Require each levee height to be above the ground surface"
    WRITE(*,'(A)',ADVANCE="YES") "       at its two-node levee pair, or"
    WRITE(*,'(A)',ADVANCE="YES") "    2. Require each levee height to be above the ground surface"
    WRITE(*,'(A)',ADVANCE="YES") "       at its two-node levee pair AND any immediately adjoining"
    WRITE(*,'(A)',ADVANCE="YES") "       two-node levee pairs?"
    WRITE(*,'(A)',ADVANCE="NO") "Enter selection (1/2): "
    READ(*,*) RaiseCriterion
    WRITE(*,'(A)',ADVANCE="NO") "Enter minimum levee height above ground surface (0.2): "
    READ(*,*) MinHeight
    WRITE(*,'(A)',ADVANCE="NO") "Enter minimum height that a levee can be raised (0.01): "
    READ(*,*) Tolerance

    Counter = 0

    

!$  IF(OMP_GET_MAX_THREADS().GT.1)THEN
!$      NumCores = OMP_GET_MAX_THREADS()
!$  ELSE
        NumCores = 1
!$  ENDIF
    CALL CPU_TIME(start)                                                                            !...ZCobell
!$  WRITE(*,'(A,I0,A)') "Begin checking levees with ",omp_get_max_threads()," cores."               !...ZCobell
    !$OMP PARALLEL SHARED(MaxIncrease,Counter,LandBoundaries,GridInfo1) PRIVATE(I,MaxGroundHeight)  !...ZCobell
    !$OMP DO SCHEDULE(DYNAMIC)                                                                      !...ZCobell
    DO J=1,NumLandBoundaries

        IF(LandBoundaries(J)%Code.EQ.13)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%Bath

                IF(RaiseCriterion.EQ.2)THEN

                    IF(I.GT.1)THEN

                        IF(MaxGroundHeight.GT.GridInfo1(INT(LandBoundaries(J)%Entry1(I-1)))%Bath)THEN

                            MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry1(I-1)))%Bath

                        ENDIF

                    ENDIF

                    IF(I.LT.LandBoundaries(J)%NumNodes)THEN

                        IF(MaxGroundHeight.GT.GridInfo1(INT(LandBoundaries(J)%Entry1(I+1)))%Bath)THEN

                            MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry1(I+1)))%Bath

                        ENDIF

                    ENDIF

                ENDIF

                IF(LandBoundaries(J)%Entry2(I).LT.(MinHeight-MaxGroundHeight).AND. &
                        (MinHeight-MaxGroundHeight-LandBoundaries(J)%Entry2(I)).GE.Tolerance)THEN
                    
                    WRITE(289,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry1(I)), &
                              GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%XCoord, &
                              GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%YCoord, &
                              LandBoundaries(J)%Entry2(I), MinHeight - MaxGroundHeight, &
                              MinHeight - MaxGroundHeight - LandBoundaries(J)%Entry2(I)

                    IF((MinHeight-MaxGroundHeight-LandBoundaries(J)%Entry2(I)).GT.MaxIncrease)THEN

                        MaxIncrease = MinHeight - MaxGroundHeight - LandBoundaries(J)%Entry2(I)

                    ENDIF

                    LandBoundaries(J)%Entry2(I) = MinHeight - MaxGroundHeight
                    Counter = Counter + 1

                ENDIF

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.23)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%Bath

                IF(RaiseCriterion.EQ.2)THEN

                    IF(I.GT.1)THEN

                        IF(MaxGroundHeight.GT.GridInfo1(INT(LandBoundaries(J)%Entry1(I-1)))%Bath)THEN

                            MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry1(I-1)))%Bath

                        ENDIF

                    ENDIF

                    IF(I.LT.LandBoundaries(J)%NumNodes)THEN

                        IF(MaxGroundHeight.GT.GridInfo1(INT(LandBoundaries(J)%Entry1(I+1)))%Bath)THEN

                            MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry1(I+1)))%Bath

                        ENDIF

                    ENDIF

                ENDIF

                IF(LandBoundaries(J)%Entry2(I).LT.(MinHeight-MaxGroundHeight).AND. &
                        (MinHeight-MaxGroundHeight-LandBoundaries(J)%Entry2(I)).GE.Tolerance)THEN

                    WRITE(289,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry1(I)), &
                              GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%XCoord, &
                              GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%YCoord, &
                              LandBoundaries(J)%Entry2(I), MinHeight - MaxGroundHeight, &
                              MinHeight - MaxGroundHeight - LandBoundaries(J)%Entry2(I)

                    IF((MinHeight-MaxGroundHeight-LandBoundaries(J)%Entry2(I)).GT.MaxIncrease)THEN

                        MaxIncrease = MinHeight - MaxGroundHeight - LandBoundaries(J)%Entry2(I)

                    ENDIF

                    LandBoundaries(J)%Entry2(I) = MinHeight - MaxGroundHeight
                    Counter = Counter + 1

                ENDIF

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.24)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%Bath

                IF(RaiseCriterion.EQ.2)THEN

                    IF(I.GT.1)THEN

                        IF(MaxGroundHeight.GT.GridInfo1(INT(LandBoundaries(J)%Entry1(I-1)))%Bath)THEN

                            MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry1(I-1)))%Bath

                        ENDIF

                    ENDIF

                    IF(I.LT.LandBoundaries(J)%NumNodes)THEN

                        IF(MaxGroundHeight.GT.GridInfo1(INT(LandBoundaries(J)%Entry1(I+1)))%Bath)THEN

                            MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry1(I+1)))%Bath

                        ENDIF

                    ENDIF

                ENDIF

                IF(MaxGroundHeight.GT.GridInfo1(INT(LandBoundaries(J)%Entry2(I)))%Bath)THEN

                    MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry2(I)))%Bath

                ENDIF

                IF(RaiseCriterion.EQ.2)THEN

                    IF(I.GT.1)THEN

                        IF(MaxGroundHeight.GT.GridInfo1(INT(LandBoundaries(J)%Entry2(I-1)))%Bath)THEN

                            MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry2(I-1)))%Bath

                        ENDIF

                    ENDIF

                    IF(I.LT.LandBoundaries(J)%NumNodes)THEN

                        IF(MaxGroundHeight.GT.GridInfo1(INT(LandBoundaries(J)%Entry2(I+1)))%Bath)THEN

                            MaxGroundHeight = GridInfo1(INT(LandBoundaries(J)%Entry2(I+1)))%Bath

                        ENDIF

                    ENDIF

                ENDIF

                IF(LandBoundaries(J)%Entry3(I).LT.(MinHeight-MaxGroundHeight).AND. &
                        (MinHeight-MaxGroundHeight-LandBoundaries(J)%Entry3(I)).GE.Tolerance)THEN
                    
                    WRITE(289,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry1(I)), &
                              GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%XCoord, &
                              GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%YCoord, &
                              LandBoundaries(J)%Entry3(I), MinHeight - MaxGroundHeight, &
                              MinHeight - MaxGroundHeight - LandBoundaries(J)%Entry3(I)

                    IF((MinHeight-MaxGroundHeight-LandBoundaries(J)%Entry3(I)).GT.MaxIncrease)THEN

                        MaxIncrease = MinHeight - MaxGroundHeight - LandBoundaries(J)%Entry3(I)

                    ENDIF

                    LandBoundaries(J)%Entry3(I) = MinHeight - MaxGroundHeight
                    Counter = Counter + 1

                ENDIF

            ENDDO

        ENDIF

    ENDDO
    !$OMP END DO         !...ZC
    !$OMP END PARALLEL   !...ZC

    
    
    !$OMP PARALLEL SHARED(GridInfo1,LandBoundaries) PRIVATE(FoundJunction,I)   !...ZC
    !$OMP DO SCHEDULE(DYNAMIC)                                                 !...ZC
    DO J=1,NumLandBoundaries

        IF(LandBoundaries(J)%Code.EQ.13)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                CALL FindJunctionNode(INT(LandBoundaries(J)%Entry1(I)))

                IF(FoundJunction.EQ.1)THEN

                    IF(MaxJunctionHeight.GT.LandBoundaries(J)%Entry2(I))THEN

                        WRITE(290,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry1(I)), &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%XCoord, &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%YCoord, &
                                  LandBoundaries(J)%Entry2(I), &
                                  MaxJunctionHeight, &
                                  MaxJunctionHeight - LandBoundaries(J)%Entry2(I)

                        LandBoundaries(J)%Entry2(I) = MaxJunctionHeight

                    ENDIF

                ENDIF

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.23)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                CALL FindJunctionNode(INT(LandBoundaries(J)%Entry1(I)))

                IF(FoundJunction.EQ.1)THEN

                    IF(MaxJunctionHeight.GT.LandBoundaries(J)%Entry2(I))THEN

                        WRITE(290,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry1(I)), &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%XCoord, &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%YCoord, &
                                  LandBoundaries(J)%Entry2(I), &
                                  MaxJunctionHeight, &
                                  MaxJunctionHeight - LandBoundaries(J)%Entry2(I)

                        LandBoundaries(J)%Entry2(I) = MaxJunctionHeight

                    ENDIF

                ENDIF

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.24)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                CALL FindJunctionNode(INT(LandBoundaries(J)%Entry1(I)))

                IF(FoundJunction.EQ.1)THEN

                    IF(MaxJunctionHeight.GT.LandBoundaries(J)%Entry3(I))THEN

                        WRITE(290,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry1(I)), &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%XCoord, &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%YCoord, &
                                  LandBoundaries(J)%Entry3(I), &
                                  MaxJunctionHeight, &
                                  MaxJunctionHeight - LandBoundaries(J)%Entry3(I)

                        LandBoundaries(J)%Entry3(I) = MaxJunctionHeight

                    ENDIF

                ENDIF

                CALL FindJunctionNode(INT(LandBoundaries(J)%Entry2(I)))

                IF(FoundJunction.EQ.1)THEN

                    IF(MaxJunctionHeight.GT.LandBoundaries(J)%Entry3(I))THEN

                        WRITE(290,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry2(I)), &
                                  GridInfo1(INT(LandBoundaries(J)%Entry2(I)))%XCoord, &
                                  GridInfo1(INT(LandBoundaries(J)%Entry2(I)))%YCoord, &
                                  LandBoundaries(J)%Entry3(I), &
                                  MaxJunctionHeight, &
                                  MaxJunctionHeight - LandBoundaries(J)%Entry3(I)

                        LandBoundaries(J)%Entry3(I) = MaxJunctionHeight

                    ENDIF

                ENDIF

            ENDDO

        ENDIF

    ENDDO
    !$OMP END DO        !...ZCobell
    !$OMP END PARALLEL  !...ZCobell

    
    !$OMP PARALLEL SHARED(GridInfo1,LandBoundaries) PRIVATE(I,FoundJunction)   !...ZCobell
    !$OMP DO SCHEDULE(DYNAMIC)                                                 !...ZCobell
    DO J=1,NumLandBoundaries

        IF(LandBoundaries(J)%Code.EQ.13)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                CALL FindJunctionNode(INT(LandBoundaries(J)%Entry1(I)))

                IF(FoundJunction.EQ.1)THEN

                    IF(MaxJunctionHeight.GT.LandBoundaries(J)%Entry2(I))THEN

                        WRITE(290,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry1(I)), &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%XCoord, &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%YCoord, &
                                  LandBoundaries(J)%Entry2(I), &
                                  MaxJunctionHeight, &
                                  MaxJunctionHeight - LandBoundaries(J)%Entry2(I)

                        LandBoundaries(J)%Entry2(I) = MaxJunctionHeight

                    ENDIF

                ENDIF

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.23)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                CALL FindJunctionNode(INT(LandBoundaries(J)%Entry1(I)))

                IF(FoundJunction.EQ.1)THEN

                    IF(MaxJunctionHeight.GT.LandBoundaries(J)%Entry2(I))THEN

                        WRITE(290,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry1(I)), &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%XCoord, &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%YCoord, &
                                  LandBoundaries(J)%Entry2(I), &
                                  MaxJunctionHeight, &
                                  MaxJunctionHeight - LandBoundaries(J)%Entry2(I)

                        LandBoundaries(J)%Entry2(I) = MaxJunctionHeight

                    ENDIF

                ENDIF

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.24)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                CALL FindJunctionNode(INT(LandBoundaries(J)%Entry1(I)))

                IF(FoundJunction.EQ.1)THEN

                    IF(MaxJunctionHeight.GT.LandBoundaries(J)%Entry3(I))THEN

                        WRITE(290,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry1(I)), &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%XCoord, &
                                  GridInfo1(INT(LandBoundaries(J)%Entry1(I)))%YCoord, &
                                  LandBoundaries(J)%Entry3(I), &
                                  MaxJunctionHeight, &
                                  MaxJunctionHeight - LandBoundaries(J)%Entry3(I)

                        LandBoundaries(J)%Entry3(I) = MaxJunctionHeight

                    ENDIF

                ENDIF

                CALL FindJunctionNode(INT(LandBoundaries(J)%Entry2(I)))

                IF(FoundJunction.EQ.1)THEN

                    IF(MaxJunctionHeight.GT.LandBoundaries(J)%Entry3(I))THEN

                        WRITE(290,'(1X,I9,2(1X,F16.9),3(1X,E20.9))') INT(LandBoundaries(J)%Entry2(I)), &
                                  GridInfo1(INT(LandBoundaries(J)%Entry2(I)))%XCoord, &
                                  GridInfo1(INT(LandBoundaries(J)%Entry2(I)))%YCoord, &
                                  LandBoundaries(J)%Entry3(I), &
                                  MaxJunctionHeight, &
                                  MaxJunctionHeight - LandBoundaries(J)%Entry3(I)

                        LandBoundaries(J)%Entry3(I) = MaxJunctionHeight

                    ENDIF

                ENDIF

            ENDDO

        ENDIF

    ENDDO
    !$OMP END DO               !...ZCobell
    !$OMP END PARALLEL         !...ZCobell
    CALL CPU_TIME(finish)

    WRITE(*,'(A,I6,A)',ADVANCE="YES") "A total of ", Counter, " levee heights were raised."
    WRITE(*,'(A,F10.6)',ADVANCE="YES") "The maximum amount that a levee height was raised was: ",MaxIncrease
    IF(debug)THEN
        WRITE(*,'(A,F0.2,A)') "Levee Check Execution time: ",(finish-start)/REAL(NumCores)," seconds"
    ENDIF

END SUBROUTINE



SUBROUTINE FindJunctionNode(LeveeNode)

    USE DATA

    IMPLICIT NONE

    INTEGER :: I
    INTEGER :: J
    INTEGER :: LeveeNode

    FoundJunction = 0
    MaxJunctionHeight = -99999.D0

    DO J=1,NumLandBoundaries

        IF(LandBoundaries(J)%Code.EQ.13)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                IF((INT(LandBoundaries(J)%Entry1(I)).EQ.LeveeNode) &
                        .AND.(LandBoundaries(J)%Entry2(I).NE.100.D0))THEN

                    FoundJunction = 1

                    IF(LandBoundaries(J)%Entry2(I).GE.MaxJunctionHeight)THEN

                        MaxJunctionHeight = LandBoundaries(J)%Entry2(I)

                    ENDIF

                ENDIF

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.23)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                IF((INT(LandBoundaries(J)%Entry1(I)).EQ.LeveeNode) &
                        .AND.(LandBoundaries(J)%Entry2(I).NE.100.D0))THEN

                    FoundJunction = 1

                    IF(LandBoundaries(J)%Entry2(I).GE.MaxJunctionHeight)THEN 

                        MaxJunctionHeight = LandBoundaries(J)%Entry2(I)

                    ENDIF

                ENDIF

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.24)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                IF((INT(LandBoundaries(J)%Entry1(I)).EQ.LeveeNode) &
                        .AND.(LandBoundaries(J)%Entry3(I).NE.100.D0))THEN

                    FoundJunction = 1

                    IF(LandBoundaries(J)%Entry3(I).GE.MaxJunctionHeight)THEN

                        MaxJunctionHeight = LandBoundaries(J)%Entry3(I)

                    ENDIF

                ELSEIF((INT(LandBoundaries(J)%Entry2(I)).EQ.LeveeNode) &
                        .AND.(LandBoundaries(J)%Entry3(I).NE.100.D0))THEN

                    FoundJunction = 1

                    IF(LandBoundaries(J)%Entry3(I).GE.MaxJunctionHeight)THEN 

                        MaxJunctionHeight = LandBoundaries(J)%Entry3(I)

                    ENDIF

                ENDIF

            ENDDO

        ENDIF

    ENDDO

END SUBROUTINE



SUBROUTINE ReadGrid

    USE DATA

    IMPLICIT NONE

    REAL*8 avelongitude				! jha
    REAL*8 riverlongitude(5)			! jha can accomodate up to five river BC
    REAL*8 elem_size, min_elem_size, xc, yc	! jha
    REAL*8 size_flag, topo_flag                 ! jha
    REAL*8 no_data_levee_flag, max_levee_crest  ! jha
    REAL*8 max_separation, x1,x2,y1,y2,d        ! jha

    CHARACTER(LEN=256) :: GridFile

    INTRINSIC          :: INT
    INTRINSIC          :: LEN

    INTEGER            :: I
    INTEGER            :: J
    INTEGER            :: ijk, n1, n2, num_small_elem   ! jha
    INTEGER, ALLOCATABLE, DIMENSION(:) :: used          ! jha
    
    LOGICAL            :: exists                        ! zc

! initialize some variables...

    riverlongitude = 0.d0			! jha
    min_elem_size = 0.d0
    num_small_elem = 0

    WRITE(*,'(A)',ADVANCE="YES") " "
    WRITE(*,'(A)',ADVANCE="NO") "Enter name of grid file: "
    READ(*,*) GridFile
    
    INQUIRE(FILE=TRIM(gridfile),EXIST=exists)
    IF(.NOT.exists)THEN
        WRITE(*,'(A)') "Grid file does not exist."
        STOP
    ENDIF
    
    OPEN(UNIT=1,FILE=GridFile,ACTION="READ")

    READ(1,'(A)') Header
    READ(1,*) NumElems, NumNodes

    ALLOCATE(GridInfo1(1:NumNodes))
    ALLOCATE(GridInfo2(1:NumElems))
    ALLOCATE(used(NumNodes))
    used = 0

    WRITE(*,*)' '
    WRITE(*,'(A)',ADVANCE="NO") "Enter minimum topo height to flag in meters (-100.0): "
    READ(*,*) topo_flag

    WRITE(*,*)' '
    WRITE(*,'(A)',ADVANCE="NO") "Enter minimum element size to flag in meters (30.0): "
    READ(*,*) size_flag



    DO I=1,NumNodes

        READ(1,*) GridInfo1(I)%Node, GridInfo1(I)%XCoord, &
                  GridInfo1(I)%YCoord, GridInfo1(I)%Bath

        IF( GridInfo1(I)%Node .ne.  I)THEN            ! jha
		write(*,*)'  '
		write(*,*)' FATAL ERROR IN GRID FILE:'
		write(*,*)' Error in node table at I = ',I
		write(*,*)'  '
		write(*,*)'  '
		stop
	ENDIF

        IF( GridInfo1(I)%Bath .lt.  topo_flag)THEN            ! jha
                write(*,*)'  '
                write(*,*)' FATAL ERROR IN GRID FILE:'
                write(*,*)' Check elevation at node  = ',I
                write(*,*)'  '
                write(*,*)'  '
                stop
        ENDIF


    ENDDO

    write(*,*)' '
    write(*,*)' Node table read correctly...'
    write(*,*)' '


    min_elem_size = 1000000.d0                             ! jha
    open(122,file='small_elem.pts', action='write')

    DO I=1,NumElems

        READ(1,*) GridInfo2(I)%Element, GridInfo2(I)%NodeCount, &
                  GridInfo2(I)%Nodes(1), GridInfo2(I)%Nodes(2), &
                  GridINfo2(I)%Nodes(3)

        IF( GridInfo2(I)%Element .ne.  I)THEN            ! jha
                write(*,*)'  '
                write(*,*)' FATAL ERROR IN GRID FILE:'
                write(*,*)' Error in connectivity table at I = ',I
                write(*,*)'  '
                write(*,*)'  '
                stop
        ENDIF
 
        if(used(GridInfo2(I)%Nodes(1)) .eq. 0) used(GridInfo2(I)%Nodes(1))=1
        if(used(GridInfo2(I)%Nodes(2)) .eq. 0) used(GridInfo2(I)%Nodes(2))=1
        if(used(GridInfo2(I)%Nodes(3)) .eq. 0) used(GridInfo2(I)%Nodes(3))=1

        CALL ElementSize(GridInfo2(I)%Nodes(1),GridInfo2(I)%Nodes(2),GridInfo2(I)%Nodes(3),elem_size)

        elem_size = elem_size * 1000.d0                ! jha - convert to meters
        IF ( elem_size .lt. min_elem_size) min_elem_size = elem_size
        IF( elem_size .le. size_flag)THEN
                num_small_elem = num_small_elem + 1
                xc = 0.d0
                yc = 0.d0
                do ijk=1,3
		   xc = xc + GridInfo1(GridInfo2(I)%Nodes(ijk))%XCoord
		   yc = yc + GridInfo1(GridInfo2(I)%Nodes(ijk))%YCoord
                enddo
                xc = xc/3.d0
                yc = yc/3.d0
		write(122,101)I,xc,yc,elem_size

                
        ENDIF	

    ENDDO
 101 format(1x,i9,1x,f15.8,1x,f15.8,1x,f18.4)

    write(*,*)' '
    write(*,*)' Element table read correctly...'
    write(*,*)' '
    write(*,*)' '
    write(*,102)num_small_elem,size_flag
    write(*,103)size_flag
    write(*,*)' '
    write(*,*)' Minimun element size = ',min_elem_size,' meters.'
    write(*,*)' '
    write(*,*)' '
    close(122)
 102 format(' Total of ',i9,' elements with an edge less than',f8.2,' m.')
 103 format(' See scatter file, small_elem.pts, for all elements < ',f8.2,' m.')

    if( sum(used) .ne. NumNodes)then
        write(*,*)' '
        write(*,*)' FATAL ERROR:'
        write(*,*)'  Disjoint nodes have been found...'
        write(*,*)' '
	do i=1,NumNodes
	   if(used(i).eq.0)then
	     write(*,*)' node ',i,' is unaffiliated.'
           endif
        enddo
        write(*,*)' '
        stop
    else
        write(*,*)' '
	write(*,*)' No dis-joint nodes were found.'
        write(*,*)' '
    endif


    WRITE(*,*)' '
    WRITE(*,'(A)',ADVANCE="NO") "Enter maximum expected levee crest in meters (12.0): "
    READ(*,*) max_levee_crest

    WRITE(*,*)' '
    WRITE(*,'(A)',ADVANCE="NO") "Enter the expected NO-DATA flag for levees (-8888.0): "
    READ(*,*) no_data_levee_flag

    WRITE(*,*)' '
    WRITE(*,'(A)',ADVANCE="NO") "Enter the maximum separation distance in meters for levee pairs (350.0): "
    READ(*,*) max_separation


    READ(1,*) NumOpenBoundaries
    write(*,*) NumOpenBoundaries
     
    BACKSPACE(1)
    READ(1,'(A)') NumOpenBoundariesC
    inner1: DO I=1,LEN(NumOpenBoundariesC)
        IF(NumOpenBoundariesC(I:I).EQ."=")THEN
            StringLen = I+2
            EXIT inner1
        ENDIF
    ENDDO inner1
    TempC = NumOpenBoundariesC(StringLen:LEN(NumOpenBoundariesC))
    NumOpenBoundariesC = TempC
    READ(1,*) TotNumOpenBoundaryNodes
    BACKSPACE(1)
    READ(1,'(A)') TotNumOpenBoundaryNodesC
    inner2: DO I=1,LEN(TotNumOpenBoundaryNodesC)
        IF(TotNumOpenBoundaryNodesC(I:I).EQ."=")THEN
            StringLen = I+2
            EXIT inner2
        ENDIF
    ENDDO inner2
    TempC = TotNumOpenBoundaryNodesC(StringLen:LEN(TotNumOpenBoundaryNodesC))
    TotNumOpenBoundaryNodesC = TempC

    ALLOCATE(OpenBoundaries(1:NumOpenBoundaries))

    DO J=1,NumOpenBoundaries

        READ(1,*) OpenBoundaries(J)%NumNodes
        BACKSPACE(1)
        READ(1,'(A)') OpenBoundaries(J)%Header 
        inner3: DO I=1,LEN(OpenBoundaries(J)%Header)
            IF(OpenBoundaries(J)%Header(I:I).EQ."=")THEN
                StringLen = I+2
                EXIT inner3
            ENDIF
        ENDDO inner3
        TempC = OpenBoundaries(J)%Header(StringLen:LEN(OpenBoundaries(J)%Header))
        OpenBoundaries(J)%Header = TempC

        ALLOCATE(OpenBoundaries(J)%Entry1(1:OpenBoundaries(J)%NumNodes)) 

        DO I=1,OpenBoundaries(J)%NumNodes

            READ(1,*) OpenBoundaries(J)%Entry1(I)

        ENDDO

    ENDDO

    READ(1,*) NumLandBoundaries
    BACKSPACE(1)
    READ(1,'(A)') NumLandBoundariesC
    inner4: DO I=1,LEN(NumLandBoundariesC)
        IF(NumLandBoundariesC(I:I).EQ."=")THEN
            StringLen = I+2
            EXIT inner4
        ENDIF
    ENDDO inner4
    TempC = NumLandBoundariesC(StringLen:LEN(NumLandBoundariesC))
    NumLandBoundariesC = TempC
    READ(1,*) TotNumLandBoundaryNodes
    BACKSPACE(1)
    READ(1,'(A)') TotNumLandBoundaryNodesC
    inner5: DO I=1,LEN(TotNumLandBoundaryNodesC)
        IF(TotNumLandBoundaryNodesC(I:I).EQ."=")THEN
            StringLen = I+2
            EXIT inner5
        ENDIF
    ENDDO inner5
    TempC = TotNumLandBoundaryNodesC(StringLen:LEN(TotNumLandBoundaryNodesC))
    TotNumLandBoundaryNodesC = TempC

    ALLOCATE(LandBoundaries(1:NumLandBoundaries))

    DO J=1,NumLandBoundaries

        READ(1,*) LandBoundaries(J)%NumNodes, LandBoundaries(J)%Code
        BACKSPACE(1)
        READ(1,'(A)') LandBoundaries(J)%Header 
        inner6: DO I=1,LEN(LandBoundaries(J)%Header)
            IF(LandBoundaries(J)%Header(I:I).EQ."=")THEN
                StringLen = I+2
                EXIT inner6
            ENDIF
        ENDDO inner6
        TempC = LandBoundaries(J)%Header(StringLen:LEN(LandBoundaries(J)%Header))
        LandBoundaries(J)%Header = TempC

        IF(INT(LandBoundaries(J)%Code).EQ.0)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I)

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.1)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I)

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.10)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I)

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.11)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I)

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.12)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I)

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.13)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))
            ALLOCATE(LandBoundaries(J)%Entry2(1:LandBoundaries(J)%NumNodes))
            ALLOCATE(LandBoundaries(J)%Entry3(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I), &
                          LandBoundaries(J)%Entry2(I), & 
                          LandBoundaries(J)%Entry3(I) 

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.20)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I)

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.21)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I)

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.22)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I)

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.23)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))
            ALLOCATE(LandBoundaries(J)%Entry2(1:LandBoundaries(J)%NumNodes))
            ALLOCATE(LandBoundaries(J)%Entry3(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I), &
                          LandBoundaries(J)%Entry2(I), & 
                          LandBoundaries(J)%Entry3(I) 

                          n1 = INT(LandBoundaries(J)%Entry1(I))

                if(LandBoundaries(J)%Entry2(I) .eq. no_data_levee_flag)then           !jha

                        write(*,*)' '
                        write(*,*)' FATAL ERROR:'
                        write(*,*)'  NO-DATA flag found for crest elevation in BC segment ',J
                        write(*,*)'  node : ',n1
                        write(*,*)' '
                        stop

                endif


                if(LandBoundaries(J)%Entry2(I) .ge. max_levee_crest)then           !jha

                        write(8,*)' '
                        write(8,*)' WARNING:'
                        write(8,*)'  Check BC segment ',J
                        write(8,*)'  node : ',n1
                        write(8,*)'  crest: ',LandBoundaries(J)%Entry2(I)
                        write(8,*)' '

                endif


            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.24)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))
            ALLOCATE(LandBoundaries(J)%Entry2(1:LandBoundaries(J)%NumNodes))
            ALLOCATE(LandBoundaries(J)%Entry3(1:LandBoundaries(J)%NumNodes))
            ALLOCATE(LandBoundaries(J)%Entry4(1:LandBoundaries(J)%NumNodes))
            ALLOCATE(LandBoundaries(J)%Entry5(1:LandBoundaries(J)%NumNodes))

            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I), &
                          LandBoundaries(J)%Entry2(I), & 
                          LandBoundaries(J)%Entry3(I), & 
                          LandBoundaries(J)%Entry4(I), & 
                          LandBoundaries(J)%Entry5(I) 

                          n1 = INT(LandBoundaries(J)%Entry1(I))
                          n2 = INT(LandBoundaries(J)%Entry2(I))

                if(LandBoundaries(J)%Entry3(I) .eq. no_data_levee_flag)then           !jha

			write(8,*)' '
			write(8,*)' FATAL ERROR:'
			write(8,*)'  NO-DATA flag found for crest elevation in BC segment ',J
			write(8,*)'  node pair : ',n1,n2
			write(8,*)' '
                        stop

		endif


                if(LandBoundaries(J)%Entry3(I) .ge. max_levee_crest)then              !jha

                        write(8,*)' '
                        write(8,*)' WARNING:'
                        write(8,*)'  Check BC segment ',J
                        write(8,*)'  pair : ',n1,n2
                        write(8,*)'  crest: ',LandBoundaries(J)%Entry3(I)
                        write(8,*)' '

                endif

                x1 = GridInfo1(n1)%XCoord             !jha
                y1 = GridInfo1(n1)%YCoord
                x2 = GridInfo1(n2)%XCoord
                y2 = GridInfo1(n2)%YCoord
		call distance(x1,y1,x2,y2,d)
		d = d *1000.0                                              ! conv to meters
                if(d .ge. max_separation)then              !jha

                        write(8,*)' '
                        write(8,*)' WARNING:'
                        write(8,*)'  Check BC segment ',J
                        write(8,*)'  pair: ',n1,n2
                        write(8,*)'  separation =  ',d,' meters'
                        write(8,*)' '

                endif

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.52)THEN

            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))

            avelongitude = 0.d0                        !jha
            DO I=1,LandBoundaries(J)%NumNodes

                READ(1,*) LandBoundaries(J)%Entry1(I)
                ijk = LandBoundaries(J)%Entry1(I)
                avelongitude = avelongitude + GridInfo1(ijk)%XCoord

            ENDDO
            avelongitude = avelongitude / LandBoundaries(J)%NumNodes

            write(*,*)' '	                		!jha
            write(*,*)' Encountered river boundary condition'
                write(*,*)'  at BC segment number ',J
            write(*,*)'  Average longitude for this river = ',avelongitude
            write(*,*)' '
            do I=1,5
            if(riverlongitude(i) .ne. 0.d0)then
                if(avelongitude .gt. riverlongitude(i))then
                    write(*,*)' FATAL ERROR:'
                    write(*,*)' river BCs not organized east to west !'
                    write(*,*)' '
                    write(*,*)' '
                    stop
                endif
            else
                riverlongitude(i) = avelongitude
                exit
            endif
            enddo

        ELSE

            WRITE(*,'(A)',ADVANCE="YES") " "
            WRITE(*,'(A)',ADVANCE="YES") "ERROR!"
            WRITE(*,'(A,I4)',ADVANCE="YES") "J = ", J
            WRITE(*,'(A,I10)',ADVANCE="YES") "LandBoundaries(J)%Code = ", LandBoundaries(J)%Code 

        ENDIF

    ENDDO

!   READ(1,*) NumGenBoundaries
!   BACKSPACE(1)
!   READ(1,'(A)') NumGenBoundariesC
!   inner37: DO I=1,LEN(NumGenBoundariesC)
!       IF(NumGenBoundariesC(I:I).EQ."=")THEN
!           StringLen = I+2
!           EXIT inner37
!       ENDIF
!   ENDDO inner37
!   TempC = NumGenBoundariesC(StringLen:LEN(NumGenBoundariesC))
!   NumGenBoundariesC = TempC
!   READ(1,*) TotNumGenBoundaryNodes
!   BACKSPACE(1)
!   READ(1,'(A)') TotNumGenBoundaryNodesC
!   inner18: DO I=1,LEN(TotNumGenBoundaryNodesC)
!       IF(TotNumGenBoundaryNodesC(I:I).EQ."=")THEN
!           StringLen = I+2
!           EXIT inner18
!       ENDIF
!   ENDDO inner18
!   TempC = TotNumGenBoundaryNodesC(StringLen:LEN(TotNumGenBoundaryNodesC))
!   TotNumGenBoundaryNodesC = TempC

!   ALLOCATE(GenBoundaries(1:NumGenBoundaries))

!   DO J=1,NumGenBoundaries

!       READ(1,*) GenBoundaries(J)%NumNodes
!       BACKSPACE(1)
!       READ(1,'(A)') GenBoundaries(J)%Header 
!       inner19: DO I=1,LEN(GenBoundaries(J)%Header)
!           IF(GenBoundaries(J)%Header(I:I).EQ."=")THEN
!               StringLen = I+2
!               EXIT inner19
!           ENDIF
!       ENDDO inner19
!       TempC = GenBoundaries(J)%Header(StringLen:LEN(GenBoundaries(J)%Header))
!       GenBoundaries(J)%Header = TempC

!       ALLOCATE(GenBoundaries(J)%Entry1(1:GenBoundaries(J)%NumNodes)) 

!       DO I=1,GenBoundaries(J)%NumNodes

!           READ(1,*) GenBoundaries(J)%Entry1(I)

!       ENDDO

!   ENDDO

    CLOSE(UNIT=1,STATUS="KEEP")

    WRITE(*,'(A)',ADVANCE="YES") "The grid file was read successfully."

END SUBROUTINE


SUBROUTINE ElementSize(n1,n2,n3,dmin)

    USE DATA

    IMPLICIT NONE

    REAL*8 x1,x2,x3,y1,y2,y3,d,dmin
    INTEGER n1,n2,n3

    x1 = GridInfo1(n1)%XCoord
    x2 = GridInfo1(n2)%XCoord
    x3 = GridInfo1(n3)%XCoord
    y1 = GridInfo1(n1)%YCoord
    y2 = GridInfo1(n2)%YCoord
    y3 = GridInfo1(n3)%YCoord
    dmin = 1000000.d0

    call distance(x1,y1,x2,y2,d)
    if (d .lt. dmin)then
	dmin = d
    endif

    call distance(x1,y1,x3,y3,d)
    if (d .lt. dmin)then
	dmin = d
    endif

    call distance(x2,y2,x3,y3,d)
    if (d .lt. dmin)then
	dmin = d
    endif

    return

END SUBROUTINE

SUBROUTINE distance(lon1,lat1,lon2,lat2,d)

    implicit none
    real*8 a,c,R,d, dlon, dlat
    real*8 lat1,lat2,lon1,lon2,rad2deg
    integer k


    R=6371.64d0                    !radius of earth at mid-latitudes

    rad2deg = 45.d0/atan(1.d0)
    dlon = (lon2 - lon1)/rad2deg
    dlat = (lat2 - lat1)/rad2deg
    a = sin(dlat/2.d0)*sin(dlat/2.d0)
    a = a + cos(lat1/rad2deg) * cos(lat2/rad2deg) * sin(dlon/2.d0)**2
    c = 2.0 * atan2( sqrt(a), sqrt(1.d0-a) )
    d = R * c  ! in km

    return

END SUBROUTINE


SUBROUTINE WriteGrid

    USE DATA

    IMPLICIT NONE

    CHARACTER(LEN=100) :: OutputFile

    INTRINSIC          :: INT
    INTRINSIC          :: LEN

    INTEGER            :: I
    INTEGER            :: J

    WRITE(*,'(A)',ADVANCE="YES") " "
    WRITE(*,'(A)',ADVANCE="NO") "Enter name of output grid file: "
    READ(*,*) OutputFile

    OPEN(UNIT=2,FILE=OutputFile,ACTION="WRITE")

    WRITE(2,10) Header
10  FORMAT(A32)
    WRITE(2,*) NumElems, NumNodes
!20  FORMAT(1X,I8,1X,I8)

    DO I=1,NumNodes

        WRITE(2,30) GridInfo1(I)%Node, GridInfo1(I)%XCoord, &
                  GridInfo1(I)%YCoord, GridInfo1(I)%Bath
30      FORMAT(I8,2(1X,F14.10),1X,E16.8)

    ENDDO

    DO I=1,NumElems

        WRITE(2,40) GridInfo2(I)%Element, &
                  GridInfo2(I)%Nodes(1), GridInfo2(I)%Nodes(2), &
                  GridINfo2(I)%Nodes(3)
40      FORMAT(1X,I8,' 3',3(1X,I8))

    ENDDO

!   WRITE(2,'(I1,A,A40)') NumOpenBoundaries, " = ", NumOpenBoundariesC
!   WRITE(2,'(I3,A,A40)') TotNumOpenBoundaryNodes, " = ", TotNumOpenBoundaryNodesC
    WRITE(2,*) NumOpenBoundaries, "  ! NOPE"
    WRITE(2,*) TotNumOpenBoundaryNodes, "  ! NETA"

    DO J=1,NumOpenBoundaries

!       WRITE(2,'(I3,A,A40)') OpenBoundaries(J)%NumNodes, " = ", OpenBoundaries(J)%Header
        WRITE(2,*) OpenBoundaries(J)%NumNodes

        DO I=1,OpenBoundaries(J)%NumNodes

!           WRITE(2,'(I7)') INT(OpenBoundaries(J)%Entry1(I))
            WRITE(2,*) INT(OpenBoundaries(J)%Entry1(I))

        ENDDO

    ENDDO

!   WRITE(2,'(I4,A,A40)') NumLandBoundaries, " = ", NumLandBoundariesC
!   WRITE(2,'(I6,A,A40)') TotNumLandBoundaryNodes, " = ", TotNumLandBoundaryNodesC
    WRITE(2,*) NumLandBoundaries, "   ! NBOU"
    WRITE(2,*) TotNumLandBoundaryNodes, "   ! NVEL"

    DO J=1,NumLandBoundaries

        WRITE(2,50) LandBoundaries(J)%NumNodes, LandBoundaries(J)%Code, J
50      FORMAT(I6,1X,I3,7X,'! seg = ',I4)

        IF(LandBoundaries(J)%Code.EQ.0)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,'(I8)') INT(LandBoundaries(J)%Entry1(I))

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.1)THEN

	WRITE(*,*)'ibt = 1 found for segment ',j
	WRITE(8,*)'ibt = 1 found for segment ',j
            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,'(I8)') INT(LandBoundaries(J)%Entry1(I))

            ENDDO

        ELSEIF(INT(LandBoundaries(J)%Code).EQ.10)THEN

	WRITE(*,*)'ibt = 10 found for segment ',j
	WRITE(8,*)'ibt = 10 found for segment ',j

            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,'(I8)') INT(LandBoundaries(J)%Entry1(I))

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.11)THEN

	WRITE(*,*)'ibt = 11 found for segment ',j
	WRITE(8,*)'ibt = 11 found for segment ',j
            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,'(I8)') INT(LandBoundaries(J)%Entry1(I))

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.12)THEN

	WRITE(*,*)'ibt = 12 found for segment ',j
	WRITE(8,*)'ibt = 12 found for segment ',j
            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,'(I8)') INT(LandBoundaries(J)%Entry1(I))

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.13)THEN
	WRITE(*,*)'ibt = 13 found for segment ',j
	WRITE(8,*)'ibt = 13 found for segment ',j

            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,60) INT(LandBoundaries(J)%Entry1(I)), &
                          LandBoundaries(J)%Entry2(I) 
60              FORMAT(1X,I8,2X,F12.6,2X,'1.00') 

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.20)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,'(I8)') INT(LandBoundaries(J)%Entry1(I))

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.21)THEN
	WRITE(*,*)'ibt = 21 found for segment ',j
	WRITE(8,*)'ibt = 21 found for segment ',j

            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,'(I8)') INT(LandBoundaries(J)%Entry1(I))

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.22)THEN
	WRITE(*,*)'ibt = 22 found for segment ',j
	WRITE(8,*)'ibt = 22 found for segment ',j

            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,'(I8)') INT(LandBoundaries(J)%Entry1(I))

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.23)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,70) INT(LandBoundaries(J)%Entry1(I)), &
                          LandBoundaries(J)%Entry2(I)
70              FORMAT(1X,I8,2X,F12.6,2X,'1.00') 

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.24)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,80) INT(LandBoundaries(J)%Entry1(I)), &
                          INT(LandBoundaries(J)%Entry2(I)), & 
                          LandBoundaries(J)%Entry3(I)
80              FORMAT(1X,I8,2X,I8,2X,F12.6,' 1.00',' 1.00') 

            ENDDO

        ELSEIF(LandBoundaries(J)%Code.EQ.52)THEN

            DO I=1,LandBoundaries(J)%NumNodes

                WRITE(2,'(I8)') INT(LandBoundaries(J)%Entry1(I))

            ENDDO

        ENDIF

    ENDDO

!   WRITE(2,'(I2,A,A40)') NumGenBoundaries, " = ", NumGenBoundariesC
!   WRITE(2,'(I4,A,A40)') TotNumGenBoundaryNodes, " = ", TotNumGenBoundaryNodesC

!   DO J=1,NumGenBoundaries

!       WRITE(2,'(I7,A,A40)') GenBoundaries(J)%NumNodes, " = ", GenBoundaries(J)%Header

!       DO I=1,GenBoundaries(J)%NumNodes

!           WRITE(2,'(I7)') INT(GenBoundaries(J)%Entry1(I))

!       ENDDO

!   ENDDO

    CLOSE(UNIT=2,STATUS="KEEP")

    WRITE(*,'(A)',ADVANCE="YES") "The grid file was written successfully."

END SUBROUTINE
!

!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
SUBROUTINE ElementCheck
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   USE DATA, ONLY: NumNodes, NumElems, GridInfo2, GridInfo1, nean, numean
   IMPLICIT NONE
   INTEGER :: i,j,l,m,n, n1, n2, m1, m2, i1, i2, icount
   INTEGER, ALLOCATABLE :: ndelmlist(:)
!
   write(*,*) 
   write(*,*)  
   write(*,*)  
   write(*,*)  
   write(*,*) 
   write(*,*) 'START SEARCHING THE OVERLAPPING ELEMENT +++++++++++++++++++++++++++++++++++++++'
   ALLOCATE( numean(NumNodes), ndelmlist(NumElems) )
!
! Make the table of elements around a node.
   numean(:) = 0
   do m = 1, NumElems
      do i = 1, 3
         n = GridInfo2(m)%Nodes(i)
         numean(n) = numean(n) + 1
      enddo
   enddo
   ALLOCATE( nean(NumNodes,INT(MAXVAL(numean(:)))) )
   numean(:) = 0
   do m = 1, NumElems
      do i = 1, 3
         n = GridInfo2(m)%Nodes(i)
         numean(n) = numean(n) + 1
         nean(n,numean(n)) = m
      enddo
   enddo
!
!
   ndelmlist(:) = 0
   do m = 1, NumElems
      do l = 1, 3
         call ELine(m, l, n1, n2)
         icount = 0
         do i1 = 1, numean(n1)
            m1 = nean(n1,i1)
            do i2 = 1, numean(n2)
               m2 = nean(n2,i2)
               if( m1 == m2 ) then
                  icount = icount + 1
                  exit
               endif
            enddo
         enddo
         if ( icount > 2 ) ndelmlist(m) = 1
      enddo
   enddo
!
   if( sum(ndelmlist(:),1) == 0 ) then
     write(*,*) ' OK!!!! There are no Overlapping Elements! '
   else
     icount = 0
     do m = 1, NumElems
        if( ndelmlist(m) /= 0 ) then
            icount = icount + 1
            ndelmlist(icount) = m
! Output for simple check using GNUPlot
!            do i = 1, 3
!               write(100,       *) GridInfo1(GridInfo2(m)%Nodes(i))%XCoord,GridInfo1(GridInfo2(m)%Nodes(i))%YCoord
!               write(100+icount,*) GridInfo1(GridInfo2(m)%Nodes(i))%XCoord,GridInfo1(GridInfo2(m)%Nodes(i))%YCoord
!            enddo
!               write(100,       *) GridInfo1(GridInfo2(m)%Nodes(1))%XCoord,GridInfo1(GridInfo2(m)%Nodes(1))%YCoord
!               write(100+icount,*) GridInfo1(GridInfo2(m)%Nodes(1))%XCoord,GridInfo1(GridInfo2(m)%Nodes(1))%YCoord
!               write(100,       *) 
!               write(100+icount,*) 
        endif
     enddo
     write(*,*)  'Overlapping Elements are included in following Elements list;'
     write(*,100) (ndelmlist(i), i = 1, icount )
   endif
100 format(10i10)
   DEALLOCATE ( ndelmlist )
     
END SUBROUTINE ElementCheck
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
SUBROUTINE ELine(m, i, n1, n2)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   Use DATA, ONLY: GridInfo2
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: m, i
   INTEGER, INTENT(OUT) :: n1, n2
!
   SELECT CASE(i)
          CASE(1)
             n1 = GridInfo2(m)%Nodes(1)
             n2 = GridInfo2(m)%Nodes(2)
          CASE(2)
             n1 = GridInfo2(m)%Nodes(2)
             n2 = GridInfo2(m)%Nodes(3)
          CASE(3)
             n1 = GridInfo2(m)%Nodes(3)
             n2 = GridInfo2(m)%Nodes(1)
   END SELECT
!
END SUBROUTINE ELine
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
SUBROUTINE BoundaryCheck
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   USE DATA, ONLY: NumNodes, NumElems, GridInfo2, GridInfo1,                    &
                   NumOpenBoundaries, TotNumOpenBoundaryNodes, OpenBoundaries,  &
                   NumLandBoundaries, TotNumLandBoundaryNodes, LandBoundaries,  &
                   nean, numean
   IMPLICIT NONE
   INTEGER :: nb, nbl
   INTEGER, ALLOCATABLE :: nprop(:), nprop2(:), nbline(:,:)
   INTEGER :: i, j, k, l, m, n, i1, i2, m1, m2, n1, n2, icount, icheck
!
   ALLOCATE ( nprop(NumNodes), nprop2(NumNodes) )
!
   nprop(:) = 0
   do m = 1, NumElems
      nprop(GridInfo2(m)%Nodes(1)) = nprop(GridInfo2(m)%Nodes(1)) + GridInfo2(m)%Nodes(2) - GridInfo2(m)%Nodes(3)
      nprop(GridInfo2(m)%Nodes(2)) = nprop(GridInfo2(m)%Nodes(2)) + GridInfo2(m)%Nodes(3) - GridInfo2(m)%Nodes(1)
      nprop(GridInfo2(m)%Nodes(3)) = nprop(GridInfo2(m)%Nodes(3)) + GridInfo2(m)%Nodes(1) - GridInfo2(m)%Nodes(2)
   enddo
!
   nb = 0
   do n = 1, NumNodes
      if ( nprop(n) /= 0 ) then
         nb = nb + 1
         nprop(n) = 1
      endif
   enddo
!
   ALLOCATE( nbline(2,int(nb*1.5)) )
   nbl = 0
   do m = 1, NumElems
      do l = 1, 3
         call ELine(m, l, n1, n2)
         icount = 0
         do i1 = 1, numean(n1)
            m1 = nean(n1,i1)
            do i2 = 1, numean(n2)
               m2 = nean(n2,i2)
               if( (m1 == m2) .and. (m1 /= m) ) then
                  icount = icount + 1
                  exit
               endif
            enddo
         enddo
         if ( icount == 0 ) then
            nbl = nbl + 1
            nbline(1,nbl) = n1
            nbline(2,nbl) = n2
         endif
      enddo
   enddo
!
   write(*,*) 
   write(*,*) 
   write(*,*) 'START the checking, Are Nodes of BC data on Boundary?  ++++++++++++++++++++++++'
   nprop2(:) = 0
   icheck = 0
   do i = 1, NumOpenBoundaries
      do j = 1, OpenBoundaries(i)%NumNodes-1
         n1 = Openboundaries(i)%Entry1(j)
         n2 = Openboundaries(i)%Entry1(j+1)
         nprop2(n1) = 1
         nprop2(n2) = 1
         icount = nprop(n1) + nprop(n2)
         if( icount /= 2 ) then
            write(*,'(a20,i10,a6,i10,a1,i10,a29)') 'OpenBoundary Number:',i, ' Edge:',n1,'-',n2, '(<=Node#) are not on Boundary'
            icheck = icheck + 1
         endif
      enddo
   enddo
   do i = 1, NumLandBoundaries
      do j = 1, int(LandBoundaries(i)%NumNodes)-1
         n1 = int(LandBoundaries(i)%Entry1(j))
         n2 = int(LandBoundaries(i)%Entry1(j+1))
         nprop2(n1) = 1
         nprop2(n2) = 1
         icount = nprop(n1) + nprop(n2)
         if( icount /= 2 ) then
            write(*,'(a20,i10,a6,i10,a1,i10,a29)') 'LandBoundary Number:',i, ' Edge:',n1,'-',n2, '(<=Node#) are not on Boundary'
            icheck = icheck + 1
         endif
         if( (int(LandBoundaries(i)%Code) == 4) .or. (int(LandBoundaries(i)%Code)==24) ) then
            n1 = int(LandBoundaries(i)%Entry2(j))
            n2 = int(LandBoundaries(i)%Entry2(j+1))
            nprop2(n1) = 1
            nprop2(n2) = 1
            icount = nprop(n1) + nprop(n2)
            if( icount /= 2 ) then
               write(*,'(a20,i10,a6,i10,a1,i10,a29)') 'LandBoundary Number:',i, ' Edge:',n1,'-',n2, '(<=Node#) are not on Boundary '
               icheck = icheck + 1
            endif
        endif
      enddo
         n1 = int(LandBoundaries(i)%Entry1(1))
         n2 = int(LandBoundaries(i)%Entry1(int(LandBoundaries(i)%NumNodes)))
         nprop2(n1) = 2
         nprop2(n2) = 2
         if( (int(LandBoundaries(i)%Code) == 4) .or. (int(LandBoundaries(i)%Code)==24) ) then
            n1 = int(LandBoundaries(i)%Entry2(1))
            n2 = int(LandBoundaries(i)%Entry2(int(LandBoundaries(i)%NumNodes)))
            nprop2(n1) = 2
            nprop2(n2) = 2
         endif
   enddo
   if( icheck == 0 ) write(*,*) '  O.K.!!!!!!!!!!!!!!!!!     ALL Nodes of BC data are on Boundary'
!
   write(*,*)
   write(*,*)
   write(*,*) 'START the checking, Are Boudary Edge Imposed Boundary Condition? ++++++++++++++'
   icheck = 0
   do l = 1, nbl
      icount = 0
      do j = 1, 2
         icount = icount + nprop2(nbline(j,l))
      enddo
      if( icount <= 1 ) then
        write(*,'(a14,i10,a6,i10,a1,i10,a27)') '0 Edge Number:',l, &
             ' Edge:',nbline(1,l),'-',nbline(2,l), '(<=Node#) is not imposed BC'
        icheck = icheck + 1
      endif
   enddo
   if( icheck == 0 ) write(*,*) '  O.K.!!!!!!!!!!!!!!!!!     ALL Boundary Nodes are imposed Boundary Condition'
   write(*,*)  
   write(*,*)  
   write(*,*)  
   write(*,*)  
!
   DEALLOCATE ( nprop, nprop2, nbline )
!
END SUBROUTINE BoundaryCheck
!

!...Check Added for Overlapping boundary conditions which short out the iterative solver
!   Z. Cobell
!
SUBROUTINE CheckOverlappingBCs
            USE DATA, ONLY: NumNodes, NumElems, GridInfo2, GridInfo1,                    &
                           NumOpenBoundaries, TotNumOpenBoundaryNodes, OpenBoundaries,  &
                           NumLandBoundaries, TotNumLandBoundaryNodes, LandBoundaries,  &
                           nean, numean
!$          USE OMP_LIB
            IMPLICIT NONE
   
            INTEGER,ALLOCATABLE :: UsedNodes(:)
            LOGICAL,ALLOCATABLE :: UsedEdges(:,:) 
            INTEGER             :: I
            INTEGER             :: J
            INTEGER             :: K
            INTEGER             :: L
            INTEGER             :: Y
            INTEGER             :: Z
            INTEGER             :: BCError
            INTEGER             :: N(4)
            INTEGER             :: tN(4)
            
!$          IF(OMP_GET_MAX_THREADS().GT.0)THEN
!$                WRITE(*,'(A,I0,A)') "Begin checking for overlapping boundary conditions"//&
!$                               " using ",OMP_GET_MAX_THREADS()," cores."
!$          ELSE
                  WRITE(*,'(A)') "Begin checking for overlapping Boundary conditions..."
!$          ENDIF

            !...First check if a node is involved in more than 2 BCs
            ALLOCATE(UsedNodes(1:NumNodes))
            UsedNodes(:) = 0
            DO I = 1,NumLandBoundaries
                DO J = 1,LandBoundaries(I)%NumNodes
                    IF(LandBoundaries(I)%Code.EQ.24)THEN
                        UsedNodes(INT(LandBoundaries(I)%Entry1(J))) = &
                            UsedNodes(INT(LandBoundaries(I)%Entry1(J))) + 1
                        UsedNodes(INT(LandBoundaries(I)%Entry2(J))) = &
                            UsedNodes(INT(LandBoundaries(I)%Entry2(J))) + 1
                    ELSE
                        UsedNodes(INT(LandBoundaries(I)%Entry1(J))) = &
                            UsedNodes(INT(LandBoundaries(I)%Entry1(J))) + 1
                    ENDIF
                ENDDO
            ENDDO
            
            BCError = 0
            DO I = 1,NumNodes
                IF(UsedNodes(I).GT.2)THEN
                    WRITE(*,'(A,I0,A,I0,A)') "Boundary Condition Warning: Node ",I," involed in ",UsedNodes(I)," BCs!"
                ENDIF
            ENDDO
            
            N(:) = 0
            tN(:) = 0
            !$OMP PARALLEL PRIVATE(I,J,K,L,N,tN) &
            !$OMP          SHARED(BCError,LandBoundaries,NumLandBoundaries)
            !$OMP DO SCHEDULE(DYNAMIC)
            DO I = 1,NumLandBoundaries
                DO J = 1,LandBoundaries(I)%NumNodes-1
                    IF(LandBoundaries(I)%Code.NE.24)THEN
                        N(1) = INT(LandBoundaries(I)%Entry1(J))
                        N(2) = INT(LandBoundaries(I)%Entry1(J+1))
                        DO K = 1,NumLandBoundaries
                            DO L = 1,LandBoundaries(K)%NumNodes-1
                                IF(K.EQ.I)CYCLE
                                IF(LandBoundaries(K)%Code.NE.24)THEN
                                    tN(1) = INT(LandBoundaries(K)%Entry1(L))
                                    tN(2) = INT(LandBoundaries(K)%Entry1(L+1))
                                    IF( ( N(1).EQ.tN(1) ).AND.( N(2).EQ.tN(2) ) .OR.&
                                        ( N(2).EQ.tN(1) ).AND.( N(1).EQ.tN(2) ) ) THEN
                                            WRITE(*,'(A,I0,A,I0)') "ERROR! Land Boundary ",I,&
                                                " in error with Land Boundary ",K
                                            WRITE(*,'(A,I0,2X,I0)')      "       Check nodes: ",N(1),N(2)
                                            BCError = BCError + 1
                                    ENDIF
                                ELSE
                                    tN(1) = INT(LandBoundaries(K)%Entry1(L))
                                    tN(2) = INT(LandBoundaries(K)%Entry1(L+1))
                                    tN(3) = INT(LandBoundaries(K)%Entry2(L))
                                    tN(4) = INT(LandBoundaries(K)%Entry2(L+1))
                                    IF( ( N(1).EQ.tN(1) ).AND.( N(2).EQ.tN(2) ) .OR.&
                                        ( N(2).EQ.tN(1) ).AND.( N(1).EQ.tN(2) ) .OR.&
                                        ( N(1).EQ.tN(3) ).AND.( N(2).EQ.tN(4) ) .OR.&
                                        ( N(2).EQ.tN(3) ).AND.( N(1).EQ.tN(4) ))THEN
                                            WRITE(*,'(A,I0,A,I0)') "ERROR! Land Boundary ",I,&
                                                " in error with Land Boundary ",K
                                            WRITE(*,'(A,I0,2X,I0)')      "       Check nodes: ",N(1),N(2)
                                        BCError = BCError + 1
                                    ENDIF
                                ENDIF
                            ENDDO
                        ENDDO
                    ELSE
                        N(1) = INT(LandBoundaries(I)%Entry1(J))
                        N(2) = INT(LandBoundaries(I)%Entry1(J+1))
                        N(3) = INT(LandBoundaries(I)%Entry2(J))
                        N(4) = INT(LandBoundaries(I)%Entry2(J+1))
                        DO K = 1,NumLandBoundaries
                            DO L = 1,LandBoundaries(K)%NumNodes-1
                                IF(K.EQ.I)CYCLE
                                IF(LandBoundaries(K)%Code.EQ.24)THEN
                                    tN(1) = INT(LandBoundaries(K)%Entry1(L))
                                    tN(2) = INT(LandBoundaries(K)%Entry1(L+1))
                                    tN(3) = INT(LandBoundaries(K)%Entry2(L))
                                    tN(4) = INT(LandBoundaries(K)%Entry2(L+1))
                                    IF( ( N(1).EQ.tN(1) ).AND.( N(2).EQ.tN(2) ) .OR.&
                                        ( N(2).EQ.tN(1) ).AND.( N(1).EQ.tN(2) ) .OR.&
                                        ( N(1).EQ.tN(3) ).AND.( N(2).EQ.tN(4) ) .OR.&
                                        ( N(2).EQ.tN(3) ).AND.( N(1).EQ.tN(4) ) .OR.&
                                        ( N(3).EQ.tN(1) ).AND.( N(4).EQ.tN(2) ) .OR.&
                                        ( N(4).EQ.tN(1) ).AND.( N(3).EQ.tN(2) ) .OR.&
                                        ( N(3).EQ.tN(3) ).AND.( N(4).EQ.tN(4) ) .OR.&
                                        ( N(4).EQ.tN(3) ).AND.( N(3).EQ.tN(4) ) )THEN
                                            WRITE(*,'(A,I0,A,I0)') "ERROR! Land Boundary ",I,&
                                                " in error with Land Boundary ",K
                                            WRITE(*,'(A,4(I0,2X))')      "       Check nodes: ",&
                                                N(1),N(2),N(3),N(4)
                                            BCError = BCError + 1
                                    ENDIF
                                ELSE
                                    tN(1) = INT(LandBoundaries(K)%Entry1(L))
                                    tN(2) = INT(LandBoundaries(K)%Entry1(L+1))
                                    IF( ( tN(1).EQ.N(1) ).AND.( tN(2).EQ.N(2) ) .OR.&
                                        ( tN(2).EQ.N(1) ).AND.( tN(1).EQ.N(2) ) .OR.&
                                        ( tN(1).EQ.N(3) ).AND.( tN(2).EQ.N(4) ) .OR.&
                                        ( tN(2).EQ.N(3) ).AND.( tN(1).EQ.N(4) ))THEN
                                            WRITE(*,'(A,I0,A,I0)') "ERROR! Land Boundary ",I,&
                                                " in error with Land Boundary ",K
                                            WRITE(*,'(A,4(I0,2X))')      "       Check nodes: ",&
                                                N(1),N(2),N(3),N(4)
                                            BCError = BCError + 1
                                    ENDIF
                                ENDIF
                            ENDDO
                        ENDDO   
                    ENDIF
                ENDDO
            ENDDO
            !$OMP END DO
            !$OMP END PARALLEL
            
            IF(BCError.GT.0)THEN
                WRITE(*,'(A)') ""
                WRITE(*,'(A)') "FATAL ERROR: Overlapping Boundaries Detected!"
                STOP
            ELSE
                WRITE(*,'(A)') "No overlapping boundaries detected!"
            ENDIF
                
            
        END SUBROUTINE
   


!----------------------------------------------------------------------------------!
!                                                                                  !
!                   BELOW IS TAKEN FROM KDTREE2 ROUTINES                           !
!                                                                                  !
!           (c) Matthew Kennel, Institute for Nonlinear Science (2004)             !
!                                                                                  !
!----------------------------------------------------------------------------------!

          MODULE KDTREE2_PRECISION_MODULE
          INTEGER, PARAMETER :: SP = KIND(0.0)
          INTEGER, PARAMETER :: DP = KIND(0.0D0)

          PRIVATE :: SP, DP
          !INTEGER, PARAMETER :: KDKIND = SP
          INTEGER, PARAMETER :: KDKIND = DP
          PUBLIC :: KDKIND

          END MODULE KDTREE2_PRECISION_MODULE

          MODULE KDTREE2_PRIORITY_QUEUE_MODULE
          USE KDTREE2_PRECISION_MODULE

          TYPE KDTREE2_RESULT
             REAL(KDKIND)    :: DIS!=0.0
             INTEGER :: IDX!=-1   INITIALIZERS CAUSE SOME BUGS IN COMPILERS.
          END TYPE KDTREE2_RESULT

          TYPE PQ
             INTEGER :: HEAP_SIZE = 0
             TYPE(KDTREE2_RESULT), POINTER :: ELEMS(:)
          END TYPE PQ

          PUBLIC :: KDTREE2_RESULT

          PUBLIC :: PQ
          PUBLIC :: PQ_CREATE
          PUBLIC :: PQ_DELETE, PQ_INSERT
          PUBLIC :: PQ_EXTRACT_MAX, PQ_MAX, PQ_REPLACE_MAX, PQ_MAXPRI
          PRIVATE

          CONTAINS


          FUNCTION PQ_CREATE(RESULTS_IN) RESULT(RES)

          TYPE(KDTREE2_RESULT), TARGET:: RESULTS_IN(:)
          TYPE(PQ) :: RES

          INTEGER :: NALLOC

          NALLOC = SIZE(RESULTS_IN,1)
          IF (NALLOC .LT. 1) THEN
             WRITE (*,*) 'PQ_CREATE: ERROR, INPUT ARRAYS MUST BE ALLOCATED.'
          END IF
          RES%ELEMS => RESULTS_IN
          RES%HEAP_SIZE = 0
          RETURN
          END FUNCTION PQ_CREATE

          SUBROUTINE HEAPIFY(A,I_IN)
          TYPE(PQ),POINTER   :: A
          INTEGER, INTENT(IN) :: I_IN
          !
          INTEGER :: I, L, R, LARGEST

          REAL(KDKIND)    :: PRI_I, PRI_L, PRI_R, PRI_LARGEST

          TYPE(KDTREE2_RESULT) :: TEMP

          I = I_IN

          BIGLOOP:  DO
             L = 2*I ! LEFT(I)
             R = L+1 ! RIGHT(I)

             IF (L .GT. A%HEAP_SIZE) THEN
                EXIT
             ELSE
                PRI_I = A%ELEMS(I)%DIS
                PRI_L = A%ELEMS(L)%DIS
                IF (PRI_L .GT. PRI_I) THEN
                   LARGEST = L
                   PRI_LARGEST = PRI_L
                ELSE
                   LARGEST = I
                   PRI_LARGEST = PRI_I
                ENDIF
                IF (R .LE. A%HEAP_SIZE) THEN
                   PRI_R = A%ELEMS(R)%DIS
                   IF (PRI_R .GT. PRI_LARGEST) THEN
                      LARGEST = R
                   ENDIF
                ENDIF
             ENDIF

             IF (LARGEST .NE. I) THEN
                TEMP = A%ELEMS(I)
                A%ELEMS(I) = A%ELEMS(LARGEST)
                A%ELEMS(LARGEST) = TEMP
                I = LARGEST
                CYCLE BIGLOOP
             ELSE
                RETURN
             END IF
          ENDDO BIGLOOP
          RETURN
          END SUBROUTINE HEAPIFY

          SUBROUTINE PQ_MAX(A,E)

          TYPE(PQ),POINTER :: A
          TYPE(KDTREE2_RESULT),INTENT(OUT)  :: E

          IF (A%HEAP_SIZE .GT. 0) THEN
             E = A%ELEMS(1)
          ELSE
             WRITE (*,*) 'PQ_MAX: ERROR, HEAP_SIZE < 1'
             STOP
          ENDIF
          RETURN
          END SUBROUTINE PQ_MAX

          REAL(KDKIND) FUNCTION PQ_MAXPRI(A)
          TYPE(PQ), POINTER :: A

          IF (A%HEAP_SIZE .GT. 0) THEN
             PQ_MAXPRI = A%ELEMS(1)%DIS
          ELSE
             WRITE (*,*) 'PQ_MAX_PRI: ERROR, HEAPSIZE < 1'
             STOP
          ENDIF
          RETURN
          END FUNCTION PQ_MAXPRI

          SUBROUTINE PQ_EXTRACT_MAX(A,E)
          TYPE(PQ),POINTER :: A
          TYPE(KDTREE2_RESULT), INTENT(OUT) :: E

          IF (A%HEAP_SIZE .GE. 1) THEN
             E = A%ELEMS(1)
             A%ELEMS(1) = A%ELEMS(A%HEAP_SIZE)
             A%HEAP_SIZE = A%HEAP_SIZE-1
             CALL HEAPIFY(A,1)
             RETURN
          ELSE
             WRITE (*,*) 'PQ_EXTRACT_MAX: ERROR,',&
                        ' ATTEMPTED TO POP NON-POSITIVE PQ'
             STOP
          END IF

          END SUBROUTINE PQ_EXTRACT_MAX


          REAL(KDKIND) FUNCTION PQ_INSERT(A,DIS,IDX)

          TYPE(PQ),POINTER  :: A
          REAL(KDKIND), INTENT(IN) :: DIS
          INTEGER, INTENT(IN) :: IDX
          INTEGER :: I, ISPARENT
          REAL(KDKIND)    :: PARENTDIS
          A%HEAP_SIZE = A%HEAP_SIZE + 1
          I = A%HEAP_SIZE

          DO WHILE (I .GT. 1)
             ISPARENT = INT(I/2)
             PARENTDIS = A%ELEMS(ISPARENT)%DIS
             IF (DIS .GT. PARENTDIS) THEN
                A%ELEMS(I)%DIS = PARENTDIS
                A%ELEMS(I)%IDX = A%ELEMS(ISPARENT)%IDX
                I = ISPARENT
             ELSE
                EXIT
             ENDIF
          END DO

          A%ELEMS(I)%DIS = DIS
          A%ELEMS(I)%IDX = IDX

          PQ_INSERT = A%ELEMS(1)%DIS
          RETURN

          END FUNCTION PQ_INSERT

          SUBROUTINE PQ_ADJUST_HEAP(A,I)
          TYPE(PQ),POINTER  :: A
          INTEGER, INTENT(IN) :: I
          REAL(KDKIND) :: PRICHILD
          INTEGER :: PARENT, CHILD, N

          TYPE(KDTREE2_RESULT) :: E

          E = A%ELEMS(I)

          PARENT = I
          CHILD = 2*I
          N = A%HEAP_SIZE

          DO WHILE (CHILD .LE. N)
             IF (CHILD .LT. N) THEN
                IF (A%ELEMS(CHILD)%DIS .LT. A%ELEMS(CHILD+1)%DIS) THEN
                   CHILD = CHILD+1
                ENDIF
             ENDIF
             PRICHILD = A%ELEMS(CHILD)%DIS
             IF (E%DIS .GE. PRICHILD) THEN
                EXIT
             ELSE
                A%ELEMS(PARENT) = A%ELEMS(CHILD)
                PARENT = CHILD
                CHILD = 2*PARENT
             END IF
          END DO
          A%ELEMS(PARENT) = E
          RETURN
          END SUBROUTINE PQ_ADJUST_HEAP


          REAL(KDKIND) FUNCTION PQ_REPLACE_MAX(A,DIS,IDX)
          TYPE(PQ),POINTER         :: A
          REAL(KDKIND), INTENT(IN) :: DIS
          INTEGER, INTENT(IN) :: IDX
          INTEGER :: PARENT, CHILD, N
          REAL(KDKIND)    :: PRICHILD, PRICHILDP1

          TYPE(KDTREE2_RESULT) :: ETMP

          IF (.TRUE.) THEN
             N=A%HEAP_SIZE
             IF (N .GE. 1) THEN
                PARENT =1
                CHILD=2

                LOOP: DO WHILE (CHILD .LE. N)
                   PRICHILD = A%ELEMS(CHILD)%DIS
                   IF (CHILD .LT. N) THEN
                      PRICHILDP1 = A%ELEMS(CHILD+1)%DIS
                      IF (PRICHILD .LT. PRICHILDP1) THEN
                        CHILD = CHILD+1
                        PRICHILD = PRICHILDP1
                      ENDIF
                   ENDIF

                   IF (DIS .GE. PRICHILD) THEN
                      EXIT LOOP
                   ELSE
                      A%ELEMS(PARENT) = A%ELEMS(CHILD)
                      PARENT = CHILD
                      CHILD = 2*PARENT
                   END IF
                END DO LOOP
                A%ELEMS(PARENT)%DIS = DIS
                A%ELEMS(PARENT)%IDX = IDX
                PQ_REPLACE_MAX = A%ELEMS(1)%DIS
             ELSE
                A%ELEMS(1)%DIS = DIS
                A%ELEMS(1)%IDX = IDX
                PQ_REPLACE_MAX = DIS
             ENDIF
          ELSE
             CALL PQ_EXTRACT_MAX(A,ETMP)
             ETMP%DIS = DIS
             ETMP%IDX = IDX
             PQ_REPLACE_MAX = PQ_INSERT(A,DIS,IDX)
          ENDIF
          RETURN
          END FUNCTION PQ_REPLACE_MAX

          SUBROUTINE PQ_DELETE(A,I)
          TYPE(PQ),POINTER :: A
          INTEGER           :: I

          IF ((I .LT. 1) .OR. (I .GT. A%HEAP_SIZE)) THEN
             WRITE (*,*) 'PQ_DELETE: ERROR, ATTEMPT TO REMOVE',&
                        ' OUT OF BOUNDS ELEMENT.'
             STOP
          ENDIF

          A%ELEMS(I) = A%ELEMS(A%HEAP_SIZE)
          A%HEAP_SIZE = A%HEAP_SIZE - 1

          CALL HEAPIFY(A,I)

          END SUBROUTINE PQ_DELETE

          END MODULE KDTREE2_PRIORITY_QUEUE_MODULE


          MODULE KDTREE2_MODULE

          USE KDTREE2_PRECISION_MODULE
          USE KDTREE2_PRIORITY_QUEUE_MODULE

          PUBLIC :: KDKIND
          PUBLIC :: KDTREE2, KDTREE2_RESULT, TREE_NODE
          PUBLIC :: KDTREE2_CREATE, KDTREE2_DESTROY
          PUBLIC :: KDTREE2_N_NEAREST,KDTREE2_N_NEAREST_AROUND_POINT
          PUBLIC :: KDTREE2_R_NEAREST, KDTREE2_R_NEAREST_AROUND_POINT
          PUBLIC :: KDTREE2_SORT_RESULTS
          PUBLIC :: KDTREE2_R_COUNT, KDTREE2_R_COUNT_AROUND_POINT
          PUBLIC :: KDTREE2_N_NEAREST_BRUTE_FORCE
          PUBLIC :: KDTREE2_R_NEAREST_BRUTE_FORCE

          INTEGER, PARAMETER :: BUCKET_SIZE = 12

          TYPE INTERVAL
          REAL(KDKIND) :: LOWER,UPPER
          END TYPE INTERVAL

          TYPE :: TREE_NODE
             PRIVATE
             INTEGER :: CUT_DIM
             REAL(KDKIND) :: CUT_VAL
             REAL(KDKIND) :: CUT_VAL_LEFT, CUT_VAL_RIGHT
             INTEGER :: L, U
             TYPE (TREE_NODE), POINTER :: LEFT, RIGHT
             TYPE(INTERVAL), POINTER :: BOX(:) => NULL()
          END TYPE TREE_NODE

          TYPE :: KDTREE2
             INTEGER :: DIMEN=0, N=0
             REAL(KDKIND), POINTER :: THE_DATA(:,:) => NULL()
             INTEGER, POINTER :: IND(:) => NULL()
             LOGICAL       :: SORT = .FALSE.
             LOGICAL       :: REARRANGE = .FALSE.
             REAL(KDKIND), POINTER :: REARRANGED_DATA(:,:) => NULL()
             TYPE (TREE_NODE), POINTER :: ROOT => NULL()
          END TYPE KDTREE2


          TYPE :: TREE_SEARCH_RECORD

             PRIVATE

             INTEGER           :: DIMEN
             INTEGER           :: NN, NFOUND
             REAL(KDKIND)      :: BALLSIZE
             INTEGER           :: CENTERIDX=999, CORRELTIME=9999
             INTEGER           :: NALLOC
             LOGICAL           :: REARRANGE
             LOGICAL           :: OVERFLOW
             REAL(KDKIND), POINTER :: QV(:)
             TYPE(KDTREE2_RESULT), POINTER :: RESULTS(:)
             TYPE(PQ) :: PQ
             REAL(KDKIND), POINTER :: DATA(:,:)
             INTEGER, POINTER      :: IND(:)
          END TYPE TREE_SEARCH_RECORD

          PRIVATE

          TYPE(TREE_SEARCH_RECORD), SAVE, TARGET :: SR

          CONTAINS

          FUNCTION KDTREE2_CREATE(INPUT_DATA,IDIM2,SORT,REARRANGE)RESULT(MR)
          TYPE (KDTREE2), POINTER :: MR
          INTEGER, INTENT(IN), OPTIONAL      :: IDIM2
          LOGICAL, INTENT(IN), OPTIONAL      :: SORT
          LOGICAL, INTENT(IN), OPTIONAL      :: REARRANGE
          REAL(KDKIND), TARGET :: INPUT_DATA(:,:)
          INTEGER :: I
          ALLOCATE (MR)
          MR%THE_DATA => INPUT_DATA

          IF (PRESENT(IDIM2)) THEN
             MR%DIMEN = IDIM2
          ELSE
             MR%DIMEN = SIZE(INPUT_DATA,1)
          END IF
          MR%N = SIZE(INPUT_DATA,2)

          IF (MR%DIMEN > MR%N) THEN
             WRITE (*,*) 'KD_TREE_TRANS: LIKELY USER ERROR.'
             WRITE (*,*) 'KD_TREE_TRANS: YOU PASSED IN MATRIX WITH D=',&
                         MR%DIMEN
             WRITE (*,*) 'KD_TREE_TRANS: AND N=',MR%N

             WRITE (*,*) 'KD_TREE_TRANS: NOTE, THAT NEW FORMAT IS',&
                        ' DATA(1:D,1:N)'
             WRITE (*,*) 'KD_TREE_TRANS: WITH USUALLY N >> D.  ',&
                        'IF N =APPROX= D, THEN A K-D TREE'
             WRITE (*,*) 'KD_TREE_TRANS: IS NOT AN APPROPRIATE DATA',&
                        ' STRUCTURE.'
             STOP
          END IF

          CALL BUILD_TREE(MR)

          IF (PRESENT(SORT)) THEN
             MR%SORT = SORT
          ELSE
             MR%SORT = .FALSE.
          ENDIF

          IF (PRESENT(REARRANGE)) THEN
             MR%REARRANGE = REARRANGE
          ELSE
             MR%REARRANGE = .TRUE.
          ENDIF

          IF (MR%REARRANGE) THEN
             ALLOCATE(MR%REARRANGED_DATA(MR%DIMEN,MR%N))
             DO I=1,MR%N
             MR%REARRANGED_DATA(:,I) = MR%THE_DATA(:,&
                   MR%IND(I))
             ENDDO
          ELSE
             NULLIFY(MR%REARRANGED_DATA)
          ENDIF

          END FUNCTION KDTREE2_CREATE

          SUBROUTINE BUILD_TREE(TP)
             TYPE (KDTREE2), POINTER :: TP
             ! ..
             INTEGER :: J
             TYPE(TREE_NODE), POINTER :: DUMMY => NULL()
             ! ..
             ALLOCATE (TP%IND(TP%N))
             FORALL (J=1:TP%N)
             TP%IND(J) = J
             END FORALL
             TP%ROOT => BUILD_TREE_FOR_RANGE(TP,1,TP%N, DUMMY)
          END SUBROUTINE BUILD_TREE

          RECURSIVE FUNCTION BUILD_TREE_FOR_RANGE(TP,L,U,PARENT)&
           RESULT (RES)
             TYPE (TREE_NODE), POINTER :: RES
             TYPE (KDTREE2), POINTER :: TP
             TYPE (TREE_NODE),POINTER           :: PARENT
             INTEGER, INTENT (IN) :: L, U
             INTEGER :: I, C, M, DIMEN
             LOGICAL :: RECOMPUTE
             REAL(KDKIND)    :: AVERAGE
             DIMEN = TP%DIMEN
             ALLOCATE (RES)
             ALLOCATE(RES%BOX(DIMEN))

             IF ( U < L ) THEN
               NULLIFY(RES)
               RETURN
             END IF

             IF ((U-L)<=BUCKET_SIZE) THEN
             DO I=1,DIMEN
                CALL SPREAD_IN_COORDINATE(TP,I,L,U,RES%BOX(I))
             END DO
             RES%CUT_DIM = 0
             RES%CUT_VAL = 0.0
             RES%L = L
             RES%U = U
             RES%LEFT =>NULL()
             RES%RIGHT => NULL()
             ELSE
             DO I=1,DIMEN
                RECOMPUTE=.TRUE.
                IF (ASSOCIATED(PARENT)) THEN
                  IF (I .NE. PARENT%CUT_DIM) THEN
                     RECOMPUTE=.FALSE.
                  END IF
                ENDIF
                IF (RECOMPUTE) THEN
                  CALL SPREAD_IN_COORDINATE(TP,I,L,U,RES%BOX(I))
                ELSE
                  RES%BOX(I) = PARENT%BOX(I)
                ENDIF
             END DO

             C = MAXLOC(RES%BOX(1:DIMEN)%UPPER-RES%BOX(1:DIMEN)%LOWER,1)

             IF (.FALSE.) THEN
                M = (L+U)/2
                CALL SELECT_ON_COORDINATE(TP%THE_DATA,TP%IND,C,M,L,U)
             ELSE
                IF (.TRUE.) THEN
                AVERAGE = SUM(TP%THE_DATA(C,TP%IND(L:U))) / &
                 REAL(U-L+1,KDKIND)
                ELSE
                AVERAGE = (RES%BOX(C)%UPPER + RES%BOX(C)%LOWER)/2.0
                ENDIF

                RES%CUT_VAL = AVERAGE
                M = SELECT_ON_COORDINATE_VALUE(&
                                        TP%THE_DATA,TP%IND,C,AVERAGE,L,U)
             ENDIF

             RES%CUT_DIM = C
             RES%L = L
             RES%U = U

             RES%LEFT => BUILD_TREE_FOR_RANGE(TP,L,M,RES)
             RES%RIGHT => BUILD_TREE_FOR_RANGE(TP,M+1,U,RES)

             IF (ASSOCIATED(RES%RIGHT) .EQV. .FALSE.) THEN
                RES%BOX = RES%LEFT%BOX
                RES%CUT_VAL_LEFT = RES%LEFT%BOX(C)%UPPER
                RES%CUT_VAL = RES%CUT_VAL_LEFT
             ELSEIF (ASSOCIATED(RES%LEFT) .EQV. .FALSE.) THEN
                RES%BOX = RES%RIGHT%BOX
                RES%CUT_VAL_RIGHT = RES%RIGHT%BOX(C)%LOWER
                RES%CUT_VAL = RES%CUT_VAL_RIGHT
             ELSE
                RES%CUT_VAL_RIGHT = RES%RIGHT%BOX(C)%LOWER
                RES%CUT_VAL_LEFT = RES%LEFT%BOX(C)%UPPER
                RES%CUT_VAL = (RES%CUT_VAL_LEFT + RES%CUT_VAL_RIGHT)/2

                RES%BOX%UPPER = MAX(RES%LEFT%BOX%UPPER,RES%RIGHT%BOX%UPPER)
                RES%BOX%LOWER = MIN(RES%LEFT%BOX%LOWER,RES%RIGHT%BOX%LOWER)
             ENDIF
             END IF
          END FUNCTION BUILD_TREE_FOR_RANGE

          INTEGER FUNCTION SELECT_ON_COORDINATE_VALUE(V,IND,C,ALPHA,LI,UI)RESULT(RES)
             INTEGER, INTENT (IN) :: C, LI, UI
             REAL(KDKIND), INTENT(IN) :: ALPHA
             REAL(KDKIND) :: V(1:,1:)
             INTEGER :: IND(1:)
             INTEGER :: TMP
             INTEGER :: LB, RB

             LB = LI; RB = UI

             DO WHILE (LB < RB)
                IF ( V(C,IND(LB)) <= ALPHA ) THEN
                   LB = LB+1
                ELSE
                   TMP = IND(LB); IND(LB) = IND(RB); IND(RB) = TMP
                   RB = RB-1
                ENDIF
             END DO

             IF (V(C,IND(LB)) <= ALPHA) THEN
                RES = LB
             ELSE
                RES = LB-1
             ENDIF

          END FUNCTION SELECT_ON_COORDINATE_VALUE

          SUBROUTINE SELECT_ON_COORDINATE(V,IND,C,K,LI,UI)
             INTEGER, INTENT (IN) :: C, K, LI, UI
             INTEGER :: I, L, M, S, T, U
             REAL(KDKIND) :: V(:,:)
             INTEGER :: IND(:)
             L = LI
             U = UI
             DO WHILE (L<U)
             T = IND(L)
             M = L
             DO I = L + 1, U
                IF (V(C,IND(I))<V(C,T)) THEN
                M = M + 1
                S = IND(M)
                IND(M) = IND(I)
                IND(I) = S
                END IF
             END DO
             S = IND(L)
             IND(L) = IND(M)
             IND(M) = S
             IF (M<=K) L = M + 1
             IF (M>=K) U = M - 1
             END DO
          END SUBROUTINE SELECT_ON_COORDINATE

          SUBROUTINE SPREAD_IN_COORDINATE(TP,C,L,U,INTERV)
             TYPE (KDTREE2), POINTER :: TP
             TYPE(INTERVAL), INTENT(OUT) :: INTERV
             INTEGER, INTENT (IN) :: C, L, U
             REAL(KDKIND) :: LAST, LMAX, LMIN, T, SMIN,SMAX
             INTEGER :: I, ULOCAL
             REAL(KDKIND), POINTER :: V(:,:)
             INTEGER, POINTER :: IND(:)
             ! ..
             V => TP%THE_DATA(1:,1:)
             IND => TP%IND(1:)
             SMIN = V(C,IND(L))
             SMAX = SMIN

             ULOCAL = U

             DO I = L + 2, ULOCAL, 2
             LMIN = V(C,IND(I-1))
             LMAX = V(C,IND(I))
             IF (LMIN>LMAX) THEN
                T = LMIN
                LMIN = LMAX
                LMAX = T
             END IF
             IF (SMIN>LMIN) SMIN = LMIN
             IF (SMAX<LMAX) SMAX = LMAX
             END DO
             IF (I==ULOCAL+1) THEN
             LAST = V(C,IND(ULOCAL))
             IF (SMIN>LAST) SMIN = LAST
             IF (SMAX<LAST) SMAX = LAST
             END IF

             INTERV%LOWER = SMIN
             INTERV%UPPER = SMAX

          END SUBROUTINE SPREAD_IN_COORDINATE


          SUBROUTINE KDTREE2_DESTROY(TP)
          TYPE (KDTREE2), POINTER :: TP
          ! ..
          CALL DESTROY_NODE(TP%ROOT)

          DEALLOCATE (TP%IND)
          NULLIFY (TP%IND)

          IF (TP%REARRANGE) THEN
             DEALLOCATE(TP%REARRANGED_DATA)
             NULLIFY(TP%REARRANGED_DATA)
          ENDIF

          DEALLOCATE(TP)
          RETURN

          CONTAINS
          RECURSIVE SUBROUTINE DESTROY_NODE(NP)
             TYPE (TREE_NODE), POINTER :: NP
             INTRINSIC ASSOCIATED
             IF (ASSOCIATED(NP%LEFT)) THEN
             CALL DESTROY_NODE(NP%LEFT)
             NULLIFY (NP%LEFT)
             END IF
             IF (ASSOCIATED(NP%RIGHT)) THEN
             CALL DESTROY_NODE(NP%RIGHT)
             NULLIFY (NP%RIGHT)
             END IF
             IF (ASSOCIATED(NP%BOX)) DEALLOCATE(NP%BOX)
             DEALLOCATE(NP)
             RETURN

          END SUBROUTINE DESTROY_NODE

          END SUBROUTINE KDTREE2_DESTROY

          SUBROUTINE KDTREE2_N_NEAREST(TP,QV,NN,RESULTS)
          TYPE (KDTREE2), POINTER      :: TP
          REAL(KDKIND), TARGET, INTENT (IN)    :: QV(:)
          INTEGER, INTENT (IN)         :: NN
          TYPE(KDTREE2_RESULT), TARGET :: RESULTS(:)


          SR%BALLSIZE = HUGE(1.0)
          SR%QV => QV
          SR%NN = NN
          SR%NFOUND = 0
          SR%CENTERIDX = -1
          SR%CORRELTIME = 0
          SR%OVERFLOW = .FALSE.

          SR%RESULTS => RESULTS

          SR%NALLOC = NN

          SR%IND => TP%IND
          SR%REARRANGE = TP%REARRANGE
          IF (TP%REARRANGE) THEN
             SR%DATA => TP%REARRANGED_DATA
          ELSE
             SR%DATA => TP%THE_DATA
          ENDIF
          SR%DIMEN = TP%DIMEN

          CALL VALIDATE_QUERY_STORAGE(NN)
          SR%PQ = PQ_CREATE(RESULTS)

          CALL SEARCH(TP%ROOT)

          IF (TP%SORT) THEN
             CALL KDTREE2_SORT_RESULTS(NN, RESULTS)
          ENDIF
          RETURN
          END SUBROUTINE KDTREE2_N_NEAREST

          SUBROUTINE KDTREE2_N_NEAREST_AROUND_POINT(TP,IDXIN,CORRELTIME,&
                                                   NN,RESULTS)
          TYPE (KDTREE2), POINTER        :: TP
          INTEGER, INTENT (IN)           :: IDXIN, CORRELTIME, NN
          TYPE(KDTREE2_RESULT), TARGET   :: RESULTS(:)

          ALLOCATE (SR%QV(TP%DIMEN))
          SR%QV = TP%THE_DATA(:,IDXIN)
          SR%BALLSIZE = HUGE(1.0)
          SR%CENTERIDX = IDXIN
          SR%CORRELTIME = CORRELTIME

          SR%NN = NN
          SR%NFOUND = 0

          SR%DIMEN = TP%DIMEN
          SR%NALLOC = NN

          SR%RESULTS => RESULTS

          SR%IND => TP%IND
          SR%REARRANGE = TP%REARRANGE

          IF (SR%REARRANGE) THEN
             SR%DATA => TP%REARRANGED_DATA
          ELSE
             SR%DATA => TP%THE_DATA
          ENDIF

          CALL VALIDATE_QUERY_STORAGE(NN)
          SR%PQ = PQ_CREATE(RESULTS)

          CALL SEARCH(TP%ROOT)

          IF (TP%SORT) THEN
             CALL KDTREE2_SORT_RESULTS(NN, RESULTS)
          ENDIF
          DEALLOCATE (SR%QV)
          RETURN
          END SUBROUTINE KDTREE2_N_NEAREST_AROUND_POINT

          SUBROUTINE KDTREE2_R_NEAREST(TP,QV,R2,NFOUND,NALLOC,RESULTS)
          TYPE (KDTREE2), POINTER      :: TP
          REAL(KDKIND), TARGET, INTENT (IN)    :: QV(:)
          REAL(KDKIND), INTENT(IN)             :: R2
          INTEGER, INTENT(OUT)         :: NFOUND
          INTEGER, INTENT (IN)         :: NALLOC
          TYPE(KDTREE2_RESULT), TARGET :: RESULTS(:)

          SR%QV => QV
          SR%BALLSIZE = R2
          SR%NN = 0
          SR%NFOUND = 0
          SR%CENTERIDX = -1
          SR%CORRELTIME = 0

          SR%RESULTS => RESULTS

          CALL VALIDATE_QUERY_STORAGE(NALLOC)
          SR%NALLOC = NALLOC
          SR%OVERFLOW = .FALSE.
          SR%IND => TP%IND
          SR%REARRANGE= TP%REARRANGE

          IF (TP%REARRANGE) THEN
             SR%DATA => TP%REARRANGED_DATA
          ELSE
             SR%DATA => TP%THE_DATA
          ENDIF
          SR%DIMEN = TP%DIMEN

          CALL SEARCH(TP%ROOT)
          NFOUND = SR%NFOUND
          IF (TP%SORT) THEN
             CALL KDTREE2_SORT_RESULTS(NFOUND, RESULTS)
          ENDIF

          IF (SR%OVERFLOW) THEN
             WRITE (*,*) 'KD_TREE_TRANS: WARNING! RETURN FROM',&
                        ' KDTREE2_R_NEAREST FOUND MORE NEIGHBORS'
             WRITE (*,*) 'KD_TREE_TRANS: THAN STORAGE WAS PROVIDED FOR.',&
                        '  ANSWER IS NOT SMALLEST BALL'
             WRITE (*,*) 'KD_TREE_TRANS: WITH THAT NUMBER OF NEIGHBORS!',&
                        '  I.E. IT IS WRONG.'
          ENDIF

          RETURN
          END SUBROUTINE KDTREE2_R_NEAREST

          SUBROUTINE KDTREE2_R_NEAREST_AROUND_POINT(TP,IDXIN,CORRELTIME,R2,&
               NFOUND,NALLOC,RESULTS)

          TYPE (KDTREE2), POINTER      :: TP
          INTEGER, INTENT (IN)         :: IDXIN, CORRELTIME, NALLOC
          REAL(KDKIND), INTENT(IN)             :: R2
          INTEGER, INTENT(OUT)         :: NFOUND
          TYPE(KDTREE2_RESULT), TARGET :: RESULTS(:)

          INTRINSIC HUGE
          ! ..
          ALLOCATE (SR%QV(TP%DIMEN))
          SR%QV = TP%THE_DATA(:,IDXIN)
          SR%BALLSIZE = R2
          SR%NN = 0
          SR%NFOUND = 0
          SR%CENTERIDX = IDXIN
          SR%CORRELTIME = CORRELTIME

          SR%RESULTS => RESULTS

          SR%NALLOC = NALLOC
          SR%OVERFLOW = .FALSE.

          CALL VALIDATE_QUERY_STORAGE(NALLOC)

          SR%IND => TP%IND
          SR%REARRANGE = TP%REARRANGE

          IF (TP%REARRANGE) THEN
             SR%DATA => TP%REARRANGED_DATA
          ELSE
             SR%DATA => TP%THE_DATA
          ENDIF
          SR%REARRANGE = TP%REARRANGE
          SR%DIMEN = TP%DIMEN

          CALL SEARCH(TP%ROOT)
          NFOUND = SR%NFOUND
          IF (TP%SORT) THEN
             CALL KDTREE2_SORT_RESULTS(NFOUND,RESULTS)
          ENDIF

          IF (SR%OVERFLOW) THEN
             WRITE (*,*) 'KD_TREE_TRANS: WARNING! RETURN FROM',&
                        ' KDTREE2_R_NEAREST FOUND MORE NEIGHBORS'
             WRITE (*,*) 'KD_TREE_TRANS: THAN STORAGE WAS PROVIDED FOR.',&
                        '  ANSWER IS NOT SMALLEST BALL'
             WRITE (*,*) 'KD_TREE_TRANS: WITH THAT NUMBER OF NEIGHBORS!',&
                        '  I.E. IT IS WRONG.'
          ENDIF

          DEALLOCATE (SR%QV)
          RETURN
          END SUBROUTINE KDTREE2_R_NEAREST_AROUND_POINT

          FUNCTION KDTREE2_R_COUNT(TP,QV,R2) RESULT(NFOUND)
          TYPE (KDTREE2), POINTER   :: TP
          REAL(KDKIND), TARGET, INTENT (IN) :: QV(:)
          REAL(KDKIND), INTENT(IN)          :: R2
          INTEGER                   :: NFOUND
          INTRINSIC HUGE
          ! ..
          SR%QV => QV
          SR%BALLSIZE = R2

          SR%NN = 0
          SR%NFOUND = 0
          SR%CENTERIDX = -1
          SR%CORRELTIME = 0

          NULLIFY(SR%RESULTS)

          SR%NALLOC = 0

          SR%IND => TP%IND
          SR%REARRANGE = TP%REARRANGE
          IF (TP%REARRANGE) THEN
             SR%DATA => TP%REARRANGED_DATA
          ELSE
             SR%DATA => TP%THE_DATA
          ENDIF
          SR%DIMEN = TP%DIMEN

          SR%OVERFLOW = .FALSE.

          CALL SEARCH(TP%ROOT)

          NFOUND = SR%NFOUND

          RETURN
          END FUNCTION KDTREE2_R_COUNT

          FUNCTION KDTREE2_R_COUNT_AROUND_POINT(TP,IDXIN,CORRELTIME,R2)RESULT(NFOUND)

          TYPE (KDTREE2), POINTER :: TP
          INTEGER, INTENT (IN)    :: CORRELTIME, IDXIN
          REAL(KDKIND), INTENT(IN)        :: R2
          INTEGER                 :: NFOUND

          INTRINSIC HUGE
          ! ..
          ALLOCATE (SR%QV(TP%DIMEN))
          SR%QV = TP%THE_DATA(:,IDXIN)
          SR%BALLSIZE = R2

          SR%NN = 0
          SR%NFOUND = 0
          SR%CENTERIDX = IDXIN
          SR%CORRELTIME = CORRELTIME
          NULLIFY(SR%RESULTS)

          SR%NALLOC = 0

          SR%IND => TP%IND
          SR%REARRANGE = TP%REARRANGE

          IF (SR%REARRANGE) THEN
             SR%DATA => TP%REARRANGED_DATA
          ELSE
             SR%DATA => TP%THE_DATA
          ENDIF
          SR%DIMEN = TP%DIMEN
          SR%OVERFLOW = .FALSE.

          CALL SEARCH(TP%ROOT)

          NFOUND = SR%NFOUND

          RETURN
          END FUNCTION KDTREE2_R_COUNT_AROUND_POINT


          SUBROUTINE VALIDATE_QUERY_STORAGE(N)

          INTEGER, INTENT(IN) :: N

          IF (SIZE(SR%RESULTS,1) .LT. N) THEN
             WRITE (*,*) 'KD_TREE_TRANS:  YOU DID NOT PROVIDE ENOUGH',&
                        ' STORAGE FOR RESULTS(1:N)'
             STOP
             RETURN
          ENDIF

          RETURN
          END SUBROUTINE VALIDATE_QUERY_STORAGE

          FUNCTION SQUARE_DISTANCE(D, IV,QV) RESULT (RES)

          REAL(KDKIND) :: RES
          INTEGER :: D
          REAL(KDKIND) :: IV(:),QV(:)

          RES = SUM( (IV(1:D)-QV(1:D))**2 )
          END FUNCTION SQUARE_DISTANCE

          RECURSIVE SUBROUTINE SEARCH(NODE)

          TYPE (TREE_NODE), POINTER          :: NODE
          ! ..
          TYPE(TREE_NODE),POINTER            :: NCLOSER, NFARTHER
          !
          INTEGER                            :: CUT_DIM, I
          ! ..
          REAL(KDKIND)                               :: QVAL, DIS
          REAL(KDKIND)                               :: BALLSIZE
          REAL(KDKIND), POINTER           :: QV(:)
          TYPE(INTERVAL), POINTER :: BOX(:)

          IF ((ASSOCIATED(NODE%LEFT) .AND. ASSOCIATED(NODE%RIGHT))&
              .EQV. .FALSE.) THEN
             IF (SR%NN .EQ. 0) THEN
             CALL PROCESS_TERMINAL_NODE_FIXEDBALL(NODE)
             ELSE
             CALL PROCESS_TERMINAL_NODE(NODE)
             ENDIF
          ELSE
             QV => SR%QV(1:)
             CUT_DIM = NODE%CUT_DIM
             QVAL = QV(CUT_DIM)

             IF (QVAL < NODE%CUT_VAL) THEN
                 NCLOSER => NODE%LEFT
                 NFARTHER => NODE%RIGHT
                 DIS = (NODE%CUT_VAL_RIGHT - QVAL)**2
             ELSE
                 NCLOSER => NODE%RIGHT
                 NFARTHER => NODE%LEFT
                 DIS = (NODE%CUT_VAL_LEFT - QVAL)**2
             ENDIF

             IF (ASSOCIATED(NCLOSER)) CALL SEARCH(NCLOSER)

             IF (ASSOCIATED(NFARTHER)) THEN
             BALLSIZE = SR%BALLSIZE
             IF (DIS <= BALLSIZE) THEN
                BOX => NODE%BOX(1:)
                DO I=1,SR%DIMEN
                IF (I .NE. CUT_DIM) THEN
                   DIS = DIS + DIS2_FROM_BND(QV(I),BOX(I)%LOWER,&
                        BOX(I)%UPPER)
                   IF (DIS > BALLSIZE) THEN
                        RETURN
                   ENDIF
                ENDIF
                END DO
                CALL SEARCH(NFARTHER)
             ENDIF
             ENDIF
          END IF
          END SUBROUTINE SEARCH


          REAL(KDKIND) FUNCTION DIS2_FROM_BND(X,AMIN,AMAX) RESULT (RES)
          REAL(KDKIND), INTENT(IN) :: X, AMIN,AMAX

          IF (X > AMAX) THEN
             RES = (X-AMAX)**2;
             RETURN
          ELSE
             IF (X < AMIN) THEN
             RES = (AMIN-X)**2;
             RETURN
             ELSE
             RES = 0.0
             RETURN
             ENDIF
          ENDIF
          RETURN
          END FUNCTION DIS2_FROM_BND

          LOGICAL FUNCTION BOX_IN_SEARCH_RANGE(NODE, SR) RESULT(RES)
          TYPE (TREE_NODE), POINTER :: NODE
          TYPE (TREE_SEARCH_RECORD), POINTER :: SR

          INTEGER :: DIMEN, I
          REAL(KDKIND)    :: DIS, BALLSIZE
          REAL(KDKIND)    :: L, U

          DIMEN = SR%DIMEN
          BALLSIZE = SR%BALLSIZE
          DIS = 0.0
          RES = .TRUE.
          DO I=1,DIMEN
             L = NODE%BOX(I)%LOWER
             U = NODE%BOX(I)%UPPER
             DIS = DIS + (DIS2_FROM_BND(SR%QV(I),L,U))
             IF (DIS > BALLSIZE) THEN
             RES = .FALSE.
             RETURN
             ENDIF
          END DO
          RES = .TRUE.
          RETURN
          END FUNCTION BOX_IN_SEARCH_RANGE


          SUBROUTINE PROCESS_TERMINAL_NODE(NODE)

          TYPE (TREE_NODE), POINTER          :: NODE

          REAL(KDKIND), POINTER          :: QV(:)
          INTEGER, POINTER       :: IND(:)
          REAL(KDKIND), POINTER  :: DATA(:,:)

          INTEGER            :: DIMEN, I, INDEXOFI, K, CENTERIDX, CORRELTIME
          REAL(KDKIND)                   :: BALLSIZE, SD, NEWPRI
          LOGICAL                :: REARRANGE
          TYPE(PQ), POINTER      :: PQP

          QV => SR%QV(1:)
          PQP => SR%PQ
          DIMEN = SR%DIMEN
          BALLSIZE = SR%BALLSIZE
          REARRANGE = SR%REARRANGE
          IND => SR%IND(1:)
          DATA => SR%DATA(1:,1:)
          CENTERIDX = SR%CENTERIDX
          CORRELTIME = SR%CORRELTIME


          MAINLOOP: DO I = NODE%L, NODE%U
             IF (REARRANGE) THEN
             SD = 0.0
             DO K = 1,DIMEN
                SD = SD + (DATA(K,I) - QV(K))**2
                IF (SD>BALLSIZE) CYCLE MAINLOOP
             END DO
             INDEXOFI = IND(I)
             ELSE
             INDEXOFI = IND(I)
             SD = 0.0
             DO K = 1,DIMEN
                SD = SD + (DATA(K,INDEXOFI) - QV(K))**2
                IF (SD>BALLSIZE) CYCLE MAINLOOP
             END DO
             ENDIF

             IF (CENTERIDX > 0) THEN
             IF (ABS(INDEXOFI-CENTERIDX) < CORRELTIME) CYCLE MAINLOOP
             ENDIF

             IF (SR%NFOUND .LT. SR%NN) THEN

             SR%NFOUND = SR%NFOUND +1
             NEWPRI = PQ_INSERT(PQP,SD,INDEXOFI)
             IF (SR%NFOUND .EQ. SR%NN) BALLSIZE = NEWPRI

             ELSE
                BALLSIZE = PQ_REPLACE_MAX(PQP,SD,INDEXOFI)
             ENDIF
          END DO MAINLOOP

          SR%BALLSIZE = BALLSIZE

          END SUBROUTINE PROCESS_TERMINAL_NODE

          SUBROUTINE PROCESS_TERMINAL_NODE_FIXEDBALL(NODE)

          TYPE (TREE_NODE), POINTER          :: NODE

          REAL(KDKIND), POINTER          :: QV(:)
          INTEGER, POINTER       :: IND(:)
          REAL(KDKIND), POINTER          :: DATA(:,:)
          INTEGER                :: NFOUND
          INTEGER                :: DIMEN, I, INDEXOFI, K
          INTEGER                :: CENTERIDX, CORRELTIME, NN
          REAL(KDKIND)                   :: BALLSIZE, SD
          LOGICAL                :: REARRANGE

          QV => SR%QV(1:)
          DIMEN = SR%DIMEN
          BALLSIZE = SR%BALLSIZE
          REARRANGE = SR%REARRANGE
          IND => SR%IND(1:)
          DATA => SR%DATA(1:,1:)
          CENTERIDX = SR%CENTERIDX
          CORRELTIME = SR%CORRELTIME
          NN = SR%NN
          NFOUND = SR%NFOUND


          MAINLOOP: DO I = NODE%L, NODE%U

             IF (REARRANGE) THEN
             SD = 0.0
             DO K = 1,DIMEN
                SD = SD + (DATA(K,I) - QV(K))**2
                IF (SD>BALLSIZE) CYCLE MAINLOOP
             END DO
             INDEXOFI = IND(I)
             ELSE
             INDEXOFI = IND(I)
             SD = 0.0
             DO K = 1,DIMEN
                SD = SD + (DATA(K,INDEXOFI) - QV(K))**2
                IF (SD>BALLSIZE) CYCLE MAINLOOP
             END DO
             ENDIF

             IF (CENTERIDX > 0) THEN
             IF (ABS(INDEXOFI-CENTERIDX)<CORRELTIME) CYCLE MAINLOOP
             ENDIF

             NFOUND = NFOUND+1
             IF (NFOUND .GT. SR%NALLOC) THEN
             SR%OVERFLOW = .TRUE.
             ELSE
             SR%RESULTS(NFOUND)%DIS = SD
             SR%RESULTS(NFOUND)%IDX = INDEXOFI
             ENDIF
          END DO MAINLOOP

          SR%NFOUND = NFOUND
          END SUBROUTINE PROCESS_TERMINAL_NODE_FIXEDBALL

          SUBROUTINE KDTREE2_N_NEAREST_BRUTE_FORCE(TP,QV,NN,RESULTS)

          TYPE (KDTREE2), POINTER :: TP
          REAL(KDKIND), INTENT (IN)       :: QV(:)
          INTEGER, INTENT (IN)    :: NN
          TYPE(KDTREE2_RESULT)    :: RESULTS(:)

          INTEGER :: I, J, K
          REAL(KDKIND), ALLOCATABLE :: ALL_DISTANCES(:)
          ! ..
          ALLOCATE (ALL_DISTANCES(TP%N))
          DO I = 1, TP%N
             ALL_DISTANCES(I) = SQUARE_DISTANCE(TP%DIMEN,QV,&
                TP%THE_DATA(:,I))
          END DO

          DO I = 1, NN
             RESULTS(I)%DIS =  HUGE(1.0)
             RESULTS(I)%IDX = -1
          END DO
          DO I = 1, TP%N
             IF (ALL_DISTANCES(I)<RESULTS(NN)%DIS) THEN

             DO J = 1, NN
                IF (ALL_DISTANCES(I)<RESULTS(J)%DIS) EXIT
             END DO

             DO K = NN - 1, J, -1
                RESULTS(K+1) = RESULTS(K)
             END DO
             RESULTS(J)%DIS = ALL_DISTANCES(I)
             RESULTS(J)%IDX = I
             END IF
          END DO
          DEALLOCATE (ALL_DISTANCES)
          END SUBROUTINE KDTREE2_N_NEAREST_BRUTE_FORCE


          SUBROUTINE KDTREE2_R_NEAREST_BRUTE_FORCE(TP,QV,R2,NFOUND,RESULTS)

          TYPE (KDTREE2), POINTER :: TP
          REAL(KDKIND), INTENT (IN)       :: QV(:)
          REAL(KDKIND), INTENT (IN)       :: R2
          INTEGER, INTENT(OUT)    :: NFOUND
          TYPE(KDTREE2_RESULT)    :: RESULTS(:)

          INTEGER :: I, NALLOC
          REAL(KDKIND), ALLOCATABLE :: ALL_DISTANCES(:)
          ! ..
          ALLOCATE (ALL_DISTANCES(TP%N))
          DO I = 1, TP%N
             ALL_DISTANCES(I) = SQUARE_DISTANCE(TP%DIMEN,QV,&
                TP%THE_DATA(:,I))
          END DO

          NFOUND = 0
          NALLOC = SIZE(RESULTS,1)

          DO I = 1, TP%N
             IF (ALL_DISTANCES(I)< R2) THEN

             IF (NFOUND .LT. NALLOC) THEN
                NFOUND = NFOUND+1
                RESULTS(NFOUND)%DIS = ALL_DISTANCES(I)
                RESULTS(NFOUND)%IDX = I
             ENDIF
             END IF
          ENDDO
          DEALLOCATE (ALL_DISTANCES)

          CALL KDTREE2_SORT_RESULTS(NFOUND,RESULTS)


          END SUBROUTINE KDTREE2_R_NEAREST_BRUTE_FORCE

          SUBROUTINE KDTREE2_SORT_RESULTS(NFOUND,RESULTS)

          INTEGER, INTENT(IN)          :: NFOUND
          TYPE(KDTREE2_RESULT), TARGET :: RESULTS(:)

          IF (NFOUND .GT. 1) CALL HEAPSORT_STRUCT(RESULTS,NFOUND)

          RETURN
          END SUBROUTINE KDTREE2_SORT_RESULTS

          SUBROUTINE HEAPSORT(A,IND,N)

          INTEGER,INTENT(IN)          :: N
          REAL(KDKIND), INTENT(INOUT)         :: A(:)
          INTEGER, INTENT(INOUT)      :: IND(:)

          !
          !
          REAL(KDKIND)        :: VALUE
          INTEGER     :: IVALUE

          INTEGER     :: I,J
          INTEGER     :: ILEFT,IRIGHT

          ILEFT=N/2+1
          IRIGHT=N

          IF(N.EQ.1) RETURN

          DO
             IF(ILEFT > 1)THEN
             ILEFT=ILEFT-1
             VALUE=A(ILEFT); IVALUE=IND(ILEFT)
             ELSE
             VALUE=A(IRIGHT); IVALUE=IND(IRIGHT)
             A(IRIGHT)=A(1); IND(IRIGHT)=IND(1)
             IRIGHT=IRIGHT-1
             IF (IRIGHT == 1) THEN
                A(1)=VALUE;IND(1)=IVALUE
                RETURN
             ENDIF
             ENDIF
             I=ILEFT
             J=2*ILEFT
             DO WHILE (J <= IRIGHT)
             IF(J < IRIGHT) THEN
                IF(A(J) < A(J+1)) J=J+1
             ENDIF
             IF(VALUE < A(J)) THEN
                A(I)=A(J); IND(I)=IND(J)
                I=J
                J=J+J
             ELSE
                J=IRIGHT+1
             ENDIF
             END DO
             A(I)=VALUE; IND(I)=IVALUE
          END DO
          END SUBROUTINE HEAPSORT

          SUBROUTINE HEAPSORT_STRUCT(A,N)

          INTEGER,INTENT(IN)                 :: N
          TYPE(KDTREE2_RESULT),INTENT(INOUT) :: A(:)

          !
          !
          TYPE(KDTREE2_RESULT) :: VALUE

          INTEGER     :: I,J
          INTEGER     :: ILEFT,IRIGHT

          ILEFT=N/2+1
          IRIGHT=N

          IF(N.EQ.1) RETURN

          DO
             IF(ILEFT > 1)THEN
             ILEFT=ILEFT-1
             VALUE=A(ILEFT)
             ELSE
             VALUE=A(IRIGHT)
             A(IRIGHT)=A(1)
             IRIGHT=IRIGHT-1
             IF (IRIGHT == 1) THEN
                A(1) = VALUE
                RETURN
             ENDIF
             ENDIF
             I=ILEFT
             J=2*ILEFT
             DO WHILE (J <= IRIGHT)
                 IF(J < IRIGHT) THEN
                    IF(A(J)%DIS < A(J+1)%DIS) J=J+1
                 ENDIF
                 IF(VALUE%DIS < A(J)%DIS) THEN
                    A(I)=A(J);
                    I=J
                    J=J+J
                 ELSE
                    J=IRIGHT+1
                 ENDIF
             END DO
             A(I)=VALUE
          END DO
          END SUBROUTINE HEAPSORT_STRUCT

        END MODULE KDTREE2_MODULE

!...Zach's interface to KDTREE        
        MODULE KDTREE2MODULE
            
            USE KDTREE2_MODULE

            CONTAINS
            
            SUBROUTINE MakeScatterKDTREE(X,Y,tree)

                IMPLICIT NONE

                TYPE(KDTREE2),POINTER,INTENT(OUT) :: tree
                INTEGER                           :: I
                REAL(8)                           :: x(:)
                REAL(8)                           :: y(:)
                REAL(8),ALLOCATABLE               :: xy(:,:)

                IF(SIZE(X).NE.SIZE(Y))THEN
                    WRITE(*,'(A)') "ERROR: Array size inconsistant"
                    STOP
                ENDIF

                ALLOCATE(XY(1:2,1:SIZE(X)))
                XY(1,:) = X(:)
                XY(2,:) = Y(:)

                Tree => KDTREE2_CREATE(XY,SORT=.TRUE.,REARRANGE=.TRUE.)

            END SUBROUTINE MakeScatterKDTREE
            
            INTEGER FUNCTION FindNearest(X,Y,tree)

                IMPLICIT NONE

                REAL(8),INTENT(IN)               :: X
                REAL(8),INTENT(IN)               :: Y
                TYPE(KDTREE2),POINTER,INTENT(IN) :: Tree
                TYPE(KDTREE2_RESULT)             :: KDRESULTS(1)


                CALL KDTREE2_N_NEAREST(TP=TREE,QV=(/X,Y/),NN=1,&
                    RESULTS=KDRESULTS)

                FindNearest = KDRESULTS(1)%IDX

                RETURN

            END FUNCTION FindNearest


            SUBROUTINE FindXNearest(X,Y,tree,SearchDepth,KDRESULTS)
                IMPLICIT NONE
                REAL(8),INTENT(IN) :: X
                REAL(8),INTENT(IN) :: Y
                INTEGER,INTENT(IN) :: SearchDepth
                INTEGER            :: SearchDepth2
                TYPE(KDTREE2),POINTER,INTENT(IN) :: Tree
                TYPE(KDTREE2_RESULT),INTENT(OUT) :: KDRESULTS(:)

                IF(SearchDepth.GT.TREE%N)THEN
                    SearchDepth2 = TREE%N
                ELSE
                    SearchDepth2 = SearchDepth
                ENDIF

                CALL KDTREE2_N_NEAREST(TP=TREE,QV=(/X,Y/),NN=SearchDepth,&
                    RESULTS=KDRESULTS)

                RETURN
            END SUBROUTINE FindXNearest


        END MODULE KDTREE2MODULE

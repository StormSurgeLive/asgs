!----------------------------------------------------------------------!
!
!                            FIGUREGEN
!                       By: Casey Dietrich
!
!  FigureGen is a Fortran program that creates images for ADCIRC files. 
!  It reads mesh files (fort.14, etc.), nodal attributes files
!  (fort.13, etc.) and output files (fort.63, fort.64, maxele.63, etc.). 
!  It plots contours, contour lines, and vectors. Using FigureGen, you 
!  can go directly from the ADCIRC input and output files to a 
!  presentation-quality figure, for one or multiple time snaps.
!
!  This program started from a script written by Brian Blanton, 
!  and I converted it to Fortran because I am more familiar with that 
!  language. It now contains code written by John Atkinson, Zach Cobell, 
!  Howard Lander, Chris Szpilka, Matthieu Vitse, Matthew Bilskie, and others.  
!  But, at its core, FigureGen behaves like a script, and it uses system 
!  calls to tell other software how to generate the figure(s).
!
!----------------------------------------------------------------------!


!----------------------------------------------------------------------!
!...Use the configuration file to compile the code
!   instead of using many options on the command line.
!   compile with the option -DHAVE_CONFIG to enable this 
!   feature. Make sure that the file "FigureGen_Options.h" 
!   is either in the current directory, or its location is
!   spcified using the -I/path/to/directory syntax.
#ifdef HAVE_CONFIG
#include 'FigureGen_Options.h'
#endif    
!----------------------------------------------------------------------!


!----------------------------------------------------------------------!
!    KDTREE2 is (c) Matthew Kennel, Institute for Nonlinear Science (2004)
!    Licensed under the Academic Free License version 1.1
!----------------------------------------------------------------------!
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
             WRITE (*,*) 'PQ_CREATE: ERROR, INPUT ARRAYS MUST BE '// &
                'ALLOCATED.'
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

          FUNCTION KDTREE2_CREATE(INPUT_DATA,IDIM2,SORT,REARRANGE)&
            RESULT(MR)
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

      MODULE GLOBALVAR
       
            TYPE DATEVAR
                INTEGER :: YEAR=0
                INTEGER :: MONTH=0
                INTEGER :: DAY=0
                INTEGER :: HOUR=0
                INTEGER :: MINUTE=0
                INTEGER :: SECOND=0
            END TYPE
            TYPE(DATEVAR),SAVE  :: ColdStartDate
            TYPE(DATEVAR),SAVE  :: ResultDate

            CHARACTER(LEN=50)   :: AlphaLabel
            CHARACTER(LEN=50)   :: BackgroundImagesFile
            CHARACTER(LEN=50)   :: BoundariesColor
            CHARACTER(LEN=50)   :: BoundariesThickness
            CHARACTER(LEN=50)   :: CoastlineColor
            CHARACTER(LEN=1)    :: CoastlineRes
            CHARACTER(LEN=50)   :: CoastlineThickness
            CHARACTER(LEN=1)    :: CoastlineWB
            CHARACTER(LEN=40)   :: ColorLines
            CHARACTER(LEN=50)   :: ContourFile1
            CHARACTER(LEN=50)   :: ContourFile2
            CHARACTER(LEN=50)   :: ContourFileFormat1
            CHARACTER(LEN=50)   :: ContourFileFormat2
            CHARACTER(LEN=50)   :: ContourFileType
            CHARACTER(LEN=50),ALLOCATABLE :: ContourFileList(:)
            CHARACTER(LEN=50)   :: ContourFileListFile
            CHARACTER(LEN=50),ALLOCATABLE :: ContourFileTag(:)
            CHARACTER(LEN=40)   :: ContourLabelRotation
            CHARACTER(LEN=40)   :: ContourUnits
            CHARACTER(LEN=50)   :: ContourXYZFile
            CHARACTER(LEN=40)   :: DiffContoursFile
            CHARACTER(LEN=50)   :: DotsLinesColor
            CHARACTER(LEN=50)   :: DotsLinesFile
            CHARACTER(LEN=50)   :: DotsLinesThickness
            CHARACTER(LEN=40)   :: Fort14File
            CHARACTER(LEN=40)   :: Fort14File2
            CHARACTER(LEN=50)   :: GridFileFormat
            CHARACTER(LEN=50)   :: GridFileFormat2
            CHARACTER(LEN=50)   :: GSPath
            CHARACTER(LEN=100)  :: InputFile
            CHARACTER(LEN=40)   :: LabelsColor
            CHARACTER(LEN=40)   :: LabelsFile
            CHARACTER(LEN=50)   :: LogoFile
            CHARACTER(LEN=50)   :: LogoLocation
            CHARACTER(LEN=50)   :: LogoWidth
            CHARACTER(LEN=200)  :: NETCDF_TYPES(38)
            CHARACTER(LEN=50)   :: Palette
            CHARACTER(LEN=50)   :: Path
            CHARACTER(LEN=50)   :: ParticleColor
            CHARACTER(LEN=50)   :: ParticleFile
            CHARACTER(LEN=50)   :: ParticleFileFormat
            CHARACTER(LEN=50)   :: ParticlePalette
            CHARACTER(LEN=50)   :: ParticlePattern
            CHARACTER(LEN=50)   :: ParticleSize
            CHARACTER(LEN=50)   :: ParticleXYZFile
            CHARACTER(LEN=50)   :: PlotLabel
            CHARACTER(LEN=50)   :: PlotLabelFile
            CHARACTER(LEN=60)   :: PlotName
            CHARACTER(LEN=40)   :: SMSPalette
            CHARACTER(LEN=60)   :: TempLabelsFile
            CHARACTER(LEN=60)   :: TempMapFile1
            CHARACTER(LEN=60)   :: TempMapFile2
            CHARACTER(LEN=50)   :: TempPath
            CHARACTER(LEN=50)   :: TimeCurrentFile
            CHARACTER(LEN=50)   :: TimeCurrentTextFile
            CHARACTER(LEN=50)   :: TimeMaxFile
            CHARACTER(LEN=40)   :: VectorFile
            CHARACTER(LEN=50)   :: VectorFileFormat
            CHARACTER(LEN=50)   :: VectorFileType
            CHARACTER(LEN=50)   :: VectorLabel
            CHARACTER(LEN=40)   :: VectorScaleFile
            CHARACTER(LEN=40)   :: VectorTextFile
            CHARACTER(LEN=40)   :: VectorUnits
            CHARACTER(LEN=50)   :: VectorUFile
            CHARACTER(LEN=50)   :: VectorVFile

            INTEGER             :: C_Node
            INTEGER             :: ContourFileNumCols
            INTEGER             :: ContourLabelSize
            INTEGER             :: DoBMP
            INTEGER             :: DoCenter
            INTEGER             :: DoEPS
            INTEGER             :: DoJPG
            INTEGER             :: DoPDF
            INTEGER             :: DoPNG
            INTEGER             :: DoTIFF
            INTEGER             :: DoPS
            INTEGER             :: FindContourRange
            INTEGER             :: FindVectorScale
            INTEGER             :: GoogleDay
            INTEGER             :: GoogleHour
            INTEGER             :: GoogleMin
            INTEGER             :: GoogleMonth
            INTEGER             :: GoogleSec
            INTEGER             :: GoogleTransparency
            INTEGER             :: GoogleYear
            INTEGER             :: IERR
            INTEGER             :: IfAddPlotLabel
            INTEGER             :: IfAddTimeBar
            INTEGER             :: IfGIS
            INTEGER             :: IfGoogle
            INTEGER             :: IfPlotBackgroundImages
            INTEGER             :: IfPlotBoundaries
            INTEGER             :: IfPlotCoastline
            INTEGER             :: IfPlotContourLines
            INTEGER             :: IfPlotDotsLines
            INTEGER             :: IfPlotFilledContours
            INTEGER             :: IfPlotGrid
            INTEGER             :: IfPlotLabels
            INTEGER             :: IfPlotLogo
            INTEGER             :: IfPlotParticles
            INTEGER             :: IfPlotVectors
            INTEGER             :: ImageTrimFlag
            INTEGER             :: KeepOpen(15)
            INTEGER             :: LabelsSize
            INTEGER             :: MyRank
            INTEGER             :: NumContourFiles
            INTEGER             :: NumEdgeFiles
            INTEGER             :: NumLayers
            INTEGER             :: NumNodesGlobal
            INTEGER             :: NumNodesLocal
            INTEGER             :: NumNodesMesh2
            INTEGER             :: NumProcs
            INTEGER             :: NumProcsInitial
            INTEGER             :: NumRecords
            INTEGER             :: NumRecs
            INTEGER             :: NumSubDomains
            INTEGER             :: NumSubRecords = 1
            INTEGER             :: OptimizeContours = 0
            INTEGER,ALLOCATABLE :: RecordsList(:)
            INTEGER             :: RemoveFiles = 1
            INTEGER             :: Resolution
            INTEGER             :: SmallJPGWidth
            INTEGER             :: SplitBy
            INTEGER             :: TimeStep
            INTEGER,ALLOCATABLE :: TranslationTable(:)
            INTEGER             :: VectorFileNumCols
            INTEGER             :: Verbose
            INTEGER             :: VersionNumber
            INTEGER,ALLOCATABLE :: XYZNodes(:)

            LOGICAL             :: NeedTranslationTable
            LOGICAL             :: OutputFileList
            LOGICAL             :: PlotVectorScale
            LOGICAL             :: UseParticlePalette
            LOGICAL,ALLOCATABLE :: BdyNodes(:)

            REAL,ALLOCATABLE    :: BathLocal(:)
            REAL                :: BorderIncrementMajor
            REAL                :: BorderIncrementMinor
            REAL                :: Buffer
            REAL                :: C_Height
            REAL                :: C_Width
            REAL                :: ContourConversionFactor
            REAL                :: ContourInterval
            REAL                :: ContourLabelEvery
            REAL                :: ContourLabelMinDist
            REAL                :: ContourMax
            REAL                :: ContourMin
            REAL(8)             :: CurrentTime
            REAL(8)             :: DEG2RAD
            REAL                :: LatLonBuffer = 0.25 
            REAL                :: LatN
            REAL                :: LatS
            REAL                :: LongE
            REAL                :: LongW
            REAL,ALLOCATABLE    :: NodeColors(:)
            REAL                :: ScaleHeight
            REAL                :: ScaleLabelEvery
            REAL                :: ScaleWidth
            REAL                :: StartTime
            REAL                :: TotalSimTime
            REAL                :: TotalSimTime1
            REAL                :: TotalSimTime2
            REAL                :: VectorConversionFactor
            REAL                :: VectorMag
            REAL                :: VectorHeadLength
            REAL                :: VectorHeadWidth
            REAL                :: VectorScaleMag
            REAL                :: VectorSpacing
            REAL                :: VectorTailWidth
            REAL                :: Width
            REAL(8),ALLOCATABLE :: G1XY(:,:)
            REAL(8),ALLOCATABLE :: G2XY(:,:)
            REAL,ALLOCATABLE    :: X(:)
            REAL,ALLOCATABLE    :: Y(:)
            REAL,ALLOCATABLE    :: Z(:)
            
            LOGICAL             :: FileExists

            CONTAINS

#ifdef NETCDF

            SUBROUTINE INITIALIZE_NETCDF()
                IMPLICIT NONE
                !...This subroutine just sets up an array we can
                !   use later. Matches all variables listed in
                !   NETCDFIO.f as of 10/8/2012

                NETCDF_TYPES(:)  = ""
                NETCDF_TYPES(1)  = "sigmat"       !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(2)  = "salinity"     !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(3)  = "temperature"  !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(4)  = "u-vel3D"      !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(5)  = "v-vel3D"      !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(6)  = "w-vel3D"      !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(7)  = "q20"          !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(8)  = "l"            !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(9)  = "ev"           !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(10) = "qsurfkp1"     !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(11) = "zeta"
                NETCDF_TYPES(12) = "zeta_max"
                NETCDF_TYPES(13) = "u-vel"
                NETCDF_TYPES(14) = "v-vel"
                NETCDF_TYPES(15) = "vel_max"
                NETCDF_TYPES(16) = "pressure"
                NETCDF_TYPES(17) = "pressure_min"
                NETCDF_TYPES(18) = "windx"
                NETCDF_TYPES(19) = "windy"
                NETCDF_TYPES(20) = "wind_max"
                NETCDF_TYPES(21) = "radstress_x"
                NETCDF_TYPES(22) = "radstress_y"
                NETCDF_TYPES(23) = "radstress_max"
                NETCDF_TYPES(24) = "swan_HS"
                NETCDF_TYPES(25) = "swan_HS_max"
                NETCDF_TYPES(26) = "swan_DIR"
                NETCDF_TYPES(27) = "swan_DIR_max"
                NETCDF_TYPES(28) = "swan_TM01"
                NETCDF_TYPES(29) = "swan_TM01_max"
                NETCDF_TYPES(30) = "swan_TPS"
                NETCDF_TYPES(31) = "swan_TPS_max"
                NETCDF_TYPES(32) = "swan_windx"
                NETCDF_TYPES(33) = "swan_windy"
                NETCDF_TYPES(34) = "swan_wind_max"
                NETCDF_TYPES(35) = "swan_TM02"
                NETCDF_TYPES(36) = "swan_TM02_max"
                NETCDF_TYPES(37) = "swan_TMM10"
                NETCDF_TYPES(38) = "swan_TMM10_max"
                
    !....NOTE: The reason some are not implemented is because multiple variables appear in those
    !          NetCDF files and a user input option will need to be speicified before these can
    !          be correctly enabled. In the current scheme, the first variable listed above will
    !          always be found. If need be, you can reorder these to plot the desired variable.

                
                RETURN
            
            END SUBROUTINE
                
    !...This is a subroutine that simply examines the NetCDF file and determines if it
    !   contains any data we can plot
            SUBROUTINE FindMyNetCDFVariable(NCID,Vector,NumCols)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN)           :: NCID
                LOGICAL,INTENT(IN),OPTIONAL  :: Vector
                INTEGER,INTENT(OUT),OPTIONAL :: NumCols
                INTEGER :: I
                INTEGER :: J
                INTEGER :: NVAR
                CHARACTER(200) :: NC_NAME

                CALL CHECK(NF90_INQUIRE(NCID,NVARIABLES=NVAR))

                IF(PRESENT(NumCols))NumCols = 1
        
                DO I = 1,NVAR
                    CALL CHECK(NF90_INQUIRE_VARIABLE(NCID,I,NAME=NC_NAME))
                    DO J = 1,SIZE(NETCDF_TYPES)
                        IF(TRIM(NC_NAME).EQ.TRIM(NETCDF_TYPES(J)))THEN
                            IF(.NOT.PRESENT(VECTOR))RETURN
                            IF(.NOT.Vector)RETURN
                            IF(Vector)THEN
                                SELECT CASE(J)
                                    CASE(13,14,18,19,21,22,33,34)
                                        NumCols = 2
                                        RETURN
                                    CASE(26,27)
                                        IF(PRESENT(NumCols))NumCols = 1
                                        RETURN
                                    CASE DEFAULT
                                        CONTINUE
                                END SELECT
                            ENDIF    
                        ENDIF
                    ENDDO
                    IF(I.EQ.NVAR)THEN
                        WRITE(*,'(A)') "ADCIRC NetCDF Variable not found in file."
#ifdef CMPI
                        CALL MPI_FINALIZE(IERR)
#endif
                        STOP
                    ENDIF
                ENDDO    
            
            END SUBROUTINE

            SUBROUTINE GetNETCDFVarID(NCID,VARID1,VARID2,NCOLS)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN)  :: NCID
                INTEGER,INTENT(OUT) :: VARID1
                INTEGER,INTENT(OUT) :: VARID2
                INTEGER,INTENT(OUT) :: NCOLS
                INTEGER             :: I
                INTEGER             :: J
                INTEGER             :: NVAR
                CHARACTER(200)      :: NC_NAME

                CALL CHECK(NF90_INQUIRE(NCID,NVARIABLES=NVAR))
        
                DO I = 1,NVAR
                    CALL CHECK(NF90_INQUIRE_VARIABLE(NCID,I,NAME=NC_NAME))
                    DO J = 1,SIZE(NETCDF_TYPES)
                        IF(TRIM(NC_NAME).EQ.TRIM(NETCDF_TYPES(J)))THEN
                            VARID1 = I
                            SELECT CASE(J)
                                CASE(13,18,21,33)
                                    NCOLS=2
                                    CALL CHECK(NF90_INQ_VARID(NCID,TRIM(NETCDF_TYPES(J+1)),VARID2))
                                CASE DEFAULT
                                    NCOLS=1
                                    VARID2=-1
                            END SELECT
                            RETURN
                        ENDIF
                    ENDDO
                ENDDO    
                STOP  
            END SUBROUTINE    
                    
            SUBROUTINE ReadMyNETCDFVariable(NCID,NUMNODES,RECORD,VARID1,VEC1,VARID2,VEC2)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN)           :: NCID
                INTEGER,INTENT(IN)           :: RECORD
                INTEGER,INTENT(IN)           :: VARID1
                INTEGER,INTENT(IN),OPTIONAL  :: VARID2
                INTEGER,INTENT(IN)           :: NUMNODES
                REAL,INTENT(OUT)             :: VEC1(:)
                REAL,INTENT(OUT),OPTIONAL    :: VEC2(:)
                CALL CHECK(NF90_GET_VAR(NCID,VARID1,VEC1,START=(/1,RECORD/),COUNT=(/NUMNODES,1/)))
                IF(PRESENT(VARID2).AND.PRESENT(VEC2))THEN
                    CALL CHECK(NF90_GET_VAR(NCID,VARID2,VEC2,START=(/1,RECORD/),COUNT=(/NUMNODES,1/)))
                ENDIF
                RETURN
            END SUBROUTINE    

            SUBROUTINE CHECK(Status)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN) :: Status
                INTEGER,ALLOCATABLE :: Dmy(:)
                IF(Status.NE.NF90_NOERR)THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(*,'(A,A)') "FATAL ERROR from ",TRIM(NF90_STRERROR(Status))
                    ENDIF
#ifdef CMPI
                    CALL MPI_FINALIZE(IERR)
#endif

#ifdef NETCDF_DEBUG
                    !...Cause an intentional seg fault
                    !   So we have an appropriate stack trace
                    !   for NetCDF errors
                    Dmy(1) = 0D0
#endif              

                    STOP
                ENDIF
            END SUBROUTINE
#endif        

            SUBROUTINE DATEADD(DATE1,ADDSEC,RESULTDATE)
                IMPLICIT NONE
                TYPE(DATEVAR),INTENT(IN)  :: DATE1
                INTEGER(8),   INTENT(IN)  :: ADDSEC
                TYPE(DATEVAR),INTENT(OUT) :: RESULTDATE
                INTEGER(8) :: JD

                JD=JULIANSEC(DATE1)
                RESULTDATE=GREGORIAN(JD+ADDSEC)

                RETURN

            END SUBROUTINE

            INTEGER(8) FUNCTION JULIANSEC(MYDATE)
                IMPLICIT NONE
                TYPE(DATEVAR),INTENT(IN) :: MYDATE
                INTEGER(8) :: I,J,K

                I = MYDATE%YEAR
                J = MYDATE%MONTH
                K = MYDATE%DAY

                IF((I.GT.2099).OR.(I.LT.1801))THEN
                    WRITE(*,*) "YEAR OUT OF RANGE!"
                    STOP
                ENDIF

                JULIANSEC = (K-32075+1461*(I+4800+(J-14)/12)/4+367* &
                    (J-2-(J-14)/12*12)/12-3*((I+4900+(J-14)/12)/100)/4)*86400

                JULIANSEC = JULIANSEC + (MYDATE%HOUR*3600) + (MYDATE%MINUTE*60) + MYDATE%SECOND


                RETURN

            END FUNCTION

            TYPE(DATEVAR) FUNCTION GREGORIAN(JUL)
                IMPLICIT NONE
                INTEGER(8),INTENT(IN) :: JUL
                INTEGER(8)         :: I,J,K,L,N
                INTEGER(8)         :: JULDAY
                INTEGER(8)         :: JULREM

                JULDAY = JUL/86400
                JULREM = JUL-(JULDAY*86400)

                L= JULDAY+68569
                N= 4*L/146097
                L= L-(146097*N+3)/4
                I= 4000*(L+1)/1461001
                L= L-1461*I/4+31
                J= 80*L/2447
                K= L-2447*J/80
                L= J/11
                J= J+2-12*L
                I= 100*(N-49)+I+L

                GREGORIAN%YEAR = I
                GREGORIAN%MONTH = J
                GREGORIAN%DAY = K
                GREGORIAN%HOUR = JULREM/3600
                JULREM = JULREM - GREGORIAN%HOUR*3600
                GREGORIAN%MINUTE = JULREM/60
                JULREM = JULREM - GREGORIAN%MINUTE*60
                GREGORIAN%SECOND = JULREM

                RETURN

            END FUNCTION

            SUBROUTINE CapitalizeWord(Word)

                IMPLICIT NONE

                INTRINSIC                   :: LEN_TRIM

                CHARACTER(*),INTENT(INOUT) :: Word

                INTEGER                     :: I

                DO I=1,LEN_TRIM(Word)

                    IF(Word(I:I).EQ."a") Word(I:I) = "A"
                    IF(Word(I:I).EQ."b") Word(I:I) = "B"
                    IF(Word(I:I).EQ."c") Word(I:I) = "C"
                    IF(Word(I:I).EQ."d") Word(I:I) = "D"
                    IF(Word(I:I).EQ."e") Word(I:I) = "E"
                    IF(Word(I:I).EQ."f") Word(I:I) = "F"
                    IF(Word(I:I).EQ."g") Word(I:I) = "G"
                    IF(Word(I:I).EQ."h") Word(I:I) = "H"
                    IF(Word(I:I).EQ."i") Word(I:I) = "I"
                    IF(Word(I:I).EQ."j") Word(I:I) = "J"
                    IF(Word(I:I).EQ."k") Word(I:I) = "K"
                    IF(Word(I:I).EQ."l") Word(I:I) = "L"
                    IF(Word(I:I).EQ."m") Word(I:I) = "M"
                    IF(Word(I:I).EQ."n") Word(I:I) = "N"
                    IF(Word(I:I).EQ."o") Word(I:I) = "O"
                    IF(Word(I:I).EQ."p") Word(I:I) = "P"
                    IF(Word(I:I).EQ."q") Word(I:I) = "Q"
                    IF(Word(I:I).EQ."r") Word(I:I) = "R"
                    IF(Word(I:I).EQ."s") Word(I:I) = "S"
                    IF(Word(I:I).EQ."t") Word(I:I) = "T"
                    IF(Word(I:I).EQ."u") Word(I:I) = "U"
                    IF(Word(I:I).EQ."v") Word(I:I) = "V"
                    IF(Word(I:I).EQ."w") Word(I:I) = "W"
                    IF(Word(I:I).EQ."x") Word(I:I) = "X"
                    IF(Word(I:I).EQ."y") Word(I:I) = "Y"
                    IF(Word(I:I).EQ."z") Word(I:I) = "Z"

                ENDDO

        END SUBROUTINE



        SUBROUTINE LowercaseWord(Word)

                IMPLICIT NONE

                INTRINSIC                   :: LEN_TRIM

                CHARACTER(50),INTENT(INOUT) :: Word

                INTEGER                     :: I

                DO I=1,LEN_TRIM(Word)

                    IF(Word(I:I).EQ."A") Word(I:I) = "a"
                    IF(Word(I:I).EQ."B") Word(I:I) = "b"
                    IF(Word(I:I).EQ."C") Word(I:I) = "c"
                    IF(Word(I:I).EQ."D") Word(I:I) = "d"
                    IF(Word(I:I).EQ."E") Word(I:I) = "e"
                    IF(Word(I:I).EQ."F") Word(I:I) = "f"
                    IF(Word(I:I).EQ."G") Word(I:I) = "g"
                    IF(Word(I:I).EQ."H") Word(I:I) = "h"
                    IF(Word(I:I).EQ."I") Word(I:I) = "i"
                    IF(Word(I:I).EQ."J") Word(I:I) = "j"
                    IF(Word(I:I).EQ."K") Word(I:I) = "k"
                    IF(Word(I:I).EQ."L") Word(I:I) = "l"
                    IF(Word(I:I).EQ."M") Word(I:I) = "m"
                    IF(Word(I:I).EQ."N") Word(I:I) = "n"
                    IF(Word(I:I).EQ."O") Word(I:I) = "o"
                    IF(Word(I:I).EQ."P") Word(I:I) = "p"
                    IF(Word(I:I).EQ."Q") Word(I:I) = "q"
                    IF(Word(I:I).EQ."R") Word(I:I) = "r"
                    IF(Word(I:I).EQ."S") Word(I:I) = "s"
                    IF(Word(I:I).EQ."T") Word(I:I) = "t"
                    IF(Word(I:I).EQ."U") Word(I:I) = "u"
                    IF(Word(I:I).EQ."V") Word(I:I) = "v"
                    IF(Word(I:I).EQ."W") Word(I:I) = "w"
                    IF(Word(I:I).EQ."X") Word(I:I) = "x"
                    IF(Word(I:I).EQ."Y") Word(I:I) = "y"
                    IF(Word(I:I).EQ."Z") Word(I:I) = "z"

                ENDDO

        END SUBROUTINE



        REAL FUNCTION ComputeDistance(Lon1, Lat1, Lon2, Lat2)

                IMPLICIT NONE

                INTRINSIC :: COS
                INTRINSIC :: SQRT

                REAL :: AdjLat1
                REAL :: AdjLat2
                REAL :: AdjLon1
                REAL :: AdjLon2
                REAL :: Deg2Rad = 0.01745329252d0
                REAL :: EarthRad = 6378206.4d0
                REAL :: Lat1
                REAL :: Lat2
                REAL :: Lon1
                REAL :: Lon2
                REAL :: SFEA0
                REAL :: SLAM0

                Deg2Rad = 0.01745329252d0
                EarthRad = 6378206.4d0
                SLAM0 = 265.5d0
                SFEA0 = 29.0d0

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


        INTEGER FUNCTION Count63(InputFile,FileFormat)

#ifdef NETCDF
                USE netcdf
#endif

                IMPLICIT NONE
                
                INTEGER        :: I
                INTEGER        :: JunkI
#ifdef NETCDF
                INTEGER,DIMENSION(NF90_MAX_VAR_DIMS) :: NC_DimIDs
                INTEGER        :: NC_ID
                INTEGER        :: NC_Var
#endif
                INTEGER        :: NumNodes
                INTEGER        :: Timesteps
                
                REAL           :: JunkR
                
                CHARACTER(50)  :: FileFormat
                CHARACTER(100) :: InputFile
                CHARACTER(100) :: JunkC

                IF(TRIM(FileFormat).EQ."ASCII")THEN
                    OPEN(FILE=TRIM(InputFile),UNIT=100,ACTION="READ") 
                    READ(100,*) JunkC
                    READ(100,*) JunkI, NumNodes, JunkC  
                    Count63 = 0
                    DO
                        READ(100,*,END=100) JunkR, JunkR
                        DO I = 1,NumNodes
                            READ(100,*) JunkI
                        ENDDO
                        Count63 = Count63 + 1
                    ENDDO
100                 CONTINUE                
                    CLOSE(100)    
                ELSEIF(TRIM(FileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    CALL Check(NF90_OPEN(TRIM(InputFile),NF90_NOWRITE,NC_ID))
                    CALL Check(NF90_INQ_VARID(NC_ID,'time',NC_Var))
                    CALL Check(NF90_INQUIRE_VARIABLE(NC_ID,NC_Var,dimids=NC_DimIDs))
                    CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_DimIDs(1),len=Count63))
                    CALL Check(NF90_CLOSE(NC_ID))
#endif
                ENDIF

        END FUNCTION



        SUBROUTINE CreateCPTFiles

                

                IMPLICIT NONE

                INTRINSIC                    :: ABS
                INTRINSIC                    :: MOD
                INTRINSIC                    :: NINT
                INTRINSIC                    :: TRIM

                CHARACTER(LEN=1)             :: JunkC
                CHARACTER(LEN=250)           :: Line
                CHARACTER(LEN=250)           :: LineB
                CHARACTER(LEN=250)           :: LineF
                CHARACTER(LEN=250)           :: LineN

                INTEGER                      :: Counter
                INTEGER                      :: I
                INTEGER                      :: J
                INTEGER                      :: K
                INTEGER                      :: L
                INTEGER,ALLOCATABLE          :: MakeGray(:)
                INTEGER                      :: NumColors
                INTEGER                      :: NumDiffContours
                INTEGER                      :: NumGMTColors
                INTEGER                      :: TempIndex
                INTEGER                      :: TempSplitBy

                LOGICAL                      :: FoundB
                LOGICAL                      :: FoundF
                LOGICAL                      :: FoundN

                DOUBLE PRECISION             :: CurrentContour
                DOUBLE PRECISION,ALLOCATABLE :: DiffContours(:)

                TYPE ColorData 
                    DOUBLE PRECISION :: Value1
                    DOUBLE PRECISION :: Red1
                    DOUBLE PRECISION :: Green1
                    DOUBLE PRECISION :: Blue1
                    DOUBLE PRECISION :: Value2
                    DOUBLE PRECISION :: Red2
                    DOUBLE PRECISION :: Green2
                    DOUBLE PRECISION :: Blue2
                END TYPE
                TYPE(ColorData), ALLOCATABLE, DIMENSION(:) :: GMTColors
                TYPE(ColorData), ALLOCATABLE, DIMENSION(:) :: PaletteColors

                FoundB = .FALSE.
                FoundF = .FALSE.
                FoundN = .FALSE.

                IF(INDEX(Palette,"INTERVALS").GT.0)THEN

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

                IF(INDEX(Palette,"SMS").GT.0)THEN

                    OPEN(UNIT=14,FILE=TRIM(SMSPalette),ACTION="READ")

                    READ(UNIT=14,FMT='(A)') JunkC
                    READ(UNIT=14,FMT='(A)') JunkC
                    READ(UNIT=14,FMT=*)     JunkC, NumColors
                    READ(UNIT=14,FMT='(A)') JunkC

                    ALLOCATE(PaletteColors(1:NumColors))

                    DO I=1,NumColors

                        READ(UNIT=14,FMT=*) PaletteColors(I)%Value1, &
                                            PaletteColors(I)%Red1,   &
                                            PaletteColors(I)%Green1, &
                                            PaletteColors(I)%Blue1

                    ENDDO

                    CLOSE(UNIT=14,STATUS="KEEP")

                ELSEIF(INDEX(Palette,"CPT").GT.0)THEN

                    OPEN(UNIT=14,FILE=TRIM(SMSPalette),ACTION="READ")

                    NumColors = 0

                    DO
                        READ(UNIT=14,FMT='(A)',END=9611) Line
                        IF((Line(1:1).NE."#").AND.(LEN_TRIM(Line).GT.0).AND. &
                           (Line(1:1).NE."B").AND.(Line(1:1).NE."F").AND.    &
                           (Line(1:1).NE."N"))THEN
                            NumColors = NumColors + 1
                        ENDIF
                    ENDDO

9611                CONTINUE

                    NumColors = NumColors + 1

                    CLOSE(UNIT=14,STATUS="KEEP")

                    ALLOCATE(PaletteColors(1:NumColors))

                    OPEN(UNIT=14,FILE=TRIM(SMSPalette),ACTION="READ")

                    Counter = 0

                    DO
                        READ(UNIT=14,FMT='(A)',END=9612) Line
                        IF(Line(1:1).EQ."B")THEN
                            FoundB = .TRUE.
                            LineB = Line
                        ELSEIF(Line(1:1).EQ."F")THEN
                            FoundF = .TRUE.
                            LineF = Line
                        ELSEIF(Line(1:1).EQ."N")THEN
                            FoundN = .TRUE.
                            LineN = Line
                        ELSEIF((Line(1:1).NE."#").AND.(LEN_TRIM(Line).GT.0))THEN
                            Counter = Counter + 1
                            IF(Counter.LT.NumColors-1)THEN
                                READ(UNIT=Line,FMT=*) PaletteColors(Counter)%Value1, &
                                                      PaletteColors(Counter)%Red1,   &
                                                      PaletteColors(Counter)%Green1, &
                                                      PaletteColors(Counter)%Blue1
                            ELSE
                                READ(UNIT=Line,FMT=*) PaletteColors(Counter)%Value1, &
                                                      PaletteColors(Counter)%Red1,   &
                                                      PaletteColors(Counter)%Green1, &
                                                      PaletteColors(Counter)%Blue1,  &
                                                      PaletteColors(Counter+1)%Value1, &
                                                      PaletteColors(Counter+1)%Red1,   &
                                                      PaletteColors(Counter+1)%Green1, &
                                                      PaletteColors(Counter+1)%Blue1
                            ENDIF
                        ENDIF
                    ENDDO

9612                CONTINUE

                    CLOSE(UNIT=14,STATUS="KEEP")

                ELSE

                    NumColors = 3

                    ALLOCATE(PaletteColors(1:NumColors))

                    PaletteColors(1)%Value1 = 0.0d0
                    PaletteColors(1)%Red1   = 0
                    PaletteColors(1)%Green1 = 0
                    PaletteColors(1)%Blue1  = 255

                    PaletteColors(2)%Value1 = 0.5d0
                    PaletteColors(2)%Red1   = 0
                    PaletteColors(2)%Green1 = 255
                    PaletteColors(2)%Blue1  = 0

                    PaletteColors(3)%Value1 = 1.0d0
                    PaletteColors(3)%Red1   = 255 
                    PaletteColors(3)%Green1 = 0
                    PaletteColors(3)%Blue1  = 0

                ENDIF

                DO K=1,3

                    IF(K.EQ.1)THEN

                        TempSplitBy = SplitBy

                    ELSEIF(K.EQ.2)THEN

                        TempSplitBy = 1

                    ELSEIF(K.EQ.3)THEN

                        IF(INDEX(Palette,"INTERVALS").GT.0)THEN
                            TempSplitBy = 1
                        ELSE
                            TempSplitBy = SplitBy
                        ENDIF

                    ENDIF

                    IF(INDEX(Palette,"INTERVALS").GT.0)THEN
                        NumGMTColors = (NumDiffContours-1)*TempSplitBy
                    ELSE
                        NumGMTColors = NINT((ContourMax-ContourMin)/ContourInterval)*TempSplitBy
                    ENDIF

                    ALLOCATE(GMTColors(1:NumGMTColors))

                    IF(INDEX(Palette,"INTERVALS").GT.0)THEN

                        DO I=1,NumGMTColors
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
                                        
                                IF((MakeGray(I).EQ.1).AND.(IfGoogle.EQ.0))THEN

                                    GMTColors(TempIndex)%Red1   = 225
                                    GMTColors(TempIndex)%Green1 = 225
                                    GMTColors(TempIndex)%Blue1  = 225
                                    GMTColors(TempIndex)%Red2   = 225
                                    GMTColors(TempIndex)%Green2 = 225
                                    GMTColors(TempIndex)%Blue2  = 225

                                ELSEIF((MakeGray(I).EQ.1).AND.(IfGoogle.EQ.1))THEN

                                    GMTColors(TempIndex)%Red1   = 255
                                    GMTColors(TempIndex)%Green1 = 255
                                    GMTColors(TempIndex)%Blue1  = 255
                                    GMTColors(TempIndex)%Red2   = 255
                                    GMTColors(TempIndex)%Green2 = 255
                                    GMTColors(TempIndex)%Blue2  = 255

                                ELSE

                                    CurrentContour = (REAL(TempIndex) - 1.0)/ &
                                            ((REAL(NumDiffContours)-1.0)*REAL(TempSplitBy)-1.0)

                                    IF(PaletteColors(1)%Value1.GT.CurrentContour)THEN

                                        GMTColors(TempIndex)%Red1   = PaletteColors(1)%Red1
                                        GMTColors(TempIndex)%Green1 = PaletteColors(1)%Green1
                                        GMTColors(TempIndex)%Blue1  = PaletteColors(1)%Blue1
                                        GMTColors(TempIndex)%Red2   = PaletteColors(1)%Red1
                                        GMTColors(TempIndex)%Green2 = PaletteColors(1)%Green1
                                        GMTColors(TempIndex)%Blue2  = PaletteColors(1)%Blue1

                                    ELSEIF(PaletteColors(NumColors)%Value1.LT.CurrentContour)THEN

                                        GMTColors(TempIndex)%Red1   = PaletteColors(NumColors)%Red1
                                        GMTColors(TempIndex)%Green1 = PaletteColors(NumColors)%Green1
                                        GMTColors(TempIndex)%Blue1  = PaletteColors(NumColors)%Blue1
                                        GMTColors(TempIndex)%Red2   = PaletteColors(NumColors)%Red1
                                        GMTColors(TempIndex)%Green2 = PaletteColors(NumColors)%Green1
                                        GMTColors(TempIndex)%Blue2  = PaletteColors(NumColors)%Blue1

                                    ELSE

                                        DO L=2,NumColors

                                            IF((PaletteColors(L-1)%Value1.LE.CurrentContour).AND.        &
                                                   (PaletteColors(L)%Value1.GE.CurrentContour))THEN

                                                GMTColors(TempIndex)%Red1   = PaletteColors(L-1)%Red1+   &
                                                        (CurrentContour-PaletteColors(L-1)%Value1)/      &
                                                        (PaletteColors(L)%Value1-PaletteColors(L-1)%Value1)* &
                                                        (PaletteColors(L)%Red1-PaletteColors(L-1)%Red1)
                                                GMTColors(TempIndex)%Green1 = PaletteColors(L-1)%Green1+ &
                                                        (CurrentContour-PaletteColors(L-1)%Value1)/      &
                                                        (PaletteColors(L)%Value1-PaletteColors(L-1)%Value1)* &
                                                        (PaletteColors(L)%Green1-PaletteColors(L-1)%Green1)
                                                GMTColors(TempIndex)%Blue1  = PaletteColors(L-1)%Blue1+  &
                                                        (CurrentContour-PaletteColors(L-1)%Value1)/      &
                                                        (PaletteColors(L)%Value1-PaletteColors(L-1)%Value1)* &
                                                        (PaletteColors(L)%Blue1-PaletteColors(L-1)%Blue1)
                                                GMTColors(TempIndex)%Red2   = PaletteColors(L-1)%Red1+   &
                                                        (CurrentContour-PaletteColors(L-1)%Value1)/      &
                                                        (PaletteColors(L)%Value1-PaletteColors(L-1)%Value1)* &
                                                        (PaletteColors(L)%Red1-PaletteColors(L-1)%Red1)
                                                GMTColors(TempIndex)%Green2 = PaletteColors(L-1)%Green1+ &
                                                        (CurrentContour-PaletteColors(L-1)%Value1)/      &
                                                        (PaletteColors(L)%Value1-PaletteColors(L-1)%Value1)* &
                                                        (PaletteColors(L)%Green1-PaletteColors(L-1)%Green1)
                                                GMTColors(TempIndex)%Blue2  = PaletteColors(L-1)%Blue1+  &
                                                        (CurrentContour-PaletteColors(L-1)%Value1)/      &
                                                        (PaletteColors(L)%Value1-PaletteColors(L-1)%Value1)* &
                                                        (PaletteColors(L)%Blue1-PaletteColors(L-1)%Blue1)

                                            ENDIF

                                        ENDDO

                                    ENDIF

                                ENDIF

                            ENDDO

                        ENDDO

                    ELSE

                        DO I=1,NumGMTColors

                            IF(TRIM(Palette).EQ."CPT")THEN
                                CurrentContour = ((I-1.0)*ContourInterval/TempSplitBy)/(ContourMax-ContourMin)
                            ELSE
                                CurrentContour = ((I-0.5)*ContourInterval/TempSplitBy)/(ContourMax-ContourMin)
                            ENDIF

                            IF(PaletteColors(1)%Value1.GT.CurrentContour)THEN

                                GMTColors(I)%Value1 = ContourMin+(I-1)*ContourInterval/TempSplitBy
                                GMTColors(I)%Red1   = PaletteColors(1)%Red1
                                GMTColors(I)%Green1 = PaletteColors(1)%Green1
                                GMTColors(I)%Blue1  = PaletteColors(1)%Blue1
                                GMTColors(I)%Value2 = ContourMin+(I)*ContourInterval/TempSplitBy
                                GMTColors(I)%Red2   = PaletteColors(1)%Red1
                                GMTColors(I)%Green2 = PaletteColors(1)%Green1
                                GMTColors(I)%Blue2  = PaletteColors(1)%Blue1

                            ELSEIF(PaletteColors(NumColors)%Value1.LT.CurrentContour)THEN

                                GMTColors(I)%Value1 = ContourMin+(I-1)*ContourInterval/TempSplitBy
                                GMTColors(I)%Red1   = PaletteColors(NumColors)%Red1
                                GMTColors(I)%Green1 = PaletteColors(NumColors)%Green1
                                GMTColors(I)%Blue1  = PaletteColors(NumColors)%Blue1
                                GMTColors(I)%Value2 = ContourMin+(I)*ContourInterval/TempSplitBy
                                GMTColors(I)%Red2   = PaletteColors(NumColors)%Red1
                                GMTColors(I)%Green2 = PaletteColors(NumColors)%Green1
                                GMTColors(I)%Blue2  = PaletteColors(NumColors)%Blue1

                            ELSE

                                DO J=2,NumColors

                                    IF((PaletteColors(J-1)%Value1.LE.CurrentContour).AND.                  &
                                           (PaletteColors(J)%Value1.GE.CurrentContour))THEN

                                        GMTColors(I)%Value1 = ContourMin+(I-1)*ContourInterval/TempSplitBy
                                        GMTColors(I)%Red1   = PaletteColors(J-1)%Red1+                     &
                                                (CurrentContour-PaletteColors(J-1)%Value1)/                &
                                                (PaletteColors(J)%Value1-PaletteColors(J-1)%Value1)*       &
                                                (PaletteColors(J)%Red1-PaletteColors(J-1)%Red1)
                                        GMTColors(I)%Green1 = PaletteColors(J-1)%Green1+                   &
                                                (CurrentContour-PaletteColors(J-1)%Value1)/                &
                                                (PaletteColors(J)%Value1-PaletteColors(J-1)%Value1)*       &
                                                (PaletteColors(J)%Green1-PaletteColors(J-1)%Green1)
                                        GMTColors(I)%Blue1  = PaletteColors(J-1)%Blue1+                    &
                                                (CurrentContour-PaletteColors(J-1)%Value1)/                &
                                                (PaletteColors(J)%Value1-PaletteColors(J-1)%Value1)*       &
                                                (PaletteColors(J)%Blue1-PaletteColors(J-1)%Blue1)
                                        GMTColors(I)%Value2 = ContourMin+(I)*ContourInterval/TempSplitBy
                                        GMTColors(I)%Red2   = PaletteColors(J-1)%Red1+                     &
                                                (CurrentContour-PaletteColors(J-1)%Value1)/                &
                                                (PaletteColors(J)%Value1-PaletteColors(J-1)%Value1)*       &
                                                (PaletteColors(J)%Red1-PaletteColors(J-1)%Red1)
                                        GMTColors(I)%Green2 = PaletteColors(J-1)%Green1+                   &
                                                (CurrentContour-PaletteColors(J-1)%Value1)/                &
                                                (PaletteColors(J)%Value1-PaletteColors(J-1)%Value1)*       &
                                                (PaletteColors(J)%Green1-PaletteColors(J-1)%Green1)
                                        GMTColors(I)%Blue2  = PaletteColors(J-1)%Blue1+                    &
                                                (CurrentContour-PaletteColors(J-1)%Value1)/                &
                                                (PaletteColors(J)%Value1-PaletteColors(J-1)%Value1)*       &
                                                (PaletteColors(J)%Blue1-PaletteColors(J-1)%Blue1)

                                    ENDIF

                                ENDDO

                            ENDIF

                        ENDDO

                    ENDIF

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

                    IF((K.EQ.1).AND.(TRIM(ContourFileType).NE."HWM-CSV"))THEN

                        IF(OptimizeContours.EQ.0)THEN

                            IF((ContourFileType.EQ."ADCIRC-OUTPUT").OR.                                  &
                               (ContourFileType.EQ."ADCIRC-OUTPUT-LIST"))THEN
                                WRITE(UNIT=15,FMT='(A,2X,I3,2X,I3,2X,I3,2X,F16.8,2X,I3,2X,I3,2X,I3)')    &
                                                    "-50.000000000",                                     &
                                                    NINT(GMTColors(1)%Red1),                             &
                                                    NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1), &
                                                    GMTColors(1)%Value1,                                 &
                                                    NINT(GMTColors(1)%Red1),                             &
                                                    NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1)
                            ELSE
                                WRITE(UNIT=15,FMT='(A,2X,I3,2X,I3,2X,I3,2X,F16.8,2X,I3,2X,I3,2X,I3)')    &
                                                    "-99990.000000000",                                  &
                                                    NINT(GMTColors(1)%Red1),                             &
                                                    NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1), &
                                                    GMTColors(1)%Value1,                                 &
                                                    NINT(GMTColors(1)%Red1),                             &
                                                    NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1)
                            ENDIF

                        ELSEIF(OptimizeContours.EQ.1)THEN

                            WRITE(UNIT=15,FMT='(A,2X,I3,2X,I3,2X,I3,2X,A,2X,I3,2X,I3,2X,I3)')        &
                                                "-88888.500000000",255,255,255,                      &
                                                "-88887.500000000",255,255,255

                            WRITE(UNIT=15,FMT='(A,2X,I3,2X,I3,2X,I3,2X,A,2X,I3,2X,I3,2X,I3)')        &
                                                "-88887.500000000",255,255,255,                      &
                                                "-88886.500000000",                                  &
                                                NINT(GMTColors(1)%Red1),                             &
                                                NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1)

                            WRITE(UNIT=15,FMT='(A,2X,I3,2X,I3,2X,I3,2X,F16.8,2X,I3,2X,I3,2X,I3)')    &
                                                "-88886.500000000",                                  &
                                                NINT(GMTColors(1)%Red1),                             &
                                                NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1), &
                                                GMTColors(1)%Value1,                                 &
                                                NINT(GMTColors(1)%Red1),                             &
                                                NINT(GMTColors(1)%Green1), NINT(GMTColors(1)%Blue1)

                        ENDIF

                    ENDIF

                    DO I=1,NumGMTColors
                   
                        WRITE(UNIT=15,FMT='(2(F16.8,2X,I3,2X,I3,2X,I3,2X))')                     &
                                            GMTColors(I)%Value1,                                 &
                                            NINT(GMTColors(I)%Red1),                             &
                                            NINT(GMTColors(I)%Green1), NINT(GMTColors(I)%Blue1), &
                                            GMTColors(I)%Value2,                                 &
                                            NINT(GMTColors(I)%Red2),                             &
                                            NINT(GMTColors(I)%Green2), NINT(GMTColors(I)%Blue2)

                    ENDDO

                    IF(INDEX(Palette,"INTERVALS").GT.0)THEN

                        IF((K.EQ.1).AND.((ContourFileNumCols.EQ.1).OR.     &
                           (TRIM(ContourFileType).EQ."GRID-BATH").OR.      &
                           (INDEX(ContourFileType,"GRID-DECOMP").GT.0).OR. &
                           (TRIM(ContourFileType).EQ."GRID-SIZE")))THEN

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

                    ENDIF

                    IF(IfPlotBackgroundImages.EQ.2)THEN
                        WRITE(UNIT=15,FMT='(A)') "B -"
                    ELSE
                        IF(FoundB)THEN
                            WRITE(UNIT=15,FMT='(A)') TRIM(LineB)
                        ELSE
                            IF(IfGoogle.EQ.0)THEN
                                WRITE(UNIT=15,FMT='(A)') "B  215  215  215"
                            ELSE
                                WRITE(UNIT=15,FMT='(A)') "B  255  255  255"
                            ENDIF    
                        ENDIF
                    ENDIF
                    
                    IF(FoundF)THEN
                        WRITE(UNIT=15,FMT='(A)') TRIM(LineF)
                    ELSE
#ifdef CBARLIMIT
                        WRITE(UNIT=15,FMT='(A)') "F  160   82   45"
                        WRITE(*,*) "F  160   82   45"
#else
                        WRITE(UNIT=15,FMT='(A,I3,A,I3,A,I3)') "F  ",                       &
                                                NINT(GMTColors(NumGMTColors)%Red2),"  ",   &
                                                NINT(GMTColors(NumGMTColors)%Green2),"  ", &
                                                NINT(GMTColors(NumGMTColors)%Blue2)
#endif
                    ENDIF
                    
                    IF(IfPlotBackgroundImages.EQ.2)THEN
                        WRITE(UNIT=15,FMT='(A)') "N -"
                    ELSE    
                        IF(FoundN)THEN
                            WRITE(UNIT=15,FMT='(A)') TRIM(LineN)
                        ELSE
                            WRITE(UNIT=15,FMT='(A)') "N  255  255  255"
                        ENDIF    
                    ENDIF

                    CLOSE(UNIT=15,STATUS="KEEP")

                    IF(ALLOCATED(GMTColors)) DEALLOCATE(GMTColors)

                ENDDO

                IF(ALLOCATED(DiffContours)) DEALLOCATE(DiffContours)        
                IF(ALLOCATED(MakeGray)) DEALLOCATE(MakeGray)        
                IF(ALLOCATED(PaletteColors)) DEALLOCATE(PaletteColors)

                IF(Verbose.GE.3)THEN
                    WRITE(*,'(A,I4.4,A)') "Core ", MyRank, " created the contour palettes."
                ENDIF

        END SUBROUTINE



        SUBROUTINE FindContourMinMax

                

#ifdef NETCDF
                USE netcdf
#endif

                IMPLICIT NONE

#ifdef NETCDF
                INTEGER             :: NC_DimIDs(NF90_MAX_VAR_DIMS)
                INTEGER             :: NC_File
                INTEGER             :: NC_ID1
                INTEGER             :: NC_ID2
                INTEGER             :: NC_Status
                INTEGER             :: NC_Var
                INTEGER             :: NC_Var2
#endif

                INTRINSIC           :: CEILING
                INTRINSIC           :: FLOOR
                INTRINSIC           :: INDEX
                INTRINSIC           :: TRIM

                CHARACTER(LEN=1)    :: JunkC

                INTEGER             :: CounterLocal
                INTEGER             :: I
                INTEGER             :: J
                INTEGER             :: JunkI
                INTEGER,ALLOCATABLE :: NC(:,:)
                INTEGER             :: NumElemsGlobal
                INTEGER             :: NumNodes1
                INTEGER             :: NumNodes2
                INTEGER             :: NumRecsLocal

                REAL,ALLOCATABLE    :: Bath1(:)
                REAL,ALLOCATABLE    :: Bath2(:)
                REAL                :: DefaultValue
                REAL                :: Dist
                REAL(8)             :: JunkR
                REAL(8)             :: JunkR2
                REAL,ALLOCATABLE    :: Lat(:)
                REAL,ALLOCATABLE    :: Lon(:)
                REAL                :: Max
                REAL                :: Min
                REAL,ALLOCATABLE    :: U1(:)
                REAL,ALLOCATABLE    :: U2(:)
                REAL,ALLOCATABLE    :: V1(:)
                REAL,ALLOCATABLE    :: V2(:)
                REAL,ALLOCATABLE    :: Vels1(:)
                REAL,ALLOCATABLE    :: Vels2(:)

                CounterLocal = 1
                Max = -9999.0
                Min =  9999.0

                IF(IfPlotFilledContours.EQ.1)THEN

                    IF(TRIM(ContourFileType).EQ."ADCIRC-OUTPUT")THEN

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) NumRecsLocal, NumNodesGlobal, JunkR, JunkI, ContourFileNumCols
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_OPEN(TRIM(ContourFile1),NF90_NOWRITE,NC_ID1))
                            CALL Check(NF90_INQ_DIMID(NC_ID1,"time",NC_Var))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_Var,len=NumRecsLocal))
                            CALL Check(NF90_INQ_DIMID(NC_ID1,"node",NC_Var))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_Var,len=NumNodesGlobal))
                            CALL FindMyNetCDFVariable(NC_ID1)
#endif
                        ENDIF

                        ALLOCATE(U1(1:NumNodesGlobal))
                        ALLOCATE(V1(1:NumNodesGlobal))
                        ALLOCATE(Vels1(1:NumNodesGlobal))

                        loopminmax1: DO J=1,NumRecsLocal 

                            IF(J.LT.RecordsList(CounterLocal))THEN

                                IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN 
                                    NumNodes1 = NumNodesGlobal
                                    CALL ReadTimeStamp(19,LEN_TRIM(ContourFile1),TRIM(ContourFile1),JunkR,JunkR2,&
                                                       NumNodes1,DefaultValue)
                                    DO I=1,NumNodes1
                                        READ(UNIT=19,FMT=*) JunkI
                                    ENDDO
                                ENDIF

                            ELSE

                                CounterLocal = CounterLocal + 1

                                IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                    NumNodes1 = NumNodesGlobal
                                    CALL ReadTimeStamp(19,LEN_TRIM(ContourFile1),TRIM(ContourFile1),JunkR,JunkR2,&
                                                       NumNodes1,DefaultValue)
                                ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                    NumNodes1 = NumNodesGlobal
                                    CALL GetNETCDFVarID(NC_ID1,NC_VAR,NC_VAR2,ContourFileNumCols)
                                    
                                    ierr = NF90_GET_ATT(NC_ID1,NC_Var,'_FillValue',DefaultValue)
                                    IF(ierr.NE.NF90_NOERR)THEN
                                        CALL Check(NF90_GET_ATT(NC_ID1,NF90_GLOBAL,'_FillValue',DefaultValue))
                                    ENDIF    

#endif
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

                                IF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                    IF(ContourFileNumCols.EQ.1)THEN
                                        CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,RECORD=J,&
                                                                  VARID1=NC_Var,VEC1=U1)
                                    ELSE
                                        CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,RECORD=J,&
                                                                  VARID1=NC_VAR,VEC1=U1,VARID2=NC_VAR2,VEC2=V1)
                                    ENDIF    
#endif
                                ENDIF

                                DO I=1,NumNodes1

                                    IF(ContourFileNumCols.EQ.1)THEN

                                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                            READ(UNIT=19,FMT=*) JunkI, U1(JunkI)
                                            IF(U1(JunkI).GT.-99998.0)THEN
                                                U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                            ENDIF
                                            Vels1(JunkI) = U1(JunkI)
                                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                            IF(U1(I).GT.-99998.0)THEN
                                                U1(I) = U1(I) * ContourConversionFactor
                                            ENDIF
                                            Vels1(I) = U1(I)
#endif
                                        ENDIF

                                    ELSEIF(ContourFileNumCols.EQ.2)THEN

                                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                            READ(UNIT=19,FMT=*) JunkI, U1(JunkI), V1(JunkI)
                                            U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                            V1(JunkI) = V1(JunkI) * ContourConversionFactor
                                            Vels1(JunkI) = SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))
#ifdef DRYZEROVEL                                            
                                            IF(Vels1(JunkI).EQ.0D0)Vels1(JunkI)=-99999D0
#endif                                            
                                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                            U1(I) = U1(I) * ContourConversionFactor
                                            V1(I) = V1(I) * ContourConversionFactor
                                            Vels1(I) = SQRT(U1(I)*U1(I)+V1(I)*V1(I))
#ifdef DRYZEROVEL                                            
                                            IF(Vels1(I).EQ.0D0)Vels1(I)=-99999D0
#endif                                            
#endif
                                        ENDIF

                                    ENDIF

                                ENDDO

                                DO I=1,NumNodes1

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

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            CLOSE(UNIT=19,STATUS="KEEP")
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_CLOSE(NC_ID1))
#endif
                        ENDIF

                    ELSEIF(INDEX(ContourFileType,"GRID-DECOMP").GT.0)THEN

                        Min = 0.0
                        Max = 1.0

                    ELSEIF((TRIM(ContourFileType).EQ."GRID-BATH").OR.      &
                           (TRIM(ContourFileType).EQ."GRID-SIZE"))THEN

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) NumElemsGlobal, NumNodesGlobal
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_OPEN(ContourFile1,NF90_NOWRITE,NC_ID1))
                            CALL Check(NF90_INQ_DIMID(NC_ID1,"nele",NC_Var))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_Var,len=NumElemsGlobal))
                            CALL Check(NF90_INQ_DIMID(NC_ID1,"node",NC_Var))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_Var,len=NumNodesGlobal))
#endif
                        ENDIF

                        IF(TRIM(ContourFileType).EQ."GRID-BATH")THEN
                            ALLOCATE(Bath1(1:NumNodesGlobal))
                        ELSEIF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN
                            ALLOCATE(Lat(1:NumNodesGlobal))
                            ALLOCATE(Lon(1:NumNodesGlobal))
                        ENDIF

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            DO I=1,NumNodesGlobal
                                IF(TRIM(ContourFileType).EQ."GRID-BATH")THEN
                                    READ(UNIT=19,FMT=*) JunkI, JunkR, JunkR, Bath1(I)
                                    Bath1(I) = Bath1(I) * ContourConversionFactor
                                ELSEIF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN
                                    READ(UNIT=19,FMT=*) JunkI, Lon(I), Lat(I), JunkR
                                ENDIF
                            ENDDO
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            IF(TRIM(ContourFileType).EQ."GRID-BATH")THEN
                                CALL Check(NF90_INQ_VARID(NC_ID1,'depth',JunkI))
                                CALL Check(NF90_GET_VAR(NC_ID1,JunkI,Bath1))
                                DO I=1,NumNodesGlobal
                                    Bath1(I) = Bath1(I) * ContourConversionFactor
                                ENDDO
                            ELSEIF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN
                                CALL Check(NF90_INQ_VARID(NC_ID1,'x',JunkI))
                                CALL Check(NF90_GET_VAR(NC_ID1,JunkI,Lon))
                                CALL Check(NF90_INQ_VARID(NC_ID1,'y',JunkI))
                                CALL Check(NF90_GET_VAR(NC_ID1,JunkI,Lat))
                            ENDIF
#endif
                        ENDIF

                        IF(TRIM(ContourFileType).EQ."GRID-BATH")THEN

                            DO I=1,NumNodesLocal
                                IF(Bath1(XYZNodes(I)).LT.Min)THEN
                                    Min = Bath1(XYZNodes(I))
                                ENDIF
                                IF(Bath1(XYZNodes(I)).GT.Max)THEN
                                    Max = Bath1(XYZNodes(I))
                                ENDIF
                            ENDDO

                        ELSEIF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN

                            ALLOCATE(NC(1:3,1:NumElemsGlobal))

                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                DO I=1,NumElemsGlobal
                                    READ(UNIT=19,FMT=*) JunkI, JunkI, NC(1,I), NC(2,I), NC(3,I)
                                ENDDO
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_INQ_VARID(NC_ID1,'element',JunkI))
                                CALL Check(NF90_GET_VAR(NC_ID1,JunkI,NC))
#endif
                            ENDIF

                            DO I=1,NumElemsGlobal

                                DO J=1,NumNodesLocal

                                    IF(NC(1,I).EQ.XYZNodes(J))THEN

                                        Dist = ComputeDistance(Lon(NC(1,I)),Lat(NC(1,I)),Lon(NC(2,I)),Lat(NC(2,I)))
                                        IF(Dist.LT.Min)THEN
                                            Min = Dist
                                        ENDIF
                                        IF(Dist.GT.Max)THEN
                                            Max = Dist
                                        ENDIF

                                        Dist = ComputeDistance(Lon(NC(1,I)),Lat(NC(1,I)),Lon(NC(3,I)),Lat(NC(3,I)))
                                        IF(Dist.LT.Min)THEN
                                            Min = Dist
                                        ENDIF
                                        IF(Dist.GT.Max)THEN
                                            Max = Dist
                                        ENDIF

                                    ELSEIF(NC(2,I).EQ.XYZNodes(J))THEN

                                        Dist = ComputeDistance(Lon(NC(2,I)),Lat(NC(2,I)),Lon(NC(1,I)),Lat(NC(1,I)))
                                        IF(Dist.LT.Min)THEN
                                            Min = Dist
                                        ENDIF
                                        IF(Dist.GT.Max)THEN
                                            Max = Dist
                                        ENDIF

                                        Dist = ComputeDistance(Lon(NC(2,I)),Lat(NC(2,I)),Lon(NC(3,I)),Lat(NC(3,I)))
                                        IF(Dist.LT.Min)THEN
                                            Min = Dist
                                        ENDIF
                                        IF(Dist.GT.Max)THEN
                                            Max = Dist
                                        ENDIF

                                    ELSEIF(NC(3,I).EQ.XYZNodes(J))THEN

                                        Dist = ComputeDistance(Lon(NC(3,I)),Lat(NC(3,I)),Lon(NC(1,I)),Lat(NC(1,I)))
                                        IF(Dist.LT.Min)THEN
                                            Min = Dist
                                        ENDIF
                                        IF(Dist.GT.Max)THEN
                                            Max = Dist
                                        ENDIF

                                        Dist = ComputeDistance(Lon(NC(3,I)),Lat(NC(3,I)),Lon(NC(2,I)),Lat(NC(2,I)))
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

                        IF(TRIM(ContourFileType).EQ."GRID-BATH")THEN
                            IF(ALLOCATED(Bath1)) DEALLOCATE(Bath1)
                        ELSEIF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN
                            IF(ALLOCATED(Lat)) DEALLOCATE(Lat)
                            IF(ALLOCATED(Lon)) DEALLOCATE(Lon)
                            IF(ALLOCATED(NC))  DEALLOCATE(NC)
                        ENDIF

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            CLOSE(UNIT=19,STATUS="KEEP")
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_CLOSE(NC_ID1))
#endif
                        ENDIF

                    ENDIF

                    ContourMin = FLOOR(Min)
                    ContourMax = CEILING(Max)

                ELSEIF(IfPlotFilledContours.EQ.2)THEN

                    IF(TRIM(ContourFileType).EQ."ADCIRC-OUTPUT")THEN

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) NumRecsLocal, NumNodes1, JunkR, JunkI, ContourFileNumCols
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_OPEN(TRIM(ContourFile1),NF90_NOWRITE,NC_ID1))
                            CALL Check(NF90_INQ_VARID(NC_ID1,'time',NC_Var))
                            CALL Check(NF90_INQUIRE_VARIABLE(NC_ID1,NC_Var,dimids=NC_DimIDs))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_DimIDs(1),len=NumRecsLocal))
                            CALL Check(NF90_INQ_VARID(NC_ID1,'depth',NC_Var))
                            CALL Check(NF90_INQUIRE_VARIABLE(NC_ID1,NC_Var,dimids=NC_DimIDs))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_DimIDs(1),len=NumNodes1))
                            CALL FindMyNetCDFVariable(NC_ID1)
#endif
                        ENDIF
                        IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                            OPEN(UNIT=23,FILE=TRIM(ContourFile2),ACTION="READ")
                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT=*) NumRecsLocal, NumNodes2, JunkR, JunkI, ContourFileNumCols
                        ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_OPEN(TRIM(ContourFile2),NF90_NOWRITE,NC_ID2))
                            CALL Check(NF90_INQ_VARID(NC_ID2,'time',NC_Var))
                            CALL Check(NF90_INQUIRE_VARIABLE(NC_ID2,NC_Var,dimids=NC_DimIDs))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID2,NC_DimIDs(1),len=NumRecsLocal))
                            CALL Check(NF90_INQ_VARID(NC_ID2,'depth',NC_Var))
                            CALL Check(NF90_INQUIRE_VARIABLE(NC_ID2,NC_Var,dimids=NC_DimIDs))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID2,NC_DimIDs(1),len=NumNodes2))
                            CALL FindMyNetCDFVariable(NC_ID2)
#endif
                        ENDIF

                        IF(NumNodes1.NE.NumNodes2.AND..NOT.NeedTranslationTable)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: The contour files should have the same number of nodes"
                            WRITE(UNIT=*,FMT='(A)') "             unless two mesh geometries are specified in the input file."  
#ifdef CMPI
                            CALL MPI_FINALIZE(IERR)
#endif
                            STOP
                        ENDIF

                        NumNodesGlobal = NumNodes1

                        ALLOCATE(U1(1:NumNodesGlobal))
                        ALLOCATE(U2(1:NumNodesMesh2))
                        ALLOCATE(V1(1:NumNodesGlobal))
                        ALLOCATE(V2(1:NumNodesMesh2))
                        ALLOCATE(Vels1(1:NumNodesGlobal))
                        ALLOCATE(Vels2(1:NumNodesMesh2))

                        loopminmax2: DO J=1,NumRecsLocal 

                            IF(J.LT.RecordsList(CounterLocal))THEN

                                IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                    NumNodes1 = NumNodesGlobal
                                    CALL ReadTimeStamp(19,LEN_TRIM(ContourFile1),TRIM(ContourFile1),JunkR,JunkR2,&
                                                       NumNodes1,DefaultValue)
                                    DO I=1,NumNodes1
                                       READ(UNIT=19,FMT=*) JunkI
                                    ENDDO
                                ENDIF
                                IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                                    NumNodes2 = NumNodesMesh2
                                    CALL ReadTimeStamp(23,LEN_TRIM(ContourFile2),TRIM(ContourFile2),JunkR,JunkR2,&
                                                       NumNodes2,DefaultValue)
                                    DO I=1,NumNodes2
                                       READ(UNIT=23,FMT=*) JunkI
                                    ENDDO
                                ENDIF

                            ELSE

                                CounterLocal = CounterLocal + 1

                                IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                    NumNodes1 = NumNodesGlobal
                                    CALL ReadTimeStamp(19,LEN_TRIM(ContourFile1),TRIM(ContourFile1),JunkR,JunkR2,&
                                                       NumNodes1,DefaultValue)
                                ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                    NumNodes1 = NumNodesGlobal
                                    CALL GetNetCDFVarID(NC_ID1,NC_Var,NC_Var2,ContourFileNumCols)
                                    
                                    ierr = NF90_GET_ATT(NC_ID1,NC_Var,'_FillValue',DefaultValue)
                                    IF(ierr.NE.NF90_NOERR)THEN
                                        CALL Check(NF90_GET_ATT(NC_ID1,NF90_GLOBAL,'_FillValue',DefaultValue))
                                    ENDIF    

#endif
                                ENDIF
                                IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                                    NumNodes2 = NumNodesMesh2
                                    CALL ReadTimeStamp(23,LEN_TRIM(ContourFile2),TRIM(ContourFile2),JunkR,JunkR2,&
                                                       NumNodes2,DefaultValue)
                                ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                                    NumNodes2 = NumNodesGlobal
                                    CALL GetNetCDFVarid(NC_ID2,NC_Var,NC_Var2,ContourFileNumCols)
                                    ierr = NF90_GET_ATT(NC_ID2,NC_Var,'_FillValue',DefaultValue)
                                    IF(ierr.NE.NF90_NOERR)THEN
                                        CALL Check(NF90_GET_ATT(NC_ID2,NF90_GLOBAL,'_FillValue',DefaultValue))
                                    ENDIF    

#endif
                                ENDIF

                                IF(INDEX(TRIM(ContourFile1),"64").GT.0)THEN
                                    DefaultValue = -99999.0
                                ENDIF

                                IF(DefaultValue.GT.-99998.0)THEN
                                    DefaultValue = DefaultValue * ContourConversionFactor
                                ENDIF

                                U1(:) = DefaultValue
                                U2(:) = DefaultValue
                                V1(:) = DefaultValue
                                V2(:) = DefaultValue
                                Vels1(:) = DefaultValue
                                Vels2(:) = DefaultValue

                                IF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                    IF(ContourFileNumCOls.EQ.1)THEN
                                        CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,VARID1=NC_Var,&
                                                                  VEC1=U1,Record=J)
                                    ELSE
                                        CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,VARID1=NC_Var,&
                                                                  VEC1=U1,VARID2=NC_VAR2,VEC2=V1,Record=J)
                                    ENDIF    
#endif
                                ENDIF

                                DO I=1,NumNodes1

                                    IF(ContourFileNumCols.EQ.1)THEN

                                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                            READ(UNIT=19,FMT=*) JunkI, U1(JunkI)
                                            IF(U1(JunkI).GT.-99998.0)THEN
                                                U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                            ENDIF
                                            Vels1(JunkI) = U1(JunkI)
                                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                            IF(U1(I).GT.-99998.0)THEN
                                                U1(I) = U1(I) * ContourConversionFactor
                                            ENDIF
                                            Vels1(I) = U1(I)
#endif
                                        ENDIF

                                    ELSEIF(ContourFileNumCols.EQ.2)THEN

                                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                            READ(UNIT=19,FMT=*) JunkI, U1(JunkI), V1(JunkI)
                                            U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                            V1(JunkI) = V1(JunkI) * ContourConversionFactor
                                            Vels1(JunkI) = SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))
#ifdef DRYZEROVEL                                            
                                            IF(Vels1(JunkI).EQ.0D0)Vels1(JunkI)=-99999D0
#endif                                            
                                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                            U1(I) = U1(I) * ContourConversionFactor
                                            V1(I) = V1(I) * ContourConversionFactor
                                            Vels1(I) = SQRT(U1(I)*U1(I)+V1(I)*V1(I))
#ifdef DRYZEROVEL                                            
                                            IF(Vels1(JunkI).EQ.0D0)Vels1(JunkI)=-99999D0
#endif                                            
#endif
                                        ENDIF

                                    ENDIF

                                ENDDO

                                IF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                                    CALL GetNetCDFVARID(NC_ID2,NC_Var,NC_Var2,ContourFileNumCols)
                                    IF(ContourFileNumCOls.EQ.1)THEN
                                        CALL ReadMyNetCDFVariable(NCID=NC_ID2,NUMNODES=NumNodesMesh2,VARID1=NC_Var,&
                                                                  VEC1=U2,RECORD=J)
                                    ELSE
                                        CALL ReadMyNetCDFVariable(NCID=NC_ID2,VARID1=NC_Var,NUMNODES=NumNodesMesh2,&
                                                                  VEC1=U2,VARID2=NC_VAR2,VEC2=V2,Record=J)
                                    ENDIF    
#endif
                                ENDIF

                                DO I=1,NumNodes2

                                    IF(ContourFileNumCols.EQ.1)THEN

                                        IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                                            READ(UNIT=23,FMT=*) JunkI, U2(JunkI)
                                            IF(U2(JunkI).GT.-99998.0)THEN
                                                U2(JunkI) = U2(JunkI) * ContourConversionFactor
                                            ENDIF
                                            Vels2(JunkI) = U2(JunkI)
                                        ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                                            IF(U2(I).GT.-99998.0)THEN
                                                U2(I) = U2(I) * ContourConversionFactor
                                            ENDIF
                                            Vels2(I) = U2(I)
#endif
                                        ENDIF

                                    ELSEIF(ContourFileNumCols.EQ.2)THEN

                                        IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                                            READ(UNIT=23,FMT=*) JunkI, U2(JunkI), V2(JunkI)
                                            U2(JunkI) = U2(JunkI) * ContourConversionFactor
                                            V2(JunkI) = V2(JunkI) * ContourConversionFactor
                                            Vels2(JunkI) = SQRT(U2(JunkI)*U2(JunkI)+V2(JunkI)*V2(JunkI))
#ifdef DRYZEROVEL                                            
                                            IF(Vels2(JunkI).EQ.0D0)Vels2(JunkI)=-99999D0
#endif                                            
                                        ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                                            U2(I) = U2(I) * ContourConversionFactor
                                            V2(I) = V2(I) * ContourConversionFactor
                                            Vels2(I) = SQRT(U2(I)*U2(I)+V2(I)*V2(I))
#ifdef DRYZEROVEL                                            
                                            IF(Vels2(JunkI).EQ.0D0)Vels2(JunkI)=-99999D0
#endif                                            
#endif
                                        ENDIF

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

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            CLOSE(UNIT=19,STATUS="KEEP")
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_CLOSE(NC_ID1))
#endif
                        ENDIF
                        IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                            CLOSE(UNIT=23,STATUS="KEEP")
                        ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_CLOSE(NC_ID2))
#endif
                        ENDIF

                    ELSEIF(TRIM(ContourFileType).EQ."GRID-BATH")THEN

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) JunkI, NumNodes1
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_OPEN(ContourFile1,NF90_NOWRITE,NC_ID1))
                            CALL Check(NF90_INQ_DIMID(NC_ID1,"node",NC_Var))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_Var,len=NumNodes1))
#endif
                        ENDIF
                        IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                            OPEN(UNIT=19,FILE=TRIM(ContourFile2),ACTION="READ")
                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) JunkI, NumNodes2
                        ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_OPEN(ContourFile2,NF90_NOWRITE,NC_ID2))
                            CALL Check(NF90_INQ_DIMID(NC_ID2,"node",NC_Var))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID2,NC_Var,len=NumNodes2))
#endif
                        ENDIF

                        IF(NumNodes1.NE.NumNodes2)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: The grids should have the same number of nodes."
#ifdef CMPI
                            CALL MPI_FINALIZE(IERR)
#endif
                            STOP
                        ENDIF

                        ALLOCATE(Bath1(1:NumNodes1))
                        ALLOCATE(Bath2(1:NumNodes2))

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            DO I=1,NumNodes1
                                READ(UNIT=19,FMT=*) JunkI, JunkR, JunkR, Bath1(I)
                                Bath1(I) = Bath1(I) * ContourConversionFactor
                            ENDDO
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_INQ_VARID(NC_ID1,'depth',JunkI))
                            CALL Check(NF90_GET_VAR(NC_ID1,JunkI,Bath1))
                            DO I=1,NumNodesGlobal
                                Bath1(I) = Bath1(I) * ContourConversionFactor
                            ENDDO
#endif
                        ENDIF
                        IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                            DO I=1,NumNodes2
                                READ(UNIT=23,FMT=*) JunkI, JunkR, JunkR, Bath2(I)
                                Bath2(I) = Bath2(I) * ContourConversionFactor
                            ENDDO
                        ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_INQ_VARID(NC_ID2,'depth',JunkI))
                            CALL Check(NF90_GET_VAR(NC_ID2,JunkI,Bath2))
                            DO I=1,NumNodesGlobal
                                Bath2(I) = Bath2(I) * ContourConversionFactor
                            ENDDO
#endif
                        ENDIF

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

                        IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                            CLOSE(UNIT=19,STATUS="KEEP")
                        ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_CLOSE(NC_ID1))
#endif
                        ENDIF
                        IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                            CLOSE(UNIT=23,STATUS="KEEP")
                        ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_CLOSE(NC_ID2))
#endif
                        ENDIF

                    ENDIF

                    ContourMin = FLOOR(Min)
                    ContourMax = CEILING(Max)

                ENDIF

                IF(Verbose.GE.3)THEN
                    WRITE(*,'(A,I4.4,A,F7.2,A,A,F7.2,A,A)')                              &
                                 "Core ", MyRank, " computed the contour range ..." &
                                 //" Minimum ~ ",ContourMin,TRIM(ContourUnits),          &
                                 ", Maximum ~ ",ContourMax,TRIM(ContourUnits),"."
                ENDIF

        END SUBROUTINE



        SUBROUTINE FindVectorScaleMag

                

#ifdef NETCDF
                USE netcdf
#endif

                IMPLICIT NONE

#ifdef NETCDF
                INTEGER           :: NC_DimIDs(NF90_MAX_VAR_DIMS)
                INTEGER           :: NC_File
                INTEGER           :: NC_ID
                INTEGER           :: NC_Status
                INTEGER           :: NC_Var
                INTEGER           :: NC_Var2
#endif

                INTRINSIC         :: CEILING

                CHARACTER(LEN=1)  :: JunkC

                INTEGER           :: CounterLocal
                INTEGER           :: I
                INTEGER           :: J
                INTEGER           :: JunkI
                INTEGER           :: NumNodes1
                INTEGER           :: NumRecsLocal

                REAL              :: DefaultValue
                REAL(8)           :: JunkR
                REAL(8)           :: JunkR2
                REAL              :: Max
                REAL,ALLOCATABLE  :: U1(:)
                REAL,ALLOCATABLE  :: V1(:)
                REAL,ALLOCATABLE  :: Vels1(:)

                CounterLocal = 1
                Max = -9999.0

                IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                    OPEN(UNIT=20,FILE=TRIM(VectorFile),ACTION="READ")
                    READ(UNIT=20,FMT='(A)') JunkC
                    READ(UNIT=20,FMT=*) NumRecsLocal, NumNodesGlobal, JunkR, JunkI, VectorFileNumCols
                ELSEIF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    CALL Check(NF90_OPEN(TRIM(VectorFile),NF90_NOWRITE,NC_ID))
                    CALL Check(NF90_INQ_DIMID(NC_ID,"time",NC_Var))
                    CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_Var,len=NumRecsLocal))
                    CALL Check(NF90_INQ_DIMID(NC_ID,"node",NC_Var))
                    CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_Var,len=NumNodesGlobal))
                    CALL FindMyNetCDFVariable(NC_ID,Vector=.TRUE.)
#endif
                ENDIF

                ALLOCATE(U1(1:NumNodesGlobal))
                ALLOCATE(V1(1:NumNodesGlobal))
                ALLOCATE(Vels1(1:NumNodesGlobal))

                loopvectorscale: DO J=1,NumRecsLocal 

                    IF(J.LT.RecordsList(CounterLocal))THEN

                        IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                            NumNodes1 = NumNodesGlobal
                            CALL ReadTimeStamp(20,LEN_TRIM(VectorFile),TRIM(VectorFile),JunkR,JunkR2,&
                                               NumNodes1,DefaultValue)
                            DO I=1,NumNodes1
                                READ(UNIT=20,FMT=*) JunkI
                            ENDDO
                        ENDIF

                    ELSE

                        CounterLocal = CounterLocal + 1

                        IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                            NumNodes1 = NumNodesGlobal
                            CALL ReadTimeStamp(20,LEN_TRIM(VectorFile),TRIM(VectorFile),JunkR,JunkR2,NumNodes1,DefaultValue)
                        ELSEIF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                            NumNodes1 = NumNodesGlobal
                            CALL GetNetCDFVarID(NC_ID,NC_Var,NC_Var2,ContourFileNumCols)

                            ierr = NF90_GET_ATT(NC_ID,NC_Var,'_FillValue',DefaultValue)
                            IF(ierr.NE.NF90_NOERR)THEN
                                CALL Check(NF90_GET_ATT(NC_ID,NF90_GLOBAL,'_FillValue',DefaultValue))
                            ENDIF    

#endif
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

                        IF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL ReadMyNetCDFVariable(NCID=NC_ID,NUMNODES=NumNodesGlobal,VARID1=NC_Var,&
                                                      VARID2=NC_Var2,VEC1=U1,VEC2=V1,RECORD=J)
#endif
                        ENDIF

                        DO I=1,NumNodes1

                            IF(VectorFileNumCols.EQ.2)THEN

                                IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                                    READ(UNIT=20,FMT=*) JunkI, U1(JunkI), V1(JunkI)
                                    U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                    V1(JunkI) = V1(JunkI) * ContourConversionFactor
                                    Vels1(JunkI) = SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))
                                    IF(Vels1(JunkI).GT.Max)THEN
                                        Max = Vels1(JunkI)
                                    ENDIF
                                ELSEIF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                                    U1(I) = U1(I) * ContourConversionFactor
                                    V1(I) = V1(I) * ContourConversionFactor
                                    Vels1(I) = SQRT(U1(I)*U1(I)+V1(I)*V1(I))
                                    IF(Vels1(I).GT.Max)THEN
                                        Max = Vels1(I)
                                    ENDIF
#endif
                                ENDIF

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

                IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                    CLOSE(UNIT=20,STATUS="KEEP")
                ELSEIF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    CALL Check(NF90_CLOSE(NC_ID))
#endif
                ENDIF

                VectorScaleMag = CEILING(Max)

                IF(Verbose.GE.3)THEN
                    WRITE(*,'(A,I4.4,A,F7.2,A,A)')                                       &
                                 "Core ", MyRank, " computed the vector scale ..."  &
                                 //" Maximum ~ ",VectorScaleMag,TRIM(VectorLabel),"."
                ENDIF

        END SUBROUTINE



        SUBROUTINE GoogleKML(WorkingRecord,IL1,IL2,IL3)

              

              IMPLICIT NONE

              INTRINSIC         :: LEN_TRIM
              INTRINSIC         :: REAL 
              INTRINSIC         :: TRIM

              CHARACTER(LEN=90) :: GoogleLabel
              CHARACTER(LEN=90) :: TempLabel
              CHARACTER(LEN=1)  :: Transparency
              CHARACTER(LEN=15) :: XMax
              CHARACTER(LEN=15) :: XMin
              CHARACTER(LEN=15) :: YMax
              CHARACTER(LEN=15) :: YMin

              INTEGER           :: IL1
              INTEGER           :: IL2
              INTEGER           :: IL3
              INTEGER           :: ILL2
              INTEGER           :: ILL3
              INTEGER           :: IR
              INTEGER           :: SecondsElapsed
              INTEGER           :: SnapStep
              INTEGER           :: TimeDay
              INTEGER           :: TimeDayInc
              INTEGER           :: TimeHour
              INTEGER           :: TimeHourInc
              INTEGER           :: TimeMin
              INTEGER           :: TimeMinInc
              INTEGER           :: TimeMonth
              INTEGER           :: TimeSec
              INTEGER           :: TimeYear
              INTEGER           :: WorkingRecord

              REAL              :: LatNLocal
              REAL              :: LatNLocal2
              REAL              :: LatSLocal
              REAL              :: LatSLocal2
              REAL              :: LongELocal
              REAL              :: LongELocal2
              REAL              :: LongWLocal
              REAL              :: LongWLocal2

1236          FORMAT(F15.8)

              findir: DO IR=1,NumRecords
                 IF(WorkingRecord.EQ.RecordsList(IR))THEN
                    EXIT findir
                 ENDIF
              ENDDO findir

              WRITE(UNIT=GoogleLabel,FMT='(A,I4.4,A,I2.2,A,I2.2,A,I2.2)') TRIM(AlphaLabel),WorkingRecord,"-",IL1,"-",IL2,"-",IL3

        !... Correct for the case when the plotted region does not extend
        !... to the edges of the map.

              LongWLocal = LongW + REAL(IL2-1)/REAL(2**(IL1-1))*(LongE-LongW)
              LongELocal = LongW + REAL(IL2  )/REAL(2**(IL1-1))*(LongE-LongW)
              LatSLocal  = LatS  + REAL(IL3-1)/REAL(2**(IL1-1))*(LatN -LatS )
              LatNLocal  = LatS  + REAL(IL3  )/REAL(2**(IL1-1))*(LatN -LatS )

              WRITE(UNIT=XMax,FMT=1236) LongELocal
              WRITE(UNIT=XMin,FMT=1236) LongWLocal
              WRITE(UNIT=YMax,FMT=1236) LatNLocal
              WRITE(UNIT=YMin,FMT=1236) LatSLocal

              OPEN(UNIT=35,FILE=TRIM(GoogleLabel)//".kml",ACTION="WRITE")

              WRITE(UNIT=35,FMT='(A)') "<?xml version="//ACHAR(34)//"1.0"//ACHAR(34)//" encoding="//ACHAR(34)//&
                                            "UTF-8"//ACHAR(34)//"?>"
              WRITE(UNIT=35,FMT='(A)') "<kml xmlns="//ACHAR(34)//"http://earth.google.com/kml/2.1"//ACHAR(34)//">"
              WRITE(UNIT=35,FMT='(A)') "   <Document>"
              WRITE(UNIT=35,FMT='(A)') "      <Name>"//TRIM(GoogleLabel)//"</Name>"

              IF(NumRecords.NE.1)THEN

                 WRITE(UNIT=35,FMT='(A)') "      <TimeSpan>"
                 IF(IR.EQ.1)THEN
                    SnapStep = 0
                 ELSE
                    SnapStep = 0.5 * ( (RecordsList(IR)-1)*TimeStep - (RecordsList(IR-1)-1)*TimeStep )
                 ENDIF
                 SecondsElapsed = (RecordsList(IR)-1)*TimeStep - SnapStep
                 TimeMinInc = 0
                 TimeSec = GoogleSec + SecondsElapsed
                 IF(TimeSec.GE.60)THEN
                    TimeMinInc = TimeSec/60
                    TimeSec = MOD(TimeSec,60)
                 ENDIF
                 TimeHourInc = 0
                 TimeMin = GoogleMin + TimeMinInc
                 IF(TimeMin.GE.60)THEN
                    TimeHourInc = TimeMin/60
                    TimeMin = MOD(TimeMin,60)
                 ENDIF
                 TimeDayInc = 0
                 TimeHour = GoogleHour + TimeHourInc
                 IF(TimeHour.GE.24)THEN
                    TimeDayInc = TimeHour/24
                    TimeHour = MOD(TimeHour,24)
                 ENDIF
                 TimeDay   = GoogleDay + TimeDayInc
                 TimeMonth = GoogleMonth
                 TimeYear  = GoogleYear
                 IF((TimeMonth.EQ.1).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 2
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.2).AND.(TimeDay.GT.28))THEN
                    TimeMonth = 3
                    TimeDay   = MOD(TimeDay,28)
                 ELSEIF((TimeMonth.EQ.3).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 4
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.4).AND.(TimeDay.GT.30))THEN
                    TimeMonth = 5
                    TimeDay   = MOD(TimeDay,30)
                 ELSEIF((TimeMonth.EQ.5).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 6
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.6).AND.(TimeDay.GT.30))THEN
                    TimeMonth = 7
                    TimeDay   = MOD(TimeDay,30)
                 ELSEIF((TimeMonth.EQ.7).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 8
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.8).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 9
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.9).AND.(TimeDay.GT.30))THEN
                    TimeMonth = 10
                    TimeDay   = MOD(TimeDay,30)
                 ELSEIF((TimeMonth.EQ.10).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 11
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.11).AND.(TimeDay.GT.30))THEN
                    TimeMonth = 12
                    TimeDay   = MOD(TimeDay,30)
                 ELSEIF((TimeMonth.EQ.12).AND.(TimeDay.GT.31))THEN
                    TimeYear  = TimeYear + 1
                    TimeMonth = 1
                    TimeDay   = MOD(TimeDay,31)
                 ENDIF
                 WRITE(UNIT=35,FMT='(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A)') &
                       "         <begin>",TimeYear,"-",TimeMonth,"-",TimeDay,"T",TimeHour,":",TimeMin,":",TimeSec,"</begin>"
                 IF(IR.EQ.NumRecords)THEN
                    SnapStep = 0
                 ELSE
                    SnapStep = 0.5 * ( (RecordsList(IR+1)-1)*TimeStep - (RecordsList(IR)-1)*TimeStep )
                 ENDIF
                 SecondsElapsed = (RecordsList(IR)-1)*TimeStep + SnapStep
                 TimeMinInc = 0
                 TimeSec = GoogleSec + SecondsElapsed
                 IF(TimeSec.GE.60)THEN
                    TimeMinInc = TimeSec/60
                    TimeSec = MOD(TimeSec,60)
                 ENDIF
                 TimeHourInc = 0
                 TimeMin = GoogleMin + TimeMinInc
                 IF(TimeMin.GE.60)THEN
                    TimeHourInc = TimeMin/60
                    TimeMin = MOD(TimeMin,60)
                 ENDIF
                 TimeDayInc = 0
                 TimeHour = GoogleHour + TimeHourInc
                 IF(TimeHour.GE.24)THEN
                    TimeDayInc = TimeHour/24
                    TimeHour = MOD(TimeHour,24)
                 ENDIF
                 TimeDay   = GoogleDay + TimeDayInc
                 TimeMonth = GoogleMonth
                 TimeYear  = GoogleYear
                 IF((TimeMonth.EQ.1).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 2
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.2).AND.(TimeDay.GT.28))THEN
                    TimeMonth = 3
                    TimeDay   = MOD(TimeDay,28)
                 ELSEIF((TimeMonth.EQ.3).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 4
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.4).AND.(TimeDay.GT.30))THEN
                    TimeMonth = 5
                    TimeDay   = MOD(TimeDay,30)
                 ELSEIF((TimeMonth.EQ.5).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 6
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.6).AND.(TimeDay.GT.30))THEN
                    TimeMonth = 7
                    TimeDay   = MOD(TimeDay,30)
                 ELSEIF((TimeMonth.EQ.7).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 8
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.8).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 9
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.9).AND.(TimeDay.GT.30))THEN
                    TimeMonth = 10
                    TimeDay   = MOD(TimeDay,30)
                 ELSEIF((TimeMonth.EQ.10).AND.(TimeDay.GT.31))THEN
                    TimeMonth = 11
                    TimeDay   = MOD(TimeDay,31)
                 ELSEIF((TimeMonth.EQ.11).AND.(TimeDay.GT.30))THEN
                    TimeMonth = 12
                    TimeDay   = MOD(TimeDay,30)
                 ELSEIF((TimeMonth.EQ.12).AND.(TimeDay.GT.31))THEN
                    TimeYear  = TimeYear + 1
                    TimeMonth = 1
                    TimeDay   = MOD(TimeDay,31)
                 ENDIF
                 WRITE(UNIT=35,FMT='(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A)') &
                       "         <end>",TimeYear,"-",TimeMonth,"-",TimeDay,"T",TimeHour,":",TimeMin,":",TimeSec,"</end>"
                 WRITE(UNIT=35,FMT='(A)')               "      </TimeSpan>"

              ENDIF

              IF(INDEX(ContourFileType,"GRID-DECOMP").LE.0)THEN
                 WRITE(UNIT=35,FMT='(A)') "      <ScreenOverlay>"
                 WRITE(UNIT=35,FMT='(A)') "         <name>Scale</name>"
                 WRITE(UNIT=35,FMT='(A)') "         <overlayXY x="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                             " y="//ACHAR(34)//"0.50"//ACHAR(34)// &
                                                             " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                             " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                 WRITE(UNIT=35,FMT='(A)') "         <screenXY  x="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                             " y="//ACHAR(34)//"0.50"//ACHAR(34)// &
                                                             " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                             " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                 WRITE(UNIT=35,FMT='(A)') "         <size      x="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                             " y="//ACHAR(34)//"0.75"//ACHAR(34)// &
                                                             " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                             " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                 WRITE(UNIT=35,FMT='(A)') "         <Icon>"
                 WRITE(UNIT=35,FMT='(A)') "            <href>Scale.png</href>"
                 WRITE(UNIT=35,FMT='(A)') "         </Icon>"
                 WRITE(UNIT=35,FMT='(A)') "      </ScreenOverlay>"
              ENDIF

              IF(IfPlotLogo.EQ.1)THEN
                 WRITE(UNIT=35,FMT='(A)')     "      <ScreenOverlay>"
                 WRITE(UNIT=35,FMT='(A)')     "         <name>Logo</name>"
                 IF((TRIM(LogoLocation).EQ."TL").OR.(TRIM(LogoLocation).EQ."LT"))THEN
                    WRITE(UNIT=35,FMT='(A)')     "         <overlayXY x="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                                    " y="//ACHAR(34)//"1.00"//ACHAR(34)// &
                                                                    " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                                    " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                    WRITE(UNIT=35,FMT='(A)')     "         <screenXY  x="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                                    " y="//ACHAR(34)//"1.00"//ACHAR(34)// &
                                                                    " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                                    " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                 ELSEIF((TRIM(LogoLocation).EQ."TR").OR.(TRIM(LogoLocation).EQ."RT"))THEN
                    WRITE(UNIT=35,FMT='(A)')     "         <overlayXY x="//ACHAR(34)//"1.00"//ACHAR(34)// &
                                                                    " y="//ACHAR(34)//"1.00"//ACHAR(34)// &
                                                                    " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                                    " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                    WRITE(UNIT=35,FMT='(A)')     "         <screenXY  x="//ACHAR(34)//"1.00"//ACHAR(34)// &
                                                                    " y="//ACHAR(34)//"1.00"//ACHAR(34)// &
                                                                    " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                                    " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                 ELSEIF((TRIM(LogoLocation).EQ."BR").OR.(TRIM(LogoLocation).EQ."RB"))THEN
                    WRITE(UNIT=35,FMT='(A)')     "         <overlayXY x="//ACHAR(34)//"1.00"//ACHAR(34)// &
                                                                    " y="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                                    " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                                    " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                    WRITE(UNIT=35,FMT='(A)')     "         <screenXY  x="//ACHAR(34)//"1.00"//ACHAR(34)// &
                                                                    " y="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                                    " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                                    " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                 ELSE
                    WRITE(UNIT=35,FMT='(A)')     "         <overlayXY x="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                                    " y="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                                    " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                                    " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                    WRITE(UNIT=35,FMT='(A)')     "         <screenXY  x="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                                    " y="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                                    " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                                    " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                 ENDIF
                 WRITE(UNIT=35,FMT='(A)')     "         <size      x="//ACHAR(34)//"0.00"//ACHAR(34)// &
                                                                 " y="//ACHAR(34)//"0.10"//ACHAR(34)// &
                                                                 " xunits="//ACHAR(34)//"fraction"//ACHAR(34)// &
                                                                 " yunits="//ACHAR(34)//"fraction"//ACHAR(34)//"/>"
                 WRITE(UNIT=35,FMT='(A)')     "         <Icon>"
                 WRITE(UNIT=35,FMT='(A,A,A)') "            <href>",TRIM(LogoFile),"</href>"
                 WRITE(UNIT=35,FMT='(A)')     "         </Icon>"
                 WRITE(UNIT=35,FMT='(A)')     "      </ScreenOverlay>"
              ENDIF

              WRITE(UNIT=35,FMT='(A)') "      <Region>"
              WRITE(UNIT=35,FMT='(A)') "         <Lod>"
              WRITE(UNIT=35,FMT='(A)') "            <minLodPixels>256</minLodPixels>"
              IF(IL1.NE.NumLayers)THEN
                 WRITE(UNIT=35,FMT='(A)') "            <maxLodPixels>512</maxLodPixels>"
              ELSE
                 WRITE(UNIT=35,FMT='(A)') "            <maxLodPixels>-1</maxLodPixels>"
              ENDIF
              WRITE(UNIT=35,FMT='(A)') "         </Lod>"
              WRITE(UNIT=35,FMT='(A)') "         <LatLonAltBox>"
              WRITE(UNIT=35,FMT='(A)') "            <north>"//TRIM(ADJUSTL(YMax))//"</north>"
              WRITE(UNIT=35,FMT='(A)') "            <south>"//TRIM(ADJUSTL(YMin))//"</south>"
              WRITE(UNIT=35,FMT='(A)') "            <east>"//TRIM(ADJUSTL(XMax))//"</east>"
              WRITE(UNIT=35,FMT='(A)') "            <west>"//TRIM(ADJUSTL(XMin))//"</west>"
              WRITE(UNIT=35,FMT='(A)') "         </LatLonAltBox>"
              WRITE(UNIT=35,FMT='(A)') "      </Region>"

              WRITE(UNIT=35,FMT='(A)')        "      <GroundOverlay>"
              WRITE(UNIT=35,FMT='(A)')        "         <name>"//TRIM(GoogleLabel)//"</name>"
              WRITE(UNIT=35,FMT='(A,I2.2,A)') "         <drawOrder>",NumLayers+1-IL1,"</drawOrder>"
              IF((  0.LE.GoogleTransparency).AND.(GoogleTransparency.LE.  3)) Transparency = "0"
              IF((  3.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 10)) Transparency = "1"
              IF(( 10.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 17)) Transparency = "2"
              IF(( 17.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 23)) Transparency = "3"
              IF(( 23.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 30)) Transparency = "4"
              IF(( 30.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 37)) Transparency = "5"
              IF(( 37.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 43)) Transparency = "6"
              IF(( 43.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 50)) Transparency = "7"
              IF(( 50.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 57)) Transparency = "8"
              IF(( 57.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 63)) Transparency = "9"
              IF(( 63.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 70)) Transparency = "a"
              IF(( 70.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 77)) Transparency = "b"
              IF(( 77.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 83)) Transparency = "c"
              IF(( 83.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 90)) Transparency = "d"
              IF(( 90.LT.GoogleTransparency).AND.(GoogleTransparency.LE. 97)) Transparency = "e"
              IF(( 97.LT.GoogleTransparency).AND.(GoogleTransparency.LE.100)) Transparency = "f"
              WRITE(UNIT=35,FMT='(A)')        "         <color>"//TRIM(Transparency)//"fffffff</color>"
              WRITE(UNIT=35,FMT='(A)')        "         <Icon>"
              WRITE(UNIT=35,FMT='(A,I4.4,A)') "            <href>"//TRIM(GoogleLabel)//".png</href>"
              WRITE(UNIT=35,FMT='(A)')        "         </Icon>"
              WRITE(UNIT=35,FMT='(A)')        "         <LatLonBox>"
              WRITE(UNIT=35,FMT='(A)')        "            <north>"//TRIM(ADJUSTL(YMax))//"</north>"
              WRITE(UNIT=35,FMT='(A)')        "            <south>"//TRIM(ADJUSTL(YMin))//"</south>"
              WRITE(UNIT=35,FMT='(A)')        "            <east>"//TRIM(ADJUSTL(XMax))//"</east>"
              WRITE(UNIT=35,FMT='(A)')        "            <west>"//TRIM(ADJUSTL(XMin))//"</west>"
              WRITE(UNIT=35,FMT='(A)')        "         </LatLonBox>"
              WRITE(UNIT=35,FMT='(A)')        "      </GroundOverlay>"

              IF(IL1.NE.NumLayers)THEN

                 LongWLocal2 = LongW + REAL(IL2-1)/REAL(2**(IL1-1))*(LongE-LongW)
                 LongELocal2 = LongW + REAL(IL2  )/REAL(2**(IL1-1))*(LongE-LongW)
                 LatSLocal2  = LatS  + REAL(IL3-1)/REAL(2**(IL1-1))*(LatN -LatS )
                 LatNLocal2  = LatS  + REAL(IL3  )/REAL(2**(IL1-1))*(LatN -LatS )

                 DO ILL2=1,2
                    DO ILL3=1,2

                       WRITE(UNIT=TempLabel,FMT=9722) TRIM(AlphaLabel),WorkingRecord,"-",IL1+1,"-",&
                                                      (IL2-1)*2+ILL2,"-",(IL3-1)*2+ILL3
 9722                  FORMAT(A,I4.4,A,I2.2,A,I2.2,A,I2.2)

                       LongWLocal = LongWLocal2 + REAL(ILL2-1)/REAL(2)*(LongELocal2-LongWLocal2)
                       LongELocal = LongWLocal2 + REAL(ILL2  )/REAL(2)*(LongELocal2-LongWLocal2)
                       LatSLocal  = LatSLocal2  + REAL(ILL3-1)/REAL(2)*(LatNLocal2 -LatSLocal2 )
                       LatNLocal  = LatSLocal2  + REAL(ILL3  )/REAL(2)*(LatNLocal2 -LatSLocal2 )

                       WRITE(UNIT=XMax,FMT=1236) LongELocal
                       WRITE(UNIT=XMin,FMT=1236) LongWLocal
                       WRITE(UNIT=YMax,FMT=1236) LatNLocal
                       WRITE(UNIT=YMin,FMT=1236) LatSLocal

                       WRITE(UNIT=35,FMT='(A)') "      <NetworkLink>"
                       WRITE(UNIT=35,FMT='(A)') "         <name>"//TRIM(TempLabel)//"</name>"
                       WRITE(UNIT=35,FMT='(A)') "         <Region>"
                       WRITE(UNIT=35,FMT='(A)') "            <Lod>"
                       WRITE(UNIT=35,FMT='(A)') "               <minLodPixels>256</minLodPixels>"
                       WRITE(UNIT=35,FMT='(A)') "               <maxLodPixels>512</maxLodPixels>"
                       WRITE(UNIT=35,FMT='(A)') "            </Lod>"
                       WRITE(UNIT=35,FMT='(A)') "            <LatLonAltBox>"
                       WRITE(UNIT=35,FMT='(A)') "               <north>"//TRIM(ADJUSTL(YMax))//"</north>"
                       WRITE(UNIT=35,FMT='(A)') "               <south>"//TRIM(ADJUSTL(YMin))//"</south>"
                       WRITE(UNIT=35,FMT='(A)') "               <east>"//TRIM(ADJUSTL(XMax))//"</east>"
                       WRITE(UNIT=35,FMT='(A)') "               <west>"//TRIM(ADJUSTL(XMin))//"</west>"
                       WRITE(UNIT=35,FMT='(A)') "               </LatLonAltBox>"
                       WRITE(UNIT=35,FMT='(A)') "         </Region>"
                       WRITE(UNIT=35,FMT='(A)') "         <Link>"
                       WRITE(UNIT=35,FMT='(A)') "            <href>"//TRIM(TempLabel)//".kml</href>"
                       WRITE(UNIT=35,FMT='(A)') "            <viewRefreshMode>onregion</viewRefreshMode>"
                       WRITE(UNIT=35,FMT='(A)') "         </Link>"
                       WRITE(UNIT=35,FMT='(A)') "      </NetworkLink>"

                    ENDDO
                 ENDDO

              ENDIF

              WRITE(UNIT=35,FMT='(A)') "   </Document>"
              WRITE(UNIT=35,FMT='(A)') "</kml>"

              CLOSE(UNIT=35,STATUS="KEEP")

              IF(Verbose.GE.3)THEN
                 WRITE(*,9723) "Core ",MyRank," wrote the local Google KML file for record ",WorkingRecord, &
                               ", layer ",IL1,", cell ",IL2,"/",IL3,"."
              ENDIF

9723          FORMAT(A,I4.4,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)

        END SUBROUTINE



        SUBROUTINE GoogleKMZ

              

              IMPLICIT NONE

              INTRINSIC          :: LEN_TRIM
              INTRINSIC          :: TRIM

              CHARACTER(LEN=100) :: GoogleLabel
              CHARACTER(LEN=240) :: Line
              CHARACTER(LEN=2)   :: TempC

              INTEGER            :: IL
              INTEGER            :: IR

              GoogleLabel = ""
              GoogleLabel = TRIM(AlphaLabel)
              IF(GoogleLabel(LEN_TRIM(GoogleLabel):LEN_TRIM(GoogleLabel)).EQ."_")THEN
                  GoogleLabel(LEN_TRIM(GoogleLabel):LEN_TRIM(GoogleLabel)) = " "
              ELSEIF(GoogleLabel(LEN_TRIM(GoogleLabel):LEN_TRIM(GoogleLabel)).EQ."-")THEN
                  GoogleLabel(LEN_TRIM(GoogleLabel):LEN_TRIM(GoogleLabel)) = " "
              ENDIF

              IF(NumRecords.NE.0)THEN

                 OPEN(UNIT=36,FILE=TRIM(GoogleLabel)//".kml",ACTION="WRITE")

                 WRITE(UNIT=36,FMT='(A)') "<?xml version="//ACHAR(34)//"1.0"//ACHAR(34)//" encoding="//&
                                                ACHAR(34)//"UTF-8"//ACHAR(34)//"?>"
                 WRITE(UNIT=36,FMT='(A)') "<kml xmlns="//ACHAR(34)//"http://earth.google.com/kml/2.1"//ACHAR(34)//">"
                 WRITE(UNIT=36,FMT='(A)') "   <Document>"
                 WRITE(UNIT=36,FMT='(A)') "      <Name>"//TRIM(GoogleLabel)//"</Name>"

                 DO IR=1,NumRecords

                    WRITE(UNIT=36,FMT='(A)')        "      <NetworkLink>"
                    WRITE(UNIT=36,FMT='(A,I4.4,A)') "         <Name>"//TRIM(GoogleLabel)//"_",RecordsList(IR),"</Name>"
                    WRITE(UNIT=36,FMT='(A)')        "         <Link>"
                    WRITE(UNIT=36,FMT='(A,I4.4,A)') "            <href>"//TRIM(GoogleLabel)//"_",RecordsList(IR),&
                                                                    "-01-01-01.kml"//"</href>"
                    WRITE(UNIT=36,FMT='(A)')        "         </Link>"
                    WRITE(UNIT=36,FMT='(A)')        "      </NetworkLink>"

                 ENDDO

                 WRITE(UNIT=36,FMT='(A)') "   </Document>"
                 WRITE(UNIT=36,FMT='(A)') "</kml>"

                 CLOSE(UNIT=36,STATUS="KEEP")

              ENDIF

        !Casey 120508: Cannot remember why we added this IF statement.
        !     IF(NumRecords.NE.1)THEN
                 Line = "zip -q "//TRIM(GoogleLabel)//".kmz "//TRIM(GoogleLabel)//".kml"
                 CALL SYSTEM(TRIM(Line))
                 CALL SYSTEM("rm "//TRIM(GoogleLabel)//".kml")
        !     ENDIF
              DO IL=1,NumLayers
                 WRITE(TempC,'(I2.2)') 2**(IL-1)
                 Line = "zip -q "//TRIM(GoogleLabel)//".kmz "//TRIM(GoogleLabel)//"*"//TRIM(TempC)//".kml"
                 CALL SYSTEM(TRIM(Line))
                 Line = "zip -q "//TRIM(GoogleLabel)//".kmz "//TRIM(GoogleLabel)//"*"//TRIM(TempC)//".png"
                 CALL SYSTEM(TRIM(Line))
                 CALL SYSTEM("rm -r "//TRIM(GoogleLabel)//"*"//TRIM(TempC)//".kml")
                 CALL SYSTEM("rm -r "//TRIM(GoogleLabel)//"*"//TRIM(TempC)//".png")
                 CALL SYSTEM("rm -r "//TRIM(GoogleLabel)//"*"//TRIM(TempC)//".ps")
              ENDDO
              IF(((IfPlotFilledContours.GE.1).OR.(TRIM(ColorLines).NE."DEFAULT")).AND. &
                 (INDEX(ContourFileType,"GRID-DECOMP").LE.0))THEN
                 CALL SYSTEM(TRIM(Path)//"ps2raster Scale.ps -A -E600 -FScale.jpg"// &
                             " -G"//TRIM(GSPath)//"gs -P -Tg")
                 Line = "zip -q "//TRIM(GoogleLabel)//".kmz "//"Scale.png"
                 CALL SYSTEM(TRIM(Line))
                 CALL SYSTEM("rm Scale.ps")
                 CALL SYSTEM("rm Scale.png")
              ENDIF
              IF(IfPlotLogo.EQ.1)THEN
                 Line = "zip -q "//TRIM(GoogleLabel)//".kmz "//TRIM(LogoFile)
                 CALL SYSTEM(TRIM(Line))
              ENDIF

              IF(Verbose.GE.3)THEN
                 WRITE(*,9724) "Core ",MyRank," wrote the Google KMZ file."
              ENDIF

9724          FORMAT(A,I4.4,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)

        END SUBROUTINE



        SUBROUTINE MapColor

                

#ifdef NETCDF
                USE netcdf
#endif

                IMPLICIT NONE

#ifdef NETCDF
                INTEGER             :: NC_ID
                INTEGER             :: NC_Var
#endif

                INTRINSIC           :: NINT
                INTRINSIC           :: TRIM

                CHARACTER(1)        :: JunkC

                INTEGER             :: Color
                INTEGER             :: I
                INTEGER             :: IE
                INTEGER             :: IN
                INTEGER             :: IS
                INTEGER             :: J
                INTEGER             :: JunkI
                INTEGER             :: MaxColor
                INTEGER             :: MaxNumNeighbors
                INTEGER,ALLOCATABLE :: NC(:,:)
                INTEGER             :: NodeNeighbor
                INTEGER             :: NodeOfInterest
                INTEGER,ALLOCATABLE :: NodeOnSubDomain(:)
                INTEGER             :: NumElems
                INTEGER             :: NumNodes

                LOGICAL             :: AlreadyFound
                LOGICAL             :: ColorUsed

                REAL,ALLOCATABLE    :: DomainColors(:)

                TYPE SubDomainConnectivity
                    INTEGER :: NumNeighbors
                    INTEGER :: Neighbors(12)
                END TYPE
                TYPE(SubDomainConnectivity),ALLOCATABLE :: SubConn(:)

                IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                    OPEN(UNIT=24,FILE=TRIM(Fort14File),ACTION="READ")
                    READ(24,'(A)') JunkC
                    READ(24,*) NumElems, NumNodes
                    DO IN=1,NumNodes
                        READ(24,*) JunkI
                    ENDDO
                    ALLOCATE(NC(1:3,1:NumElems))
                    DO IE=1,NumElems
                        READ(24,*) JunkI, JunkI, NC(1,IE), NC(2,IE), NC(3,IE)
                    ENDDO
                    CLOSE(UNIT=24,STATUS='KEEP')
                ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    CALL Check(NF90_OPEN(Fort14File,NF90_NOWRITE,NC_ID))
                    CALL Check(NF90_INQ_DIMID(NC_ID,"nele",NC_Var))
                    CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_Var,len=NumElems))
                    CALL Check(NF90_INQ_DIMID(NC_ID,"node",NC_Var))
                    CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_Var,len=NumNodes))
                    CALL Check(NF90_INQ_VARID(NC_ID,'element',NC_Var))
                    CALL Check(NF90_GET_VAR(NC_ID,NC_Var,NC))
                    CALL Check(NF90_CLOSE(NC_ID))
#endif
                ENDIF

                OPEN(UNIT=34,FILE=TRIM(ContourFile1),ACTION='READ')
                ALLOCATE(NodeOnSubDomain(1:NumNodes))
                DO IN=1,NumNodes
                   READ(34,*) JunkI
                   NodeOnSubDomain(IN) = JunkI
                ENDDO
                CLOSE(UNIT=34,STATUS='KEEP')

                ALLOCATE(SubConn(1:NumSubDomains))
                DO IS=1,NumSubDomains
                   SubConn(IS)%NumNeighbors = 0
                   DO IE=1,NumElems
                      DO I=1,3
                         NodeOfInterest = NC(1,IE)
                         IF(NodeOnSubDomain(NodeOfInterest).EQ.IS)THEN
                            DO J=1,2
                               IF((I.EQ.1).AND.(J.EQ.1)) NodeNeighbor = NC(2,IE)
                               IF((I.EQ.1).AND.(J.EQ.2)) NodeNeighbor = NC(3,IE)
                               IF((I.EQ.2).AND.(J.EQ.1)) NodeNeighbor = NC(1,IE)
                               IF((I.EQ.2).AND.(J.EQ.2)) NodeNeighbor = NC(3,IE)
                               IF((I.EQ.3).AND.(J.EQ.1)) NodeNeighbor = NC(1,IE)
                               IF((I.EQ.3).AND.(J.EQ.2)) NodeNeighbor = NC(2,IE)
                               IF(NodeOnSubDomain(NodeNeighbor).NE.IS)THEN
                                  AlreadyFound = .FALSE.
                                  DO IN=1,SubConn(IS)%NumNeighbors
                                     IF(SubConn(IS)%Neighbors(IN).EQ.NodeOnSubDomain(NodeNeighbor))THEN
                                        AlreadyFound = .TRUE.
                                     ENDIF
                                  ENDDO
                                  IF(.NOT.AlreadyFound)THEN
                                     SubConn(IS)%NumNeighbors = SubConn(IS)%NumNeighbors + 1
                                     SubConn(IS)%Neighbors(SubConn(IS)%NumNeighbors) = NodeOnSubDomain(NodeNeighbor)
                                  ENDIF
                               ENDIF
                            ENDDO
                         ENDIF
                      ENDDO
                   ENDDO
                ENDDO

                MaxNumNeighbors = 0
                DO IS=1,NumSubDomains
                   IF(SubConn(IS)%NumNeighbors.GT.MaxNumNeighbors)THEN
                      MaxNumNeighbors = SubConn(IS)%NumNeighbors
                   ENDIF
                ENDDO

                ALLOCATE(DomainColors(1:NumSubDomains))
                DO IS=1,NumSubDomains
                   DomainColors(IS) = 0.0
                   IF(SubConn(IS)%NumNeighbors.EQ.MaxNumNeighbors)THEN
                      Color = 1
                      loop1: DO
                         ColorUsed = .FALSE.
                         DO IN=1,SubConn(IS)%NumNeighbors
                            IF(NINT(DomainColors(SubConn(IS)%Neighbors(IN))).EQ.Color)THEN
                               ColorUsed = .TRUE.
                            ENDIF
                         ENDDO
                         IF(ColorUsed)THEN
                            Color = Color + 1
                         ELSE
                            DomainColors(IS) = Color
                            EXIT loop1
                         ENDIF
                      ENDDO loop1
                   ENDIF
                ENDDO

                DO IS=1,NumSubDomains
                   IF(NINT(DomainColors(IS)).EQ.0)THEN
                      Color = 1
                      loop2: DO
                         ColorUsed = .FALSE.
                         DO IN=1,SubConn(IS)%NumNeighbors
                            IF(NINT(DomainColors(SubConn(IS)%Neighbors(IN))).EQ.Color)THEN
                               ColorUsed = .TRUE.
                            ENDIF
                         ENDDO
                         IF(ColorUsed)THEN
                            Color = Color + 1
                         ELSE
                            DomainColors(IS) = Color
                            EXIT loop2
                         ENDIF
                      ENDDO loop2
                   ENDIF
                ENDDO

                MaxColor = 0
                DO IS=1,NumSubDomains
                   IF(NINT(DomainColors(IS)).GT.MaxColor)THEN
                      MaxColor = NINT(DomainColors(IS))
                   ENDIF
                ENDDO

                ALLOCATE(NodeColors(1:NumNodes))
                DO IN=1,NumNodes
                   NodeColors(IN) = (DomainColors(NodeOnSubDomain(IN))-1.0)/(MaxColor-1.0)
                   NodeColors(IN) = ContourMin + NodeColors(IN) * (ContourMax - ContourMin)
                ENDDO

                IF(Verbose.GE.3)THEN
                    WRITE(*,'(A,I4.4,A)') "Core ", MyRank, " applied an optimal color connectivity table."
                ENDIF

                DEALLOCATE(DomainColors)
                DEALLOCATE(NC)
                DEALLOCATE(NodeOnSubDomain)
                DEALLOCATE(SubConn)

        END SUBROUTINE


        SUBROUTINE MakeTranslationTable
#ifdef NETCDF
            USE netcdf
#endif            
            USE KDTREE2_MODULE
            IMPLICIT NONE
            
            CHARACTER(40)         :: JunkC
            INTEGER               :: I
            INTEGER               :: JunkI
            INTEGER               :: NE
            INTEGER               :: NN
            INTEGER               :: NCID
            INTEGER               :: NODEDIM
            INTEGER               :: XVAR
            INTEGER               :: YVAR
            REAL(8)               :: X
            REAL(8)               :: Y
            TYPE(KDTREE2),POINTER :: SearchTree
            TYPE(KDTREE2_RESULT)  :: SearchResult(1)


            !...Start reading the second mesh
            IF(INDEX(Fort14File2,".nc").GT.0)THEN
#ifndef NETCDF
                WRITE(*,'(A)') "ERROR: Not compiled for NetCDF"
                STOP
#else            
                CALL CHECK(NF90_OPEN(TRIM(Fort14File2),NF90_NOWRITE,NCID))
                CALL CHECK(NF90_INQ_DIMID(NCID,"node",NODEDIM))
                CALL CHECK(NF90_INQUIRE_DIMENSION(NCID,NODEDIM,LEN=NN))
                ALLOCATE(G2XY(1:2,1:NN))
                CALL CHECK(NF90_INQ_VARID(NCID,'x',XVAR))
                CALL CHECK(NF90_INQ_VARID(NCID,'y',YVAR))
                CALL CHECK(NF90_GET_VAR(NCID,XVAR,G2XY(1,:)))
                CALL CHECK(NF90_GET_VAR(NCID,YVAR,G2XY(2,:)))
                CALL CHECK(NF90_CLOSE(NCID))
#endif
            ELSE
                OPEN(FILE=TRIM(Fort14File2),UNIT=141,ACTION="READ",STATUS="OLD")
                READ(141,*) JunkC
                READ(141,*) NE,NN
                ALLOCATE(G2XY(1:2,1:NN))
                NumNodesMesh2 = NN
                DO I = 1,NN
                    READ(141,*) JunkI,G2XY(1,I),G2XY(2,I)
                ENDDO
                CLOSE(141)
            ENDIF    

            ALLOCATE(TranslationTable(1:NumNodesGlobal))

            !...Create a search tree
            SearchTree => KDTREE2_CREATE(G2XY,SORT=.TRUE.,REARRANGE=.TRUE.)

            !...Make the translation
            DO I = 1,NumNodesGlobal
                X = G1XY(1,I)
                Y = G1XY(2,I)
                CALL KDTREE2_N_NEAREST(TP=SearchTree,QV=(/X,Y/),NN=1,&
                    RESULTS=SearchResult)
                TranslationTable(I) = SearchResult(1)%IDX
            ENDDO

            CALL KDTREE2_DESTROY(SearchTree)
            DEALLOCATE(G1XY)
            DEALLOCATE(G2XY)

            IF(Verbose.GE.3)THEN
                WRITE(*,'(A,I4.4,A)') "Core ", MyRank, " created the node-to-node translation table."
            ENDIF

        END SUBROUTINE

        SUBROUTINE ReadOutputFileList(ListFile,ListLength,List,Tag,Error)
            IMPLICIT NONE
            INTEGER,INTENT(OUT)      :: ListLength
            CHARACTER(*),INTENT(IN)  :: ListFile
            CHARACTER(*),ALLOCATABLE,INTENT(OUT) :: List(:)
            CHARACTER(*),ALLOCATABLE,INTENT(OUT) :: Tag(:)
            LOGICAL,INTENT(OUT)      :: Error
            INTEGER                  :: I
            INTEGER                  :: J
            LOGICAL                  :: FOUND

            ERROR = .FALSE.
            INQUIRE(FILE=TRIM(ListFile),EXIST=FOUND)
            IF(.NOT.FOUND)THEN
                WRITE(*,'(A)') "ERROR: The ADCIRC-OUTPUT-LIST file was not found."
                ERROR = .TRUE.
                RETURN
            ENDIF

            OPEN(FILE=TRIM(ListFile),UNIT=60,ACTION="READ",STATUS="OLD")
            READ(60,*) ListLength
            ALLOCATE(List(1:ListLength))
            ALLOCATE(Tag(1:ListLength))
            
            DO I = 1,ListLength
                READ(60,*) List(I),Tag(I)
                INQUIRE(FILE=TRIM(List(I)),EXIST=FOUND)
                IF(.NOT.FOUND)THEN
                    Error = .TRUE.
                    WRITE(*,'(3A)') "ERROR: The file: ",TRIM(List(I))," was not found."
                    CLOSE(60)
                    RETURN
                ENDIF
            ENDDO

            !...Sanity check so we don't write same file
            DO I = 1,ListLength
                DO J = 1,ListLength
                    IF(I.EQ.J)CYCLE
                    IF(Tag(I).EQ.Tag(J))THEN
                        Error = .TRUE.
                        WRITE(*,'(A)') "ERROR: No two output list tags may match."
                        RETURN
                    ENDIF
                ENDDO
            ENDDO

            !...Build the RecordsList array
            IF(ALLOCATED(RecordsList))DEALLOCATE(RecordsList)
            ALLOCATE(RecordsList(1:ListLength))
            DO I = 1,ListLength
                RecordsList(I) = I
            ENDDO    

            RETURN

        END SUBROUTINE



        SUBROUTINE ProcessFort14File

                

#ifdef NETCDF
                USE netcdf
#endif

                IMPLICIT NONE

#ifdef CMPI
                INCLUDE 'mpif.h'
#endif

#ifdef NETCDF
                INTEGER,ALLOCATABLE :: NC_Code(:)
                INTEGER             :: NC_ID
                INTEGER,ALLOCATABLE :: NC_LandBoundEntry(:,:)
                INTEGER,ALLOCATABLE :: NC_NumNodesLandBound(:)
                INTEGER,ALLOCATABLE :: NC_NumNodesOpenBound(:)
                INTEGER,ALLOCATABLE :: NC_OpenBoundEntry(:,:)
                INTEGER             :: NC_Temp
                INTEGER             :: NC_Var
#endif

                INTRINSIC           :: INDEX
                INTRINSIC           :: NINT

                CHARACTER(LEN=1)    :: JunkC
                CHARACTER(LEN=100)  :: EdgeFileName

                INTEGER             :: Counter
                INTEGER             :: I
                INTEGER             :: J
                INTEGER             :: JunkI
                INTEGER             :: K
                INTEGER,ALLOCATABLE :: NC(:,:)
                INTEGER,ALLOCATABLE :: NEList(:)
                INTEGER,ALLOCATABLE :: NET(:)
                INTEGER,ALLOCATABLE :: NPList(:)
                INTEGER,ALLOCATABLE :: NPT(:)
                INTEGER             :: NumElemsGlobal
                INTEGER             :: NumElemsLocal
                INTEGER             :: NumLandBoundaries
                INTEGER             :: NumOpenBoundaries
                INTEGER,ALLOCATABLE :: OldToNew(:)
                INTEGER             :: TotNumLandBoundaryNodes
                INTEGER             :: TotNumOpenBoundaryNodes

                REAL,ALLOCATABLE    :: BathGlobal(:)
                REAL,ALLOCATABLE    :: Lat(:)
                REAL,ALLOCATABLE    :: Long(:)
                REAL                :: Temp_Lat
                REAL                :: Temp_Long
                REAL                :: ZVal

                TYPE BoundaryListing
                    INTEGER            :: NumNodes
                    INTEGER            :: Code
                    CHARACTER(LEN=100) :: Header
                    REAL,ALLOCATABLE   :: Entry1(:)
                    REAL,ALLOCATABLE   :: Entry2(:)
                    REAL,ALLOCATABLE   :: Entry3(:)
                    REAL,ALLOCATABLE   :: Entry4(:)
                    REAL,ALLOCATABLE   :: Entry5(:)
                END TYPE
                TYPE(BoundaryListing),ALLOCATABLE :: LandBoundaries(:)
                TYPE(BoundaryListing),ALLOCATABLE :: OpenBoundaries(:)

                IF((INDEX(ContourFileType,"GRID-DECOMP").GT.0).OR.(INDEX(ColorLines,"GRID-DECOMP").GT.0))THEN
                    CALL MapColor
                ENDIF

                IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                    OPEN(UNIT=24,FILE=TRIM(Fort14File),ACTION="READ")
                ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    CALL Check(NF90_OPEN(Fort14File,NF90_NOWRITE,NC_ID))
#endif
                ENDIF

                OPEN(UNIT=13,FILE=TRIM(TempPath)//TRIM(Fort14File)//".tri",ACTION="WRITE")
                OPEN(UNIT=16,FILE=TRIM(TempPath)//TRIM(Fort14File)//".bnd.xy",ACTION="WRITE")
                IF(IfPlotGrid.EQ.1)THEN
                    NumEdgeFiles = 1
                    WRITE(UNIT=EdgeFileName,FMT='(A,I3.3,A)') TRIM(TempPath)//TRIM(Fort14File)//".edges.",NumEdgeFiles,".xy"
                    OPEN(UNIT=31,FILE=TRIM(EdgeFileName),ACTION="WRITE")
                ENDIF

                IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                    READ(UNIT=24,FMT='(A)') JunkC
                    READ(UNIT=24,FMT=*) NumElemsGlobal, NumNodesGlobal
                ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    CALL Check(NF90_INQ_DIMID(NC_ID,"nele",NC_Var))
                    CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_Var,len=NumElemsGlobal))
                    CALL Check(NF90_INQ_DIMID(NC_ID,"node",NC_Var))
                    CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_Var,len=NumNodesGlobal))
#endif
                ENDIF

                ALLOCATE(BathGlobal(NumNodesGlobal))
                ALLOCATE(Lat(NumNodesGlobal))
                ALLOCATE(Long(NumNodesGlobal))
                ALLOCATE(NC(3,NumElemsGlobal))
                ALLOCATE(NET(NumElemsGlobal))
                ALLOCATE(NPT(NumNodesGlobal))
                ALLOCATE(OldToNew(NumNodesGlobal))

                IF(OptimizeContours.EQ.1)THEN
                    ALLOCATE(BdyNodes(NumNodesGlobal))
                    BdyNodes = .FALSE.
                ENDIF

                IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                    DO I=1,NumNodesGlobal
                        READ(24,*) JunkI,Long(I),Lat(I),BathGlobal(I)
                    ENDDO
                ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    CALL Check(NF90_INQ_VARID(NC_ID,'x',NC_Var))
                    CALL Check(NF90_GET_VAR(NC_ID,NC_Var,Long))
                    CALL Check(NF90_INQ_VARID(NC_ID,'y',NC_Var))
                    CALL Check(NF90_GET_VAR(NC_ID,NC_Var,Lat))
                    CALL Check(NF90_INQ_VARID(NC_ID,'depth',NC_Var))
                    CALL Check(NF90_GET_VAR(NC_ID,NC_Var,BathGlobal))
#endif
                ENDIF

                !...First pass at Lat/Lon buffer
                NPT = 0
                DO I=1,NumNodesGlobal
                    IF(Long(I).GT.(LongE+LatLonBuffer))CYCLE
                    IF(Long(I).LT.(LongW-LatLonBuffer))CYCLE
                    IF(Lat(I).GT.(LatN+LatLonBuffer))CYCLE
                    IF(Lat(I).LT.(LatS-LatLonBuffer))CYCLE
                    NPT(I) = 1
                ENDDO
                        
                        

        ! Shouldn't be necessary now that we have switched to a larger page?
                
        !       IF(DoCenter.EQ.1)THEN
                
        !           Temp_Long = Long(C_Node)
        !           Temp_Lat = Lat(C_Node)
                    
                    !...Correct Aspect Ratio to 3:2 to prevent
                    ! plotting off of page.
        !           IF((C_Width/C_Height).LT.(1.5))THEN
        !               C_Width = C_Height * 1.5
        !           ENDIF
                    
        !           LongW = Temp_Long - C_Width / 2.0d0
        !           LongE = Temp_Long + C_Width / 2.0d0
        !           LatN = Temp_Lat + C_Height / 2.0d0
        !           LatS = Temp_Lat - C_Height / 2.0d0
                    
        !           NPT = 0
        !           DO I=1,NumNodesGlobal
        !               IF(Long(I).GT.(LongE+LatLonBuffer))CYCLE
        !               IF(Long(I).LT.(LongW-LatLonBuffer))CYCLE
        !               IF(Lat(I).GT.(LatN+LatLonBuffer))CYCLE
        !               IF(Lat(I).LT.(LatS-LatLonBuffer))CYCLE
        !               NPT(I) = 1
        !           ENDDO
                    
        !       ENDIF

                NumNodesLocal = SUM(NPT)

                ALLOCATE(NPList(NumNodesLocal))

                K = 0
                OldToNew = 0
                DO I=1,NumNodesGlobal
                    IF(NPT(I).EQ.1)THEN
                        K = K + 1
                        NPList(K) = I
                        OldToNew(I) = K
                    ENDIF
                ENDDO

                IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                    DO I=1,NumElemsGlobal
                        READ(24,*) JunkI, K,NC(1,JunkI),NC(2,JunkI),NC(3,JunkI)
                        IF(JunkI.NE.I)THEN
#ifdef CMPI
                            DO K=1,NumProcs-1
                                CALL MPI_SEND(1,1,MPI_INTEGER,K,1,MPI_COMM_WORLD,IERR)
                            ENDDO
#endif
                            STOP 'FATAL ERROR: Elements out of order in grid file.'
                        ENDIF
                    ENDDO
                ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    CALL Check(NF90_INQ_VARID(NC_ID,'element',NC_Var))
                    CALL Check(NF90_GET_VAR(NC_ID,NC_Var,NC))
#endif
                ENDIF

                NET = 1
                DO I=1,NumElemsGlobal
                    DO K=1,3
                        IF(NPT(NC(K,I)).EQ.0)THEN
                            NET(I) = 0
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
                ALLOCATE(BathLocal(1:NumNodesLocal))

                DO I=1,NumNodesLocal
                    XYZNodes(I) = NPList(I)
                    X(I)        = Long(NPList(I))
                    Y(I)        = Lat(NPList(I))
                    BathLocal(I)= BathGlobal(NPList(I))
                ENDDO

                Counter = 0

                DO I=1,NumElemsLocal

                    WRITE(UNIT=13,FMT=*) OldToNew(NC(1,NEList(I)))-1, &
                                         OldToNew(NC(2,NEList(I)))-1, &
                                         OldToNew(NC(3,NEList(I)))-1

                    IF(IfPlotGrid.EQ.1)THEN

                        IF(Counter.GT.99999)THEN
                            Counter = 0
                            CLOSE(UNIT=31,STATUS="KEEP")
                            NumEdgeFiles = NumEdgeFiles + 1
                            WRITE(UNIT=EdgeFileName,FMT='(A,I3.3,A)') TRIM(TempPath)//TRIM(Fort14File)//".edges.",&
                                                                      NumEdgeFiles,".xy"
                            OPEN(UNIT=31,FILE=TRIM(EdgeFileName),ACTION="WRITE")
                        ENDIF

                        IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
                            ZVal = 0.5D0*(BathGlobal(NC(1,NEList(I)))+BathGlobal(NC(2,NEList(I))))
                            ZVal = ZVal * ContourConversionFactor
                            WRITE(UNIT=31,FMT='(A,F15.6)') "> -Z",ZVal
                        ELSEIF(INDEX(ColorLines,"GRID-DECOMP").GT.0)THEN
                            ZVal = 0.5D0*(NodeColors(NC(1,NEList(I)))+NodeColors(NC(2,NEList(I))))
                            WRITE(UNIT=31,FMT='(A,F15.6)') "> -Z",ZVal
                        ELSEIF(TRIM(ColorLines).EQ."GRID-SIZE")THEN
                            ZVal = ComputeDistance( Long(NC(1,NEList(I))), &
                                                    Lat( NC(1,NEList(I))), &
                                                    Long(NC(2,NEList(I))), &
                                                    Lat( NC(2,NEList(I)))  )
                            ZVal = ZVal * ContourConversionFactor
                            WRITE(UNIT=31,FMT='(A,F15.6)') "> -Z",ZVal
                        ELSE
                            WRITE(UNIT=31,FMT='(A)') ">"
                        ENDIF
                        WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                            Long(NC(1,NEList(I))),      &
                                            Lat( NC(1,NEList(I)))
                        WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                            Long(NC(2,NEList(I))),      &
                                            Lat( NC(2,NEList(I)))
                        Counter = Counter + 1

                        IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
                            ZVal = 0.5D0*(BathGlobal(NC(2,NEList(I)))+BathGlobal(NC(3,NEList(I))))
                            ZVal = ZVal * ContourConversionFactor
                            WRITE(UNIT=31,FMT='(A,F15.6)') "> -Z",ZVal
                        ELSEIF(INDEX(ColorLines,"GRID-DECOMP").GT.0)THEN
                            ZVal = 0.5D0*(NodeColors(NC(2,NEList(I)))+NodeColors(NC(3,NEList(I))))
                            WRITE(UNIT=31,FMT='(A,F15.6)') "> -Z",ZVal
                        ELSEIF(TRIM(ColorLines).EQ."GRID-SIZE")THEN
                            ZVal = ComputeDistance( Long(NC(2,NEList(I))), &
                                                    Lat( NC(2,NEList(I))), &
                                                    Long(NC(3,NEList(I))), &
                                                    Lat( NC(3,NEList(I)))  )
                            ZVal = ZVal * ContourConversionFactor
                            WRITE(UNIT=31,FMT='(A,F15.6)') "> -Z",ZVal
                        ELSE
                            WRITE(UNIT=31,FMT='(A)') ">"
                        ENDIF
                        WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                            Long(NC(2,NEList(I))),      &
                                            Lat( NC(2,NEList(I)))
                        WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                            Long(NC(3,NEList(I))),      &
                                            Lat( NC(3,NEList(I)))
                        Counter = Counter + 1

                        IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
                            ZVal = 0.5D0*(BathGlobal(NC(3,NEList(I)))+BathGlobal(NC(1,NEList(I))))
                            ZVal = ZVal * ContourConversionFactor
                            WRITE(UNIT=31,FMT='(A,F15.6)') "> -Z",ZVal
                        ELSEIF(INDEX(ColorLines,"GRID-DECOMP").GT.0)THEN
                            ZVal = 0.5D0*(NodeColors(NC(3,NEList(I)))+NodeColors(NC(1,NEList(I))))
                            WRITE(UNIT=31,FMT='(A,F15.6)') "> -Z",ZVal
                        ELSEIF(TRIM(ColorLines).EQ."GRID-SIZE")THEN
                            ZVal = ComputeDistance( Long(NC(3,NEList(I))), &
                                                    Lat( NC(3,NEList(I))), &
                                                    Long(NC(1,NEList(I))), &
                                                    Lat( NC(1,NEList(I)))  )
                            ZVal = ZVal * ContourConversionFactor
                            WRITE(UNIT=31,FMT='(A,F15.6)') "> -Z",ZVal
                        ELSE
                            WRITE(UNIT=31,FMT='(A)') ">"
                        ENDIF
                        WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                            Long(NC(3,NEList(I))),      &
                                            Lat( NC(3,NEList(I)))
                        WRITE(UNIT=31,FMT='(2(F11.6,1X))')              &
                                            Long(NC(1,NEList(I))),      &
                                            Lat( NC(1,NEList(I)))
                        Counter = Counter + 1

                    ENDIF

                ENDDO

                Counter = 0
                IF(IfPlotBoundaries.GE.1)THEN

                    IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                        READ(24,*) NumOpenBoundaries
                        READ(24,*) TotNumOpenBoundaryNodes
                    ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                        CALL Check(NF90_INQ_DIMID(NC_ID,"nope",NC_Var))
                        CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_Var,len=NumOpenBoundaries))
                        CALL Check(NF90_INQ_VARID(NC_ID,"neta",NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID,NC_Var,TotNumOpenBoundaryNodes))
#endif
                    ENDIF

                    ALLOCATE(OpenBoundaries(1:NumOpenBoundaries))

                    IF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                        ALLOCATE(NC_NumNodesOpenBound(1:NumOpenBoundaries))
                        CALL Check(NF90_INQ_VARID(NC_ID,'nvdll',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID,NC_Var,NC_NumNodesOpenBound))
                        CALL Check(NF90_INQ_VARID(NC_ID,'max_nvdll',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID,NC_Var,NC_Temp))
                        ALLOCATE(NC_OpenBoundEntry(1:NumOpenBoundaries,1:NC_Temp))
                        CALL Check(NF90_INQ_VARID(NC_ID,'nbdv',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID,NC_Var,NC_OpenBoundEntry))
#endif
                    ENDIF

                    DO J=1,NumOpenBoundaries

                        IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                            READ(24,*) OpenBoundaries(J)%NumNodes
                        ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                            OpenBoundaries(J)%NumNodes = NC_NumNodesOpenBound(J)
#endif
                        ENDIF

                        ALLOCATE(OpenBoundaries(J)%Entry1(1:OpenBoundaries(J)%NumNodes))

                        DO I=1,OpenBoundaries(J)%NumNodes

                            IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                                READ(24,*) OpenBoundaries(J)%Entry1(I)
                            ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                                OpenBoundaries(J)%Entry1(I) = REAL(NC_OpenBoundEntry(J,I))
#endif
                            ENDIF

                            IF(OptimizeContours.EQ.1)THEN
                                BdyNodes(NINT(OpenBoundaries(J)%Entry1(I))) = .TRUE.
                            ENDIF

                            IF(I.GT.1)THEN
                              
                                IF(((Long(NINT(OpenBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                                    (Long(NINT(OpenBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                                    (Lat(NINT(OpenBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                                    (Lat(NINT(OpenBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                                   ((Long(NINT(OpenBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                                    (Long(NINT(OpenBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                                    (Lat(NINT(OpenBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                                    (Lat(NINT(OpenBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer))))THEN

                                    WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                                    WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                        Long(NINT(OpenBoundaries(J)%Entry1(I-1))), &
                                                        Lat(NINT(OpenBoundaries(J)%Entry1(I-1)))
                                    WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                        Long(NINT(OpenBoundaries(J)%Entry1(I))),   &
                                                        Lat(NINT(OpenBoundaries(J)%Entry1(I)))
                                    Counter = Counter + 1
                                    
                                ENDIF

                            ENDIF

                        ENDDO

                    ENDDO

                    IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                        READ(24,*) NumLandBoundaries
                        READ(24,*) TotNumLandBoundaryNodes
                    ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                        CALL Check(NF90_INQ_DIMID(NC_ID,"nbou",NC_Var))
                        CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_Var,len=NumLandBoundaries))
                        CALL Check(NF90_INQ_VARID(NC_ID,"nvel",NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID,NC_VAR,TotNumLandBoundaryNodes))
#endif
                    ENDIF

                    ALLOCATE(LandBoundaries(1:NumLandBoundaries))

                    IF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                        ALLOCATE(NC_NumNodesLandBound(1:NumLandBoundaries))
                        CALL Check(NF90_INQ_VARID(NC_ID,'nvell',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID,NC_Var,NC_NumNodesLandBound))
                        ALLOCATE(NC_Code(1:NumLandBoundaries))
                        CALL Check(NF90_INQ_VARID(NC_ID,'ibtype',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID,NC_Var,NC_Code))
                        CALL Check(NF90_INQ_VARID(NC_ID,'max_nvell',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID,NC_Var,NC_Temp))
                        ALLOCATE(NC_LandBoundEntry(1:NumLandBoundaries,1:NC_Temp))
                        CALL Check(NF90_INQ_VARID(NC_ID,'nbvv',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID,NC_Var,NC_LandBoundEntry))
#endif
                    ENDIF

                    DO J=1,NumLandBoundaries

                        IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                            READ(24,*) LandBoundaries(J)%NumNodes, LandBoundaries(J)%Code
                        ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                            LandBoundaries(J)%NumNodes = NC_NumNodesLandBound(J)
                            LandBoundaries(J)%Code = NC_Code(J)
#endif
                        ENDIF

                        IF((LandBoundaries(J)%Code.EQ. 0).OR. &
                           (LandBoundaries(J)%Code.EQ. 1).OR. &
                           (LandBoundaries(J)%Code.EQ. 2).OR. &
                           (LandBoundaries(J)%Code.EQ.10).OR. &
                           (LandBoundaries(J)%Code.EQ.11).OR. &
                           (LandBoundaries(J)%Code.EQ.12).OR. &
                           (LandBoundaries(J)%Code.EQ.13).OR. &
                           (LandBoundaries(J)%Code.EQ.20).OR. &
                           (LandBoundaries(J)%Code.EQ.21).OR. &
                           (LandBoundaries(J)%Code.EQ.22).OR. &
                           (LandBoundaries(J)%Code.EQ.23).OR. &
                           (LandBoundaries(J)%Code.EQ.52))THEN

                            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))
         
                            DO I=1,LandBoundaries(J)%NumNodes
         
                                IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                                    READ(24,*) LandBoundaries(J)%Entry1(I)
                                ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                                    LandBoundaries(J)%Entry1(I) = REAL(NC_LandBoundEntry(J,I))
#endif
                                ENDIF

                                IF(OptimizeContours.EQ.1)THEN
                                    BdyNodes(NINT(LandBoundaries(J)%Entry1(I))) = .TRUE.
                                ENDIF

                                IF(I.GT.1)THEN
                                
                                    IF(((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                                        (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                                        (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                                        (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                                       ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                                        (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                                        (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                                        (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer))))THEN

                                        WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                                        WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                            Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                            Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                                        WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                            Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                            Lat(NINT(LandBoundaries(J)%Entry1(I)))
                                        Counter = Counter + 1
                                        
                                    ENDIF

                                ENDIF

                            ENDDO

                        ELSEIF(LandBoundaries(J)%Code.EQ.24)THEN

                            ALLOCATE(LandBoundaries(J)%Entry1(1:LandBoundaries(J)%NumNodes))
                            ALLOCATE(LandBoundaries(J)%Entry2(1:LandBoundaries(J)%NumNodes))

                            DO I=1,LandBoundaries(J)%NumNodes

                                IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                                    READ(24,*) LandBoundaries(J)%Entry1(I),LandBoundaries(J)%Entry2(I)
                                ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                                    LandBoundaries(J)%Entry1(I) = REAL(NC_LandBoundEntry(J,I))
#endif
                                ENDIF

                                IF(I.GT.1)THEN
                                
                                    IF(((Long(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                                        (Long(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                                        (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                                        (Lat(NINT(LandBoundaries(J)%Entry1(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                                       ((Long(NINT(LandBoundaries(J)%Entry1(I))).GE.(LongW-LatLonBuffer)).AND.   &
                                        (Long(NINT(LandBoundaries(J)%Entry1(I))).LE.(LongE+LatLonBuffer)).AND.   &
                                        (Lat(NINT(LandBoundaries(J)%Entry1(I))).GE.(LatS -LatLonBuffer)).AND.    &
                                        (Lat(NINT(LandBoundaries(J)%Entry1(I))).LE.(LatN +LatLonBuffer))))THEN

                                        WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                                        WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                            Long(NINT(LandBoundaries(J)%Entry1(I-1))), &
                                                            Lat(NINT(LandBoundaries(J)%Entry1(I-1)))
                                        WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                            Long(NINT(LandBoundaries(J)%Entry1(I))),   &
                                                            Lat(NINT(LandBoundaries(J)%Entry1(I)))
                                        Counter = Counter + 1
                                        
                                    ENDIF

                                ENDIF

                            ENDDO

                            IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                                DO I=1,LandBoundaries(J)%NumNodes
                                    IF(I.GT.1)THEN
                                        IF(((Long(NINT(LandBoundaries(J)%Entry2(I-1))).GE.(LongW-LatLonBuffer)).AND. &
                                            (Long(NINT(LandBoundaries(J)%Entry2(I-1))).LE.(LongE+LatLonBuffer)).AND. &
                                            (Lat(NINT(LandBoundaries(J)%Entry2(I-1))).GE.(LatS -LatLonBuffer)).AND.  &
                                            (Lat(NINT(LandBoundaries(J)%Entry2(I-1))).LE.(LatN +LatLonBuffer))).OR.  &
                                           ((Long(NINT(LandBoundaries(J)%Entry2(I))).GE.(LongW-LatLonBuffer)).AND.   &
                                            (Long(NINT(LandBoundaries(J)%Entry2(I))).LE.(LongE+LatLonBuffer)).AND.   &
                                            (Lat(NINT(LandBoundaries(J)%Entry2(I))).GE.(LatS -LatLonBuffer)).AND.    &
                                            (Lat(NINT(LandBoundaries(J)%Entry2(I))).LE.(LatN +LatLonBuffer))))THEN

                                            WRITE(UNIT=16,FMT='(A,I6,A,I6)') "> BND Edge ",Counter,"->",Counter+1
                                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                                Long(NINT(LandBoundaries(J)%Entry2(I-1))), &
                                                                Lat(NINT(LandBoundaries(J)%Entry2(I-1)))
                                            WRITE(UNIT=16,FMT='(2(F11.6,1X))')                              &
                                                                Long(NINT(LandBoundaries(J)%Entry2(I))),   &
                                                                Lat(NINT(LandBoundaries(J)%Entry2(I)))
                                            Counter = Counter + 1                               
                                        ENDIF
                                    ENDIF
                                ENDDO
                            ENDIF

                        ELSE

                            WRITE(*,'(A)',ADVANCE="YES") " "
                            WRITE(*,'(A)',ADVANCE="YES") "ERROR!"
                            WRITE(*,'(A,I4)',ADVANCE="YES") "J = ", J
                            WRITE(*,'(A,I10)',ADVANCE="YES") "LandBoundaries(J)%Code = ", LandBoundaries(J)%Code 

                        ENDIF

                    ENDDO

                    CLOSE(UNIT=16,STATUS="KEEP")

                ENDIF

                IF(TRIM(GridFileFormat).EQ."ASCII")THEN
                    CLOSE(UNIT=24,STATUS="KEEP")
                ELSEIF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    CALL Check(NF90_CLOSE(NC_ID))
#endif
                ENDIF
                CLOSE(UNIT=13,STATUS="KEEP")
                IF(IfPlotGrid.EQ.1)THEN
                    CLOSE(UNIT=31,STATUS="KEEP")
                ENDIF

                IF(NeedTranslationTable)THEN
                    !...Make sure we save some information
                    ALLOCATE(G1XY(1:2,1:NumNodesGlobal))
                    G1XY(1,:) = Long(:)
                    G1XY(2,:) = Lat(:)
                ENDIF

                IF(ALLOCATED(BathGlobal))           DEALLOCATE(BathGlobal)
                IF(ALLOCATED(LandBoundaries))       DEALLOCATE(LandBoundaries)
                IF(ALLOCATED(Lat))                  DEALLOCATE(Lat)
                IF(ALLOCATED(Long))                 DEALLOCATE(Long)
                IF(ALLOCATED(NC))                   DEALLOCATE(NC)
                IF(ALLOCATED(NET))                  DEALLOCATE(NET)
                IF(ALLOCATED(NPT))                  DEALLOCATE(NPT)
                IF(ALLOCATED(OldToNew))             DEALLOCATE(OldToNew)
                IF(ALLOCATED(OpenBoundaries))       DEALLOCATE(OpenBoundaries)

                IF(TRIM(GridFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                    IF(ALLOCATED(NC_Code))              DEALLOCATE(NC_Code)
                    IF(ALLOCATED(NC_LandBoundEntry))    DEALLOCATE(NC_LandBoundEntry)
                    IF(ALLOCATED(NC_NumNodesLandBound)) DEALLOCATE(NC_NumNodesLandBound)
                    IF(ALLOCATED(NC_NumNodesOpenBound)) DEALLOCATE(NC_NumNodesOpenBound)
                    IF(ALLOCATED(NC_OpenBoundEntry))    DEALLOCATE(NC_OpenBoundEntry)
#endif
                ENDIF

                IF(Verbose.GE.3)THEN
                    WRITE(*,'(A,I4.4,A)',ADVANCE="YES") "Core ",MyRank," processed the grid file."
                ENDIF

        END SUBROUTINE



        SUBROUTINE ReadInputFile(ERROR)

#ifdef NETCDF
                USE netcdf
#endif
                

                IMPLICIT NONE

                LOGICAL,INTENT(OUT) :: ERROR

#ifdef NETCDF
                INTEGER           :: NC_ID
                INTEGER           :: NC_Status
#endif

                INTRINSIC         :: INDEX
                INTRINSIC         :: LEN_TRIM
                INTRINSIC         :: TRIM

                CHARACTER(LEN=50) :: GoogleDateC
                CHARACTER(LEN=50) :: GoogleTransparencyC
                CHARACTER(LEN=1)  :: JunkC
                CHARACTER(LEN=4)  :: LabelsSizeC
                CHARACTER(LEN=50) :: NumLayersC
                CHARACTER(LEN=50) :: TempC
                CHARACTER(LEN=50) :: TempC2

                INTEGER           :: I
                INTEGER           :: IOS
                INTEGER           :: JunkI
                INTEGER           :: NumRecs63
                INTEGER           :: P
                INTEGER           :: Position
                INTEGER           :: RecordsBegin
                INTEGER           :: RecordsEnd
                INTEGER           :: RecordsInc
                INTEGER           :: TempI

                REAL              :: TempR

                IF(VersionNumber.EQ.0)THEN
                    IF(InputFile(1:2).EQ."FG")THEN
                        READ(UNIT=InputFile,FMT='(2X,I2)') VersionNumber
                    ELSE
                        VersionNumber = 49
                    ENDIF
                ENDIF

                OPEN(UNIT=11,FILE=TRIM(InputFile),ACTION="READ")

                READ(UNIT=11,FMT='(A1)')  JunkC ! FOR BEST RESULTS ...
                READ(UNIT=11,FMT='(A1)')  JunkC ! 01234567890 ...

                READ(UNIT=11,FMT=*)       Verbose 
                READ(UNIT=11,FMT='(A50)') Path
                IF(VersionNumber.LT.41)THEN
                    GSPath = " "
                ELSE
                    READ(UNIT=11,FMT='(A50)') GSPath
                ENDIF
                READ(UNIT=11,FMT='(A50)') TempPath
                IF(TempPath(LEN_TRIM(TempPath):LEN_TRIM(TempPath)).NE."/")THEN
                    TempPath = TRIM(TempPath)//"/"
                ENDIF
                IF(MyRank.EQ.0)THEN
                    OPEN(FILE=TRIM(TempPath)//"TESTER.txt",UNIT=99,ACTION="WRITE",IOSTAT=IOS)
                    IF(IOS.NE.0)THEN
                        CALL SYSTEM("mkdir "//TRIM(TempPath))
                        WRITE(*,'(A)') "WARNING: Temporary folder doesn't exist.  FigureGen has created it."
                    ELSE
                        CLOSE(99,STATUS="DELETE")
                    ENDIF
                ENDIF    
                READ(UNIT=11,FMT='(A50)') AlphaLabel
                READ(UNIT=11,FMT='(A50)') TempC
                IF(TempC(1:1).EQ."1")THEN
                    TempC2 = TempC(1:INDEX(TempC,",")-1)
                    READ(UNIT=TempC2,FMT=*) IfAddPlotLabel
                    PlotLabel = TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC))
                ENDIF
                READ(UNIT=11,FMT=*)       IfAddTimeBar
                IF(IfAddTimeBar.EQ.2.OR.IfAddTimeBar.EQ.3)THEN
                    BACKSPACE(11)
                    READ(11,*) JunkI,ColdStartDate%Year,ColdStartDate%Month,ColdStartDate%Day, &
                        ColdStartDate%Hour,ColdStartDate%Minute,ColdStartDate%Second
                ENDIF        

                READ(UNIT=11,FMT='(A50)') TempC
                
                CALL CapitalizeWord(TempC)
                IF(TRIM(TempC).EQ."CENTER")THEN
                    DoCenter = 1
                    READ(UNIT=11,FMT=*) C_Node
                    READ(UNIT=11,FMT=*) C_Width
                    READ(UNIT=11,FMT=*) C_Height
                ELSE
                    BACKSPACE(UNIT=11)
                    READ(UNIT=11,FMT=*) LongW
                    READ(UNIT=11,FMT=*) LongE
                    READ(UNIT=11,FMT=*) LatS
                    READ(UNIT=11,FMT=*) LatN
                ENDIF    
                   
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

                IF(INDEX(Fort14File,",").GT.0)THEN
                    !...We will make a translation table later
                    NeedTranslationTable = .TRUE.
                    TempC       = Fort14File
                    Fort14File  = TempC(1:INDEX(TempC,",")-1)
                    Fort14File2 = TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC))
                ELSE
                    NeedTranslationTable = .FALSE.
                ENDIF

                INQUIRE(FILE=TRIM(Fort14File),EXIST=FileExists)
                IF(.NOT.FileExists)THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(*,'(A)') "FATAL ERROR: Grid file does not exist. FigureGen is quitting."
                    ENDIF

                    ERROR = .TRUE.
                    RETURN

                ENDIF

                IF(NeedTranslationTable)THEN
                    INQUIRE(FILE=TRIM(Fort14File2),EXIST=FileExists)
                    IF(.NOT.FileExists)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: Second Grid file does not exist. FigureGen is quitting."
                        ENDIF

                        ERROR = .TRUE.
                        RETURN
                    ENDIF    
                ENDIF    


                IF(INDEX(Fort14File,".nc").GT.0)THEN
#ifdef NETCDF
                    NC_Status = NF90_OPEN(Fort14File,NF90_NOWRITE,NC_ID)
                    IF(NC_Status.EQ.NF90_NOERR)THEN
                        WRITE(GridFileFormat,'(A)') "NETCDF"
                        CALL Check(NF90_CLOSE(NC_ID))
                    ELSE
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: Grid file is not in NetCDF format."
                        ENDIF
                    
                    ERROR = .TRUE.
                    RETURN
                    
                    ENDIF
#else
                    IF(MyRank.EQ.0)THEN
                        WRITE(*,'(A)') "FATAL ERROR: FigureGen has not been compiled to be compatible"
                        WRITE(*,'(A)') "             with NetCDF files.  Please use something like:"
                        WRITE(*,'(A)') " "
                        WRITE(*,'(A)') "   pgf90 FigureGen.F90 -DNETCDF -I$TACC_NETCDF_INC -L$TACC_NETCDF_LIB -lnetcdf"
                        WRITE(*,'(A)') " "
                    ENDIF

                    ERROR = .TRUE.
                    RETURN

#endif
                ELSE
                    WRITE(GridFileFormat,'(A)') "ASCII"
                ENDIF
                READ(UNIT=11,FMT=*)       IfPlotGrid

                READ(UNIT=11,FMT='(A1)')  JunkC ! PARAMETERS FOR CONTOURS
                
                READ(UNIT=11,FMT=*)       IfPlotFilledContours
                READ(UNIT=11,FMT=*)       IfPlotContourLines
                IF((IfPlotFilledContours.EQ.1).AND.(IfPlotContourLines.EQ.2))THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: IfPlotFilledContours = 1, then IfPlotContourLines = 0 or 1."
                    ENDIF
                
                    ERROR = .TRUE.
                    RETURN
                
                ENDIF
                IF((IfPlotFilledContours.EQ.2).AND.(IfPlotContourLines.EQ.1))THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: IfPlotFilledContours = 2, then IfPlotContourLines = 0 or 2."
                    ENDIF
                
                
                    ERROR = .TRUE.
                    RETURN
                
                ENDIF
                READ(UNIT=11,FMT='(A50)') TempC
                IF(IfPlotFilledContours.LE.1)THEN
                    IF(INDEX(TempC,",").GT.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: IfPlotFilledContours = 0 or 1, then "//&
                                        "list only one contour file name."
                        ENDIF
                    
                    
                        ERROR = .TRUE.
                        RETURN
                    
                    ENDIF
                    ContourFile1 = TRIM(TempC)
                    INQUIRE(FILE=TRIM(ContourFile1),EXIST=FileExists)
                    IF(.NOT.FileExists)THEN
                        IF(IfPlotFilledContours.GT.0.OR.IfPlotContourLines.GT.0)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: The contour file does not exist. FigureGen is quitting."
                            ENDIF
                        
                            ERROR = .TRUE.
                            RETURN
                        
                        ELSE
                            GOTO 2222
                        ENDIF
                    ENDIF
                    IF(INDEX(ContourFile1,".nc").GT.0)THEN
#ifdef NETCDF
                        NC_Status = NF90_OPEN(ContourFile1,NF90_NOWRITE,NC_ID)
                        IF(NC_Status.EQ.NF90_NOERR)THEN
                            WRITE(ContourFileFormat1,'(A)') "NETCDF"
                            CALL Check(NF90_CLOSE(NC_ID))
                        ELSE
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: Contour file is not in NetCDF format."
                            ENDIF
                        
                            ERROR = .TRUE.
                            RETURN
                        
                        ENDIF
#else
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: FigureGen has not been compiled to be compatible"
                            WRITE(*,'(A)') "             with NetCDF files.  Please use something like:"
                            WRITE(*,'(A)') " "
                            WRITE(*,'(A)') "   pgf90 FigureGen.F90 -DNETCDF -I$TACC_NETCDF_INC -L$TACC_NETCDF_LIB -lnetcdf"
                            WRITE(*,'(A)') " "
                        ENDIF
                        
                        ERROR = .TRUE.
                        RETURN
#endif              
                    ELSE
                        WRITE(ContourFileFormat1,'(A)') "ASCII"
                    ENDIF
                ELSE
                    IF(INDEX(TempC,",").LE.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: IfPlotFilledContours = 2, then list two contour file names."
                        ENDIF
                    
                        ERROR = .TRUE.
                        RETURN
                    
                    ENDIF
                    ContourFile1 = TempC(1:INDEX(TempC,",")-1)
                    INQUIRE(FILE=TRIM(ContourFile1),EXIST=FileExists)
                    IF(.NOT.FileExists)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: The contour file does not exist. FigureGen is quitting."
                        ENDIF
                    
                        ERROR = .TRUE.
                        RETURN
                    ENDIF
                    IF(INDEX(ContourFile1,".nc").GT.0)THEN
#ifdef NETCDF
                        NC_Status = NF90_OPEN(ContourFile1,NF90_NOWRITE,NC_ID)
                        IF(NC_Status.EQ.NF90_NOERR)THEN
                            WRITE(ContourFileFormat1,'(A)') "NETCDF"
                            CALL Check(NF90_CLOSE(NC_ID))
                        ELSE
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: Contour file is not in NetCDF format."
                            ENDIF
                        
                            ERROR = .TRUE.
                            RETURN
                        
                        ENDIF
#else
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: FigureGen has not been compiled to be compatible"
                            WRITE(*,'(A)') "             with NetCDF files.  Please use something like:"
                            WRITE(*,'(A)') " "
                            WRITE(*,'(A)') "   pgf90 FigureGen.F90 -DNETCDF -I$TACC_NETCDF_INC -L$TACC_NETCDF_LIB -lnetcdf"
                            WRITE(*,'(A)') " "
                        ENDIF
                    
                        ERROR = .TRUE.
                        RETURN
#endif                    
                    ELSE
                        WRITE(ContourFileFormat1,'(A)') "ASCII"
                    ENDIF
                    ContourFile2 = TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC))
                    INQUIRE(FILE=TRIM(ContourFile2),EXIST=FileExists)
                    IF(.NOT.FileExists)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: The contour file does not exist. FigureGen is quitting."
                        ENDIF
                    
                        ERROR = .TRUE.
                        RETURN
                    
                    ENDIF
                    IF(INDEX(ContourFile2,".nc").GT.0)THEN
#ifdef NETCDF
                        NC_Status = NF90_OPEN(ContourFile2,NF90_NOWRITE,NC_ID)
                        IF(NC_Status.EQ.NF90_NOERR)THEN
                            WRITE(ContourFileFormat2,'(A)') "NETCDF"
                            CALL Check(NF90_CLOSE(NC_ID))
                        ELSE
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: Contour file is not in NetCDF format."
                            ENDIF
                        
                            ERROR = .TRUE.
                            RETURN
                        
                        ENDIF
#else
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: FigureGen has not been compiled to be compatible"
                            WRITE(*,'(A)') "             with NetCDF files.  Please use something like:"
                            WRITE(*,'(A)') " "
                            WRITE(*,'(A)') "   pgf90 FigureGen.F90 -DNETCDF -I$TACC_NETCDF_INC -L$TACC_NETCDF_LIB -lnetcdf"
                            WRITE(*,'(A)') " "
                        ENDIF
                    
                        ERROR = .TRUE.
                        RETURN
#endif                    
                    ELSE
                        WRITE(ContourFileFormat2,'(A)') "ASCII"
                    ENDIF
                ENDIF
 2222           CONTINUE
                READ(UNIT=11,FMT='(A)')   ContourFileType
                
                !...Check if we're doing a list of plots
                IF(TRIM(ContourFileType).EQ."ADCIRC-OUTPUT-LIST")THEN
                    OutputFileList = .TRUE.
                    ContourFileListFile = ContourFile1
                ELSE
                    OutputFileList = .FALSE.
                ENDIF

                IF(TRIM(ContourFileType).EQ."0")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileType = 0 and reset to ADCIRC-OUTPUT."
                    ENDIF
                    ContourFileType = "ADCIRC-OUTPUT"
                ENDIF
                IF(TRIM(ContourFileType).EQ."1")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileType = 1 and reset to ADCIRC-OUTPUT."
                    ENDIF
                    ContourFileType = "ADCIRC-OUTPUT"
                ENDIF
                IF(TRIM(ContourFileType).EQ."2")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileType = 2 and reset to GRID-BATH."
                    ENDIF
                    ContourFileType = "GRID-BATH"
                ENDIF
                IF(TRIM(ContourFileType).EQ."OUTPUT-FULL")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileType = OUTPUT-FULL and reset to ADCIRC-OUTPUT."
                    ENDIF
                    ContourFileType = "ADCIRC-OUTPUT"
                ENDIF
                IF(TRIM(ContourFileType).EQ."OUTPUT-SPARSE")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileType = OUTPUT-SPARSE and reset to ADCIRC-OUTPUT."
                    ENDIF
                    ContourFileType = "ADCIRC-OUTPUT"
                ENDIF
                IF(INDEX(ContourFileType,"GRID-DECOMP").GT.0)THEN
                    READ(UNIT=ContourFileType,FMT='(12X,I6)') NumSubDomains
                ELSE
                    NumSubDomains = 1
                ENDIF
                IF((INDEX(ContourFileType,"GRID-DECOMP").GT.0).AND.(IfPlotFilledContours.EQ.2))THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ContourFileType = GRID-DECOMP, but IfPlotFilledContours = 2."
                    ENDIF
                
                    ERROR = .TRUE.
                    RETURN
                
                ENDIF
                IF((INDEX(ContourFileType,"GRID-DECOMP").GT.0).AND.(IfPlotContourLines.EQ.2))THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ContourFileType = GRID-DECOMP, but IfPlotContourLines = 2."
                    ENDIF
                
                    ERROR = .TRUE.
                    RETURN
                
                ENDIF
                IF((TRIM(ContourFileType).EQ."GRID-SIZE").AND.(IfPlotFilledContours.EQ.2))THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ContourFileType = GRID-SIZE, but IfPlotFilledContours = 2."
                    ENDIF
                
                    ERROR = .TRUE.
                    RETURN
                
                ENDIF
                IF((TRIM(ContourFileType).EQ."GRID-SIZE").AND.(IfPlotContourLines.EQ.2))THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ContourFileType = GRID-SIZE, but IfPlotContourLines = 2."
                    ENDIF
                
                    ERROR = .TRUE.
                    RETURN
                
                ENDIF
                IF(TRIM(ContourFileType).EQ."HWM-CSV")THEN
                    IF(IfPlotFilledContours.GT.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileType = HWM-CSV, so IfPlotFilledContours reset to 0."
                        ENDIF
                        IfPlotFilledContours = 0
                    ENDIF
                    IF(IfPlotContourLines.GT.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "WARNING: ContourFileType = HWM-CSV, so IfPlotContourLines reset to 0."
                        ENDIF
                        IfPlotContourLines = 0
                    ENDIF
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
                IF(INDEX(Palette,"INTERVALS").LE.0)THEN
                    IF(INDEX(TempC,",").GT.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: Palette = RGB, SMS or CPT, then list only one file name."
                        ENDIF
                    
                        ERROR = .TRUE.
                        RETURN
                    
                    ENDIF
                    SMSPalette = TRIM(TempC)
                    INQUIRE(FILE=TRIM(SMSPalette),EXIST=FileExists)
                    IF((.NOT.FileExists).AND.((IfPlotFilledContours.GT.0).OR.(IfPlotContourLines.GT.0)))THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: The palette file does not exist. FigureGen is quitting."
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
                    ENDIF 
                ELSE
                    IF(INDEX(ContourFileType,"GRID-DECOMP").GT.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: Do not use the (SMS/CPT)+INTERVALS option " &
                                                    //" when plotting the grid decomposition."
                        ENDIF
                    
                        ERROR = .TRUE.
                        RETURN
                    
                    ENDIF
                    IF(INDEX(TempC,",").LE.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: Palette = (SMS/CPT)+INTERVALS, then list two file names."
                        ENDIF
                        
                        ERROR = .TRUE.
                        RETURN
                    
                    ENDIF
                    SMSPalette = TempC(1:INDEX(TempC,",")-1)
                    INQUIRE(FILE=TRIM(SMSPalette),EXIST=FileExists)
                    IF(.NOT.FileExists)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: The palette file does not exist. FigureGen is quitting."
                        ENDIF
                    
                        ERROR = .TRUE.
                        RETURN
                    
                    ENDIF 
                    DiffContoursFile = TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC))
                    INQUIRE(FILE=TRIM(DiffContoursFile),EXIST=FileExists)
                    IF(.NOT.FileExists)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: The intervals file does not exist. FigureGen is quitting."
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
                    ENDIF 
                ENDIF
                READ(UNIT=11,FMT='(A40)') ColorLines
                IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
                    IF(IfPlotGrid.EQ.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ColorLines = GRID-BATH, then IfPlotGrid = 1."
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
                    ENDIF
                ELSEIF(INDEX(ColorLines,"GRID-DECOMP").GT.0)THEN
                    IF(INDEX(ContourFileType,"GRID-DECOMP").LE.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ColorLines = GRID-DECOMP, then ContourFileType = GRID-DECOMP."
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
                    ENDIF
                    READ(UNIT=ColorLines,FMT='(12X,I6)') TempI
                    IF(TempI.NE.NumSubDomains)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: Number of sub-domains should match "// &
                                    "between ContourFileType and ColorLines."
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
                    ENDIF
                    IF(IfPlotGrid.EQ.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ColorLines = GRID-DECOMP, then IfPlotGrid = 1."
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
                    ENDIF
                ELSEIF((TRIM(ColorLines).EQ."GRID-SIZE").OR.(TRIM(ColorLines).EQ."MESH-SIZE"))THEN
                    IF(TRIM(ColorLines).EQ."MESH-SIZE")THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "WARNING: ColorLines = MESH-SIZE reset to GRID-SIZE."
                        ENDIF
                        ColorLines = "GRID-SIZE                                         "
                    ENDIF
                    IF(IfPlotGrid.EQ.0)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ColorLines = GRID-SIZE, then IfPlotGrid = 1."
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
                    ENDIF
                ELSEIF(TRIM(ColorLines).EQ."CONTOUR-LINES")THEN
        !           IF(IfPlotContourLines.EQ.0)THEN
        !               IF(MyRank.EQ.0)THEN
        !                   WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: ColorLines = CONTOUR-LINES, then IfPlotContourLines > 0."
        !               ENDIF
        !               ERROR = .TRUE.
        !               RETURN
        !           ENDIF
                ELSE
                    ColorLines = "DEFAULT"
                ENDIF
                
                READ(UNIT=11,FMT='(A50)') TempC
                IF(INDEX(Palette,"INTERVALS").LE.0)THEN
                    IF(TempC(1:4).EQ."FIND")THEN
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
                            ERROR = .TRUE.
                            RETURN
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

                IF(VersionNumber.GE.47)THEN

                    READ(UNIT=11,FMT='(A1)')  JunkC ! PARAMETERS FOR PARTICLES

                    READ(UNIT=11,FMT=*)       IfPlotParticles
                    READ(UNIT=11,FMT='(A50)') ParticleFile
                    IF(IfPlotParticles.GT.0)THEN
                        INQUIRE(FILE=TRIM(ParticleFile),EXIST=FileExists)
                        IF(.NOT.FileExists)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: Particle path file does not exist. FigureGen is quitting."
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
                        ENDIF
                        IF(INDEX(ParticleFile,".nc").GT.0)THEN
#ifdef NETCDF
                            NC_Status = NF90_OPEN(ParticleFile,NF90_NOWRITE,NC_ID)
                            IF(NC_Status.EQ.NF90_NOERR)THEN
                                WRITE(ParticleFileFormat,'(A)') "NETCDF"
                                CALL Check(NF90_CLOSE(NC_ID))
                            ELSE
                                IF(MyRank.EQ.0)THEN
                                    WRITE(*,'(A)') "FATAL ERROR: Particle file is not in NetCDF format."
                                ENDIF
                                ERROR = .TRUE.
                                RETURN
                            ENDIF
#else
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: FigureGen has not been compiled to be compatible"
                                WRITE(*,'(A)') "             with NetCDF files.  Please use something like:"
                                WRITE(*,'(A)') " "
                                WRITE(*,'(A)') "   pgf90 FigureGen.F90 -DNETCDF -I$TACC_NETCDF_INC -L$TACC_NETCDF_LIB -lnetcdf"
                                WRITE(*,'(A)') " "
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
#endif
                        ELSE
                            WRITE(ParticleFileFormat,'(A)') "ASCII"
                        ENDIF
                    ENDIF
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(TempC(1:1).EQ."0")THEN
                        IfPlotParticles = 0
                    ELSE
                        pth1: DO I=1,LEN_TRIM(TempC)
                            IF(TempC(I:I).EQ.",")THEN
                                ParticleSize = TempC(1:I-1)
                                EXIT pth1
                            ENDIF
                        ENDDO pth1
                        pth2: DO I=LEN_TRIM(ParticleSize)+2,LEN_TRIM(TempC)
                            IF(TempC(I:I).EQ.",")THEN
                                ParticlePattern = TempC(LEN_TRIM(ParticleSize)+2:I-1)
                                ParticleColor   = TempC(I+1:LEN_TRIM(TempC))
                                CALL LowercaseWord(ParticleColor)
                                
                                !...Cobell - Check for a palette file
                                IF(INDEX(ParticleColor,"cpt").GT.0)THEN
                                    UseParticlePalette = .TRUE.
                                    ParticlePalette    = TempC(I+5:LEN_TRIM(TempC))
                                ELSE
                                    IF(INDEX(ParticleColor,",").GT.0)THEN
                                        ParticleColor = ParticleColor(1:INDEX(ParticleColor,",")-1)
                                    ENDIF
                                ENDIF
                                    
                                EXIT pth2
                            ENDIF
                        ENDDO pth2
                    ENDIF

                ENDIF

                READ(UNIT=11,FMT='(A1)')  JunkC ! PARAMETERS FOR VECTORS

                READ(UNIT=11,FMT=*)       IfPlotVectors
                READ(UNIT=11,FMT='(A40)') VectorFile
                IF(IfPlotVectors.EQ.1)THEN
                    PlotVectorScale = .TRUE.
                    INQUIRE(FILE=TRIM(VectorFile),EXIST=FileExists)
                    IF(.NOT.FileExists)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: Vector file does not exist. FigureGen is quitting."
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
                    ENDIF
                    IF(INDEX(VectorFile,".nc").GT.0)THEN
#ifdef NETCDF
                        NC_Status = NF90_OPEN(VectorFile,NF90_NOWRITE,NC_ID)
                        IF(NC_Status.EQ.NF90_NOERR)THEN
                            WRITE(VectorFileFormat,'(A)') "NETCDF"
                            CALL Check(NF90_CLOSE(NC_ID))
                        ELSE
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: Vector file is not in NetCDF format."
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
                        ENDIF
#else
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "FATAL ERROR: FigureGen has not been compiled to be compatible"
                            WRITE(*,'(A)') "             with NetCDF files.  Please use something like:"
                            WRITE(*,'(A)') " "
                            WRITE(*,'(A)') "   pgf90 FigureGen.F90 -DNETCDF -I$TACC_NETCDF_INC -L$TACC_NETCDF_LIB -lnetcdf"
                            WRITE(*,'(A)') " "
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
#endif
                    ELSE
                        WRITE(VectorFileFormat,'(A)') "ASCII"
                    ENDIF
                ENDIF
                READ(UNIT=11,FMT='(A)')   VectorFileType
                IF(TRIM(VectorFileType).EQ."0")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "WARNING: VectorFileType = 0 and reset to ADCIRC-OUTPUT."
                    ENDIF
                    VectorFileType = "ADCIRC-OUTPUT"
                ENDIF
                IF(TRIM(VectorFileType).EQ."1")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "WARNING: VectorFileType = 1 and reset to ADCIRC-OUTPUT."
                    ENDIF
                    VectorFileType = "ADCIRC-OUTPUT"
                ENDIF
                IF(TRIM(VectorFileType).EQ."OUTPUT-FULL")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "WARNING: VectorFileType = OUTPUT-FULL and reset to ADCIRC-OUTPUT."
                    ENDIF
                    VectorFileType = "ADCIRC-OUTPUT"
                ENDIF
                IF(TRIM(VectorFileType).EQ."OUTPUT-SPARSE")THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "WARNING: VectorFileType = OUTPUT-SPARSE and reset to ADCIRC-OUTPUT."
                    ENDIF
                    VectorFileType = "ADCIRC-OUTPUT"
                ENDIF
                READ(UNIT=11,FMT=*)       VectorConversionFactor
                READ(UNIT=11,FMT='(A40)') VectorLabel
                READ(UNIT=11,FMT=*)       VectorMag
                IF(VersionNumber.LT.42)THEN
                    VectorUnits = "d"
                    READ(UNIT=11,FMT=*)       VectorSpacing
                ELSE
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(INDEX(TempC,",").LE.0)THEN
                        VectorUnits = "d"
                        READ(UNIT=TempC,FMT=*)   VectorSpacing
                    ELSE
                        TempI = LEN_TRIM(TempC)
                        VectorUnits = TempC(TempI:TempI)
                        TempC = TempC(1:TempI-2)
                        READ(UNIT=TempC,FMT=*)   VectorSpacing
                    ENDIF
                ENDIF
                IF(VectorUnits.EQ."d")THEN
                    VectorUnits = ""
                ELSEIF(VectorUnits.EQ."m")THEN
                    VectorUnits = "e"
                ELSEIF(VectorUnits.EQ."k")THEN
                    VectorUnits = "k"
                ELSE
                    VectorUnits = ""
                ENDIF
                IF(VersionNumber.LT.39)THEN
                    READ(UNIT=11,FMT=*)       VectorHeadLength
                    READ(UNIT=11,FMT=*)       VectorHeadWidth
                    READ(UNIT=11,FMT=*)       VectorTailWidth
                ELSE
                    READ(UNIT=11,FMT='(A50)') TempC
                    findheadlength: DO I=1,LEN_TRIM(TempC)
                        IF(TempC(I:I).EQ.",")THEN
                            READ(TempC(1:I-1),*) VectorHeadLength
                            Position = I
                            EXIT findheadlength
                        ENDIF
                    ENDDO findheadlength
                    findheadwidth: DO I=Position+1,LEN_TRIM(TempC)
                        IF(TempC(I:I).EQ.",")THEN
                            READ(TempC(Position+1:I-1),*) VectorHeadWidth
                            Position = I
                            EXIT findheadwidth
                        ENDIF
                    ENDDO findheadwidth
                    READ(TempC(Position+1:LEN_TRIM(TempC)),*) VectorTailWidth
                ENDIF
                READ(UNIT=11,FMT='(A50)') TempC
                IF(TempC(1:1).EQ."F")THEN
                    IF(IfPlotVectors.EQ.1)THEN
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
                IF(TempC(1:1).EQ."0")THEN
                    IfPlotBoundaries = 0
                ELSE
                    IF(VersionNumber.LT.26)THEN
                        BoundariesThickness = "2"
                        READ(BoundariesThickness,*) IfPlotBoundaries
                        BoundariesColor = "Brown"
                    ELSE
                        bdy: DO I=1,LEN_TRIM(TempC)
                            IF(TempC(I:I).EQ.",")THEN
                                BoundariesThickness = TempC(1:I-1)
                                READ(BoundariesThickness,*) IfPlotBoundaries
                                IF(VersionNumber.LT.42)THEN
                                    BoundariesThickness = "2"
                                ENDIF
                                BoundariesColor = TempC(I+1:LEN_TRIM(TempC))
                                EXIT bdy
                            ENDIF
                        ENDDO bdy
                    ENDIF
                ENDIF
                IF(VersionNumber.LT.41)THEN
                    IfPlotCoastline = 0
                ELSE
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(TempC(1:1).EQ."0")THEN
                        IfPlotCoastline = 0
                    ELSE
                        coast1: DO I=1,LEN_TRIM(TempC)
                            IF(TempC(I:I).EQ.",")THEN
                                CoastlineThickness = TempC(1:I-1)
                                READ(CoastlineThickness,*) IfPlotCoastline
                                IF(VersionNumber.LT.42)THEN
                                   CoastlineThickness = "2"
                                ENDIF
                                EXIT coast1
                            ENDIF
                        ENDDO coast1
                        coast2: DO I=3,LEN_TRIM(TempC)
                            IF(TempC(I:I).EQ.",")THEN
                                CoastlineColor = TempC(3:I-1)
                                EXIT coast2
                            ENDIF
                        ENDDO coast2
                        CALL CapitalizeWord(CoastlineColor)
                        IF(VersionNumber.LT.41)THEN
                            CoastlineRes = "f"
                            CoastlineWB = "1"
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "WARNING: Coastline will be added with internal commands; file will be ignored."
                            ENDIF
                        ELSE
                            TempC = TempC(4+LEN_TRIM(CoastlineColor):LEN_TRIM(TempC))
                            JunkC = TempC(1:1)
                            READ(JunkC,'(I1.1)') TempI
                            IF(TempI.EQ.1)THEN
                                CoastlineRes = "l"
                            ELSEIF(TempI.EQ.2)THEN
                                CoastlineRes = "i"
                            ELSEIF(TempI.EQ.3)THEN
                                CoastlineRes = "h"
                            ELSEIF(TempI.EQ.4)THEN
                                CoastlineRes = "f"
                            ELSE
                                CoastlineRes = "f"
                            ENDIF
                            JunkC = TempC(3:3)
                            READ(JunkC,'(I1.1)') TempI
                            IF(TempI.EQ.1)THEN
                                CoastlineWB = "1"
                            ELSEIF(TempI.EQ.2)THEN
                                CoastlineWB = "2"
                            ELSEIF(TempI.EQ.3)THEN
                                CoastlineWB = "3"
                            ELSEIF(TempI.EQ.4)THEN
                                CoastlineWB = "4"
                            ELSE
                                CoastlineWB = "4"
                            ENDIF
                            !WRITE(CoastlineThickness,*) TRIM(TempC(5:LEN_TRIM(TempC)))
                            CoastlineThickness = TRIM(TempC(5:LEN_TRIM(TempC)))
                        ENDIF
                    ENDIF
                ENDIF
                IF(VersionNumber.LT.26)THEN
                    IfPlotDotsLines = 0
                ELSE
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(TempC(1:1).NE."0")THEN
                        IF(TempC(1:1).EQ."1")THEN
                            IfPlotDotsLines = 1
                        ELSEIF(TempC(1:1).EQ."2")THEN
                            IfPlotDotsLines = 2
                        ELSEIF(TempC(1:1).EQ."3")THEN
                            IfPlotDotsLines = 3
                        ENDIF
                        lines1: DO I=3,LEN_TRIM(TempC)
                            IF(TempC(I:I).EQ.",")THEN
                                DotsLinesColor = TempC(3:I-1)
                            EXIT lines1
                            ENDIF
                        ENDDO lines1
                        TempC = TempC(3+LEN_TRIM(DotsLinesColor):LEN_TRIM(TempC))
                        lines2: DO I = 2, LEN_TRIM(TempC)
                            IF(TempC(I:I).EQ.",")THEN
                                DotsLinesFile=TempC(2:I-1)
                            EXIT lines2
                            ENDIF
                        ENDDO lines2
                        IF(IfPlotDotsLines.GE.2)THEN
                            DotsLinesThickness=TempC(3+LEN_TRIM(DotsLinesFile):LEN_TRIM(TempC))
                        ELSE
                            DotsLinesThickness="1"
                        ENDIF
                        INQUIRE(FILE=TRIM(DotsLinesFile),EXIST=FileExists)
                        IF(.NOT.FileExists)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: The dots/lines file does not exist. FigureGen is quitting."
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
                        ENDIF 
                    ELSE
                        IfPlotDotsLines = 0
                    ENDIF
                ENDIF
                IF(VersionNumber.LT.26)THEN
                    IfPlotLabels = 0
                ELSEIF(VersionNumber.LT.43)THEN
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(TempC(1:1).NE."0")THEN
                        IfPlotLabels = 2
                        LabelsSizeC = "14"
                        LabelsSize = 14
                        DO I=3,LEN_TRIM(TempC)
                            IF(TempC(I:I).EQ.",")THEN
                                LabelsColor = TempC(3:I-1)
                            ENDIF
                        ENDDO
                        LabelsFile = TempC(4+LEN_TRIM(LabelsColor):LEN_TRIM(TempC))
                        INQUIRE(FILE=TRIM(LabelsFile),EXIST=FileExists)
                        IF(.NOT.FileExists)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: The labels file does not exist. FigureGen is quitting."
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
                        ENDIF 
                    ELSE
                        IfPlotLabels = 0
                    ENDIF
                ELSE
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(TempC(1:1).NE."0")THEN
                        IF(TempC(1:1).EQ."1")THEN
                            IfPlotLabels = 1
                        ELSEIF(TempC(1:1).EQ."2")THEN
                            IfPlotLabels = 2
                        ENDIF
                        LabelsFile = TempC(3:LEN_TRIM(TempC))
                        INQUIRE(FILE=TRIM(LabelsFile),EXIST=FileExists)
                        IF(.NOT.FileExists)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: The labels file does not exist. FigureGen is quitting."
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
                        ENDIF 
                    ELSE
                        IfPlotLabels = 0
                    ENDIF
                ENDIF
                IF(VersionNumber.LT.27)THEN
                    IfPlotLogo = 0
                ELSE
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(TempC(1:1).EQ."1")THEN
                        IfPlotLogo = 1
                        innerlogo1: DO I=3,LEN_TRIM(TempC)
                           IF(TempC(I:I).EQ.",")THEN
                               LogoLocation = TempC(3:I-1)
                               EXIT innerlogo1
                           ENDIF
                        ENDDO innerlogo1
                        innerlogo2: DO I=4+LEN_TRIM(LogoLocation),LEN_TRIM(TempC)
                           IF(TempC(I:I).EQ.",")THEN
                               LogoWidth = TempC(4+LEN_TRIM(LogoLocation):I-1)
                               EXIT innerlogo2
                           ENDIF
                        ENDDO innerlogo2
                        LogoFile = TempC(5+LEN_TRIM(LogoLocation)+LEN_TRIM(LogoWidth):LEN_TRIM(TempC))
                        INQUIRE(FILE=TRIM(LogoFile),EXIST=FileExists)
                        IF(.NOT.FileExists)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: The logo file does not exist. FigureGen is quitting."
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
                        ENDIF 
                    ENDIF
                ENDIF
                IF(VersionNumber.LT.47)THEN
                    IfPlotBackgroundImages = 0
                ELSE
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(TempC(1:1).EQ."1")THEN
                        IfPlotBackgroundImages = 1
                        BackgroundImagesFile = TempC(3:LEN_TRIM(TempC))
                        INQUIRE(FILE=TRIM(BackgroundImagesFile),EXIST=FileExists)
                        IF(.NOT.FileExists)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: The background images file does not exist. FigureGen is quitting."
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
                        ENDIF
                    ELSEIF(TempC(1:1).EQ."2")THEN
                        IfPlotBackgroundImages = 2
                        BackgroundImagesFile = TempC(3:LEN_TRIM(TempC))
                        INQUIRE(FILE=TRIM(BackgroundImagesFile),EXIST=FileExists)
                        IF(.NOT.FileExists)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: The background images file does not exist. FigureGen is quitting."
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
                        ENDIF
                    ENDIF
                ENDIF
                READ(UNIT=11,FMT=*)       Width
                IF((ABS(LongW-LongE).LT.ABS(LatN-LatS)).AND.(Width.GT.4.))THEN
        !           Width = 4.D0
                    IF(MyRank.EQ.0)THEN
                        WRITE(*,'(A)') "WARNING: Plot must orient correctly on page.  PlotWidth was adjusted downward"// &
                                       " so plot will fit on the page."
                    ENDIF
                ENDIF
                READ(UNIT=11,FMT=*)       Buffer
                READ(UNIT=11,FMT=*)       BorderIncrementMinor
                READ(UNIT=11,FMT=*)       BorderIncrementMajor
                IF(VersionNumber.LT.28)THEN
                    ImageTrimFlag = 1
                ELSE
                    READ(UNIT=11,FMT=*)   ImageTrimFlag
                ENDIF
                READ(UNIT=11,FMT=*)       Resolution
                IF(VersionNumber.LT.41)THEN
                    READ(UNIT=11,FMT=*)       SmallJPGWidth
                ELSE
                    SmallJPGWidth = 1200
                ENDIF
                IF(VersionNumber.LT.41)THEN
                    DoJPG = 1
                    DoPNG = 0
                    DoPDF = 0
                    DoBMP = 0
                    DoEPS = 0
                    DoTIFF = 0
                    DoPS = 1
                ELSE
                    DoJPG = 0
                    DoPNG = 0
                    DoPDF = 0
                    DoBMP = 0
                    DoEPS = 0
                    DoTIFF = 0
                    DoPS = 0
                    READ(UNIT=11,FMT='(A50)') TempC
                    P = 1
                    typesearch_outer: DO
                        IF(INDEX(TempC,",").GT.0)THEN
                            typesearch_inner: DO I=1,LEN_TRIM(TempC)
                                IF(TempC(I:I).EQ.",")THEN
                                    TempC2 = TempC(1:I-1)
                                    CALL CapitalizeWord(TempC2)
                                    IF(TRIM(TempC2).EQ."JPG")THEN
                                        DoJPG = 1
                                    ELSEIF(TRIM(TempC2).EQ."PNG")THEN
                                        DoPNG = 1
                                    ELSEIF(TRIM(TempC2).EQ."PDF")THEN
                                        DoPDF = 1
                                    ELSEIF(TRIM(TempC2).EQ."BMP")THEN
                                        DoBMP = 1
                                    ELSEIF(TRIM(TempC2).EQ."EPS")THEN
                                        DoEPS = 1
                                    ELSEIF(TRIM(TempC2).EQ."TIFF")THEN
                                        DoTIFF = 1
                                    ELSEIF(TRIM(TempC2).EQ."PS")THEN
                                        DoPS = 1
                                    ELSE
                                        WRITE(*,'(A,A,A)') 'WARNING: Raster file format "',TRIM(TempC2),'" not recognized.'
                                    ENDIF
                                    P = I + 1
                                    TempC = TempC(I+1:LEN_TRIM(TempC))
                                    EXIT typesearch_inner
                                ENDIF
                            ENDDO typesearch_inner
                        ELSE
                            CALL CapitalizeWord(TempC2)
                            IF(TRIM(TempC).EQ."JPG")THEN
                                DoJPG = 1
                            ELSEIF(TRIM(TempC).EQ."PNG")THEN
                                DoPNG = 1
                            ELSEIF(TRIM(TempC).EQ."PDF")THEN
                                DoPDF = 1
                            ELSEIF(TRIM(TempC).EQ."BMP")THEN
                                DoBMP = 1
                            ELSEIF(TRIM(TempC).EQ."EPS")THEN
                                DoEPS = 1
                            ELSEIF(TRIM(TempC).EQ."TIFF")THEN
                                DoTIFF = 1
                            ELSEIF(TRIM(TempC2).EQ."PS")THEN
                                DoPS = 1
                            ELSE
                                WRITE(*,'(A,A,A)') 'WARNING: Raster file format "',TRIM(TempC),'" not recognized.'
                            ENDIF
                            EXIT typesearch_outer
                        ENDIF
                    ENDDO typesearch_outer
                    IF((DoJPG.EQ.0).AND.(DoPNG.EQ.0).AND.(DoPDF.EQ.0).AND. &
                       (DoEPS.EQ.0).AND.(DoBMP.EQ.0).AND.(DoTIFF.EQ.0).AND.(DoPS.EQ.0))THEN
                            DoJPG = 1
                    ENDIF
                ENDIF
                IF(VersionNumber.LT.30)THEN
                    IfGoogle = 0
                ELSEIF(VersionNumber.LT.38)THEN
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(TempC(1:1).EQ."1")THEN
                        IfGoogle = 1
                        innergoogle11: DO I=3,LEN_TRIM(TempC)
                           IF(TempC(I:I).EQ.",")THEN
                              GoogleTransparencyC = TempC(3:I-1)
                              READ(GoogleTransparencyC,*) GoogleTransparency
                              EXIT innergoogle11
                           ENDIF
                        ENDDO innergoogle11
                        NumLayers = 1
                        JunkI = 3 + LEN_TRIM(GoogleTransparencyC)
                        IF(TempC(JunkI:JunkI).EQ.",")THEN
                           GoogleDateC = TempC(4+LEN_TRIM(GoogleTransparencyC):LEN_TRIM(TempC))
                           READ(UNIT=GoogleDateC,FMT='(I4,I2,I2,I2,I2,I2)') &
                                GoogleYear, GoogleMonth, GoogleDay, GoogleHour, GoogleMin, GoogleSec
                        ENDIF
                    ELSE
                        IfGoogle = 0
                    ENDIF
                ELSE
                    READ(UNIT=11,FMT='(A50)') TempC
                    IF(TempC(1:1).EQ."1")THEN
                        IfGoogle = 1
                        innergoogle1: DO I=3,LEN_TRIM(TempC)
                           IF(TempC(I:I).EQ.",")THEN
                              GoogleTransparencyC = TempC(3:I-1)
                              READ(GoogleTransparencyC,*) GoogleTransparency
                              EXIT innergoogle1
                           ENDIF
                        ENDDO innergoogle1
                        NumLayers = 0
                        innergoogle2: DO I=4+LEN_TRIM(GoogleTransparencyC),LEN_TRIM(TempC)
                           IF(TempC(I:I).EQ.",")THEN
                              NumLayersC = TempC(4+LEN_TRIM(GoogleTransparencyC):I-1)
                              READ(NumLayersC,*) NumLayers
                              IF((NumLayers.LT.1).OR.(NumLayers.GT.99))THEN
                                 IF(MyRank.EQ.0)THEN
                                    WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: The number of Google layers must be in the range 1-99."
                                 ENDIF
                                ERROR = .TRUE.
                                RETURN
                              ENDIF
                              EXIT innergoogle2
                           ENDIF
                        ENDDO innergoogle2
                        IF(NumLayers.EQ.0)THEN
                           NumLayersC = TempC(4+LEN_TRIM(GoogleTransparencyC):LEN_TRIM(TempC))
                           READ(NumLayersC,*) NumLayers
                           IF((NumLayers.LT.1).OR.(NumLayers.GT.99))THEN
                              IF(MyRank.EQ.0)THEN
                                 WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: The number of Google layers must be in the range 1-99."
                              ENDIF
                            ERROR = .TRUE.
                            RETURN
                           ENDIF
                        ENDIF
                        JunkI = 4 + LEN_TRIM(GoogleTransparencyC) + LEN_TRIM(NumLayersC)
                        IF(TempC(JunkI:JunkI).EQ.",")THEN
                           GoogleDateC = TempC(5+LEN_TRIM(GoogleTransparencyC)+LEN_TRIM(NumLayersC):LEN_TRIM(TempC))
                           READ(UNIT=GoogleDateC,FMT='(I4,I2,I2,I2,I2,I2)') &
                                GoogleYear, GoogleMonth, GoogleDay, GoogleHour, GoogleMin, GoogleSec
                        ENDIF
                    ELSE
                        IfGoogle = 0
                    ENDIF
                ENDIF
                IF(IfGoogle.EQ.1)THEN
                    IF(IfAddPlotLabel.EQ.1)THEN
                        IfAddPlotLabel = 0
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "WARNING: Plot label will not be included in Google KMZ file."
                        ENDIF
                    ENDIF
                    IF((IfPlotVectors.GT.0).AND.(FindVectorScale.EQ.1))THEN
                        FindVectorScale = 0
                        VectorScaleMag = 0.0
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "WARNING: Vector scale will not be included in Google KMZ file."
                        ENDIF
                    ENDIF
                    IF((IfPlotLogo.EQ.1).AND.(INDEX(LogoFile,".eps").GT.0))THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: Logo for Google KMZ cannot be in EPS format."
                        ENDIF
                        ERROR = .TRUE.
                        RETURN
                    ENDIF
                    IF(Resolution.GT.200)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "WARNING: Resolution for Google KMZ images reset to 200."
                        ENDIF
                        Resolution = 200
                    ENDIF
                    IF(DoJPG.EQ.1)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "WARNING: JPG format requested, but Google KMZ only supports PNG."
                        ENDIF
                        DoJPG = 0
                        DoPNG = 1
                    ENDIF
                    IF(DoBMP.EQ.1)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "WARNING: BMP format requested, but Google KMZ only supports PNG."
                        ENDIF
                        DoBMP = 0
                        DoPNG = 1
                    ENDIF
                    IF(DoTIFF.EQ.1)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "WARNING: TIFF format requested, but Google KMZ only supports PNG."
                        ENDIF
                        DoTIFF = 0
                        DoPNG = 1
                    ENDIF
                    IF(DoEPS.EQ.1)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "WARNING: EPS format requested, but Google KMZ only supports PNG."
                        ENDIF
                        DoEPS = 0
                        DoPNG = 1
                    ENDIF
                    IF(DoPDF.EQ.1)THEN
                        IF(MyRank.EQ.0)THEN
                            WRITE(*,'(A)') "WARNING: PDF format requested, but Google KMZ only supports PNG."
                        ENDIF
                        DoPDF = 0
                        DoPNG = 1
                    ENDIF
                ENDIF
                IF(VersionNumber.LT.41)THEN
                    IfGIS = 0
                ELSE
                    READ(UNIT=11,FMT='(A)') TempC
                    IF(INDEX(TempC,",").LE.0)THEN
                        READ(UNIT=TempC,FMT=*) IfGIS
                        IF(IfGIS.GT.0)THEN
                            NumLayers = 0
                        ENDIF
                    ELSE
                        READ(UNIT=TempC(1:INDEX(TempC,",")-1),FMT=*) IfGIS
                        IF(IfGIS.GT.0)THEN
                            READ(UNIT=TempC(INDEX(TempC,",")+1:LEN_TRIM(TempC)),FMT=*) NumLayers
                        ENDIF
                    ENDIF
                    IF(IfGIS.GT.0)THEN
                        IF(IfPlotLogo.GT.0)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "WARNING: Cannot plot logo on a georeferenced image."
                            ENDIF
                            IfPlotLogo = 0
                        ENDIF
                        IF(IfAddPlotLabel.GT.0)THEN
                           IF(MyRank.EQ.0)THEN
                               WRITE(*,'(A)') "WARNING: Cannot add plot label to georeferenced image."
                           ENDIF
                           IfAddPlotLabel = 0
                        ENDIF
                        IF(IfAddTimeBar.GT.0)THEN
                           IF(MyRank.EQ.0)THEN
                               WRITE(*,'(A)') "WARNING: Cannot add time bar to georeferenced image."
                           ENDIF
                           IfAddTimeBar = 0
                        ENDIF
                        IF(DoPNG.EQ.1)THEN
                           IF(MyRank.EQ.0)THEN
                               WRITE(*,'(A)') "WARNING: PNG format requested, but georeferencing only supports JPG."
                           ENDIF
                           DoPNG = 0
                           DoJPG = 1
                        ENDIF
                        IF(DoBMP.EQ.1)THEN
                           IF(MyRank.EQ.0)THEN
                               WRITE(*,'(A)') "WARNING: BMP format requested, but georeferencing only supports JPG."
                           ENDIF
                           DoBMP = 0
                           DoJPG = 1
                        ENDIF
                        IF(DoTIFF.EQ.1)THEN
                           IF(MyRank.EQ.0)THEN
                               WRITE(*,'(A)') "WARNING: TIFF format requested, but georeferencing only supports JPG."
                           ENDIF
                           DoTIFF = 0
                           DoJPG = 1
                        ENDIF
                        IF(DoEPS.EQ.1)THEN
                           IF(MyRank.EQ.0)THEN
                               WRITE(*,'(A)') "WARNING: EPS format requested, but georeferencing only supports JPG."
                           ENDIF
                           DoEPS = 0
                           DoJPG = 1
                        ENDIF
                        IF(DoPDF.EQ.1)THEN
                           IF(MyRank.EQ.0)THEN
                               WRITE(*,'(A)') "WARNING: PDF format requested, but georeferencing only supports JPG."
                           ENDIF
                           DoPDF = 0
                           DoJPG = 1
                        ENDIF
                        IF(IfGoogle.GT.0)THEN
                            IF(MyRank.EQ.0)THEN
                                WRITE(*,'(A)') "FATAL ERROR: Cannot create Google KMZ and georeferenced image on same run."
                            ENDIF
                            ERROR = .TRUE.
                            RETURN
                        ENDIF
                    ENDIF
                ENDIF
                IF((IfPlotLogo.EQ.1).AND.((INDEX(LogoFile,".eps").LE.0).AND.(INDEX(LogoFile,".txt").LE.0)).AND. &
                   ((IfGoogle.EQ.0).AND.(IfGIS.EQ.0)))THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(UNIT=*,FMT='(A)') "FATAL ERROR: Logo must be in EPS format."
                    ENDIF
                    ERROR = .TRUE.
                    RETURN
                ENDIF
                READ(UNIT=11,FMT=*)       NumRecords
               IF(((TRIM(ContourFileType).EQ."GRID-BATH").OR.      &
                   (INDEX(ContourFileType,"GRID-DECOMP").GT.0).OR. &
                   (INDEX(ContourFileType,"ADCIRC-OUTPUT-LIST").GT.0).OR.&
                   (TRIM(ContourFileType).EQ."GRID-SIZE")).AND.    &
                   (IfPlotVectors.EQ.0).AND.(IfPlotParticles.EQ.0))THEN
                   IF(MyRank.EQ.0)THEN
                       WRITE(*,'(A)') "WARNING: Record information ignored at end of input file; "// &
                                      "one record will be generated from grid file."
                   ENDIF
                   NumRecords = 1
                   ALLOCATE(RecordsList(1:NumRecords))
                   DO I=1,NumRecords
                       RecordsList(I) = 1
                   ENDDO
               ELSEIF(INDEX(ContourFileType,"13-").GT.0)THEN
                    IF(INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0)THEN
                        IF(TRIM(ContourFileType).EQ."13-WIND-REDUCTION")THEN
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
                                READ(UNIT=ContourFileType,FMT='(18X,I2)') RecordsList(I)
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
                ENDIF
                IF(NumRecords.GT.0.AND..NOT.ALLOCATED(RecordsList))THEN
                    ALLOCATE(RecordsList(1:NumRecords))
                    DO I=1,NumRecords
                        READ(UNIT=11,FMT=*) RecordsList(I)
                    ENDDO
                ELSEIF(NumRecords.LT.0.AND..NOT.ALLOCATED(RecordsList))THEN
                    RecordsInc = ABS(NumRecords)
                    READ(UNIT=11,FMT=*) RecordsBegin
                    READ(UNIT=11,FMT=*) RecordsEnd
                        
                        !...Added - Zach
                        ! Don't know if this functionality is available by another means, but I just added this for my own
                        ! purposes to say grab the last 10 timesnaps before a blowup by setting in the input file:
                        ! RecordsInc = -1
                        ! RecordsBegin = -10
                        ! RecordsEnd = 0
                        
                    IF((RecordsBegin.LT.0).OR.(RecordsEnd.LE.0))THEN
                        NumRecs63 = Count63(TRIM(ContourFile1),TRIM(ContourFileFormat1))
                    ENDIF
                       
                    IF(RecordsBegin.LE.0)THEN
                        RecordsBegin = NumRecs63 + RecordsBegin
                    ENDIF
                        
                    IF(RecordsEnd.LE.0)THEN
                        RecordsEnd = NumRecs63 + RecordsEnd
                    ENDIF  
                       
                    NumRecords = (RecordsEnd - RecordsBegin + 1)/RecordsInc
                    ALLOCATE(RecordsList(1:NumRecords))
                    DO I=1,NumRecords
                        RecordsList(I) = RecordsBegin + (I-1)*RecordsInc
                    ENDDO
                ENDIF

                CLOSE(UNIT=11,STATUS="KEEP")

                IF(Verbose.GE.3)THEN
                    WRITE(*,'(A,I4.4,A)') "Core ",MyRank," read the input file."
                ENDIF

        END SUBROUTINE



#ifdef SLOWREAD
        SUBROUTINE ReadNodeVals(UnitNumber, FileNameLen, FileName, FileType, JunkI, UVal, VVal)

                IMPLICIT NONE

                CHARACTER(LEN=50),INTENT(IN) :: FileName
                CHARACTER(LEN=50)            :: FileNameDisplay 
                CHARACTER(LEN=250)           :: Line

                INTEGER,INTENT(IN)           :: FileNameLen
                INTEGER,INTENT(IN)           :: FileType
                INTEGER                      :: I
                INTEGER,INTENT(OUT)          :: JunkI
                INTEGER,INTENT(IN)           :: UnitNumber

                REAL,INTENT(OUT)             :: UVal
                REAL,INTENT(OUT)             :: VVal

                WRITE(UNIT=FileNameDisplay(1:FileNameLen),FMT='(A)') FileName(1:FileNameLen)
                DO I=FileNameLen+1,50
                    WRITE(FileNameDisplay(I:I),'(A)') " "
                ENDDO

                outer: DO

                    READ(UNIT=UnitNumber,FMT='(A)',END=8002,ERR=8002) Line
                    IF(FileType.EQ.0)THEN
                       READ(UNIT=Line,FMT=*,END=8003,ERR=8003) JunkI
                    ELSEIF(FileType.EQ.1)THEN
                       READ(UNIT=Line,FMT=*,END=8003,ERR=8003) JunkI, UVal
                    ELSEIF(FileType.EQ.2)THEN
                       READ(UNIT=Line,FMT=*,END=8003,ERR=8003) JunkI, UVal, VVal
                    ENDIF
                    EXIT outer

8003                CONTINUE
                    WRITE(*,'(A)') "FATAL ERROR: Something went wrong while reading from: "//TRIM(FileNameDisplay)
                    WRITE(*,'(A)') "             The last line read was: ",TRIM(Line)
                    STOP

8002                CONTINUE
                    IF(Verbose.GE.4)THEN
                        WRITE(*,'(A,I4.4,A,A,A)') "Core ",MyRank," is trying to read from the end of ",TRIM(FileNameDisplay),"."
                    ENDIF
                    BACKSPACE(UnitNumber)
                    CALL SLEEP(60)

                ENDDO outer

        END SUBROUTINE
#endif



        SUBROUTINE ReadTimeStamp(UnitNumber, FileNameLen, FileName, TimeReal, TimeReal2, NonDefaultNodes, DefaultValue)

                IMPLICIT NONE

                INTRINSIC                    :: TRIM

                CHARACTER(LEN=*),INTENT(IN)  :: FileName
                CHARACTER(LEN=50)            :: FileNameDisplay 
                CHARACTER(LEN=100)           :: Line

                INTEGER,INTENT(IN)           :: FileNameLen
                INTEGER                      :: I
                INTEGER,INTENT(INOUT)        :: NonDefaultNodes
                INTEGER,INTENT(IN)           :: UnitNumber

                REAL,INTENT(OUT)             :: DefaultValue
                REAL(8),INTENT(OUT)          :: TimeReal
                REAL(8),INTENT(OUT)          :: TimeReal2

                WRITE(UNIT=FileNameDisplay(1:FileNameLen),FMT='(A)') FileName(1:FileNameLen)
                DO I=FileNameLen+1,50
                    WRITE(FileNameDisplay(I:I),'(A)') " "
                ENDDO

                outer: DO

                    READ(UNIT=UnitNumber,FMT='(A)',END=8001,ERR=8001) Line
                    READ(UNIT=Line,FMT=*,END=9917,ERR=9917) TimeReal, TimeReal2, NonDefaultNodes, DefaultValue
                    GOTO 9918
9917                READ(UNIT=Line,FMT=*) TimeReal, TimeReal2
                    DefaultValue = 0.0
9918                EXIT outer

8001                CONTINUE
                    IF(Verbose.GE.4)THEN
                        WRITE(*,'(A,I4.4,A,A,A)') "Core ",MyRank," is trying to read from the end of ",TRIM(FileNameDisplay),"."
                    ENDIF
                    BACKSPACE(UnitNumber)
                    CALL SLEEP(60)

                ENDDO outer

        END SUBROUTINE



        SUBROUTINE RemoveIfExists(Ext)

                IMPLICIT NONE

                INTRINSIC                       :: LEN
                INTRINSIC                       :: LEN_TRIM
                INTRINSIC                       :: TRIM

                CHARACTER(LEN=1000),ALLOCATABLE :: DirList(:)
                CHARACTER(LEN=*)                :: Ext
                CHARACTER(LEN=10)               :: Extension
                CHARACTER(LEN=120),ALLOCATABLE  :: FilesList(:)
                CHARACTER(LEN=1)                :: JunkC

                INTEGER                         :: DirLength
                INTEGER                         :: ExtLen
                INTEGER                         :: I
                INTEGER                         :: J
                INTEGER                         :: Length
                INTEGER                         :: LineLength
                INTEGER                         :: NameBegin
                INTEGER                         :: NameEnd
                INTEGER                         :: NumFiles
                INTEGER                         :: NumFound

                Extension = TRIM(Ext)

                CALL SYSTEM("ls -l > "//TRIM(TempPath)//"DirList.tmp")

                OPEN(UNIT=38,FILE=TRIM(TempPath)//"DirList.tmp",ACTION="READ")

                DirLength = 0

                DO
                    READ(UNIT=38,FMT=*,END=9925) JunkC
                    DirLength = DirLength + 1
                ENDDO

9925            CLOSE(UNIT=38,STATUS="KEEP")

                ALLOCATE(DirList(1:DirLength))

                OPEN(UNIT=38,FILE=TRIM(TempPath)//"DirList.tmp",ACTION="READ")

                NumFiles = 0

                ExtLen = LEN_TRIM(Extension)-1

                DO I=1,DirLength

                    READ(UNIT=38,FMT='(A1000)') DirList(I)

                    inner1: DO J=LEN(DirList(I)),1,-1
                        IF(DirList(I)(J:J).NE." ")THEN
                           LineLength = J
                           EXIT inner1
                        ENDIF
                    ENDDO inner1

                    IF(DirList(I)((LineLength-ExtLen):LineLength).EQ.Extension)THEN
                        NumFiles = NumFiles + 1
                    ENDIF

                ENDDO

                IF(RemoveFiles.EQ.0)THEN
                    CLOSE(UNIT=38,STATUS="KEEP")
                ELSEIF(RemoveFiles.EQ.1)THEN
                    CLOSE(UNIT=38,STATUS="DELETE")
                ENDIF

                ALLOCATE(FilesList(1:NumFiles))

                NumFound = 0

                DO I=1,DirLength

                    inner2: DO J=LEN(DirList(I)),1,-1
                        IF(DirList(I)(J:J).NE." ")THEN
                            LineLength = J
                            EXIT inner2
                        ENDIF
                    ENDDO inner2

                    IF(DirList(I)((LineLength-ExtLen):LineLength).EQ.Extension)THEN

                        NumFound = NumFound + 1

                        inner3: DO J=LineLength,1,-1
                            IF(DirList(I)(J:J).NE." ")THEN
                                NameEnd = J
                                EXIT inner3
                            ENDIF
                        ENDDO inner3

                        inner4: DO J=NameEnd,1,-1
                            IF(DirList(I)(J:J).EQ." ")THEN
                                NameBegin = J+1
                                EXIT inner4
                            ENDIF
                        ENDDO inner4

                        FilesList(NumFound) = DirList(I)(NameBegin:NameEnd)

                    ENDIF

                ENDDO

                DO I=1,NumFiles
                    CALL SYSTEM("rm "//TRIM(FilesList(I)))
                ENDDO

        END SUBROUTINE

        SUBROUTINE WritePSImage(Record,IL1,IL2,IL3)

                

                IMPLICIT NONE

                INTRINSIC         :: ABS
                INTRINSIC         :: LEN_TRIM
                INTRINSIC         :: NINT
                INTRINSIC         :: REAL
                INTRINSIC         :: TRIM

                CHARACTER(LEN=50) :: BackgroundImage
                CHARACTER(LEN=50) :: BackgroundImageRef
                CHARACTER(LEN=15) :: BorderIncrementMajorC
                CHARACTER(LEN=15) :: BorderIncrementMinorC
                CHARACTER(LEN=15) :: CentralMeridianC
                CHARACTER(LEN=15) :: ContourLabelEveryC
                CHARACTER(LEN=15) :: ContourLabelMinDistC
                CHARACTER(LEN=8)  :: ContourLabelSizeC
                CHARACTER(LEN=15) :: ContourScaleYC
                CHARACTER(LEN=100):: EdgeFileName
                CHARACTER(LEN=100):: GeoRefFileEPS
                CHARACTER(LEN=100):: GeoRefFilePNG
                CHARACTER(LEN=8)  :: GeoRefHeightC
                CHARACTER(LEN=8)  :: GeoRefOffsetXC
                CHARACTER(LEN=8)  :: GeoRefOffsetYC
                CHARACTER(LEN=8)  :: GeoRefWidthC
                CHARACTER(LEN=15) :: HeightC
                CHARACTER(LEN=60) :: JunkC
                CHARACTER(LEN=40) :: LabelAlign
                CHARACTER(LEN=40) :: LabelC
                CHARACTER(LEN=40) :: LabelLat
                CHARACTER(LEN=40) :: LabelLon
                CHARACTER(LEN=40) :: LabelsSizeC
                CHARACTER(LEN=40) :: LabelX
                CHARACTER(LEN=40) :: LabelY
                CHARACTER(LEN=500):: Line
                CHARACTER(LEN=50) :: MemLimitLine
                CHARACTER(LEN=8)  :: MinDistC
                CHARACTER(LEN=15) :: PlotLabelXAdjustC
                CHARACTER(LEN=15) :: PlotLabelYAdjustC
                CHARACTER(LEN=50) :: ProjectionC
                CHARACTER(LEN=8)  :: ResolutionC
                CHARACTER(LEN=15) :: ScaleLabelEveryC
                CHARACTER(LEN=15) :: ScaleHeightC
                CHARACTER(LEN=15) :: ScaleWidthC
                CHARACTER(LEN=8)  :: SideBarTriangleHeightC
                CHARACTER(LEN=15) :: SideBarXC
                CHARACTER(LEN=8)  :: SmallJPGWidthC
                CHARACTER(LEN=50) :: TempLogoFile
                CHARACTER(LEN=15) :: TimeScaleYC
                CHARACTER(LEN=15) :: TimeScaleTextYC
                CHARACTER(LEN=15) :: VectorHeadLengthC
                CHARACTER(LEN=15) :: VectorHeadWidthC
                CHARACTER(LEN=15) :: VectorMagC
                CHARACTER(LEN=15) :: VectorScaleXC
                CHARACTER(LEN=8)  :: VectorScaleYC
                CHARACTER(LEN=15) :: VectorTailWidthC
                CHARACTER(LEN=15) :: WidthC
                CHARACTER(LEN=15) :: XMax
                CHARACTER(LEN=15) :: XMin
                CHARACTER(LEN=15) :: YMax
                CHARACTER(LEN=15) :: YMin
                CHARACTER(LEN=2)  :: AMPMS

                INTEGER           :: DoScale
                INTEGER           :: Found
                INTEGER           :: I
                INTEGER           :: IfStarted
                INTEGER           :: IL1
                INTEGER           :: IL2
                INTEGER           :: IL3
                INTEGER           :: IN
                INTEGER           :: IO
                INTEGER           :: J
                INTEGER           :: JunkI
                INTEGER           :: Position
                INTEGER           :: NumLabels
                INTEGER           :: NumLogos
                INTEGER           :: Record

                LOGICAL           :: AddBackgroundImage
                LOGICAL           :: WroteBorder

                REAL              :: CentralMeridian
                REAL              :: ContourScaleY
                REAL              :: GeoRefDX
                REAL              :: GeoRefDY
                REAL              :: GeoRefHeight
                REAL              :: GeoRefOffsetX
                REAL              :: GeoRefOffsetY
                REAL              :: GeoRefWidth
                REAL              :: GeoRefX
                REAL              :: GeoRefY
                REAL              :: Height
                REAL              :: HWMError
                REAL              :: HWMLat
                REAL              :: HWMLon
                REAL              :: JunkR
                REAL              :: LatNLocal
                REAL              :: LatSLocal
                REAL              :: LongELocal
                REAL              :: LongETemp
                REAL              :: LongWLocal
                REAL              :: LongWTemp
                REAL              :: MinDist
                REAL              :: PlotLabelXAdjust
                REAL              :: PlotLabelYAdjust
                REAL              :: SideBarTriangleHeight
                REAL              :: SideBarX
                REAL              :: TimeScaleY
                REAL              :: TimeScaleTextY
                REAL              :: VectorScaleX
                REAL              :: VectorScaleY

                IfStarted = 0

                WroteBorder = .FALSE.

                LongWLocal = LongW + REAL(IL2-1)/REAL(2**(IL1-1))*(LongE-LongW)
                LongELocal = LongW + REAL(IL2  )/REAL(2**(IL1-1))*(LongE-LongW)
                LatSLocal  = LatS  + REAL(IL3-1)/REAL(2**(IL1-1))*(LatN -LatS )
                LatNLocal  = LatS  + REAL(IL3  )/REAL(2**(IL1-1))*(LatN -LatS )

                LongWTemp =  360.0
                LongETemp = -360.0
                DO IN=1,NumNodesLocal
                    IF((X(IN).LT.LongWTemp).AND.((LatNLocal .GE.Y(IN)).AND.(LatSLocal .LE.Y(IN))))THEN
                        LongWTemp = X(IN)
                    ENDIF
                    IF((X(IN).GT.LongETemp).AND.((LatNLocal .GE.Y(IN)).AND.(LatSLocal .LE.Y(IN))))THEN
                        LongETemp = X(IN)
                    ENDIF
                ENDDO
                IF(LongWTemp.LT.LongW) LongWTemp = LongWLocal
                IF(LongETemp.GT.LongE) LongETemp = LongELocal
                CentralMeridian = 0.5 * (LongWTemp + LongETemp)

                IF(TRIM(ContourFileType).EQ."HWM-CSV")THEN
                    SideBarTriangleHeight = 0.5
                ELSE
                    SideBarTriangleHeight = 0.0
                ENDIF

                WRITE(UNIT=SideBarTriangleHeightC,FMT='(F3.1)') SideBarTriangleHeight

 1236           FORMAT(F15.8)
                WRITE(UNIT=CentralMeridianC,FMT=1236) CentralMeridian
                WRITE(UNIT=WidthC,FMT=1236) Width
                WRITE(UNIT=XMax  ,FMT=1236) LongELocal
                WRITE(UNIT=XMin  ,FMT=1236) LongWLocal
                WRITE(UNIT=YMax  ,FMT=1236) LatNLocal
                WRITE(UNIT=YMin  ,FMT=1236) LatSLocal

                ProjectionC = ""
!               IF(IfGoogle.EQ.0)THEN
!                  ProjectionC = "-JM"//TRIM(ADJUSTL(WidthC))//"i"
!               ELSE
!                   ProjectionC = "-JQ"//TRIM(CentralMeridianC)//"/"//TRIM(ADJUSTL(WidthC))//"i"
!               ENDIF
                ProjectionC = "-JQ"//TRIM(ADJUSTL(CentralMeridianC))//"/"//TRIM(ADJUSTL(WidthC))//"i"

                WRITE(UNIT=TempMapFile1,FMT='(A,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)') TRIM(TempPath), &
                                            "mapproject.", Record,"-",IL1,"-",IL2,"-",IL3,".inp"
                WRITE(UNIT=TempMapFile2,FMT='(A,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)') TRIM(TempPath), &
                                            "mapproject.", Record,"-",IL1,"-",IL2,"-",IL3,".out"
                OPEN(UNIT=29,FILE=TRIM(TempMapFile1),ACTION="WRITE")
                WRITE(UNIT=29,FMT=*) LongELocal,LatNLocal
                CLOSE(UNIT=29,STATUS="KEEP")
                IF(IfGoogle.EQ.0)THEN        
                    CALL SYSTEM(TRIM(Path)//"mapproject "//TRIM(TempMapFile1)                 &
                                //" -Di "//TRIM(ProjectionC)//" -R"//TRIM(ADJUSTL(XMin))//"/" &
                                //TRIM(ADJUSTL(XMax))//"/"//TRIM(ADJUSTL(YMin))//"/"          &
                                //TRIM(ADJUSTL(YMax))//" > "//TRIM(TempMapFile2))                        
                ELSEIF(IfGoogle.EQ.1)THEN
                    CALL SYSTEM(TRIM(Path)//"mapproject "//TRIM(TempMapFile1)               &
                                //" -Di "//TRIM(ProjectionC)                                &
                                //" -R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))             &
                                //" > "//TRIM(TempMapFile2))
                ENDIF
                OPEN(UNIT=30,FILE=TRIM(TempMapFile2),ACTION="READ")
                READ(UNIT=30,FMT=*) JunkR, Height
                CLOSE(UNIT=30,STATUS="KEEP")
                IF(RemoveFiles.EQ.1)THEN
                    OPEN(UNIT=29,FILE=TRIM(TempMapFile1),ACTION="WRITE")
                    CLOSE(UNIT=29,STATUS="DELETE")
                    OPEN(UNIT=30,FILE=TRIM(TempMapFile2),ACTION="WRITE")
                    CLOSE(UNIT=30,STATUS="DELETE")
                ENDIF

                IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0).AND..NOT.OutputFileList)THEN
                    WRITE(UNIT=PlotName,FMT='(A,I4.4)') TRIM(AlphaLabel),Record
                ELSEIF(OutputFileList)THEN
                    WRITE(UNIT=PlotName,FMT='(A,A)') TRIM(AlphaLabel),TRIM(ContourFileTag(Record))
                ELSE
                    WRITE(UNIT=PlotName,FMT='(A,I4.4,A,I2.2,A,I2.2,A,I2.2)') TRIM(AlphaLabel),Record,"-",IL1,"-",IL2,"-",IL3
                ENDIF

                WRITE(UNIT=HeightC,FMT=1236) Height

                IF((IfAddTimeBar.EQ.0).AND.(IfPlotVectors.EQ.0))THEN 
                    ScaleHeight = Height - 0.3 - SideBarTriangleHeight
                ELSEIF(((IfAddTimeBar.EQ.1).OR.(IfAddTimeBar.EQ.2).OR.IfAddTimeBar.EQ.3).AND.(IfPlotVectors.EQ.0))THEN
                    ScaleHeight = Height - 1.2 - SideBarTriangleHeight
                ELSEIF((IfAddTimeBar.EQ.0).AND.((IfPlotVectors.EQ.1).OR.(IfPlotVectors.EQ.3)))THEN
                    ScaleHeight = Height - 0.8 - SideBarTriangleHeight
                ELSE
                    ScaleHeight = Height - 1.6 - SideBarTriangleHeight
                ENDIF
                IF(.NOT.PlotVectorScale)ScaleHeight = ScaleHeight + 0.4d0
                WRITE(UNIT=ScaleHeightC,FMT=1236) ScaleHeight

                WRITE(UNIT=BorderIncrementMajorC,FMT=1236) BorderIncrementMajor
                WRITE(UNIT=BorderIncrementMinorC,FMT=1236) BorderIncrementMinor
                WRITE(UNIT=ContourLabelEveryC   ,FMT=1236) ContourLabelEvery
                WRITE(UNIT=ContourLabelMinDistC ,FMT=1236) ContourLabelMinDist

                WRITE(UNIT=ContourLabelSizeC,FMT='(I4.4)') ContourLabelSize

                ContourScaleY = 0.5 * ScaleHeight + 0.5*SideBarTriangleHeight
                WRITE(UNIT=ContourScaleYC,FMT=1236) ContourScaleY

                IF(OptimizeContours.EQ.1)THEN
                    MinDist = ABS(LongWLocal-LongELocal)/2000.
                    WRITE(MinDistC,'(F8.6)') MinDist
                ENDIF

                WRITE(UNIT=ResolutionC,FMT='(I4.4)') Resolution

                PlotLabelXAdjust = 0.5 * Width
                WRITE(UNIT=PlotLabelXAdjustC,FMT=1236) PlotLabelXAdjust

                PlotLabelYAdjust = Height + Buffer
                WRITE(UNIT=PlotLabelYAdjustC,FMT=1236) PlotLabelYAdjust

                WRITE(UNIT=ScaleLabelEveryC,FMT=1236) ScaleLabelEvery
                WRITE(UNIT=ScaleWidthC     ,FMT=1236) ScaleWidth

                SideBarX = Width + Buffer
                WRITE(UNIT=SideBarXC,FMT=1236) SideBarX

                WRITE(UNIT=SmallJPGWidthC,FMT='(I4.4)') SmallJPGWidth

                TimeScaleY = Height - 0.35
                WRITE(UNIT=TimeScaleYC,FMT=1236) TimeScaleY

                TimeScaleTextY = Height
                WRITE(UNIT=TimeScaleTextYC,FMT=1236) TimeScaleTextY

                WRITE(UNIT=VectorHeadLengthC,FMT=1236) VectorHeadLength
                WRITE(UNIT=VectorHeadWidthC ,FMT=1236) VectorHeadWidth
                WRITE(UNIT=VectorMagC       ,FMT=1236) VectorMag

                VectorScaleX = Width + Buffer
                WRITE(UNIT=VectorScaleXC,FMT=1236) VectorScaleX

                WRITE(UNIT=VectorTailWidthC,FMT=1236) VectorTailWidth

                !....Setup Keep Status

                IF(((IfPlotFilledContours.GE.1).OR.(TRIM(ColorLines).NE."DEFAULT")).AND. &
                   (INDEX(ContourFileType,"GRID-DECOMP").LE.0).AND.(IfGIS.EQ.0))THEN
                        DoScale = 1
                ENDIF

                IF((IfPlotLogo.EQ.1).AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                    IF(VersionNumber.GE.43)THEN
                        IF(INDEX(LogoFile,".txt").GT.0)THEN
                            KeepOpen(1:14) = 0
                            OPEN(UNIT=26,FILE=TRIM(LogoFile),ACTION="READ")
                            READ(26,*) NumLogos
                            DO IO=1,NumLogos
                                READ(26,'(A)') Line
                                READ(Line,*) JunkI
                                IF(JunkI.EQ.Record)THEN
                                    KeepOpen(1:14) = 1
                                ENDIF
                            ENDDO
                            CLOSE(UNIT=26,STATUS="KEEP")
                        ELSE
                            KeepOpen(1:14) = 1
                        ENDIF
                    ELSE
                        KeepOpen(1:14) = 1
                    ENDIF
                ENDIF
                IF((IfAddTimeBar.GT.0).AND.(IfGoogle.EQ.0))THEN
                    KeepOpen(1:13) = 1
                ELSEIF(IfAddPlotLabel.GT.0)THEN
                    KeepOpen(1:12) = 1
                ELSEIF(IfPlotVectors.GT.0)THEN
                    KeepOpen(1:11) = 1
                ELSEIF(IfPlotLabels.GT.0)THEN
                    KeepOpen(1:10) = 1
                ELSEIF((IfGoogle.GT.0).OR.(IfGIS.GT.0))THEN
                    KeepOpen(1:9) = 1
                ELSEIF(TRIM(ContourFileType).EQ."HWM-CSV")THEN
                    KeepOpen(1:8) = 1
                ELSEIF(IfPlotDotsLines.GT.0)THEN
                    KeepOpen(1:7) = 1
                ELSEIF(IfPlotParticles.GT.0)THEN
                    KeepOpen(1:6) = 1
                ELSEIF(IfPlotCoastline.GT.0)THEN
                    KeepOpen(1:5) = 1
                ELSEIF(IfPlotBoundaries.GT.0)THEN
                    KeepOpen(1:4) = 1
                ELSEIF(DoScale.EQ.1)THEN
                    KeepOpen(1:3) = 1
                ELSEIF(IfPlotContourLines.GT.0)THEN
                    KeepOpen(1:2) = 1
                ELSEIF(IfPlotGrid.GT.0)THEN
                    KeepOpen(1:1) = 1
                ELSEIF(IfPlotFilledContours.GT.0)THEN
                    !Not Used
                ELSE
                    IF(MyRank.EQ.0) THEN
                        WRITE(*,*) "FATAL ERROR: No plotting options selected."
                    ENDIF
#ifdef CMPI
                    CALL MPI_FINALIZE(IERR)
#endif
                    STOP
                ENDIF

                IF(IfPlotBackgroundImages.EQ.1.OR.IfPlotBackgroundImages.EQ.2)THEN

                    AddBackgroundImage = .FALSE.
                    OPEN(UNIT=42,FILE=TRIM(BackgroundImagesFile),ACTION='READ')
                    DO
                        READ(42,'(A)',END=11311,ERR=11311) Line
                        innerbackground2: DO I=1,LEN_TRIM(Line)
                            IF(Line(I:I).EQ.",")THEN
                                READ(Line(1:I-1),*) J
                                Position = I+1
                                EXIT innerbackground2
                            ENDIF
                        ENDDO innerbackground2
                        IF(J.EQ.0.OR.J.EQ.Record)THEN
                            AddBackgroundImage = .TRUE.
                            innerbackground3: DO I=Position,LEN_TRIM(Line)
                                IF(Line(I:I).EQ.",")THEN
                                    BackgroundImage = Line(Position:I-1)
                                    Position = I+1
                                    EXIT innerbackground3
                                ENDIF
                            ENDDO innerbackground3
                            BackgroundImageRef = Line(Position:LEN_TRIM(Line))
                            GOTO 11311
                        ENDIF
                    ENDDO
11311               CONTINUE
                    CLOSE(UNIT=42,STATUS='KEEP')

                    IF(AddBackgroundImage)THEN

                        OPEN(UNIT=39,FILE=TRIM(BackgroundImageRef),ACTION='READ')
                        READ(39,*)     GeoRefDX
                        READ(39,'(A)') JunkC
                        READ(39,'(A)') JunkC
                        READ(39,*)     GeoRefDY
                        READ(39,*)     GeoRefX
                        READ(39,*)     GeoRefY
                        CLOSE(UNIT=39,STATUS='KEEP')

                        GeoRefWidth   = ABS(LongWLocal-LongELocal)/ABS(GeoRefDX)
                        GeoRefHeight  = ABS(LatNLocal -LatSLocal )/ABS(GeoRefDY)
                        GeoRefOffsetX = (LongWLocal - GeoRefX)/GeoRefDX
                        GeoRefOffsetY = (LatNLocal  - GeoRefY)/GeoRefDY
                        WRITE(GeoRefWidthC  ,'(I5.5)') NINT(GeoRefWidth)
                        WRITE(GeoRefHeightC ,'(I5.5)') NINT(GeoRefHeight)
                        WRITE(GeoRefOffsetXC,'(I5.5)') NINT(GeoRefOffsetX) + 9999
                        WRITE(GeoRefOffsetYC,'(I5.5)') NINT(GeoRefOffsetY) + 9999 + 5
                        WRITE(GeoRefFilePNG,'(A,I4.4,A)') TRIM(BackgroundImage)//".",Record,".png"
                        WRITE(GeoRefFileEPS,'(A,I4.4,A)') TRIM(BackgroundImage)//".",Record,".eps"

                        Line = ""
                        Line = TRIM(Line)//"convert"
                        Line = TRIM(Line)//" "//"-bordercolor White"
                        Line = TRIM(Line)//" "//"-border "//"9999"//"x"//"9999"
                        Line = TRIM(Line)//" "//"-crop "//TRIM(GeoRefWidthC)//"x"//TRIM(GeoRefHeightC) &
                                              //"+"//TRIM(GeoRefOffsetXC)//"+"//TRIM(GeoRefOffsetYC)
                        Line = TRIM(Line)//" "//TRIM(BackgroundImage)
                        Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(GeoRefFilePNG)
                        CALL SYSTEM(TRIM(Line))

                        Line = ""
                        Line = TRIM(Line)//"convert"
                        Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(GeoRefFilePNG)
                        Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(GeoRefFileEPS)
                        CALL SYSTEM(TRIM(Line))

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"psimage"
                        Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(GeoRefFileEPS)
                        Line = TRIM(Line)//" "//"-W"//TRIM(ADJUSTL(WidthC))//"i/"//TRIM(ADJUSTL(HeightC))//"i"
                        Line = TRIM(Line)//" "//"-C0/0/BL"
                        Line = TRIM(Line)//" "//"-K"
                        Line = TRIM(Line)//" "//">"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))
                        IfStarted = 1

                    ENDIF

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the background images for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the background images for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(IfPlotFilledContours.GE.1)THEN

                    IF(OptimizeContours.EQ.0)THEN

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"pscontour"
                        Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(ContourXYZFile)
                        Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"ContourPalette.cpt"
                        Line = TRIM(Line)//" "//TRIM(ProjectionC)
                        Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                    //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                        IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WroteBorder = .TRUE.
                            Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                               "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                        ENDIF
                        Line = TRIM(Line)//" "//"-I"
                        IF(TRIM(ContourFileType).EQ."OWI-PRESS".OR.TRIM(ContourFileType).EQ."OWI-WIND")THEN
                            !Line = TRIM(Line)//" "//"-T"//TRIM(TempPath)//TRIM(ContourFile1)//".tri"
                            !...Let GMT triangulate on its own
                        ELSE    
                            Line = TRIM(Line)//" "//"-T"//TRIM(TempPath)//TRIM(Fort14File)//".tri"
                        ENDIF    
                        IF(KeepOpen(1).EQ.1)THEN
                            Line = TRIM(Line)//" "//"-K"
                        ENDIF
                        IF(IfStarted.EQ.0)THEN
                            Line = TRIM(Line)//" "//">"
                            IfStarted = 1
                        ELSE
                            Line = TRIM(Line)//" "//"-O >>"
                        ENDIF
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))

                    ELSEIF(OptimizeContours.EQ.1)THEN

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"triangulate"
                        Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(ContourXYZFile)
                        Line = TRIM(Line)//" "//"-G"//TRIM(TempPath)//TRIM(ContourXYZFile)//".grd"
                        Line = TRIM(Line)//" "//"-I"//TRIM(MinDistC)
                        Line = TRIM(Line)//" "//TRIM(ProjectionC)
                        Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                    //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                        Line = TRIM(Line)//" "//"> /dev/null"
                        CALL SYSTEM(TRIM(Line))

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"grdimage"
                        Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(ContourXYZFile)//".grd"
                        Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"ContourPalette.cpt"
                        Line = TRIM(Line)//" "//TRIM(ProjectionC)
                        IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WroteBorder = .TRUE.
                            Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                               "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                        ENDIF
                        Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                    //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                        IF(KeepOpen(1).EQ.1)THEN
                            Line = TRIM(Line)//" "//"-K"
                        ENDIF
                        IF(IfStarted.EQ.0)THEN
                            Line = TRIM(Line)//" "//">"
                            IfStarted = 1
                        ELSE
                            Line = TRIM(Line)//" "//"-O >>"
                        ENDIF
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))

                    ENDIF

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the filled contours for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the filled contours for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(IfPlotGrid.EQ.1)THEN

                    DO IN=1,NumEdgeFiles

                        WRITE(UNIT=EdgeFileName,FMT='(A,I3.3,A)') TRIM(TempPath)//TRIM(Fort14File)//".edges.",IN,".xy"

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"psxy"
                        Line = TRIM(Line)//" "//TRIM(EdgeFileName)
                        Line = TRIM(Line)//" "//TRIM(ProjectionC)
                        Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                    //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                        IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WroteBorder = .TRUE.
                            Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                               "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                        ENDIF
                        IF(INDEX(ColorLines,"GRID").GT.0)THEN
                            Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"ContourPalette.cpt"
                        ENDIF
                        Line = TRIM(Line)//" "//"-m"
                        IF(TRIM(ColorLines).EQ."GRID-BATH")THEN
                            Line = TRIM(Line)//" "//"-W+"
                        ELSEIF(INDEX(ColorLines,"GRID-DECOMP").GT.0)THEN
                            Line = TRIM(Line)//" "//"-W+"
                        ELSEIF(TRIM(ColorLines).EQ."GRID-SIZE")THEN
                            Line = TRIM(Line)//" "//"-W+"
                        ELSE
                            Line = TRIM(Line)//" "//"-W1,Black"
                        ENDIF
                        IF(IN.NE.NumEdgeFiles)THEN
                            Line = TRIM(Line)//" "//"-K"
                        ELSEIF(KeepOpen(2).EQ.1)THEN
                            Line = TRIM(Line)//" "//"-K"
                        ENDIF
                        IF(IfStarted.EQ.0)THEN
                            Line = TRIM(Line)//" "//">"
                            IfStarted = 1
                        ELSE
                            Line = TRIM(Line)//" "//"-O >>"
                        ENDIF
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))

                    ENDDO

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the underlying mesh for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the underlying mesh for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(IfPlotContourLines.GE.1)THEN

                    IF(OptimizeContours.EQ.0)THEN

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"pscontour"
                        Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(ContourXYZFile)
                        Line = TRIM(Line)//" "//TRIM(ProjectionC)
                        Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                    //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                        IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WroteBorder = .TRUE.
                            Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                               "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                        ENDIF
                        Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"LabelPalette.cpt"
                        IF(ContourLabelEvery.LT.0.0000001)THEN
                            Line = TRIM(Line)//" "//"-A-"
                        ELSE
                            Line = TRIM(Line)//" "//"-A+a"//TRIM(ContourLabelRotation)//"+s"//TRIM(ContourLabelSizeC)
                            Line = TRIM(Line)//" "//"-Gd"//TRIM(ADJUSTL(ContourLabelEveryC))// &
                                                    "i+r"//TRIM(ADJUSTL(ContourLabelMinDistC))//"i"
                        ENDIF
                        IF(TRIM(ColorLines).EQ."CONTOUR-LINES")THEN
                            Line = TRIM(Line)//" "//"-W+"
                        ELSE
                            Line = TRIM(Line)//" "//"-W"
                        ENDIF
                        IF(TRIM(ContourFileType).EQ."OWI-PRESS".OR.TRIM(ContourFileType).EQ."OWI-WIND")THEN
                            !...Lets use GMT triangulation
                        ELSE
                            Line = TRIM(Line)//" "//"-T"//TRIM(TempPath)//TRIM(Fort14File)//".tri"
                        ENDIF    
                        IF(KeepOpen(3).EQ.1)THEN
                            Line = TRIM(Line)//" "//"-K"
                        ENDIF
                        IF(IfStarted.EQ.0)THEN
                            Line = TRIM(Line)//" "//">"
                            IfStarted = 1
                        ELSE
                            Line = TRIM(Line)//" "//"-O >>"
                        ENDIF
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))

                    ELSEIF(OptimizeContours.EQ.1)THEN

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"grdcontour"
                        Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(ContourXYZFile)//".grd"
                        Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"LabelPalette.cpt"
                        Line = TRIM(Line)//" "//TRIM(ProjectionC)
                        IF(ContourLabelEvery.LT.0.0000001)THEN
                            Line = TRIM(Line)//" "//"-A-"
                        ELSE
                            Line = TRIM(Line)//" "//"-A+a"//TRIM(ContourLabelRotation)//"+s"//TRIM(ContourLabelSizeC)
                            Line = TRIM(Line)//" "//"-Gd"//TRIM(ADJUSTL(ContourLabelEveryC))// &
                                                    "i+r"//TRIM(ADJUSTL(ContourLabelMinDistC))//"i"
                        ENDIF
                        IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WroteBorder = .TRUE.
                            Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                               "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                        ENDIF
                        Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                    //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                        IF(TRIM(ColorLines).EQ."CONTOUR-LINES")THEN
                            Line = TRIM(Line)//" "//"-W+"
                        ELSE
                            Line = TRIM(Line)//" "//"-W"
                        ENDIF
                        IF(KeepOpen(3).EQ.1)THEN
                            Line = TRIM(Line)//" "//"-K"
                        ENDIF
                        IF(IfStarted.EQ.0)THEN
                            Line = TRIM(Line)//" "//">"
                            IfStarted = 1
                        ELSE
                            Line = TRIM(Line)//" "//"-O >>"
                        ENDIF
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))

                    ENDIF

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the contour lines for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the contour lines for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(((IfPlotFilledContours.GE.1).OR.((IfPlotContourLines.GE.1).AND.(INDEX(ColorLines,"CONTOUR").GT.0)).OR. &
                   ((IfPlotGrid.GT.0).AND.(INDEX(ColorLines,"GRID").GT.0)).OR. &
                   (TRIM(ContourFileType).EQ."HWM-CSV")).AND.(INDEX(ColorLines,"GRID-DECOMP").LE.0).AND.(IL1.EQ.1).AND.   &
                   UseParticlePalette.EQV..FALSE.)THEN

                    Line = ""
#ifdef CBARLIMIT
                    Line = TRIM(Line)//TRIM(Path)//"psscale -E"
#else
                    Line = TRIM(Line)//TRIM(Path)//"psscale"
#endif
                    Line = TRIM(Line)//" "//"-D"//TRIM(ADJUSTL(SideBarXC))//"i/"//TRIM(ADJUSTL(ContourScaleYC))//"i/" &
                                     //TRIM(ADJUSTL(ScaleHeightC))//"i/"//TRIM(ADJUSTL(ScaleWidthC))//"i"
                    Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"ScalePalette.cpt"
                    IF(INDEX(Palette,"INTERVALS").LE.0)THEN
                        Line = TRIM(Line)//" "//"-B"//TRIM(ADJUSTL(ScaleLabelEveryC))//"::/:"//""""//TRIM(ContourUnits)//""""//":"
                    ELSEIF(INDEX(Palette,"INTERVALS").GT.0)THEN
                        Line = TRIM(Line)//" "//"-B"//"::/:"//""""//TRIM(ContourUnits)//""""//":"
                        Line = TRIM(Line)//" "//"-L"
                    ENDIF
                    IF(TRIM(ContourFileType).EQ."HWM-CSV")THEN
                        Line = TRIM(Line)//" "//"-E"//TRIM(SideBarTriangleHeightC)
                    ENDIF
                    IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                        IF(KeepOpen(4).EQ.1)THEN
                            Line = TRIM(Line)//" "//"-K"
                        ENDIF
                        IF(IfStarted.EQ.0)THEN
                            Line = TRIM(Line)//" "//">"
                            IfStarted = 1
                        ELSE
                            Line = TRIM(Line)//" "//"-O >>"
                        ENDIF
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                    ELSE
                        Line = TRIM(Line)//" "//">"
                        Line = TRIM(Line)//" "//"Scale.ps"
                    ENDIF
                    CALL SYSTEM(TRIM(Line))

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).OR.(IfGIS.GT.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the scale for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the scale for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(IfPlotBoundaries.GE.1)THEN

                    Line = ""
                    Line = TRIM(Line)//TRIM(Path)//"psxy"
                    Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(Fort14File)//".bnd.xy"
                    Line = TRIM(Line)//" "//TRIM(ProjectionC)
                    Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                    IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                        WroteBorder = .TRUE.
                        Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                           "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                           "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                           "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                    ENDIF
                    Line = TRIM(Line)//" "//"-m"
                    Line = TRIM(Line)//" "//"-W"//TRIM(BoundariesThickness)//","//TRIM(BoundariesColor)
                    IF(KeepOpen(5).EQ.1)THEN
                        Line = TRIM(Line)//" "//"-K"
                    ENDIF
                    IF(IfStarted.EQ.0)THEN
                        Line = TRIM(Line)//" "//">"
                        IfStarted = 1
                    ELSE
                        Line = TRIM(Line)//" "//"-O >>"
                    ENDIF
                    Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the levee/road boundaries for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the levee/road boundaries for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(IfPlotCoastline.GT.0)THEN

                    Line = ""
                    Line = TRIM(Line)//TRIM(Path)//"pscoast"
                    Line = TRIM(Line)//" "//TRIM(ProjectionC)
                    IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                        WroteBorder = .TRUE.
                        Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                           "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                           "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                           "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                    ENDIF
                    Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                    Line = TRIM(Line)//" "//"-W"//TRIM(CoastlineThickness)//"p/"//TRIM(CoastlineColor)
                    Line = TRIM(Line)//" "//"-D"//CoastlineRes//"+"
                    Line = TRIM(Line)//" "//"-A0/0/"//CoastlineWB
                    IF(KeepOpen(6).EQ.1) THEN
                        Line = TRIM(Line)//" "//"-K"
                    ENDIF
                    IF(IfStarted.EQ.0)THEN
                        Line = TRIM(Line)//" "//">"
                        IfStarted = 1
                    ELSE
                        Line = TRIM(Line)//" "//"-O >>"
                    ENDIF
                    Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the coastline for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the coastline for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(IfPlotParticles.GT.0)THEN

                    WRITE(UNIT=ParticleXYZFile,FMT='(A,A,I4.4,A)') TRIM(ParticleFile),".",Record,".xy"
                    Line = ""
                    Line = TRIM(Line)//TRIM(Path)//"psxy"
                    Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(ParticleXYZFile)
                    Line = TRIM(Line)//" "//TRIM(ProjectionC)
                    Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                    IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                        WroteBorder = .TRUE.
                        Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                           "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                           "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                           "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                    ENDIF
                    IF(UseParticlePalette)THEN
                        Line = TRIM(Line)//" -C"//TRIM(ParticlePalette)
                    ELSE
                        IF(ParticlePattern(1:1).EQ."0")THEN
                            Line = TRIM(Line)//" "//"-G"//TRIM(ParticleColor)
                        ELSE
                            Line = TRIM(Line)//" "//"-Gp100/"//TRIM(ParticlePattern)//":F"//TRIM(ParticleColor)//"B-"
                        ENDIF
                    ENDIF    
                    IF(KeepOpen(7).EQ.1.OR.UseParticlePalette) THEN
                        Line = TRIM(Line)//" "//"-K"
                    ENDIF
                    Line = TRIM(Line)//" "//"-Sc"//TRIM(ParticleSize)//"p"
                    IF(IfStarted.EQ.0)THEN
                        Line = TRIM(Line)//" "//">"
                        IfStarted = 1
                    ELSE
                        Line = TRIM(Line)//" "//"-O >>"
                    ENDIF
                    Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF(UseParticlePalette)THEN
                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"psscale"
                        Line = TRIM(Line)//" "//"-D"//TRIM(ADJUSTL(SideBarXC))//"i/"//TRIM(ADJUSTL(ContourScaleYC))//"i/" &
                                         //TRIM(ADJUSTL(ScaleHeightC))//"i/"//TRIM(ADJUSTL(ScaleWidthC))//"i"
                        Line = TRIM(Line)//" "//"-C"//TRIM(ParticlePalette)
                        Line = TRIM(Line)//" "//"-B"//TRIM(ADJUSTL(ScaleLabelEveryC))//"::/:"//""""//TRIM(ContourUnits)//""""//":"
                        IF(KeepOpen(7).EQ.1)THEN
                            Line = TRIM(Line)//" "//"-K"
                        ENDIF
                        IF(IfStarted.EQ.0)THEN
                            Line = TRIM(Line)//" "//">"
                            IfStarted = 1
                        ELSE
                            Line = TRIM(Line)//" "//"-O >>"
                        ENDIF
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))
                    ENDIF

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the particles for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the particles for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(IfPlotDotsLines.GT.0)THEN

                    Line = ""
                    Line = TRIM(Line)//TRIM(Path)//"psxy"
                    Line = TRIM(Line)//" "//TRIM(DotsLinesFile)
                    Line = TRIM(Line)//" "//TRIM(ProjectionC)
                    Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                    IF(IfPlotDotsLines.EQ.1)THEN
                        Line = TRIM(Line)//" "//"-G"//TRIM(DotsLinesColor)
                        Line = TRIM(Line)//" "//"-Sc2p"
                    ELSEIF(IfPlotDotsLines.EQ.2)THEN
                        Line = TRIM(Line)//" "//"-W"//TRIM(DotsLinesThickness)//"p/"//TRIM(DotsLinesColor)
                    ELSEIF(IfPlotDotsLines.EQ.3)THEN
                        Line = TRIM(Line)//" "//"-W"//TRIM(DotsLinesThickness)//"p/"//TRIM(DotsLinesColor)
                        Line = TRIM(Line)//" "//"-m'>'"
                    ENDIF
                    Line = TRIM(Line)//" "//"-O"
                    IF(KeepOpen(8).EQ.1) THEN
                        Line = TRIM(Line)//" "//"-K"
                    ENDIF
                    Line = TRIM(Line)//" >> "//TRIM(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the dots/lines for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the dots/lines for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(TRIM(ContourFileType).EQ."HWM-CSV")THEN

                    OPEN(UNIT=37,FILE=TRIM(ContourFile1),ACTION="READ")
                   
                    I = 0 
                    DO
                        READ(UNIT=37,FMT='(A)',END=999) JunkC
                        I = I + 1
                    ENDDO
999                 CONTINUE
                    REWIND(37)

                    DO J = 1,I

                        READ(37,*,ERR=9724,END=9724) HWMLon,HWMLat,JunkR,JunkR,JunkR,HWMError
                        HWMError = HWMError * ContourConversionFactor

                        WRITE(UNIT=TempLabelsFile,FMT='(A,A,I4.4,A)') TRIM(TempPath), "Labels.", Record, ".txt"
                        OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                        WRITE(UNIT=33,FMT='(F11.6,2X,F11.6,2X,F11.6)') HWMLon, HWMLat, HWMError
                        CLOSE(UNIT=33,STATUS="KEEP")

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"psxy"
                        Line = TRIM(Line)//" "//TRIM(TempLabelsFile)
                        Line = TRIM(Line)//" "//TRIM(ProjectionC)
                        Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                    //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                        IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WroteBorder = .TRUE.
                            Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                               "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                               "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                        ENDIF
                        Line = TRIM(Line)//" "//"-GBlack"
                        Line = TRIM(Line)//" "//"-Sc12p"
                        Line = TRIM(Line)//" "//"-K"
                        Line = TRIM(Line)//" "//"-O >>"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"psxy"
                        Line = TRIM(Line)//" "//TRIM(TempLabelsFile)
                        Line = TRIM(Line)//" "//"-C"//TRIM(TempPath)//"ContourPalette.cpt"
                        Line = TRIM(Line)//" "//TRIM(ProjectionC)
                        Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                    //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                        Line = TRIM(Line)//" "//"-Sc10p"
                        IF((KeepOpen(9).EQ.1).OR.(J.LT.I))THEN
                            Line = TRIM(Line)//" "//"-K"
                        ENDIF
                        Line = TRIM(Line)//" "//"-O >>"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))

                        IF(RemoveFiles.EQ.1)THEN
                            OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                            CLOSE(UNIT=33,STATUS="DELETE")
                        ENDIF

                    ENDDO

 9724               CONTINUE

                    CLOSE(UNIT=37,STATUS="KEEP")

                ENDIF

                IF((IfGoogle.EQ.1).OR.(IfGIS.GT.0))THEN

                    WRITE(UNIT=TempLabelsFile,FMT='(A,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)') &
                            TRIM(TempPath),"GoogleExtents.",Record,"-",IL1,"-",IL2,"-",IL3,".txt"
                    OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                    WRITE(UNIT=33,FMT='(A,2X,A)') TRIM(ADJUSTL(XMin)), TRIM(ADJUSTL(YMin))
                    WRITE(UNIT=33,FMT='(A,2X,A)') TRIM(ADJUSTL(XMin)), TRIM(ADJUSTL(YMax))
                    WRITE(UNIT=33,FMT='(A,2X,A)') TRIM(ADJUSTL(XMax)), TRIM(ADJUSTL(YMax))
                    WRITE(UNIT=33,FMT='(A,2X,A)') TRIM(ADJUSTL(XMax)), TRIM(ADJUSTL(YMin))
                    CLOSE(UNIT=33,STATUS="KEEP")

                    Line = ""
                    Line = TRIM(Line)//TRIM(Path)//"psxy"
                    Line = TRIM(Line)//" "//TRIM(TempLabelsFile)
                    Line = TRIM(Line)//" "//TRIM(ProjectionC)
                    Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                    Line = TRIM(Line)//" "//"-GBlack"
                    Line = TRIM(Line)//" "//"-Sp"
                    IF(KeepOpen(10).EQ.1)THEN
                       Line = TRIM(Line)//" "//"-K"
                    ENDIF
                    Line = TRIM(Line)//" "//"-O >>"
                    Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF(RemoveFiles.EQ.1)THEN
                        OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                        CLOSE(UNIT=33,STATUS="DELETE")
                    ENDIF

                ENDIF

                IF(IfPlotLabels.GT.0)THEN

                    OPEN(UNIT=32,FILE=TRIM(LabelsFile),ACTION="READ")

                    WRITE(UNIT=TempLabelsFile,FMT='(A,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)') &
                            TRIM(TempPath),"Labels.",Record,"-",IL1,"-",IL2,"-",IL3,".txt"

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
                        Position = LEN_TRIM(LabelC) + 2
                        Found = 0
                        DO J=Position,LEN_TRIM(Line)
                            IF((Line(J:J).EQ.",").AND.(Found.EQ.0))THEN
                                LabelLon = Line(Position:J-1)
                                Found = 1
                            ENDIF
                        ENDDO
                        Position = Position + LEN_TRIM(LabelLon) + 1
                        Found = 0
                        DO J=Position,LEN_TRIM(Line)
                            IF((Line(J:J).EQ.",").AND.(Found.EQ.0))THEN
                                LabelLat = Line(Position:J-1)
                                Found = 1
                            ENDIF
                        ENDDO
                        Position = Position + LEN_TRIM(LabelLat) + 1
                        Found = 0
                        DO J=Position,LEN_TRIM(Line)
                            IF((Line(J:J).EQ.",").AND.(Found.EQ.0))THEN
                                LabelAlign = Line(Position:J-1)
                                Found = 1
                            ENDIF
                            IF((J.EQ.LEN_TRIM(Line)).AND.(Found.EQ.0))THEN
                                LabelAlign = Line(Position:J)
                                Found = 2
                            ENDIF
                        ENDDO
                        IF(VersionNumber.GE.43)THEN
                            IF(Found.EQ.2)THEN
                                WRITE(*,'(A)') "WARNING: Beginning with FigureGen v.43, label size and colors should be "// &
                                           "included at the end of each line in the labels file.  Values will be assumed."
                                LabelsSize = 10
                                LabelsColor = "Black"
                            ELSE
                                Position = Position + LEN_TRIM(LabelAlign) + 1
                                Found = 0
                                DO J=Position,LEN_TRIM(Line)
                                    IF((Line(J:J).EQ.",").AND.(Found.EQ.0))THEN
                                        LabelsSizeC = Line(Position:J-1)
                                        READ(UNIT=LabelsSizeC,FMT=*) LabelsSize
                                        Found = 1
                                    ENDIF
                                ENDDO
                                Position = Position + LEN_TRIM(LabelsSizeC) + 1
                                Found = 0
                                DO J=Position,LEN_TRIM(Line)
                                    IF((Line(J:J).EQ.",").AND.(Found.EQ.0))THEN
                                        LabelsColor = Line(Position:J-1)
                                        Found = 1
                                    ENDIF
                                    IF((J.EQ.LEN_TRIM(Line)).AND.(Found.EQ.0))THEN
                                        LabelsColor = Line(Position:J)
                                        Found = 2
                                    ENDIF
                                ENDDO
                            ENDIF
                        ENDIF

                        CALL CapitalizeWord(LabelAlign)
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

                        IF(IfPlotLabels.EQ.2)THEN

                            OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                            WRITE(UNIT=33,FMT='(A10,2X,A10)') LabelLon, LabelLat
                            CLOSE(UNIT=33,STATUS="KEEP")

                            Line = ""
                            Line = TRIM(Line)//TRIM(Path)//"psxy"
                            Line = TRIM(Line)//" "//TRIM(TempLabelsFile)
                            Line = TRIM(Line)//" "//TRIM(ProjectionC)
                            Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                        //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                            Line = TRIM(Line)//" "//"-G"//TRIM(LabelsColor)
                            Line = TRIM(Line)//" "//"-Sc5p"
                            Line = TRIM(Line)//" "//"-K"
                            Line = TRIM(Line)//" "//"-O >>"
                            Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                            CALL SYSTEM(TRIM(Line))

                        ENDIF

                        OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                        WRITE(UNIT=33,FMT='(A,2X,A,2X,I2.2,2X,I2,2X,I2,2X,A,2X,A)') TRIM(LabelLon), TRIM(LabelLat), LabelsSize, &
                                                                                    0, 1, TRIM(LabelAlign), TRIM(LabelC)
                        CLOSE(UNIT=33,STATUS="KEEP")

                        Line = ""
                        Line = TRIM(Line)//TRIM(Path)//"pstext"
                        Line = TRIM(Line)//" "//TRIM(TempLabelsFile)
                        Line = TRIM(Line)//" "//TRIM(ProjectionC)
                        Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                    //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                        Line = TRIM(Line)//" "//"-D"//TRIM(LabelX)//"/"//TRIM(LabelY)
                        Line = TRIM(Line)//" "//"-G"//TRIM(LabelsColor)
                        IF((KeepOpen(11).EQ.1).OR.(I.LT.NumLabels))THEN
                            Line = TRIM(Line)//" "//"-K"
                        ENDIF
                        Line = TRIM(Line)//" "//"-O >>"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))

                        IF(RemoveFiles.EQ.1)THEN
                            OPEN(UNIT=33,FILE=TRIM(TempLabelsFile),ACTION="WRITE")
                            CLOSE(UNIT=33,STATUS="DELETE")
                        ENDIF

                    ENDDO

                    CLOSE(UNIT=32,STATUS="KEEP")

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the labels for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the labels for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(IfPlotVectors.EQ.1)THEN

                    Line = ""
                    Line = TRIM(Line)//TRIM(Path)//"grdvector"
                    Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(VectorUFile)//".grd"
                    Line = TRIM(Line)//" "//TRIM(TempPath)//TRIM(VectorVFile)//".grd"
                    Line = TRIM(Line)//" "//TRIM(ProjectionC)
                    Line = TRIM(Line)//" "//"-GBlack"
                    Line = TRIM(Line)//" "//"-Q"//TRIM(ADJUSTL(VectorTailWidthC))//"i/" &
                                     //TRIM(ADJUSTL(VectorHeadLengthC))//"i/"           &
                                     //TRIM(ADJUSTL(VectorHeadWidthC))//"i"
                    Line = TRIM(Line)//" "//"-R"//TRIM(ADJUSTL(XMin))//"/"//TRIM(ADJUSTL(XMax))//"/" &
                                                //TRIM(ADJUSTL(YMin))//"/"//TRIM(ADJUSTL(YMax))
                    IF(.NOT.WroteBorder.AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                        WroteBorder = .TRUE.
                        Line = TRIM(Line)//" "//"-Bp"//TRIM(ADJUSTL(BorderIncrementMajorC))// &
                                           "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//        &
                                           "/s"//TRIM(ADJUSTL(BorderIncrementMajorC))//       &
                                           "f"//TRIM(ADJUSTL(BorderIncrementMinorC))//"WeSn"
                    ENDIF
                    Line = TRIM(Line)//" "//"-S"//TRIM(ADJUSTL(VectorMagC))//"i"
                    IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                       IF(KeepOpen(13).EQ.0.AND.KeepOpen(14).EQ.0.AND..NOT.PlotVectorScale)THEN
                          !...Nothing
                       ELSE
                           Line = TRIM(Line)//" "//"-K"
                       ENDIF
                    ELSEIF(KeepOpen(12).EQ.1)THEN
                       Line = TRIM(Line)//" "//"-K"
                    ENDIF
                    IF(IfStarted.EQ.0)THEN
                        Line = TRIM(Line)//" "//">"
                        IfStarted = 1
                    ELSE
                        Line = TRIM(Line)//" "//"-O >>"
                    ENDIF
                    Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0).AND.PlotVectorScale)THEN

                        WRITE(UNIT=VectorScaleFile,FMT='(A,I4.4,A)') "vectorscale.",Record,".txt"

                        OPEN(UNIT=21,FILE=TRIM(TempPath)//TRIM(VectorScaleFile),ACTION="WRITE")
                        WRITE(UNIT=21,FMT='(A,F5.2)') "0.0 0.0 0 ",VectorScaleMag/VectorMag
                        CLOSE(UNIT=21,STATUS="KEEP")

                        IF(IfAddTimeBar.EQ.0)THEN
                            VectorScaleY = Height - 0.3
                        ELSE
                            VectorScaleY = Height - 1.1
                        ENDIF

                        IF(VectorScaleY<1.0)THEN
                            WRITE(UNIT=VectorScaleYC,FMT='(F8.6)') VectorScaleY
                        ELSEIF(VectorScaleY<10.0)THEN
                            WRITE(UNIT=VectorScaleYC,FMT='(F8.6)') VectorScaleY
                        ELSE
                            WRITE(UNIT=VectorScaleYC,FMT='(F8.5)') VectorScaleY
                        ENDIF

                        CALL SYSTEM(TRIM(Path)//"psxy "//TRIM(TempPath)//TRIM(VectorScaleFile)// &
                                " -Jx1"//                                                        &
                                " -R0/1/0/1 -GBlack -N -O"//                                     &
                                " -Sv"//TRIM(ADJUSTL(VectorTailWidthC))//                        &
                                    "i/"//TRIM(ADJUSTL(VectorHeadLengthC))//                     &
                                    "i/"//TRIM(ADJUSTL(VectorHeadWidthC))//                      &
                                    "i -Xa"//TRIM(ADJUSTL(VectorScaleXC))//"i"//                 &
                                " -Ya"//TRIM(VectorScaleYC)//"i -K >> "//                        &
                                TRIM(PlotName)//".ps")

                        IF(RemoveFiles.EQ.1)THEN
                            OPEN(UNIT=21,FILE=TRIM(TempPath)//TRIM(VectorScaleFile),ACTION="WRITE")
                            CLOSE(UNIT=21,STATUS="DELETE")
                        ENDIF

                        WRITE(UNIT=VectorTextFile,FMT='(A,I4.4,A)') "vectortext.",Record,".txt"

                        OPEN(UNIT=22,FILE=TRIM(TempPath)//TRIM(VectorTextFile),ACTION="WRITE")
                        WRITE(UNIT=22,FMT='(A,F6.2,A)') "0.0 0.0 14 0 0 LT ",                     &
                                VectorScaleMag," "//TRIM(VectorLabel)
                        CLOSE(UNIT=22,STATUS="KEEP")

                        IF(IfAddTimeBar.EQ.0)THEN
                            VectorScaleY = Height
                        ELSE
                            VectorScaleY = Height - 0.8
                        ENDIF

                        IF(VectorScaleY<1.0)THEN
                            WRITE(UNIT=VectorScaleYC,FMT='(F8.6)') VectorScaleY
                        ELSEIF(VectorScaleY<10.0)THEN
                            WRITE(UNIT=VectorScaleYC,FMT='(F8.6)') VectorScaleY
                        ELSE
                            WRITE(UNIT=VectorScaleYC,FMT='(F8.5)') VectorScaleY
                        ENDIF

                        Line = ""
                        Line = TRIM(Path)//"pstext "//TRIM(TempPath)//TRIM(VectorTextFile)// &
                                " -Jx1"
                        IF(KeepOpen(12).EQ.1)THEN
                            Line = TRIM(Line)//" -K"
                        ENDIF
                        Line = TRIM(Line)//" -R0/1/0/1 -GBlack -N -O -Xa"//TRIM(ADJUSTL(SideBarXC))//"i"// &
                                " -Ya"//TRIM(VectorScaleYC)//"i >> "//                            &
                                TRIM(PlotName)//".ps"
                        CALL SYSTEM(TRIM(Line))

                        IF(RemoveFiles.EQ.1)THEN
                            OPEN(UNIT=22,FILE=TRIM(TempPath)//TRIM(VectorTextFile),ACTION="WRITE")
                            CLOSE(UNIT=22,STATUS="DELETE")
                        ENDIF

                    ENDIF

                    IF(Verbose.GE.3)THEN
                        IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the vectors for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the vectors for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF(IfAddPlotLabel.EQ.1)THEN

                    WRITE(UNIT=PlotLabelFile,FMT='(A,I4.4,A)') "plotlabel.",Record,".txt"

                    OPEN(UNIT=28,FILE=TRIM(TempPath)//TRIM(PlotLabelFile),ACTION="WRITE")
                    WRITE(UNIT=28,FMT='(A,A)') "0.0 0.0 14 0 0 BC ",TRIM(PlotLabel)
                    CLOSE(UNIT=28,STATUS="KEEP")

                    Line = TRIM(Path)//"pstext "//TRIM(TempPath)//TRIM(PlotLabelFile)      &
                                //" -JX1i -R0/8/0/1 -Xa"//TRIM(ADJUSTL(PlotLabelXAdjustC)) &
                                //"i -Ya"//TRIM(ADJUSTL(PlotLabelYAdjustC))//"i"  
                    IF(KeepOpen(13).EQ.1)THEN
                        Line = TRIM(Line)//" -K"
                    ENDIF
                    Line = TRIM(Line)//" -N -O >>"//Trim(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF(RemoveFiles.EQ.1)THEN
                        OPEN(UNIT=28,FILE=TRIM(TempPath)//TRIM(PlotLabelFile),ACTION="WRITE")
                        CLOSE(UNIT=28,STATUS="DELETE")
                    ENDIF

                    IF(Verbose.GE.3)THEN
                        IF(IfGoogle.EQ.0)THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the plot label for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the plot label for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF((IfAddTimeBar.GT.0).AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN

                    WRITE(UNIT=TimeMaxFile,FMT='(A,I4.4,A)') "timemax.",Record,".txt"
                    OPEN(UNIT=25,FILE=TRIM(TempPath)//TRIM(TimeMaxFile),ACTION="WRITE")
                    WRITE(UNIT=25,FMT='(A)') "0 0 0 1.0"
                    CLOSE(UNIT=25,STATUS="KEEP")

                    Line = TRIM(Path)//"psxy "//TRIM(TempPath)//TRIM(TimeMaxFile) &
                                //" -JX1i -R0/2/0/2 -N -O -Sv0.2i/0.0i/0.01"      &
                                //" -Xa"//TRIM(ADJUSTL(SideBarXC))//"i"           &
                                //" -Ya"//TRIM(ADJUSTL(TimeScaleYC))//"i"
                    Line = TRIM(Line)//" -K"
                    Line = TRIM(Line)//" -N -O >> "//TRIM(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF(RemoveFiles.EQ.1)THEN
                        OPEN(UNIT=25,FILE=TRIM(TempPath)//TRIM(TimeMaxFile),ACTION="WRITE")
                        CLOSE(UNIT=25,STATUS="DELETE")
                    ENDIF

                    WRITE(UNIT=TimeCurrentFile,FMT='(A,I4.4,A)') "timecurrent.",Record,".txt"
                    OPEN(UNIT=26,FILE=TRIM(TempPath)//TRIM(TimeCurrentFile),ACTION="WRITE")
                    IF(NumRecs.NE.0)THEN
                        IF(TotalSimTime.NE.-1D0)THEN
                            WRITE(UNIT=26,FMT='(A,F4.2)') "0 0 0 ",(CurrentTime-StartTime)/(TotalSimTime*86400D0-StartTime)
                        ELSE
                            WRITE(UNIT=26,FMT='(A,F4.2)') "0 0 0 ",1.0*Record/NumRecs
                        ENDIF    
                    ELSE
                        WRITE(UNIT=26,FMT='(A)') "0 0 0 0.0"
                    ENDIF
                    CLOSE(UNIT=26,STATUS="KEEP")

                    Line = TRIM(Path)//"psxy "//TRIM(TempPath)//TRIM(TimeCurrentFile) &
                                //" -JX1i -R0/2/0/2 -GBlack -N -O -Sv0.2i/0.0i/0.0i"  &
                                //" -Xa"//TRIM(ADJUSTL(SideBarXC))//"i"               &
                                //" -Ya"//TRIM(ADJUSTL(TimeScaleYC))//"i"
                    Line = TRIM(Line)//" -K"
                    Line = TRIM(Line)//" -N -O >> "//TRIM(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF(RemoveFiles.EQ.1)THEN
                        OPEN(UNIT=26,FILE=TRIM(TempPath)//TRIM(TimeCurrentFile),ACTION="WRITE")
                        CLOSE(UNIT=26,STATUS="DELETE")
                    ENDIF

                    WRITE(UNIT=TimeCurrentTextFile,FMT='(A,I4.4,A)') "timecurrenttext.",Record,".txt"
                    OPEN(UNIT=27,FILE=TRIM(TempPath)//TRIM(TimeCurrentTextFile),ACTION="WRITE")
                    IF(IfAddTimeBar.EQ.1)THEN
                        WRITE(UNIT=27,FMT='(A,F5.2,A)') "0 0 14 0 0 LT ",CurrentTime/86400.0," days"
                    ELSEIF(IfAddTimeBar.EQ.2.OR.IfAddTimeBar.EQ.3)THEN
                        CALL DATEADD(ColdStartDate,INT(CurrentTime,8),ResultDate)
                        IF(IfAddTimeBar.EQ.3)THEN
                            IF(ResultDate%Hour.GE.12)THEN
                                AMPMS="PM"
                                IF(ResultDate%Hour.GT.12)THEN
                                    ResultDate%Hour=ResultDate%Hour-12
                                ENDIF
                            ELSEIF(ResultDate%Hour.EQ.0)THEN
                                ResultDate%Hour=12
                                AMPMS="AM"
                            ELSE
                                AMPMS="AM"
                            ENDIF   
                        ENDIF
                        WRITE(UNIT=27,FMT='(A,I2.2,A,I2.2,A,I4.4)') &
                            "0 0.125 10 0 0 LT ",&
                            ResultDate%Month,"/",ResultDate%Day,"/",ResultDate%Year
                        IF(IfAddTimeBar.EQ.3)THEN
                            WRITE(UNIT=27,FMT='(A,I2.2,A,I2.2,A,I2.2,A)') &
                                "0 -0.125 10 0 0 LT ",&
                                ResultDate%Hour,":",ResultDate%Minute,":",ResultDate%Second,AMPMS
                        ELSEIF(IfAddTimeBar.EQ.2)THEN
                            WRITE(UNIT=27,FMT='(A,I2.2,A,I2.2,A,I2.2)') &
                                "0 -0.125 10 0 0 LT ",&
                                ResultDate%Hour,":",ResultDate%Minute,":",ResultDate%Second
                        ENDIF    
                    ENDIF
                    CLOSE(UNIT=27,STATUS="KEEP")

                    Line = TRIM(Path)//"pstext "//TRIM(TempPath)                &
                                //TRIM(TimeCurrentTextFile)//" -JX1i -R0/2/0/2" &
                                //" -Xa"//TRIM(ADJUSTL(SideBarXC))//"i"         &
                                //" -Ya"//TRIM(ADJUSTL(TimeScaleTextYC))//"i"

                    IF(KeepOpen(14).EQ.1)THEN
                        Line = TRIM(Line)//" -K"
                    ENDIF
                    Line = TRIM(Line)//" -N -O >> "//TRIM(PlotName)//".ps"
                    CALL SYSTEM(TRIM(Line))

                    IF(RemoveFiles.EQ.1)THEN
                        OPEN(UNIT=27,FILE=TRIM(TempPath)//TRIM(TimeCurrentTextFile),ACTION="WRITE")
                        CLOSE(UNIT=27,STATUS="DELETE")
                    ENDIF

                    IF(Verbose.GE.3)THEN
                        IF(IfGoogle.EQ.0)THEN
                            WRITE(*,9720) "Core ",MyRank," wrote the time bar for record ",Record,"."
                        ELSE
                            WRITE(*,9721) "Core ",MyRank," wrote the time bar for record ",Record, &
                                          ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                        ENDIF
                    ENDIF

                ENDIF

                IF((IfPlotLogo.EQ.1).AND.(IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN

                    IF(VersionNumber.GE.43)THEN
                        IF(INDEX(LogoFile,".txt").GT.0)THEN
                            WRITE(TempLogoFile,'(A)') "NULL"
                            OPEN(UNIT=26,FILE=TRIM(LogoFile),ACTION="READ")
                            READ(26,*) NumLogos
                            DO IO=1,NumLogos
                                READ(26,'(A)') Line
                                READ(Line,*) JunkI
                                IF(JunkI.EQ.Record)THEN
                                    JunkI = INDEX(Line,",")
                                    WRITE(TempLogoFile,'(A)') Line(JunkI+1:LEN_TRIM(Line))
                                ENDIF
                            ENDDO
                            CLOSE(UNIT=26,STATUS="KEEP")
                        ELSE
                            WRITE(TempLogoFile,'(A)') LogoFile
                        ENDIF
                    ELSE
                        WRITE(TempLogoFile,'(A)') LogoFile
                    ENDIF

                    IF((TRIM(LogoLocation).EQ."TL").OR.(TRIM(LogoLocation).EQ."LT"))THEN
                        Line = "0/"//TRIM(ADJUSTL(HeightC))//"i/TL"
                    ELSEIF((TRIM(LogoLocation).EQ."TR").OR.(TRIM(LogoLocation).EQ."RT"))THEN
                        Line = TRIM(ADJUSTL(WidthC))//"i/"//TRIM(ADJUSTL(HeightC))//"i/TR"
                    ELSEIF((TRIM(LogoLocation).EQ."BR").OR.(TRIM(LogoLocation).EQ."RB"))THEN
                        Line = TRIM(ADJUSTL(WidthC))//"i/0/BR"
                    ELSE
                        Line = "0/0/BL"
                    ENDIF

                    IF(INDEX(TempLogoFile,"NULL").GT.0)THEN
                        CONTINUE
                    ELSE
                        CALL SYSTEM(TRIM(Path)//"psimage "//TRIM(TempLogoFile)//   &
                                " -W"//TRIM(LogoWidth)//"i"//                  &
                                " -C"//TRIM(Line)//                            &
                                " -Fthin,black -O >> "//TRIM(PlotName)//".ps")

                        IF(Verbose.GE.3)THEN
                            IF(IfGoogle.EQ.0)THEN
                                WRITE(*,9720) "Core ",MyRank," wrote the logo for record ",Record,"."
                            ELSE
                                WRITE(*,9721) "Core ",MyRank," wrote the logo for record ",Record, &
                                              ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF

                IF(IfGoogle.EQ.1)THEN

                    CALL SYSTEM("grep -v showpage "//TRIM(PlotName)//".ps | sed -e "//    &
                                "'s/scale 0 A/scale 0 A showpage/g' > "//TRIM(TempPath)// &
                                TRIM(PlotName)//".ps")

                    CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//".ps "// &
                                TRIM(PlotName)//".ps")

                    CALL SYSTEM(TRIM(Path)//"ps2raster "//TRIM(PlotName)//".ps -A"//      &
                                " -E"//TRIM(ResolutionC)//" -F"//TRIM(TempPath)//         &
                                TRIM(PlotName)//".png"//" -G"//TRIM(GSPath)//"gs -P -TG")

                    CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//".png .")

                ELSEIF(IfGIS.GT.0)THEN

                    CALL SYSTEM(TRIM(Path)//"ps2raster "//TRIM(PlotName)//".ps -A"//      &
                                " -E"//TRIM(ResolutionC)//" -F"//TRIM(TempPath)//         &
                                TRIM(PlotName)//"_grf"//" -G"//TRIM(GSPath)//"gs -P -Tj -W")             

                    CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//"_grf* .")

                ELSE

                    ! Casey 2012/01/26: For some reason, this step is not necessary
                    !    if the only thing in the PostScript file is the mesh (with no plot label,
                    !    no logo, etc.).  Try this fix.
                    IF(KeepOpen(2).EQ.1)THEN
                        CALL SYSTEM("grep -v showpage "//TRIM(PlotName)//".ps | sed -e "//    &
                                    "'s/scale 0 A/scale 0 A showpage/g' > "//TRIM(TempPath)// &
                                    TRIM(PlotName)//".ps")
                        CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//".ps "// &
                                    TRIM(PlotName)//".ps")
                    ENDIF

                    IF(DoPNG.EQ.1)THEN
                        Line = TRIM(Path)//"ps2raster"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        IF(ImageTrimFlag.EQ.1)THEN
                            Line = TRIM(Line)//" "//"-A"
                        ENDIF
                        Line = TRIM(Line)//" "//"-E"//TRIM(ResolutionC)
                        Line = TRIM(Line)//" "//"-F"//TRIM(TempPath)//TRIM(PlotName)//".png"
                        Line = TRIM(Line)//" "//"-G"//TRIM(GSPath)//"gs"
                        Line = TRIM(Line)//" "//"-P"
                        Line = TRIM(Line)//" "//"-TG"
                        CALL SYSTEM(TRIM(Line))
                        CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//".png .")
                    ENDIF

                    IF(DoJPG.EQ.1)THEN
                        Line = TRIM(Path)//"ps2raster"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        IF(ImageTrimFlag.EQ.1)THEN
                            Line = TRIM(Line)//" "//"-A"
                        ENDIF
                        Line = TRIM(Line)//" "//"-E"//TRIM(ResolutionC)
                        Line = TRIM(Line)//" "//"-F"//TRIM(TempPath)//TRIM(PlotName)//".jpg"
                        Line = TRIM(Line)//" "//"-G"//TRIM(GSPath)//"gs"
                        Line = TRIM(Line)//" "//"-P"
                        Line = TRIM(Line)//" "//"-Tj"
                        CALL SYSTEM(TRIM(Line))
                        CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//".jpg .")
                    ENDIF

                    IF(DoPDF.EQ.1)THEN
                        Line = TRIM(Path)//"ps2raster"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        IF(ImageTrimFlag.EQ.1)THEN
                            Line = TRIM(Line)//" "//"-A"
                        ENDIF
                        Line = TRIM(Line)//" "//"-E"//TRIM(ResolutionC)
                        Line = TRIM(Line)//" "//"-F"//TRIM(TempPath)//TRIM(PlotName)//".pdf"
                        Line = TRIM(Line)//" "//"-G"//TRIM(GSPath)//"gs"
                        Line = TRIM(Line)//" "//"-P"
                        Line = TRIM(Line)//" "//"-Tf"
                        CALL SYSTEM(TRIM(Line))
                        CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//".pdf .")
                    ENDIF

                    IF(DoBMP.EQ.1)THEN
                        Line = TRIM(Path)//"ps2raster"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        IF(ImageTrimFlag.EQ.1)THEN
                            Line = TRIM(Line)//" "//"-A"
                        ENDIF
                        Line = TRIM(Line)//" "//"-E"//TRIM(ResolutionC)
                        Line = TRIM(Line)//" "//"-F"//TRIM(TempPath)//TRIM(PlotName)//".bmp"
                        Line = TRIM(Line)//" "//"-G"//TRIM(GSPath)//"gs"
                        Line = TRIM(Line)//" "//"-P"
                        Line = TRIM(Line)//" "//"-Tb"
                        CALL SYSTEM(TRIM(Line))
                        CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//".bmp .")
                    ENDIF

                    IF(DoTIFF.EQ.1)THEN
                        Line = TRIM(Path)//"ps2raster"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        IF(ImageTrimFlag.EQ.1)THEN
                            Line = TRIM(Line)//" "//"-A"
                        ENDIF
                        Line = TRIM(Line)//" "//"-E"//TRIM(ResolutionC)
                        Line = TRIM(Line)//" "//"-F"//TRIM(TempPath)//TRIM(PlotName)//".tif"
                        Line = TRIM(Line)//" "//"-G"//TRIM(GSPath)//"gs"
                        Line = TRIM(Line)//" "//"-P"
                        Line = TRIM(Line)//" "//"-Tt"
                        CALL SYSTEM(TRIM(Line))
                        CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//".tif .")
                    ENDIF

                    IF(DoEPS.EQ.1)THEN
                        Line = TRIM(Path)//"ps2raster"
                        Line = TRIM(Line)//" "//TRIM(PlotName)//".ps"
                        IF(ImageTrimFlag.EQ.1)THEN
                            Line = TRIM(Line)//" "//"-A"
                        ENDIF
                        Line = TRIM(Line)//" "//"-E"//TRIM(ResolutionC)
                        Line = TRIM(Line)//" "//"-F"//TRIM(TempPath)//TRIM(PlotName)//".eps"
                        Line = TRIM(Line)//" "//"-G"//TRIM(GSPath)//"gs"
                        Line = TRIM(Line)//" "//"-P"
                        Line = TRIM(Line)//" "//"-Te"
                        CALL SYSTEM(TRIM(Line))
!                       CALL SYSTEM("mv "//TRIM(TempPath)//TRIM(PlotName)//".eps .")
                    ENDIF

                ENDIF
         
                IF(Verbose.GE.3)THEN
                    IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                        WRITE(*,9720) "Core ",MyRank," created the raster images for record ",Record,"."
                    ELSE
                        WRITE(*,9721) "Core ",MyRank," created the raster images for record ",Record, &
                                      ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                    ENDIF
                ENDIF
             

 9720           FORMAT(A,I4.4,A,I4.4,A)
 9721           FORMAT(A,I4.4,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)

        END SUBROUTINE



        SUBROUTINE WriteXYZFiles(Record)

                

#ifdef NETCDF
                USE netcdf
#endif
                
                IMPLICIT NONE

                INTRINSIC           :: INDEX
                INTRINSIC           :: LEN_TRIM
                INTRINSIC           :: SQRT
                INTRINSIC           :: TRIM

                CHARACTER(LEN=50)   :: AttributeLabel
                CHARACTER(LEN=50)   :: ContourFile1Local
                CHARACTER(LEN=1)    :: JunkC
                CHARACTER(LEN=250)  :: UnitFile1
                CHARACTER(LEN=250)  :: UnitFile2
                CHARACTER(LEN=10)   :: VectorSpacingC
                CHARACTER(LEN=15)   :: XMaxBuf
                CHARACTER(LEN=15)   :: XMinBuf
                CHARACTER(LEN=15)   :: YMaxBuf
                CHARACTER(LEN=15)   :: YMinBuf

                INTEGER,ALLOCATABLE :: Count(:)
                INTEGER,SAVE        :: CurrentRecord
                INTEGER             :: I
                INTEGER             :: IRET
                INTEGER             :: J
                INTEGER             :: JunkI
                INTEGER,ALLOCATABLE :: NC(:,:)
#ifdef NETCDF
                INTEGER,DIMENSION(NF90_MAX_VAR_DIMS) :: NC_DimIDs
                INTEGER             :: NC_File
                INTEGER             :: NC_ID1
                INTEGER             :: NC_ID2
                INTEGER             :: NC_JunkI(1)
                INTEGER             :: NC_Status
                INTEGER             :: NC_Var
                INTEGER             :: NC_Var2
                INTEGER             :: NC_Var3
                INTEGER             :: NC_Var4
#endif
                INTEGER             :: ContourFileNumCols2
                INTEGER             :: LocalRecord
                INTEGER             :: NumAttributes1
                INTEGER             :: NumAttributes2
                INTEGER             :: NumElems
                INTEGER             :: NumNodes1
                INTEGER             :: NumNodes2
                INTEGER             :: NumNonDefault
                INTEGER             :: NumParticles
                INTEGER             :: ParticleFileOpened
                INTEGER             :: POW_I
                INTEGER             :: Record
                INTEGER             :: VectorFileIsContourFile
                INTEGER             :: VectorFileOpened

                LOGICAL             :: UnitFile1Opened
                LOGICAL             :: UnitFile2Opened

                REAL,ALLOCATABLE    :: Attributes1(:)
                REAL,ALLOCATABLE    :: Attributes2(:)
                REAL                :: AttributeDefault
                REAL,ALLOCATABLE    :: AttrWR(:)
                REAL,ALLOCATABLE    :: Bath1(:)
                REAL,ALLOCATABLE    :: Bath2(:)
                REAL                :: DefaultValue
                REAL                :: Dist
                REAL(8)             :: JunkR
                REAL                :: JunkR1
                REAL                :: JunkR2
                REAL(8),ALLOCATABLE :: OWI_XYZUV(:,:)
                REAL                :: POW
                REAL                :: TRUNC
                REAL,ALLOCATABLE    :: Lat(:)
                REAL,ALLOCATABLE    :: Lon(:)
#ifdef NETCDF
                REAL,ALLOCATABLE    :: ParticleLat(:)
                REAL,ALLOCATABLE    :: ParticleLon(:)
                REAL,ALLOCATABLE    :: ParticleFlag(:)
                REAL(8)             :: NC_JunkR(1)
                REAL(8)             :: NC_Time(1)
#endif
                REAL,ALLOCATABLE    :: Sizes(:)
                REAL,ALLOCATABLE    :: U1(:)
                REAL,ALLOCATABLE    :: U2(:)
                REAL,ALLOCATABLE    :: V1(:)
                REAL,ALLOCATABLE    :: V2(:)
                REAL,ALLOCATABLE    :: Vels1(:)
                REAL,ALLOCATABLE    :: Vels2(:)

1236            FORMAT(F15.8)

                IF((IfPlotFilledContours.GE.1).OR.(IfPlotContourLines.GE.1))THEN

                    IF((IfPlotFilledContours.EQ.1).OR.(IfPlotContourLines.EQ.1))THEN

                        IF((TRIM(ContourFileType).EQ."ADCIRC-OUTPUT").OR.&
                           (TRIM(ContourFileType).EQ."ADCIRC-OUTPUT-LIST"))THEN

                            IF(TRIM(ContourFileType).EQ."ADCIRC-OUTPUT-LIST")THEN
                                ContourFile1Local = ContourFileList(Record)
                                LocalRecord = 1
                                CurrentRecord = 1
                                IF(INDEX(ContourFile1Local,".nc").LE.0)THEN
                                    ContourFileFormat1 = "ASCII"
                                ELSE
                                    ContourFileFormat1 = "NETCDF"
                                ENDIF
                            ELSE
                                ContourFile1Local = ContourFile1
                                LocalRecord = Record
                            ENDIF

                            WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1Local), ".", Record, ".xyz"
                            OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")

                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                UnitFile1Opened = .FALSE.
                                INQUIRE(UNIT=19,OPENED=UnitFile1Opened)
                                IF(.NOT.UnitFile1Opened.OR.OutputFileList)THEN
                                    IF(.NOT.UnitFile1Opened)THEN
                                        OPEN(UNIT=19,FILE=TRIM(ContourFile1Local),ACTION="READ")
                                    ELSEIF(OutputFileList)THEN
                                        REWIND(19)
                                    ENDIF
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) NumRecs, JunkI, JunkR, JunkI, ContourFileNumCols
                                    CurrentRecord = 1
                                ENDIF
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_OPEN(TRIM(ContourFile1Local),NF90_NOWRITE,NC_ID1))
                                CALL Check(NF90_INQ_VARID(NC_ID1,'time',NC_Var))
                                CALL Check(NF90_INQUIRE_VARIABLE(NC_ID1,NC_Var,dimids=NC_DimIDs))
                                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_DimIDs(1),len=NumRecs))
                                CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time,start=(/LocalRecord/),count=(/1/)))
                                CurrentTime = NC_Time(1)
                                !...Grab the start and end times as well
                                CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time,start=(/1/),count=(/1/)))
                                StartTime = NC_Time(1)
                                iret = NF90_GET_ATT(NC_ID1,NF90_GLOBAL,'rnday',TotalSimTime1)
                                IF(iret.NE.NF90_NOERR)THEN
                                    TotalSimTime1 = -1D0
                                ENDIF
                                CALL CHECK(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time,start=(/NumRecs/),count=(/1/)))
                                TotalSimTime2 = NC_Time(1)/86400D0
                                TotalSimTime = MAX(TotalSimTime1,TotalSimTime2)

                                CALL FindMyNETCDFVariable(NC_ID1)
#endif
                            ENDIF

                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                IF(CurrentRecord.LT.LocalRecord)THEN
                                    DO J=CurrentRecord,LocalRecord-1
                                        NumNodes1 = NumNodesGlobal
                                        CALL ReadTimeStamp(19,LEN_TRIM(ContourFile1Local),TRIM(ContourFile1Local),&
                                                           CurrentTime,JunkR,NumNodes1,DefaultValue)
                                        DO I=1,NumNodes1
#ifdef SLOWREAD
                                            CALL ReadNodeVals(19,LEN_TRIM(ContourFile1Local),TRIM(ContourFile1Local),0,&
                                                              JunkI,JunkR1,JunkR2)
#else
                                            READ(19,*)
#endif
                                        ENDDO
                                    ENDDO
                                ENDIF
                            ENDIF

                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                NumNodes1 = NumNodesGlobal
                                CALL ReadTimeStamp(19,LEN_TRIM(ContourFile1Local),TRIM(ContourFile1Local),CurrentTime,JunkR, &
                                                   NumNodes1,DefaultValue)
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                NumNodes1 = NumNodesGlobal
                                CALL GetNETCDFVARID(NC_ID1,NC_Var,NC_Var2,ContourFileNumCols)

                                ierr = NF90_GET_ATT(NC_ID1,NC_Var,'_FillValue',DefaultValue)
                                IF(ierr.NE.NF90_NOERR)THEN
                                    CALL Check(NF90_GET_ATT(NC_ID1,NF90_GLOBAL,'_FillValue',DefaultValue))
                                ENDIF    

#endif
                            ENDIF

                            IF(.NOT.ALLOCATED(U1))    ALLOCATE(U1(1:NumNodesGlobal))
                            IF(.NOT.ALLOCATED(V1))    ALLOCATE(V1(1:NumNodesGlobal))
                            IF(.NOT.ALLOCATED(Vels1)) ALLOCATE(Vels1(1:NumNodesGlobal))

                            IF(INDEX(TRIM(ContourFile1Local),"64").GT.0)THEN
                                DO I=1,NumNodesGlobal
                                    U1(I)    = -99999.0
                                    V1(I)    = -99999.0
                                    Vels1(I) = -99999.0
                                ENDDO
                            ELSEIF(DefaultValue.GT.-99998.0)THEN
                                DO I=1,NumNodesGlobal
                                    U1(I)    = DefaultValue * ContourConversionFactor
                                    V1(I)    = DefaultValue * ContourConversionFactor
                                    Vels1(I) = DefaultValue * ContourConversionFactor
                                ENDDO
                            ELSE
                                DO I=1,NumNodesGlobal
                                    U1(I)    = DefaultValue
                                    V1(I)    = DefaultValue
                                    Vels1(I) = DefaultValue
                                ENDDO
                            ENDIF

                            IF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                
                                IF(ContourFileNumCols.EQ.1)THEN
                                    CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,VARID1=NC_Var,&
                                                              VEC1=U1,RECORD=LocalRecord)
                                ELSE
                                    CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,VARID1=NC_Var,&
                                                              VARID2=NC_Var2,VEC1=U1,VEC2=V1,RECORD=LocalRecord)
                                ENDIF
#endif
                            ENDIF

                            DO I=1,NumNodes1

                                IF(ContourFileNumCols.EQ.1)THEN
                                    
                                    IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
#ifdef SLOWREAD
                                        CALL ReadNodeVals(19,LEN_TRIM(ContourFile1Local),TRIM(ContourFile1Local),1,JunkI,&
                                                          JunkR1,JunkR2)
#else
                                        READ(19,*) JunkI,JunkR1
#endif
                                        U1(JunkI) = JunkR1
                                        IF(U1(JunkI).GT.-99998.0)THEN
                                            U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                        ENDIF
                                        Vels1(JunkI) = U1(JunkI)
                                    ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                        IF(U1(I).GT.-99998.0)THEN
                                            U1(I) = U1(I) * ContourConversionFactor
                                        ENDIF
                                        Vels1(I) = U1(I)
#endif
                                    ENDIF

                                ELSEIF(ContourFileNumCols.EQ.2)THEN

                                    IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN 
#ifdef SLOWREAD
                                        CALL ReadNodeVals(19,LEN_TRIM(ContourFile1Local),TRIM(ContourFile1Local),2,JunkI,&
                                                          JunkR1,JunkR2)
#else
                                        READ(19,*) JunkI,JunkR1,JunkR2
#endif
                                        U1(JunkI) = JunkR1
                                        V1(JunkI) = JunkR2
                                        Vels1(JunkI) = ContourConversionFactor * SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))
#ifdef DRYZEROVEL                                            
                                        IF(Vels1(JunkI).EQ.0D0)Vels1(JunkI)=-99999D0
#endif
                                    ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN 
#ifdef NETCDF
                                        Vels1(I) = ContourConversionFactor * SQRT(U1(I)*U1(I)+V1(I)*V1(I))
#ifdef DRYZEROVEL                                            
                                        IF(Vels1(I).EQ.0D0)Vels1(I)=-99999D0
#endif                                        
#endif
                                    ENDIF

                                ENDIF

                            ENDDO

                            DO I=1,NumNodesLocal

                                Z(I) = Vels1(XYZNodes(I))

                                IF(OptimizeContours.EQ.1)THEN
                                   IF(BdyNodes(XYZNodes(I)))THEN
                                      Z(I) = -88888.
                                   ENDIF
                                ENDIF

                                WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                            ENDDO

                            CLOSE(UNIT=12,STATUS="KEEP")
                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                CONTINUE
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_CLOSE(NC_ID1))
#endif
                            ENDIF

                        ELSEIF(INDEX(ContourFileType,"GRID-DECOMP").GT.0)THEN

                            WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"

                            OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")

                            DO I=1,NumNodesLocal

                                Z(I) = NodeColors(XYZNodes(I))

                                IF(OptimizeContours.EQ.1)THEN
                                   IF(BdyNodes(XYZNodes(I)))THEN
                                      Z(I) = -88888.
                                   ENDIF
                                ENDIF

                                WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                            ENDDO

                            CLOSE(UNIT=12,STATUS="KEEP")

                        ELSEIF((TRIM(ContourFileType).EQ."GRID-BATH").OR. &
                               (TRIM(ContourFileType).EQ."GRID-SIZE"))THEN

                            WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"
                            OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")

                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                                READ(UNIT=19,FMT='(A)') JunkC
                                READ(UNIT=19,FMT=*) NumElems, NumNodes1
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_OPEN(TRIM(ContourFile1),NF90_NOWRITE,NC_ID1))
                                CALL Check(NF90_INQ_DIMID(NC_ID1,"nele",JunkI))
                                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,JunkI,len=NumElems))
                                CALL Check(NF90_INQ_DIMID(NC_ID1,"node",JunkI))
                                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,JunkI,len=NumNodes1))
#endif
                            ENDIF

                            IF(TRIM(ContourFileType).EQ."GRID-BATH")THEN
                                ALLOCATE(Bath1(1:NumNodes1))
                            ELSEIF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN
                                ALLOCATE(Lat(1:NumNodes1))
                                ALLOCATE(Lon(1:NumNodes1))
                            ENDIF

                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                DO I=1,NumNodes1
                                    IF(TRIM(ContourFileType).EQ."GRID-BATH")THEN
                                        READ(UNIT=19,FMT=*) JunkI, JunkR, JunkR, Bath1(I)
                                    ELSEIF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN
                                        READ(UNIT=19,FMT=*) JunkI, Lon(I), Lat(I), JunkR
                                    ENDIF
                                ENDDO
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                IF(TRIM(ContourFileType).EQ."GRID-BATH")THEN
                                    CALL Check(NF90_INQ_VARID(NC_ID1,'depth',JunkI))
                                    CALL Check(NF90_GET_VAR(NC_ID1,JunkI,Bath1))
                                ELSEIF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN
                                    CALL Check(NF90_INQ_VARID(NC_ID1,'x',JunkI))
                                    CALL Check(NF90_GET_VAR(NC_ID1,JunkI,Lon))
                                    CALL Check(NF90_INQ_VARID(NC_ID1,'y',JunkI))
                                    CALL Check(NF90_GET_VAR(NC_ID1,JunkI,Lat))
                                ENDIF
#endif
                            ENDIF

                            IF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN

                                ALLOCATE(NC(1:3,1:NumElems))
                                ALLOCATE(Sizes(1:NumNodes1))
                                ALLOCATE(Count(1:NumNodes1))

                                DO I=1,NumNodes1
                                    Sizes(I) = 0.D0
                                    Count(I) = 0
                                ENDDO

                                IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                    DO I=1,NumElems
                                        READ(UNIT=19,FMT=*) JunkI, JunkI, NC(1,I), NC(2,I), NC(3,I)
                                    ENDDO
                                ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                    CALL Check(NF90_INQ_VARID(NC_ID1,'element',JunkI))
                                    CALL Check(NF90_GET_VAR(NC_ID1,JunkI,NC))
#endif
                                ENDIF

                                DO I=1,NumElems

                                    Dist = ComputeDistance(Lon(NC(1,I)),Lat(NC(1,I)),Lon(NC(2,I)),Lat(NC(2,I)))
                                    Sizes(NC(1,I)) = Sizes(NC(1,I)) + Dist
                                    Sizes(NC(2,I)) = Sizes(NC(2,I)) + Dist
                                    Count(NC(1,I)) = Count(NC(1,I)) + 1
                                    Count(NC(2,I)) = Count(NC(2,I)) + 1

                                    Dist = ComputeDistance(Lon(NC(2,I)),Lat(NC(2,I)),Lon(NC(3,I)),Lat(NC(3,I)))
                                    Sizes(NC(2,I)) = Sizes(NC(2,I)) + Dist
                                    Sizes(NC(3,I)) = Sizes(NC(3,I)) + Dist
                                    Count(NC(2,I)) = Count(NC(2,I)) + 1
                                    Count(NC(3,I)) = Count(NC(3,I)) + 1

                                    Dist = ComputeDistance(Lon(NC(3,I)),Lat(NC(3,I)),Lon(NC(1,I)),Lat(NC(1,I)))
                                    Sizes(NC(3,I)) = Sizes(NC(3,I)) + Dist
                                    Sizes(NC(1,I)) = Sizes(NC(1,I)) + Dist
                                    Count(NC(3,I)) = Count(NC(3,I)) + 1
                                    Count(NC(1,I)) = Count(NC(1,I)) + 1

                                ENDDO

                            ENDIF

                            DO I=1,NumNodesLocal

                                IF(TRIM(ContourFileType).EQ."GRID-BATH")THEN
                                    Z(I) = Bath1(XYZNodes(I)) * ContourConversionFactor
                                ELSEIF(TRIM(ContourFileType).EQ."GRID-SIZE")THEN
                                    Z(I) = (Sizes(XYZNodes(I)) / Count(XYZNodes(I))) * ContourConversionFactor
                                ENDIF

                                IF(OptimizeContours.EQ.1)THEN
                                   IF(BdyNodes(XYZNodes(I)))THEN
                                      Z(I) = -88888.
                                   ENDIF
                                ENDIF

                                WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                            ENDDO

                            CLOSE(UNIT=12,STATUS="KEEP")

                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                CLOSE(UNIT=19,STATUS="KEEP")
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_CLOSE(NC_ID1))
#endif
                            ENDIF

                        ELSEIF((TRIM(ContourFileType).EQ."OWI-PRESS").OR.&
                               (TRIM(ContourFileType).EQ."OWI-WIND"))THEN

                            WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record,".xyz"
                            OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                            IF(ContourFileType.EQ."OWI-PRESS")THEN
                                CALL READOWISNAP("PRESS",Record,ContourFile1,OWI_XYZUV,CurrentTime)
                            ELSE
                                CALL READOWISNAP("WIND",Record,ContourFile1,OWI_XYZUV,CurrentTime)
                            ENDIF



                            DO I = 1,SIZE(OWI_XYZUV(:,1))
                                WRITE(UNIT=12,FMT='(3(2X,F16.8))') OWI_XYZUV(I,1),OWI_XYZUV(I,2),OWI_XYZUV(I,3)*&
                                                                   ContourConversionFactor
                            ENDDO

                            CLOSE(UNIT=12,STATUS="KEEP")


                        ELSEIF((TRIM(ContourFileType).EQ."13-MANNING").OR.           &
                               (INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0).OR. &
                               (TRIM(ContourFileType).EQ."13-CANOPY").OR.            &
                               (TRIM(ContourFileType).EQ."13-TAU0").OR.              &
                               (TRIM(ContourFileType).EQ."13-EVIS").OR.              &
                               (TRIM(ContourFileType).EQ."13-STARTDRY").OR.          &
                               (TRIM(ContourFileType).EQ."13-REFRAC"))THEN

                            IF(INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0)THEN
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

                                IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileType).EQ."13-MANNING"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault 

                                ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileType).EQ."13-CANOPY"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault 

                                ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                                       (INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), &
                                                        AttrWR(5), AttrWR(6), AttrWR(7), AttrWR(8), &
                                                        AttrWR(9), AttrWR(10), AttrWR(11), AttrWR(12)

                                    AttributeDefault = AttrWR(Record)

                                ELSEIF((INDEX(AttributeLabel,"primitive_weighting").GT.0).AND.&
                                       (TRIM(ContourFileType).EQ."13-TAU0"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault 

                                ELSEIF((INDEX(AttributeLabel,"eddy_viscosity").GT.0).AND.(TRIM(ContourFileType).EQ."13-EVIS"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault 

                                ELSEIF((INDEX(AttributeLabel,"surface_submergence_state").GT.0) &
     &                              .AND.(TRIM(ContourFileType).EQ."13-STARTDRY"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault 

                                ELSEIF((INDEX(AttributeLabel,"wave_refraction_in_swan").GT.0) &
     &                             .AND.(TRIM(ContourFileType).EQ."13-REFRAC"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault 

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

                                IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileType).EQ."13-MANNING"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileType).EQ."13-CANOPY"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                                       (INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), &
                                                            AttrWR(5), AttrWR(6), AttrWR(7), AttrWR(8), AttrWR(9), &
                                                            AttrWR(10), AttrWR(11), AttrWR(12)
                                        Attributes1(JunkI) = AttrWR(Record)

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"primitive_weighting").GT.0).AND.&
                                       (TRIM(ContourFileType).EQ."13-TAU0"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"eddy_viscosity").GT.0).AND.(TRIM(ContourFileType).EQ."13-EVIS"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"surface_submergence_state").GT.0) &
     &                              .AND.(TRIM(ContourFileType).EQ."13-STARTDRY"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO
                                
                                ELSEIF((INDEX(AttributeLabel,"wave_refraction_in_swan").GT.0) &
     &                               .AND.(TRIM(ContourFileType).EQ."13-REFRAC"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

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

                                IF(OptimizeContours.EQ.1)THEN
                                   IF(BdyNodes(XYZNodes(I)))THEN
                                      Z(I) = -88888.
                                   ENDIF
                                ENDIF

                                WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     


                            ENDDO

                            CLOSE(UNIT=12,STATUS="KEEP")
                            CLOSE(UNIT=19,STATUS="KEEP")

                        ENDIF

                    ELSEIF((IfPlotFilledContours.EQ.2).OR.(IfPlotContourLines.EQ.2))THEN

                        IF(TRIM(ContourFileType).EQ."ADCIRC-OUTPUT")THEN

                            WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"

                            OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                UnitFile1Opened = .FALSE.
                                INQUIRE(UNIT=19,OPENED=UnitFile1Opened)
                                IF(.NOT.UnitFile1Opened)THEN
                                    OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) NumRecs, JunkI, JunkR, JunkI, ContourFileNumCols
                                    CurrentRecord = 1
                                ENDIF
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_OPEN(TRIM(ContourFile1),NF90_NOWRITE,NC_ID1))
                                CALL Check(NF90_INQ_VARID(NC_ID1,'time',NC_Var))
                                CALL Check(NF90_INQUIRE_VARIABLE(NC_ID1,NC_Var,dimids=NC_DimIDs))
                                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_DimIDs(1),len=NumRecs))
                                CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time,start=(/Record/),count=(/1/)))
                                CurrentTime = NC_Time(1)
                                !...Grab the start and end times as well
                                CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time,start=(/1/),count=(/1/)))
                                StartTime = NC_Time(1)
                                iret = NF90_GET_ATT(NC_ID1,NF90_GLOBAL,'rnday',TotalSimTime1)
                                IF(iret.NE.NF90_NOERR)THEN
                                    TotalSimTime1 = -1D0
                                ENDIF
                                CALL CHECK(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time,start=(/NumRecs/),count=(/1/)))
                                TotalSimTime2 = NC_Time(1)/86400D0
                                TotalSimTime = MAX(TotalSimTime1,TotalSimTime2)
                                CALL FindMyNETCDFVariable(NC_ID1)
#endif
                            ENDIF
                            IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                                UnitFile2Opened = .FALSE.
                                INQUIRE(UNIT=23,OPENED=UnitFile2Opened)
                                IF(.NOT.UnitFile2Opened)THEN
                                    OPEN(UNIT=23,FILE=TRIM(ContourFile2),ACTION="READ")
                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT=*) NumRecs, JunkI, JunkR, JunkI, ContourFileNumCols
                                    CurrentRecord = 1
                                ENDIF
                            ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_OPEN(TRIM(ContourFile2),NF90_NOWRITE,NC_ID2))
                                CALL Check(NF90_INQ_VARID(NC_ID2,'time',NC_Var))
                                CALL Check(NF90_INQUIRE_VARIABLE(NC_ID2,NC_Var,dimids=NC_DimIDs))
                                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID2,NC_DimIDs(1),len=NumRecs))
                                CALL Check(NF90_GET_VAR(NC_ID2,NC_Var,NC_Time,start=(/Record/),count=(/1/)))
                                CALL FindMyNetCDFVariable(NC_ID2)
                                CurrentTime = NC_Time(1)
#endif
                            ENDIF

                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                IF(CurrentRecord.LT.Record)THEN
                                    DO J=CurrentRecord,Record-1
                                        NumNodes1 = NumNodesGlobal
                                        CALL ReadTimeStamp(19,LEN_TRIM(ContourFile1),TRIM(ContourFile1),CurrentTime,JunkR, &
                                                           NumNodes1,DefaultValue)
                                        DO I=1,NumNodes1
#ifdef SLOWREAD
                                            CALL ReadNodeVals(19,LEN_TRIM(ContourFile1),TRIM(ContourFile1),0,JunkI,JunkR1,JunkR2)
#else
                                            READ(19,*)
#endif
                                        ENDDO
                                    ENDDO
                                ENDIF
                            ENDIF
                            IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                                IF(CurrentRecord.LT.Record)THEN
                                    DO J=CurrentRecord,Record-1
                                        NumNodes2 = NumNodesMesh2
                                        CALL ReadTimeStamp(23,LEN_TRIM(ContourFile2),TRIM(ContourFile2),CurrentTime,JunkR, &
                                                           NumNodes2,DefaultValue)
                                        DO I=1,NumNodes2
#ifdef SLOWREAD
                                            CALL ReadNodeVals(23,LEN_TRIM(ContourFile2),TRIM(ContourFile2),0,JunkI,JunkR1,JunkR2)
#else
                                            READ(23,*)
#endif
                                        ENDDO
                                    ENDDO
                                ENDIF
                            ENDIF

                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                NumNodes1 = NumNodesGlobal
                                CALL ReadTimeStamp(19,LEN_TRIM(ContourFile1),TRIM(ContourFile1),CurrentTime,JunkR, &
                                                   NumNodes1,DefaultValue)
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                NumNodes1 = NumNodesGlobal
                                CALL GetNetCDFVarID(NC_ID1,NC_Var,NC_Var2,ContourFileNumCols)
                                
                                ierr = NF90_GET_ATT(NC_ID1,NC_Var,'_FillValue',DefaultValue)
                                IF(ierr.NE.NF90_NOERR)THEN
                                    CALL Check(NF90_GET_ATT(NC_ID1,NF90_GLOBAL,'_FillValue',DefaultValue))
                                ENDIF    

#endif
                            ENDIF
                            IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                                NumNodes2 = NumNodesMesh2
                                CALL ReadTimeStamp(23,LEN_TRIM(ContourFile2),TRIM(ContourFile2),CurrentTime,JunkR, &
                                                   NumNodes2,DefaultValue)
                            ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                                NumNodes2 = NumNodesMesh2 
                                CALL GetNetCDFVARID(NC_ID2,NC_Var3,NC_Var4,ContourFileNumCols2)
                                
                                ierr = NF90_GET_ATT(NC_ID2,NC_Var,'_FillValue',DefaultValue)
                                IF(ierr.NE.NF90_NOERR)THEN
                                    CALL Check(NF90_GET_ATT(NC_ID2,NF90_GLOBAL,'_FillValue',DefaultValue))
                                ENDIF    

#endif
                            ENDIF

                            IF(.NOT.ALLOCATED(U1))    ALLOCATE(U1(1:NumNodesGlobal))
                            IF(.NOT.ALLOCATED(U2))    ALLOCATE(U2(1:NumNodesMesh2))
                            IF(.NOT.ALLOCATED(V1))    ALLOCATE(V1(1:NumNodesGlobal))
                            IF(.NOT.ALLOCATED(V2))    ALLOCATE(V2(1:NumNodesMesh2))
                            IF(.NOT.ALLOCATED(Vels1)) ALLOCATE(Vels1(1:NumNodesGlobal))
                            IF(.NOT.ALLOCATED(Vels2)) ALLOCATE(Vels2(1:NumNodesMesh2))

                            IF(INDEX(TRIM(ContourFile1),"64").GT.0)THEN
                                U1(:)    = -99999.0
                                U2(:)    = -99999.0
                                V1(:)    = -99999.0
                                V2(:)    = -99999.0
                                Vels1(:) = -99999.0
                                Vels2(:) = -99999.0
                            ELSEIF(DefaultValue.GT.-99998.0)THEN
                                U1(:)    = DefaultValue * ContourConversionFactor
                                U2(:)    = DefaultValue * ContourConversionFactor
                                V1(:)    = DefaultValue * ContourConversionFactor
                                V2(:)    = DefaultValue * ContourConversionFactor
                                Vels1(:) = DefaultValue * ContourConversionFactor
                                Vels2(:) = DefaultValue * ContourConversionFactor
                            ELSE
                                U1(:)    = DefaultValue
                                U2(:)    = DefaultValue
                                V1(:)    = DefaultValue
                                V2(:)    = DefaultValue
                                Vels1(:) = DefaultValue
                                Vels2(:) = DefaultValue
                            ENDIF

                            IF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                
                                IF(ContourFileNumCols.EQ.1)THEN
                                    CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,VARID1=NC_Var,VEC1=U1,&
                                                              Record=Record)
                                ELSE
                                    CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,VARID1=NC_Var,VARID2=NC_Var2,&
                                                              VEC1=U1,VEC2=V1,Record=Record)
                                ENDIF
#endif
                            ENDIF
                            IF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF

                                IF(ContourFileNumCols2.EQ.1)THEN
                                    CALL ReadMyNetCDFVariable(NCID=NC_ID2,NUMNODES=NumNodesMesh2,VARID1=NC_Var3,VEC1=U2,&
                                                              Record=Record)
                                ELSE
                                    CALL ReadMyNetCDFVariable(NCID=NC_ID2,NUMNODES=NumNodesMesh2,VARID1=NC_Var3,VARID2=NC_Var4,&
                                                              VEC1=U2,VEC2=V2,Record=Record)
                                ENDIF

#endif
                            ENDIF

                            DO I=1,NumNodes1

                                IF(ContourFileNumCols.EQ.1)THEN

                                    IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
#ifdef SLOWREAD
                                        CALL ReadNodeVals(19,LEN_TRIM(ContourFile1),TRIM(ContourFile1),1,JunkI,JunkR1,JunkR2)
#else
                                        READ(19,*) JunkI,JunkR1
#endif
                                        U1(JunkI) = JunkR1
                                        IF(U1(JunkI).GT.-99998.0)THEN
                                            U1(JunkI) = U1(JunkI) * ContourConversionFactor
                                        ENDIF
                                        Vels1(JunkI) = U1(JunkI)
                                    ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                        IF(U1(I).GT.-99998.0)THEN
                                            U1(I) = U1(I) * ContourConversionFactor
                                        ENDIF
                                        Vels1(I) = U1(I)
#endif
                                    ENDIF

                                ELSEIF(ContourFileNumCols.EQ.2)THEN

                                    IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
#ifdef SLOWREAD
                                        CALL ReadNodeVals(19,LEN_TRIM(ContourFile1),TRIM(ContourFile1),2,JunkI,JunkR1,JunkR2)
#else
                                        READ(19,*) JunkI,JunkR1,JunkR2
#endif
                                        U1(JunkI) = JunkR1
                                        V1(JunkI) = JunkR2
                                        Vels1(JunkI) = ContourConversionFactor * SQRT(U1(JunkI)*U1(JunkI)+V1(JunkI)*V1(JunkI))
#ifdef DRYZEROVEL                                            
                                        IF(Vels1(JunkI).EQ.0D0)Vels1(JunkI)=-99999D0
#endif                                        
                                    ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                        Vels1(I) = ContourConversionFactor * SQRT(U1(I)*U1(I)+V1(I)*V1(I))
#ifdef DRYZEROVEL                                            
                                        IF(Vels1(I).EQ.0D0)Vels1(I)=-99999D0
#endif                                        
#endif
                                    ENDIF

                                ENDIF

                            ENDDO

                            DO I=1,NumNodes2

                                IF(ContourFileNumCols.EQ.1)THEN

                                    IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
#ifdef SLOWREAD
                                        CALL ReadNodeVals(23,LEN_TRIM(ContourFile2),TRIM(ContourFile2),1,JunkI,JunkR1,JunkR2)
#else
                                        READ(23,*) JunkI,JunkR1
#endif
                                        U2(JunkI) = JunkR1
                                        IF(U2(JunkI).GT.-99998.0)THEN
                                            U2(JunkI) = U2(JunkI) * ContourConversionFactor
                                        ENDIF
                                        Vels2(JunkI) = U2(JunkI)
                                    ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                                        IF(U2(I).GT.-99998.0)THEN
                                            U2(I) = U2(I) * ContourConversionFactor
                                        ENDIF
                                        Vels2(I) = U2(I)
#endif
                                    ENDIF

                                ELSEIF(ContourFileNumCols.EQ.2)THEN

                                    IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN 
#ifdef SLOWREAD
                                        CALL ReadNodeVals(23,LEN_TRIM(ContourFile2),TRIM(ContourFile2),2,JunkI,JunkR1,JunkR2)
#else
                                        READ(23,*) JunkI,JunkR1,JunkR2
#endif
                                        U2(JunkI) = JunkR1
                                        V2(JunkI) = JunkR2
                                        Vels2(JunkI) = ContourConversionFactor * SQRT(U2(JunkI)*U2(JunkI)+V2(JunkI)*V2(JunkI))
#ifdef DRYZEROVEL                                            
                                        IF(Vels1(JunkI).EQ.0D0)Vels1(JunkI)=-99999D0
#endif                                        
                                    ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN 
#ifdef NETCDF
                                        Vels2(I) = ContourConversionFactor * SQRT(U2(I)*U2(I)+V2(I)*V2(I))
#ifdef DRYZEROVEL                                            
                                        IF(Vels2(I).EQ.0D0)Vels2(I)=-99999D0
#endif                                        
#endif
                                    ENDIF

                                ENDIF

                            ENDDO

                            DO I=1,NumNodesLocal

                                IF(IfPlotBackgroundImages.NE.2)THEN
#ifdef DEPTHDIFF                                
                                    IF((Vels1(XYZNodes(I)).LT.-90000D0).AND.&
                                       (Vels2(TranslationTable(XYZNodes(I))).GT.-90000D0))THEN
                                        Z(I) = BathLocal(I) - Vels2(TranslationTable(XYZNodes(I)))
                                    ELSEIF((Vels1(XYZNodes(I)).GT.-90000D0).AND.&
                                       (Vels2(TranslationTable(XYZNodes(I))).LT.-90000D0))THEN
                                        Z(I) = Vels1(XYZNodes(I)) - BathLocal(I) 
                                    ELSEIF((Vels1(XYZNodes(I)).LT.-90000D0).AND.&
                                       (Vels2(TranslationTable(XYZNodes(I))).LT.-90000D0))THEN
#ifdef DRYDIFF
                                        Z(I) = -99999D0
#else                                    
                                        Z(I) = 0.0d0
#endif
                                    ELSE
                                        Z(I) = Vels1(XYZNodes(I)) - Vels2(TranslationTable(XYZNodes(I)))
                                    ENDIF
#elif defined(DRYDIFF)
                                    IF((Vels1(XYZNodes(I)).LT.-90000D0).AND.(Vels2(TranslationTable(XYZNodes(I))).LT.-90000D0))THEN
                                        Z(I) = -99999D0
                                    ELSE
                                        Z(I) = Vels1(XYZNodes(I)) - Vels2(TranslationTable(XYZNodes(I)))
                                    ENDIF
#else
                                    Z(I) = Vels1(XYZNodes(I)) - Vels2(TranslationTable(XYZNodes(I)))
#endif                                    
                                ELSE
                                    !...Set up a range (1/16 of the difference range) that is unplotted
                                    POW = -LOG10((ContourMax-ContourMin)/16D0)
                                    POW_I = INT(POW)+1
                                    TRUNC = 10D0**DBLE(POW_I)
                                    Z(I) = Vels1(XYZNodes(I)) - Vels2(TranslationTable(XYZNodes(I)))
                                    IF(Z(I).LE.-90000)THEN
                                        Z(I) = -99999D0
                                    ELSEIF(ABS(DBLE(INT(Z(I)*TRUNC))/TRUNC).LE.0D0)THEN
                                        Z(I) = -99999D0
                                    ENDIF    
                                ENDIF

                                IF(OptimizeContours.EQ.1)THEN
                                   IF(BdyNodes(XYZNodes(I)))THEN
                                      Z(I) = -88888.
                                   ENDIF
                                ENDIF

                                WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                            ENDDO

                            CLOSE(UNIT=12,STATUS="KEEP")
                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                CONTINUE
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_CLOSE(NC_ID1))
#endif
                            ENDIF
                            IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                                CONTINUE
                            ELSEIF(TRIM(ContourFileFormat2).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_CLOSE(NC_ID2))
#endif
                            ENDIF

                        ELSEIF(TRIM(ContourFileType).EQ."GRID-BATH")THEN

                            WRITE(UNIT=ContourXYZFile,FMT='(A,A,I4.4,A)') TRIM(ContourFile1), ".", Record, ".xyz"

                            OPEN(UNIT=12,FILE=TRIM(TempPath)//TRIM(ContourXYZFile),ACTION="WRITE")
                            OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                            OPEN(UNIT=23,FILE=TRIM(ContourFile2),ACTION="READ")

                            READ(UNIT=19,FMT='(A)') JunkC
                            READ(UNIT=19,FMT=*) JunkI, NumNodes1
                            READ(UNIT=23,FMT='(A)') JunkC
                            READ(UNIT=23,FMT=*) JunkI, NumNodes2

!                            IF(NumNodes1.NE.NumNodes2)THEN
!                                STOP "FATAL ERROR: The number of nodes should be the same in the two grids."
!                            ENDIF

                            ALLOCATE(Bath1(1:NumNodes1))
                            ALLOCATE(Bath2(1:NumNodes2))

                            DO I=1,NumNodes1
                                READ(UNIT=19,FMT=*) JunkI, JunkR, JunkR, Bath1(I)
                            ENDDO
                            DO I=1,NumNodes2
                                READ(UNIT=23,FMT=*) JunkI, JunkR, JunkR, Bath2(I)
                            ENDDO
                            
                            DO I=1,NumNodesLocal

                                Z(I) = (Bath1(XYZNodes(I)) - Bath2(TranslationTable(XYZNodes(I)))) * ContourConversionFactor

                                IF(OptimizeContours.EQ.1)THEN
                                   IF(BdyNodes(XYZNodes(I)))THEN
                                      Z(I) = -88888.
                                   ENDIF
                                ENDIF

                                WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                            ENDDO

                            CLOSE(UNIT=12,STATUS="KEEP")
                            CLOSE(UNIT=19,STATUS="KEEP")
                            CLOSE(UNIT=23,STATUS="KEEP")

                        ELSEIF((TRIM(ContourFileType).EQ."13-MANNING").OR.           &
                               (INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0).OR. &
                               (TRIM(ContourFileType).EQ."13-CANOPY").OR.            &
                               (TRIM(ContourFileType).EQ."13-TAU0").OR.              &
                               (TRIM(ContourFileType).EQ."13-EVIS").OR.              &
                               (TRIM(ContourFileType).EQ."13-STARTDRY").OR.          &
                               (TRIM(ContourFileType).EQ."13-REFRAC"))THEN

                            IF(INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0)THEN
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

!                            IF(NumNodes1.NE.NumNodes2)THEN
!                                WRITE(*,'(A)') "FATAL ERROR: The 13 files do not have the same number of nodes."
!                                STOP
!                            ENDIF

                            DO I=1,NumAttributes1

                                READ(UNIT=19,FMT='(A)') AttributeLabel

                                IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileType).EQ."13-MANNING"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault

                                ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.&
                                       (TRIM(ContourFileType).EQ."13-CANOPY"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault

                                ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                                       (INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), &
                                                        AttrWR(5), AttrWR(6), AttrWR(7), AttrWR(8), &
                                                        AttrWR(9), AttrWR(10), AttrWR(11), AttrWR(12)

                                    AttributeDefault = AttrWR(Record)

                                ELSEIF((INDEX(AttributeLabel,"primitive_weighting").GT.0).AND.&
                                       (TRIM(ContourFileType).EQ."13-TAU0"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault

                                ELSEIF((INDEX(AttributeLabel,"eddy_viscosity").GT.0).AND.&
                                       (TRIM(ContourFileType).EQ."13-EVIS"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault

                                ELSEIF((INDEX(AttributeLabel,"surface_submergence_state").GT.0) &
     &                               .AND.(TRIM(ContourFileType).EQ."13-STARTDRY"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault

                                ELSEIF((INDEX(AttributeLabel,"wave_refraction_in_swan").GT.0) &
     &                               .AND.(TRIM(ContourFileType).EQ."13-REFRAC"))THEN

                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT='(A)') JunkC
                                    READ(UNIT=19,FMT=*) AttributeDefault

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

                                IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileType).EQ."13-MANNING"))THEN

                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT=*) AttributeDefault

                                ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileType).EQ."13-CANOPY"))THEN

                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT=*) AttributeDefault 

                                ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                                       (INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0))THEN

                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT=*) AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), &
                                                        AttrWR(5), AttrWR(6), AttrWR(7), AttrWR(8), &
                                                        AttrWR(9), AttrWR(10), AttrWR(11), AttrWR(12)

                                    AttributeDefault = AttrWR(Record)

                                ELSEIF((INDEX(AttributeLabel,"primitive_weighting").GT.0).AND.&
                                        (TRIM(ContourFileType).EQ."13-TAU0"))THEN

                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT=*) AttributeDefault 

                                ELSEIF((INDEX(AttributeLabel,"eddy_viscosity").GT.0).AND.&
                                       (TRIM(ContourFileType).EQ."13-EVIS"))THEN

                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT=*) AttributeDefault 

                                ELSEIF((INDEX(AttributeLabel,"surface_submergence_state").GT.0) &
     &                              .AND.(TRIM(ContourFileType).EQ."13-STARTDRY"))THEN

                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT=*) AttributeDefault 

                                ELSEIF((INDEX(AttributeLabel,"wave_refraction_in_swan").GT.0) &
     &                               .AND.(TRIM(ContourFileType).EQ."13-REFRAC"))THEN

                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT='(A)') JunkC
                                    READ(UNIT=23,FMT=*) AttributeDefault 

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

                                IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileType).EQ."13-MANNING"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileType).EQ."13-CANOPY"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                                       (INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, AttrWR(1), AttrWR(2), AttrWR(3), AttrWR(4), AttrWR(5), &
                                                            AttrWR(6), AttrWR(7), AttrWR(8), AttrWR(9), AttrWR(10), &
                                                            AttrWR(11), AttrWR(12)
                                        Attributes1(JunkI) = AttrWR(Record)

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"primitive_weighting").GT.0).AND.&
                                       (TRIM(ContourFileType).EQ."13-TAU0"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"eddy_viscosity").GT.0).AND.&
                                       (TRIM(ContourFileType).EQ."13-EVIS"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"surface_submergence_state").GT.0) &
     &                               .AND.(TRIM(ContourFileType).EQ."13-STARTDRY"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"wave_refraction_in_swan").GT.0) &
     &                                .AND.(TRIM(ContourFileType).EQ."13-REFRAC"))THEN

                                    READ(UNIT=19,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=19,FMT=*) JunkI, JunkR
                                        Attributes1(JunkI) = JunkR

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

                                IF((INDEX(AttributeLabel,"mannings_n").GT.0).AND.(TRIM(ContourFileType).EQ."13-MANNING"))THEN

                                    READ(UNIT=23,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=23,FMT=*) JunkI, JunkR
                                        Attributes2(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"canopy").GT.0).AND.(TRIM(ContourFileType).EQ."13-CANOPY"))THEN

                                    READ(UNIT=23,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=23,FMT=*) JunkI, JunkR
                                        Attributes2(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"surface_directional").GT.0).AND. &
                                       (INDEX(ContourFileType,"13-WIND-REDUCTION").GT.0))THEN

                                    READ(UNIT=23,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=23,FMT=*) JunkI, AttrWR(1), AttrWR(2), AttrWR(3), &
                                                            AttrWR(4), AttrWR(5), AttrWR(6), &
                                                            AttrWR(7), AttrWR(8), AttrWR(9), &
                                                            AttrWR(10), AttrWR(11), AttrWR(12)
                                        Attributes2(JunkI) = AttrWR(Record)

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"primitive_weighting").GT.0).AND.&
                                       (TRIM(ContourFileType).EQ."13-TAU0"))THEN

                                    READ(UNIT=23,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=23,FMT=*) JunkI, JunkR
                                        Attributes2(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"eddy_viscosity").GT.0).AND.(TRIM(ContourFileType).EQ."13-EVIS"))THEN

                                    READ(UNIT=23,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=23,FMT=*) JunkI, JunkR
                                        Attributes2(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"surface_submergence_state").GT.0) &
     &                               .AND.(TRIM(ContourFileType).EQ."13-STARTDRY"))THEN

                                    READ(UNIT=23,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=23,FMT=*) JunkI, JunkR
                                        Attributes2(JunkI) = JunkR

                                    ENDDO

                                ELSEIF((INDEX(AttributeLabel,"wave_refraction_in_swan").GT.0) &
     &                                .AND.(TRIM(ContourFileType).EQ."13-REFRAC"))THEN

                                    READ(UNIT=23,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=23,FMT=*) JunkI, JunkR
                                        Attributes2(JunkI) = JunkR

                                    ENDDO

                                ELSE

                                    READ(UNIT=23,FMT=*) NumNonDefault

                                    DO J=1,NumNonDefault

                                        READ(UNIT=23,FMT='(A)') JunkC

                                    ENDDO

                                ENDIF

                            ENDDO

                            DO I=1,NumNodesLocal

                                Z(I) = (Attributes1(XYZNodes(I))-Attributes2(TranslationTable(XYZNodes(I)))) * &
                                        ContourConversionFactor

                                IF(OptimizeContours.EQ.1)THEN
                                   IF(BdyNodes(XYZNodes(I)))THEN
                                      Z(I) = -88888.
                                   ENDIF
                                ENDIF

                                WRITE(UNIT=12,FMT='(3(2X,F16.8))') X(I), Y(I), Z(I)     

                            ENDDO

                            CLOSE(UNIT=12,STATUS="KEEP")
                            CLOSE(UNIT=19,STATUS="KEEP")
                            CLOSE(UNIT=23,STATUS="KEEP")

                        ENDIF

                    ENDIF

                ENDIF

                IF(IfPlotParticles.GT.0)THEN

                    WRITE(UNIT=ParticleXYZFile,FMT='(A,A,I4.4,A)') TRIM(ParticleFile),".",Record,".xy"
                    OPEN(UNIT=41,FILE=TRIM(TempPath)//TRIM(ParticleXYZFile),ACTION="WRITE")
                    IF(TRIM(ParticleFileFormat).EQ."ASCII")THEN
                        ParticleFileOpened = 0
                        UnitFile1Opened = .FALSE.
                        INQUIRE(UNIT=40,OPENED=UnitFile1Opened)
                        IF(UnitFile1Opened)THEN
                            INQUIRE(UNIT=40,NAME=UnitFile1)
                            IF(INDEX(TRIM(UnitFile1),TRIM(ParticleFile)).GT.0)THEN
                                ParticleFileOpened = 1
                            ENDIF
                        ENDIF
                        IF(ParticleFileOpened.EQ.0)THEN
                            OPEN(UNIT=40,FILE=TRIM(ParticleFile),ACTION="READ")
                            READ(40,'(A)') JunkC
                            READ(40,*) NumRecs,NumParticles
                            CurrentRecord = 1
                        ENDIF
                        IF(CurrentRecord.LT.Record)THEN
                            DO J=CurrentRecord,Record-1
                                READ(40,*) JunkR1,NumParticles
                                DO I=1,NumParticles
                                    READ(40,*) JunkI,JunkR1,JunkR2
                                ENDDO
                            ENDDO
                        ENDIF
                        READ(40,*) JunkR1,NumParticles
                        DO I=1,NumParticles
                            READ(40,*) JunkI,JunkR1,JunkR2
                            IF(MOD(I,IfPlotParticles).EQ.0)THEN
                                WRITE(41,FMT='(2(2X,F16.8))') JunkR1,JunkR2
                            ENDIF
                        ENDDO
#ifdef NETCDF
                    ELSEIF(TRIM(ParticleFileFormat).EQ."NETCDF")THEN
                        CALL Check(NF90_OPEN(TRIM(ParticleFile),NF90_NOWRITE,NC_ID1))
                        CALL Check(NF90_INQ_VARID(NC_ID1,'Time_index',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_JunkR,start=(/Record/), &
                                   count=(/1/)))
                        CurrentTime = NC_JunkR(1)
                        CALL Check(NF90_INQ_VARID(NC_ID1,'Particles_per_time_snap',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_JunkI,start=(/Record/), &
                                   count=(/1/)))
                        NumParticles = NC_JunkI(1)
                        IF(.NOT.ALLOCATED(ParticleLon)) ALLOCATE(ParticleLon(1:NumParticles))
                        CALL Check(NF90_INQ_VARID(NC_ID1,'X_particle',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,ParticleLon,start=(/1,Record/), &
                                   count=(/NumParticles,1/)))
                        IF(.NOT.ALLOCATED(ParticleLat)) ALLOCATE(ParticleLat(1:NumParticles))
                        CALL Check(NF90_INQ_VARID(NC_ID1,'Y_particle',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,ParticleLat,start=(/1,Record/), &
                                   count=(/NumParticles,1/)))
                        IF(.NOT.ALLOCATED(ParticleFlag)) ALLOCATE(ParticleFlag(1:NumParticles))
                        CALL Check(NF90_INQ_VARID(NC_ID1,'Z_particle',NC_Var))
                        CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,ParticleFlag,start=(/1,Record/), &
                                   count=(/NumParticles,1/)))
                        CALL Check(NF90_CLOSE(NC_ID1))
                        DO I=1,NumParticles
                            IF(MOD(I,IfPlotParticles).EQ.0)THEN
                                IF(UseParticlePalette)THEN
                                    WRITE(41,FMT='(3(2X,F16.8))') ParticleLon(I),ParticleLat(I),ParticleFlag(I)
                                ELSE
                                    WRITE(41,FMT='(2(2X,F16.8))') ParticleLon(I),ParticleLat(I)
                                ENDIF
                            ENDIF
                        ENDDO
#endif
                    ENDIF
                    CLOSE(UNIT=41,STATUS="KEEP")

                ENDIF

                IF(IfPlotVectors.EQ.1)THEN

                    WRITE(UNIT=VectorUFile,FMT='(A,A,I4.4,A)') TRIM(VectorFile),".",Record,".u"
                    WRITE(UNIT=VectorVFile,FMT='(A,A,I4.4,A)') TRIM(VectorFile),".",Record,".v"
                    OPEN(UNIT=17,FILE=TRIM(TempPath)//TRIM(VectorUFile)//".xyz",ACTION="WRITE")
                    OPEN(UNIT=18,FILE=TRIM(TempPath)//TRIM(VectorVFile)//".xyz",ACTION="WRITE")

                    IF(TRIM(VectorFileType).EQ."ADCIRC-OUTPUT")THEN

                    IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                        VectorFileIsContourFile = 0
                        UnitFile1Opened = .FALSE.
                        INQUIRE(UNIT=19,OPENED=UnitFile1Opened)
                        IF(UnitFile1Opened)THEN
                            INQUIRE(UNIT=19,NAME=UnitFile1)
                            IF(INDEX(TRIM(UnitFile1),TRIM(VectorFile)).GT.0)THEN
                                VectorFileIsContourFile = 1
                            ENDIF
                        ENDIF
                        UnitFile2Opened = .FALSE.
                        INQUIRE(UNIT=23,OPENED=UnitFile2Opened)
                        IF(UnitFile2Opened)THEN
                            INQUIRE(UNIT=23,NAME=UnitFile2)
                            IF(INDEX(TRIM(UnitFile2),TRIM(VectorFile)).GT.0)THEN
                                VectorFileIsContourFile = 2
                            ENDIF
                        ENDIF
                        IF(VectorFileIsContourFile.EQ.0)THEN
                            VectorFileOpened = 0
                            UnitFile1Opened = .FALSE.
                            INQUIRE(UNIT=20,OPENED=UnitFile1Opened)
                            IF(UnitFile1Opened)THEN
                                INQUIRE(UNIT=20,NAME=UnitFile1)
                                IF(INDEX(TRIM(UnitFile1),TRIM(VectorFile)).GT.0)THEN
                                    VectorFileOpened = 1
                                ENDIF
                            ENDIF
                        ENDIF
                        IF((VectorFileIsContourFile.EQ.0).AND.(VectorFileOpened.EQ.0))THEN
                            OPEN(UNIT=20,FILE=TRIM(VectorFile),ACTION="READ")
                            READ(UNIT=20,FMT='(A)') JunkC
                            READ(UNIT=20,FMT=*) NumRecs, JunkI, JunkR, JunkI, VectorFileNumCols
                            CurrentRecord = 1
                            IF(VectorFileNumCols.EQ.1)THEN
                                PlotVectorScale = .FALSE.
                            ENDIF
                        ENDIF
                    ELSEIF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                        CALL Check(NF90_OPEN(TRIM(VectorFile),NF90_NOWRITE,NC_ID1))
                        CALL Check(NF90_INQ_VARID(NC_ID1,'time',NC_Var))
                        CALL Check(NF90_INQUIRE_VARIABLE(NC_ID1,NC_Var,dimids=NC_DimIDs))
                        CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_DimIDs(1),len=NumRecs))
                        CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time,start=(/Record/),count=(/1/)))
                        CurrentTime = NC_Time(1)
                        CALL FindMyNETCDFVariable(NC_ID1,VECTOR=.TRUE.,NumCols=VectorFileNumCols)
                        VectorFileIsContourFile = 0
#endif
                    ENDIF

                    IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                        IF(VectorFileIsContourFile.EQ.0)THEN
                            IF(CurrentRecord.LT.Record)THEN
                                WRITE(*,*) CurrentRecord,Record
                                DO J=CurrentRecord,Record-1
                                    NumNodes1 = NumNodesGlobal
                                    CALL ReadTimeStamp(20,LEN_TRIM(VectorFile),TRIM(VectorFile),CurrentTime,JunkR,NumNodes1,&
                                                        DefaultValue)
                                    DO I=1,NumNodes1
#ifdef SLOWREAD
                                        CALL ReadNodeVals(20,LEN_TRIM(VectorFile),TRIM(VectorFile),0,JunkI,JunkR1,JunkR2)
#else
                                        READ(20,*)
#endif
                                    ENDDO
                                ENDDO
                            ENDIF
                        ENDIF
                    ENDIF

                    IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                        IF(VectorFileIsContourFile.EQ.0)THEN
                            NumNodes1 = NumNodesGlobal
                            CALL ReadTimeStamp(20,LEN_TRIM(VectorFile),TRIM(VectorFile),CurrentTime,JunkR,NumNodes1,DefaultValue)
                        ELSE
                            NumNodes1 = NumNodesGlobal
                        ENDIF
                    ELSEIF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                        NumNodes1 = NumNodesGlobal
                        CALL GetNETCDFVarID(NC_ID1,NC_Var,NC_Var2,VectorFileNumCols)
                        IF(VectorFileNumCols.EQ.1)THEN
                            PlotVectorScale = .FALSE.
                        ENDIF

                        ierr = NF90_GET_ATT(NC_ID1,NC_Var,'_FillValue',DefaultValue)
                        IF(ierr.NE.NF90_NOERR)THEN
                            CALL Check(NF90_GET_ATT(NC_ID1,NF90_GLOBAL,'_FillValue',DefaultValue))
                        ENDIF
#endif
                    ENDIF

                    IF((VectorFileIsContourFile.EQ.0).OR. &
                      ((IfPlotFilledContours.EQ.0).AND.(IfPlotContourLines.EQ.0)))THEN
                        DefaultValue = DefaultValue * VectorConversionFactor
                        IF(.NOT.ALLOCATED(U1)) ALLOCATE(U1(1:NumNodesGlobal))
                        IF(.NOT.ALLOCATED(V1)) ALLOCATE(V1(1:NumNodesGlobal))
                        DO I=1,NumNodesGlobal
                            U1(I) = DefaultValue
                            V1(I) = DefaultValue
                        ENDDO
                    ELSEIF(VectorFileIsContourFile.EQ.1)THEN
                        WHERE(U1.LT.-99998.0)
                            U1 = DefaultValue
                        ENDWHERE
                        WHERE(V1.LT.-99998.0)
                            V1 = DefaultValue
                        ENDWHERE
                    ELSEIF(VectorFileIsContourFile.EQ.2)THEN
                        WHERE(U2.LT.-99998.0)
                            U2 = DefaultValue
                        ENDWHERE
                        WHERE(V2.LT.-99998.0)
                            V2 = DefaultValue
                        ENDWHERE
                    ENDIF

                    IF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                        IF(VectorFileNumCols.EQ.1)THEN
                            CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,VARID1=NC_Var,VEC1=U1,RECORD=Record)
                            DO I = 1,NumNodes1
                                !...Lets assume this is a direction, and
                                !   split into components of a unit vector
                                IF(U1(I).GT.-99990D0.AND.U1(I).NE.0D0)THEN
                                    V1(I) = SIN(U1(I)*DEG2RAD)
                                    U1(I) = COS(U1(I)*DEG2RAD)
                                ELSE
                                    U1(I) = 0D0
                                    V1(I) = 0D0
                                ENDIF
                            ENDDO
                        ELSE
                            CALL ReadMyNetCDFVariable(NCID=NC_ID1,NUMNODES=NumNodesGlobal,VARID1=NC_Var,VARID2=NC_Var2,&
                                                      VEC1=U1,VEC2=V1,RECORD=Record)
                        ENDIF
#endif
                    ENDIF

                    IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                        DO I=1,NumNodes1
                            IF(VectorFileIsContourFile.EQ.0)THEN
#ifdef SLOWREAD
                                IF(VectorFileNumCols.EQ.1)THEN
                                    CALL ReadNodeVals(20,LEN_TRIM(VectorFile),TRIM(VectorFile),1,JunkI,JunkR1,JunkR2)
                                ELSE
                                    CALL ReadNodeVals(20,LEN_TRIM(VectorFile),TRIM(VectorFile),2,JunkI,JunkR1,JunkR2)
                                ENDIF
#else
                                IF(VectorFileNumCols.EQ.1)THEN
                                    READ(20,*) JunkI,JunkR1
                                ELSE
                                    READ(20,*) JunkI,JunkR1,JunkR2
                                ENDIF
#endif
                                IF(VectorFileNumCols.EQ.1)THEN
                                    !...Lets assume this is a direction, and
                                    !   split into components of a unit vector
                                    !   If it isn't a direction, the user made
                                    !   a boo-boo, and shall pay with an ugly 
                                    !   plot.
                                    IF(JunkR1.GT.-99990.AND.JunkR1.NE.0D0)THEN
                                        U1(JunkI) = COS(JunkR1*(DEG2RAD))
                                        V1(JunkI) = SIN(JunkR1*(DEG2RAD))
                                    ELSE
                                        U1(JunkI) = 0D0
                                        V1(JunkI) = 0D0
                                    ENDIF
                                ELSE
                                    U1(JunkI) = JunkR1
                                    V1(JunkI) = JunkR2
                                ENDIF
                            ELSEIF(VectorFileIsContourFile.EQ.1)THEN
                                CONTINUE
                            ELSEIF(VectorFileIsContourFile.EQ.2)THEN
                                U1(I) = U2(I)
                                V1(I) = V2(I)
                            ENDIF
                        ENDDO
                    ENDIF
                    
                    DO I=1,NumNodesLocal
                        WRITE(UNIT=17,FMT='(3(2X,F16.8))') X(I), Y(I), U1(XYZNodes(I)) * VectorConversionFactor
                        WRITE(UNIT=18,FMT='(3(2X,F16.8))') X(I), Y(I), V1(XYZNodes(I)) * VectorConversionFactor
                    ENDDO

                    
                    
                    ELSEIF(TRIM(VectorFileType).EQ."OWI-WIND")THEN
                        CALL ReadOWISnap("WIND",Record,VectorFile,OWI_XYZUV)
                        DO I = 1,SIZE(OWI_XYZUV(:,1))
                            WRITE(UNIT=17,FMT='(3(2X,F16.8))') OWI_XYZUV(I,1),OWI_XYZUV(I,2),OWI_XYZUV(I,4)*VectorConversionFactor
                            WRITE(UNIT=18,FMT='(3(2X,F16.8))') OWI_XYZUV(I,1),OWI_XYZUV(I,2),OWI_XYZUV(I,5)*VectorConversionFactor
                        ENDDO    
                    ENDIF

                    CLOSE(UNIT=17,STATUS="KEEP")
                    CLOSE(UNIT=18,STATUS="KEEP")
                    IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                        CONTINUE
                    ELSEIF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                        CALL Check(NF90_CLOSE(NC_ID1))
#endif
                    ENDIF

                    IF(VectorSpacing<1.0)THEN
                        WRITE(UNIT=VectorSpacingC,FMT='(F8.6)') VectorSpacing
                    ELSEIF(VectorSpacing<10.0)THEN
                        WRITE(UNIT=VectorSpacingC,FMT='(F8.6)') VectorSpacing
                    ELSEIF(VectorSpacing<100.0)THEN
                        WRITE(UNIT=VectorSpacingC,FMT='(F9.6)') VectorSpacing
                    ELSEIF(VectorSpacing<1000.0)THEN
                        WRITE(UNIT=VectorSpacingC,FMT='(F9.4)') VectorSpacing
                    ELSE
                        WRITE(UNIT=VectorSpacingC,FMT='(F8.3)') VectorSpacing
                    ENDIF

                    WRITE(UNIT=XMaxBuf,FMT=1236) LongE+LatLonBuffer
                    WRITE(UNIT=XMinBuf,FMT=1236) LongW-LatLonBuffer
                    WRITE(UNIT=YMaxBuf,FMT=1236) LatN+LatLonBuffer
                    WRITE(UNIT=YMinBuf,FMT=1236) LatS-LatLonBuffer

                    CALL SYSTEM(TRIM(Path)//"xyz2grd "//TRIM(TempPath)//TRIM(VectorUFile)//".xyz"//     &
                            " -G"//TRIM(TempPath)//TRIM(VectorUFile)//".grd -I"//TRIM(VectorSpacingC)// &
                            TRIM(VectorUnits)//                                                         &
                            " -R"//TRIM(ADJUSTL(XMinBuf))//"/"//TRIM(ADJUSTL(XMaxBuf))//"/"             &
                            //TRIM(ADJUSTL(YMinBuf))//"/"//TRIM(ADJUSTL(YMaxBuf))//                     &
                            " -N0.0")
                            
                    CALL SYSTEM(TRIM(Path)//"xyz2grd "//TRIM(TempPath)//TRIM(VectorVFile)//".xyz"//     &
                            " -G"//TRIM(TempPath)//TRIM(VectorVFile)//".grd -I"//TRIM(VectorSpacingC)// &
                            TRIM(VectorUnits)//                                                         &
                            " -R"//TRIM(ADJUSTL(XMinBuf))//"/"//TRIM(ADJUSTL(XMaxBuf))//"/"             &
                            //TRIM(ADJUSTL(YMinBuf))//"/"//TRIM(ADJUSTL(YMaxBuf))//                     &
                            " -N0.0")

                ENDIF

                CurrentRecord = Record + 1

                IF(ALLOCATED(Attributes1)) DEALLOCATE(Attributes1)
                IF(ALLOCATED(Attributes2)) DEALLOCATE(Attributes2)
                IF(ALLOCATED(AttrWR))      DEALLOCATE(AttrWR)
                IF(ALLOCATED(Bath1))       DEALLOCATE(Bath1)
                IF(ALLOCATED(Bath2))       DEALLOCATE(Bath2)
                IF(ALLOCATED(Count))       DEALLOCATE(Count)
                IF(ALLOCATED(Lat))         DEALLOCATE(Lat)
                IF(ALLOCATED(Lon))         DEALLOCATE(Lon)
                IF(ALLOCATED(NC))          DEALLOCATE(NC)
                IF(ALLOCATED(Sizes))       DEALLOCATE(Sizes)
                IF(ALLOCATED(U1))          DEALLOCATE(U1)
                IF(ALLOCATED(U2))          DEALLOCATE(U2)
                IF(ALLOCATED(V1))          DEALLOCATE(V1)
                IF(ALLOCATED(V2))          DEALLOCATE(V2)
                IF(ALLOCATED(Vels1))       DEALLOCATE(Vels1)
                IF(ALLOCATED(Vels2))       DEALLOCATE(Vels2)

                IF(Verbose.GE.3)THEN
                    WRITE(*,'(A,I4.4,A,I4.4,A)') "Core ",MyRank," wrote the XYZ files for record ",Record,"."
                ENDIF

        END SUBROUTINE 



        SUBROUTINE Finisher(Flag)

                

                IMPLICIT NONE

                CHARACTER(LEN=100) :: EdgeFileName

                INTEGER            :: IN
                INTEGER            :: Flag

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
                            DO IN=1,NumEdgeFiles
                               WRITE(UNIT=EdgeFileName,FMT='(A,I3.3,A)') TRIM(TempPath)//TRIM(Fort14File)//".edges.",IN,".xy"
                               OPEN(UNIT=31,FILE=TRIM(EdgeFileName),ACTION="WRITE")
                               CLOSE(UNIT=31,STATUS="DELETE")
                            ENDDO
                        ENDIF

                        CALL SYSTEM("rm -f "//TRIM(TempPath)//"/XYZFileNames*")
                        CALL SYSTEM("rm -f "//TRIM(TempPath)//"/GoogleExtents*")
                        CALL SYSTEM("rm -f "//TRIM(TempPath)//"/mapproject*")

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

                        IF(IfPlotVectors.EQ.1)THEN

                            OPEN(UNIT=17,FILE=TRIM(TempPath)//TRIM(VectorUFile)//".xyz",ACTION="WRITE")
                            CLOSE(UNIT=17,STATUS="DELETE")

                            OPEN(UNIT=17,FILE=TRIM(TempPath)//TRIM(VectorUFile)//".grd",ACTION="WRITE")
                            CLOSE(UNIT=17,STATUS="DELETE")

                            OPEN(UNIT=18,FILE=TRIM(TempPath)//TRIM(VectorVFile)//".xyz",ACTION="WRITE")
                            CLOSE(UNIT=18,STATUS="DELETE")

                            OPEN(UNIT=18,FILE=TRIM(TempPath)//TRIM(VectorVFile)//".grd",ACTION="WRITE")
                            CLOSE(UNIT=18,STATUS="DELETE")

                        ENDIF

                        IF(DoPS.EQ.0)THEN
                            OPEN(UNIT=20,FILE=TRIM(PlotName)//".ps",ACTION="WRITE")
                            CLOSE(UNIT=20,STATUS="DELETE")
                        ENDIF

                    ENDIF

                    IF(Flag.EQ.0)THEN

                        IF((IfPlotFilledContours.GE.1).OR.(IfPlotContourLines.GE.1))THEN

                            IF((IfPlotFilledContours.EQ.1).OR.(IfPlotContourLines.EQ.1))THEN

                                IF(TRIM(ContourFileType).EQ."ADCIRC-OUTPUT")THEN

                                   IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                      CLOSE(UNIT=19,STATUS="KEEP")
                                   ENDIF

                                ENDIF

                            ELSEIF((IfPlotFilledContours.EQ.2).OR.(IfPlotContourLines.EQ.2))THEN

                                IF(TRIM(ContourFileType).EQ."ADCIRC-OUTPUT")THEN

                                   IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                      CLOSE(UNIT=19,STATUS="KEEP")
                                   ENDIF

                                   IF(TRIM(ContourFileFormat2).EQ."ASCII")THEN
                                      CLOSE(UNIT=23,STATUS="KEEP")
                                   ENDIF

                                ENDIF

                            ENDIF

                        ENDIF

                        IF(IfPlotVectors.EQ.1)THEN

                            IF((IfPlotFilledContours.EQ.0).AND.(IfPlotContourLines.EQ.0))THEN

                                IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                                    CLOSE(UNIT=20,STATUS="KEEP")
                                ENDIF

                            ENDIF

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
                ENDIF
#endif

        END SUBROUTINE
        
        SUBROUTINE READOWISNAP(FILETYPE,MYRECORD,FILENAME,XYZUV,Time)
            IMPLICIT NONE
            CHARACTER(*),INTENT(IN) :: FILETYPE,FILENAME
            INTEGER,INTENT(IN)      :: MYRECORD
            REAL(8),INTENT(OUT),OPTIONAL :: Time
            REAL(8),ALLOCATABLE,INTENT(OUT) :: XYZUV(:,:)
            
            CHARACTER(200)          :: OWIHEADER
            CHARACTER(200)          :: DateString
            
            INTEGER                 :: LOCALSNAP
            INTEGER                 :: IOS
            INTEGER                 :: I
            INTEGER                 :: J
            INTEGER                 :: iLong
            INTEGER                 :: iLat
            INTEGER                 :: IDX
            INTEGER                 :: icymdhr
            INTEGER                 :: imin
            INTEGER(8)              :: StartSec
            INTEGER(8)              :: CurrentSec
            
            REAL(8)                 :: JunkR
            REAL(8)                 :: swLat
            REAL(8)                 :: swLong
            REAL(8)                 :: dx
            REAL(8)                 :: dy
            REAL(8),ALLOCATABLE     :: OWI_GRIDDATA(:,:,:)

            TYPE(DATEVAR)           :: MyDate
            
            OPEN(FILE=TRIM(FILENAME),UNIT=221,ACTION="READ")
            READ(221,'(A)') OWIHEADER

            READ(OWIHEADER(56:59),'(I4)')  MyDate%Year
            READ(OWIHEADER(60:61),'(I2)')  MyDate%Month
            READ(OWIHEADER(62:63),'(I2)')  MyDate%Day
            READ(OWIHEADER(64:65),'(I2)')  MyDate%Hour
            MyDate%Minute = 0
            MyDate%Second = 0
            StartSec = JulianSec(MyDate)

            LOCALSNAP=0
            DO
                LOCALSNAP = LOCALSNAP + 1
                READ(221,11,IOSTAT=IOS) iLat,iLong,dx,dy,swlat,swlong,&
                    icymdhr,imin
                IF(IOS.NE.0)THEN
                    WRITE(*,'(A)') "ERROR READING OWI"
                    STOP
                ENDIF

                IF(PRESENT(Time))THEN
                    WRITE(DateString,'(I0,I0)') icymdhr,imin
                    READ(DateString(1:4),'(I4)') MyDate%Year
                    READ(DateString(5:6),'(I2)') MyDate%Month
                    READ(DateString(7:8),'(I2)') MyDate%Day
                    READ(DateString(9:10),'(I2)') MyDate%Hour
                    READ(DateString(11:12),'(I2)') MyDate%Minute
                    MyDate%Second = 0
                    Time = DBLE(JulianSec(MyDate) - StartSec)
                ENDIF
    
                IF(TRIM(FILETYPE).EQ."PRESS")THEN
                    IF(LOCALSNAP.LT.MYRECORD)THEN
                        READ(221,22) ((JunkR,I=1,iLong),J=1,iLat)
                    ELSEIF(LOCALSNAP.EQ.MYRECORD)THEN
                        IF(ALLOCATED(XYZUV))DEALLOCATE(XYZUV)
                        IF(ALLOCATED(OWI_GRIDDATA))DEALLOCATE(OWI_GRIDDATA)
                        ALLOCATE(XYZUV(1:iLong*iLat,1:5))
                        ALLOCATE(OWI_GRIDDATA(1:iLong,1:iLat,1))
                        IDX = 0
                        READ(221,22) &
                            ((OWI_GRIDDATA(I,J,1),I=1,iLong),J=1,iLat)
                        DO I = 1,iLong
                            DO J = 1,iLat
                                IDX = IDX + 1
                                XYZUV(IDX,1) = swLong + DBLE(I-1)*dx
                                XYZUV(IDX,2) = swLat  + DBLE(J-1)*dy
                                XYZUV(IDX,3) = OWI_Griddata(I,J,1)
                                XYZUV(IDX,4) = OWI_Griddata(I,J,1)
                                XYZUV(IDX,5) = 0D0
                            ENDDO
                        ENDDO
                        CLOSE(221)
                        RETURN
                    ENDIF
                ELSEIF(TRIM(FILETYPE).EQ."WIND")THEN
                    IF(LOCALSNAP.LT.MYRECORD)THEN
                        READ(221,22) ((JunkR,I=1,iLong),J=1,iLat)
                        READ(221,22) ((JunkR,I=1,iLong),J=1,iLat)
                    ELSEIF(LOCALSNAP.EQ.MYRECORD)THEN
                        ALLOCATE(XYZUV(1:iLong*iLat,1:5))
                        ALLOCATE(OWI_GRIDDATA(1:iLong,1:iLat,1:2))
                        IDX = 0
                        READ(221,22) &
                            ((OWI_GRIDDATA(I,J,1),I=1,iLong),J=1,iLat)
                        READ(221,22) &
                            ((OWI_GRIDDATA(I,J,2),I=1,iLong),J=1,iLat)
                        DO I = 1,iLong
                            DO J = 1,iLat
                                IDX = IDX + 1
                                XYZUV(IDX,1) = swLong + DBLE(I-1)*dx
                                XYZUV(IDX,2) = swLat  + DBLE(J-1)*dy
                                XYZUV(IDX,3) = SQRT(OWI_Griddata(I,J,1)**2D0 + OWI_Griddata(I,J,2)**2D0)
                                XYZUV(IDX,4) = OWI_Griddata(I,J,1)
                                XYZUV(IDX,5) = OWI_Griddata(I,J,2)
                            ENDDO
                        ENDDO
                        CLOSE(221)
                        RETURN
                    ENDIF
                ENDIF
            ENDDO    
11          FORMAT(T6,I4,T16,I4,T23,F6.0,T32,F6.0,T44,F8.0,T58,F8.0,T69,I10,I2)
22          FORMAT(8F10.0)
                
        END SUBROUTINE

        SUBROUTINE GetOWILength(FileType,Filename,NumWindSnaps)
            IMPLICIT NONE

            CHARACTER(*),INTENT(IN) :: FileType,Filename
            INTEGER,INTENT(OUT)     :: NumWindSnaps

            INTEGER                 :: IOS
            INTEGER                 :: I
            INTEGER                 :: J
            INTEGER                 :: iLat
            INTEGER                 :: iLong
            INTEGER                 :: icymdhr
            INTEGER                 :: imin

            REAL(8)                 :: dx
            REAL(8)                 :: dy
            REAL(8)                 :: swlat
            REAL(8)                 :: swlong
            REAL(8)                 :: JunkR

            CHARACTER(200)          :: JunkC

            OPEN(FILE=TRIM(FILENAME),UNIT=221,ACTION="READ")
            READ(221,'(A)') JunkC
            NumWindSnaps = 0
            DO
                READ(221,11,IOSTAT=IOS) iLat,iLong,dx,dy,swlat,swlong,&
                    icymdhr,imin
                IF(IOS.NE.0)THEN
                    CLOSE(221)
                    RETURN
                ENDIF
                NumWindSnaps = NumWindSnaps + 1
                IF(TRIM(FILETYPE).EQ."PRESS")THEN
                    READ(221,22) ((JunkR,I=1,iLong),J=1,iLat)
                ELSEIF(TRIM(FILETYPE).EQ."WIND")THEN
                    READ(221,22) ((JunkR,I=1,iLong),J=1,iLat)
                    READ(221,22) ((JunkR,I=1,iLong),J=1,iLat)
                ENDIF    
            ENDDO
11          FORMAT(T6,I4,T16,I4,T23,F6.0,T32,F6.0,T44,F8.0,T58,F8.0,T69,I10,I2)
22          FORMAT(8F10.0)
        END SUBROUTINE

            
    
    END MODULE

    PROGRAM FigureGen

                USE GLOBALVAR

#ifdef NETCDF
                USE netcdf
#endif

                IMPLICIT NONE

#ifdef CMPI
                INCLUDE 'mpif.h'
                INTEGER             :: Counter
                INTEGER,ALLOCATABLE :: MPIRequests(:)
                INTEGER             :: RecordsFinished
                INTEGER,ALLOCATABLE :: RecordsOnProcs(:)
#endif

#ifdef NETCDF
                INTEGER             :: NC_DimIDs(NF90_MAX_VAR_DIMS)
                INTEGER             :: NC_ID
                INTEGER             :: NC_Var
#endif

                INTRINSIC           :: NINT
                INTRINSIC           :: REAL 

                CHARACTER(LEN=1)    :: JunkC
                CHARACTER(LEN=60)   :: RecordC
                CHARACTER(LEN=60)   :: RecordLabel
                CHARACTER(LEN=60)   :: TempC
                CHARACTER(LEN=60)   :: ZipFile

                INTEGER             :: I
                INTEGER             :: IARGC
                INTEGER             :: IL1
                INTEGER             :: IL2
                INTEGER             :: IL3
                INTEGER             :: Increment
                INTEGER             :: J
                INTEGER             :: JunkI
                INTEGER             :: Record
                INTEGER,ALLOCATABLE :: RecordsIndex(:,:)
                INTEGER,ALLOCATABLE :: SubRecordsList(:)
                INTEGER             :: WorkingRecord

                LOGICAL             :: InputFileError
                LOGICAL             :: InputFileFound
                LOGICAL             :: ReadError

                REAL                :: JunkR
                REAL                :: JunkTime(2)
                REAL                :: MaxDiff
                REAL                :: N
                REAL                :: Target = 0.5d0

                DEG2RAD = 4D0*ATAN(1D0)/180D0 !...PI/180
#ifdef CMPI
                CALL MPI_INIT(IERR)
                CALL MPI_COMM_RANK(MPI_COMM_WORLD, MyRank, IERR)
                CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NumProcs, IERR)
#else
                MyRank = 0
#endif

#ifdef NETCDF
                CALL Initialize_NetCDF()
#endif                

                IF(MyRank.EQ.0)THEN

                    WRITE(*,'(A)') " "
                    WRITE(*,'(A)') "--------------------------------------"
                    WRITE(*,'(A)') "FigureGen 51                2012/10/08"
                    WRITE(*,'(A)') " "
                    WRITE(*,'(A)') "This program reads raw ADCIRC files"
                    WRITE(*,'(A)') "and uses GMT to generate a figure with"
                    WRITE(*,'(A)') "contours and vectors plotted within a"
                    WRITE(*,'(A)') "specified lat/lon box."
                    WRITE(*,'(A)') "--------------------------------------"
                    WRITE(*,'(A)') " "

                    InputFileFound = .FALSE.
                    VersionNumber  = 0
                    IF(IARGC().GT.0)THEN
                        I = 0
                        DO WHILE ( I < IARGC() )
                            I = I + 1
                            CALL GETARG(I,TempC)
                            IF(TempC(1:2).EQ."-I")THEN
                                InputFileFound = .TRUE.
                                I = I + 1
                                CALL GETARG(I,TempC)
                                READ(TempC,'(A)') InputFile
                            ELSEIF(TempC(1:2).EQ."-V")THEN
                                I = I + 1
                                CALL GETARG(I,TempC)
                                READ(TempC,*) VersionNumber
                            ENDIF
                        ENDDO
                    ENDIF

                    IF(.NOT.InputFileFound)THEN
                        WRITE(*,'(A,$)') "Enter name of input file: "
                        READ(*,'(A)') InputFile
                    ENDIF
                    
                ENDIF

#ifdef CMPI
                CALL MPI_BCAST(InputFile, 100, MPI_CHARACTER, 0, MPI_COMM_WORLD, IERR)
#endif

                INQUIRE(FILE=TRIM(InputFile),EXIST=FileExists)
                IF(.NOT.FileExists)THEN
                    IF(MyRank.EQ.0)THEN
                        WRITE(*,'(A)') "FATAL ERROR: The specified input file does not exist."
                    ENDIF
#ifdef CMPI
                    CALL MPI_FINALIZE(IERR)
#endif
                    STOP
                ENDIF

                InputFileError = .FALSE.

                IF(MyRank.EQ.0)THEN
                   CALL ReadInputFile(InputFileError)
#ifdef CMPI
                   DO I=1,NumProcs-1
                       CALL MPI_SEND(0,1,MPI_INTEGER,I,1,MPI_COMM_WORLD,IERR)
                       JunkR = 0D0
                       DO J=1,10000
                           JunkR=JunkR+(0.5**J)**2
                       ENDDO
                       JunkR=0
                   ENDDO
#endif                   
                   IF(InputFileError)THEN
#ifdef CMPI
                        CALL MPI_FINALIZE(IERR)
#endif
                        STOP
                   ENDIF
                   
                   IF(OutputFileList)THEN
                       CALL ReadOutputFileList(ContourFileListFile,NumContourFiles,ContourFileList,ContourFileTag,ReadError)
#ifdef CMPI            
                       CALL MPI_BCAST(ReadError,1,MPI_LOGICAL,0,MPI_COMM_WORLD,IERR)
#endif
                       IF(ReadError)THEN
#ifdef CMPI
                           CALL MPI_FINALIZE(IERR)
#endif                           
                           STOP
                       ENDIF 
#ifdef CMPI                       
                       CALL MPI_BCAST(NumContourFiles,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
                       CALL MPI_BCAST(RecordsList,NumContourfiles,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
                       
                       DO I=1,NumContourFiles
                           CALL MPI_BCAST(ContourFileList(I),60,MPI_CHARACTER,0,MPI_COMM_WORLD,IERR)
                           CALL MPI_BCAST(ContourFileTag(I),60,MPI_CHARACTER,0,MPI_COMM_WORLD,IERR)
                       ENDDO    
#endif                       
                       NumRecords = NumContourFiles
                   ENDIF
                   
    

                ELSE
#ifdef CMPI                
                   CALL MPI_RECV(JunkI,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,MPI_STATUS_IGNORE,IERR)
                   CALL ReadInputFile(InputFileError)
                   IF(InputFileError)THEN
                        CALL MPI_FINALIZE(IERR)
                        STOP
                   ENDIF
                   
                   IF(OutputFileList)THEN
                      CALL MPI_BCAST(ReadError,1,MPI_LOGICAL,0,MPI_COMM_WORLD,IERR)
                      IF(ReadError)THEN
                         CALL MPI_FINALIZE(IERR)
                         STOP
                      ENDIF

                      CALL MPI_BCAST(NumContourFiles,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
                     
                      IF(ALLOCATED(RecordsList))DEALLOCATE(RecordsList)
                      ALLOCATE(RecordsList(1:NumContourFiles))
                      
                      CALL MPI_BCAST(RecordsList,NumContourfiles,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
                      ALLOCATE(ContourFileList(NumContourFiles))
                      ALLOCATE(ContourFileTag(NumContourFiles))
                      DO I=1,NumContourFiles
                         CALL MPI_BCAST(ContourFileList(I),60,MPI_CHARACTER,0,MPI_COMM_WORLD,IERR)
                         CALL MPI_BCAST(ContourFileTag(I),60,MPI_CHARACTER,0,MPI_COMM_WORLD,IERR)
                      ENDDO   
                      NumRecords = NumContourFiles
                   ENDIF   
#endif                   
                ENDIF

                IF(MyRank.EQ.0)THEN

                    IF(Verbose.GE.2)THEN
                        WRITE(*,'(A)') " "
                        WRITE(*,'(A)') "PRE-PROCESSING:"
                        WRITE(*,'(A)') " "
                    ENDIF

                    CALL ProcessFort14File
                    IF(NeedTranslationTable)THEN
                        CALL MakeTranslationTable
                    ELSE
                        ALLOCATE(TranslationTable(1:NumNodesGlobal))
                        NumNodesMesh2 = NumNodesGlobal
                        DO I = 1,NumNodesGlobal
                            TranslationTable(I) = I
                        ENDDO
                    ENDIF

                ENDIF                        

#ifdef CMPI
                IF(DoCenter.EQ.1)THEN
                    CALL MPI_BCAST(LongW, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                    CALL MPI_BCAST(LongE, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERR) 
                    CALL MPI_BCAST(LatN,  1, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                    CALL MPI_BCAST(LatS,  1, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                ENDIF
#endif

                IF(MyRank.EQ.0)THEN
                
#ifdef CMPI            

                    DO I=1,NumProcs-1
                       CALL MPI_SEND(0,1,MPI_INTEGER,I,1,MPI_COMM_WORLD,IERR)
                    ENDDO
#endif

                    IF((IfPlotFilledContours.GT.0).OR.(IfPlotContourLines.GT.0))THEN
                        IF(TRIM(ContourFileType).EQ."ADCIRC-OUTPUT")THEN
                            IF(TRIM(ContourFileFormat1).EQ."ASCII")THEN
                                OPEN(UNIT=19,FILE=TRIM(ContourFile1),ACTION="READ")
                                READ(UNIT=19,FMT='(A)') JunkC
                                READ(UNIT=19,FMT=*) NumRecs, JunkI, JunkR, JunkI, ContourFileNumCols
                                CLOSE(UNIT=19,STATUS="KEEP")
                                IF(IfGoogle.EQ.1)THEN
                                    TimeStep = NINT(JunkR)
                                ENDIF
                            ELSEIF(TRIM(ContourFileFormat1).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_OPEN(TRIM(ContourFile1),NF90_NOWRITE,NC_ID))
                                CALL Check(NF90_INQUIRE(NC_ID,NC_Var))
                                CALL Check(NF90_INQ_VARID(NC_ID,'time',NC_Var))
                                CALL Check(NF90_INQUIRE_VARIABLE(NC_ID,NC_Var,dimids=NC_DimIDs))
                                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_DimIDs(1),len=NumRecs))
                                CALL Check(NF90_GET_ATT(NC_ID,NF90_GLOBAL,"dt",JunkR))
                                IF(IfGoogle.EQ.1)THEN
                                    TimeStep = NINT(JunkR)
                                ENDIF
                                CALL Check(NF90_CLOSE(NC_ID))
#endif
                            ENDIF
                        ELSEIF(TRIM(ContourFileType).EQ."OWI-PRESS")THEN
                            CALL GetOWILength("PRESS",ContourFile1,NumRecs)
                        ELSEIF(TRIM(ContourFileType).EQ."OWI-WIND")THEN
                            CALL GetOWILength("WIND",ContourFile1,NumRecs)
                        ENDIF
                    ENDIF
                    IF(IfPlotVectors.GT.0)THEN
    
                        IF(TRIM(VectorFileType).EQ."ADCIRC-OUTPUT")THEN

                            IF(TRIM(VectorFileFormat).EQ."ASCII")THEN
                                OPEN(UNIT=20,FILE=TRIM(VectorFile),ACTION="READ")
                                READ(UNIT=20,FMT='(A)') JunkC
                                READ(UNIT=20,FMT=*) NumRecs, JunkI, JunkR
                                CLOSE(UNIT=20,STATUS="KEEP")
                                IF(IfGoogle.EQ.1)THEN
                                    TimeStep = NINT(JunkR)
                                ENDIF
                            ELSEIF(TRIM(VectorFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                                CALL Check(NF90_OPEN(TRIM(VectorFile),NF90_NOWRITE,NC_ID))
                                CALL Check(NF90_INQUIRE(NC_ID,NC_Var))
                                CALL Check(NF90_INQ_VARID(NC_ID,'time',NC_Var))
                                CALL Check(NF90_INQUIRE_VARIABLE(NC_ID,NC_Var,dimids=NC_DimIDs))
                                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_DimIDs(1),len=NumRecs))
                                !CALL Check(NF90_GET_VAR(NC_ID,NC_Var,JunkTime,start=(/1/),count=(/2/)))
                                !IF(IfGoogle.EQ.1)THEN
                                !    TimeStep = NINT(JunkTime(2)-JunkTime(1))
                                !ENDIF
                                CALL Check(NF90_CLOSE(NC_ID))
#endif
                            ENDIF

                        ENDIF
                    ENDIF
                    IF((IfPlotFilledContours.EQ.0.OR.TRIM(ContourFileType).NE."ADCIRC-OUTPUT") &
                           .AND.(IfPlotContourLines.EQ.0).AND. &
                           (IfPlotVectors.EQ.0).AND.(IfPlotParticles.NE.0))THEN
                        IF(TRIM(ParticleFileFormat).EQ."NETCDF")THEN
#ifdef NETCDF
                            CALL Check(NF90_OPEN(TRIM(ParticleFile),NF90_NOWRITE,NC_ID))
                            CALL Check(NF90_INQ_DIMID(NC_ID,'ntimesnap',NC_Var))
                            CALL Check(NF90_INQUIRE_DIMENSION(NC_ID,NC_Var,len=NumRecs))
                            CALL Check(NF90_INQ_VARID(NC_ID,'Time_index',NC_Var))
                            CALL Check(NF90_GET_VAR(NC_ID,NC_Var,JunkTime,start=(/1/),count=(/2/)))
                            IF(IfGoogle.EQ.1)THEN
                                TimeStep = NINT(JunkTime(2)-JunkTime(1))
                            ENDIF
                            CALL Check(NF90_CLOSE(NC_ID))
#endif
                        ENDIF
                    ENDIF

                ENDIF

#ifdef CMPI
                IF(MyRank.NE.0)THEN
                    CALL MPI_RECV(JunkI,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,MPI_STATUS_IGNORE,IERR)
                    IF(JunkI.NE.0)THEN
                       STOP
                    ENDIF
                ENDIF
                CALL MPI_BCAST(NumRecs, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
                IF(IfPlotGrid.EQ.1)THEN
                    CALL MPI_BCAST(NumEdgeFiles, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
                ENDIF
#endif

                IF(NumRecords.EQ.0)THEN
                    NumRecords = NumRecs
                    ALLOCATE(RecordsList(1:NumRecords))
                    DO I=1,NumRecords
                        RecordsList(I) = I
                    ENDDO
                ENDIF

                IF(MyRank.EQ.0)THEN

                    IF(FindContourRange.EQ.1)THEN
                        CALL FindContourMinMax
                    ENDIF
                    IF(FindVectorScale.EQ.1)THEN
                        CALL FindVectorScaleMag
                    ENDIF

                    IF((IfPlotFilledContours.GT.0).OR.(IfPlotContourLines.GT.0).OR. &
                       (INDEX(ColorLines,"DEFAULT").LE.0).OR.                       &
                       (INDEX(ContourFileType,"HWM-CSV").GT.0))THEN
                        CALL CreateCPTFiles
                    ENDIF

                ENDIF

#ifdef CMPI
                IF(IfGoogle.EQ.1)THEN
                    CALL MPI_BCAST(TimeStep, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
                ENDIF
                IF(FindContourRange.EQ.1)THEN
                    CALL MPI_BCAST(ContourMax, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                    CALL MPI_BCAST(ContourMin, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                ENDIF
                IF(FindVectorScale.EQ.1)THEN
                    CALL MPI_BCAST(VectorScaleMag, 1, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                ENDIF
                CALL MPI_BCAST(NumNodesGlobal, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
                CALL MPI_BCAST(NumNodesLocal, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
#endif

                IF(MyRank.NE.0)THEN
                    IF(OptimizeContours.EQ.1)THEN
                        ALLOCATE(BdyNodes(1:NumNodesLocal))
                    ENDIF
                    ALLOCATE(XYZNodes(1:NumNodesLocal))
                    ALLOCATE(X(1:NumNodesLocal))
                    ALLOCATE(Y(1:NumNodesLocal))
                    ALLOCATE(Z(1:NumNodesLocal))
                    ALLOCATE(BathLocal(1:NumNodesLocal))
                    ALLOCATE(TranslationTable(NumNodesGlobal))
                ENDIF

#ifdef CMPI
                IF(OptimizeContours.EQ.1)THEN
                    CALL MPI_BCAST(BdyNodes, NumNodesLocal, MPI_LOGICAL, 0, MPI_COMM_WORLD, IERR)
                ENDIF
                CALL MPI_BCAST(XYZNodes, NumNodesLocal, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
                CALL MPI_BCAST(X, NumNodesLocal, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                CALL MPI_BCAST(Y, NumNodesLocal, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                CALL MPI_BCAST(Z, NumNodesLocal, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                CALL MPI_BCAST(BathLocal, NumNodesLocal, MPI_REAL, 0, MPI_COMM_WORLD, IERR)
                CALL MPI_BCAST(NumNodesMesh2, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
                CALL MPI_BCAST(TranslationTable, NumNodesGlobal, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
#endif

                IF(IfGIS.GT.0)THEN

                    IF(NumLayers.EQ.0)THEN

                        MaxDiff = MAX(ABS(LongW-LongE),ABS(LatN-LatS))
                        N = (2D0 / LOG(4D0)) * LOG(MaxDiff / Target) + 1D0
                        IF(N-INT(N).GT.0.5D0)THEN
                            NumLayers = INT(N) + 1
                        ELSE
                            NumLayers = INT(N)
                        ENDIF
                        Increment = NumLayers - 1
                        IF(NumLayers.EQ.0)THEN
                            NumLayers = 1
                        ENDIF
                        IF(NumLayers.EQ.1)THEN
                            Increment = 1
                        ENDIF
                        NumSubRecords = 4**(NumLayers-1) + 1

                        ALLOCATE(SubRecordsList(1:NumRecords*NumSubRecords))
                        DO I=1,NumRecords*NumSubRecords
                            SubRecordsList(I) = I
                        ENDDO

                    ELSE

                        IF(NumLayers.EQ.1)THEN
                            Increment = 1
                            NumSubRecords = 1
                        ELSE
                           Increment = NumLayers - 1
                           NumSubRecords = 4**(NumLayers-1) + 1
                        ENDIF

                        ALLOCATE(SubRecordsList(1:NumRecords*NumSubRecords))
                        DO I=1,NumRecords*NumSubRecords
                            SubRecordsList(I) = I
                        ENDDO

                    ENDIF

                ELSEIF(IfGoogle.GT.0)THEN

                    Increment = 1
                    NumSubRecords = 0
                    DO I=1,NumLayers
                        NumSubRecords = NumSubRecords + 4**(I-1)
                    ENDDO

                    ALLOCATE(SubRecordsList(1:NumRecords*NumSubRecords))
                    DO I=1,NumRecords*NumSubRecords
                        SubRecordsList(I) = I
                    ENDDO

                ELSE

                    Increment = 1
                    NumLayers = 1
                    NumSubRecords = 1

                    ALLOCATE(SubRecordsList(1:NumRecords*NumSubRecords))
                    DO I=1,NumRecords*NumSubRecords
                        SubRecordsList(I) = I
                    ENDDO

                ENDIF

                ALLOCATE(RecordsIndex(1:NumRecords*NumSubRecords,4))
                I = 1
                DO J=1,NumRecords
                    DO IL1=1,NumLayers,Increment
                        DO IL2=1,2**(IL1-1)
                            DO IL3=1,2**(IL1-1)
                                RecordsIndex(I,1) = RecordsList(J)
                                RecordsIndex(I,2) = IL1
                                RecordsIndex(I,3) = IL2
                                RecordsIndex(I,4) = IL3
                                I = I + 1
                            ENDDO
                        ENDDO
                    ENDDO
                ENDDO

                IF(MyRank.EQ.0)THEN

                    IF(Verbose.GE.2)THEN
                        WRITE(*,'(A)') " "
                        WRITE(*,'(A)') "GENERATING IMAGES:"
                        WRITE(*,'(A)') " "
                    ENDIF

                    CALL SYSTEM(TRIM(Path)//"gmtset PAPER_MEDIA A0")
                    CALL SYSTEM(TRIM(Path)//"gmtset PLOT_DEGREE_FORMAT -D")
                    CALL SYSTEM(TRIM(Path)//"gmtset OUTPUT_DEGREE_FORMAT -D")
                    CALL SYSTEM(TRIM(Path)//"gmtset BASEMAP_TYPE fancy")
                    CALL SYSTEM(TRIM(Path)//"gmtset D_FORMAT %lg")
                    CALL SYSTEM(TRIM(Path)//"gmtset HISTORY FALSE")
#ifdef PLAIN
                    CALL SYSTEM(TRIM(Path)//"gmtset BASEMAP_TYPE plain")
#endif

#ifdef CMPI
                    ALLOCATE(MPIRequests(1:(NumProcs-1)))
                    ALLOCATE(RecordsOnProcs(1:(NumProcs-1)))
                    Counter = 0
                    JunkI = 0
                    RecordsFinished = 0

        ! Send the initial batch of records to the processors.

                    IF((NumProcs-1).GT.NumRecords*NumSubRecords)THEN
                        IF(Verbose.GE.1)THEN
                            WRITE(*,'(A)') "WARNING: There are more cores to use than time snaps to assign."// &
                                           "  Some resources will be wasted."
                        ENDIF
                        NumProcsInitial = NumProcs
                        NumProcs = NumRecords*NumSubRecords + 1
                    ELSE
                        NumProcsInitial = NumProcs
                    ENDIF

                    DO I=1,NumProcsInitial-1
                        IF(I.LE.NumProcs-1)THEN
                            Counter = Counter + 1
                            WorkingRecord = SubRecordsList(Counter)
                            RecordsOnProcs(I) = WorkingRecord
                            CALL MPI_SEND(WorkingRecord, 1, MPI_INTEGER, I, 1, MPI_COMM_WORLD, IERR)
                            CALL MPI_IRECV(JunkI, 1, MPI_INTEGER, I, RecordsOnProcs(I), &
                                           MPI_COMM_WORLD, MPIRequests(I), IERR)
                        ELSE
                            WorkingRecord = 0
                            CALL MPI_SEND(WorkingRecord, 1, MPI_INTEGER, I, 1, MPI_COMM_WORLD, IERR)
                        ENDIF
                    ENDDO

        ! Loop continuously over the processors, testing to see if any are finished.  When a processor finishes,
        ! assign the next record to it.

                    outer1: DO

                        IF(RecordsFinished.GE.NumRecords*NumSubRecords)THEN
                            EXIT outer1
                        ENDIF

                        CALL MPI_WAITANY(NumProcs-1, MPIRequests, I, MPI_STATUS_IGNORE, IERR)

                        RecordsFinished = RecordsFinished + 1
                        IF(Verbose.GE.4)THEN
                            WRITE(*,'(A,I4.4,A,I4.4,A)') "Core ",MyRank," received the all-clear"//  &
                                                         " from core ",I,"."
                        ENDIF

                        IF(Counter.LT.NumRecords*NumSubRecords)THEN

                            Counter = Counter + 1
                            WorkingRecord = Counter
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
                    outer2: DO I=1,NumRecords*NumSubRecords
#endif

#ifdef CMPI
                        CALL MPI_RECV(WorkingRecord, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, IERR)
#else
                        WorkingRecord = I
#endif

                        IF(WorkingRecord.GT.0)THEN

                            Record = RecordsIndex(WorkingRecord,1)
                            IL1    = RecordsIndex(WorkingRecord,2)
                            IL2    = RecordsIndex(WorkingRecord,3)
                            IL3    = RecordsIndex(WorkingRecord,4)

                            IF(OutputFileList)THEN
                                Record = WorkingRecord
                            ENDIF

                            WRITE(UNIT=RecordC,FMT='(I4.4)') Record

                            IF(Verbose.GE.3)THEN
                                IF((IfGoogle.EQ.0).AND.(IfGIS.EQ.0))THEN
                                    WRITE(*,'(A,I4.4,A,I4.4,A)') "Core ",MyRank," started record ",Record,"."
                                ELSE
                                    WRITE(*,'(A,I4.4,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)') "Core ",MyRank," started record ",Record, &
                                            ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                                ENDIF
                            ENDIF

                            IF((IfPlotFilledContours.GT.0).OR.(IfPlotContourLines.GT.0).OR.&
                               (IfPlotParticles.GT.0).OR.(IfPlotVectors.GT.0))THEN

                                WRITE(UNIT=RecordC,FMT='(I4.4)') Record
                                TempC = TRIM(TempPath)//"XYZFileNames_"//TRIM(RecordC)//".tmp"

                                IF((IL1.EQ.1).AND.(IL2.EQ.1).AND.(IL3.EQ.1))THEN
                                    CALL WriteXYZFiles(Record)
                                    OPEN(UNIT=100,FILE=TRIM(TempC),ACTION="WRITE")
                                    IF((IfPlotFilledContours.GT.0).OR.(IfPlotContourLines.GT.0))THEN
                                        WRITE(100,'(A)') TRIM(ContourXYZFile)
                                    ENDIF
                                    IF(IfPlotVectors.GT.0)THEN
                                        WRITE(100,'(A)') TRIM(VectorUFile)
                                        WRITE(100,'(A)') TRIM(VectorVFile)
                                    ENDIF
                                    WRITE(100,'(A)') " "
                                    CLOSE(100)
                                ENDIF

                                filestatus: DO
                                    INQUIRE(FILE=TRIM(TempC),EXIST=FileExists)
                                    IF(FileExists)THEN
                                        OPEN(UNIT=100,FILE=TRIM(TempC),ACTION="READ")
                                        IF((IfPlotFilledContours.GT.0).OR.(IfPlotContourLines.GT.0))THEN
                                            READ(UNIT=100,FMT='(A)',ERR=978,END=978) ContourXYZFile
                                        ENDIF
                                        IF(IfPlotVectors.GT.0)THEN
                                            READ(UNIT=100,FMT='(A)',ERR=978,END=978) VectorUFile
                                            READ(UNIT=100,FMT='(A)',ERR=978,END=978) VectorVFile
                                        ENDIF
                                        CLOSE(100)
                                        EXIT filestatus
 978                                    CONTINUE
                                    ENDIF
                                    CALL SLEEP(5)
                                ENDDO filestatus

                            ENDIF

                            CALL WritePSImage(Record,IL1,IL2,IL3)
                            IF(IfGoogle.EQ.1)THEN
                                CALL GoogleKML(Record,IL1,IL2,IL3)
                            ENDIF

                            IF(Verbose.GE.2)THEN
                                IF(NumLayers.EQ.1)THEN
                                    WRITE(*,'(A,I4.4,A,I4.4,A)') "Core ",MyRank," completed record ",Record,"."
                                ELSE
                                    WRITE(*,'(A,I4.4,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)') "Core ",MyRank," completed record ",Record, &
                                            ", layer ",IL1,", cell ",IL2,"/",IL3,"."
                                ENDIF
                            ENDIF

#ifdef CMPI
                            CALL MPI_SEND(JunkI, 1, MPI_INTEGER, 0, WorkingRecord, MPI_COMM_WORLD, IERR)
#endif

                            IF(RemoveFiles.EQ.1)THEN
                                IF(WorkingRecord.EQ.NumRecords*NumSubRecords)THEN
                                    CALL Finisher(1)
                                ELSEIF(Record.NE.RecordsIndex(WorkingRecord+1,1))THEN
                                    CALL Finisher(1)
                                ENDIF
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

                IF((MyRank.EQ.0).AND.(IfGoogle.EQ.1))THEN
                    CALL GoogleKMZ
                ENDIF

                IF((MyRank.EQ.0).AND.(IfGIS.EQ.2))THEN

                    IF(INDEX(ContourFileType,"GRID-DECOMP").LE.0)THEN
                        CALL SYSTEM(TRIM(Path)//"ps2raster Scale.ps -A -E200 -FScale.jpg"// &
                                    " -G"//TRIM(GSPath)//"gs -P -Tj")
                    ENDIF

                    WRITE(ZipFile,'(A)') TRIM(AlphaLabel)
                    IF(ZipFile(LEN_TRIM(ZipFile):LEN_TRIM(ZipFile)).EQ."_")THEN
                       WRITE(ZipFile(LEN_TRIM(ZipFile):LEN_TRIM(ZipFile)),'(A)') " "
                    ELSEIF(ZipFile(LEN_TRIM(ZipFile):LEN_TRIM(ZipFile)).EQ."-")THEN
                       WRITE(ZipFile(LEN_TRIM(ZipFile):LEN_TRIM(ZipFile)),'(A)') " "
                    ENDIF

                    INQUIRE(FILE=TRIM(ZipFile)//".zip",EXIST=FileExists)
                    IF(FileExists)THEN
                        CALL SYSTEM("rm "//TRIM(ZipFile)//".zip")
                    ENDIF

                    DO I=1,NumRecords
                        Record = RecordsList(I)
                        WRITE(UNIT=RecordLabel,FMT='(A,I4.4)') TRIM(AlphaLabel),Record
                        CALL SYSTEM("mkdir "//TRIM(RecordLabel))
                        CALL SYSTEM("mv "//TRIM(RecordLabel)//"*.jpg "//TRIM(RecordLabel))
                        CALL SYSTEM("mv "//TRIM(RecordLabel)//"*.jgw "//TRIM(RecordLabel))
                        CALL SYSTEM("zip -qr "//TRIM(ZipFile)//" "//TRIM(RecordLabel))
                        CALL SYSTEM("rm -fR "//TRIM(RecordLabel))
                        CALL SYSTEM("rm -fR "//TRIM(RecordLabel)//"*.ps")
                    ENDDO

                    IF(INDEX(ContourFileType,"GRID-DECOMP").LE.0)THEN
                        CALL SYSTEM("zip -qr "//TRIM(ZipFile)//" Scale.jpg")
                        CALL SYSTEM("rm Scale*")
                    ENDIF

                    CALL RemoveIfExists(".bb")

                ENDIF

                IF(RemoveFiles.EQ.1)THEN
                    CALL Finisher(0)
                ENDIF

#ifdef CMPI
                IF(ALLOCATED(RecordsOnProcs))   DEALLOCATE(RecordsOnProcs)
                CALL MPI_FINALIZE(IERR)
#endif

    END PROGRAM

!-----GPL----------------------------------------------------------------------
!
! This file is part of the arcadis-util library
! Copyright (C) 2010-2016  Arcadis
!
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: Zachary Cobell, zachary.cobell@arcadis.com
!  Arcadis
!  11001 W. 120th Ave, Suite 200
!  Broomfield, CO 80021
!
!  All indications and logos of, and references to, "Arcadis"
!  are registered trademarks of Arcadis, and remain the property of
!  Arcadis. All rights reserved.
!
!------------------------------------------------------------------------------
!
!  File: ADCmodules.F90
!
!------------------------------------------------------------------------------
!
!----------------------------------------------------------------------------------!
!                                                                                  !
!                   BELOW IS TAKEN FROM KDTREE2 ROUTINES                           !
!                                                                                  !
!           (c) Matthew Kennel, Institute for Nonlinear Science (2004)             !
!                                                                                  !
!----------------------------------------------------------------------------------!

          MODULE ADCMOD_KDTREE2_PRECISION_MODULE
          INTEGER, PARAMETER :: SP = KIND(0.0)
          INTEGER, PARAMETER :: DP = KIND(0.0D0)

          PRIVATE :: SP, DP
          !INTEGER, PARAMETER :: KDKIND = SP
          INTEGER, PARAMETER :: KDKIND = DP
          PUBLIC :: KDKIND

          END MODULE ADCMOD_KDTREE2_PRECISION_MODULE

          MODULE ADCMOD_KDTREE2_PRIORITY_QUEUE_MODULE
          USE ADCMOD_KDTREE2_PRECISION_MODULE

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

          END MODULE ADCMOD_KDTREE2_PRIORITY_QUEUE_MODULE


          MODULE ADCMOD_KDTREE2_MODULE

          USE ADCMOD_KDTREE2_PRECISION_MODULE
          USE ADCMOD_KDTREE2_PRIORITY_QUEUE_MODULE

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

        END MODULE ADCMOD_KDTREE2_MODULE

!----------------------------------------------------------------------------------!
!                                                                                  !
!                          END OF KDTREE ROUTINES                                  !
!                                                                                  !
!----------------------------------------------------------------------------------!

!----------------------------------------------------------------------------------!
!                                                                                  !
!                           START ADCIRC ROUTINES                                  !
!                                                                                  !
!----------------------------------------------------------------------------------!

!> \brief Module useful for programming when modeling with ADCIRC
!> \author Zach Cobell
!> \copyright GNU Public License, Version 3
        MODULE ADCMOD
#ifdef ADCNETCDF
            USE NETCDF
#endif
            USE ADCMOD_KDTREE2_MODULE

            !...Variables that we will save in memory related to processing
            !   command line arguments comming from C++ code. These are not
            !   exposed to outside code

            !>Variable retained holding the command line arguments when using C++
            CHARACTER(200),ALLOCATABLE,SAVE,PRIVATE :: ADCModules_CommandLineArgs(:)

            !>Variable holding the number of command line arguments
            INTEGER,SAVE,PRIVATE                    :: ADCModules_NumCommandLineArgs

!................................................................................
!
!                      ADCIRC TYPE VARIABLES
!
!................................................................................

            !>Type BoundaryListing is a derived type for ADCIRC boundary conditions
            TYPE BoundaryListing
                !>Number of nodes for this particular boundary
                INTEGER                    :: NumNodes
                !>ADCIRC model boundary type code
                INTEGER                    :: Code
                !>Array of nodes on first side of the boundary
                INTEGER,ALLOCATABLE        :: N1(:)
                !>Array of nodes on second side of boundary (may be unused)
                INTEGER,ALLOCATABLE        :: N2(:)
                !>Crest height of weir type boundaries
                REAL(8),ALLOCATABLE        :: Crest(:)
                !>Coefficient of supercitical flow for weir type boundaries
                REAL(8),ALLOCATABLE        :: Supercritical(:)
                !>Coefficient of subcritical flow for weir type boundaries
                REAL(8),ALLOCATABLE        :: Subcritical(:)
            END TYPE

            !>Type Grid is a derived type containing all adcirc grid information
            TYPE Grid
                !>Title line found in mesh file
                CHARACTER(200)             :: title
                !>Number of elements in the ADCIRC mesh
                INTEGER                    :: NumElements
                !>Number of nodes in the ADCIRC mesh
                INTEGER                    :: NumNodes
                !>2D array that is NumNodes by 3 containing the Nodal
                !>position and elevation. Index 2 contains x, y, and z
                !>as index 1, 2, and 3.
                REAL(8),ALLOCATABLE        :: nodes(:,:)
                !>Connectivity table for the ADCIRC mesh that is
                !>NumElements by 3. References the node table.
                INTEGER,ALLOCATABLE        :: conn(:,:)
                !>Number of open boundaries in the ADCIRC mesh
                INTEGER                    :: NumOpenBoundaries
                !>Number of open boundary nodes in the ADCIRC mesh
                INTEGER                    :: TotNumOpenBoundaryNodes
                !>Number of land boundaries in the ADCIRC mesh
                INTEGER                    :: NumLandBoundaries
                !>Total number of land boundary nodes in the ADCIRC mesh
                INTEGER                    :: TotNumLandBoundaryNodes
                !>Array of open boundaries
                TYPE(BoundaryListing),ALLOCATABLE :: OceanBC(:)
                !>Array of land boundaries
                TYPE(BoundaryListing),ALLOCATABLE :: LandBC(:)
            END TYPE

            !>Type Fort13 contains information for individual parameters in a
            !>fort.13
            TYPE Parameters
                !>Name of this nodal parameter
                CHARACTER(200)      :: Attribute
                !>Default value for this nodal parameter
                REAL                :: DefaultValue
                !>Units read in from the fort.13 file
                CHARACTER(10)       :: Units
                !>Number of values for each node. Usually 1 or 12.
                INTEGER             :: NumValues
                !>Array that contains the values for this nodal parameter at each
                !>node
                REAL,ALLOCATABLE    :: Values(:,:)
            END TYPE

            !>The global fort.13 structure which relies on TYPE Parameters
            TYPE fort13
                !>Title found in the header of the fort.13 file
                CHARACTER(200)      :: Title
                !>Number of nodes in the mesh that this file was created for
                INTEGER             :: NumNodes
                !>Number of nodal parameters that are found within this file
                INTEGER             :: NumAttributes
                !>Array containing the nodal parameters
                TYPE(parameters),ALLOCATABLE :: nodal_param(:)
            END TYPE

            !>Type Timestep contains the information from a single ADCIRC timestep
            TYPE Timestep
                !>Integer timestep number for this output cycle
                !> \f$TS = Time \cdot DT\f$
                INTEGER                    :: TS
                !>Number of non-default nodes. Used when ADCIRC output is written
                !>in sparse format
                INTEGER                    :: NumNonDefault
                !>Value not written to ADCIRC output files. Used when ADCIRC
                !>output is written in sparse format.
                REAL(8)                    :: DefaultValue
                !>Time of the current output cycle
                !> \f$Time = \frac{TS}{DT}\f$
                REAL(8)                    :: Time
                !>Values contains the array of output values for the current
                !>output cycle
                REAL(8),ALLOCATABLE        :: Values(:,:)
            END TYPE

            !>Type AdcircOutput contains all the information inside an ADCIRC output file
            TYPE AdcircOutput
                !>Title string found in ADCIRC output file
                CHARACTER(200)             :: title
                !>netCDF variable locator for first output array
                CHARACTER(200)             :: NC_VARIABLE1
                !>netCDF variable locator for second output array
                CHARACTER(200)             :: NC_VARIABLE2
                !>Interval between output cycles in the ADCIRC output file
                REAL(8)                    :: dt
                !>Number of columns in output array. Scalar or Vector quantity
                INTEGER                    :: NumValues
                !>Number of nodes in mesh used to generate this output file
                INTEGER                    :: NumNodes
                !>Number of time steps in this output file
                INTEGER                    :: NumTimeSteps
                !>Number of ADCIRC time steps between output intervals
                INTEGER                    :: Interval
                !>File format identifier. Generally not used.
                INTEGER                    :: FileFormat
                !>Array of output cycles
                TYPE(Timestep),ALLOCATABLE :: Output(:)
            END TYPE

            !>The following is a type created for ADCIRC hot start output. The naming convention
            !>mirrors that which is found within the ADCIRC code (v50).
            TYPE HOTSTART2D
                REAL(8),ALLOCATABLE :: ETA1(:)
                REAL(8),ALLOCATABLE :: ETA2(:)
                REAL(8),ALLOCATABLE :: ETADisc(:)
                REAL(8),ALLOCATABLE :: UU2(:)
                REAL(8),ALLOCATABLE :: VV2(:)
                INTEGER,ALLOCATABLE :: NNODECODE(:)
                INTEGER,ALLOCATABLE :: NOFF(:)
                REAL(8),ALLOCATABLE :: CH1(:)
                INTEGER             :: IMHS
                REAL(8)             :: TimeLoc
                INTEGER             :: InputFileFmtVn
                INTEGER             :: ITHS
                INTEGER             :: NP_G_IN
                INTEGER             :: NE_G_IN
                INTEGER             :: NP_A_IN
                INTEGER             :: NE_A_IN
                INTEGER             :: IESTP
                INTEGER             :: NSCOUE
                INTEGER             :: IVSTP
                INTEGER             :: NSCOUV
                INTEGER             :: ICSTP
                INTEGER             :: NSCOUC
                INTEGER             :: IPSTP
                INTEGER             :: IWSTP
                INTEGER             :: NSCOUM
                INTEGER             :: IGEP
                INTEGER             :: NSCOUGE
                INTEGER             :: IGVP
                INTEGER             :: NSCOUGV
                INTEGER             :: IGCP
                INTEGER             :: NSCOUGC
                INTEGER             :: IGPP
                INTEGER             :: IGWP
                INTEGER             :: NSCOUGW
            END TYPE

            !>Inner type to hold element table information
            TYPE ElementTableInner
                !>Number of elements conntected to this node
                INTEGER                        :: NumElementsAroundMe
                !>Array of elements connected to this node
                INTEGER,ALLOCATABLE            :: ElementsAroundMe(:)
            END TYPE

            !>Type for the global element table structure
            TYPE ElementTable
                !>Number of nodes within the mesh this table was created for
                INTEGER                             :: NumNodes
                !>Array of element lists for each node
                TYPE(ElementTableInner),ALLOCATABLE :: Node(:)
            END TYPE

            !>Inner type to hold node table information
            TYPE NODETABLE_INNER
                !>Number of nodes connected to this node
                INTEGER :: NumNodesAroundMe
                !>Array of nodes connected to this node
                INTEGER,ALLOCATABLE :: NodesAroundMe(:)
                !>Distance to each node connected to this node (meters)
                REAL(8),ALLOCATABLE :: distance(:)
            END TYPE

            !>Type for global node table information
            TYPE NODETABLE
                !>Number of nodes within the mesh this table was created for
                INTEGER :: NumNodes
                !>Array of node lists for each node
                TYPE(NODETABLE_INNER),ALLOCATABLE :: node(:)
            END TYPE

            !>IMEDS Data Container
            TYPE IMEDS_DATA
                !>Y position in decimal degrees
                REAL(8)                      :: LATITUDE
                !>X position in decimal degrees
                REAL(8)                      :: LONGITUDE
                !>Name of the current station
                CHARACTER(200)               :: STATION_NAME
                !>Number of output snaps in this container
                INTEGER                      :: NSNAPS
                !>Index of this station in the global IMEDS file
                INTEGER                      :: STATION_INDEX
                !>Year for each output cycle
                INTEGER,ALLOCATABLE          :: YEAR(:)
                !>Month for each output cycle
                INTEGER,ALLOCATABLE          :: MONTH(:)
                !>Day for each output cycle
                INTEGER,ALLOCATABLE          :: DAY(:)
                !>Hour for each output cycle
                INTEGER,ALLOCATABLE          :: HOUR(:)
                !>Minute for each output cycle
                INTEGER,ALLOCATABLE          :: MINUTE(:)
                !>Second for each output cycle (generally unused)
                INTEGER,ALLOCATABLE          :: SECOND(:)
                !>Measured (or modeled) value for each output cycle
                REAL(8),ALLOCATABLE          :: VALUE(:)
            END TYPE

            !>IMEDS Type variable (Wrapper)
            TYPE IMEDS
                !>Number of stations found within this file
                INTEGER                      :: NSTATIONS
                CHARACTER(200)               :: HEADER1
                CHARACTER(200)               :: HEADER2
                CHARACTER(200)               :: HEADER3
                !>Data container for the array of stations
                TYPE(IMEDS_DATA),ALLOCATABLE :: STATION(:)
            END TYPE

            !>Date variable for date math
            TYPE DATEVAR
                INTEGER :: YEAR=0
                INTEGER :: MONTH=0
                INTEGER :: DAY=0
                INTEGER :: HOUR=0
                INTEGER :: MINUTE=0
                INTEGER :: SECOND=0
            END TYPE

            !>Listing of output types written to ADCIRC files
            CHARACTER(200) :: NETCDF_TYPES(38)
            !>Long name for each type of ADCIRC output
            CHARACTER(200) :: NC_LONGNAME(38)
            !>Common name for each type of ADCIRC output
            CHARACTER(200) :: NC_STDNAME(38)

            CONTAINS

!................................................................................
!
!                      ADCIRC GRIDS
!
!................................................................................

            !>This subroutine will read an ADCIRC mesh and return an ADCIRC
            !>grid type variable. It will also perform some basic checks to
            !>ensure that the grid is relatively sane and can be used in
            !>other code to follow. Will automatically determine an FORTRAN
            !>read unit so as not to conflict with other user written code
            !>\author Zach Cobell
            SUBROUTINE ReadGrid(gridfile,MyGrid)
            !...Modular subroutine to read ADCIRC grid. Must have Grid data structure from GlobalGrid module
#ifdef ADCNETCDF
                USE NETCDF
#endif
                IMPLICIT NONE
                !>String containing the name of the file to be read
                CHARACTER(*),INTENT(IN)        :: gridfile
                !>Output variable of type GRID containing the information
                !>read from the file
                TYPE(Grid),INTENT(OUT)         :: MyGrid


                INTEGER                        :: I,J
                INTEGER                        :: N = 0
                INTEGER                        :: JunkI
                INTEGER                        :: JunkI2
                INTEGER                        :: READUNIT
                LOGICAL                        :: ISNETCDF
                
                INTEGER                        :: NODEDIM,ELEDIM
                INTEGER                        :: NETAVAR,NVDLLVAR
                INTEGER                        :: MAXNODES,NBOUDIM
                INTEGER                        :: NOPEDIM,MXNVELLVAR
                INTEGER                        :: IBTYPEVAR,NVELLVAR
                INTEGER                        :: ELVAR,XVAR,YVAR,ZVAR
                INTEGER                        :: MXNVDLLVAR,NDBVVAR
                INTEGER                        :: NBVVVAR,NVELVAR
                INTEGER                        :: CHUNK,CHUNK2,DMY(2)
                INTEGER,ALLOCATABLE            :: NUMNODES_OPENBNDRY(:)
                INTEGER,ALLOCATABLE            :: LANDBOUNDARY_ENTRIES(:,:)
                INTEGER,ALLOCATABLE            :: OPENBOUNDARY_ENTRIES(:,:)
                INTEGER,ALLOCATABLE            :: NumNodes_LandBndry(:)
                INTEGER,ALLOCATABLE            :: ELTEMP(:,:),IBTYPE(:),TEMPI(:,:)

                REAL(8),ALLOCATABLE            :: TEMPR(:)

                !...Determine if file is netcdf
                IF(INDEX(gridfile,".nc").GT.1)THEN
                    ISNETCDF=.TRUE.
                ELSE
                    ISNETCDF=.FALSE.
                ENDIF


                IF(ISNETCDF)THEN
#ifndef ADCNETCDF
                    WRITE(*,'(A)') "ERROR: ADCModules not compiled for NetCDF."
                    STOP
#else
                   !...Open NetCDF
                   !WRITE(*,'(A,$)') "WARNING - not all information available "//&
                   !                 "in ADCIRC netCDF grid format!"
                    CALL CHECK(NF90_OPEN(TRIM(GRIDFILE),NF90_NOWRITE,READUNIT))
                    
                    !...Get sizes
                    CALL CHECK(NF90_INQ_DIMID(READUNIT,"node",NODEDIM))
                    CALL CHECK(NF90_INQ_DIMID(READUNIT,"nele",ELEDIM))
                    CALL CHECK(NF90_INQUIRE_DIMENSION(READUNIT,NODEDIM,LEN=MYGRID%NUMNODES))
                    CALL CHECK(NF90_INQUIRE_DIMENSION(READUNIT,ELEDIM,LEN=MYGRID%NUMELEMENTS))
                    ALLOCATE(MYGRID%NODES(1:MYGRID%NUMNODES,1:3))
                    ALLOCATE(MYGRID%CONN(1:MYGRID%NUMELEMENTS,1:3))
                    ALLOCATE(ELTEMP(1:3,1:MYGRID%NUMELEMENTS))
                    
                    !...Get X,Y,Z
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'x',XVAR))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'y',YVAR))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'depth',ZVAR))
                   
                    ALLOCATE(TempR(1:MYGRID%NUMNODES))
                    CALL CHECK(NF90_GET_VAR(READUNIT,XVAR,TEMPR))
                    MYGRID%NODES(:,1) = TEMPR(:)
                    CALL CHECK(NF90_GET_VAR(READUNIT,YVAR,TEMPR))
                    MYGRID%NODES(:,2) = TEMPR(:)
                    CALL CHECK(NF90_GET_VAR(READUNIT,ZVAR,TEMPR))
                    MYGRID%NODES(:,3) = TEMPR(:)
                    
                    !...Get elements
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'element',ELVAR))
                    CHUNK=500000
                    ALLOCATE(TEMPI(1:3,1:CHUNK))
                    DO I = 1,MYGRID%NUMELEMENTS,CHUNK
                        IF(I+CHUNK.GT.MYGRID%NUMELEMENTS)THEN
                            CHUNK2 = MYGRID%NUMELEMENTS-I+1
                        ELSE
                            CHUNK2 = CHUNK
                        ENDIF
                        CALL CHECK(NF90_GET_VAR(READUNIT,ELVAR,TEMPI,START=(/1,I/),COUNT=(/3,CHUNK2/)))
                        ELTEMP(1:3,I:I+CHUNK2-1) = TEMPI(1:3,1:CHUNK2)
                    ENDDO
                    MYGRID%CONN(:,1) = ELTEMP(1,:)
                    MYGRID%CONN(:,2) = ELTEMP(2,:)
                    MYGRID%CONN(:,3) = ELTEMP(3,:)
#if 0                    
                    !...Get Open Boundaries
                    CALL CHECK(NF90_INQ_DIMID(READUNIT,'nope',NOPEDIM))
                    CALL CHECK(NF90_INQUIRE_DIMENSION(READUNIT,NOPEDIM,LEN=MYGRID%NUMOPENBOUNDARIES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'neta',NETAVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NETAVAR,MYGRID%TOTNUMOPENBOUNDARYNODES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nvdll',NVDLLVAR))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'max_nvdll',MXNVDLLVAR))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nbdv',NDBVVAR))
                    ALLOCATE(NUMNODES_OPENBNDRY(1:MYGRID%NUMOPENBOUNDARIES))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NVDLLVAR,NUMNODES_OPENBNDRY))
                    CALL CHECK(NF90_GET_VAR(READUNIT,MXNVDLLVAR,MAXNODES))
                    ALLOCATE(OPENBOUNDARY_ENTRIES(1:MYGRID%NUMOPENBOUNDARIES,1:MAXNODES))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NDBVVAR,OPENBOUNDARY_ENTRIES))
                    ALLOCATE(MYGRID%OCEANBC(1:MYGRID%NUMOPENBOUNDARIES))
                    DO I = 1,MYGRID%NUMOPENBOUNDARIES
                        MYGRID%OCEANBC(I)%NUMNODES = NUMNODES_OPENBNDRY(I)
                        ALLOCATE(MYGRID%OCEANBC(I)%N1(1:NUMNODES_OPENBNDRY(I)))
                        DO J = 1,NUMNODES_OPENBNDRY(I)
                            MYGRID%OCEANBC(I)%N1(J) = OPENBOUNDARY_ENTRIES(I,J)
                        ENDDO
                    ENDDO
                    
                    !...Get Land Boundaries
                    CALL CHECK(NF90_INQ_DIMID(READUNIT,'nbou',NBOUDIM))
                    CALL CHECK(NF90_INQUIRE_DIMENSION(READUNIT,NBOUDIM,LEN=MYGRID%NUMLANDBOUNDARIES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nvel',NVELVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NVELVAR,MYGRID%TOTNUMLANDBOUNDARYNODES))
                    ALLOCATE(NUMNODES_LANDBNDRY(1:MYGRID%NUMLANDBOUNDARIES))
                    ALLOCATE(IBTYPE(1:MYGRID%NUMLANDBOUNDARIES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nvell',NVELLVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NVELLVAR,NUMNODES_LANDBNDRY))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'ibtype',IBTYPEVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,IBTYPEVAR,IBTYPE))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'max_nvell',MXNVELLVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,MXNVELLVAR,MAXNODES))
                    ALLOCATE(LANDBOUNDARY_ENTRIES(1:MYGRID%NUMLANDBOUNDARIES,1:MAXNODES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nbvv',NBVVVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NBVVVAR,LANDBOUNDARY_ENTRIES))
                    ALLOCATE(MYGRID%LANDBC(1:MYGRID%NUMLANDBOUNDARIES))
                    DO I = 1,MYGRID%NUMLANDBOUNDARIES
                        MYGRID%LANDBC(I)%NUMNODES = NUMNODES_LANDBNDRY(I)
                        MYGRID%LANDBC(I)%CODE = IBTYPE(I)
                        SELECT CASE(IBTYPE(I))
                            CASE(0,1,2,10,11,12,20,21,22,30,52)
                                ALLOCATE(MYGRID%LANDBC(I)%N1(1:NUMNODES_LANDBNDRY(I)))
                            CASE(3,13,23)
                                ALLOCATE(MYGRID%LANDBC(I)%N1(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%CREST(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%SUPERCRITICAL(1:NUMNODES_LANDBNDRY(I)))                           
                                
                            CASE(4,24,5,25)
                                ALLOCATE(MYGRID%LANDBC(I)%N1(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%N2(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%CREST(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%SUPERCRITICAL(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%SUBCRITICAL(1:NUMNODES_LANDBNDRY(I)))
                            
                            CASE DEFAULT
                                ALLOCATE(MYGRID%LANDBC(I)%N1(1:NUMNODES_LANDBNDRY(I)))
                        END SELECT
                    ENDDO
#endif                    
                
#endif                
                ELSE
                    READUNIT = GetFreeUnit()
                    OPEN(FILE=TRIM(gridfile),UNIT=READUNIT,ACTION="READ")

                    !...Read header
                    READ(READUNIT,'(A)') MyGrid%title
                    READ(READUNIT,*) MyGrid%NumElements,MyGrid%NumNodes
                    ALLOCATE(MyGrid%nodes(1:MyGrid%NumNodes,1:3))
                    ALLOCATE(MyGrid%conn(1:MyGrid%NumElements,1:3))

                    !...Read nodes
                    DO I = 1,MyGrid%NumNodes
                        READ(READUNIT,*) JunkI,MyGrid%nodes(I,1),&
                            MyGrid%nodes(I,2),MyGrid%nodes(I,3)
                        IF(JunkI.NE.I)THEN
                            WRITE(*,'(A)') ""
                            WRITE(*,'(A)') "ERROR: Mesh needs renumbering."
                            !CALL Quit
                            STOP
                        ENDIF
                        !CALL ShowBar(MyGrid%NumNodes+MyGrid%NumElements,I,N)
                    ENDDO

                    !...Read elements
                    DO I = 1,MyGrid%NumElements
                        READ(READUNIT,*) JunkI,JunkI2,MyGrid%conn(I,1),&
                            MyGrid%conn(I,2),MyGrid%conn(I,3)
                       IF(JunkI.NE.I)THEN
                            WRITE(*,'(A)') ""
                            WRITE(*,'(A)') "ERROR: Mesh needs renumbering."
                            !CALL Quit
                            STOP
                        ENDIF
                        !CALL ShowBar(MyGrid%NumNodes+MyGrid%NumElements,MyGrid%NumNodes+I,N)
                    ENDDO

                    !...Read Open BCs
                    READ(READUNIT,*,END=100) MyGrid%NumOpenBoundaries
                    READ(READUNIT,*,END=100) MyGrid%TotNumOpenBoundaryNodes
                    ALLOCATE(MyGrid%OceanBC(MyGrid%NumOpenBoundaries))
                    DO I = 1,MyGrid%NumOpenBoundaries
                        READ(READUNIT,*) MyGrid%OceanBC(I)%NumNodes
                        ALLOCATE(MyGrid%OceanBC(I)%N1(1:MyGrid%OceanBC(I)%NumNodes))
                        DO J = 1,MyGrid%OceanBC(I)%NumNodes
                            READ(READUNIT,*) MyGrid%OceanBC(I)%N1(J)
                        ENDDO
                    ENDDO

                    !...Read Land BCs
                    READ(READUNIT,*,END=200) MyGrid%NumLandBoundaries
                    READ(READUNIT,*,END=200) MyGrid%TotNumLandBoundaryNodes
                    ALLOCATE(MyGrid%LandBC(MyGrid%NumLandBoundaries))
                    DO I = 1,MyGrid%NumLandBoundaries
                        READ(READUNIT,*) MyGrid%LandBC(I)%NumNodes,&
                            MyGrid%LandBC(I)%Code
                        SELECT CASE(MyGrid%LandBC(I)%Code)
                            CASE(0,1,10,11,12,20,21,22,52)
                                ALLOCATE(MyGrid%LandBC(I)%N1(1:MyGrid%LandBC(I)%NumNodes))
                                DO J = 1,MyGrid%LandBC(I)%NumNodes
                                    READ(READUNIT,*) MyGrid%LandBC(I)%N1(J)
                                ENDDO
                            CASE(13,23)
                                ALLOCATE(MyGrid%LandBC(I)%N1(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Crest(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Supercritical(1:MyGrid%LandBC(I)%NumNodes))
                                DO J = 1,MyGrid%LandBC(I)%NumNodes
                                    READ(READUNIT,*) MyGrid%LandBC(I)%N1(J), &
                                               MyGrid%LandBC(I)%Crest(J), &
                                               MyGrid%LandBC(I)%Supercritical(J)
                                ENDDO
                            CASE(24)
                                ALLOCATE(MyGrid%LandBC(I)%N1(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%N2(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Crest(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Supercritical(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Subcritical(1:MyGrid%LandBC(I)%NumNodes))
                                DO J = 1,MyGrid%LandBC(I)%NumNodes
                                    READ(READUNIT,*) MyGrid%LandBC(I)%N1(J), &
                                               MyGrid%LandBC(I)%N2(J), &
                                               MyGrid%LandBC(I)%Crest(J), &
                                               MyGrid%LandBC(I)%Subcritical(J), &
                                               MyGrid%LandBC(I)%Supercritical(J)
                                ENDDO
                            CASE DEFAULT
                                ALLOCATE(MyGrid%LandBC(I)%N1(1:MyGrid%LandBC(I)%NumNodes))
                                WRITE(*,'(2A,I0)') "WARNING: Unknown boundary ",&
                                    "condition. ADCIRC TYPE = ",&
                                    MyGrid%LandBC(I)%Code
                                WRITE(*,'(A)') "         READ AS A SINGLE NODE."
                                DO J = 1,MyGrid%LandBC(I)%NumNodes
                                    READ(READUNIT,*) MyGrid%LandBC(I)%N1(J)
                                ENDDO
                        END SELECT
                    ENDDO

                    CLOSE(READUNIT)

                    RETURN

100                 CONTINUE
                    MyGrid%NumOpenBoundaries = 0
                    MyGrid%TotNumOpenBoundaryNodes = 0
200                 CONTINUE
                    MyGrid%NumLandBoundaries = 0
                    MyGrid%TotNumLandBoundaryNodes = 0
                    RETURN
                ENDIF

            END SUBROUTINE

            !>Subroutine to write an adcirc grid. Will automatically determine
            !>the FORTRAN write unit so as not to disturb other user written
            !>code
            !>\author Zach Cobell
            SUBROUTINE WriteGrid(gridname,MyGrid)

                IMPLICIT NONE

                !>Name of the file to write the ADCIRC mesh to
                CHARACTER(*),INTENT(IN) :: gridname
                !>ADCIRC mesh that is to be written
                TYPE(grid),INTENT(IN)   :: MyGrid

                INTEGER                 :: I,J
                INTEGER                 :: WRITEUNIT
                INTEGER                 :: N = 0

                WRITEUNIT = GetFreeUnit()

                OPEN(FILE=TRIM(gridname),UNIT=WRITEUNIT,ACTION="WRITE")
                WRITE(WRITEUNIT,'(A)') TRIM(MyGrid%title)
                WRITE(WRITEUNIT,*) MyGrid%NumElements,MyGrid%NumNodes
                DO I = 1,MyGrid%NumNodes
                    WRITE(WRITEUNIT,'(I10,3(2X,F16.8))') I,MyGrid%Nodes(I,1),MyGrid%Nodes(I,2),MyGrid%Nodes(I,3)
                ENDDO
                DO I = 1,MyGrid%NumElements
                    WRITE(WRITEUNIT,'(5(I10,2X))') I,3,MyGrid%conn(I,1),MyGrid%conn(I,2),MyGrid%conn(I,3)
                ENDDO
                WRITE(WRITEUNIT,'(I10)') MyGrid%NumOpenBoundaries
                WRITE(WRITEUNIT,'(I10)') MyGrid%TotNumOpenBoundaryNodes
                DO I = 1,MyGrid%NumOpenBoundaries
                    WRITE(WRITEUNIT,'(I10)') MyGrid%OceanBC(I)%NumNodes
                    DO J = 1,MyGrid%OceanBC(I)%NumNodes
                        WRITE(WRITEUNIT,*) MyGrid%OceanBC(I)%N1(J)
                    ENDDO
                ENDDO
                WRITE(WRITEUNIT,*) MyGrid%NumLandBoundaries
                WRITE(WRITEUNIT,*) MyGrid%TotNumLandBoundaryNodes
                DO I = 1,MyGrid%NumLandBoundaries
                    WRITE(WRITEUNIT,'(I6,4X,I6,4X,A,I6)') MyGrid%LandBC(I)%NumNodes,MyGrid%LandBC(I)%Code,"   !=seg",I
                    SELECT CASE(MyGrid%LandBC(I)%Code)
                        CASE(0,1,10,11,12,20,21,22,52)
                            DO J = 1,MyGrid%LandBC(I)%NumNodes
                                WRITE(WRITEUNIT,'(I10)') MyGrid%LandBC(I)%N1(J)
                            ENDDO
                        CASE(13,23)
                            DO J = 1,MyGrid%LandBC(I)%NumNodes
                                WRITE(WRITEUNIT,'(I10,2X,F16.3,2X,F16.3)') MyGrid%LandBC(I)%N1(J),MyGrid%LandBC(I)%Crest(J),&
                                    MyGrid%LandBC(I)%Supercritical(J)
                            ENDDO
                        CASE(24)
                            DO J = 1,MyGrid%LandBC(I)%NumNodes
                                WRITE(WRITEUNIT,'(I10,2X,I10,2X,F16.3,2X,F16.3,2X,F16.3,2X,F16.3)') &
                                    MyGrid%LandBC(I)%N1(J),MyGrid%LandBC(I)%N2(J),&
                                    MyGrid%LandBC(I)%Crest(J),MyGrid%LandBC(I)%Subcritical(J),&
                                    MyGrid%LandBC(I)%Supercritical(J)
                            ENDDO

                        CASE DEFAULT
                            DO J = 1,MyGrid%LandBC(I)%NumNodes
                                WRITE(WRITEUNIT,'(I10)') MyGrid%LandBC(I)%N1(J)
                            ENDDO
                    END SELECT
                ENDDO

                CLOSE(WRITEUNIT)

                RETURN


            END SUBROUTINE

!................................................................................
!
!                      Nodal Attributes
!
!................................................................................

            !>Subroutine to read a fort.13 file into the fort13 structure.
            !>Read unit will be automatically determined.
            !>\author Zach Cobell
            SUBROUTINE Read13(fort13_file,My13)
                IMPLICIT NONE

                !>File to read the nodal parameter information from
                CHARACTER(*),INTENT(IN) :: fort13_file
                !>Variable containing the fort.13 information read from the file
                TYPE(fort13),INTENT(OUT) :: My13

                CHARACTER(200)  :: JunkC
                CHARACTER(200)  :: TempC
                CHARACTER(200)  :: MyAttribute
                INTEGER   :: I,J,K
                INTEGER   :: IDX
                INTEGER   :: node
                INTEGER   :: READUNIT
                INTEGER   :: NumNonDefault
                REAL(8)   :: VLUE
                REAL(8)   :: DFLT
                REAL(8),ALLOCATABLE :: temp_array(:,:)
                LOGICAL   :: exists

                READUNIT = GETFREEUNIT()

                INQUIRE(FILE=TRIM(fort13_file),EXIST=exists)
                IF(.NOT.exists)THEN
                    WRITE(*,'(A)') "13 file does not exist."
                    STOP
                ENDIF

                OPEN(FILE=TRIM(fort13_file),UNIT=READUNIT,ACTION="READ")

                READ(READUNIT,'(A)') My13%title
                READ(READUNIT,*) My13%NumNodes
                READ(READUNIT,*) My13%NumAttributes
                ALLOCATE(My13%nodal_param(1:My13%NumAttributes))
                DO I = 1,My13%NumAttributes
                    READ(READUNIT,'(A)') TempC
                    My13%nodal_param(I)%Attribute = ADJUSTL(TempC)
                    READ(READUNIT,'(A)') My13%nodal_param(I)%units
                    READ(READUNIT,*) My13%nodal_param(I)%NumValues
                    READ(READUNIT,*) Dflt
                    ALLOCATE(My13%nodal_param(I)%values(&
                        1:My13%NumNodes,1:My13%nodal_param(I)%NumValues))
                    My13%nodal_param(I)%values = Dflt
                    My13%nodal_param(I)%DefaultValue = Dflt
                ENDDO

                DO I = 1,My13%NumAttributes
                    READ(READUNIT,'(A)') MyAttribute
                    READ(READUNIT,*) NumNonDefault
                    IDX = FindAttributeIndex(MyAttribute,My13)

                    IF(My13%nodal_param(IDX)%NumValues.EQ.1)THEN
                        DO J = 1,NumNonDefault
                            READ(READUNIT,*) node,vlue
                            My13%nodal_param(IDX)%values(node,1) = vlue
                        ENDDO
                    ELSEIF(My13%nodal_param(IDX)%NumValues.GT.1)THEN
                        DO J = 1,NumNonDefault
                            READ(READUNIT,*) node,(My13%nodal_param(IDX)%&
                                values(node,k),k=1,My13%nodal_param(IDX)%NumValues)
                        ENDDO
                    ENDIF

                ENDDO

                CLOSE(READUNIT)

            END SUBROUTINE

            !>This subroutine builds an empty fort.13 file. This is useful so
            !>that the user does not need to
            !>\author Zach Cobell
            SUBROUTINE BuildEmptyFort13(NumNodes,NumAttributes,&
                NumValues,DefaultValues,Units,ParamNames,OUTPUT)

                IMPLICIT NONE

                !>Number of nodes to size the arrays for
                INTEGER,INTENT(IN)       :: NumNodes
                !>Number of attributes to build in the empty file
                INTEGER,INTENT(IN)       :: NumAttributes
                !>Number of values for each of the parameters, generally 1 or 12
                INTEGER,INTENT(IN)       :: NumValues(*)
                !>Default values for each parameter
                REAL(8),INTENT(IN)       :: DefaultValues(*)
                !>Names of each parameter in the empty fort.13 file
                CHARACTER(*),INTENT(IN)  :: ParamNames(*)
                !>Units to be used for each parameter in the empty fort.13 file
                CHARACTER(*),INTENT(IN)  :: Units(*)
                !>Variable containing the empty fort.13 file
                TYPE(FORT13),INTENT(OUT) :: OUTPUT

                INTEGER                  :: I

                OUTPUT%TITLE         = "EmptyFort13"
                OUTPUT%NumNodes      = NumNodes
                OUTPUT%NumAttributes = NumAttributes
                ALLOCATE(OUTPUT%nodal_param(1:NumAttributes))
                DO I = 1,NumAttributes
                    ALLOCATE(OUTPUT%nodal_param(I)% &
                        values(1:NumNodes,1:NumValues(I)))
                    OUTPUT%nodal_param(I)%NumValues    = NumValues(I)
                    OUTPUT%nodal_param(I)%DefaultValue = DefaultValues(I)
                    OUTPUT%nodal_param(I)%values(:,:)  = DefaultValues(I)
                    OUTPUT%nodal_param(I)%Attribute    = ParamNames(I)
                    OUTPUT%nodal_param(I)%Units        = Units(I)
                ENDDO

                RETURN

            END SUBROUTINE

            !>Subroutine to write a fort.13 file
            !>\author Zach Cobell
            SUBROUTINE Write13(filename,Input13,REDUCE)
                IMPLICIT NONE

                !>Name of the file to write
                CHARACTER(*),INTENT(IN)     :: filename
                !>Variable to write the fort.13 from
                TYPE(fort13),INTENT(IN)     :: Input13
                !>Optional: Select an optimal value for the default value
                !>in the header of the fort.13. This will likely reduce file
                !>size
                LOGICAL,INTENT(IN),OPTIONAL :: Reduce

                INTEGER                 :: I,J,K
                INTEGER,ALLOCATABLE     :: defaults(:)
                INTEGER                 :: WRITEUNIT
                CHARACTER(50)           :: MultipleValueFormat
                REAL(8)                 :: default_value
                LOGICAL                 :: DoReduce
                TYPE(FORT13)            :: My13

                WRITEUNIT = GETFREEUNIT()

                !...Check if new default values should be calculated
                IF(.NOT.PRESENT(REDUCE))THEN
                    DoReduce = .TRUE.
                ELSE
                    DoReduce = Reduce
                ENDIF

                My13 = Input13
                IF(DoReduce)THEN
                    CALL Reduce13(My13)
                ENDIF

                OPEN(FILE=TRIM(filename),UNIT=WRITEUNIT,ACTION="WRITE")

                WRITE(WRITEUNIT,'(A)') TRIM(ADJUSTL(My13%title))
                WRITE(WRITEUNIT,'(I0)') My13%NumNodes
                WRITE(WRITEUNIT,'(I0)') My13%NumAttributes
                !...Count non default values
                ALLOCATE(defaults(1:My13%NumAttributes))
                defaults(:) = 0
                DO I = 1,My13%NumAttributes
                    default_value = My13%nodal_param(I)%DefaultValue
                    IF(My13%nodal_param(I)%NumValues.EQ.1)THEN
                        DO J = 1,My13%NumNodes
                            IF(My13%nodal_param(I)%values(J,1).NE.&
                                My13%nodal_param(I)%DefaultValue)THEN
                                    defaults(I) = defaults(I) + 1
                            ENDIF
                        ENDDO
                    ELSEIF(My13%nodal_param(I)%NumValues.GT.1)THEN
                        DO J = 1,My13%NumNodes
                            IF(SUM(My13%nodal_param(I)%values(J,:)).NE.0d0)THEN
                                defaults(I) = defaults(I) + 1
                            ENDIF
                        ENDDO
                    ENDIF
                 ENDDO

                 !...Write 13 header
                 DO I = 1,My13%NumAttributes
                    WRITE(WRITEUNIT,'(A)') TRIM(ADJUSTL(My13%nodal_param(I)%Attribute))
                    WRITE(WRITEUNIT,'(A)') TRIM(ADJUSTL(My13%nodal_param(I)%units))
                    WRITE(WRITEUNIT,'(I0)') My13%nodal_param(I)%NumValues
                    IF(My13%nodal_param(I)%NumValues.EQ.1)THEN
                        WRITE(WRITEUNIT,'(F0.6)') My13%nodal_param(I)%DefaultValue
                    ELSEIF(My13%nodal_param(I)%Numvalues.GT.1)THEN
                        WRITE(MultipleValueFormat,'(A,I0,A)') "(",&
                               My13%nodal_param(I)%Numvalues,"(F10.6,X))"
                        WRITE(WRITEUNIT,TRIM(MultipleValueFormat)) &
                            (My13%nodal_param(I)%DefaultValue,k=1,&
                            My13%nodal_param(I)%Numvalues)
                    ENDIF
                 ENDDO


                 DO I = 1,My13%NumAttributes
                    WRITE(WRITEUNIT,'(A)') TRIM(My13%nodal_param(I)%Attribute)
                    WRITE(WRITEUNIT,'(I0)') defaults(I)
                    IF(My13%nodal_param(I)%Numvalues.EQ.1)THEN
                        DO J = 1,My13%NumNodes
                            IF(My13%nodal_param(I)%values(J,1).NE.&
                                My13%nodal_param(I)%DefaultValue)THEN
                                WRITE(WRITEUNIT,'(I10,2X,F10.6)') J, My13%nodal_param(I)%values(J,1)
                            ENDIF
                        ENDDO
                    ELSEIF(My13%nodal_param(I)%Numvalues.GT.1)THEN
                        WRITE(MultipleValueFormat,'(A,I0,A)') "(I0,2X,",&
                            My13%nodal_param(I)%Numvalues,"(F10.6,X))"
                        DO J = 1,My13%NumNodes
                           IF(SUM(My13%nodal_param(I)%values(J,:)).NE.0d0)THEN
                                WRITE(WRITEUNIT,MultipleValueFormat) J,&
                                    (My13%nodal_param(I)%values(J,K),&
                                    K=1,My13%nodal_param(I)%Numvalues)
                           ENDIF
                        ENDDO
                    ENDIF
                ENDDO


            END SUBROUTINE

            !>Subroutine that selects the best value for the default value in
            !>a fort.13 file. This is done by first sorting all values for each
            !>parameter, then generating a unique list, and finally counting the
            !>number of times each item on the unique list appears.
            !>\author Zach Cobell
            SUBROUTINE Reduce13(My13,DETAILIN)
                IMPLICIT NONE

                !>The fort.13 variable to be optimized. It will be edited
                !>in place, so there is no need to specify an output
                !>variable.
                TYPE(Fort13),INTENT(INOUT) :: My13
                !>Optional: The user may wish for the code to print status
                !>information about the selection of parameters. This can be
                !>helpful in diagnosing obvious issues with your fort.13 file
                !>such as many values where few are expected.
                LOGICAL,INTENT(IN),OPTIONAL  :: DETAILIN

                INTEGER                    :: I
                INTEGER                    :: N
                REAL(8),ALLOCATABLE        :: A(:,:)
                REAL(8),ALLOCATABLE        :: V(:)
                REAL(8),ALLOCATABLE        :: TEMP(:)
                INTEGER,ALLOCATABLE        :: U(:)
                LOGICAL                    :: DETAIL

                IF(PRESENT(DETAILIN))THEN
                    DETAIL = DETAILIN
                ELSE
                    DETAIL = .FALSE.
                ENDIF

                ALLOCATE(A(1:My13%NumAttributes,1:My13%NumNodes))
                ALLOCATE(U(1:My13%NumAttributes))
                ALLOCATE(V(1:My13%NumAttributes))
                N = My13%NumNodes
                ALLOCATE(TEMP(1:N))

                !...Use HEAPSORT to sort all parameters in ascending order
                !   very quickly.
                DO I = 1,My13%NumAttributes
                    IF(My13%nodal_param(I)%Numvalues.LT.2)THEN
                        A(I,:) = My13%nodal_param(I)%values(:,1)
                        IF(DETAIL)WRITE(*,'(3A,$)') "Start Heapsort on ",&
                        TRIM(My13%nodal_param(I)%attribute),"..."
                        TEMP(1:N) = A(I,1:N)
                        CALL HEAPSORT(My13%NumNodes,TEMP)
                        A(I,1:N) = TEMP(1:N)
                        IF(DETAIL)WRITE(*,'(A)') "done!"
                    ENDIF
                ENDDO

                IF(DETAIL)WRITE(*,'(A)') ""

                !...Count Unique Values
                IF(DETAIL) WRITE(*,'(A)') "Counting unique values..."
                DO I = 1,My13%NumAttributes
                IF(My13%nodal_param(I)%NumValues.LT.2)THEN
                        TEMP(1:N) = A(I,1:N)
                        CALL Unique(TEMP,My13%NumNodes,U(I))
                        IF(DETAIL)WRITE(*,'(2A,I0)') TRIM(My13%nodal_param(I)%attribute),&
                            ": ",U(I)
                    ENDIF
                ENDDO

                !...Find Frequency
                IF(DETAIL) WRITE(*,'(A)') ""
                IF(DETAIL) WRITE(*,'(A)') "Finding value with maximum frequency..."
                DO I = 1, My13%NumAttributes
                    IF(My13%nodal_param(I)%NumValues.LT.2)THEN
                        TEMP(1:N) = A(I,1:N)
                        CALL Frequency(TEMP,My13%NumNodes,U(I),V(I))
                        IF(DETAIL)WRITE(*,'(3A,F0.6)') "New default for ",&
                            TRIM(My13%nodal_param(I)%attribute),": ",V(I)
                        My13%nodal_param(I)%DefaultValue = V(I)
                    ENDIF
                ENDDO

            END SUBROUTINE

!................................................................................
!
!                      ADCIRC Output
!
!................................................................................

            !>Subroutine to write ADCIRC Output to file in ASCII format from an
            !>ADCIRCOutput variable with sparse or full format depending user
            !>selection
            !>\author Zach Cobell
            SUBROUTINE WriteADCIRCOutputASCII(OutputFile,MyOutput,USparse)
                IMPLICIT NONE
                !>Name of file to be written
                CHARACTER(*),INTENT(IN)       :: OutputFile
                !>Variable containing ADCIRC output information
                TYPE(ADCIRCOutput),INTENT(IN) :: MyOutput
                !>Optionally select sparse or full output. Default: Full
                LOGICAL,INTENT(IN),OPTIONAL   :: USparse

                CHARACTER(200)     :: WRITEFORMAT
                INTEGER            :: I
                INTEGER            :: J
                INTEGER            :: K
                INTEGER            :: WRITEUNIT
                LOGICAL            :: Sparse

                WRITEUNIT = GETFREEUNIT()

                !...DEFAULT TO NON-SPARSE
                IF(PRESENT(USparse))THEN
                    SPARSE = USPARSE
                ELSE
                    SPARSE = .FALSE.
                ENDIF

                WRITE(WRITEFORMAT,'(A,I0,A)') "(2X,I8,", &
                                  MyOutput%NumValues,"(2X,1PE20.10E3))"

                OPEN(FILE=TRIM(OutputFile),UNIT=WRITEUNIT,ACTION="WRITE")
                WRITE(WRITEUNIT,'(A)') TRIM(MyOutput%Title)
                WRITE(WRITEUNIT,&
                        '(I0,2X,I0,2X,F0.6,2X,I0,2X,I0,2X,A,2X,I0)') &
                    MyOutput%NumTimeSteps,MyOutput%NumNodes,&
                    DBLE(MyOutput%dt),INT(MyOutput%dt),&
                    MyOutput%NumValues,"FileFmtVersion:",&
                    MyOutput%Fileformat
                DO I = 1,MyOutput%NumTimeSteps
                    IF(Sparse)THEN
                        WRITE(WRITEUNIT,*) MyOutput%Output(I)%Time,&
                            MyOutput%Output(I)%TS, &
                            MyOutput%Output(I)%NumNonDefault,&
                            MyOutput%Output(I)%DefaultValue
                    ELSE
                        WRITE(WRITEUNIT,*) MyOutput%Output(I)%Time,&
                            MyOutput%Output(I)%TS
                    ENDIF
                    IF(Sparse)THEN
                        DO J = 1,MyOutput%NumNodes
                            IF(ALL(MyOutput%Output(I)%Values(J,:).NE.&
                                    MyOutput%Output(I)%DefaultValue))THEN
                                WRITE(WRITEUNIT,WRITEFORMAT) &
                                    (MyOutput%Output(I)%Values(J,K), &
                                     K=1,MyOutput%NumValues)
                            ENDIF
                        ENDDO
                    ELSE
                        DO J = 1,MyOutput%NumNodes
                            WRITE(WRITEUNIT,WRITEFORMAT) J, &
                                (MyOutput%Output(I)%Values(J,K),K=1,&
                                 MyOutput%NumValues)
                        ENDDO
                    ENDIF
                ENDDO
                CLOSE(WRITEUNIT)

            END SUBROUTINE

            !>Used for initializing a netCDF output file. If the file is not
            !>to be netCDF (determined by the .nc extension), then an ASCII file
            !>is written when this routine is called.
            !>\author Zach Cobell
            SUBROUTINE InitializeADCIRCOutput(Filename,MyOutput,&
                    MyMesh)
                IMPLICIT NONE
                !>Filename to be written. Also used to determine format
                CHARACTER(*),INTENT(IN)          :: filename
                !>ADCIRC Output data to be written
                TYPE(ADCIRCOUTPUT),INTENT(IN)    :: MyOutput
                !>Optional: If using netCDF, ADCIRC mesh data can be written to
                !>the file as would be in a standard ADCIRC output file. This is
                !>recommended, however, it is not required.
                TYPE(GRID),INTENT(IN),OPTIONAL   :: MyMesh

                IF(INDEX(filename,".nc").GT.0)THEN
                    IF(PRESENT(MyMesh))THEN
                        CALL InitializeADCIRCNetCDFOutput(filename,MyOutput,&
                            MyMesh)
                    ELSE
                        CALL InitializeADCIRCNetCDFOutput(filename,MyOutput)
                    ENDIF
                ELSE
                    CALL WRITEADCIRCOUTPUTASCII(filename,MyOutput) !...Temporarily preserves old behavior
                !    CALL InitializeADCIRCASCIIOutput(filename,MyOutput,&
                !        FileUnit)
                ENDIF
                RETURN

            END SUBROUTINE

            !>Subroutine to initialize an ADCIRC netCDF output file. This will
            !>set up the variables and write the mesh data if it is provided.
            !>Note that the user should not have the need to call this routine
            !>and should instead use the WriteADCIRCOutput routine to have features
            !>handled automatically
            !>\author Zach Cobell
            SUBROUTINE InitializeADCIRCNetCDFOutput(filename,MyOutput,&
                    MyMesh)
                IMPLICIT NONE
                !>Filename to be written
                CHARACTER(*),INTENT(IN)        :: filename
                !>Output data to be written
                TYPE(ADCIRCOUTPUT),INTENT(IN)  :: MyOutput
                !>Optional: Mesh data to be written.
                TYPE(GRID),INTENT(IN),OPTIONAL :: MyMesh

                CHARACTER(200)                 :: NC_VARIABLE
                CHARACTER(200)                 :: NC_VARIABLE2

                INTEGER                        :: I
                INTEGER                        :: NCOL
                INTEGER                        :: NCID
                INTEGER                        :: VAR_IDX
                INTEGER                        :: NC_FILETYPE
                INTEGER                        :: VARID_TIME
                INTEGER                        :: VARID_X
                INTEGER                        :: VARID_Y
                INTEGER                        :: VARID_DEPTH
                INTEGER                        :: VARID_ELEMENT
                INTEGER                        :: VARID_ADC(2)
                INTEGER                        :: DIMID_NODE
                INTEGER                        :: DIMID_NELE
                INTEGER                        :: DIMID_NVERTEX
                INTEGER                        :: DIMID_SINGLE
                INTEGER                        :: DIMID_TIME
                INTEGER,ALLOCATABLE            :: HOLDER2(:,:)

                REAL(8),ALLOCATABLE            :: HOLDER(:)

                LOGICAL                        :: exists

#ifndef ADCNETCDF
                WRITE(*,'(A)') "ERROR: ADCModules not compiled for "//&
                               "NetCDF."
                STOP
#else

                NC_FILETYPE=NF90_CLOBBER
#ifdef HAVE_NETCDF4
                NC_FILETYPE=IOR(NF90_HDF5,NF90_CLASSIC_MODEL)
#endif

                CALL CHECK(NF90_CREATE(TRIM(filename),NC_FILETYPE,NCID))
                CALL CHECK(NF90_DEF_DIM(NCID,'time',NF90_UNLIMITED,&
                    DIMID_TIME))
                CALL CHECK(NF90_DEF_VAR(NCID,'time',NF90_DOUBLE,&
                    DIMID_TIME,VARID_TIME))

                NC_VARIABLE  = MyOutput%NC_VARIABLE1
                NC_VARIABLE2 = MyOutput%NC_VARIABLE2
                NCOL         = MyOutput%NumValues

                DO I = 1,SIZE(NETCDF_TYPES)
                    IF(TRIM(NC_VARIABLE).EQ.NETCDF_TYPES(I))THEN
                        VAR_IDX = I
                        EXIT
                    ENDIF
                    IF(I.EQ.SIZE(NETCDF_TYPES))THEN
                        WRITE(*,'(A)') "ERROR: Invalid NetCDF variable."
                        STOP
                    ENDIF
                ENDDO

                !...Start putting mesh variables if mesh was input
                !   Note: Not full ADCIRC implementation
                CALL CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,"dt",MyOutput%dt))
                IF(PRESENT(MYMESH))THEN
                    CALL CHECK(NF90_DEF_DIM(NCID,'node',MyMesh%NumNodes,DIMID_NODE))
                    CALL CHECK(NF90_DEF_DIM(NCID,'nele',MyMesh%NumElements,DIMID_NELE))
                    CALL CHECK(NF90_DEF_DIM(NCID,'nvertex',3,DIMID_NVERTEX))
                    CALL CHECK(NF90_DEF_DIM(NCID,'single',1,DIMID_SINGLE))

                    CALL CHECK(NF90_DEF_VAR(NCID,'x',NF90_DOUBLE,DIMID_NODE,VARID_X))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_X,'long_name','longitude'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_X,'standard_name','longitude'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_X,'units','degrees_east'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_X,'positive','east'))

                    CALL CHECK(NF90_DEF_VAR(NCID,'y',NF90_DOUBLE,DIMID_NODE,VARID_Y))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_Y,'long_name','latitude'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_Y,'standard_name','latitude'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_Y,'units','degrees_north'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_Y,'positive','north'))

                    CALL CHECK(NF90_DEF_VAR(NCID,'element',NF90_INT,(/DIMID_NVERTEX,DIMID_NELE/),VARID_ELEMENT))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ELEMENT,'long_name','element'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ELEMENT,'standard_name','face_node_connectivity'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ELEMENT,'units','nondimensional'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ELEMENT,'start_index',1))

                    CALL CHECK(NF90_DEF_VAR(NCID,'depth',NF90_DOUBLE,VARID_DEPTH))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'long_name','distance_from_geoid'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'standard_name','depth_below_geoid'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'location','node'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'mesh','adcirc_mesh'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'units','m'))

#ifdef HAVE_NETCDF4
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_X,1,1,2))
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_Y,1,1,2))
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_DEPTH,1,1,2))
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_ELEMENT,1,1,2))
#endif

                ENDIF

                !...Adcirc Variable
                CALL CHECK(NF90_DEF_VAR(NCID,NETCDF_TYPES(VAR_IDX),NF90_DOUBLE,(/DIMID_NODE,DIMID_TIME/),VARID_ADC(1)))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'_FillValue',-99999.0D0))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'long_name',NC_LONGNAME(VAR_IDX)))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'standard_name',NC_STDNAME(VAR_IDX)))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'coordinates','time y x'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'location','node'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'mesh','adcirc_mesh'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'units','metric'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'positive','east'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'dry_Value',-99999.0d0))
                IF(NCOL.EQ.2)THEN
                    CALL CHECK(NF90_DEF_VAR(NCID,NETCDF_TYPES(VAR_IDX+1),NF90_DOUBLE,(/DIMID_NODE,DIMID_TIME/),VARID_ADC(2)))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'_FillValue',-99999.0D0))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'long_name',NC_LONGNAME(VAR_IDX+1)))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'standard_name',NC_STDNAME(VAR_IDX+1)))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'coordinates','time y x'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'location','node'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'mesh','adcirc_mesh'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'units','metric'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'positive','east'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'dry_Value',-99999.0d0))
                ENDIF
#ifdef HAVE_NETCDF4
                CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_ADC(1),1,1,2))
                IF(NCOL.EQ.2)THEN
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_ADC(2),1,1,2))
                ENDIF
#endif

                !...Finished with definitions
                CALL CHECK(NF90_ENDDEF(NCID))

                IF(PRESENT(MyMesh))THEN
                    ALLOCATE(HOLDER(1:MyMesh%NumNodes))
                    HOLDER(:) = MyMesh%Nodes(:,1)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID_X,Holder))
                    HOLDER(:) = MyMesh%Nodes(:,2)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID_Y,Holder))
                    HOLDER(:) = MyMesh%Nodes(:,3)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID_DEPTH,Holder))
                    DEALLOCATE(HOLDER)
                    ALLOCATE(HOLDER2(1:3,1:MyMesh%NumElements))
                    Holder2(1,:) = MyMesh%Conn(:,1)
                    Holder2(2,:) = MyMesh%Conn(:,2)
                    Holder2(3,:) = MyMesh%Conn(:,3)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID_ELEMENT,Holder2))
                ENDIF

                !...Close NetCDF file
                CALL CHECK(NF90_CLOSE(NCID))

                RETURN
#endif

            END SUBROUTINE

            !>This routine writes the next output interval for an ADCIRC netCDF
            !>output file. If it is called for an ADCII file, the entire file
            !>will be written to the specified file.
            !>\author Zach Cobell
            SUBROUTINE WriteADCIRCOutput(Filename,MyOutput,MyMesh,FirstCall)
                IMPLICIT NONE

                !>File to be written
                CHARACTER(*),INTENT(IN)        :: Filename
                !>Output data to be written
                TYPE(ADCIRCOUTPUT),INTENT(IN)  :: MyOutput
                !>Optional: Mesh data to be written
                TYPE(GRID),INTENT(IN),OPTIONAL :: MyMesh
                !>Variable to inform the code if this is the first time the
                !>routine is being called and if initialization is required. It
                !>will be set to false upon exit automatically
                LOGICAL,INTENT(INOUT)          :: FirstCall
                LOGICAL                        :: exists

                !...Check if initialization has occured
                IF(FirstCall)THEN
                    IF(PRESENT(MyMesh))THEN
                        CALL InitializeADCIRCOutput(Filename,MyOutput,MyMesh)
                    ELSE
                        CALL InitializeADCIRCOutput(Filename,MyOutput)
                    ENDIF
                    FirstCall = .FALSE.
                ENDIF

                IF(INDEX(Filename,".nc").GT.0)THEN
                    CALL WriteADCIRCOutputNETCDF(Filename,MyOutput)
                ELSE
                    CALL WriteADCIRCOutputASCII(Filename,MyOutput)
                ENDIF

                RETURN

            END SUBROUTINE

            !>This subroutine writes the next output cycle to an ADCIRC
            !>netCDF formatted file. The user should not have need to call this
            !>routine.
            !>\author Zach Cobell
            SUBROUTINE WriteADCIRCOutputNETCDF(Filename,MyOutput)
                IMPLICIT NONE

                !>File to be written
                CHARACTER(*),INTENT(IN)       :: Filename
                !>Output data to be written
                TYPE(ADCIRCOUTPUT),INTENT(IN) :: MYOUTPUT

                INTEGER                       :: NCID
                INTEGER                       :: VARID1
                INTEGER                       :: VARID2
                INTEGER                       :: NCOLS
                INTEGER                       :: NATT
                INTEGER                       :: NVAR
                INTEGER                       :: NDIM
                INTEGER                       :: DIMID_TIME
                INTEGER                       :: VARID_TIME
                INTEGER                       :: NC_FORMAT
                INTEGER                       :: NSNAP
                INTEGER                       :: NC_COUNT(2)
                INTEGER                       :: NC_START(2)

                REAL(8),ALLOCATABLE           :: HOLDER(:)
                REAL(8)                       :: Time(1)

#ifdef ADCNETCDF
                !...Open the netcdf file
                CALL CHECK(NF90_OPEN(TRIM(FILENAME),NF90_WRITE,NCID))

                !...Find the netcdf variable
                CALL GetNETCDFVarID(NCID,VARID1,VARID2,NCOLS)

                !...Find out how many datasets currently in file
                CALL CHECK(NF90_INQUIRE(NCID,NDIM,NVAR,NATT,DIMID_TIME,NC_FORMAT))
                CALL CHECK(NF90_INQUIRE_DIMENSION(NCID,DIMID_TIME,LEN=NSNAP))
                CALL CHECK(NF90_INQ_VARID(NCID,"time",VARID_TIME))

                !...Increment to the next snap
                NSNAP = NSNAP + 1

                !...Put new time into file
                Time(1) = MyOutput%Output(1)%Time
                CALL CHECK(NF90_PUT_VAR(NCID,VARID_TIME,Time,(/NSNAP/),(/1/)))

                !...Put the record into the file
                ALLOCATE(HOLDER(1:MyOutput%NumNodes))
                HOLDER(:) = MyOutput%Output(1)%Values(:,1)
                NC_COUNT = (/MyOutput%NumNodes, 1/)
                NC_START = (/1,NSNAP/)

                CALL CHECK(NF90_PUT_VAR(NCID,VARID1,HOLDER,NC_START,NC_COUNT))
                IF(NCOLS.EQ.2)THEN
                    HOLDER(:) = MyOutput%Output(1)%Values(:,2)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID2,HOLDER,NC_START,NC_COUNT))
                ENDIF
                DEALLOCATE(HOLDER)

                !...Close NetCDF
                CALL CHECK(NF90_CLOSE(NCID))

                RETURN
#endif

            END SUBROUTINE

            !>This routine will read an entire ADCIRC ASCII output file. It can
            !>be used for global or station output files.
            !>\author Zach Cobell
            SUBROUTINE ReadADCIRCOutput(filename,MyOutput)
                IMPLICIT NONE

                !>File to be read
                CHARACTER(*),INTENT(IN) :: filename
                !>Variable to store data read from ADCIRC output file
                TYPE(AdcircOutput),INTENT(OUT) :: MyOutput

                CHARACTER(500) :: JunkC
                INTEGER :: I
                INTEGER :: J
                INTEGER :: K
                INTEGER :: JunkI
                INTEGER :: TempN
                INTEGER :: READUNIT
                REAL(8),ALLOCATABLE :: TempV(:)
                REAL(8) :: JunkR
                LOGICAL :: exists
                LOGICAL :: sparse

                READUNIT = GETFREEUNIT()

                INQUIRE(FILE=TRIM(filename),EXIST=exists)
                IF(.NOT.exists)THEN
                    WRITE(*,'(A)') ""
                    WRITE(*,'(A)') "ADCIRC output file doesn't exist."
                    STOP
                ENDIF

                OPEN(FILE=TRIM(filename),UNIT=READUNIT,ACTION="READ")
                READ(READUNIT,'(A)') MyOutput%title
                READ(READUNIT,'(A)') JunkC

                MyOutput%NumTimeSteps = -999999
                MyOutput%NumNodes = -999999
                MyOutput%dt = -999999
                MyOutput%NumValues = -999999
                MyOutput%Fileformat = -999999

                READ(JunkC,*,END=100,ERR=100) MyOutput%NumTimeSteps,MyOutput%NumNodes,JunkR,&
                    MyOutput%dt,MyOutput%NumValues,JunkC,&
                    MyOutput%Fileformat
100             CONTINUE
                IF( ( MyOutput%NumTimeSteps.LE.-900000 ) .OR. ( MyOutput%NumNodes.LE.-900000 ) &
                    .OR. ( MyOutput%dt.LE.-900000 ) )THEN
                    WRITE(*,'(A)') "Error reading file."
                    STOP
                ELSEIF(MyOutput%NumValues.LE.-900000)THEN
                    MyOutput%NumValues = 1
                ELSEIF(MyOutput%Fileformat.LE.-900000)THEN
                    MyOutput%FileFormat = 1050624
                ENDIF

                !...Determine output format (Sparse/Full)
                READ(READUNIT,'(A)') JunkC
                sparse = .FALSE.
                READ(JunkC,*,END=200,ERR=200) JunkR,JunkI,JunkI,JunkR
                sparse = .TRUE.
200             CONTINUE
                BACKSPACE(READUNIT)
                ALLOCATE(MyOutput%Output(1:MyOutput%NumTimeSteps))
                ALLOCATE(TempV(1:MyOutput%NumValues))
                DO K = 1,MyOutput%NumTimeSteps
                    IF(sparse)THEN
                        READ(READUNIT,*) MyOutput%Output(K)%Time,MyOutput%Output(K)%TS,&
                            MyOutput%Output(K)%NumNonDefault,MyOutput%Output(K)%DefaultValue
                    ELSE
                        READ(READUNIT,*) MyOutput%Output(K)%Time,MyOutput%Output(K)%TS
                        MyOutput%Output(K)%NumNonDefault = MyOutput%NumNodes
                        MyOutput%Output(K)%DefaultValue = -99999d0
                    ENDIF
                    ALLOCATE(MyOutput%Output(K)%Values(MyOutput%NumNodes,1:MyOutput%NumValues))
                    MyOutput%Output(K)%Values(1:MyOutput%NumNodes,1:MyOutput%NumValues) = &
                        MyOutput%Output(K)%DefaultValue
                    DO I = 1,MyOutput%Output(K)%NumNonDefault
                        READ(READUNIT,*) TempN,(TempV(J),J=1,MyOutput%NumValues)
                        MyOutput%Output(K)%Values(TempN,:) = TempV(:)
                    ENDDO
                ENDDO

                CLOSE(READUNIT)

                RETURN

            END SUBROUTINE

!................................................................................
!
!                      ADCIRC Binary Hot Start
!
!................................................................................

        !>Subroutine to read an ADCIRC 2D binary hot start file.
        !>\author Zach Cobell
        SUBROUTINE ReadHotStart2D(Filename,HSOut)
            IMPLICIT NONE

            !>File to be read
            CHARACTER(*),INTENT(IN)      :: FILENAME
            !>Variable containing 2D hot start data
            TYPE(HOTSTART2D),INTENT(OUT) :: HSOut

            INTEGER                 :: IHOT
            INTEGER                 :: IHOTSTP


            OPEN(FILE=TRIM(Filename),UNIT=68,ACTION="READ",&
                ACCESS="DIRECT",RECL=8,STATUS="OLD")

            !...Begin hot start read
            IHOT = GETFREEUNIT()
            IHOTSTP = 1
            READ(IHOT,REC=IHOTSTP) HSOUT%InputFileFmtVn ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IMHS           ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%TimeLoc        ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%ITHS           ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NP_G_IN        ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NE_G_IN        ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NP_A_IN        ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NE_A_IN        ; IHOTSTP = IHOTSTP + 1
            ALLOCATE(HSOUT%ETA1(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%ETA2(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%ETADisc(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%UU2(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%VV2(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%NNODECODE(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%NOFF(1:HSOUT%NE_G_IN))
            IF(HSOUT%IMHS.EQ.10)ALLOCATE(HSOUT%CH1(1:HSOUT%NP_G_IN))

            CALL BinaryRead2D(HSOUT%ETA1,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryRead2D(HSOUT%ETA2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryRead2D(HSOUT%ETADisc,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryRead2D(HSOUT%UU2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryRead2D(HSOUT%VV2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            IF(HSOUT%IMHS.EQ.10)CALL BinaryRead2D(HSOUT%CH1,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryIntRead2D(HSOUT%NNODECODE,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryIntRead2D(HSOUT%NOFF,HSOUT%NE_G_IN,IHOT,IHOTSTP)
            READ(IHOT,REC=IHOTSTP) HSOUT%IESTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUE ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IVSTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUV ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%ICSTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUC ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IPSTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IWSTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUM ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGEP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUGE ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGVP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUGV ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGCP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUGC ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGPP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGWP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUGW ; IHOTSTP = IHOTSTP + 1
            !...End hot start read
            CLOSE(IHOT)
            RETURN

        END SUBROUTINE

        !>Subroutine to read a real(8) array from a ADCIRC binary hot start file
        !>\author Zach Cobell
        SUBROUTINE BinaryRead2D(array,length,lun,counter)
            IMPLICIT NONE
            !>Array containing the information read from the hot start file
            REAL(8),INTENT(OUT),DIMENSION(:) :: ARRAY(*)
            !>Length of the array that will be read
            INTEGER,INTENT(IN)               :: LENGTH
            !>FORTRAN unit number to read
            INTEGER,INTENT(IN)               :: LUN
            !>Counter that keeps track of binary record position
            INTEGER,INTENT(INOUT)            :: COUNTER

            INTEGER                          :: I
            REAL(8)                          :: TempR

            DO I = 1,LENGTH
                READ(LUN,REC=COUNTER) ARRAY(I)
                COUNTER = COUNTER + 1
            ENDDO

            RETURN

        END SUBROUTINE

        !>Subroutine to read an integer array form an ADCIRC binary hot start file
        !>\author Zach Cobell
        SUBROUTINE BinaryIntRead2D(array,length,lun,counter)
            IMPLICIT NONE
            !>Array containing the information read from the hot start file
            INTEGER,INTENT(OUT),DIMENSION(:) :: ARRAY(*)
            !>Length of the array that will be read
            INTEGER,INTENT(IN)               :: LENGTH
            !>FORTRAN unit number to read
            INTEGER,INTENT(IN)               :: LUN
            !>Counter that keeps track of binary record position
            INTEGER,INTENT(INOUT)            :: COUNTER

            INTEGER                          :: I

            DO I = 1,LENGTH
                READ(LUN,REC=COUNTER) ARRAY(I)
                COUNTER = COUNTER + 1
            ENDDO

            RETURN

        END SUBROUTINE

        !>Subroutine to write a 2D ADCIRC hot start file
        !>\author Zach Cobell
        SUBROUTINE WriteHotStart2D(Filename,HSOut)
            IMPLICIT NONE
            !>File to be written
            CHARACTER(*),INTENT(IN)      :: FILENAME
            !>2D hot start data to be written
            TYPE(HOTSTART2D),INTENT(IN)  :: HSOut

            INTEGER                      :: IHOT
            INTEGER                      :: IHOTSTP

            OPEN(FILE=TRIM(Filename),UNIT=68,ACTION="WRITE",&
                ACCESS="DIRECT",RECL=8)

            !...Begin hot start write
            IHOT = 68
            IHOTSTP = 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%InputFileFmtVn ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IMHS           ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%TimeLoc        ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%ITHS           ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NP_G_IN        ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NE_G_IN        ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NP_A_IN        ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NE_A_IN        ; IHOTSTP = IHOTSTP + 1
            CALL BinaryWrite2D(HSOUT%ETA1,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryWrite2D(HSOUT%ETA2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryWrite2D(HSOUT%ETADisc,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryWrite2D(HSOUT%UU2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryWrite2D(HSOUT%VV2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            IF(HSOUT%IMHS.EQ.10)CALL BinaryWrite2D(HSOUT%CH1,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryIntWrite2D(HSOUT%NNODECODE,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryIntWrite2D(HSOUT%NOFF,HSOUT%NE_G_IN,IHOT,IHOTSTP)
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IESTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUE ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IVSTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUV ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%ICSTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUC ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IPSTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IWSTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUM ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGEP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUGE ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGVP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUGV ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGCP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUGC ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGPP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGWP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUGW ; IHOTSTP = IHOTSTP + 1
            !...End hot start read
            RETURN

        END SUBROUTINE

        !>Subroutine to write a real(8) array to a binary hot start file
        !>\author Zach Cobell
        SUBROUTINE BinaryWrite2D(array,length,lun,counter)
            IMPLICIT NONE
            !>Array to write to file
            REAL(8),INTENT(IN),DIMENSION(:)  :: ARRAY(*)
            !>Length of array to write
            INTEGER,INTENT(IN)               :: LENGTH
            !>FORTRAN unit number to write to
            INTEGER,INTENT(IN)               :: LUN
            !>Binary record position counter
            INTEGER,INTENT(INOUT)            :: COUNTER
            
            INTEGER                          :: I
            REAL(8)                          :: TempR

            DO I = 1,LENGTH
                WRITE(LUN,REC=COUNTER) ARRAY(I)
                COUNTER = COUNTER + 1
            ENDDO

            RETURN

        END SUBROUTINE

        !>Subroutine to write an integer array to a binary hot start file
        !>\author Zach Cobell
        SUBROUTINE BinaryIntWrite2D(array,length,lun,counter)
            IMPLICIT NONE
            !>Array to write to file
            INTEGER,INTENT(IN),DIMENSION(:)  :: ARRAY(*)
            !>Length of array to write
            INTEGER,INTENT(IN)               :: LENGTH
            !>FORTRAN unit number to write to
            INTEGER,INTENT(IN)               :: LUN
            !>Binary record position counter
            INTEGER,INTENT(INOUT)            :: COUNTER
            INTEGER                          :: I

            DO I = 1,LENGTH
                WRITE(LUN,REC=COUNTER) ARRAY(I)
                COUNTER = COUNTER + 1
            ENDDO

            RETURN

        END SUBROUTINE

!................................................................................
!
!                      IMEDS Functions
!
!................................................................................
        !>Subroutine to read an IMEDS formatted file and save the data in a 
        !>container
        !>\author Zach Cobell
        SUBROUTINE READ_IMEDS(Filename,OutIMEDS)
            IMPLICIT NONE
            !>File to be read
            CHARACTER(*),INTENT(IN)   :: Filename
            !>Data container where information will
            !>be read into
            TYPE(IMEDS),INTENT(OUT)   :: OutIMEDS

            CHARACTER(200)            :: TempName
            CHARACTER(2000)           :: TempChar

            INTEGER                   :: TempINT
            INTEGER                   :: IOS,IOS2
            INTEGER                   :: IMEDUNIT
            INTEGER                   :: I,J

            REAL(8)                   :: TempX,TempY
            REAL(8)                   :: TempREAL

            LOGICAL                   :: exists
            LOGICAL                   :: HASSEC

            exists = FindFile(Filename)
            IMEDUNIT=GETFREEUNIT()

            OPEN(FILE=TRIM(Filename),UNIT=IMEDUNIT,ACTION="READ")

            !...Read 3 header lines into structure
            READ(IMEDUNIT,'(A)') OutIMEDS%HEADER1
            READ(IMEDUNIT,'(A)') OutIMEDS%HEADER2
            READ(IMEDUNIT,'(A)') OutIMEDS%HEADER3

            !...Start counting things up
            OutIMEDS%NSTATIONS = 0
            DO  !...Outer infinite loop over stations
                READ(IMEDUNIT,*,IOSTAT=IOS) TempName,TempY,TempX
                IF(IOS.NE.0)THEN
                    BACKSPACE(IMEDUNIT)
                    EXIT
                ENDIF
                DO !...Inner infinite loop over data
                    READ(IMEDUNIT,*,IOSTAT=IOS) TempINT,TempINT,TempINT,TempINT,TempREAL
                    IF(IOS.NE.0)THEN
                        BACKSPACE(IMEDUNIT)
                        EXIT
                    ENDIF
                ENDDO
                OutIMEDS%NSTATIONS = OutIMEDS%NSTATIONS + 1
            ENDDO

            ALLOCATE(OutIMEDS%Station(1:OutIMEDS%NSTATIONS))

            HASSEC = .FALSE.

            !...Start the loop over, this time counting snaps
            REWIND(IMEDUNIT)
            READ(IMEDUNIT,'(A)') TempName
            READ(IMEDUNIT,'(A)') TempName
            READ(IMEDUNIT,'(A)') TempName
            DO I = 1,OutIMEDS%NSTATIONS
                READ(IMEDUNIT,*) OutIMEDS%STATION(I)%STATION_NAME,&
                    OutIMEDS%STATION(I)%LATITUDE,OutIMEDS%STATION(I)%LONGITUDE
                OutIMEDS%STATION(I)%NSNAPS = 0
                DO !...Inner infinite loop to count
                    READ(IMEDUNIT,'(A)',IOSTAT=IOS2) TEMPCHAR
                    IF(IOS2.NE.0)THEN
                        EXIT
                    ENDIF
                    READ(TEMPCHAR,*,IOSTAT=IOS) TempINT,TempINT,TempINT,TempINT,TempINT,TempINT,TempREAL
                    IF(IOS.NE.0)THEN
                        READ(TEMPCHAR,*,IOSTAT=IOS) TempINT,TempINT,TempINT,TempINT,TempINT,TempREAL
                        IF(IOS.NE.0)THEN
                            BACKSPACE(IMEDUNIT)
                            EXIT
                        ELSE
                            HASSEC=.FALSE.
                        ENDIF
                    ELSE
                        HASSEC=.TRUE.
                    ENDIF
                    OutIMEDS%STATION(I)%NSNAPS = OutIMEDS%STATION(I)%NSNAPS + 1
                ENDDO
                ALLOCATE(OutIMEDS%STATION(I)%YEAR(1:OutIMEDS%STATION(I)%NSNAPS))
                ALLOCATE(OutIMEDS%STATION(I)%MONTH(1:OutIMEDS%STATION(I)%NSNAPS))
                ALLOCATE(OutIMEDS%STATION(I)%DAY(1:OutIMEDS%STATION(I)%NSNAPS))
                ALLOCATE(OutIMEDS%STATION(I)%HOUR(1:OutIMEDS%STATION(I)%NSNAPS))
                ALLOCATE(OutIMEDS%STATION(I)%MINUTE(1:OutIMEDS%STATION(I)%NSNAPS))
                ALLOCATE(OutIMEDS%STATION(I)%SECOND(1:OutIMEDS%STATION(I)%NSNAPS))
                ALLOCATE(OutIMEDS%STATION(I)%VALUE(1:OutIMEDS%STATION(I)%NSNAPS))
            ENDDO

            !...Final pass for data
            REWIND(IMEDUNIT)
            READ(IMEDUNIT,'(A)') TempName
            READ(IMEDUNIT,'(A)') TempName
            READ(IMEDUNIT,'(A)') TempName
            DO I = 1,OutIMEDS%NSTATIONS
                READ(IMEDUNIT,*) TempName
                DO J = 1,OutIMEDS%STATION(I)%NSNAPS
                    IF(.NOT.HASSEC)THEN
                        READ(IMEDUNIT,*) OutIMEDS%STATION(I)%YEAR(J),&
                                         OutIMEDS%STATION(I)%MONTH(J),&
                                         OutIMEDS%STATION(I)%DAY(J),&
                                         OutIMEDS%STATION(I)%HOUR(J),&
                                         OutIMEDS%STATION(I)%MINUTE(J),&
                                         OutIMEDS%STATION(I)%VALUE(J)
                        OutIMEDS%Station(I)%SECOND(J) = 0
                    ELSE
                        READ(IMEDUNIT,*) OutIMEDS%STATION(I)%YEAR(J),&
                                         OutIMEDS%STATION(I)%MONTH(J),&
                                         OutIMEDS%STATION(I)%DAY(J),&
                                         OutIMEDS%STATION(I)%HOUR(J),&
                                         OutIMEDS%STATION(I)%MINUTE(J),&
                                         OutIMEDS%Station(I)%SECOND(J),&
                                         OutIMEDS%STATION(I)%VALUE(J)
                    ENDIF
                ENDDO
            ENDDO


            CLOSE(IMEDUNIT)

        END SUBROUTINE
        
        !>Subroutine to write an IMEDS data container to 
        !>the IMEDS file format
        !>\author Zach Cobell
        SUBROUTINE WRITE_IMEDS(Filename,IMEDSData)
            IMPLICIT NONE
    
            !>File to be written
            CHARACTER(*),INTENT(IN) :: Filename
            !>IMEDS data container to be written
            TYPE(IMEDS),INTENT(IN)  :: IMEDSData

            CHARACTER(200) :: StationName
            INTEGER        :: IMEDSUNIT
            INTEGER        :: I,J

            IMEDSUNIT=GetFreeUnit()
            OPEN(FILE=TRIM(Filename),UNIT=IMEDSUNIT,ACTION="WRITE")
            WRITE(IMEDSUNIT,'(A)') TRIM(IMEDSData%HEADER1)
            WRITE(IMEDSUNIT,'(A)') TRIM(IMEDSData%HEADER2)
            WRITE(IMEDSUNIT,'(A)') TRIM(IMEDSData%HEADER3)
            DO I = 1,IMEDSData%NSTATIONS
                !...Remove spaces from station names
                StationName = ""
                DO J = 1,LEN_TRIM(IMEDSData%Station(I)%Station_Name)
                    IF(IMEDSData%Station(I)%Station_Name(J:J).EQ." ")THEN
                        StationName(J:J) = "_"
                    ELSE
                        StationName(J:J) = IMEDSData%Station(I)%Station_Name(J:J)
                    ENDIF
                ENDDO
                WRITE(IMEDSUNIT,'(A,2X,F0.8,2X,F0.8)') &
                    TRIM(StationName),&
                    IMEDSData%Station(I)%Latitude,&
                    IMEDSData%Station(I)%Longitude
                DO J = 1,IMEDSData%Station(I)%NSNAPS
                    WRITE(IMEDSUNIT,201) IMEDSData%Station(I)%YEAR(J),&
                                         IMEDSData%Station(I)%MONTH(J),&
                                         IMEDSData%Station(I)%DAY(J),&
                                         IMEDSData%Station(I)%HOUR(J),&
                                         IMEDSData%Station(I)%MINUTE(J),&
                                         IMEDSData%Station(I)%SECOND(J),&
                                         IMEDSData%Station(I)%VALUE(J)
                ENDDO
            ENDDO
            CLOSE(IMEDSUNIT)

201         FORMAT(6I7,F18.8)

        END SUBROUTINE

!................................................................................
!
!                      Misc
!
!................................................................................

            !>Distance function that calculates either spherical distance
            !>using the Haversine formula (default) or optionally cartesian distance.
            !>When spherical distance is calculated, the radius of the earth is 
            !>estimated using the RADIUS_EARTH routine. Distances are returned
            !>in meters when calculating spherical distance or in native 
            !>units when using the cartesian option.
            !>\author Zach Cobell
            REAL(8) FUNCTION distance(lon1,lat1,lon2,lat2,CARTESIAN)
                IMPLICIT NONE

                INTRINSIC  :: DATAN
                INTRINSIC  :: DATAN2
                INTRINSIC  :: DSIN
                INTRINSIC  :: DCOS
                INTRINSIC  :: DSQRT

                !>Option to use calculate cartesian distance instead of spherical
                LOGICAL,INTENT(IN),OPTIONAL ::  CARTESIAN !...Toggle between spherical or cartesian coordinates
                !>Y value 1 for calculation
                REAL(8),INTENT(IN)          ::  lat1
                !>Y value 2 for calculation
                REAL(8),INTENT(IN)          ::  lat2
                !>X value 1 for calculation
                REAL(8),INTENT(IN)          ::  lon1
                !>X value 2 for calculation
                REAL(8),INTENT(IN)          ::  lon2

                REAL(8),PARAMETER           ::  R = 6371.64d0
                REAL(8)                     ::  a
                REAL(8)                     ::  c
                REAL(8)                     ::  dlon
                REAL(8)                     ::  dlat
                REAL(8)                     ::  rad2deg
                INTEGER                     ::  k


                IF(PRESENT(CARTESIAN))THEN
                    IF(CARTESIAN)THEN
                        DISTANCE = DSQRT( (lon2-lon1)**2D0 + (lat2-lat1)**2D0 )
                    ELSE
#if 0
                        rad2deg = 45.d0/DATAN(1.d0)
                        dlon = (lon2-lon1)/rad2deg
                        dlat = (lat2-lat1)/rad2deg
                        a = DSIN(dlat/2.d0)*DSIN(dlat/2.d0)
                        a = a+DCOS(lat1/rad2deg)*DCOS(lat2/rad2deg)*DSIN(dlon/2.d0)**2D0
                        c = 2.0*DATAN2(DSQRT(a),DSQRT(1.d0-a))
                        distance = R*c
#else
                        distance = HAVERSINE(lon1,lat1,lon2,lat2)
#endif
                    ENDIF
                ELSE
#if 0
                    rad2deg = 45.d0/DATAN(1.d0)
                    dlon = (lon2-lon1)/rad2deg
                    dlat = (lat2-lat1)/rad2deg
                    a = DSIN(dlat/2.d0)*DSIN(dlat/2.d0)
                    a = a+DCOS(lat1/rad2deg)*DCOS(lat2/rad2deg)*DSIN(dlon/2.d0)**2D0
                    c = 2.0*DATAN2(DSQRT(a),DSQRT(1.d0-a))
                    distance = R*c
#else
                    distance = HAVERSINE(lon1,lat1,lon2,lat2)
#endif
                ENDIF

                RETURN

            END FUNCTION

            !>Function to calculate the azimuth between two points on 
            !>a sphere.
            !>\author Zach Cobell
            REAL(8) FUNCTION Azimuth(Lon1d,Lat1d,Lon2d,Lat2d,Convention)
                IMPLICIT NONE

                INTRINSIC   :: DSIN
                INTRINSIC   :: DCOS
                INTRINSIC   :: DATAN2
                INTRINSIC   :: DSQRT

                !>Option to select either NAUTICAL convention or 
                !>cartesian convention (default). Cartesian selected
                !>if this parameter is not included.
                CHARACTER(*),INTENT(IN),OPTIONAL :: Convention
                !>Y value 1 for calculation in decimal degrees
                REAL(8),INTENT(IN) ::  Lat1d
                !>Y value 2 for calculation in decimal degrees
                REAL(8),INTENT(IN) ::  Lat2d
                !>X value 1 for calculation in decimal degrees
                REAL(8),INTENT(IN) ::  Lon1d
                !>X value 2 for calculation in decimal degrees
                REAL(8),INTENT(IN) ::  Lon2d
                
                REAL(8)     ::  pi
                REAL(8)     ::  rad
                REAL(8)     ::  Lat1
                REAL(8)     ::  Lat2
                REAL(8)     ::  Lon1
                REAL(8)     ::  Lon2
                REAL(8)     ::  meanlat
                REAL(8)     ::  latdif
                REAL(8)     ::  londif
                REAL(8)     ::  a1
                REAL(8)     ::  b1
                REAL(8)     ::  e
                REAL(8)     ::  mrcurt
                REAL(8)     ::  prcurt
                REAL(8)     ::  A
                REAL(8)     ::  B
                REAL(8)     ::  Az

                pi = 4D0*DATAN2(1d0,1d0)
                rad = 180.0d0 / pi

                Lat1 = Lat1d / rad
                Lat2 = Lat2d / rad
                Lon1 = Lon1d / rad
                Lon2 = Lon2d / rad

                latdif = lat1 - lat2
                londif = lon1 - lon2
                meanlat = (lat1 + lat2) / 2d0

                a1 = 6377276.3450d0
                b1 = 6356075.4131d0
                e = DSQRT(((a1**2d0)-(b1**2d0))/(a1**2d0))
                mrcurt = (a1*(1d0-e**2d0)) / &
                    ( 1d0 - (e**2d0)*(DSIN(meanlat))**2d0)**(3d0/2d0)

                prcurt = a1 / DSQRT(1d0-(e*DSIN(meanlat))**2d0)
                A = 2d0*DATAN2(londif*((prcurt/mrcurt)*(DCOS(meanlat))),latdif)
                B = londif*(DSIN(meanlat))

                Az = (A-B)/2.0d0

                IF(PRESENT(Convention))THEN
                    IF(TRIM(Convention).EQ.'NAUTICAL')THEN
                        IF((londif.GT.0d0).AND.(latdif.LT.0d0))THEN
                            Az = Az + pi
                        ELSEIF((londif.LT.0d0).AND.(LatDif.LT.0d0))THEN
                            Az = Az + pi
                        ELSEIF((londif.LT.0d0).AND.(LatDif.GT.0d0))THEN
                            Az = Az + 2d0*pi
                        ENDIF
                        Azimuth = Az*rad
                    ELSE
                         Azimuth = Az*rad + 180.0D0
                    ENDIF
                ELSE
                    IF((londif.GT.0d0).AND.(latdif.LT.0d0))THEN
                        Az = Az + pi
                    ELSEIF((londif.LT.0d0).AND.(LatDif.LT.0d0))THEN
                        Az = Az + pi
                    ELSEIF((londif.LT.0d0).AND.(LatDif.GT.0d0))THEN
                        Az = Az + 2d0*pi
                    ENDIF
                    Azimuth = Az*rad
                ENDIF

                RETURN

            END FUNCTION

            !>Function to approximate the radius of the earth using the
            !>polar and equitorial radii. 
            !>\author Zach Cobell
            REAL(8) FUNCTION RADIUS_EARTH(LATITUDE)
                IMPLICIT NONE

                REAL(8),PARAMETER  :: ER = 6378137.0D0
                REAL(8),PARAMETER  :: PR = 6356752.3D0
                
                !>Latitude to approxaimte the Earth's radius
                REAL(8),INTENT(IN) :: LATITUDE

                REAL(8)            :: LAT
                REAL(8)            :: DEG2RAD
                REAL(8)            :: PI

                PI      = 4D0*ATAN2(1D0,1D0)
                DEG2RAD = PI / 180D0
                LAT     = LATITUDE * DEG2RAD

                RADIUS_EARTH = SQRT(                     &
                    (ER**4D0*COS(LAT)*COS(LAT) +         &
                     PR**4D0*SIN(LAT)*SIN(LAT))          &
                    /                                    &
                    (ER**2D0*COS(LAT)*COS(LAT) +         &
                     PR**2D0*SIN(LAT)*SIN(LAT) ) )

                RETURN

            END FUNCTION

            !>Function to use the haversine formula, which calculates
            !>the distance on a sphere. This routine specifically
            !>calculates radii on the Earth. The radius of the earth
            !>is approximated using the RADIUS_EARTH routine
            !>\author Zach Cobell
            REAL(8) FUNCTION HAVERSINE(LON1d,LAT1d,LON2d,LAT2d)
                IMPLICIT NONE
                !>X value 1 for calculation in decimal degrees
                REAL(8),INTENT(IN) :: LON1d
                !>Y value 1 for calculation in decimal degrees
                REAL(8),INTENT(IN) :: LAT1d
                !>X value 2 for calculation in decimal degrees
                REAL(8),INTENT(IN) :: LON2d
                !>Y value 2 for calculation in decimal degrees
                REAL(8),INTENT(IN) :: LAT2d
                REAL(8)            :: LON1
                REAL(8)            :: LAT1
                REAL(8)            :: LON2
                REAL(8)            :: LAT2
                REAL(8)            :: RADIUS
                REAL(8)            :: PI
                REAL(8)            :: DEG2RAD

                PI       = 4D0*ATAN2(1D0,1D0)
                DEG2RAD  = PI / 180D0
                RADIUS   = RADIUS_EARTH((LAT1d+LAT2d)/2D0)
                LAT1     = LAT1d*DEG2RAD
                LAT2     = LAT2d*DEG2RAD
                LON1     = LON1d*DEG2RAD
                LON2     = LON2d*DEG2RAD

                HAVERSINE = 2D0*RADIUS*ASIN(SQRT(                      &
                    SIN( (LAT2-LAT1)/2D0 )**2D0 +                      &
                    COS(LAT1)*COS(LAT2)*SIN( (LON2-LON1)/2D0)**2D0 ) )

                RETURN

            END FUNCTION

            !>This function will return the index of a particular attribute name
            !>in a fort.13 file, allowing the user to quickly search for
            !>a specific index.
            !>\author Zach Cobell
            INTEGER FUNCTION FindAttributeIndex(attributename,My13)
                IMPLICIT NONE
                !>Name of attribute to be searched for
                CHARACTER(*),INTENT(IN)   :: attributename
                !>fort.13 data container to locate attribute
                TYPE(fort13),INTENT(IN)   :: My13
                INTEGER        :: I

                FindAttributeIndex = -1
                DO I = 1,My13%NumAttributes
                    IF(TRIM(ADJUSTL(My13%nodal_param(I)%Attribute)).EQ.&
                        TRIM(ADJUSTL(attributename)))THEN
                            FindAttributeIndex = I
                            EXIT
                    ENDIF
                ENDDO

                RETURN

            END FUNCTION

            !>Subroutine to display a progress bar
            !>\author Zach Cobell
            SUBROUTINE ShowBar(ntotal, now, ncount)
                IMPLICIT NONE
                !>Iteration that will be indexed to 100% completion
                INTEGER,INTENT(IN)   :: ntotal
                !>Current iteration
                INTEGER,INTENT(IN)   :: now
                !>A variable passed back and forth between this routine.
                !>The user should initially set it to zero and then
                !>leave it alone. This subroutine will manipulate it as needed.
                INTEGER,INTENT(OUT)  :: ncount

                INTEGER              :: nout
                INTEGER              :: n
                IF(now.EQ.1)THEN
                    WRITE(*,'(A)') "0%                      50%                      100%"
                    WRITE(*,'(A,$)') "|"
                ENDIF
                nout = INT(DBLE(ntotal/50))
                n = MOD(ntotal,50)
                IF (n.NE.0) nout = nout + 1
                IF(now.GE.ncount*nout)THEN
                    ncount = ncount + 1
                    SELECT CASE(ncount)
                        CASE(10, 20, 30, 40)
                            WRITE(6,'(A,$)') '+'
                        CASE(50)
                            WRITE(6,'(A)') "|"
                        CASE DEFAULT
                            WRITE(6,'(A,$)') '-'
                    END SELECT
                ENDIF

                RETURN

            END SUBROUTINE

            !>Subroutine that will evoke a function to quit
            !>a program. It will pause, asking the user to press
            !>enter to exit before stopping execution.
            !>\author Zach Cobell
            SUBROUTINE Quit
                WRITE(*,'(A)') ""
                WRITE(*,'(A)') "<--Press enter to exit-->"
                READ(*,*)
                STOP
            END SUBROUTINE

            !>This function will transform a single point to UTM coordinates
            !>from Geographic coordinates.
            !>\author Zach Cobell
            !>\author Seizo Tanaka
            SUBROUTINE LatLon2UTM(MyX,MyY,MyZone,MyHorizontal,UTM_X,UTM_Y)

                IMPLICIT NONE

                INTRINSIC :: DACOS

                !..IN/OUT Variables
                !>Horizontal system to be used for input and output
                INTEGER,INTENT(IN)  :: MyHorizontal
                !>UTM Zone
                INTEGER,INTENT(IN)  :: MyZone
                !>X coordinate in decimal degrees
                REAL(8),INTENT(IN)  :: MyX
                !>Y coordinate in decimal degrees
                REAL(8),INTENT(IN)  :: MyY
                !>Output X coordinate in UTM coordinates
                REAL(8),INTENT(OUT) :: UTM_X
                !>Output Y coordinate in UTM coordinates
                REAL(8),INTENT(OUT) :: UTM_Y

                !...Local Variables
                REAL(8) :: DLat
                REAL(8) :: DLon
                REAL(8) :: RLat
                REAL(8) :: RLon
                REAL(8) :: X
                REAL(8) :: Y
                REAL(8),PARAMETER :: UTMScaleFactor = 0.9996d0
                REAL(8) :: A
                REAL(8) :: B
                REAL(8) :: PI
                REAL(8) :: DN
                REAL(8) :: SALPHA
                REAL(8) :: SBETA
                REAL(8) :: SGAMMA
                REAL(8) :: SDELTA
                REAL(8) :: SEPSILON
                REAL(8) :: SLENGTH
                REAL(8) :: CMERIDIAN
                REAL(8) :: SEP2
                REAL(8) :: SNU2
                REAL(8) :: SN
                REAL(8) :: T
                REAL(8) :: T2
                REAL(8) :: TMP
                REAL(8) :: S1
                REAL(8) :: SL
                REAL(8) :: SL3COEF
                REAL(8) :: SL4COEF
                REAL(8) :: SL5COEF
                REAL(8) :: SL6COEF
                REAL(8) :: SL7COEF
                REAL(8) :: SL8COEF
                INTEGER :: I

                SELECT CASE(MyHorizontal)
                    CASE(1)
                       A = 6378137.d0      ! Equatorial Radius
                       B = 6356752.3141d0  ! Polar Radius
                    CASE(2)
                       A = 6378137.d0      ! Equatorial Radius
                       B = 6356752.3142d0  ! Polar Radius
                    CASE(3)
                       A = 6378135.d0      ! Equatorial Radius
                       B = 6356750.5000d0  ! Polar Radius
                   CASE DEFAULT
                       WRITE(*,'(A)') "Invalid Horizontal System."
                       STOP
                END SELECT

                PI = DACOS(-1.0D0)

                DLON = MyX
                DLAT = MyY
                RLAT = DLAT * PI / 180.0D0
                RLON = DLON * PI / 180.0D0
                DN = (A-B) / (A+B)
                SALPHA = ((A+B)/2.D0) * ( 1.D0 + DN**(2)/4.D0 + DN**(4)/ 64.D0 )
                SBETA = ( -3.D0*DN/2.D0 ) + ( 9.D0*DN**(3)/16.D0 ) + (-3.D0*DN**(5)/32.D0)
                SGAMMA = ( 15.D0*DN**2)/16.D0 - (15.D0*DN**(4))/32.D0
                SDELTA = (-35.D0*DN**(3))/48.D0 + (105.D0*DN**(5))/256.D0
                SEPSILON = 315.D0 * DN ** (4) / 512.D0
                SLENGTH = SALPHA * ( RLAT + SBETA  * DSIN(2.D0*RLAT) + SGAMMA * DSIN(4.D0*RLAT) &
                                   + SDELTA * DSIN(6.D0*RLAT) + SEPSILON * DSIN(8.D0*RLAT) )
                CMERIDIAN = ( -183.D0 + MyZone*6.D0 ) * PI / 180.D0

                SEP2 = (A**(2) - B **(2)) / (B**(2))
                SNU2 = SEP2 * (DCOS(RLAT) ** (2))
                SN   = A*A / ( B*DSQRT(1.D0+SNU2) )
                T    = DTAN(RLAT)
                T2   = T * T
                TMP  = ( T2*T2*T2 ) - T**(6)
                SL   = RLON - CMERIDIAN
                SL3COEF = 1.D0 - T2 + SNU2
                SL4COEF = 5.D0 - T2 + 9.D0*SNU2 + 4.D0*SNU2*SNU2
                SL5COEF = 5.D0 - 18.D0*T2 + T2*T2 + 14.D0*SNU2 - 58.D0*  T2*SNU2
                SL6COEF = 61.D0 - 58.D0*T2 + T2*T2 + 270.D0*SNU2 - 330.D0*  T2*SNU2
                SL7COEF = 61.D0 - 479.D0*T2 + 179.D0*T2*T2 - T2*T2*T2
                SL8COEF = 1385.D0 - 3311.D0*T2 + 543.D0*T2*T2 - T2*T2*T2
                X = SN * DCOS(RLAT)                * SL                &
                    + SN * DCOS(RLAT)**(3) * SL3COEF * SL**(3) /    6.D0 &
                    + SN * DCOS(RLAT)**(5) * SL5COEF * SL**(5) /  120.D0 &
                    + SN * DCOS(RLAT)**(7) * SL7COEF * SL**(7) / 5040.D0
                Y = SLENGTH &
                    + T * SN * DCOS(RLAT)**(2)           * SL**(2) /     2.D0 &
                    + T * SN * DCOS(RLAT)**(4) * SL4COEF * SL**(4) /    24.D0 &
                    + T * SN * DCOS(RLAT)**(6) * SL6COEF * SL**(6) /   720.D0 &
                    + T * SN * DCOS(RLAT)**(8) * SL8COEF * SL**(8) / 40320.D0

                x = x * UTMScaleFactor + 500000.d0
                y = y * UTMScaleFactor

                IF(Y.LT.0.0D0)THEN
                    Y = Y + 10000000.D0
                ENDIF

                UTM_X = X
                UTM_Y = Y

                RETURN

            END SUBROUTINE

            !>This function will transform an ADCIRC grid to UTM coordinates
            !>from Geographic coordinates.
            !>\author Zach Cobell
            !>\author Seizo Tanaka
            SUBROUTINE LatLon2UTMGrid(MyMesh,MyZone,MyHorizontal,MyUTMMesh)

                IMPLICIT NONE

                INTRINSIC :: DACOS

                !..IN/OUT Variables
                !>ADCIRC grid in geographic coordiantes
                TYPE(grid),INTENT(IN) :: MyMesh
                !>Output ADCIRC grid in UTM coordinates
                TYPE(grid),INTENT(OUT) :: MyUTMMesh
                !>Horizontal system to use
                INTEGER,INTENT(IN) :: MyHorizontal
                !>UTM Zone to transform into
                INTEGER,INTENT(IN) :: MyZone

                !...Local Variables
                REAL(8) :: DLat
                REAL(8) :: DLon
                REAL(8) :: RLat
                REAL(8) :: RLon
                REAL(8) :: X
                REAL(8) :: Y
                REAL(8),PARAMETER :: UTMScaleFactor = 0.9996d0
                REAL(8) :: A
                REAL(8) :: B
                REAL(8) :: PI
                REAL(8) :: DN
                REAL(8) :: SALPHA
                REAL(8) :: SBETA
                REAL(8) :: SGAMMA
                REAL(8) :: SDELTA
                REAL(8) :: SEPSILON
                REAL(8) :: SLENGTH
                REAL(8) :: CMERIDIAN
                REAL(8) :: SEP2
                REAL(8) :: SNU2
                REAL(8) :: SN
                REAL(8) :: T
                REAL(8) :: T2
                REAL(8) :: TMP
                REAL(8) :: S1
                REAL(8) :: SL
                REAL(8) :: SL3COEF
                REAL(8) :: SL4COEF
                REAL(8) :: SL5COEF
                REAL(8) :: SL6COEF
                REAL(8) :: SL7COEF
                REAL(8) :: SL8COEF
                INTEGER :: I

                SELECT CASE(MyHorizontal)
                    CASE(1)
                       A = 6378137.d0      ! Equatorial Radius
                       B = 6356752.3141d0  ! Polar Radius
                    CASE(2)
                       A = 6378137.d0      ! Equatorial Radius
                       B = 6356752.3142d0  ! Polar Radius
                    CASE(3)
                       A = 6378135.d0      ! Equatorial Radius
                       B = 6356750.5000d0  ! Polar Radius
                   CASE DEFAULT
                       WRITE(*,'(A)') "Invalid Horizontal System."
                       STOP
                END SELECT

                PI = DACOS(-1.0D0)
                MyUTMMesh = MyMesh !...Copy structure of mesh to UTM mesh

                DO I = 1,MyUTMMesh%NumNodes
                    DLON = MYMESH%NODES(I,1)
                    DLAT = MYMESH%NODES(I,2)
                    RLAT = DLAT * PI / 180.0D0
                    RLON = DLON * PI / 180.0D0
                    DN = (A-B) / (A+B)
                    SALPHA = ((A+B)/2.D0) * ( 1.D0 + DN**(2)/4.D0 + DN**(4)/ 64.D0 )
                    SBETA = ( -3.D0*DN/2.D0 ) + ( 9.D0*DN**(3)/16.D0 ) + (-3.D0*DN**(5)/32.D0)
                    SGAMMA = ( 15.D0*DN**2)/16.D0 - (15.D0*DN**(4))/32.D0
                    SDELTA = (-35.D0*DN**(3))/48.D0 + (105.D0*DN**(5))/256.D0
                    SEPSILON = 315.D0 * DN ** (4) / 512.D0
                    SLENGTH = SALPHA * ( RLAT + SBETA  * DSIN(2.D0*RLAT) + SGAMMA * DSIN(4.D0*RLAT) &
                                       + SDELTA * DSIN(6.D0*RLAT) + SEPSILON * DSIN(8.D0*RLAT) )
                    CMERIDIAN = ( -183.D0 + MyZone*6.D0 ) * PI / 180.D0

                    SEP2 = (A**(2) - B **(2)) / (B**(2))
                    SNU2 = SEP2 * (DCOS(RLAT) ** (2))
                    SN   = A*A / ( B*DSQRT(1.D0+SNU2) )
                    T    = DTAN(RLAT)
                    T2   = T * T
                    TMP  = ( T2*T2*T2 ) - T**(6)
                    SL   = RLON - CMERIDIAN
                    SL3COEF = 1.D0 - T2 + SNU2
                    SL4COEF = 5.D0 - T2 + 9.D0*SNU2 + 4.D0*SNU2*SNU2
                    SL5COEF = 5.D0 - 18.D0*T2 + T2*T2 + 14.D0*SNU2 - 58.D0*  T2*SNU2
                    SL6COEF = 61.D0 - 58.D0*T2 + T2*T2 + 270.D0*SNU2 - 330.D0*  T2*SNU2
                    SL7COEF = 61.D0 - 479.D0*T2 + 179.D0*T2*T2 - T2*T2*T2
                    SL8COEF = 1385.D0 - 3311.D0*T2 + 543.D0*T2*T2 - T2*T2*T2
                    X = SN * DCOS(RLAT)                * SL                &
                        + SN * DCOS(RLAT)**(3) * SL3COEF * SL**(3) / 6.D0 &
                        + SN * DCOS(RLAT)**(5) * SL5COEF * SL**(5) / 120.D0 &
                        + SN * DCOS(RLAT)**(7) * SL7COEF * SL**(7) / 5040.D0
                    Y = SLENGTH &
                        + T * SN * DCOS(RLAT)**(2) * SL**(2) / 2.D0 &
                        + T * SN * DCOS(RLAT)**(4) * SL4COEF * SL**(4) /    24.D0 &
                        + T * SN * DCOS(RLAT)**(6) * SL6COEF * SL**(6) /   720.D0 &
                        + T * SN * DCOS(RLAT)**(8) * SL8COEF * SL**(8) / 40320.D0

                    x = x * UTMScaleFactor + 500000.d0
                    y = y * UTMScaleFactor

                    IF(Y.LT.0.0D0)THEN
                        Y = Y + 10000000.D0
                    ENDIF

                    MyUTMMesh%nodes(I,1) = X
                    MyUTMMesh%nodes(I,2) = Y

                ENDDO

                RETURN

            END SUBROUTINE

            !>Heapsort algorithm from Numerical Recipies in Fortran for
            !>REAL(8) variables.
            !>\author Zach Cobell
            SUBROUTINE HEAPSORT(N,RA)
                IMPLICIT NONE
                !>Number of items to sort
                INTEGER,INTENT(IN)    :: N
                !>The array that will be sorted in place
                REAL(8),INTENT(INOUT) :: RA(N)

                INTEGER :: L,IR,J,I
                REAL(8) :: RRA

                L = (N / 2) + 1
                IR = N
10              CONTINUE
                IF(L.GT.1)THEN
                    L = L - 1
                    RRA = RA(L)
                ELSE
                    RRA = RA(IR)
                    RA(IR) = RA(1)
                    IR = IR - 1
                    IF(IR.EQ.1)THEN
                        RA(1) = RRA
                        RETURN
                    ENDIF
                ENDIF
                I = L
                J = L + L
20              IF(J.LE.IR)THEN
                    IF(J.LT.IR)THEN
                        IF(RA(J).LT.RA(J+1))THEN
                            J = J + 1
                        ENDIF
                    ENDIF
                    IF(RRA < RA(J))THEN
                        RA(I) = RA(J)
                        I = J
                        J = J + J
                    ELSE
                        J = IR + J
                    ENDIF
                    GOTO 20
                ENDIF
                RA(I) = RRA
                GOTO 10
            END SUBROUTINE

            !>Heapsort algorithm from Numerical Recipies in Fortran for
            !>INTEGER variables.
            !>\author Zach Cobell
            SUBROUTINE HEAPSORTINT(N,RA)
                IMPLICIT NONE
                !>Number of items to sort
                INTEGER,INTENT(IN) :: N
                !>The array that will be sorted in place
                INTEGER,INTENT(INOUT) :: RA(N)

                INTEGER :: L,IR,J,I
                INTEGER :: RRA

                L = (N / 2) + 1
                IR = N
10              CONTINUE
                IF(L.GT.1)THEN
                    L = L - 1
                    RRA = RA(L)
                ELSE
                    RRA = RA(IR)
                    RA(IR) = RA(1)
                    IR = IR - 1
                    IF(IR.EQ.1)THEN
                        RA(1) = RRA
                        RETURN
                    ENDIF
                ENDIF
                I = L
                J = L + L
20              IF(J.LE.IR)THEN
                    IF(J.LT.IR)THEN
                        IF(RA(J).LT.RA(J+1))THEN
                            J = J + 1
                        ENDIF
                    ENDIF
                    IF(RRA < RA(J))THEN
                        RA(I) = RA(J)
                        I = J
                        J = J + J
                    ELSE
                        J = IR + J
                    ENDIF
                    GOTO 20
                ENDIF
                RA(I) = RRA
                GOTO 10
            END SUBROUTINE

            SUBROUTINE Unique(A,NumNodes,N)
                IMPLICIT NONE
                REAL(8) :: PREV_VAL
                INTEGER :: N
                INTEGER :: I
                INTEGER :: NumNodes
                REAL(8) :: Tolorance
                REAL(8),DIMENSION(NumNodes) :: A
                PREV_VAL = A(1)
                N = 1
                Tolorance = EPSILON(1.0D0)
                DO I = 2,NumNodes
                    IF(DABS(A(I)-PREV_VAL).LE.Tolorance)CYCLE
                    PREV_VAL = A(I)
                    N = N + 1
                ENDDO
                RETURN
            END SUBROUTINE

            SUBROUTINE Frequency(A,N,U,V)
                IMPLICIT NONE
                REAL(8) :: Prev_Val
                REAL(8) :: V
                INTEGER :: N
                INTEGER :: U
                INTEGER :: I
                INTEGER :: idx
                REAL(8),ALLOCATABLE :: counter(:,:)
                REAL(8),DIMENSION(N) :: A
                REAL(8) :: M
                REAL(8) :: Tolerance

                Tolerance = EPSILON(1.0D0)

                IF(U.GT.1)THEN
                    IF(ALLOCATED(Counter))DEALLOCATE(Counter)
                    ALLOCATE(Counter(1:U,2))
                    Counter = -1
                    idx = 1
                    Prev_Val = A(1)
                    DO I = 2,N
                        IF(DABS(A(I)-Prev_VAL).LE.Tolerance)THEN
                            IF(counter(idx,1).EQ.-1d0)THEN
                                counter(idx,1) = Prev_VAL
                                counter(idx,2) = 1d0
                            ENDIF
                            counter(idx,2) = counter(idx,2) + 1d0
                        ELSE
                            Prev_VAL = A(I)
                            idx = idx + 1
                        ENDIF
                    ENDDO

                    M = -1
                    DO I = 1,U
                        IF(counter(I,2).GT.M)THEN
                            M = counter(I,2)
                            V = counter(I,1)
                        ENDIF
                    ENDDO
                ELSE
                    V = A(1)
                ENDIF

                RETURN

            END SUBROUTINE

            SUBROUTINE ComputeElementArea(MyMesh,Area)

                IMPLICIT NONE

                TYPE(grid),INTENT(IN)             :: MyMesh
                REAL(8),ALLOCATABLE,INTENT(INOUT) :: Area(:)
                REAL(8) :: X1,X2,X3
                REAL(8) :: Y1,Y2,Y3
                REAL(8) :: S,S1,S2,S3
                REAL(8) :: A
                INTEGER :: I

                IF(.NOT.ALLOCATED(Area))ALLOCATE(Area(1:MyMesh%NumElements))
                DO I = 1,MyMesh%NumElements
                    X1 = MyMesh%nodes(MyMesh%Conn(I,1),1)
                    X2 = MyMesh%nodes(MyMesh%Conn(I,2),1)
                    X3 = MyMesh%nodes(MyMesh%Conn(I,3),1)
                    Y1 = MyMesh%nodes(MyMesh%Conn(I,1),2)
                    Y2 = MyMesh%nodes(MyMesh%Conn(I,2),2)
                    Y3 = MyMesh%nodes(MyMesh%Conn(I,3),2)
                    S1 = DISTANCE(X1,Y1,X2,Y2) !*1000D0 !Side1 (m)
                    S2 = DISTANCE(X2,Y2,X3,Y3) !*1000D0 !Side2 (m)
                    S3 = DISTANCE(X3,Y3,X1,Y1) !*1000D0 !Side3 (m)
                    S  = (S1+S2+S3)/2D0               !Perimiter/2 (m)
                    A  = SQRT(S*(S-S1)*(S-S2)*(S-S3)) !Area (m2)
                    Area(I) = A
                ENDDO

                RETURN

            END SUBROUTINE

            SUBROUTINE ComputeGridScale(MyMesh,GridScale,Cartesian)
                IMPLICIT NONE
                TYPE(GRID),INTENT(IN) :: MyMesh
                LOGICAL,INTENT(IN),OPTIONAL :: Cartesian
                REAL(8),ALLOCATABLE,INTENT(OUT) :: GridScale(:)
                LOGICAL :: C

                INTEGER :: I,J,J1,J2
                REAL(8) :: D

                IF(PRESENT(Cartesian))THEN
                    C = CARTESIAN
                ELSE
                    C = .FALSE.
                ENDIF

                ALLOCATE(GridScale(1:MyMesh%NumNodes))
                DO I = 1,MyMesh%NumElements
                    DO J = 1,3
                        J1 = J
                        J2 = J + 1
                        IF(J2.GT.3)THEN
                            J2 = 1
                        ENDIF
                        D = Distance( MyMesh%nodes(MyMesh%conn(I,J1),1),  &
                                      MyMesh%nodes(MyMesh%conn(I,J1),2),  &
                                      MyMesh%nodes(MyMesh%conn(I,J2),1),  &
                                      MyMesh%nodes(MyMesh%conn(I,J2),2),C )
                        IF(D.GT.gridscale(MyMesh%conn(I,J1)))THEN
                            gridscale(MyMesh%conn(I,J1)) = D
                        ENDIF
                        IF(D.GT.gridscale(MyMesh%conn(I,J2)))THEN
                            gridscale(MyMesh%conn(I,J2)) = D
                        ENDIF
                    ENDDO
                ENDDO

            END SUBROUTINE


            SUBROUTINE BuildElementTable(MyMesh,MyElementTable)
                IMPLICIT NONE
                TYPE(grid),INTENT(IN)            :: MyMesh
                TYPE(ElementTable),INTENT(INOUT) :: MyElementTable
                INTEGER :: I,J,N1

                !...Allocation of outer arrays
                ALLOCATE(MyElementTable%Node(1:MyMesh%NumNodes))
                MyElementTable%Node(:)%NumElementsAroundMe = 0

                !...Run the routine in three parts, first find out how many elements
                !   are around a specific node for allocation purposes. This slower
                !   scheme will avoid allocating unused memory. It is possible to
                !   allocate the number of elements to a "maximum" value and leave
                !   the rest of the slots unused when the max is not reached, however,
                !   I've chosen this scheme for better use with the very large meshes
                DO I = 1,MyMesh%NumElements
                    DO J = 1,3
                        N1 = MyMesh%Conn(I,J)
                        MyElementTable%Node(N1)%NumElementsAroundMe = &
                            MyElementTable%Node(N1)%NumElementsAroundMe + 1
                    ENDDO
                ENDDO

                !...Now, dynamically allocate the arrays
                DO I = 1,MyMesh%NumNodes
                    ALLOCATE(MyElementTable%Node(I)%ElementsAroundMe(&
                        1:MyElementTable%Node(I)%NumElementsAroundMe))
                ENDDO


                !...Finally, list the elements around the node
                !   Set counter array back to zero and build it back up
                MyElementTable%Node(:)%NumElementsAroundMe = 0
                DO I = 1,MyMesh%NumElements
                    DO J = 1,3
                        N1 = MyMesh%Conn(I,J)
                        MyElementTable%Node(N1)%NumElementsAroundMe = &
                            MyElementTable%Node(N1)%NumElementsAroundMe + 1
                        MyElementTable%Node(N1)%ElementsAroundMe(&
                            MyElementTable%Node(N1)%NumElementsAroundMe) = I
                    ENDDO
                ENDDO

            END SUBROUTINE

            SUBROUTINE BuildConnectivityTable(MyMesh,Table)
                IMPLICIT NONE
                TYPE(GRID),INTENT(IN)       :: MyMesh
                TYPE(NODETABLE),INTENT(OUT) :: Table
                TYPE(ElementTable)          :: EleTable
                INTEGER                     :: I,J,K,N,IDX,IG
                INTEGER                     :: nnan_max = 20
                REAL(8)                     :: X1,X2,Y1,Y2
                INTEGER,ALLOCATABLE         :: list(:),list2(:)
                LOGICAL,ALLOCATABLE         :: ignore(:)

                CALL BuildElementTable(mymesh,EleTable)
                ALLOCATE(list(1:nnan_max))
                ALLOCATE(ignore(1:nnan_max))
                ALLOCATE(table%node(1:mymesh%numnodes))

                DO I = 1,mymesh%NumNodes

                    !...Initialize variables
                    list(:) = -1
                    ignore(:) = .FALSE.
                    n = 0
                    ig = 0

                    !...Find all the nodes
                    DO J = 1,EleTable%node(I)%NumElementsAroundMe
                        DO K = 1,3
                            IF(mymesh%conn(EleTable%node(I)%ElementsAroundMe(J),K).NE.I)THEN
                                n = n + 1
                                list(n) = mymesh%conn(EleTable%node(I)%ElementsAroundMe(J),K)
                            ENDIF
                        ENDDO
                    ENDDO

                    !...Sort
                    CALL HEAPSORTINT(nnan_max,list)
                    DO J = 1,nnan_max
                        IF(J.EQ.1.AND.list(J).EQ.-1)THEN
                            ignore(J) = .TRUE.
                        ELSEIF(list(J).EQ.-1.OR.list(J).EQ.list(J-1))THEN
                            ignore(J) = .TRUE.
                            ig = ig + 1
                        ENDIF
                    ENDDO

                    !...Remove duplicates
                    n = nnan_max - ig - 1
                    idx = 0
                    ALLOCATE(list2(1:n))
                    DO J = 1,nnan_max
                        IF(ignore(J))CYCLE
                        idx = idx + 1
                        list2(idx) = list(J)
                    ENDDO

                    !...Save the new list
                    Table%node(I)%NumNodesAroundMe = n
                    ALLOCATE(Table%node(I)%NodesAroundMe(1:n))
                    ALLOCATE(Table%node(I)%distance(1:n))
                    Table%node(I)%NodesAroundMe(:) = list2(:)

                    !...Prep list2 for next node
                    DEALLOCATE(list2)
                ENDDO

                !...Compute the linear distance between the
                !   center node and neighbors
                DO I = 1,mymesh%NumNodes
                    X1 = mymesh%nodes(I,1)
                    Y1 = mymesh%nodes(I,2)
                    DO J = 1,table%node(I)%NumNodesAroundMe
                        X2 = mymesh%nodes(table%node(I)%NodesAroundMe(J),1)
                        Y2 = mymesh%nodes(table%node(I)%NodesAroundMe(J),2)
                        Table%node(I)%distance(J) = haversine(X1,Y1,X2,Y2)
                    ENDDO
                ENDDO

                RETURN

            END SUBROUTINE


            LOGICAL FUNCTION FindFile(filename,returnfalse)
                CHARACTER(*),INTENT(IN) :: Filename
                LOGICAL,INTENT(IN),OPTIONAL :: returnfalse
                LOGICAL                 :: exists

                INQUIRE(FILE=TRIM(filename),EXIST=exists)

                FindFile=.TRUE.
                IF(.NOT.exists)THEN
                    IF(PRESENT(RETURNFALSE))THEN
                        IF(returnfalse)THEN
                            FindFile=.FALSE.
                            RETURN
                        ELSE
                            WRITE(*,'(3A)') "Specified file ",TRIM(filename),&
                                " does not exist."
                            STOP
                        ENDIF
                    ELSE
                        WRITE(*,'(3A)') "Specified file ",TRIM(filename),&
                            " does not exist."
                        STOP
                    ENDIF
                ENDIF
            END FUNCTION


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
            
            CHARACTER(200) FUNCTION WRITE_DATE(MYDATE)
                IMPLICIT NONE
                
                TYPE(DATEVAR),INTENT(IN) :: MYDATE
                CHARACTER(200)           :: DATE_STRING
                
                WRITE(DATE_STRING,'(I4.4,"/",I2.2,"/",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
                                     MYDATE%YEAR,MYDATE%MONTH,&
                                     MYDATE%DAY,MYDATE%HOUR,&
                                     MYDATE%MINUTE,MYDATE%SECOND
                                     
                WRITE_DATE = DATE_STRING
                
            END FUNCTION
                
            
            INTEGER(8) FUNCTION GET_TIMEZONE_OFFSET(TZ)
                IMPLICIT NONE
                CHARACTER(*),INTENT(IN) :: TZ
                INTEGER(8)              :: OFFSET_SECONDS
                ! Abbreviation    Offset Seconds    Offset           Time zone name
                !     ADT             -10800        UTC -3       Atlantic Daylight Time
                !     AKDT            -28800        UTC -8       Alaska Daylight Time
                !     AKST            -32400        UTC -9       Alaska Standard Time
                !     AST             -14400        UTC -4       Atlantic Standard Time
                !     CDT             -18000        UTC -5       Central Daylight Time
                !     CST             -21600        UTC -6       Central Standard Time
                !     EDT             -14400        UTC -4       Eastern Daylight Time
                !     EGST                 0        UTC +0       Eastern Greenland Summer Time
                !     EGT              -3600        UTC -1       East Greenland Time
                !     EST             -18000        UTC -5       Eastern Standard Time
                !     GMT                  0        UTC +0       Greenwich Mean Time
                !     HADT            -32400        UTC -9       Hawaii-Aleutian Daylight Time
                !     HAST            -36000        UTC -10      Hawaii-Aleutian Standard Time
                !     MDT             -21600        UTC -6       Mountain Daylight Time
                !     MST             -25200        UTC -7       Mountain Standard Time
                !     NDT              -9000        UTC -2:30    Newfoundland Daylight Time
                !     NST             -12600        UTC -3:30    Newfoundland Standard Time
                !     PDT             -25200        UTC -7       Pacific Daylight Time
                !     PMDT             -7200        UTC -2       Pierre & Miquelon Daylight Time
                !     PMST            -10800        UTC -3       Pierre & Miquelon Standard Time
                !     PST             -28800        UTC -8       Pacific Standard Time
                !     WGST             -7200        UTC -2       Western Greenland Summer Time
                !     WGT             -10800        UTC -3       West Greenland Time
                SELECT CASE(TRIM(TZ))
                    CASE("ADT")
                        OFFSET_SECONDS = -10800
                    CASE("AKDT")
                        OFFSET_SECONDS = -28800
                    CASE("AKST")
                        OFFSET_SECONDS = -32400                
                    CASE("AST")
                        OFFSET_SECONDS = -14400
                    CASE("CDT")
                        OFFSET_SECONDS = -18000
                    CASE("CST")
                        OFFSET_SECONDS = -21600
                    CASE("EDT")
                        OFFSET_SECONDS = -14400
                    CASE("EGST")
                        OFFSET_SECONDS = 0
                    CASE("EGT")
                        OFFSET_SECONDS = -3600
                    CASE("EST")
                        OFFSET_SECONDS = -18000
                    CASE("GMT")
                        OFFSET_SECONDS = 0
                    CASE("HADT")
                        OFFSET_SECONDS = -32400
                    CASE("HAST")
                        OFFSET_SECONDS = -36000
                    CASE("MDT")
                        OFFSET_SECONDS = -21600
                    CASE("MST")
                        OFFSET_SECONDS = -25200
                    CASE("NDT")
                        OFFSET_SECONDS = -9000
                    CASE("NST")
                        OFFSET_SECONDS = -12600
                    CASE("PDT")
                        OFFSET_SECONDS = -25200
                    CASE("PMDT")
                        OFFSET_SECONDS = -7200
                    CASE("PMST")
                        OFFSET_SECONDS = -10800
                    CASE("PST")
                        OFFSET_SECONDS = -28800
                    CASE("WGST")
                        OFFSET_SECONDS = -7200
                    CASE("WGT")
                        OFFSET_SECONDS = -10800
                    CASE("UTC")
                        OFFSET_SECONDS = 0
                    CASE DEFAULT
                        WRITE(*,'(A)') "ERROR: Time zone not available"
                        STOP
                END SELECT
                
                GET_TIMEZONE_OFFSET = OFFSET_SECONDS
                
            END FUNCTION
            
            SUBROUTINE TIMEZONE_CHANGE(DATE1,TZ1,TZ2,RESULTDATE)
                IMPLICIT NONE
                
                TYPE(DATEVAR),INTENT(IN)     :: DATE1
                CHARACTER(*),INTENT(IN)      :: TZ1
                CHARACTER(*),INTENT(IN)      :: TZ2
                TYPE(DATEVAR),INTENT(OUT)    :: RESULTDATE
                INTEGER(8)                   :: OFFSET_FROM,OFFSET_TO
                INTEGER(8)                   :: OFFSET_TOTAL
                
                OFFSET_FROM = GET_TIMEZONE_OFFSET(TZ1)
                OFFSET_TO   = GET_TIMEZONE_OFFSET(TZ2)
                
                OFFSET_TOTAL = -(OFFSET_FROM - OFFSET_TO)
                
                CALL DATEADD(DATE1,OFFSET_TOTAL,RESULTDATE)
                
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


!------------------------------------------------------------------------------!
!                                                                              !
!       THE FOLLOWING FUNCTIONS ARE FOR ADCIRC GRIDS USE WITH KDTREE2          !
!                                                                              !
!------------------------------------------------------------------------------!


            SUBROUTINE MakeGridNodeKDTREE(mesh,tree)

                IMPLICIT NONE

                TYPE(grid),INTENT(IN)             :: mesh
                TYPE(KDTREE2),POINTER,INTENT(OUT) :: tree
                REAL(8),ALLOCATABLE               :: XY(:,:)

                ALLOCATE(XY(1:2,1:mesh%NumNodes))

                XY(1,:) = mesh%nodes(:,1)
                XY(2,:) = mesh%nodes(:,2)

                Tree => KDTREE2_CREATE(xy,SORT=.TRUE.,REARRANGE=.TRUE.)

                RETURN

            END SUBROUTINE

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

            END SUBROUTINE

            SUBROUTINE MakeElementKDTREE(MyMesh,MyTree)

                IMPLICIT NONE

                INTEGER               :: I
                INTEGER               :: N1
                INTEGER               :: N2
                INTEGER               :: N3
                REAL(8)               :: X1
                REAL(8)               :: X2
                REAL(8)               :: X3
                REAL(8)               :: Y1
                REAL(8)               :: Y2
                REAL(8)               :: Y3
                REAL(8),ALLOCATABLE   :: XY(:,:)
                TYPE(grid),INTENT(IN) :: MyMesh
                TYPE(KDTREE2),POINTER,INTENT(OUT) :: MyTree

                !...Compute Element centers
                ALLOCATE(XY(1:2,1:MyMesh%NumElements))
                DO I = 1,MyMesh%NumElements
                    N1 = MyMesh%conn(I,1)
                    N2 = MyMesh%conn(I,2)
                    N3 = MyMesh%conn(I,3)
                    X1 = MyMesh%nodes(N1,1)
                    X2 = MyMesh%nodes(N2,1)
                    X3 = MyMesh%nodes(N3,1)
                    Y1 = MyMesh%nodes(N1,2)
                    Y2 = MyMesh%nodes(N2,2)
                    Y3 = MyMesh%nodes(N3,2)
                    XY(1,I) = ( X1 + X2 + X3 ) / 3D0
                    XY(2,I) = ( Y1 + Y2 + Y3 ) / 3D0
                ENDDO

                MyTree => KDTREE2_CREATE(xy,SORT=.TRUE.,REARRANGE=.TRUE.)

                RETURN

            END SUBROUTINE

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

            END FUNCTION

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
            END SUBROUTINE



            SUBROUTINE FindElement(X,Y,mesh,tree,E,W1,W2,W3,ElementFound,USearchDepth)

                IMPLICIT NONE

                !...OPTIONAL PARAMETERS
                INTEGER,INTENT(OUT),OPTIONAL     :: E             !...Return element that was found
                INTEGER,INTENT(IN),OPTIONAL      :: USearchDepth  !...Set the maximum search depth
                REAL(8),INTENT(OUT),OPTIONAL     :: W1,W2,W3      !...Return the weights of the 3 nodes
                LOGICAL,INTENT(OUT),OPTIONAL     :: ElementFound  !...Return if the x,y resides in an element

                !...REQUIRED PARAMETERS
                REAL(8),INTENT(IN)               :: X
                REAL(8),INTENT(IN)               :: Y
                TYPE(grid),INTENT(IN)            :: MESH
                TYPE(KDTREE2),POINTER,INTENT(IN) :: TREE

                INTEGER                          :: SearchDepth = 20 !...Maximum number of near elements to check
                INTEGER                          :: MyE

                REAL(8)                          :: X1,X2,X3
                REAL(8)                          :: Y1,Y2,Y3
                REAL(8)                          :: S1,S2,S3
                REAL(8)                          :: TA
                INTEGER                          :: N1,N2,N3
                INTEGER                          :: I
                TYPE(KDTREE2_RESULT),ALLOCATABLE :: KDRESULTS(:)
                TYPE(KDTREE2_RESULT),ALLOCATABLE :: KDRESULTS2(:)
                LOGICAL                          :: SmallSearch
                LOGICAL                          :: Found
                LOGICAL                          :: BADIN
                LOGICAL                          :: EXTENDEDINFOWEIGHT


                !...User specified search depth, otherwise 20
                IF(PRESENT(USEARCHDEPTH))THEN
                    SearchDepth=USEARCHDEPTH
                ENDIF

                !...Sanity Check on depth of search
                IF(Tree%N.LT.SearchDepth)THEN
                    SearchDepth = Tree%N
                ENDIF

                ALLOCATE(KDRESULTS(1:SearchDepth))
                CALL KDTREE2_N_NEAREST(TP=TREE,QV=(/X,Y/),NN=SearchDepth,&
                        RESULTS=KDRESULTS)

                !...SANITY CHECK ON INPUTS
                BADIN=.FALSE.
                IF(PRESENT(W1).AND.&
                    ((.NOT.PRESENT(W2)).OR.&
                     (.NOT.PRESENT(W3)).OR.&
                     (.NOT.PRESENT(E))))THEN
                        BADIN=.TRUE.
                ELSEIF(PRESENT(W2).AND.&
                    ((.NOT.PRESENT(W1)).OR.&
                     (.NOT.PRESENT(W3)).OR.&
                     (.NOT.PRESENT(E))))THEN
                        BADIN=.TRUE.
                ELSEIF(PRESENT(W3).AND.&
                    ((.NOT.PRESENT(W2)).OR.&
                     (.NOT.PRESENT(W1)).OR.&
                     (.NOT.PRESENT(E))))THEN
                        BADIN=.TRUE.
                ENDIF

                IF(PRESENT(W1))THEN
                    EXTENDEDINFOWEIGHT=.TRUE.
                ELSE
                    EXTENDEDINFOWEIGHT=.FALSE.
                ENDIF

                IF(BADIN)THEN
                    WRITE(*,'(A)') "ERROR: Please check input parameters to 'FindElement' subroutine."
                    WRITE(*,'(A)') "   W1, W2, W3 and E are all optional prameters but must be"
                    WRITE(*,'(A)') "   specified so that if any of W1, W2, or W3 are specified,"
                    WRITE(*,'(A)') "   all must be specified, including E."
                    STOP
                ENDIF

                Found = .FALSE.
                FindEL: DO I = 1,SearchDepth
                    MyE = KDRESULTS(I)%IDX
                    IF(PRESENT(E))E=MyE
                    N1 = mesh%conn(MyE,1)
                    N2 = mesh%conn(MyE,2)
                    N3 = mesh%conn(MyE,3)
                    X1 = mesh%nodes(N1,1)
                    X2 = mesh%nodes(N2,1)
                    X3 = mesh%nodes(N3,1)
                    Y1 = mesh%nodes(N1,2)
                    Y2 = mesh%nodes(N2,2)
                    Y3 = mesh%nodes(N3,2)
                    S1 = ABS((X2*Y3-X3*Y2)-(X*Y3-X3*Y)+(X*Y2-X2*Y))
                    S2 = ABS((X*Y3-X3*Y)-(X1*Y3-X3*Y1)+(X1*Y-X*Y1))
                    S3 = ABS((X2*Y-X*Y2)-(X1*Y-X*Y1)+(X1*Y2-X2*Y1))
                    TA = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))
                    IF((S1+S2+S3).LE.1.001D0*TA)THEN
                        IF(EXTENDEDINFOWEIGHT)THEN
                            W1 = ((X-X3)*(Y2-Y3)+(X2-X3)*(Y3-Y))/TA
                            W2 = ((X-X1)*(Y3-Y1)-(Y-Y1)*(X3-X1))/TA
                            W3 = ((Y-Y1)*(X2-X1)-(X-X1)*(Y2-Y1))/TA
                        ENDIF
                        Found = .TRUE.
                        EXIT FindEL
                    ENDIF
                ENDDO FindEL

                IF(.NOT.Found)THEN
                    IF(PRESENT(E))E  = KDRESULTS(1)%IDX
                    IF(EXTENDEDINFOWEIGHT)THEN
                        W1 = 1D0/3D0
                        W2 = 1D0/3D0
                        W3 = 1D0/3D0
                    ENDIF
                ENDIF

                IF(PRESENT(ELEMENTFOUND))ELEMENTFOUND=FOUND

                RETURN

            END SUBROUTINE

!------------------------------------------------------------------------------!
!                                                                              !
!       THE FOLLOWING ARE DEALLOCATION ROUTINES TO BE CALLED TO DESTROY A      !
!       DERRIVED TYPE VARIABLE CREATED BY THIS CODE                            !
!                                                                              !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE DESTROY_ADCMOD(MESH,NODALATTRIBUTES,ADCOUTPUT)
            IMPLICIT NONE

            TYPE(GRID),INTENT(INOUT),OPTIONAL         :: MESH
            TYPE(FORT13),INTENT(INOUT),OPTIONAL       :: NODALATTRIBUTES
            TYPE(ADCIRCOUTPUT),INTENT(INOUT),OPTIONAL :: ADCOUTPUT
            INTEGER                                   :: DOMESH
            INTEGER                                   :: DONODALATTRIBUTES
            INTEGER                                   :: DOADCOUTPUT
            INTEGER                                   :: I

            DOMESH=0
            DONODALATTRIBUTES=0
            DOADCOUTPUT=0

            IF(PRESENT(MESH))DOMESH=1
            IF(PRESENT(NODALATTRIBUTES))DONODALATTRIBUTES=1
            IF(PRESENT(ADCOUTPUT))DOADCOUTPUT=1

            IF(DOMESH+DONODALATTRIBUTES+DOADCOUTPUT.GT.1)THEN
                WRITE(*,'(A)') "ERROR: Too many inputs specified. Only specify one."
                STOP
            ENDIF

            IF(DOMESH+DONODALATTRIBUTES+DOADCOUTPUT.LT.1)THEN
                WRITE(*,'(A)') "ERROR: Too few inputs specified. You must specify one."
            ENDIF

            IF(DOMESH.EQ.1)THEN
                IF(ALLOCATED(MESH%NODES))DEALLOCATE(MESH%NODES)
                IF(ALLOCATED(MESH%CONN))DEALLOCATE(MESH%CONN)
                IF(MESH%NUMOPENBOUNDARIES.GT.0)THEN
                    DO I = 1,MESH%NUMLANDBOUNDARIES
                        IF(ALLOCATED(MESH%LANDBC(I)%N1))&
                            DEALLOCATE(MESH%LANDBC(I)%N1)
                        IF(ALLOCATED(MESH%LANDBC(I)%N2))&
                            DEALLOCATE(MESH%LANDBC(I)%N2)
                        IF(ALLOCATED(MESH%LANDBC(I)%CREST))&
                            DEALLOCATE(MESH%LANDBC(I)%CREST)
                        IF(ALLOCATED(MESH%LANDBC(I)%SUPERCRITICAL))&
                            DEALLOCATE(MESH%LANDBC(I)%SUPERCRITICAL)
                        IF(ALLOCATED(MESH%LANDBC(I)%SUBCRITICAL))&
                            DEALLOCATE(MESH%LANDBC(I)%SUBCRITICAL)
                    ENDDO
                ENDIF
                IF(MESH%NUMLANDBOUNDARIES.GT.0)THEN
                    DO I = 1,MESH%NUMLANDBOUNDARIES
                        IF(ALLOCATED(MESH%LANDBC(I)%N1))&
                            DEALLOCATE(MESH%LANDBC(I)%N1)
                        IF(ALLOCATED(MESH%LANDBC(I)%N2))&
                            DEALLOCATE(MESH%LANDBC(I)%N2)
                        IF(ALLOCATED(MESH%LANDBC(I)%CREST))&
                            DEALLOCATE(MESH%LANDBC(I)%CREST)
                        IF(ALLOCATED(MESH%LANDBC(I)%SUPERCRITICAL))&
                            DEALLOCATE(MESH%LANDBC(I)%SUPERCRITICAL)
                        IF(ALLOCATED(MESH%LANDBC(I)%SUBCRITICAL))&
                            DEALLOCATE(MESH%LANDBC(I)%SUBCRITICAL)
                    ENDDO
                ENDIF
                MESH%Title = ""
                MESH%NumElements = 0
                MESH%NumNodes = 0
                MESH%NumOpenBoundaries = 0
                MESH%NumLandBoundaries = 0
                MESH%TotNumOpenBoundaryNodes = 0
                MESH%TotNumLandBoundaryNodes = 0
            ENDIF

            IF(DONODALATTRIBUTES.EQ.1)THEN
                IF(NODALATTRIBUTES%NumAttributes.GT.0)THEN
                    DO I = 1,NODALATTRIBUTES%NumAttributes
                        IF(ALLOCATED(NODALATTRIBUTES%NODAL_PARAM(I)%VALUES))&
                            DEALLOCATE(NODALATTRIBUTES%NODAL_PARAM(I)%VALUES)
                        NODALATTRIBUTES%NODAL_PARAM(I)%Attribute = ""
                        NODALATTRIBUTES%NODAL_PARAM(I)%DefaultValue = 0D0
                        NODALATTRIBUTES%NODAL_PARAM(I)%Units = ""
                        NODALATTRIBUTES%NODAL_PARAM(I)%NumValues = 0
                    ENDDO
                    IF(ALLOCATED(NODALATTRIBUTES%nodal_param))&
                        DEALLOCATE(NODALATTRIBUTES%nodal_param)
                ENDIF
                NODALATTRIBUTES%Title = ""
                NODALATTRIBUTES%NumNodes = 0
                NODALATTRIBUTES%NumAttributes = 0
            ENDIF

            IF(DOADCOUTPUT.EQ.1)THEN
                IF(ADCOUTPUT%NumTimeSteps.GT.0)THEN
                    DO I = 1,ADCOUTPUT%NumTimeSteps
                        IF(ALLOCATED(ADCOUTPUT%OUTPUT(I)%VALUES))&
                            DEALLOCATE(ADCOUTPUT%OUTPUT(I)%VALUES)
                        ADCOUTPUT%OUTPUT(I)%TS = 0
                        ADCOUTPUT%OUTPUT(I)%NumNonDefault = 0
                        ADCOUTPUT%OUTPUT(I)%DefaultValue = 0
                        ADCOUTPUT%Output(I)%Time = 0
                    ENDDO
                    IF(ALLOCATED(ADCOUTPUT%OUTPUT))&
                        DEALLOCATE(ADCOUTPUT%OUTPUT)
                ENDIF
                ADCOUTPUT%TITLE = ""
                ADCOUTPUT%DT = 0D0
                ADCOUTPUT%NUMVALUES = 0
                ADCOUTPUT%NumNodes = 0
                ADCOUTPUT%NumTimeSteps = 0
                ADCOUTPUT%INTERVAL = 0
                ADCOUTPUT%FILEFORMAT = 0
            ENDIF

            RETURN

        END SUBROUTINE


!------------------------------------------------------------------------------!
!                                                                              !
!       THE FOLLOWING ARE ArcGIS ASCII FORMAT READ/WRITE ROUTINES              !
!                                                                              !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE ASCII2BINARY(FILENAME,REALDATA,INTDATA)

            CHARACTER(*),INTENT(IN)     :: FILENAME
            LOGICAL,INTENT(IN),OPTIONAL :: REALDATA
            LOGICAL,INTENT(IN),OPTIONAL :: INTDATA

            INTEGER :: NCOL
            INTEGER :: NROW
            INTEGER :: DUM_INT
            INTEGER :: RECLENR
            INTEGER :: RECLENI
            INTEGER :: STARTRECR
            INTEGER :: STARTRECI
            INTEGER :: I,J,IDX,N
            INTEGER :: ND_VALUEI
            INTEGER :: READUNIT,WRITEUNIT
            INTEGER,ALLOCATABLE :: TEMPCOLI(:)
            REAL(8) :: CellSize
            REAL(8) :: XLLGAP
            REAL(8) :: YLLGAP
            REAL(8) :: YU,YL,XL,XR
            REAL(8) :: ND_VALUER
            REAL(8) :: DUM_REAL
            REAL(8),ALLOCATABLE :: TEMPCOLR(:)

            !...Get Read/Write Unit numbers
            READUNIT = GETFREEUNIT()
            WRITEUNIT = GETFREEUNIT()

            !...Sanity check input
            IF(PRESENT(REALDATA).AND.PRESENT(INTDATA))THEN
                IF(REALDATA.EQV.INTDATA)THEN
                    WRITE(*,'(A)') "ERROR: Must Select only one file format."
                    STOP
                ENDIF
            ELSEIF(.NOT.PRESENT(REALDATA).AND..NOT.PRESENT(INTDATA))THEN
                WRITE(*,'(A)') "ERROR: Must select a file format."
                STOP
            ENDIF

            !...Set the write length of the record
            INQUIRE(IOLENGTH=RECLENR) DUM_REAL
            INQUIRE(IOLENGTH=RECLENI) DUM_INT

            !...First "DATA" Record for REAL after header
            STARTRECR = 10

            !...First "DATA" Record for INT after header, Convert BYTE offset
            STARTRECI = (RECLENR*(STARTRECR-1))/RECLENI

            !...Read header from ASCII
            OPEN(FILE=TRIM(FILENAME),UNIT=1,ACTION="READ")
            READ(READUNIT,*) JunkC, NCOL
            READ(READUNIT,*) JunkC, NROW
            READ(READUNIT,*) JunkC, xllGAP
            READ(READUNIT,*) JunkC, yllGAP
            READ(READUNIT,*) JunkC, cellsize
            IF(REALDATA)THEN
                READ(READUNIT,*) JunkC, ND_ValueR
            ELSEIF(INTDATA)THEN
                READ(READUNIT,*) JunkC, ND_ValueI
            ENDIF

            !...Find corners
            xl = xllGAP
            xr = xllGAP + (dble(ncol)-1) * cellsize
            yl = yllGAP
            yu = yllGAP + (dble(nrow)-1) * cellsize

            !...Allocate value holding array
            IF(REALDATA)THEN
                ALLOCATE(TEMPCOLR(1:NCOL))
            ELSEIF(INTDATA)THEN
                ALLOCATE(TEMPCOLI(1:NCOL))
            ENDIF

            !...Open binary file and write sequential records for each
            !   data type
            IF(REALDATA)THEN
                OPEN(FILE=TRIM(FILENAME)//".binary",UNIT=2,ACTION="WRITE",&
                    ACCESS="DIRECT",RECL=RECLENR)
                !...HEADER
                WRITE(WRITEUNIT,REC=1) 0D0        !...Confirm data type, 0=Real, 1=Int
                WRITE(WRITEUNIT,REC=2) DBLE(NCOL) !...Number of columns
                WRITE(WRITEUNIT,REC=3) DBLE(NROW) !...Number of rows
                WRITE(WRITEUNIT,REC=4) XL         !...Left Corners X
                WRITE(WRITEUNIT,REC=5) XR         !...Right Corners X
                WRITE(WRITEUNIT,REC=6) YL         !...Lower Corners Y
                WRITE(WRITEUNIT,REC=7) YU         !...Upper Corners Y
                WRITE(WRITEUNIT,REC=8) CellSize   !...Size of each cell
                WRITE(WRITEUNIT,REC=9) ND_ValueR  !...No Data value (Real)
                IDX = STARTRECR-1
                N = 0
                !...BODY
                DO I = 1,NROW
                    READ(READUNIT,*) (TEMPCOLR(J),J=1,NCOL)
                    DO J = 1,NCOL
                        IDX = IDX + 1
                        WRITE(WRITEUNIT,REC=IDX) TEMPCOLR(J)
                        CALL SHOWBAR(NROW*NCOL,IDX,N)
                    ENDDO
                ENDDO
                CLOSE(READUNIT)
                CLOSE(WRITEUNIT)
            ELSEIF(INTDATA)THEN
                OPEN(FILE=TRIM(FILENAME)//".binary",UNIT=WRITEUNIT,ACTION="WRITE",&
                    ACCESS="DIRECT",RECL=RECLENR)
                !...HEADER
                WRITE(WRITEUNIT,REC=1) 1D0        !...Confirm data type, 0=Real, 1=Int
                WRITE(WRITEUNIT,REC=2) DBLE(NCOL) !...Number of columns
                WRITE(WRITEUNIT,REC=3) DBLE(NROW) !...Number of rows
                WRITE(WRITEUNIT,REC=4) XL         !...Left Corners X
                WRITE(WRITEUNIT,REC=5) XR         !...Right Corners X
                WRITE(WRITEUNIT,REC=6) YL         !...Lower Corners Y
                WRITE(WRITEUNIT,REC=7) YU         !...Upper Corners Y
                WRITE(WRITEUNIT,REC=8) CellSize   !...Size of each cell
                WRITE(WRITEUNIT,REC=9) DBLE(ND_ValueI)  !...No Data value (Int)
                CLOSE(WRITEUNIT)
                OPEN(FILE=TRIM(FILENAME)//".binary",UNIT=WRITEUNIT,ACTION="WRITE",&
                    ACCESS="DIRECT",RECL=RECLENI)
                IDX = STARTRECI
                N = 0
                !...BODY
                DO I = 1,NROW
                    READ(READUNIT,*) (TEMPCOLI(J),J=1,NCOL)
                    DO J = 1,NCOL
                        IDX = IDX + 1
                        WRITE(WRITEUNIT,REC=IDX) TEMPCOLI(J)
                        CALL SHOWBAR(NROW*NCOL,IDX,N)
                    ENDDO
                ENDDO
                CLOSE(READUNIT)
                CLOSE(WRITEUNIT)
            ENDIF


        END SUBROUTINE

        SUBROUTINE READBINARYDATA(FILENAME,X,Y,FLAG,VALUESINT,VALUESREAL)

            CHARACTER(*),INTENT(IN)        :: FILENAME
            REAL(8),INTENT(IN)             :: X
            REAL(8),INTENT(IN)             :: Y
            REAL(8),INTENT(IN)             :: FLAG
            INTEGER,INTENT(INOUT),OPTIONAL :: VALUESINT(:,:)
            REAL(8),INTENT(INOUT),OPTIONAL :: VALUESREAL(:,:)

            !...Flag values to match Griddata functions
            !   -777   nearest
            !   -888   highest in 1x
            !   -999   1x
            !   -950   2x
            !   -960   4x
            !   -970   8x

            IF(FLAG.EQ.-777D0)THEN

            ELSEIF(FLAG.EQ.-888D0)THEN

            ELSEIF(FLAG.EQ.-999D0)THEN

            ELSEIF(FLAG.EQ.-950D0)THEN

            ELSEIF(FLAG.EQ.-960D0)THEN

            ELSEIF(FLAG.EQ.-970D0)THEN

            ENDIF


        END SUBROUTINE


        !...This routine will ensure that units are not doubled by the user. This module uses this
        !   exclusively to avoid conflicts.
        INTEGER FUNCTION GETFREEUNIT()
            INTEGER :: I
            LOGICAL :: ISOPEN
            I = 0
            DO
                I = I + 1
                INQUIRE(UNIT=I,OPENED=ISOPEN)
                IF(ISOPEN)CYCLE
                GETFREEUNIT = I
                EXIT
            ENDDO
            RETURN
        END FUNCTION

#ifdef ADCNETCDF
!------------------------------------------------------------------------------!
!                                                                              !
!       THE FOLLOWING FUNCTIONS ARE FOR ADCIRC USE WITH NETCDF                 !
!                                                                              !
!------------------------------------------------------------------------------!
!
!  USE: nc-config --flibs  }
!               and        }   These two commands will show you how to compile NetCDF on your system
!       nc-config --fflags }
!
!       WARNING: Make sure the path to NetCDF lib directory is part of your LD_LIBRARY_PATH variable
!
!
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

                NC_LONGNAME(:)  = ""
                NC_LONGNAME(1)  = "water column vertically varying density"
                NC_LONGNAME(2)  = "water column vertically varying salinity"
                NC_LONGNAME(3)  = "water column vertically varying temperature"
                NC_LONGNAME(4)  = "water column vertically varying east/west velocity"
                NC_LONGNAME(5)  = "water column vertically varying north/south velocity"
                NC_LONGNAME(6)  = "water column vertically varying up/down velocity"
                NC_LONGNAME(7)  = "water column vertically varying turbulent kinetic energy"
                NC_LONGNAME(8)  = "water column vertically varying mixing length"
                NC_LONGNAME(9)  = "water column vertically varying eddy viscosity"
                NC_LONGNAME(10) = "sea surface temperature at the k+1 time level"
                NC_LONGNAME(11) = "water surface elevation above geoid"
                NC_LONGNAME(12) = "maximum water surface elevation above geoid"
                NC_LONGNAME(13) = "water column vertically averaged east/west velocity"
                NC_LONGNAME(14) = "water column vertically averaged north/south velocity"
                NC_LONGNAME(15) = "maximum water column vertically averaged velocity"
                NC_LONGNAME(16) = "air pressure at sea level"
                NC_LONGNAME(17) = "minimum air pressure at sea level"
                NC_LONGNAME(18) = "wind velocity in x-direction"
                NC_LONGNAME(19) = "wind velocity in y-direction"
                NC_LONGNAME(20) = "maximum wind velocity"
                NC_LONGNAME(21) = "radiation stress gradient x component"
                NC_LONGNAME(22) = "radiation stress gradient y component"
                NC_LONGNAME(23) = "maximum radiation stress gradient"
                NC_LONGNAME(24) = "significant wave height"
                NC_LONGNAME(25) = "maximum significant wave height"
                NC_LONGNAME(26) = "mean wave direction"
                NC_LONGNAME(27) = "maximum mean wave direction"
                NC_LONGNAME(28) = "mean absolute wave period"
                NC_LONGNAME(29) = "maximum TM01 mean wave period"
                NC_LONGNAME(30) = "smoothed peak period"
                NC_LONGNAME(31) = "maximum smoothed peak period"
                NC_LONGNAME(32) = "wind velocity in x-direction"
                NC_LONGNAME(33) = "wind velocity in y-direction"
                NC_LONGNAME(34) = "maximum wind stress"
                NC_LONGNAME(35) = "mean absoloute zero crossing period"
                NC_LONGNAME(36) = "maximum TM02 mean wave period"
                NC_LONGNAME(37) = "mean absolute wave period"
                NC_LONGNAME(38) = "maximum TMM10 mean wave period"

                NC_STDNAME(:)  = ""
                NC_STDNAME(1)  = "water_density_vertically_varying"
                NC_STDNAME(2)  = "water_salinity_vertically_varying"
                NC_STDNAME(3)  = "water_temperature_vertically_varying"
                NC_STDNAME(4)  = "eastward_water_velocity_vertically_varying"
                NC_STDNAME(5)  = "northward_water_velocity_vertically_varying"
                NC_STDNAME(6)  = "upward_water_velocity_vertically_varying"
                NC_STDNAME(7)  = "turbulent_kinetic_energy_vertically_varying"
                NC_STDNAME(8)  = "water_mixing_length_vertically_varying"
                NC_STDNAME(9)  = "water_eddy_viscosity_vertically_varying"
                NC_STDNAME(10) = "future sea surface temperature"
                NC_STDNAME(11) = "sea_surface_height_above_geoid"
                NC_STDNAME(12) = "maximum_sea_surface_height_above_geoid"
                NC_STDNAME(13) = "x_water_velocity_depth_averaged"
                NC_STDNAME(14) = "y_water_velocity_depth_averaged"
                NC_STDNAME(15) = "maximum_water_velocity_depth_averaged"
                NC_STDNAME(16) = "air_pressure_at_sea_level"
                NC_STDNAME(17) = "minimum_air_pressure_at_sea_level"
                NC_STDNAME(18) = "x_wind"
                NC_STDNAME(19) = "y_wind"
                NC_STDNAME(20) = "maximum_wind"
                NC_STDNAME(21) = "radiation_stress_gradient_x"
                NC_STDNAME(22) = "radiation_stress_gradient_y"
                NC_STDNAME(23) = "maximum_radiation_stress"
                NC_STDNAME(24) = "sea_surface_wave_significant_height"
                NC_STDNAME(25) = "maximum_sea_surface_wave_significant_height"
                NC_STDNAME(26) = "sea_surface_wave_to_direction"
                NC_STDNAME(27) = "maximum_sea_surface_wave_to_direction"
                NC_STDNAME(28) = "sea_surface_wave_mean_period_from_variance_spectral_density_first_frequency_moment"
                NC_STDNAME(29) = "maximum_sea_surface_wave_mean_period_from_variance_spectral_density_first_frequency_moment"
                NC_STDNAME(30) = "sea_surface_wave_period_at_variance_spectral_density_maximum"
                NC_STDNAME(31) = "maximum_sea_surface_wave_period_at_variance_spectral_density_maximum"
                NC_STDNAME(32) = "x_wind"
                NC_STDNAME(33) = "y_wind"
                NC_STDNAME(34) = "maximum_wind"
                NC_STDNAME(35) = "sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment"
                NC_STDNAME(36) = "maximum_sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment"
                NC_STDNAME(37) = "sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment"
                NC_STDNAME(38) = "maximum_sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment"


        !....NOTE: The reason some are not implemented is because multiple variables appear in those
        !          NetCDF files and a user input option will need to be speicified before these can
        !          be correctly enabled. In the current scheme, the first variable listed above will
        !          always be found. If need be, you can reorder these to plot the desired variable.


                RETURN

            END SUBROUTINE


            SUBROUTINE FindMyNetCDFVariable(NCID,Vector)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN) :: NCID
                LOGICAL,INTENT(IN),OPTIONAL :: Vector
                INTEGER :: I
                INTEGER :: J
                INTEGER :: NVAR
                CHARACTER(200) :: NC_NAME

                CALL CHECK(NF90_INQUIRE(NCID,NVARIABLES=NVAR))

                CALL Initialize_NETCDF()
                DO I = 1,NVAR
                    CALL CHECK(NF90_INQUIRE_VARIABLE(NCID,I,&
                        NAME=NC_NAME))
                    DO J = 1,SIZE(NETCDF_TYPES)
                        IF(TRIM(ADJUSTL(NC_NAME)).EQ.&
                                TRIM(ADJUSTL(NETCDF_TYPES(J))))THEN
                            IF(.NOT.PRESENT(VECTOR))RETURN
                            IF(.NOT.Vector)RETURN
                            IF(Vector)THEN
                                SELECT CASE(J)
                                    CASE(13,14,18,19,21,22,33,34)
                                        RETURN
                                    CASE DEFAULT
                                        CONTINUE
                                END SELECT
                            ENDIF
                        ENDIF
                    ENDDO
                    IF(I.EQ.NVAR)THEN
                        WRITE(*,'(A)') &
                            "ADCIRC NetCDF Variable not found in file."
                        STOP
                    ENDIF
                ENDDO

            END SUBROUTINE


            SUBROUTINE GetNETCDFVarID(NCID,VARID1,VARID2,NCOLS,VarName1,VarName2)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN)  :: NCID
                INTEGER,INTENT(OUT) :: VARID1
                INTEGER,INTENT(OUT) :: VARID2
                INTEGER,INTENT(OUT) :: NCOLS
                CHARACTER(*),INTENT(OUT),OPTIONAL :: VarName1,VarName2
                INTEGER             :: I
                INTEGER             :: J
                INTEGER             :: NVAR
                CHARACTER(200)      :: NC_NAME

                CALL CHECK(NF90_INQUIRE(NCID,NVARIABLES=NVAR))

                DO I = 1,NVAR
                    CALL CHECK(NF90_INQUIRE_VARIABLE(NCID,I,&
                        NAME=NC_NAME))
                    DO J = 1,SIZE(NETCDF_TYPES)
                        IF(TRIM(NC_NAME).EQ.TRIM(NETCDF_TYPES(J)))THEN
                            VARID1 = I
                            IF(PRESENT(VarName1))THEN
                                VarName1 = NC_NAME
                            ENDIF
                            SELECT CASE(J)
                                CASE(13,18,21,33)
                                    NCOLS=2
                                    CALL CHECK(NF90_INQ_VARID(NCID,&
                                        TRIM(NETCDF_TYPES(J+1)),VARID2))
                                    IF(PRESENT(VarName2))THEN
                                        VarName2 = NETCDF_TYPES(J+1)
                                    ENDIF
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

            SUBROUTINE ReadMyNETCDFVariable(NCID,RECORD,VARID1,VEC1,VARID2,VEC2,NumNodes)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN)           :: NCID
                INTEGER,INTENT(IN)           :: RECORD
                INTEGER,INTENT(IN)           :: VARID1
                INTEGER,INTENT(IN),OPTIONAL  :: VARID2
                INTEGER,INTENT(IN)           :: NumNodes
                REAL(8),INTENT(OUT)             :: VEC1(:)
                REAL(8),INTENT(OUT),OPTIONAL    :: VEC2(:)
                CALL CHECK(NF90_GET_VAR(NCID,VARID1,VEC1,START=(/1,RECORD/),COUNT=(/NumNodes,1/)))
                IF(PRESENT(VARID2).AND.PRESENT(VEC2))THEN
                    CALL CHECK(NF90_GET_VAR(NCID,VARID2,VEC2,START=(/1,RECORD/),COUNT=(/NumNodes,1/)))
                ENDIF
                RETURN
            END SUBROUTINE

            SUBROUTINE CHECK(Status)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN) :: Status
                INTEGER,ALLOCATABLE :: Dmy(:)
                IF(Status.NE.NF90_NOERR)THEN
                    WRITE(*,'(A,A)') "FATAL ERROR from ",TRIM(NF90_STRERROR(Status))
#ifdef EBUG
                    Dmy(1) = 1.0D0
#endif
                    STOP
                ENDIF
            END SUBROUTINE

            SUBROUTINE ReadADCIRCOutputNC(Filename,MyOutput,Record)
                IMPLICIT NONE

                CHARACTER(*),INTENT(IN)               :: Filename
                INTEGER,DIMENSION(NF90_MAX_VAR_DIMS)  :: NC_DimIDs
                INTEGER                               :: I
                INTEGER                               :: J
                INTEGER                               :: JunkI
                INTEGER                               :: NC_FILE
                INTEGER                               :: NC_ID1
                INTEGER                               :: NC_ID2
                INTEGER                               :: NC_Status
                INTEGER                               :: NC_Var
                INTEGER                               :: NC_Var2
                INTEGER                               :: NumNodes
                INTEGER                               :: NumRecs
                INTEGER                               :: NumValues
                INTEGER                               :: ierr
                INTEGER,INTENT(IN)                    :: Record
                REAL(8),ALLOCATABLE                   :: DATA1(:)
                REAL(8),ALLOCATABLE                   :: DATA2(:)
                REAL(8)                               :: DefaultValue
                REAL(8)                               :: NC_TIME(1)
                REAL(8)                               :: NC_TIME2(2)
                REAL(8)                               :: DT
                REAL(8)                               :: DTDP
                REAL(8)                               :: Interval
                TYPE(ADCIRCOutput),INTENT(OUT)        :: MyOutput

                !...Open and read netcdf file for specified time snap
                CALL Check(NF90_OPEN(TRIM(Filename),NF90_NOWRITE,&
                    NC_ID1))
                CALL Check(NF90_INQ_DIMID(NC_ID1,"node",JunkI))
                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,JunkI,&
                    len=NumNodes))
                CALL Check(NF90_INQUIRE(NC_ID1,NC_Var))
                CALL Check(NF90_INQ_VARID(NC_ID1,'time',NC_Var))
                CALL Check(NF90_INQUIRE_VARIABLE(NC_ID1,NC_Var,&
                    dimids=NC_DimIDs))
                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_DimIDs(1),len=NumRecs))
                CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time,start=(/Record/),count=(/1/)))
                ierr = NF90_GET_ATT(NC_ID1,NF90_GLOBAL,"dt",DTDP)
                IF(ierr.NE.0)DTDP=1D0

                IF(NumRecs.GT.1)THEN
                    CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time2,&
                        start=(/1/),count=(/2/)))
                    Interval = INT((NC_TIME2(2) - NC_TIME2(1)) / DTDP)
                    DT   = NC_TIME2(2) - NC_TIME2(1)
                ELSE
                    Interval = INT(NC_TIME(1)/DTDP)
                    DT   = NC_TIME(1)
                ENDIF

                CALL Check(NF90_GET_ATT(NC_ID1,NF90_GLOBAL,"dt",DTDP))
                CALL FindMyNetCDFVariable(NC_ID1)
                CALL GetNETCDFVarID(NC_ID1,NC_VAR,NC_VAR2,NumValues)
                CALL Check(NF90_GET_ATT(NC_ID1,NC_Var,'_FillValue',&
                    DefaultValue))

                IF(NumValues.EQ.1)THEN
                    ALLOCATE(DATA1(1:NumNodes))
                    CALL ReadMyNetCDFVariable(NCID=NC_ID1,&
                        RECORD=Record,VARID1=NC_Var,VEC1=DATA1,&
                        NumNodes=NumNodes)
                ELSE
                    ALLOCATE(DATA1(1:NumNodes))
                    ALLOCATE(DATA2(1:NumNodes))
                    CALL ReadMyNetCDFVariable(NCID=NC_ID1,&
                        RECORD=Record,VARID1=NC_Var,VEC1=DATA1,&
                        VARID2=NC_Var2,VEC2=DATA2,NumNodes=NumNodes)
                ENDIF
                CALL Check(NF90_CLOSE(NC_ID1))

                !...Assemble the read data into AdcircOutput structure
                MyOutput%Title        = "NetCDFOutput"
                MyOutput%dt           = DT
                MyOutput%NumNodes     = NumNodes
                MyOutput%NumValues    = NumValues
                MyOutput%NumTimeSteps = 1
                MyOutput%Interval     = Interval
                MyOutput%FileFormat   = 1050624
                ALLOCATE(MyOutput%Output(1:1))
                MyOutput%Output(1)%TS            = INT(NC_TIME(1))
                MyOutput%Output(1)%NumNonDefault = 0
                MyOutput%Output(1)%DefaultValue  = DefaultValue
                MyOutput%Output(1)%Time          = NC_TIME(1)
                ALLOCATE(MyOutput%Output(1)%Values(1:NumNodes,&
                    1:NumValues))
                MyOutput%Output(1)%Values(:,1)   = DATA1(:)
                IF(NumValues.EQ.2)THEN
                    MyOutput%Output(1)%Values(:,2) = DATA2(:)
                ENDIF
                !...Count default value nodes
                IF(Numvalues.EQ.1)THEN
                    DO I = 1,NumNodes
                        IF(MyOutput%Output(1)%Values(I,1).EQ.&
                                DefaultValue)THEN
                            MyOutput%Output(1)%NumNonDefault = &
                                MyOutput%Output(1)%NumNonDefault + 1
                        ENDIF
                    ENDDO
                ELSEIF(NumValues.EQ.2)THEN
                    DO I = 1,NumNodes
                        IF((MyOutput%Output(1)%Values(I,1).EQ.&
                           DefaultValue).AND.&
                           (MyOutput%Output(1)%Values(I,1).EQ.&
                            DefaultValue))THEN
                                MyOutput%Output(1)%NumNonDefault = &
                                MyOutput%Output(1)%NumNonDefault + 1
                        ENDIF
                    ENDDO
                ENDIF

            END SUBROUTINE

#endif

!...Some functions for parsing command line arguments from C++
#ifdef SINGLEEXE
            !>Subroutine to read and parse the command line arguments that come from the C++
            !>code and save them in an array local to this code for retrival later.
            SUBROUTINE ParseCPPCommandLineArgs(InputLength,InputArguments,NumCommandLineArgs)
                USE,INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE

                !>Integer conforming to C standard containing the length of the
                !>command line argument string
                INTEGER(C_INT),INTENT(IN),VALUE        :: InputLength
                !>Character array forming to the C standard containing the
                !>command line arguments

                CHARACTER(C_CHAR),INTENT(IN)           :: InputArguments(*)
                INTEGER,INTENT(OUT)                    :: NumCommandLineArgs
                CHARACTER(InputLength)                 :: InputArguments2
                INTEGER                                :: I,N,PrevSpace

                !...Convert the C_CHAR to a FORTRAN Character array
                DO I = 1,InputLength
                    InputArguments2(I:I) = InputArguments(I)
                ENDDO

                !...Count the spaces so we can determine the command line argument list
                ADCModules_NumCommandLineArgs = 0
                DO I = 1,InputLength
                    IF(InputArguments2(I:I).EQ." ")THEN
                        ADCModules_NumCommandLineArgs = ADCModules_NumCommandLineArgs + 1
                    ENDIF
                ENDDO

                !...Parse the command line arguments into a character array
                ALLOCATE(ADCModules_CommandLineArgs(1:ADCModules_NumCommandLineArgs))
                N = 0
                PrevSpace = 0
                DO I = 2,InputLength
                    IF(InputArguments2(I:I).EQ." ")THEN
                        N = N + 1
                        ADCModules_CommandLineArgs(N) = ADJUSTL(InputArguments2(PrevSpace+1:I-1))
                        PrevSpace = I
                    ELSEIF(I.EQ.InputLength)THEN
                        N = N + 1
                        ADCModules_CommandLineArgs(N) = ADJUSTL(InputArguments2(PrevSpace+1:I))
                    ENDIF
                ENDDO

                NumCommandLineArgs = ADCModules_NumCommandLineArgs

                RETURN
            END SUBROUTINE
#endif

            SUBROUTINE GETARG2(IDX,CLO)
                IMPLICIT NONE
                INTEGER,INTENT(IN)       :: IDX
                CHARACTER(*),INTENT(OUT) :: CLO
#ifndef SINGLEEXE
                CALL GETARG(IDX,CLO)
#else
                IF(IDX.GT.ADCModules_NumCommandLineArgs)THEN
                    CLO = ""
                ELSE
                    CLO = TRIM(ADCModules_CommandLineArgs(IDX))
                ENDIF
#endif
            END SUBROUTINE
                        
            SUBROUTINE SYSTEMCALL(SYSCOMMAND)
                IMPLICIT NONE               
                INTEGER  :: IERR
#ifdef SINGLEEXE                
                EXTERNAL :: SYSTEMCALLC
#endif                
                CHARACTER(*),INTENT(IN) :: SYSCOMMAND
#ifdef SINGLEEXE                
                CALL SYSTEMCALLC(SYSCOMMAND//CHAR(0))
#else
                CALL SYSTEM(SYSCOMMAND)
#endif                
                RETURN
            END SUBROUTINE

        END MODULE ADCMOD

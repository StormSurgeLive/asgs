C      PROGRAM INSERT_GRID
C 
C THIS PROGRAM WILL INSERT ONE ADCIRC GRID INTO ANOTHER.
C
C----------------------------------------------------------------------
C Copyright(C) 2007,2012  Nathan Dill
C
C This program is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C
C This program is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this program.  If not, see <http://www.gnu.org/licenses/>.
C----------------------------------------------------------------------
C
C no need for boundary.map file anymore (sms keeps changing the format)
C now use a open boundary around the delete area mesh.  8-17-11 -nld
C added locatcheck in weirgrid of insert grid to eliminate disjoint nodes/elements
C made sure weirs from both grids get put togethte properly
C added openmmp and ability to do weirs from both grids 3-7-09. -nld
C
C======================================================================
      PROGRAM INSERT_GRID
C----------------------------------------------------------------------
      use gridstuff      

      IMPLICIT NONE
      INTEGER I,J,K,CNT,NNBIG,NEBIG,NNINS,NEINS,HIT,DELNCNT,DELECNT,
     & NEful,NNful,jj,MAXI,L,M,jjj ,mnbou
      INTEGER, ALLOCATABLE :: NIDBIG(:),NIDINS(:),NOCBIG(:,:),
     & NOCINS(:,:),EIDBIG(:),EIDINS(:),INDXBIG(:),INDXINS(:),
     & DEL_NODES(:),minus(:),WEIR_NOC(:,:)
      
      REAL*8 MAXX,MAXY,MINX,MINY,TOL,DIFF,DIFF2,MAXTOL,
     & MAXTOLSQ,XS,TOL2,CROSS,DS1(2),DS2(2),DS3(2),XC,YC
      REAL*8, ALLOCATABLE :: XYZBIG(:,:),XYZINS(:,:),XYweir(:,:)
     & ,XYBND(:,:)
      CHARACTER*80 FILE1,FILE2,FILE3,STR,JUNK,FILE4
C      CHARACTER*2 JUNK


      integer mm(3),ll(3)
      INTEGER NOPEbig,NETAbig,NBOUbig,NVELbig,WEIR_NE
      
      INTEGER, ALLOCATABLE :: NVDLLbig(:),
     &  NBDVbig(:),IBTYPEbig(:),NBVVbig(:),NVELLbig(:),IBCONNbig(:)
      
      REAL*8, ALLOCATABLE :: BARLANHTbig(:),
     & BARLANCFSPbig(:),BARINHTbig(:),BARINCFSBbig(:),BARINCFSPbig(:)




C----------------------------------------------------------------------      
C . . GET FILENAMES
      WRITE(*,*)'WHAT IS THE NAME OF THE BIG GRID?'
      READ(*,*)FILE1
      WRITE(*,*)'WHAT IS THE NAME OF THE INSERT GRID?'
      READ(*,*)FILE2
c      WRITE(*,*)'WHAT IS THE NAME OF THE MAP FILE WITH BOUNDARY ARCS?'
c      READ(*,*)FILE3
      write(*,*)'Name for Delete area grid?'
      read(*,*)FILE4
C----------------------------------------------------------------------      
C . . READ THE BOUNDARY FROM THE *.MAP FILE       
cc      OPEN(UNIT=11,FILE=FILE3)
      
cc      DO I=1,10000
cc         XYBND(1,I)=0.D0
cc         XYBND(2,I)=0.D0
cc      END DO   
      
cc      CNT=1

C . . . . . . . . . . . .LOOP TO READ MAP FILE . . . . . . . . . . . .       
cc      WRITE(*,*)'READING ',TRIM(FILE3)
cc      WRITE(*,*)
cc      DO WHILE(.TRUE.)
      
cc         READ(11,*)STR
cc         IF (TRIM(STR).EQ.'NODE') GOTO 10
cc         IF (TRIM(STR).EQ.'ARC')  GOTO 20
cc         IF (TRIM(STR).EQ.'LEND') GOTO 60
cc         GOTO 50
 
cc 10      READ(11,*)JUNK,XYBND(1,CNT),XYBND(2,CNT)       
CC         CNT=CNT+1
CC         GOTO 50  
         
cc 20      CONTINUE
cc         DO I=1,7
cc            READ(11,*)
cc         END DO
cc         READ(11,*)JUNK,J
cc         DO I=1,J
cc         READ(11,*)XYBND(1,CNT),XYBND(2,CNT)  
cc         CNT=CNT+1
cc         END DO
cc         
cc         
cc         
cc 50      CONTINUE        
cc      END DO
cc 60   CONTINUE
cc      CLOSE(11)
cc      CNT=CNT-1
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .      
C DEBUG      
cc      OPEN(17,FILE='BNDXY.XYZ')
cc      DO I=1,CNT
cc        WRITE(17,*)XYBND(1,I),XYBND(2,I),I
cc      END DO
cc      CLOSE(17)
cc      WRITE(*,*)'THERE ARE ',CNT,' BOUNDARY VERTICIES IN ',TRIM(FILE3) 
cc      WRITE(*,*)
      
      
C----------------------------------------------------------------------
C . . READ IN GRID FILES
      OPEN(UNIT=12,FILE=TRIM(FILE1))
      OPEN(UNIT=13,FILE=TRIM(FILE2))

      WRITE(*,*)'READING ',TRIM(FILE1)
      WRITE(*,*)
      
      READ(12,'(A50)')STR
      READ(12,*)NEBIG,NNBIG
      ALLOCATE( XYZBIG(3,NNBIG),NOCBIG(3,NEBIG),NIDBIG(NNBIG),
     & EIDBIG(NEBIG) )
      
      DO I=1,NNBIG
         READ(12,*)NIDBIG(I),XYZBIG(1,I),XYZBIG(2,I),XYZBIG(3,I)
      END DO
      DO I=1,NEBIG
         READ(12,*)EIDBIG(I),J,NOCBIG(1,I),NOCBIG(2,I),NOCBIG(3,I)
      END DO
      
      CLOSE(12)
      
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .       
      WRITE(*,*)'READING ',TRIM(FILE2)
      WRITE(*,*)
     
      READ(13,'(A50)')JUNK
      READ(13,*)NEINS,NNINS
      ALLOCATE( XYZINS(3,NNINS),NOCINS(3,NEINS),NIDINS(NNINS),
     & EIDINS(NEINS) )
     


      

C . . USE THESE FOR EXTENT OF INSERT GRID      
      MAXX=-990.D99
      MAXY=-990.D99
      MINX=9999999999999.99
      MINY=9999999999999.99
      
      DO I=1,NNINS
         READ(13,*)NIDINS(I),XYZINS(1,I),XYZINS(2,I),XYZINS(3,I)
      MAXX=MAX(MAXX,XYZINS(1,I))
      MAXY=MAX(MAXY,XYZINS(2,I))
      MINX=MIN(MINX,XYZINS(1,I))
      MINY=MIN(MINY,XYZINS(2,I))
      END DO
      DO I=1,NEINS
         READ(13,*)EIDINS(I),J,NOCINS(1,I),NOCINS(2,I),NOCINS(3,I)
      END DO   
      WRITE(*,*)'FINISHED READING GRID FILES'
      WRITE(*,*)'WILL INSERT ',TRIM(JUNK),' INTO ',TRIM(STR)
      WRITE(*,*)'THE EXTENT OF THE INSERT GRID IS: (',MINX,',',MINY,
     & ') TO (',MAXX,',',MAXY,')'
      WRITE(*,*) 
                                                

      CLOSE(13)
      
      
      
C____________________________________________________________________      
c get the weirgrid WEIR_NOC from the insert grid
c      CALL READ14(FILE2)
     
c      WRITE(*,*)'GRIDING WEIRS FROM INSERT GRID'
c      CALL GRIDWEIRS
c      DEALLOCATE( NOC,NID,X,Y,Z,EID )
c      DEALLOCATE( NVELL,NBVV,BARLANHT,
c     & BARLANCFSP,IBCONN,BARINHT,
c     & BARINCFSB,BARINCFSP,IBTYPE )
c      DEALLOCATE( NVDLL, NBDV )

C use the delete_grid.14 created by meshing interior of boundary only map instead
c this should get everything

      WRITE(*,*)'READING delete area mesh'
c      junk='delete_grid.14'
      CALL READ14(FILE4)
      
      ALLOCATE(WEIR_NOC(3,NE),XYweir(2,nn))
      weir_ne=ne
      DO I=1,NE
         DO J=1,3
            WEIR_NOC(J,I)=NOC(J,I)
         END DO
      END DO      
      DO I=1,NN
         XYweir(1,I)=X(I)
         XYweir(2,I)=Y(I)
      END DO
      
      
c    get delete boundary data from  open boundary in delete area mesh
      write(*,*)'i am here',NOPE,NETA
           
      ALLOCATE(XYBND(2,NETA))
      DO I=1,NETA
         XYBND(1,I)=0.D0
         XYBND(2,I)=0.D0
      END DO   
         
       write(*,*)'i am here2',NVDLL(1)
           
      CNT=0
      DO I=1,NOPE
         DO J=1,NVDLL(I)
            CNT=CNT+1
            XYBND(1,CNT)=X(NBDV(cnt))
            XYBND(2,CNT)=Y(NBDV(cnt))
         END DO
      END DO
      
      OPEN(17,FILE='BNDXY.XYZ')
      DO I=1,CNT
        WRITE(17,*)XYBND(1,I),XYBND(2,I),I
      END DO
      CLOSE(17)
      WRITE(*,*)'THERE ARE ',CNT,' BOUNDARY VERTICIES IN ',TRIM(FILE4) 
      WRITE(*,*)
      
      
      
      
      DEALLOCATE( NOC,NID,X,Y,Z,EID )
      DEALLOCATE( NVELL,NBVV,BARLANHT,
     & BARLANCFSP,IBCONN,BARINHT,
     & BARINCFSB,BARINCFSP,IBTYPE )
      DEALLOCATE( NVDLL, NBDV )
c--------------------------------------------------------------------      

 
    
      
      
      
      
C______________________________________________________________________
C----------------------------------------------------------------------       
C . . FIND NODES IN BIG GRID THAT ARE NEAREST TO BOUNDARY NODES OF INSERT GRID
      
      ALLOCATE ( INDXBIG(CNT),INDXINS(CNT) )
             
      DO I=1,CNT
         INDXBIG(I)=0
         INDXINS(I)=0
      END DO   
      WRITE(*,*)'FINDING NODES IN BIG GRID NEAREST BOUNDARY OF INSERT'
      WRITE(*,*)
      WRITE(*,*)' CHOOSE A STARTING SEARCH TOLERANCE ?'
      READ(*,*)TOL
      WRITE(*,*)' MAXIMUM TOLERANCE(IF MAX IS REACHED WILL ABORT) ?'
      READ(*,*)MAXTOL

      MAXTOLSQ=MAXTOL**2.D0

C FIND THE NODES THAT ARE WITHIN TOL     

      DO I=1,NNBIG
        
         DO J=1,CNT
            IF ( (XYBND(1,J).LT.XYZBIG(1,I)+TOL) .AND. 
     &           (XYBND(1,J).GT.XYZBIG(1,I)-TOL) .AND.
     &           (XYBND(2,J).LT.XYZBIG(2,I)+TOL) .AND.
     &           (XYBND(2,J).GT.XYZBIG(2,I)-TOL))  THEN    
               INDXBIG(J)=NIDBIG(I)
               GOTO 100
            END IF 
         END DO
         
 100    CONTINUE         
      END DO     
      
C-------------------------------------------------------------------
C COUNT THE NUMBER OF NODES NOT FOUND WITHIN TOL
C AND INCREASE TOL
      K=0
      DO I=1,CNT
         IF(INDXBIG(I).EQ.0) K=K+1 
      END DO

      WRITE(*,*)K,' NODES WERE NOT FOUND WITHIN ',TOL

C----------------------------------------------------------------
C if some nodes weren't found within the first tolerance, they might have been moved slightly                 
      IF(K.GT.0) THEN
        WRITE(*,*)' HOW MUCH DO YOU WANT INCREASE TOL(?*TOL)?'
        READ(*,*)XS
      

C---------------------------------------------------------------------
C NOW FIND NODES THAT WEREN'T INDEXED, INCREASE TOL AND LOOK AGAIN     
      
        DO J=1,CNT
        TOL2= TOL*XS
         
         IF(INDXBIG(J).EQ.0) THEN 
 150       CONTINUE           
            DIFF=MAXTOLSQ
            HIT=0
 
            DO I=1,NNBIG 
            IF ( (XYBND(1,J).LT.XYZBIG(1,I)+TOL2) .AND. 
     &           (XYBND(1,J).GT.XYZBIG(1,I)-TOL2) .AND.
     &           (XYBND(2,J).LT.XYZBIG(2,I)+TOL2) .AND.
     &           (XYBND(2,J).GT.XYZBIG(2,I)-TOL2) )  THEN  
C THE NEXT 3 LINES ENSURE THAT YOU FIND THE CLOSEST NODE, NOT JUST THE FIRST ON WITHIN TOL2
               DIFF2=DIFF   
               DIFF=MIN(DIFF2,(XYBND(1,J)-XYZBIG(1,I))**2+
     &                         (XYBND(2,J)-XYZBIG(2,I))**2)
                  IF(DIFF.LT.DIFF2) THEN  
                     INDXBIG(J)=NIDBIG(I)

                     HIT=HIT+1
                  END IF
               END IF                
            END DO
            
            IF (HIT.GT.0) THEN
cc              WRITE(*,*)
cc              WRITE(*,*)'NODE ',J,' WAS HIT ',HIT,' TIMES WITHIN ',TOL2
               GOTO 200
            END IF    
            
            
C..............................................................            
C this section only run if not found within current TOL2            

              TOL2=TOL2*XS
c              WRITE(*,*)'tol2 = ',TOL2 
              
                IF(TOL2.GT.MAXTOL) THEN
ccccc                MAXTOL=MAXTOL**0.5D0
                WRITE(*,*)'  NEW NODE NUMBER ',J
                WRITE(*,*)' NOT FOUND WITHIN ',MAXTOL
                WRITE(*,*)' THIS IS A PROBLEM, QUITTING'
                PAUSE
                GOTO 999
                END IF
            
              GOTO 150
C..............................................................
            
 200         CONTINUE            
           END IF
        END DO 
C end if some weren't found within first tol
      END IF  
 300  CONTINUE
      OPEN(UNIT=22,FILE='BND_2_BIG.TXT')
      DO I=1,CNT
        WRITE(22,*)INDXBIG(I)
      END DO
      CLOSE(22)
      WRITE(*,*)'INDEX WRITTEN, BND_2_BIG.TXT'        
      
      
      
      
      
      
C______________________________________________________________________
C----------------------------------------------------------------------       
C . . FIND NODES THAT ARE ON BOUNDARY OF INSERT GRID
      WRITE(*,*)
      WRITE(*,*)'NOW INDEXING NODES ON BOUNDARY OF INSERT GRID'
      WRITE(*,*)
             
      WRITE(*,*)' search TOLERANCE ?'
      READ(*,*)TOL
      WRITE(*,*)' MAXIMUM TOLERANCE ?'
      READ(*,*)MAXTOL

      MAXTOLSQ=MAXTOL**2.D0

C FIND THE NODES THAT ARE WITHIN TOL     
      DO I=1,NNINS
        
         DO J=1,CNT
            IF ( (XYBND(1,J).LT.XYZINS(1,I)+TOL) .AND. 
     &           (XYBND(1,J).GT.XYZINS(1,I)-TOL) .AND.
     &           (XYBND(2,J).LT.XYZINS(2,I)+TOL) .AND.
     &           (XYBND(2,J).GT.XYZINS(2,I)-TOL))  THEN    
               INDXINS(J)=NIDINS(I)
               GOTO 310
            END IF 
         END DO
         
 310    CONTINUE         
      END DO     
      
C-------------------------------------------------------------------
C COUNT THE NUMBER OF NODES NOT FOUND WITHIN TOL
C AND INCREASE TOL
      K=0
      DO I=1,CNT
         IF(INDXINS(I).EQ.0) K=K+1 
      END DO

      WRITE(*,*)K,' NODES WERE NOT FOUND WITHIN ',TOL

C----------------------------------------------------------------
C if some nodes weren't found within the first tolerance, they might have been moved slightly                 
      IF(K.GT.0) THEN
        WRITE(*,*)' HOW MUCH DO YOU WANT INCREASE TOL(?*TOL)?'
        READ(*,*)XS
      

C---------------------------------------------------------------------
C NOW FIND NODES THAT WEREN'T INDEXED, INCREASE TOL AND LOOK AGAIN     
      
        DO J=1,CNT
        TOL2= TOL*XS
         
         IF(INDXINS(J).EQ.0) THEN 
 320       CONTINUE           
            DIFF=MAXTOLSQ
            HIT=0
 
            DO I=1,NNINS
            IF ( (XYBND(1,J).LT.XYZINS(1,I)+TOL2) .AND. 
     &           (XYBND(1,J).GT.XYZINS(1,I)-TOL2) .AND.
     &           (XYBND(2,J).LT.XYZINS(2,I)+TOL2) .AND.
     &           (XYBND(2,J).GT.XYZINS(2,I)-TOL2) )  THEN  
C THE NEXT 3 LINES ENSURE THAT YOU FIND THE CLOSEST NODE, NOT JUST THE FIRST ON WITHIN TOL2
               DIFF2=DIFF   
               DIFF=MIN(DIFF2,(XYBND(1,J)-XYZINS(1,I))**2+
     &                         (XYBND(2,J)-XYZINS(2,I))**2)
                  IF(DIFF.LT.DIFF2) THEN  
                     INDXINS(J)=NIDINS(I)

                     HIT=HIT+1
                  END IF
               END IF                
            END DO
            
            IF (HIT.GT.0) THEN
              WRITE(*,*)
              WRITE(*,*)'NODE ',J,' WAS HIT ',HIT,' TIMES WITHIN ',TOL2
               GOTO 330
            END IF    
            
            
C..............................................................            
C this section only run if not found within current TOL2            

              TOL2=TOL2*XS
c              WRITE(*,*)'tol2 = ',TOL2 
              
                IF(TOL2.GT.MAXTOL) THEN
ccccc                MAXTOL=MAXTOL**0.5D0
                WRITE(*,*)'  NEW NODE NUMBER ',J
                WRITE(*,*)' NOT FOUND WITHIN ',MAXTOL
                WRITE(*,*)' THIS IS A PROBLEM, QUITTING'
                PAUSE
                GOTO 400
                END IF
            
              GOTO 320
C..............................................................
            
 330         CONTINUE            
           END IF
        END DO 
C end if some weren't found within first tol
      END IF  
 400  CONTINUE
      OPEN(UNIT=23,FILE='BND_2_INS.TXT')
      DO I=1,CNT
        WRITE(23,*)INDXINS(I)
      END DO
      CLOSE(23)
      WRITE(*,*)'INDEX WRITTEN, BND_2_INS.TXT'      
      
      
      
              
C______________________________________________________________________
C----------------------------------------------------------------------
C FIND BIG GRID NODES THAT ARE INSIDE THE BOUNDARY
      WRITE(*,*) 'FINDING NODES WITHIN INSERT'
                                        open(19,file='del_nodes.xyz')
      
      DELNCNT=0  
C$OMP PARALLEL DO PRIVATE(I,J,JJ,K,DS1,DS2,DS3) REDUCTION(+:DELNCNT)
      DO I=1,NNBIG

CC      WRITE(*,*)I,' OF ',NNBIG
         IF ((XYZBIG(1,I).GE.MINX).AND.
     &       (XYZBIG(1,I).LE.MAXX).AND.
     &       (XYZBIG(2,I).GE.MINY).AND.
     &       (XYZBIG(2,I).LE.MAXY) )     THEN
            
C . . . . . CALCULATE THE CROSSPROCUCTS TO SEE IF BIG GRID NODES LIE 
C . . . . . WITHIN ELEMENTS IN THE INSERT GRID 


C!!!!!!! since we're using the delet_grid instead, there is no need to look in the insert grid  !!!!!!!!!!!!
C            DO J=1,NEINS
C
C               DS1(1)=XYZINS(1,NOCINS(1,J))-XYZBIG(1,I)
C               DS1(2)=XYZINS(2,NOCINS(1,J))-XYZBIG(2,I)
C               DS2(1)=XYZINS(1,NOCINS(2,J))-XYZBIG(1,I)
C               DS2(2)=XYZINS(2,NOCINS(2,J))-XYZBIG(2,I)
C               DS3(1)=XYZINS(1,NOCINS(3,J))-XYZBIG(1,I)
C               DS3(2)=XYZINS(2,NOCINS(3,J))-XYZBIG(2,I)
C . . ALL + CROSS PRODS. MEANS PART. IS FOUND IF NOC IS ANTI-CLOCKWISE      

C               IF ((CROSS(DS1,DS2).GE. 0.d0).AND.
C     &             (CROSS(DS2,DS3).GE. 0.d0).AND.
C     &             (CROSS(DS3,DS1).GE. 0.d0))       THEN
     
C . . CHECK TO SEE IF IT IS A NODE ON BOUNDARY
C                  DO K=1,CNT
C                     IF (NIDBIG(I).EQ.INDXBIG(K)) then    
                              
C                        GOTO 605 
C                     end if     
C                  END DO
C . . IF ITS NOT, IT WILL BE MARKED FOR DELETION BY SETTING ITS NID TO ZERO              
C                  DELNCNT=DELNCNT+1
C                  NIDBIG(I)=0

C                          write(19,*)(xyzbig(jj,i),jj=1,2),i,j
C                  goto 605
C               END IF
C 600          CONTINUE
C            END DO
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!            
            
            
            
C...........CHECK WITHIN WEIRGRID   ! really using the delete_grid
            DO J=1,WEIR_NE

               DS1(1)=XYweir(1,WEIR_NOC(1,J))-XYZBIG(1,I)
               DS1(2)=XYweir(2,WEIR_NOC(1,J))-XYZBIG(2,I)
               DS2(1)=XYweir(1,WEIR_NOC(2,J))-XYZBIG(1,I)
               DS2(2)=XYweir(2,WEIR_NOC(2,J))-XYZBIG(2,I)
               DS3(1)=XYweir(1,WEIR_NOC(3,J))-XYZBIG(1,I)
               DS3(2)=XYweir(2,WEIR_NOC(3,J))-XYZBIG(2,I)
               

C . . ALL + CROSS PRODS. MEANS PART. IS FOUND IF NOC IS ANTI-CLOCKWISE  or clockwise...????    

               IF (((CROSS(DS1,DS2).GE. 0.d0).AND.
     &             (CROSS(DS2,DS3).GE. 0.d0).AND.
     &             (CROSS(DS3,DS1).GE. 0.d0))  )  THEN
     
C . . CHECK TO SEE IF IT IS A NODE ON BOUNDARY
                  DO K=1,CNT
                     IF (NIDBIG(I).EQ.INDXBIG(K)) then    
                              
                        GOTO 605 
                     end if     
                  END DO
C . . IF ITS NOT, IT WILL BE MARKED FOR DELETION BY SETTING ITS NID TO ZERO              
                  DELNCNT=DELNCNT+1
                  NIDBIG(I)=0

                          write(19,*)(xyzbig(jj,i),jj=1,2),'99 999'
                  goto 605
               END IF
 601          CONTINUE
            END DO            
            
            
 605       continue           
         END IF
      END DO
                                     close(19)
      
C----------------------------------------------------------------------
C INSTEAD DELETE ELMENTS WHICH CONTAIN AT LEAST ONE DELETED NODE

C . . MAKE LIST OF DELETED NODE NUMBERS
cnld 3-7-09 not sure, del_nodes.xyz might be messed up by openmp
      ALLOCATE (DEL_NODES(DELNCNT))  
c      WRITE(*,*)
c      WRITE(*,*)'READING LIST OF DELETED NODES FROM delnodes.XYZ'
c      OPEN(19,FILE='del_nodes.xyz')
c      DO I=1,DELNCNT
c         READ(19,*)XC,YC,DEL_NODES(I)
c      END DO
c      CLOSE(19)
      K=0
      DO I=1,NNBIG
        IF (NIDBIG(I).EQ.0) THEN
           K=K+1
           DEL_NODES(K)=I
         END IF
      END DO

      WRITE(*,*)
      WRITE(*,*)'MARKING ELEMENTS FOR DELETION'
      
      DELECNT=0

C$OMP PARALLEL DO PRIVATE(I,J) REDUCTION(+:DELECNT)                 
      DO I=1,NEBIG
CC        write(*,*)i,' of ', nebig 
         DO J=1,DELNCNT 
            IF ( (NOCBIG(1,I).EQ. DEL_NODES(J)) .OR.
     &           (NOCBIG(2,I).EQ. DEL_NODES(J)) .OR.
     &           (NOCBIG(3,I).EQ. DEL_NODES(J))    ) THEN
               EIDBIG(I)=0
               DELECNT=DELECNT+1
               GOTO 880
            END IF
         END DO
 880    CONTINUE
      END DO    
      
      
      WRITE(*,*)
      WRITE(*,*)DELNCNT,' NODES AND ',DELECNT,' ELEMENTS WILL BE ',
     & 'DELETED FROM BIG GRID' 
      WRITE(*,*)
C______________________________________________________________________    

C______________________________________________________________________
C CHECK FOR DUPLICATE ELEMENTS
      WRITE(*,*)'CHECKING FOR DUPLICATE ELEMENTS'
      OPEN(89,FILE='DUPS.OUT')
      
C$OMP PARALLEL DO PRIVATE(I,J,K,ll,MM) REDUCTION(+:DELECNT)      
      do i=1,NEINS
        write(*,*)i
        do k=1,3
           ll(k)=NOCINS(k,i)
        end do
        do j=1,NEBIG
           do k=1,3
              MM(k)=NOCBIG(k,j)
           end do   
              
           if (ll(1).eq. MM(1)) then
             if (ll(2).eq. MM(2)) then
               if (ll(3).eq. MM(3)) then
             write(89,'(8i12)')i,j,(ll(k),k=1,3),(MM(k),k=1,3)
               EIDBIG(J)=0
               DELECNT=DELECNT+1
             
             goto 995
               end if
             end if
           end if
              
           if (ll(1).eq. MM(3)) then
             if (ll(2).eq. MM(1)) then
               if (ll(3).eq. MM(2)) then
             write(89,'(8i12)')i,j,(ll(k),k=1,3),(MM(k),k=1,3)
               EIDBIG(J)=0
               DELECNT=DELECNT+1
             goto 995
               end if
             end if
           end if
              
           if (ll(1).eq. MM(2)) then
             if (ll(2).eq. MM(3)) then
               if (ll(3).eq. MM(1)) then
             write(89,'(8i12)')i,j,(ll(k),k=1,3),(MM(k),k=1,3)
               EIDBIG(J)=0
               DELECNT=DELECNT+1
             goto 995
               end if
             end if
           end if
        end do
 995    continue        
      end do     

      CLOSE(89)
      

cnld 7-18-11
c replace node numbers in big grid on boundary of insert grid with 
c with insert grid node numbers plus nnbig (insert includes boundary)      
      DO I=1,CNT
      NIDBIG(INDXBIG(I))=INDXINS(I)+NNBIG
      END DO

  
C----------------------------------------------------------------------
C  NOW WRITE OUT THE RESULTING GRID
      NEful=NEBIG-DELECNT+NEINS
      NNful=NNBIG-DELNCNT+NNINS-CNT  
C----------------------------------------------------------------------
C THE NODES
      WRITE(*,*)'WRITING NEWFORT.14'
      OPEN(UNIT=44,FILE='NEWFORT.14')
      WRITE(44,*)TRIM(JUNK),' STUFFED INTO ',TRIM(STR)
      WRITE(44,*)NEful,NNful
C . . WRITE NODE LOCATION OF BIG GRID EXCEPT THOSE DELETED
      DO I=1,NNBIG
         IF (NIDBIG(I).NE.0) THEN
            WRITE(44,'(i12,3e20.12)')NIDBIG(I),(XYZBIG(J,I),J=1,3)
         END IF
      END DO
C . . WRITE NODE LOCATIONS OF INSERT GRID EXCEPT BOUNDARY     
      K=NNBIG+1
      DO I=1,NNINS
         DO J=1,CNT
            IF (I.EQ.INDXINS(J)) GOTO 900
         END DO 
         WRITE(44,'(i12,3e20.12)')K,(XYZINS(J,I),J=1,3)
 900     K=K+1
      END DO
      
C----------------------------------------------------------------------
C THE ELEMENTS
C . . REPLACE NIDS IN NOCINS WITH APPROPRIATE NUMBERS FROM INDXBIG
cc      MAXI=0

CNLD 7-18-11  TAKE CARE OF BOUNDARY NOW INCLUDED IN INSERT HERE TOO


      JJ=NNBIG
C$OMP PARALLEL DO PRIVATE(I,J,K)
      DO I=1,NEINS
         DO J=1,3
            NOCINS(J,I)=NOCINS(J,I)+JJ
cc            MAXI=MAX(MAXI,NOCINS(J,I))
CNLD 7-18-11             DO K=1,CNT
CNLD 7-18-11                IF ( (NOCINS(J,I)-JJ) .EQ. INDXINS(K) ) THEN        
CNLD 7-18-11                   NOCINS(J,I)=INDXBIG(K)
                  
CC                  write(*,*)'found match: el,j,ins,big'
CC                  write(*,*) i,j,indxins(k),indxbig(k)
CC                  pause
cc               ELSE IF (K .EQ. 1) THEN
cc                  NOCINS(J,I)=NOCINS(J,I)+NNBIG
CNLD 7-18-11                   GOTO 950
CNLD 7-18-11                END IF
CNLD 7-18-11             END DO
CNLD 7-18-11  950       CONTINUE           
         END DO
      END DO
C$OMP END PARALLEL DO
      
cc      WRITE(*,*)'MAX NODE# IN NOC IS ',MAXI
      

        
      K=1
      DO I=1,NEBIG
         IF (EIDBIG(I) .EQ. 0) GOTO 990 

CNLD 7-18-11  TAKE CARE OF BOUNDARY NOW INCLUDED IN INSERT HERE TOO         
         DO J=1,3
           DO L=1,CNT
            IF (NOCBIG(J,I) .EQ. INDXBIG(L)) THEN
              NOCBIG(J,I)=INDXINS(L)+nnbig
            END IF
           END DO
         END DO
         
         WRITE(44,'(i12,a,3i12)')K,' 3 ',(NOCBIG(J,I),J=1,3)
         K=K+1
 990     CONTINUE
      END DO
      DO I=1,NEINS
         WRITE(44,'(i12,a,3i12)')K,' 3 ',(NOCINS(J,I),J=1,3)
         K=K+1
      END DO
      
C FINISH UP BY WRITING BOUNDARY INFO SAME AS AT END OF BIG GRID
c      DO WHILE(.TRUE.)
c         READ(12,'(A80)',END=999)STR
c         WRITE(44,'(A80)')STR
c      END DO

c finish up by writing all boundary info.
      CALL READ14(FILE1)
      
      NOPEBIG=NOPE
      NETABIG=NETA
      
      ALLOCATE( NVDLLBIG(NOPE), NBDVBIG(NETA) )
      
      K=0
      DO I=1,NOPEBIG
         NVDLLBIG(I)=NVDLL(I) 
         DO J=1,NVDLLBIG(I)
            K=K+1
            NBDVBIG(K)=NBDV(K)
         END DO
      END DO
      
      NBOUBIG=NBOU
      NVELBIG=NVEL
      
       ALLOCATE( NVELLbig(NBOU),NBVVbig(NVEL),BARLANHTbig(NVEL),
     & BARLANCFSPbig(NVEL),IBCONNbig(NVEL),BARINHTbig(NVEL),
     & BARINCFSBbig(NVEL),BARINCFSPbig(NVEL),IBTYPEbig(NBOU),
     & minus(nbou) )
      
      K=0     
      DO I=1,NBOU
         NVELLbig(I)=NVELL(I)
         IBTYPEbig(I)=IBTYPE(I)
         minus(i)=0
         DO J=1,NVELL(I)
            K=K+1
            IF ((IBTYPE(I).EQ. 3) .OR. (IBTYPE(I) .EQ. 13) .OR. 
     &        (IBTYPE(I) .EQ. 23) )  THEN
C               READ(14,*)NBVV(K),BARLANHT(K),BARLANCFSP(K)
               NBVVbig(K)=NBVV(K)
               BARLANHTbig(K)=BARLANHT(K)
               BARLANCFSPbig(K)=BARLANCFSP(K)
                        
            ELSE IF ((IBTYPE(I).EQ. 4) .OR. (IBTYPE(I) .EQ. 14) .OR. 
     &        (IBTYPE(I) .EQ. 24) )  THEN
C              READ(14,*)NBVV(K),IBCONN(K),BARINHT(K),
C     &         BARINCFSB(K),BARINCFSP(K)
               NBVVbig(K)=NBVV(K)
               IBCONNbig(K)=IBCONN(K)
               BARINHTbig(K)=BARINHT(K)
               BARINCFSBbig(K)=BARINCFSB(K)
               BARINCFSPBIG(K)=BARINCFSP(K)
            ELSE
C              READ(14,*)NBVV(K)
               NBVVbig(K)=NBVV(K)
            END IF
         END DO
      END DO
      
c check for land boundaries  between the grids
c also check for weir boundaries
      k=0
      mnbou=0
      do i=1,nboubig
         minus(i)=0
         do j=1,nvellbig(i)
           k=k+1
 
          
c this section to mark weir node pairs within the insert grid for removal          
          if (ibtypebig(i).eq.24  .or. ibtypebig(i).eq.14
     &      .or. ibtypebig(i).eq.4 ) then
     
            if (nidbig(nbvvbig(k)).eq.0 .or.          ! these are the boundary nodes inside the insert grid
     &           nidbig(ibconnbig(k)).eq.0) then      ! this must be done before following loop (l=1,cnt) or NBVVBIG values 
                 ibconnbig(k)=0     
                 nbvvbig(k)=0                  ! adusted to the INDXIN+NNBIG below will cause access violation
                 minus(i)=minus(i)+1
                 nvelbig=nvelbig-2
            end if
            do l=1,cnt
cc               if (nbvvbig(k).eq.indxbig(l).or.
cc     &          ibconnbig(k).eq.indxbig(l)) then 
cc                 nbvvbig(k)=0
cc                 ibconnbig(k)=0
cc                 minus(i)=minus(i)+1
cc                 nvelbig=nvelbig-2
cc               end if
               if (nbvvbig(k).eq.indxbig(l)) then
                 nbvvbig(k)=indxins(l)+nnbig  ! this node is on the boundary, use insert grid #
               end if   
               if (ibconnbig(k).eq.indxbig(l)) then 
                 ibconnbig(k)=indxins(l)+nnbig  ! this node is on the boundary, use insert grid #
               end if  
            end do 
            
          else   
            if (nidbig(nbvvbig(k)).eq.0 ) then     ! these are the boundary nodes inside the insert grid          
                 nbvvbig(k)=0                      ! this must be done before following loop (l=1,cnt) or NBVVBIG values    
                 minus(i)=minus(i)+1               ! adusted to the INDXIN+NNBIG below will cause access violation
                 nvelbig=nvelbig-1
            end if  
            do l=1,cnt
               if (nbvvbig(k).eq.indxbig(l)) then 
                   nbvvbig(k)=indxins(l)+nnbig  ! this node is on the boundary, use insert grid #
cc                 nbvvbig(k)=0
cc                 minus(i)=minus(i)+1
cc                 nvelbig=nvelbig-1
               end if
            end do 

          end if
          
          
          
          
         end do
         nvellbig(i)=nvellbig(i)-minus(i)
         
         if (nvellbig(i).le.1) then
         write(*,*)ibtypebig(i)
             ibtypebig(i)=911
             mnbou=mnbou+1
         write(*,*)'911',i,nbvvbig(k),ibconnbig(k)
         end if    
             
      end do
      
      nboubig=nboubig-mnbou
      
      
      DEALLOCATE( NOC,NID,X,Y,Z,EID )
      
      DEALLOCATE( NVELL,NBVV,BARLANHT,
     & BARLANCFSP,IBCONN,BARINHT,
     & BARINCFSB,BARINCFSP,IBTYPE )
     
      DEALLOCATE( NVDLL, NBDV )
      
      
      CALL READ14(FILE2)
     
      
      K=NOPEbig+NOPE
      WRITE(44,*)K
      K=NETAbig+NETA
      WRITE(44,*)K
      
      K=0
      DO I=1,NOPEbig
         WRITE(44,*) NVDLLbig(I) 
         DO J=1,NVDLLbig(I)
            K=K+1
            WRITE(44,*)NBDVbig(K)
         END DO
      END DO
      
      K=0
      DO I=1,NOPE
         WRITE(44,*)NVDLL(I),' 0 '
         DO J=1,NVDLL(I)
           K=K+1
           WRITE(44,*)NBDV(K)
         END DO
      END DO
      
      K=NBOU+NBOUbig
      WRITE(44,*)K
      write(*,*)'total nbou ',k
      
     
      K=NVEL+NVELbig
      WRITE(44,*)K
      write(*,*)'total nvel ',k
      
      
       K=0     
      DO I=1,NBOUbig+mnbou
C        READ(14,*)NVELL(I),IBTYPE(I)
       if (ibtypebig(i).ne.911) then

c         jjj=nvellbig(i)-minus(i)
         WRITE(44,*)nvellbig(i),IBTYPEbig(I)
         DO J=1,NVELLbig(I)+minus(i)
            K=K+1
            IF ((IBTYPEBIG(I).EQ. 3) .OR. (IBTYPEBIG(I) .EQ. 13) .OR. 
     &        (IBTYPEBIG(I) .EQ. 23) )  THEN
               WRITE(44,'(I8,F12.3,f6.3)')NBVVBIG(K),BARLANHTBIG(K),
     &          BARLANCFSPBIG(K)
                        
         ELSE IF ((IBTYPEBIG(I).EQ. 4) .OR. (IBTYPEBIG(I) .EQ. 14)
     &     .OR.     (IBTYPEBIG(I) .EQ. 24) )  THEN
     
           if (nbvvbig(k).ne.0) then
              WRITE(44,'(2I8,F12.3,2f6.3)')NBVVBIG(K),IBCONNBIG(K),
     &         BARINHTBIG(K),
     &         BARINCFSBBIG(K),BARINCFSPBIG(K)
           end if
     
            else if (ibtypebig(i).eq.0) then
               if (nbvvbig(k).ne.0)  WRITE(44,'(i8)')NBVVBIG(K)
            ELSE
              WRITE(44,'(i8)')NBVVBIG(K)
            END IF
         END DO
       else
         do j=1,nvellbig(i)+minus(i)
           k=k+1
         end do  
                 
       end if  
      END DO
      
      
      
      K=0     
      DO I=1,NBOU
         WRITE(44,*)NVELL(I),IBTYPE(I)
         DO J=1,NVELL(I)
            K=K+1
            
            L=NBVV(K)+NNBIG
            M=IBCONN(K)+NNBIG
            IF ((IBTYPE(I).EQ. 3) .OR. (IBTYPE(I) .EQ. 13) .OR. 
     &        (IBTYPE(I) .EQ. 23) )  THEN
               WRITE(44,'(I8,F12.3,f6.3)')L,BARLANHT(K),BARLANCFSP(K)
                        
            ELSE IF ((IBTYPE(I).EQ. 4) .OR. (IBTYPE(I) .EQ. 14) .OR. 
     &        (IBTYPE(I) .EQ. 24) )  THEN
              WRITE(44,'(2I8,f12.3,2f6.3)')L,M,BARINHT(K),
     &         BARINCFSB(K),BARINCFSP(K)
            ELSE
              write(44,'(i8)')L
            END IF
         END DO
      END DO
      
     
     
      CLOSE(44)
     

 999  CONTINUE   
      
      PAUSE
      STOP
      END PROGRAM
      
      
      
      
      
      
      
C______________________________________________________________________
C======================================================================
      FUNCTION CROSS(DS1,DS2)
C----------------------------------------------------------------------
C THIS FUNCTION RETURNS THE CROSSPRODUCT OF TWO 2D VECTORS
C----------------------------------------------------------------------
      IMPLICIT NONE
      
      REAL*8 CROSS,DS1(2),DS2(2)
      
      CROSS=(DS1(1)*DS2(2))-(DS1(2)*DS2(1))
      
      RETURN
      END FUNCTION
C======================================================================

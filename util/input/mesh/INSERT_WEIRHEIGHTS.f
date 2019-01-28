C----------------------------------------------------------------------
C
C INSERT_WEIRHEIGHTS.f
C this program takes an ADCIRC grid file and two xyz files for input 
C (one that has higher priority than the other, you can just use a 
C dummy file for the lower priority if you want).  Then for each 
C weirheight in the grid, if the weirheight is less than -99999 
C (-989898 for IBTYPE 23), it replaces the weirheight value with the z
C value of the closest point from the xyz file. The closest value must
C be closer than the two weir nodes are together for it to be replaced. 
C It also works with the IBTYPE 23 boundaries.
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
C 10-5-11 EDITED TO ALSO INSERT BARLANHT FOR TYPE 23 BOUNDARIES
C======================================================================
      PROGRAM MAIN
C----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER I,J,K,NE,NN,NP_1,NOPE,NETA,NVDLL,IBTYPEE,N,
     & NBOU,NVEL,N1,N2,L,M,NVELL,IBTYPE,maxnid,IFOUND,NP_2,KNT
      INTEGER, ALLOCATABLE :: NID(:),indx(:),NOC(:,:)
      REAL*8  XC,YC,WH,SUB,SUP,DIFF,DIFF2,ds
      REAL*8, ALLOCATABLE :: XYZGRID(:,:),XYZWEIR_1(:,:),
     & XYZWEIR_2(:,:)
      CHARACTER*80 FILE1_1,FILE1_2,FILE2,LINE1
C----------------------------------------------------------------------
      WRITE(*,*)'NAME OF WEIRHEIGHT XYZ FILE? HIGHER PRIORITY'
      READ(*,*)FILE1_1
      
      WRITE(*,*)'NAME OF WEIRHEIGHT XYZ FILE? LOWER PRIORITY'
      READ(*,*)FILE1_2
      
      WRITE(*,*)'NAME OF GRID FILE?'
      READ(*,*)FILE2
      
      write(*,*)'name of new grid file with weirheights?'
      read(*,*)line1
      
      OPEN(11,FILE=FILE1_1)
      OPEN(12,FILE=FILE1_2)
      OPEN(14,FILE=FILE2)
      OPEN(24,FILE=line1)

C . . COUNT THE NUMBER OF XYZ POINTS            -HIGHER PRIORITY
      READ(11,*)
      NP_1=0
      DO WHILE(.TRUE.)
         READ(11,*,END=10)
         NP_1=NP_1+1
      END DO  
 10   CLOSE(11)
      OPEN(11,FILE=FILE1_1)
      
      WRITE(*,*)
      WRITE(*,*)'THERE ARE ',np_1,'WEIR POINTS'
      WRITE(*,*)    
      
C . . ALLOCATE ARRAY AND READ XYZ POINTS            
      ALLOCATE ( XYZWEIR_1(3,np_1) )
      
      READ(11,*)
      DO I=1,NP_1
         READ(11,*)(XYZWEIR_1(K,I),K=1,3)
      END DO
      CLOSE(11)
      
      
      
CCCCCCCCCCCCCCCCCCCCCCCC      
      
      
      

C . . COUNT THE NUMBER OF XYZ POINTS            -LOWER PRIORITY
      READ(12,*)
      NP_2=0
      DO WHILE(.TRUE.)
         READ(12,*,END=15)
         NP_2=NP_2+1
      END DO  
 15   CLOSE(12)
      OPEN(12,FILE=FILE1_2)
      
      WRITE(*,*)
      WRITE(*,*)'THERE ARE ',np_2,'WEIR POINTS'
      WRITE(*,*)       
      
C . . ALLOCATE ARRAY AND READ XYZ POINTS            
      ALLOCATE ( XYZWEIR_2(3,np_2) )
      
      READ(12,*)
      DO I=1,NP_2
         READ(12,*)(XYZWEIR_2(K,I),K=1,3)
      END DO
      CLOSE(12)    
      
      



C . . BEGIN READING GRID AND WRITING NEW GRID
      READ(14,'(a80)')LINE1
      WRITE(24,'(a)')TRIM(LINE1)
      
      READ(14,*)NE,NN
      WRITE(24,*)NE,NN
      ALLOCATE( XYZGRID(3,NN),NID(NN),NOC(NE,5) )
      
      maxnid=0
      DO I=1,NN
         READ(14,*)NID(I),(XYZGRID(K,I),K=1,3)
         
C REVERSE THE SIGN OF Z BECAUSE SMS DOES STUPID THINGS !
c         XYZGRID(3,I)=XYZGRID(3,I)*(-1.D0)        SMS must have fixed this with 10      
         
         WRITE(24,'(i12,3e20.12)')NID(I),(XYZGRID(K,I),K=1,3)
         maxnid=max(maxnid,nid(i))
      END DO
      
      write(*,*)'maxnid=',maxnid,' nn=',nn
      
      allocate ( indx(maxnid) )
      do i=1,nn
         indx(nid(i))=i
      end do
      
      
      DO I=1,NE
         READ(14,*)(NOC(I,K),K=1,5)
         WRITE(24,'(5i12)')(NOC(I,K),K=1,5)
      END DO

C . . NOW READ BOUNDARY INFORMATION 
      READ(14,'(A80)')LINE1   
      WRITE(24,'(a)')trim(LINE1)
      READ(LINE1,*)NOPE   
      READ(14,'(A80)')LINE1  
      WRITE(24,'(a)')trim(LINE1)
      READ(LINE1,*)NETA
      DO I=1,NOPE
         READ(14,'(A80)')LINE1
         WRITE(24,'(a)')trim(LINE1)
         READ(LINE1,*)NVDLL
         DO J=1,NVDLL
            READ(14,*)N
            WRITE(24,'(i12)')N
         END DO
      END DO
      
      READ(14,'(A80)')LINE1   
      WRITE(24,'(a)')trim(LINE1)
      READ(LINE1,*)NBOU   
      READ(14,'(A80)')LINE1  
      WRITE(24,'(a)')trim(LINE1)
      READ(LINE1,*)NVEL
      DO I=1,NBOU
         READ(14,'(A80)')LINE1
         WRITE(24,'(a)')trim(LINE1)
         READ(LINE1,*)NVELL,IBTYPE
C----------------------------------------------------------------------
C HERE'S WHERE YOU DEAL WITH THE WEIRHEIGHTS   
         OPEN(67,FILE='FOUNDPOINTS_WIERS.XYZ')


         IF (IBTYPE.EQ.24 ) THEN
         
            IBTYPE=24
            DO J=1,NVELL
               READ(14,*)N1,N2,WH,SUB,SUP
cc               write(*,*)'n1,n2,...',N1,N2,WH
cc               pause
c uncomment this if you want to replace all weir heights               WH=-99999.D0         

               IF (WH .LT. -99998.d0) THEN
               
                XC=(XYZGRID(1,indx(N1))+XYZGRID(1,indx(N2)))/2
                YC=(XYZGRID(2,indx(N1))+XYZGRID(2,indx(N2)))/2
               
                
                ds=( (XYZGRID(1,indx(N1))-XYZGRID(1,indx(N2)))**2
     &        +(XYZGRID(2,indx(N1))-XYZGRID(2,indx(N2)))**2 )**0.5d0  
                
                

c                                     the higher priority                      
               DIFF=9.D20
                IFOUND=0
                DO L=1,NP_1
                 DIFF2=DIFF
                 DIFF=MIN(DIFF2,((XC-XYZWEIR_1(1,L))**2 +
     &                            (YC-XYZWEIR_1(2,L))**2)**0.5D0  )
                 IF (DIFF.LT.DIFF2 .and. diff.le.ds) then
cc                    write(*,*)'replacing ',wh,' with ',XYZWEIR(3,L)
cc                    pause
                    WH=XYZWEIR_1(3,L)
                    IFOUND=1
                 end if
                END DO
                
                
c                                     the LOWER priority  
             IF (iFOUND.NE.1) THEN         
             WRITE(*,*)'CHECKING IN LOWER PRIORITY DATA SET'    
               DIFF=9.D20
                IFOUND=0
                DO L=1,NP_2
                 DIFF2=DIFF
                 DIFF=MIN(DIFF2,((XC-XYZWEIR_2(1,L))**2 +
     &                            (YC-XYZWEIR_2(2,L))**2)**0.5D0  )
                 IF (DIFF.LT.DIFF2 .and. diff.le.ds) then
cc                    write(*,*)'replacing ',wh,' with ',XYZWEIR(3,L)
cc                    pause
                    WH=XYZWEIR_2(3,L)
                    IFOUND=1
                 end if
                END DO              
              END IF  
                
                
                
                
               END IF

               IF (IFOUND.EQ.1) WRITE(67,'(3E20.11)')XC,YC,WH     
                  
               WRITE(24,'(2i12,f12.3,2f6.3)')N1,N2,WH,SUB,SUP
               
            END DO
         
               
            ELSE IF (IBTYPE.EQ.23 ) THEN

            DO J=1,NVELL
               READ(14,*)N1,WH,SUP
cc               write(*,*)'n1,n2,...',N1,N2,WH
cc               pause
c uncomment this if you want to replace all weir heights      WH=-989898.D0         

               IF (WH .LT. -989897.d0) THEN
               
               XC=XYZGRID(1,indx(N1))
               YC=XYZGRID(2,indx(N1))
               
C              DETERMINE DS BASED ON DISTANCE TO A NEIGHBORING NODE
               DS=0
               KNT=0
               DO L=1,NE               
                  DO K=3,5
                     IF (N1.EQ.NOC(L,K)) THEN
                        IF (K.EQ.3) THEN
                     DS= 0.5D0 * ( (XC-XYZGRID(1,INDX(NOC(L,4))))**2D0
     &                + (yC-XYZGRID(2,INDX(NOC(L,4))))**2D0 ) **0.5D0
                          GOTO 312
                        END IF
                        IF (K.EQ.4) THEN
                     DS= 0.5D0 * ( (XC-XYZGRID(1,INDX(NOC(L,5))))**2D0
     &                + (yC-XYZGRID(2,INDX(NOC(L,5))))**2D0 ) **0.5D0
                          GOTO 312
                        END IF
                        IF (K.EQ.5) THEN
                     DS= 0.5D0 * ( (XC-XYZGRID(1,INDX(NOC(L,3))))**2D0
     &                + (yC-XYZGRID(2,INDX(NOC(L,3))))**2D0 ) **0.5D0
                          GOTO 312
                        END IF
                     END IF
                  END DO
               END DO
 312           CONTINUE               
                
c                                     the higher priority                      
               DIFF=9.D20
               IFOUND=0
                DO L=1,NP_1
                 DIFF2=DIFF
                 DIFF=MIN(DIFF2,((XC-XYZWEIR_1(1,L))**2 +
     &                            (YC-XYZWEIR_1(2,L))**2)**0.5D0  )
                 IF (DIFF.LT.DIFF2 .and. diff.le.ds) then
                    WH=XYZWEIR_1(3,L)
                    IFOUND=1
                 end if
                END DO
     
               IF (IFOUND.NE.1) THEN
                DO L=1,NP_2
                 DIFF2=DIFF
                 DIFF=MIN(DIFF2,((XC-XYZWEIR_2(1,L))**2 +
     &                            (YC-XYZWEIR_2(2,L))**2)**0.5D0  )
                 IF (DIFF.LT.DIFF2 .and. diff.le.ds) then
                    WH=XYZWEIR_2(3,L)
                    IFOUND=1
                 end if
                END DO              
               END IF  
                
                
                
                
               END IF

               IF (IFOUND.EQ.1) WRITE(67,'(3E20.11)')XC,YC,WH     
                  
               WRITE(24,'(i12,f12.3,f6.3)')N1,WH,SUP
                
            END DO
            
            
            
            
         ELSE
            DO J=1,NVELL
               READ(14,'(a80)')LINE1
               WRITE(24,'(a)')trim(LINE1)
            END DO
         END IF
C----------------------------------------------------------------------         
      END DO
      CLOSE(14)
      CLOSE(24)
      CLOSE(67)
      
      STOP
      END PROGRAM
      
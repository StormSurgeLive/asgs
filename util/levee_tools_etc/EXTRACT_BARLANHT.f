C      PROGRAM EXTRACT_BARLANHT
C Reads fort.14 and MAKES A XYZ FILE FOR ITYPE 24 BOUNDARY WEIRHEIGHTS
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
C 10-5-11 EDITED TO ALSO WRITE FILE FOR TYPE 23 BOUNDARIES
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

      WRITE(*,*)'NAME OF GRID FILE?'
      READ(*,*)FILE2
      
      OPEN(14,FILE=FILE2)
      OPEN(24,FILE='BARLANHT.XYZ')
      OPEN(34,FILE='WEIRHEIGHT.XYZ')



C . . BEGIN READING GRID AND WRITING NEW GRID
      READ(14,'(a80)')LINE1

      
      READ(14,*)NE,NN
      ALLOCATE( XYZGRID(3,NN),NID(NN),NOC(NE,5) )
      
      maxnid=0
      DO I=1,NN
         READ(14,*)NID(I),(XYZGRID(K,I),K=1,3)
         maxnid=max(maxnid,nid(i))
      END DO
      
      write(*,*)'maxnid=',maxnid,' nn=',nn
      
      allocate ( indx(maxnid) )
      do i=1,nn
         indx(nid(i))=i
      end do
      
      
      DO I=1,NE
         READ(14,*)(NOC(I,K),K=1,5)
      END DO

C . . NOW READ BOUNDARY INFORMATION 
      READ(14,'(A80)')LINE1   
      READ(LINE1,*)NOPE   
      READ(14,'(A80)')LINE1  
      READ(LINE1,*)NETA
      DO I=1,NOPE
         READ(14,'(A80)')LINE1
         READ(LINE1,*)NVDLL
         DO J=1,NVDLL
            READ(14,*)N
         END DO
      END DO
      
      READ(14,'(A80)')LINE1   
      READ(LINE1,*)NBOU   
      READ(14,'(A80)')LINE1  
      READ(LINE1,*)NVEL
      DO I=1,NBOU
         READ(14,'(A80)')LINE1
         READ(LINE1,*)NVELL,IBTYPE
C----------------------------------------------------------------------
C HERE'S WHERE YOU DEAL WITH THE BARLANHTS

         IF (IBTYPE.EQ.24 ) THEN
            DO J=1,NVELL
               READ(14,*)N1,N2,WH,SUB,SUP
               XC=(XYZGRID(1,indx(N1))+XYZGRID(1,indx(N2)))/2
               YC=(XYZGRID(2,indx(N1))+XYZGRID(2,indx(N2)))/2
                WRITE(34,'(3E20.11)')XC,YC,WH     
            END DO
         
               
        ELSE IF (IBTYPE.EQ.23 ) THEN

            DO J=1,NVELL
               READ(14,*)N1,WH,SUP
               XC=XYZGRID(1,indx(N1))
               YC=XYZGRID(2,indx(N1))
               WRITE(24,'(3E20.11)')XC,YC,WH     
            END DO
 
         ELSE
            DO J=1,NVELL
               READ(14,'(a80)')LINE1
            END DO
         END IF
C----------------------------------------------------------------------         
      END DO
      CLOSE(14)
      CLOSE(24)
      CLOSE(34)
      
      STOP
      END PROGRAM
      
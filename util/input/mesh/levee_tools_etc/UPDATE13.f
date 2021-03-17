C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C      PROGRAM UPDATE13
C THIS PROGRAM WILL READ AN ADCIRC FORT.13 FILE AND ALLOW YOU TO UPDATE
C THE NODAL ATTRIBUTE INFORMATION WITH NODAL ATTRIBUTE VALUES SUPPLIED 
C IN PLACE OF ELEVATION VALUES IN A FORT.14 TYPE FILE.
C
C IT ALSO WILL WRITE OUT A SERIES OF *.63 FILES CONTAINING THE ATTRIBUTE
C DATA THAT IS IN THE FORT.13 FILE.  
C
C----------------------------------------------------------------------
C Copyright(C) 2008,2012  Nathan Dill
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
C---------------------------------------------------------------------
      PROGRAM UPDATE13
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IMPLICIT NONE
      INTEGER       I,J,K,L,NN,NATTR,NATTRPERN(10),NOTDEFAULT,CNT,
     & UP(10),OUT(10),NID
      INTEGER, ALLOCATABLE :: NDF(:)
      REAL*8   DEFAULT(10,12),X,Y,Z,ZZ(12),D1
      REAL*8,  ALLOCATABLE :: ATTRDATA(:,:)
      CHARACTER*80  TMPLINE, ATTRNAME(10),UNITS(10),TMPFILE
C......................................................................
      write(*,*)'name for original fort.13 file?'
      read(*,*)tmpfile
      OPEN(13,FILE=tmpfile)
      
      write(*,*)'name for new fort.13 file?'
      read(*,*)tmpline
      OPEN(23,FILE=tmpline)
      
      READ(13,1313,END=99)TMPLINE
      WRITE(23,1313)TMPLINE
      WRITE(*,*)
      WRITE(*,*)'READING ',TRIM(TMPLINE)
      WRITE(*,*)
      READ(13,*)NN
      WRITE(23,*)NN
      READ(13,*)NATTR
      WRITE(23,*)NATTR
      DO I=1,NATTR
         READ(13,1313)ATTRNAME(I)
         READ(13,1313)UNITS(I)
         READ(13,*)NATTRPERN(I)
         READ(13,*)(DEFAULT(I,J),J=1,NATTRPERN(I))
c 1-14-08 fixed b.c you cannot have blanks before of after attrname          
         WRITE(23,'(a)')trim(ATTRNAME(I))
         WRITE(23,'(a)')trim(UNITS(I))
         WRITE(23,*)NATTRPERN(I)
         WRITE(23,'(12f5.2)')(DEFAULT(I,J),J=1,NATTRPERN(I))
      END DO
      
      ALLOCATE( ATTRDATA(NN,12),NDF(NN) )
      
      
      WRITE(*,'(A,I3,A)')' THIS FORT.13 FILE HAS',NATTR,
     &                                       ' NODAL ATTRIBUTES IN IT'
      WRITE(*,*)
      DO I=1,NATTR
         WRITE(*,1314)I,') ',TRIM(ATTRNAME(I))
      END DO
      WRITE(*,*)
      
      DO I=1,10
        UP(I)=0
        OUT(I)=0
      END DO
      
      DO I=1,NATTR
         WRITE(*,1315)' DO YOU WANT TO CHANGE "',TRIM(ATTRNAME(I)),
     &    '" (YES=1,NO=0)?'
         READ(*,*)UP(I)
      END DO
   
      
CCCCCC JUST DO THIS AUTOMATICALLY        
CC      WRITE(*,*)
CC      DO I=1,NATTR
CC         WRITE(*,1315)' WRITE OUT *.63 TYPE FILE FOR "',
CC     &                          TRIM(ATTRNAME(I)),'" (YES=1,NO=0)?'
CC         READ(*,*)OUT(I)
CC      END DO
      
      
      DO I=1,NATTR
C . . . .INITALIZE ATTRDATA WITH DEFAULT VALUE
         DO J=1,NN
            DO K=1,NATTRPERN(I)
               ATTRDATA(J,K)=DEFAULT(I,K)
            END DO
         END DO
      
C . . . .READ THIS ATTRIBUTE          
         READ(13,1313)ATTRNAME(I)
         READ(13,*)NOTDEFAULT
         DO J=1,NOTDEFAULT
            READ(13,*)NID,(ATTRDATA(NID,K),K=1,NATTRPERN(I))
         END DO
         
C . . . .WRITE OUT *.63 FILE 
         WRITE(TMPFILE,*)TRIM(ATTRNAME(I))//'.63'
         WRITE(*,1315)'WRITING ',TRIM(TMPFILE)   
         OPEN(63,FILE=TMPFILE)      
         write(63,'(A)')ATTRNAME(I)
         IF(NATTRPERN(I).EQ.1) THEN
            J=2
         ELSE
            J=NATTRPERN(I)
         END if
         write(63,*)J,nn,' 10 10 1'
         DO K=1,J
            D1=K*10
            WRITE(63,*)D1,D1
            DO L=1,NN
               WRITE(63,*)L,ATTRDATA(L,K)
            END DO
         END DO
         CLOSE(63)
         
        
         
         IF (UP(I).EQ. 1) THEN           
C . . . . . READ ATTRIBUTE DATA FROM GRID FILE            
            WRITE(*,*)
            WRITE(*,1315)'FILENAME FOR "',TRIM(ATTRNAME(I)),
     &                                      '" GRID TYPE FILE?'
            READ(*,*)TMPLINE
            OPEN(11,FILE=TMPLINE)
            READ(11,*)
            READ(11,*)K,NN
               DO L=1,NN
                 READ(11,*)NID,X,Y,
     &                 (ATTRDATA(NID,K),K=1,NATTRPERN(I))
               END DO
            CLOSE(11)
         END IF
         
C . . . . . COUNT THE NUMBER OF NON DEFAULT VALUES
         CNT=0
         DO J=1,NN
            NDF(J)=0
            DO K=1,NATTRPERN(I)
               IF (ATTRDATA(J,K).NE.DEFAULT(I,K)) THEN
                  NDF(J)=1
                  CNT=CNT+1
                  GOTO 98
               END IF  
            END DO
 98         CONTINUE
         END DO
            
C . . . .NOW WRITE OUT THE NON DEFAULT VALUES IN THE NEWFORT.13            
         WRITE(23,'(a)')trim(ATTRNAME(I))
         WRITE(23,*)CNT
         DO J=1,NN
            IF (NDF(J).EQ.1) THEN
               WRITE(23,1316)J,(ATTRDATA(J,K),K=1,NATTRPERN(I))
            END IF
            
         END DO

C. . . LOOP OVER ATTRIBUTES
      END DO
               
  
      
      
            
      
      GOTO 100
 99   WRITE(*,*)"!! OOPS - WHERE'S THE FORT.13? !!"      
 100  CONTINUE
      CLOSE(13)
      CLOSE(23)
 1313 FORMAT(A80)    
 1314 FORMAT(I2,A2,A) 
 1315 FORMAT(3a)
 1316 FORMAT(I8,12E16.8)
      STOP
      END PROGRAM UPDATE13
      
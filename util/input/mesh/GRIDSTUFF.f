C----------------------------------------------------------------------
C
C GRIDSTUFF.f module; This module contains some subroutines and 
C variablesfor use with reading, analyzing, modifying ADCIRC grid files
C
C----------------------------------------------------------------------
C
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
C====================================================================
      MODULE  GRIDSTUFF
C____________________________________________________________________
      IMPLICIT NONE
       
      INTEGER NE,NN,NOPE,NETA,NBOU,NVEL,MAXNID
      
      INTEGER, ALLOCATABLE :: NOC(:,:),NID(:),EID(:),NVDLL(:),
     &  NBDV(:),IBTYPE(:),NBVV(:),NVELL(:),IBCONN(:)
      
      REAL*8, ALLOCATABLE :: X(:),Y(:),Z(:),BARLANHT(:),
     & BARLANCFSP(:),BARINHT(:),BARINCFSB(:),BARINCFSP(:) 
      
      CHARACTER*60 AGRID
      
CC   CLIP VARIABLES      
      INTEGER CLIP_NN,CLIP_NE
      INTEGER, ALLOCATABLE :: CLIP_NOC(:,:),IFOUNDN(:),IFOUNDE(:),
     & CLIP_NID(:),CLIP_EID(:)
      REAL*8, ALLOCATABLE  :: CLPX(:),CLPY(:),CLPZ(:)
      
      
      
      CONTAINS
C====================================================================
      
      SUBROUTINE READ14(FNAME)
C--------------------------------------------------------------------
C THIS SUBROUTINE READS IN ALL THE GRID FILE DATA.  
C--------------------------------------------------------------------
      
      CHARACTER*80  FNAME
      INTEGER I,J,K,l
   
      
      OPEN(14,FILE=FNAME)
      READ(14,'(A60)')AGRID
      READ(14,*)NE,NN
      
      MAXNID=0
      DO I=1,NN
         READ(14,*)J
         MAXNID=MAX(MAXNID,J)
      END DO
      
      CLOSE(14)
      OPEN(14,FILE=FNAME)
      READ(14,'(A60)')AGRID
      READ(14,*)NE,NN
      
      write(*,*)'allocating noc by ', ne
      
      IF (ALLOCATED(NOC)) then
        DEALLOCATE(NOC,NID,X,Y,Z,EID,NVDLL,NBDV,NVELL,NBVV,BARLANHT,
     & BARLANCFSP,IBCONN,BARINHT,BARINCFSB,BARINCFSP,IBTYPE)
      END IF
      
      ALLOCATE( NOC(3,NE),NID(MAXNID),X(MAXNID),Y(MAXNID),
     &  Z(MAXNID),EID(NE) )
      
      do i=1,maxnid
         nid(i)=0
         x(i)=0.d0
         y(i)=0.d0
         z(i)=-99999d0
      end do
     
      WRITE(*,222)trim(AGRID),' HAS ',NE,' ELEMENTS AND ',NN,' NODES'
      WRITE(*,*)'READING NODES'
      DO I=1,NN
         READ(14,*)NID(I),X(NID(I)),Y(NID(I)),Z(NID(I))         
      END DO
      
 222  format(2A,I10,A,I10,A)     
      
      IF (MAXNID .NE. NID(NN)) THEN
         WRITE(*,*)'NODE NUMBERS NON-SQEQUENTIAL'
      END IF
      
      WRITE(*,*)'READING ELEMENTS'
      DO I=1,NE
         READ(14,*)EID(I),K,(NOC(J,I),J=1,3)
      END DO
      
      
      
      READ(14,*)NOPE
      READ(14,*)NETA
      
      WRITE(*,*)NOPE,' OPEN BNDS WITH',NETA,' NODES'
      
      ALLOCATE( NVDLL(NOPE), NBDV(NETA) )
      
      K=0
      DO I=1,NOPE
         READ(14,*) NVDLL(I) 
         DO J=1,NVDLL(I)
            K=K+1
            READ(14,*)NBDV(K)
         END DO
      END DO
      
      
      READ(14,*)NBOU
      READ(14,*)NVEL
     
        
      WRITE(*,*)NBOU,'NORMAL FLOW BNDS WITH',NVEL,' NODES'
      ALLOCATE( NVELL(NBOU),NBVV(NVEL),BARLANHT(NVEL),
     & BARLANCFSP(NVEL),IBCONN(NVEL),BARINHT(NVEL),
     & BARINCFSB(NVEL),BARINCFSP(NVEL),IBTYPE(NBOU) )
     
      K=0     
      DO I=1,NBOU
         READ(14,*)NVELL(I),IBTYPE(I)
         DO J=1,NVELL(I)
            K=K+1
            IF ((IBTYPE(I).EQ. 3) .OR. (IBTYPE(I) .EQ. 13) .OR. 
     &        (IBTYPE(I) .EQ. 23) )  THEN
               READ(14,*)NBVV(K),BARLANHT(K),BARLANCFSP(K)
                        
            ELSE IF ((IBTYPE(I).EQ. 4) .OR. (IBTYPE(I) .EQ. 14) .OR. 
     &        (IBTYPE(I) .EQ. 24) )  THEN
              READ(14,*)NBVV(K),IBCONN(K),BARINHT(K),
     &         BARINCFSB(K),BARINCFSP(K)
            ELSE
              READ(14,*)NBVV(K)
            END IF
         END DO
      END DO
      CLOSE(14)
      
      RETURN
      
      
      END SUBROUTINE READ14
c====================================================================


c____________________________________________________________________
      SUBROUTINE CLIPGRID(XLIM,YLIM)
C THIS SUBROUTINE CLIPS THE GRID TO A SMALLER DOMAIN USING THE 
C BOUNDARIES DEFINED BY XLIM AND YLIM (E.G. XLIM=(XMIN,XMAX))
      
      INTEGER I,J,K,l
      REAL*8  XLIM(2),YLIM(2)
      
      ! write(*,*)'DEBUG: deallocating'
      IF (ALLOCATED(IFOUNDE))   DEALLOCATE(IFOUNDE,IFOUNDN,CLIP_NOC,
     &                             CLPX,CLPY,CLPZ,CLIP_NID,CLIP_EID )


      ALLOCATE(IFOUNDN(MAXNID), IFOUNDE(NE))
      
      CLIP_NN=0
      CLIP_NE=0
      
      ! write(*,*)'DEBUG: finding nodes'
      DO I=1,NN
         IFOUNDN(NID(I))=0
         
        IF ( (X(NID(I)) .GE. XLIM(1)) .AND. (X(NID(I)) .LE. XLIM(2))
     &    .AND. (Y(NID(I)).GE. YLIM(1)).AND.(Y(NID(I)).LE. YLIM(2)) ) 
     &     THEN
            IFOUNDN(NID(I))=1
            CLIP_NN=CLIP_NN+1
            
         END IF
      END DO
      
      ! write(*,*) 'DEBUG: finding elements'
      DO I=1,NE
c      write(*,*)'element', i,' of',ne
         IFOUNDE(I)=0
         K=0
c         if (i.gt.13990) then
c            write(*,*)( noc(l,i),l=1,3 )
c         end if
         DO J=1,3
c            write(*,*)'j=',j
            K=K+IFOUNDN(NOC(J,I))
         END DO
c         write(*,*)'k=',k
         IF (K.EQ.3) THEN
            IFOUNDE(I)=1
            CLIP_NE=CLIP_NE+1
         ELSE IF (K.EQ.1 .OR. K.EQ.2) THEN
            IFOUNDE(I)=-1
            CLIP_NE=CLIP_NE+1
         END IF
      END DO
            
C UPDATE NODES THAT ARE OUTSIDE BUT ARE PART OF AN ELEMENT THAT IS IN
      DO I=1,NE
         IF (IFOUNDE(I).EQ.-1)  THEN 
            IFOUNDE(I)=1
            DO J=1,3
               IF (IFOUNDN(NOC(J,I)).EQ.0) THEN
                  IFOUNDN(NOC(J,I))=1
                  CLIP_NN=CLIP_NN+1
               END IF
            END DO
         END IF
      END DO
      
      ! write(*,*)'DEBUG: allocating noc,xyz,nid'
      ALLOCATE( CLIP_NID(CLIP_NN),CLPX(CLIP_NN),CLPY(CLIP_NN),
     &  CLPZ(CLIP_NN),CLIP_NOC(3,CLIP_NE),CLIP_EID(CLIP_NE) )
      
      K=0
      DO I=1,NN
         IF (IFOUNDN(NID(I)).EQ.1) THEN
           K=K+1
           CLIP_NID(K)=NID(I)
           CLPX(K)=X(NID(I))
           CLPY(K)=Y(NID(I))
           CLPZ(K)=Z(NID(I))
         END IF
      END DO
      
      K=0
      DO I=1,NE
        IF (IFOUNDE(I).EQ.1) THEN
          K=K+1
          CLIP_EID(K)=I
          DO J=1,3
            CLIP_NOC(J,K)=NOC(J,I)
          END DO
        END IF
      END DO
      
         
      
      RETURN

      END SUBROUTINE CLIPGRID
C---------------------------------------------------

C____________________________________________________________________
      SUBROUTINE GRIDWEIRS
C--------------------------------------------------------------------
C THIS SUBROUTINE WILL CREATE NODES IN THE CENTER OF WEIR PAIRS USING
C WEIR HEIGHT FOR NODAL ELEVATION(DEPTH) 
C THEN IT WILL MESH THE WEIR AREA WITH ELEMENTS      
C--------------------------------------------------------------------
      INTEGER I,J,K,L,WNN,WNE,II
      
      INTEGER, ALLOCATABLE :: WNID(:,:)
      REAL*8, ALLOCATABLE :: WHT(:)
      CHARACTER*80 STR
      
      real*8 xtmp,ytmp
      
      OPEN(44,FILE='WEIRGRID.2DM')
      WRITE(44,'(A6)')'MESH2D'
      open(45,file='weirheight.xyz')

      
C COUNT UP THE NUMBER OF WEIR PAIRS     
      K=0 
      DO I=1,NBOU
         IF (IBTYPE(I).EQ.4 .OR. IBTYPE(I).EQ.24 .OR. 
     &                                    IBTYPE(I).EQ.14 ) THEN
            K=K+1
            WNN=WNN+NVELL(I)
         END IF
      END DO
      

      ALLOCATE  ( WNID(2,WNN), WHT(WNN) )
C      WNN=WNN*2
      WRITE(*,*)'WNN;',WNN
      WNN=WNN*2
      
C GET THE WEIR NODE NUMBERS AND HEIGHTS      
      K=0
      L=0
      DO I=1,NBOU
        IF (IBTYPE(I).EQ.4 .OR. IBTYPE(I).EQ.24 .OR. 
     &                                    IBTYPE(I).EQ.14 ) THEN
           DO J=1,NVELL(I)
              K=K+1
              L=L+1
              WNID(1,L)=NBVV(K)
              WNID(2,L)=IBCONN(K)
              WHT(L)=BARINHT(K)
           END DO
        ELSE
           DO J=1,NVELL(I)
              K=K+1
           END DO
        END IF
      END DO
      
C CHECK FOR DUPLICATE NODE NUMBERS MARK THEM WITH A NEGATIVE
CC      DO L=1,WNN
CC        DO K=L+1,WNN
CC          IF (WNID(1,L).EQ.WNID(1,K)) THEN
CC           WRITE(*,*)'DUPLICATE NODE A',WNID(1,L)
CC            WNID(1,K)=-999
CC          END IF
CC        END DO
CC      END DO
CC     
CC      DO L=1,WNN
CC        DO K=L+1,WNN
CC          IF (WNID(2,L).EQ.WNID(2,K)) THEN
CC            WRITE(*,*)'DUPLICATE NODE B',WNID(2,L)
CC            WNID(2,K)=-999
CC          END IF
CC        END DO
CC      END DO
      
CC      DO L=1,WNN
CC        DO K=1,WNN
CC          IF (WNID(1,L).EQ.WNID(2,K)) THEN
CC            WRITE(*,*)'DUPLICATE NODE B',WNID(2,L)
CC            WNID(2,K)=-999
CC          END IF
CC        END DO
CC      END DO
          
      
      
      
      
 
C WRITE THE ELEMENTS IN 2DM FORMAT        
      WNE=0   
      L=0
      DO I=1,NBOU
        IF (IBTYPE(I).EQ.4 .OR. IBTYPE(I).EQ.24 .OR. 
     &                                    IBTYPE(I).EQ.14 ) THEN
         DO J=1,NVELL(I)-1
           L=L+1
           WRITE(44,225)L,WNID(2,L),WNID(1,L),WNID(1,L+1),WNID(2,L+1)
           WNE=WNE+2
         END DO
         l=l+1
        END IF
      END DO
 225  FORMAT('E4Q ',5I12,'  1')     
      
      
C WRITE THE NODES IN 2DM FORMAT      
      L=0
      DO I=1,NBOU
        IF (IBTYPE(I).EQ.4 .OR. IBTYPE(I).EQ.24 .OR. 
     &                                    IBTYPE(I).EQ.14 ) THEN
         DO J=1,NVELL(I)
           L=L+1
CC           IF (WNID(1,L) .NE. -999) THEN
              WRITE(44,226)WNID(1,L),X(WNID(1,L)),Y(WNID(1,L)),WHT(L)
CC           END IF
CC           IF (WNID(2,L).NE.-999) THEN 
              WRITE(44,226)WNID(2,L),X(WNID(2,L)),Y(WNID(2,L)),WHT(L) 
CC           END IF

             xtmp=(X(WNID(1,L))+X(WNID(2,L)))/2.d0
             ytmp=(Y(WNID(1,L))+Y(WNID(2,L)))/2.d0
             
             write(45,'(3f18.4)')xtmp,ytmp,wht(l)
             

         END DO
        END IF
      END DO
 226  FORMAT('ND ',I12,3E18.9)
 
      CLOSE(44)
      close(45)
         
C WRITE A GRID OF TRIANGLES IN ADCIRC STYLE

      OPEN(44,FILE='TMP.14')
      
      
      WRITE(44,'(A14,A60)')'WEIRGRID FROM ',AGRID
C      WNE=WNE*2
      WRITE(44,*)WNE,WNN
C WRITE NODES      
      L=0
      DO I=1,NBOU
        IF (IBTYPE(I).EQ.4 .OR. IBTYPE(I).EQ.24 .OR. 
     &                                    IBTYPE(I).EQ.14 ) THEN
         DO J=1,NVELL(I)
           L=L+1
CC           IF (WNID(1,L) .NE. -999) THEN
              WRITE(44,227)WNID(1,L),X(WNID(1,L)),Y(WNID(1,L)),WHT(L)
CC           END IF
CC           IF (WNID(2,L).NE.-999) THEN 
              WRITE(44,227)WNID(2,L),X(WNID(2,L)),Y(WNID(2,L)),WHT(L) 
CC          END IF 
         END DO
        END IF
      END DO
 227  FORMAT(I12,3E18.9)
 
C WRITE ELEMENTS
      L=0
      k=0
      DO I=1,NBOU
        IF (IBTYPE(I).EQ.4 .OR. IBTYPE(I).EQ.24 .OR. 
     &                                    IBTYPE(I).EQ.14 ) THEN
         DO J=1,NVELL(I)-1
           L=L+1
           k=k+1
           WRITE(44,228)k,WNID(2,L),WNID(1,L),WNID(1,L+1)
           k=k+1
           WRITE(44,228)k,WNID(2,L),WNID(1,L+1),WNID(2,L+1)
         END DO
         l=l+1
        END IF
      END DO
 228  FORMAT(I12,' 3 ',3I12)      
      WRITE(44,*)'0'
      WRITE(44,*)'0'
      CLOSE(44)
      WRITE(*,*)'WNN;2',WNN
C REMOVE THE DUPLICATE NODES      
      DEALLOCATE(WNID)
      DEALLOCATE(WHT)
      OPEN(13,FILE='TMP.14')
      READ(13,*)
      READ(13,*)WNE,WNN
            WRITE(*,*)'WNN;3',WNN
      K=0
      DO I=1,WNN
         READ(13,*)J
         K=MAX(J,K)
      END DO
      CLOSE(13)
      ALLOCATE(WNID(1,K))
      DO I=1,K
        WNID(1,K)=0
      END DO
      
      OPEN(13,FILE='TMP.14')
      READ(13,*)
      READ(13,*)WNE,WNN
      DO I=1,WNN
         READ(13,*)J
         WNID(1,J)=J
      END DO
      CLOSE(13)
      
      WNN=0
      DO I=1,K
         IF (WNID(1,I) .NE. 0)  WNN=WNN+1
      END DO
         
      OPEN(13,FILE='TMP.14')
      OPEN(14,FILE='WEIRGRID.14')
      
      READ(13,'(A80)')STR
      WRITE(14,'(A80)')STR
      
      READ(13,*)WNE,L
      WRITE(14,*)WNE,WNN

      DO I=1,L
         READ(13,'(A80)')STR
         READ(STR,*)J
         IF (WNID(1,J).NE.0) THEN
            WRITE(14,'(A80)')STR
            WNID(1,J)=0
         END IF
      END DO
      DO I=1,WNE
         READ(13,'(A80)')STR
         WRITE(14,'(A80)')STR
      END DO
      WRITE(14,*)'0'
      WRITE(14,*)'0'
      CLOSE(14)
      CLOSE(13)
      
      
            
      
            
      END SUBROUTINE GRIDWEIRS
C____________________________________________________________________
C====================================================================


C--------------------------------------------------------------------

      
      END MODULE GRIDSTUFF
C____________________________________________________________________
C====================================================================






C******************************************************************************
C                                                                             *
C    Transform from lon,lat (lamda,phi) coordinates into CPP coordinates.     *
C    Lon,Lat must be in radians.                                              *
C                                                                             *
C******************************************************************************

C      SUBROUTINE CPP(X,Y,RLAMBDA,PHI,RLAMBDA0,PHI0)
C      IMPLICIT NONE
C      REAL*8 X,Y,RLAMBDA,PHI,RLAMBDA0,PHI0,R
C      R=6378206.4d0
C      X=R*(RLAMBDA-RLAMBDA0)*COS(PHI0)
C      Y=PHI*R
C      RETURN
C      END


C******************************************************************************
C                                                                             *
C    Transform from CPP coordinates to lon,lat (lamda,phi) coordinates        *
C    Lon,Lat is in radians.                                                   *
C                                                                             *
C******************************************************************************

C      SUBROUTINE INVCP(XXCP,YYCP,RLAMBDA,PHI,RLAMBDA0,PHI0)
C      IMPLICIT NONE
C      REAL*8 XXCP,YYCP,RLAMBDA,PHI,RLAMBDA0,PHI0,R
C      R=6378206.4d0
C      RLAMBDA=RLAMBDA0+XXCP/(R*COS(PHI0))
C      PHI=YYCP/R
C      RETURN
C      END


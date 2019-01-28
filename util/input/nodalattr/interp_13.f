C THIS PROGRAM WILL GENERATE A newfort.13 for a new grid based on
c a fort.13 file associated with an "original grid"
C IN ADDITION TO TO THE original fort.13 FILE YOU ALSO NEED TO HAVE 
c INDX.IN (RENAMED from INDX.OUT) CREATED BY THE INDX_INTERP PROGRAM
C
C
C BY NATHAN DILL 
C WOODS HOLE GROUP INC. 01-08-08
C THIS CODE WAS IS INTENDED FOR USE ON A SPECIFIC ADCIRC 
C MODELING PROJECT. THE AUTHOR MAKES NO CLAIMS CONCERNING THE POTENTIAL 
C FOR ERRORS IN THIS CODE AND TAKES NO RESPONSIBILITY FOR PROBLEMS 
C RESULTING FROM USE OF THIS CODE OUTSIDE OF ITS INTENDED PURPOSE.  
C IF YOU FIND ERRORS AND/OR MAKE MODIFICATIONS PLEASE SHARE THEM WITH 
C MR. DILL VIA EMAIL AT ndill@whgrp.com
C====================================================================== 
C----------------------------------------------------------------------
      PROGRAM MAIN
C----------------------------------------------------------------------     
      IMPLICIT NONE
      
      INTEGER I,J,K,NN,NNORG,IS12,NEORG,NATTR,NDF,ntxnodes
      INTEGER, ALLOCATABLE :: INDX(:),N1(:),N2(:),N3(:),NATTRPERN(:),
     &   NNDF(:),ISDEF(:,:),ISTX(:)
      
      REAL*4 D1,D2
      REAL*4, ALLOCATABLE :: TT(:),UU(:),ATTRDATA(:,:,:),DEFAULT(:,:),
     & NEWDATA(:,:,:)

      CHARACTER*80 DESC,FILE12
      CHARACTER*80, ALLOCATABLE :: ATTRNAME(:),UNITS(:)
C----------------------------------------------------------------------
      WRITE(*,*)'DO YOU WANT TO USE "tf01_x2_v07.in12"',
     &          'INSTEAD OF A *.13 FILE? YES=1'
      READ(*,*)IS12 
      
      IF (IS12.EQ.1) THEN
         FILE12='tf01_x2_v07.in12'
      ELSE
         WRITE(*,*)'FILENAME OF ORIGINAL fort.13 FILE'
         READ(*,*)FILE12
      END IF
      
      OPEN(12,FILE=FILE12)
      OPEN(13,FILE='newfort.13')
      
C . . READ THE INDX AND INTERPOLATION COEFFICIENTS    
      WRITE(*,*)
      WRITE(*,*)'READING INDX.IN'
      WRITE(*,*)
      OPEN(11,FILE='INDX.IN')
      READ(11,*)NN,NNORG
      ALLOCATE ( INDX(NN),N1(NN),N2(NN),N3(NN),TT(NN),UU(NN) )
      DO I=1,NN
         READ(11,*)INDX(I),N1(I),N2(I),N3(I),TT(I),UU(I)
      END DO      
      CLOSE(11)
      
      
      
c3-8-09 for tx nodes
c      write(*,*)'reading Texas node numbers'
c      allocate(istx(nn))
c      do i=1,nn
c        istx(i)=0
c      end do
      
c      open(56,file='tx_nodes_only.14')
c      read(56,*)
c      read(56,*)i,ntxnodes
c      do i=1,ntxnodes
c        read(56,*)j
c        istx(j)=1
c      end do
c      close(56)      
      
      
      
      
C . . READ FORT.13 OR FORT.12 FILE      
      IF (IS12.EQ.1) THEN
         WRITE(*,*)
         WRITE(*,*)'READING "tf01_x2_v07.in12" FILE'
         WRITE(*,*)

         READ(12,*)
         READ(12,*)NEORG,NNORG
         NATTR=5
         ALLOCATE( ATTRNAME(NATTR),UNITS(NATTR),NATTRPERN(NATTR),
     &   DEFAULT(NATTR,12),ATTRDATA(NATTR,NNORG,12),NNDF(NATTR))
         
         DESC='ATTRIBUTE DATA INTERPOLATED FROM tf01_x2_v07.in12'
         ATTRNAME(1)='primitive_weighting_in_continuity_equation'
         ATTRNAME(2)='sea_surface_height_above_geoid'
         ATTRNAME(3)='surface_submergence_state'
         ATTRNAME(4)='surface_directional_effective_roughness_length'
         ATTRNAME(5)='surface_canopy_coefficient'
         NNDF(1)=0
         NNDF(2)=0
         NNDF(3)=NNORG
         NNDF(4)=NNORG
         NNDF(5)=NNORG
         DO I=1,5
            NATTRPERN(I)=1
            UNITS(I)='SOMEUNIT'
         END DO
         NATTRPERN(4)=12
         DEFAULT(1,1)=0.005D0
         DEFAULT(2,1)=0.36576D0
         DEFAULT(3,1)=0.D0
         DO I=1,12
           DEFAULT(4,I)=0.D0
         END DO
         DEFAULT(5,1)=1.D0
         
C . . . .INITALIZE ATTRIBUTE DATA
         DO I=1,NATTR
            DO J=1,NNORG
               DO K=1,NATTRPERN(I)
                  ATTRDATA(I,J,K)=DEFAULT(I,K)
               END DO
            END DO    
         END DO         
C . . . .READ ATTRIBUTE DATA FROM FORT.12 FILE         
         
         DO I=1,NNORG
           READ(12,*)J,D1,D2,ATTRDATA(3,J,1),(ATTRDATA(4,J,K),K=1,12)
     &                ,ATTRDATA(5,J,1)
C . . . . .DON'T FORGET, STARTDRY WAS TRIGGERED BY -88888 IN V42.10!!!!     
           IF (ATTRDATA(3,J,1).EQ.-88888) ATTRDATA(3,J,1)=1.D0
           IF (I.NE.J) THEN
              WRITE(*,*)'NODE #S IN FORT.12 ARE NONCONSCUTIVE'
              STOP
           END IF
         END DO
         WRITE(*,*)'FINISHED READING tf01_x2_v07.in12 '
      ELSE      
C . . . .READ FORT.13 FORMAT      
         READ(12,1313)DESC
         WRITE(*,*)
         WRITE(*,*)'READING FORT.13 ',TRIM(DESC)
         WRITE(*,*)
         READ(12,*)NNORG
         READ(12,*)NATTR
         ALLOCATE( ATTRNAME(NATTR),UNITS(NATTR),NATTRPERN(NATTR),
     &             DEFAULT(NATTR,12),ATTRDATA(NATTR,NNORG,12)
     &             ,NNDF(NATTR) )
C . . . .READ NAMES, UNITS, DEFAULTS          
         DO I=1,NATTR
            READ(12,1313)ATTRNAME(I)
            READ(12,1313)UNITS(I)
            READ(12,*)NATTRPERN(I)
            READ(12,*)(DEFAULT(I,J),J=1,NATTRPERN(I))
         END DO
C . . . .INITALIZE ATTRIBUTE DATA
         DO I=1,NATTR
            DO J=1,NNORG
               DO K=1,NATTRPERN(I)
                  ATTRDATA(I,J,K)=DEFAULT(I,K)
               END DO
            END DO    
         END DO
C . . . .READ NOT DEFAULT DATA
         DO I=1,NATTR
           READ(12,1313)ATTRNAME(I)
           READ(12,*)NNDF(I)
           DO J=1,NNDF(I)
             READ(12,*)NDF,(ATTRDATA(I,NDF,K),K=1,NATTRPERN(I))
           END DO
         END DO
         WRITE(*,*)
         WRITE(*,'(2A)')'FINISHED READING ',TRIM(FILE12)
         WRITE(*,*)
      END IF
C . . ALL INPUT HAS BEEN READ
C----------------------------------------------------------------------      
C . . DO INTERPOLATION AND WRITE OUT NEW FORT.13      
C----------------------------------------------------------------------
      ALLOCATE( NEWDATA(NATTR,NN,12),ISDEF(NATTR,NN) )
C . . WRITE BEGINNING OF FORT.13
      WRITE(13,1314)TRIM(DESC)
      WRITE(13,*)NN
      WRITE(13,*)NATTR
      DO I=1,NATTR
         WRITE(13,1314)trim(ATTRNAME(I))
         WRITE(13,1314)trim(UNITS(I))
         WRITE(13,*)NATTRPERN(I)
         WRITE(13,'(12e16.8)')(DEFAULT(I,J),J=1,NATTRPERN(I))
      END DO
      
C-----LOOP OVER ATTRIBUTES FOR INTERPOLATIN----------------------------
      DO I=1,NATTR
         write(*,'(a)')trim(attrname(i))
         DO J=1,NN
            ISDEF(I,J)=1.D0
         END DO
C . . . .SKIP ATTRIBUTES THAT HAVE ALL DEFAULT VALUES      
         IF (NNDF(I).EQ.0) GOTO 200
         
C . . . .FIND ATTRIBUTE DATA FOR NEW GRID
         DO J=1,NN    
cnld 3-8-09 for texas nodes         
c            if(istx(j).eq.1) then
c              do k=1,nattrpern(i)
c                 newdata(i,j,k)=default(i,k)
c              end do
c             goto 149
c            end if
              
cc            WRITE(*,'(A,1X,I8,A4,I8)')TRIM(ATTRNAME(I)),J,' OF ',NN
C. . . . . .IN THE CASE THAT THE NODE HASN'T MOVED DATA IS INDEXED NODE TO NODE          
            IF (INDX(J).NE.0) THEN
               DO K=1,NATTRPERN(I)
                  NEWDATA(I,J,K)=ATTRDATA(I,INDX(J),K)
               END DO
            END IF   
C . . . . . USE VELINTERP TO TO DO INTERPOLATION WITHIN AN ELEMENT            
            IF (INDX(J).EQ.0) THEN
               DO K=1,NATTRPERN(I) 
                  CALL VELINTRP2(ATTRDATA(I,N1(J),K),
     &                           ATTRDATA(I,N2(J),K),
     &                           ATTRDATA(I,N3(J),K),    
     &                           NEWDATA(I,J,K),TT(J),UU(J))
               END DO
            END IF
 149      continue  
         END DO   
         
C. . . . FOR "ZERO OR ONE" ATTRIBUTES ROUND ACCORDINGLY         
         IF ((TRIM(ATTRNAME(I)).EQ.'surface_submergence_state') .OR. 
     &       (TRIM(ATTRNAME(I)).EQ.'surface_canopy_coefficient') ) THEN
            DO J=1,NN
               IF (NEWDATA(I,J,1).GT.0.5d0) THEN
                  NEWDATA(I,J,1)=1.D0
               ELSE
                  NEWDATA(I,J,1)=0.D0
               END IF
            END DO
         END IF
      
C . . . .COUNT THE NON DEFAULT VALUES
         NNDF(I)=0
         DO J=1,NN  
            DO K=1,NATTRPERN(I)
               IF (NEWDATA(I,J,K).NE.DEFAULT(I,K)) THEN
                  ISDEF(I,J)=0
                  GOTO 150
               END IF
            END DO
 150        CONTINUE
            IF (ISDEF(I,J).EQ.0) NNDF(I)=NNDF(I)+1
         END DO
               
 200  CONTINUE
      END DO     
C-----END LOOP OVER ATTRIBUTES-----------------------------------------      
      
C . . WRITE FINAL PART OF FORT.13
      DO I=1,NATTR
         WRITE(13,1314)TRIM(ATTRNAME(I))
         WRITE(13,*)NNDF(I)
         IF (NNDF(I).EQ.0) GOTO 250
         DO J=1,NN
            IF (ISDEF(I,J).EQ.0) THEN
               WRITE(13,1316)J,(NEWDATA(I,J,K),K=1,NATTRPERN(I))
            END IF
         END DO
 250     CONTINUE        
      END DO
      
C----------------------------------------------------------------------
      CLOSE(13)
 1313 FORMAT(A80)    
 1314 FORMAT(A)  
 1316 FORMAT(I8,12E16.8) 
      STOP
      END PROGRAM MAIN
C______________________________________________________________________      
      
           
C______________________________________________________________________
C======================================================================
      SUBROUTINE VELINTRP1(X0,Y0,X1,Y1,X2,Y2,XP,YP,T,U)
C----------------------------------------------------------------------
C THIS SUBROUTINE RETURNS T AND U THE COERRICIENTS NEEDED TO CALCULATE VP THE VALUE AT POINT (XP,YP) LINEARLY
C INTERPOLATED ON A PLANE BETWEEN THE THREE POINTS (X0,Y0)(X1,Y1)(X2,Y2)
C----------------------------------------------------------------------     
      IMPLICIT NONE
      
      REAL*4 X0,Y0,X1,Y1,X2,Y2,XP,YP,T,U,DET,
     &    XX1,YY1,XX2,YY2,XXP,YYP 

      
      XX1=X1-X0
      XX2=X2-X0
      YY1=Y1-Y0
      YY2=Y2-Y0
      XXP=XP-X0
      YYP=YP-Y0
      
      DET=(YY2*XX1-XX2*YY1)

      T=(XXP*YY2-XX2*YYP)/DET
      U=(XX1*YYP-YY1*XXP)/DET

CC         VP=V0+T*(V1-V0)+U*(V2-V0)

         
      RETURN
      END SUBROUTINE
C======================================================================

C||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

C______________________________________________________________________
C======================================================================
      SUBROUTINE VELINTRP2(V0,V1,V2,VP,T,U)
C----------------------------------------------------------------------
C THIS SUBROUTINE RETURNS T AND U THE COERRICIENTS NEEDED TO CALCULATE VP THE VALUE AT POINT (XP,YP) LINEARLY
C INTERPOLATED ON A PLANE BETWEEN THE THREE POINTS (X0,Y0)(X1,Y1)(X2,Y2)
C----------------------------------------------------------------------     
      IMPLICIT NONE
      
      REAL*4 V0,V1,V2,VP,T,U

      VP=V0+T*(V1-V0)+U*(V2-V0)
         
      RETURN
      END SUBROUTINE
C======================================================================




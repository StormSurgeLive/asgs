C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C THIS PROGRAM CREATES AN INDEX FROM ONE ADCIRC GRID TO ANOTHER FOR
C USE IN INTERPOLATNIG NODAL ATTRIBUTE(OR OTHER) DATA FROM ONE GRID 
C TO ANOTHER. IT IS ESPECIALLY USEFUL WHEN GRID MODIFICATION HAVE
C BEEN MADE WHICH CHANGE THE NUMBER OF NODES ANT/OR RENUMBER A GRID
C AND FORCING DATA AND NODAL ATTRIBUTE DATA FILES MUST BE MADE TO
C MATCH THE MODIFIED GRID. IT FIRST ATTEMPTS TO FIND NODES THAT ARE
C WITHIN A USER SPECIFIED TOLERANCE. (because of this it best if the 
c grid is in cartsean coordinates)BY USING A SMALL TOLERANCE THIS
C WILL EFFECTIVELY FIND ANY NODES THAT WEREN'T MOVED, BUT MAY HAVE
C HAD THEIR ID # CHANGED.  IT NEXT WILL TRY TO LOCATE THE REMAINING
C NODES WITHIN ELEMENTS IN THE ORIGINAL GRID AND WILL CALCULATE
C INTERPOLATION COEFFICIENTS REQUIRED FOR LINEAR INTERPOLATION WITHIN
C THOSE ELEMENTS. IF AFTER THIS SOME NODES STILL HAVE NOT BEEN FOUND
C (THEY LIE OUTSIDE THE ORIGINAL GRID) THE NEAREST NODE IN THE ORIGNIAL
C GRID WILL BE FOUND AND USED FOR THE INDEX.  FINALLY THE PROGRAM
C WRITES OUT A FILE 'INDX.OUT' WHICH CONAINS:
C 
C NN NNORG
C FOR I=1,NN
C    INDX(I),N1(I),N2(I),N3(I),TT(I),UU(I)
C END
C
C WHERE 
C
C NN       IS THE NUMBER OF NODES IN THE NEW GRID, I
c NNORG    IS THE NUMBER OF NODES IN THE ORIGINAL GRID
C INDX     IS THE ORIGINAL GRID NODE NUMBER OF THE NODE NEAREST TO NEW GRID NODE I. 
C N1,N2,N3 ARE THE ORIGINAL GRID NODE NUMBERS DEFINING THE ORIGINAL GRID ELEMENT IN
C            WHICH NEW NODE I WAS FOUND.  
c TT,UU    ARE INTERPOLATIN COEFFICIENTS USED BY THE SUBROUTINES VELINTERP1 
c            AND VELINTERP2 BELOW. IF INDX==0 THE OTHERS WILL BE NONZERO, IF INDX.NE.0 
c            THE OTHER VALUES WILL BE ZERO.
C
C BY NATHAN DILL 
C WOODS HOLE GROUP INC. 01-08-08
C THIS CODE WAS IS INTENDED FOR USE ON A SPECIFIC ADCIRC 
C MODELING PROJECT. THE AUTHOR MAKES NO CLAIMS CONCERNING THE POTENTIAL 
C FOR ERRORS IN THIS CODE AND TAKES NO RESPONSIBILITY FOR PROBLEMS 
C RESULTING FROM USE OF THIS CODE OUTSIDE OF ITS INTENDED PURPOSE.  
C IF YOU FIND ERRORS AND/OR MAKE MODIFICATIONS PLEASE SHARE THEM WITH 
C MR. DILL VIA EMAIL AT ndill@whgrp.com
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      PROGRAM INDX_INTERP
C----------------------------------------------------------------------     
      IMPLICIT NONE
      
      INTEGER I,J,K,L,THING,NE,NNORG,MAXNN,NNNEW,CNT,NEORG,FOUND,HIT
      INTEGER, ALLOCATABLE :: NIDORG(:),NOC(:,:),LOCAT(:),NIDNEW(:),
     &  INDX(:),N1(:),N2(:),N3(:)
      
      REAL*8 TOL,MAXTOL,XS,TOL2,DIFF,DIFF2,RAD2DEG,DEG2RAD
      REAL*8, ALLOCATABLE :: X(:),Y(:),X2(:),Y2(:)
      
      CHARACTER*80 FILENAME14,FILENAME24

C THING2 VARIABLES     
      INTEGER K1,K2,K3,ISCPP 
      REAL*8 D1,D2,LON0,LAT0,PHI,LAM
      REAL*8, ALLOCATABLE :: STARTDRY(:),VCANOPY(:),ZLAND(:,:)
     &          ,TT(:),UU(:),SDRYNEW(:),VCANNEW(:),ZLANDNEW(:,:)  
      
      CHARACTER*80 FILE12,FILE22,FILE23,TMPLINE
C----------------------------------------------------------------------      

C:::::::::::::::::::::: READ GRID FILES :::::::::::::::::::::::::::::::      
      DEG2RAD=3.141592653589793d0/180.d0
      RAD2DEG=1.D0/DEG2RAD
C . . GET FILE NAMES AND TOLERANCE INFO . . . . . . . . . . . . . . . .
      WRITE(*,*)
      WRITE(*,*)'filename of ORIGNIAL grid?'
      READ(*,*)FILENAME14
      WRITE(*,*)'filename for new grid?'
      READ(*,*)FILENAME24    
      WRITE(*,*)'USE CPP PROJECTION? (1=YES)'
      READ(*,*)ISCPP
      IF (ISCPP.EQ.1) THEN
        WRITE(*,*)'LON0,LAT0?'
        READ(*,*)LON0,LAT0
        LON0=LON0*DEG2RAD
        LAT0=LAT0*DEG2RAD
      END IF

      OPEN(UNIT=14,FILE=FILENAME14)
      OPEN(UNIT=24,FILE=FILENAME24)
     
C . . READ IN ORIGINAL GRID . . . . . . . . . . . . . . . . . . . . . . 
      READ(14,*)
      READ(14,*)NEORG,NNORG
      WRITE(*,*)'READING ORIGINAL GRID ',FILENAME14
      WRITE(*,*)'ORIGINAL GRID NN= ',NNORG
      WRITE(*,*)'ORIGNIAL GRID NE= ',NEORG
      
      ALLOCATE ( NIDORG(NNORG),X(NNORG),Y(NNORG),NOC(3,NEORG))
     
      MAXNN=0   
      DO I=1,NNORG
         READ(14,*)NIDORG(I),LAM,PHI
         MAXNN=MAX(MAXNN,NIDORG(I))
         IF (ISCPP.EQ.1) THEN
            LAM=DEG2RAD*LAM
            PHI=DEG2RAD*PHI
            call CPP(X(I),Y(I),LAM,PHI,LON0,LAT0)
         ELSE
            X(I)=LAM
            Y(I)=PHI
         END IF
      END DO

      DO I=1,NEORG
         READ(14,*)J,K,(NOC(L,I),L=1,3)
      END DO
 
      CLOSE(14)
      WRITE(*,*)'GRID FILE READ SUCCESSFULLY: ',FILENAME14
      
      IF (MAXNN.NE.NNORG) THEN
         WRITE(*,*)'ORIGINAL GRID NODE NUMBERS ARE NOT CONSECUTIVE'
         WRITE(*,*)'THIS IS PROBABLY OKAY, NN IS ',NNORG
         WRITE(*,*)'MAX NN IN ',TRIM(FILENAME14),
     &                              ' IS ',MAXNN 
      END IF
      
C . . READ IN NEW GRID  . . . . . . . . . . . . . . . . . . . . . . . . 
      READ(24,*)
      READ(24,*)NE,NNNEW
      WRITE(*,*)'READING NEW GRID ',FILENAME24
      WRITE(*,*)'NEW GRID NN= ',NNNEW
      WRITE(*,*)'NEW GRID NE= ',NE

      ALLOCATE ( NIDNEW(NNNEW),X2(NNNEW),Y2(NNNEW),INDX(NNNEW), 
     &locat(nnnew),N1(NNNEW),N2(NNNEW),N3(NNNEW),TT(NNNEW),UU(NNNEW))
          
      MAXNN=0
      DO I=1,NNNEW
         locat(i)=0
         READ(24,*)NIDNEW(I),LAM,PHI
         MAXNN=MAX(MAXNN,NIDNEW(I))
         IF (ISCPP.EQ.1) THEN         
            LAM=DEG2RAD*LAM
            PHI=DEG2RAD*PHI
            call CPP(X2(I),Y2(I),LAM,PHI,LON0,LAT0)
         ELSE
            X2(I)=LAM
            Y2(I)=PHI
         END IF
      END DO
      
      CLOSE(24)
      
      WRITE(*,*)'GRID FILE READ SUCCESSFULLY: ',FILENAME24
      
      IF (MAXNN.NE.NNNEW) THEN
         WRITE(*,*)'NEW GRID NODE NUMBERS ARE NOT CONSECUTIVE'
         WRITE(*,*)'MAX NN IN ',TRIM(FILENAME24),
     &                              ' IS ',MAXNN 
         WRITE(*,*)
         WRITE(*,*)'!! THIS IS A PROBLEM, PLEASE RENUMBER GRID !!'
         WRITE(*,*)
         STOP
      END IF

C:::::::::::::::::::::::::::::: THING 1 :::::::::::::::::::::::::::::::      


      WRITE(*,*)'SEARCH DISTANCE TO FIND NEAREST-NEIGHBOR NODES '
      WRITE(*,*)'   (USE A SMALL NUMBER e.g. 1.0) ?'
      READ(*,*)TOL
      WRITE(*,*)'MAXIMUM SEARCH DISTANCE FOR NODES NOT FOUND IN AN'
      WRITE(*,*)'   ELEMENT (USE A BIG NUMBER e.g. 999999) ?'
      READ(*,*)MAXTOL 
      maxtol=maxtol**2.d0           
            
C . . FILL INDX ETC... WITH ZEROS  . . . . . . . . . . . . . . . . . . . . . . 
      DO I=1,NNNEW 
         INDX(I)=0
         N1(I)=0
         N2(I)=0
         N3(I)=0
         TT(I)=0.
         UU(I)=0.
      END DO          
      
C . . FIND THE NODES THAT ARE WITHIN TOL  . . . . . . . . . . . . . . .
      WRITE(*,*)
      WRITE(*,*)'FINDING NODES THAT HAVE CORRESPONDING NODES WITHIN THE'
      WRITE(*,*)'   NEAREST-NEIGHBOR SEARCH DISTANCE'

      CNT=0
c$omp parallel do private(i,j) reduction(+:cnt)      
      DO I=1,NNORG
c      write(*,*)i,'of',nnorg
        
         DO J=1,NNNEW
            IF ( (X2(J) .LT. X(I)+TOL) .AND. 
     &           (X2(J) .GT. X(I)-TOL) .AND.
     &           (Y2(J) .LT. Y(I)+TOL) .AND.
     &           (Y2(J) .GT. Y(I)-TOL))  THEN    
               INDX(J)=NIDORG(I)
               CNT=CNT+1
               GOTO 100
            END IF 
         END DO
         
 100    CONTINUE         
      END DO    
      
      WRITE(*,*)
      WRITE(*,*)CNT,'NODES WERE FOUND WITH NEAREST-NEIGHBORS' 
      
ccc
C            WRITE(*,*)
C      WRITE(*,*)'WRITING INDX1.OUT'
      OPEN(UNIT=11,FILE='INDX1.OUT')
      DO I=1,NNNEW
         WRITE(11,*)INDX(I),LOCAT(I)
      END DO
      CLOSE(11)
ccc      
      
      
      
      
          
C . . NOW FIND THE LOCAT OF THE NODES THAT WEREN'T FOUND WITHIN TOL
      WRITE(*,*)
      WRITE(*,*)' FINDING ELEMENTAL LOCATION OF NODES THAT WERE '
      WRITE(*,*)'NOT FOUND WITH NEAREST-NEIGHBORS, BUT ARE IN GRID'
     &,' ELEMENTS'
      
c$omp parallel do private(i,j,found) schedule(dynamic,1000)     
      DO I=1,NNNEW
c         write(*,*)i,'of',nnnew
         IF (INDX(I).EQ.0) THEN
        
         DO J=1,NEORG

            CALL LOCAT_CHK(J,X2(I),Y2(I),NOC,X,Y,FOUND)
          
            
            IF (FOUND.EQ.1) THEN
               locat(i)=j
               N1(I)=NOC(1,J)
               N2(I)=NOC(2,J)
               N3(I)=NOC(3,J)
               GOTO 110 
            END IF
            
         END DO
 110     CONTINUE     
         END IF
      END DO
      
      
ccc
C            WRITE(*,*)
c      WRITE(*,*)'WRITING INDX2.OUT'
      OPEN(UNIT=11,FILE='INDX2.OUT')
      DO I=1,NNNEW
         WRITE(11,*)INDX(I),LOCAT(I)
      END DO
      CLOSE(11)
ccc      
      
      
C . . COUNT HOW MANY NODES DON'T HAVE AN INDX OR LOCAT YET  . . . . . .
      CNT=0
      DO I=1,NNNEW
         IF (INDX(I).EQ.0 .AND. N1(I).EQ.0) CNT=CNT+1
      END DO
      WRITE(*,*)
      WRITE(*,*)CNT,' NODES WERE NOT FOUND WITH A NEAREST-NEIGHBOR'
      WRITE(*,*)' OR WITHIN A GRID ELEMENT'

               
      IF(CNT.GT.0) THEN
        WRITE(*,*)' DOUBLING THE SEARCH DISTANCE AND SEARCHING UNTIL'
        WRITE(*,*)'  IT IS GREATER THAN THE MAX SEARCH DISTANCE'
        XS=2.
C        READ(*,*)XS

      
        DO J=1,NNNEW
c        write(*,*)j,' of ',nnnew
        TOL2= TOL*XS
         IF(INDX(J).EQ.0 .AND. N1(J).EQ.0) THEN 
 150       CONTINUE           
            DIFF=MAXTOL
            HIT=0
 
            DO I=1,NNORG 
               IF ( (X2(J) .LT. X(I)+TOL2) .AND. 
     &              (X2(J) .GT. X(I)-TOL2) .AND.
     &              (Y2(J) .LT. Y(I)+TOL2) .AND.
     &              (Y2(J) .GT. Y(I)-TOL2))  THEN    
C THE NEXT 3 LINES ENSURE THAT YOU FIND THE CLOSEST NODE, NOT JUST THE FIRST ON WITHIN TOL2
               DIFF2=DIFF   
               DIFF=MIN( DIFF2,(X2(J)-X(I))**2 + (Y2(J)-Y(I))**2 )
                  IF(DIFF.LT.DIFF2) THEN  
                     INDX(J)=NIDORG(I)

                     HIT=HIT+1
                  END IF
               END IF                
            END DO
            
            IF (HIT.GT.0) GOTO 200
C . . . . . this section only run if not found within current TOL2            
            TOL2=TOL2*XS
              
            IF(TOL2.GT.MAXTOL) THEN
               maxtol=maxtol**0.5d0
               WRITE(*,*)'  NEW NODE NUMBER ',J
               WRITE(*,*)' NOT FOUND WITHIN ',MAXTOL
               WRITE(*,*)' THIS IS A PROBLEM, QUITTING'
               STOP
            END IF
            
            GOTO 150

            
 200         CONTINUE            
           END IF
        END DO 
C . . SOME NODES DONNT HAVE LOCAT OR INDX YET
      END IF 

C:::::::::::::::: CALCULATE INTERPOLATION COEFFICIENTS
      WRITE(*,*)'CALCULATING INTERPOLATION COEFFICIENTS'
      DO I=1,NNNEW
c         WRITE(*,*)'INTERP',I,'OF',NNNEW
         IF (INDX(I).EQ.0) THEN

            CALL VELINTRP1( X(N1(I)),Y(N1(I)),
     &                      X(N2(I)),Y(N2(I)),
     &                      X(N3(I)),Y(N3(I)),
     &                      X2(I),Y2(I),
     &                      TT(I),UU(I)    )
         END IF
      END DO
      
      
      
C . . NOW WRITE OUT RESULTS
      WRITE(*,*)
      WRITE(*,*)'WRITING INDX.OUT'
      OPEN(UNIT=11,FILE='INDX.OUT')
      WRITE(11,*)NNNEW,NNORG
      DO I=1,NNNEW
         WRITE(11,*)INDX(I),N1(I),N2(I),N3(I),TT(I),UU(I)
      END DO
      CLOSE(11)

      
      STOP
      END PROGRAM INDX_INTERP
C______________________________________________________________________      
         
      
C______________________________________________________________________
C======================================================================
      SUBROUTINE VELINTRP1(X0,Y0,X1,Y1,X2,Y2,XP,YP,T,U)
C----------------------------------------------------------------------
C THIS SUBROUTINE RETURNS T AND U, THE COEFFICIENTS USED BY VELINTERP2
C TO LINEARLY INTERPOLATE SOME VALUE AT POINT (XP,YP) USING KNOWN VALUES
C AT THE THREE POINTS (X0,Y0)(X1,Y1)(X2,Y2). ASSUMING THE VALUE LIES ON 
C A PLANE PASSING THROUGH THE THREE POINTS.
C----------------------------------------------------------------------     
      IMPLICIT NONE
      
      REAL*8 X0,Y0,X1,Y1,X2,Y2,XP,YP,T,U,DET,
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

C BY UNCOMMENTING THE FOLLOWING LINE AND ADDING VP,V0,V1,V2 TO THE 
C ARGUMENT LIST YOU CAN ACOMPLISH THE FULL INTERPOLATION AND GET VP
C UNCOMMENT ME       VP=V0+T*(V1-V0)+U*(V2-V0) 

         
      RETURN
      END SUBROUTINE
C======================================================================

C||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

C______________________________________________________________________
C======================================================================
      SUBROUTINE VELINTRP2(V0,V1,V2,VP,T,U)
C----------------------------------------------------------------------
C THIS SUBROUTINE USES THE T AND U COEFFICIENTS CREATED BY VELINTERP1
C TO FINISH THE LINEAR INTERPOLATION CALCULATION AND PROVIDE THE VALUE
C----------------------------------------------------------------------     
      IMPLICIT NONE
      
      REAL*8 V0,V1,V2,VP,T,U

      VP=V0+T*(V1-V0)+U*(V2-V0)
         
      RETURN
      END SUBROUTINE
C======================================================================

C||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

C______________________________________________________________________
C======================================================================
      SUBROUTINE LOCAT_CHK(LOCAT1,XP,YP,NOC,X,Y,FOUND)
C----------------------------------------------------------------------
C THIS SUBROUTINE CHECKS IF A PARTICLE RESIDES WITHIN THE ELEMENT LOCAT
C IT RETURNS THE VALUE FOUND=1 IF THE PARTICLE IS FOUND OR FOUND=0 IF 
C IT WAS NOT FOUND. LOCAT,XP,YP ARE SCALAR INPUT. 
C----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER LOCAT1,FOUND,NOC(3,1),I
      REAL*8 XP,YP,X(1),Y(1),
     &                 DS1(2),DS2(2),DS3(2),CROSS,C1,C2,C3
      
     
      FOUND=0
             
C . . GET DISPLACEMENTS FROM PARTICLE TO NODES    
      DS1(1)=X(NOC(1,LOCAT1))-XP
      DS1(2)=Y(NOC(1,LOCAT1))-YP
      DS2(1)=X(NOC(2,LOCAT1))-XP
      DS2(2)=Y(NOC(2,LOCAT1))-YP
      DS3(1)=X(NOC(3,LOCAT1))-XP
      DS3(2)=Y(NOC(3,LOCAT1))-YP

C . . ALL + CROSS PRODS. MEANS PART. IS FOUND IF NOC IS ANTI-CLOCKWISE      
      C1=CROSS(DS1,DS2)
      C2=CROSS(DS2,DS3)
      C3=CROSS(DS3,DS1)
       
      
      
      IF ((C1.GE.0).AND.(C2.GE.0).AND.(C3.GE.0)) FOUND=1
      
      RETURN
      END SUBROUTINE
C======================================================================

C||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

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










C******************************************************************************
C                                                                             *
C    Transform from lon,lat (lamda,phi) coordinates into CPP coordinates.     *
C    Lon,Lat must be in radians.                                              *
C                                                                             *
C******************************************************************************

      SUBROUTINE CPP(X,Y,RLAMBDA,PHI,RLAMBDA0,PHI0)
      IMPLICIT NONE
      REAL*8 X,Y,RLAMBDA,PHI,RLAMBDA0,PHI0,R
      R=6378206.4d0
      X=R*(RLAMBDA-RLAMBDA0)*COS(PHI0)
      Y=PHI*R
      RETURN
      END


C******************************************************************************
C                                                                             *
C    Transform from CPP coordinates to lon,lat (lamda,phi) coordinates        *
C    Lon,Lat is in radians.                                                   *
C                                                                             *
C******************************************************************************

      SUBROUTINE INVCP(XXCP,YYCP,RLAMBDA,PHI,RLAMBDA0,PHI0)
      IMPLICIT NONE
      REAL*8 XXCP,YYCP,RLAMBDA,PHI,RLAMBDA0,PHI0,R
      R=6378206.4d0
      RLAMBDA=RLAMBDA0+XXCP/(R*COS(PHI0))
      PHI=YYCP/R
      RETURN
      END


!23456789012345678901234567890123456789012345678901234567890123456789012345
!
! This code uses swan output to create wave height vector components
! for plotting wave height and direction in FigureGen. Specifically,
! it reads in sig wave height and wave direction values from swan 
! global output files. It then finds the maximum sig wave height in the
! time series output, as well as the corresponding wave direction value
! at that time. The wave direction and wave height are then used to
! create easting and northing components of wave height. This code could
! easily be modified to write out global output files to show vector
! components as a function of time. Note that calling these "maximum"
! significant wave heights are not strictly correct since you are
! extracting values from your time series output. The swan_HS_max.63
! file contains the actual maximum sig. wave height values calculated
! during the simulation, but there is no way to determine the
! corresponding wave directions for those data. So be sure to keep in
! mind that the frequency of your global output could bias your
! reporting of the max sig. wave height in this output.  

! Author:        Bret M. Webb, bwebb@southalabama.edu
! Last Revision: September 21, 2018 
!

       PROGRAM MAKE_WAVE_VECTORS 

       IMPLICIT NONE

       CHARACTER :: RUNDES*32, RUNID*24, AGRID*24, PREFIX*4, EXTEND*4
       CHARACTER :: AA*32, BB*24, CC*24, FILENAME*11, OUTNAME*19

       INTEGER :: NDSETSE, DTPLOT, NSPOOLGE, IRTYPE, KK, NE, NP, J, &
                  K, JN
       
       REAL :: HSMAXVAL, PI, ANGLE

       REAL, ALLOCATABLE :: TIME(:), IT(:), X(:), Y(:), DP(:), &
                            HS(:,:), HSMAX(:), DIRMAX(:), DIR(:,:),&
                            HSXX(:), HSYY(:)

       PI=4.0*ATAN(1.0) ! My favorite way to represent the constant PI!

! On the next line change "0" to "1" if you need to read in fort.14
! file info for some reason. It is not necessary to read in these data
! but it is left here just in case.
       IF(0.eq.1)THEN 
! Read in fort.14 for lon, lat, and depth

       OPEN(10,FILE='fort.14',STATUS='old')

       READ(10,24) AGRID
 24    FORMAT(a24)
       READ(10,*) NE, NP
 
       ALLOCATE(X(NP))
       ALLOCATE(Y(NP))
       ALLOCATE(DP(NP))

       DO K=1,NP
        READ(10,*) JN, X(K), Y(K), DP(K)
       ENDDO
       CLOSE(10)
       ENDIF

! Read in swan global output for sig. wave height
       OPEN(20,FILE='swan_HS.63',STATUS='old')

       READ(20,11) AA, BB, CC
 11    FORMAT(2x,a32,2x,a24,1x,a24)
       RUNDES=TRIM(AA)
       RUNID=TRIM(BB)
       AGRID=TRIM(CC)
 
       READ(20,*) NDSETSE, NP, DTPLOT, NSPOOLGE, IRTYPE
      
! NDSETSE OVERRIDE: use this if you have an incomplete file or  
! if you want to ignore reading data late in the simulation output
       !NDSETSE=204 
       ALLOCATE(TIME(NDSETSE))
       ALLOCATE(IT(NDSETSE))
       ALLOCATE(HS(NDSETSE,NP))

       DO J=1,NDSETSE
        READ(20,*) TIME(J), IT(J)
        DO K=1,NP
         READ(20,*) KK, HS(J,K) 
        ENDDO
       ENDDO
       CLOSE(20)

! Read in swan global output for wave direction
       OPEN(21,FILE='swan_DIR.63',STATUS='old')

       READ(21,11) AA, BB, CC
       RUNDES=TRIM(AA)
       RUNID=TRIM(BB)
       AGRID=TRIM(CC)
 
       READ(21,*) NDSETSE, NP, DTPLOT, NSPOOLGE, IRTYPE
      
! NDSETSE OVERRIDE: use this if you have an incomplete file or  
! if you want to ignore reading data late in the simulation output
       !NDSETSE=204 
       ALLOCATE(DIR(NDSETSE,NP))

       DO J=1,NDSETSE
        READ(21,*) TIME(J), IT(J)
        DO K=1,NP
         READ(21,*) KK, DIR(J,K) 
        ENDDO
       ENDDO
       CLOSE(21)

      

! Sweep through data and look for corresponding max Hs and Direction
       ALLOCATE(HSMAX(NP))
       ALLOCATE(DIRMAX(NP))
       ALLOCATE(HSXX(NP))
       ALLOCATE(HSYY(NP))
       DO K=1,NP
       HSMAXVAL=0.0
        DO J=1,NDSETSE
         IF(HS(J,K).GT.HSMAXVAL)THEN
          HSMAX(K)=HS(J,K)
          HSMAXVAL=HS(J,K)
          DIRMAX(K)=DIR(J,K)
         ENDIF
        ENDDO
       ENDDO


! Now "vectorize" the wave height based on direction.
! This is not elegant, but it works. I am too lazy to clean this up. 
       DO K=1,NP
        IF(DIRMAX(K).ge.0.0 .and. DIRMAX(K).le.90.0)THEN
         ANGLE=DIRMAX(K)-0.0
         HSXX(K)=-1.0*HSMAX(K)*SIN(ANGLE*PI/180.)
         HSYY(K)=-1.0*HSMAX(K)*COS(ANGLE*PI/180.)
        ELSEIF(DIRMAX(K).gt.90.0 .and. DIRMAX(K).le.180.0)THEN
         ANGLE=DIRMAX(K)-90.0
         HSXX(K)=-1.0*HSMAX(K)*COS(ANGLE*PI/180.)
         HSYY(K)=HSMAX(K)*SIN(ANGLE*PI/180.)
        ELSEIF(DIRMAX(K).gt.180.0 .and. DIRMAX(K).le.270.0)THEN
         ANGLE=DIRMAX(K)-180.0
         HSXX(K)=HSMAX(K)*SIN(ANGLE*PI/180.)
         HSYY(K)=HSMAX(K)*COS(ANGLE*PI/180.)
        ELSEIF(DIRMAX(K).gt.270.0 .and. DIRMAX(K).le.360.0)THEN
         ANGLE=DIRMAX(K)-270.0
         HSXX(K)=HSMAX(K)*COS(ANGLE*PI/180.)
         HSYY(K)=-1.0*HSMAX(K)*SIN(ANGLE*PI/180.)
        ENDIF
       ENDDO

! Write max swan sig. wave height values to output formatted as .63 file     
       OPEN(28,FILE='swan_HS_max.63.ex',STATUS='NEW')
       WRITE(28,11) AA, BB, CC
       WRITE(28,*) NDSETSE, NP, DTPLOT, NSPOOLGE, IRTYPE
       WRITE(28,*) TIME(1), IT(1)
       DO K=1,NP
        IF(HSMAX(K).EQ.0.0)THEN
         HSMAX(K)=-99999.0 ! Flag for blanking values
        ENDIF
        WRITE(28,*) K, HSMAX(K)
       ENDDO
       CLOSE(28) 

! Write max wave direction vectors to output formatted as .64 file
       OPEN(29,FILE='swan_HSvec_max.64.ex',STATUS='NEW')
       WRITE(29,11) AA, BB, CC
       WRITE(29,*) NDSETSE, NP, DTPLOT, NSPOOLGE, IRTYPE
       WRITE(29,*) TIME(1), IT(1)
       DO K=1,NP
        IF(HSMAX(K).LT.-9999.0)THEN
         HSXX(K)=0.0 ! Make sure that flagged values are zeroed
         HSYY(K)=0.0 ! FigureGen does not like blanking for vectors
        ENDIF
        WRITE(29,*) K, HSXX(K), HSYY(K)
       ENDDO
       CLOSE(29) 

       STOP
       END PROGRAM MAKE_WAVE_VECTORS 
!23456789012345678901234567890123456789012345678901234567890123456789012345

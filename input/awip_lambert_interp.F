C***********************************************************************
C   Program to read multiple ETA 32km (grid 221) met files containing  *
C   u10,v10,p data, interpolate this data to an ADCIRC grid, rotate    *
C   the wind into E and N components, convert wind speed to wind stress*
C   and write out a single fort.22 file in ADCIRC NWS=-2 format.       *
C                                                                      *
C   This version of this code is set up to work with 5 hindcast met    *
C   files.                                                             *
C                                                                      *
C   Locations of the FE nodes within the 221 grid and wind rotation    *
C   factors are determined by NWS subroutine GDSWZD03.                 *
C                                                                      * 
C   This version includes an algorithm for dealing with points that    *
C   lie outside the 221 Grid and for interpolating met values when     *
C   some of the met values in the 221 Grid are fill values             *
C   (e.g., 9.999e20).                                                  *
C                                                                      *
C   This version will attempt to deal with missing met files by        *
C   interpolating between available files.  If the 1st (last) met      *
C   file is missing, the code will use the next (previous) available   *
C   file in its place.                                                 *
C                                                                      *
C   Language: Fortran 90 with dynamically allocated arrays             *
C   Written by:  Rick Luettich   UNC IMS   9/10/01 - v1                *
C                                         10/04/01 - v2                *
C                                         10/15/01 - v3                *
C                                                                      *
C    Mod by BOB for q51 grid extent and file structures 10/23/01       *
C    Mod by BOB for arbitrary ll input 29/01/03                        *
C
C    jgf20100323: 
C    + Added 'implicit none' to subroutines and modules except GDSWZD03.
C    + Took note of the extremely dangerous coding practices in GDSWZD03
C    and shuddered. 
C    + Took note of the floating point comparison tests in the main 
C    program and shuddered again. 
C    + TODO: Eliminate floating point comparison tests in main program,
C    at least.
C    + Explicitly declared previously implicity declared variables,
C    except in GDSWZD03. 
C    + Eliminated occurrences of implicit precision loss.
C    + Used int() explicitly where truncation was required, at least in
C    the main program.
C    + Added a check to be sure a file exists before attempting to
C    open it.
C    + Made GDSWZD03 and the main program use the same values for 
C    physical constants.
C    + Added help message and more informative error messages.
C    + Added options to produce output in terms of wind
C    velocity rather than wind stress, and pressure in units of 
C    millibars rather than m of H2O. Also made the application of 
C    a time weighting conversion factor an option.
C    jgf20100513:
C    +changed the log file name to lambert_diag.out to make it more special
C    +switched the unit number of all log messages to 16 to keep this program
C     from writing fort.1 file
C    +changed the format from A30 to A132 for writing file names to the log
C     file so that the full value of the file name would be captured there
C
C    Build Instructions:
C    g95 -ffixed-form -Wall -o awip_lambert_interp.x awip_lambert_interp.f90
C
C    ifort -o awip_lambert_interp.x awip_lambert_interp.F
C
C    On garnet at ERDC, be sure to add the -tp=shanghai-64 flag to the 
C    fortran compiler flags; otherwise the executable will fail with
C    an opaque "Illegal instruction" error message. 
C
C
C***********************************************************************

C-----------------------------------------------------------------------

      MODULE GLOBAL
      IMPLICIT NONE

C...
C...SET PRECISION OF REAL NUMBERS
C...     
      INTEGER, PARAMETER :: SZ = 4
c     INTEGER, PARAMETER :: SZ = 8

C...
C...SET GLOBAL PARAMETER CONSTANTS
C...

C.....value in eta32 met file meaning no data
C      REAL(SZ), PARAMETER :: FILL = 9.999e20  !established in code

C.....radius of the earth
      REAL(SZ), PARAMETER  ::  RERTH=6.3712E6

C.....air, seawater density
      REAL(SZ), PARAMETER  ::  RHOAIR=1.225, RHOH2O=1024.

C.....PI and degrees to radians conversions
      REAL(SZ), PARAMETER  ::  PI=3.14159265358979
      REAL(SZ), PARAMETER  ::  DPR=180./PI

C...
C...DECLARE ALL ARRAYS
C...
      REAL(SZ),ALLOCATABLE ::  XPTS(:),YPTS(:)
      REAL(SZ),ALLOCATABLE ::  RLON(:),RLAT(:)
      REAL(SZ),ALLOCATABLE ::  CROT(:),SROT(:)
      REAL(SZ),ALLOCATABLE ::  XLON(:),XLAT(:),YLON(:),YLAT(:)
      REAL(SZ),ALLOCATABLE ::  AREA(:)
      REAL(SZ),ALLOCATABLE ::  VARfe(:,:), Ufe(:), Vfe(:)
      REAL(SZ),ALLOCATABLE ::  VAReta(:,:,:),Ueta(:,:),Veta(:,:)

      INTEGER, ALLOCATABLE ::  KGDS(:)
C...
C...DECLARE ALL NON ARRAY VARIABLES
C...
      REAL(SZ) wstress_x_fe,wstress_y_fe
      REAL(SZ) windmag,wdragco
      REAL(SZ) DELI,DELJ
      REAL(SZ) XRATIO1,XRATIO2,YRATIO1,YRATIO2
      REAL(SZ) X1Y1RATIO, X1Y2RATIO, X2Y1RATIO, X2Y2RATIO
      REAL(SZ) X1Y1RATIOp,X1Y2RATIOp,X2Y1RATIOp,X2Y2RATIOp
      REAL(SZ) X1Y1RATIOu,X1Y2RATIOu,X2Y1RATIOu,X2Y2RATIOu
      REAL(SZ) CORR,skip,Cd,q
      REAL(SZ) u10_awip,v10_awip
      INTEGER  IOPT,LROT,LMAP
      INTEGER  NE,NP,NPTS,NN,NF,NF1
      INTEGER  I,J,iskip
      !
      ! jgf20100324 GRID coordinates surrounding interpolated point:
      REAL(SZ) GX1,GX2,GY1,GY2 
      !
      ! array indices of grid points surrounding interpolated point:
      INTEGER  I1,I2,J1,J2
C
      CHARACTER(LEN=132) ::BUFFER,MET_F_NAME,MET_F_NAME_OUT,TARGPTNAME
      INTEGER :: GNUM

      END MODULE GLOBAL
      
      
      MODULE GDS
      ! NCEP GRID-DEPENDENT GDS PARAMETERS      
      IMPLICIT NONE
      INTEGER, PARAMETER :: SZ = 4
      INTEGER IM,JM,IROT,IPROJ,ISCAN,JSCAN,NSCAN
      REAL(SZ) RLAT1,RLON1,ORIENT,DX,DY,RLATI1,RLATI2
      END MODULE GDS

C************************************************************************

      PROGRAM AWIP_LAMBERT_INTERP

      USE GLOBAL
      USE GDS
      IMPLICIT NONE
      ! jgf20100323: Declare command line arguments. 
      INTEGER NCOL ! how many columns of data to be reprojected/reinterpolated
      ! jgf20100323: Declare previously implicitly declared variables.
      REAL FILL ! new fill val in netCDF versions of NCEP grib files
      INTEGER NXP
      INTEGER NYP
      INTEGER NRET     ! INTEGER NUMBER OF VALID POINTS COMPUTED
      INTEGER IOBFLAG
      INTEGER K
      REAL XYRATIOP_SUM
      REAL XYRATIOU_SUM
      LOGICAL FileFound  ! jgf20100324: .true. if file exists
      !
      ! jgf20100324: .true. if we should convert wind velocity to wind stress
      LOGICAL windStress
      !
      ! jgf20100324: either 'velocity' or 'stress'
      CHARACTER(LEN=132) :: windUnitsString
      !
      ! jgf20100324: multiplier for wind components; can be used to
      ! convert 1min averaged winds to 10min averaged winds, for example 
      REAL windMult
      INTEGER IARGC
C
      windStress = .true. ! convert wind velocity to wind stress by default
      windMult = 1.0e0    ! just keep the wind components the same, by default
C
C     Formulate help message
      IF (IARGC().LT.5)THEN
         ! Provide useful error message and hint at meaning of command
         ! line arguments. 
         WRITE(*,*)
         WRITE(*,*) 'Error: Needed at least 5 command-line arguments.'
         WRITE(*,*) 'Sample usage:'
         WRITE(*,19)
19       FORMAT('./awip_lambert_interp.x GNUM NCOL MET_F_NAME ',
     &          'TARGPTNAME MET_F_NAME_OUT [wind units] [wind mult]')
         WRITE(*,*) ' where '
         WRITE(*,*) 'GNUM is the AWIP grid number: 218, 221, or 357.'
         WRITE(*,*) 
     &   'NCOL is the number of columns of data to be reprojected.' 
         WRITE(*,*) 
     &   'MET_F_NAME is the name of the AWIP gridded data file.'
         WRITE(*,*) 
     &   'TARGPTNAME file is a list of coordinates for reprojection.'
         WRITE(*,*) 
     &   'MET_F_NAME_OUT contains the reprojected data.'
         WRITE(*,*) 'Optional Arguments:'
         WRITE(*,*) '[wind units] is stress (the default) or velocity.'
         WRITE(*,*) 
     &   '[wind mult] applies the specified factor to the wind data.'
         WRITE(*,*)
         WRITE(*,*) 'Example: U,V,P data from NAM 12km grid in a file'
         WRITE(*,*) 'called nam.in must be reprojected to a set of '
         WRITE(*,*) 'lat/lon pairs in a file called mymesh.grd, with'
         WRITE(*,*) 'the data written to a file called met.out with'
         WRITE(*,*) 'wind in units of velocity, but multiplied by 0.9:'
         WRITE(*,*)
         WRITE(*,18) 
18       FORMAT(' ./awip_lambert_interp.x ',
     &         '218 3 nam.in mymesh.grd met.out velocity 0.9')
         WRITE(*,*) 
         WRITE(*,*) 'Execution of AWIP_LAMBERT_INTERP is terminated.' 
         STOP 
      END IF


C...Open a diagnostics output file

      OPEN(16,file='lambert_diag.out',status='replace')

      CALL GETARG(1,BUFFER)
      READ(BUFFER,'(i4)')GNUM
      WRITE(16,2)GNUM
 2    FORMAT('GNUM=',1x,I6) 

      CALL GETARG(2,BUFFER)
      READ(BUFFER,'(i4)')NCOL
      WRITE(16,3)NCOL
 3    FORMAT('NCOL=',1x,I6) 

      CALL GETARG(3,BUFFER)
      READ(BUFFER,'(a)')MET_F_NAME
      WRITE(16,4)TRIM(MET_F_NAME)
 4    FORMAT('MET_F_NAME',1x,A132) 

      CALL GETARG(4,BUFFER)
      READ(BUFFER,'(a)')TARGPTNAME
      WRITE(16,5)TRIM(TARGPTNAME)
 5    FORMAT('TARGPTNAME',1x,A132) 

      CALL GETARG(5,BUFFER)
      READ(BUFFER,'(a)')MET_F_NAME_OUT
      WRITE(16,6)TRIM(MET_F_NAME_OUT)
 6    FORMAT('MET_F_NAME_OUT',1x,A132) 
   
      IF (IARGC().gt.5) THEN
         CALL GETARG(6,BUFFER)
         READ(BUFFER,'(a)') windUnitsString
         WRITE(16,7)TRIM(windUnitsString)
 7       FORMAT('Velocity units',1x,A30) 
         SELECT CASE (trim(windUnitsString))
         CASE("velocity","Velocity","VELOCITY")
            windStress = .false.
         CASE("stress","Stress","STRESS")
            windStress = .true. ! this is the default anyway
         CASE DEFAULT
            WRITE(*,21) trim(windUnitsString)
         END SELECT
21       FORMAT("Could not recognize wind units: '",(A),"'. ",
     &          "Needed 'stress' or 'velocity'.")  
      ENDIF
C
      IF (IARGC().gt.6) THEN
         CALL GETARG(7,BUFFER)
         READ(BUFFER,*) windMult
         WRITE(16,*) 'wind multiplier is ', windMult
      ENDIF 

C set KGDS based on GNUM
      ALLOCATE (KGDS(200))
      CALL SETKGDS(GNUM,KGDS)
      
C      FILL=9.999e20
C new fill val in netCDF versions of NCEP grib files
      FILL=-9999.

      WRITE(16,*) 'The value used to represent fill is ',fill

C...Open file of lls to interpolate to
      INQUIRE(file=TARGPTNAME,EXIST=FileFound)
      IF (FileFound.eqv..false.) THEN
         WRITE(*,20) TARGPTNAME
         STOP
      ENDIF  
      OPEN(14,file=TARGPTNAME,STATUS='OLD')
      read(14,*)NPTS
20    FORMAT('ERROR: FileNotFound: ',(A))  

C...Allocate input and output arrays based on number of pairs
      ALLOCATE ( XPTS(NPTS),YPTS(NPTS))
      ALLOCATE ( RLON(NPTS),RLAT(NPTS))
      ALLOCATE ( CROT(NPTS),SROT(NPTS))
      ALLOCATE ( XLON(NPTS),XLAT(NPTS),YLON(NPTS),YLAT(NPTS)) 
      ALLOCATE ( AREA(NPTS))
      ALLOCATE ( VARfe(NPTS,NCOL-2),Ufe(NPTS),Vfe(NPTS))

      NXP=IM
      NYP=JM

      ALLOCATE (VAReta(NXP,NYP,NCOL-2))
      ALLOCATE (Ueta(NXP,NYP))
      ALLOCATE (Veta(NXP,NYP))
C...Read in the Lon,Lat of FE nodes

      DO NP=1,NPTS
         READ(14,*) NN,RLON(NP),RLAT(NP)
      ENDDO
      CLOSE(14)

C...Locate these points in AWIP grid and compute the velocity rotation factors

      IOPT=-1
      LROT=1
      LMAP=0

      CALL GDSWZD03(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &          LROT,CROT,SROT,LMAP,XLON,XLAT,YLON,YLAT,AREA,IOBFLAG)


C...Read in AWIP grid met file
      INQUIRE(file=MET_F_NAME,EXIST=FileFound)
      IF (FileFound.eqv..false.) THEN
         WRITE(*,20) MET_F_NAME
         STOP
      ENDIF  
      OPEN(99,FILE=MET_F_NAME,STATUS='OLD')
      DO J=1,NYP
         DO I=1,NXP
            READ(99,*,END=10) 
     &         Ueta(I,J),Veta(I,J),(VAReta(I,J,K),K=1,NCOL-2)
C               write(16,*) "DEBUG: Ueta, Veta, VAReta(",I,",",J,")=",
C     &            Ueta(I,J), Veta(I,J), VAReta(I,J,1)
         END DO
      END DO
 10   CONTINUE
      CLOSE(99)
c      WRITE(*,*) 'Finished reading file ',MET_F_NAME
c      WRITE(1,*) 'Finished reading file ',MET_F_NAME


C...Interpolate the Met info to the Lon,Lat locations, rotate wind
C... velocity into E and N directions, convert to wind stress 
C... and write output
C23456+
      OPEN(22,file=MET_F_NAME_OUT,STATUS='REPLACE')

      DO NP=1,NPTS     
         IF((XPTS(NP).EQ.FILL).OR.(YPTS(NP).EQ.FILL)) THEN
            VARfe(NP,:)=FILL
            Ufe(NP)=FILL
            Vfe(NP)=FILL
         ELSE
            GX1=FLOAT(INT(XPTS(NP))) ! truncate to west nearest neighbor
            GX2=GX1+1.0e0            ! east nearest neigbor  
            GY1=FLOAT(INT(YPTS(NP))) ! truncate to south nearest neighbor
            GY2=GY1+1.0e0            ! north nearest neighbor
            DELJ=1.e0
            DELI=1.e0
            XRATIO1 = 1.e0 - (XPTS(NP)-GX1)/DELI
            YRATIO1 = 1.e0 - (YPTS(NP)-GY1)/DELJ
            XRATIO2 = 1.e0 - (GX2-XPTS(NP))/DELI
            YRATIO2 = 1.e0 - (GY2-YPTS(NP))/DELJ
            X1Y1RATIO = XRATIO1*YRATIO1
            X1Y2RATIO = XRATIO1*YRATIO2
            X2Y1RATIO = XRATIO2*YRATIO1
            X2Y2RATIO = XRATIO2*YRATIO2

            X1Y1RATIOp = X1Y1RATIO
            X2Y1RATIOp = X2Y1RATIO
            X1Y2RATIOp = X1Y2RATIO
            X2Y2RATIOp = X2Y2RATIO
            ! use nearest neighbor GRID lines as array indices
            I1 = INT(GX1)
            I2 = INT(GX2)
            J1 = INT(GY1)
            J2 = INT(GY2)
            IF(VAReta(I1,J1,1).eq.FILL) THEN
               CORR=1.0e0-X1Y1RATIOP
               IF(CORR.EQ.0.) CORR=FILL*FILL
               X1Y1RATIOp = 0.e0
               X2Y1RATIOp = X2Y1RATIOp/CORR
               X1Y2RATIOp = X1Y2RATIOp/CORR
               X2Y2RATIOp = X2Y2RATIOp/CORR
            ENDIF
            IF(VAReta(I2,J1,1).eq.FILL) THEN
               CORR=1.0e0-X2Y1RATIOP
               IF(CORR.EQ.0.) CORR=FILL*FILL
               X1Y1RATIOp = X1Y1RATIOp/CORR
               X2Y1RATIOp = 0.e0
               X1Y2RATIOp = X1Y2RATIOp/CORR
               X2Y2RATIOp = X2Y2RATIOp/CORR
            ENDIF
            IF(VAReta(I1,J2,1).eq.FILL) THEN
               CORR=1.0e0-X1Y2RATIOP
               IF(CORR.EQ.0.) CORR=FILL*FILL
               X1Y1RATIOp = X1Y1RATIOp/CORR
               X2Y1RATIOp = X2Y1RATIOp/CORR
               X1Y2RATIOp = 0.e0
               X2Y2RATIOp = X2Y2RATIOp/CORR
            ENDIF
            IF(VAReta(I2,J2,1).eq.FILL) THEN
               CORR=1.0e0-X2Y2RATIOP
               IF(CORR.EQ.0.) CORR=FILL*FILL
               X1Y1RATIOp = X1Y1RATIOp/CORR
               X2Y1RATIOp = X2Y1RATIOp/CORR
               X1Y2RATIOp = X1Y2RATIOp/CORR
               X2Y2RATIOp = 0.e0
            ENDIF
      
            XYRATIOp_sum=X1Y1RATIOp+X2Y1RATIOp+X1Y2RATIOp+X2Y2RATIOp
            IF(XYRATIOp_sum.eq.0.0) THEN
               VARfe(NP,:) = FILL
            ELSE
               VARfe(NP,:) = VAReta(I1,J1,:)*X1Y1RATIOp
     &                     + VAReta(I1,J2,:)*X1Y2RATIOp
     &                     + VAReta(I2,J1,:)*X2Y1RATIOp
     &                     + VAReta(I2,J2,:)*X2Y2RATIOp
              
            ENDIF
 
            X1Y1RATIOu = X1Y1RATIO
            X2Y1RATIOu = X2Y1RATIO
            X1Y2RATIOu = X1Y2RATIO
            X2Y2RATIOu = X2Y2RATIO
            IF((Ueta(I1,J1).eq.FILL).OR.(Veta(I1,J1).eq.FILL))THEN
               CORR=1.0e0-X1Y1RATIOu
               IF(CORR.EQ.0.) CORR=FILL*FILL
               X1Y1RATIOu = 0.e0
               X2Y1RATIOu = X2Y1RATIOu/CORR
               X1Y2RATIOu = X1Y2RATIOu/CORR
               X2Y2RATIOu = X2Y2RATIOu/CORR
            ENDIF
            IF((Ueta(I2,J1).eq.FILL).OR.(Veta(I2,J1).eq.FILL))THEN
               CORR=1.0e0-X2Y1RATIOu
               IF(CORR.EQ.0.) CORR=FILL*FILL
               X1Y1RATIOu = X1Y1RATIOu/CORR
               X2Y1RATIOu = 0.e0
               X1Y2RATIOu = X1Y2RATIOu/CORR
               X2Y2RATIOu = X2Y2RATIOu/CORR
            ENDIF
            IF((Veta(I1,J2).eq.FILL).OR.(Veta(I1,J2).eq.FILL))THEN
               CORR=1.0e0-X1Y2RATIOu
               IF(CORR.EQ.0.) CORR=FILL*FILL
               X1Y1RATIOu = X1Y1RATIOu/CORR
               X2Y1RATIOu = X2Y1RATIOu/CORR
               X1Y2RATIOu = 0.e0
               X2Y2RATIOu = X2Y2RATIOu/CORR
            ENDIF
            IF((Ueta(I2,J2).eq.FILL).OR.(Veta(I2,J2).eq.FILL))THEN
               CORR=1.0e0-X2Y2RATIOu
               IF(CORR.EQ.0.) CORR=FILL*FILL
               X1Y1RATIOu = X1Y1RATIOu/CORR
               X2Y1RATIOu = X2Y1RATIOu/CORR
               X1Y2RATIOu = X1Y2RATIOu/CORR
               X2Y2RATIOu = 0.e0
            ENDIF
            XYRATIOu_sum=X1Y1RATIOu+X2Y1RATIOu+X1Y2RATIOu+X2Y2RATIOu
            IF(XYRATIOu_sum.eq.0.0) THEN
               Ufe(NP) = FILL
               Vfe(NP) = FILL
            ELSE
               u10_awip         = Ueta(I1,J1)*X1Y1RATIOu
     &                          + Ueta(I1,J2)*X1Y2RATIOu
     &                          + Ueta(I2,J1)*X2Y1RATIOu
     &                          + Ueta(I2,J2)*X2Y2RATIOu
               v10_awip         = Veta(I1,J1)*X1Y1RATIOu
     &                          + Veta(I1,J2)*X1Y2RATIOu
     &                          + Veta(I2,J1)*X2Y1RATIOu
     &                          + Veta(I2,J2)*X2Y2RATIOu
               Ufe(NP) = u10_awip*CROT(NP) + v10_awip*SROT(NP)
               Vfe(NP) =-u10_awip*SROT(NP) + v10_awip*CROT(NP)

            ENDIF

            ! apply wind multiplier
            IF((Ufe(NP).NE.FILL).and.(Vfe(NP).NE.FILL)) THEN
               Ufe(NP) = Ufe(NP) * windMult
               Vfe(NP) = Vfe(NP) * windMult
            ENDIF

c......convert wind speed to stress
            ! jgf20100324: unless velocity was requested 
            IF (windStress.eqv..true.) THEN
               IF((Ufe(NP).EQ.FILL).or.(Vfe(NP).EQ.FILL)) THEN
                  wstress_x_fe=FILL
                  wstress_y_fe=FILL
c	          stop 'FILL VAL FOR Ufe,Vfe.'
               ELSE
                  windmag=SQRT(Ufe(NP)*Ufe(NP)+Vfe(NP)*Vfe(NP))
                  wdragco = 0.001*(0.75+0.067*windmag)
                  IF(wdragco.gt.0.003) wdragco=0.003
                  wstress_x_fe=0.001293*wdragco*Ufe(NP)*windmag
                  wstress_y_fe=0.001293*wdragco*Vfe(NP)*windmag
C           Hsu wind stress
c              q=max(.1,windmag) 
c              Cd=max(6.0e-04,(0.4/(14.56-2*log(q)))**2)
c              wstress_x_fe=RHOAIR*Cd*Ufe(NP)*windmag/RHOH2O
c              wstress_y_fe=RHOAIR*Cd*Vfe(NP)*windmag/RHOH2O
               ENDIF
            ENDIF
         ENDIF
C          WRITE(22,100) NP,Ufe(NP),Vfe(NP),(VARfe(NP,K),K=1,NCOL-2)
!        ! jgf20100324: Write out the data that was requested
         IF (WindStress.eqv..true.) THEN
            WRITE(22,100) NP,wstress_x_fe,wstress_y_fe,
     &                   (VARfe(NP,K),K=1,NCOL-2)
         ELSE
            WRITE(22,100) NP,Ufe(NP),Vfe(NP),(VARfe(NP,K),K=1,NCOL-2)
         ENDIF
      END DO  ! END DO NP
 100  FORMAT(i10,1x,16(1x,e16.6))
      CLOSE(22)

      END PROGRAM AWIP_LAMBERT_INTERP



C This routine defines the parameters for lambert-conformal projections.

      SUBROUTINE SETKGDS(GNUM,KGDS)
      
C           (2)   - NX NR POINTS ALONG X-AXIS
C           (3)   - NY NR POINTS ALONG Y-AXIS
C           (4)   - LA1 LAT OF ORIGIN (LOWER LEFT)
C           (5)   - LO1 LON OF ORIGIN (LOWER LEFT)
C           (6)   - RESOLUTION (RIGHT ADJ COPY OF OCTET 17)
C           (7)   - LOV - ORIENTATION OF GRID
C           (8)   - DX - X-DIR INCREMENT
C           (9)   - DY - Y-DIR INCREMENT
C           (10)  - PROJECTION CENTER FLAG
C           (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
C           (12)  - LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
C           (13)  - LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER
      USE GDS 
      IMPLICIT NONE
      INTEGER GNUM
      INTEGER KGDS(200)
      IF (GNUM.EQ.221)THEN
        IM=349                          
        JM=277                         
        RLAT1=1.0                     
        RLON1=214.5                  
        IROT=1                      
        ORIENT=253.0              
        DX=32463.41                
        DY=32463.41              
        IPROJ=0                         
        ISCAN=0                        
        JSCAN=1                       
        NSCAN=0                         
        RLATI1=50.0                    
        RLATI2=50.0                   
      ELSEIF (GNUM.EQ.218)THEN
        IM=614                          
        JM=428                         
        RLAT1=12.190                     
        RLON1=226.514                  
        IROT=1                      
        ORIENT=265.0              
        DX= 12190.58       ! in meters   !!   
        DY= 12190.58       ! in meters   !!  
        IPROJ=0                         
        ISCAN=0                        
        JSCAN=1                       
        NSCAN=0                         
        RLATI1=25.0                    
        RLATI2=25.0                   
      ELSEIF (GNUM.EQ.357)THEN
        IM=559                          
        JM=419                         
        RLAT1=1.698277                    
        RLON1=263.5569             
        IROT=1                      
        ORIENT=291.6              
        DX= 12000.00       ! in meters   !!   
        DY= 12000.00      ! in meters   !!  
        IPROJ=0                         
        ISCAN=0                        
        JSCAN=1                       
        NSCAN=0                         
        RLATI1=15.0                    
        RLATI2=45.0                   
      ELSE
         WRITE(*,*) 'ERROR: GNUM value "',GNUM,'" is not valid.'
         WRITE(*,*) 'Valid choices are 218, 221, or 357.'
         STOP
      END IF
      END

C-----------------------------------------------------------------------

      SUBROUTINE GDSWZD03(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &             LROT,CROT,SROT,LMAP,XLON,XLAT,YLON,YLAT,AREA,IOBFLAG)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  GDSWZD03   GDS WIZARD FOR LAMBERT CONFORMAL CONICAL
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C     THIS SUBROUTINE HAS BEEN MODIFIED SLIGHTLY SO THAT THE GRID 221
C     PARAMETERS ARE HARDWIRED IN PLACE AND NOT READ IN VIA THE KGDS
C     ARRAY.  ALSO, POINTS THAT LIE OUTSIDE GRID 221 ARE MOVED EITHER 
C     E-W OR N-S TO THE BOUNDARY OF GRID 221, THEREBY PROVIDING UNIFORM
C     MET VALUES IN THE E-W OR N-S DIRECTION OUTSIDE OF THE DOMAIN.
C
C     RICK LUETTICH       ORG: UNC IMS       DATE:2001-SEPT-10
C
C
C ABSTRACT: THIS SUBPROGRAM DECODES THE GRIB GRID DESCRIPTION SECTION
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63)
C           AND RETURNS ONE OF THE FOLLOWING:
C             (IOPT=+1) EARTH COORDINATES OF SELECTED GRID COORDINATES
C             (IOPT=-1) GRID COORDINATES OF SELECTED EARTH COORDINATES
C           FOR LAMBERT CONFORMAL CONICAL PROJECTIONS.
C           IF THE SELECTED COORDINATES ARE MORE THAN ONE GRIDPOINT
C           BEYOND THE THE EDGES OF THE GRID DOMAIN, THEN THE RELEVANT
C           OUTPUT ELEMENTS ARE SET TO FILL VALUES.
C           THE ACTUAL NUMBER OF VALID POINTS COMPUTED IS RETURNED TOO.
C           OPTIONALLY, THE VECTOR ROTATIONS AND THE MAP JACOBIANS
C           FOR THIS GRID MAY BE RETURNED AS WELL.
C
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   96-10-01  IREDELL   PROTECTED AGAINST UNRESOLVABLE POINTS
C   97-10-20  IREDELL  INCLUDE MAP OPTIONS
C 1999-04-27  GILBERT   CORRECTED MINOR ERROR CALCULATING VARIABLE AN
C                       FOR THE SECANT PROJECTION CASE (RLATI1.NE.RLATI2).
C
C USAGE:    CALL GDSWZD03(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
C    &                    LROT,CROT,SROT,LMAP,XLON,XLAT,YLON,YLAT,AREA)
C
C   INPUT ARGUMENT LIST:
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C     IOPT     - INTEGER OPTION FLAG
C                (+1 TO COMPUTE EARTH COORDS OF SELECTED GRID COORDS)
C                (-1 TO COMPUTE GRID COORDS OF SELECTED EARTH COORDS)
C     NPTS     - INTEGER MAXIMUM NUMBER OF COORDINATES
C     FILL     - REAL FILL VALUE TO SET INVALID OUTPUT DATA
C                (MUST BE IMPOSSIBLE VALUE; SUGGESTED VALUE: -9999.)
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT>0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT>0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT<0
C                (ACCEPTABLE RANGE: -360. TO 360.)
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT<0
C                (ACCEPTABLE RANGE: -90. TO 90.)
C     LROT     - INTEGER FLAG TO RETURN VECTOR ROTATIONS IF 1
C     LMAP     - INTEGER FLAG TO RETURN MAP JACOBIANS IF 1
C
C   OUTPUT ARGUMENT LIST:
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT<0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT<0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT>0
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT>0
C     NRET     - INTEGER NUMBER OF VALID POINTS COMPUTED
C     CROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION COSINES IF LROT=1
C     SROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION SINES IF LROT=1
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C     XLON     - REAL (NPTS) DX/DLON IN 1/DEGREES IF LMAP=1
C     XLAT     - REAL (NPTS) DX/DLAT IN 1/DEGREES IF LMAP=1
C     YLON     - REAL (NPTS) DY/DLON IN 1/DEGREES IF LMAP=1
C     YLAT     - REAL (NPTS) DY/DLAT IN 1/DEGREES IF LMAP=1
C     AREA     - REAL (NPTS) AREA WEIGHTS IN M**2 IF LMAP=1
C                (PROPORTIONAL TO THE SQUARE OF THE MAP FACTOR)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$


      USE GDS
      USE GLOBAL, ONLY : RERTH, PI, DPR
      INTEGER KGDS(200),IOPT,NPTS,NRET,LROT,LMAP,IOBFLAG
      REAL XPTS(NPTS),YPTS(NPTS),RLON(NPTS),RLAT(NPTS)
      REAL CROT(NPTS),SROT(NPTS)
      REAL XLON(NPTS),XLAT(NPTS),YLON(NPTS),YLAT(NPTS),AREA(NPTS)
      
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      KGDS(1)=3                         !added by RL 9/01
      IF(KGDS(1).EQ.003) THEN

c       IM=KGDS(2)                      !commented out by RL 9/01
c       JM=KGDS(3)                      !commented out by RL 9/01
c       RLAT1=KGDS(4)*1.E-3             !commented out by RL 9/01
c       RLON1=KGDS(5)*1.E-3             !commented out by RL 9/01
c       IROT=MOD(KGDS(6)/8,2)           !commented out by RL 9/01
c       ORIENT=KGDS(7)*1.E-3            !commented out by RL 9/01
c       DX=KGDS(8)                      !commented out by RL 9/01
c       DY=KGDS(9)                      !commented out by RL 9/01
c       IPROJ=MOD(KGDS(10)/128,2)       !commented out by RL 9/01
c       ISCAN=MOD(KGDS(11)/128,2)       !commented out by RL 9/01
c       JSCAN=MOD(KGDS(11)/64,2)        !commented out by RL 9/01
c       NSCAN=MOD(KGDS(11)/32,2)        !commented out by RL 9/01
c       RLATI1=KGDS(12)*1.E-3           !commented out by RL 9/01
c       RLATI2=KGDS(13)*1.E-3           !commented out by RL 9/01

     
         H=(-1.)**IPROJ
         HI=(-1.)**ISCAN
         HJ=(-1.)**(1-JSCAN)
         DXS=DX*HI
         DYS=DY*HJ
         IF(RLATI1.EQ.RLATI2) THEN
            AN=SIN(H*RLATI1/DPR)
         ELSE
            AN=LOG(COS(RLATI1/DPR)/COS(RLATI2/DPR))/
     &           LOG(TAN((H*90-RLATI1)/2/DPR)/TAN((H*90-RLATI2)/2/DPR))
         ENDIF
         DE=RERTH*COS(RLATI1/DPR)*TAN((H*RLATI1+90)/2/DPR)**AN/AN
         IF(H*RLAT1.EQ.90) THEN
            XP=1
            YP=1
         ELSE
            DR=DE/TAN((H*RLAT1+90)/2/DPR)**AN
            DLON1=MOD(RLON1-ORIENT+180+3600,360.)-180
            XP=1-H*SIN(AN*DLON1/DPR)*DR/DXS
            YP=1+COS(AN*DLON1/DPR)*DR/DYS
         ENDIF
         ANTR=1/(2*AN)
         DE2=DE**2
c        XMIN=0                          !commented out by RL 9/01
c        XMAX=IM+1                       !commented out by RL 9/01
c        YMIN=0                          !commented out by RL 9/01
c        YMAX=JM+1                       !commented out by RL 9/01
         XMIN=1                          !added by RL 9/01
         YMIN=1                          !added by RL 9/01
         XMAX=IM                         !added by RL 9/01
         YMAX=JM                         !added by RL 9/01
         NRET=0

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSLATE GRID COORDINATES TO EARTH COORDINATES
         IF(IOPT.EQ.0.OR.IOPT.EQ.1) THEN ! unreachable b/c iopt was hardcoded to -1
            DO N=1,NPTS
               IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.
     &             YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
                  DI=(XPTS(N)-XP)*DXS
                  DJ=(YPTS(N)-YP)*DYS
                  DR2=DI**2+DJ**2
                  IF(DR2.LT.DE2*1.E-6) THEN
                     RLON(N)=0.
                     RLAT(N)=H*90.
                  ELSE
                     RLON(N)=MOD(ORIENT+H/AN*DPR*ATAN2(DI,-DJ)+3600,360.)
                     RLAT(N)=H*(2*DPR*ATAN((DE2/DR2)**ANTR)-90)
                  ENDIF
                  NRET=NRET+1
                  IF(LROT.EQ.1) THEN
                     IF(IROT.EQ.1) THEN
                        DLON=MOD(RLON(N)-ORIENT+180+3600,360.)-180
                        CROT(N)=H*COS(AN*DLON/DPR)
                        SROT(N)=SIN(AN*DLON/DPR)
                     ELSE
                        CROT(N)=1
                       SROT(N)=0
                     ENDIF
                  ENDIF
                  IF(LMAP.EQ.1) THEN
                     DR=SQRT(DR2)
                     DLON=MOD(RLON(N)-ORIENT+180+3600,360.)-180
                     CLAT=COS(RLAT(N)/DPR)
                     IF(CLAT.LE.0.OR.DR.LE.0) THEN
                        XLON(N)=FILL
                        XLAT(N)=FILL
                        YLON(N)=FILL
                        YLAT(N)=FILL
                        AREA(N)=FILL
                     ELSE
                        XLON(N)=H*COS(AN*DLON/DPR)*AN/DPR*DR/DXS
                        XLAT(N)=-SIN(AN*DLON/DPR)*AN/DPR*DR/DXS/CLAT
                        YLON(N)=SIN(AN*DLON/DPR)*AN/DPR*DR/DYS
                        YLAT(N)=H*COS(AN*DLON/DPR)*AN/DPR*DR/DYS/CLAT
                        AREA(N)=RERTH**2*CLAT**2*DXS*DYS/(AN*DR)**2
                     ENDIF
                  ENDIF
               ELSE
                  RLON(N)=FILL
                  RLAT(N)=FILL
               ENDIF
            ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C           TRANSLATE EARTH COORDINATES TO GRID COORDINATES
         ELSEIF(IOPT.EQ.-1) THEN ! always executed b/c iopt hardcoded to -1 above
            DO N=1,NPTS
               IF(ABS(RLON(N)).LE.360.AND.ABS(RLAT(N)).LE.90.AND.
     &                                 H*RLAT(N).NE.-90) THEN
                  DR=DE*TAN((90-H*RLAT(N))/2/DPR)**AN
                  DLON=MOD(RLON(N)-ORIENT+180+3600,360.)-180
                  XPTS(N)=XP+H*SIN(AN*DLON/DPR)*DR/DXS
                  YPTS(N)=YP-COS(AN*DLON/DPR)*DR/DYS
                  IF(XPTS(N).LE.XMIN) XPTS(N)=XMIN+0.0001  ! added by RL 9/01
                  IF(XPTS(N).GE.XMAX) XPTS(N)=XMAX-0.0001  ! added by RL 9/01
                  IF(YPTS(N).LE.YMIN) YPTS(N)=YMIN+0.0001  ! added by RL 9/01
                  IF(YPTS(N).GE.YMAX) YPTS(N)=YMAX-0.0001  ! added by RL 9/01
                  IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.
     &               YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN

                     NRET=NRET+1
                     IF (LROT.EQ.1) THEN
                        IF(IROT.EQ.1) THEN
                           CROT(N)=H*COS(AN*DLON/DPR)
                           SROT(N)=SIN(AN*DLON/DPR)
                        ELSE
                           CROT(N)=1
                           SROT(N)=0
                        ENDIF
                     ENDIF
                     IF (LMAP.EQ.1) THEN
                        CLAT=COS(RLAT(N)/DPR)
                        IF (CLAT.LE.0.OR.DR.LE.0) THEN
                           XLON(N)=FILL
                           XLAT(N)=FILL
                           YLON(N)=FILL
                           YLAT(N)=FILL
                           AREA(N)=FILL
                        ELSE
                           XLON(N)=H*COS(AN*DLON/DPR)*AN/DPR*DR/DXS
                           XLAT(N)=-SIN(AN*DLON/DPR)*AN/DPR*DR/DXS/CLAT
                           YLON(N)=SIN(AN*DLON/DPR)*AN/DPR*DR/DYS
                           YLAT(N)=H*COS(AN*DLON/DPR)*AN/DPR*DR/DYS/CLAT
                           AREA(N)=RERTH**2*CLAT**2*DXS*DYS/(AN*DR)**2
                        ENDIF
                     ENDIF
                  ELSE
                     XPTS(N)=FILL
                     YPTS(N)=FILL
                  ENDIF
               ELSE
                  XPTS(N)=FILL
                  YPTS(N)=FILL
               ENDIF
            ENDDO
         ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PROJECTION UNRECOGNIZED
      ELSE
         NRET=-1 ! jgf20100324 changed this from the nonexistent variable IRET
         IF(IOPT.GE.0) THEN
            DO N=1,NPTS
               RLON(N)=FILL
               RLAT(N)=FILL
            ENDDO
         ENDIF
         IF(IOPT.LE.0) THEN
            DO N=1,NPTS
               XPTS(N)=FILL
               YPTS(N)=FILL
            ENDDO
         ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      END

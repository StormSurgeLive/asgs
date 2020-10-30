C PROGRAM TO COMPUTE NODAL FACTORS AND EQUILIBRIUM ARGUMENTS
C
C     jgf20110526: Added command line option processing to bypass menu
C     driven interface; menu driven interface is still the default and is
C     used if no command line options are present; added optional alternate
C     output format that just contains the name of the tidal constituent
C     and the nodal factors and equlilibrium arguments to simplify use 
C     in an automated context (scripting).
C
      PROGRAM TIDE_FAC
C     TODO: IMPLICIT NONE
      PARAMETER(NCNST=37)

      CHARACTER CNAME(NCNST)*8
      COMMON /CNSNAM/ CNAME
      ! EQUILIBRIUM ARGUMENT IS REFERENCED TO THE GRENWICH MERIDIAN
      REAL NODFAC,MONTH
      DIMENSION NCON(NCNST)
      COMMON /CNST/ NODFAC(NCNST),GRTERM(NCNST),SPEED(NCNST),P(NCNST)

      INTEGER :: ARGCOUNT ! number of command line arguments
      INTEGER :: IARGC    ! function to return command line arguments
      INTEGER :: I        ! loop counter for command line arguments
      CHARACTER(2048) :: CMDLINEARG ! content of cmd line arg
      CHARACTER(2048) :: OUTPUTDIR ! directory to place output file
      CHARACTER(len=3), dimension(12) :: monthChar ! string to represent month
      INTEGER :: numTidalConstituents  ! number of tidal const. to interpolate
      CHARACTER(len=10), ALLOCATABLE :: interpTidalConstituents(:)
      CHARACTER(len=1024), dimension(7) :: defaultTidalConstituents ! basic set of 7
      INTEGER, ALLOCATABLE :: constituentList(:)
      integer :: outputFormat ! SIMPLEOUTPUT or ADCIRCOUTPUT
      integer, parameter :: WITHHEADER=0 ! to write nodal factor and equilibrium arguments in legacy format
      integer, parameter :: SIMPLEOUTPUT=1 ! to output just the nf and eq args
      integer, parameter :: ADCIRCOUTPUT=2 ! to produce the output in format ready for insertion into fort.15 

      numTidalConstituents = 7
      outputFormat = WITHHEADER
      OUTPUTDIR = "."
      ARGCOUNT = IARGC() ! count up command line options
      IF (ARGCOUNT.gt.0) THEN
         I=0
         DO WHILE (I.lt.ARGCOUNT)
            I = I + 1
            CALL GETARG(I, CMDLINEARG)
            WRITE(*,*) "INFO: tide_fac.f: Processing ",TRIM(CMDLINEARG)
            SELECT CASE(TRIM(CMDLINEARG))
            CASE("--length")
               I = I + 1
               CALL GETARG(I,CMDLINEARG)
               READ(CMDLINEARG,*) XDAYS
            CASE("--year")
               I = I + 1
               CALL GETARG(I,CMDLINEARG)
               READ(CMDLINEARG,*) IYR
            CASE("--month")
               I = I + 1
               CALL GETARG(I,CMDLINEARG)
               READ(CMDLINEARG,*) IMO
            CASE("--day")
               I = I + 1
               CALL GETARG(I,CMDLINEARG)
               READ(CMDLINEARG,*) IDAY
            case("-n","--numtidalconstituents") ! number of tidal constituents to interpolate
               i = i + 1
               call getarg(i,cmdlinearg)
               read(cmdlinearg,*) numTidalConstituents
               allocate(interpTidalConstituents(numTidalConstituents))
               ! then read this many space-separated strings on the command line
               do j=1, numTidalConstituents
                  i = i + 1
                  call getarg(i,interpTidalConstituents(j))
               end do
            CASE("--hour")
               I = I + 1
               CALL GETARG(I,CMDLINEARG)
               READ(CMDLINEARG,*) BHR
            CASE("--outputformat","--outputFormat")
               I = I + 1
               CALL GETARG(I,CMDLINEARG)
               select case(trim(CMDLINEARG))
               case("simple","SIMPLE","Simple")
                  outputFormat = SIMPLEOUTPUT
               case("adcirc","ADCIRC","Adcirc")
                  outputFormat = ADCIRCOUTPUT
               case default
                  WRITE(*,*) "ERROR: tide_fac.f: '",TRIM(CMDLINEARG),
     &               "' was not recognized as an output format."
               end select
            CASE("--outputdir")
               I = I + 1
               CALL GETARG(I,OUTPUTDIR)
            CASE DEFAULT
               WRITE(*,*) 
     &            "WARNING: tide_fac.f: Command line argument '",
     &            TRIM(CMDLINEARG),"' was not recognized."
            END SELECT
         END DO
      ELSE
         WRITE(*,*) 'ENTER LENGTH OF RUN TIME (DAYS)'
         READ(*,*) XDAYS
         WRITE(*,*)
     &      ' ENTER START TIME - BHR,IDAY,IMO,IYR (IYR e.g. 1992)'
         READ(*,*) BHR,IDAY,IMO,IYR
      ENDIF
      defaultTidalConstituents(1) = 'M2'
      defaultTidalConstituents(2) = 'S2'
      defaultTidalConstituents(3) = 'N2'
      defaultTidalConstituents(4) = 'K1'
      defaultTidalConstituents(5) = 'K2'
      defaultTidalConstituents(6) = 'O1'
      defaultTidalConstituents(7) = 'Q1'
      if (.not.allocated(interpTidalConstituents)) then
         allocate(interpTidalConstituents(numTidalConstituents))
         interpTidalConstituents(1:7) = defaultTidalConstituents(1:7)
      endif

      RHRS=XDAYS*24.
      YR=IYR
      MONTH=IMO
      DAY=IDAY
      HRM=BHR+RHRS/2.

      WRITE(*,10) BHR,IDAY,IMO,IYR
      WRITE(*,11) XDAYS

      OPEN(UNIT=11,FILE=TRIM(OUTPUTDIR)//'/tide_fac.out',
     &   STATUS='UNKNOWN')
      IF (outputFormat.eq.WITHHEADER) THEN
         WRITE(11,10) BHR,IDAY,IMO,IYR
  10     FORMAT(' TIDAL FACTORS STARTING: ', 
     &       ' HR-',F5.2,',  DAY-',I3,',  MONTH-',I3,'  YEAR-',I5,/)
         WRITE(11,11) XDAYS
  11     FORMAT(' FOR A RUN LASTING ',F8.2,' DAYS',//)
         WRITE(11,*) 'CONST   NODE     EQ ARG (ref GM)'
         WRITE(11,1300)
 1300    FORMAT(' NAME   FACTOR    (DEG) ',//)
      ENDIF

C-- DETERMINE THE JULIAN TIME AT BEGINNING AND MIDDLE OF RECORD
      DAYJ=DAYJUL(YR,MONTH,DAY)

C-- DETERMINE NODE FACTORS AT MIDDLE OF RECORD
      CALL NFACS(YR,DAYJ,HRM)

C-- DETERMINE GREENWICH EQUIL. TERMS AT BEGINNING OF RECORD
      CALL GTERMS(YR,DAYJ,BHR,DAYJ,HRM)

      data monthChar 
     & /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     & 'OCT','NOV','DEC'/
      if (outputFormat.eq.ADCIRCOUTPUT) then
         numLoops = 2
      else
         numLoops = 1
      endif
      do i=1,numLoops
         if (outputFormat.eq.ADCIRCOUTPUT) then
            ! tidal potential nodal factors and equilibrium arguments
            if (i.eq.1) then
               write(11,fmt=100)
     & numTidalConstituents, int(BHR),iday,monthChar(imo),iyr, xdays
            ! boundary forcing nodal factors and equilibrium arguments
            else
               write(11,'(i0, 6x,"! NBFR number of tidal boundary constituents, computed by ASGS with tide_fac.f")') numTidalConstituents
            endif
         endif
 100     format(i0, 6x,
     &   "! NTIF number of tidal potential constituents ! start date is ",i0,
     &   "Z ",i0,1x,a,1x,i0," ! run length is ",f6.2," days")
         do nc=1,numTidalConstituents
            select case(trim(interpTidalConstituents(nc)))
            case("k1","K1")
                ic=4
                tidalPotentialAmplitude = 0.141565d0
                tidalFrequency = 0.000072921158358d0
                earthTidePotentialReductionFactor = 0.736
            case("o1","O1")
                ic=6
                tidalPotentialAmplitude = 0.100514d0
                tidalFrequency = 0.000067597744151d0
                earthTidePotentialReductionFactor = 0.695
            case("p1","P1")
                ic=30
                tidalPotentialAmplitude = 0.046843d0
                tidalFrequency = 0.000072522945975d0
                earthTidePotentialReductionFactor = 0.706            
            case("q1","Q1")
                ic=26
                tidalPotentialAmplitude = 0.019256d0
                tidalFrequency = 0.000064958541129d0
                earthTidePotentialReductionFactor = 0.695         
            case("n2","N2")
                ic=3
                tidalPotentialAmplitude = 0.046398d0
                tidalFrequency = 0.000137879699487d0
                earthTidePotentialReductionFactor =  0.693          
            case("m2","M2")
                ic=1
                tidalPotentialAmplitude = 0.242334d0
                tidalFrequency = 0.000140518902509d0
                earthTidePotentialReductionFactor = 0.693        
            case("s2","S2")
                ic=2
                tidalPotentialAmplitude = 0.112841d0
                tidalFrequency = 0.000145444104333d0
                earthTidePotentialReductionFactor = 0.693           
            case("k2","K2")
                ic=35
                tidalPotentialAmplitude = 0.030704
                tidalFrequency = 0.000145842317201d0
                earthTidePotentialReductionFactor = 0.693                         
            case default
                write(6,*) 'ERROR: The tidal constituent '//trim(cname(ic))//' was not recognized.'
            end select
            if (outputFormat.eq.ADCIRCOUTPUT) then
               if (i.eq.1) then
                  ! tidal potential constituents
                  write(11,'(a)') trim(cname(ic))
                  write(11,'(f9.6,2x,f18.14,2x,f7.3,2x,f9.5,2x,f7.2)') 
     &               tidalPotentialAmplitude,tidalFrequency,
     &               earthTidePotentialReductionFactor, nodfac(ic), grterm(ic)
                  ! tidal boundary constituents
               else
                  write(11,'(a)') trim(cname(ic))
                  write(11,'(f18.14,2x,f9.5,2x,f7.2)') 
     &               tidalFrequency, nodfac(ic), grterm(ic)
               endif
            else
               WRITE(11,2001) CNAME(IC),NODFAC(IC),GRTERM(IC)
            endif
         end do
      end do

 2001 FORMAT(1X,A4,2x,F9.5,4x,F7.2,2x,F7.4)

      STOP
C---------------------------------------------------------------------      
      END PROGRAM TIDE_FAC
C---------------------------------------------------------------------      



C---------------------------------------------------------------------
C-- CALCULATES NODE FACTORS FOR CONSTITUENT TIDAL SIGNAL

C-- THE EQUATIONS USED IN THIS ROUTINE COME FROM:
C         "MANUAL OF HARMONIC ANALYSIS AND PREDICTION OF TIDES"
C         BY PAUL SCHUREMAN, SPECIAL PUBLICATION #98, US COAST
C         AND GEODETIC SURVEY, DEPARTMENT OF COMMERCE (1958).

C-- IF DAYM AND HRM CORRESPOND TO MIDYEAR, THEN THIS ROUTINE
C-- RETURNS THE SAME VALUES AS FOUND IN TABLE 14 OF SCHUREMAN.
C---------------------------------------------------------------------
      SUBROUTINE NFACS(YR,DAYJ,HR)
      CHARACTER*8   CST(37)
      REAL          I,N,NU

      COMMON/ORBITF/DS,DP,DH,DP1,DN,DI,DNU,DXI,DNUP,DNUP2,DPC
      COMMON/ CNST /FNDCST(37),EQCST(37),ACST(37),PCST(37)
      COMMON/CNSNAM/CST

C-- CONSTITUENT NAMES:
      DATA CST     /'M2      ','S2      ','N2      ','K1      ',
     *              'M4      ','O1      ','M6      ','MK3     ',
     *              'S4      ','MN4     ','NU2     ','S6      ',
     *              'MU2     ','2N2     ','OO1     ','LAMBDA2 ',
     *              'S1      ','M1      ','J1      ','MM      ',
     *              'SSA     ','SA      ','MSF     ','MF      ',
     *              'RHO1    ','Q1      ','T2      ','R2      ',
     *              '2Q1     ','P1      ','2SM2    ','M3      ',
     *              'L2      ','2MK3    ','K2      ','M8      ',
     *              'MS4     '/

C-- ORBITAL SPEEDS (DEGREES/HOUR):
      DATA ACST/28.9841042,30.0,28.4397295,15.0410686,57.9682084,
     *13.9430356,86.9523127,44.0251729,60.0,57.4238337,28.5125831,90.0,
     *27.9682084,27.8953548,16.1391017,29.4556253,15.0,14.4966939,
     *15.5854433,0.5443747,0.0821373,0.0410686,1.0158958,1.0980331,
     *13.4715145,13.3986609,29.9589333,30.0410667,12.8542862,14.9589314,
     *31.0158958,43.4761563,29.5284789,42.9271398,30.0821373,
     *115.9364169,58.9841042/

C-- NUMBER OF TIDE CYCLES PER DAY PER CONSTITUENT:
      DATA PCST/2.,2.,2.,1.,4.,1.,6.,3.,4.,4.,2.,6.,2.,2.,1.,2.,1.,1.,
     $1.,0.,0.,0.,0.,0.,1.,1.,2.,2.,1.,1.,2.,3.,2.,3.,2.,8.,4./

      PI180=3.14159265/180.
      CALL ORBIT(YR,DAYJ,HR)
      N=DN*PI180
      I=DI*PI180
      NU=DNU*PI180
      XI=DXI*PI180
      P=DP*PI180
      PC=DPC*PI180
      SINI=SIN(I)
      SINI2=SIN(I/2.)
      SIN2I=SIN(2.*I)
      COSI2=COS(I/2.)
      TANI2=TAN(I/2.)
C-- EQUATION 197, SCHUREMAN
      QAINV=SQRT(2.310+1.435*COS(2.*PC))
C-- EQUATION 213, SCHUREMAN
      RAINV=SQRT(1.-12.*TANI2**2*COS(2.*PC)+36.*TANI2**4)
C-- VARIABLE NAMES REFER TO EQUATION NUMBERS IN SCHUREMAN
      EQ73=(2./3.-SINI**2)/.5021
      EQ74=SINI**2/.1578
      EQ75=SINI*COSI2**2/.37988
      EQ76=SIN(2*I)/.7214
      EQ77=SINI*SINI2**2/.0164
      EQ78=(COSI2**4)/.91544
      EQ149=COSI2**6/.8758
      EQ207=EQ75*QAINV
      EQ215=EQ78*RAINV
      EQ227=SQRT(.8965*SIN2I**2+.6001*SIN2I*COS(NU)+.1006)
      EQ235=.001+SQRT(19.0444*SINI**4+2.7702*SINI**2*COS(2.*NU)+.0981)
C-- NODE FACTORS FOR 37 CONSTITUENTS:
      FNDCST(1)=EQ78
      FNDCST(2)=1.0
      FNDCST(3)=EQ78
      FNDCST(4)=EQ227
      FNDCST(5)=FNDCST(1)**2
      FNDCST(6)=EQ75
      FNDCST(7)=FNDCST(1)**3
      FNDCST(8)=FNDCST(1)*FNDCST(4)
      FNDCST(9)=1.0
      FNDCST(10)=FNDCST(1)**2
      FNDCST(11)=EQ78
      FNDCST(12)=1.0
      FNDCST(13)=EQ78
      FNDCST(14)=EQ78
      FNDCST(15)=EQ77
      FNDCST(16)=EQ78
      FNDCST(17)=1.0
C** EQUATION 207 NOT PRODUCING CORRECT ANSWER FOR M1
C**SET NODE FACTOR FOR M1 = 0 UNTIL CAN FURTHER RESEARCH
      FNDCST(18)=0.
C     FNDCST(18)=EQ207
      FNDCST(19)=EQ76
      FNDCST(20)=EQ73
      FNDCST(21)=1.0
      FNDCST(22)=1.0
      FNDCST(23)=EQ78
      FNDCST(24)=EQ74
      FNDCST(25)=EQ75
      FNDCST(26)=EQ75
      FNDCST(27)=1.0
      FNDCST(28)=1.0
      FNDCST(29)=EQ75
      FNDCST(30)=1.0
      FNDCST(31)=EQ78
      FNDCST(32)=EQ149
C** EQUATION 215 NOT PRODUCING CORRECT ANSWER FOR L2
C** SET NODE FACTOR FOR L2 = 0 UNTIL CAN FURTHER RESEARCH
      FNDCST(33)=0.
C     FNDCST(33)=EQ215
      FNDCST(34)=FNDCST(1)**2*FNDCST(4)
      FNDCST(35)=EQ235
      FNDCST(36)=FNDCST(1)**4
      FNDCST(37)=EQ78
C---------------------------------------------------------------------
      END SUBROUTINE NFACS
C---------------------------------------------------------------------      


C---------------------------------------------------------------------
C-- CALCULATES EQUILIBRIUM ARGUMENTS V0+U FOR CONSTITUENT TIDE
C
C-- THE EQUATIONS USED IN THIS ROUTINE COME FROM:
C         "MANUAL OF HARMONIC ANALYSIS AND PREDICTION OF TIDES"
C         BY PAUL SCHUREMAN, SPECIAL PUBLICATION #98, US COAST
C         AND GEODETIC SURVEY, DEPARTMENT OF COMMERCE (1958).
C
C-- IF DAYM AND HRM CORRESPOND TO MIDYEAR, THEN THIS ROUTINE
C-- RETURNS THE SAME VALUES AS FOUND IN TABLE 15 OF SCHUREMAN.
C---------------------------------------------------------------------
      SUBROUTINE GTERMS(YR,DAYJ,HR,DAYM,HRM)
      REAL NU,NUP,NUP2,I
      COMMON /ORBITF/DS,DP,DH,DP1,DN,DI,DNU,DXI,DNUP,DNUP2,DPC
      COMMON /CNST/ FNDCST(37),EQCST(37),ACST(37),PCST(37)
      PI180=3.14159265/180.
C* OBTAINING ORBITAL VALUES AT BEGINNING OF SERIES FOR V0
      CALL ORBIT(YR,DAYJ,HR)
      S=DS
      P=DP
      H=DH
      P1=DP1
      T=ANGLE(180.+HR*(360./24.))
C** OBTAINING ORBITAL VALUES AT MIDDLE OF SERIES FOR U
      CALL ORBIT(YR,DAYM,HRM)
      NU=DNU
      XI=DXI
      NUP=DNUP
      NUP2=DNUP2
C* SUMMING TERMS TO OBTAIN EQUILIBRIUM ARGUMENTS
      EQCST(1)=2.*(T-S+H)+2.*(XI-NU)
      EQCST(2)=2.*T
      EQCST(3)=2.*(T+H)-3.*S+P+2.*(XI-NU)
      EQCST(4)=T+H-90.-NUP
      EQCST(5)=4.*(T-S+H)+4.*(XI-NU)
      EQCST(6)=T-2.*S+H+90.+2.*XI-NU
      EQCST(7)=6.*(T-S+H)+6.*(XI-NU)
      EQCST(8)=3.*(T+H)-2.*S-90.+2.*(XI-NU)-NUP
      EQCST(9)=4.*T
      EQCST(10)=4.*(T+H)-5.*S+P+4.*(XI-NU)
      EQCST(11)=2.*T-3.*S+4.*H-P+2.*(XI-NU)
      EQCST(12)=6.*T
      EQCST(13)=2.*(T+2.*(H-S))+2.*(XI-NU)
      EQCST(14)=2.*(T-2.*S+H+P)+2.*(XI-NU)
      EQCST(15)=T+2.*S+H-90.-2.*XI-NU
      EQCST(16)=2.*T-S+P+180.+2.*(XI-NU)
      EQCST(17)=T
      I=DI*PI180
      PC=DPC*PI180
      TOP=(5.*COS(I)-1.)*SIN(PC)
      BOTTOM=(7.*COS(I)+1.)*COS(PC)
      Q=ARCTAN(TOP,BOTTOM,1)
      EQCST(18)=T-S+H-90.+XI-NU+Q
      EQCST(19)=T+S+H-P-90.-NU
      EQCST(20)=S-P
      EQCST(21)=2.*H
      EQCST(22)=H
      EQCST(23)=2.*(S-H)
      EQCST(24)=2.*S-2.*XI
      EQCST(25)=T+3.*(H-S)-P+90.+2.*XI-NU
      EQCST(26)=T-3.*S+H+P+90.+2.*XI-NU
      EQCST(27)=2.*T-H+P1
      EQCST(28)=2.*T+H-P1+180.
      EQCST(29)=T-4.*S+H+2.*P+90.+2.*XI-NU
      EQCST(30)=T-H+90.
      EQCST(31)=2.*(T+S-H)+2.*(NU-XI)
      EQCST(32)=3.*(T-S+H)+3.*(XI-NU)
      R=SIN(2.*PC)/((1./6.)*(1./TAN(.5*I))**2-COS(2.*PC))
      R=ATAN(R)/PI180
      EQCST(33)=2.*(T+H)-S-P+180.+2.*(XI-NU)-R
      EQCST(34)=3.*(T+H)-4.*S+90.+4.*(XI-NU)+NUP
      EQCST(35)=2.*(T+H)-2.*NUP2
      EQCST(36)=8.*(T-S+H)+8.*(XI-NU)
      EQCST(37)=2.*(2.*T-S+H)+2.*(XI-NU)
      DO IH=1,37
         EQCST(IH)=ANGLE(EQCST(IH))
      END DO 
C---------------------------------------------------------------------
      END SUBROUTINE GTERMS
C---------------------------------------------------------------------
 

C---------------------------------------------------------------------
C-- DETERMINATION OF PRIMARY AND SECONDARY ORBITAL FUNCTIONS
C
C-- THE EQUATIONS PROGRAMMED HERE ARE NOT REPRESENTED BY EQUATIONS IN
C   SCHUREMAN.  THE CODING IN THIS ROUTINE DERIVES FROM A PROGRAM BY
C   THE NATIONAL OCEANIC AND ATMOSPHERIC ADMINISTRATION (NOAA).
C   HOWEVER, TABULAR VALUES OF THE ORBITAL FUNCTIONS CAN BE FOUND IN
C   TABLE 1 OF SCHUREMAN.
C---------------------------------------------------------------------
      SUBROUTINE ORBIT(YR,DAYJ,HR)
      REAL I,N,NU,NUP,NUP2
      COMMON /ORBITF/DS,DP,DH,DP1,DN,DI,DNU,DXI,DNUP,DNUP2,DPC

      PI180=3.14159265/180.
      X=AINT((YR-1901.)/4.)
      DYR=YR-1900.
      DDAY=DAYJ+X-1.
C-- DN IS THE MOON'S NODE (CAPITAL N, TABLE 1, SCHUREMAN)
      DN=259.1560564-19.328185764*DYR-.0529539336*DDAY-.0022064139*HR
      DN=ANGLE(DN)
      N=DN*PI180
C-- DP IS THE LUNAR PERIGEE (SMALL P, TABLE 1)
      DP=334.3837214+40.66246584*DYR+.111404016*DDAY+.004641834*HR
      DP=ANGLE(DP)
      P=DP*PI180
      I=ACOS(.9136949-.0356926*COS(N))
      DI=ANGLE(I/PI180)
      NU=ASIN(.0897056*SIN(N)/SIN(I))
      DNU=NU/PI180
      XI=N-2.*ATAN(.64412*TAN(N/2.))-NU
      DXI=XI/PI180
      DPC=ANGLE(DP-DXI)
C-- DH IS THE MEAN LONGITUDE OF THE SUN (SMALL H, TABLE 1)
      DH=280.1895014-.238724988*DYR+.9856473288*DDAY+.0410686387*HR
      DH=ANGLE(DH)
C-- DP1 IS THE SOLAR PERIGEE (SMALL P1, TABLE 1)
      DP1=281.2208569+.01717836*DYR+.000047064*DDAY+.000001961*HR
      DP1=ANGLE(DP1)
C-- DS IS THE MEAN LONGITUDE OF THE MOON (SMALL S, TABLE 1)
      DS=277.0256206+129.38482032*DYR+13.176396768*DDAY+.549016532*HR
      DS=ANGLE(DS)
      NUP=ATAN(SIN(NU)/(COS(NU)+.334766/SIN(2.*I)))
      DNUP=NUP/PI180
      NUP2=ATAN(SIN(2.*NU)/(COS(2.*NU)+.0726184/SIN(I)**2))/2.
      DNUP2=NUP2/PI180
C---------------------------------------------------------------------
      END SUBROUTINE ORBIT
C---------------------------------------------------------------------

C---------------------------------------------------------------------
C
C*** THIS ROUTINE PLACES AN ANGLE IN 0-360 (+) FORMAT
C
C---------------------------------------------------------------------
      FUNCTION ANGLE(ARG)
      M=-IFIX(ARG/360.)
      ANGLE=ARG+FLOAT(M)*360.
      IF(ANGLE .LT. 0.) ANGLE=ANGLE+360.
C---------------------------------------------------------------------
      END
C---------------------------------------------------------------------      
      

C---------------------------------------------------------------------
C** DETERMINE ARCTANGENT AND PLACE IN CORRECT QUADRANT
C   IF KEY EQ 0  NO QUADRANT SELECTION MADE
C   IF KEY .NE. 0 PROPER QUADRANT IS SELECTED
C---------------------------------------------------------------------
      ! jgf: doing the minimum to eliminate the arithmetic if 
      ! from this pile of spaghetti :-)
      ! (the arithmetic if is a fortran deleted feature and 
      ! its use produces warnings in gfortran 9.3.0)
      FUNCTION ARCTAN(TOP,BOTTOM,KEY)
      IF(BOTTOM .NE. 0.0) GO TO 4
      IF (TOP.lt.0.) goto 2
      if (top.eq.0.) goto 9
      if (top.gt.0.) goto 3
    2 ARCTAN=270.
      RETURN
    3 ARCTAN=90.
      RETURN
    4 ARCTAN=ATAN(TOP/BOTTOM)*57.2957795
      IF(KEY.EQ.0) RETURN
      IF (TOP.lt.0.) goto 5
      if (top.eq.0.) goto 5
      if (top.gt.0.) goto 7
    5 IF (BOTTOM.lt.0.) goto 6
      if (bottom.eq.0.) goto 9
      if (bottom.gt.0.) goto 8
    6 ARCTAN=ARCTAN+180.
      RETURN
    7 IF (BOTTOM.lt.0.) goto 6
      if (bottom.eq.0.) goto 3
      if (bottom.gt.0.) goto 10
    8 ARCTAN=ARCTAN+360.
      RETURN
    9 ARCTAN=0.
   10 RETURN
C---------------------------------------------------------------------
      END
C---------------------------------------------------------------------


C---------------------------------------------------------------------
C
C*** THIS ROUTINE COMPUTES THE JULIAN DAY (AS A REAL VARIABLE)
C
C---------------------------------------------------------------------
      FUNCTION DAYJUL(YR,XMONTH,DAY)
      DIMENSION DAYT(12),DAYS(12)
      DATA DAYT/0.,31.,59.,90.,120.,151.,181.,212.,243.,273.,304.,334./
      DATA DAYS(1),DAYS(2) /0.,31./
      DINC=0.
      YRLP=MOD((YR-1900.),4.)
      IF(YRLP .EQ. 0.) DINC=1.
      DO I=3,12
         DAYS(I)=DAYT(I)+DINC
      END DO
      DAYJUL=DAYS(IFIX(XMONTH))+DAY
C---------------------------------------------------------------------
      END
C---------------------------------------------------------------------

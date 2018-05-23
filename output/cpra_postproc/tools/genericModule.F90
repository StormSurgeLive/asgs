


        MODULE GENERICMODULE
            IMPLICIT NONE

            CONTAINS

            LOGICAL FUNCTION FindFile(filename,returnfalse)
                CHARACTER(*),INTENT(IN) :: Filename
                LOGICAL,INTENT(IN),OPTIONAL :: returnfalse
                LOGICAL                 :: exists

                INQUIRE(FILE=TRIM(filename),EXIST=exists)

                FindFile=.TRUE.
                IF(.NOT.exists)THEN
                    IF(PRESENT(RETURNFALSE))THEN
                        IF(returnfalse)THEN
                            FindFile=.FALSE.
                            RETURN
                        ELSE
                            WRITE(*,'(3A)') "Specified file ",TRIM(filename),&
                                " does not exist."
                            STOP
                        ENDIF
                    ELSE
                        WRITE(*,'(3A)') "Specified file ",TRIM(filename),&
                            " does not exist."
                        STOP
                    ENDIF
                ENDIF
            END FUNCTION
        

            INTEGER FUNCTION GETFREEUNIT()
                INTEGER :: I
                LOGICAL :: ISOPEN
                I = 0
                DO
                    I = I + 1
                    INQUIRE(UNIT=I,OPENED=ISOPEN)
                    IF(ISOPEN)CYCLE
                    GETFREEUNIT = I
                    EXIT
                ENDDO
                RETURN
            END FUNCTION GETFREEUNIT
           
#ifdef _NETCDF           
            SUBROUTINE CHECK(stat,error,fatal)
                USE NETCDF
                IMPLICIT NONE
                INTEGER,INTENT(IN)             :: stat
                LOGICAL,INTENT(OUT),OPTIONAL   :: error
                LOGICAL,INTENT(IN),OPTIONAL    :: fatal
                LOGICAL                        :: fatal_local
                INTEGER,ALLOCATABLE            :: Dmy(:)

                IF(.NOT.PRESENT(FATAL))THEN
                    FATAL_LOCAL = .TRUE.
                ELSE
                    FATAL_LOCAL = FATAL
                ENDIF
                
                IF(fatal_local)THEN
                    IF(stat.NE.NF90_NOERR)THEN
                        WRITE(*,'(A,A)') "FATAL ERROR from ",TRIM(NF90_STRERROR(stat))
#ifdef EBUG
                        !...Intentional segfault to trigger stack trace
                        Dmy(1) = 1.0D0
#endif
                        STOP
                    ELSE
                        IF(PRESENT(error))error = .FALSE.
                    ENDIF
                ELSE
                    IF(stat.NE.NF90_NOERR)THEN
                        IF(PRESENT(error))error = .TRUE.
                    ELSE
                        IF(PRESENT(error))error = .FALSE.
                    ENDIF
                ENDIF
            END SUBROUTINE CHECK
#endif            
            
            !>Distance function that calculates either spherical distance
            !>using the Haversine formula (default) or optionally cartesian distance.
            !>When spherical distance is calculated, the radius of the earth is 
            !>estimated using the RADIUS_EARTH routine. Distances are returned
            !>in meters when calculating spherical distance or in native 
            !>units when using the cartesian option.
            !>\author Zach Cobell
            REAL(8) FUNCTION distance(lon1,lat1,lon2,lat2,CARTESIAN)
                IMPLICIT NONE

                INTRINSIC  :: DATAN
                INTRINSIC  :: DATAN2
                INTRINSIC  :: DSIN
                INTRINSIC  :: DCOS
                INTRINSIC  :: DSQRT

                !>Option to use calculate cartesian distance instead of spherical
                LOGICAL,INTENT(IN),OPTIONAL ::  CARTESIAN !...Toggle between spherical or cartesian coordinates
                !>Y value 1 for calculation
                REAL(8),INTENT(IN)          ::  lat1
                !>Y value 2 for calculation
                REAL(8),INTENT(IN)          ::  lat2
                !>X value 1 for calculation
                REAL(8),INTENT(IN)          ::  lon1
                !>X value 2 for calculation
                REAL(8),INTENT(IN)          ::  lon2

                REAL(8),PARAMETER           ::  R = 6371.64d0
                REAL(8)                     ::  a
                REAL(8)                     ::  c
                REAL(8)                     ::  dlon
                REAL(8)                     ::  dlat
                REAL(8)                     ::  rad2deg
                INTEGER                     ::  k


                IF(PRESENT(CARTESIAN))THEN
                    IF(CARTESIAN)THEN
                        DISTANCE = DSQRT( (lon2-lon1)**2D0 + (lat2-lat1)**2D0 )
                    ELSE
#if 0
                        rad2deg = 45.d0/DATAN(1.d0)
                        dlon = (lon2-lon1)/rad2deg
                        dlat = (lat2-lat1)/rad2deg
                        a = DSIN(dlat/2.d0)*DSIN(dlat/2.d0)
                        a = a+DCOS(lat1/rad2deg)*DCOS(lat2/rad2deg)*DSIN(dlon/2.d0)**2D0
                        c = 2.0*DATAN2(DSQRT(a),DSQRT(1.d0-a))
                        distance = R*c
#else
                        distance = HAVERSINE(lon1,lat1,lon2,lat2)
#endif
                    ENDIF
                ELSE
#if 0
                    rad2deg = 45.d0/DATAN(1.d0)
                    dlon = (lon2-lon1)/rad2deg
                    dlat = (lat2-lat1)/rad2deg
                    a = DSIN(dlat/2.d0)*DSIN(dlat/2.d0)
                    a = a+DCOS(lat1/rad2deg)*DCOS(lat2/rad2deg)*DSIN(dlon/2.d0)**2D0
                    c = 2.0*DATAN2(DSQRT(a),DSQRT(1.d0-a))
                    distance = R*c
#else
                    distance = HAVERSINE(lon1,lat1,lon2,lat2)
#endif
                ENDIF

                RETURN

            END FUNCTION

            !>Function to calculate the azimuth between two points on 
            !>a sphere.
            !>\author Zach Cobell
            REAL(8) FUNCTION Azimuth(Lon1d,Lat1d,Lon2d,Lat2d,Convention)
                IMPLICIT NONE

                INTRINSIC   :: DSIN
                INTRINSIC   :: DCOS
                INTRINSIC   :: DATAN2
                INTRINSIC   :: DSQRT

                !>Option to select either NAUTICAL convention or 
                !>cartesian convention (default). Cartesian selected
                !>if this parameter is not included.
                CHARACTER(*),INTENT(IN),OPTIONAL :: Convention
                !>Y value 1 for calculation in decimal degrees
                REAL(8),INTENT(IN) ::  Lat1d
                !>Y value 2 for calculation in decimal degrees
                REAL(8),INTENT(IN) ::  Lat2d
                !>X value 1 for calculation in decimal degrees
                REAL(8),INTENT(IN) ::  Lon1d
                !>X value 2 for calculation in decimal degrees
                REAL(8),INTENT(IN) ::  Lon2d
                
                REAL(8)     ::  pi
                REAL(8)     ::  rad
                REAL(8)     ::  Lat1
                REAL(8)     ::  Lat2
                REAL(8)     ::  Lon1
                REAL(8)     ::  Lon2
                REAL(8)     ::  meanlat
                REAL(8)     ::  latdif
                REAL(8)     ::  londif
                REAL(8)     ::  a1
                REAL(8)     ::  b1
                REAL(8)     ::  e
                REAL(8)     ::  mrcurt
                REAL(8)     ::  prcurt
                REAL(8)     ::  A
                REAL(8)     ::  B
                REAL(8)     ::  Az

                pi = 4D0*DATAN2(1d0,1d0)
                rad = 180.0d0 / pi

                Lat1 = Lat1d / rad
                Lat2 = Lat2d / rad
                Lon1 = Lon1d / rad
                Lon2 = Lon2d / rad

                latdif = lat1 - lat2
                londif = lon1 - lon2
                meanlat = (lat1 + lat2) / 2d0

                a1 = 6377276.3450d0
                b1 = 6356075.4131d0
                e = DSQRT(((a1**2d0)-(b1**2d0))/(a1**2d0))
                mrcurt = (a1*(1d0-e**2d0)) / &
                    ( 1d0 - (e**2d0)*(DSIN(meanlat))**2d0)**(3d0/2d0)

                prcurt = a1 / DSQRT(1d0-(e*DSIN(meanlat))**2d0)
                A = 2d0*DATAN2(londif*((prcurt/mrcurt)*(DCOS(meanlat))),latdif)
                B = londif*(DSIN(meanlat))

                Az = (A-B)/2.0d0

                IF(PRESENT(Convention))THEN
                    IF(TRIM(Convention).EQ.'NAUTICAL')THEN
                        IF((londif.GT.0d0).AND.(latdif.LT.0d0))THEN
                            Az = Az + pi
                        ELSEIF((londif.LT.0d0).AND.(LatDif.LT.0d0))THEN
                            Az = Az + pi
                        ELSEIF((londif.LT.0d0).AND.(LatDif.GT.0d0))THEN
                            Az = Az + 2d0*pi
                        ENDIF
                        Azimuth = Az*rad
                    ELSE
                         Azimuth = Az*rad + 180.0D0
                    ENDIF
                ELSE
                    IF((londif.GT.0d0).AND.(latdif.LT.0d0))THEN
                        Az = Az + pi
                    ELSEIF((londif.LT.0d0).AND.(LatDif.LT.0d0))THEN
                        Az = Az + pi
                    ELSEIF((londif.LT.0d0).AND.(LatDif.GT.0d0))THEN
                        Az = Az + 2d0*pi
                    ENDIF
                    Azimuth = Az*rad
                ENDIF

                RETURN

            END FUNCTION

            !>Function to approximate the radius of the earth using the
            !>polar and equitorial radii. 
            !>\author Zach Cobell
            REAL(8) FUNCTION RADIUS_EARTH(LATITUDE)
                IMPLICIT NONE

                REAL(8),PARAMETER  :: ER = 6378137.0D0
                REAL(8),PARAMETER  :: PR = 6356752.3D0
                
                !>Latitude to approxaimte the Earth's radius
                REAL(8),INTENT(IN) :: LATITUDE

                REAL(8)            :: LAT
                REAL(8)            :: DEG2RAD
                REAL(8)            :: PI

                PI      = 4D0*ATAN2(1D0,1D0)
                DEG2RAD = PI / 180D0
                LAT     = LATITUDE * DEG2RAD

                RADIUS_EARTH = SQRT(                     &
                    (ER**4D0*COS(LAT)*COS(LAT) +         &
                     PR**4D0*SIN(LAT)*SIN(LAT))          &
                    /                                    &
                    (ER**2D0*COS(LAT)*COS(LAT) +         &
                     PR**2D0*SIN(LAT)*SIN(LAT) ) )

                RETURN

            END FUNCTION

            !>Function to use the haversine formula, which calculates
            !>the distance on a sphere. This routine specifically
            !>calculates radii on the Earth. The radius of the earth
            !>is approximated using the RADIUS_EARTH routine
            !>\author Zach Cobell
            REAL(8) FUNCTION HAVERSINE(LON1d,LAT1d,LON2d,LAT2d)
                IMPLICIT NONE
                !>X value 1 for calculation in decimal degrees
                REAL(8),INTENT(IN) :: LON1d
                !>Y value 1 for calculation in decimal degrees
                REAL(8),INTENT(IN) :: LAT1d
                !>X value 2 for calculation in decimal degrees
                REAL(8),INTENT(IN) :: LON2d
                !>Y value 2 for calculation in decimal degrees
                REAL(8),INTENT(IN) :: LAT2d
                REAL(8)            :: LON1
                REAL(8)            :: LAT1
                REAL(8)            :: LON2
                REAL(8)            :: LAT2
                REAL(8)            :: RADIUS
                REAL(8)            :: PI
                REAL(8)            :: DEG2RAD

                PI       = 4D0*ATAN2(1D0,1D0)
                DEG2RAD  = PI / 180D0
                RADIUS   = RADIUS_EARTH((LAT1d+LAT2d)/2D0)
                LAT1     = LAT1d*DEG2RAD
                LAT2     = LAT2d*DEG2RAD
                LON1     = LON1d*DEG2RAD
                LON2     = LON2d*DEG2RAD

                HAVERSINE = 2D0*RADIUS*ASIN(SQRT(                      &
                    SIN( (LAT2-LAT1)/2D0 )**2D0 +                      &
                    COS(LAT1)*COS(LAT2)*SIN( (LON2-LON1)/2D0)**2D0 ) )

                RETURN

            END FUNCTION


        END MODULE GENERICMODULE

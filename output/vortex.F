      !=================================================================
      !=================================================================
      !=================================================================
      !      =====                                           =====
      !      =====               MODULE vortex               =====
      !      =====                                           =====
      !=================================================================
      !=================================================================
      !=================================================================

      !=================================================================
      ! This module allows one to create an asymmetric hurricane vortex
      ! object from National Hurricane Center forecast advisories, then
      ! compute wind and pressure fields suitable for forcing a storm
      ! surge forecast model. The shape and intensity of the vortex can
      ! be continuously adjusted during the forecast period, as it moves
      ! through the model domain.
      !
      ! Revision history:
      !    Date        Programmer                 Description of change
      !    ----        ----------                 ---------------------
      !    05/23/06    Craig  Mattocks, UNC-CEP   Wrote original code
      !    06/30/06    Cristina Forbes, UNC-CEP   Tested in ADCIRC model
      !    08/25/06    Craig  Mattocks, UNC-CEP   Applied wind reduction:
      !                                           top of SFC layer -> SFC
      !                                           in subroutine uvp.
      !    09/12/06    Craig  Mattocks, UNC-CEP   Subtracted translational
      !                                           wind speed from Vmax in
      !                                           nws9get.
      !    05/12/09    Robert Weaver, UNC-IMS     
      !                              Modified damping of translational 
      !                              velocity based on ratio of V/Vmax
      !    05/19/09    Cristina Forbes, UNC-IMS   
      !                              Implemented old bug fix: units conversion 
      !    05/22/09    Cristina Forbes, UNC-IMS   
      !                              Reverted damping of translational
      !                              velocity back to the original formulation
      !                              developed by Craig Mattocks
      !    05/26/09    Cristina Forbes, UNC-IMS   
      !                              Fixed implementation of Rick Luettich 
      !                              V/Vmax tapering formulation (as in NWS=8) 
      !                              for future experimentation - not activated
      !    06/2009      Robert Weaver, UNC-IMS    
      !                              changed the gradient wind formula to 
      !                              use Vmax instead of pressure difference
      !                              to enable easier manipulation of storm 
      !                              characteristics
      !    07/2009      Robert Weaver, UNC-IMS    
      !                          using modules from this code, 
      !                          wrote a preprocessor to compute Rmax and B
      !                          prior to running adcirc, and write out an
      !                          input file like the ATCF file with these
      !                          variables to make it easier to manipulate
      !                          the storm parameters.  Changed this code
      !                          to read in that input file and use the values
      !                          found there 
      !                                           
      !
      !=================================================================
      MODULE vortex
      USE precision
      USE constants         ! Import global constants
      IMPLICIT NONE
      SAVE
 
      PUBLIC :: newVortex, calcRmaxes, fitRmaxes,
     &          Rmw, Vh, uvp, uvtrans,
     &          latlon2xy, xy2latlon, 
     &          getShapeParameter   ,
     &          setShapeParameter   , 
     &          getLatestRmax,
     &          getLatestAngle      , 
     &          getUseQuadrantVr    , getRmaxes,
     &          setUseQuadrantVr    , setRmaxes,
     &          setIsotachWindSpeed ,
     &          setIsotachWindSpeeds, setIsotachRadii

      
      PRIVATE

      INTEGER, PARAMETER :: nQuads  = 4       ! Number of quadrants for
                                                 ! which wind radii are
                                                 ! provided
      INTEGER, PARAMETER :: nPoints = nQuads+2! Number of (theta, Rmax)
                                                 ! points for curve fit
      REAL(sz), DIMENSION(nPoints) :: Rmaxes    ! Radius of maximum winds
                                                 ! (nautical miles)
      REAL(sz) :: Pn                          ! Ambient surface pressure (mb)
      REAL(sz) :: Pc                          ! Surface pressure at center of
                                              ! storm (mb)
      REAL(sz) :: cLat                        ! Latitude  of storm center
                                              ! (degrees north)
      REAL(sz) :: cLon                        ! Longitude of storm center
                                              ! (degrees east )
      REAL(sz) :: Vmax                        ! Max sustained wind velocity
                                              ! in storm (knots)
      REAL(sz) :: B                           ! Exponential shape parameter
      REAL(sz) :: corio                       ! Coriolis force (1/s)
      REAL(sz) :: Vr                          ! Velocity @ wind radii (knots)
      REAL(sz), DIMENSION(nQuads) :: VrQuadrant
      REAL(sz), DIMENSION(nQuads) :: radius   ! Wind radii - the distance
                                              ! winds of velocity Vr extend
                                              ! outward from center of storm
                                              ! (nautical miles)
      INTEGER  :: quad                        ! Quadrant counter
     
      REAL(sz) :: latestRmax                  ! most recently calculated
                                              ! value of fitted rmax
      REAL(sz) :: latestAngle                 ! angle of the most recently
                                              ! calculated node w.r.t. the
                                              ! storm location
      LOGICAL :: useQuadrantVr

      CONTAINS

      !=================
            ! Vortex functions
            !=================

            !=================================================================
            ! Create a new Vortex object.
            !
            ! On input:
            !    Pn           Ambient surface pressure (mb)
            !    Pc           Surface pressure at center of storm (mb)
            !    cLat         Latitude  of storm center (degrees north)
            !    cLon         Longitude of storm center (degrees east )
            !    Vmax         Max sustained wind velocity in storm (knots)
            !
            ! On output:
            !    A new vortex is created with essential parameters calculated.
            !=================================================================
            SUBROUTINE newVortex(pinf, p0, lat, lon, vm)
               REAL(sz), INTENT(IN) :: Pinf
               REAL(sz), INTENT(IN) :: P0
               REAL(sz), INTENT(IN) :: lat
               REAL(sz), INTENT(IN) :: lon
               REAL(sz), INTENT(IN) :: vm
               ! set instance variables 
               Pn = pinf
               Pc = p0
               cLat = lat
               cLon = lon
               Vmax = vm
               ! evaluate basic physical params
               corio = two * omega * SIN(deg2rad*cLat)
               B = (Vmax*kt2ms)**2 * RhoAir * EXP(one)
     &                   / ((Pn - Pc) * mb2pa)
               B = MAX( MIN(B,twoPointFive), one) ! limit B to range 1.0->2.5

            END SUBROUTINE newVortex

            !===============================================================
            ! Calculate the radius of maximum winds for all storm quadrants.
            !
            ! On input:
            !    none
            !
            ! On output:
            !    Rmax    radius of maximum winds (nm) in all quadrants, plus
            !            2 extra values to tie down circular periodicity
            !===============================================================
            SUBROUTINE calcRmaxes()
!              REAL(sz), EXTERNAL  :: func
               REAL(sz)            :: root        ! Radius of maximum winds
               REAL(sz), PARAMETER :: innerRadius = one
               REAL(sz), PARAMETER :: outerRadius = oneHundred
               REAL(sz), PARAMETER :: accuracy    = .0001_dp
               REAL(sz), PARAMETER :: zoom        = oneHundredth
               INTEGER , PARAMETER :: itmax = 3
               REAL(sz)            :: r1,r2, r3,r4, dr
               INTEGER             :: n, iter, i
               REAL(sz) :: vicinity

              !-----------------------------
              ! Loop over quadrants of storm
              !-----------------------------
               DO n = 1, nQuads
                  quad = n
                  root = -one
!                 PRINT *, "========================================"
!                 PRINT *, "calcRmax:  Quadrant    = ", quad
!                 PRINT *, "calcRmax:  Wind radius = ", radius(quad)
                     r1 = innerRadius
                     r2 = outerRadius
                     dr = one
                     DO iter = 1, itmax
                        root = findRoot(VhWithCori, r1,r2, dr, r3,r4)
!                       PRINT *, 
!     &                 " iter ", iter,": ", r1,", ",r2,", ",dr,", ",root
                        r1 = r3
                        r2 = r4
                        dr = dr * zoom
                     END DO
!                  PRINT *, "-------------------------------------------"
!                 PRINT *, "calcRmax: findRoot Rmax = ",root
                  ! determine if Rmax is actually in the vicinity of the 
                  ! isotach radius that we are using to solve for Rmax, 
                  ! and if so, take another shot at finding the 
                  ! Rmax using the gradient wind equation that neglects 
                  ! coriolis (and is appropriate in the vicinity of Rmax)
                  vicinity = 
     &              abs(root-radius(quad))/root 
                  if ( (root.lt.0.d0).or.(vicinity.le.0.1d0)) then
!                     PRINT *, "calcRmax:  Rmax is close to isotach."
!                     PRINT *, "calcRmax:  Solving again,",
!     &                            " neglecting Coriolis."
                     r1 = innerRadius
                     r2 = outerRadius
                     dr = one
                     DO iter = 1, itmax
                        root = findRoot(VhNoCori, r1,r2, dr, r3,r4)
!                       PRINT *, 
!     &                 " iter ", iter,": ", r1,", ",r2,", ",dr,", ",root
                        r1 = r3
                        r2 = r4
                        dr = dr * zoom
                     END DO
!                  PRINT *, "-------------------------------------------"
!                 PRINT *, "calcRmax: findRoot Rmax = ",root
                  endif 
                  Rmaxes(n+1) = root
               END DO

            END SUBROUTINE calcRmaxes

            !====================================================
            ! External function f(x) = 0 for which a root is
            ! sought using Brent's root-finding method.
            !
            ! On input:
            !    x       iterative values which converge to root
            !
            ! On output:
            !    func    f(x)
            !
            ! Internal parameters:
            !    vortex instance variables via accessor functions
            !====================================================
            FUNCTION VhWithCori(x)

               REAL(sz), INTENT(IN) :: x    ! x = Rmax
               REAL(sz) :: VhWithCori

               REAL(sz) :: VeeH
               REAL(sz) :: thisVr ! the radial wind speed we've been given

               !-------------------------
               ! func(x = Rmax) = Vh - Vr
               !-------------------------

               if ( getUseQuadrantVr().eqv..true. ) then
                  thisVr = VrQuadrant(quad)
               else 
                  thisVr = Vr
               endif
              VeeH = Vh (radius(quad), x) 
              VhWithCori = 
     &           ABS(Vh(radius(quad), x)) 
     &           - thisVr
!              PRINT *, "----------------------------------------"
!              PRINT *, "func:  Quadrant    = ", quad
!              PRINT *, "func:  Wind radius = ", radius(quad)
!              PRINT *, "func:  x           = ", x
!              PRINT *, "func:  B           = ", B
!              PRINT *, "func:  Pn          = ", Pn
!              PRINT *, "func:  Pc          = ", Pc
!              PRINT *, "func:  cLat        = ", cLat
!              PRINT *, "func   Vh          = ", Veeh
!              PRINT *, "func:  Vmax        = ", Vmax
!              PRINT *, "func:  Vr          = ", thisVr
!              PRINT *, "func:  VhWithCori        = ", VhWithCori
!              PRINT *, "----------------------------------------"
            END FUNCTION VhWithCori


            !===============================================================
            !===============================================================
            REAL(sz) FUNCTION VhNoCori(testRmax)
            REAL(sz), INTENT(IN) :: testRmax
            REAL(sz) :: thisVr ! the radial wind speed we've been given
            if ( getUseQuadrantVr().eqv..true. ) then
               thisVr = VrQuadrant(quad)
            else 
               thisVr = Vr
            endif
            VhNoCori = ABS(ms2kt *
     &           SQRT( (Vmax*kt2ms)**2.0d0 * (testRmax/radius(quad))**B
     &           * EXP(1 - (testRmax/radius(quad))**B)))
     &           - thisVr

            !===============================================================
            END FUNCTION VhNoCori
            !===============================================================



            !==============================================================
            ! Use brute-force marching to find a root the interval [x1,x2].
            !
            ! On input:
            !    func        function f(x)=0 for which root is sough
            !    x1          left  side of interval
            !    x2          right side of interval
            !    dx          x increment for march
            !
            ! On output:
            !    a           left  side of interval that brackets the root
            !    b           right side of interval that brackets the root
            !    findRoot    root returned
            !==============================================================
            FUNCTION findRoot(func, x1, x2, dx, a,b)
               USE CONSTANTS, ONLY : error
               REAL(sz), EXTERNAL :: func
               REAL(sz), INTENT(IN) :: x1, x2   ! Search interval [x1,x2]
               REAL(sz), INTENT(IN) :: dx       ! Marching increment
               REAL(sz), INTENT(OUT) :: a, b    ! x values that bracket root
               REAL(sz) :: findRoot             ! The root found

               INTEGER , PARAMETER :: itmax = 1000000 ! Max # of iterations
               INTEGER :: iter                        ! iteration counter
               REAL(sz) :: fa,fb                      ! function values f(x)
               !
               ! Initialize left side of interval
               a  = x1
               fa = func(a)
               !
               ! March along interval until root is found
               ! or solution diverges.
               findRoot = a
               DO iter = 1,itmax
                  b = x1 + iter * dx
                  fb = func(b)
                  ! Check progress
!                  PRINT *, "FUNCTION findRoot:  At iteration # ", iter
!                  PRINT *, "   [a,b]     = [", a, ",", b, "]"
!                  PRINT *, "   f(a),f(b) = ", fa, ", ", fb
                  IF ((fa*fb < zero) .OR. (ABS(fb) > ABS(fa))) THEN
                     ! Assign root
                     IF (ABS(fb) > ABS(fa)) THEN
                        findRoot = a
                     ELSE
                        findRoot = b
                     END IF
                     EXIT
                  END IF
                  !
                  ! Move right search interval values to left side
                  ! for next iteration.
                  a  = b
                  fa = fb
               END DO

               IF (iter >= itmax) THEN
                  PRINT *, 
     &              "FUNCTION findRoot: exceeded max # of iterations"
                  findRoot = error
               END IF
            END FUNCTION findRoot

            !==================================================================
            ! Calculate radial wind speed using Holland's hurricane wind model.
            !
            !    Vh = SQRT( B/RhoAir * (Rmax/r)**B * (Pn-Pc)*exp[-(Rmax/r)**B]
            !              + (r*coriolis/2)**2 ) - r*coriolis/2
            !
            !    Vh = SQRT( Vmax**2 * (Rmax/r)**B * exp[1 -(Rmax/r)**B]
            !              + (r*coriolis/2)**2 ) - r*coriolis/2
            ! On input:
            !    r            Distance from center of storm (NM)
            !    Rmax         Radius of maximum winds (NM)
            !    B            Exponential shape parameter (no units)
            !    Pn           Ambient surface pressure (mb)
            !    Pc           Surface pressure at center of storm (mb)
            !    Vmax         Maximum wind speed (kts)
            !
            ! On output:
            !    Vh           Radial wind speed (knots)
            !
            ! Note:
            !    Computation is performed in mks, then converted back to knots.
            !==================================================================
            REAL(sz) FUNCTION Vh(r, Rmax)
            REAL(sz), INTENT(IN) :: r
            REAL(sz), INTENT(IN) :: Rmax

              Vh = ms2kt * SQRT( 
     &            (Vmax*kt2ms)**2.d0 * (Rmax/r)**B  
     &                   * EXP(1-(Rmax/r)**B)
     &            + (nm2m*r*corio/2.d0)**2.d0) 
     &            - nm2m*r*corio/2.d0


            END FUNCTION Vh

            !=================================================================
            ! Calculate (u,v) wind components and surface pressure from an
            ! asymmetric hurricane wind model.
            !
            ! On input:
            !    lat         Latitude  of nodal point (degrees north)
            !    lon         Longitude of nodal point (degrees east )
            !    uTrans      x component of translational velocity (m/s)
            !    vTrans      y component of translational velocity (m/s)
            !
            ! On output:
            !    u           x component of wind velocity at nodal point (m/s)
            !    v           y component of wind velocity at nodal point (m/s)
            !    p           Surface pressure at nodal point (Pa)
            !
            ! Internal parameters:
            !    dampRadii   How far out (# of Rmax radii) to begin damping
            !                out the translational velocity
            !
            ! Note:
            !    Subroutine directly accesses global class instance variables
            !=================================================================
            SUBROUTINE uvp(lat,lon, uTrans,vTrans, u,v, p)

               REAL(sz), INTENT(IN)  :: lat
               REAL(sz), INTENT(IN)  :: lon
               REAL(sz), INTENT(IN)  :: uTrans
               REAL(sz), INTENT(IN)  :: vTrans

               REAL(sz), INTENT(OUT) :: u
               REAL(sz), INTENT(OUT) :: v
               REAL(sz), INTENT(OUT) :: p
  
               REAL(sz) :: TransSpdX  !NWS8-style translation speed
               REAL(sz) :: TransSpdY  !NWS8-style translation speed

               REAL(sz) :: dx
               REAL(sz) :: dy
               REAL(sz) :: dist
               REAL(sz) :: rmx
               REAL(sz) :: angle
               REAL(sz) :: speed
               REAL(sz) :: maxspeed
               REAL(sz) :: uf
               REAL(sz) :: vf
               REAL(sz) :: damp
               REAL(sz) :: percentCoriolis
               REAL(sz) :: speedAtRmax
               REAL(sz) :: vmaxFactor
               INTEGER :: i
               !------------------------------------------------------
               ! Calculate distance and angle between eye of hurricane
               ! and input nodal point
               !------------------------------------------------------
               dx = deg2rad * Rearth * (lon - cLon) * COS(deg2rad*cLat)
               dy = deg2rad * Rearth * (lat - cLat)
               dist = SQRT(dx*dx + dy*dy)

               !----------------------------------------
               ! Handle special case at eye of hurricane
               !----------------------------------------
               ! in eye velocity is zero not translational velocity
               !----------------------------------------
               IF (dist < one) THEN
                 u = zero
                 v = zero
                 p = Pc * mb2pa
                 RETURN
               END IF

               dist = m2nm * dist
               angle = threeSixty + rad2deg * ATAN2(dx,dy)
               IF (angle > threeSixty) angle = angle - threeSixty
               latestAngle = angle
               rmx =  Rmw(angle)
               latestRmax = rmx

               !---------------------------------------------------
               ! Compute (u,v) wind velocity components from the
               ! asymmetric hurricane vortex.
               !
               ! Note: the vortex winds are valid at the top of the
               ! surface layer, so reduce the winds to the surface.
               ! Also convert the winds from max sustained 1-minute
               ! averages to 10-minute averages for the storm surge
               ! model.
               !---------------------------------------------------
            ! jgfdebug
            percentCoriolis=1.0d0
            speed = SQRT( 
     &           (Vmax*kt2ms)**2.d0 * (rmx/dist)**B  
     &                  * EXP(1.d0-(rmx/dist)**B)
     &            + (nm2m*dist*percentCoriolis
     &                           *corio/2.d0)**2.d0) 
     &            - nm2m*dist*percentCoriolis*corio/2.d0
            
            ! calculate the wind speed (m/s) at Rmax, using 
            ! equation that includes full coriolis
            speedAtRmax = SQRT( 
     &           (Vmax*kt2ms)**2.d0 *   
     &                  * EXP(0.d0)
     &            + (nm2m*dist*percentCoriolis
     &                           *corio/2.d0)**2.d0) 
     &            - nm2m*dist*percentCoriolis*corio/2.d0

            ! calculate a factor to place the velocity profile so that
            ! it hits vmax
            vmaxFactor = Vmax*kt2ms / speedAtRmax
 
           ! jgf20111007: Calculate NWS8-like translation speed
           TransSpdX = (abs(speed/(speedAtRmax)))*uTrans
           TransSpdY = (abs(speed/(speedAtRmax)))*vTrans

           speed = speed * vmaxFactor

           ! now reduce the wind speed to the surface
           speed = speed * windReduction
 
           u = -speed * COS(deg2rad*angle) 
           v =  speed * SIN(deg2rad*angle) 
           !
           ! Alter wind direction by adding a frictional inflow angle
           CALL rotate(u,v, fang(dist,rmx), cLat, uf,vf)
           u = uf
           v = vf
           !
           ! jgf20111007: Add in the translation velocity
           u = u + TransSpdX
           v = v + TransSpdY
           !
           ! convert from 1 minute averaged winds to 10 minute averaged
           ! winds for use in ADCIRC
           u = u * one2ten
           v = v * one2ten
           !
           ! Compute surface pressure from asymmetric hurricane vortex
           p = mb2pa * (Pc + (Pn - Pc) * EXP(-(rmx/dist)**B))


           END SUBROUTINE uvp

            !=================================================================
            ! calculate the radius of maximum winds.
            !
            ! On input:
            !    angle        Azimuthal angle (degrees)
            !
            ! On output:
            !    Rmw          Radius of maximum winds (meters) from curve fit
            !                      I DO NOT BELIEVE IT IS IN METERS rjw
            !=================================================================
            REAL(sz) FUNCTION Rmw(angle)
               REAL(sz), INTENT(IN) :: angle
               INTEGER :: base_quadrant
               REAL(sz) :: delta_angle
            if (angle.le.45.0d0) then
               base_quadrant = 5
               delta_angle = 45.d0 + angle
            else if (angle.le.135.d0) then
               base_quadrant = 2
               delta_angle = angle - 45.d0
            else if (angle.le.225.d0) then
               base_quadrant = 3
               delta_angle = angle - 135.d0
            else if (angle.le.315.d0) then
               base_quadrant = 4
               delta_angle = angle - 225.d0
            else if (angle.gt.315.d0) then
               base_quadrant = 5
               delta_angle = angle - 315.d0
            endif
            Rmw = (delta_angle/90.0d0)
     &             *(Rmaxes(base_quadrant+1)-Rmaxes(base_quadrant))
     &            + Rmaxes(base_quadrant)

            END FUNCTION Rmw
            !========================================================
            ! Rotate a 2D vector (x,y) by an angle.
            !
            ! On input:
            !    x           x component of vector
            !    y           y component of vector
            !    angle       angle to rotate vector (degrees)
            !    whichWay    direction of rotation:
            !                   - = clockwise, + = counter-clockwise
            !
            ! On output:
            !    xr          x component of rotated vector
            !    yr          y component of rotated vector
            !========================================================
            SUBROUTINE rotate(x,y, angle, whichWay, xr,yr)
               REAL(sz), INTENT(IN)  :: x
               REAL(sz), INTENT(IN)  :: y
               REAL(sz), INTENT(IN)  :: angle
               REAL(sz), INTENT(IN)  :: whichWay

               REAL(sz), INTENT(OUT) :: xr
               REAL(sz), INTENT(OUT) :: yr

               REAL(sz) :: A, cosA, sinA

               A = SIGN(one, whichWay) * deg2rad * angle
               cosA = COS(A)
               sinA = SIN(A)

               xr = x * cosA - y * sinA
               yr = x * sinA + y * cosA
            END SUBROUTINE rotate

            !=======================================================
            ! Compute a wind angle to parameterize frictional inflow
            ! across isobars.
            !
            ! On input:
            !    r           distance from center of storm
            !    rmx         radius of maximum winds
            !
            ! On output:
            !    fang        frictional inflow angle (degrees)
            !=======================================================
            REAL(sz) FUNCTION fang(r, rmx)
               REAL(sz), INTENT(IN) :: r
               REAL(sz), INTENT(IN) :: rmx

               IF (zero <= r .AND. r < rmx) THEN
                  fang = ten * r/rmx
               ELSE IF (rmx <= r .AND. r < onePointTwo*rmx) THEN
                  fang = ten + seventyFive * (r/rmx - one)
               ELSE IF (r >= onePointTwo*rmx) THEN
                  fang = twentyFive
               ELSE
                  fang = zero
               END IF
            END FUNCTION fang


            !============================================================
            ! Calculate the translational velocity of a moving hurricane.
            !
            ! On input:
            !    latOld      Previous latitude  of center (degrees north)
            !    lonOld      Previous longitude of center (degrees east )
            !    latNew      Current  latitude  of center (degrees north)
            !    lonNew      Current  longitude of center (degrees east )
            !    tOld        Previous time (seconds)
            !    tNew        Current  time (seconds)
            !
            ! On output:
            !    uTrans      x component of translational velocity (m/s)
            !    vTrans      y component of translational velocity (m/s)
            !============================================================
            SUBROUTINE uvtrans(latOld,lonOld, latNew,lonNew, tOld,tNew,
     &                         uTrans,vTrans)

               REAL(sz), INTENT(IN)  :: latOld
               REAL(sz), INTENT(IN)  :: lonOld

               REAL(sz), INTENT(IN)  :: latNew
               REAL(sz), INTENT(IN)  :: lonNew

               REAL(sz), INTENT(IN)  :: tOld
               REAL(sz), INTENT(IN)  :: tNew

               REAL(sz), INTENT(OUT) :: uTrans
               REAL(sz), INTENT(OUT) :: vTrans

               REAL(sz) :: dx
               REAL(sz) :: dy
               REAL(sz) :: dt

               dx = deg2rad * Rearth * (lonNew - lonOld)
     &            * COS(deg2rad*half * (latOld + latNew))

               dy = deg2rad * Rearth * (latNew - latOld)

               dt = tNew - tOld

               uTrans = dx/dt
               vTrans = dy/dt


            END SUBROUTINE uvtrans

            !=================================================================
            ! Transform (lat,lon) --> (x,y) coordinates.
            !
            ! On input:
            !    lat        Latitude  (degrees north)
            !    lon        Longitude (degrees east )
            !    lat0       Latitude  where projection is true (degrees north)
            !    lon0       Longitude where projection is true (degrees east )
            !
            ! On output:
            !    x          x (meters)
            !    y          y (meters)
            !=================================================================
            SUBROUTINE latlon2xy(lat,lon, lat0,lon0, x,y)
               REAL(sz), INTENT(IN)  :: lat ,lon
               REAL(sz), INTENT(IN)  :: lat0,lon0
               REAL(sz), INTENT(OUT) :: x,y

               x = deg2rad * Rearth * (lon - lon0) * COS(deg2rad*lat0)
               y = deg2rad * Rearth * lat
            END SUBROUTINE latlon2xy

            !=================================================================
            ! Transform (x,y) --> (lat,lon) coordinates.
            !
            ! On input:
            !    x          x (meters)
            !    y          y (meters)
            !    lat0       Latitude  where projection is true (degrees north)
            !    lon0       Longitude where projection is true (degrees east )
            !
            ! On output:
            !    lat        Latitude  (degrees north)
            !    lon        Longitude (degrees east )
            !=================================================================
            SUBROUTINE xy2latlon(x,y, lat0,lon0, lat,lon)
               REAL(sz), INTENT(IN)  :: x,y
               REAL(sz), INTENT(IN)  :: lat0,lon0
               REAL(sz), INTENT(OUT) :: lat ,lon

               lat = y / (deg2rad * Rearth)
               lon = lon0 + x / (deg2rad * Rearth * COS(deg2rad*lat0))
            END SUBROUTINE xy2latlon

            !===============================================================
            ! RJW 07 - 2009 
            ! Calculate the coefficients that fit the given 
            ! radius of maximum winds for all storm quadrants.
            !
            ! On input:
            !   Rmax in all 4 quadrants plus 2 extra values to tie down circular periodicity
            !
            ! On output:
            !    Rmax    radius of maximum winds (nm) in all quadrants, plus
            !            2 extra values to tie down circular periodicity
            !===============================================================
            SUBROUTINE fitRmaxes()
               REAL(sz)            :: root        ! Radius of maximum winds
               INTEGER             :: n, iter,i

               !
               ! Generate 2 additional (theta,Rmax) points for curve-fit
               Rmaxes(1) = Rmaxes(5)
               Rmaxes(6) = Rmaxes(2)

            END SUBROUTINE fitRmaxes


            SUBROUTINE setUseQuadrantVr(u)
               LOGICAL, INTENT(IN) :: u
               useQuadrantVr = u
            END SUBROUTINE setUseQuadrantVr

            SUBROUTINE setIsotachWindSpeeds(vrq)
               REAL(sz), DIMENSION(4), INTENT(IN) :: vrq
               VrQuadrant(:) = vrq(:) 
            END SUBROUTINE setIsotachWindSpeeds

            SUBROUTINE setIsotachWindSpeed(sp)
               REAL(sz), INTENT(IN) :: sp
               Vr = sp 
            END SUBROUTINE setIsotachWindSpeed

            SUBROUTINE setIsotachRadii(ir)
               REAL(sz), DIMENSION(4), INTENT(IN) :: ir
               radius(:) = ir(:) 
            END SUBROUTINE setIsotachRadii

            REAL(sz) FUNCTION getShapeParameter()
               getShapeParameter = B
            END FUNCTION getShapeParameter

            SUBROUTINE setShapeParameter(param)
               REAL(sz) :: param
               B = param
            END SUBROUTINE setShapeParameter

            SUBROUTINE getRmaxes(rmaxw)
               REAL(sz), DIMENSION(4), INTENT(OUT) :: rmaxw
               INTEGER :: i
               do i=1,4
                  rmaxw(i) = Rmaxes(i+1)
               end do
            END SUBROUTINE getRmaxes

            SUBROUTINE setRmaxes(rmaxw)
               REAL(sz), DIMENSION(4), INTENT(IN) :: rmaxw
               INTEGER :: i
               do i=1,4
                  Rmaxes(i+1) = rmaxw(i) 
               end do
            END SUBROUTINE setRmaxes

            REAL(sz) FUNCTION getLatestRmax()
               getLatestRmax = latestRmax
            END FUNCTION getLatestRmax

            REAL(sz) FUNCTION getLatestAngle()
               getLatestAngle = latestAngle
            END FUNCTION getLatestAngle

            LOGICAL FUNCTION getUseQuadrantVr()
               getUseQuadrantVr = useQuadrantVr
            END FUNCTION getUseQuadrantVr
 
      END MODULE vortex

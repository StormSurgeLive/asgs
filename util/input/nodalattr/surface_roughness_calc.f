!     setenv FFLAGS "-O2 -FR -w -xT"
!     ifort $FFLAGS -c TimerClass.F
!     ifort $FFLAGS -FI -132 -o surface_roughness_calc surface_roughness_calc_v9.f *.o
!     ./surface_roughness_calc >& src.out &
!     \rm matlab_output.out cape_lookout_surf_rough_l_crys src.out

      program surface_roughness_calc

!    *********************************************************************
!    *                                                                   *
!    * Program to take a National Land Cover Database zone file          *
!    * downloaded from http://www.mrlc.gov/index.asp and an ADCIRC grid  *
!    * file and extract the surface roughness length in 12 directions    *
!    * for each node in the grid file.                                   *
!    *                                                                   *
!    * This program uses an input file called surf_rough.in.  The file   *
!    * contains the following lines:                                     *
!    *                                                                   *
!    *         Line 1:  Name of the grid file used (in lat/lon)          *
!    *         Line 2:  Name of the output file to write                 *
!    *         Line 3:  The total distance to average the surface        *
!    *                  roughness lengths over (in the units your files  *
!    *                  are in, our typical value is 10,000m or 10km)    *
!    *         Line 4:  The weighted distance to use (in the units your  *
!    *                  files are in, our typical value is 3000m or 3km) *
!    *         Line 5:  The input file that contains the NLCD class      *
!    *                  numbers to obtain the surface roughness lengths  *
!    *                  from                                             *
!    *         Line 6:  =1 for sector method                             *
!    *                  =2 for linear method                             *
!    *                                                                   *
!    * The output file is fort.13 format without the header.  It is      *
!    * simply in node number, surface roughness for each of the 12       *
!    * directions.                                                       *
!    *                                                                   *
!    * This program uses the simple method of calculating the values     *
!    * along the designated angle (eg, 0, 30, 60, 90, etc) of each       *
!    * surface roughness length and using a weighted average method      *
!    * using the weighting factor of:                                    *
!    *                          (- dist^2 )                              *
!    *                          -----------                              *                                        
!    *                          (2*sigma^2)                              *
!    *                 w(i)= exp                                         *
!    *                                                                   *
!    *       where: sigma=weighted distance specified in .in file        *
!    *              dist=distance between current point and original     *
!    *                   point                                           *
!    *                                                                   *
!    *         version 1.0 - Crystal Fulcher - April 19, 2007            *
!    *         version 2.0 - Crystal Fulcher - April 20, 2007            *
!    *                 this version calculates the x and y values on the *
!    *                 fly so memory can be conserved and the program    *
!    *                 will run with the larger nlcd files               *
!    *         version 3.0 - Crystal Fulcher - April 24, 2007            *
!    *                 this version uses a new method where x and y do   *
!    *                 not have to be calculated that really cuts down   *
!    *                 on run time.                                      *
!    *         version 4.0 - Crystal Fulcher - May 2, 2007               *
!    *                 fixed bugs -  now the results look right in all   *
!    *                 12 directions                                     *
!    *         version 5.0 - Crystal Fulcher - May 16, 2007              *
!    *                 Added in subroutines from Craig's sector version  *
!    *                 of the program.  This finds all the nodes in the  *
!    *                 sector and averages them using a Gaussian weighted*
!    *                 average.  An additional line is now needed in the *
!    *                 input file to determine whether to use the sector *
!    *                 version (=1) or the linear average version (=2).  * 
!    *                 The new section (=1) was written by Craig         *
!    *                 Mattocks as landuse.f and constants.f, I          *
!    *                 incorporated his calculations into this program.  *
!    *         version 6.0 - Crystal Fulcher - May 21, 2007              *
!    *                 Output is now in format of the fort.13 file minus *
!    *                 the needed header information.  Output for matlab *
!    *                 testing purposes is still in the program too.     *
!    *         version 7.0 - Crystal Fulcher - May 31, 2007              *
!    *                 fixed a bug in direction 10, also for linear      *
!    *                 version, fixed it so that if one of the directions*
!    *                 has a value of -9999, then that node will not be  *
!    *                 printed out in the output file.                   *
!    *         version 8.0 - Crystal Fulcher - June 5, 2007              *
!    *                 re-did some of the sector code so it computes the *
!    *                 distance between the node and the NLCD point      *
!    *                 before any other calculations to save going       *
!    *                 through other calculations if the distance is     *
!    *                 greater than the total distance.                  *
!    *         version 9.0 - Craig Matttocks - June 9, 2007              *
!    *                 optimized sector code, fixed several bugs,        *
!    *                 included CPU timer.                               *
!    *         version 10.0 - Crystal Fulcher - June 15, 2007            *
!    *                 removed unnecessary lines for matlab output,      *
!    *                 added info about grid file projection in header   *
!    *                 This is the first release to go on the NC FEMA    *
!    *                 project ftp site.                                 *
!    *         version 11.0 - Crystal Fulcher - August 3, 2007           *
!    *                 bug fix in the linear version to account for new  *
!    *                 NLCD file that has -9999 for missing data along   *
!    *                 with the usual code of 127                        *
!    *         version 12.0 - Crystal Fulcher - May 7, 2008              *
!    *                 bug fix to remove any -9999 values for missing    *
!    *                 data that would be in the output file.            *
!    *         version 13.0 - drc - June 06 2008- added albers -         *
!    *                 grid file (input) should be in long/lat           *
!    *         version 14.0 - Crystal Fulcher - Oct. 25, 2010 - header   *
!    *                 corrections and change parameter statements to    *
!    *                 allocatable variables.  Now there is no need to   *
!    *                 change the program no matter the size of the      *
!    *                 landcover file used.                              *
!    *         version 15.0 - Crystal Fulcher - Dec 20, 2011 - corrected *
!    *                 so output lines up with how ADCIRC expects to     *
!    *                 read it. Previously it was output starting at N   * 
!    *                 and proceeding CW, this version corrects it to    *
!    *                 start at E and proceed CCW. This correction is    *
!    *                 is made to both versions, the sector and the      *
!    *                 linear.                                           *
!    *         version 16.0 - Crystal Fulcher - Jan 3, 2012 - corrected  *
!    *                 do output lines up with how ADCIRC expects to     *
!    *                 read it. This time correcting from thinking it is *
!    *                 using the from wind direction and instead using   *
!    *                 the to wind direction.  ie, 0 degrees means the   *
!    *                 wind is blowing due east (from west to east)      *
!    *                                                                   *
!    *********************************************************************

      !use constants

      !-----------------------
      ! Import CPU timer class
      !-----------------------
      !USE TimerClass

      !--------------------------------------------
      ! Force explicit declaration of all variables
      !--------------------------------------------
      implicit none

      !---------------------
      ! CPU timing variables
      !---------------------
      REAL    :: cpuTime = 0.    ! Net execution time
      INTEGER :: minutes = 0     ! Minutes required for execution and
      REAL    :: seconds = 0.    ! seconds required for execution
      REAL toc 


      integer maxrow, maxcol
      integer ndir
      parameter(ndir=12)

!TGA      integer nx, ny
!TGA      parameter(nx=500, ny=500)

      character*13 ndum
      character*90 gridfile,outputfile,inputfile
      character*60 header

      integer ncount_sum_sr(ndir)
      integer, allocatable ::  nlcd_class(:,:)
      real*8 surf_rough_l

      integer x_toplt,y_toplt,x_botrt,y_botrt
      integer x_botlt,y_botlt,x_y_dist
      integer x_inc,y_inc

      real*8 x_temp,y_temp
      real*8 x_gis,y_gis,x_gis_init,y_gis_init
!TGA      real*8 x1(nx),y1(ny)
      real*8 x_diff, y_diff
      real*8 x_dist,y_dist, xy_dist
      real*8 x_30, y_30, x_30_dist, y_30_dist 
      real*8 percent, surf_rough_l2, surf_rough_wt_avg
      real*8 dist, dist_sq, surf_rough_temp
      real*8 total_dist, total_dist_sq
      real*8 weighted_dist, inv_two_weighted_dist_sq
      real*8 angle, w
      real*8 rough(ndir),weight(ndir), arc,half_arc
      real*8 sum_surf_rough(ndir)
      real*8 sum_weight(ndir)
      real*8 final_surf_rough(ndir)
      real*8 dist_1,dist_2
      real*8 pi,sqrt2,sqrtpi
      real*8 weight_factor
      real*8 xl, yl
      real*8 x_inc_r,y_inc_r
      real*8 zero, half, one, two
      real*8 oneEighty,threeSixty,pi2,twopi,deg2rad,rad2deg
      real*8 missing
      integer nPoints(ndir)
      integer icount, nsum
      integer nodata, nodata_class
      integer n_calc_type
      real*8 row, col, times, decimal
      integer nrows, ncols, ntimes, n_row, n_col, n_beg_row, n_end_row
      integer n_ident_row, n_ident_col, n_ident_tot, inum, ident
      integer i, j, k, l
      integer nele, nnodes

      INTEGER num1,num2,num3,num4,num5,nm1
      DOUBLE PRECISION :: E1,M1,M2,ALPHAA,ALPHA0
      DOUBLE PRECISION :: ALPHAB,ALPHA1,ALPHAC,ALPHA2
      DOUBLE PRECISION :: N,C,RHO0,E,RAD,O
      DOUBLE PRECISION :: LON,LAT,ALPHA,THETA,P,RHO
      DOUBLE PRECISION :: x2,y2,x,y
      DOUBLE PRECISION :: PHI1,PHI2,A,LON0,LAT0,EE
      INTEGER(kind=8) xo,yo

CTGA-BEGIN:  Added for fort.13 generation automation
      character*60 fillc2sderl
      character*30 strtotal_dist,strweighted_dist,strn_calc_type
      INTEGER nlc
      real(8),allocatable :: lookuptable(:,:)
CTGA-END:  Added for fort.13 generation automation

      PHI1 = 29.5d0
      PHI2 = 45.5d0
      A = 6378137.0d0                 
      LON0 = -96.0d0                 
      LAT0 = 23.0d0                 
      EE = 0.0066943800229d0
      E = 0.0818191910428d0                       
      RAD = 4.0d0*DATAN(1.000d0)/180.d0                 

      E1 = 1-EE                                            
      O = (0.5/E)*E1
      M1 = DCOS(PHI1*RAD)/(1-EE*DSIN(PHI1*RAD)**2)**0.5
      M2 = DCOS(PHI2*RAD)/(1-EE*DSIN(PHI2*RAD)**2)**0.5
      ALPHAA = (0.5/E)*DLOG((1-E*DSIN(LAT0*RAD))/(1+E*DSIN(LAT0*RAD)))
      ALPHA0 = E1*DSIN(LAT0*RAD)/(1-EE*DSIN(LAT0*RAD)**2)-E1*ALPHAA 
      ALPHAB = (0.5/E)*DLOG((1-E*DSIN(PHI1*RAD))/(1+E*DSIN(PHI1*RAD)))
      ALPHA1 = E1*DSIN(PHI1*RAD)/(1-EE*Dsin(PHI1*RAD)**2)-E1*ALPHAB
      ALPHAC = (0.5/E)*DLOG((1-E*DSIN(PHI2*RAD))/(1+E*DSIN(PHI2*RAD)))
      ALPHA2 = E1*DSIN(PHI2*RAD)/(1-EE*DSIN(PHI2*RAD)**2)-E1*ALPHAC
      N = (M1**2-M2**2)/(ALPHA2-ALPHA1)
      C = M1**2+N*ALPHA1
      RHO0 = A*((C-N*ALPHA0)**(0.5))/N

      !---------------------------
      ! Start CPU time "stopwatch"
      !---------------------------

CTGA-BEGIN:  Added for fort.13 generation automation
!TGA      CALL tic
CTGA-END:  Added for fort.13 generation automation

!    Define constants

      sqrt2=sqrt(2.d0)
      nodata_class=127
      zero=0.d0
      half=.5d0
      one=1.d0
      two=2.d0
      oneEighty=180.d0
      threeSixty=360.d0
      arc = threeSixty/ndir
      half_arc = half * arc
      missing = -9999.d0

      !---------------------
      ! pi and factors of pi
      !---------------------
      pi = 3.141592653589793238462643383279502884197d0
      pi2 = 1.57079632679489661923132169163975144209858d0
      twopi = 6.283185307179586476925286766559005768394d0 
      sqrtpi = sqrt(pi)

      !----------------------------------------
      ! Degrees <--> radians conversion factors
      !----------------------------------------
      deg2rad = pi / oneEighty
      rad2deg = oneEighty / pi
     
!    Open the input file required to run this program and also open the NCLD
!    file and read the header information.  
 
CTGA-BEGIN:  Added for fort.13 generation automation
      CALL GETARG(1,gridfile)
      CALL GETARG(2,outputfile)
      CALL GETARG(3,inputfile)
      CALL GETARG(4,fillc2sderl)
      CALL GETARG(5,strtotal_dist)
      CALL GETARG(6,strweighted_dist)
      CALL GETARG(7,strn_calc_type)
      read(strtotal_dist,*)total_dist
      read(strweighted_dist,*)weighted_dist
      read(strn_calc_type,*)n_calc_type
      open(20,file=fillc2sderl)
      read(20,*)nlc
      allocate(lookuptable(nlc,2))
      do j=1,nlc
         read(20,*)lookuptable(j,1:2)
      enddo
!TGA      open(10,file='surf_rough.in')
!TGA      read(10,*) gridfile
!TGA      read(10,*) outputfile
!TGA      read(10,*) total_dist
!TGA      read(10,*) weighted_dist
!TGA      read(10,*) inputfile
!TGA      read(10,*) n_calc_type
CTGA-END:  Added for fort.13 generation automation

      total_dist_sq = total_dist*total_dist
      inv_two_weighted_dist_sq = one / (two*weighted_dist*weighted_dist)


      open(13,file=TRIM(ADJUSTL(inputfile)))
      read(13,*) ndum,ncols
      read(13,*) ndum,nrows
      read(13,*) ndum,x_botlt
      read(13,*) ndum,y_botlt
      read(13,*) ndum,x_y_dist
      read(13,*) ndum,nodata
     
      allocate  ( nlcd_class(nrows,ncols) )    
      
      x_inc=x_y_dist
      y_inc=x_y_dist
      x_inc_r=x_inc
      y_inc_r=y_inc

      x_botrt=x_botlt+(ncols-1)*x_inc
      y_botrt=y_botlt
      x_toplt=x_botlt          
      y_toplt=y_botlt+(nrows-1)*y_inc

      n_ident_tot=nrows*ncols

!    Read in the NLCD Classifications from the NLCD files.
         
      do j=1,nrows
         read(13,*) (nlcd_class(j,k),k=1,ncols)
      end do
      
!    Open the grid file, create the output file and read in the header information from
!    the grid file.

      open(11,file=TRIM(ADJUSTL(gridfile)))
      open(12,file=TRIM(ADJUSTL(outputfile)))
      read(11, '(a60)') header
      read(11,*) nele,nnodes
      
!   Based on input setting of n_calc_type, chose to use the sector method (=1) 
!   or the line average method (=2)  This incorporates a subroutine by Craig Mattocks that had the 
!   following header:
      !=================================================================
      ! Compute Normalized Gaussian-weighted surface roughness in all
      ! directions around ADCIRC nodes.
      !
      ! On input:
      !    np           Number of ADCIRC nodes
      !    xn           x coordinates of nodes
      !    yn           y coordinates of nodes
      !    idim         Number of landuse values in x direction
      !    jdim         Number of landuse values in y direction
      !    xl           x coordinates of landuse data points
      !    yl           y coordinates of landuse data points
      !    cat          NLCD category at all landuse data points
      !
      ! On output:
      !    nPoints      Number of landuse data points that fall in each
      !                 directional sector.
      !    rough        Normalized Gaussian-weighted surface roughness
      !                 for each directional sector. 
      !    weight       Sum of Gaussian-weights in each sector used to
      !                 normalize surface roughness. 
      !
      ! Note:
      !    Subroutine directly accesses global class instance variables
      !    not contained in its signature.
      !
      ! Revision history:
      !    Date      Programmer                 Description of change
      !    ----      ----------                 ---------------------
      !    04/02/07  Craig  Mattocks, UNC-IMS   Wrote original code
      !    05/16/07  Crystal Fulcher, UNC-IMS   Added Craig's code into
      !                                         surface_roughness_calc_v5.f
      !                                         Calculations are the same, I 
      !                                         just meshed it with my program 
      !    12/20/11  Crystal Fulcher, UNC-IMS   Changed output from starting at 
      !                                         N and going CW to starting at E
      !                                         and going CCW
      !=================================================================

      if (n_calc_type .eq. 1) then    ! Sector averaged method
         do i=1,nnodes
            do j=1,ndir
               nPoints(j) = 0
               rough  (j) = zero
               weight (j) = zero
            end do
            read(11,*) inum,LON,LAT
              P = O*DLOG((1-E*DSIN(LAT*RAD))/(1+E*DSIN(LAT*RAD))) 
              ALPHA = E1*DSIN(LAT*RAD)/(1-EE*DSIN(LAT*RAD)**2)-P  
              THETA = N*(LON-LON0) 
              RHO = A*((C-N*ALPHA)**(0.5))/N
              x2 = RHO*DSIN(THETA*RAD)
              y2 = RHO0-RHO*DCOS(THETA*RAD)
              xo = ANINT(x2*100.0)
              yo = ANINT(y2*100.0)
              x = xo/100.0
              y = yo/100.0
            DO j = 1, nrows            ! Loop over all landuse data points
               DO k = 1, ncols
                  xl = x_toplt + (k-1)*x_inc
                  yl = y_toplt - (j-1)*y_inc
                  x_dist = xl-x
                  y_dist = yl-y
                  dist_sq = x_dist*x_dist + y_dist*y_dist
                  IF (dist_sq <= total_dist_sq) THEN
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(j,k), surf_rough_l,
     &                  missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(j,k), surf_rough_l,
!TGA     &                  missing)
CTGA-END:  Added for fort.13 generation automation
     
                     IF (surf_rough_l >= zero) THEN
                        !--------------------------------------
                        ! Calculate the angle between an ADCIRC
                        ! nodal point and a data point.
                        !--------------------------------------
                        angle = MOD(rad2deg * ATAN2(x_dist,y_dist) + 
     &                              threeSixty, threeSixty)
         
                        !---------------------------------------------
                        ! Find the sector into which this angle falls.
                        !---------------------------------------------
                        l = (angle + half_arc) / arc
                        IF (l < 1) l = ndir
                        nPoints(l) = nPoints(l) + 1

                        !------------------------------
                        ! Calculate Gaussian weighting.
                        !------------------------------
                        w = EXP(-dist_sq * inv_two_weighted_dist_sq)
                        rough (l) = rough (l) + surf_rough_l*w
                        weight(l) = weight(l) + w
                     END IF
                  END IF
               END DO   ! landuse columns loop
            END DO      ! landuse rows    loop

            ! -------------------
            ! Normalize roughness
            ! -------------------
            do j=1,ndir
               if (weight(j) > zero) then
                  rough(j) = rough(j) / weight(j)
               else
                  go to 3
               end if
            end do
               
!    Output written in fort.13 format

            write(12,130) inum,(rough(j),j=10,1,-1), 
     &            (rough(j),j=12,11,-1)
               
3           continue            
         end do
      else                             ! Nearest-neighbor linear method

!    For each node make sure the node is in the NLCD data and if it is begin processing 
!    each direction for the node, if it is not then give the sum_surf_rough a value of 
!    'missing' (-9999) in all 12 directions.

      do i=1,nnodes
         read(11,*) inum,LON,LAT
              P = O*DLOG((1-E*DSIN(LAT*RAD))/(1+E*DSIN(LAT*RAD))) 
              ALPHA = E1*DSIN(LAT*RAD)/(1-EE*DSIN(LAT*RAD)**2)-P  
              THETA = N*(LON-LON0) 
              RHO = A*((C-N*ALPHA)**(0.5))/N
              x2 = RHO*DSIN(THETA*RAD)
              y2 = RHO0-RHO*DCOS(THETA*RAD)
              xo = ANINT(x2*100.0)
              yo = ANINT(y2*100.0)
              x = xo/100.0
              y = yo/100.0
         if ((x>x_toplt).and.(x<x_botrt).and.(y<y_toplt).and.
     &       (y>y_botrt)) then
             x_diff=x_toplt-x
             y_diff=y_toplt-y
             n_row=abs(y_diff/y_inc)
             row=abs(y_diff/y_inc)
             decimal=row-n_row
             if (decimal > half) then
                n_row=n_row+1
             end if
             n_col=abs(x_diff/x_inc)
             col=abs(x_diff/x_inc)
             decimal=col-n_col
             if (decimal > half) then
                n_col=n_col+1
             end if
             ident=(n_row)*ncols+(n_col+1)
             n_beg_row=n_row*ncols+1
             n_end_row=(n_row+1)*ncols
          else
             do j=1,ndir
                sum_surf_rough(j)=zero
             end do
             go to 120
          end if
         do j=1,ndir
            sum_weight(j)=zero
            sum_surf_rough(j)=zero
!    Direction one is due north (0 degrees - or between 345° and 15°)
               if (j == 1) then
                  times=abs(total_dist/y_inc)
                  ntimes=abs(total_dist/y_inc)
                  decimal=times-ntimes
                  n_ident_row=n_row
                  if (decimal > half) then
                     ntimes=ntimes+2
                  else
                     ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  n_ident_col=n_col
                  do l=1,ntimes
                     if ((n_ident_row<=0).or.(n_ident_row>nrows)) 
     &                  go to 5
                     if (nlcd_class(n_ident_row,n_col) == nodata_class) 
     &                   go to 4
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,n_col),
     &                  surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,n_col),
!TGA     &                  surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 5
                     dist=sqrt(((l-1)*y_inc_r)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     sum_weight(j)=sum_weight(j)+weight_factor
                     sum_surf_rough(j)=sum_surf_rough(j)+weight_factor*
     &                  surf_rough_l
                     n_ident_row=n_row-l
                    
4                    continue
                  end do

5                   continue

!    Direction two is 30 degrees - or between 15° and 45°)

               elseif (j == 2) then
                  x_30=abs(y_inc*tan(deg2rad*30.0d0))
                  xy_dist=sqrt(y_inc**2+x_30**2)
                  times=abs(total_dist/xy_dist)
                  ntimes=abs(total_dist/xy_dist)
                  decimal=times-ntimes
                  if (decimal > half) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  n_ident_col=n_col
                  dist_2=x_inc-x_30
                  dist_1=x_inc
                  x_dist=x_inc
                  do l=1,ntimes
                     if ((n_ident_row.le.0).or.(n_ident_col.le.0)) go to
     &                  15
                     if ((n_ident_row.gt.nrows).or.(n_ident_col.gt.
     &                  ncols)) go to 15
                     dist=sqrt((xy_dist*l)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     if ((nlcd_class(n_ident_row,n_ident_col).eq.
     &                  nodata_class).or.(nlcd_class(n_ident_row,
     &                  n_ident_col-1).eq.nodata_class)) then
                        x_30_dist=x_30*(l+1)
                        if (x_30_dist.gt.x_dist) then
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col+1
                           x_dist=x_dist+x_inc
                           dist_2=x_dist-x_30_dist
                        else
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col
                           dist_2=x_dist-x_30_dist
                        end if
                        go to 10
                     end if
                     percent=(dist_2/dist_1)
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                    n_ident_col),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                    n_ident_col),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 15
                     if (l.eq.1) then
                        sum_surf_rough(j)=surf_rough_l*weight_factor
                        sum_weight(j)=weight_factor
                        x_30_dist=x_30*(l+1)
                        if (x_30_dist.gt.x_dist) then
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col+1
                           x_dist=x_dist+x_inc
                           dist_2=x_dist-x_30_dist
                        else
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col
                           dist_2=x_dist-x_30_dist
                        end if
                        go to 10
                     end if
                     surf_rough_l2=surf_rough_l
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,n_ident_col-
     &                 1),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,n_ident_col-
!TGA     &                 1),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 15
                     surf_rough_wt_avg=(1-percent)*surf_rough_l2+
     &                   percent*surf_rough_l
                     sum_surf_rough(j)=sum_surf_rough(j)+
     &                   surf_rough_wt_avg*weight_factor
                     sum_weight(j)=sum_weight(j)+weight_factor
                     x_30_dist=x_30*(l+1)
                     if (x_30_dist.gt.x_dist) then
                         n_ident_row=n_ident_row-1
                         n_ident_col=n_ident_col+1
                         x_dist=x_dist+x_inc
                         dist_2=x_dist-x_30_dist
                     else
                         n_ident_row=n_ident_row-1
                         n_ident_col=n_ident_col
                         dist_2=x_dist-x_30_dist
                     end if
10                   continue                        
                     end do

15                continue

!    Direction three is 60 degrees - or between 45° and 75°
                                    
               elseif (j.eq.3) then
                  y_30=abs(x_inc*tan(deg2rad*30.0d0))
                  xy_dist=sqrt(y_30**2+x_inc**2)
                  times=abs(total_dist/xy_dist)
                  ntimes=abs(total_dist/xy_dist)
                  decimal=times-ntimes
                  if (decimal.gt.0.5) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  n_ident_col=n_col
                  dist_2=y_inc-y_30
                  dist_1=y_inc
                  y_dist=y_inc
                  do l=1,ntimes
                     if ((n_ident_row.le.0).or.(n_ident_col.le.0)) go to
     &                  25
                     if ((n_ident_row.gt.nrows).or.(n_ident_col.gt. 
     &                  ncols)) go to 25
                     dist=sqrt((xy_dist*l)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     if ((nlcd_class(n_ident_row,n_ident_col).eq.
     &                  nodata_class).or.(nlcd_class(n_ident_row,
     &                  n_ident_col-1).eq.nodata_class)) then
                         y_30_dist=y_30*(l+1)
                         if (y_30_dist.gt.y_dist) then
                            n_ident_row=n_ident_row-1
                            n_ident_col=n_ident_col+1
                            y_dist=y_dist+y_inc
                            dist_2=y_dist-y_30_dist
                         else
                            n_ident_row=n_ident_row
                            n_ident_col=n_ident_col+1
                         end if
                         go to 20
                     end if
                     percent=(dist_2/dist_1)
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                 n_ident_col),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                 n_ident_col),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 25
                     if (l.eq.1) then
                        sum_surf_rough(j)=surf_rough_l*weight_factor
                        sum_weight(j)=weight_factor
                        y_30_dist=y_30*(l+1)
                        if (y_30_dist.gt.y_dist) then
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col+1
                           y_dist=y_dist+y_inc
                           dist_2=y_dist-y_30_dist
                        else
                           n_ident_row=n_ident_row
                           n_ident_col=n_ident_col+1
                           dist_2=y_dist-y_30_dist
                        end if
                        go to 20
                     end if
                     surf_rough_l2=surf_rough_l
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,n_ident_col
     &                 -1),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,n_ident_col
!TGA     &                 -1),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 25
                     surf_rough_wt_avg=(1-percent)*surf_rough_l2+
     &                  percent*surf_rough_l
                     sum_surf_rough(j)=sum_surf_rough(j)+
     &                   surf_rough_wt_avg*weight_factor
                     sum_weight(j)=sum_weight(j)+weight_factor
                     y_30_dist=y_30*(l+1)
                    
                     if (y_30_dist.gt.y_dist) then
                        n_ident_row=n_ident_row-1
                        n_ident_col=n_ident_col+1
                        y_dist=y_dist+y_inc
                        dist_2=y_dist-y_30_dist
                     else
                        n_ident_row=n_ident_row
                        n_ident_col=n_ident_col+1
                        dist_2=y_dist-y_30_dist
                     end if
20                continue
                  end do

25             continue

!    Direction four is due east (90 degrees - or between 75° and 105°)
                               
               elseif (j.eq.4) then
                  times=(total_dist/x_inc)
                  ntimes=(total_dist/x_inc)
                  decimal=times-ntimes
                  if (decimal.gt.0.5) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_col=n_col
                  do l=1,ntimes
                     if ((n_ident_col.le.0).or.(n_ident_col.gt.ncols)) 
     &                  go to 35
                     if (nlcd_class(n_row,n_ident_col).eq.nodata_class) 
     &                  go to 30
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_row,n_ident_col),
     &                  surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_row,n_ident_col),
!TGA     &                  surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 35
                     dist=sqrt(((l-1)*x_inc_r)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     sum_surf_rough(j)=sum_surf_rough(j)+ weight_factor*
     &                  surf_rough_l
                     sum_weight(j)=sum_weight(j)+weight_factor
                     n_ident_col=n_col+l
30                   continue
                     end do

35              continue

!    Direction five is 120 degrees - or between 105° and 135°

               elseif (j.eq.5) then
                  y_30=abs(x_inc*tan(deg2rad*30.0d0))
                  xy_dist=sqrt(y_30**2+x_inc**2)
                  times=abs(total_dist/xy_dist)
                  ntimes=abs(total_dist/xy_dist)
                  decimal=times-ntimes
                  if (decimal.gt.0.5) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  n_ident_col=n_col
                  dist_2=y_inc-y_30
                  dist_1=y_inc
                  y_dist=y_inc
                  do l=1,ntimes
                     if ((n_ident_row.le.0).or.(n_ident_col.le.0)) go to
     &                 45
                     if ((n_ident_row.gt.nrows).or.(n_ident_col.gt. 
     &                  ncols)) go to 45
                     dist=sqrt((xy_dist*l)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     if ((nlcd_class(n_ident_row,n_ident_col).eq.
     &                  nodata_class).or.(nlcd_class(n_ident_row,
     &                  n_ident_col-1).eq.nodata_class).or.
     &                  (nlcd_class(n_ident_row,n_ident_col).eq.nodata)
     &                  .or.(nlcd_class(n_ident_row,n_ident_col-1).eq.
     &                  nodata)) then
                        y_30_dist=y_30*(l+1)
                        if (y_30_dist.gt.y_dist) then
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col+1
                           y_dist=y_dist+y_inc
                           dist_2=y_dist-y_30_dist
                        else
                           n_ident_row=n_ident_row
                           n_ident_col=n_ident_col+1
                           dist_2=y_dist-y_30_dist
                        end if
                        go to 40
                     end if
                     percent=(dist_2/dist_1)
                     if (surf_rough_l.eq.-9999) goto 45
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (l.eq.1) then
                        sum_surf_rough(j)=surf_rough_l*weight_factor
                        sum_weight(j)=weight_factor
                        y_30_dist=y_30*(l+1)
                        if (y_30_dist.gt.y_dist) then
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col+1
                           y_dist=y_dist+y_inc
                           dist_2=y_dist-y_30_dist
                        else
                           n_ident_row=n_ident_row
                           n_ident_col=n_ident_col+1
                           dist_2=y_dist-y_30_dist
                        end if
                        go to 40
                        end if
                     surf_rough_l2=surf_rough_l
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,n_ident_col
     &                 -1),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,n_ident_col
!TGA     &                 -1),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 45
                     surf_rough_wt_avg=(1-percent)*surf_rough_l2+
     &                 percent*surf_rough_l
                     sum_surf_rough(j)=sum_surf_rough(j)+
     &                  surf_rough_wt_avg*weight_factor
                     sum_weight(j)=sum_weight(j)+weight_factor
                     y_30_dist=y_30*(l+1)
                     if (y_30_dist.gt.y_dist) then
                        n_ident_row=n_ident_row+1
                        n_ident_col=n_ident_col+1
                        y_dist=y_dist+y_inc
                        dist_2=y_dist-y_30_dist
                     else
                        n_ident_row=n_ident_row
                        n_ident_col=n_ident_col+1
                        dist_2=y_dist-y_30_dist
                     end if
40                continue
                  end do
45              continue

!    Direction six is 150 degrees - or between 135° and 165°

               elseif (j.eq.6) then
                  x_30=abs(y_inc*tan(deg2rad*30.0d0))
                  xy_dist=sqrt(y_inc**2+x_30**2)
                  times=abs(total_dist/xy_dist)
                  ntimes=abs(total_dist/xy_dist)
                  decimal=times-ntimes
                  if (decimal.gt.0.5) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  n_ident_col=n_col
                  dist_2=x_inc-x_30
                  dist_1=x_inc
                  x_dist=x_inc
                  do l=1,ntimes
                     if ((n_ident_row.le.0).or.(n_ident_col.le.0)) go to
     &                  55
                     if ((n_ident_row.gt.nrows).or.(n_ident_col.gt. 
     &                  ncols)) go to 55
                     dist=sqrt((xy_dist*l)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     if ((nlcd_class(n_ident_row,n_ident_col).eq.
     &                  nodata_class).or.(nlcd_class(n_ident_row,
     &                  n_ident_col-1).eq.nodata_class)) then
                        x_30_dist=x_30*(l+1)
                        if (x_30_dist.gt.x_dist) then
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col+1
                           x_dist=x_dist+x_inc
                           dist_2=x_dist-x_30_dist
                        else
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col
                           dist_2=x_dist-x_30_dist
                        end if
                        go to 50
                     end if
                     percent=(dist_2/dist_1)
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 55
                     if (l.eq.1) then
                        sum_surf_rough(j)=surf_rough_l*weight_factor
                        sum_weight(j)=weight_factor
                        x_30_dist=x_30*(l+1)
                        if (x_30_dist.gt.x_dist) then
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col+1
                           x_dist=x_dist+x_inc
                           dist_2=x_dist-x_30_dist
                        else
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col
                           dist_2=x_dist-x_30_dist
                        end if
                        go to 50
                     end if
                     surf_rough_l2=surf_rough_l
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col-1),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col-1),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 55
                     surf_rough_wt_avg=(1-percent)*surf_rough_l2+
     &                 percent*surf_rough_l
                     sum_surf_rough(j)=sum_surf_rough(j)+
     &                  surf_rough_wt_avg*weight_factor
                     sum_weight(j)=sum_weight(j)+weight_factor
                     x_30_dist=x_30*(l+1)
                     if (x_30_dist.gt.x_dist) then
                         n_ident_row=n_ident_row+1
                         n_ident_col=n_ident_col+1
                         x_dist=x_dist+x_inc
                         dist_2=x_dist-x_30_dist
                     else
                         n_ident_row=n_ident_row+1
                         n_ident_col=n_ident_col
                         dist_2=x_dist-x_30_dist
                     end if
50                continue
                  end do

55              continue

!    Direction seven is due south (180 degrees - or between 165° and 195°)

               elseif (j.eq.7) then
                  times=abs(total_dist/y_inc)
                  ntimes=abs(total_dist/y_inc)
                  decimal=times-ntimes
                  if (decimal.gt.0.5) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  do l=1,ntimes
                     if ((n_ident_row.le.0).or.(n_ident_row.gt.nrows))  
     &                  go to 65
                     if (nlcd_class(n_ident_row,n_col).eq.nodata_class) 
     &                  go to 60
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,n_col),
     &                  surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,n_col),
!TGA     &                  surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 65
                     dist=sqrt(((l-1)*y_inc_r)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     sum_surf_rough(j)=sum_surf_rough(j)+weight_factor*
     &                  surf_rough_l
                     sum_weight(j)=sum_weight(j)+weight_factor
                     n_ident_row=n_row+l
60                   continue
                     end do

65             continue
 
               elseif (j.eq.8) then
                  x_30=abs(y_inc*tan(deg2rad*30.0d0))
                  xy_dist=sqrt(y_inc**2+x_30**2)
                  times=abs(total_dist/xy_dist)
                  ntimes=abs(total_dist/xy_dist)
                  decimal=times-ntimes
                  if (decimal.gt.0.5) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  n_ident_col=n_col
                  dist_2=x_inc-x_30
                  dist_1=x_inc
                  x_dist=x_inc
                  do l=1,ntimes
                     if ((n_ident_row.le.0).or.(n_ident_col.le.0)) go to
     &                  75
                     if ((n_ident_row.gt.nrows).or.(n_ident_col.gt. 
     &                  ncols)) go to 75
                     dist=sqrt((xy_dist*l)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     if ((nlcd_class(n_ident_row,n_ident_col).eq.
     &                  nodata_class).or.(nlcd_class(n_ident_row,
     &                  n_ident_col-1).eq.nodata_class)) then
                        x_30_dist=x_30*(l+1)
                        if (x_30_dist.gt.x_dist) then
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col-1
                           x_dist=x_dist+x_inc
                           dist_2=x_dist-x_30_dist
                        else
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col
                           dist_2=x_dist-x_30_dist
                        end if
                        go to 70
                     end if
                     percent=(dist_2/dist_1)
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 75
                     if (l.eq.1) then
                        sum_surf_rough(j)=surf_rough_l*weight_factor
                        sum_weight(j)=weight_factor
                        x_30_dist=x_30*(l+1)
                        if (x_30_dist.gt.x_dist) then
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col-1
                           x_dist=x_dist+x_inc
                           dist_2=x_dist-x_30_dist
                        else
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col
                           dist_2=x_dist-x_30_dist
                        end if
                        go to 70
                     end if
                     surf_rough_l2=surf_rough_l
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,n_ident_col
     &                  -1),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,n_ident_col
!TGA     &                  -1),surf_rough_l,missing
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 75
                     surf_rough_wt_avg=(1-percent)*surf_rough_l2+
     &                    percent*surf_rough_l
                     sum_surf_rough(j)=sum_surf_rough(j)+
     &                  surf_rough_wt_avg*weight_factor
                     sum_weight(j)=sum_weight(j)+weight_factor
                     x_30_dist=x_30*(l+1)
                     if (x_30_dist.gt.x_dist) then
                         n_ident_row=n_ident_row+1
                         n_ident_col=n_ident_col-1
                         x_dist=x_dist+x_inc
                         dist_2=x_dist-x_30_dist
                     else
                         n_ident_row=n_ident_row+1
                         n_ident_col=n_ident_col
                         dist_2=x_dist-x_30_dist
                     end if
70                   continue
                     end do

75             continue

               elseif (j.eq.9) then
                  y_30=abs(x_inc*tan(deg2rad*30.0d0))
                  xy_dist=sqrt(y_30**2+x_inc**2)
                  times=abs(total_dist/xy_dist)
                  ntimes=abs(total_dist/xy_dist)
                  decimal=times-ntimes
                  if (decimal.gt.0.5) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  n_ident_col=n_col
                  dist_2=y_inc-y_30
                  dist_1=y_inc
                  y_dist=y_inc
                  do l=1,ntimes
                     if ((n_ident_row.le.0).or.(n_ident_col.le.0)) go to 
     &                  85
                     if ((n_ident_row.gt.nrows).or.(n_ident_col.gt. 
     &                  ncols)) go to 85
                     dist=sqrt((xy_dist*l)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     if ((nlcd_class(n_ident_row,n_ident_col).eq.
     &                  nodata_class).or.(nlcd_class(n_ident_row,
     &                  n_ident_col-1).eq.nodata_class)) then
                        y_30_dist=y_30*(l+1)
                        if (y_30_dist.gt.y_dist) then
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col-1
                           y_dist=y_dist+y_inc
                           dist_2=y_dist-y_30_dist
                        else
                           n_ident_row=n_ident_row
                           n_ident_col=n_ident_col-1
                           dist_2=y_dist-y_30_dist
                        end if
                        go to 80
                     end if
                     percent=(dist_2/dist_1)
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 85
                     if (l.eq.1) then
                        sum_surf_rough(j)=surf_rough_l*weight_factor
                        sum_weight(j)=weight_factor
                        y_30_dist=y_30*(l+1)
                        if (y_30_dist.gt.y_dist) then
                           n_ident_row=n_ident_row+1
                           n_ident_col=n_ident_col-1
                           y_dist=y_dist+y_inc
                           dist_2=y_dist-y_30_dist
                        else
                           n_ident_row=n_ident_row
                           n_ident_col=n_ident_col-1
                           dist_2=y_dist-y_30_dist
                        end if
                        go to 80
                        end if
                     surf_rough_l2=surf_rough_l
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col-1),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col-1),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 85
                     surf_rough_wt_avg=(1-percent)*surf_rough_l2+
     &                  percent*surf_rough_l
                     sum_surf_rough(j)=sum_surf_rough(j)+
     &                  surf_rough_wt_avg*weight_factor
                     sum_weight(j)=sum_weight(j)+weight_factor
                     y_30_dist=y_30*(l+1)
                     if (y_30_dist.gt.y_dist) then
                        n_ident_row=n_ident_row+1
                        n_ident_col=n_ident_col-1
                        y_dist=y_dist+y_inc
                        dist_2=y_dist-y_30_dist
                     else
                        n_ident_row=n_ident_row
                        n_ident_col=n_ident_col-1
                        dist_2=y_dist-y_30_dist
                     end if
80                   continue
                     end do

85             continue

               elseif (j.eq.10) then
                  times=abs(total_dist/x_inc)
                  ntimes=abs(total_dist/x_inc)
                  decimal=times-ntimes
                  if (decimal > half) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_col=n_col
                  do l=1,ntimes
                     if ((n_ident_col.le.0).or.(n_ident_col.gt.ncols)) 
     &                  go to 95
                     if (nlcd_class(n_row,n_ident_col).eq.nodata_class) 
     &                  go to 90
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_row,n_ident_col),
     &                  surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_row,n_ident_col),
!TGA     &                  surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 95
                     dist=sqrt(((l-1)*x_inc_r)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     sum_surf_rough(j)=sum_surf_rough(j)+weight_factor*
     &                  surf_rough_l
                     sum_weight(j)=sum_weight(j)+weight_factor
                     n_ident_col=n_col-l
90                   continue
                     end do

95             continue

               elseif (j.eq.11) then
                  y_30=abs(x_inc*tan(deg2rad*30.0d0))
                  xy_dist=sqrt(y_30**2+x_inc**2)
                  times=abs(total_dist/xy_dist)
                  ntimes=abs(total_dist/xy_dist)
                  decimal=times-ntimes
                  if (decimal.gt.0.5) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  n_ident_col=n_col
                  dist_2=y_inc-y_30
                  dist_1=y_inc
                  y_dist=y_inc
                  do l=1,ntimes
                     if ((n_ident_row.le.0).or.(n_ident_col.le.0)) go to
     &                  105
                     if ((n_ident_row.gt.nrows).or.(n_ident_col.gt. 
     &                  ncols)) go to 105
                     dist=sqrt((xy_dist*l)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     if ((nlcd_class(n_ident_row,n_ident_col).eq.
     &                  nodata_class).or.(nlcd_class(n_ident_row,
     &                  n_ident_col-1).eq.nodata_class)) then
                        y_30_dist=y_30*(l+1)
                        if (y_30_dist.gt.y_dist) then
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col-1
                           y_dist=y_dist+y_inc
                           dist_2=y_dist-y_30_dist
                        else
                           n_ident_row=n_ident_row
                           n_ident_col=n_ident_col-1
                           dist_2=y_dist-y_30_dist
                        end if
                        go to 100
                     end if
                     percent=(dist_2/dist_1)
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 105
                     if (l.eq.1) then
                        sum_surf_rough(j)=surf_rough_l*weight_factor
                        sum_weight(j)=weight_factor
                        y_30_dist=y_30*(l+1)
                        if (y_30_dist.gt.y_dist) then
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col-1
                           y_dist=y_dist+y_inc
                           dist_2=y_dist-y_30_dist
                        else
                           n_ident_row=n_ident_row
                           n_ident_col=n_ident_col-1
                           dist_2=y_dist-y_30_dist
                        end if
                        go to 100
                        end if
                     surf_rough_l2=surf_rough_l
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col-1),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col-1),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 105
                     surf_rough_wt_avg=(1-percent)*surf_rough_l2+
     &                  percent*surf_rough_l
                     sum_surf_rough(j)=sum_surf_rough(j)+
     &                  surf_rough_wt_avg*weight_factor
                     sum_weight(j)=sum_weight(j)+weight_factor
                     y_30_dist=y_30*(l+1)
                     if (y_30_dist.gt.y_dist) then
                        n_ident_row=n_ident_row-1
                        n_ident_col=n_ident_col-1
                        y_dist=y_dist+y_inc
                        dist_2=y_dist-y_30_dist
                     else
                        n_ident_row=n_ident_row
                        n_ident_col=n_ident_col-1
                        dist_2=y_dist-y_30_dist
                     end if
100                  continue
                     end do
105            continue
         
               elseif (j.eq.12) then
                  x_30=abs(y_inc*tan(deg2rad*30.0d0))
                  xy_dist=sqrt(y_inc**2+x_30**2)
                  times=abs(total_dist/xy_dist)
                  ntimes=abs(total_dist/xy_dist)
                  decimal=times-ntimes
                  if (decimal.gt.0.5) then
                      ntimes=ntimes+2
                  else
                      ntimes=ntimes+1
                  end if
                  n_ident_row=n_row
                  n_ident_col=n_col
                  dist_2=x_inc-x_30
                  dist_1=x_inc
                  x_dist=x_inc
                  do l=1,ntimes
                     if ((n_ident_row.le.0).or.(n_ident_col.le.0)) 
     &                  go to 115
                     if ((n_ident_row.gt.nrows).or.(n_ident_col.gt. 
     &                  ncols)) go to 115
                     dist=sqrt((xy_dist*l)**2)
                     weight_factor=exp(-(dist**2)/(2*weighted_dist**2))
                     if ((nlcd_class(n_ident_row,n_ident_col).eq.
     &                  nodata_class).or.(nlcd_class(n_ident_row,
     &                 n_ident_col-1).eq.nodata_class)) then
                        x_30_dist=x_30*(l+1)
                        if (x_30_dist.gt.x_dist) then
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col-1
                           x_dist=x_dist+x_inc
                           dist_2=x_dist-x_30_dist
                        else
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col
                           dist_2=x_dist-x_30_dist
                        end if
                        go to 110
                     end if
                     percent=(dist_2/dist_1)
CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation
                     if (surf_rough_l.eq.-9999) goto 115
                     if (l.eq.1) then
                        sum_surf_rough(j)=surf_rough_l*weight_factor
                        sum_weight(j)=weight_factor
                        x_30_dist=x_30*(l+1)
                        if (x_30_dist.gt.x_dist) then
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col-1
                           x_dist=x_dist+x_inc
                           dist_2=x_dist-x_30_dist
                        else
                           n_ident_row=n_ident_row-1
                           n_ident_col=n_ident_col
                           dist_2=x_dist-x_30_dist
                        end if
                        go to 110
                     end if
                     surf_rough_l2=surf_rough_l

CTGA-BEGIN:  Added for fort.13 generation automation
                     call surf_rough(nlcd_class(n_ident_row,
     &                  n_ident_col-1),surf_rough_l,missing,
     &                  lookuptable,nlc)
!TGA                     call surf_rough(nlcd_class(n_ident_row,
!TGA     &                  n_ident_col-1),surf_rough_l,missing)
CTGA-END:  Added for fort.13 generation automation

                     if (surf_rough_l.eq.-9999) goto 115
                     surf_rough_wt_avg=(1-percent)*surf_rough_l2+
     &                  percent*surf_rough_l
                     sum_surf_rough(j)=sum_surf_rough(j)+
     &                  surf_rough_wt_avg*weight_factor
                     sum_weight(j)=sum_weight(j)+weight_factor
                     x_30_dist=x_30*(l+1)
                     if (x_30_dist.gt.x_dist) then
                         n_ident_row=n_ident_row-1
                         n_ident_col=n_ident_col-1
                         x_dist=x_dist+x_inc
                         dist_2=x_dist-x_30_dist
                     else
                         n_ident_row=n_ident_row-1
                         n_ident_col=n_ident_col
                         dist_2=x_dist-x_30_dist
                     end if
110               continue 
                  end do
                end if                       

115            continue

           if (sum_surf_rough(j) == zero) then
              final_surf_rough(j) = missing
           else
              final_surf_rough(j)=sum_surf_rough(j)/sum_weight(j)
           end if
              
           end do
       nsum=0
       do j=1,ndir
          if (final_surf_rough(j) < zero) go to 120
       end do
       
       write(12,130) inum,(final_surf_rough(j), j=10,1,-1),
     &    (final_surf_rough(j), j=12,11,-1)

120    continue
       end do
      end if


130   format(i7,12(2x,f13.6))

      !--------------------------
      ! Stop CPU time "stopwatch"
      !--------------------------
CTGA-BEGIN:  Added for fort.13 generation automation
!TGA      cpuTime = toc()
!TGA
!TGA      !----------------------------------------------
!TGA      ! Calculate the CPU time required for execution
!TGA      !----------------------------------------------
!TGA      minutes = INT (cpuTime / 60.)
!TGA      seconds = AMOD(cpuTime , 60.)
!TGA
!TGA      WRITE (*,"(53('-'))")
!TGA      WRITE (*,"('Execution required ',i3,' minutes and ', f9.6, 
!TGA     &   ' seconds.')")  minutes, seconds
!TGA      WRITE (*,"(53('-'))")
CTGA-END:  Added for fort.13 generation automation

      end


CTGA-BEGIN:  Added for fort.13 generation automation
!TGA      SUBROUTINE Surf_rough(nlcd_class, surf_rough_l, missing)
      SUBROUTINE Surf_rough(nlcd_class, surf_rough_l, missing,
     &     lookuptable,nlc)
        real(8) lookuptable(nlc,2)
!TGA         !--------------------------------------------
!TGA         ! Force explicit declaration of all variables
!TGA         !--------------------------------------------
!TGA         implicit none
CTGA-END:  Added for fort.13 generation automation

         integer nlcd_class
         real*8 surf_rough_l
         real*8 missing

CTGA-BEGIN:  Added for fort.13 generation automation
!        Surface Roughness lengths
      k=0
      surf_rough_l=missing
      do j=1,nlc
         if (lookuptable(j,1).eq.nlcd_class) then
            surf_rough_l=lookuptable(j,2)
            k=k+1
         endif
      enddo
      if (k.gt.1) then
         write(*,*)'Error multiple values for land use code ',nlcd_class
      elseif (k.eq.0) then
         write(*,*)'Error no value for land use code ',nlcd_class
      endif
!TGA         SELECT CASE (nlcd_class)
!TGA            CASE (11)
!TGA               surf_rough_l = .001d0      ! Open Water
!TGA            CASE (12)
!TGA               surf_rough_l = .012d0      ! Perennial Ice/Snow
!TGA            CASE (21)
!TGA               surf_rough_l = .100d0      ! Developed - Open Space
!TGA            CASE (22)
!TGA               surf_rough_l = .300d0      ! Developed - Low Intensity
!TGA            CASE (23)
!TGA               surf_rough_l = .400d0      ! Developed - Medium Intensity
!TGA            CASE (24)
!TGA               surf_rough_l = .550d0      ! Developed - High Intensity
!TGA            CASE (31)
!TGA               surf_rough_l = .040d0      ! Barren Land (Rock/Sand/Clay)
!TGA            CASE (32)
!TGA               surf_rough_l = .090d0      ! Unconsolidated Shore
!TGA            CASE (41)
!TGA               surf_rough_l = .650d0      ! Deciduous Forest
!TGA            CASE (42)
!TGA               surf_rough_l = .720d0      ! Evergreen Forest
!TGA            CASE (43)
!TGA               surf_rough_l = .710d0      ! Mixed Forest
!TGA            CASE (51)
!TGA               surf_rough_l = .100d0      ! Dwarf Scrub
!TGA            CASE (52)
!TGA               surf_rough_l = .120d0      ! Shrub/Scrub
!TGA            CASE (71)
!TGA               surf_rough_l = .040d0      ! Grassland/Herbaceous
!TGA            CASE (72)
!TGA               surf_rough_l = .030d0      ! Sedge/Herbaceous
!TGA            CASE (73)
!TGA               surf_rough_l = .025d0      ! Lichens
!TGA            CASE (74)
!TGA               surf_rough_l = .020d0      ! Moss
!TGA            CASE (81)
!TGA               surf_rough_l = .060d0      ! Pasture/Hay
!TGA            CASE (82)
!TGA               surf_rough_l = .060d0      ! Cultivated Crops
!TGA            CASE (90)
!TGA               surf_rough_l = .550d0      ! Woody Wetlands
!TGA            CASE (91)
!TGA               surf_rough_l = .550d0      ! Palustrine Forested Wetland
!TGA            CASE (92)
!TGA               surf_rough_l = .120d0      ! Palustrine Scrub/Shrub Wetland
!TGA            CASE (93)
!TGA               surf_rough_l = .550d0      ! Estuarine Forested Wetland
!TGA            CASE (94)
!TGA               surf_rough_l = .120d0      ! Estuarine Scrub/Shrub Wetland
!TGA            CASE (95)
!TGA               surf_rough_l = .110d0      ! Emergent Herbaceous Wetlands
!TGA            CASE (96)
!TGA               surf_rough_l = .110d0      ! Palustrine Emergent Wetland (Persistent)
!TGA            CASE (97)
!TGA               surf_rough_l = .110d0      ! Estuarine  Emergent Wetland
!TGA            CASE (98)
!TGA               surf_rough_l = .030d0      ! Palustrine Aquatic Bed
!TGA            CASE (99)
!TGA               surf_rough_l = .030d0      ! Estuarine  Aquatic Bed
!TGA            CASE (127)
!TGA               surf_rough_l = missing     ! Missing - usually water boundaries
!TGA            CASE (-9999)
!TGA               surf_rough_l = missing     
!TGA            CASE DEFAULT
!TGA               surf_rough_l = missing     ! Missing
!TGA         END SELECT
CTGA-END:  Added for fort.13 generation automation

      RETURN
      END
      
! -----------------------------------------------
! Model the matlab tic function, for use with toc
! -----------------------------------------------
      SUBROUTINE tic 
      CALL cpu_time (before)
      END SUBROUTINE tic

! -----------------------------------------------
! Model the matlab toc function, for use with tic
! -----------------------------------------------
      REAL FUNCTION toc ( ) 
      CALL cpu_time (after)          ! Start CPU time "stopwatch"
      toc = after - before
      END FUNCTION toc
      


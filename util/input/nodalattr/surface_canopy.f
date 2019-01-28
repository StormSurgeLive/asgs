        program surface_canopy

!    ****************************************************************
!    *                                                              *
!    * Program to take a grid file and files downloaded from the    *
!    * National Land Coverage database and interpolate the surface  *
!    * canopy coefficient  onto the grid to be used in a nodal      *
!    * attribute file (fort.13).                                    *
!    *                                                              *
!    * The NLCD files used by this program (v1) are downloaded from *
!    * the NLCD 2001 website (http://www.mrlc.gov/mrlc2k_nlcd.asp)  *
!    * and converted into ascii format using the Raster to Ascii    *
!    * conversion in the ArcGIS toolbox.                            *                 
!    *                                                              *
!    * The output is in fort.13 format.  The header information     *
!    * will have to be added.  The default value will be 1, the     *
!    * values printed from this program are the 0 values (which     *
!    * zeroes out the wind stress in forested areas).               *
!    *                                                              *
!    * An input file is required.  The input file is called         *
!    * surface_canopy.in and contains:                              *
!    *                                                              *
!    *      Name of the grid file                                   *
!    *      Name of the output file to create                       *
!    *      NLCD datafile name                                      *
!    *                                                              *
!    *           v1.0 - 5/21/2007                                   *
!    *           v2.0 - 10/13/2007 - bug fix to allow for -9999 as  *
!    *                               a no data class value          *
!    *                                                              *
!    *                 Crystal Fulcher                              *
!    *                                                              *
!    *          v3.0 - drc - June 06 2008- added albers -           *
!    *			grid file should be in long/lat                     *
!    *          v4.0 -  cwf - 8/24/2009 - bug fix to allow for      *
!    *                             input file described above       *
!    *          v5.0 -  cwf - 10/25/2010 - changed from parameter   *
!    *                 statements for maxrow and maxcol variables   *
!    *                 to allocatable variables, so no changes have *
!    *                 to be made to program before  use no matter  *
!    *                 the size of the landcover file.              *
!    ****************************************************************


      character*10 ndum
      character*90 gridfile,outputfile,inputfile
      character*60 header

      integer, allocatable ::  nlcd_class(:,:)

      real*8 x_toplt,y_toplt
      real*8 x_botrt,y_botrt
      real*8 x_temp,y_temp
      real*8 x_inc,y_inc
      real*8 x_diff, y_diff

        integer n_canopy

        INTEGER num1,num2,num3,num4,num5,nm1
        DOUBLE PRECISION :: E1,M1,M2,ALPHAA,ALPHA0
        DOUBLE PRECISION :: ALPHAB,ALPHA1,ALPHAC,ALPHA2
        DOUBLE PRECISION :: N,C,RHO0,E,RAD,O
        DOUBLE PRECISION :: LON,LAT,ALPHA,THETA,P,RHO
      DOUBLE PRECISION :: x2,y2,x,y
      INTEGER(kind=8) x1,y1

CTGA-BEGIN:  Added for fort.13 generation automation
      character*60 fillc2scc
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

!    Open the input file required to run this program and also open the NCLD
!    files and read the header information into arrays for future use.  Compute
!    the number of columns and rows in the files for ease in finding the right
!    node to correspond to a grid node.

CTGA-BEGIN:  Added for fort.13 generation automation
      CALL GETARG(1,gridfile)
      CALL GETARG(2,outputfile)
      CALL GETARG(3,inputfile)
      CALL GETARG(4,fillc2scc)
      open(20,file=fillc2scc)
      read(20,*)nlc
      allocate(lookuptable(nlc,2))
      do j=1,nlc
         read(20,*)lookuptable(j,1:2)
      enddo
!TGA      open(10,file='surface_canopy.in')
!TGA      read(10,5) gridfile
!TGA      read(10,5) outputfile
!TGA      read(10,5) inputfile
CTGA-END:  Added for fort.13 generation automation

5     format(a30)

      open(13,file=inputfile)
      read(13,*) ndum,ncols
      read(13,*) ndum,nrows
      read(13,*) ndum,x_botlt
      read(13,*) ndum,y_botlt
      read(13,*) ndum,x_y_dist
      read(13,*) ndum,nodata
      
      allocate  ( nlcd_class(nrows,ncols) )    

      x_inc=x_y_dist
      y_inc=x_y_dist

      x_botrt=x_botlt+(ncols-1)*x_inc
      y_botrt=y_botlt
      x_toplt=x_botlt
      y_toplt=y_botlt+(nrows-1)*y_inc

10    format(a60)

      do j=1,nrows
         read(13,*) (nlcd_class(j,k),k=1,ncols)
         enddo

!    Open the grid file, create the output file and read in the header information from
!    the grid file.

      open(11,file=gridfile)
      open(12,file=outputfile)
        read(11,10) header
      read(11,*) nele,nnodes


!    Read the nodal information from the grid file and from it figure out the 
!    right NLCD file to use by finding if the node location falls in between the
!    top left coordinates and bottom right coordinates of the NLCD file.  When 
!    the correct file is found, find the place in the file to go to to get the 
!    NLCD Class number for the nodal location and then convert that NLCD class 
!    number to it's appropriate Mannings-n coefficient.  This is then written to 
!    the output file.  If a location is not found for the node then -9999.0 is written
!    to the file.

      do i=1,nnodes
            read(11,*) inum,LON,LAT
              P = O*DLOG((1-E*DSIN(LAT*RAD))/(1+E*DSIN(LAT*RAD))) 
              ALPHA = E1*DSIN(LAT*RAD)/(1-EE*DSIN(LAT*RAD)**2)-P  
              THETA = N*(LON-LON0) 
              RHO = A*((C-N*ALPHA)**(0.5))/N
              x2 = RHO*DSIN(THETA*RAD)
              y2 = RHO0-RHO*DCOS(THETA*RAD)
            x1 = ANINT(x2*100.0)
            y1 = ANINT(y2*100.0)
              x = x1/100.0
            y = y1/100.0
          if ((x.gt.x_toplt).and.(x.lt.x_botrt).and.
     &       (y.lt.y_toplt).and.(y.gt.y_botrt)) then
              x_diff=x_toplt-x
            y_diff=y_toplt-y
            n_col=abs(x_diff/x_inc)
            col=abs(x_diff/x_inc)
            decimal=col-n_col
            if (decimal.gt.0.5) n_col=n_col+1
            n_row=abs(y_diff/y_inc)
            row=abs(y_diff/y_inc)
            decimal=row-n_row
            if (decimal.gt.0.5) n_row=n_row+1
!	      ident=n_row*ncols+(n_col+1)
CTGA-BEGIN:  Added for fort.13 generation automation
            call surface_canopy_value(nlcd_class(n_row,n_col),
     &           lookuptable,nlc,n_canopy)
!TGA            call surface_canopy_value(nlcd_class(n_row,n_col),n_canopy)
CTGA-END:  Added for fort.13 generation automation

            if (n_canopy.eq.0) then
               write(12,*) inum,n_canopy
               endif
         endif               
      enddo

      end

CTGA-BEGIN:  Added for fort.13 generation automation
!TGA      SUBROUTINE surface_canopy_value(nlcd_class,n_canopy)
      SUBROUTINE surface_canopy_value(nlcd_class,lookuptable,nlc,
     &     n_canopy)
      real(8) lookuptable(nlc,2)
CTGA-END:  Added for fort.13 generation automation

      integer n_canopy

!    Mannings n values
CTGA-BEGIN:  Added for fort.13 generation automation
      k=0
      n_canopy=-99999
      do j=1,nlc
         if (lookuptable(j,1).eq.nlcd_class) then
            n_canopy=lookuptable(j,2)
            k=k+1
         endif
      enddo
      if (k.gt.1) then
         write(*,*)'Error multiple values for land use code ',nlcd_class
      elseif (k.eq.0) then
         write(*,*)'Error no value for land use code ',nlcd_class
      endif
!TGA      n_canopy=-9999
!TGA      if (nlcd_class.eq.11) n_canopy=1   !Open Water
!TGA      if (nlcd_class.eq.12) n_canopy=1   !Perennial Ice/Snow
!TGA      if (nlcd_class.eq.21) n_canopy=1   !Developed - Open Space
!TGA      if (nlcd_class.eq.22) n_canopy=1   !Developed - Low Intensity
!TGA      if (nlcd_class.eq.23) n_canopy=1   !Developed - Medium Intensity
!TGA      if (nlcd_class.eq.24) n_canopy=1   !Developed - High Intensity
!TGA      if (nlcd_class.eq.31) n_canopy=1   !Barren Land (Rock/Sand/Clay)
!TGA      if (nlcd_class.eq.32) n_canopy=1   !Unconsolidated Shore
!TGA      if (nlcd_class.eq.41) n_canopy=0   !Deciduous Forest
!TGA      if (nlcd_class.eq.42) n_canopy=0   !Evergreen Forest
!TGA      if (nlcd_class.eq.43) n_canopy=0   !Mixed Forest
!TGA      if (nlcd_class.eq.51) n_canopy=1   !Dwarf Scrub
!TGA      if (nlcd_class.eq.52) n_canopy=1   !Shrub/Scrub
!TGA      if (nlcd_class.eq.71) n_canopy=1   !Grassland/Herbaceous
!TGA      if (nlcd_class.eq.72) n_canopy=1   !Sedge/Herbaceous
!TGA      if (nlcd_class.eq.73) n_canopy=1   !Lichens
!TGA      if (nlcd_class.eq.74) n_canopy=1   !Moss
!TGA      if (nlcd_class.eq.81) n_canopy=1   !Pasture/Hay
!TGA      if (nlcd_class.eq.82) n_canopy=1   !Cultivated Crops
!TGA      if (nlcd_class.eq.90) n_canopy=0   !Woody Wetlands
!TGA      if (nlcd_class.eq.91) n_canopy=0   !Palustrine Forested Wetland
!TGA      if (nlcd_class.eq.92) n_canopy=0   !Palustrine Scrub/Shrib Wetland
!TGA      if (nlcd_class.eq.93) n_canopy=0   !Estuarine Forested Wetland
!TGA      if (nlcd_class.eq.94) n_canopy=1   !Estuarine Scrub/Shrub Wetland
!TGA      if (nlcd_class.eq.95) n_canopy=1   !Emergent Herbaceous Wetlands
!TGA      if (nlcd_class.eq.96) n_canopy=1   !Palustrine Emergent Wetland (Persistant)
!TGA      if (nlcd_class.eq.97) n_canopy=1   !Estuarine Emergent Wetland
!TGA      if (nlcd_class.eq.98) n_canopy=1   !Palustrine Aquatic Bed
!TGA      if (nlcd_class.eq.99) n_canopy=1   !Estuarine Aquatic Bed
!TGA      if (nlcd_class.eq.127) n_canopy=1  !No data class
!TGA      if (nlcd_class.eq.-9999) n_canopy=1  !No data class
!TGA      if (nlcd_class.eq.0) n_canopy=1  !No data class
!TGA      if (n_canopy.eq.-9999) then
!TGA         write(*,*)"Undefined land cover class: ",nlcd_class
!TGA      endif
      
      RETURN

      END


                  
            





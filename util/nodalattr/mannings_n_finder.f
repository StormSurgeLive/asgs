      program mannings_n_finder


C    ****************************************************************
C    *                                                              *
C    * Program to take a grid file and files downloaded from the    *
C    * National Land Coverage database and interpolate the          *
C    * mannings-n value onto the grid to be used in a nodal         *
C    * attribute file (fort.13).                                    *
C    *                                                              *
C    * The NLCD files used by this program are downloaded from      *
C    * the NLCD 2001 website (http://www.mrlc.gov/mrlc2k_nlcd.asp)  *
C    * and converted into ascii format using the Raster to Ascii    *
C    * conversion in the ArcGIS toolbox.                            *                 
C    *                                                              *
C    * The output is in fort.13 format.  The header information     *
C    * will have to be added.  The default value will be 0, the     *
C    * values printed from this program are the other mannings-n    *
C    * values
C    *                                                              *
C    * An input file is required.  The input file is called         *
C    * mannings_n.in and contains:                                  *
C    *                                                              *
C    *      Name of the grid file                                   *
C    *      Name of the output file to create                       *
C    *      NLCD datafile name                                      *
C    *                  Crystal Fulcher                             *
C    *                                                              *
C    *                 v1.0 - generic version - 2/21/2007           *
C    *                 v2.0 - changes to make this compatible with  *
C    *                        one single NLCD file. 4/5/2007        *
C    *                 v3.0 - further simplify the program and      *
C    *                        also allow for rounding up when       *
C    *                        finding the closest point             *
C    *                 v4.0 - Output in fort.13 format (matlab      *
C    *                        format is still in here also for      *
C    *                        checking) - 5/21/2007                 *
C    *                 v5.0 - Fixed bug so it would not print out   *
C    *                        nodes where mannings-n equals 0       *
C    *                        6/14/2007                             *
C    *                 v6.0 - Fixed the open water mannings-n value *
C    *                        to be 0.02 instead of 0.001           *
C    *                        1/8/2007                              *
C    *                       Crystal Fulcher                        *
C    *                v7.0 - drc - June 06 2008- added albers -     *
C    *                       grid file should be in long/lat        *
C    *          v8.0 -  cwf - 8/24/2009 - bug fix to allow for      *
C    *                             input file described above       *
C    *          v9.0 -  cwf - 10/25/2010 - changed from parameter   *
C    *                 statements for maxrow and maxcol variables   *
C    *                 to allocatable variables, so no changes have *
C    *                 to be made to program before  use no matter  *
C    *                 the size of the landcover file.              *
C    *          v10.0 - cwf - 10/26/2010 - header corrections       *
C    ****************************************************************


      character*10 ndum
      character*120 gridfile, outputfile, inputfile
      character*60 header

      integer, allocatable ::  nlcd_class(:,:)

      real*8 x_toplt,y_toplt
      real*8 x_botrt,y_botrt
      real*8 x_temp,y_temp
      real*8 x_inc,y_inc
      real*8 x_diff, y_diff

      real mannings_n

      INTEGER num1,num2,num3,num4,num5,nm1
      DOUBLE PRECISION :: E1,M1,M2,ALPHAA,ALPHA0
      DOUBLE PRECISION :: ALPHAB,ALPHA1,ALPHAC,ALPHA2
      DOUBLE PRECISION :: N,C,RHO0,E,RAD,O
      DOUBLE PRECISION :: LON,LAT,ALPHA,THETA,P,RHO
      DOUBLE PRECISION :: x2,y2,x,y
      INTEGER(kind=8) x1,y1

CTGA-BEGIN:  Added for fort.13 generation automation
      character*60 fillc2mnasf
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


C    Open the NCLD files and read the header information into arrays for future use.  
C    Compute the number of columns and rows in the files for ease in finding the right
C    node to correspond to a grid node.

CTGA-BEGIN:  Added for fort.13 generation automation
      CALL GETARG(1,gridfile)
      CALL GETARG(2,outputfile)
      CALL GETARG(3,inputfile)
      CALL GETARG(4,fillc2mnasf)
      open(20,file=fillc2mnasf)
      read(20,*)nlc
      allocate(lookuptable(nlc,2))
      do j=1,nlc
         read(20,*)lookuptable(j,1:2)
      enddo
!TGA      open(10,file='mannings_n.in')
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

10      format(a60)

        do j=1,nrows
C           n_row_temp=(j-1)*ncols
           read(13,*) (nlcd_class(j,k),k=1,ncols)
      enddo


        mannings_n=0.0

C    Open the grid file, create the output file and read in the header information from
C    the grid file.

      open(11,file=gridfile)
      open(12,file=outputfile)
      read(11,10) header
      read(11,*) nele,nnodes


C    Read the nodal information from the grid file and from it figure out the 
C    right NLCD file to use by finding if the node location falls in between the
C    top left coordinates and bottom right coordinates of the NLCD file.  When 
C    the correct file is found, find the place in the file to go to to get the 
C    NLCD Class number for the nodal location and then convert that NLCD class 
C    number to its appropriate Mannings-n coefficient.  This is then written to 
C    the output file.  If a location is not found for the node then -9999.0 is written
C    to the file.

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
C              ident=n_row*ncols+(n_col+1)
CTGA-BEGIN:  Added for fort.13 generation automation
              call mannings_n_value(nlcd_class(n_row,n_col),lookuptable,
     &            nlc,mannings_n)
!TGA              call mannings_n_value(nlcd_class(n_row,n_col),mannings_n)
CTGA-END:  Added for fort.13 generation automation

              if (mannings_n.eq.0.0) goto 20
 
              write(12,*) inum,mannings_n

20             continue

           endif
           mannings_n=0.0
      enddo

      end

CTGA-BEGIN:  Added for fort.13 generation automation
!TGA      SUBROUTINE Mannings_n_value(nlcd_class,mannings_n)
      SUBROUTINE Mannings_n_value(nlcd_class,lookuptable,nlc,mannings_n)
      real(8) lookuptable(nlc,2)
CTGA-END:  Added for fort.13 generation automation

      real mannings_n

C    Mannings n values

CTGA-BEGIN:  Added for fort.13 generation automation
      k=0
      mannings_n=-99999
      do j=1,nlc
         if (lookuptable(j,1).eq.nlcd_class) then
            mannings_n=lookuptable(j,2)
            k=k+1
         endif
      enddo
      if (k.gt.1) then
         write(*,*)'Error multiple values for land use code ',nlcd_class
      elseif (k.eq.0) then
         write(*,*)'Error no value for land use code ',nlcd_class
      endif
!TGA        if (nlcd_class.eq.11) mannings_n=0.02    !Open Water
!TGA        if (nlcd_class.eq.12) mannings_n=0.010   !Perennial Ice/Snow
!TGA        if (nlcd_class.eq.21) mannings_n=0.020   !Developed - Open Space
!TGA        if (nlcd_class.eq.22) mannings_n=0.050   !Developed - Low Intensity
!TGA        if (nlcd_class.eq.23) mannings_n=0.100   !Developed - Medium Intensity
!TGA        if (nlcd_class.eq.24) mannings_n=0.150   !Developed - High Intensity
!TGA        if (nlcd_class.eq.31) mannings_n=0.090   !Barren Land (Rock/Sand/Clay)
!TGA        if (nlcd_class.eq.32) mannings_n=0.040   !Unconsolidated Shore
!TGA        if (nlcd_class.eq.41) mannings_n=0.100   !Deciduous Forest
!TGA        if (nlcd_class.eq.42) mannings_n=0.110   !Evergreen Forest
!TGA        if (nlcd_class.eq.43) mannings_n=0.100   !Mixed Forest
!TGA        if (nlcd_class.eq.51) mannings_n=0.040   !Dwarf Scrub
!TGA        if (nlcd_class.eq.52) mannings_n=0.050   !Shrub/Scrub
!TGA        if (nlcd_class.eq.71) mannings_n=0.034   !Grassland/Herbaceous
!TGA        if (nlcd_class.eq.72) mannings_n=0.030   !Sedge/Herbaceous
!TGA        if (nlcd_class.eq.73) mannings_n=0.027   !Lichens
!TGA        if (nlcd_class.eq.74) mannings_n=0.025   !Moss
!TGA        if (nlcd_class.eq.81) mannings_n=0.033   !Pasture/Hay
!TGA        if (nlcd_class.eq.82) mannings_n=0.037   !Cultivated Crops
!TGA        if (nlcd_class.eq.90) mannings_n=0.100   !Woody Wetlands
!TGA        if (nlcd_class.eq.91) mannings_n=0.100   !Palustrine Forested Wetland
!TGA        if (nlcd_class.eq.92) mannings_n=0.048   !Palustrine Scrub/Shrib Wetland
!TGA        if (nlcd_class.eq.93) mannings_n=0.100   !Estuarine Forested Wetland
!TGA        if (nlcd_class.eq.94) mannings_n=0.048   !Estuarine Scrub/Shrub Wetland
!TGA        if (nlcd_class.eq.95) mannings_n=0.045   !Emergent Herbaceous Wetlands
!TGA        if (nlcd_class.eq.96) mannings_n=0.045   !Palustrine Emergent Wetland (Persistant)
!TGA        if (nlcd_class.eq.97) mannings_n=0.045   !Estuarine Emergent Wetland
!TGA        if (nlcd_class.eq.98) mannings_n=0.015   !Palustrine Aquatic Bed
!TGA        if (nlcd_class.eq.99) mannings_n=0.015   !Estuarine Aquatic Bed
CTGA-END:  Added for fort.13 generation automation
      
        RETURN

        END

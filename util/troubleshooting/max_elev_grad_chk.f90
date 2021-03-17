      program max_elev_grad_chk
      
C    ****************************************************************
C    *                                                              *
C    * Program to read in the grid and the maximum elevation file   *
C    * and compute the gradient value for each element.  If the     *
C    * gradient value for an element is greater than the desired    *
C    * warning value (set below - WarnElevGrad) then write out the  *
C    * element number and gradient value to a file called           *
C    * grad_warn.out.                                               *
C    *                                                              *
C    * This program is set up to read the input files with standard *
C    * names - grid file is fort.14 (and is in lat/lon) and the     *
C    * maximum elevation file is maxele.63.                         *
C    *                                                              *
C    *             Crystal Fulcher - 4/7/2011                       *
C    *                 UNC-IMS                                      *
C    *                                                              *
C    ****************************************************************      
       
      integer MNE, MNP, numnode, numele
      integer ndum
      integer NM1, NM2, NM3
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NM
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODECODE
      REAL*8, DIMENSION(:), ALLOCATABLE :: SLAM
      REAL*8, DIMENSION(:), ALLOCATABLE :: SFEA
      REAL*8, DIMENSION(:), ALLOCATABLE :: X
      REAL*8, DIMENSION(:), ALLOCATABLE :: Y
      REAL*8, DIMENSION(:), ALLOCATABLE :: SFAC
      REAL*8, DIMENSION(:), ALLOCATABLE :: Z
      REAL*8, DIMENSION(:), ALLOCATABLE :: ETA2
      REAL*8, DIMENSION(:), ALLOCATABLE :: AREAS
      REAL*8 SLAM0, SFEA0, SLAM0R, SFEA0R, WarnElevGrad

       

      PI=3.141592653589793d0
      WarnElevGrad = 0.02d0
      SLAM0=-79.0
      SFEA0=35.0
      SLAM0R=SLAM0*PI/180.0d0
      SFEA0R=SFEA0*PI/180.0d0
       
      open(14,file='fort.14')
      open(63,file='maxele.63')
      open(16,file='grad_warn.out')

C   Read in the grid file, convert the lat,lon to x,y, read in elements and set up for future
C   calculations
       
      read(14,*)
      read(14,*) MNE, MNP

      ALLOCATE ( SLAM(MNP) )
      ALLOCATE ( SFEA(MNP) )
      ALLOCATE ( X(MNP) )
      ALLOCATE ( Y(MNP) )
      ALLOCATE ( NM(MNE,3) )
      ALLOCATE ( Z(MNP) )
      ALLOCATE ( ETA2(MNP) )
      ALLOCATE ( NODECODE(MNP) )
      ALLOCATE ( AREAS(MNE) )
      ALLOCATE ( SFAC(MNP) )

      do i=1,MNP
         read(14,*) numnode,SLAM(i),SFEA(i),z(numnode)
           SLAM(I)=SLAM(I)*PI/180.0d0
           SFEA(I)=SFEA(I)*PI/180.0d0
           CALL CPP(X(I),Y(I),SLAM(I),SFEA(I),SLAM0R,SFEA0R)
           SFAC(I)=COS(SFEA0R)/COS(SFEA(I))        
         enddo
      do i=1,MNE
         read(14,*) numele,ndum,nm(numele,1),nm(numele,2),nm(numele,3)
         X1=X(NM(I,1))
         X2=X(NM(I,2))
         X3=X(NM(I,3))
         Y1=Y(NM(I,1))
         Y2=Y(NM(I,2))
         Y3=Y(NM(I,3))
         AREAS(I)=(X1-X3)*(Y2-Y3)+(X3-X2)*(Y1-Y3)
         enddo

C   Read in the max elevation file and calculate the water level for each node, if water level
C   is less than zero, the node is dry and the nodecode=0, if the water level is greater than zero
C   the node is wet and the nodecode=1.
        
      read(63,*)
      read(63,*)
      read(63,*)
      do i=1,MNP
         read(63,*) numnode,eta2(numnode)
         water_lvl=z(i)+eta2(i)
         if (water_lvl.gt.0) then
            NODECODE(i)=1
         else
            NODECODE(i)=0
         endif
         enddo
C   Loop over all the elements and calculate the gradient for each element.
C   If the gradient is greater than the warning gradient, write out the 
C   element number and the gradient value.       

      DO IE=1,MNE
         NM1=NM(IE,1)
         NM2=NM(IE,2)
         NM3=NM(IE,3)
         NC1=NODECODE(NM1)
         NC2=NODECODE(NM2)
         NC3=NODECODE(NM3)
C         NCEle=NC1*NC2*NC3*NOFF(IE)
         NCEle=NC1*NC2*NC3
 
         IF(NCEle.EQ.0) Then
            dEta2Mag=-9999.
         ELSE
            SFacAvg=(SFAC(NM1)+SFAC(NM2)+SFAC(NM3))/3.d0
            AreaIE2=Areas(IE)
            FDX1=(Y(NM2)-Y(NM3))*SFacAvg !b1
            FDX2=(Y(NM3)-Y(NM1))*SFacAvg !b2
            FDX3=(Y(NM1)-Y(NM2))*SFacAvg !b3
            FDY1=X(NM3)-X(NM2)  !a1
            FDY2=X(NM1)-X(NM3)  !a2
            FDY3=X(NM2)-X(NM1)  !a3
            dEta2Dx = (Eta2(NM1)*FDX1+Eta2(NM2)*FDX2+Eta2(NM3)*FDX3)
     &                                                     /AreaIE2
            dEta2Dy = (Eta2(NM1)*FDY1+Eta2(NM2)*FDY2+Eta2(NM3)*FDY3)
     &                                                     /AreaIE2
      
            dEta2Mag = sqrt(dEta2Dx*dEta2Dx+dEta2Dy*dEta2Dy)
         ENDIF
         IF(dEta2Mag.GE.WarnElevGrad) THEN
            write(16,*) IE,dEta2Mag
            endif
         enddo
      END

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


      PROGRAM tau0_gen  
!
!   Robert Weaver
!   Oct 31, 2008 
!
! this program will generate data for tau0 base to be used in the fort.13 file
! the data is formatted to be read in when the tau0 flag is set to -3.0 in the fort.15 file 
! for versions fo ADCIRC later than 47.31 and older versions of the code
! which may have been modified seperately from that kept on the svn repository
! For these versions, setting the flag of tau0 = -3.0 implies that a base value for tau0 
! will be read in from the fort.13 file.   


! For tau0 = -3  we need only one value for tau0 ( a min or base value) in the fort.13 file, 
! the variable is *primitive_weighting_in_continuity_equation*


! The simple Fortran program makes use of a subroutine from the adcirc source code, 
! to find the neighboring elements.  If the average distance between a node and its
! neighbors is less than or equal to a critical value (currently set to 1750 m) 
! then tau0 base will be set to the time varying flag value of 0.03.


! Otherwise the base value is depth dependant:
! 0.005 for depths greater than 10m
! 0.02  for depths less than 10m


! in order to keep the file size down, this program sets the default
! value to 0.03 and only writes out the points where the distance 
! between nodes is greater than the critical distance


      IMPLICIT NONE
      integer nodes,nelement,nno, nel
      integer numattr ,N
      real,dimension(:),allocatable::  node, h, x, y, dx_avg       !(nodes)
      real,dimension(:),allocatable::  declongitude, declatitude !(nodes)
      real,dimension(:),allocatable::  tau0_min, tau0_max !(nodes)
      integer,dimension(:),allocatable:: n1, n2, n3 !(nelement)
      integer,dimension(:,:),allocatable:: nm !(nelement)
      integer,dimension(:),allocatable:: nelem, neigh !(nelement)
      integer,dimension(:),allocatable:: var_node !(nodes)
      integer NEIMIN,NEIMAX,varnode_count 
      INTEGER,dimension(:),allocatable :: NNeigh
      INTEGER,dimension(:,:),allocatable :: NeiTab, NeiTabEle
      real dx_crit,RLAMBDA0,PHI0,PI,R,DEG2RAD, tau0_default
      integer nt, i, j, k,NHG,t
      character*30  runid

CTGA-BEGIN:  Added for fort.13 generation automation
      character*90 gridfile,outputfile
      CALL GETARG(1,gridfile)
      call getarg(2,outputfile)
      open(unit=19,file=gridfile,status='old')  
!TGA      open(unit=19,file='fort.14',status='old')  
CTGA-END:  Added for fort.13 generation automation
       
      read(19,*)  runid
      read(19,*)  nelement, nodes  
      allocate(node(nodes),h(nodes),dx_avg(nodes), tau0_min(nodes),
     &tau0_max(nodes) )
      allocate(x(nodes),y(nodes),declongitude(nodes),declatitude(nodes))
      allocate( n1(nelement), n2(nelement), n3(nelement))
      allocate( nm(nelement,3))
      allocate( nelem(nelement), neigh(nelement))
      allocate( NNeigh(nodes), NeiTab(nodes,15),NeiTabEle(nodes,15))
      allocate(var_node(nodes))


! read in element locations 
      PI=4.0d0*atan(1.0d0)
      R=6378206.4d0
      DEG2RAD=PI/180.0d0
!        write (*,*) " read nodes"
      do 10 nno = 1, nodes


         read(19,*) node(nno),declongitude(nno),declatitude(nno),h(nno)
              X(nno) = ((360.0+declongitude(nno))-279.d0)*R*DEG2RAD
     &        *cos(29.d0*DEG2RAD)
              Y(nno) = (declatitude(nno)-29.d0)*R*DEG2RAD
10    continue 
!        write(*,*) " read in nodes and converted from lat lon to cartesian coordinates"


! read in element neighbors 


      do 20 nel = 1, nelement


         read(19,*)nelem(nel),neigh(nel),nm(nel,1),nm(nel,2),nm(nel,3) 


20    continue 
!       write(*,*) " read in element table " 


! calculate neighbor table to find each neighboring node
! then compute  distance to each neighboring node
! then average all distances
!          write(*,*) " calling subroutine to compute node neighbor table and distances"
       CALL NEIGHB(nelement,nodes,nm,NNeigh,NeiTab,NeiTabEle,NEIMIN,
     & NEIMAX,X,Y,dx_avg)
!          write(*,*) "returning from subroutine to compute node neighbor table and distances"


! now we have average dx for each node
! test to see if greater than dx_crit
! and write fort.13 accordingly
         tau0_default=0.030d0 
         dx_crit=1750.00
              varnode_count=0
           DO N=1,nodes
             if ( dx_avg(N) .ge. dx_crit ) then
               if  ( h(N) .le. 10.0 ) then
                varnode_count=varnode_count+1
                var_node(varnode_count) = N  
                tau0_min(varnode_count) = 0.020d0
               else
                varnode_count=varnode_count+1
                var_node(varnode_count) = N  
                tau0_min(varnode_count) = 0.0050d0
               endif
             endif
           ENDDO


! write output in fort.13 format for one attribute


CTGA-BEGIN:  Added for fort.13 generation automation
!TGA      numattr=1
!TGA        open(unit=13,file='fort.13.tau0base',status='unknown')
!TGA        write(13,*) "Coefficients for FL grid"
!TGA        write(13,'(i8)') nodes
!TGA        write(13,'(i8)') numattr
!TGA          write(13,'(a42)') "primitive_weighting_in_continuity_equation"
!TGA          write(13,'(i4)') 1
!TGA          write(13,'(i4)') 1
!TGA          write(13,'(f13.6)') tau0_default
!TGA          
!TGA! now input non_default data
!TGA          write(13,'(a42)') "primitive_weighting_in_continuity_equation"
!TGA          write(13,'(i8)') varnode_count
          
         open(unit=13,file=outputfile,status='unknown')
         do 65  N = 1, varnode_count
             write(13,'(i8,f13.6)') var_node(N), tau0_min(N)
65         continue


      deallocate(node,h,dx_avg)
      deallocate(x,y,declongitude,declatitude)
      deallocate( n1, n2, n3,var_node)
      deallocate( nelem, neigh,tau0_min, tau0_max)


      STOP


      END
       
!******************************************************************************
!                                                                             *
!      Subroutine to generate neighbor tables from a connectivity table.      *
!                                                                             *
!      NOTES                                                                  *
!      a node neighbor table is generated with the node itself is listed as   *
!         neighbor #1 and all other neighbors are sorted and placed in cw     *
!         order from east                                                     *
!      a neighbor element table is generated with:                            *
!         entry 1 = element # defined by neighbors 1,2,3                      *
!         entry 2 = element # defined by neighbors 1,3,4                      *
!         entry 3 = element # defined by neighbors 1,4,5                      *
!          .......                                                            *
!         entry last = element # defined by neighbors 1,nneigh,2              *
!         a zero area means that the defined triangle lies outside the domain *
!                                                                             *
!                                                                             *
!    v1.0   R.L.   6/29/99  used in 3D code                                   *
!    v2.0   R.L.   5/23/02  adapted to provide neighbor el table              *
!******************************************************************************
!                                                                             *
!     -  PARAMETERS WHICH MUST BE SET TO CONTROL THE DIMENSIONING OF ARRAYS   *
!           ARE AS FOLLOWS:                                                   *
!                                                                             *
!          MNP = MAXIMUM NUMBER OF NODAL POINTS                               *
!          MNE = MAXIMUM NUMBER OF ELEMENTS                                   *
!          MNEI= 1+MAXIMUM NUMBER OF NODES CONNECTED TO ANY ONE NODE IN THE   *
!                  FINITE ELEMENT GRID                                        *
!                                                                             *
!******************************************************************************
!                                                                             *
!    VARIABLE DEFINITIONS:                                                    *
!       NE - NUMBER OF ELEMENTS                                               *
!       NP - NUMBER OF NODES                                                  *
!       NM(MNE,3) - NODE NUMBERS ASSOCIATED WITH EACH ELEMENT                 *
!       NNeigh(MNP) NUMBER OF NEIGHBORS FOR EACH NODE                         *
!       NeiTab(MNP,NEIMAX) 2D ARRAY OF NEIGHBORS FOR EACH NODE                *
!       NeiTabEle(MNP,NEIMAX) 2D ARRAY OF NEIGHBOR ELEMENTS FOR EACH NODE     *
!       NEIMIN - 1+MINIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!       NEIMAX - 1+MAXIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!                                                                             *
!******************************************************************************
!


      SUBROUTINE NEIGHB(NE,NP,NM,NNeigh,NeiTab,NeiTabEle,NEIMIN,NEIMAX,
     &X,Y,dx_avg)
!CALL NEIGHB(nelement,nodes,nm,NNeigh,NeiTab,NeiTabEle,NEIMIN,NEIMAX,X,Y,dx_avg)
!     USE SIZES
      IMPLICIT NONE
      INTEGER :: MNP, MNEI, MNE
      INTEGER :: NP,NE,NEIMIN,NEIMAX,NSCREEN,N,NN,I,J,JJ,K,JLOW
      INTEGER :: NN1,NN2,NN3,NE1,NE2,NE3
      INTEGER :: NeiTab(NP,15), NNeigh(NP), NeiTabEle(NP,15)
      INTEGER :: NM(NE,3)
      REAL :: X(NP),Y(NP),DELX,DELY,DIST,dx_avg(NP)
      REAL :: ANGLELOW,ANGLEMORE,RAD2DEG
      REAL, ALLOCATABLE :: ANGLE(:)
      INTEGER, ALLOCATABLE :: NEITEM(:)
      INTEGER, ALLOCATABLE :: NNEIGHELE(:)
!      WRITE(*,*)"defined arrays variables"
!
        MNEI=15
        MNP=NP
        MNE=NE
      ALLOCATE ( ANGLE(15) )
      ALLOCATE ( NEITEM(NP) )
      ALLOCATE ( NNeighEle(NP) )
!      WRITE(*,*) " allocated 3 arrays"
!
      RAD2DEG=45.0d0/ATAN(1.0d0)
!
!      WRITE(*,*) " defined RAD2DEG "
      DO N=1,NP
       NNeigh(N)=0
         NNeighEle(N)=0
         DO NN=1,MNEI
            NeiTab(N,NN)=0
            NeiTabEle(N,NN)=0
         END DO
      END DO
!      WRITE(*,*) " initialized arrays"


      DO 10 N=1,NE
         NN1 = NM(N,1)
         NN2 = NM(N,2)
         NN3 = NM(N,3)


         NNeighEle(NN1)=NNeighEle(NN1)+1
         NNeighEle(NN2)=NNeighEle(NN2)+1
         NNeighEle(NN3)=NNeighEle(NN3)+1
         NeiTabEle(NN1,NNeighEle(NN1))=N
         NeiTabEle(NN2,NNeighEle(NN2))=N
         NeiTabEle(NN3,NNeighEle(NN3))=N


         DO J=1,NNeigh(NN1)
            IF(NN2.EQ.NeiTab(NN1,J)) GOTO 25
         END DO
         NNeigh(NN1)=NNeigh(NN1)+1
         NNeigh(NN2)=NNeigh(NN2)+1
         IF((NNeigh(NN1).GT.MNEI-1).OR.(NNeigh(NN2).GT.MNEI-1)) GOTO 999
         NeiTab(NN1,NNeigh(NN1))=NN2
         NeiTab(NN2,NNeigh(NN2))=NN1


 25      CONTINUE
         DO J=1,NNeigh(NN1)
            IF(NN3.EQ.NeiTab(NN1,J)) GOTO 35
         END DO
         NNeigh(NN1)=NNeigh(NN1)+1
         NNeigh(NN3)=NNeigh(NN3)+1
         IF((NNeigh(NN1).GT.MNEI-1).OR.(NNeigh(NN3).GT.MNEI-1)) GOTO 999
         NeiTab(NN1,NNeigh(NN1))=NN3
         NeiTab(NN3,NNeigh(NN3))=NN1


 35      CONTINUE
         DO J=1,NNeigh(NN2)
            IF(NN3.EQ.NeiTab(NN2,J)) GOTO 10
         END DO
         NNeigh(NN2)=NNeigh(NN2)+1
         NNeigh(NN3)=NNeigh(NN3)+1
         IF((NNeigh(NN2).GT.MNEI-1).OR.(NNeigh(NN3).GT.MNEI-1)) GOTO 999
         NeiTab(NN2,NNeigh(NN2))=NN3
         NeiTab(NN3,NNeigh(NN3))=NN2


 10   CONTINUE


!      WRITE(*,*) "looped through elements and from element table found neighbors"


!
!     INSERT NODE ITSELF IN PLACE #1 and SORT other NEIGHBORS by
!     increasing cw angle from East
!
      DO I=1,NP
         DO J=1,NNeigh(I)
            NEITEM(J)=NeiTab(I,J)
            DELX=X(NEITEM(J))-X(I)
            DELY=Y(NEITEM(J))-Y(I)
!       write(*,*) I,NNeigh(I),J
!       write(*,*)NEITEM(J)
!       write(*,*)  X(I), Y(I)
!       write(*,*)X(NEITEM(J)),Y(NEITEM(J))
!       write(*,*) DELX, DELY
            DIST=SQRT(DELX*DELX+DELY*DELY)
            IF(DIST.EQ.0.0d0) GOTO 998
            IF(DELY.NE.0.0d0) THEN
               ANGLE(J)=RAD2DEG*ACOS(DELX/DIST)
               IF(DELY.GT.0.0) ANGLE(J)=360.0d0-ANGLE(J)
            ENDIF
            IF(DELY.EQ.0.0d0) THEN
               IF(DELX.GT.0.0d0) ANGLE(J)=0.0d0
               IF(DELX.LT.0.d0) ANGLE(J)=180.0d0
            ENDIF
         END DO
         ANGLEMORE=-1.d0
         DO JJ=1,NNeigh(I)
            ANGLELOW=400.d0
            DO J=1,NNeigh(I)
               IF((ANGLE(J).LT.ANGLELOW).AND.    
     &             (ANGLE(J).GT.ANGLEMORE)) THEN
                  ANGLELOW=ANGLE(J)
                  JLOW=J
               ENDIF
            END DO
            NeiTab(I,JJ+1)=NEITEM(JLOW)
            ANGLEMORE=ANGLELOW
         END DO
         NeiTab(I,1)=I
         NNeigh(I)=NNeigh(I)+1
      ENDDO
!      WRITE(*,*) " looped through nodes and and create neighbor table"
!
!     MATCH EACH SET OF 3 NODES WITH CORRESPONDING ELEMENT AND REORDER
!     ELEMENTS ACCORDINGLY
!
      DO I=1,NP
         DO K=1,NNeighEle(I)
            NEITEM(K)=NeiTabEle(I,K)
            NeiTabEle(I,K)=0
         END DO
         DO J=2,NNeigh(I)
            NN1=NeiTab(I,1)
            NN3=NeiTab(I,J)
            IF(J.NE.NNeigh(I)) NN2=NeiTab(I,J+1)
            IF(J.EQ.NNeigh(I)) NN2=NeiTab(I,2)
            DO K=1,NNeighEle(I)
               IF(NEITEM(K).NE.0) THEN
                  IF(NM(NEITEM(K),1).EQ.NN1) THEN
                     NE1=NM(NEITEM(K),1)
                     NE2=NM(NEITEM(K),2)
                     NE3=NM(NEITEM(K),3)
                  ENDIF
                  IF(NM(NEITEM(K),2).EQ.NN1) THEN
                     NE1=NM(NEITEM(K),2)
                     NE2=NM(NEITEM(K),3)
                     NE3=NM(NEITEM(K),1)
                  ENDIF
                  IF(NM(NEITEM(K),3).EQ.NN1) THEN
                     NE1=NM(NEITEM(K),3)
                     NE2=NM(NEITEM(K),1)
                     NE3=NM(NEITEM(K),2)
                  ENDIF
                  IF((NE2.EQ.NN2).AND.(NE3.EQ.NN3)) THEN
                     NeiTabEle(I,J-1)=NEITEM(K)
                     NEITEM(K)=0
                  ENDIF
               ENDIF
            END DO
         END DO
      END DO


!      WRITE(*,*) "looped through nodes and matched nodes with elements and reorder them "
!
!  DETERMINE THE MAXIMUM AND MINIMUM NUMBER OF NEIGHBORS
!
      NEIMAX = 0
      NEIMIN = 1000
      DO N=1,NP
         IF(NNeigh(N).LT.NEIMIN) NEIMIN=NNeigh(N)
         IF(NNeigh(N).GT.NEIMAX) NEIMAX=NNeigh(N)
      END DO


!      WRITE(*,*)" determined max and min # of neighb"
!*****************************************************************************
! compute the distance from every node
! to each of its neighbors and then
! divide by the total number of neighbors for that node
      dx_avg=0.0
      DO N = 1, NP
          DO J=2,NNEIGH(N)


         dx_avg(N)=dx_avg(N) + sqrt( (X(NeiTab(N,1))-X(NeiTab(N,J)))**2
     &   + (Y(NeiTab(N,1))-Y(NeiTab(N,J)))**2 )


          ENDDO
         dx_avg(N)=dx_avg(N)/NNEIGH(N)


      ENDDO


!      WRITE(*,*)" computed distances"
!*****************************************************************************
!
!  WRITE OUT DIAGNOSTIC OUTPUT
!
!1     OPEN(333,file='fort.333')
!1     DO N=1,NP
!1       WRITE(333,331) (NeiTab(N,J),J=1,NNEIGH(N))
!1       WRITE(333,331) N,(NeiTab(N,J),J=1,NNEIGH(N))
!1       WRITE(333,331) N,(NeiTab(N,J),J=2,NNEIGH(N))
!       WRITE(333,*) dx_avg(N)
!       WRITE(333,*) ' '
!331    FORMAT(15(1X,I7))
!       END DO
!     CLOSE (333)


!      WRITE(*,*)" wrote out a test file"




!  Deallocate local work arrays


      DEALLOCATE ( ANGLE )
      DEALLOCATE ( NEITEM )
      DEALLOCATE ( NNEIGHELE )


!  DONE


      RETURN


 999  CONTINUE
      WRITE(*,99311)
99311 FORMAT(////,1X,'!!!!!!!!!!  WARNING - FATAL ERROR !!!!!!!!!', 
     &     //,1X,'THE DIMENSIONING PARAMETER MNEI IS TOO SMALL',   
     &    /,1X,'THERE IS A PROBLEM WITH THE DYNAMIC MEMORY ALLOCATION',
     &    //,1X,'!!!!!! EXECUTION WILL NOW BE TERMINATED !!!!!!',//)
      STOP


 998  CONTINUE
      WRITE(*,99312) I,NEITEM(J)
99312 FORMAT(////,1X,'!!!!!!!!!!  WARNING - FATAL ERROR !!!!!!!!!',  
     &     //,1X,'NODES ',I7,' AND ',I7,' HAVE THE SAME COORDINATES' 
     &    //,1X,'!!!!!! EXECUTION WILL NOW BE TERMINATED !!!!!!',//)
      STOP
      END




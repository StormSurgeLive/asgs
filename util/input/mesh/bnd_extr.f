C*******************************************************************************
C    Program to set up the boundaries nodes for an ADCIRC grid file given only *
C    the node table and the element table.                                     *
C                                                                              *
C    Note, this makes use of the neighbor table and is much faster than        *
C    determining whether a side falls in two elements.                         *
C                                                                              *
C    Input is an ADCIRC grid file (the user is prompted for the name)          *
C    with no boundary information.                                             *
C                                                                              *
C    Output are two boundary files:                                            *
C       adcirc.bnd - in ADCIRC boundary form, ready to be concatenated onto    *
C                    the end of the ADCIRC grid file.                          *
C       gredit.bnd - in XMGREDIT nodal boundary format.                        *
C								               *
C                                                                              *
C        VERSION 2.1  R. Luettich  2/7/96 - for ADCIRC v28.05 and higher       *
C*******************************************************************************
C
      PROGRAM BND_EXTR

C*******************************************************************************
C PARAMETER STATEMENT - MNP is the maximum number of nodes, MNEI is the maximum*
C                       number of neighbors.                                   *
C                                                                              *
      PARAMETER(MNP=500000,MNE=2*MNP,MNEI=12)
C                                                                              *
C*******************************************************************************

      COMMON/HGRID/ X(MNP),Y(MNP),NM(MNE,3)
      DIMENSION NEITAB(MNP,MNEI),NNEIGH(MNP)

      LOGICAL FOUND
	CHARACTER*60 GRID
	CHARACTER*80 LINEI
	CHARACTER*1 CHARI(80)
      EQUIVALENCE (LINEI,CHARI(1))


  60  FORMAT(A60)
  80  FORMAT(A80)
 180  FORMAT(80A1)
 1010 FORMAT(' File ',A60,/,' WAS NOT FOUND!  Try again',/)
 1011 FORMAT(' File ',A60,/,' WAS FOUND!  Opening & Processing file',/)

C....Enter, locate, open and read the ADCIRC grid file

 31   WRITE(*,*) 'Enter the name of the ADCIRC UNIT 14 (grid) file:'
      WRITE(*,*) '  '
      READ(*,60) GRID
      WRITE(*,*) '  '
      INQUIRE(FILE=GRID,EXIST=FOUND)
      IF(FOUND) GOTO 32
      WRITE(*,1010) GRID
      GOTO 31
 32   WRITE(*,1011) GRID
      OPEN(14,FILE=GRID)

 33   READ(14,80) LINEI                             !SKIP OVER AGRID
      DO I=1,80
         IF(CHARI(I).NE.' ') GOTO 34
         END DO
      GOTO 33
 34   READ(14,*) NE,NP                              !PROCESS NE,NP
      IF(NP.GT.MNP) THEN
         WRITE(*,*) '                                        '
         WRITE(*,*) '************* FATAL ERROR **************'
         WRITE(*,*) '***  THE PARAMETER MNP IN THIS CODE  ***'
         WRITE(*,*) '***  MUST BE SET TO BE LARGER THAN   ***'
         WRITE(*,*) '*** THE NUMBER OF NODES IN THE GRID ****'
         WRITE(*,*) '********* PROGRAM TERMINATED ***********'
         STOP
         ENDIF

      DO I=1,NP
         READ(14,*) nn,X(nn),Y(nn)
         end do

      DO I=1,NE
         READ(14,*) NN,idum,(NM(NN,J),J=1,3)
         end do

      CLOSE(14)

C....Compute the neighbor table

      write(*,*) 'Creating neighbor table'

      CALL NEIGHB(NE,NP,NNEIGH,NEITAB)

c... Figure out boundary nodes

      write(*,*) 'Determining boundary nodes'

      CALL BOUND(NEITAB,NNEIGH,NP,LINEI)

      STOP
      END


C******************************************************************************
C                                                                             *
C      Subroutine to generate a neighbor table from a connectivity table.     *
c                                                                             *
c      NOTE:the node itself is listed as neighbor #1                          *
c      NOTE:all other neighbors are sorted and placed in cw order from east   *
c                                                                             *
c                       R.L.       6/22/95                                    *
C******************************************************************************
C                                                                             *
C     -  PARAMETERS WHICH MUST BE SET TO CONTROL THE DIMENSIONING OF ARRAYS   *
C           ARE AS FOLLOWS:                                                   *
C                                                                             *
C          MNP = MAXIMUM NUMBER OF NODAL POINTS                               *
C          MNE = MAXIMUM NUMBER OF ELEMENTS                                   *
C          MNEI= 1+MAXIMUM NUMBER OF NODES CONNECTED TO ANY ONE NODE IN THE   *
C                   FINITE ELEMENT GRID                                       *
C                                                                             *
C******************************************************************************
C                                                                             *
C    VARIABLE DEFINITIONS:                                                    *
C       NE - NUMBER OF ELEMENTS                                               *
C       NP - NUMBER OF NODES                                                  *
C       NM(MNE,3) - NODE NUMBERS ASSOCIATED WITH EACH ELEMENT                 *
C       NNEIGH(MNP) NUMBER OF NEIGHBORS FOR EACH NODE                         *
C       NEIGH(MNP,NEIMAX) 2D ARRAY OF NEIGHBORS FOR EACH NODE                 *
C       NEIMIN - 1+MINIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
C       NEIMAX - 1+MAXIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
C                                                                             *
C******************************************************************************
C
      SUBROUTINE NEIGHB(NE,NP,NNEIGH,NEIGH)

C*******************************************************************************
C PARAMETER STATEMENT - MNP is the maximum number of nodes, MNEI is the maximum*
C                       number of neighbors.                                   *
C                                                                              *
      PARAMETER(MNP=500000,MNE=2*MNP,MNEI=12)
C                                                                              *
C*******************************************************************************

      COMMON/HGRID/ X(MNP),Y(MNP),NM(MNE,3)
      DIMENSION NEIGH(MNP,MNEI),NNEIGH(MNP),NEITEM(MNEI),ANGLE(MNEI)
      INTEGER EN1,EN2,EN3

      RAD2DEG=45./ATAN(1.)

      DO 5 N=1,NP
         NNEIGH(N) = 0
         DO 5 NN=1,MNEI
            NEIGH(N,NN) = 0
   5        CONTINUE

      DO 10 N=1,NE
         EN1 = NM(N,1)
         EN2 = NM(N,2)
         EN3 = NM(N,3)
         DO 20 J=1,NNEIGH(EN1)
  20        IF(EN2.EQ.NEIGH(EN1,J)) GOTO 25
         NNEIGH(EN1)=NNEIGH(EN1)+1
         NNEIGH(EN2)=NNEIGH(EN2)+1
         IF((NNEIGH(EN1).GT.MNEI-1).OR.(NNEIGH(EN2).GT.MNEI-1)) GOTO 999
         NEIGH(EN1,NNEIGH(EN1))=EN2
         NEIGH(EN2,NNEIGH(EN2))=EN1
  25     DO 30 J=1,NNEIGH(EN1)
  30        IF(EN3.EQ.NEIGH(EN1,J)) GOTO 35
         NNEIGH(EN1)=NNEIGH(EN1)+1
         NNEIGH(EN3)=NNEIGH(EN3)+1
         IF((NNEIGH(EN1).GT.MNEI-1).OR.(NNEIGH(EN3).GT.MNEI-1)) GOTO 999
         NEIGH(EN1,NNEIGH(EN1))=EN3
         NEIGH(EN3,NNEIGH(EN3))=EN1
  35     DO 50 J=1,NNEIGH(EN2)
  50        IF(EN3.EQ.NEIGH(EN2,J)) GOTO 10
         NNEIGH(EN2)=NNEIGH(EN2)+1
         NNEIGH(EN3)=NNEIGH(EN3)+1
         IF((NNEIGH(EN2).GT.MNEI-1).OR.(NNEIGH(EN3).GT.MNEI-1)) GOTO 999
         NEIGH(EN2,NNEIGH(EN2))=EN3
         NEIGH(EN3,NNEIGH(EN3))=EN2
  10     CONTINUE

C....INSERT NODE ITSELF IN PLACE #1 and SORT other NEIGHBORS by increasing cw angle from East

      DO I=1,NP
         DO J=1,NNEIGH(I)
            NEITEM(J)=NEIGH(I,J)
            DELX=X(NEITEM(J))-X(I)
            DELY=Y(NEITEM(J))-Y(I)
            DIST=SQRT(DELX*DELX+DELY*DELY)
            IF(DIST.EQ.0) GOTO 998
            ANGLE(J)=RAD2DEG*ACOS(DELX/DIST)
            IF(DELY.GT.0) ANGLE(J)=360.-ANGLE(J)
            END DO
         ANGLEMORE=-1.
         DO JJ=1,NNEIGH(I)
            ANGLELOW=400.
            DO J=1,NNEIGH(I)
               IF((ANGLE(J).LT.ANGLELOW).AND.(ANGLE(J).GT.ANGLEMORE))
     &                                                              THEN
                  ANGLELOW=ANGLE(J)
                  JLOW=J
                  ENDIF
               END DO
            NEIGH(I,JJ+1)=NEITEM(JLOW)
            ANGLEMORE=ANGLELOW
            END DO
         NEIGH(I,1)=I
         NNEIGH(I)=NNEIGH(I)+1
         END DO

C....DETERMINE THE MAXIMUM AND MINIMUM NUMBER OF NEIGHBORS

      NEIMAX = 0
      NEIMIN = 1000
      DO 60 N=1,NP
         IF(NNEIGH(N).LT.NEIMIN) NEIMIN=NNEIGH(N)
         IF(NNEIGH(N).GT.NEIMAX) NEIMAX=NNEIGH(N)
  60     CONTINUE

      RETURN

C....TERMINATE PROGRAM IF MAXIMUM NUMBER OF NEIGHBORS SET TOO SMALL

 999  CONTINUE
      WRITE(6,99311)
      WRITE(16,99311)
99311 FORMAT(////,1X,'!!!!!!!!!!  WARNING - FATAL ERROR !!!!!!!!!',
     &      //,1X,'THE DIMENSIONING PARAMETER MNEI IS TOO SMALL'
     &     /,1X,'USER MUST RE-DIMENSION PROGRAM',
     &     //,1X,'!!!!!! EXECUTION WILL NOW BE TERMINATED !!!!!!',//)
      STOP

 998  CONTINUE
      WRITE(6,99312) I,NEITEM(J)
      WRITE(16,99312) I,NEITEM(J)
99312 FORMAT(////,1X,'!!!!!!!!!!  WARNING - FATAL ERROR !!!!!!!!!',
     &      //,1X,'NODES ',I7,' AND ',I7,' HAVE THE SAME COORDINATES'
     &     //,1X,'!!!!!! EXECUTION WILL NOW BE TERMINATED !!!!!!',//)
      STOP
      END


C******************************************************************************
C                                                                             *
C    Subroutine to determine and write boundary nodes.                        *
c                                                                             *
c                                                                             *
c                       R.L.       2/5/96                                     *
C******************************************************************************

      subroutine bound(NEITAB,NNEIGH,NP,LINEI)

C*******************************************************************************
C PARAMETER STATEMENT - MNP is the maximum number of nodes, MNEI is the maximum*
C                       number of neighbors.                                   *
C                                                                              *
      PARAMETER(MNP=500000,MNE=2*MNP,MNEI=12)
C                                                                              *
C*******************************************************************************

      CHARACTER*80 LINEI
      INTEGER NEITAB(MNP,MNEI),NNEIGH(MNP),TBNODE(MNP,2)
      INTEGER CONBNODE(MNP),NBN(MNP),ibbeg(mnp),ibend(mnp),ibtype(mnp)
      INTEGER extbns(mnp),bsegbn(mnp),bsegen(mnp)

      NSEG=0
      DO I=1,NP
        DO J=2,NNEIGH(I)
          NEINOD1=NEITAB(I,J)                            !pick a neighbor
          DO K=2,NNEIGH(NEINOD1)
            IF(NEITAB(NEINOD1,K).EQ.I) NEINUM=K          !find yourself in that
            END DO                                       !node's neighbor table
          NEINUMP1=NEINUM+1
          IF(NEINUM.EQ.NNEIGH(NEINOD1)) NEINUMP1=2
          NEINOD2=NEITAB(NEINOD1,NEINUMP1)               !find the next cw neighbor
          DO K=2,NNEIGH(NEINOD2)
            IF(NEITAB(NEINOD2,K).EQ.NEINOD1) NEINUM=K
            END DO
          NEINUMP1=NEINUM+1
          IF(NEINUM.EQ.NNEIGH(NEINOD2)) NEINUMP1=2
          NEINOD3=NEITAB(NEINOD2,NEINUMP1)
          IF(NEINOD3.NE.I) THEN                          !first leg was a boundary seg
            NSEG=NSEG+1
            TBNODE(NSEG,2)=I
            TBNODE(NSEG,1)=NEINOD1
            ENDIF
          END DO
        END DO

      WRITE(*,*) NSEG,' boundary pieces were found'

C....Sort into connected boundary nodes

      n=0
      npbn=0
      nbou=0
   9  continue
      do i=1,nseg
        if(tbnode(i,1).ne.0) goto 10
        end do
      goto 99
  10  j=i
  11  n=n+1
      conbnode(n)=tbnode(j,2)
      if(conbnode(n).eq.tbnode(i,1)) then
        tbnode(i,1)=0
        nbou=nbou+1
        nbn(nbou)=n-npbn
        npbn=n
        goto 9
        endif
      do j=1,nseg
        if(tbnode(j,1).eq.conbnode(n)) then
          tbnode(j,1)=0
          goto 11
          endif
        end do

C....Write GREDIT boundary file

   99 open(10,file='gredit.bnd')                         
      write(10,'(a80)') LINEI
      write(10,*) nbou
      n=0
      do i=1,nbou
        write(10,*) nbn(i),1
        do j=1,nbn(i)
          n=n+1
          write(10,*) conbnode(n)
          end do
        end do
      close(10)

C....Write ADCIRC boundary file

      open(11,file='adcirc.bnd')

      write(*,*) ' '
      write(*,*) '************** External Boundary Setup **************'
      write(*,*) ' '
      write(*,*) '   It is assumed that the external boundary consists '
      write(*,*) 'of one or more "segments".  All nodes in a segment   '
      write(*,*) 'are contigous and have the same boundary type.       '
      write(*,*) 'Unless the external boundary consists of a single    '
      write(*,*) 'segment, the beginning and end nodes of each '
      write(*,*) 'segment belong to two segments.'
      write(*,*) ' '
      write(*,*) '   All segments must be specified keeping the outside'
      write(*,*) 'of the domain to the right when progressing from ',
     &           'beginning to end.'
      write(*,*) ' '
      write(*,*) '   The following external boundary types are ',
     &           'currently recognized:'
      write(*,*) ' '
      write(*,*) 'Open - elevation specified'
      write(*,*) 'Land - zero normal flux as an essential condition'
      write(*,*) 'Land - zero normal flux as a natural condition'
      write(*,*) 'Land - zero normal and tangential fluxes as ',
     &           'essential conditions'
      write(*,*) 'Land - specified normal flux as an essential ',
     &           'condition'
      write(*,*) 'Land - specified normal and zero tangential fluxes ',
     &           'as essential conditions'
      write(*,*) 'Land - specified normal flux as a natural condition'
      write(*,*) ' '

C......Prompt for information about open boundaries

      write(*,*) '-----------------------------------------------------'
      write(*,*) ' '
      write(*,*) 'Specify the open boundary segments first.'
      write(*,*) ' '
      write(*,*) '-----------------------------------------------------'
      write(*,*) ' '
      write(*,*) 'Enter the number of open boundary segments ',
     &           '(elevation specified)'
      write(*,*) ' '
      read(*,*) nob
      write(*,*) ' '
      write(*,*) 'Enter the beginning and ending node on each o.b.',
     &           'segment.'
      do i=1,nob
         write(*,*) ' '
         write(*,*) 'open boundary # ',i
         write(*,*) ' '
         read(*,*) ibbeg(i),ibend(i)
         end do

c......Figure out which of the previously identified boundary groups
c.......is the external boundary by looking for the first node in the
c.......open boundary.

      n=0
      do i=1,nbou
        nref=n
        do j=1,nbn(i)
          n=n+1 
          if(ibbeg(1).eq.conbnode(n)) then
            nbegnbnm1=nref
            nbegm1=n-1
            ib=i
            endif
          end do
        end do

c......Load the external boundary into its own array

      if(nbou.ne.0) then
	  n=nbegm1 
        ncheck=nbegnbnm1+nbn(ib)
        do j=1,nbn(ib)
          n=n+1
          if(n.gt.ncheck) n=n-nbn(ib)	!wrap back to beginning of conbnode array
          extbns(j)=conbnode(n)
          conbnode(n)=0
          end do
        extbns(nbn(ib)+1)=extbns(1)
	  endif

c......Find the beginning and ending spot of each open boundary segment

      do k=1,nob
        if(ibbeg(k).eq.extbns(1)) bsegbn(k)=1
        do j=2,nbn(ib)
          if(ibbeg(k).eq.extbns(j)) bsegbn(k)=j
          if(ibend(k).eq.extbns(j)) bsegen(k)=j
          end do
        if(ibend(k).eq.extbns(nbn(ib)+1)) bsegen(k)=nbn(ib)+1
        end do

c......Compute the total number of open boundary nodes

      itotobn=0
      do k=1,nob
         itotobn=itotobn+(bsegen(k)-bsegbn(k))+1
         end do

c......Write the open boundary information

      write(11,*) nob
      write(11,*) itotobn
      do k=1,nob
         nobn=bsegen(k)-bsegbn(k)+1
         write(11,*) nobn
         write(11,*) extbns(bsegbn(k))
         do j=bsegbn(k)+1,bsegen(k)-1
            write(11,*) extbns(j)
            extbns(j)=0
            end do
         write(11,*) extbns(bsegen(k))
         end do

C......Prompt for information about land boundaries .

      write(*,*) ' '
      write(*,*) '-----------------------------------------------------'
      write(*,*) ' '
      write(*,*) 'Specify the land boundary segments next.'
      write(*,*) ' '
      write(*,*) '   If the external boundary is entirely closed by one'
     &           ,' segment, (e.g., a lake),'
      write(*,*) 'LEAVE IT UNCLOSED!!  It will be closed automatically.'
      write(*,*) ' '
      write(*,*) '   Do not use a no normal flux boundary as the first',
     &           ' boundary segment'
      write(*,*) 'if a specified flux boundary segment preceeds it ',
     &           '(i.e., lies clockwise)'
      write(*,*) ' '
      write(*,*) '   The following external land boundary types are ',
     &           'recognized:'
      write(*,*) ' '
      write(*,*) ' 0 - no normal flux essential boundary condition'
      write(*,*) ' 2 - specified normal flux essential boundary ',
     &           'condition'
      write(*,*) '10 - no normal or tangential flux boundary condition'
      write(*,*) '12 - specified normal flux and no tangential flux ',
     &           'boundary condition'
      write(*,*) '20 - no normal flux natural boundary condition'
      write(*,*) '22 - specified normal flux natural boundary condition'
      write(*,*) ' '
      write(*,*) '-----------------------------------------------------'
      write(*,*) ' '
      write(*,*) 'Enter the number of land boundary segments ',
     &           '(all types)'
      write(*,*) ' '
      read(*,*) nexlb
      write(*,*) ' '
      write(*,*) 'Enter the beginning node, ending node and boundary ',
     &           'type of each l.b. segment.'
      do i=1,nexlb
        write(*,*) ' '
        write(*,*) 'land boundary # ',i
        write(*,*) ' '
        read(*,*) ibbeg(i),ibend(i),ibtype(i)
        end do

c......If there were no open boundaries, figure out which of the previously 
c.......identified boundary groups is the external boundary by looking for 
c.......the first node in the land boundary.

      if(nob.eq.0) then
        n=0
        do i=1,nbou
          nref=n
          do j=1,nbn(i)
            n=n+1 
            if(ibbeg(1).eq.conbnode(n)) then
              nbegnbnm1=nref
              nbegm1=n-1
              ib=i
              endif
            end do
         end do

c......Load the external boundary into its own array

        n=nbegm1 
        ncheck=nbegnbnm1+nbn(ib)
        do j=1,nbn(ib)
          n=n+1
          if(n.gt.ncheck) n=n-nbn(ib)
          extbns(j)=conbnode(n)
          conbnode(n)=0
          end do
        extbns(nbn(ib)+1)=extbns(1)
        endif

c......Find the beginning and ending spot of each land boundary segment
 
      do k=1,nexlb
        if(ibbeg(k).eq.extbns(1)) bsegbn(k)=1
        do j=2,nbn(ib)
          if(ibbeg(k).eq.extbns(j)) bsegbn(k)=j
          if(ibend(k).eq.extbns(j)) bsegen(k)=j
          end do
        if(ibend(k).eq.extbns(nbn(ib)+1)) bsegen(k)=nbn(ib)+1
        end do

c......Compute the total number of land boundary segments 
c.......including islands

      nlb=nexlb+nbou-1

c......Compute the total number of land boundary nodes

      itotlbn=0
      do k=1,nexlb
         itotlbn=itotlbn+(bsegen(k)-bsegbn(k))+1
         end do
      if((nob.eq.0).and.(nexlb.eq.1)) itotlbn=itotlbn+1
      do j=1,nbou
        if(j.ne.ib) itotlbn=itotlbn+nbn(j)+1
        end do 

c.......Write the land boundary information

      write(11,*) nlb
      write(11,*) itotlbn
      do k=1,nexlb
         nlbn=bsegen(k)-bsegbn(k)+1
         if((nob.eq.0).and.(nexlb.eq.1)) nlbn=nlbn+1
         write(11,*) nlbn,ibtype(k)
         write(11,*) extbns(bsegbn(k))
         do j=bsegbn(k)+1,bsegen(k)-1
            write(11,*) extbns(j)
            extbns(j)=0
            end do
         write(11,*) extbns(bsegen(k))
         end do
      if((nob.eq.0).and.(nexlb.eq.1)) write(11,*) extbns(bsegen(k)+1)

      write(*,*) ' '
      write(*,*) '************** Internal Boundary Setup **************'
      write(*,*) ' '
      if(nbou.eq.1) then
        write(*,*) 'There are no internal boundaryies'
        write(*,*) ' '
        else
        write(*,*) '   It is assumed that all internal (island) '
        write(*,*) 'boundaries have the same boundary type.'
        write(*,*) ' '
        write(*,*) 'The following internal boundary types are ',
     &             'currently recognized: '
        write(*,*) ' '
        write(*,*) ' 1 - no normal flux essential boundary condition'
	write(*,*) '11 - no normal or tangential flux boundary ',
     &             'condition'
        write(*,*) '21 - no normal flux natural boundary condition'
        write(*,*) ' '
        write(*,*) 'Enter the internal boundary type now. '
        write(*,*) ' '
        read(*,*) intbtype
     	
        n=0
        do i=1,nbou
          if(i.ne.ib) then
            write(11,*) nbn(i)+1,intbtype
            do j=1,nbn(i)
              n=n+1
	        if(j.eq.1) nrep=n
              write(11,*) conbnode(n)
              end do
            write(11,*) conbnode(nrep)
            else
            n=n+nbn(i)
            endif
          end do
	  endif

      close(11)
   
      return
      end

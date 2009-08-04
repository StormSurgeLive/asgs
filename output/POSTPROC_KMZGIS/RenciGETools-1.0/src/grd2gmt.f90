! GRD2GMT - Generate GMTgrid files from ADCIRC fort.14 file
! Brian Blanton, RENCI, 30 June 2007
!
! This program outputs .gmt.{xy,xyz,tri,bnd.xy} files by scanning
! an ADCIRC fort.14 file.  The F90 compiler used must support 
! command-line arguments thru the getarg subroutine and the iargc()
! function.  
!
! To compile on a Mac with g95 : 
!    g95 -lSystemStubs grd2gmt.f90 -O4 -o grd2gmt.x 

!
! Usage: grd2gmt.x <grdfilename> <gridname>
! Example: ./grd2gmt.x lawest_2007_r10.grd lawest_2007_r10
! Example: ./grd2gmt.x fort.14 lawest_2007_r10
!

PROGRAM GRD2GMT
IMPLICIT NONE

character*132 grdfile,grdname,header
INTEGER I,II,IB1,IB2,IBO1,IBO2,J,JJ,BTYPE
INTEGER NE,NP,NOPEN,NOPENNODES,NOPENTOTALNODES
INTEGER NLAND,NLANDNODES,NLANDTOTALNODES
REAL*8 F1,F2,F3,Z
REAL*8, ALLOCATABLE :: X(:),Y(:)
INTEGER E1,E2,E3
INTEGER, EXTERNAL :: iargc

if (iargc() .ne. 2)then
   WRITE(*,*)'Usage : grd2gmt <grdfile> <grdname>'
   stop
endif
call getarg(1,grdfile)
call getarg(2,grdname)

open (10, file=trim(grdfile), status='old')
open (20, file=trim(grdname)//'.gmt.xy', status='unknown')
open (21, file=trim(grdname)//'.gmt.tri', status='unknown')
open (22, file=trim(grdname)//'.gmt.bnd.xy', status='unknown')
open (23, file=trim(grdname)//'.gmt.xyz', status='unknown')

read(10,'(A)')header
READ(10,*)NE,NP
WRITE(*,*)NE,NP

ALLOCATE(X(NP),Y(NP))

! SCAN/PRINT GRID NODES
WRITE(*,*)'SCANNING GRID NODES'
DO I=1,NP
   READ(10,*)II,X(II),Y(II),Z
   WRITE(20,*)X(II),Y(II)
   WRITE(23,*)X(II),Y(II),Z
ENDDO

! SCAN/PRINT GRID ELEMENTS
WRITE(*,*)'SCANNING GRID ELEMENTS'
DO I=1,NE
   READ(10,*)II,JJ,E1,E2,E3
   WRITE(21,*)E1-1,E2-1,E3-1
ENDDO

! SCAN OPEN BOUNDARY SEGMENTS
WRITE(*,*)'SCANNING GRID OPEN BOUNDARIES'
READ(10,*)NOPEN
READ(10,*)NOPENTOTALNODES
DO I=1,NOPEN
   READ(10,*)NOPENNODES
   READ(10,*)IB1
   DO J=2,NOPENNODES
       READ(10,*)IB2
       WRITE(22,100)I,IB1-1,IB2-1
       WRITE(22,*)X(IB1),Y(IB1)
       WRITE(22,*)X(IB2),Y(IB2)
	   IB1=IB2
   ENDDO
ENDDO

!SCAN LAND BOUNDARY SEGMENTS
WRITE(*,*)'SCANNING GRID LAND BOUNDARIES'
READ(10,*)NLAND
READ(10,*)NLANDTOTALNODES
DO I=1,NLAND

   READ(10,*)NLANDNODES,BTYPE

   SELECT CASE (BTYPE)
   CASE (  0 , 1 , 2 , 10 , 11 , 12 , 20 , 21 , 22 , 30 , 52 )
      READ(10,*)IB1
      DO J=2,NLANDNODES
         READ(10,*)IB2
         WRITE(22,101)I,BTYPE,IB1-1,IB2-1
         WRITE(22,*)X(IB1),Y(IB1)
         WRITE(22,*)X(IB2),Y(IB2)
         IB1=IB2
      ENDDO

   CASE ( 3 , 13 , 23 )
      READ(10,*)IB1,F1,F2
      DO J=2,NLANDNODES
         READ(10,*)IB2,F1,F2
         WRITE(22,101)I,BTYPE,IB1-1,IB2-1
         WRITE(22,*)X(IB1),Y(IB1)
         WRITE(22,*)X(IB2),Y(IB2)
         IB1=IB2
      ENDDO
   
   CASE ( 4 , 24 )
      READ(10,*)IB1,IBO1,F1,F2,F3
      DO J=2,NLANDNODES
         READ(10,*)IB2,IBO2,F1,F2,F3
         WRITE(22,102)I,BTYPE,IB1-1,IB2-1
         WRITE(22,*)X(IB1),Y(IB1)
         WRITE(22,*)X(IB2),Y(IB2)
         WRITE(22,103)I,BTYPE,IBO1-1,IBO2-1
         WRITE(22,*)X(IBO1),Y(IBO1)
         WRITE(22,*)X(IBO2),Y(IBO2)
         IB1=IB2
         IBO1=IBO2
      ENDDO

   END SELECT

ENDDO

100 FORMAT("> OPEN BND ",I8," : Edge ",I8," -> ",I8)
101 FORMAT("> LAND BND ",I8," : Type ",I4," : Edge ",I8," -> ",I8)
102 FORMAT("> WEIR BND ",I8," : Type ",I4," : Edge ",I8," -> ",I8)
103 FORMAT("> WEIR BND MATCH ",I8," : Type ",I4," : Edge ",I8," -> ",I8)

CLOSE(14)
CLOSE(20)
CLOSE(21)
CLOSE(22)
CLOSE(23)

END PROGRAM GRD2GMT



      PROGRAM nodesoutabox
!Simple utility to select all nodes not within a box from an ADCIRC
!fort.14 mesh file.  The nodes not within the bounding box are written 
!to the output file, with a 0 next to them for insertion into a fort.13
!nodal attribute file.  x1,y1 are the lower left corner of the box, 
!x2,y2 are the upper-right corner of the box.  filein is the mesh.  
!
!Taylor Asher taylorgasher@gmail.com
!July 9, 2012


      IMPLICIT NONE

      CHARACTER(100) :: dumc
      CHARACTER(100) :: filein
      CHARACTER(100) :: fileout
      CHARACTER(30)  :: x1str
      CHARACTER(30)  :: y1str
      CHARACTER(30)  :: x2str
      CHARACTER(30)  :: y2str
      INTEGER        :: ctr
      INTEGER        :: dumi
      INTEGER        :: n
      INTEGER        :: nn
      REAL(8)        :: dumr
      REAL(8)        :: x
      REAL(8)        :: x1
      REAL(8)        :: x2
      REAL(8)        :: y
      REAL(8)        :: y1
      REAL(8)        :: y2



      CALL GETARG(1,filein)
      CALL GETARG(2,fileout)
      CALL GETARG(3,x1str)
      CALL GETARG(4,y1str)
      CALL GETARG(5,x2str)
      CALL GETARG(6,y2str)
      READ(x1str,*)x1
      READ(y1str,*)y1
      READ(x2str,*)x2
      READ(y2str,*)y2

      OPEN(14,FILE=TRIM(filein))
      READ(14,*)dumc
      READ(14,*)dumi,nn
      OPEN(15,FILE=TRIM(fileout))
      DO ctr=1,nn
        READ(14,*)n,x,y,dumr
        IF(NOT((x.GE.x1).AND.(x.LE.x2).AND.(y.GE.y1).AND.(y.LE.y2)))THEN
          WRITE(15,*)n,0
        ENDIF
      ENDDO
      END PROGRAM

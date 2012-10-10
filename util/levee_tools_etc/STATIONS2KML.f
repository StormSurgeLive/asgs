C____________________________________________________________________
C      PROGRAM STATIONS2KML
C--------------------------------------------------------------------      
C this program takes as input an ADCIRC grid and file containing  
C x,y,!description station locations (I simply copy this out of fort.15.
C anything after x,y preceeded by a "!" will be used as a description in the kml.)
C As output it produces a kml file that provieds information about where
C each station falls in the grid (element number, nodes of that element, 
C maximum elevation of the nodes, and draws a small snapshot of the grid around
C each location. the snapshots of the grid can also be saved in fort.14
C format by uncommenting some of the lines below
C
C---------------------------------------------------------------------
C
C Copyright(C) 2010,2012  Nathan Dill
C
C This program is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C
C This program is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this program.  If not, see <http://www.gnu.org/licenses/>.
C---------------------------------------------------------------------
      PROGRAM STATIONS2KML

      USE GRIDSTUFF
      
      IMPLICIT NONE
      
      INTEGER I,J,K,L,II,JJ,NS,INELE,FOUND
c      REAL*8, ALLOCATABLE :: X1TMP(:), Y1TMP(:), X2TMP(:), Y2TMP(:)
   
     
      REAL*8 FACTOR,XLIM(2),YLIM(2),SCL,pad,XTMP,YTMP,ZTMP,MAXZ,MINZ
      REAL*8 INTERVAL,XC(2),YC(2),CONTOUR
      
      CHARACTER*80 FILENAME,TMPSTR,FILENAME2,DESC,clipname

C--------------------------------------------------------------------
      
      WRITE(*,*)'GRID FILE?'
      READ(*,*)FILENAME
      
      WRITE(*,*)'STATION X,Y FILE?'
      READ(*,*)FILENAME2
      
      SCL=.8d0
C READ IN THE STATIONS
      OPEN(12,FILE=FILENAME2)

      I=0
      DO WHILE(.TRUE.)
       READ(12,*,END=7)
       I=I+1
      END DO
      
 7    NS=I
      CLOSE(12)
      OPEN(12,FILE=FILENAME2)
      
      WRITE(*,'(A,I4,A,A)')' THERE ARE ',NS,
     & ' STATIONS IN ',TRIM(FILENAME2)
      

      CALL READ14(FILENAME)
      
      
      OPEN(11,FILE='stations.kml')
      
      WRITE(11,10)
      WRITE(11,20)
      WRITE(11,30)
      WRITE(TMPSTR,'(A,A)')'Stations in ',TRIM(AGRID)
      WRITE(*,*)TMPSTR
      WRITE(11,40)TMPSTR

C     STYLE     
      WRITE(11,300)'style1'
C      WRITE(11,302)
C      WRITE(11,304)
      WRITE(11,307)
      WRITE(11,314)SCL
      WRITE(11,311)
      WRITE(11,312)'Dot.png'
      WRITE(11,313)
      WRITE(11,304)'ff0000ff'
      WRITE(11,308)
      WRITE(11,309)
      WRITE(11,304)'ff000000'
      WRITE(11,310)
C      WRITE(11,303)
      WRITE(11,301)
C     END  STYLE

      
C     STYLE         FOR CONTOURS
      WRITE(11,300)'style2'
      WRITE(11,309)
      WRITE(11,304)'ffff0000'
      WRITE(11,310)
      WRITE(11,301)
C     END  STYLE
C     STYLE         FOR CONTOURS
      WRITE(11,300)'style3'
      WRITE(11,309)
      WRITE(11,304)'ffff7f00'
      WRITE(11,310)
      WRITE(11,301)
C     END  STYLE
C     STYLE         FOR CONTOURS
      WRITE(11,300)'style4'
      WRITE(11,309)
      WRITE(11,304)'ff9fff00'
      WRITE(11,310)
      WRITE(11,301)
C     END  STYLEC     STYLE         FOR CONTOURS
      WRITE(11,300)'style5'
      WRITE(11,309)
      WRITE(11,304)'ff00ff00'
      WRITE(11,310)
      WRITE(11,301)
C     END  STYLE
C     STYLE         FOR CONTOURS
      WRITE(11,300)'style6'
      WRITE(11,309)
      WRITE(11,304)'ff00ff7f'
      WRITE(11,310)
      WRITE(11,301)
C     END  STYLE
C     STYLE         FOR CONTOURS
      WRITE(11,300)'style7'
      WRITE(11,309)
      WRITE(11,304)'ff007fff'
      WRITE(11,310)
      WRITE(11,301)
C     END  STYLEC     STYLE         FOR CONTOURS
      WRITE(11,300)'style8'
      WRITE(11,309)
      WRITE(11,304)'ff0000ff'
      WRITE(11,310)
      WRITE(11,301)
C     END  STYLE


      
C BIG LOOP OVER STATIONS     
      DO I=1,NS
        READ(12,'(A80)')TMPSTR
        READ(TMPSTR,*)XTMP,YTMP
        K=INDEX(TMPSTR,'!')
        DESC=TMPSTR(K:80)
        write(*,*)xtmp,ytmp
        ZTMP=0.D0
        
        
c make folder for this station
         WRITE(11,319)
         WRITE(TMPSTR,'(A,I4)')'Station',I
         WRITE(11,40)TRIM(TMPSTR)        
        
        
C WRITE THE POINT PLACEMARK
        WRITE(11,50)    
        WRITE(TMPSTR,'(A,I4)')'STATION',I
        WRITE(11,40)TRIM(TMPSTR)
        WRITE(11,306)'style1'
        WRITE(11,315)
        WRITE(11,110)
        WRITE(11,200)XTMP,YTMP,ZTMP
        WRITE(11,111)
        WRITE(11,316)
C HOLD OFF ON CLOSING THIS PLACEMARK UNTIL WE WRITE THE DESCRIPTION        WRITE(11,51)          
      
        
C GET THE CLIPPED GRID
          PAD=1.D0/120.D0
          XLIM(1)=XTMP-PAD
          XLIM(2)=XTMP+PAD
          YLIM(1)=YTMP-PAD
          YLIM(2)=YTMP+PAD
                
          WRITE(*,*)
          write(*,*)'CLIPPING GRID...'      
          CALL  CLIPGRID(XLIM,YLIM)
          
C---------------- WRITES OUT THE CLIPPED GRID -----------------------  
c  uncomment this block to write out the clipped grids in fort.14 format
c--------------------------------------------------------------------
c          write(clipname,'(a,i0,a)')'clip',i,'.14'
c          OPEN(15,FILE=clipname)
c          WRITE(*,*)'WRITING CLIPPED GRID'
c          WRITE(15,*)'CLIPEDGRID'
c          WRITE(15,*)CLIP_NE,CLIP_NN
c          DO Ii=1,CLIP_NN
c             WRITE(15,990)CLIP_NID(Ii), CLPX(Ii),CLPY(Ii),CLPZ(Ii)
c          END DO
c          K=3
c          DO Ii=1,CLIP_NE
c             WRITE(15,995)Ii,K,(CLIP_NOC(J,Ii),J=1,3)
c          END DO
c          CLOSE(15)
c 990      FORMAT(I12,3E17.8)     
c 995      FORMAT(I12,I4,3I12)      
C------------------------------------------------------------------------                   
          
C FIND THE ELEMENT IN WHICH IT RESIDES
          INELE=0
          DO J=1,CLIP_NE
c                WRITE(*,*)'LOOKING IN ELEMENT',J,CLIP_NE
                CALL LOCAT_CHK(J,XTMP,YTMP,CLIP_NOC,X,Y,K)
                IF (K.EQ.1) THEN
                INELE=CLIP_EID(J)
                MAXZ=99D99
                DO K=1,3
                   MAXZ=MIN(MAXZ,Z(CLIP_NOC(K,J)))
                END DO                   
                GOTO 919
                END IF
          END DO  
          
 919      CONTINUE
 
C NOW WRITE DESCRIPTION

         WRITE(11,321)
         WRITE(11,500)'Description: ',TRIM(DESC)
         WRITE(11,501)'Longitude: ',XTMP
         WRITE(11,501)'Latitude:  ',YTMP
         
         IF (INELE.EQ.0) THEN
            WRITE(11,'("<p><b>NOT FOUND IN GRID</b></p>")')
         ELSE
            WRITE(11,502)'Found in Element ',INELE
            WRITE(11,504)'Nodes: ',(NOC(K,INELE),K=1,3)
            MAXZ=-3.28*MAXZ
            WRITE(11,503)'Max Nodal Elev: ',MAXZ,' Feet'
         END IF
         WRITE(11,322) 
C CLOSE THE POINT PLACEMARK NOW
        WRITE(11,51)              
          
        
C  WRITE THE PLACEMARK WITH LineString 
         WRITE(11,319)
         WRITE(TMPSTR,'(A,I4)')'Grid',I
         WRITE(11,40)TRIM(TMPSTR) 

         DO J=1,CLIP_NE
           WRITE(11,50)
           WRITE(11,306)'style1'
           WRITE(11,317)
           WRITE(11,110)
         
          DO K=1,3
            WRITE(11,200)X(CLIP_NOC(K,J)),Y(CLIP_NOC(K,J)),
     &       Z(CLIP_NOC(K,J))
          END DO
            WRITE(11,200)X(CLIP_NOC(1,J)),Y(CLIP_NOC(1,J)),
     &       Z(CLIP_NOC(1,J))

          
         
            WRITE(11,111)
            WRITE(11,318)
            WRITE(11,51)
          END DO
          WRITE(11,320)  ! close folder

C WRITE THE PLACEMARK WITH THE CONTOURS
         WRITE(11,319)
         WRITE(TMPSTR,'(A,I4)')'CONTOURS',I
         WRITE(11,40)TRIM(TMPSTR) 
               
         
c         MINZ=99.D99
c         MAXZ=-99.D99
c         DO J=1,CLIP_NE
c           DO K=1,3 
c           MINZ=MIN( MINZ, Z(CLIP_NOC(K,J)))
c           MAXZ=MAX( MAXZ, Z(CLIP_NOC(K,J)))
c           END DO
c         END DO
         
c         INTERVAL=(MAXZ-MINZ)/6.D0
         
c         CONTOUR=MINZ
c         DO J=1,5
ccccccccccccccccccccccccccccccccccccccccccccccccccc   -1.5 meter            
                       CONTOUR=1.d0
           DO JJ=1,CLIP_NE
             FOUND=0
              CALL  TRI_CONTOUR(X,Y,Z,CLIP_NOC,jj,CONTOUR,XC,YC,FOUND)
             IF (FOUND.EQ.1) THEN
                WRITE(11,50)
                WRITE(11,306)'style8'
                WRITE(11,317)
                WRITE(11,110)
                DO K=1,2
                 WRITE(11,200)XC(K),YC(K),CONTOUR
                END DO
                WRITE(11,111)
                WRITE(11,318)
                WRITE(11,51)
             END IF
            END DO
            

ccccccccccccccccccccccccccccccccccccccccccccccccccc   -1 meters
           CONTOUR=-1.d0
           DO JJ=1,CLIP_NE
             FOUND=0
              CALL  TRI_CONTOUR(X,Y,Z,CLIP_NOC,jj,CONTOUR,XC,YC,FOUND)
             IF (FOUND.EQ.1) THEN
                WRITE(11,50)
                WRITE(11,306)'style7'
                WRITE(11,317)
                WRITE(11,110)
                DO K=1,2
                 WRITE(11,200)XC(K),YC(K),CONTOUR
                END DO
                WRITE(11,111)
                WRITE(11,318)
                WRITE(11,51)
             END IF
            END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccc   -0.5 meters
           CONTOUR=-0.5d0
           DO JJ=1,CLIP_NE
             FOUND=0
              CALL  TRI_CONTOUR(X,Y,Z,CLIP_NOC,jj,CONTOUR,XC,YC,FOUND)
             IF (FOUND.EQ.1) THEN
                WRITE(11,50)
                WRITE(11,306)'style6'
                WRITE(11,317)
                WRITE(11,110)
                DO K=1,2
                 WRITE(11,200)XC(K),YC(K),CONTOUR
                END DO
                WRITE(11,111)
                WRITE(11,318)
                WRITE(11,51)
             END IF
            END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccc   0 meters           
                       CONTOUR=0.d0
           DO JJ=1,CLIP_NE
             FOUND=0
              CALL  TRI_CONTOUR(X,Y,Z,CLIP_NOC,jj,CONTOUR,XC,YC,FOUND)
             IF (FOUND.EQ.1) THEN
                WRITE(11,50)
                WRITE(11,306)'style5'
                WRITE(11,317)
                WRITE(11,110)
                DO K=1,2
                 WRITE(11,200)XC(K),YC(K),CONTOUR
                END DO
                WRITE(11,111)
                WRITE(11,318)
                WRITE(11,51)
             END IF
            END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccc   0.5 meters           
                       CONTOUR=0.5d0
           DO JJ=1,CLIP_NE
             FOUND=0
              CALL  TRI_CONTOUR(X,Y,Z,CLIP_NOC,jj,CONTOUR,XC,YC,FOUND)
             IF (FOUND.EQ.1) THEN
                WRITE(11,50)
                WRITE(11,306)'style4'
                WRITE(11,317)
                WRITE(11,110)
                DO K=1,2
                 WRITE(11,200)XC(K),YC(K),CONTOUR
                END DO
                WRITE(11,111)
                WRITE(11,318)
                WRITE(11,51)
             END IF
            END DO

ccccccccccccccccccccccccccccccccccccccccccccccccccc   1 meter            
                       CONTOUR=1.d0
           DO JJ=1,CLIP_NE
             FOUND=0
              CALL  TRI_CONTOUR(X,Y,Z,CLIP_NOC,jj,CONTOUR,XC,YC,FOUND)
             IF (FOUND.EQ.1) THEN
                WRITE(11,50)
                WRITE(11,306)'style3'
                WRITE(11,317)
                WRITE(11,110)
                DO K=1,2
                 WRITE(11,200)XC(K),YC(K),CONTOUR
                END DO
                WRITE(11,111)
                WRITE(11,318)
                WRITE(11,51)
             END IF
            END DO
            
           
ccccccccccccccccccccccccccccccccccccccccccccccccccc   1.5 meter            
                       CONTOUR=1.5d0
           DO JJ=1,CLIP_NE
             FOUND=0
              CALL  TRI_CONTOUR(X,Y,Z,CLIP_NOC,jj,CONTOUR,XC,YC,FOUND)
             IF (FOUND.EQ.1) THEN
                WRITE(11,50)
                WRITE(11,306)'style2'
                WRITE(11,317)
                WRITE(11,110)
                DO K=1,2
                 WRITE(11,200)XC(K),YC(K),CONTOUR
                END DO
                WRITE(11,111)
                WRITE(11,318)
                WRITE(11,51)
             END IF
            END DO
            
            
            
            
C END WRITING CONTOURS     
              
          WRITE(11,320)  ! close folders      
          WRITE(11,320)
    
      END DO
      
C END OF LOOP OVER STATIONS

C WRITE CLOSE OUT
      WRITE(11,31)
      WRITE(11,21)         
      
      CLOSE(11)   
      
 10   FORMAT('<?xml version="1.0" encoding="UTF-8"?>')
 20   FORMAT('<kml xmlns="http://www.opengis.net/kml/2.2">')   
 21   FORMAT('</kml>') 
 30   FORMAT('<Document>')
 31   FORMAT('</Document>')
 40   FORMAT('<name>',A,'</name>')
 50   FORMAT('<Placemark>')
 51   FORMAT('</Placemark>')
 60   FORMAT('<Polygon>')
 61   FORMAT('</Polygon>')
 70   FORMAT('<extrude>1</extrude>')
 80   FORMAT('<LinearRing>')
 81   FORMAT('</LinearRing>')
 90   FORMAT('<outerBoundaryIs>')
 91   FORMAT('</outerBoundaryIs>')
 100  FORMAT('<innerBoundaryIs>')
 101  FORMAT('</innerBoundaryIs>')
 110  FORMAT('<coordinates>')
 111  FORMAT('</coordinates>')
 120  FORMAT('<altitudeMode>absolute</altitudeMode>')
 200  FORMAT(F18.14,',',F17.14,',',F7.5)
 
 300  FORMAT('<Style id="',A,'">')
 301  FORMAT('</Style>)')
 302  FORMAT('<PolyStyle>')
 303  FORMAT('</PolyStyle>')
C 304  FORMAT('<color>f80000ff</color>')
 304  FORMAT('<color>',A,'</color>')
 305  FORMAT('<colorMode>normal</colorMode>)')
 306  FORMAT('<styleUrl>',A,'</styleUrl>')
 307  FORMAT('<IconStyle>')
 308  FORMAT('</IconStyle>')
 309  FORMAT('<LineStyle>')
 310  FORMAT('</LineStyle>')
 
 311  FORMAT('<Icon>')
 312  FORMAT('<href>',A,'</href>')
 313  FORMAT('</Icon>')
 314  FORMAT('<scale>',F10.3,'</scale>')     
 
 315  FORMAT('<Point>')
 316  FORMAT('</Point>')
      
 317  FORMAT('<LineString>')
 318  FORMAT('</LineString>')
 
 319  FORMAT('<Folder>')
 320  FORMAT('</Folder>')
 
 321  FORMAT('<description>')
 322  FORMAT('</description>')
      
 
 500  format('<p><b>',A,'</b>',A,'</p>')
 501  format('<p><b>',A,'</b>',f14.8,'</p>')
 502  format('<p><b>',A,'</b>',i12,'</p>')
 503  format('<p><b>',A,'</b>',f7.3,A,'</p>')
 504  format('<p><b>',A,'</b>',3I9,'</p>')
 
      STOP
      END PROGRAM STATIONS2KML
C======================================================================
C||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


     
C______________________________________________________________________
C======================================================================
      SUBROUTINE LOCAT_CHK(LOCAT,XP,YP,NOC,X,Y,FOUND)
C----------------------------------------------------------------------
C THIS SUBROUTINE CHECKS IF A PARTICLE RESIDES WITHIN THE ELEMENT LOCAT
C IT RETURNS THE VALUE FOUND=1 IF THE PARTICLE IS FOUND OR FOUND=0 IF 
C IT WAS NOT FOUND. LOCAT,XP,YP ARE SCALAR INPUT. 
C----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER LOCAT,FOUND,NOC(3,1),I,NID(1)
      DOUBLE PRECISION XP,YP,X(1),Y(1),X1,X2,X3,Y1,Y2,Y3,
     &                 DS1(2),DS2(2),DS3(2),CROSS,C1,C2,C3
      
     
      FOUND=0
             
C . . GET DISPLACEMENTS FROM PARTICLE TO NODES    
      DS1(1)=X(NOC(1,LOCAT))-XP
      DS1(2)=Y(NOC(1,LOCAT))-YP
      DS2(1)=X(NOC(2,LOCAT))-XP
      DS2(2)=Y(NOC(2,LOCAT))-YP
      DS3(1)=X(NOC(3,LOCAT))-XP
      DS3(2)=Y(NOC(3,LOCAT))-YP

C . . ALL + CROSS PRODS. MEANS PART. IS FOUND IF NOC IS ANTI-CLOCKWISE      
      C1=CROSS(DS1,DS2)
      C2=CROSS(DS2,DS3)
      C3=CROSS(DS3,DS1)
       
      IF ((C1.GE.0).AND.(C2.GE.0).AND.(C3.GE.0)) FOUND=1
      
      RETURN
      END SUBROUTINE
C======================================================================
C||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||



C______________________________________________________________________
C======================================================================
      FUNCTION CROSS(DS1,DS2)
C----------------------------------------------------------------------
C THIS FUNCTION RETURNS THE CROSSPRODUCT OF TWO 2D VECTORS
C----------------------------------------------------------------------
      IMPLICIT NONE
      
      DOUBLE PRECISION CROSS,DS1(2),DS2(2)
      
      CROSS=(DS1(1)*DS2(2))-(DS1(2)*DS2(1))
      
      RETURN
      END FUNCTION
C======================================================================
C||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


      SUBROUTINE TRI_CONTOUR(X,Y,Z,NOC,EID,ELEV,XOUT,YOUT,FOUND)
      
      IMPLICIT NONE
     
      INTEGER I,J,K,NOC(3,1),N1,N2,INDX(3,2),EID,FOUND
      
      REAL*8 X(1),Y(1),Z(1),XOUT(2),YOUT(2),ELEV
      REAL*8 MINZ,MAXZ,X1,Y1,Z1,X2,Y2,Z2,DX,DY,DZ
      REAL*8 DX1,DY1,DZ1,RATIO
      
      INDX(1,1)=1
      INDX(1,2)=2
      INDX(2,1)=2
      INDX(2,2)=3
      INDX(3,1)=3
      INDX(3,2)=1

      
      DO J=1,2
        XOUT(J)=-9988.D0
        YOUT(J)=-9988.D0
      END DO
      
      FOUND=0
      I=EID
      
         MINZ=MIN( Z(NOC(1,I)), Z(NOC(2,I)) )
         MINZ=MIN( MINZ, Z(NOC(3,I)) )         
         
         MAXZ=MAX( Z(NOC(1,I)), Z(NOC(2,I)) )
         MAXZ=MAX( MAXZ, Z(NOC(3,I)) )
         
      
         IF ( MINZ.LT.ELEV .AND. MAXZ.GT.ELEV)    THEN  ! contour line crosses this element, find the points
      
            K=0
            DO J=1,3
         
             N1=NOC(INDX(J,1),I)
             N2=NOC(INDX(J,2),I)
         
             X1=X(N1) 
             Y1=Y(N1) 
             Z1=Z(N1) 
             X2=X(N2) 
             Y2=Y(N2) 
             Z2=Z(N2) 
           
                         
             if ( (min(z1,z2) .LT. elev) .AND. 
     &            (elev .LT. max(z1,z2))       ) THEN 
                  
                K=K+1
                dx = x2-x1
                dy = y2-y1
                dz = z2-z1
             
                dz1= elev - z1
            
                ratio= dz1 / dz
            
                dx1=dx * ratio
                dy1=dy * ratio
            
                XOUT(K)=x1 + dx1
                YOUT(K)=y1 + dy1
              END IF
              
              IF (K.EQ.2) FOUND=1
             
            END DO
         END IF 
                   
      RETURN
      END SUBROUTINE       
             
             
   

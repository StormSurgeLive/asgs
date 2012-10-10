C----------------------------------------------------------------------
C
C chkweirs.f program; This program checks weirs and land barrier
C heights in an ADCIRC grid and adjusts nodal elevations if necessary.
C it also has provisions for filling barrier heights where a no data
C values is specified (i.e. -99999) by linearly interpolating between
C adjacent barrier height values (interpolation assumes barrier nodes
C are equally spaced)
C
C----------------------------------------------------------------------
C
C Copyright(C) 2007,2012  Nathan Dill
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

C---------------------------------------------------------------------- 
      PROGRAM CHKWEIRS
C----------------------------------------------------------------------


      use gridstuff
       
      implicit none
      
      integer i,j,k,ll,n1,n2,n3
      
      real*8 xx(10000),tmp,DF,xxx(10000)
      
      character*80 filename,FNAME2
      
      
      
      
      write(*,*)'grid file?'
      read(*,*)filename
      
      WRITE(*,*)'OUT GRID FILE?'
      READ(*,*)FNAME2
      
      call read14(filename)
      
      IF (MAXNID.NE.NN) STOP 'NEED TO RENUMBER GRID'
      
c check to see if any weir heights haven't been defined yet
c linearly interpolate the heights between the ones that have been defined

      OPEN(87,FILE='NODES_2_HIGH.XYZ')

      K=0     
      DO I=1,NBOU
         write(*,*)'nbou',i,nbou
   
         DO J=1,NVELL(I)
cc            write(*,*)'nvell',j,nvell(i)
            K=K+1
cc            write(*,*)'k',k     
            
            IF ((IBTYPE(I).EQ. 4) .OR. (IBTYPE(I) .EQ. 14) .OR. 
     &        (IBTYPE(I) .EQ. 24) )  THEN
              IBTYPE(I)=24
              xx(j)=BARINHT(K)
            END IF
            IF ((IBTYPE(I).EQ. 3) .OR. (IBTYPE(I) .EQ. 13) .OR. 
     &        (IBTYPE(I) .EQ. 23) )  THEN
              IBTYPE(I)=23
              xxx(j)=BARLANHT(K)
            END IF
         END DO
         

         
          IF ((IBTYPE(I).EQ. 4) .OR. (IBTYPE(I) .EQ. 14) .OR. 
     &        (IBTYPE(I) .EQ. 24)) THEN
     
              call line_interp(xx,nvell(i),-99999.d0)
         
              k=k-nvell(i)
cc              write(*,*)'k2',k
           do j=1,nvell(i)
            k=k+1
cc            write(*,*)'k3',k
            BARINHT(k)=xx(j)
c also check to see if weirs are lower than nodes           
            tmp=(z(nbvv(k)) * -1.d0) 
            if (BARINHT(K) .LT. TMP) THEN
               DF=TMP-BARINHT(K)
C               BARINHT(K)=TMP+0.1D0
               z(nbvv(k))=-1.D0*(BARINHT(K) -0.3D0) 
               WRITE(87,'(3E20.11)')X(NBVV(K)),Y(NBVV(K)),DF
            END IF
           
cc            write(*,*)'heres the problem'
cc            write(*,*)k
cc            write(*,*)ibconn(k)
cc            write(*,*)z(ibconn(k))
           
            tmp=(z(IBCONN(k)) * -1.d0) 
            if (BARINHT(K) .LT. TMP) THEN
               DF=TMP-BARINHT(K)
C               BARINHT(K)=TMP+0.1D0
               z(IBCONN(k))=-1.D0*(BARINHT(K) -0.3D0) 
               WRITE(87,'(3E20.11)')X(IBCONN(K)),Y(IBCONN(K)),DF
            END IF

           
          end do
          
         end if 
         

         
         IF ((IBTYPE(I).EQ. 3) .OR. (IBTYPE(I) .EQ. 13) .OR. 
     &        (IBTYPE(I) .EQ. 23)) THEN
     
           k=k-nvell(i)
           do j=1,nvell(i)
            k=k+1        
            tmp=(z(nbvv(k)) * -1.d0) 
            if (BARLANHT(K) .LT. TMP) THEN
               DF=TMP-BARLANHT(K)
C               BARINHT(K)=TMP+0.1D0
               z(nbvv(k))=-1.D0*(BARLANHT(K) -0.3D0) 
               WRITE(87,'(3E20.11)')X(NBVV(K)),Y(NBVV(K)),DF
            END IF
         
           
          end do
          
         end if 
         
         
         
 
         
      END DO      
      
      CLOSE(87)
      
      
      
      
C WRITE RESULTS 
      OPEN(24,FILE=FNAME2)
      
            write(24,'(a60)')agrid
      write(24,*)ne,NN
c write nodes   

      do i=1,nn
          write(24,'(i12,3e20.12)')NID(I),x(nid(i)),y(nid(i)),z(nid(i))
      end do
      
c write elements      
      k=3
      do i=1,ne
        write(24,'(5i12)')i,k,(noc(j,i),j=1,3)
      end do
      
      
c write boundary info      
      
      write(24,*)NOPE,' = Number of open boundaries'
      write(24,*)NETA,' = Number of open boundary nodes'

      K=0
      DO I=1,NOPE
         write(24,*) NVDLL(I)
         DO J=1,NVDLL(I)
            K=K+1
            write(24,*)NBDV(K)
         END DO
      END DO
      
      
      write(24,*)NBOU
      write(24,*)NVEL
     
        

     
      K=0     
      DO I=1,NBOU
         write(24,*)NVELL(I),IBTYPE(I)
         DO J=1,NVELL(I)
            K=K+1
            IF ((IBTYPE(I).EQ. 3) .OR. (IBTYPE(I) .EQ. 13) .OR. 
     &        (IBTYPE(I) .EQ. 23) )  THEN
               write(24,'(I8,F12.3,f6.3)')NBVV(K),BARLANHT(K),
     &          BARLANCFSP(K)
                        
            ELSE IF ((IBTYPE(I).EQ. 4) .OR. (IBTYPE(I) .EQ. 14) .OR. 
     &        (IBTYPE(I) .EQ. 24) )  THEN
              write(24,'(2I8,F12.3,2f6.3)')NBVV(K),IBCONN(K)
     &         ,BARINHT(K),
     &         BARINCFSB(K),BARINCFSP(K)
            ELSE
              write(24,*)NBVV(K)
            END IF
         END DO
      END DO

           
      close(24)
      STOP 
      END PROGRAM
      
      
      
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
      subroutine line_interp(x,n,tag)
      
      implicit none
      
      integer i,j,k,n,itag(10000),ifrst,ilast,cnt
      real*8 x(1),tag,dx
      
            

      ifrst=0
      ilast=0


      
      do i=1,n
         itag(i)=0
c         write(*,*)'someting'
         
         if (x(i).eq.tag) itag(i)=1
c         write(*,*)'itag',itag(i)
         
      end do
      
      k=0
      do i=1,n
        if (itag(i).eq.1) k=k+1
      end do
      
      if (k.eq.n) then
         do i=1,n
           x(i)=tag
         end do
         return
      end if
      
      
      
      i=1
 5    continue     
      if (itag(i) .eq. 1) then
         i=i+1
         goto 5
      else 
        ifrst=i
      end if
      
      if (ifrst.ne.1) then
         do j=1,ifrst
            x(j)=x(ifrst)
         end do
      end if   
        
      i=ifrst+1      
 10   continue     
      if (itag(i).eq.0) then
        ifrst=i
        i=i+1
        if (i.gt.n) goto 30
        goto 10
      else  
        ilast=i
      end if
      
      i=ilast
 20   continue
      if (itag(i).eq.1)   then
        ilast=i+1
        i=i+1
        if (i.gt.n) then
           do j=ifrst,n
             x(j)=x(ifrst)
           end do
           goto 30
        end if 
          
        
        goto 20
      else 
        dx=x(ilast)-x(ifrst)
        k=ilast-ifrst
        dx=dx/k
        k=0
        do j=ifrst+1,ilast-1
           k=k+1
           x(j)=x(ifrst)+dx*k
        end do
      end if
      
      ifrst=ilast
      i=ilast
      if (i.lt.n) goto 10    
      
 30   continue     
       
 
      return
      end subroutine
        
      
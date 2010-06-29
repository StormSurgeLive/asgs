PROGRAM Connect2D
IMPLICIT NONE

        INTEGER i,j,k,nel,nno,iel,ino,mnndel,n1,n2,n3,nedges,ed_id,i1,i2
        INTEGER ied,jjel,jel,j1,j2,idum,ned,jed
        INTEGER LED(2,3),e(3)
        INTEGER, ALLOCATABLE :: nndel(:),ndel(:,:),nm(:,:),neled(:,:)
        INTEGER, ALLOCATABLE :: edgeflag(:,:),nedno(:,:),nedel(:,:)
        REAL(8), ALLOCATABLE :: x(:),y(:)
        CHARACTER(99) FILENAME,OUTPUTFILEMANE

         !  WRITE(*,*) "Please input grid file: "
         !  READ(6,*) FILENAME
         OPEN(11,File='drogue_input_1',Status='old')
           read(11,*)
           read(11,*) FILENAME
         CLOSE(11)

        OPEN(1,FILE=FILENAME)
                READ(1,*)
                READ(1,*) nel,nno

                ! Total number of edges
                ned = 3*nel

                ! Allocate memory
                ALLOCATE(x(nno),y(nno),nndel(nno),edgeflag(3,nel),nm(nel,3))
                ALLOCATE(neled(3,nel),nedno(2,ned),nedel(2,ned))

                DO i=1,nno
                        READ(1,*) j,x(j),y(j)
                ENDDO
                DO i=1,nel
                        READ(1,*) j,idum,nm(j,1),nm(j,2),nm(j,3)
                ENDDO
        CLOSE(1)
        WRITE(*,*) "Grid file read successfully"

        ! Count maximum of the number of the elements associated with a node
        nndel = 0
        DO i=1,nel
                nndel(nm(i,1)) = nndel(nm(i,1)) + 1
                nndel(nm(i,2)) = nndel(nm(i,2)) + 1
                nndel(nm(i,3)) = nndel(nm(i,3)) + 1
        ENDDO
        mnndel = 0
        DO ino=1,nno
                IF (nndel(ino)>mnndel) mnndel = nndel(ino)
        ENDDO
        !WRITE(*,*) "Max elements over all nodes :: ",mnndel
        ALLOCATE(ndel(nno,mnndel))
        WRITE(*,*) "Maximum elements for each node counted and allocated successfully"

        ! Get node to element connectivity
        nndel = 0
        DO iel=1,nel
                n1 = nm(iel,1); n2 = nm(iel,2); n3 = nm(iel,3)
                nndel(n1) = nndel(n1) + 1;  ndel(n1,nndel(n1)) = iel
                nndel(n2) = nndel(n2) + 1;  ndel(n2,nndel(n2)) = iel
                nndel(n3) = nndel(n3) + 1;  ndel(n3,nndel(n3)) = iel
        ENDDO
        WRITE(*,*) "Node to element connectivity calculated successfully"


        ! Get edge to element connectivity 
        edgeflag = 0
        nedno    = 0
        nedel    = 0
        neled    = 0
        nedges   = 0
        DO iel=1,nel
        !        print *,"iel: ",iel

                n1 = nm(iel,1); n2 = nm(iel,2); n3 = nm(iel,3)
                led(1,1) = n2; led(2,1) = n3
                led(1,2) = n3; led(2,2) = n1
                led(1,3) = n1; led(2,3) = n2

                DO ied = 1,3
                        IF (edgeflag(ied,iel)==1) CYCLE    
                        
                        i1 = led(1,ied); i2 = led(2,ied)
                        nedges = nedges + 1
                        ed_id = nedges
                        neled(ied,iel)  = ed_id
                        nedno(1,ed_id)  = i1
                        nedno(2,ed_id)  = i2
                        nedel(1,ed_id)  = iel
                        edgeflag(ied,iel) = 1

                        DO jjel = 1,nndel(I1)
                                jel = ndel(i1,jjel)
                                if (jel==iel) CYCLE
                                
                                DO jed=1,3
                                        j1 = nm(jel,MOD(jed+0,3)+1)
                                        j2 = nm(jel,MOD(jed+1,3)+1)
                                        IF ( ((j1==i1).and.(j2==i2)) .or. ((j1==i2).and.(j2==i1)) ) THEN
                                                neled(jed,jel) = ed_id
                                                nedel(2,ed_id) = jel
                                                edgeflag(jed,jel) = 1
                                                GOTO 10
                                        ENDIF
                                ENDDO
                        ENDDO
10              ENDDO


        ENDDO

        !**************************************************************
        !**************************************************************

        WRITE(*,*) "Writing output file"
        write(OUTPUTFILEMANE,'(A)') TRIM(FILENAME)//'.gr2'
        OPEN(10,FILE=OUTPUTFILEMANE)
                WRITE(10,*) nel,nno
                DO i=1,nno
                        WRITE(10,*) i,x(i),y(i)
                ENDDO
                DO i=1,nel
                        WRITE(10,*) i,nm(i,1),nm(i,2),nm(i,3)
                ENDDO

                ! Writing element-to-element connectivity
                DO i=1,nel
                        DO j=1,3
                                IF (nedel(2,neled(j,i)) .ne. i ) THEN
                                        e(j) = nedel(2,neled(j,i))
                                ELSE
                                        e(j) = nedel(1,neled(j,i))
                                ENDIF
                        ENDDO
                        WRITE(10,*) i,e(1),e(2),e(3)
                ENDDO
                
                ! Writing node-to-element connectivity
                DO i=1,nno
                        WRITE(10,'(20i10)') i,nndel(i),(ndel(i,j),j=1,nndel(i))
                ENDDO
        CLOSE(10)

        !**************************************************************
        !**************************************************************


        DEALLOCATE(nndel,ndel,nm,neled)
        DEALLOCATE(edgeflag,nedno,nedel)
        DEALLOCATE(x,y)


END PROGRAM


















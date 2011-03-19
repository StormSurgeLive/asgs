c**********************************************************************
c                                                                     *
c  Program to extract the FES95.2 tidal database of Le Provost to     *
c  values at the nodes of an ADCIRC grid.                             *
c                                                                     *
c  Output can be generated either at grid open boundary nodes for     *
c  incorporation into an ADCIRC run or at all nodes (in "TEA" format) *
c  for visualization (ACE/vis) or comparison with other results.      *
c                                                                     *
c  User is prompted for the names of the ADCIRC grid file and tidal   *
c  data base file.  The user is also prompted for the constituent to  *
c  analyse and whether phase output should be in degrees or radians.  *
c                                                                     *
c  Nodes located inside fully active FES95.2 grid square are assigned *
c  values using bilinear interpolation.  Nodes located inside a FES   *
c  grid square with only 3 active corners are assigned values using   *
c  linear interpolation from the 3 active corners.  Nodes located     *
c  inside a FES grid square with only 2 or 1 active nodes are not     *
c  computed.  Rather the dummy value of -999, -999. is assigned.      *
c                                                                     *
c            Comment line cleanup by R.L. 10/12/01                    *
c             Written by R.L.  3/10/1995    v2.06                     *
c               Modified from OSU_tea.for  v1.03                      *
c                 and from French_tea.for v1.05                       *
c                                                                     *
c**********************************************************************
c                                                                     *
c  The format of the output file is:                                  *
c                                                                     *
c  For a TEA format output:                                           *
c    output file name (e.g. M2_FES.tea)                               *
c    number of frequencies in the file (=1)                           *
c    frequency (rad/sec), status (1=active)                           *
c    constituent name (e.g. M2)                                       *
c    node #  amp (m)  phase (deg or rad)  # corners in FES grid       *
c                                         with undefined values       *
c                                         for this node (0-4)         *
c    node #  u amp  u phase  v amp  v phase (note these are dummy #s) *
c                                                                     *
c                                                                     *
c  For open boundary condition output:                                *
c    output file name (e.g. M2_FES.obc)                               *
c    number of frequencies in the file (=1)                           *
c    frequency (rad/sec), number of nodes (nnodes=neta)               *
c    constituent name (e.g. M2)                                       *
c    amp (m)  phase (deg or rad)  node #  # corners in FES grid       *
c                                         with undefined values       *
c                                         for this node (0-4)         *
c                                                                     *
c**********************************************************************
c                                                                     *
c   The FES95.2 tidal database has separate input files for each      *
c  constituent.  Each file is ascii with the format:                  *
c                                                                     *
c  lonmin, lonmax     (-180, 180)                                     *
c  latmin, latmax     ( -90,  90)                                     *
c  dlon,dlat          ( 0.5, 0.5)                                     *
c  nlon,nlat          ( 720, 361)                                     *
c  UNDEFa,UNDEFp     (999.9,999.9)                                    *
c                                                                     *
c  do j=1,nlat                                                        *
c     do i=1,nlon-1,30                                                *
c        (Ga(ii,j),ii=i,i+29)                                         *
c        (Gp(ii,j),ii=i,i+29)                                         *
c        enddo                                                        *
c     enddo                                                           *
c                                                                     *
c**********************************************************************

      program FES_interp

      PARAMETER(MNLON=720,MNLAT=361,MNP=50000)

      DIMENSION     Ga(MNLON,MNLAT),Gp(MNLON,MNLAT)
      DIMENSION     node(MNP),xlon(MNP),ylat(MNP),xtemp(MNP),ytemp(MNP)
      DIMENSION     iobcor(4)
      REAL*8        phi(4),c(4),s(4),tc,ts
      REAL*8        deg2rad,rad2deg,freq
      REAL          latmin,latmax,lonmin,lonmax
      character*55  datafile,gridfile,header
      character*3   consname
      logical       found

      deg2rad = atan(1.d0)/45.d0
      rad2deg = 45.d0/atan(1.d0)


c   General format statements

1055  FORMAT(a55)
1056  FORMAT(a2)
1110  FORMAT(' File ',A55,' WAS NOT FOUND!')
1111  FORMAT(' File ',A55,' WAS FOUND!')
1102  format(' *********************  Try Again  *********************')

c   Prompt for type of output, either open boundary values for ADCIRC run or
c   tea format output for entire grid

      write(*,*)' Do interpolation at 1=open boundary nodes/2=all nodes'
      read(*,*) interp
      write(*,*) ' '
      if((interp.lt.1).or.(interp.gt.2)) then
         write(*,*) ' Previous answer must be 1 or 2 '
         write(*,*) ' Program terminated'
         stop
         endif

c   Prompt for phase output in radians or degrees

      write(*,*) ' Phase angle output in 1=radians/2=degrees ?'
      read(*,*) ipout
      write(*,*) ' '
      if((ipout.lt.1).or.(ipout.gt.2)) then
         write(*,*) ' Previous answer must be 1 or 2 '
         write(*,*) ' Program terminated'
         stop
         endif

c   Prompt for, open and read in ADCIRC grid file

  21  write(*,*) ' Enter name of the ADCIRC grid file'
      write(*,*) ' '
      read(*,1055) gridfile
      inquire(file=gridfile,exist=found)
      if(found) goto 22
      write(*,1110) gridfile
      goto 21
  22  write(*,1111) gridfile

      open(11,file=gridfile)

      read(11,*)header
      read(11,*)ne,np
      if(np.ge.MNP) then
         write(*,*) ' ************** FATAL ERROR **********************'
         write(*,*) ' The number of nodes in the ADCIRC grid file > MNP'
         write(*,*) ' in the PARAMETER statement of this program.  '
         write(*,*) ' Increase MNP > ',np,' and recompile the source.'
         write(*,*) ' *************************************************'
         stop
         endif

      do n=1,np
         read(11,*) node(n),xtemp(n),ytemp(n),depth
         if(xtemp(n).lt.-180.) xtemp(n)=xtemp(n)+360.
         if(xtemp(n).ge.180.) xtemp(n)=xtemp(n)-360.
         end do

      if(interp.eq.2) then
         nnodes=np
         do n=1,nnodes
            xlon(n)=xtemp(n)
            ylat(n)=ytemp(n)
            end do
         endif

      if(interp.eq.1) then
         do n=1,ne
            read(11,*) nn,idum,i1,i2,i3
            end do
         read(11,*) nope
         read(11,*) neta
         nnodes=neta
         n=0
         do k=1,nope
            read(11,*) nvdll
            do i=1,nvdll
               n=n+1
               read(11,*) node(n)
               end do
            end do
         if(n.ne.neta) then
            write(*,*) 'Warning, the number of open boundary nodes does'
            write(*,*) '         not match NETA in the ADCIRC grid file'
            write(*,*) ' '
            endif
         nnodes=n
         do n=1,nnodes
            xlon(n)=xtemp(node(n))
            ylat(n)=ytemp(node(n))
            end do
         endif

      close(11)

      write(*,*)'Finished reading ADCIRC grid file'
      write(*,*)' '

c   Enter tidal constituent to process, assign frequency and open output file

      write(*,*)' Enter tidal constituent to process.'
      write(*,*)' The choices are K2,L2,M2,N2,S2,T2,K1,O1,P1,Q1,2N2,',
     &                                                      'MU2,NU2'
      write(*,*)' '
      read(*,1056) consname

      if((consname.eq.'K2').or.(consname.eq.'k2')) then
         consname='K2'
         freq=0.000145842317201d0
         if(interp.eq.1) then
            open(12,file='K2_FES.obc')
            write(12,*) ' K2_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='K2_FES.tea')
            write(12,*) ' K2_FES.tea'
            endif
         endif
      if((consname.eq.'L2').or.(consname.eq.'l2')) then
         consname='L2'
         freq=0.
         if(interp.eq.1) then
            open(12,file='L2_FES.obc')
            write(12,*) ' L2_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='L2_FES.tea')
            write(12,*) ' L2_FES.tea'
            endif
         endif
      if((consname.eq.'M2').or.(consname.eq.'m2')) then
         consname='M2'
         freq=0.000140518902509d0
         if(interp.eq.1) then
            open(12,file='M2_FES.obc')
            write(12,*) ' M2_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='M2_FES.tea')
            write(12,*) ' M2_FES.tea'
            endif
         endif
      if((consname.eq.'N2').or.(consname.eq.'n2')) then
         consname='N2'
         freq=0.000137879699487d0
         if(interp.eq.1) then
            open(12,file='N2_FES.obc')
            write(12,*) ' N2_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='N2_FES.tea')
            write(12,*) ' N2_FES.tea'
            endif
         endif
      if((consname.eq.'S2').or.(consname.eq.'s2')) then
         consname='S2'
         freq=0.000145444104333d0
         if(interp.eq.1) then
            open(12,file='S2_FES.obc')
            write(12,*) ' S2_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='S2_FES.tea')
            write(12,*) ' S2_FES.tea'
            endif
         endif
      if((consname.eq.'T2').or.(consname.eq.'t2')) then
         consname='T2'
         freq=0.
         if(interp.eq.1) then
            open(12,file='T2_FES.obc')
            write(12,*) ' T2_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='T2_FES.tea')
            write(12,*) ' T2_FES.tea'
            endif
         endif
      if((consname.eq.'K1').or.(consname.eq.'k1'))then
         consname='K1'
         freq=0.000072921158358d0
         if(interp.eq.1) then
            open(12,file='K1_FES.obc')
            write(12,*) ' K1_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='K1_FES.tea')
            write(12,*) ' K1_FES.tea'
            endif
         endif
      if((consname.eq.'O1').or.(consname.eq.'o1')) then
         consname='O1'
         freq=0.000067597744151d0
         if(interp.eq.1) then
            open(12,file='O1_FES.obc')
            write(12,*) ' O1_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='O1_FES.tea')
            write(12,*) ' O1_FES.tea'
            endif
         endif
      if((consname.eq.'P1').or.(consname.eq.'p1')) then
         consname='P1'
         freq=0.000072522945975d0
         if(interp.eq.1) then
            open(12,file='P1_FES.obc')
            write(12,*) ' P1_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='P1_FES.tea')
            write(12,*) ' P1_FES.tea'
            endif
         endif
      if((consname.eq.'Q1').or.(consname.eq.'q1')) then
         consname='Q1'
         freq=0.000064958541129d0
         if(interp.eq.1) then
            open(12,file='Q1_FES.obc')
            write(12,*) ' Q1_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='Q1_FES.tea')
            write(12,*) ' Q1_FES.tea'
            endif
         endif
      if((consname.eq.'2N2').or.(consname.eq.'2n2')) then
         consname='2N2'
         freq=0.
         if(interp.eq.1) then
            open(12,file='2N2_FES.obc')
            write(12,*) ' 2N2_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='2N2_FES.tea')
            write(12,*) ' 2N2_FES.tea'
            endif
         endif
      if((consname.eq.'MU2').or.(consname.eq.'mu2')) then
         consname='MU2'
         freq=0.
         if(interp.eq.1) then
            open(12,file='MU2_FES.obc')
            write(12,*) ' MU2_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='MU2_FES.tea')
            write(12,*) ' MU2_FES.tea'
            endif
         endif
      if((consname.eq.'NU2').or.(consname.eq.'nu2')) then
         consname='NU2'
         freq=0.
         if(interp.eq.1) then
            open(12,file='NU2_FES.obc')
            write(12,*) ' NU2_FES.obc'
            endif
         if(interp.eq.2) then
            open(12,file='NU2_FES.tea')
            write(12,*) ' NU2_FES.tea'
            endif
         endif

      write(12,*) 1
      if(interp.eq.1) write(12,*) freq,nnodes
      if(interp.eq.2) write(12,*) freq,1
      write(12,*) consname

c   Prompt for, open and read in FES95.2 tidal database file

  31  write(*,*) ' Enter name of the FES95.2 elevation data file'
      write(*,*) ' '
      read(*,1055) datafile
      inquire(file=datafile,exist=found)
      if(found) goto 32
      write(*,1110) datafile
      goto 31
  32  write(*,1111) datafile

      open(11,file=datafile)

      read(11,*) lonmin,lonmax
      read(11,*) latmin,latmax
      read(11,*) dlon,dlat
      read(11,*) nlon,nlat
      read(11,*) UNDEFa,UNDEFp

c      write(*,*) ' lonmin, lonmax = ',lonmin,lonmax
c      write(*,*) ' latmin, latmax = ',latmin,latmax
c      write(*,*) ' dlon, dlat = ',dlon,' ',dlat
c      write(*,*) ' nlon, nlat = ',nlon,' ',nlat
c      write(*,*) ' UNDEFa, UNDEFp = ',UNDEFa,' ',UNDEFp
c      write(*,*) ' '

      if(UNDEFa.ne.UNDEFp) then
         write(*,*) ' FATAL ERROR: UNDEFa <> UNDEFp '
         stop
         else
         UNDEF=UNDEFa
         endif

      do j=1,nlat
         do i=1,nlon-1,30
            read(11,*) (Ga(ii,j),ii=i,i+29)
            read(11,*) (Gp(ii,j),ii=i,i+29)
         enddo
      enddo

      close(11)

      write(*,*) 'Finished reading in FES95.2 data'
      write(*,*) ' '


c   Main loop for every node in ADCIRC grid

      do 99 n=1,nnodes

c     Find where in the FES95.2 grid the node is

         iobflag=0
         ixlo = (xlon(n)-lonmin)/dlon + 1
         xlonlo = lonmin + (ixlo-1)*dlon
         ixhi = ixlo + 1
         if(ixlo.eq.nlon) ixhi=1
         jylo = (ylat(n)-latmin)/dlat + 1
         ylatlo = latmin + (jylo-1)*dlat
         jyhi= jylo + 1

c        write(*,*) ' '
c        write(*,1000) Ga(ixlo,jylo),Gp(ixlo,jylo),
c    &                 Ga(ixhi,jylo),Gp(ixhi,jylo),
c    &                 Ga(ixhi,jyhi),Gp(ixhi,jyhi),
c    &                 Ga(ixlo,iyhi),Gp(ixlo,jyhi)
c1000 format(1x,f8.2,1x,f8.2,3x,f8.2,1x,f8.2)

         if((Ga(ixlo,jylo).eq.UNDEF).or.(Gp(ixlo,jylo).eq.UNDEF)) then
            iobflag=iobflag+1
            iobcor(iobflag)=1
            else
            c(1)=Ga(ixlo,jylo)*cos(deg2rad*Gp(ixlo,jylo))
            s(1)=Ga(ixlo,jylo)*sin(deg2rad*Gp(ixlo,jylo))
            endif
         if((Ga(ixhi,jylo).eq.UNDEF).or.(Gp(ixhi,jylo).eq.UNDEF)) then
            iobflag=iobflag+1
            iobcor(iobflag)=2
            else
            c(2)=Ga(ixhi,jylo)*cos(deg2rad*Gp(ixhi,jylo))
            s(2)=Ga(ixhi,jylo)*sin(deg2rad*Gp(ixhi,jylo))
            endif
         if((Ga(ixhi,jyhi).eq.UNDEF).or.(Gp(ixhi,jyhi).eq.UNDEF)) then
            iobflag=iobflag+1
            iobcor(iobflag)=3
            else
            c(3)=Ga(ixhi,jyhi)*cos(deg2rad*Gp(ixhi,jyhi))
            s(3)=Ga(ixhi,jyhi)*sin(deg2rad*Gp(ixhi,jyhi))
            endif
         if((Ga(ixlo,jyhi).eq.UNDEF).or.(Gp(ixlo,jyhi).eq.UNDEF)) then
            iobflag=iobflag+1
            iobcor(iobflag)=4
            else
            c(4)=Ga(ixlo,jyhi)*cos(deg2rad*Gp(ixlo,jyhi))
            s(4)=Ga(ixlo,jyhi)*sin(deg2rad*Gp(ixlo,jyhi))
            endif

c     Determine factors to interpolate FES results to the node

         if(iobflag.eq.0) then       ! 4 corners are active, use rectangle based linear interp.
            zeta=(xlon(n)-xlonlo)/dlon
            eta=(ylat(n)-ylatlo)/dlat
            phi(1)=(1.-zeta)*(1.-eta)
            phi(2)=zeta*(1.-eta)
            phi(3)=zeta*eta
            phi(4)=(1.-zeta)*eta
            endif

         if(iobflag.eq.1) then       ! 3 corners are active, use triangle based linear interp.
            if(iobcor(1).eq.1) then
               x1=xlonlo+dlon
               x2=xlonlo+dlon
               x3=xlonlo
               y1=ylatlo
               y2=ylatlo+dlat
               y3=ylatlo+dlat
               endif
            if(iobcor(1).eq.2) then
               x1=xlonlo+dlon
               x2=xlonlo
               x3=xlonlo
               y1=ylatlo+dlat
               y2=ylatlo+dlat
               y3=ylatlo
               endif
            if(iobcor(1).eq.3) then
               x1=xlonlo
               x2=xlonlo
               x3=xlonlo+dlon
               y1=ylatlo+dlat
               y2=ylatlo
               y3=ylatlo
               endif
            if(iobcor(1).eq.4) then
               x1=xlonlo
               x2=xlonlo+dlon
               x3=xlonlo+dlon
               y1=ylatlo
               y2=ylatlo
               y3=ylatlo+dlat
               endif
            twoarea=dlon*dlat
            phi(1)=((xlon(n)-x3)*(y2-y3)+(x3-x2)*(ylat(n)-y3))/twoarea
            phi(2)=((xlon(n)-x1)*(y3-y1)+(x1-x3)*(ylat(n)-y1))/twoarea
            phi(3)=((xlon(n)-x1)*(y1-y2)+(x2-x1)*(ylat(n)-y1))/twoarea
            endif

c     Interpolate constituent

         tc=0.d0
         ts=0.d0
         if(iobflag.eq.0) then        ! 4 corners are active
            do i=1,4
               tc=tc+phi(i)*c(i)
               ts=ts+phi(i)*s(i)
               enddo
            endif
         if(iobflag.eq.1) then        ! 3 corners are active
            do i=1,3
               j=iobcor(1)+i
               if(j.gt.4) j=j-4
               tc=tc+phi(i)*c(j)
               ts=ts+phi(i)*s(j)
               enddo
            endif

c     Compute amplitude and phase

         if(iobflag.le.1) then
            amp = sqrt(tc*tc+ts*ts)
            pha = rad2deg*acos(tc/amp)
            amp=amp/100.
            if(ts.lt.0) pha=360.-pha
            if(ipout.eq.1) pha=pha*deg2rad
            else
            amp=-999.
            pha=-999.
            endif
         if(interp.eq.2) write(12,1298) node(n),amp,pha,iobflag
 1298    format(1x,i7,1x,f10.4,1x,f8.2,1x,i2)
         if(interp.eq.1) write(12,1299) amp,pha,node(n),iobflag
 1299    format(1x,f10.4,1x,f8.2,1x,i7,1x,i2)

  99     continue

C   If writing output at all nodes, tack on to the end of the file dummy U and V
C   velocity amplitude and phases

      if(interp.eq.2) then
         uamp = 1.0
         upha = 2.0
         vamp = 3.0
         vpha = 4.0
         do j=1,nnodes
            write(12,1300) j,uamp,upha,vamp,vpha
 1300       format(1x,i7,1x,f10.4,1x,f8.2,1x,f10.4,1x,f8.2)
            end do
         endif

      stop
      END

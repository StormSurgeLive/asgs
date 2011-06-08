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
C  jgf2.07 20110319: added implicit none, explicitly declared all
C  variables, added capability to control input via command line 
C  parameters, thus making this program more amenable to automation.
C  Repeated code for specifying file extensions was removed. 
C  The pre-existing menu input capability was retained.
C               
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
      implicit none
      character(80), parameter :: version = "2.08"
      real, allocatable :: Ga(:,:), Gp(:,:)
      integer, allocatable :: node(:)
      real, allocatable :: xlon(:),ylat(:),xtemp(:),ytemp(:)
      integer :: iobcor(4)
      integer       iobflag, ixlo, ixhi, jylo, jyhi
      real*8        phi(4),c(4),s(4),tc,ts
      real*8        deg2rad,rad2deg,freq
      real*8        latmin,latmax,lonmin,lonmax
      real*8        dlon, dlat, ylatlo, xlonlo

      real*8        zeta, eta, x1, x2, x3, y1, y2, y3, twoarea, amp
      real          pha, uamp, upha, vamp, vpha
      integer       nlon, nlat
      character(2048) datafile,gridfile,header
      character(3)  consname
      logical       found
      integer       interp
      integer       ipout
      integer       ne, np, nope, neta, nvdll
      integer       n, nn, idum, i, i1, i2, i3, ii, j, k
      real          depth, undefa, undefp, undef
      integer       nnodes ! number of nodes to interpolate to in the target
      integer       argcount
      character(2048) cmdlinearg    
      character(4)  outFileExt
      character(2048) outFileName
      character(2048) dataDir
      logical       latLonOnly ! if true, just write lat lon and exit
      logical       toPosLon   ! if true, convert lon from -180/180 to 0/360

      deg2rad = atan(1.d0)/45.d0
      rad2deg = 45.d0/atan(1.d0)

c     General format statements

1055  FORMAT(a55)
1056  FORMAT(a2)
C
C     Initialize to reasonable defaults
      interp=1  ! we usually use this program to establish tidal b.c.s
      ipout=2   ! ADCIRC requires phase angle to be degrees
      gridfile='fort.14' ! this is what we usually name the ADCIRC mesh file
      consname='M2'      ! just use the most commonly requested as the default
      datafile='m2.fes95.2'
      outFileExt='.obc'  ! for ADCIRC
      outFileName='m2_FES.obc' ! for ADCIRC m2 tide
      dataDir = "."
      latLonOnly = .false.
      toPosLon = .false.
C
C     Process command line options, if any
      argcount = iargc()
      if (argcount.gt.0) then
         i = 0
         do while(i.lt.argcount)
            i = i + 1
            call getarg(i, cmdlinearg)
            select case(cmdlinearg(1:2))
               case("-b") ! just write node locations as lat lon (i.e., 
                          ! reversed); useful for tpx0 interpolation 
                  latLonOnly = .true.
               case("-l") ! to convert lon range from -180 to 180 -> 
                          ! 0 to 360; useful for tpx0 interpolation
                  toPosLon = .true.
               case("-n") ! either open boundaries or all nodes
                  i = i + 1
                  call getarg(i,cmdlinearg)
                  select case(trim(cmdlinearg))
                     case("open")
                        ! do nothing; open boundaries is the default
                     case("allnodes")
                        interp=2
                     case default
                        write(*,*) "ERROR: FES952_interp: -n '",
     &                     trim(cmdlinearg),"' not recognized."
                        stop
                  end select
               case("-p") ! phase angle units, degrees or radians
                  i = i + 1
                  call getarg(i,cmdlinearg)
                  select case(cmdlinearg)
                     case("degrees")
                        ! do nothing, degrees is the default
                     case("radians")
                        ipout = 2
                     case default
                        write(*,*) "ERROR: FES952_interp: -p '",
     &                     trim(cmdlinearg),"' not recognized."
                        stop
                  end select
               case("-f") ! adcirc mesh file name
                  i = i + 1
                  call getarg(i,gridfile)
               case("-c") ! name of tidal constituent
                  i = i + 1
                  call getarg(i,consname)
               case("-d") ! directory where tidal data files are stored
                  i = i + 1
                  call getarg(i,datadir)
               case("-v") ! show version information
                  write(*,*) "FES952_interp version ",version
               case("-h") ! show a help message
                  write(*,*) "Usage information:"
                  write(*,*) "-n [open] or [allnodes]"
                  write(*,*) "-p [degrees] or [radians]"
                  write(*,*) "-f ADCIRC mesh file name"
                  write(*,*) "-b (output node locations only)"
                  write(*,*) "-c name of tidal constituent"
                  write(*,*) "-d directory containing tidal db"
                  write(*,*) "-v show version information"
                  write(*,*) "-h this message"
                  stop
               case default
                  write(*,*) "ERROR: FES952_interp: ",
     &               "Command line option '",
     &               trim(cmdlinearg)," not recognized."
                  stop
            end select
         end do
      else ! end processing of command line options, if any
C
C        if command line options were not specified, use the menu approach
C
         ! Prompt for type of output, either open boundary values for ADCIRC
         ! run or tea format output for entire grid
         write(*,*) 
     &      ' Do interpolation at 1=open boundary nodes/2=all nodes'
         read(*,*) interp
         write(*,*) ' '
         if((interp.lt.1).or.(interp.gt.2)) then
            write(*,*) ' Previous answer must be 1 or 2 '
            write(*,*) ' Program terminated'
            stop
         endif
         ! Prompt for phase output in radians or degrees
         write(*,*) ' Phase angle output in 1=radians/2=degrees ?'
         read(*,*) ipout
         write(*,*) ' '
         if((ipout.lt.1).or.(ipout.gt.2)) then
            write(*,*) ' Previous answer must be 1 or 2 '
            write(*,*) ' Program terminated'
            stop
         endif
         ! Prompt for the ADCIRC grid file
  21     write(*,*) ' Enter name of the ADCIRC grid file'
         write(*,*) ' '
         read(*,1055) gridfile
         ! Enter tidal constituent to process
         write(*,*)' Enter tidal constituent to process.'
         write(*,*)
     &      ' The choices are K2,L2,M2,N2,S2,T2,K1,O1,P1,Q1,2N2,',
     &                                                      'MU2,NU2'
         write(*,*)' '
         read(*,1056) consname
      endif ! end menu-driven prompts for program input
C
C     Open and read in ADCIRC grid file
      inquire(file=trim(gridfile),exist=found)
      if (found.eqv..false.) then
         write(*,*) "ERROR: FES952_interp: The ADCIRC mesh file '",
     &      trim(gridfile),"' was not found."
         stop
      endif
      open(11,file=trim(gridfile),status='old',action='read')
      read(11,*) header
      read(11,*) ne,np
      allocate(node(np),xlon(np),ylat(np),xtemp(np),ytemp(np))
      do n=1,np
         read(11,*) node(n),xtemp(n),ytemp(n),depth
         if (xtemp(n).lt.-180.) xtemp(n)=xtemp(n)+360.
         if (xtemp(n).ge.180.) xtemp(n)=xtemp(n)-360.
         if ((toPosLon.eqv..true.).and.(xtemp(n).lt.0.0)) then
            xtemp(n)=xtemp(n)+360.
         endif
      end do
C
      if(interp.eq.2) then ! interpolate to all nodes in the target mesh
         nnodes=np
         xlon(:)=xtemp(:)
         ylat(:)=ytemp(:)
      endif
C
      if(interp.eq.1) then ! only interpolate to open boundary nodes
         do n=1,ne
            read(11,*) nn,idum,i1,i2,i3 ! skip past the element table
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
            write(*,*) 'WARNING: the number of open boundary nodes does'
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

      write(*,*)
     &   'INFO: FES952_interp: Finished reading ADCIRC grid file.'
C      
C     If user specified -b, just write lat lon to file ... this is useful
C     for tpxo interpolation. 
      if (latLonOnly.eqv..true.) then
         write(*,*) "INFO: Writing lat_lon file with node locations."
         open(12,file="lat_lon",status='replace',action='write')
         do n=1,nnodes
            write(12,*) ylat(n),xlon(n)
         end do
         close(12)
         write(*,*) "INFO: Finished writing lat_lon file."
         stop
      endif
C
C     Output file 
      ! extension  
      if (interp.eq.2) then
         outFileExt = '.tea'
      endif
      ! form output file name 
      outFileName = trim(consname)//'_FES'//trim(outFileExt)
      ! open output file
      open(12,file=outFileName,status='replace',action='write')
      ! assign frequency
      select case(consname)
         case('K2','k2')
            freq=0.000145842317201d0  
         case('L2','l2')
            freq=0.
         case('M2','m2')
            freq=0.000140518902509d0
         case('N2','n2')
            freq=0.000137879699487d0   
         case('S2','s2')
            freq=0.000145444104333d0
         case('T2','t2')
            freq=0.
         case('K1','k1')
            freq=0.000072921158358d0  
         case('O1','o1')
            freq=0.000067597744151d0
         case('P1','p1')
            freq=0.000072522945975d0
         case('Q1','q1')
            freq=0.000064958541129d0 
         case('2N2','2n2')
            freq=0.
         case('MU2','mu2')
            freq=0. 
         case('NU2','nu2')
            freq=0.
         case default
            write(*,*) "ERROR: FES952_interp: The tidal constituent '",
     &         trim(consname),"' was not recognized."
            stop
      end select
C
      write(12,*) 1
      if (interp.eq.1) write(12,*) freq,nnodes
      if (interp.eq.2) write(12,*) freq,1
      write(12,*) consname
C
c     open and read in FES95.2 tidal database file
      datafile = trim(dataDir)//'/'//trim(consname)//'.fes95.2'
      inquire(file=datafile,exist=found)
      if (found.eqv..false.) then
         write(*,*) "ERROR: FES952_interp: The tidal db file '",
     &      trim(datafile),"' was not found."
         stop
      endif
      open(11,file=trim(datafile),status='old',action='read')
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

      allocate(Ga(nlon,nlat),Gp(nlon,nlat))
      do j=1,nlat
         do i=1,nlon-1,30
            read(11,*) (Ga(ii,j),ii=i,i+29)
            read(11,*) (Gp(ii,j),ii=i,i+29)
         enddo
      enddo

      close(11)

      write(*,*)'INFO: FES952_interp: Finished reading in FES95.2 data.'


c     Main loop for every target node in ADCIRC grid

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

  99  continue

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
      write(*,*) "INFO: FES952_interp: Finished writing data to '",
     &   trim(outFileName),"'."
      stop
      END

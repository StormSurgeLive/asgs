!
!        GRID-SCOPE
!        OVER THE LIMIT FOR MEMORY USAGE OF SMS (32bit)
!          Phase 0. Checking Grid
!          Phase 1. Pull out the sub-grid from the global(original) grid.
!                   ( You can edit sub-gird on SMS. )
!          Phase 2. Rezone the nodal attributes on the sub-grid.
!          Phase 3. Recombine edited sub-grid on global grid.
!
!                            Copyleft 2008- by Seizo Tanaka,
!                                  and C.H.L. at University of Notre Dame
!
!        Release note;
!          2008.11.29. Trial Version (Ver.0.40)
!                      Except for Phase 2
!          2008.12. 1. Fix the cutted sub-grid boundary condition data style
!          2008.12. 3. Add the output file name option            (Ver.0.42a)
!          2009. 1.22. Add the Sequencer for node
!                      Don't read/write agrid!                    (Ver.0.43 )
!          2009. 2.17. Add the rectangular sub-grid shapes        (Ver.0.50 )
!          2009. 2.20. Add fort.13 auto-interplation system          (RC1)
!          2009. 3. 2. Change output format of fort.13 like fort.14  (RC2)
!          2009. 3. 4. Add Phase 0 to check the grid                 (RC2)
!          2009. 3.11. Add fort.13 option at Phase 0
!                      Fix renumbering for fort.13 at Phase 2,3      (RC2c)
!          2009. 3.12. Change search algorithm of overlapped element (RC2d)
!          2009. 3.18. Change output format                          (RC2d)
!          2009. 3.24. Move to Version 1.0
!          2009. 3.26. Fix the Bugs of counting algorithm of fort.13 (Ver.1.02)
!
!          2009. 8. 7. Compress space in output files                (ver.1.10)
!          2009. 9.14. Fix the Bug for renum13 in check grid.(Phase0)(ver.1.11)
!          2010. 2.10. Fix the Bug on Searching Algorithm of
!                      pair node of Barrier B.C. in Phase3           (ver.1.20)
!          2010. 4.07. Don't ask toal num of overlapped elements to eliminate,
!                      if all overlap elements are eliminated automatically.
!                                                                    (ver.1.20)
!          2010. 7.06. Memory Re-allocation system
!                          for Sefety counting of B.C. segment       (ver.1.22)
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
module gblgrid
 integer :: ne, np
 double precision, allocatable :: xyd(:,:)
 integer, allocatable :: nm(:,:)
end module gblgrid
module subgrid
 integer :: nes, nps
 double precision, allocatable :: xyds(:,:)
 integer, allocatable :: nms(:,:)
end module subgrid

program gridscope
implicit none
character(len=1024) :: fort14 ! mesh fle name
character(len=1024) :: fort13 ! nodal attributes file name
character(len=1024) :: project_name
character(len=1024) :: cmdlineopt
character(len=1024) :: cmdlinearg
logical :: checkgrid = .false.
logical :: pullsubgrid = .false.
logical :: recombinesubgrid = .false.
logical :: pullnodalattributes = .false.
logical :: leveeeditor = .false.
logical :: attributeeditor = .false.
logical :: bathymetryeditor = .false.
integer :: argcount  ! number of command line arguments
integer :: i         ! command line argument counter
!
fort14 = 'null'
fort13 = 'null'
project_name = 'project_name'
!
! process command line options
argcount = iargc() ! count up command line options
write(6,'(a,i0,a)') 'INFO: There are ',argcount,' command line options.'
i=0
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--meshfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      fort14 = trim(cmdlinearg)
   case("--nodalattributesfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      fort13 = trim(cmdlinearg)
   case("--projectname")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      project_name = trim(cmdlinearg)
   case("--checkgrid")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      checkgrid = .true.
   case("--pullsubgrid")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      pullsubgrid = .true.
   case("--pullnodalattributes")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      pullnodalattributes = .true.
   case("--recombinesubgrid")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      recombinesubgrid = .true.
   case("--leveeeditor")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      leveeeditor = .true.
   case("--attributeeditor")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      attributeeditor = .true.
   case("--bathymetryeditor")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      bathymetryeditor = .true.
   case default
      write(6,'(a,i0,a,a)') 'WARNING: Command line option ',i,' "',TRIM(cmdlineopt),'" was not recognized.'
   end select
end do      
!
if (checkgrid.eqv..true.) then
   call grid_check(fort14, fort13)
endif
if (pullsubgrid.eqv..true.) then
   call pull_out_subgrid (fort14, project_name)
endif
if (pullnodalattributes.eqv..true.) then
   if (trim(fort13).eq.'null') then
      write(6,'(a)') 'ERROR: Please provide the fort.13 file for pulling nodal attributes.'
   else
      call pull_out_nodals (fort14, project_name, fort13)
   endif
endif
if (recombinesubgrid.eqv..true.) then
   call merge_domain (fort14, project_name, fort13)
endif
if (leveeeditor.eqv..true.) then
   call levee_editor(fort14, project_name, fort13)
endif
if (attributeeditor.eqv..true.) then
   call fort13_editor(fort14, project_name, fort13)
endif   
if (bathymetryeditor.eqv..true.) then
   call bath_editor(fort14, project_name,fort13 )
endif
!
!
end program gridscope
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
subroutine pull_out_subgrid(fort14, project_name )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      character(120), intent(in) :: fort14, project_name
!
      character(120) :: agrid
      integer :: ne, np
      double precision, allocatable  :: xyd(:,:)
      integer,          allocatable  :: nm(:,:)
      integer :: nope, neta, nvdl_max
      integer,          allocatable  :: nvdll(:), nbdv(:,:)
      integer :: nbou, nvel, nvel_max
      integer,          allocatable  :: nvell(:), ibtype(:),  nbvv(:,:), ibconn(:,:)
      double precision, allocatable :: bar(:,:,:)
      integer :: nodemax
      integer, allocatable :: nsequencer(:)
!
!
!Open(14), for dynamic allocation
      write(6,*) 'START! READING of Global data:  ', fort14(1:len_trim(fort14))
      open(14,file=fort14,status='old',action='read')
         call read14_alloc ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax )
      close(14)
! Dymanic Memory Allocation for Global Grid
         allocate( xyd(3,np) )
         allocate( nm(ne,3) )
         allocate( nvdll(nope)  )
         allocate( nbdv(nope,nvdl_max) )
         allocate( nvell(nbou), ibtype(nbou)  )
         allocate( nbvv(nbou,nvel_max), ibconn(nbou,nvel_max), bar(3,nbou,nvel_max) )
         allocate( nsequencer(nodemax) )
!Re-open & read global grid data(14)
      open(14,file=fort14,status='old',action='read')
         call read14 ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax, nsequencer,      &
                       agrid, xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, &
                       bar )
      close(14)
      write(6,*) 'FINISH!'
      write(6,*)
! Pull the sub-grid from global grid
      call pullout ( ne, np, nope, nbou, nvdl_max, nvel_max,                        &
                     agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar, &
                     project_name )
!
   end subroutine pull_out_subgrid
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
! READ GRID DATA FOR MEMORY ALLOCATION
   subroutine read14_alloc ( iunit, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: iunit
      integer, intent(out) :: ne, np, nope, nbou, nvdl_max, nvel_max
      integer, intent(out) :: nodemax ! for Sequencer of node
      integer :: i, j, k
!
      nvdl_max = 0
      nvel_max = 0
      nodemax = 0
         read(iunit,*)
         read(iunit,*) ne, np
         do k = 1, np
            read(iunit,*) i
            nodemax = max(i,nodemax)
         enddo
         do k = 1, ne
            read(iunit,*)
         enddo
         write(6,*) '  |'
         read(iunit,*) nope
         read(iunit,*)
         do k = 1, nope
            read(iunit,*) i
            if( i >= nvdl_max ) nvdl_max = i
            do j = 1, i
               read(iunit,*)
            enddo
         enddo
         read(iunit,*) nbou
         read(iunit,*)
         do k = 1, nbou
            read(iunit,*) i
            if( i >= nvel_max ) nvel_max = i
            do j = 1, i
               read(iunit,*)
            enddo
         enddo
!
   end subroutine read14_alloc
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
! READ GRID DATA
   subroutine read14 ( iunit, ne, np, nope, nbou, nvdl_max, nvel_max,  nodemax, nsequencer,  &
                       agrid, xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, &
                       bar )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: iunit, nodemax
      integer, intent(inout) :: ne, np, nope, nbou, nvdl_max, nvel_max
      double precision, intent(out) :: xyd(3,np)
      integer, intent(out) :: nm(ne,3)
      character(120), intent(out) :: agrid
      integer, intent(out) :: neta, nvdll(nope), nbdv(nope,nvdl_max)
      integer, intent(out) :: nvel, nvell(nbou), nbvv(nbou,nvel_max), ibtype(nbou)
      integer, intent(out) :: ibconn(nbou,nvel_max)
      double precision, intent(out) :: bar(3,nbou,nvel_max)
!
      integer, intent(out) :: nsequencer(nodemax)
!
     integer :: i, j, k, jn, je, nhy
!
      nsequencer(:) = 0
      bar(:,:,:) = 0.0d0
      ibconn(:,:) = 0
!
        agrid = ' '
        read(iunit,*)
         read(iunit,*) ne, np
         do k = 1, np
            read(iunit,*) jn, (xyd(j,k), j=1,3)
            nsequencer(jn) = k
         enddo
         write(6,*) '  + '
         do k = 1, ne
            read(iunit,*) je, nhy, ( nm(k,j), j = 1, 3 )
            do j = 1, 3
               if( nm(k,j) <= 0 ) write(6,*) k,j, nm(k,j)
               nm(k,j) = nsequencer(nm(k,j))
            enddo
         enddo
         read(iunit,*) nope
         read(iunit,*) neta
         do k = 1, nope
            read(iunit,*) nvdll(k)
            do j = 1, nvdll(k)
               read(iunit,*) nbdv(k,j)
               nbdv(k,j) = nsequencer(nbdv(k,j))
            enddo
         enddo
         read(iunit,*) nbou
         read(iunit,*) nvel
         do k = 1, nbou
            read(iunit,*) nvell(k), ibtype(k)
            select case(ibtype(k))
               case(0,1,2,10,11,12,20,21,22,30,52)
                  do j = 1, nvell(k)
                     read(iunit,*) nbvv(k,j)
                     nbvv(k,j) = nsequencer(nbvv(k,j))
                  enddo
               case(3, 13, 23)
                  do j = 1, nvell(k)
                     read(iunit,*) nbvv(k,j), (bar(i,k,j), i=1,2)
                     nbvv(k,j) = nsequencer(nbvv(k,j))
                  enddo
               case(4, 24)
                  do j = 1, nvell(k)
                     read(iunit,*) nbvv(k,j), ibconn(k,j), (bar(i,k,j), i=1,3)
                     nbvv(k,j) = nsequencer(nbvv(k,j))
                     ibconn(k,j) = nsequencer(ibconn(k,j))
                  enddo
               case default
                  write(6,*) ' IBTYPE is not allowed', ibtype(k)
                  write(6,*)
                  write(6,*) '**** Hit the Enter-Key to stop ****'
                  read(5,*)
                  stop
            end select
         enddo
!
!      deallocate( nsequencer )
!
   end subroutine read14
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
! PULL THE SUB GRID OUT FROM GLOBAL GRID
   subroutine pullout ( ne, np, nope, nbou, nvdl_max, nvel_max,                       &
                        agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar,&
                        project_name )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: ne, np
      integer, intent(inout) :: nope, nbou, nvdl_max, nvel_max
      double precision, intent(inout) :: xyd(3,np)
      integer, intent(inout) :: nm(ne,3)
      character(120), intent(in) :: agrid, project_name
      integer, intent(inout) :: nvdll(nope), nbdv(nope,nvdl_max)
      integer, intent(inout) :: nvell(nbou), nbvv(nbou,nvel_max), ibtype(nbou), ibconn(nbou,nvel_max)
      double precision, intent(inout) :: bar(3,nbou,nvel_max)
!
!Pulled Grid (Sub-Grid)
      integer :: nes, nps
      integer :: nopes, nbous, nvdls_max, nvels_max
      double precision, allocatable :: xyds(:,:)
      integer, allocatable :: nms(:,:)
      integer, allocatable :: nvdlls(:), nbdvs(:,:)
      integer, allocatable :: nvells(:), nbvvs(:,:), ibtypes(:), ibconns(:,:)
      double precision, allocatable :: bars(:,:,:)
      character(120) :: gridname
!
      integer :: nbls, nbns_max
      integer, allocatable :: nblncs(:,:)
      integer, allocatable :: nprops(:), mprops(:)
!

      integer, allocatable :: nprop1(:), nprop2(:), mselect(:), mprop(:)
      integer :: n, m, i, j, k, l, n1, n2, n3, m1, m2, ks, nd, nep, neta, nvel, isw
      integer :: icdigit
      integer :: ishape ! sub-grid shapes parameter (0:Circle, 1:Rectangle)
      integer :: multip
!
      character(50) :: c(10)

!
      data icdigit / 20 /
!
      integer :: me, mp, mope, mbou
!
      allocate( nprop1(np), nprop2(np), mselect(ne), mprop(ne) )
!
      write(6,*) 'Select the sub-grid shape'
!!-PCK-
      call select_target(np, xyd, nprop1)
!
!!      write(6,*) '     0: Circle   1: Rectangle'
!!      read(5,*) ishape
!
!!      select case(ishape)
!!         case(0)
!!            call select_circle( np, xyd, nprop1 )
!!        case(1)
!!            call select_rectan( np, xyd, nprop1 )
!!         case(3)
!!            call select_arbitr( np, xyd, nprop1 )
!!         case default
!!            write(6,*) 'You have to select 0 or 1'
!!            write(6,*) 'Hit the Enter-key to stop'
!!            read(5,*)
!!            stop
!!      end select
!!-PCK-
!
! Select the element & make element table (sub => globe)
      nprop2(:) = 0
      mprop(:) = 0
      me = 0
      do m = 1, ne
         nep = 0
         do i = 1, 3
            nep = nep + nprop1(nm(m,i))
         enddo
         if( nep /= 0 ) then
             me = me + 1
             mselect(me) = m
             mprop(m) = 1
         endif
      enddo
      do m = 1, me
         do i = 1, 3
            nprop2(nm(mselect(m),i)) = 1
         enddo
      enddo
!
! Reconstruct node table (sub=> globe)
      mp = 0
      do n = 1, np
         if( nprop2(n) == 1 ) then
            mp = mp + 1
            nprop1(mp) = n
         endif
      enddo
!
      nes = me
      nps = mp
      allocate( xyds(3,nps), nms(nes,3), nprops(nps), mprops(nes) )
!
! Make the xyd and node connectivity data of sub-grid
      nprop2(:) = 0
      do n = 1, nps
         do i = 1, 3
            xyds(i,n) = xyd(i,nprop1(n))
         enddo
         nprop2(nprop1(n)) = n
      enddo
      do m = 1, nes
         do i = 1, 3
            nms(m,i) = nprop2(nm(mselect(m),i))
         enddo
      enddo
!
! Counting nodes on boudary of NEW sub-grid (for Allocate)
      nprops(:) = 0
      do m = 1, nes
            nprops(nms(m,1)) = nprops(nms(m,1)) + nms(m,2) - nms(m,3)
            nprops(nms(m,2)) = nprops(nms(m,2)) + nms(m,3) - nms(m,1)
            nprops(nms(m,3)) = nprops(nms(m,3)) + nms(m,1) - nms(m,2)
      enddo
      nbns_max = 20
      do n = 1, nps
         if( (nprops(n)/=0)  ) nbns_max = nbns_max + 1
      enddo
!
      allocate( nblncs(2,nbns_max) )
      mprops(:) = 1
      call  mkeline( nps, nes, nms, mprops, nbns_max, nbls, nblncs )
      write(6,*) nbls
!
      mope = 0
      neta = 0
      do k = 1, nope
         i = 0
         mope = mope + 1
         do j = 1, nvdll(k)
            if( nprop2(nbdv(k,j)) /= 0 ) then
               i = i + 1
               nbdv(mope,i) = nprop2(nbdv(k,j))
               neta = neta + 1
            endif
         enddo
         if( i == 0 ) then
           mope = mope - 1
         else
           nvdll(mope) = i
         endif
      enddo
!
      mbou = 0
      nvel = 0
      do k = 1, nbou
         i = 0
         m = ibtype(k)
         mbou = mbou + 1
         select case( m )
            case(0,1,2,10,11,12,20,21,22,30,52)
               do j = 1, nvell(k)
                  n = nbvv(k,j)
                  if( nprop2(n) /= 0 ) then
                     i = i + 1
                     nbvv(mbou,i) = nprop2(n)
                     nvel = nvel + 1
                  endif
               enddo
            case(3, 13, 23)
               do j = 1, nvell(k)
                  n = nbvv(k,j)
                  if( nprop2(n) /= 0 ) then
                     i = i + 1
                     nbvv(mbou,i) = nprop2(n)
                     bar(1,mbou,i) = bar(1,k,j)
                     bar(2,mbou,i) = bar(2,k,j)
                     nvel = nvel + 1
                  endif
               enddo
            case(4, 24)
               do j = 1, nvell(k)
                  n = nbvv(k,j)
                  if( (nprop2(n)/=0).and.(nprop2(ibconn(k,j))/= 0) ) then
                     i = i + 1
                     nbvv(mbou,i) = nprop2(n)
                     ibconn(mbou,i) = nprop2(ibconn(k,j))
                     bar(1,mbou,i) = bar(1,k,j)
                     bar(2,mbou,i) = bar(2,k,j)
                     bar(3,mbou,i) = bar(3,k,j)
                     nvel = nvel + 2
                  endif
               enddo
         end select
         if ( i == 0 ) then
            mbou = mbou - 1
         else
            nvell(mbou) = i
            ibtype(mbou) = m
         endif
      enddo
!
      nvdls_max = 0
      do m = 1, mope
         nvdls_max = max( nvdls_max, nvdll(m) )
      enddo

      multip = 2
 665  continue
      multip = multip + 1
      allocate( nvdlls(mope*multip), nbdvs(mope*multip,nvdls_max) )
      nopes = 0
      nvdlls(:) = 0
      do k = 1, mope
         nopes = nopes + 1
         if( nopes >= mope*multip ) then
           write(6,*) '     memory reallocation for BC', mope, nopes
           deallocate( nvdlls, nbdvs )
           goto 665
         endif
         nvdlls(nopes) = nvdlls(nopes) + 1
         nbdvs(nopes,nvdlls(nopes)) = nbdv(k,1)
         do j = 1, nvdll(k)-1
            n1= nbdv(k,j)
            n2= nbdv(k,j+1)
            nd = 0
            do n = 1, nbls
               m1 = nblncs(1,n)
               m2 = nblncs(2,n)
               if( (( n1 == m1 ) .and. (n2 == m2)).or. &
                   (( n1 == m2 ) .and. (n2 == m1))     ) then
                   nvdlls(nopes) = nvdlls(nopes) + 1
                   nbdvs(nopes,nvdlls(nopes)) = n2
                   nd = 1
                   exit
               endif
            enddo
            if( nd == 0 ) then
               nopes = nopes + 1
               if( nopes >= mope*multip ) then
                 write(6,*) '     memory reallocation for BC', mope, nopes
                 deallocate( nvdlls, nbdvs )
                 goto 665
               endif
               nvdlls(nopes) = nvdlls(nopes) + 1
               nbdvs(nopes,nvdlls(nopes)) = n2
            endif
         enddo
      enddo
!
      nvels_max = 0
      do m = 1, mbou
         nvels_max = max( nvels_max, nvell(m) )
      enddo

      multip = 2
 666  continue
      multip = multip + 1
      allocate( nvells(mbou*multip), nbvvs(mbou*multip,nvels_max),bars(3,mbou*multip,nvels_max) )
      allocate( ibtypes(mbou*multip), ibconns(mbou*multip,nvels_max) )
!
      nbous = 0
      nvells(:) = 0
      nbvvs(:,:) = 0
      ibconns(:,:) = 0
      do k = 1, mbou
            select case (ibtype(k))
               case(3, 13, 23) ;   ks = 2
               case(4, 24)     ;   ks = 3
               case default    ;   ks = 0
            end select
         nbous = nbous + 1
         if( nbous >= mbou*multip ) then
           write(6,*) '     memory reallocation for BC', mbou, nbous
           deallocate( nvells, nbvvs,bars )
           deallocate( ibtypes, ibconns )
           goto 666
         endif

         ibtypes(nbous) = ibtype(k)
         nvells(nbous) = nvells(nbous) + 1
         nbvvs(nbous,nvells(nbous)) = nbvv(k,1)
         do i = 1, ks
            bars(i,nbous,nvells(nbous)) = bar(i,k,1)
         enddo
         if(ks==3) ibconns(nbous,nvells(nbous)) = ibconn(k,1)
         do j = 1, nvell(k)-1
            n1= nbvv(k,j)
            n2= nbvv(k,j+1)
            nd = 0
            do n = 1, nbls
               m1 = nblncs(1,n)
               m2 = nblncs(2,n)
               if( (( n1 == m1 ) .and. (n2 == m2)).or. &
                   (( n1 == m2 ) .and. (n2 == m1))     ) then
                   nvells(nbous) = nvells(nbous) + 1
                   nbvvs(nbous,nvells(nbous)) = n2
                   do i = 1, ks
                      bars(i,nbous,nvells(nbous)) = bar(i,k,j+1)
                   enddo
                   if(ks==3) ibconns(nbous,nvells(nbous)) = ibconn(k,j+1)
                   nd = 1
                   exit
               endif
            enddo
            if( nd == 0 )then
                nbous = nbous + 1
                if( nbous >= mbou*multip ) then
                  write(6,*) '     memory reallocation for BC', mbou, nbous
                  deallocate( nvells, nbvvs,bars )
                  deallocate( ibtypes, ibconns )
                  goto 666
                endif
                ibtypes(nbous) = ibtype(k)
                nvells(nbous) = nvells(nbous) + 1
                nbvvs(nbous,nvells(nbous)) = n2
                do i = 1, ks
                   bars(i,nbous,nvells(nbous)) = bar(i,k,j+1)
                enddo
                if(ks==3) ibconns(nbous,nvells(nbous)) = ibconn(k,j+1)
            endif
         enddo
      enddo

!
! OUTPUT SUB-GRID
      isw = 50
      gridname = project_name(1:len_trim(project_name))//'-sub.grd'
      open(isw,file=gridname, status='unknown')
      write(6,*) 'Output GRID DATA:', gridname(1:len_trim(gridname))
      n3 = 3
!
!      write(isw,*) agrid(1:len_trim(agrid))
      write(isw,*) 'grid'
      write(c(1),*) nes
      write(c(2),*) nps
      write(isw,130) ( trim(adjustl(c(k))), k=1,2 )
!      write(isw,*) nes, nps
      do i = 1, nps
         write(c(1),*) i
         write(c(2),'(f18.10)') xyds(1,i)
         write(c(3),'(f18.10)') xyds(2,i)
         write(c(4),'(e18.10)') xyds(3,i)
         write(isw,130) ( trim(adjustl(c(k))), k=1,4 )
!         write(isw,501) i, (xyds(j,i), j=1,3)
      enddo
         write(c(2),*) n3
      do i = 1, nes
         write(c(1),*) i
         do j = 1, 3
            write(c(j+2),*) nms(i,j)
         enddo
         write(isw,130) ( trim(adjustl(c(k))), k=1,5 )
!         write(isw,502) i, n3, ( nms(i,j), j = 1, 3 )
      enddo
! 501  format(i10,2f18.10,e18.10)
! 502  format(5i10)
!
      write(isw,*) nopes
      write(isw,*) sum(nvdlls(:))
      do k = 1, nopes
         write(isw,*) nvdlls(k)
         do j = 1, nvdlls(k)
            write(c(1),*) nbdvs(k,j)
            write(isw,130) ( trim(adjustl(c(l))), l=1,1 )
!            write(isw,*) nbdvs(k,j)
         enddo
      enddo
!
      write(isw,*) nbous
      n = sum( nvells(:) )
      do k = 1, nbous
         if( (ibtypes(k)==4) .or. (ibtypes(k)==24) ) n=n+nvells(k)
      enddo
      write(isw,*) n
      do k = 1, nbous
         write(c(1),*) nvells(k)
         write(c(2),*) ibtypes(k)
         write(isw,130) ( trim(adjustl(c(l))), l=1,2 )
!         write(isw,*) nvells(k), ibtypes(k)
         select case(ibtypes(k))
         case(0,1,2,10,11,12,20,21,22,30,52)
            do j = 1, nvells(k)
               write(c(1),*) nbvvs(k,j)
               write(isw,130) ( trim(adjustl(c(l))), l=1,1 )
!               write(isw,510) nbvvs(k,j)
            enddo
         case(3,13,23)
            do j = 1, nvells(k)
               write(c(1),*) nbvvs(k,j)
               write(c(2),'(f18.10)') bars(1,k,j)
               write(c(3),'(f18.10)') bars(2,k,j)
               write(isw,130) ( trim(adjustl(c(l))), l=1,3 )
!               write(isw,510) nbvvs(k,j), (bars(i,k,j), i=1,2)
            enddo
         case(4,24)
            do j = 1, nvells(k)
               write(c(1),*) nbvvs(k,j)
               write(c(2),*) ibconns(k,j)
               write(c(3),'(f18.10)') bars(1,k,j)
               write(c(4),'(f18.10)') bars(2,k,j)
               write(c(5),'(f18.10)') bars(3,k,j)
               write(isw,130) ( trim(adjustl(c(l))), l=1,5 )
!               write(isw,511) nbvvs(k,j), ibconns(k,j), (bars(i,k,j), i=1,3)
            enddo
         end select
      enddo
 130  format( 10(1x,a) )
! 510  format( i10, 2f18.10)
! 511  format(2i10, 3f18.10)
!
      close(isw)
!
!
      open(50,file='.tmp01',status='replace',action='write')
      n1 = 0
      write(50,'(i0)') icdigit
      m = mod(ne,icdigit)
      if( m == 0 ) then
        m = ne / icdigit
      else
        m = ne / icdigit + 1
      endif
      do i = 1, m
         n2 = 0
         do j = icdigit, 1, -1
            n1 = n1 + 1
            if( n1 >  ne ) exit
               n2 = n2 + ( 2**(j-1))*mprop(n1)
         enddo
         write(50,'(i0)') n2
      enddo
!      write(50,'(20i2)') ( mprop(m), m = 1, ne )
!
      deallocate(nprop1,nprop2, mselect, mprop)
!
      write(6,*) 'Finish, please hit Enter key'
      read(5,*)
!
!
   end subroutine pullout
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine select_circle( np, xyd, nprop )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer,intent(in) :: np
      double precision, intent(in) :: xyd(3,np)
      integer, intent(out) :: nprop(np)
      integer :: n, i
      double precision :: xc(2), dx, xd
!
      write(6,*) 'To define the circle,'
      write(6,*) 'you have to designate the center and diameter.'
      write(6,*) '  ===> Input center of sub-grid (x,y) ;'
      read(5,*) xc(1), xc(2)
      write(6,*) '  ===> Input the diameter of sub-grid (degrees):'
      read(5,*) dx
        dx = dx * 0.5d0
        dx = dx * dx
! Select the nodes in the sub-grid area
      nprop(:) = 0
      do n = 1, np
         xd = 0.0d0
         do i = 1, 2
            xd = xd + (xc(i)-xyd(i,n)) * (xc(i)-xyd(i,n))
         enddo
         if ( xd <= dx ) nprop(n) = 1
      enddo
!
   end subroutine select_circle
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine select_rectan( np, xyd, nprop )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer,intent(in) :: np
      double precision, intent(in) :: xyd(3,np)
      integer, intent(out) :: nprop(np)
      integer :: n, i
      double precision :: x1(2), x2(2), xa(2), aa(2)
!
      write(6,*) 'To define the rectangle,'
      write(6,*) 'you have to designate the edge of diagonal.'
      write(6,*) '  ===> Input the coodinate of 1st point (x,y) ;'
      read(5,*) x1(1), x1(2)
      write(6,*) '  ===> Input the coodinate of 2nd point (x,y) ;'
      read(5,*) x2(1), x2(2)
! Select the nodes in the sub-grid area
      do i = 1, 2
         xa(i) = (x1(i) + x2(i) ) * 0.5d0
         aa(i) = dabs( x1(i) - x2(i) ) * 0.5d0
      enddo
      nprop(:) = 0
      do n = 1, np
         do i = 1, 2
            x1(i) = dabs(xyd(i,n) - xa(i))
         enddo
         if( (x1(1) <= aa(1)) .and. (x1(2) <= aa(2)) ) nprop(n) = 1
      enddo
!
   end subroutine select_rectan
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine select_arbitr( np, xyd, nprop )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!$    use omp_lib
      implicit none
      integer,intent(in) :: np
      double precision, intent(in) :: xyd(3,np)
      integer, intent(out) :: nprop(np)
      integer :: n, m, i, j, icount
      integer :: node, nelm
      double precision, allocatable :: xx(:,:)
      integer, allocatable :: nc(:,:)
!
      double precision :: xmax(2), xmin(2), xa(2), aa(2)
      double precision :: xc, yc, x1, x2, x3, y1, y2, y3, a1, a2, a3, a0
      double precision, parameter :: eps = 1.0d-12
      character*120 :: filename
!
      write(6,*) 'To define the sub-grid area with arbitrary shapes,'
      write(6,*) 'you have to input the triangles based on FE mesh.'
      write(6,*) '    (SMS GRID Format)'
      write(6,*) '  ===> Input file name of file ;'
      read(5,*) filename
      open(10,file=filename,status='old',action='read')
      read(10,*)
      read(10,*) nelm, node
      allocate( xx(2,node), nc(3,nelm) )
      do j = 1, node
         read(10,*) n,    ( xx(i,n), i=1,2 )
      enddo
      read(10,*) (n, m, ( nc(i,n), i=1,3), j=1,nelm)
!
      do i = 1, 2
         xmax(i) = xx(i,1)
         xmin(i) = xx(i,1)
      enddo
      do n = 1, node
         do i = 1, 2
            xmax(i) = dmax1(xmax(i),xx(i,n))
            xmin(i) = dmin1(xmin(i),xx(i,n))
         enddo
      enddo
      do i = 1, 2
         xa(i) = ( xmax(i) + xmin(i) ) * 0.5d0
         aa(i) = dabs( xmax(i) - xmin(i) ) * 0.5d0
      enddo

! Select the nodes in the sub-grid area
      nprop(:) = 0
!$OMP PARALLEL DO PRIVATE( n, xc, yc, m, x1, x2, x3, y1, y2, y3, a0, a1, a2, a3 )  &
!$OMP             SHARED( np, xyd, nelm, nc, nprop )
      do n = 1, np
         xc = xyd(1,n); yc = xyd(2,n)
         if( (dabs( xc - xa(1) ) > aa(1)) .and. (dabs( yc - xa(2) ) > aa(2) ) ) cycle
         do m = 1, nelm
            x1 = xx(1,nc(1,m))
            x2 = xx(1,nc(2,m))
            x3 = xx(1,nc(3,m))
            y1 = xx(2,nc(1,m))
            y2 = xx(2,nc(2,m))
            y3 = xx(2,nc(3,m))
            a0 =  (x1 - x2) * (y1 - y3) - (x1 - x3) * (y1 - y2)
            a1 = ((xc - x2) * (yc - y3) - (xc - x3) * (yc - y2) ) / a0
            a2 = ((x1 - xc) * (y1 - y3) - (x1 - x3) * (y1 - yc) ) / a0
            a3 = ((x1 - x2) * (y1 - yc) - (x1 - xc) * (y1 - y2) ) / a0
            if( (a1 >= -eps ) .and. ( a2 >= -eps ) .and. ( a3 >= -eps ) ) then
               nprop(n) = 1
               exit
            endif
         enddo
      enddo
!$OMP END PARALLEL DO
!
      deallocate( xx, nc )
!
   end subroutine select_arbitr
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine merge_domain (fort14, project_name, fort13 )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      use gblgrid
      use subgrid
      implicit none
      character(120), intent(in) :: fort14, project_name, fort13
!
      character(120) :: agrid, gridname
! Old Grobal grid
!      integer :: ne, np
!      double precision, allocatable  :: xyd(:,:)
!      integer,          allocatable  :: nm(:,:)
      integer :: nope, neta, nvdl_max
      integer,          allocatable  :: nvdll(:), nbdv(:,:)
      integer :: nbou, nvel, nvel_max
      integer,          allocatable  :: nvell(:), ibtype(:),  nbvv(:,:), ibconn(:,:)
      double precision, allocatable  :: bar(:,:,:)
      integer :: nodemax
      integer, allocatable :: nsequencer(:)
! NEW Sub-grid grid
!      integer :: nes, nps
!      double precision, allocatable  :: xyds(:,:)
!      integer,          allocatable  :: nms(:,:)
      integer :: nopes, netas, nvdls_max
      integer,          allocatable  :: nvdlls(:), nbdvs(:,:)
      integer :: nbous, nvels, nvels_max
      integer,          allocatable  :: nvells(:), ibtypes(:),  nbvvs(:,:), ibconns(:,:)
      double precision, allocatable  :: bars(:,:,:)
      integer :: nodemaxs
      integer, allocatable :: nsequencers(:)
!
!
!
!Open(14), for dynamic allocation
      write(6,*) 'START! READING of Global data:  ', fort14(1:len_trim(fort14))
      open(14,file=fort14,status='old',action='read')
         call read14_alloc ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax )
      close(14)
! Dymanic Memory Allocation for Global Grid
         allocate( xyd(3,np) )
         allocate( nm(ne,3) )
         allocate( nvdll(nope)  )
         allocate( nbdv(nope,nvdl_max) )
         allocate( nvell(nbou), ibtype(nbou)  )
         allocate( nbvv(nbou,nvel_max), ibconn(nbou,nvel_max), bar(3,nbou,nvel_max) )
         allocate( nsequencer(nodemax) )
!Re-open & READ GLOBAL GRID DATA(14)
      open(14,file=fort14,status='old',action='read')
         call read14 ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax, nsequencer,     &
                       agrid, xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, &
                       bar )
      write(6,*) 'FINISH!'
      write(6,*)
      close(14)
!
!Open(14), for dynamic allocation ( Edited Sub-grid )
      gridname = project_name(1:len_trim(project_name))//'-sub.grd'
      write(6,*) 'START! READING of Edited Sub-grid data:  ', gridname(1:len_trim(gridname))
      open(14,file=gridname,status='old',action='read')
         call read14_alloc ( 14, nes, nps, nopes, nbous, nvdls_max, nvels_max, nodemaxs )
      close(14)
! Dymanic Memory Allocation for Sub-grid
         allocate( xyds(3,nps) )
         allocate( nms(nes,3) )
         allocate( nvdlls(nopes)  )
         allocate( nbdvs(nopes,nvdls_max) )
         allocate( nvells(nbous), ibtypes(nbous)  )
         allocate( nbvvs(nbous,nvels_max), ibconns(nbous,nvels_max), bars(3,nbous,nvels_max) )
         allocate( nsequencers(nodemaxs) )
!Re-open & READ SUB-GRID DATA(14)
      open(14,file=gridname,status='old',action='read')
         call read14 ( 14, nes, nps, nopes, nbous, nvdls_max, nvels_max, nodemaxs, nsequencers,        &
                       agrid, xyds, nms, netas, nvdlls, nbdvs, nvels, nvells, ibtypes, nbvvs, ibconns, &
                       bars )
      write(6,*) 'FINISH!'
      write(6,*)
      close(14)
!
!
      call merge_ ( nope,  nbou,  nvdl_max,  nvel_max,                           &
                    agrid, nvdll,  nbdv,  nvell,  ibtype,  nbvv,  ibconn,  bar, &
                    nopes, nbous, nvdls_max, nvels_max,                          &
                           nvdlls, nbdvs, nvells, ibtypes, nbvvs, ibconns, bars,&
                    project_name, fort14, fort13, nodemax, nsequencer )
!
   end subroutine merge_domain
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine merge_ ( nope,  nbou,  nvdl_max,  nvel_max,                           &
                       agrid, nvdll,  nbdv,  nvell,  ibtype,  nbvv,  ibconn,  bar, &
                       nopes, nbous, nvdls_max, nvels_max,                          &
                              nvdlls, nbdvs, nvells, ibtypes, nbvvs, ibconns, bars,&
                       project_name, fort14, fort13, nodemax, nsequencer )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      use gblgrid
      use subgrid
      implicit none
!      integer, intent(in) :: ne, np
      integer, intent(in) :: nope, nbou, nvdl_max, nvel_max
!      double precision, intent(in) :: xyd(3,np)
!      integer, intent(in) :: nm(ne,3)
      character(120), intent(in) :: agrid, project_name, fort14, fort13
      integer, intent(in) :: nvdll(nope), nbdv(nope,nvdl_max)
      integer, intent(in) :: nvell(nbou), nbvv(nbou,nvel_max), ibtype(nbou), ibconn(nbou,nvel_max)
      double precision, intent(in) :: bar(3,nbou,nvel_max)
!
      integer, intent(in) :: nodemax, nsequencer(nodemax)
!
      integer, intent(in) :: nopes, nbous, nvdls_max, nvels_max
      integer, intent(in) :: nvdlls(nopes), nbdvs(nopes,nvdls_max)
      integer, intent(in) :: nvells(nbous), nbvvs(nbous,nvels_max), ibtypes(nbous), ibconns(nbous,nvels_max)
      double precision, intent(in) :: bars(3,nbous,nvels_max)

!
      integer          :: nen, npn
      integer          :: nopen, nboun, nvdln_max, nveln_max
      double precision, allocatable :: xydn(:,:)
      integer, allocatable :: nmn(:,:)
      integer, allocatable :: nvdlln(:), nbdvn(:,:)
      integer, allocatable :: nvelln(:), nbvvn(:,:), ibtypen(:), ibconnn(:,:)
      double precision, allocatable :: barn(:,:,:)

      integer :: nseg
      integer, allocatable :: nvllseg(:), nbvseg(:,:), ibtypeseg(:), ibconnseg(:,:), nplseg(:)
      double precision, allocatable :: barseg(:,:,:)
!
      integer, allocatable :: nmap(:), mmap(:), netable(:), nptable(:), nblnc(:,:), nbdnc(:,:)
      integer, allocatable :: nprop1(:)
!
      integer :: nblg
      integer, allocatable :: nblncg(:,:)
      integer, allocatable :: npline(:), nplines(:)
!
      integer, allocatable :: nblncs(:,:), nbdncs(:,:), nmaps(:), mmaps(:), nprops(:)
      integer, allocatable :: nnbn(:), npseg(:), mpseg(:)
      integer :: i, j, k, l, n, m, n1, n2, m1, m2, l1, l2, ii, io, nd, js, ks, nsta, ista,nend, iend, ip
      integer :: nbn_max, nbl, nbd, nbn
      integer :: nbns_max, nbls, nbds, nbns
      double precision :: xn1, xn2, yn1, yn2, dx1, dx2, dy1, dy2
      double precision :: eps
!
      integer :: nchk, lchk, multip
!
      character(120) :: gridname
!
!
      data eps / 1.0d-6 /
!
     allocate( nptable(np), nprop1(np), netable(ne) )
!Make boundary edge data of global grid ( Entire Domain )
! Counting nodes on boudary of global grid (for Allocate)
  write(6,*)
  write(6,*) ' Now, Searching the border of global-sub grid'
      nprop1(:) = 0
      do m = 1, ne
         nprop1(nm(m,1)) = nprop1(nm(m,1)) + nm(m,2) - nm(m,3)
         nprop1(nm(m,2)) = nprop1(nm(m,2)) + nm(m,3) - nm(m,1)
         nprop1(nm(m,3)) = nprop1(nm(m,3)) + nm(m,1) - nm(m,2)
      enddo
      nbn_max = 20
      do n = 1, np
         if( (nprop1(n)/=0)  ) nbn_max = nbn_max + 1
      enddo
      netable(:) = 1
!
      allocate(nblncg(2,nbn_max) )
      call  mkeline( np, ne, nm, netable, nbn_max, nblg, nblncg )
  write(6,*) '    +'
!
!
!
!Read element property of global grid (Contained in sub:1, not:0)
      call readetab( ne, netable )
!
  write(6,*) '    |'
!
!Make boundary edge data of global grid ( Around Sub-grid )
! Counting nodes on boudary of sub-grid (for Allocate)
      nprop1(:) = 0
      nptable(:) = 0
      do m = 1, ne
         if( netable(m) == 1 ) then
            do i = 1, 3
               nptable(nm(m,i)) = 1
            enddo
            nprop1(nm(m,1)) = nprop1(nm(m,1)) + nm(m,2) - nm(m,3)
            nprop1(nm(m,2)) = nprop1(nm(m,2)) + nm(m,3) - nm(m,1)
            nprop1(nm(m,3)) = nprop1(nm(m,3)) + nm(m,1) - nm(m,2)
         endif
      enddo
      nbn_max = 20
      do n = 1, np
         if( (nprop1(n)/=0)  ) nbn_max = nbn_max + 1
      enddo
!
      allocate(nblnc(2,nbn_max) )
      call  mkeline( np, ne, nm, netable, nbn_max, nbl, nblnc )
!
  write(6,*) '    +'
      allocate( npline(nbl) )
      npline(:) = 1
      do m = 1, nbl
         m1 = nblnc(1,m)
         m2 = nblnc(2,m)
         do n = 1, nblg
            n1 = nblncg(1,n)
            n2 = nblncg(2,n)
            if( (m1==n1) .and. (m2==n2) ) then
                npline(m) = 0  ! Not Border Edge between Glob and Sub.
                exit
            endif
         enddo
      enddo
      do k = 1, nbou
       if( (ibtype(k)/=4) .and. (ibtype(k)/=24) ) cycle
         do j = 1, nvell(k)-1
            n1 = nbvv(k,j)
            n2 = nbvv(k,j+1)
            l1 = ibconn(k,j)
            l2 = ibconn(k,j+1)
            nchk=0
            do m = 1, nbl
               m1 = nblnc(1,m)
               m2 = nblnc(2,m)
               if( ((m1==n1) .and. (m2==n2)) .or. &
                   ((m1==n2) .and. (m2==n1)) ) then
                   nchk=m
                   exit
               endif
            enddo
            lchk=0
            do m = 1, nbl
               m1 = nblnc(1,m)
               m2 = nblnc(2,m)
               if( ((m1==l1) .and. (m2==l2)) .or. &
                   ((m1==l2) .and. (m2==l1)) ) then
                   lchk=m
                   exit
               endif
            enddo
            if( (nchk*lchk == 0) .and. (nchk+lchk /= 0) ) then
              npline(nchk+lchk) = 1
            endif
         enddo
      enddo
!
      nbd = sum(npline(:))
      allocate(nbdnc(2,nbd))
      nbd = 0
      do n = 1, nbl
         if(npline(n) == 0) cycle
            nbd = nbd + 1
            do i = 1, 2
               nbdnc(i,nbd) = nblnc(i,n)
            enddo
      enddo
  write(6,*) '    |'
!
!
! Counting nodes on boudary of NEW sub-grid (for Allocate)
      allocate(nprops(nps))
      nprops(:) = 0
      do m = 1, nes
            nprops(nms(m,1)) = nprops(nms(m,1)) + nms(m,2) - nms(m,3)
            nprops(nms(m,2)) = nprops(nms(m,2)) + nms(m,3) - nms(m,1)
            nprops(nms(m,3)) = nprops(nms(m,3)) + nms(m,1) - nms(m,2)
      enddo
      nbns_max = 20
      do n = 1, nps
         if( (nprops(n)/=0)  ) nbns_max = nbns_max + 1
      enddo
!
      allocate( nblncs(2,nbns_max),mmaps(nes) )
      mmaps(:) = 1
      call  mkeline( nps, nes, nms, mmaps, nbns_max, nbls, nblncs )
!
  write(6,*) '    +'
!
!
  write(6,*) ' Now Checking! The pair nodes of borders of global-NEWsub grid'
      allocate( nmap(np), nmaps(nps), mmap(ne) )
      nmap(:) = 0
      nprop1(:) = 0
      nmaps(:) = -1
      j = 0
      do n = 1, nbd
         n1 = nbdnc(1,n)
         n2 = nbdnc(2,n)
         xn1 = xyd(1,n1); yn1 = xyd(2,n1)
         xn2 = xyd(1,n2); yn2 = xyd(2,n2)
         nptable(n1) = 2
         nptable(n2) = 2
         i = 0
         do m = 1, nbls
            m1 = nblncs(1,m)
            m2 = nblncs(2,m)
            dx1 = xn1-xyds(1,m1); dy1 = yn1 - xyds(2,m1)
            dx2 = xn2-xyds(1,m2); dy2 = yn2 - xyds(2,m2)
            dx1 = dx1*dx1 + dy1*dy1
            dx2 = dx2*dx2 + dy2*dy2
            if((dsqrt(dx1)<=eps).and.(dsqrt(dx2)<=eps)) then
               i = i + 1
               nmaps(m1) = n1
               nmaps(m2) = n2
               nprop1(n1) = m1
               nprop1(n2) = m2
               exit
            endif
         enddo
         if( i == 0 ) then
            j = j + 1
            write(6,*) '      Dedicatus545: Node on border of grids is not match'
            write(6,*) '                   ',n1, n2 ,'are not found in sub-grid'
            write(6,*) '                    System will be stop'
         endif
      enddo
      if( j /= 0 ) then
            write(6,*)
            write(6,*) '**** Hit the Enter-Key to stop ****'
            read(5,*)
            stop
      endif
  write(6,*) '    |'
!
!      allocate( nprops(nps) )
      nprops(:) = 0
      nd = 0
      do n = 1, nps
         if( nmaps(n) == -1 ) then
             nd = nd + 1
             nprops(nd) = n
         endif
      enddo
!
!deflag
!Make Node MAP
  write(6,*) ' Now Making! Node-map sub-grid => New Global grid'
      n1 = 0
      n2 = 0
      m = 0
      npn = 0
      do n = 1, np
         select case ( nptable(n) )
         case(0)
            m = m + 1
            nmap(n) = m
         case(1)
            if( n1 + 1 <= nd ) then
                n1 = n1 + 1
                m = m + 1
                nmaps(nprops(n1)) = m
            endif
         case(2)
            m = m + 1
            nmap(n) = m
            nmaps(nprop1(n)) = m
         end select
      enddo
  write(6,*) '    |'
      do n = n1+1, nd
         m = m + 1
         nmaps(nprops(n)) = np + n - n1
      enddo
      npn = m
!
!
!
!Make ELEMENT MAP
  write(6,*) ' Now Making! Element-map sub-grid => New Global grid'
      n = 0
      do m = 1, ne
         if( netable(m) == 1 ) then
            if( n+1 <= nes ) then
               n = n + 1
               mmaps(n) = m
            endif
         endif
      enddo
      nd = mmaps(n)
      do i = n+1, nes
         mmaps(i) = nd + i - n
      enddo
  write(6,*) '    |'
      nen = nes
      n = 0
      do m = 1, ne
         if( netable(m) == 0 ) then
            nen = nen + 1
            if( m <= nd ) then
               mmap(m) = m
            else
               n = n + 1
               mmap(m) = mmaps(nes) + n
            endif
         endif
      enddo
  write(6,*) '    -'
  write(6,*) '     '
!
!MAKE New Global Grid
  write(6,*) ' Now Creating! New Global grid'
  write(6,*) '     Coodinates'
      allocate(xydn(3,npn), nmn(nen,3))
      do n = 1, nps
         do i = 1, 3
            xydn(i,nmaps(n)) = xyds(i,n)
         enddo
      enddo
  write(6,*) '        |'
      do n = 1, np
         if( nptable(n) == 0 ) then
         do i =1,3
            xydn(i,nmap(n)) = xyd(i,n)
         enddo
         endif
      enddo
!
  write(6,*) '     Element Connectivity'
      do m = 1, nes
         do i = 1, 3
            nmn(mmaps(m),i) = nmaps(nms(m,i))
         enddo
      enddo
  write(6,*) '        |'
      do m = 1, ne
         if( netable(m) == 0 ) then
            do i = 1, 3
               nmn(mmap(m),i) = nmap(nm(m,i))
            enddo
         endif
      enddo
!
!
! OUTPUT GRID
!
 gridname = project_name(1:len_trim(project_name))//'.grd'
 write(6,*)
 write(6,*)
 write(6,*)
 write(6,*) 'START OUTPUT MERGED GRID'
 write(6,*) '  SELECT OUTPUT FILE NAME OPTION:'
 write(6,*) '     1. Use default name            : ( ',gridname(1:len_trim(gridname)),' )'
 write(6,*) '     2. Input the name              : '
 do
    read(5,*) n
         select case (n)
            case(1)
               gridname = gridname
               exit
            case(2)
               Write(6,*) '     Please enter the file name ;   '
               read(5,*) gridname
               exit
            case default
               write(6,*) '       You have to select 1,2 or 3'
         end select
 enddo

 write(6,*)
 write(6,*) 'Now Writing!'
      open(14,file=gridname,status='unknown',action='write')

!      write(14,*) agrid(1:len_trim(agrid))
      write(14,*) 'grid' !gridname(1:len_trim(gridname))
      write(14,*) nen, npn
      write(14,501) ( n, (xydn(i,n),i=1,3), n=1,npn)
      write(14,502) ( m, 3, (nmn(m,i),i=1,3), m=1,nen)
!
deallocate( xydn, nmn )
deallocate( xyd,  nm  )
deallocate( xyds, nms )
!
!
!
!
!
!
  write(6,*) '     Boundary Condition(Elevation)'
      k = max(nvdl_max,nvdls_max)
      allocate( nvllseg((nope+nopes)*3), nbvseg((nope+nopes)*3,k) )
      nvllseg(:) = 0
      nseg = 0
      do k = 1, nope
         j = 0
         do; j = j + 1 ; if( j > nvdll(k) ) exit
            if( nptable(nbdv(k,j)) == 1 ) cycle
                  nseg = nseg + 1
                  nvllseg(nseg) = nvllseg(nseg) + 1
                  nbvseg(nseg,nvllseg(nseg)) = nmap(nbdv(k,j))
                  do js = j, nvdll(k) - 1
                     if( nptable(nbdv(k,js+1)) == 1 ) then
                        j = js
                        exit
                     endif
                     nvllseg(nseg) = nvllseg(nseg) + 1
                     nbvseg(nseg,nvllseg(nseg)) = nmap(nbdv(k,js+1))
                  enddo
                  j = js
         enddo
      enddo
  write(6,*) '        |'
      do k = 1, nopes
         nseg = nseg + 1
         nvllseg(nseg) = nvllseg(nseg) + 1
         nbvseg(nseg,nvllseg(nseg)) = nmaps(nbdvs(k,1))
         do j = 1, nvdlls(k)-1
            n1= nbdvs(k,j)
            n2= nbdvs(k,j+1)
            nd = 0
            do n = 1, nbls
               m1 = nblncs(1,n)
               m2 = nblncs(2,n)
               if( ((m1==n1) .and. (m2==n2)) .or. &
                    ((m1==n2) .and. (m2==n1)) ) then
                   nvllseg(nseg) = nvllseg(nseg) + 1
                   nbvseg(nseg,nvllseg(nseg)) = nmaps(n2)
                   nd = 1
                   exit
               endif
            enddo
            if( nd == 0 ) then
               nseg = nseg + 1
               nvllseg(nseg) = nvllseg(nseg) + 1
               nbvseg(nseg,nvllseg(nseg)) = nmaps(n2)
            endif
         enddo
      enddo
!
  write(6,*) '        |'
      k = nvdl_max+nvdls_max
      allocate( nvdlln((nseg)), nbdvn((nseg),k))
      allocate( npseg(nseg), mpseg(nseg) )
      nopen = 0
      nvdlln(:) = 0
      npseg(:) = 0
      mpseg(:) = 0
      do while ( sum(npseg(:)) <  nseg )
      do n = 1, nseg ! Search the head
         if( npseg(n) /= 0 ) cycle
         nsta = nbvseg(n,1)
         nd = 0
         do m = 1, nseg
!taizo
            if( (n==m) .or. (npseg(m) /= 0) ) cycle
            if( nsta == nbvseg(m,nvllseg(m)) ) then
               nd = m
               exit
            endif
         enddo
         if( nd == 0 ) then
            nopen = nopen + 1
            do j = 1, nvllseg(n)
               nvdlln(nopen) = nvdlln(nopen) + 1
               nbdvn(nopen,nvdlln(nopen)) = nbvseg(n,j)
            enddo
            nend = nbvseg(n,nvllseg(n))
            npseg(n) = 1
            exit
         endif
      enddo
      do n1 = 1, nseg
         k = 0
         do n = 1, nseg
            if( npseg(n) /= 0 ) cycle
            if( nend /= nbvseg(n,1) ) cycle
               do j = 2, nvllseg(n)
                  nvdlln(nopen) = nvdlln(nopen) + 1
                  nbdvn(nopen,nvdlln(nopen)) = nbvseg(n,j)
               enddo
               nend = nbvseg(n,nvllseg(n))
               npseg(n) = 1
               k = 1
               exit
         enddo
         if( k == 0 ) exit
      enddo
      enddo
!
!
! TAIZO WORKING
      write(14,*) nopen
      write(14,*) sum(nvdlln(:))
      do k = 1, nopen
         write(14,*) nvdlln(k)
         do j = 1, nvdlln(k)
              write(14,*) nbdvn(k,j)
         enddo
      enddo
!
deallocate( nvdlln, nbdvn )
!
!
!
      k = int(max(nvel_max,nvels_max)*1.1)
  write(6,*) '     Boundary Condition(Normal Flow)', k, (nbou+nbous)*3
      deallocate( nvllseg, nbvseg )
      multip = 2
 666  continue
      multip = multip + 1
      allocate( nvllseg((nbou+nbous)*multip), nbvseg((nbou+nbous)*multip,k) )
      allocate( ibtypeseg((nbou+nbous)*multip), ibconnseg((nbou+nbous)*multip,k) )
      allocate( barseg(3,(nbou+nbous)*multip,k) )
      allocate( nplseg((nbou+nbous)*multip) )
      nvllseg(:) = 0
      nseg = 0
      nplseg(:) = 1
      ibconnseg(:,:) = 0
      do k = 1, nbou
         j = 0
            select case (ibtype(k))
               case(3, 13, 23) ;   ks = 2
               case(4, 24)     ;   ks = 3
               case default    ;   ks = 0
            end select
         do; j = j + 1 ; if( j > nvell(k) ) exit
            if( nptable(nbvv(k,j)) == 1 ) cycle
                  nseg = nseg + 1
                  if( nseg >= (nbou+nbous)*multip ) then
                    write(6,*) '     memory reallocation for BC', nbou+nbous, nseg
                    deallocate( nvllseg, nbvseg )
                    deallocate( ibtypeseg, ibconnseg )
                    deallocate( barseg, nplseg)
                    goto 666
                  endif
                  ibtypeseg(nseg) = ibtype(k)
                  nvllseg(nseg) = nvllseg(nseg) + 1
                  nbvseg(nseg,nvllseg(nseg)) = nmap(nbvv(k,j))
                  do i = 1, ks
                     barseg(i,nseg,nvllseg(nseg)) = bar(i,k,j)
                  enddo
                  if(ks==3) ibconnseg(nseg,nvllseg(nseg)) = nmap(ibconn(k,j))
                  if( j+1 <= nvell(k) ) then
                     if( nptable(nbvv(k,j+1)) /= 1 ) then
                         n1 = nbvv(k,j)
                         n2 = nbvv(k,j+1)
                         do m = 1, nbl
                            m1 = nblnc(1,m)
                            m2 = nblnc(2,m)
                            if( (n1==m2) .and. (n2==m1) ) then
                              nplseg(nseg) = -1
                              exit
                            endif
                         enddo
                     endif
                  endif

                  do js = j, nvell(k) - 1
                     if( nptable(nbvv(k,js+1)) == 1 ) then
                        j = js
                        exit
                     endif
                     nvllseg(nseg) = nvllseg(nseg) + 1
                     nbvseg(nseg,nvllseg(nseg)) = nmap(nbvv(k,js+1))
                     do i = 1, ks
                        barseg(i,nseg,nvllseg(nseg)) = bar(i,k,js+1)
                     enddo
                     if(ks==3) ibconnseg(nseg,nvllseg(nseg)) = nmap(ibconn(k,js+1))
                  enddo
                  j = js
         enddo
      enddo
      do k = 1, nbous
            select case (ibtypes(k))
               case(3, 13, 23) ;   ks = 2
               case(4, 24)     ;   ks = 3
               case default    ;   ks = 0
            end select
         nseg = nseg + 1
                  if( nseg >= (nbou+nbous)*multip ) then
                    write(6,*) '     memory reallocation for BC', nbou+nbous, nseg
                    deallocate( nvllseg, nbvseg )
                    deallocate( ibtypeseg, ibconnseg )
                    deallocate( barseg, nplseg)
                    goto 666
                  endif
         ibtypeseg(nseg) = ibtypes(k)
         nvllseg(nseg) = nvllseg(nseg) + 1
         nbvseg(nseg,nvllseg(nseg)) = nmaps(nbvvs(k,1))
         do i = 1, ks
            barseg(i,nseg,nvllseg(nseg)) = bars(i,k,1)
         enddo
         if(ks==3) ibconnseg(nseg,nvllseg(nseg)) = nmaps(ibconns(k,1))
         do j = 1, nvells(k)-1
            n1= nbvvs(k,j)
            n2= nbvvs(k,j+1)
            nd = 0
            do n = 1, nbls
               m1 = nblncs(1,n)
               m2 = nblncs(2,n)
               if( (( n1 == m1 ) .and. (n2 == m2)).or. &
                   (( n1 == m2 ) .and. (n2 == m1))     ) then
                   if(n1==m2) nplseg(nseg) = -1
                   nvllseg(nseg) = nvllseg(nseg) + 1
                   nbvseg(nseg,nvllseg(nseg)) = nmaps(n2)
                   do i = 1, ks
                      barseg(i,nseg,nvllseg(nseg)) = bars(i,k,j+1)
                   enddo
                   if(ks==3) ibconnseg(nseg,nvllseg(nseg)) = nmaps(ibconns(k,j+1))
                   nd = 1
                   exit
               endif
            enddo
            if( nd == 0 )then
                nseg = nseg + 1
                  if( nseg >= (nbou+nbous)*multip ) then
                    write(6,*) '     memory reallocation for BC', nbou+nbous, nseg
                    deallocate( nvllseg, nbvseg )
                    deallocate( ibtypeseg, ibconnseg )
                    deallocate( barseg, nplseg)
                    goto 666
                  endif
                ibtypeseg(nseg) = ibtypes(k)
                nvllseg(nseg) = nvllseg(nseg) + 1
                nbvseg(nseg,nvllseg(nseg)) = nmaps(n2)
                do i = 1, ks
                   barseg(i,nseg,nvllseg(nseg)) = bars(i,k,j+1)
                enddo
                if(ks==3) ibconnseg(nseg,nvllseg(nseg)) = nmaps(ibconns(k,j+1))
            endif
         enddo
      enddo
!
!
  write(6,*) '        |', nvel_max, nvels_max
      k = nvel_max+nvels_max
      write(6,*) k, nbou+nbous, nseg
      allocate( nvelln(nseg), ibtypen(nseg), nbvvn(nseg,k) )
      allocate( barn(3,nseg,k), ibconnn(nseg,k) )
      deallocate( npseg, mpseg )
      allocate( npseg(nseg), mpseg(nseg) )
      nboun = 0
      nvelln(:) = 0
      mpseg(:) = 0
      ibconnn(:,:) = 0
      do k = 1, nseg
         npseg(k) = 0
         if( nvllseg(k) == 1 ) npseg(k) = 1   ! Need? Maybe Yes!!!
      enddo
      do while ( sum(npseg(:)) <  nseg )
         do n = 1, nseg ! Search the head
            if( npseg(n) /= 0 ) cycle
            nsta = nbvseg(n,1)
            ista = ibconnseg(n,1)
            nd = 0
            do m = 1, nseg
               io = 0
               ii = 0
               if( (n==m) .or. (npseg(m) /= 0) .or. (ibtypeseg(n) /=ibtypeseg(m)) ) cycle
               if( (nsta == nbvseg(m,nvllseg(m))) .and. (ista==ibconnseg(m,nvllseg(m))) ) io = 1
               if( (ista == nbvseg(m,nvllseg(m))) .and. (nsta==ibconnseg(m,nvllseg(m))) ) ii = 1
               if( io+ii == 0 ) cycle
                  if( io+ii == 2 ) then
                        write(6,*) '      Basis104: Fatal Error'
                        write(6,*) '                System will be stop'
                        write(6,*)
                        write(6,*) '**** Hit the Enter-Key to stop ****'
                        read(5,*)
                        stop
                  endif
                  nd = m
                  mpseg(n) = m
                  n1 = n
                  do                           ! Anti-Infinite Loop measure
                     n1 = mpseg(n1)
                     if( n1 == 0 ) then
                        exit
                     else
                        if( n1 == n ) then
                           nd = 0
                           exit
                        endif
                     endif
                  enddo
                  exit
            enddo
            if( nd == 0 ) then
               nboun = nboun + 1
               ibtypen(nboun) = ibtypeseg(n)
               select case (ibtypeseg(n))
                  case(3, 13, 23) ;   ks = 2
                  case(4, 24)     ;   ks = 3
                  case default    ;   ks = 0
               end select
               do j = 1, nvllseg(n)
                  nvelln(nboun) = nvelln(nboun) + 1
                       nbvvn(nboun,nvelln(nboun)) = nbvseg(n,j)
                       if(ks==3) ibconnn(nboun,nvelln(nboun)) = ibconnseg(n,j)
                  do i = 1, ks
                     barn(i,nboun,nvelln(nboun)) = barseg(i,n,j)
                  enddo
               enddo
               nend = nbvvn(nboun,nvelln(nboun))
               iend = ibconnn(nboun,nvelln(nboun))
               ip = ibtypeseg(n)
               npseg(n) = 1
               n2 = n
               exit
            endif
         enddo
         do n1 = 1, nseg
            k = 0
            do n = 1, nseg
               if( (npseg(n) /= 0) .or. (ip/=ibtypeseg(n)) ) cycle
               io = 0
               ii = 0
               if( (nend == nbvseg(n,1)) .and. (iend==ibconnseg(n,1)) ) io = 1
               if( (iend == nbvseg(n,1)) .and. (nend==ibconnseg(n,1)) ) ii = 1
               if( io+ii == 0 ) cycle
                  if( io+ii == 2 ) then
                        write(6,*) '      Basis105: Fatal Error'
                        write(6,*) '                System will be stop'
                        write(6,*)
                        write(6,*) '**** Hit the Enter-Key to stop ****'
                        read(5,*)
                        stop
                  endif
!                  write(69,*) 'merge', n2, n, ip, nend, iend
                  do j = 2, nvllseg(n)
                     nvelln(nboun) = nvelln(nboun) + 1
                     select case(io)
                        case(1)
                           nbvvn(nboun,nvelln(nboun)) = nbvseg(n,j)
                           if(ks==3) ibconnn(nboun,nvelln(nboun)) = ibconnseg(n,j)
                        case(0)
                           nbvvn(nboun,nvelln(nboun)) = ibconnseg(n,j)
                           if(ks==3) ibconnn(nboun,nvelln(nboun)) = nbvseg(n,j)
                     end select
                     do i = 1, ks
                        barn(i,nboun,nvelln(nboun)) = barseg(i,n,j)
                     enddo
                  enddo
                  nend = nbvvn(nboun,nvelln(nboun))
                  if(ks==3) iend = ibconnn(nboun,nvelln(nboun))
                  npseg(n) = 1
                  k = 1
                  exit
            enddo
            if( k == 0 ) exit
         enddo
      enddo
!
! OUTPUT
      write(14,*) nboun
      n1 =  0
      do n = 1, nboun
            n1 = n1 + nvelln(n)
         if( (ibtypen(n) == 4) .or. (ibtypen(n) == 24)) then
            n1 = n1 + nvelln(n)
         endif
      enddo
      write(14,*) n1
      do k = 1, nboun
         write(14,*) nvelln(k), ibtypen(k)
         select case (ibtypen(k))
            case(3,13,23)
               do j = 1, nvelln(k)
                  write(14,510) nbvvn(k,j), (barn(i,k,j), i=1,2)
               enddo
            case(4,24)
               do j = 1, nvelln(k)
                  if((nbvvn(k,j) == 0 ).or.(ibconnn(k,j)==0)) then
                     write(6,*) k,j,nbvvn(k,j), ibconnn(k,j)
                  endif
                  write(14,511) nbvvn(k,j), ibconnn(k,j), (barn(i,k,j), i=1,3)
               enddo
            case default
               do j = 1, nvelln(k)
                  write(14,'(i8)') nbvvn(k,j)
               enddo
         end select
      enddo
 501  format(i10,2f15.6,e16.8)
 502  format(5i10)
 510  format( i10, 2f15.6)
 511  format(2i10, 3f15.6)
!
      deallocate( nvelln, ibtypen, nbvvn )
      deallocate( barn, ibconnn )
      deallocate( nvllseg, nbvseg )
      deallocate( ibtypeseg, ibconnseg )
      deallocate( barseg )
      deallocate( nplseg )
!
  write(6,*) '     Finish!! GRID'
!
  if(trim(fort13).ne.'null') then
      call merge_nodals( npn, nps, np, nmaps, nmap, nptable, project_name, fort13,&
                         nodemax, nsequencer )
  endif
!
!
!
      write(6,*) 'Finish, please hit Enter key'
      read(5,*)
!
   end subroutine  merge_
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine  mkeline( np, ne, nm, netable, nbn_max, nbl, nblnc )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: np, ne, nm(ne,3), nbn_max, netable(ne)
      integer, intent(out) :: nbl, nblnc(2,nbn_max)
      integer, allocatable :: nean(:,:), numean(:)
      integer :: nean_max=10
      integer :: n, m, i, j, n1, n2, icheck, m1, m2, i1, i2
!
      allocate ( numean(np) )
!
! Search Elements around a node
  icheck = 0
  666 continue
      allocate ( nean(np,nean_max) )
      numean(:) = 0
      do m = 1, ne
         do i = 1, 3
            n = nm(m,i)
            numean(n) = numean(n) + 1
            if( numean(n) > nean_max ) then
               icheck = icheck + 1
               if(icheck==1) write(6,*) '      Salvere000: System require much more nean_max'
               nean_max = nean_max + 5
               write(6,*) '                  System try to re-calculate with nean_max=', nean_max
               deallocate( nean )
               go to 666
            endif
            nean(n,numean(n)) = m
         enddo
      enddo
      if( icheck/=0 ) then
         write(6,*) '                  Success!'
         write(6,*)
         write(6,*)
      endif
!
! Search the border of global grid and sub-grid
      nbl = 0
      do m = 1, ne
         if( netable(m) == 1 ) then
            do i = 1, 3
               call eline(i, n1, n2, nm, m, ne)
               icheck = 0
               do i1 = 1, numean(n1)
                  m1 = nean(n1,i1)
                  do i2 = 1, numean(n2)
                     m2 = nean(n2,i2)
                     if( m1 == m2 ) then
                        if( m1 /= m ) then
                           if( netable(m1) == 1 ) then
                              icheck = icheck + 1
                              exit
                           endif
                        endif
                     endif
                  enddo
                  if( icheck /= 0 ) exit
               enddo
                  if( icheck == 0 ) then
                      nbl = nbl + 1
                      nblnc(1,nbl) = n1
                      nblnc(2,nbl) = n2
                  endif
            enddo
         endif
      enddo
!
!
   end subroutine mkeline

!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine eline(i, n1, n2, nm, m, ne)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: ne, nm(ne,3), m, i
      integer, intent(out) :: n1, n2
!
            select case(i)
                   case(3)
                      n1 = nm(m,1)
                      n2 = nm(m,2)
                   case(1)
                      n1 = nm(m,2)
                      n2 = nm(m,3)
                   case(2)
                      n1 = nm(m,3)
                      n2 = nm(m,1)
            end select
!
   end subroutine eline
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine  pull_out_nodals ( fort14, project_name, fort13 )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!$    use omp_lib
      implicit none
      character(120), intent(in) :: fort14, project_name, fort13
!
      character(120) :: agrid, gridname
! Old Grobal grid
      integer :: ne, np
      double precision, allocatable  :: xyd(:,:)
      integer,          allocatable  :: nm(:,:)
! Sub-grid grid
      integer :: nes, nps
      double precision, allocatable  :: xyds(:,:)
      integer,          allocatable  :: nms(:,:)
!
! Global fort.13
      integer :: nattr, max_valpernode, max_numnodesnotdefault
      integer,          allocatable :: valuespernode(:), numnodesnotdefaultval(:), nattrval(:,:)
      double precision, allocatable :: defaultattrval(:,:), attrval(:,:,:)
!
      character*160, allocatable :: attrname(:)
!
      integer :: npos, neos, nodemax
      integer, allocatable :: nptable(:), nemap(:), netable(:), list_ehasn(:), nsequencer(:)
!
      integer :: i, j, k, je, nhy, n, m
      double precision :: xc, yc
!
      integer, parameter :: idiv=10000
      integer :: ne_in_piece_sum
      integer, allocatable :: ne_piece(:,:), ne_piece_add(:,:), ne_piece_list(:)
      double precision :: xmin(2), xmax(2)
!
!$    integer :: mythread
!$    double precision :: time0
!
!
!Open(14), for dynamic allocation ( Edited Sub-grid )
      gridname = project_name(1:len_trim(project_name))//'-sub.grd'
      write(6,*) 'START! READING of Edited Sub-grid data:  ', gridname(1:len_trim(gridname))
      write(6,*) '  + '
      open(14,file=gridname,status='old',action='read')
         read(14,*)
         read(14,*) nes, nps
         allocate( xyds(3,nps), nms(nes,3), nptable(nps) )
         do i = 1, nps
            read(14,*) nptable(i), (xyds(j,i), j=1,3)
         enddo
         nodemax = maxval(nptable(:))
         allocate( nsequencer(nodemax))
         nsequencer(:) = 0
         do i = 1, nps
            nsequencer(nptable(i)) = i
         enddo
         write(6,*) '  + '
         do k = 1, nes
            read(14,*) je, nhy, ( nms(k,j), j = 1, 3 )
            do j = 1, 3
!               if( nms(k,j) <= 0 ) write(6,*) k,j, nm(k,j)
               nms(k,j) = nsequencer(nms(k,j))
            enddo
         enddo
      write(6,*) 'FINISH!'
      write(6,*)
      close(14)
      deallocate( nsequencer, nptable )
!
!Open(14), for dynamic allocation
      write(6,*) 'START! READING of Global data:  ', fort14(1:len_trim(fort14))
      open(14,file=fort14,status='old',action='read')
         read(14,*)
         read(14,*) ne, np
         allocate( xyd(3,np), nm(ne,3), nptable(np) )
         do i = 1, np
            read(14,*) nptable(i), (xyd(j,i), j=1,3)
         enddo
         nodemax = maxval(nptable(:))
         allocate( nsequencer(nodemax))
         nsequencer(:) = 0
         do i = 1, np
            nsequencer(nptable(i)) = i
         enddo
         write(6,*) '  + '
         do k = 1, ne
            read(14,*) je, nhy, ( nm(k,j), j = 1, 3 )
            do j = 1, 3
               nm(k,j) = nsequencer(nm(k,j))
            enddo
         enddo
      close(14)
!
!Read element property of global grid (Contained in sub:1, not:0)
      allocate( netable(ne) )
      call readetab(ne,netable)
! Make Old sub-grid map
      neos = sum(netable(:))
      allocate(nemap(neos))
      nptable(:) = 0
      neos = 0
      do m = 1, ne
         if( netable(m) == 1 ) then
            neos = neos + 1
            nemap(neos) = m
            netable(m) = neos
            do i = 1, 3
               nptable(nm(m,i)) = 1
            enddo
         endif
      enddo
      npos = 0
      do n = 1, np
         if( nptable(n) == 1 ) then
            npos = npos + 1
            nptable(n) = npos
         endif
      enddo
!
!$    time0 = - omp_get_wtime()
      allocate( ne_piece(0:idiv+1,0:idiv+1), ne_piece_add(idiv,idiv) )
      call count_list(np, ne, nm, xyd, neos, nemap, xmax, xmin,idiv,ne_piece_add, ne_piece, ne_in_piece_sum)
      write(6,*)ne_piece(1,1)
      allocate( ne_piece_list(ne_in_piece_sum) )
      call mkelocation( np, ne, nm, xyd, neos, nemap, &
                        idiv, xmax, xmin, ne_in_piece_sum, ne_piece_add, ne_piece, ne_piece_list )
      write(6,*)ne_piece(1,1)
!
!
      write(6,*) '     START! SEARCHING the Element for interpolation '
!$    write(6,*) '        USING ', omp_get_max_threads(),'COREs'
!
      allocate( list_ehasn(nps) )
!$OMP PARALLEL DO DEFAULT(SHARED), PRIVATE(xc,yc,n,m)
      do n = 1, nps
         xc = xyds(1,n); yc = xyds(2,n)
         m = 0
         call search_inelm(ne,np,xyd,nm, neos,nemap, m, xc,yc, &
                        idiv, xmax, xmin, ne_in_piece_sum, ne_piece_add, ne_piece, ne_piece_list )
!
! Old Search Area Bombing!!
!         call search_inelm_ab(ne,np,xyd,nm, neos,nemap, m, xc,yc )
!
         list_ehasn(n) = m
!         write(6,*) nps, n, m
      enddo
!$OMP END PARALLEL DO
!$    time0 = time0 + omp_get_wtime()
!$    write(6,*) '        Searching time:',time0
      write(6,*) '     Finish!--Searching '
!
      open(13,file=fort13,status='old',action='read')
         call read13_alloc( 13, nattr, max_valpernode )
      close(13)
!
!      write(6,*) 'START! READING of Global Nodal attributes data :  ', fort13(1:len_trim(fort13))
      open(13,file=fort13,status='old',action='read')
         gridname = project_name(1:len_trim(project_name))//'-sub13'
         call pullout13( ne, np, xyd, nm, nes, nps, xyds,  nms,    gridname,    &
                         netable, nptable, neos, nemap, npos,  list_ehasn,      &
                         nattr, max_valpernode, nodemax, nsequencer)
      close(13)
!
   end subroutine pull_out_nodals
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine read13_alloc( iunit, nattr, max_valpernode )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: iunit
      integer, intent(out) :: nattr, max_valpernode
      integer, allocatable :: idummy1(:)
      integer :: i, j, k, n
      double precision :: dummy
!
      read(iunit,*)       !Agrid
      read(iunit,*)       !Num of nodes = np
      read(iunit,*) nattr !NAttr
      allocate( idummy1(nattr) )
      do i = 1, nattr
         read(iunit,*)    !AttrName
         read(iunit,*)    !Units
         read(iunit,*) idummy1(i)
         read(iunit,*) ( dummy, k = 1,idummy1(i) )
      enddo
      max_valpernode = maxval(idummy1(:))
      deallocate ( idummy1 )
   end subroutine read13_alloc

!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine pullout13( ne, np, xyd, nm, nes, nps, xyds, nms,  gridname,       &
                         netable, nptable, neos, nemap, npos,  list_ehasn,      &
                         nattr, max_valpernode, nodemax, nsequencer)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: ne, np, nm(ne,3), nodemax, nsequencer(nodemax)
      double precision, intent(in) :: xyd(3,np)
      integer, intent(in) :: nps, nes, nms(nes,3)
      double precision, intent(in) :: xyds(3,nps)
      integer, intent(in) :: netable(ne), nptable(np), neos, nemap(neos), npos, list_ehasn(nps)
      integer, intent(in) :: nattr, max_valpernode
      character(120),intent(iN) :: gridname

      integer :: numnodesnotdefaultval
      integer, allocatable :: valuespernode(:)
      integer, allocatable :: npropg(:)
      double precision, allocatable :: defaultattrval(:,:), valp(:), &
                                       attrval(:,:)
      character*160, allocatable :: attrname(:), units(:)
      character*160              :: outname
      character*2 :: num1, num2
!
      double precision, allocatable:: values(:)
!
      integer :: i, j, k, n, m, nen
      double precision :: dummy, xc, yc, p, sl(3)
!
!
      double precision, parameter :: outflag=-9999999.0d0
!
      allocate( npropg(np) )
      read(13,*)         !Agrid
!      write(23,*)        !Agrid
      read(13,*)  n      !Num of nodes = np
!      write(23,*) nps    !Num of nodes = np
      if( n /= np ) then
          write(6,*) ' Do you use correct fort.13? (Total number of nodes are not equal)'
          write(6,*) n, np
          read(5,*)
          stop
      endif
      read(13,*)         !NAttr
!      write(23,*) nattr  !NAttr
!
      allocate(attrname(nattr), units(nattr))
      allocate(valuespernode(nattr), defaultattrval(nattr,max_valpernode))
!
      do i = 1, nattr
         read (13,*) attrname(i)   !AttrName
         read (13,*) units(i)   !Units
         read (13,*) valuespernode(i)
         read (13,*) ( defaultattrval(i,k), k = 1,valuespernode(i) )
      enddo
!
      allocate( attrval(npos,max_valpernode) )
      allocate( valp(max_valpernode) )
      allocate( values(nps) )
      do i = 1, nattr
         read(13,*)    !AttrName
!         write(23,*) attrname(i)(1:len_trim(attrname(i)))   !AttrName
         write(6,*) '     ', attrname(i)(1:len_trim(attrname(i)))   !AttrName
         read(13,*) numnodesnotdefaultval
         do j = 1, valuespernode(i)
            do k = 1, npos
               attrval(k,j) = defaultattrval(i,j)
            enddo
         enddo
         npropg(:) = 0
         do j = 1, numnodesnotdefaultval
            read(13,*) n, ( valp(k), k = 1,valuespernode(i) )
            n = nsequencer(n)
            npropg(n) = 1
            if( nptable(n) == 0 ) cycle
            do k = 1, valuespernode(i)
               attrval(nptable(n),k) = valp(k)
            enddo
         enddo
!!!!!!
         do k = 1, valuespernode(i)
            write(num1,'(i2.2)') i
            write(num2,'(i2.2)') k
            outname = gridname(1:len_trim(gridname)) // '-' // num1 // '-' // num2 //'.14'
            open(100+i,file=outname,status='replace',action='write')
            write(6,*) '     ', k,outname(1:len_trim(outname))
            do n = 1, nps
               values(n) = defaultattrval(i,k)
               xc = xyds(1,n); yc = xyds(2,n)
               m = list_ehasn(n)
               if( m == 0 ) then
                   values(n) = outflag
               else
                      call mkareacood( ne, np, xyd, nm, m, xc, yc, sl )
                      p = 0.0d0
                      do j = 1, 3
                         p = p + attrval(nptable(nm(m,j)),k) * sl(j)
                      enddo
                      values(n) = p
!                  endif
               endif
            enddo
            write(100+i,*) 'grid '// attrname(i)(1:len_trim(attrname(i)))
            write(100+i,101) nes, nps
            write(100+i,102) ( n, xyds(1,n), xyds(2,n), values(n), n=1,nps )
            write(100+i,101) ( n, 3, (nms(n,j), j=1,3), n=1,nes )
            close(100+i)
         enddo
      enddo
  100 format(a90,2i5)
  101 format(5i10)
  102 format(i10,2f18.10,e18.10)
!
   end subroutine pullout13
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine count_list(np, ne, nm, xyd, neos, nemap, xmax, xmin, idiv,ne_piece_add,ne_piece, ne_piece_sum)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer,intent(in) :: ne, np, nm(ne,3), idiv, neos, nemap(neos)
      double precision, intent(in) :: xyd(3,np)
!
      double precision, intent(out) :: xmax(2), xmin(2)
      integer, intent(out) :: ne_piece_sum, ne_piece(0:idiv+1,0:idiv+1), ne_piece_add(idiv,idiv)
!
      integer :: i, j, im , m, n, ix(2,3), istart
      double precision :: dx(2)
!
      do i = 1, 2
         xmin(i) = xyd(i,nm(nemap(1),1))
         xmax(i) = xyd(i,nm(nemap(1),1))
      enddo
      do im = 1, neos
         m = nemap(im)
         do i = 1, 2
            do j = 1, 3
               xmin(i) = dmin1(xmin(i), xyd(i,nm(m,j)))
               xmax(i) = dmax1(xmax(i), xyd(i,nm(m,j)))
            enddo
         enddo
      enddo
!
      write(6,*) xmin(1), xmin(2)
      write(6,*) xmax(1), xmax(2)
!
      do i = 1, 2
         xmax(i) = xmax(i) + 1.d0
      enddo

      dx(:) = ( xmax(:)-xmin(:) ) / idiv
      ne_piece(:,:) = 0
      do im = 1, neos
         m = nemap(im)
         do j = 1, 3
            do i = 1, 2
               n = nm(m,j)
               ix(i,j) = int( (xyd(i,n)-xmin(i)) / dx(i) ) + 1
            enddo
         enddo
         do i = MINVAL(ix(1,1:3)), MAXVAL(IX(1,1:3))
           do j = MINVAL(ix(2,1:3)), MAXVAL(IX(2,1:3))
              ne_piece(i,j) = ne_piece(i,j) + 1
           enddo
         enddo
      enddo
      istart = 0
      do i = 1, idiv
         do j = 1, idiv
            ne_piece_add(i,j) = istart
            istart = istart + ne_piece(i,j)
         enddo
      enddo
      ne_piece_sum = sum( ne_piece(:,:) )
      write(6,*) ne_piece_sum, neos
   end subroutine count_list
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine mkelocation( np, ne, nm, xyd, neos, nemap, &
                           idiv, xmax, xmin, ne_in_piece_sum, ne_piece_add, ne_piece, ne_piece_list )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: np, ne, nm(ne,3), neos, nemap(neos), idiv, ne_in_piece_sum
      double precision, intent(in) :: xyd(3,np), xmax(2), xmin(2)
      integer, intent(in)  :: ne_piece_add(idiv,idiv)
      integer, intent(out) :: ne_piece(0:idiv+1,0:idiv+1), ne_piece_list(ne_in_piece_sum)
!
      integer :: im, n, m, i, j, ix(2,3)
      double precision :: dx(2)
!
      dx(:) = ( xmax(:)-xmin(:) ) / idiv
!
      ne_piece(:,:) = 0
      do im = 1, neos
         m = nemap(im)
         do j = 1, 3
            do i = 1, 2
               n = nm(m,j)
               ix(i,j) = int( (xyd(i,n)-xmin(i)) / dx(i) ) + 1
            enddo
         enddo
         do i = MINVAL(ix(1,1:3)), MAXVAL(ix(1,1:3))
           do j = MINVAL(ix(2,1:3)), MAXVAL(ix(2,1:3))
             ne_piece(i,j) = ne_piece(i,j) + 1
             ne_piece_list( ne_piece(i,j)+ne_piece_add(i,j) ) = m
           enddo
         enddo
      enddo
   end subroutine mkelocation

!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine search_inelm_ab(ne,np,xyd,nm, ilist,list_gelem, m, xc, yc)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer,intent(in) :: ne, np, nm(ne,3), ilist, list_gelem(ilist)
      double precision, intent(in) :: xyd(3,np)
      integer, intent(inout) :: m
      double precision, intent(inout) :: xc, yc
!
      integer :: i, mi
      double precision :: x1, y1, x2, y2, x3, y3, a1, a2, a3, a0
      double precision, parameter :: eps = 1.0d-12
!
      do i = 1, ilist
         mi = list_gelem(i)
         x1 = xyd(1,nm(mi,1))
         x2 = xyd(1,nm(mi,2))
         x3 = xyd(1,nm(mi,3))
         y1 = xyd(2,nm(mi,1))
         y2 = xyd(2,nm(mi,2))
         y3 = xyd(2,nm(mi,3))
         a0 =  (x1 - x2) * (y1 - y3) - (x1 - x3) * (y1 - y2)
         a1 = ((xc - x2) * (yc - y3) - (xc - x3) * (yc - y2) ) / a0
         a2 = ((x1 - xc) * (y1 - y3) - (x1 - x3) * (y1 - yc) ) / a0
         a3 = ((x1 - x2) * (y1 - yc) - (x1 - xc) * (y1 - y2) ) / a0
         if( (a1 >= -eps ) .and. ( a2 >= -eps ) .and. ( a3 >= -eps ) ) then
            m = mi
            exit
         endif
      enddo
   end subroutine search_inelm_ab
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine search_inelm(ne,np,xyd,nm, ilist,list_gelem, m, xc, yc,                &
                        idiv, xmax, xmin, ne_in_piece_sum, ne_piece_add, ne_piece, ne_piece_list )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer,intent(in) :: ne, np, nm(ne,3), ilist, list_gelem(ilist)
      double precision, intent(in) :: xyd(3,np)
      integer, intent(inout) :: m
      double precision, intent(inout) :: xc, yc
!
      integer, intent(in) :: idiv, ne_in_piece_sum, ne_piece_add(idiv,idiv),&
                             ne_piece(0:idiv+1,0:idiv+1), ne_piece_list(ne_in_piece_sum)
      double precision, intent(in) :: xmax(2), xmin(2)
!
      integer :: i, mi, ix, iy, iadd
      double precision :: x1, y1, x2, y2, x3, y3, a1, a2, a3, a0, dx, dy
      double precision, parameter :: eps = 1.0d-08
!
      dx = ( xmax(1)-xmin(1) ) / idiv
      dy = ( xmax(2)-xmin(2) ) / idiv
      ix = int( (xc -xmin(1)) / dx ) + 1
      iy = int( (yc -xmin(2)) / dy ) + 1
      ix = max(0,ix); ix = MIN(idiv+1,ix)
      iy = max(0,iy); iy = MIN(idiv+1,iy)
!
      do i = 1, ne_piece(ix,iy)
         mi = ne_piece_list(i+ne_piece_add(ix,iy))
         x1 = xyd(1,nm(mi,1))
         x2 = xyd(1,nm(mi,2))
         x3 = xyd(1,nm(mi,3))
         y1 = xyd(2,nm(mi,1))
         y2 = xyd(2,nm(mi,2))
         y3 = xyd(2,nm(mi,3))
         a0 =  (x1 - x2) * (y1 - y3) - (x1 - x3) * (y1 - y2)
         a1 = ((xc - x2) * (yc - y3) - (xc - x3) * (yc - y2) ) / a0
         a2 = ((x1 - xc) * (y1 - y3) - (x1 - x3) * (y1 - yc) ) / a0
         a3 = ((x1 - x2) * (y1 - yc) - (x1 - xc) * (y1 - y2) ) / a0
         if( (a1 >= -eps ) .and. ( a2 >= -eps ) .and. ( a3 >= -eps ) ) then
            m = mi
            exit
         endif
      enddo
!
!      if ( m == 0 ) then
!         do i = 1, ilist
!            mi = list_gelem(i)
!            x1 = xyd(1,nm(mi,1))
!            x2 = xyd(1,nm(mi,2))
!            x3 = xyd(1,nm(mi,3))
!            y1 = xyd(2,nm(mi,1))
!            y2 = xyd(2,nm(mi,2))
!            y3 = xyd(2,nm(mi,3))
!            a0 =  (x1 - x2) * (y1 - y3) - (x1 - x3) * (y1 - y2)
!            a1 = ((xc - x2) * (yc - y3) - (xc - x3) * (yc - y2) ) / a0
!            a2 = ((x1 - xc) * (y1 - y3) - (x1 - x3) * (y1 - yc) ) / a0
!            a3 = ((x1 - x2) * (y1 - yc) - (x1 - xc) * (y1 - y2) ) / a0
!            if( (a1 >= -eps ) .and. ( a2 >= -eps ) .and. ( a3 >= -eps ) ) then
!               m = mi
!               exit
!            endif
!         enddo
!      endif
   end subroutine search_inelm
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine mkareacood( ne, np, xyd, nm, m, xc, yc, sl )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: ne, np, nm(ne,3), m
      double precision, intent(in) :: xyd(3,np), xc, yc
      double precision, intent(out) :: sl(3)
!
      double precision :: x1, y1, x2, y2, x3, y3, a1, a2, a3, a0
!
       sl(:) = 0.0d0
       x1 = xyd(1,nm(m,1))
       x2 = xyd(1,nm(m,2))
       x3 = xyd(1,nm(m,3))
       y1 = xyd(2,nm(m,1))
       y2 = xyd(2,nm(m,2))
       y3 = xyd(2,nm(m,3))
       a0 =  (x1 - x2) * (y1 - y3) - (x1 - x3) * (y1 - y2)
       a1 = ((xc - x2) * (yc - y3) - (xc - x3) * (yc - y2) ) / a0
       a2 = ((x1 - xc) * (y1 - y3) - (x1 - x3) * (y1 - yc) ) / a0
       a3 = ((x1 - x2) * (y1 - yc) - (x1 - xc) * (y1 - y2) ) / a0
       sl(1) = a1
       sl(2) = a2
       sl(3) = a3
!
   end subroutine mkareacood
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine merge_nodals( npn, nps, np, nmaps, nmap, nptable, project_name, fort13,&
                         nodemax, nsequencer )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: npn, nps, np, nmaps(nps), nmap(np), nptable(np)
      integer, intent(in) :: nodemax, nsequencer(nodemax)
      character(120), intent(in) :: project_name, fort13
      character(120) :: gridname, sub13
      character*2 :: num1, num2
!
! Global fort.13
      integer :: nattr, max_valpernode, numnodesnotdefaultval
      integer,          allocatable :: valuespernode(:)
      double precision, allocatable :: defaultattrval(:,:)
      character*160, allocatable :: attrname(:), units(:)
!
! Sub-fort.13
      integer :: nattrs, max_valpernodes, numnodesnotdefaultvals
      integer,          allocatable :: valuespernodes(:)
      double precision, allocatable :: defaultattrvals(:,:)
      character*160, allocatable :: attrnames(:), unitss(:)
!
      integer, allocatable :: nwork(:)
      double precision, allocatable :: valp1(:), valp2(:)
!
      integer :: i, j, k, id, n, m, icheck
      double precision :: dum
      double precision, parameter :: eps=1.0d-06
!
      integer :: isw1, isw2, icount1, icount2, n1, n2

!
      double precision, parameter :: outflag=-9999999.0d0
!
! Global 13
      write(6,*) 'START! READING of Global Nodal attributes data :  ', fort13(1:len_trim(fort13))
         open(13,file=fort13,status='old',action='read')
         call read13_alloc( 13, nattr, max_valpernode )
      close(13)
      write(6,*) 'FINISH! fort13 alloc'
!
      gridname = project_name(1:len_trim(project_name))//'-sub.13'
!      open(23,file=gridname,status='old',action='read')
!         call read13_alloc( 23, nattrs, max_valpernodes )
!      close(23)
!      if( nattrs /= nattr ) then
!        write(6,*) 'STOP!!!! Tell Seizo 1 !!'
!        stop
!      endif
!
      allocate ( attrname(nattr), units(nattr) )
      allocate ( valuespernode(nattr) )
      allocate ( defaultattrval(nattr,max_valpernode) )
!
      open(13,file=fort13,status='old',action='read')
!      open(23,file=gridname,status='old',action='read')
      gridname = project_name(1:len_trim(project_name))//'.13'
      write(6,*)
      write(6,*)
      write(6,*)
      write(6,*) 'START OUTPUT MERGED NODAL ATTRIBUTES'
      write(6,*) '  SELECT OUTPUT FILE NAME OPTION:'
      write(6,*) '     1. Use default name            : ( ',gridname(1:len_trim(gridname)),' )'
      write(6,*) '     2. Input the name              : '
      do
         read(5,*) n
              select case (n)
                 case(1)
                    gridname = gridname
                    exit
                 case(2)
                    Write(6,*) '     Please enter the file name ;   '
                    read(5,*) gridname
                    exit
                 case default
                    write(6,*) '       You have to select 1 or 2'
              end select
      enddo
      open(33,file=gridname,status='replace',action='write')
!
! Default part
      read (13,*)          !Agrid
      read (13,*)  n
          if( n /= np ) then
             write(6,*) ' Do you use correct gbl-fort.13?', n, np
             stop
          endif
      read(13,*)   !NATTR
      write (33,*) 'fort13' !gridname(1:len_trim(gridname))           !Agrid
      write(33,*)  npn
      write(33,*)  nattr
!
      do i = 1, nattr
         read (13,*) attrname(i)   !AttrName
         write(33,*) attrname(i)(1:len_trim(attrname(i)))   !AttrName
         read (13,*) units(i)   !Units
         write(33,*) units(i)(1:len_trim(units(i)))   !AttrName
         read (13,*) valuespernode(i)
         write(33,'(i10)') valuespernode(i)
         read (13,*) ( defaultattrval(i,k), k = 1,valuespernode(i) )
         write(33,'(200f15.6)') ( defaultattrval(i,k), k = 1,valuespernode(i) )
      enddo
!)
      allocate( nwork(npn), valp1(max_valpernode),valp2(max_valpernode) )
      write(6,*) npn, max_valpernode
!      read(5,*)
! Not Default
      do i = 1, nattr
         read (13,*) attrname(i)   !AttrName
         write(33,*) attrname(i)(1:len_trim(attrname(i)))   !AttrName
         write (6,*) attrname(i)(1:len_trim(attrname(i)))   !AttrName
         read (13,*) numnodesnotdefaultval
         nwork(:) = 0
         do j = 1, valuespernode(i)
            write(num1,'(i2.2)') i
            write(num2,'(i2.2)') j
            sub13 = project_name(1:len_trim(project_name))//'-sub13'
            sub13 = sub13(1:len_trim(sub13)) // '-' // num1 // '-' // num2 //'.14'
            write(6,*) sub13(1:len_trim(sub13))
            open(unit=60+j,file=sub13,status='old',action='read')
            read(60+j,*)
            read(60+j,*) m, n
            if( n /= nps ) then
                write(6,*) ' Do you use correct gbl-fort.13?', n, nps
            endif
         enddo
         icount1=0
         icount2=0
         isw1 = 1
         isw2 = 1
         do        !??? = 1, ????
            if( isw1 == 1 ) then
               do
                  icount1 = icount1 + 1
                  if( icount1 > numnodesnotdefaultval ) then
                     icount1 = icount1 - 1
                     n1 = npn + 1
                     exit
                  endif
!
                  read(13,*) n, (valp1(k), k=1, valuespernode(i))
                  n = nsequencer(n)
                  if( nmap(n) /= 0 ) then
                      n1 = nmap(n)
                      exit
                  endif
               enddo
            endif
!
            if( isw2 == 1 ) then
               do
               id = 0
                  icount2 = icount2 + 1
                  if( icount2 > nps ) then
                     icount2 = icount2 - 1
                     n2 = npn + 1
                     exit
                  endif
                  do k = 1, valuespernode(i)
                     read(60+k,*) n2, dum, dum, valp2(k)
                        if( valp2(k) == outflag ) then
                            write(6,*) n2
                            write(6,*) '      Aura293     : Sub-fort.13 has non-eddited nodal values.'
                            write(6,*) '                    System will be stop'
                            write(6,*)
                            write(6,*) '**** Hit the Enter-Key to stop ****'
                            read(5,*)
                            stop
                        endif
                     if( dabs( valp2(k)-defaultattrval(i,k) ) > eps ) then
                         id = 1
                     endif
                  enddo
                  m = n2
                  n2 = nmaps(n2)
                  if( id == 1 ) exit
               enddo
            endif
!
            if( (n1 > npn) .and. (n2 > npn) ) exit
!
            if( n1 < n2 ) then
               isw1 = 1
               isw2 = 0
               if( nwork(n1) == 0 ) then
                   nwork(n1) = 1
               endif
            else
               if( n1 > n2 ) then
                  isw1 = 0
                  isw2 = 1
                  if( nwork(n2) == 0 ) then
                      nwork(n2) = 1
                  endif
               else
                  isw1 = 1
                  isw2 = 1
                  if( nwork(n1) == 0 ) then
                     nwork(n1) = 1
                  endif
               endif
            endif
         enddo
!
         do j = 1, valuespernode(i)
            close(60+j)
         enddo
!
!         write(6,*)numnodesnotdefaultval, icount1, icount2, nps
         do j =1, icount1
            backspace(13)
         enddo
!
!         write(6,'(3i10)') sum(nwork(:)), icount2, npn
!
         icount1 = 0
         do j = 1, npn
            if( nwork(j) /= 0 ) then
                 icount1 = icount1 + 1
!                 if(nwork(j) ==1)  nwork(j) = 0
            endif
         enddo
         nwork(:) = 0
!
         write(33,'(i10)') icount1
!
         do j = 1, valuespernode(i)
            write(num1,'(i2.2)') i
            write(num2,'(i2.2)') j
            sub13 = project_name(1:len_trim(project_name))//'-sub13'
            sub13 = sub13(1:len_trim(sub13)) // '-' // num1 // '-' // num2 //'.14'
            open(unit=60+j,file=sub13,status='old',action='read')
            read(60+j,*)
            read(60+j,*) m, n
         enddo
         icount1=0
         icount2=0
         isw1 = 1
         isw2 = 1
         do        !??? = 1, ????
            if( isw1 == 1 ) then
               do
                  icount1 = icount1 + 1
                  if( icount1 > numnodesnotdefaultval ) then
                     icount1 = icount1 - 1
                     n1 = npn + 1
                     exit
                  endif
!
                  read(13,*) n, (valp1(k), k=1, valuespernode(i))
                  n = nsequencer(n)
                  if( nmap(n) /= 0 ) then
                      n1 = nmap(n)
                      exit
                  endif
               enddo
            endif
!
            if( isw2 == 1 ) then
               do
               id = 0
                  icount2 = icount2 + 1
                  if( icount2 > nps ) then
                     icount2 = icount2 - 1
                     n2 = npn + 1
                     exit
                  endif
                  do k = 1, valuespernode(i)
                     read(60+k,*) n2, dum, dum, valp2(k)
                        if( valp2(k) == outflag ) then
                            write(6,*) n2
                            write(6,*) '      Aura293     : Sub-fort.13 has non-eddited nodal values.'
                            write(6,*) '                    System will be stop'
                            write(6,*)
                            write(6,*) '**** Hit the Enter-Key to stop ****'
                            read(5,*)
                            stop
                        endif
                     if( dabs( valp2(k)-defaultattrval(i,k) ) > eps ) then
                         id = 1
                     endif
                  enddo
                  m = n2
                  n2 = nmaps(n2)
                  if( id == 1 ) exit
               enddo
            endif
!
            if( (n1 > npn) .and. (n2 > npn) ) exit
!
            if( n1 < n2 ) then
               isw1 = 1
               isw2 = 0
               if( nwork(n1) == 0 ) then
                   write(33,'(i10,200f15.6)') n1,( valp1(k), k=1,valuespernode(i) )
                   nwork(n1) = 1
               endif
            else
               if( n1 > n2 ) then
                  isw1 = 0
                  isw2 = 1
                  if( nwork(n2) == 0 ) then
                      write(33,'(i10,200f15.6)') n2,( valp2(k), k=1,valuespernode(i) )
                      nwork(n2) = 1
                  endif
               else
                  isw1 = 1
                  isw2 = 1
                  if( nwork(n1) == 0 ) then
                     write(33,'(i10,200f15.6)') n1,( valp1(k), k=1,valuespernode(i) )
                     nwork(n1) = 1
                  endif
               endif
            endif
         enddo
!
         do j = 1, valuespernode(i)
            close(60+j)
         enddo
      enddo
!
!
   end subroutine merge_nodals
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine readetab( ne, netable )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: ne
      integer, intent(out) :: netable(ne)
      integer :: icdigit, n1, i, m, n, j
      integer, allocatable :: nw(:)

!Read element property of global grid (Contained in sub:1, not:0)
      netable(:) = 0
      open(20,file='.tmp01',status='unknown',action='read')
         read(20,*) icdigit
         allocate( nw(icdigit) )
         m = mod( ne, icdigit )
         if( m == 0 ) then
            m = ne / icdigit
         else
            m = ne / icdigit + 1
         endif
         n1 = 0
         do i = 1, m
            read(20,*) n
            do j = 1, icdigit
               nw(j) = mod(n,2)
               n = n / 2
            enddo
            do j = icdigit, 1, -1
               n1 = n1 + 1
               if( n1 > ne ) exit
                  netable(n1) = nw(j)
            enddo
         enddo
      close(20)
      deallocate ( nw )
   end subroutine readetab
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine grid_check(fort14, fort13 )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      character(120), intent(in) :: fort14, fort13
!
      integer :: ne, np
      double precision, allocatable  :: xyd(:,:)
      integer,          allocatable  :: nm(:,:)
      integer :: nope, neta, nvdl_max
      integer,          allocatable  :: nvdll(:), nbdv(:,:)
      integer :: nbou, nvel, nvel_max
      integer,          allocatable  :: nvell(:), ibtype(:),  nbvv(:,:), ibconn(:,:)
      double precision, allocatable :: bar(:,:,:)
      integer :: nodemax
      integer, allocatable :: nsequencer(:)
      character(120) :: agrid
!
!
!Open(14), for dynamic allocation
      write(6,*) 'START! READING of Global data:  ', fort14(1:len_trim(fort14))
      open(14,file=fort14,status='old',action='read')
         call read14_alloc ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax )
      close(14)
! Dymanic Memory Allocation for Global Grid
         allocate( xyd(3,np) )
         allocate( nm(ne,3) )
         allocate( nvdll(nope)  )
         allocate( nbdv(nope,nvdl_max) )
         allocate( nvell(nbou), ibtype(nbou)  )
         allocate( nbvv(nbou,nvel_max), ibconn(nbou,nvel_max), bar(3,nbou,nvel_max) )
         allocate( nsequencer(nodemax) )
!
!Re-open & read global grid data(14)
      open(14,file=fort14,status='old',action='read')
         call read14 ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax, nsequencer,      &
                       agrid, xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, &
                       bar )

      close(14)
      write(6,*) 'FINISH!'
      write(6,*)
!
!Check Grid
      call checkgrd ( ne, np, nope, nbou, nvdl_max, nvel_max, nodemax, nsequencer, fort13, &
                      xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, bar )

!
   end subroutine grid_check

!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine checkgrd ( ne, np, nope, nbou, nvdl_max, nvel_max, nodemax, nsequencer, fort13,&
                         xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, bar )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: ne, np, nope, nbou, nvdl_max, nvel_max
      integer, intent(in) :: nodemax, nsequencer(nodemax)
      character(120), intent(in) :: fort13
      double precision, intent(inout) :: xyd(3,np)
      integer, intent(inout) :: nm(ne,3)
      integer, intent(inout) :: neta, nvdll(nope), nbdv(nope,nvdl_max)
      integer, intent(inout) :: nvel, nvell(nbou), nbvv(nbou,nvel_max), ibtype(nbou)
      integer, intent(inout) :: ibconn(nbou,nvel_max)
      double precision, intent(inout) :: bar(3,nbou,nvel_max)
!
      integer :: nen, npn, nopen, nboun
!
      integer :: i, j, k, n, m, n1, n2, m1, m2, i1, i2, icount, ki, iriver
      integer, allocatable :: nmap_l1(:), nmap_l2(:)
      integer :: nean_max=10
      integer, allocatable :: numean(:), nean(:,:), nmapriver(:), nriver(:)
      double precision :: r1, r2
      double precision, allocatable :: rlocation(:)
      character(160) :: outname
      character(1) :: yn
!
      allocate( nmap_l1(np), nmap_l2(np) )
! Check Disjoint Node
      write(6,*)
      write(6,*) '   *CHECK Dis-joint node'
      nmap_l2(:) = 0
      do m = 1, ne
         do i = 1, 3
            nmap_l1(nm(m,i)) = 1
         enddo
      enddo
      if( sum(nmap_l1(:)) == np ) then
         write(6,*) '      Dis-joint node was not found!'
         npn = np
         nen = ne
         do n = 1, npn
            nmap_l1(n) = n
         enddo
      else
         npn = np
         nen = ne
         call eliminate_node( ne, np, nen, npn, nope, nbou, nvdl_max, nvel_max, nmap_l1,      &
                              xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, bar )
      endif
!
! Check Overlapping Element
      write(6,*)
      write(6,*) '   *CHECK Overlapping Element'
      allocate ( numean(np) )
!
!     Search Elements around a node
  666 continue
      allocate ( nean(np,nean_max) )
      numean(:) = 0
      do m = 1, nen
         do i = 1, 3
            n = nm(m,i)
            numean(n) = numean(n) + 1
            if( numean(n) > nean_max ) then
               nean_max = nean_max + 5
               deallocate( nean )
               go to 666
            endif
            nean(n,numean(n)) = m
         enddo
      enddo
!
      call overlap_e( ne, np, nen, npn, xyd, nm, nean_max, numean, nean )
!
! Check Disjoint Node
      write(6,*)
      write(6,*) '   *RE-CHECK Dis-joint node'
      nmap_l2(:) = 0
      do m = 1, nen
         do i = 1, 3
            nmap_l2(nm(m,i)) = 1
         enddo
      enddo
      if( sum(nmap_l2(:)) == npn ) then
         write(6,*) '      Dis-joint node was not found!'
         do n = 1, npn
            nmap_l2(n) = n
         enddo
      else
         call eliminate_node( ne, np, nen, npn, nope, nbou, nvdl_max, nvel_max, nmap_l2,      &
                              xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, bar )
      endif
!
! Check Overlapping Element
!
      write(6,*)
      write(6,*) 'Please Enter name of output renumbered/checked grid:'
      read(5,*) outname
      open(14,file=outname,status='replace',action='write')
!
      write(14,*) 'grid'
      write(14,*) nen, npn
      do n = 1, npn
         write(14,501)  n, (xyd(i,n),i=1,3)
      enddo
      do m = 1, nen
         write(14,502) m, 3, (nm(m,i),i=1,3)
      enddo
      write(14,*) nope
      n1 =  0
      do n = 1, nope
         n1 = n1 + nvdll(n)
      enddo
      write(14,*) n1
      do k = 1, nope
         write(14,*) nvdll(k)
         do j = 1, nvdll(k)
              write(14,*) nbdv(k,j)
         enddo
      enddo
      write(14,*) nbou
      n1 =  0
      do n = 1, nbou
            n1 = n1 + nvell(n)
         if( (ibtype(n) == 4) .or. (ibtype(n) == 24)) then
            n1 = n1 + nvell(n)
         endif
      enddo
      write(14,*) n1
!
      allocate( nmapriver(nbou) )
      write(6,*)
      write(6,*) ' --- Do you want to check the rivers locasion?(Y/N) '
  678 continue
      read(5,*) yn
      if( (yn == 'N').or. (yn == 'n') ) then
          do k = 1, nbou
             nmapriver(k) = k
          enddo
      else
         if( (yn == 'Y').or. (yn == 'y') ) then
             iriver = 0
             do k = 1, nbou
                if( ibtype(k) == 52 ) iriver = iriver + 1
             enddo
             allocate( nriver(iriver), rlocation(iriver) )
             rlocation(:) = 0.0d0
             iriver = 0
             do k = 1, nbou
                if( ibtype(k) == 52 ) then
                    iriver = iriver + 1
                    nriver(iriver) = k
                    do j = 1, nvell(k)
                       rlocation(iriver) = rlocation(iriver) + xyd(1,nbvv(k,j))
                    enddo
                    rlocation(iriver) = rlocation(iriver) / dble(nvell(k))
                endif
             enddo
             do n1 = 1, iriver
                do n2 = 1, iriver - 1
                   r1 = rlocation(n2)
                   r2 = rlocation(n2+1)
                   m1 = nriver(n2)
                   m2 = nriver(n2+1)
                   if( r1 <= r2 ) then
                       rlocation(n2)   = r2
                       rlocation(n2+1) = r1
                       nriver(n2)   = m2
                       nriver(n2+1) = m1
                   endif
                enddo
             enddo
             iriver = 0
             do k = 1, nbou
                if( ibtype(k) /= 52 ) then
                    nmapriver(k) = k
                else
                    iriver = iriver + 1
                    nmapriver(k) = nriver(iriver)
                endif
             enddo
         else
             write(6,*) '     Please input Y or N '
             go to 678
         endif
      endif
!
      do ki = 1, nbou
         k = nmapriver(ki)
         write(14,*) nvell(k), ibtype(k)
         select case (ibtype(k))
            case(3,13,23)
               do j = 1, nvell(k)
                  write(14,510) nbvv(k,j), (bar(i,k,j), i=1,2)
               enddo
            case(4,24)
               do j = 1, nvell(k)
                  if((nbvv(k,j) == 0 ).or.(ibconn(k,j)==0)) then
                     write(6,*) k,j,nbvv(k,j), ibconn(k,j)
                  endif
                  write(14,511) nbvv(k,j), ibconn(k,j), (bar(i,k,j), i=1,3)
               enddo
            case default
               do j = 1, nvell(k)
                  write(14,'(i8)') nbvv(k,j)
               enddo
         end select
      enddo
 501  format(i10,2f15.6, e16.8)
 502  format(5i10)
 510  format( i10, 2f15.6)
 511  format(2i10, 3f15.6)
!
      if(trim(fort13).ne.'null') then
         call renum13( fort13, nodemax, nsequencer, np, npn, nmap_l1, nmap_l2 )
      endif
!
!
      write(6,*) 'Finish, please hit Enter key'
      read(5,*)
!
!
   end subroutine checkgrd
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine eliminate_node( ne, np, nen, npn, nope, nbou, nvdl_max, nvel_max, nwork,      &
                              xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, bar )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: ne, np, nope, nbou, nvdl_max, nvel_max
      double precision, intent(inout) :: xyd(3,np)
      integer, intent(inout) :: nm(ne,3), nwork(np)
      integer, intent(inout) :: neta, nvdll(nope), nbdv(nope,nvdl_max)
      integer, intent(inout) :: nvel, nvell(nbou), nbvv(nbou,nvel_max), ibtype(nbou)
      integer, intent(inout) :: ibconn(nbou,nvel_max)
      double precision, intent(inout) :: bar(3,nbou,nvel_max)
      integer, intent(inout) :: nen, npn
!
      integer :: i, j, k, n, m, icount, n1, n2
!
      icount = 0
      do n = 1, npn
         if(nwork(n)==0) then
            write(6,*) '       node',n,'will be eliminated.'
            cycle
         endif
         icount = icount + 1
         nwork(n) = icount
         do i = 1, 3
            xyd(i,icount) = xyd(i,n)
         enddo
      enddo
      npn = icount
      do m = 1, nen
         do i = 1, 3
            nm(m,i) = nwork(nm(m,i))
         enddo
      enddo
!
      write(6,*) '     Dis-joint node check on BC'
      write(6,*) '       BC of Elevation'
      do i = 1, nope
         icount = 0
         do j = 1, nvdll(i)
            n = nbdv(i,j)
            if( nwork(n) == 0 ) then
               write(6,*) '       node',i,'-',j, n,'will be eliminated.'
               cycle
            endif
            icount = icount + 1
            nbdv(i,icount) = n
         enddo
         nvdll(i) = icount
      enddo
      write(6,*) '       BC of Velocity'
      do i = 1, nbou
         select case(ibtype(i))
            case(0,1,2,10,11,12,20,21,22,30,52)
               icount = 0
               do j = 1, nvell(i)
                  n = nbvv(i,j)
                  if( nwork(n) == 0 ) then
                     write(6,*) '       node',i,'-',j, n,'will be eliminated.'
                     cycle
                  endif
                  icount = icount + 1
                  nbvv(i,icount) = n
               enddo
               nvell(i) = icount
            case(3, 13, 23)
               icount = 0
               do j = 1, nvell(i)
                  n = nbvv(i,j)
                  if( nwork(n) == 0 ) then
                     write(6,*) '       node',i,'-',j, n,'will be eliminated.'
                     cycle
                  endif
                  icount = icount + 1
                  nbvv(i,icount) = n
                  do k = 1, 2
                     bar(k,i,icount) = bar(k,i,j)
                  enddo
               enddo
               nvell(i) = icount
            case(4, 24)
               icount = 0
               do j = 1, nvell(i)
                  n1 = nbvv(i,j)
                  n2 = ibconn(i,j)
                  if( (nwork(n1) == 0).or.(nwork(n2)==0) ) then
                     write(6,*) '       node',i,'-',j, n1,'and',n2,'will be eliminated.'
                     cycle
                  endif
                  icount = icount + 1
                  nbvv(i,icount) = n1
                  ibconn(i,icount) = n2
                  do k = 1, 3
                     bar(k,i,icount) = bar(k,i,j)
                  enddo
               enddo
               nvell(i) = icount
         end select
      enddo
!
   end subroutine eliminate_node
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine overlap_e( ne, np, nen, npn, xyd, nm, nean_max, numean, nean )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: ne, np, nean_max, numean(np), nean(np,nean_max)
      integer, intent(inout) :: nen, npn, nm(ne,3)
      double precision, intent(inout) :: xyd(3,np)
!
      integer :: i, j, n, m, n1, n2, n3, m1, m2, i1, i2, j1, j2, j3, icount
      integer :: nc(3), mc(3)
!
      integer:: ielmel
      integer, allocatable :: ndelmlist(:), nwork(:), mwork(:), nelmel(:), negative(:)
      double precision :: earea, ea1, ea2
!
      allocate (ndelmlist(nen), nwork(np), mwork(ne), negative(ne) )
!
      negative(:) = 1
      do m = 1, nen
         n1 = nm(m,1)
         n2 = nm(m,2)
         n3 = nm(m,3)
         call element_area (np, xyd, n1, n2, n3, earea)
         if( earea <= 0.0d0 ) negative(m) = -1
      enddo
!
      ndelmlist(:) = 0
      mwork(:) = 0
      do m = 1, nen
         if( mwork(m) /= 0 ) cycle
         do i = 1, 3
            call eline(i, n1, n2, nm, m, ne)
            n3 = nm(m,i)
            call element_area (np, xyd, n1, n2, n3, ea1)
            icount = 0
            do i1 = 1, numean(n1)
               m1 = nean(n1,i1)
               if((m1 /= m)) then
                  do i2 = 1, numean(n2)
                     m2 = nean(n2,i2)
                     if( m1 == m2 ) then
                        do j = 1, 3
                           if( n1 == nm(m1,j) ) then
                               j1 = n1
                           else
                              if( n2 == nm(m1,j) ) then
                                  j2 = n2
                              else
                                  j3 = nm(m1,j)
                              endif
                           endif
                        enddo
                        call element_area (np, xyd, j1, j2, j3, ea2)
                        if( ea1*ea2 > 0.0d0 ) then
                           mwork(m)  = 2
                           mwork(m1) = 2
                           exit
                        endif
                     endif
                  enddo
               endif
            enddo
         enddo
      enddo
!
      if( sum( mwork(:) ) == 0 ) then
         write(*,*) '       Good! There are no Overlapping Elements! '
      else
         icount = 0
         ndelmlist(:) = 0
         do m = 1, nen
            if( mwork(m) /= 0 ) then
                icount = icount + 1
                ndelmlist(icount) = m
            endif
         enddo
         write(*,*) '       There are some Overlapping Elements!'
         do i = 1, icount
               n1 = 0
            do i1 = 1, 3
               nc(i1) = nm(ndelmlist(i),i1)
               n1 = n1 + nc(i1)
            enddo
            do j = i+1, icount
!               if( i == j ) cycle
               n2 = 0
               do i1 = 1, 3
                  mc(i1) = nm(ndelmlist(j),i1)
                  n2 = n2 + mc(i1)
               enddo
               if( maxval(nc(:)) == maxval(mc(:)) ) then
               if( minval(nc(:)) == minval(mc(:)) ) then
               if( n1 == n2 ) then
               if( mwork(ndelmlist(j)) == 2 ) then
                  if( negative(ndelmlist(i)) == -1 ) then
                      if( negative(ndelmlist(j)) == -1 ) then
                       mwork(ndelmlist(i)) = 0
                       mwork(ndelmlist(j)) = 1
                       write(6,*) '        Auto-eliminating Overlap Element:',ndelmlist(j)
                      else
                       mwork(ndelmlist(j)) = 0
                       mwork(ndelmlist(i)) = 1
                       write(6,*) '        Auto-eliminating Overlap Element:',ndelmlist(i)
                      endif
                  else
                     mwork(ndelmlist(i)) = 0
                     mwork(ndelmlist(j)) = 1
                     write(6,*) '        Auto-eliminating Overlap Element:',ndelmlist(j)
                  endif
               endif
               endif
               endif
               endif
            enddo
         enddo
         icount = 0
         ndelmlist(:) = 0
         do m = 1, nen
            if( mwork(m) == 2 ) then
                icount = icount + 1
                ndelmlist(icount) = m
            endif
         enddo
         nwork(:) = 0
         do i = 1, icount
            m = ndelmlist(i)
            do j = 1, 3
               nwork(nm(m,j)) = 1
            enddo
         enddo
         if ( icount /= 0 ) then
           open(66,file='overlap.grd',status='replace',action='write')
           write(6,*)
           write(6,*) '       *Overlap elements are outputed as [overlap.grd]'
           write(66,*)'Overlap Element'
           write(66,*) icount, sum(nwork(:))
           do n = 1, npn
             if( nwork(n) == 1 ) then
                write(66,'(i10,3g16.8)') n, (xyd(i,n), i=1,3 )
             endif
           enddo
           write(6,*) '     LIST of Overlapped Elements'
           do i = 1, icount
              m = ndelmlist(i)
              write(66,'(5i10)') m, 3, (nm(m,j), j=1,3)
              write(6,'(15x,i10)') m
           enddo
           close(66)
           write(6,*) '     Please input Total Number of Elements you want to eliminate'
           read(5,*) ielmel
         else
           write(6,*) '        *** Gridscope could eliminate ALL overlapping elements!!'
           ielmel = 0
         endif
         n1 = ielmel
         do m = 1, nen
            if( mwork(m) == 1 ) then
                n1 = n1 + 1
            endif
         enddo
         allocate ( nelmel(n1) )
         n1 = 0
         do m = 1, nen
            if( mwork(m) == 1 ) then
                n1 = n1 + 1
                nelmel(n1) = m
            endif
         enddo

         mwork(:) = 0
         if( ielmel /= 0 ) then
            write(6,*) '     Please input Number of Element you want to eliminate'
            read(5,*) ( nelmel(j+n1), j=1,ielmel )
            do i = 1, ielmel
               m = 0
               do n = 1, icount
                  if( ndelmlist(n) == nelmel(i+n1) ) then
                     m = m + 1
                     exit
                  endif
               enddo
               if( m == 0 ) then
                   write(6,*) '========= Your Selection is wrong!, system will stop'
                   stop
               endif
               mwork(nelmel(i+n1)) = 1
            enddo
         endif
         do m = 1, n1
            mwork(nelmel(m)) = 1
         enddo
!
         ielmel = 0
         do m = 1, nen
            if( mwork(m) == 0 ) then
                ielmel = ielmel + 1
                do i = 1, 3
                   nm(ielmel,i) = nm(m,i)
                enddo
            endif
         enddo
         nen = ielmel
      endif
!
      ielmel = 0
      mwork(:) = 0
      do m = 1, nen
         n1 = nm(m,1)
         n2 = nm(m,2)
         n3 = nm(m,3)
         call element_area (np, xyd, n1, n2, n3, earea)
         if( earea <= 0.0d0 ) mwork(m) = 1
      enddo
      if( sum(mwork(:)) == 0 ) then
         write(*,*) '       There are no Negative Elements! '
      else
         write(6,*) '  LIST of Negative Elements'
         do m = 1, nen
             if( mwork(m) == 1 ) then
                 write(6,'(15x,i10)') m
             endif
         enddo
  698    continue
         write(6,*) '  What do you want to do for Negative Elements?'
         write(6,*) '    1. Eliminate them'
         write(6,*) '    2. Fix the Element Connectibity to Counter-Clock'
         write(6,*) '    3. Do nothing'
         read(5,*) icount
         select case(icount)
            case(1)
               ielmel = 0
               do m = 1, nen
                  if( mwork(m) == 0 ) then
                      ielmel = ielmel + 1
                      do i = 1, 3
                         nm(ielmel,i) = nm(m,i)
                      enddo
                  endif
               enddo
                 nen = ielmel
            case(2)
               do m = 1, nen
                  if( mwork(m) == 1 ) then
                      n1 = nm(m,1)
                      n2 = nm(m,2)
                      nm(m,1) = n2
                      nm(m,2) = n1
                  endif
               enddo
         end select
         write(*,*)
      endif
!
      deallocate (ndelmlist, nwork, mwork)
!
   end subroutine overlap_e
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine element_area (np, xyd, n1, n2, n3, earea)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: np, n1, n2, n3
      double precision, intent(in) :: xyd(3,np)
      double precision, intent(out) :: earea
!
      double precision :: x1, x2, x3, y1, y2, y3
!
         x1 = xyd(1,n1)
         x2 = xyd(1,n2)
         x3 = xyd(1,n3)
         y1 = xyd(2,n1)
         y2 = xyd(2,n2)
         y3 = xyd(2,n3)
         earea =  (x1 - x2) * (y1 - y3) - (x1 - x3) * (y1 - y2)
  end subroutine element_area
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
  subroutine renum13 ( fort13, nodemax, nsequencer, np, npn, nmap_l1, nmap_l2 )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: nodemax, nsequencer(nodemax)
      integer, intent(in) :: np, nmap_l1(np), nmap_l2(np), npn
      character(120), intent(in) :: fort13
!
      character*120 :: a120, outname, b120
!
      character*120, allocatable :: attrname(:)
!
      integer :: n, i, j, k, nattr, num, icount
      integer, allocatable :: valuespernode(:), nwork(:)
      double precision, allocatable :: valp(:)
!
      allocate(nwork(np))
!
      open(13,file=fort13,status='old',action='read')
      write(6,*)
      write(6,*) 'Please Enter name of output renumbered fort13 :'
      read(5,*) outname
      open(33,file=outname,status='replace',action='write')
!
      read(13,*)           !Agrid
      write(33,*) 'fort13' !Agrid
      read(13,*)  n        !Num of nodes = np
      write(33,*) npn      !Num of nodes = np
      if( n /= np ) then
          write(6,*) ' Do you use correct fort.13? (Total number of nodes are not equal)'
          write(6,*) n, np
          stop
      endif
      read(13,*) nattr   !NAttr
      write(33,*) nattr  !NAttr
!
      allocate(valuespernode(nattr))
!
!
      n = 12
      allocate(valp(n), attrname(nattr))
      do i = 1, nattr
         read (13,*) a120                     !AttrName
         attrname(i) = adjustl(a120)
         write(33,*) a120(1:len_trim(a120))   !AttrName
         read (13,*) a120                     !Units
         write(33,*) a120(1:len_trim(a120))   !Units
         read (13,*) valuespernode(i)
         write(33,*) valuespernode(i)
         if( n < valuespernode(i) ) then
             write(6,*) 'System Stop'
             stop
!             n = valuespernode(i)
!             deallocate( valp )
!             allocate(valp(n))
         endif
         read (13,*) ( valp(k), k = 1,valuespernode(i) )
         write(33,'(200f15.6)') ( valp(k), k = 1,valuespernode(i) )
      enddo
!
      do i = 1, nattr
         read (13,*) a120                     !AttrName
         write(33,*) a120(1:len_trim(a120))   !AttrName
         write(6,*) '     ', a120(1:len_trim(a120))   !AttrName
         read(13,*) num
         icount = 0
         nwork(:) = 0
         if ( i == nattr ) then
            do j = 1, num
               read(13,*) b120
               icount = icount + 1
               read(b120,*) n
               nwork(nsequencer(n)) = 1
            enddo
         else
            do
               read(13,*) b120
               if( adjustl(b120) == adjustl(attrname(i+1)) ) exit
               icount = icount + 1
               read(b120,*) n
               nwork(nsequencer(n)) = 1
            enddo
            backspace(13)
         endif
         num = icount
         do j = 1, icount
            backspace(13)
         enddo
         icount = 0
         do j = 1, num
            read(13,*) n
            n = nsequencer(n)
            if( nwork(n) /= 1 ) cycle
               nwork(n) = nwork(n) + 1
               if( n /= 0 ) then
                  n = nmap_l1(n)
                  if( n /= 0 ) then
                     n = nmap_l2(n)
                     if( n /= 0 ) then
                         icount = icount + 1
                     endif
                  endif
               endif
         enddo
         do j = 1, num
            backspace(13)
         enddo
         write(33,'(i10)') icount
         do j = 1, num
            read(13,*) n, ( valp(k), k = 1,valuespernode(i) )
            n = nsequencer(n)
            if( nwork(n) /= 2 ) cycle
               nwork(n) = nwork(n) + 1
               if( n /= 0 ) then
                  n = nmap_l1(n)
                  if( n /= 0 ) then
                     n = nmap_l2(n)
                     if( n /= 0 ) then
                         write(33,'(i10,200f15.6)') n, ( valp(k), k = 1,valuespernode(i) )
                     endif
                  endif
               endif
         enddo
      enddo
      close(13)
      close(33)
!
  end subroutine renum13
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!!-PCK-
!
! Routines:
!
!   1) levee_editor
!   2) fort13_editor
!   3) bath_editor
!   4) select_target
!   5) output_grid
!   6) adjustment_method
!   7) select_polygon
!   8) switch_endian
!   9) polybuild
!   10) tribuild
!   11) read1314_alloc
!   12) read1314
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine levee_editor(fort14, project_name, fort13)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!
      implicit none
!
      character(120), intent(in)    :: fort14, fort13, project_name
!
      character(120)                :: agrid, gridname
      integer                       :: ne, np, nope, nbou, nvdl_max, nvel_max, neta, nvel, nodemax
      integer                       :: h, i, j, k, m          ! Loop Counters
      integer                       :: itarget, icase, ishape
      integer, allocatable          :: nvdll(:), nbdv(:,:), nm(:,:)
      integer, allocatable          :: nprop(:)            ! Node Selected? 1 = Yes, 0 = No
      integer, allocatable          :: nvell(:), nbvv(:,:), ibtype(:), ibconn(:,:), nsequencer(:)
      double precision, allocatable :: xyd(:,:), bar(:,:,:)
! Cases 1, 2, & 3
      integer                       :: imethod             ! Adjustment Method (0 to 4)
      double precision              :: ifactor             ! Adjstument Factor
! Cases 0 & 4
      integer, allocatable          :: TEND(:,:,:,:)       ! Levee End Connection Information
      integer                       :: vz(20,4)            ! Intersecting Information
      integer                       :: Xvz(20,4)
      integer                       :: vx                  ! # of Triangles connected at Intersection
      integer                       :: k1, j1, i1          ! Old Intersection Triangle Details
      integer                       :: k2, j2, i2          ! New Intersection Triangle Details
      integer                       :: k3, j3, i3          ! Forward Intersection Triangle Details
! Case 3
      integer, allocatable          :: Xnvdll(:),Xnbdv(:,:)
      character(1)                  :: c1a, c1b, c1c, c1d
      character(2)                  :: c2
      character(3)                  :: c3
      character(4)                  :: c4
      integer                       :: nNADD, nEADD         ! Number of Nodes/Elements Added
      integer                       :: A1,B1,C1,E1          ! Temporary Storage
      integer                       :: ibox
      integer                       :: Xnp
      integer                       :: Xne
      integer                       :: Xnbou
      integer                       :: Xnvel
      integer                       :: Ynp
      integer                       :: Yne
      integer                       :: Zne
      integer                       :: Znp
      integer                       :: Znbou
      integer, allocatable          :: EADD(:,:)
      integer, allocatable          :: Xpropn(:,:)
      integer, allocatable          :: Xprope(:,:)
      integer, allocatable          :: Xnvell(:)
      integer, allocatable          :: Xnbvv(:,:)
      integer, allocatable          :: Xibtype(:)
      integer, allocatable          :: Xibconn(:,:)
      integer, allocatable          :: Xnm(:,:)
      integer, allocatable          :: Zbar(:,:,:)
      integer, allocatable          :: Znm(:,:)
      double precision              :: D1,G1
      double precision, allocatable :: Ndist(:,:), Edist(:,:), Wdist(:,:), Sdist(:,:)
      double precision, allocatable :: Zxyd(:,:)
      double precision, allocatable :: Xbar(:,:,:)
      double precision, allocatable :: Xxyd(:,:)
      integer                       :: ifill               ! Fill in Levees (1=Yes)
      integer                       :: iend                ! Fill in Intersections (1=Yes)
      integer, allocatable          :: ppt(:)
      double precision              :: Intz,Inty,Intx
      integer                       :: Rnbou
      integer, allocatable          :: RTEND(:,:,:,:)
! Case 4
      integer                       :: z1, z2, z3, z4      ! Pivot Point Node Numbers
      integer                       :: nloops              ! Endless Loop Count
      integer                       :: ntri                ! # of Triangles
      double precision              :: t1                  ! Reordering Calculation
      integer                       :: t2                  ! Reordering Temp
      integer, allocatable          :: tri(:,:)            ! Triangle Points
      integer, allocatable          :: isel(:)             ! Triangle Already Selected
      integer, allocatable          :: iSide(:,:)          ! Side Already Accounted For
      integer                       :: nDoub               ! Number of Duplicated Sides
      double precision, allocatable :: sid(:,:)            ! Polygon Side Coords
      integer                       :: nsid                ! Number of Sides
      integer                       :: s1a,s1b,s2a,s2b     ! Triangle Nodes
      integer                       :: npoly               ! Number of Polygons
      integer                       :: iExit               ! Exit?
      integer,allocatable           :: npts(:)             ! Number of Points in Each Polygon
      double precision, allocatable :: plg(:,:,:)          ! Polygon Coordinates
      integer, allocatable          :: ppi(:,:)            ! Poly Data
      integer                       :: ptmax               ! Max Vertices
      integer                       :: pts                 ! Total Number of Points
      integer, allocatable          :: ConV(:,:)           ! Element Connections
      integer                       :: PCK                 ! 1 = Yes Intersections
      integer,allocatable           :: Pint(:)             ! 1 = Yes Int Done
!! Open(14), for dynamic allocation
      write(6,*) 'START! READING of Global data:  ', fort14(1:len_trim(fort14))
      open(14,file=fort14,status='old',action='read')
         call read14_alloc ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax )
      close(14)
      nvel_max = nvel_max + 2
!! Dymanic Memory Allocation for Global Grid
         allocate( xyd(3,np) )
         allocate( nm(ne,3) )
         allocate( nvdll(nope)  )
         allocate( nbdv(nope,nvdl_max) )
         allocate( nvell(nbou), ibtype(nbou)  )
         allocate( nbvv(nbou,nvel_max), ibconn(nbou,nvel_max), bar(3,nbou,nvel_max) )
         allocate( nsequencer(nodemax) )
!! Re-open & read global grid data(14)
      open(14,file=fort14,status='old',action='read')
        call read14 ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax, nsequencer,      &
                      agrid, xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, &
                      bar )
      close(14)
      write(6,*) 'FINISH!'
      write(6,*)
!
      allocate( nprop(np) )
!
      write(6,*)
      write(6,*)
      write(6,*)
      write(6,*)
      write(6,*) '*Levee Editor*'
      write(6,*)
      write(6,*) 'Select an option'
      write(6,*) ' 0. Remove Levees'
      write(6,*) ' 1. Adjust internal barrier height (BARINHT)'
      write(6,*) ' 2. Adjust coef of free surface SUBcritical flow (BARINCFSB)'
      write(6,*) ' 3. Adjust coef of free surface SUPERcritical flow (BARINCFSP)'
      write(6,*) ' 4. Build Polygon Shapefile around Levees'
      read(5,*) icase
      select case(icase)
        case(0)
          gridname = project_name(1:len_trim(project_name))//'-LeveeRemove.grd'
        case(1)
          gridname = project_name(1:len_trim(project_name))//'-LeveeHeight.grd'
        case(2)
          gridname = project_name(1:len_trim(project_name))//'-LeveeSub.grd'
        case(3)
          gridname = project_name(1:len_trim(project_name))//'-LeveeSuper.grd'
        case(4)
          gridname = project_name(1:len_trim(project_name))//'-Polygons'
        case default
          write(6,*) 'You have to select 0,1,2, or 3'
          write(6,*) 'Hit the Enter-key to stop'
          read(5,*)
          stop
      end select
!
! IDENTIFY TARGET AREA
      write(6,*)
      write(6,*) 'Select the target shape'
      call select_target(np, xyd, nprop)
      write(6,*) 'Target Identified'

      write(6,*)
      write(6,*) 'Do you wish to adjust levees inside or outside the target shape?'
      write(6,*) 'Select an option'
      write(6,*) ' 0. Inside'
      write(6,*) ' 1. Outside'
      write(6,*)
      read(5,*) itarget
      write(6,*)
      if (itarget.EQ.1) then
        do k = 1,np
          if (nprop(k).EQ.1) then
            nprop(k) = 0
          else
            nprop(k) = 1
          end if
        end do
      end if
!
      write(6,*) 'Beginning', gridname

      select case(icase)
! Cases 1,2, & 3 change the levee height values
        case(1,2,3)
          write(6,*)
          write(6,*) 'Choose adjustment Method'
          call adjustment_method( imethod, ifactor )
          write(6,*)
          h = 0
          do k = 1,nbou
            select case(ibtype(k))
              case(4, 24)
                do j = 1, nvell(k)
                  if ((nprop(nbvv(k,j)).EQ.1).OR.(nprop(ibconn(k,j)).EQ.1)) then
                    if (imethod.EQ.0) bar(icase,k,j) = ifactor
                    if (imethod.EQ.1) bar(icase,k,j) = bar(icase,k,j) + ifactor
                    if (imethod.EQ.2) bar(icase,k,j) = bar(icase,k,j) * ifactor
                    if ((imethod.EQ.3).AND.(bar(icase,k,j).LT.ifactor)) bar(icase,k,j) = ifactor
                    if ((imethod.EQ.4).AND.(bar(icase,k,j).GT.ifactor)) bar(icase,k,j) = ifactor
                    h = h + 1
                  end if
                end do
            end select
          end do
          write(6,*) h, 'Points Adjusted'
          call output_grid( ne, np, nope, nbou, nvdl_max, nvel_max, project_name, gridname, &
                            agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar, neta )
! Cases 0 & 4 collect information
        case(0,4)
          allocate ( TEND(2,2,nbou,4) )
          TEND(:,:,:,:) = 0
          do k = 1,nbou                                                        ! Loop through Boundaries
            if ((ibtype(k).EQ.4).OR.(ibtype(k).EQ.24)) then                 ! Internal boundary?
              do j = 1,nbou                                                    ! Loop through Boundaries
                if ((ibtype(j).EQ.4).OR.(ibtype(j).EQ.24)) then             ! Internal boundary?
                  if ((k.NE.j).AND.(nbvv(k,1).EQ.nbvv(j,1))) then              ! Check Left nbvv
                    TEND(1,1,k,1) = j   !nbou 'j'
                    TEND(1,1,k,2) = 1   !Left 'j'
                    TEND(1,1,k,3) = 1   !nbvv 'j'
                  elseif (nbvv(k,1).EQ.nbvv(j,nvell(j))) then
                    TEND(1,1,k,1) = j   !nbou 'j'
                    TEND(1,1,k,2) = 2   !Right 'j'
                    TEND(1,1,k,3) = 1   !nbvv 'j'
                  elseif ((k.NE.j).AND.(nbvv(k,1).EQ.ibconn(j,1))) then
                    TEND(1,1,k,1) = j   !nbou 'j'
                    TEND(1,1,k,2) = 1   !Left 'j'
                    TEND(1,1,k,3) = 2   !ibconn 'j'
                  elseif (nbvv(k,1).EQ.ibconn(j,nvell(j))) then
                    TEND(1,1,k,1) = j   !nbou 'j'
                    TEND(1,1,k,2) = 2   !Right 'j'
                    TEND(1,1,k,3) = 2   !ibconn 'j'
                  end if
                  if ((k.NE.j).AND.(ibconn(k,1).EQ.nbvv(j,1))) then            ! Check Left end ibconn
                    TEND(1,2,k,1) = j   !nbou 'j'
                    TEND(1,2,k,2) = 1   !Left 'j'
                    TEND(1,2,k,3) = 1   !nbvv 'j'
                  elseif (ibconn(k,1).EQ.nbvv(j,nvell(j))) then
                    TEND(1,2,k,1) = j   !nbou 'j'
                    TEND(1,2,k,2) = 2   !Right 'j'
                    TEND(1,2,k,3) = 1   !nbvv 'j'
                  elseif ((k.NE.j).AND.(ibconn(k,1).EQ.ibconn(j,1))) then
                    TEND(1,2,k,1) = j   !nbou 'j'
                    TEND(1,2,k,2) = 1   !Left 'j'
                    TEND(1,2,k,3) = 2   !ibconn 'j'
                  elseif (ibconn(k,1).EQ.ibconn(j,nvell(j))) then
                    TEND(1,2,k,1) = j   !nbou 'j'
                    TEND(1,2,k,2) = 2   !Right 'j'
                    TEND(1,2,k,3) = 2   !ibconn 'j'
                  end if
                  if (nbvv(k,nvell(k)).EQ.nbvv(j,1)) then       ! Check Right end nbvv
                    TEND(2,1,k,1) = j   !nbou 'j'
                    TEND(2,1,k,2) = 1   !Left 'j'
                    TEND(2,1,k,3) = 1   !nbvv 'j'
                  elseif ((k.NE.j).AND.(nbvv(k,nvell(k)).EQ.nbvv(j,nvell(j)))) then
                    TEND(2,1,k,1) = j   !nbou 'j'
                    TEND(2,1,k,2) = 2   !Right 'j'
                    TEND(2,1,k,3) = 1   !nbvv 'j'
                  elseif (nbvv(k,nvell(k)).EQ.ibconn(j,1)) then
                    TEND(2,1,k,1) = j   !nbou 'j'
                    TEND(2,1,k,2) = 1   !Left 'j'
                    TEND(2,1,k,3) = 2   !ibconn 'j'
                  elseif ((k.NE.j).AND.(nbvv(k,nvell(k)).EQ.ibconn(j,nvell(j)))) then
                    TEND(2,1,k,1) = j   !nbou 'j'
                    TEND(2,1,k,2) = 2   !Right 'j'
                    TEND(2,1,k,3) = 2   !ibconn 'j'
                  end if
                  if (ibconn(k,nvell(k)).EQ.nbvv(j,1)) then     ! Check Right end ibconn
                    TEND(2,2,k,1) = j   !nbou 'j'
                    TEND(2,2,k,2) = 1   !Left 'j'
                    TEND(2,2,k,3) = 1   !nbvv 'j'
                  elseif ((k.NE.j).AND.(ibconn(k,nvell(k)).EQ.nbvv(j,nvell(j)))) then
                    TEND(2,2,k,1) = j   !nbou 'j'
                    TEND(2,2,k,2) = 2   !Right 'j'
                    TEND(2,2,k,3) = 1   !nbvv 'j'
                  elseif (ibconn(k,nvell(k)).EQ.ibconn(j,1)) then
                    TEND(2,2,k,1) = j   !nbou 'j'
                    TEND(2,2,k,2) = 1   !Left 'j'
                    TEND(2,2,k,3) = 2   !ibconn 'j'
                  elseif ((k.NE.j).AND.(ibconn(k,nvell(k)).EQ.ibconn(j,nvell(j)))) then
                    TEND(2,2,k,1) = j   !nbou 'j'
                    TEND(2,2,k,2) = 2   !Right 'j'
                    TEND(2,2,k,3) = 2   !ibconn 'j'
                  end if
                end if
              end do
            end if
          end do
          allocate ( RTEND(2,2,nbou,2) )
          RTEND(:,:,:,:) = 0
          do k = 1,nbou                                                        ! Loop through Boundaries
            if ((ibtype(k).EQ.4).OR.(ibtype(k).EQ.24)) then                 ! Internal boundary?
              do j = 1,nbou                                                    ! Loop through Boundaries
                if ((ibtype(j).NE.4).AND.(ibtype(j).NE.24)) then             ! Internal boundary?
                  if (nbvv(k,1).EQ.nbvv(j,1)) then              ! Check Left nbvv
                    RTEND(1,1,k,1) = j   !nbou 'j'
                    RTEND(1,1,k,2) = 1   !Left 'j'
                  elseif (nbvv(k,1).EQ.nbvv(j,nvell(j))) then
                    RTEND(1,1,k,1) = j   !nbou 'j'
                    RTEND(1,1,k,2) = nvell(j)   !Right 'j'
                  end if
                  if (ibconn(k,1).EQ.nbvv(j,1)) then            ! Check Left end ibconn
                    RTEND(1,2,k,1) = j   !nbou 'j'
                    RTEND(1,2,k,2) = 1   !Left 'j'
                  elseif (ibconn(k,1).EQ.nbvv(j,nvell(j))) then
                    RTEND(1,2,k,1) = j   !nbou 'j'
                    RTEND(1,2,k,2) = nvell(j)   !Right 'j'
                  end if
                  if (nbvv(k,nvell(k)).EQ.nbvv(j,1)) then       ! Check Right end nbvv
                    RTEND(2,1,k,1) = j   !nbou 'j'
                    RTEND(2,1,k,2) = 1   !Left 'j'
                  elseif (nbvv(k,nvell(k)).EQ.nbvv(j,nvell(j))) then
                    RTEND(2,1,k,1) = j   !nbou 'j'
                    RTEND(2,1,k,2) = nvell(j)   !Right 'j'
                  end if
                  if (ibconn(k,nvell(k)).EQ.nbvv(j,1)) then     ! Check Right end ibconn
                    RTEND(2,2,k,1) = j   !nbou 'j'
                    RTEND(2,2,k,2) = 1   !Left 'j'
                  elseif (ibconn(k,nvell(k)).EQ.nbvv(j,nvell(j))) then
                    RTEND(2,2,k,1) = j   !nbou 'j'
                    RTEND(2,2,k,2) = nvell(j)   !Right 'j'
                  end if
                end if
              end do
            end if
          end do
          write(6,*) 'Boundary Connections Determined'
      end select


      select case(icase)
        case(0)
          write(6,*)
          write(6,*) 'Do you wish to Fill in Levees ( 1=Yes )'
          read(5,*) ifill

          allocate ( Sdist(nbou,nvel_max), Ndist(nbou,nvel_max), Edist(nbou,nvel_max), Wdist(nbou,nvel_max) )
          allocate ( Zbar(3,nbou,nvel_max) )
          nNADD = 0   ! # of Nodes Added
          nEADD = 0   ! # of Elements Added

!-----+-----DETERMINE ALLOCATIONS---+---------+---------+---------+---------+---------+---------+---------+
! Determine interior NADD allocation
          Zbar(1,:,:) = 2
          do k = 1,nbou                                                        ! Loop through Boundaries
            if ((ibtype(k).EQ.4).OR.(ibtype(k).EQ.24)) then
              do j = 2,nvell(k)                                                ! Loop through internal levee squares
                Sdist(k,j) = ((xyd(1,nbvv(k,j))-xyd(1,nbvv(k,j-1)))**2+(xyd(2,nbvv(k,j))-xyd(2,nbvv(k,j-1)))**2)**0.5
                Ndist(k,j) = ((xyd(1,ibconn(k,j))-xyd(1,ibconn(k,j-1)))**2+(xyd(2,ibconn(k,j))-xyd(2,ibconn(k,j-1)))**2)**0.5
                Edist(k,j) = ((xyd(1,nbvv(k,j))-xyd(1,ibconn(k,j)))**2+(xyd(2,nbvv(k,j))-xyd(2,ibconn(k,j)))**2)**0.5
                Wdist(k,j) = ((xyd(1,nbvv(k,j-1))-xyd(1,ibconn(k,j-1)))**2+(xyd(2,nbvv(k,j-1))-xyd(2,ibconn(k,j-1)))**2)**0.5
                if ((nprop(nbvv(k,j))+nprop(ibconn(k,j))+nprop(nbvv(k,j-1))+nprop(ibconn(k,j-1))).GE.1) then ! Inside the Target Area

                  Zbar(3,k,j) = 1
                  if (j.eq.2) Zbar(3,k,1) = 1
                  if ((j.EQ.2).and.(nbvv(k,1).ne.nbvv(k,nvell(k)))) then                                             ! Determine Condition of Left End
                    if (4*Wdist(k,j).LE.(Sdist(k,j)+Ndist(k,j))) then            ! NARROW levee
                      nNADD = nNADD + 1
                      Zbar(2,k,1) = np + nNADD                                   ! Assign Middle Node #
                      Zbar(1,k,1) = 1
                    end if
                  end if
                  if ((Edist(k,j).GE.(Sdist(k,j)+Ndist(k,j))).AND.(j.NE.nvell(k))) then            ! WIDE levee
                    nNADD = nNADD + 1
                    Zbar(2,k,j) = np + nNADD                                 ! Assign Middle Node #
                    Zbar(1,k,j) = 3
                  else if ((4*Edist(k,j).LE.(Sdist(k,j)+Ndist(k,j))).and.(nbvv(k,1).ne.nbvv(k,nvell(k)))) then       ! NARROW levee
                    nNADD = nNADD+1                                          ! +nNADD
                    Zbar(2,k,j) = np + nNADD                                 ! Assign Middle Node #
                    Zbar(1,k,j) = 1
                  end if
                end if
              end do
            end if
          end do

          Znbou = 0
          do k = 1, nbou
            select case(ibtype(k))
              case(0,1,2,3,10,11,12,13,20,21,22,23,30,52)
                Znbou = Znbou + 1
              case(4, 24)
                do j = 1, nvell(k)
                  if (Zbar(3,k,j).EQ.0) then
                    if (j.NE.nvell(k)) then
!2                      if (Zbar(3,k,j+1).EQ.1) then
                      if ((Zbar(3,k,j+1).EQ.1).and.(j.ne.1)) then !2
                        Znbou = Znbou + 1
                      end if
                    else
                      if (Zbar(3,k,j-1).NE.1) Znbou = Znbou + 1 !2
!2                      Znbou = Znbou + 1
                    end if
                  end if
                end do
            end select
          end do

          deallocate(Sdist,Ndist,Edist,Wdist)
! Determine interior EADD Allocation
          do k = 1,nbou                                                        ! Loop through Boundaries
            if ((ibtype(k).EQ.4).OR.(ibtype(k).EQ.24)) then                    ! Internal boundary?
              do j = 2,nvell(k)                                                ! Loop through internal levee squares
                if ((Zbar(3,k,j).EQ.1).OR.(Zbar(3,k,j-1).EQ.1)) then             ! Insert Elements
                  write(c1a,'(i1)') Zbar(1,k,j-1)
                  write(c1b,'(i1)') Zbar(1,k,j)
                  c2 = c1a//c1b
                  read(c2,'(i2)') ibox
                  select case(ibox)
                    case (12,21)
                      nEADD = nEADD + 1
                    case (13,22,31)
                      nEADD = nEADD + 2
                    case (23)
                      if (Zbar(1,k,j+1).EQ.3) then
                        nEADD = nEADD + 5
                      else
                        nEADD = nEADD + 3
                      end if
                    case (32)
                      nEADD = nEADD + 3
                    case (33)
                      if (Zbar(1,k,j+1).EQ.3) then
                        nEADD = nEADD + 4
                      else
                        nEADD = nEADD + 2
                      end if
                  end select
                end if
              end do
            end if
          end do
          write(6,*) 'Interior Allocations Determined'
          Ynp = nNADD
! Determine end EADD & NADD allocation
          write(6,*) 'Preparing Search Data'
          allocate( pint(np) )
          allocate( ConV(np,20) )
          ConV(:,:) = 0
          do k = 1,ne
            do j = 1,3
              h = nm(k,j)
              i = 2
              do while (ConV(h,i).NE.0)
                i = i + 1
              end do
              ConV(h,1) = i
              ConV(h,i) = k
            end do
          end do
          write(6,*) 'Preparing Exterior Allocations'
          loop99: do k = 1,nbou
            if ((ibtype(k).EQ.4).OR.(ibtype(k).EQ.24)) then                        ! Internal boundary?
              loop98: do j=1,nvell(k),nvell(k)-1          ! Left/Right
                if (j.EQ.1) then
                  if (Zbar(3,k,1).EQ.0) cycle loop98
                else
                  if (Zbar(3,k,nvell(k)).EQ.0) exit loop98
                end if
                if (pint(nbvv(k,j)).eq.0) then
                  j1 = 1
                  if (j.eq.nvell(k)) j1 = 2
!                  if ((TEND(j1,1,k,1).EQ.0).and.(TEND(j1,2,k,1).EQ.0)) then
!                    if ((ConV(nbvv(k,j),1).EQ.2).OR.(ConV(ibconn(k,j),1).EQ.2)) then
!                      cycle loop98 !Borders Boundary Section
!                    end if
!                  end if
                  pint(nbvv(k,j)) = 1
                  pint(ibconn(k,j))=1
                  vx = 1                          ! Number of Connections
                  vz(:,:) = 0                     ! Connected Boundary (k), L/R (j), nbvv/ibconn (i)
                  vz(vx,1) = nbvv(k,j)           ! start with nbvv
                  vz(vx,2) = k
                  vz(vx,3) = j                    ! start with left side
                  vz(vx,4) = 1                    ! start with nbvv
                  loop97: do                       ! Loop through intersection vertices
                    if (vx.GT.9) then             ! If too many points, assume no connections
                      vz(:,:) = 0
                      vx = 2
                      vz(1,1) = nbvv(k,j)
                      vz(1,2) = k
                      vz(1,3) = j
                      vz(1,4) = 1
                      vz(2,1) = ibconn(k,j)
                      vz(2,2) = k
                      vz(2,3) = j
                      vz(2,4) = 2
!                      write(6,*) 'CHECK', k, j
                      exit loop97
                    end if
                    k1 = vz(vx,2)
                    j1 = vz(vx,3)
                    i1 = vz(vx,4)
                    if (j1.GT.1) j1 = 2
                    if ((TEND(j1,i1,k1,1).NE.0).AND.(k1.NE.0)) then   ! Connected to another Boundary
                      k2 = TEND(j1,i1,k1,1)
                      j2 = TEND(j1,i1,k1,2)
                      i2 = TEND(j1,i1,k1,3)
                      vx = vx +1
                      vz(vx,2) = k2
                      vz(vx,3) = j2
                      if (j2.NE.1) vz(vx,3) = nvell(k2)
                      vz(vx,4) = 1
                      if (i2.EQ.1) vz(vx,4) = 2
                      vz(vx,1) = nbvv(vz(vx,2),vz(vx,3))
                      if (i2.EQ.1) vz(vx,1) = ibconn(vz(vx,2),vz(vx,3))
                    else                                    ! No Direct Levee Connection
                      if (k1.EQ.0) then                     ! Second+ Loop, Set pivot points
                        h = z1                             ! Temporary node pt storage
                        z1 = z2
                        z2 = z4
                        z4 = h
                      else                                  ! First Loop, set pivot points
                        z4 = 0
                        if (j1.EQ.1) then
                          if (i1.EQ.1) then
                            z1 = nbvv(k1,1)
                            z2 = nbvv(k1,2)
                          else
                            z1 = ibconn(k1,1)
                            z2 = ibconn(k1,2)
                          end if
                        else
                          if (i1.EQ.1) then
                            z1 = nbvv(k1,nvell(k1))
                            z2 = nbvv(k1,nvell(k1)-1)
                          else
                            z1 = ibconn(k1,nvell(k1))
                            z2 = ibconn(k1,nvell(k1)-1)
                          end if
                        end if
                      end if
                      nloops = 0
                      loop96: do                        ! Loop through adj tris
                        z3 = 0
                        nloops = nloops + 1
                        if (nloops.GT.10) then         ! Max 10 Loops
                          write(6,*) 'Problem , endless loop'
                          write(6,*) nbvv(k,j)
                          read(5,*)
                          stop
                        end if
                        if (z1.NE.0) then
                          loop95: do m = 2,ConV(z1,1)
                            h = ConV(z1,m)
                            if ((nm(h,1).EQ.z1).AND.(nm(h,2).EQ.z2).AND.(nm(h,3).NE.z4)) z3 = nm(h,3)
                            if ((nm(h,1).EQ.z1).AND.(nm(h,3).EQ.z2).AND.(nm(h,2).NE.z4)) z3 = nm(h,2)
                            if ((nm(h,2).EQ.z1).AND.(nm(h,1).EQ.z2).AND.(nm(h,3).NE.z4)) z3 = nm(h,3)
                            if ((nm(h,2).EQ.z1).AND.(nm(h,3).EQ.z2).AND.(nm(h,1).NE.z4)) z3 = nm(h,1)
                            if ((nm(h,3).EQ.z1).AND.(nm(h,1).EQ.z2).AND.(nm(h,2).NE.z4)) z3 = nm(h,2)
                            if ((nm(h,3).EQ.z1).AND.(nm(h,2).EQ.z2).AND.(nm(h,1).NE.z4)) z3 = nm(h,1)
                          end do loop95
                        end if
                        if (z3.NE.0) then
                          z4 = z2
                          z2 = z3
                        else
                          vx = vx + 1
                          vz(vx,1) = z2
                          vz(vx,2) = 0
                          vz(vx,3) = 0
                          vz(vx,4) = 0
                          loop94: do h = 1,nbou         ! Loop through boundaries to node on levee
                            select case(ibtype(h))
                              case(4, 24)
                                if (nbvv(h,1).EQ.z2) then
                                  vz(vx,2) = h
                                  vz(vx,3) = 1
                                  vz(vx,4) = 1
                                elseif (ibconn(h,1).EQ.z2) then
                                  vz(vx,2) = h
                                  vz(vx,3) = 1
                                  vz(vx,4) = 2
                                elseif (nbvv(h,nvell(h)).EQ.z2) then
                                  vz(vx,2) = h
                                  vz(vx,3) = nvell(h)
                                  vz(vx,4) = 1
                                elseif (ibconn(h,nvell(h)).EQ.z2) then
                                  vz(vx,2) = h
                                  vz(vx,3) = nvell(h)
                                  vz(vx,4) = 2
                                end if
                            end select
                          end do loop94
                          if ((vz(vx,1).NE.ibconn(k,j)).AND.(vz(vx,2).NE.0)) then !Node is on new Levee
                            vx = vx + 1
                            vz(vx,2) = vz(vx-1,2)
                            vz(vx,3) = vz(vx-1,3)
                            vz(vx,4) = 1
                            if (vz(vx-1,4).EQ.1) vz(vx,4) = 2
                            vz(vx,1) = nbvv(vz(vx,2),vz(vx,3))
                            if (vz(vx,4).EQ.2) vz(vx,1) = ibconn(vz(vx,2),vz(vx,3))
                          end if
                          exit loop96
                        end if
                      end do loop96
                    end if
                    if (vz(vx,1).EQ.ibconn(k,j)) exit loop97 !Not necesary to do other side of levee
                  end do loop97
                  if ((Zbar(1,k,j).eq.2).and.(vx.eq.4)) then
                    if ((vz(2,2).ne.0).and.(vz(vx,2).ne.0)) then
                      if ((Zbar(3,vz(2,2),vz(2,3)).eq.0).and.(Zbar(3,vz(vx,2),vz(vx,3)).eq.0)) then
                        cycle loop98
                      end if
                    end if
                  end if
                  A1 = 0
                  if (vx.GT.2) then
                    do h = 1,4
                      Xvz(1,h) = vz(vx,h)
                      Xvz(vx+2,h) = vz(1,h)
                    end do
                    do i = 1,vx
                      do h = 1,4
                        Xvz(i+1,h) = vz(i,h)
                      end do
                    end do
                    C1 = 0
                    z1 = 0
                    loop71: do i = 1,vx    !Loop through points
                      pint(vz(i,1))=1
                      k1 = Xvz(i,2)
                      k2 = Xvz(i+1,2)
                      k3 = Xvz(i+2,2)
                      j1 = Xvz(i,3)
                      j2 = Xvz(i+1,3)
                      j3 = Xvz(i+2,3)
                      i1 = Xvz(i,4)
                      i2 = Xvz(i+1,4)
                      i3 = Xvz(i+2,4)
                      t2 = 1
                      if (j2.gt.1) t2=2
                      z3 = 0
!                      write(6,*) k,j,i,vx,vz(i,1),z1 !!!!!!!!!!!!!
                      if (k3.ne.0) z3 = Zbar(1,k3,j3)
                      if (vz(i,2).EQ.0) then
                        A1 = A1 + 1
                        z1 = 0
                        cycle loop71
                      end if
                      if (Zbar(1,k2,j2).EQ.3) then
                        if ((k3.eq.0).or.((k2.eq.k3).and.(j2.eq.j3))) then
                          A1 = A1 + 1
                          z1 = 1
                        elseif (TEND(t2,i2,k2,1).eq.0) then
                          A1 = A1 + 1
                          z1 = 2
                        elseif (Zbar(1,k3,j3).ne.1) then
                          A1 = A1 + 1
                          z1 = 3
                        else
                          z1 = 4
                        end if
                        A1 = A1 + 1
                        cycle loop71
                      end if
                      if (Zbar(1,k2,j2).EQ.2) then
                        if ((k3.eq.0).or.((k2.eq.k3).and.(j2.eq.j3))) then
                          A1 = A1 + 1
                          z1 = 5
                        elseif (TEND(t2,i2,k2,1).eq.0) then
                          A1 = A1 + 1
                          z1 = 6
                        elseif (Zbar(1,k3,j3).ne.1) then
                          A1 = A1 + 1
                          z1 = 7
                        else
                          z1 = 8
                        end if
                        cycle loop71
                      end if
                      if (Zbar(1,k2,j2).EQ.1) then
                        if (vx.eq.i) then
                          if (C1.ne.1) then
                            if ((k1.eq.k2).and.(j1.eq.j2)) then
                              A1 = A1 + 1
                              z1 = 9
                              cycle loop71
                            end if
                          else
                            z1 = 10
                            cycle loop71
                          end if
                        elseif ((k2.eq.k3).and.(j2.eq.j3)) then
                          z1 = 11
                          cycle loop71
                        elseif (k3.eq.0) then
                          if ((k1.eq.k2).and.(j1.eq.j2)) then
                            if (i.eq.1) C1 = 1
                            A1 = A1 + 1
                            z1 = 12
                            cycle loop71
                          end if
                        elseif(TEND(t2,i2,k2,1).eq.0) then
                          z1 = 13
                          if ((k1.eq.k2).and.(j1.eq.j2)) then
                            if (i.eq.1) C1 = 1
                            A1 = A1 + 1
                            cycle loop71
                          end if
                        elseif (Zbar(1,k3,j3).eq.1) then
                          z1 = 14
                          cycle loop71
                        else
                          if ((k1.eq.k2).and.(j1.eq.j2)) then
                            if (i.eq.1) C1 = 1
                            A1 = A1 + 1
                            z1 = 15
                            cycle loop71
                          end if
                        end if
                        A1 = A1 + 1
                        z1 = 16
                        if (Zbar(1,k1,j1).NE.1) then
                          cycle loop71
                        end if
                        nNADD = nNADD + 1
                        B1 = i
                        h = 1
                        loop61: do while (Zbar(1,k1,j1).EQ.1)
                          h = h + 1
                          B1 = B1 - 1
                          if (B1.eq.0) then
                            B1 = vx
                            C1 = 1
                          end if
                          z1 = 17
                          if (((k1.eq.k2).and.(j1.eq.j2)).or.(h.eq.vx)) exit loop61
                          k1 = Xvz(B1,2)
                          j1 = Xvz(B1,3)
                          i1 = Xvz(B1,4)
                          k2 = Xvz(B1+1,2)
                          j2 = Xvz(B1+1,3)
                          i2 = Xvz(B1+1,4)
                          z1 = 18
                        end do loop61
                      end if
                    end do loop71
                    if (A1.GT.2) then
                      do h = 3,A1
                        nEADD = nEADD + 1
                      end do
                    end if
                  end if
                end if
!                write(6,*) k,j,vx,A1,nNADD,nEADD
              end do loop98
            end if
          end do loop99
!          read(5,*)

          TEND(:,1,:,4) = 0
          write(6,*) 'End Allocations Determined'

!-----+-----BUILD ALLOCATIONS-------+---------+---------+---------+---------+---------+---------+---------+

          Znp = np + nNADD
          Zne = ne + nEADD
          allocate (ppt(10))
          allocate (Zxyd(3,Znp))
          allocate (Znm(Zne,3))
          allocate (Xpropn(4,Znp))
          allocate (Xprope(3,Zne))
          allocate (Xnvell(Znbou),Xibtype(Znbou))
          allocate( Xnbvv(Znbou,nvel_max), Xibconn(Znbou,nvel_max), Xbar(3,Znbou,nvel_max) )
          Zxyd(:,:) = 0
          Znm(:,:) = 0
          Xpropn(:,:) = 0
          Xprope(:,:) = 0
          Xnvell(:) = 0
          Xibtype(:) = 0
          if (ifill.eq.1) then

          do k = 1,np
            do j = 1,3
              Zxyd(j,k) = xyd(j,k)
            end do
          end do

          do k = 1,ne
            do j = 1,3
              Znm(k,j) = nm(k,j)
            end do
          end do
          Yne = ne
          nNADD = np + Ynp
          write(6,*) 'Allocations Built'

!-----+-----REMOVE LEVEE--+---------+---------+---------+---------+---------+---------+---------+---------+
! Build Interior Elements
          do k = 1,nbou                                                        ! Loop through Boundaries
            if ((ibtype(k).EQ.4).OR.(ibtype(k).EQ.24)) then                    ! Internal boundary?
              do j = 2,nvell(k)                                                ! Loop through internal levee squares
                if ((Zbar(3,k,j).EQ.1).OR.(Zbar(3,k,j-1).EQ.1)) then           ! Insert Elements
                  if (j.EQ.2) then                                       ! Determine Condition of Left End
                    if (Zbar(1,k,1).EQ.1) then                           ! NARROW levee
                      Xpropn(1,nbvv(k,1)) = 1                          ! Removed
                      Xpropn(1,ibconn(k,1)) = 1                        ! Removed
                      Xpropn(3,nbvv(k,1)) = Zbar(2,k,1)                ! Replacement Node
                      Xpropn(3,ibconn(k,1)) = Zbar(2,k,1)              ! Replacement Node
                      do i = 1,3
                        Zxyd(i,Zbar(2,k,1)) = 0.5*(Zxyd(i,nbvv(k,1))+Zxyd(i,ibconn(k,1)))
                      end do
                    end if
                  end if
                  if (Zbar(1,k,j).EQ.1) then
                    Xpropn(1,nbvv(k,j)) = 1                          ! Removed
                    Xpropn(1,ibconn(k,j)) = 1                        ! Removed
                    Xpropn(3,nbvv(k,j)) = Zbar(2,k,j)                ! Replacement Node
                    Xpropn(3,ibconn(k,j)) = Zbar(2,k,j)              ! Replacement Node
                    do i = 1,3
                      Zxyd(i,Zbar(2,k,j)) = 0.5*(Zxyd(i,nbvv(k,j))+Zxyd(i,ibconn(k,j)))
                    end do
                  end if
                  write(c1a,'(i1)') Zbar(1,k,j-1)
                  write(c1b,'(i1)') Zbar(1,k,j)
                  c2 = c1a//c1b
                  read(c2,'(i2)') ibox
                  select case(ibox)
                    case (12)
                      Znm(Yne+1,1) = Zbar(2,k,j-1)
                      Znm(Yne+1,2) = nbvv(k,j)
                      Znm(Yne+1,3) = ibconn(k,j)
                      Yne = Yne + 1
                    case (13)
                      do i = 1,3
                        Zxyd(i,Zbar(2,k,j)) = 0.5*(Zxyd(i,nbvv(k,j))+Zxyd(i,ibconn(k,j)))
                      end do
                      Znm(Yne+1,1) = Zbar(2,k,j-1)
                      Znm(Yne+1,2) = nbvv(k,j)
                      Znm(Yne+1,3) = Zbar(2,k,j)
                      Znm(Yne+2,1) = Zbar(2,k,j-1)
                      Znm(Yne+2,2) = Zbar(2,k,j)
                      Znm(Yne+2,3) = ibconn(k,j)
                      Yne = Yne + 2
                    case (21)
                      Znm(Yne+1,1) = nbvv(k,j-1)
                      Znm(Yne+1,2) = Zbar(2,k,j)
                      Znm(Yne+1,3) = ibconn(k,j-1)
                      Yne = Yne + 1
                    case (22)
                      D1 = (xyd(1,nbvv(k,j-1))-xyd(1,ibconn(k,j)))**2+&
                           (xyd(2,nbvv(k,j-1))-xyd(2,ibconn(k,j)))**2
                      G1 = (xyd(1,nbvv(k,j))-xyd(1,ibconn(k,j-1)))**2+&
                           (xyd(2,nbvv(k,j))-xyd(2,ibconn(k,j-1)))**2
                      if (D1.LE.G1) then
                        Znm(Yne+1,1) = nbvv(k,j-1)
                        Znm(Yne+1,2) = nbvv(k,j)
                        Znm(Yne+1,3) = ibconn(k,j)
                        Znm(Yne+2,1) = ibconn(k,j-1)
                        Znm(Yne+2,2) = nbvv(k,j-1)
                        Znm(Yne+2,3) = ibconn(k,j)
                      else
                        Znm(Yne+1,1) = nbvv(k,j-1)
                        Znm(Yne+1,2) = nbvv(k,j)
                        Znm(Yne+1,3) = ibconn(k,j-1)
                        Znm(Yne+2,1) = ibconn(k,j-1)
                        Znm(Yne+2,2) = nbvv(k,j)
                        Znm(Yne+2,3) = ibconn(k,j)
                      end if
                      Yne = Yne + 2
                    case (23)
                      if (Zbar(1,k,j+1).EQ.3) then
                        do i = 1,3
                          Zxyd(i,Zbar(2,k,j)) = (Zxyd(i,nbvv(k,j-1))+Zxyd(i,ibconn(k,j-1))+2*Zxyd(i,nbvv(k,j))+ &
                          & 2*Zxyd(i,ibconn(k,j)))/6
                        end do
                        Znm(Yne+1,1) = nbvv(k,j-1)
                        Znm(Yne+1,2) = nbvv(k,j)
                        Znm(Yne+1,3) = Zbar(2,k,j)
                        Znm(Yne+2,1) = nbvv(k,j-1)
                        Znm(Yne+2,2) = Zbar(2,k,j)
                        Znm(Yne+2,3) = ibconn(k,j-1)
                        Znm(Yne+3,1) = ibconn(k,j-1)
                        Znm(Yne+3,2) = Zbar(2,k,j)
                        Znm(Yne+3,3) = ibconn(k,j)
                        Znm(Yne+4,1) = Zbar(2,k,j)
                        Znm(Yne+4,2) = nbvv(k,j)
                        Znm(Yne+4,3) = Zbar(2,k,j+1)
                        Znm(Yne+5,1) = Zbar(2,k,j)
                        Znm(Yne+5,2) = Zbar(2,k,j+1)
                        Znm(Yne+5,3) = ibconn(k,j)
                        Yne = Yne + 5
                      else
                        do i = 1,3
                          Zxyd(i,Zbar(2,k,j)) = 0.5*(Zxyd(i,nbvv(k,j))+Zxyd(i,ibconn(k,j)))
                        end do
                        Znm(Yne+1,1) = nbvv(k,j-1)
                        Znm(Yne+1,2) = nbvv(k,j)
                        Znm(Yne+1,3) = Zbar(2,k,j)
                        Znm(Yne+2,1) = nbvv(k,j-1)
                        Znm(Yne+2,2) = Zbar(2,k,j)
                        Znm(Yne+2,3) = ibconn(k,j-1)
                        Znm(Yne+3,1) = ibconn(k,j-1)
                        Znm(Yne+3,2) = Zbar(2,k,j)
                        Znm(Yne+3,3) = ibconn(k,j)
                        Yne = Yne + 3
                      end if
                    case (31)
                      Znm(Yne+1,1) = nbvv(k,j-1)
                      Znm(Yne+1,2) = Zbar(2,k,j)
                      Znm(Yne+1,3) = Zbar(2,k,j-1)
                      Znm(Yne+2,1) = Zbar(2,k,j-1)
                      Znm(Yne+2,2) = Zbar(2,k,j)
                      Znm(Yne+2,3) = ibconn(k,j-1)
                      Yne = Yne + 2
                    case (32)
                      Znm(Yne+1,1) = nbvv(k,j-1)
                      Znm(Yne+1,2) = nbvv(k,j)
                      Znm(Yne+1,3) = Zbar(2,k,j-1)
                      Znm(Yne+2,1) = Zbar(2,k,j-1)
                      Znm(Yne+2,2) = nbvv(k,j)
                      Znm(Yne+2,3) = ibconn(k,j)
                      Znm(Yne+3,1) = Zbar(2,k,j-1)
                      Znm(Yne+3,2) = ibconn(k,j)
                      Znm(Yne+3,3) = ibconn(k,j-1)
                      Yne = Yne + 3
                    case (33)
                      if (Zbar(1,k,j+1).EQ.3) then
                        do i = 1,3
                          Zxyd(i,Zbar(2,k,j)) = 0.25*(Zxyd(i,nbvv(k,j-1))+Zxyd(i,ibconn(k,j-1))+Zxyd(i,nbvv(k,j))+ &
                          & Zxyd(i,ibconn(k,j)))
                        end do
                        Znm(Yne+1,1) = nbvv(k,j-1)
                        Znm(Yne+1,2) = nbvv(k,j)
                        Znm(Yne+1,3) = Zbar(2,k,j)
                        Znm(Yne+2,1) = ibconn(k,j-1)
                        Znm(Yne+2,2) = Zbar(2,k,j)
                        Znm(Yne+2,3) = ibconn(k,j)
                        Znm(Yne+3,1) = Zbar(2,k,j)
                        Znm(Yne+3,2) = nbvv(k,j)
                        Znm(Yne+3,3) = Zbar(2,k,j+1)
                        Znm(Yne+4,1) = Zbar(2,k,j)
                        Znm(Yne+4,2) = Zbar(2,k,j+1)
                        Znm(Yne+4,3) = ibconn(k,j)
                        Yne = Yne + 4
                      else
                        do i = 1,3
                          Zxyd(i,Zbar(2,k,j)) = (Zxyd(i,nbvv(k,j-1))+Zxyd(i,ibconn(k,j-1))+2*Zxyd(i,nbvv(k,j))+ &
                          & 2*Zxyd(i,ibconn(k,j)))/6
                        end do
                        Znm(Yne+1,1) = nbvv(k,j-1)
                        Znm(Yne+1,2) = nbvv(k,j)
                        Znm(Yne+1,3) = Zbar(2,k,j)
                        Znm(Yne+2,1) = ibconn(k,j-1)
                        Znm(Yne+2,2) = Zbar(2,k,j)
                        Znm(Yne+2,3) = ibconn(k,j)
                        Yne = Yne + 2
                      end if
                  end select
                end if
              end do
            end if
          end do
          write(6,*) 'Interior Elements Created'

! Build Exterior Elements
          pint(:)=0
          loop89: do k = 1,nbou
            if ((ibtype(k).EQ.4).OR.(ibtype(k).EQ.24)) then                        ! Internal boundary?
              loop88: do j=1,nvell(k),nvell(k)-1          ! Left/Right
                if (j.EQ.1) then
                  if (Zbar(3,k,1).EQ.0) cycle loop88
                else
                  if (Zbar(3,k,nvell(k)).EQ.0) exit loop88
                end if
                if (pint(nbvv(k,j)).eq.0) then
                  j1 = 1
                  if (j.eq.nvell(k)) j1 = 2
!                  if ((TEND(j1,1,k,1).EQ.0).and.(TEND(j1,2,k,1).EQ.0)) then
!                    if ((ConV(nbvv(k,j),1).EQ.2).OR.(ConV(ibconn(k,j),1).EQ.2)) then
!                      cycle loop88 !Borders Boundary Section
!                    end if
!                  end if
                  pint(nbvv(k,j)) = 1
                  pint(ibconn(k,j))=1
                  vx = 1                          ! Number of Connections
                  vz(:,:) = 0                     ! Connected Boundary (k), L/R (j), nbvv/ibconn (i)
                  vz(vx,1) = nbvv(k,j)            ! start with nbvv
                  vz(vx,2) = k
                  vz(vx,3) = j                    ! start with left side
                  vz(vx,4) = 1                    ! start with nbvv
                  loop87: do                       ! Loop through intersection vertices
                    if (vx.GT.9) then             ! If too many points, assume no connections
                      vz(:,:) = 0
                      vx = 2
                      vz(1,1) = nbvv(k,j)
                      vz(1,2) = k
                      vz(1,3) = j
                      vz(1,4) = 1
                      vz(2,1) = ibconn(k,j)
                      vz(2,2) = k
                      vz(2,3) = j
                      vz(2,4) = 2
                      exit loop87
                    end if
                    k1 = vz(vx,2)
                    j1 = vz(vx,3)
                    i1 = vz(vx,4)
                    if (j1.GT.1) j1 = 2
                    if ((TEND(j1,i1,k1,1).NE.0).AND.(k1.NE.0)) then   ! Connected to another Boundary
                      k2 = TEND(j1,i1,k1,1)
                      j2 = TEND(j1,i1,k1,2)
                      i2 = TEND(j1,i1,k1,3)
                      vx = vx +1
                      vz(vx,2) = k2
                      vz(vx,3) = j2
                      if (j2.NE.1) vz(vx,3) = nvell(k2)
                      vz(vx,4) = 1
                      if (i2.EQ.1) vz(vx,4) = 2
                      vz(vx,1) = nbvv(vz(vx,2),vz(vx,3))
                      if (i2.EQ.1) vz(vx,1) = ibconn(vz(vx,2),vz(vx,3))
                    else                                    ! No Direct Levee Connection
                      if (k1.EQ.0) then                     ! Second+ Loop, Set pivot points
                        h = z1                             ! Temporary node pt storage
                        z1 = z2
                        z2 = z4
                        z4 = h
                      else                                  ! First Loop, set pivot points
                        z4 = 0
                        if (j1.EQ.1) then
                          if (i1.EQ.1) then
                            z1 = nbvv(k1,1)
                            z2 = nbvv(k1,2)
                          else
                            z1 = ibconn(k1,1)
                            z2 = ibconn(k1,2)
                          end if
                        else
                          if (i1.EQ.1) then
                            z1 = nbvv(k1,nvell(k1))
                            z2 = nbvv(k1,nvell(k1)-1)
                          else
                            z1 = ibconn(k1,nvell(k1))
                            z2 = ibconn(k1,nvell(k1)-1)
                          end if
                        end if
                      end if
                      nloops = 0
                      loop86: do                        ! Loop through adj tris
                        z3 = 0
                        nloops = nloops + 1
                        if (nloops.GT.10) then         ! Max 10 Loops
                          write(6,*) 'Problem , endless loop'
                          write(6,*) nbvv(k,j)
                          read(5,*)
                          stop
                        end if
                        if (z1.NE.0) then
                          loop85: do m = 2,ConV(z1,1)
                            h = ConV(z1,m)
                            if ((nm(h,1).EQ.z1).AND.(nm(h,2).EQ.z2).AND.(nm(h,3).NE.z4)) z3 = nm(h,3)
                            if ((nm(h,1).EQ.z1).AND.(nm(h,3).EQ.z2).AND.(nm(h,2).NE.z4)) z3 = nm(h,2)
                            if ((nm(h,2).EQ.z1).AND.(nm(h,1).EQ.z2).AND.(nm(h,3).NE.z4)) z3 = nm(h,3)
                            if ((nm(h,2).EQ.z1).AND.(nm(h,3).EQ.z2).AND.(nm(h,1).NE.z4)) z3 = nm(h,1)
                            if ((nm(h,3).EQ.z1).AND.(nm(h,1).EQ.z2).AND.(nm(h,2).NE.z4)) z3 = nm(h,2)
                            if ((nm(h,3).EQ.z1).AND.(nm(h,2).EQ.z2).AND.(nm(h,1).NE.z4)) z3 = nm(h,1)
                          end do loop85
                        end if
                        if (z3.NE.0) then
                          z4 = z2
                          z2 = z3
                        else
                          vx = vx + 1
                          vz(vx,1) = z2
                          vz(vx,2) = 0
                          vz(vx,3) = 0
                          vz(vx,4) = 0
                          loop84: do h = 1,nbou         ! Loop through boundaries to node on levee
                            select case(ibtype(h))
                              case(4, 24)
                                if (nbvv(h,1).EQ.z2) then
                                  vz(vx,2) = h
                                  vz(vx,3) = 1
                                  vz(vx,4) = 1
                                elseif (ibconn(h,1).EQ.z2) then
                                  vz(vx,2) = h
                                  vz(vx,3) = 1
                                  vz(vx,4) = 2
                                elseif (nbvv(h,nvell(h)).EQ.z2) then
                                  vz(vx,2) = h
                                  vz(vx,3) = nvell(h)
                                  vz(vx,4) = 1
                                elseif (ibconn(h,nvell(h)).EQ.z2) then
                                  vz(vx,2) = h
                                  vz(vx,3) = nvell(h)
                                  vz(vx,4) = 2
                                end if
                            end select
                          end do loop84
                          if ((vz(vx,1).NE.ibconn(k,j)).AND.(vz(vx,2).NE.0)) then !Node is on new Levee
                            vx = vx + 1
                            vz(vx,2) = vz(vx-1,2)
                            vz(vx,3) = vz(vx-1,3)
                            vz(vx,4) = 1
                            if (vz(vx-1,4).EQ.1) vz(vx,4) = 2
                            vz(vx,1) = nbvv(vz(vx,2),vz(vx,3))
                            if (vz(vx,4).EQ.2) vz(vx,1) = ibconn(vz(vx,2),vz(vx,3))
                          end if
                          exit loop86
                        end if
                      end do loop86
                    end if
                    if (vz(vx,1).EQ.ibconn(k,j)) exit loop87 !Not necesary to do other side of levee
                  end do loop87
                  if ((Zbar(1,k,j).eq.2).and.(vx.eq.4).and.(vz(1,2).ne.vz(vx,2))) then
                    if ((vz(2,2).ne.0).and.(vz(vx,2).ne.0).and.(vz(2,2).ne.vz(3,2))) then
                      if ((Zbar(3,vz(2,2),vz(2,3)).eq.0).and.(Zbar(3,vz(vx,2),vz(vx,3)).eq.0)) then
                        k2 = vz(2,2)
                        j2 = vz(2,3)
                        if (j2.NE.1) j2 = nvell(k2)
                        i2 = vz(2,4)
                        k3 = vz(vx,2)
                        j3 = vz(vx,3)
                        if (j3.NE.1) j3 = nvell(k3)
                        i3 = vz(vx,4)
                        if (j2.gt.1) then
                          if (i2.ne.i3) then
                            nbvv(k2,j2+1) = nbvv(k3,j3)
                            ibconn(k2,j2+1) = ibconn(k3,j3)
                          else
                            nbvv(k2,j2+1) = ibconn(k3,j3)
                            ibconn(k2,j2+1) = nbvv(k3,j3)
                          end if
                          bar(1,k2,j2+1) = bar(1,k3,j3)
                          bar(2,k2,j2+1) = bar(2,k3,j3)
                          bar(3,k2,j2+1) = bar(3,k3,j3)
                        else
                          do h = 1,nvell(k2)
                            nbvv(k2,nvell(k2)-h+2)=nbvv(k2,nvell(k2)-h+1)
                            ibconn(k2,nvell(k2)-h+2)=ibconn(k2,nvell(k2)-h+1)
                            bar(1,k2,nvell(k2)-h+2) = bar(1,k2,nvell(k2)-h+1)
                            bar(2,k2,nvell(k2)-h+2) = bar(2,k2,nvell(k2)-h+1)
                            bar(3,k2,nvell(k2)-h+2) = bar(3,k2,nvell(k2)-h+1)
                          end do
                          if (i2.ne.i3) then
                            nbvv(k2,1) = nbvv(k3,j3)
                            ibconn(k2,1) = ibconn(k3,j3)
                          else
                            nbvv(k2,1) = ibconn(k3,j3)
                            ibconn(k2,1) = nbvv(k3,j3)
                          end if
                          bar(1,k2,1) = bar(1,k3,1)
                          bar(2,k2,1) = bar(2,k3,1)
                          bar(3,k2,1) = bar(3,k3,1)
                        end if
                        nvell(k2) = nvell(k2) + 1
                        cycle loop88
                      end if
                    end if
                  end if
                  A1 = 0
                  Xvz(:,:)=0
                  if (vx.GT.2) then
                    do h = 1,4
                      Xvz(1,h) = vz(vx,h)
                      Xvz(vx+2,h) = vz(1,h)
                    end do
                    do i = 1,vx
                      do h = 1,4
                        Xvz(i+1,h) = vz(i,h)
                      end do
                    end do
                    C1 = 0
                    z1 = 0
                    loop79: do i = 1,vx    !Loop through points
                      pint(vz(i,1))=1
                      k1 = Xvz(i,2)
                      k2 = Xvz(i+1,2)
                      k3 = Xvz(i+2,2)
                      j1 = Xvz(i,3)
                      j2 = Xvz(i+1,3)
                      j3 = Xvz(i+2,3)
                      i1 = Xvz(i,4)
                      i2 = Xvz(i+1,4)
                      i3 = Xvz(i+2,4)
                      t2 = 1
                      if (j2.gt.1) t2=2
                      z3 = 0
!                      write(6,*) k,j,i,vx,vz(i,1),z1 !!!!!!!!!!!!!!!!!!!!!!
                      if (k3.ne.0) z3 = Zbar(1,k3,j3)
                      if (vz(i,2).EQ.0) then
                        A1 = A1 + 1
                        ppt(A1) = vz(i,1)
                        z1 = 1
                        cycle loop79
                      end if
                      if (Zbar(1,k2,j2).EQ.3) then
                        if ((k3.eq.0).or.((k2.eq.k3).and.(j2.eq.j3))) then
                          A1 = A1 + 1
                          ppt(A1)=vz(i,1)
                          z1 = 2
                        elseif (TEND(t2,i2,k2,1).eq.0) then
                          A1 = A1 + 1
                          ppt(A1)=vz(i,1)
                          z1 = 3
                        elseif (Zbar(1,k3,j3).ne.1) then
                          A1 = A1 + 1
                          ppt(A1)=vz(i,1)
                          z1 = 4
                        else
                          z1 = 5
                        end if
                        A1 = A1 + 1
                        ppt(A1)=Zbar(2,k2,j2)
                        cycle loop79
                      end if
                      if (Zbar(1,k2,j2).EQ.2) then
                        if ((k3.eq.0).or.((k2.eq.k3).and.(j2.eq.j3))) then
                          A1 = A1 + 1
                          ppt(A1)=vz(i,1)
                          z1 = 6
                        elseif (TEND(t2,i2,k2,1).eq.0) then
                          A1 = A1 + 1
                          ppt(A1)=vz(i,1)
                          z1 = 7
                        elseif (Zbar(1,k3,j3).ne.1) then
                          A1 = A1 + 1
                          ppt(A1)=vz(i,1)
                          z1 = 8
                        else
                          z1 = 9
                        end if
                        cycle loop79
                      end if
                      if (Zbar(1,k2,j2).EQ.1) then
                        if (i.eq.vx) then
                          if (C1.ne.1) then
                            if ((k1.eq.k2).and.(j1.eq.j2)) then
                              A1 = A1 + 1
                              ppt(A1)=Zbar(2,k2,j2)
                              z1 = 10
                              cycle loop79
                            end if
                          else
                            cycle loop79
                          end if
                        elseif ((k2.eq.k3).and.(j2.eq.j3)) then
                          z1 = 11
                          cycle loop79
                        elseif (k3.eq.0) then
                          if ((k1.eq.k2).and.(j1.eq.j2)) then
                            if (i.eq.1) C1 = 1
                            A1 = A1 + 1
                            ppt(A1)=Zbar(2,k2,j2)
                            z1 = 12
                            cycle loop79
                          end if
                        elseif (TEND(t2,i2,k2,1).eq.0) then
                          z1 = 13
                          if ((k1.eq.k2).and.(j1.eq.j2)) then
                            if (i.eq.1) C1 = 1
                            A1 = A1 + 1
                            ppt(A1) = Zbar(2,k2,j2)
                            cycle loop79
                          end if
                        elseif (Zbar(1,k3,j3).eq.1) then
                          z1 = 14
                          cycle loop79
                        else
                          if ((k1.eq.k2).and.(j1.eq.j2)) then
                            if (i.eq.1) C1 = 1
                            A1 = A1 + 1
                            ppt(A1)=Zbar(2,k2,j2)
                            z1 = 15
                            cycle loop79
                          end if
                        end if
                        z1 = 16
                        A1 = A1 + 1
                        if (Zbar(1,k1,j1).NE.1) then
                          ppt(A1) = Zbar(2,k2,j2)
                          cycle loop79
                        end if
                        nNADD = nNADD + 1
                        ppt(A1) = nNADD
                        Intx = Zxyd(1,Zbar(2,k2,j2))
                        Inty = Zxyd(2,Zbar(2,k2,j2))
                        Intz = Zxyd(3,Zbar(2,k2,j2))
                        B1 = i
                        h = 1
                        loop81: do while (Zbar(1,k1,j1).EQ.1)
                          Intx = Intx + Zxyd(1,Zbar(2,k1,j1))
                          Inty = Inty + Zxyd(2,Zbar(2,k1,j1))
                          Intz = Intz + Zxyd(3,Zbar(2,k1,j1))
                          Xpropn(1,Zbar(2,k2,j2))=1
                          Xpropn(3,Zbar(2,k2,j2))=nNADD
                          Xpropn(1,Zbar(2,k1,j1))=1
                          Xpropn(3,Zbar(2,k1,j1))=nNADD
                          h = h + 1
                          B1 = B1 - 1
                          if (B1.eq.0) then
                            B1 = vx
                            C1 = 1
                          end if
                          z1 = 17
!                          write(6,*) k1,k2,h !!!!!!!!!!!!!!!!!!
                          if (((k1.eq.k2).and.(j1.eq.j2)).or.(h.eq.vx)) exit loop81
                          k1 = Xvz(B1,2)
                          j1 = Xvz(B1,3)
                          i1 = Xvz(B1,4)
                          k2 = Xvz(B1+1,2)
                          j2 = Xvz(B1+1,3)
                          i2 = Xvz(B1+1,4)
!                          write(6,*) k1,j1,k2,j2,Zbar(1,k1,j1)        !!!!!!!!!!!!!!!!!
                          z1 = 18
                        end do loop81
                        Zxyd(1,nNADD) = Intx/h
                        Zxyd(2,nNADD) = Inty/h
                        Zxyd(3,nNADD) = Intz/h
                      end if
                    end do loop79
!                    write(6,*) z1                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  end if
                  if (vx.EQ.2) then
                    if (j.EQ.1) then
                      if (Zbar(1,k,1).EQ.1) then
                        do h = 1,Zne
                          if ((nbvv(k,1).EQ.Znm(h,1)).and.(ibconn(k,1).EQ.Znm(h,2))) Xprope(1,h) = 1
                          if ((nbvv(k,1).EQ.Znm(h,1)).and.(ibconn(k,1).EQ.Znm(h,3))) Xprope(1,h) = 1
                          if ((nbvv(k,1).EQ.Znm(h,2)).and.(ibconn(k,1).EQ.Znm(h,1))) Xprope(1,h) = 1
                          if ((nbvv(k,1).EQ.Znm(h,2)).and.(ibconn(k,1).EQ.Znm(h,3))) Xprope(1,h) = 1
                          if ((nbvv(k,1).EQ.Znm(h,3)).and.(ibconn(k,1).EQ.Znm(h,1))) Xprope(1,h) = 1
                          if ((nbvv(k,1).EQ.Znm(h,3)).and.(ibconn(k,1).EQ.Znm(h,2))) Xprope(1,h) = 1
                        end do
                        if ((TEND(1,1,k,1).eq.TEND(1,2,k,1)).and.(TEND(1,1,k,1).ne.0).and.(TEND(1,1,k,2).eq.TEND(1,2,k,2))) then
                          k2 = TEND(1,1,k,1)
                          j2 = TEND(1,1,k,2)
                          if (j2.eq.2) j2 = nvell(k2)
                          pint(nbvv(k2,j2)) = 1
                          pint(ibconn(k2,j2))=1
                          Xpropn(1,Zbar(2,k2,j2))=1
                          Xpropn(3,Zbar(2,k2,j2))=Zbar(2,k,1)
                        end if
                      else
                        if (RTEND(1,1,k,1).ne.0) then
                          k2 = RTEND(1,1,k,1)
                          j2 = RTEND(1,1,k,2)
                          B1 = ibconn(k,j)
                          if (ibtype(k2).ne.0) then
                            k2 = RTEND(1,2,k,1)
                            j2 = RTEND(1,2,k,2)
                            B1 = nbvv(k,j)
                            if (ibtype(k2).eq.52) then
                              k2 = RTEND(1,1,k,1)
                              j2 = RTEND(1,1,k,2)
                              B1 = ibconn(k,j)
                            end if
                           end if
                          if (j2.gt.1) then
                            nbvv(k2,nvell(k2)+1) = B1
                            bar(1,k2,nvell(k2)+1) = bar(1,k2,nvell(k2))
                            bar(2,k2,nvell(k2)+1) = bar(2,k2,nvell(k2))
                          else
                            do h = 1,nvell(k2)
                              nbvv(k2,nvell(k2)-h+2)=nbvv(k2,nvell(k2)-h+1)
                              bar(1,k2,nvell(k2)-h+2) = bar(1,k2,nvell(k2)-h+1)
                              bar(2,k2,nvell(k2)-h+2) = bar(2,k2,nvell(k2)-h+1)
                            end do
                            nbvv(k2,1) = B1
                            bar(1,k2,1) = bar(1,k2,2)
                            bar(2,k2,1) = bar(2,k2,2)
                          end if
                          nvell(k2) = nvell(k2) + 1
                        end if
                      end if
                    else
                      if (Zbar(1,k,nvell(k)).EQ.1) then
                        do h = 1,Zne
                          if ((nbvv(k,nvell(k)).EQ.Znm(h,1)).and.(ibconn(k,nvell(k)).EQ.Znm(h,2))) Xprope(1,h) = 1
                          if ((nbvv(k,nvell(k)).EQ.Znm(h,1)).and.(ibconn(k,nvell(k)).EQ.Znm(h,3))) Xprope(1,h) = 1
                          if ((nbvv(k,nvell(k)).EQ.Znm(h,2)).and.(ibconn(k,nvell(k)).EQ.Znm(h,1))) Xprope(1,h) = 1
                          if ((nbvv(k,nvell(k)).EQ.Znm(h,2)).and.(ibconn(k,nvell(k)).EQ.Znm(h,3))) Xprope(1,h) = 1
                          if ((nbvv(k,nvell(k)).EQ.Znm(h,3)).and.(ibconn(k,nvell(k)).EQ.Znm(h,1))) Xprope(1,h) = 1
                          if ((nbvv(k,nvell(k)).EQ.Znm(h,3)).and.(ibconn(k,nvell(k)).EQ.Znm(h,2))) Xprope(1,h) = 1
                        end do
                        if ((TEND(2,1,k,1).eq.TEND(2,2,k,1)).and.(TEND(2,1,k,1).ne.0).and.(TEND(2,1,k,2).eq.TEND(2,2,k,2))) then
                          k2 = TEND(2,1,k,1)
                          j2 = TEND(2,1,k,2)
                          if (j2.eq.2) j2 = nvell(k2)
                          pint(nbvv(k2,j2)) = 1
                          pint(ibconn(k2,j2))=1
                          Xpropn(1,Zbar(2,k2,j2))=1
                          Xpropn(3,Zbar(2,k2,j2))=Zbar(2,k,nvell(k))
                        end if
                      else
                        if (RTEND(2,1,k,1).ne.0) then
                          k2 = RTEND(2,1,k,1)
                          j2 = RTEND(2,1,k,2)
                          B1 = ibconn(k,j)
                          if (ibtype(k2).ne.0) then
                            k2 = RTEND(2,2,k,1)
                            j2 = RTEND(2,2,k,2)
                            B1 = nbvv(k,j)
                            if (ibtype(k2).eq.52) then
                              k2 = RTEND(2,1,k,1)
                              j2 = RTEND(2,1,k,2)
                              B1 = ibconn(k,j)
                            end if
                          end if
                          if (j2.gt.1) then
                            nbvv(k2,nvell(k2)+1) = B1
                            bar(1,k2,nvell(k2)+1) = bar(1,k2,nvell(k2))
                            bar(2,k2,nvell(k2)+1) = bar(2,k2,nvell(k2))
                          else
                            do h = 1,nvell(k2)
                             nbvv(k2,nvell(k2)-h+2)=nbvv(k2,nvell(k2)-h+1)
                             bar(1,k2,nvell(k2)-h+2) = bar(1,k2,nvell(k2)-h+1)
                             bar(2,k2,nvell(k2)-h+2) = bar(2,k2,nvell(k2)-h+1)
                            end do
                            nbvv(k2,1) = B1
                            bar(1,k2,1) = bar(1,k2,2)
                            bar(2,k2,1) = bar(2,k2,2)
                          end if
                          nvell(k2) = nvell(k2) + 1
                        end if
                      end if
                    end if
                  elseif (A1.eq.1) then
                    do i = 1,vx-1
                      do C1=i+1,vx
                        do h = 1,Zne
                          if ((vz(i,1).EQ.Znm(h,1)).and.(vz(C1,1).EQ.Znm(h,2))) Xprope(1,h) = 1
                          if ((vz(i,1).EQ.Znm(h,1)).and.(vz(C1,1).EQ.Znm(h,3))) Xprope(1,h) = 1
                          if ((vz(i,1).EQ.Znm(h,2)).and.(vz(C1,1).EQ.Znm(h,1))) Xprope(1,h) = 1
                          if ((vz(i,1).EQ.Znm(h,2)).and.(vz(C1,1).EQ.Znm(h,3))) Xprope(1,h) = 1
                          if ((vz(i,1).EQ.Znm(h,3)).and.(vz(C1,1).EQ.Znm(h,1))) Xprope(1,h) = 1
                          if ((vz(i,1).EQ.Znm(h,3)).and.(vz(C1,1).EQ.Znm(h,2))) Xprope(1,h) = 1
                        end do
                      end do
                    end do
                  elseif (A1.GT.2) then
                    do h = 3,A1
                      Znm(Yne+h-2,1) = ppt(1)
                      Znm(Yne+h-2,2) = ppt(h-1)
                      Znm(Yne+h-2,3) = ppt(h)
                    end do
                      Yne = Yne + A1-2
                  end if
                end if
!                write(6,*) k,j,nNADD-np,Yne-ne,vx,A1
!                read(5,*)                                  !!!!!!!!!!!!!!!!!
              end do loop88
            end if
          end do loop89
          write(6,*) 'End Elements Created'

!-----+-----INFORM NODE,ELEMENT, AND BOUNDARY DATA------+---------+---------+---------+---------+---------+

          Xnp = 0                                                        ! NEW # of Nodes
          Xne = 0                                                        ! NEW # of Elements
! Build New Node Table
          do k = 1,Znp                                                   ! Loop through existing Nodes
            if (Xpropn(1,k).NE.1) then                                   ! Node Not Removed?
              Xnp = Xnp + 1                                              ! +Xnp
              Xpropn(4,k) = Xnp                                          ! Assign NEW Node #
            end if
          end do
          allocate (Xxyd(3,Xnp))
          do k = 1,Znp                                                   ! Loop through existing Nodes
            if (Xpropn(1,k).NE.1) then                                   ! Node Not Removed?
              do j = 1,3
               Xxyd(j,Xpropn(4,k)) = Zxyd(j,k)                          ! +Xnp
              end do
            end if
          end do
          write(6,*) 'Node Table Built'

! Build New Element Table
          do k = 1,Zne                                                   ! Loop through existing Nodes
            if (Xprope(1,k).NE.1) then                                   ! Node Not Removed?
              Xne = Xne + 1                                              ! +Xne
              Xprope(3,k) = Xne                                          ! Assign NEW Element #
            end if
          end do
          allocate (Xnm(Xne,3))
          do k = 1,Zne                                                   ! Loop through existing Nodes
            if (Xprope(1,k).NE.1) then                                   ! Node Not Removed?
              do j = 1,3
                Xnm(Xprope(3,k),j) = Znm(k,j)                          ! +Xnp
              end do
            end if
          end do
          write(6,*) 'Element Table Built'

! Fix Node Numbers in Element Table
          k2 = 0
          do k = 1,Xne
            do j = 1,3
              h = Xnm(k,j)
              if (Xpropn(1,h).EQ.0) then
                Xnm(k,j) = Xpropn(4,h)
              else if (Xpropn(1,Xpropn(3,h)).EQ.0) then
                Xnm(k,j) = Xpropn(4,Xpropn(3,h))
              else if (Xpropn(1,Xpropn(3,Xpropn(3,h))).EQ.0) then
                Xnm(k,j) = Xpropn(4,Xpropn(3,Xpropn(3,h)))
              else
                write(6,*) 'oops', k, j, h
                read(5,*)
                stop
              end if
!              write(6,*) h, Xnm(k,j)
!              read(5,*)
            end do
            D1 = (Xxyd(1,Xnm(k,2))-Xxyd(1,Xnm(k,1)))*(Xxyd(2,Xnm(k,3))-Xxyd(2,Xnm(k,1))) &
                & - (Xxyd(1,Xnm(k,3))-Xxyd(1,Xnm(k,1)))*(Xxyd(2,Xnm(k,2))-Xxyd(2,Xnm(k,1)))
            if (D1.LT.0) then ! Fix Nodal Orientation
              j1 = Xnm(k,1)
              Xnm(k,1) = Xnm(k,3)
              Xnm(k,3) = j1
              k2 = k2 + 1
            end if
          end do
          write(6,*) 'Element Table Corrected'

  else
    Xne = ne
    Xnp = np
    allocate( Xxyd(3,np) )
    allocate( Xnm(ne,3) )
    Xxyd = xyd
    Xnm = nm

  end if

! Fix Nope List
        allocate ( Xnvdll(nope) )
        allocate ( Xnbdv(nope,nvdl_max) )
        do k = 1,nope
          Xnvdll(k) = nvdll(k)
          do j = 1,nvdll(k)
            Xnbdv(k,j) = nbdv(k,j)
            if (ifill.eq.1) then
              if ((Xpropn(1,Xnbdv(k,j))).NE.1) then
                Xnbdv(k,j) = Xpropn(4,Xnbdv(k,j))
              else if (Xpropn(1,Xpropn(3,Xnbdv(k,j))).NE.1) then
                  Xnbdv(k,j) = Xpropn(4,Xpropn(3,Xnbdv(k,j)))
              else if (Xpropn(1,Xpropn(3,Xpropn(3,Xnbdv(k,j)))).NE.1) then
                  Xnbdv(k,j) = Xpropn(4,Xpropn(3,Xpropn(3,Xnbdv(k,j))))
              else
                write(6,*) 'oops'
                read(5,*)
                stop
              end if
            end if
          end do
        end do

! Fix Boundary List
        Xnbou = 0
        do k = 1, nbou
          select case(ibtype(k))
            case(0,1,2,10,11,12,20,21,22,30,52)
              Xnvell(Xnbou+1) = nvell(k)
              Xibtype(Xnbou+1) = ibtype(k)
              do j = 1, nvell(k)
                Xnbvv(Xnbou+1,j) = nbvv(k,j)
                if (ifill.eq.1) then
                  if ((Xpropn(1,Xnbvv(Xnbou+1,j))).NE.1) then
                    Xnbvv(Xnbou+1,j) = Xpropn(4,Xnbvv(Xnbou+1,j))
                  else if (Xpropn(1,Xpropn(3,Xnbvv(Xnbou+1,j))).NE.1) then
                      Xnbvv(Xnbou+1,j) = Xpropn(4,Xpropn(3,Xnbvv(Xnbou+1,j)))
                  else if (Xpropn(1,Xpropn(3,Xpropn(3,Xnbvv(Xnbou+1,j)))).NE.1) then
                      Xnbvv(Xnbou+1,j) = Xpropn(4,Xpropn(3,Xpropn(3,Xnbvv(Xnbou+1,j))))
                  else
                    write(6,*) 'oops'
                    read(5,*)
                    stop
                  end if
                end if
              enddo
              Xnbou = Xnbou + 1
            case(3, 13, 23)
              Xnvell(Xnbou+1) = nvell(k)
              Xibtype(Xnbou+1) = ibtype(k)
              do j = 1, nvell(k)
                Xnbvv(Xnbou+1,j) = nbvv(k,j)
                if (ifill.eq.1) then
                  if ((Xpropn(1,Xnbvv(Xnbou+1,j))).NE.1) then
                    Xnbvv(Xnbou+1,j) = Xpropn(4,Xnbvv(Xnbou+1,j))
                  else if (Xpropn(1,Xpropn(3,Xnbvv(Xnbou+1,j))).NE.1) then
                      Xnbvv(Xnbou+1,j) = Xpropn(4,Xpropn(3,Xnbvv(Xnbou+1,j)))
                  else if (Xpropn(1,Xpropn(3,Xpropn(3,Xnbvv(Xnbou+1,j)))).NE.1) then
                      Xnbvv(Xnbou+1,j) = Xpropn(4,Xpropn(3,Xpropn(3,Xnbvv(Xnbou+1,j))))
                  else
                    write(6,*) 'oops'
                    read(5,*)
                    stop
                  end if
                end if
                Xbar(1,Xnbou+1,j) = bar(1,k,j)
                Xbar(2,Xnbou+1,j) = bar(2,k,j)
              enddo
              Xnbou = Xnbou + 1
            case(4,24) !4,24
              h = 1
              do j = 1, nvell(k)
!2                if (Zbar(3,k,j).EQ.0) then
                A1 = Zbar(3,k,j)
                z1 = Zbar(3,k,1)
                z2 = Zbar(3,k,2)
                B1 = Zbar(3,k,nvell(k))
                C1 = Zbar(3,k,nvell(k)-1)
                E1 = nvell(k)
                if (((A1.EQ.0).and.(j.ne.E1).and.(j.ne.1)).or.((j.eq.E1).and.(C1.eq.0).and.(B1.eq.0)))then
                  Xnbvv(Xnbou+1,h) = nbvv(k,j)
                  Xibconn(Xnbou+1,h) = ibconn(k,j)
                  if (ifill.eq.1) then
                    if ((Xpropn(1,Xnbvv(Xnbou+1,h))).NE.1) then
                      Xnbvv(Xnbou+1,h) = Xpropn(4,Xnbvv(Xnbou+1,h))
                    else if (Xpropn(1,Xpropn(3,Xnbvv(Xnbou+1,h))).NE.1) then
                      Xnbvv(Xnbou+1,h) = Xpropn(4,Xpropn(3,Xnbvv(Xnbou+1,h)))
                    else if (Xpropn(1,Xpropn(3,Xpropn(3,Xnbvv(Xnbou+1,h)))).NE.1) then
                      Xnbvv(Xnbou+1,h) = Xpropn(4,Xpropn(3,Xpropn(3,Xnbvv(Xnbou+1,h))))
                    else
                      write(6,*) 'oops'
                      read(5,*)
                      stop
                    end if
                    if ((Xpropn(1,Xibconn(Xnbou+1,h))).NE.1) then
                      Xibconn(Xnbou+1,h) = Xpropn(4,Xibconn(Xnbou+1,h))
                    else if (Xpropn(1,Xpropn(3,Xibconn(Xnbou+1,h))).NE.1) then
                      Xibconn(Xnbou+1,h) = Xpropn(4,Xpropn(3,Xibconn(Xnbou+1,h)))
                    else if (Xpropn(1,Xpropn(3,Xpropn(3,Xibconn(Xnbou+1,h)))).NE.1) then
                      Xnbvv(Xnbou+1,h) = Xpropn(4,Xpropn(3,Xpropn(3,Xibconn(Xnbou+1,h))))
                    else
                      write(6,*) 'oops'
                      read(5,*)
                      stop
                    end if
                  end if
                  Xbar(1,Xnbou+1,h) = bar(1,k,j)
                  Xbar(2,Xnbou+1,h) = bar(2,k,j)
                  Xbar(3,Xnbou+1,h) = bar(3,k,j)
                  Xibtype(Xnbou+1) = ibtype(k)
                  Xnvell(Xnbou+1) = h
                  h = h + 1
                  if (j.NE.nvell(k)) then
                    if (Zbar(3,k,j+1).EQ.1) then
                      h = 1
                      Xnbou = Xnbou + 1
                    end if
                  else
                    Xnbou = Xnbou + 1
                  end if
                else if ((j.eq.1).and.(z1.eq.0).and.(z2.eq.0)) then
                  Xnbvv(Xnbou+1,h) = nbvv(k,j)
                  Xibconn(Xnbou+1,h) = ibconn(k,j)
                  if (ifill.eq.1) then
                    if ((Xpropn(1,Xnbvv(Xnbou+1,h))).NE.1) then
                      Xnbvv(Xnbou+1,h) = Xpropn(4,Xnbvv(Xnbou+1,h))
                    else if (Xpropn(1,Xpropn(3,Xnbvv(Xnbou+1,h))).NE.1) then
                      Xnbvv(Xnbou+1,h) = Xpropn(4,Xpropn(3,Xnbvv(Xnbou+1,h)))
                    else if (Xpropn(1,Xpropn(3,Xpropn(3,Xnbvv(Xnbou+1,h)))).NE.1) then
                      Xnbvv(Xnbou+1,h) = Xpropn(4,Xpropn(3,Xpropn(3,Xnbvv(Xnbou+1,h))))
                    else
                      write(6,*) 'oops'
                      read(5,*)
                      stop
                    end if
                    if ((Xpropn(1,Xibconn(Xnbou+1,h))).NE.1) then
                      Xibconn(Xnbou+1,h) = Xpropn(4,Xibconn(Xnbou+1,h))
                    else if (Xpropn(1,Xpropn(3,Xibconn(Xnbou+1,h))).NE.1) then
                      Xibconn(Xnbou+1,h) = Xpropn(4,Xpropn(3,Xibconn(Xnbou+1,h)))
                    else if (Xpropn(1,Xpropn(3,Xpropn(3,Xibconn(Xnbou+1,h)))).NE.1) then
                      Xnbvv(Xnbou+1,h) = Xpropn(4,Xpropn(3,Xpropn(3,Xibconn(Xnbou+1,h))))
                    else
                      write(6,*) 'oops'
                      read(5,*)
                      stop
                    end if
                  end if
                  Xbar(1,Xnbou+1,h) = bar(1,k,j)
                  Xbar(2,Xnbou+1,h) = bar(2,k,j)
                  Xbar(3,Xnbou+1,h) = bar(3,k,j)
                  Xibtype(Xnbou+1) = ibtype(k)
                  Xnvell(Xnbou+1) = h
                  h = h + 1
                  if (j.NE.nvell(k)) then
                    if (Zbar(3,k,j+1).EQ.1) then
                      h = 1
                      Xnbou = Xnbou + 1
                    end if
                  else
                    Xnbou = Xnbou + 1
                  end if
                end if
              end do
           end select
        end do
        write(6,*) 'Boundary Table Built'

        write(6,*)
          write(6,*) 'Preparing to Output'
          write(6,*)
          call output_grid( Xne, Xnp, nope, Xnbou, nvdl_max, nvel_max, &
                            project_name, gridname, agrid, Xxyd, Xnm, Xnvdll, &
                            Xnbdv, Xnvell, Xibtype, Xnbvv, Xibconn, Xbar, neta )

! Prepare files to diagnose problems
      open(60,file='Znp.txt', status='unknown')
      do i = 1, Znp
        write(60,561) i, (Zxyd(j,i), j=1,3), (Xpropn(k,i), k=1,4)
      enddo
 561  format(i10,2f18.10,e18.10,4i10)
     close(60)
      open(70,file='Znm.txt', status='unknown')
      do i = 1, Zne
        write(70,571) i,(Znm(i,j), j=1,3), (Xprope(k,i), k=1,3)
      enddo
 571  format(7i10)
      close(70)
      open(80,file='nbou.txt', status='unknown')
      do k = 1, nbou
        select case(ibtype(k))
          case(4, 24)
            do j = 1, nvell(k)
              write(80,581) k, j, nbvv(k,j), ibconn(k,j), (Zbar(i,k,j), i=1,3)
            end do
        end select
      end do
 581  format(7i10)
      close(80)

!! AMASS TRIANGLES
        case(4)

          ntri = 0
          allocate( pint(np) )
          allocate( tri(ne,3) )
          allocate( isel(ne) )
          allocate( ConV(np,20) )
          ConV(:,:) = 0
          write(6,*) 'Preparing Search Data'
          do k = 1,ne
            do j = 1,3
              h = nm(k,j)
              i = 2
              do while (ConV(h,i).NE.0)
                i = i + 1
              end do
              ConV(h,1) = i
              ConV(h,i) = k
            end do
          end do
          write(6,*) 'Amassing Triangles'
          do k = 1,nbou
            select case(ibtype(k))
              case(4, 24)
                do j = 1, nvell(k)
                  if ((nprop(nbvv(k,j)).EQ.1).OR.(nprop(ibconn(k,j)).EQ.1)) then    !If Inside Target Area
                    do m = 2,ConV(nbvv(k,j),1)
                      i = ConV(nbvv(k,j),m)
                      if (isel(i).NE.1) then      ! Add all triangles containing
                        ntri = ntri + 1
                        tri(ntri,1) = nm(i,1)
                        tri(ntri,2) = nm(i,2)
                        tri(ntri,3) = nm(i,3)
                        isel(i)=1
                      end if
                    end do
                    do m = 2,ConV(ibconn(k,j),1)
                      i = ConV(ibconn(k,j),m)
                      if (isel(i).NE.1) then      ! Add all triangles containing
                        ntri = ntri + 1
                        tri(ntri,1) = nm(i,1)
                        tri(ntri,2) = nm(i,2)
                        tri(ntri,3) = nm(i,3)
                        isel(i)=1
                      end if
                    end do
                    if ((j.NE.1).and.(j.NE.nvell(k))) then                ! Add Triangles between Levee
                      if ((nprop(nbvv(k,j-1)).NE.1).OR.(nprop(ibconn(k,j-1)).NE.1)) then    !Left in Target?
                        ntri = ntri + 1
                        tri(ntri,1) = nbvv(k,j)
                        tri(ntri,2) = nbvv(k,j-1)
                        tri(ntri,3) = ibconn(k,j-1)
                        ntri = ntri + 1
                        tri(ntri,1) = nbvv(k,j)
                        tri(ntri,2) = ibconn(k,j-1)
                        tri(ntri,3) = ibconn(k,j)
                      end if
                      ntri = ntri + 1
                      tri(ntri,1) = nbvv(k,j)
                      tri(ntri,2) = ibconn(k,j)
                      tri(ntri,3) = nbvv(k,j+1)
                      ntri = ntri + 1
                      tri(ntri,1) = nbvv(k,j+1)
                      tri(ntri,2) = ibconn(k,j)
                      tri(ntri,3) = ibconn(k,j+1)
                    else if ((j.EQ.1).OR.(j.EQ.nvell(k))) then           !At Ends, Add triangles
                      if (j.EQ.1) then                                   !At Start, Add triangle
                        ntri = ntri + 1
                        tri(ntri,1) = nbvv(k,j)
                        tri(ntri,2) = ibconn(k,j)
                        tri(ntri,3) = nbvv(k,j+1)
                        ntri = ntri + 1
                        tri(ntri,1) = nbvv(k,j+1)
                        tri(ntri,2) = ibconn(k,j)
                        tri(ntri,3) = ibconn(k,j+1)
                      else                                                !At Finish, Add Triangles
                        if ((nprop(nbvv(k,j-1)).NE.1).OR.(nprop(ibconn(k,j-1)).NE.1)) then    !Left in Target?
                          ntri = ntri + 1
                          tri(ntri,1) = nbvv(k,j)
                          tri(ntri,2) = nbvv(k,j-1)
                          tri(ntri,3) = ibconn(k,j-1)
                          ntri = ntri + 1
                          tri(ntri,1) = nbvv(k,j)
                          tri(ntri,2) = ibconn(k,j-1)
                          tri(ntri,3) = ibconn(k,j)
                        end if
                      end if
                      if (pint(nbvv(k,j)).eq.0) then
                      pint(nbvv(k,j)) = 1
                      pint(ibconn(k,j))=1
                      vx = 1                          ! Number of Connections
                      vz(:,:) = 0                     ! Connected Boundary (k), L/R (j), nbvv/ibconn (i)
                      vz(vx,1) = nbvv(k,j)            ! start with nbvv
                      vz(vx,2) = k
                      vz(vx,3) = j                    ! start with left side
                      vz(vx,4) = 1                    ! start with nbvv
                      loop15: do                       ! Loop through intersection vertices
                        if (vx.GT.9) then             ! If too many points, assume no connections
                          vz(:,:) = 0
                          vx = 2
                          vz(1,1) = nbvv(k,j)
                          vz(1,2) = k
                          vz(1,3) = j
                          vz(1,4) = 1
                          vz(2,1) = ibconn(k,j)
                          vz(2,2) = k
                          vz(2,3) = j
                          vz(2,4) = 2
                          write(6,*) 'CHECK', k, j
                          exit loop15
                        end if
                        k1 = vz(vx,2)
                        j1 = vz(vx,3)
                        i1 = vz(vx,4)
                        if (j1.GT.1) j1 = 2
                        if ((TEND(j1,i1,k1,1).NE.0).AND.(k1.NE.0)) then   ! Connected to another Boundary
                          k2 = TEND(j1,i1,k1,1)
                          j2 = TEND(j1,i1,k1,2)
                          i2 = TEND(j1,i1,k1,3)
                          vx = vx +1
                          vz(vx,2) = k2
                          vz(vx,3) = j2
                          if (j2.NE.1) vz(vx,3) = nvell(k2)
                          vz(vx,4) = 1
                          if (i2.EQ.1) vz(vx,4) = 2
                          vz(vx,1) = nbvv(vz(vx,2),vz(vx,3))
                          if (i2.EQ.1) vz(vx,1) = ibconn(vz(vx,2),vz(vx,3))
                        else                                    ! No Direct Levee Connection
                          if (k1.EQ.0) then                     ! Second+ Loop, Set pivot points
                             h = z1                             ! Temporary node pt storage
                            z1 = z2
                            z2 = z4
                            z4 = h
                          else                                  ! First Loop, set pivot points
                            z4 = 0
                            if (j1.EQ.1) then
                              if (i1.EQ.1) then
                                z1 = nbvv(k1,1)
                                z2 = nbvv(k1,2)
                              else
                                z1 = ibconn(k1,1)
                                z2 = ibconn(k1,2)
                              end if
                            else
                              if (i1.EQ.1) then
                                z1 = nbvv(k1,nvell(k1))
                                z2 = nbvv(k1,nvell(k1)-1)
                              else
                                z1 = ibconn(k1,nvell(k1))
                                z2 = ibconn(k1,nvell(k1)-1)
                              end if
                            end if
                          end if
                          nloops = 0
                          loop16: do                        ! Loop through adj tris
                            z3 = 0
                            nloops = nloops + 1
                            if (nloops.GT.10) then         ! Max 10 Loops
                              write(6,*) 'Problem , endless loop'
                              write(6,*) nbvv(k,j)
                              read(5,*)
                              stop
                            end if
                            if (z1.NE.0) then
                              loop17: do m = 2,ConV(z1,1)
                                h = ConV(z1,m)
                                if ((nm(h,1).EQ.z1).AND.(nm(h,2).EQ.z2).AND.(nm(h,3).NE.z4)) z3 = nm(h,3)
                                if ((nm(h,1).EQ.z1).AND.(nm(h,3).EQ.z2).AND.(nm(h,2).NE.z4)) z3 = nm(h,2)
                                if ((nm(h,2).EQ.z1).AND.(nm(h,1).EQ.z2).AND.(nm(h,3).NE.z4)) z3 = nm(h,3)
                                if ((nm(h,2).EQ.z1).AND.(nm(h,3).EQ.z2).AND.(nm(h,1).NE.z4)) z3 = nm(h,1)
                                if ((nm(h,3).EQ.z1).AND.(nm(h,1).EQ.z2).AND.(nm(h,2).NE.z4)) z3 = nm(h,2)
                                if ((nm(h,3).EQ.z1).AND.(nm(h,2).EQ.z2).AND.(nm(h,1).NE.z4)) z3 = nm(h,1)
                              end do loop17
                            end if
                            if (z3.NE.0) then
                              z4 = z2
                              z2 = z3
                            else
                              vx = vx + 1
                              vz(vx,1) = z2
                              vz(vx,2) = 0
                              vz(vx,3) = 0
                              vz(vx,4) = 0
                              loop18: do h = 1,nbou         ! Loop through boundaries to node on levee
                                select case(ibtype(h))
                                  case(4, 24)
                                    if (nbvv(h,1).EQ.z2) then
                                      vz(vx,2) = h
                                      vz(vx,3) = 1
                                      vz(vx,4) = 1
                                    elseif (ibconn(h,1).EQ.z2) then
                                      vz(vx,2) = h
                                      vz(vx,3) = 1
                                      vz(vx,4) = 2
                                    elseif (nbvv(h,nvell(h)).EQ.z2) then
                                      vz(vx,2) = h
                                      vz(vx,3) = nvell(h)
                                      vz(vx,4) = 1
                                    elseif (ibconn(h,nvell(h)).EQ.z2) then
                                      vz(vx,2) = h
                                      vz(vx,3) = nvell(h)
                                      vz(vx,4) = 2
                                    end if
                                end select
                              end do loop18
                              if ((vz(vx,1).NE.ibconn(k,j)).AND.(vz(vx,2).NE.0)) then !Node is on new Levee
                                vx = vx + 1
                                vz(vx,2) = vz(vx-1,2)
                                vz(vx,3) = vz(vx-1,3)
                                vz(vx,4) = 1
                                if (vz(vx-1,4).EQ.1) vz(vx,4) = 2
                                vz(vx,1) = nbvv(vz(vx,2),vz(vx,3))
                                if (vz(vx,4).EQ.2) vz(vx,1) = ibconn(vz(vx,2),vz(vx,3))
                              end if
                              exit loop16
                            end if
                          end do loop16
                        end if
                        if (vz(vx,1).EQ.ibconn(k,j)) exit loop15 !Not necesary to do other side of levee
                      end do loop15
                      if (vx.GT.2) then
                        do i = 2,vx-1                      ! Loop through outer triangles and add to list
                          pint(vz(i,1))=1
                          ntri = ntri + 1
                          tri(ntri,1) = vz(1,1)
                          tri(ntri,2) = vz(i+1,1)
                          tri(ntri,3) = vz(i,1)
                        end do
                      end if
                      end if
                    end if
                  end if
                end do
            end select
          end do
! Fix Ordering of Triangles
          write(6,*) 'Fixing order of Triangles'
          do i = 1,ntri
            t1 = (xyd(1,tri(i,2)) - xyd(1,tri(i,1))) * (xyd(2,tri(i,3)) - xyd(2,tri(i,1))) &
              & - (xyd(1,tri(i,3)) - xyd(1,tri(i,1))) * (xyd(2,tri(i,2)) - xyd(2,tri(i,1)))
            if (t1.GT.0) then ! Fix Nodal Orientation
              t2 = tri(i,3)
              tri(i,3) = tri(i,2)
              tri(i,2) = t2
            end if
          end do
          allocate( iSide(ntri,3) )
          nDoub = 0
          iSide(:,:) = 0
          call tribuild(gridname,ntri,tri,xyd,ne,np)
! Loop through triangles and identify shared triangle sides
        write(6,*) 'Identifying Shared Sides'
        do i=1,ntri-1                  !Loop through Each Triangle
          do j=1,3                     !Loop through Each Side
            if (iSide(i,j).NE.1) then    !If Side has not been accounted for
              s1a = tri(i,j)
              if (j.EQ.3) then
               s1b = tri(i,1)
              else
               s1b = tri(i,j+1)
              end if
              loop23: do k=i+1,ntri
                loop25: do h = 1,3
                  if (iSide(k,h).NE.1) then
                    s2a = tri(k,h)
                    if (h.EQ.3) then
                      s2b = tri(k,1)
                    else
                      s2b = tri(k,h+1)
                    end if
                    if (((s1a.EQ.s2a).AND.(s1b.EQ.s2b)).OR.((s1a.EQ.s2b).AND.(s1b.EQ.s2a)))  then
                      iSide(k,h) = 1
                      iSide(i,j) = 1
                      exit loop23
                    end if
                  end if
                end do loop25
              end do loop23
            end if
          end do
        end do
! Count number of interior triangle sides
        write(6,*) 'Counting Sides'
        do i = 1,ntri
          do j = 1,3
            if (iSide(i,j).EQ.1) then
              nDoub = nDoub + 1
            end if
          end do
        end do
! Store Sides
        write(6,*) 'Storing Sides'
        h = ntri*3-nDoub
        allocate( sid(h,4) )
        allocate( ppi(h,2) )
        nsid = 0
        do i=1,ntri                      !Loop through Each Triangle
          do j=1,3                       !Loop through Each Side
            if (iSide(i,j).NE.1) then    !If Side has not been accounted for
              nsid = nsid+1
              s1a = tri(i,j)
              if (j.EQ.3) then
                s1b = tri(i,1)
              else
                s1b = tri(i,j+1)
              end if
              sid(nsid,1) = xyd(1,s1a)
              sid(nsid,2) = xyd(2,s1a)
              sid(nsid,3) = xyd(1,s1b)
              sid(nsid,4) = xyd(2,s1b)
            end if
          end do
        end do
! Identify points and polygons
        write(6,*) 'Identifying Polygon Points'
        deallocate(isel)
        allocate( isel(nsid) )
        isel(:)=0
        npoly = 0
        do i = 1,nsid-1
          if (isel(i).NE.1) then
            k = i
            npoly = npoly + 1
            ppi(i,1) = npoly
            ppi(i,2) = 1
            nloops = 0
            loop21: do while ((isel(i).EQ.0).AND.(nloops.LE.100))
              loop24: do j = 1,nsid
                if ((j.NE.k).and.(isel(j).NE.1)) then
                  if ((sid(k,1).EQ.sid(j,1)).AND.(sid(k,2).EQ.sid(j,2))) then
                    s1a = sid(j,1)
                    s1b = sid(j,2)
                    sid(j,1) = sid(j,3)
                    sid(j,2) = sid(j,4)
                    sid(j,3) = s1a
                    sid(j,4) = s1b
                  end if
                  if ((sid(k,1).EQ.sid(j,3)).AND.(sid(k,2).EQ.sid(j,4))) then
                    ppi(j,1) = ppi(k,1)
                    ppi(j,2) = ppi(k,2)+1
                    isel(j) = 1
                    k = j
                  exit loop24
                  end if
                end if
              end do loop24
              if (j.eq.nsid) nloops = nloops + 1
              if (nloops.EQ.99) then
                write(6,*) 'Problem: Endless Loop'
                write(6,*) i
                write(6,*) sid(k,1)
                write(6,*) sid(k,2)
                read(5,*)
                stop
              end if
            end do loop21
          end if
        end do
! Determine Number of Points in Each Polygon
        write(6,*) 'Counting Polygons'
        isel(:) = 0
        allocate( npts(npoly) )
        npts(:) = 0
        ptmax = 0
        do i=1,nsid
          if (npts(ppi(i,1)).LT.ppi(i,2)) then
            npts(ppi(i,1)) = ppi(i,2)
          end if
          if (ppi(i,2).GT.ptmax) then
            ptmax = ppi(i,2)
          end if
        end do
 ! Build Polgyons
        write(6,*) 'Building polygons'
        allocate( plg(npoly, ptmax,2) )
        do i=1,nsid
          plg(ppi(i,1),ppi(i,2),1) = sid(i,1)
          plg(ppi(i,1),ppi(i,2),2) = sid(i,2)
        end do
        do i=1,npoly
          plg(i,1,1)=plg(i,npts(i),1)
          plg(i,1,2)=plg(i,npts(i),2)
        end do
        pts = nsid + npoly
        call polybuild(gridname,plg,pts,npts,npoly,ptmax)
        write(6,*) npoly, 'Polygons Built'
      end select

   end subroutine levee_editor
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine fort13_editor(fort14, project_name, fort13 )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!
      implicit none
      character(120), intent(in) :: fort14, fort13, project_name
!
      character(120) :: agrid, gridname, fort1314
      integer :: ne, np, nope, nbou, nvdl_max, nvel_max, neta, nvel, nodemax
      integer :: i, j, k, n, imethod, icase
      integer, allocatable :: nm(:,:), nsequencer(:), nvdll(:), nbdv(:,:)
      integer, allocatable :: nvell(:), nbvv(:,:), ibtype(:), ibconn(:,:), nprop(:)
      double precision :: ival1,ival2,ival3
      double precision, allocatable :: xyd(:,:), bar(:,:,:)
      character(120) :: fortfort, ft
!
      integer :: ane, anp
      double precision, allocatable  :: axyd(:,:)
      integer,          allocatable  :: anm(:,:)
      integer :: anope, aneta, anvdl_max
      integer,          allocatable  :: anvdll(:), anbdv(:,:)
      integer :: anbou, anvel, anvel_max
      integer,          allocatable  :: anvell(:), aibtype(:),  anbvv(:,:), aibconn(:,:)
      double precision, allocatable :: abar(:,:,:)
      integer :: anodemax
      integer, allocatable :: ansequencer(:)
!
      nope = 0
      neta = 0
      nbou = 0
      nvdl_max = 0
      nvel_max = 0
      ft='01-01'
      fort1314=project_name(1:len_trim(project_name))//'-sub13-'//ft(1:len_trim(ft))//'.14'
      open(14,file=fort1314,status='old',action='read')
        call read1314_alloc ( 14, ne, np, nodemax )
      close(14)
      allocate(xyd(3,np),nm(ne,3),nsequencer(nodemax),nvdll(nope),nbdv(nope,nvdl_max),nvell(nbou))
      allocate( ibtype(nbou),nbvv(nbou,nvel_max), ibconn(nbou,nvel_max), bar(3,nbou,nvel_max) )
      write(6,*)
      write(6,*)
      write(6,*)
      write(6,*) '*Fort.13 Editor*'
      write(6,*)
      write(6,*) 'Choose Method to adjust nodal attribute data'
      write(6,*) ' 0. Unique Value'
      write(6,*) ' 1. Closest of Two Values'
      write(6,*) ' 2. Count Flags'
      write(6,*) ' 3. Match Flagged to Nearest Node Value'
      write(6,*) ' 4. Adjust to Bathymmetry'
      read(5,*) imethod
      write(6,*)
!
      select case(imethod)
        case(0,1,2,4)
          write(6,*) 'Enter Nodal File to Modify, e.g. 02-01'
          read(5,*) ft
          fort1314=project_name(1:len_trim(project_name))//'-sub13-'//ft(1:len_trim(ft))//'.14'
          gridname=fort1314(1:len_trim(fort1314))//'-ORIG.14'
          open(14,file=fort1314,status='old',action='read')
            call read1314 ( 14, ne, np, nodemax, nsequencer, agrid, xyd, nm )
          close(14)
          if (imethod.ne.2) then
            call output_grid( ne, np, nope, nbou, nvdl_max, nvel_max, project_name, gridname, &
                              agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar, neta )
          end if
      end select
      select case(imethod)
        case(0)
          write(6,*) 'Enter unique value'
          read(5,*) ival1
          write(6,*)
          do k = 1,np
            xyd(3,k) = ival1
          end do
        case(1)
          write(6,*) 'Enter low value'
          read(5,*) ival1
          write(6,*)
          write(6,*) 'Enter high value'
          read(5,*) ival2
          write(6,*)
          do k = 1,np
            ival3 = (xyd(3,k)-ival1)/(ival2-ival1)
            xyd(3,k) = ival1
            if (ival3.gt.0.5) xyd(3,k) = ival2
          end do
        case(2)
          n = 0
          do k = 1,np
            if (xyd(3,k).lt.-900000) n = n + 1
          end do
          write(6,*) n, 'Flags'
        case(4)
!Open(14), for dynamic allocation
      open(44,file=fort14,status='old',action='read')
         call read14_alloc ( 44, ane, anp, anope, anbou, anvdl_max, anvel_max, anodemax )
      close(44)
! Dymanic Memory Allocation for Global Grid
         allocate( axyd(3,anp) )
         allocate( anm(ane,3) )
         allocate( anvdll(anope)  )
         allocate( anbdv(anope,anvdl_max) )
         allocate( anvell(anbou), aibtype(anbou)  )
         allocate( anbvv(anbou,anvel_max), aibconn(anbou,anvel_max), abar(3,anbou,anvel_max) )
         allocate( ansequencer(anodemax) )
!Re-open & read global grid data(14)
      open(14,file=fort14,status='old',action='read')
         call read14 ( 14, ane, anp, anope, anbou, anvdl_max, anvel_max, anodemax, ansequencer, &
                       agrid, axyd, anm, aneta, anvdll, anbdv, anvel, anvell, aibtype, anbvv, &
                       aibconn, abar )
          write(6,*) 'Enter low bathymmetry value'
          read(5,*) ival1
          write(6,*)
          write(6,*) 'Enter high bathmmetry value'
          read(5,*) ival2
          write(6,*)
          write(6,*) 'Enter fort13 value'
          read(5,*) ival3
          write(6,*)
          do k = 1,np
            if ((axyd(3,k).ge.ival1).and.(axyd(3,k).le.ival2)) xyd(3,k) = ival3
          end do
      end select
      select case(imethod)
        case(0,1,4)
          call output_grid( ne, np, nope, nbou, nvdl_max, nvel_max, project_name, fort1314, &
                         agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar, neta )
        case(3)
          write(6,*) 'Choose Method'
          write(6,*) ' 0. Specific Nodal File'
          write(6,*) ' 1. All Nodal Files'
          read(5,*) icase
          select case(icase)
            case(0)
              write(6,*) 'Enter Nodal File to Modify, e.g. 02-01'
              read(5,*) ft
              fort1314=project_name(1:len_trim(project_name))//'-sub13-'//ft(1:len_trim(ft))//'.14'
              gridname=fort1314(1:len_trim(fort1314))//'-ORIG.14'
              open(14,file=fort1314,status='old',action='read')
                call read1314 ( 14, ne, np, nodemax, nsequencer, agrid, xyd, nm )
              close(14)
              call output_grid( ne, np, nope, nbou, nvdl_max, nvel_max, project_name, gridname, &
                                agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar, neta )
              do k = 1,np
                if (xyd(3,k).lt.-900000) then
                  ival1 = (xyd(1,k)-xyd(1,1))**2+(xyd(2,k)-xyd(2,1))**2
                  n = 1
                  if (k.eq.1) ival1 = (xyd(1,k)-xyd(1,2))**2+(xyd(2,k)-xyd(2,2))**2
                  if (k.eq.1) n = 2
                  do j = n,np
                    ival2 = (xyd(1,k)-xyd(1,j))**2+(xyd(2,k)-xyd(2,j))**2
                    if ((j.ne.k).and.(ival2.lt.ival1).and.(xyd(3,j).gt.-9000000)) then
                      ival1 = ival2
                      xyd(3,k) = xyd(3,j)
                      n = j
                    end if
                  end do
                end if
              end do
              call output_grid( ne, np, nope, nbou, nvdl_max, nvel_max, project_name, fort1314, &
                                agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar, neta )
            case(1)
      fortfort='01-0102-0103-0104-0105-0106-0106-0206-0306-0406-0506-0606-0706-0806-0906-1006-1106-1207-0108-01'

              do i = 1,19
                ft = fortfort(5*(i-1)+1:5*(i-1)+5)
                fort1314=project_name(1:len_trim(project_name))//'-sub13-'//ft(1:len_trim(ft))//'.14'
                gridname=fort1314(1:len_trim(fort1314))//'-ORIG.14'
                open(14,file=fort1314,status='old',action='read')
                  call read1314 ( 14, ne, np, nodemax, nsequencer, agrid, xyd, nm )
                close(14)
                write(6,*)
                call output_grid( ne, np, nope, nbou, nvdl_max, nvel_max, project_name, gridname, &
                                  agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar, neta )
                do k = 1,np
                  if (xyd(3,k).lt.-900000) then
                    ival1 = (xyd(1,k)-xyd(1,1))**2+(xyd(2,k)-xyd(2,1))**2
                    n = 1
                    if (k.eq.1) ival1 = (xyd(1,k)-xyd(1,2))**2+(xyd(2,k)-xyd(2,2))**2
                    if (k.eq.1) n = 2
                    do j = n,np
                      ival2 = (xyd(1,k)-xyd(1,j))**2+(xyd(2,k)-xyd(2,j))**2
                        if ((j.ne.k).and.(ival2.lt.ival1).and.(xyd(3,j).gt.-900000)) then
                        ival1 = ival2
                        xyd(3,k) = xyd(3,j)
                        n = j
                      end if
                    end do
                  end if
                end do
                call output_grid( ne, np, nope, nbou, nvdl_max, nvel_max, project_name, fort1314, &
                                  agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar, neta )
              end do
          end select
      end select
      write(6,*) '..Finished!'
      write(6,*)
      write(6,*) 'Press Enter to Exit'
      read(5,*)

   end subroutine fort13_editor
!
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine bath_editor(fort14, project_name, fort13)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!
      implicit none
      character(120), intent(in) :: fort14, fort13, project_name
!
      character(120) :: agrid, gridname
      integer :: ne, np, nope, nbou, nvdl_max, nvel_max, neta, nvel, nodemax
      integer :: i, j, k, n, imethod, icase
      integer, allocatable :: nm(:,:), nsequencer(:), nvdll(:), nbdv(:,:)
      integer, allocatable :: nvell(:), nbvv(:,:), ibtype(:), ibconn(:,:), nprop(:)
      double precision :: ivalue
      double precision, allocatable :: xyd(:,:), bar(:,:,:)
!
      gridname = project_name(1:len_trim(project_name))//'-Bath.grd'
!
!Open(14), for dynamic allocation
      write(6,*) 'START! READING of Global data:  ', fort14(1:len_trim(fort14))
      open(14,file=fort14,status='old',action='read')
         call read14_alloc ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax )
      close(14)
! Dymanic Memory Allocation for Global Grid
         allocate( xyd(3,np) )
         allocate( nm(ne,3) )
         allocate( nvdll(nope)  )
         allocate( nbdv(nope,nvdl_max) )
         allocate( nvell(nbou), ibtype(nbou)  )
         allocate( nbvv(nbou,nvel_max), ibconn(nbou,nvel_max), bar(3,nbou,nvel_max) )
         allocate( nsequencer(nodemax) )
!Re-open & read global grid data(14)
      open(14,file=fort14,status='old',action='read')
         call read14 ( 14, ne, np, nope, nbou, nvdl_max, nvel_max, nodemax, nsequencer,      &
                       agrid, xyd, nm, neta, nvdll, nbdv, nvel, nvell, ibtype, nbvv, ibconn, &
                       bar )
      close(14)
      write(6,*) 'FINISH!'
      write(6,*)

      allocate( nprop(np) )
!
      write(6,*)
      write(6,*)
      write(6,*)
      write(6,*)
      write(6,*) '*Bathymetry Editor*'
      write(6,*)
      write(6,*) 'Select the target shape'
      call select_target(np, xyd, nprop)
      write(6,*) 'Target Identified'
      write(6,*)
      write(6,*) 'Choose Method to adjust bathymetric depth'
      call adjustment_method( imethod, ivalue )
      write(6,*)
      n = 0
      do k = 1,np
        if ((nprop(k).EQ.1).AND.(imethod.EQ.0)) xyd(3,k) = ivalue
        if ((nprop(k).EQ.1).AND.(imethod.EQ.1)) xyd(3,k) = xyd(3,k) + ivalue
        if ((nprop(k).EQ.1).AND.(imethod.EQ.2)) xyd(3,k) = xyd(3,k) * ivalue
        if ((nprop(k).EQ.1).AND.(imethod.EQ.3).AND.(xyd(3,k).LT.ivalue)) xyd(3,k) = ivalue
        if ((nprop(k).EQ.1).AND.(imethod.EQ.4).AND.(xyd(3,k).GT.ivalue)) xyd(3,k) = ivalue
        if (nprop(k).EQ.1) n = n + 1
      end do
      write(6,*) n, 'Points Adjusted'

! OUTPUT NEW GRID
      write(6,*)
      call output_grid( ne, np, nope, nbou, nvdl_max, nvel_max, project_name, gridname, &
                        agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar, neta )

   end subroutine bath_editor
!
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine select_target (np, xyd, nprop)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!
      implicit none
      integer, intent(in) :: np
      double precision, intent(inout) :: xyd(3,np)
      integer, intent(inout) :: nprop(np)
      integer :: ishape ! sub-grid shapes parameter (0:Circle, 1:Rectangle, 2:Polygon)
!
      write(6,*) '     0: Circle   1: Rectangle   2: Polygon(s)'
      read(5,*) ishape
      select case(ishape)
         case(0)
            call select_circle( np, xyd, nprop )
         case(1)
            call select_rectan( np, xyd, nprop )
         case(2)
            call select_polygon( np, xyd, nprop )
         case(3)
            call select_arbitr( np, xyd, nprop )
         case default
            write(6,*) 'You have to select 0,1 or 2'
            write(6,*) 'Hit the Enter-key to stop'
            read(5,*)
            stop
      end select
!
   end subroutine select_target
!
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine output_grid ( ne, np, nope, nbou, nvdl_max, nvel_max, project_name, gridname, &
                            agrid, xyd, nm, nvdll, nbdv, nvell, ibtype, nbvv, ibconn, bar,neta )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!
      implicit none
      integer, intent(in) :: ne, np, neta
      integer, intent(in) :: nope, nbou, nvdl_max, nvel_max
      double precision, intent(in) :: xyd(3,np)
      integer, intent(in) :: nm(ne,3)
      character(120), intent(in) :: agrid, project_name, gridname
      integer, intent(in) :: nvdll(nope), nbdv(nope,nvdl_max)
      integer, intent(in) :: nvell(nbou), nbvv(nbou,nvel_max), ibtype(nbou), ibconn(nbou,nvel_max)
      double precision, intent(in) :: bar(3,nbou,nvel_max)
      integer :: i, j, k, n, n3

      open(50,file=gridname, status='unknown')

      write(6,*) 'Output GRID DATA:', gridname(1:len_trim(gridname))
      n3 = 3

      write(50,*) agrid(1:len_trim(agrid))
      write(6,*) agrid
      write(50,503) ne, np
      do i = 1, np
         write(50,501) i, (xyd(j,i), j=1,3)
      enddo
      do i = 1, ne
         write(50,502) i, n3, ( nm(i,j), j = 1, 3 )
      enddo
 501  format(i10,2f18.10,e18.10)
 502  format(i10,i2,3i10)
 503  format(2i12)

      write(50,504) nope
      write(50,504) neta
      do k = 1, nope
         write(50,504) nvdll(k)
         do j = 1, nvdll(k)
            write(50,504) nbdv(k,j)
         enddo
      enddo

504   format(i12)

      write(50,506) nbou
      n = sum( nvell(:) )
      do k = 1, nbou
         if( (ibtype(k)==4) .or. (ibtype(k)==24) ) n=n+nvell(k)
      enddo
      write(50,506) n

506   format(i12)

      do k = 1, nbou
         write(50,512) nvell(k), ibtype(k)
         select case(ibtype(k))
         case(0,1,2,10,11,12,20,21,22,30,52)
            do j = 1, nvell(k)
               write(50,510) nbvv(k,j)
            enddo
         case(3,13,23)
            do j = 1, nvell(k)
               write(50,510) nbvv(k,j), (bar(i,k,j), i=1,2)
            enddo
         case(4,24)
            do j = 1, nvell(k)
               write(50,511) nbvv(k,j), ibconn(k,j), (bar(i,k,j), i=1,3)
            enddo
         end select
      enddo

 510  format(i10,f15.6,f5.2)
 511  format(2i10,f15.6,2f5.2)
 512  format(i6,i4)
       close(50)

   end subroutine output_grid
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine adjustment_method ( imethod, ifactor )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
!
      integer :: imethod
      double precision :: ifactor
!
      write(6,*) '     0: Define   1: Add   2: Multiply    3: Set Minimum     4: Set Maximum'
      read(5,*) imethod
      select case(imethod)
        case(0)
          write(6,*)
          write(6,*) 'Enter the new value'
          read(5,*) ifactor
        case(1)
          write(6,*)
          write(6,*) 'Enter the number to add'
          read(5,*) ifactor
        case(2)
          write(6,*)
          write(6,*) 'Enter the multiplication factor'
          read(5,*) ifactor
        case(3)
          write(6,*)
          write(6,*) 'Enter the new minimum value'
          read(5,*) iifactor
        case(4)
          write(6,*)
          write(6,*) 'Enter the new maximum value'
          read(5,*) ifactor
        case default
           write(6,*) 'You have to select 0, 1, 2, 3, or 4'
           write(6,*) 'Hit the Enter-key to stop'
           read(5,*)
           stop
      end select
!
   end subroutine adjustment_method
!
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine select_polygon(np, xyd, nprop)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
!
      integer,intent(in)           :: np
      double precision, intent(in) :: xyd(3,np)
      integer, intent(out)         :: nprop(np)
!
      character(60)                 :: in_file
      integer                       :: n, i, k, j, cn, icase, npoly, nvertmax, nRec
      integer, allocatable          :: nvert (:), rank(:)
      double precision              :: vt
      double precision, allocatable :: xy(:,:,:)

      integer(4)       :: tmp4(2), length, version, shape
      integer(4)       :: record(100),content(100),nparts(100),npoints(100),parts(100,100)
      real(8)          :: xbound(2),ybound(2),zbound(2),mbound(2)
      real(8)          :: xmin(100),xmax(100),ymin(100),ymax(100),zmin(100),zmax(100)
      real(8)          :: mmin(100),mmax(100),x(100,100),y(100,100)
!
      write(6,*) 'To define the polygon,'
      write(6,*) 'you must have a shapefile (*.shp).'
      WRITE(6,FMT='(A)',ADVANCE='NO') 'Enter the name of the input file (no extension): '
      READ(5,*) in_file

      write(6,*)
      write(6,*) 'Begin! Reading file, writing output file...'
! Input Main File Header
      open(UNIT=1, FILE=TRIM(in_file)//".shp",FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4)
        read(1,REC=7) tmp4(1)
        call switch_endian( tmp4(1), length)
        read(1,REC=8) version
        read(1,REC=9) shape
        read(1,REC=10) tmp4(1)
        read(1,REC=11) tmp4(2)
        xbound(1) = transfer(tmp4,xbound(1))
        read(1,REC=12) tmp4(1)
        read(1,REC=13) tmp4(2)
        ybound(1) = transfer(tmp4,ybound(1))
        read(1,REC=14) tmp4(1)
        read(1,REC=15) tmp4(2)
        xbound(2) = transfer(tmp4,xbound(2))
        read(1,REC=16) tmp4(1)
        read(1,REC=17) tmp4(2)
        ybound(2) = transfer(tmp4,ybound(2))
        read(1,REC=18) tmp4(1)
        read(1,REC=19) tmp4(2)
        zbound(1) = transfer(tmp4,zbound(1))
        read(1,REC=20) tmp4(1)
        read(1,REC=21) tmp4(2)
        zbound(2) = transfer(tmp4,zbound(2))
        read(1,REC=22) tmp4(1)
        read(1,REC=23) tmp4(2)
        mbound(1) = transfer(tmp4,mbound(1))
        read(1,REC=24) tmp4(1)
        read(1,REC=25) tmp4(2)
        mbound(2) = transfer(tmp4,mbound(2))
! Input Records
        nREC = 26
        n = 0
        loopA: do
          if (nREC.GE.length/2) then
            exit loopA
          end if
          n = n + 1
          read(1,REC=nREC) tmp4(1)
          call switch_endian( tmp4(1), record(n) )
          read(1,REC=nREC+1) tmp4(1)
          call switch_endian( tmp4(1), content(n) )
!         REC=nREC+2 - shape repeated
          read(1,REC=nREC+3) tmp4(1)
          read(1,REC=nREC+4) tmp4(2)
          xmin(n) = transfer(tmp4,xmax(n))
          read(1,REC=nREC+5) tmp4(1)
          read(1,REC=nREC+6) tmp4(2)
          ymin(n) = transfer(tmp4,ymin(n))
          read(1,REC=nREC+7) tmp4(1)
          read(1,REC=nREC+8) tmp4(2)
          xmax(n) = transfer(tmp4,xmax(n))
          read(1,REC=nREC+9) tmp4(1)
          read(1,REC=nREC+10) tmp4(2)
          ymax(n) = transfer(tmp4,ymax(n))
          read(1,REC=nREC+11) nparts(n)
          read(1,REC=nREC+12) npoints(n)
          if (npoints(n).GE.nvertmax) nvertmax = npoints(n)
          loopB: do k = 1,nparts(n)
            read(1,REC=nREC+12+k) parts(k,n)
          end do loopB
          loopC: do k = 1,npoints(n)
            read(1,REC=nREC+12+4*(k-1)+1+nparts(n)) tmp4(1)
            read(1,REC=nREC+12+4*(k-1)+2+nparts(n)) tmp4(2)
            x(k,n) = transfer(tmp4,x(k,n))
            read(1,REC=nREC+12+4*(k-1)+3+nparts(n)) tmp4(1)
            read(1,REC=nREC+12+4*(k-1)+4+nparts(n)) tmp4(2)
            y(k,n) = transfer(tmp4,y(k,n))
          end do loopC
          nREC = nREC+12+nparts(n)+4*npoints(n)+1
        end do loopA
      close(1)

! Convert shapefile information to subroutine variables
      npoly = sum(nparts(:))
      allocate ( rank(npoly), nvert(npoly), xy(2,nvertmax,npoly) )
      npoly = 0

      do k = 1,n
        if (nparts(n).EQ.1) then
          npoly = npoly + 1
          rank(npoly) = 1
          nvert(npoly) = npoints(k)
          do i = 1,npoints(k)
            xy(1,i,npoly) = x(i,k)
            xy(2,i,npoly) = y(i,k)
          end do
        else !Holes in the Polygon, i.e. a Donut, mmmmmm.... Donut....
          do j = 1,nparts(k)-1
            npoly = npoly + 1
            if (j.EQ.1) rank(npoly) = 1
            nvert(npoly) = parts(j+1,k)-parts(j,k)
            do i = 1,nvert(npoly)
              xy(1,i,npoly) = x(parts(j,k)+i,k)
              xy(2,i,npoly) = y(parts(j,k)+i,k)
            end do
          end do
          npoly = npoly + 1
          nvert(npoly) = npoints(k)-parts(nparts(k),k)
          do i = 1,nvert(npoly)
            xy(1,i,npoly) = x(parts(nparts(k),k)+i,k)
            xy(2,i,npoly) = y(parts(nparts(k),k)+i,k)
          end do
        end if
      end do
!
! Peform Polygon Crossing Test
      nprop(:) = 0
      do n = 1, np
        cn = 0
        do k = 1, npoly
          do j = 1,nvert(k)-1
            if ((((xy(2,j,k)).LE.(xyd(2,n))).AND.((xy(2,j+1,k)).GT.(xyd(2,n)))).OR.&
            & (((xy(2,j,k)).GT.(xyd(2,n))).AND.((xy(2,j+1,k)).LE.(xyd(2,n))))) then
              vt = (xyd(2,n) - xy(2,j,k)) / (xy(2,j+1,k) - xy(2,j,k));
              if (xyd(1,n) < xy(1,j,k) + vt * (xy(1,j+1,k) - xy(1,j,k))) cn = cn + 1
! P.x < intersect ! A valid crossing of y=P.y right of P.x
            end if
          end do
        end do
! 0 if even (out), and 1 if odd (in)
        if(mod(cn,2).EQ.1) nprop(n) = 1
      end do
!
   end subroutine select_polygon
!
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
   subroutine switch_endian( IntIn, IntOut )
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+

!...Subroutine to change the endian of the input

      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: IntIn
      INTEGER, INTENT(OUT)   :: IntOut
      INTEGER                :: i_element
      INTEGER                :: i_element_br

      i_element = TRANSFER( IntIn, 0 )

      CALL MVBITS( i_element, 24, 8, i_element_br, 0  )
      CALL MVBITS( i_element, 16, 8, i_element_br, 8  )
      CALL MVBITS( i_element,  8, 8, i_element_br, 16 )
      CALL MVBITS( i_element,  0, 8, i_element_br, 24 )

      IntOut = TRANSFER( i_element_br, 0 )

   end subroutine switch_endian
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
  subroutine polybuild(gridname,plg,pts,npts,npoly,ptmax)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
!
      integer,intent(in)                       :: npoly
      character(120),intent(in)                :: gridname
      integer,intent(in)                       :: pts
      integer,intent(in)                       :: ptmax
      integer, intent(in)          :: npts(npoly)
      double precision, intent(in) :: plg(npoly,ptmax,2)
      !
      integer                      :: a, b, h, i, j, k, nRec

      integer(1)       :: tmp1
      integer(4)       :: tmp4(2), length, version, shape
      double precision :: zbound, mbound, Xmin, Xmax, Ymin, Ymax
      double precision :: XXmin, XXmax, YYmin, YYmax

      zbound = 0
      mbound = 0
      Xmin = plg(1,1,1)
      Ymin = plg(1,1,2)
      Xmax = plg(1,1,1)
      Ymax = plg(1,1,2)
      do i = 1,npoly
        do j = 1,npts(i)
          if (Xmin.GT.plg(i,j,1)) Xmin = plg(i,j,1)
          if (Ymin.GT.plg(i,j,2)) Ymin = plg(i,j,2)
          if (Xmax.LT.plg(i,j,1)) Xmax = plg(i,j,1)
          if (Ymax.LT.plg(i,j,2)) Ymax = plg(i,j,2)
        end do
      end do
! Write *.shp

      open(CONVERT='LITTLE ENDIAN', UNIT=1, FILE=TRIM(gridname)//'.shp', status='REPLACE', &
      & FORM='UNFORMATTED', ACCESS='DIRECT', RECL=4)

! WRITE MAIN FILE HEADER
        call switch_endian(9994,tmp4(1))
        write(1,REC=1) tmp4(1)                            ! File Code
        write(1,REC=2) 0                                   ! Unused
        write(1,REC=3) 0                                   ! Unused
        write(1,REC=4) 0                                   ! Unused
        write(1,REC=5) 0                                   ! Unused
        write(1,REC=6) 0                                   ! Unused
        length = INT(50+28*nPoly+8*pts)
        call switch_endian(length, tmp4(1))
        write(1,REC=7) tmp4(1)                            ! File Length
        version = 1000
        write(1,REC=8) version                             ! Version
        shape = 5
        write(1,REC=9) shape                               ! Shape Type (Polygon)

        tmp4 = transfer(Xmin,tmp4)
        write(1,REC=10) tmp4(1)                            ! Xmin
        write(1,REC=11) tmp4(2)
        tmp4 = transfer(Ymin,tmp4)
        write(1,REC=12) tmp4(1)                            ! Ymin
        write(1,REC=13) tmp4(2)
        tmp4 = transfer(Xmax,tmp4)
        write(1,REC=14) tmp4(1)                            ! Xmax
        write(1,REC=15) tmp4(2)
        tmp4 = transfer(Ymax,tmp4)
        write(1,REC=16) tmp4(1)                            ! Ymax
        write(1,REC=17) tmp4(2)
        tmp4 = transfer(zbound,tmp4)
        write(1,REC=18) tmp4(1)                            ! Zmin
        write(1,REC=19) tmp4(2)
        tmp4 = transfer(zbound,tmp4)
        write(1,REC=20) tmp4(1)                            ! Zmax
        write(1,REC=21) tmp4(2)
        tmp4 = transfer(mbound,tmp4)
        write(1,REC=22) tmp4(1)                            ! Mmin
        write(1,REC=23) tmp4(2)
        tmp4 = transfer(mbound,tmp4)
        write(1,REC=24) tmp4(1)                            ! Mmax
        write(1,REC=25) tmp4(2)

! WRITE Polygon Records
        nREC = 25
        do i = 1,nPoly
          call switch_endian( i,tmp4(1) )
          write(1,REC=nREC+1) tmp4(1)                        ! Record Number
          call switch_endian( 24+8*npts(i), tmp4(1))
          write(1,REC=nREC+2) tmp4(1)                      ! Content Length

          write(1,REC=nREC+3) shape                        ! Shape Type (Polygon)

          XXmin = plg(i,1,1)
          YYmin = plg(i,1,2)
          XXmax = plg(i,1,1)
          YYmax = plg(i,1,2)

          do j = 2,npts(i)
            if (XXmin.GT.plg(i,j,1)) XXmin = plg(i,j,1)
            if (YYmin.GT.plg(i,j,2)) YYmin = plg(i,j,2)
            if (XXmax.LT.plg(i,j,1)) XXmax = plg(i,j,1)
            if (YYmax.LT.plg(i,j,2)) YYmax = plg(i,j,2)
          end do

          tmp4 = transfer(XXmin,tmp4)
          write(1,REC=nREC+4) tmp4(1)                       ! Box (Xmin)
          write(1,REC=nREC+5) tmp4(2)                       ! Box (Xmin)
          tmp4 = transfer(YYmin,tmp4)
          write(1,REC=nREC+6) tmp4(1)                       ! Box (Ymin)
          write(1,REC=nREC+7) tmp4(2)                       ! Box (Ymin)
          tmp4 = transfer(XXmax,tmp4)
          write(1,REC=nREC+8) tmp4(1)                       ! Box (Xmax)
          write(1,REC=nREC+9) tmp4(2)                       ! Box (Xmax)
          tmp4 = transfer(YYmax,tmp4)
          write(1,REC=nREC+10) tmp4(1)                      ! Box (Ymax)
          write(1,REC=nREC+11) tmp4(2)                      ! Box (Ymax)
          tmp4(1) = 1
          write(1,REC=nREC+12) tmp4(1)                      ! NumParts
          tmp4(1) = npts(i)
          write(1,REC=nREC+13) tmp4(1)                      ! NumPoints
          tmp4(1) = 0
          write(1,REC=nREC+14) tmp4(1)                      ! Parts
          nREC = nREC + 14
          do j = 1,npts(i)
            tmp4 = transfer(plg(i,j,1),tmp4)
            write(1,REC=nREC+1) tmp4(1)            ! Points (x1)
            write(1,REC=nREC+2) tmp4(2)            ! Points (x1)
            tmp4 = transfer(plg(i,j,2),tmp4)
            write(1,REC=nREC+3) tmp4(1)            ! Points (y1)
            write(1,REC=nREC+4) tmp4(2)            ! Points (y1)
            nREC = nREC + 4
          end do
        end do

      close(1)
!
! Write *.shx
      open(CONVERT='LITTLE_ENDIAN', UNIT=2, FILE=TRIM(gridname)//'.shx', status='REPLACE', &
      & FORM='UNFORMATTED', ACCESS='DIRECT', RECL=4)

! WRITE MAIN FILE HEADER
        call switch_endian(9994,tmp4(1))
        write(2,REC=1) tmp4(1)                            ! File Code
        write(2,REC=2) 0                                   ! Unused
        write(2,REC=3) 0                                   ! Unused
        write(2,REC=4) 0                                   ! Unused
        write(2,REC=5) 0                                   ! Unused
        write(2,REC=6) 0                                   ! Unused
        length = INT(50+4*nPoly)
        call switch_endian(length, tmp4(1))
        write(2,REC=7) tmp4(1)                            ! File Length
        version = 1000
        write(2,REC=8) version                             ! Version
        shape = 5
        write(2,REC=9) shape                               ! Shape Type (Polygon)

        tmp4 = transfer(Xmin,tmp4)
        write(2,REC=10) tmp4(1)                            ! Xmin
        write(2,REC=11) tmp4(2)
        tmp4 = transfer(Ymin,tmp4)
        write(2,REC=12) tmp4(1)                            ! Ymin
        write(2,REC=13) tmp4(2)
        tmp4 = transfer(Xmax,tmp4)
        write(2,REC=14) tmp4(1)                            ! Xmax
        write(2,REC=15) tmp4(2)
        tmp4 = transfer(Ymax,tmp4)
        write(2,REC=16) tmp4(1)                            ! Ymax
        write(2,REC=17) tmp4(2)
        tmp4 = transfer(zbound,tmp4)
        write(2,REC=18) tmp4(1)                            ! Zmin
        write(2,REC=19) tmp4(2)
        tmp4 = transfer(zbound,tmp4)
        write(2,REC=20) tmp4(1)                            ! Zmax
        write(2,REC=21) tmp4(2)
        tmp4 = transfer(mbound,tmp4)
        write(2,REC=22) tmp4(1)                            ! Mmin
        write(2,REC=23) tmp4(2)
        tmp4 = transfer(mbound,tmp4)
        write(2,REC=24) tmp4(1)                            ! Mmax
        write(2,REC=25) tmp4(2)

! WRITE Polygon Records
        nREC = 26
        h = 50
        do i = 1,nPoly
          call switch_endian( h,tmp4(1) )
          write(2,REC=nREC) tmp4(1)                        ! Offset
          call switch_endian(24+8*npts(i), tmp4(1))
          write(2,REC=nREC+1) tmp4(1)                      ! Content Length
          nREC = nREC + 2
          h = h + 28+8*npts(i)
        end do

      close(2)

! Write DBF
      OPEN(CONVERT='LITTLE_ENDIAN', UNIT=3, FILE=TRIM(gridname)//'.dbf', status='REPLACE', &
      & FORM='UNFORMATTED', ACCESS='DIRECT', RECL=1)
        tmp1 = 3
        write(3,REC=1) tmp1                                      ! 0,Version Number (3 = File without DBT)
        tmp1 = 109
        write(3,REC=2) tmp1                                       ! 1,Date (YY,eg. 2009 - 1900 = 109)
        tmp1 = 1
        write(3,REC=3) tmp1                                         ! 2,Date (MM)
        write(3,REC=4) tmp1                                         ! 3,Date (DD)
      CLOSE(3)

      OPEN(CONVERT='LITTLE_ENDIAN', UNIT=3, FILE=TRIM(gridname)//'.dbf', FORM='UNFORMATTED', ACCESS='DIRECT', RECL=4)
      tmp4(1) = nPoly
        write(3,REC=2) tmp4(1)                                     ! 4-7,Number of Records in File
      CLOSE(3)

      OPEN(CONVERT='LITTLE_ENDIAN', UNIT=3, FILE=TRIM(gridname)//'.dbf', FORM='UNFORMATTED', ACCESS='DIRECT', RECL=1)
        tmp1 = 0
        write(3,REC=8) tmp1                                        ! 8-9,Length of Header Structure
        tmp1 = 65
        write(3,REC=9) tmp1                                        ! 8-9,Length of Header Structure
        tmp1 = 0
        write(3,REC=10) tmp1                                         ! 10-11,Length of Each Record
        tmp1 = 7
        write(3,REC=11) tmp1                                         ! 10-11,Length of Each Record
        tmp1 = 0
        do nREC = 12,29
          write(3,REC=nREC) tmp1                                    ! 12-27, Reserved; 28, Table Flags; 29, Code page mark; 30-31, Reserved; 32 - n + 2, Field subrecords
        end do
        tmp1 = 87
        write(3,REC=30) tmp1                                       !
        tmp1 = 0
        write(3,REC=31) tmp1                                        !
        write(3,REC=32) tmp1                                        !
        tmp1 = 73
        write(3,REC=33) tmp1                                       ! Field Name (49 = I)
        tmp1 = 100
        write(3,REC=34) tmp1                                      ! Field Name (63 = d)
        tmp1 = 0
        do nREC = 35,43
          write(3,REC=nREC) tmp1
        end do
        tmp1 = 78
        write(3,REC=44) tmp1                                       ! Field Type (4E = Long)
        tmp1 = 0
        do nREC = 45,48
          write(3,REC=nREC) tmp1
        end do
        tmp1 = 6
        write(3,REC=49) tmp1                                        ! Field Length
        tmp1 = 0
        do nREC = 50,64
          write(3,REC=nREC) tmp1                                    !Unused/Reserved Bytes
        end do
        tmp1 = 13
        write(3, REC=65) tmp1                                      !Terminator (0D)
        nREC = 65
        do i = 1,nPoly                                           ! Write Records
          tmp1 = 32
          do j = 1,6
            write(3, REC=nREC + j) tmp1                            !20
          end do
          tmp1 = 48
          write(3, REC=nREC+7) tmp1                                ! 30
          nREC = nREC + 7
        end do
        tmp1 = 26
        write(3,REC=nREC+1) tmp1                                   ! End of File (1A)
      CLOSE(3)

end subroutine polybuild
!
!
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
  subroutine tribuild(gridname,ntri,tri,xyd,ne,np)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
!
      integer,intent(in)            :: ntri
      integer,intent(in)            :: ne
      integer,intent(in)            :: np
      integer,intent(in)            :: tri(ne,3)
      double precision,intent(in)   :: xyd(3,np)
      character(120),intent(in)     :: gridname
!
      integer                       :: a, b, i, j, nRec

      integer(1)                    :: tmp1
      integer(4)                    :: tmp4(2), length, version, shape
      double precision              :: zbound, mbound, Xmin, Xmax, Ymin, Ymax
      double precision              :: XXmin,XXmax,YYmin,YYmax
      zbound = 0
      mbound = 0
      Xmin = xyd(1,tri(1,1))
      Ymin = xyd(2,tri(1,2))
      Xmax = xyd(1,tri(1,1))
      Ymax = xyd(2,tri(1,2))
      do i = 1,ntri
        do j = 1,3
          if (Xmin.GT.xyd(1,tri(i,j))) Xmin = xyd(1,tri(i,j))
          if (Ymin.GT.xyd(2,tri(i,j))) Ymin = xyd(2,tri(i,j))
          if (Xmax.LT.xyd(1,tri(i,j))) Xmax = xyd(1,tri(i,j))
          if (Ymax.LT.xyd(2,tri(i,j))) Ymax = xyd(2,tri(i,j))
        end do
      end do
! Write *.shp
      open(CONVERT='LITTLE_ENDIAN', UNIT=1, FILE=TRIM(gridname)//'Triangles.shp', status='REPLACE', &
      & FORM='UNFORMATTED', ACCESS='DIRECT', RECL=4)
! WRITE MAIN FILE HEADER
        call switch_endian(9994,tmp4(1))
        write(1,REC=1) tmp4(1)                            ! File Code
        write(1,REC=2) 0                                   ! Unused
        write(1,REC=3) 0                                   ! Unused
        write(1,REC=4) 0                                   ! Unused
        write(1,REC=5) 0                                   ! Unused
        write(1,REC=6) 0                                   ! Unused
        length = INT(50+60*ntri)
        call switch_endian(length, tmp4(1))
        write(1,REC=7) tmp4(1)                            ! File Length
        version = 1000
        write(1,REC=8) version                             ! Version
        shape = 5
        write(1,REC=9) shape                               ! Shape Type (Polygon)

        tmp4 = transfer(Xmin,tmp4)
        write(1,REC=10) tmp4(1)                            ! Xmin
        write(1,REC=11) tmp4(2)
        tmp4 = transfer(Ymin,tmp4)
        write(1,REC=12) tmp4(1)                            ! Ymin
        write(1,REC=13) tmp4(2)
        tmp4 = transfer(Xmax,tmp4)
        write(1,REC=14) tmp4(1)                            ! Xmax
        write(1,REC=15) tmp4(2)
        tmp4 = transfer(Ymax,tmp4)
        write(1,REC=16) tmp4(1)                            ! Ymax
        write(1,REC=17) tmp4(2)
        tmp4 = transfer(zbound,tmp4)
        write(1,REC=18) tmp4(1)                            ! Zmin
        write(1,REC=19) tmp4(2)
        tmp4 = transfer(zbound,tmp4)
        write(1,REC=20) tmp4(1)                            ! Zmax
        write(1,REC=21) tmp4(2)
        tmp4 = transfer(mbound,tmp4)
        write(1,REC=22) tmp4(1)                            ! Mmin
        write(1,REC=23) tmp4(2)
        tmp4 = transfer(mbound,tmp4)
        write(1,REC=24) tmp4(1)                            ! Mmax
        write(1,REC=25) tmp4(2)

! WRITE Polygon Records
        nREC = 26
        do i = 1,ntri
          call switch_endian( i,tmp4(1) )
          write(1,REC=nREC) tmp4(1)                        ! Record Number
          call switch_endian( 56, tmp4(1))
          write(1,REC=nREC+1) tmp4(1)                      ! Content Length

          write(1,REC=nREC+2) shape                        ! Shape Type (Polygon)

          XXmin = xyd(1,tri(i,1))
          YYmin = xyd(2,tri(i,1))
          XXmax = xyd(1,tri(i,1))
          YYmax = xyd(2,tri(i,1))

          do j = 2,3
            if (XXmin.GT.xyd(1,tri(i,j))) XXmin = xyd(1,tri(i,j))
            if (YYmin.GT.xyd(2,tri(i,j))) YYmin = xyd(2,tri(i,j))
            if (XXmax.LT.xyd(1,tri(i,j))) XXmax = xyd(1,tri(i,j))
            if (YYmax.LT.xyd(2,tri(i,j))) YYmax = xyd(2,tri(i,j))
          end do
          tmp4 = transfer(XXmin,tmp4)
          write(1,REC=nREC+3) tmp4(1)                       ! Box (Xmin)
          write(1,REC=nREC+4) tmp4(2)                       ! Box (Xmin)
          tmp4 = transfer(YYmin,tmp4)
          write(1,REC=nREC+5) tmp4(1)                       ! Box (Ymin)
          write(1,REC=nREC+6) tmp4(2)                       ! Box (Ymin)
          tmp4 = transfer(XXmax,tmp4)
          write(1,REC=nREC+7) tmp4(1)                       ! Box (Xmax)
          write(1,REC=nREC+8) tmp4(2)                       ! Box (Xmax)
          tmp4 = transfer(YYmax,tmp4)
          write(1,REC=nREC+9) tmp4(1)                       ! Box (Ymax)
          write(1,REC=nREC+10) tmp4(2)                      ! Box (Ymax)
          tmp4(1) = 1
          write(1,REC=nREC+11) tmp4(1)                            ! NumParts
          tmp4(1) = 4
          write(1,REC=nREC+12) tmp4(1)                            ! NumPoints
          tmp4(1) = 0
          write(1,REC=nREC+13) tmp4(1)                            ! Parts
          tmp4 = transfer(xyd(1,tri(i,1)),tmp4)
          write(1,REC=nREC+14) tmp4(1)            ! Points (x1)
          write(1,REC=nREC+15) tmp4(2)            ! Points (x1)
          tmp4 = transfer(xyd(2,tri(i,1)),tmp4)
          write(1,REC=nREC+16) tmp4(1)            ! Points (y1)
          write(1,REC=nREC+17) tmp4(2)            ! Points (y1)
          tmp4 = transfer(xyd(1,tri(i,2)),tmp4)
          write(1,REC=nREC+18) tmp4(1)            ! Points (x2)
          write(1,REC=nREC+19) tmp4(2)            ! Points (x2)
          tmp4 = transfer(xyd(2,tri(i,2)),tmp4)
          write(1,REC=nREC+20) tmp4(1)            ! Points (y2)
          write(1,REC=nREC+21) tmp4(2)            ! Points (y2)
          tmp4 = transfer(xyd(1,tri(i,3)),tmp4)
          write(1,REC=nREC+22) tmp4(1)            ! Points (x3)
          write(1,REC=nREC+23) tmp4(2)            ! Points (x3)
          tmp4 = transfer(xyd(2,tri(i,3)),tmp4)
          write(1,REC=nREC+24) tmp4(1)            ! Points (y3)
          write(1,REC=nREC+25) tmp4(2)            ! Points (y3)
          tmp4 = transfer(xyd(1,tri(i,1)),tmp4)
          write(1,REC=nREC+26) tmp4(1)            ! Points (x4)
          write(1,REC=nREC+27) tmp4(2)            ! Points (x4)
          tmp4 = transfer(xyd(2,tri(i,1)),tmp4)
          write(1,REC=nREC+28) tmp4(1)            ! Points (y4)
          write(1,REC=nREC+29) tmp4(2)            ! Points (y4)
          nREC = nREC + 30
        end do

      close(1)
!
! Write *.shp
      open(CONVERT='LITTLE_ENDIAN', UNIT=2, FILE=TRIM(gridname)//'Triangles.shx', status='REPLACE', &
      & FORM='UNFORMATTED', ACCESS='DIRECT', RECL=4)

! WRITE MAIN FILE HEADER
        call switch_endian(9994,tmp4(1))
        write(2,REC=1) tmp4(1)                            ! File Code
        write(2,REC=2) 0                                   ! Unused
        write(2,REC=3) 0                                   ! Unused
        write(2,REC=4) 0                                   ! Unused
        write(2,REC=5) 0                                   ! Unused
        write(2,REC=6) 0                                   ! Unused
        length = INT(50+4*ntri)
        call switch_endian(length, tmp4(1))
        write(2,REC=7) tmp4(1)                            ! File Length
        version = 1000
        write(2,REC=8) version                             ! Version
        shape = 5
        write(2,REC=9) shape                               ! Shape Type (Polygon)

        tmp4 = transfer(Xmin,tmp4)
        write(2,REC=10) tmp4(1)                            ! Xmin
        write(2,REC=11) tmp4(2)
        tmp4 = transfer(Ymin,tmp4)
        write(2,REC=12) tmp4(1)                            ! Ymin
        write(2,REC=13) tmp4(2)
        tmp4 = transfer(Xmax,tmp4)
        write(2,REC=14) tmp4(1)                            ! Xmax
        write(2,REC=15) tmp4(2)
        tmp4 = transfer(Ymax,tmp4)
        write(2,REC=16) tmp4(1)                            ! Ymax
        write(2,REC=17) tmp4(2)
        tmp4 = transfer(zbound,tmp4)
        write(2,REC=18) tmp4(1)                            ! Zmin
        write(2,REC=19) tmp4(2)
        tmp4 = transfer(zbound,tmp4)
        write(2,REC=20) tmp4(1)                            ! Zmax
        write(2,REC=21) tmp4(2)
        tmp4 = transfer(mbound,tmp4)
        write(2,REC=22) tmp4(1)                            ! Mmin
        write(2,REC=23) tmp4(2)
        tmp4 = transfer(mbound,tmp4)
        write(2,REC=24) tmp4(1)                            ! Mmax
        write(2,REC=25) tmp4(2)

! WRITE Polygon Records
        nREC = 26
        do i = 1,ntri
          call switch_endian( 50+60*(i-1),tmp4(1) )
          write(2,REC=nREC) tmp4(1)                        ! Offset
          call switch_endian( 56, tmp4(1))
          write(2,REC=nREC+1) tmp4(1)                      ! Content Length
          nREC = nREC + 2
        end do

      close(2)

! Write DBF
      OPEN(CONVERT='LITTLE_ENDIAN', UNIT=3, FILE=TRIM(gridname)//'Triangles.dbf', status='REPLACE', &
      & FORM='UNFORMATTED', ACCESS='DIRECT', RECL=1)
        tmp1 = 3
        write(3,REC=1) tmp1                                      ! 0,Version Number (3 = File without DBT)
        tmp1 = 109
        write(3,REC=2) tmp1                                       ! 1,Date (YY,eg. 2009 - 1900 = 109)
        tmp1 = 1
        write(3,REC=3) tmp1                                         ! 2,Date (MM)
        write(3,REC=4) tmp1                                         ! 3,Date (DD)
      CLOSE(3)

      OPEN(CONVERT='LITTLE_ENDIAN', UNIT=3, FILE=TRIM(gridname)//'Triangles.dbf', FORM='UNFORMATTED', ACCESS='DIRECT', RECL=4)
      tmp4(1) = ntri
        write(3,REC=2) tmp4(1)                                     ! 4-7,Number of Records in File
      CLOSE(3)

      OPEN(CONVERT='LITTLE_ENDIAN', UNIT=3, FILE=TRIM(gridname)//'Triangles.dbf', FORM='UNFORMATTED', ACCESS='DIRECT', RECL=1)
        tmp1 = 0
        write(3,REC=8) tmp1                                        ! 8-9,Length of Header Structure
        tmp1 = 65
        write(3,REC=9) tmp1                                        ! 8-9,Length of Header Structure
        tmp1 = 0
        write(3,REC=10) tmp1                                         ! 10-11,Length of Each Record
        tmp1 = 7
        write(3,REC=11) tmp1                                         ! 10-11,Length of Each Record
        tmp1 = 0
        do nREC = 12,29
          write(3,REC=nREC) tmp1                                    ! 12-27, Reserved; 28, Table Flags; 29, Code page mark; 30-31, Reserved; 32 - n + 2, Field subrecords
        end do
        tmp1 = 87
        write(3,REC=30) tmp1                                       !
        tmp1 = 0
        write(3,REC=31) tmp1                                        !
        write(3,REC=32) tmp1                                        !
        tmp1 = 73
        write(3,REC=33) tmp1                                       ! Field Name (49 = I)
        tmp1 = 100
        write(3,REC=34) tmp1                                      ! Field Name (63 = d)
        tmp1 = 0
        do nREC = 35,43
          write(3,REC=nREC) tmp1
        end do
        tmp1 = 78
        write(3,REC=44) tmp1                                       ! Field Type (4E = Long)
        tmp1 = 0
        do nREC = 45,48
          write(3,REC=nREC) tmp1
        end do
        tmp1 = 6
        write(3,REC=49) tmp1                                        ! Field Length
        tmp1 = 0
        do nREC = 50,64
          write(3,REC=nREC) tmp1                                    !Unused/Reserved Bytes
        end do
        tmp1 = 13
        write(3, REC=65) tmp1                                      !Terminator (0D)
        nREC = 65
        do i = 1,ntri                                           ! Write Records
          tmp1 = 32
          do j = 1,6
            write(3, REC=nREC + j) tmp1                            !20
          end do
          tmp1 = 48
          write(3, REC=nREC+7) tmp1                                ! 30
          nREC = nREC + 7
        end do
        tmp1 = 26
        write(3,REC=nREC+1) tmp1                                   ! End of File (1A)
      CLOSE(3)

end subroutine tribuild

!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
! READ GRID DATA FOR MEMORY ALLOCATION
   subroutine read1314_alloc ( iunit, ne, np, nodemax)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: iunit
      integer, intent(out) :: ne, np
      integer, intent(out) :: nodemax ! for Sequencer of node
      integer :: i, j, k
!
      nodemax = 0
         read(iunit,*)
         read(iunit,*) ne, np
         do k = 1, np
            read(iunit,*) i
            nodemax = max(i,nodemax)
         enddo
         do k = 1, ne
            read(iunit,*)
         enddo
!
   end subroutine read1314_alloc
!
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
! READ GRID DATA
   subroutine read1314 ( iunit, ne, np, nodemax, nsequencer, agrid, xyd, nm)
!-----+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
      implicit none
      integer, intent(in) :: iunit, nodemax
      integer, intent(inout) :: ne, np
      double precision, intent(out) :: xyd(3,np)
      integer, intent(out) :: nm(ne,3)
      character(120), intent(out) :: agrid
!
      integer, intent(out) :: nsequencer(nodemax)
!
     integer :: i, j, k, jn, je, nhy
!
      nsequencer(:) = 0
!
        read(iunit,'(a)') agrid
         read(iunit,*) ne, np
         do k = 1, np
            read(iunit,*) jn, (xyd(j,k), j=1,3)
            nsequencer(jn) = k
         enddo
         write(6,*) '  + '
         do k = 1, ne
            read(iunit,*) je, nhy, ( nm(k,j), j = 1, 3 )
            do j = 1, 3
               if( nm(k,j) <= 0 ) write(6,*) k,j, nm(k,j)
               nm(k,j) = nsequencer(nm(k,j))
            enddo
         enddo
!
!      deallocate( nsequencer )
!
   end subroutine read1314
!

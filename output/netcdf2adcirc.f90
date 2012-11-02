!--------------------------------------------------------------------------
! netcdf2adcirc.f90
!
! A program to convert adcirc files that are in netcdf format to
! adcirc ascii format.
!--------------------------------------------------------------------------
! Copyright(C) 2012 Jason Fleming
!
! This file is part of the ADCIRC Surge Guidance System (ASGS).
!
! The ASGS is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! ASGS is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
!--------------------------------------------------------------------------
!
! Example of compiling this program with g95:
! g95 -o netcdf2adcirc.x -ffree-form -ffree-line-length-huge -I/usr/local/netcdf/netcdf-4.1.1/f90 netcdf2adcirc.f90 -L/usr/local/hdf5/hdf5-1.8.8/hdf5/lib -lnetcdf -lhdf5_hl -lhdf5 -lhdf5_fortran -lz
!
! Example of compiling this program with pgf90:
! pgf90 -o netcdf2adcirc.x -Mpreprocess -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/opt/cray/netcdf/4.1.3/pgi/109/include  netcdf2adcirc.f90 -lnetcdf
!
! Example of compiling this program with gfortran:
! gfortran -o netcdf2adcirc.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I$HOME/include -L$HOME/lib netcdf2adcirc.f90 -lnetcdf -lnetcdff
!
! Example of compiling this program with intel fortran at RENCI: 
! ifort -o netcdf2adcirc.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -i-dynamic -I/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.1.2-gcc4.1-ifort/include -L/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.1.2-gcc4.1-ifort/lib netcdf2adcirc.f90 -lnetcdf -lnetcdff -lz
!
      include 'adcmesh.f90'

      program netcdf2adcirc
      use adcmesh
      use netcdf
      implicit none

      integer :: iargc
      character(NF90_MAX_NAME) :: varName(3)
      character(NF90_MAX_NAME) :: thisVarName
      integer :: NC_VarID(3)
      integer :: NC_DimID_time
      integer :: NC_VarID_time
      integer :: nc_count(2)
      integer :: nc_start(2)
      integer :: nc_count3D(3)
      integer :: nc_start3D(3)
      integer :: numNodesNonDefault
      integer :: nc_id
      integer :: ndim
      integer :: nvar
      integer :: natt
      integer :: ncformat
      integer :: ndset
      integer :: num_components
      integer :: ag,agold,agnew
      real(8) :: time_increment
      real(8) :: defaultValue
      real(8), allocatable :: timesec(:)  ! time in seconds associated with each dataset
      real(8), allocatable :: adcirc_data(:,:) ! generic holder for converted data
      real(8), allocatable :: adcirc_data3D(:,:,:) ! generic holder for converted data
      integer :: nspool
      integer :: it
      integer :: argcount
      integer :: i, j, k, m
      character(1024) :: cmdlineopt
      character(1024) :: cmdlinearg
      character(1024) :: datafile
      character(1024) :: ascii_datafile_name
      logical :: meshonly
      logical :: sparse
      logical :: stationfile ! true if the data represent recording stations

      meshonly = .false.
      sparse = .false.
      stationfile = .false.
      agrid = 'null'

      write(6,*) "INFO: adcirc2netcdf was compiled with the following netcdf library: ",trim(nf90_inq_libvers())

      deg2rad = 2*pi/360.0

      argcount = iargc() ! count up command line options
      if (argcount.gt.0) then
         i=0
         do while (i.lt.argcount)
            i = i + 1
            call getarg(i, cmdlineopt)
            select case(trim(cmdlineopt))
               case("--meshonly")
                  meshonly = .true.
                  write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
               case("--sparse")
                  sparse = .true.
                  write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
               case("--datafile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  datafile = trim(cmdlinearg)
               case default
                  write(6,*) "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
            end select
         end do
      end if
      ! open the netcdf file
      call check(nf90_open(trim(datafile), NF90_NOWRITE, nc_id))
      ! determine the type of data stored in the file
      call check(nf90_inquire(nc_id, ndim, nvar, natt, nc_dimid_time, ncformat))
      if ( (ncformat.eq.nf90_format_netcdf4).or.(ncformat.eq.nf90_format_netcdf4_classic) ) then
         write(6,*) "INFO: The data file uses netcdf4 formatting."
      endif
      ! determine the number of snapshots in the file
      call check(nf90_inquire_dimension(nc_id, nc_dimid_time, len=ndset))
      !
      !  get time
      !
      ! load up the time values (in seconds)
      allocate(timesec(ndset))
      call check(nf90_inq_varid(nc_id, "time", NC_VarID_time))
      call check(nf90_get_var(nc_id, NC_VarID_time, timesec, (/ 1 /), (/ ndset /) ))
      !
      ! determine the type of file that we have
      varname(:) = "null"
      num_components = 1
      do i=1,nvar
         call check(nf90_inquire_variable(nc_id, i, thisVarName))
         select case(trim(thisVarName))
         case("station_name")
            stationfile = .true.
         case("u-vel3D","v-vel3D","w-vel3D")
            write(6,*) "INFO: Preparing to write an ADCIRC 3D water current velocity (fort.45) file."
            ascii_datafile_name = "fort.45"
            num_components = 3
            varname(1) = "u-vel3D"
            varname(2) = "v-vel3D"
            varname(3) = "w-vel3D"
            exit
         case("zeta")
            write(6,*) "INFO: Preparing to write an ADCIRC water surface elevation (fort.63) file."
            ascii_datafile_name = "fort.63"
            varname(1) = "zeta"
            exit
         case("u-vel","v-vel")
            write(6,*) "INFO: Preparing to write an ADCIRC water current velocity (fort.64) file."
            ascii_datafile_name = "fort.64"
            num_components = 2
            varname(1) = "u-vel"
            varname(2) = "v-vel"
            exit
         case("pressure")
            write(6,*) "INFO: Preparing to write an ADCIRC barometric pressure (fort.73) file."
            ascii_datafile_name = "fort.73"
            varname(1) = "pressure"
            exit
         case("windx","windy")
            write(6,*) "INFO: Preparing to write an ADCIRC wind velocity (fort.74) file."
            ascii_datafile_name = "fort.74"
            num_components = 2
            varname(1) = "windx"
            varname(2) = "windy"
            exit
         case("zeta_max")
            write(6,*) "INFO: Preparing to write an ADCIRC maximum water elevation (maxele.63) file."
            ascii_datafile_name = "maxele.63"
            ndset = 1
            varname(1) = "zeta_max"
            exit
         case("maxwvel")
            write(6,*) "INFO: Preparing to write an ADCIRC maximum wind speed (maxwvel.63) file."
            ascii_datafile_name = "maxwvel.63"
            ndset = 1
            varname(1) = "maxwvel"
            exit
         case("dir")
            write(6,*) "INFO: Preparing to write a swan_DIR.63 file."
            ascii_datafile_name = "swan_DIR.63"
            varname(1) = "dir"
            exit
         case("hs")
            write(6,*) "INFO: Preparing to write a swan_HS.63 file."
            ascii_datafile_name = "swan_HS.63"
            varname(1) = "hs"
            exit
         case("tmm10")
            write(6,*) "INFO: Preparing to write a swan_TMM10.63 file."
            ascii_datafile_name = "swan_TMM10.63"
            varname(1) = "tmm10"
            exit
         case("tps")
            write(6,*) "INFO: Preparing to write a swan_TPS.63 file."
            ascii_datafile_name = "swan_TPS.63"
            varname(1) = "tps"
            exit
         case default
            !jgf this is tmi: write(6,*) "INFO: Did not recognize the variable name '"//trim(thisVarName)//"'."
            cycle     ! did not recognize this variable name
         end select
      end do
      ! if this is actually a station file, change the ascii name
      if ( stationfile.eqv..true.) then
         select case(trim(ascii_datafile_name))
         case("fort.63")
            ascii_datafile_name = "fort.61"
         case("fort.74")
            ascii_datafile_name = "fort.72"
         end select
         call check(nf90_inq_dimid(nc_id, "station", nc_dimid_node))
      else
         ! determine the number of nodes
         call check(nf90_inq_dimid(nc_id, "node", nc_dimid_node))
         agold=nf90_get_att(nc_id,nf90_global,'grid',agrid)
         agnew=nf90_get_att(nc_id,nf90_global,'agrid',agrid)
         if(agold.EQ.NF90_NOERR)then
           ag=nf90_get_att(nc_id,nf90_global,'grid',agrid)
         elseif(agnew.EQ.NF90_NOERR)then
           ag=nf90_get_att(nc_id,nf90_global,'agrid',agrid)
         else
           call check(agnew)
         endif
      endif
      call check(nf90_inquire_dimension(nc_id, nc_dimid_node, len=np))
      ! determine time increment between output writes
      if ( ndset.gt.1 ) then
         time_increment = timesec(2) - timesec(1)
      else
         time_increment = -99999.d0
      endif
      nspool = -99999
      it = -99999
      defaultValue = -99999.d0

      ! get the variable id(s) of the data we want to convert
      do i=1,num_components
         call check(nf90_inq_varid(nc_id, varname(i), nc_varid(i)))
      end do

      if ((num_components.eq.1).or.(num_components.eq.2)) then
          allocate(adcirc_data(np,num_components))
         nc_count = (/ np, 1 /)
      endif
      if (num_components.eq.3) then
         call check(nf90_inq_dimid(nc_id, "num_v_nodes", nc_dimid_vnode))
         call check(nf90_inquire_dimension(nc_id, nc_dimid_vnode, len=nfen))
         nc_count3D = (/ np, nfen, 1 /)
         allocate(adcirc_data3D(np,nfen,num_components))
         allocate(sigma(nfen))
         call check(nf90_inq_varid(nc_id, "sigma", nc_varid_sigma))
         call check(nf90_get_var(nc_id, nc_varid_sigma, sigma))
      endif

      write(6,*) "INFO: Commence writing file ..."
      !
      ! open the ascii adcirc file that will hold the data
      open(11,file=trim(ascii_datafile_name),status='replace',action='write')
      ! write header info
      write(11,'(A)') trim(agrid)
      if ((num_components.eq.1).or.(num_components.eq.2)) then
         write(11,1010) ndset, np, time_increment, nspool, num_components
      endif
      if (num_components.eq.3) then
         write(11,1011) ndset, np, time_increment, nspool, nfen, num_components
      endif
      do i=1,ndset
         if ((num_components.eq.1).or.(num_components.eq.2)) then
            nc_start = (/ 1, i /)
         endif
         if (num_components.eq.3) then
            nc_start3D = (/ 1, 1, i /)
         endif
         do j=1,num_components
            if ((num_components.eq.1).or.(num_components.eq.2)) then
               call check(nf90_get_var(nc_id,nc_varid(j),adcirc_data(:,j),nc_start,nc_count))
            endif
            if (num_components.eq.3) then
               call check(nf90_get_var(nc_id,nc_varid(j),adcirc_data3D(:,:,j),nc_start3D,nc_count3D))
            endif
         end do
         if ( sparse.eqv..true. ) then
            numNodesNonDefault = count( adcirc_data(:,1).eq.-99999.d0 )
            write(11,*) timesec(i), it, numNodesNonDefault, defaultValue
            do k=1,np
               if ( adcirc_data(k,1).ne.-99999.d0 ) then
                  write(11,*) k,(adcirc_data(k,j),j=1,num_components)
               endif
             end do
         else
            if ((num_components.eq.1).or.(num_components.eq.2)) then
               write(11,2120) timesec(i), it
               do k=1,np
                  write(11,2453) k,(adcirc_data(k,j),j=1,num_components)
               end do
            endif
            if (num_components.eq.3) then
               write(11,2121) timesec(i), it, (sigma(m),sigma(m),sigma(m),m=1,nfen-1),sigma(nfen),sigma(nfen)
               do k=1,np
                  write(11,2454) k,(adcirc_data3D(k,j,1),adcirc_data3D(k,j,2),adcirc_data3D(k,j,3),j=1,nfen)
               end do
            endif
         endif
         write(6,advance='no',fmt='(I4)') i
      enddo
      write(6,'(/,A)') "INFO: ... finished writing file."
      write(6,*) "INFO: Wrote ",i-1," data sets."
      close(11)
      call check(nf90_close(nc_id))
!
 1010 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,'FileFmtVersion: ',I10)
 1011 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,I2,1X,'FileFmtVersion: ',I10)
 2120 FORMAT(2X,1pE20.10E3,5X,I10)
 2121 FORMAT(2X,1pE20.10E3,5X,I10,99(1pE20.10E3,2X))
 2453 FORMAT(2x, i8, 2x, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3)
 2454 FORMAT(2x, i8, 2x, 99(1pE20.10E3))
!---------------------------------------------------------------------
      end program netcdf2adcirc
!---------------------------------------------------------------------



!----------------------------------------------------------------------
!  CHECK
!---------------------------------------------------------------------
      SUBROUTINE Check(ncStatus)
 !     USE DATA,ONLY: MyRank
      USE netcdf
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: ncStatus
      IF(ncStatus.NE.NF90_NOERR)THEN
         WRITE(*,'(A,A)') "ERROR: NetCDF: ",TRIM(NF90_STRERROR(ncStatus))
         STOP
      ENDIF
!---------------------------------------------------------------------
      END SUBROUTINE check
!---------------------------------------------------------------------



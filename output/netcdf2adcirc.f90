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
      include 'adcmesh.f90'

      program netcdf2adcirc
      use adcmesh
      use netcdf
      implicit none

      character(NF90_MAX_NAME) :: varName(3)
      character(NF90_MAX_NAME) :: thisVarName
      integer :: NC_VarID(3)
      integer :: NC_DimID_time
      integer :: NC_VarID_time
      integer :: nc_count(2)
      integer :: nc_start(2)
      integer :: numNodesNonDefault
      integer :: nc_id
      integer :: ndim
      integer :: nvar
      integer :: natt
      integer :: ncformat
      integer :: ndset
      integer :: num_components
      real(8) :: time_increment
      real(8) :: defaultValue
      real(8), allocatable :: timesec(:)  ! time in seconds associated with each dataset
      real(8), allocatable :: adcirc_data(:,:) ! generic holder for converted data
      integer :: nspool
      integer :: it
      integer :: argcount
      integer :: i, j, k
      character(1024) :: cmdlineopt
      character(1024) :: cmdlinearg
      character(1024) :: datafile
      character(1024) :: ascii_datafile_name
      logical :: meshonly
      logical :: sparse

      meshonly = .false.
      sparse = .false.

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
      ! determine the number of nodes
      call check(nf90_inq_dimid(nc_id, "node", nc_dimid_node))
      call check(nf90_inquire_dimension(nc_id, nc_dimid_node, len=np))
      !
      !  get time
      !
      ! load up the time values (in seconds)
      allocate(timesec(ndset))
      call check(nf90_inq_varid(nc_id, "time", NC_VarID_time))
      call check(nf90_get_var(nc_id, NC_VarID_time, timesec, (/ 1 /), (/ ndset /) ))
      !
      ! determine the type of file that we have
      varname(1) = "null"
      num_components = 1
      do i=1,nvar
         call check(nf90_inquire_variable(nc_id, i, thisVarName))
         select case(trim(thisVarName))
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
         case("maxele")
            write(6,*) "INFO: Preparing to write an ADCIRC maximum water elevation (maxele.63) file."
            ascii_datafile_name = "maxele.63"
            ndset = 1
            varname(1) = "maxele"
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
      !
      agrid = 'null'
      call check(nf90_get_att(nc_id,nf90_global,'grid',agrid))
      ! determine time increment between output writes
      if ( ndset.gt.1 ) then
         time_increment = timesec(2) - timesec(1)
      else
         time_increment = -99999.d0
      endif
      nspool = -99999
      it = -99999
      defaultValue = -99999.d0
      !
      ! open the ascii adcirc file that will hold the data
      open(11,file=trim(ascii_datafile_name),status='replace',action='write')
      ! write header info
      write(11,'(A)') trim(agrid)
      write(11,1010) ndset, np, time_increment, nspool, num_components
      ! get the variable id(s) of the data we want to convert
      do i=1,num_components
         call check(nf90_inq_varid(nc_id, varname(i), nc_varid(i)))
      end do
      allocate(adcirc_data(np,num_components))
      nc_count = (/ np, 1 /)
      write(6,*) "INFO: Commence writing file ..."
      do i=1,ndset
         nc_start = (/ 1, i /)
         do j=1,num_components
            call check(nf90_get_var(nc_id,nc_varid(j),adcirc_data(:,j),nc_start,nc_count))
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
            write(11,2120) timesec(i), it
            do k=1,np
               write(11,2453) k,(adcirc_data(k,j),j=1,num_components)
            end do
         endif
         write(6,advance='no',fmt='(I4)') i
      enddo
      write(6,'(/,A)') "INFO: ... finished writing file."
      write(6,*) "INFO: Wrote ",i-1," data sets."
      close(11)
      call check(nf90_close(nc_id))
!
 1010 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,'FileFmtVersion: ',I10)
 2120 FORMAT(2X,1pE20.10E3,5X,I10)
 2453 FORMAT(2x, i8, 2x, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3)
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



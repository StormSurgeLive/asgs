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
      program netcdf2adcirc
      use netcdf
      use adcmesh
      implicit none

      character(NF90_MAX_NAME) :: varName

      write(6,*) "INFO: adcirc2netcdf was compiled with the following netcdf library: ",trim(nf90_inq_libvers())

      deg2rad = 2*pi/360.0
      meshFileName = "null"
      attFile = "null"
      menuOpt = 0
      useNetCDF4 = .false.
      meshonly = .false.
      withXDMF = .false.
      onlyXDMF = .false.
      projectCPP = .false.
      cppUpdated = .false.

      argcount = iargc() ! count up command line options
      if (argcount.gt.0) then
         i=0
         do while (i.lt.argcount)
            i = i + 1
            call getarg(i, cmdlineopt)
            select case(trim(cmdlineopt))
               case("--meshonly")
                  meshonly = .true.
                  menuOpt = 14
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
      call check(nf90_open(trim(datafile), NF90_NOWRITE, nc_id)
      ! determine the type of data stored in the file
      call check(nf90_inquire(nc_id, ndim, nvar, natt, nc_dimid_time, ncformat))
      if ( (ncformat.eq.nf90_format_netcdf4).or.(ncformat.eq.nf90_format_netcdf4_classic) ) then
         write(6,*) "INFO: The data file uses netcdf4 formatting."
      endif
      ! determine the number of snapshots in the file
      call check(nf90_inq_dimlen(nc_id, nc_dimid_time, numSnaps))
      ! determine the number of nodes
      call check(nf90_inq_dimid(nc_id, "node", nc_dimid_node)
      call check(nf90_inq_dimlen(nc_id, nc_dimid_node, NumNodes)
      !
      !  get time
      call check(nf90_inq_varid(nc_id, "time", nc_dimid_time))
      call check(nf90_get_vara_DOUBLE(nc_id, nc_dimid_time, outputTime)
      !
      !  get data
      do i=1,nvar
         call check(nf90_inquire_variable(nc_id, i, varName)
         select case(trim(varName))
            case("zeta")
            case("u-vel")
            case("v-vel")
            case("pressure")
            case("windx")
            case("windy")
            case("maxele")
         end select

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



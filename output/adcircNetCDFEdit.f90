!--------------------------------------------------------------------------
! adcircNetCDFEdit.f90
!
! A program to modify an existing NetCDF formatted ADCIRC file.
!
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

! adcircNetCDFEdit.f90
! 
! Copyright(C) 2012 Jason Fleming
!
! Example of compiling adcircNetCDFEdit.f90 with gfortran:
! gfortran -o adcircNetCDFEdit.x -cpp -ffree-form -ffree-line-length-none -I. adcircNetCDFEdit.f90 
! 
! Example of compiling adcircNetCDFEdit.f90 with ifort on blueridge:
! for production:
! ifort -o adcircNetCDFEdit.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -i-dynamic -I/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.1.2-gcc4.1-ifort/include -L/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.1.2-gcc4.1-ifort/lib adcircNetCDFEdit.f90 -lnetcdf -lnetcdff -lz
! for debugging:
! ifort -o adcircNetCDFEdit.x -cpp -g -O0 -traceback -debug -check all -DDEBUG -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -i-dynamic -I/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.1.2-gcc4.1-ifort/include -L/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.1.2-gcc4.1-ifort/lib adcircNetCDFEdit.f90 -lnetcdf -lnetcdff -lz


include 'adcmesh.f90'

   program adcircNetCDFEdit

      USE adcmesh
      USE netcdf
      IMPLICIT NONE
      CHARACTER(120)                :: InputFile
      character(1000)               :: Line
      DOUBLE PRECISION              :: DefaultValue, FillValue=-99999.0d0
      DOUBLE PRECISION, ALLOCATABLE :: Global1(:), Global2(:), Global3(:)
      integer                       :: natt, i, j, k, N, SS, NumNodes, NumSnaps
      integer                       :: nopt         ! actual number of files to convert
      integer                       :: menuOpt   ! user's selection
      integer                       :: argcount  ! number of command line arguments
      integer                       :: iargc     ! function to return command line arguments
      character(2048)               :: cmdlineopt ! command line option
      character(2048)               :: cmdlinearg ! content of command line argument
      character(1024) :: datafile
                                                  ! files formatted in hdf5 format
      integer :: nc_id
      integer :: var_id
      integer :: NC_DimID_time
      integer :: NC_VarID_time
      integer :: ncformat
      integer :: ndim
      integer :: nvar
      integer :: att_type
      integer :: att_len
      integer :: att_num
      character(80) :: dummy
      real(8) :: dataval
      integer :: nodenum
      character(2048) :: oldAttValue
      character(2048) :: newValue
      logical :: changeAtt
      logical :: newValueProvided
      character(2048) :: attToChange
      logical :: variableSpecified
      character(2048) :: targetVariable
      !
      ! initializations   
      meshFileName = "null"
      variableSpecified = .false.
      newValueProvided = .false.
      targetVariable(:) = ' ' 
      newValue(:) = ' ' 
      attToChange(:) = ' ' 
      datafile(:) = ' ' 
      !
      ! Report netcdf version
      write(6,*) "INFO: adcircNetCDFEdit was compiled with the following netcdf library: ",trim(nf90_inq_libvers())

      argcount = iargc() ! count up command line options
      if (argcount.gt.0) then
         i=0
         do while (i.lt.argcount)
            i = i + 1
            call getarg(i, cmdlineopt)
            select case(trim(cmdlineopt))
               case("--meshfile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  meshFileName = trim(cmdlinearg)
               case("--datafile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  InputFile = trim(cmdlinearg)
               case("--variable")
                  variableSpecified = .true.
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  targetVariable = trim(cmdlinearg)
               case("--attribute")
                  changeAtt = .true.
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  attToChange = trim(cmdlinearg)
               case("--new-value")
                  newValueProvided = .true.
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  newValue = trim(cmdlinearg)
               case default
                  write(6,*) "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
            end select
         end do
      end if
      !
      ! sanity check on command line options
      if (changeAtt.ne.newValueProvided) then
         write(6,*) "ERROR: To change the value of an attribute, "    &
         //"the attribute to change must be provided with the "       &
         //"--attribute option, and the new value for the attribute " &
         //"must be specified with the --new-value option."
         stop
      endif
      !
      ! open the netcdf file
      write(6,*) "INFO: Opening the file '",trim(InputFile),"'."
      call check(nf90_open(trim(InputFile), NF90_WRITE, nc_id))
      ! determine the type of data stored in the file
      call check(nf90_inquire(nc_id, ndim, nvar, natt, nc_dimid_time, ncformat))
      if ( (ncformat.eq.nf90_format_netcdf4).or.(ncformat.eq.nf90_format_netcdf4_classic) ) then
         write(6,*) "INFO: The data file uses netcdf4 formatting."
      endif

      if ( changeAtt.eqv..true. ) then
         ! if the attribute that is to be changed is associated with 
         ! a certain variable (as opposed to being a global attribute)
         ! then get the variable id here
         if ( variableSpecified.eqv..true.) then
            call check(nf90_inq_varid(nc_id, trim(targetVariable),var_id))
         else
            var_id = NF90_GLOBAL
         endif
         ! get info about attribute 
         call check(nf90_inquire_attribute(nc_id, var_id, attToChange, att_type, att_len, att_num))
         ! get current attribute value
         call check(nf90_get_att(nc_id, var_id, trim(attToChange), oldAttValue))
         write(6,*) "INFO: Changing '",trim(attToChange),"' from '",trim(oldAttValue),"' to '",trim(newValue),"'."
         ! go into netcdf "redefinition" mode so we can change attributes
         call check(nf90_redef(nc_id))
         ! put in the new value for this attribute
         call check(nf90_put_att(nc_id, var_id, trim(attToChange), trim(newValue)))
         ! leave netcdf "redefinition" mode
         call check(nf90_enddef(nc_id))
         ! verify that we now have the expected value in the file
         call check(nf90_get_att(nc_id, var_id, trim(attToChange), oldAttValue))
         write(6,*) "INFO: The value of '",trim(attToChange),"' is now '",trim(oldAttValue),"'."
      endif 
      call check(nf90_close(nc_id))
      end program adcircNetCDFEdit

!----------------------------------------------------------------------
!  CHECK
!---------------------------------------------------------------------
      SUBROUTINE Check(ncStatus)
      USE netcdf
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: ncStatus
#ifdef DEBUG
      ! if we are in debug mode, create a fake array that will be 
      ! used to generate a stack trace
      REAL, ALLOCATABLE :: debug_dummy(:)
#endif
      IF(ncStatus.NE.NF90_NOERR)THEN
         WRITE(*,'(A,A)') "adcircNetCDFEdit.x: ERROR: NetCDF: ",TRIM(NF90_STRERROR(ncStatus))
#ifdef DEBUG
         ! intentionally create a segmentation fault so that we can get
         ! a stack trace to determine the line number of the netcdf call
         ! that went bad ... this assumes that the code was compiled with
         ! debugging symbols, bounds checking, and stack trace turned on.
         debug_dummy(10) = 99.9d0
#endif
         STOP
      ENDIF
!---------------------------------------------------------------------
      END SUBROUTINE check
!---------------------------------------------------------------------


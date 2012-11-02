!--------------------------------------------------------------------------
! adcircMaxColumnar.f90
!
! A program to reformat ADCIRC min/max data into columns with long, 
! lat, max, total depth.
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

! adcircMaxColumnar.f90
! 
! Copyright(C) 2012 Jason Fleming
!
! Example of compiling adcircMaxColumnar.f90 with gfortran:
! gfortran -o adcircMaxColumnar.x -cpp -ffree-form -ffree-line-length-none -I. adcircMaxColumnar.f90 

include 'adcmesh.f90'

   program adcircMaxColumnar

      USE adcmesh
      USE netcdf
      IMPLICIT NONE
      CHARACTER(120)                :: InputFile, OutputFile, AttFile
      character(120)                :: datenum
      character(120),   allocatable :: att(:,:)
      character(1000)               :: Line
      character(1)                  :: JunkC, Tadj
      DOUBLE PRECISION              :: DefaultValue, FillValue=-99999.0d0
      double precision              :: temp1, temp2, SnapR, Interval, time(1)
      DOUBLE PRECISION, ALLOCATABLE :: Global1(:), Global2(:), Global3(:)
      integer                       :: yy, mo, dd, hh, mi
      integer                       :: natt, i, j, k, N, SS, NumNodes, NumSnaps
      integer                       :: NumNodesNonDefault, SnapI
      integer                       :: unitnumber, nCol
      integer, allocatable          :: iopt(:)         ! files to convert
      integer                       :: nopt         ! actual number of files to convert
      integer                       :: menuOpt   ! user's selection
      integer                       :: argcount  ! number of command line arguments
      integer                       :: iargc     ! function to return command line arguments
      character(2048)               :: cmdlineopt ! command line option
      character(2048)               :: cmdlinearg ! content of command line argument
      logical                       :: useNetCDF4 ! .true. if user wants netcdf classic model
                                                  ! files formatted in hdf5 format
      logical                       :: meshonly   ! .true. if user just wants to conver the mesh
      integer                       :: ncFileType
      integer                       :: NC_ID
      INTEGER                       :: NC_DimID(2)
      INTEGER                       :: NC_Count(2)
      INTEGER                       :: NC_Start(2)

      integer                       :: NC_DimID_time
      integer                       :: NC_DimID_single

      integer                       :: NC_VarID_time
      integer                       :: NC_VarID_zeta
      integer                       :: NC_VarID_u_vel
      integer                       :: NC_VarID_v_vel
      integer                       :: NC_VarID_maxele
      integer                       :: NC_VarID_maxwvel
      integer                       :: NC_VarID_p
      integer                       :: NC_VarID_windx
      integer                       :: NC_VarID_windy
      integer                       :: NC_VarID_dir
      integer                       :: NC_VarID_hs
      integer                       :: NC_VarID_tmm10
      integer                       :: NC_VarID_tps
      integer, parameter            :: version = 4
      integer                       :: num_components ! variable components for netcdf4 compression
      integer                       :: varid(3) ! varids for netcdf4 compression
      character(80) :: dummy
      real(8) :: dataval
      integer :: nodenum


      ! initializations
      deg2rad = 2*pi/360.0
      meshFileName = "null"
      attFile = "null"
      menuOpt = 0
      useNetCDF4 = .false.
      meshonly = .false.
      !
      !write(6,*) "INFO: adcirc2netcdf version ",version,"."
      ! Report netcdf version
      write(6,*) "INFO: adcirc2netcdf was compiled with the following netcdf library: ",trim(nf90_inq_libvers())

      ! jgf: Process command line options; can be used along with menu choices;
      ! if command line options provide all needed input, menu will not
      ! be presented to user; programs with command line options are
      ! slightly easier to automate than menu-based programs
      argcount = iargc() ! count up command line options
      if (argcount.gt.0) then
         i=0
         do while (i.lt.argcount)
            i = i + 1
            call getarg(i, cmdlineopt)
            select case(trim(cmdlineopt))
               case("--netcdf4")
                  useNetCDF4 = .true.
                  write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
               case("--meshonly")
                  meshonly = .true.
                  menuOpt = 14
                  write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
               case("--meshfile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  meshFileName = trim(cmdlinearg)
               case("--attfile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  attFile = trim(cmdlinearg)
               case("--datafile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  select case(trim(cmdlinearg))
                     case("fort.63")
                        menuOpt = 1
                     case("fort.64")
                        menuOpt = 2
                     case("fort.73")
                        menuOpt = 3
                     case("fort.74")
                        menuOpt = 4
                     case("maxele.63")
                        menuOpt = 5
                     case("swan_DIR.63")
                        menuOpt = 6
                     case("swan_HS.63")
                        menuOpt = 7
                     case("swan_TMM10.63")
                        menuOpt = 8
                     case("swan_TPS.63")
                        menuOpt = 9
                     case("adcirc")
                        menuOpt = 10
                     case("swan")
                        menuOpt = 11
                     case("adcirc_swan")
                        menuOpt = 12
                     case("fort.14")
                        menuOpt = 14
                     case("maxwvel.63")
                        menuOpt = 15
                     case default
                        write(6,*) "WARNING: Command line argument '",TRIM(cmdlinearg),"' was not recognized."
                  end select
                  InputFile = trim(cmdlinearg)
               case default
                  write(6,*) "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
            end select
         end do
      end if

      ! Load fort.14
      call read14()

      ! load maxele.63
      call openFileForRead(63, trim(InputFile))
      read(63,*) dummy  ! header 1
      read(63,*) dummy  ! header 2
      read(63,*) dummy  ! time and timestep
      allocate(global1(np))
      do i=1,np
         read(63,*) nodeNum, dataval
         global1(nodeNum) = dataval
      end do
      open(11,file='maxele.txt',status='replace',action='write')
      do i=1,np
         write(11,*) xyd(1,i),xyd(2,i),global1(i),global1(i)+xyd(3,i)
      end do
      close(11)
      stop
      end program adcircMaxColumnar

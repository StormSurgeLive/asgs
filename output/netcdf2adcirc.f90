!--------------------------------------------------------------------------
! netcdf2adcirc.f90
!
! A program to convert adcirc files that are in netcdf format to
! adcirc ascii format.
!--------------------------------------------------------------------------
! Copyright(C) 2012--2014 Jason Fleming
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
! Compiling with pgf90 on garnet at ERDC 20130926:
! pgf90 -o netcdf2adcirc.x -Mpreprocess -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/opt/cray/netcdf/4.3.0/pgi/121/include -L/opt/cray/netcdf/4.3.0/pgi/121/lib netcdf2adcirc.f90 -lnetcdf -lnetcdff
!
! Example of compiling this program with gfortran:
! gfortran -o netcdf2adcirc.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I$HOME/include -L$HOME/lib netcdf2adcirc.f90 -lnetcdf -lnetcdff
!
! Example of compiling this program with intel fortran at RENCI 20130516: 
! ifort -o netcdf2adcirc.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -i-dynamic -I/projects/ncfs/apps/netcdf/netcdf-fortran-4.2/include -L/projects/ncfs/apps/netcdf/netcdf-fortran-4.2/lib netcdf2adcirc.f90 -lnetcdf -lnetcdff -lz
!
! Example using ifort on Diamond at ERDC 20130726:
! ifort -cpp -o netcdf2adcirc.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -i-dynamic -I/usr/local/usp/PETtools/CE/pkgs/netcdf-4.2.1.1-intel-serial/include -L/usr/local/usp/PETtools/CE/pkgs/netcdf-4.2.1.1-intel-serial/lib netcdf2adcirc.f90 -lnetcdf -lnetcdff -lz
!
! Example of compiling this program with intel fortran on hatteras at RENCI:
! ifort -cpp -o netcdf2adcirc.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -i-dynamic -I/projects/ncfs/apps/netcdf/netcdf-fortran-4.2/include -L/projects/ncfs/apps/netcdf/netcdf-fortran-4.2/lib netcdf2adcirc.f90 -lnetcdff -lz
!
program netcdf2adcirc
use netcdf
use asgsio
use adcmesh
use adcircdata
implicit none
integer :: ag
integer :: agnew
integer :: agold
integer :: i, j, k, m, n
logical :: meshonly
integer :: ncstatus

meshonly = .false.
sparse = .false.
stationfile = .false.
extremesWithTime = .false.
agrid = 'null'

write(6,*) "INFO: adcirc2netcdf was compiled with the following " &
   // "netcdf library: ",trim(nf90_inq_libvers())

argcount = command_argument_count() ! count up command line options
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
            write(6,*) "INFO: Processing ",trim(cmdlineopt)," ", &
               trim(cmdlinearg),"."
            datafile = trim(cmdlinearg)
         case default
            write(6,*) "WARNING: Command line option '", &
               TRIM(cmdlineopt),"' was not recognized."
      end select
   end do
end if
!
! open the netcdf file
call check(nf90_open(trim(datafile), NF90_NOWRITE, nc_id))
!
! determine the type of data stored in the file
call check(nf90_inquire(nc_id, ndim, nvar, natt, &
                        nc_dimid_time, ncformat))
if ( (ncformat.eq.nf90_format_netcdf4).or. &
   (ncformat.eq.nf90_format_netcdf4_classic) ) then
   write(6,*) "INFO: The data file uses netcdf4 formatting."
endif
!
! determine the number of snapshots in the file
call check(nf90_inquire_dimension(nc_id,nc_dimid_time,len=ndset))
!
!  get time
!
! load up the time values (in seconds)
allocate(timesec(ndset))
call check(nf90_inq_varid(nc_id, "time", NC_VarID_time))
call check(nf90_get_var(nc_id, NC_VarID_time, timesec, &
   (/ 1 /), (/ ndset /) ))
!
! determine the type of file that we have
varname(:) = "null"
num_components = 1
! is it a station file?
do i=1,nvar
   call check(nf90_inquire_variable(nc_id, i, thisVarName))
   select case(trim(thisVarName))
   case("station_name")
      stationfile = .true.
      call check(nf90_inq_dimid(nc_id, "station", nc_dimid_node))
      exit
   case default
      ! do nothing
   end select
end do
! determine the type of data in the file, and set the output
! filename accordingly
do i=1,nvar
   call check(nf90_inquire_variable(nc_id, i, thisVarName))
   select case(trim(thisVarName))
   case("u-vel3D","v-vel3D","w-vel3D")
      write(6,*) "INFO: Preparing to write an ADCIRC 3D " &
         // "water current velocity file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.42"
      else
         ascii_datafile_name = "fort.45"
      endif
      num_components = 3
      varname(1) = "u-vel3D"
      varname(2) = "v-vel3D"
      varname(3) = "w-vel3D"
      exit
   case("zeta")
      write(6,*) "INFO: Preparing to write an ADCIRC water " &
         // "surface elevation file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.61"          
      else 
         ascii_datafile_name = "fort.63"
      endif 
      varname(1) = "zeta"
      exit
   case("u-vel","v-vel")
      write(6,*) "INFO: Preparing to write an ADCIRC water " &
         // "current velocity file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.62"
      else
         ascii_datafile_name = "fort.64"
      endif
      num_components = 2
      varname(1) = "u-vel"
      varname(2) = "v-vel"
      exit
   case("pressure")
      write(6,*) "INFO: Preparing to write an ADCIRC barometric " &
         // "pressure file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.71"
      else
         ascii_datafile_name = "fort.73"
      endif
      varname(1) = "pressure"
      exit
   case("windx","windy")
      write(6,*) "INFO: Preparing to write an ADCIRC wind " &
         // "velocity file."
      if ( stationfile.eqv..true. ) then
          ascii_datafile_name = "fort.72"
      else
          ascii_datafile_name = "fort.74"
      endif
      num_components = 2
      varname(1) = "windx"
      varname(2) = "windy"
      exit
   case("zeta_max")
      write(6,*) "INFO: Preparing to write an ADCIRC maximum " &
         // "water elevation file."
      ascii_datafile_name = "maxele.63"
      varname(1) = "zeta_max"
      num_components = 1
      ndset = 1
      timeOfVarName = 'time_of_'//trim(thisVarName)
      do j=1,nvar
         ! check to see if this is a new-style min/max file that records
         ! the time of the min or max, and if so, prepare to convert the
         ! time information as well
         ncStatus = nf90_inquire_variable(nc_id, j, aVarName)         
         if ((ncStatus.eq.NF90_NOERR).and.(trim(aVarName).eq.trim(timeOfVarName)) ) then
            write(6,'(a)') 'INFO: The file contains time of occurrence data.'
            extremesWithTime = .true.
            varname(2) = trim(timeOfVarName)
            num_components = 2
            ndset = 2
            exit
         endif 
      end do
      exit
   case("wind_max")
      write(6,*) "INFO: Preparing to write an ADCIRC maximum " &
         // "wind speed file."
      ascii_datafile_name = "maxwvel.63"
      ndset = 1
      num_components = 1
      varname(1) = "wind_max"
      ! check to see if this is a new-style min/max file that records
      ! the time of the min or max, and if so, prepare to convert the
      ! time information as well
      timeOfVarName = 'time_of_'//trim(thisVarName)
      do j=1,nvar
         call check(nf90_inquire_variable(nc_id, j, aVarName))
         if (trim(aVarName).eq.trim(timeOfVarName)) then
            write(6,'(a)') 'INFO: The file contains time of occurrence data.'
            extremesWithTime = .true.
            varname(2) = trim(timeOfVarName)
            num_components = 2
            ndset = 2
            exit
         endif 
      end do
      exit
   case("dir")
      write(6,*) "INFO: Preparing to write a mean wave " &
         // "direction file."
      ascii_datafile_name = "swan_DIR.63"
      varname(1) = "dir"
      exit
   case("hs")
      write(6,*) "INFO: Preparing to write a significant " &
          // "wave height file."
      ascii_datafile_name = "swan_HS.63"
      varname(1) = "hs"
      exit
   case("tmm10")
      write(6,*) "INFO: Preparing to write a mean absolute " &
        // "wave period file."
      ascii_datafile_name = "swan_TMM10.63"
      varname(1) = "tmm10"
      exit
   case("tps")
      write(6,*) "INFO: Preparing to write a relative peak " &
         // "period file."
      ascii_datafile_name = "swan_TPS.63"
      varname(1) = "tps"
      exit
   case("swan_HS_max")
      write(6,*) "INFO: Preparing to write a maximum " &
         // "significant wave height file."
      ascii_datafile_name = "swan_HS_max.63"
      ndset = 1
      varname(1) = "swan_HS_max"
      exit
   case("swan_TPS_max")
      write(6,*) "INFO: Preparing to write an maximum relative " &
        // "peak wave period file."
      ascii_datafile_name = "swan_TPS_max.63"
      ndset = 1
      varname(1) = "swan_TPS_max"
      exit                        
   case default
      !jgf this is tmi: write(6,*) "DEBUG: Did not recognize the variable name '"//trim(thisVarName)//"'."
      cycle     ! did not recognize this variable name
   end select
end do
write(6,*) "INFO: " // trim(ascii_datafile_name)
! if this is not a station file, find the mesh node dimension and
! comment 
if ( stationfile.eqv..false.) then
   ! determine the number of nodes
   call check(nf90_inq_dimid(nc_id, "node", nc_dimid_node))
   ! get the name of the mesh
   agold = nf90_get_att(nc_id,nf90_global,'grid',agrid)
   agnew = nf90_get_att(nc_id,nf90_global,'agrid',agrid)
   if (agold.EQ.NF90_NOERR) then
      ag = nf90_get_att(nc_id,nf90_global,'grid',agrid)
   elseif (agnew.EQ.NF90_NOERR) then
      ag = nf90_get_att(nc_id,nf90_global,'agrid',agrid)
   else
     write(*,'(a)') 'WARNING: The name of the mesh was not found.'
     agrid = 'mesh_name_not_found'
   endif
endif
call check(nf90_inquire_dimension(nc_id, nc_dimid_node, len=np))
! determine time increment between output writes
if ( (ndset.gt.1).and.(extremesWithTime.eqv..false.) ) then
   time_increment = timesec(2) - timesec(1)
else
   time_increment = -99999.d0
endif
nspool = -99999
it = -99999
defaultValue = -99999.d0
!
! get the variable id(s) of the data we want to convert
do i=1,num_components
   call check(nf90_inq_varid(nc_id, varname(i), nc_varid(i)))
end do
!
write(6,'(a,i0,a)') 'INFO: The file contains ',ndset,' datasets.'
write(6,*) "INFO: Commence writing file ..."
!
! open the ascii adcirc file that will hold the data
open(11,file=trim(ascii_datafile_name),status='replace',action='write')
! write header info
write(11,'(A)') trim(agrid)

select case(num_components)
case(1,2)
   allocate(adcirc_data(np,num_components))
   nc_count = (/ np, 1 /)
   if (extremesWithTime.eqv..false.) then
      write(11,1010) ndset, np, time_increment, nspool, num_components
   else
      write(11,1010) ndset, np, time_increment, nspool, 1
   endif
   !
   ! loop over datasets   
   do i=1,ndset
      !
      ! read the dataset from netcdf
      do j=1,num_components
         nc_start = (/ 1, i /)
         call check(nf90_get_var(nc_id,nc_varid(j),adcirc_data(:,j),nc_start,nc_count))
      end do
      !   
      ! write the dataset to ascii
      if ( sparse.eqv..true. ) then
         numNodesNonDefault = count( adcirc_data(:,1).eq.-99999.d0 )
         write(11,*) timesec(i), it, numNodesNonDefault, defaultValue
         do k=1,np
            if ( adcirc_data(k,1).ne.-99999.d0 ) then
               write(11,*) k,(adcirc_data(k,j),j=1,num_components)
            endif
          end do
      else
         ! nonsparse ascii output
         if (extremesWithTime.eqv..false.) then 
            write(11,2120) timesec(i), it
            do k=1,np
               write(11,2453) k,(adcirc_data(k,j),j=1,num_components)
            end do
         else
            ! min or max file with time of occurrence
            do n=1,2
               write(11,2120) timesec(i), it
               do k=1,np
                  write(11,2453) k,adcirc_data(k,n)
               end do
            end do
            exit  ! don't need to write any more data in this case
         endif
      endif
   write(6,advance='no',fmt='(I4)') i
   end do  
case(3)
   call check(nf90_inq_dimid(nc_id, "num_v_nodes", nc_dimid_vnode))
   call check(nf90_inquire_dimension(nc_id, nc_dimid_vnode, len=nfen))
   nc_count3D = (/ np, nfen, 1 /)
   allocate(adcirc_data3D(np,nfen,num_components))
   allocate(sigma(nfen))
   call check(nf90_inq_varid(nc_id, "sigma", nc_varid_sigma))
   call check(nf90_get_var(nc_id, nc_varid_sigma, sigma))
   write(11,1011) ndset, np, time_increment, nspool, nfen, num_components
   !
   ! loop over datasets   
   do i=1,ndset
      !
      ! read 3D data from netcdf
      do j=1,num_components
         nc_start3D = (/ 1, 1, i /)
         call check(nf90_get_var(nc_id,nc_varid(j),adcirc_data3D(:,:,j),nc_start3D,nc_count3D))   
      end do
      !
      ! write 3D data to ascii
      write(11,2121) timesec(i), it, (sigma(m),sigma(m),sigma(m),m=1,nfen-1),sigma(nfen),sigma(nfen)
      do k=1,np
         write(11,2454) k,(adcirc_data3D(k,j,1),adcirc_data3D(k,j,2),adcirc_data3D(k,j,3),j=1,nfen)
      end do
      write(6,advance='no',fmt='(I4)') i
   end do
case default
   write(6,'(a,i0,a)') 'ERROR Cannot convert files with ',num_components,' components.'
   stop
end select

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



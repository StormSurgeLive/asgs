!--------------------------------------------------------------------------
! netcdf2adcirc.f90
!
! A program to convert adcirc files that are in netcdf format to
! adcirc ascii format.
!--------------------------------------------------------------------------
! Copyright(C) 2012--2015 Jason Fleming
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
! Compile with accompanying makefile.
!--------------------------------------------------------------------------
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

write(6,'(a,a)') "INFO: adcirc2netcdf was compiled with the following " &
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
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--sparse")
            sparse = .true.
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
         case("--datafile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ", &
               trim(cmdlinearg),"."
            datafile = trim(cmdlinearg)
         case default
            write(6,'(a,a,a)') "WARNING: Command line option '", &
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
   write(6,'(a)') "INFO: The data file uses netcdf4 formatting."
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
   call check(nf90_inquire_variable(nc_id, i, varname(1)))
   select case(trim(varname(1)))
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
   call check(nf90_inquire_variable(nc_id, i, varname(1)))
   select case(trim(varname(1)))
   case("u-vel3D","v-vel3D","w-vel3D")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC 3D " &
         // "water current velocity file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.42"
      else
         ascii_datafile_name = "fort.45"
      endif
      num_components = 3
      varname(2) = "v-vel3D"
      varname(3) = "w-vel3D"
      exit
   case("zeta")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC water " &
         // "surface elevation file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.61"          
      else 
         ascii_datafile_name = "fort.63"
      endif 
      exit
   case("u-vel")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC water " &
         // "current velocity file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.62"
      else
         ascii_datafile_name = "fort.64"
      endif
      num_components = 2
      varname(2) = "v-vel"
      exit
   case("pressure")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC barometric " &
         // "pressure file."
      if ( stationfile.eqv..true. ) then
         ascii_datafile_name = "fort.71"
      else
         ascii_datafile_name = "fort.73"
      endif
      exit
   case("windx")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC wind " &
         // "velocity file."
      if ( stationfile.eqv..true. ) then
          ascii_datafile_name = "fort.72"
      else
          ascii_datafile_name = "fort.74"
      endif
      num_components = 2
      varname(2) = "windy"
      exit
   case("zeta_max")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC maximum " &
         // "water elevation file."
      ascii_datafile_name = "maxele.63"
      varname(2) = 'time_of_'//trim(varname(1))
      call checkForTimeOfOccurrence(nc_id, varname(2), nvar, extremesWithTime, &
         num_components, ndset)       
      exit
   case("wind_max")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC maximum " &
         // "wind speed file."
      ascii_datafile_name = "maxwvel.63"
      varname(2) = 'time_of_'//trim(varname(1))
      call checkForTimeOfOccurrence(nc_id, varname(2), nvar, extremesWithTime, &
         num_components, ndset)      
      exit     
   case("vel_max")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC maximum " &
         // "water current velocity file."
      ascii_datafile_name = "maxvel.63"
      varname(2) = 'time_of_'//trim(varname(1))      
      call checkForTimeOfOccurrence(nc_id, varname(2), nvar, extremesWithTime, &
         num_components, ndset)      
      exit
   case("pressure_min")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC maximum " &
         // "minimum pressure file."
      ascii_datafile_name = "prmin.63"
      varname(2) = 'time_of_'//trim(varname(1))
      call checkForTimeOfOccurrence(nc_id, varname(2), nvar, extremesWithTime, &
         num_components, ndset)      
      exit
   case("radstress_max")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC maximum " &
         // "wave radiation stress file."
      ascii_datafile_name = "maxrs.63"
      varname(2) = 'time_of_'//trim(varname(1))      
      call checkForTimeOfOccurrence(nc_id, varname(2), nvar, extremesWithTime, &
         num_components, ndset)      
      exit
   case("inun_time")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC " &
         // "inundation time file."
      ascii_datafile_name = "inundationtime.63"
      varname(2) = 'last_'//trim(varname(1))      
      call checkForTimeOfOccurrence(nc_id, varname(2), nvar, extremesWithTime, &
         num_components, ndset)      
      exit
   case("inun_max")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC " &
         // "maximum inundation depth file."
      ascii_datafile_name = "maxinundepth.63"
      varname(2) = 'time_of_'//trim(varname(1))      
      call checkForTimeOfOccurrence(nc_id, varname(2), nvar, extremesWithTime, &
         num_components, ndset)      
      exit      
   case("initiallydry")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC " &
         // "initially dry file."
      ascii_datafile_name = "initiallydry.63"
      isInteger = .true.
      num_components = 1
      ndset = 1     
      exit      
   case("everdried")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC " &
         // "ever dried node file."
      ascii_datafile_name = "everdried.63"
      varname(2) = 'time_of_'//trim(varname(1))
      call checkForTimeOfOccurrence(nc_id, varname(2), nvar, extremesWithTime, &
         num_components, ndset)       
      exit            
   case("endrisinginun")
      write(6,'(a)') "INFO: Preparing to write an ADCIRC " &
         // "rising inundation at end of simulation file."
      ascii_datafile_name = "endrisinginun.63"
      isInteger = .true.
      num_components = 1
      ndset = 1     
      exit      
   case("dir")
      write(6,'(a)') "INFO: Preparing to write a mean wave " &
         // "direction file."
      ascii_datafile_name = "swan_DIR.63"
      exit
   case("hs")
      write(6,'(a)') "INFO: Preparing to write a significant " &
          // "wave height file."
      ascii_datafile_name = "swan_HS.63"
      exit
   case("tmm10")
      write(6,'(a)') "INFO: Preparing to write a mean absolute " &
        // "wave period file."
      ascii_datafile_name = "swan_TMM10.63"
      exit
   case("tps")
      write(6,'(a)') "INFO: Preparing to write a relative peak " &
         // "period file."
      ascii_datafile_name = "swan_TPS.63"
      exit
   case("swan_HS_max")
      write(6,'(a)') "INFO: Preparing to write a maximum " &
         // "significant wave height file."
      ascii_datafile_name = "swan_HS_max.63"
      ndset = 1
      exit
   case("swan_TPS_max")
      write(6,'(a)') "INFO: Preparing to write a maximum relative " &
        // "peak wave period file."
      ascii_datafile_name = "swan_TPS_max.63"
      ndset = 1
      exit                        
   case default
      !jgf this is tmi: write(6,*) "DEBUG: Did not recognize the variable name '"//trim(thisVarName)//"'."
      cycle     ! did not recognize this variable name
   end select
end do
write(6,'(a)') "INFO: " // trim(ascii_datafile_name)
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
     write(6,'(a)') 'WARNING: The name of the mesh was not found.'
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
write(6,'(a)') "INFO: Commence writing file ..."
!
! open the ascii adcirc file that will hold the data
open(11,file=trim(ascii_datafile_name),status='replace',action='write')
! write header info
write(11,'(a)') trim(agrid)

select case(num_components)
case(1,2)
   if ( isInteger.eqv..true.) then
      allocate(adcirc_idata(np,num_components))
   endif
   allocate(adcirc_data(np,num_components))
   nc_count = (/ np, 1 /)
   !
   ! min or max file with time of occurrence
   if (extremesWithTime.eqv..true.) then
      write(11,1010) ndset, np, time_increment, nspool, 1
      write(11,2120) timesec(1), it
      nc_start = (/ 1, 1 /)
      if (isInteger.eqv..true.) then
         call check(nf90_get_var(nc_id,nc_varid(1),adcirc_idata(:,1),nc_start,nc_count))
         do k=1,np
            write(11,2452) k,adcirc_idata(k,1)
         end do
      else
         call check(nf90_get_var(nc_id,nc_varid(1),adcirc_data(:,1),nc_start,nc_count))
         do k=1,np
            write(11,2453) k,adcirc_data(k,1)
         end do
      endif
      ! time of occurrence data
      write(11,2120) timesec(1), it
      nc_start = (/ 1, 2 /)      
      call check(nf90_get_var(nc_id,nc_varid(2),adcirc_data(:,2),nc_start,nc_count))
      do k=1,np
         write(11,2453) k,adcirc_data(k,2)
      end do
   else
      write(11,1010) ndset, np, time_increment, nspool, num_components
      !
      ! loop over datasets (either a time varying file or a min max file
      ! without time of occurrence information
      do i=1,ndset
         !
         ! read the dataset from netcdf
         do j=1,num_components
            nc_start = (/ 1, i /)
            if (isInteger.eqv..true.) then
               call check(nf90_get_var(nc_id,nc_varid(j),adcirc_idata(:,j),nc_start,nc_count))
            else
               call check(nf90_get_var(nc_id,nc_varid(j),adcirc_data(:,j),nc_start,nc_count))
            endif
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
            write(11,2120) timesec(i), it
            do k=1,np
               if (isInteger.eqv..true.) then
                  write(11,2452) k,(adcirc_idata(k,j),j=1,num_components)
               else
                  write(11,2453) k,(adcirc_data(k,j),j=1,num_components)
               endif
            end do
         endif
         write(6,advance='no',fmt='(i4)') i
      end do
   endif
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
      write(6,advance='no',fmt='(i4)') i
   end do
case default
   write(6,'(a,i0,a)') 'ERROR Cannot convert files with ',num_components,' components.'
   stop
end select

write(6,'(/,A)') "INFO: ... finished writing file."
write(6,'(a,i0,a)') "INFO: Wrote ",i-1," data sets."
close(11)
call check(nf90_close(nc_id))
!
 1010 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,'FileFmtVersion: ',I10)
 1011 FORMAT(1X,I10,1X,I10,1X,E15.7E3,1X,I8,1X,I5,1X,I2,1X,'FileFmtVersion: ',I10)
 2120 FORMAT(2X,1pE20.10E3,5X,I10)
 2121 FORMAT(2X,1pE20.10E3,5X,I10,99(1pE20.10E3,2X))
 2452 FORMAT(2x, i8, 2x, i0, 5x, i0, 5x, i0, 5x, i0)
 2453 FORMAT(2x, i8, 2x, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3, 1pE20.10E3)
 2454 FORMAT(2x, i8, 2x, 99(1pE20.10E3))
!---------------------------------------------------------------------
      end program netcdf2adcirc
!---------------------------------------------------------------------


!-----------------------------------------------------------------------
!                      S U B R O U T I N E     
!      C H E C K   F O R   T I M E   O F   O C C U R R E N C E  
!-----------------------------------------------------------------------
! Check to see if this is a new-style min/max file that records
! the time of the min or max, and if so, prepare to convert the
! time information as well.     
!-----------------------------------------------------------------------
subroutine checkForTimeOfOccurrence(ncid, vn, nvar, found, numComp, ndset)
use netcdf
implicit none

integer, intent(in) :: ncid ! netcdf ID of the file we are reading 
character(NF90_MAX_NAME), intent(in) :: vn  ! time of occurrence variable name
integer, intent(in) :: nvar  ! number of variables in the file
logical, intent(out) :: found ! .true. if time of occurrence data were found
integer, intent(out) :: numComp ! number of components of the data
integer, intent(out) :: ndset ! number of datasets in the data
!
integer :: ncStatus ! i/o status of netcdf call
character(NF90_MAX_NAME) :: aVarName ! name of variable in netcdf file
integer :: j
 
do j=1,nvar
   ncStatus = nf90_inquire_variable(ncid, j, aVarName)         
   if ((ncStatus.eq.NF90_NOERR).and.(trim(aVarName).eq.trim(vn)) ) then
      write(6,'(a)') 'INFO: The file contains time of occurrence data.'
      found = .true.
      numComp = 2
      ndset = 2
      exit
   else
      found = .false.
      numComp = 1
      ndset = 1
   endif 
end do
!-----------------------------------------------------------------------
end subroutine checkForTimeOfOccurrence
!-----------------------------------------------------------------------

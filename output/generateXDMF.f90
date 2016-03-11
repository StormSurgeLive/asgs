!--------------------------------------------------------------------------
! generateXDMF.f90
!
! A program to generate XDMF xml for NetCDF4 formatted ADCIRC files.
!
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
! Compile this program with the accompanying makefile.
!--------------------------------------------------------------------------
program generateXDMF
use netcdf
use asgsio, only : check, fileMetaData_t
use adcmesh
implicit none
integer :: iargc
!
! NetCDF related variables
logical :: fileFound = .false.
integer :: ncStatus
integer :: NC_DimID_time
integer :: NC_VarID_time
integer :: NC_VarID
integer :: nc_id
integer :: ndim     ! number of dimensions in the netcdf file
integer :: nvar     ! number of variables in the netcdf file
integer :: natt     ! number of global attributes in the netcdf file
integer :: ncformat ! whether netcdf 3 or netcdf 4
integer :: nSnaps   ! number of snapshots in time (length of netcdf time dimension)
integer :: iSnap    ! snapshot counter
real(8), allocatable :: timesec(:)  ! time in seconds associated with each dataset
character(NF90_MAX_NAME) :: thisVarName ! netcdf variable names
logical :: nodalAttributesFile ! .true. if the netcdf file contains nodal attributes
!
! XDMF XML related variables.
character(1024) :: xmf ! name of XDMF xml file
!character(NF90_MAX_NAME) :: varMagName
!
type(fileMetaData_t) :: fileMetaData
!
integer argcount
character(1024) :: cmdlineopt
character(1024) :: cmdlinearg
!logical :: useMag ! true if vector magnitude data are found in the file
integer i, j ! loop counters

!useMag = .false.
fileMetaData % useCPP = .false.
fileMetaData % initialized = .false.
fileMetaData % xmfUnit = 10 

argcount = iargc() ! count up command line options
if (argcount.gt.0) then
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
         case("--datafile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a,a,a,a,a)') "INFO: generateXDMF.f90: Processing '",trim(cmdlineopt)," ",trim(cmdlinearg),"'."
            fileMetaData%netCDFFile = trim(cmdlinearg)
         case("--use-cpp")
            fileMetaData % useCPP = .true.
            write(6,'(a,a,a)') "INFO: generateXDMF.f90: Processing ",trim(cmdlineopt),"."
         case default
      end select
   end do
end if
!
! Check to see if the NetCDF file exists.
inquire(FILE=trim(fileMetaData%netCDFFile),EXIST=fileFound)
if (fileFound.eqv..false.) then
   write(6,'("ERROR: generateXDMF.f90: The file ",A," was not found.")') trim(fileMetaData%netCDFFile)
   stop
else
   ! netcdf file exists; open it
   call check(nf90_open(trim(fileMetaData%netCDFFile), NF90_NOWRITE, nc_id))
endif
!
! Make sure the file is NetCDF4 formatted (i.e., HDF5 underneath) because
! this is required for XDMF.
call check(nf90_inquire(nc_id, formatNum=ncformat))
if ( (ncformat.ne.nf90_format_netcdf4).and.(ncformat.ne.nf90_format_netcdf4_classic) ) then
   write(6,'(a)') 'ERROR: generateXDMF.f90: This file is netcdf3 format; XDMF requires netcdf4 formatted files.'
   call check(nf90_close(nc_id))
   stop
endif
!
! Inquire netCDF file about mesh dimensions, variables, and attributes.
call check(nf90_inq_dimid(nc_id, "node", NC_DimID_node))
call check(nf90_inquire_dimension(nc_id, NC_DimID_node, len=np))
call check(nf90_inq_dimid(nc_id, "nele", NC_DimID_nele))
call check(nf90_inquire_dimension(nc_id, NC_DimID_nele, len=ne))
agrid = "mesh"
! Some netcdf files have the comment line at the top of the fort.14 in
! an attribute named "agrid" while in others the attribute is named "grid".
ncStatus = nf90_get_att(nc_id, NF90_GLOBAL, 'agrid', agrid)
if ( ncStatus.ne.NF90_NOERR ) then
   call check(nf90_get_att(nc_id, NF90_GLOBAL, 'grid', agrid))
endif
write(6,'(a)') 'INFO: generateXDMF.f90: Read mesh dimensions from netCDF successfully.'
!
! Have a look at how much data is in the netcdf file.
! determine the type of data stored in the file
call check(nf90_inquire(nc_id, ndim, nvar, natt, nc_dimid_time, ncformat))
!
! determine whether this is a nodal attributes file
nodalAttributesFile = .false.
do i=1,natt
   call check(nf90_inq_attname(nc_id, NF90_GLOBAL, i, thisVarName))
   if (trim(thisVarName).eq.'nodalAttributesComment') then
      nodalAttributesFile = .true.
      exit
   endif
end do
if (nodalAttributesFile.eqv..false.) then
   call check(nf90_inquire(nc_id, unlimitedDimId=NC_DimID_time))
   call check(nf90_inquire_dimension(nc_id, NC_DimID_time, len=nSnaps))
   call check(nf90_inq_varid(nc_id, 'time', NC_VarID_time))
endif
!
! Determine the type of netCDF file that we have based on the name(s) 
! of the variable(s) in the file. Set the number of variables, number
! of components for each one and their names for the XML file. 
!
!jgfdebug
!write(6,'(a,i0,a)') 'DEBUG: generateXDMF: There are ',nvar,' variables in the NetCDF file:'
!do i=1,nvar
!   call check(nf90_inquire_variable(nc_id, i, thisVarName))
!   write(6,'(a)') trim(thisVarName)
!end do 

do i=1,nvar
   call check(nf90_inquire_variable(nc_id, i, thisVarName))
   select case(trim(thisVarName))
   case("zeta")
      fileMetaData % fileTypeDesc = 'a time varying 2D ADCIRC water surface elevation file (fort.63)'
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("nodecode")
      fileMetaData % fileTypeDesc = 'a node wet/dry state file (nodecode.63)'
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      exit  
   case("noff")
      fileMetaData % fileTypeDesc = 'an element wet/dry state file (noff.100)'
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      fileMetaData % dataCenter(1) = 'Cell' ! noff
      exit          
   case("zetad")
      fileMetaData % fileTypeDesc = 'a 2D ADCIRC hotstart file (fort.67/fort.68)'
      fileMetaData % timeVarying = .false. 
      call initFileMetaData(fileMetaData, thisVarName, 7, 6)
      fileMetaData % varNameNetCDF(1) = "zeta1"  ! eta1 
      fileMetaData % varNameNetCDF(2) = "zeta2"  ! eta2 
      fileMetaData % varNameNetCDF(3) = "zetad"  ! EtaDisc 
      fileMetaData % varNameNetCDF(4) = "u-vel"  ! uu2 \_combine as vector in XDMF_
      fileMetaData % varNameNetCDF(5) = "v-vel"  ! vv2 /
      fileMetaData % varNameNetCDF(6) = "nodecode"  ! nodecode                   
      fileMetaData % varNameNetCDF(7) = "noff"   ! noff<---element/cell centered
      !
      fileMetaData % numComponentsXDMF(4) = 2  ! velocity (uu2,vv2)
      call initNamesXDMF(fileMetaData, nc_id)
      fileMetaData % varNameXDMF(4) = 'hot_start_velocity'
      fileMetaData % dataCenter(6) = 'Cell' ! noff
      fileMetaData % timeVarying = .false.
      exit
   case("u-vel","v-vel")
      fileMetaData % fileTypeDesc = 'a 2D ADCIRC water current velocity file (fort.64)'     
      call initFileMetaData(fileMetaData, thisVarName, 2, 1)
      fileMetaData % varNameNetCDF(1) = "u-vel"  ! uu2 in ADCIRC
      fileMetaData % varNameNetCDF(2) = "v-vel"  ! vv2 in ADCIRC
      fileMetaData % numComponentsXDMF(1) = 2
      call initNamesXDMF(fileMetaData, nc_id)
      fileMetaData % varNameXDMF(1) = 'water_current_velocity'
      exit
   !case("vel_mag","wind_mag","radstress_mag","swan_wind_mag") 
   !   write(6,'(a)') "INFO: generateXDMF.f90:  The file contains vector magnitude data."
   !   varMagName = trim(thisVarName)
   !   useMag = .true.
   case("pressure")
      fileMetaData % fileTypeDesc = "an ADCIRC barometric pressure file (fort.73)"
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)
      call initNamesXDMF(fileMetaData, nc_id)
      exit 
   case("windx","windy")
      fileMetaData % fileTypeDesc = "an ADCIRC wind velocity file (fort.74)"
      call initFileMetaData(fileMetaData, thisVarName, 2, 1)
      fileMetaData % varNameNetCDF(1) = "windx"  
      fileMetaData % varNameNetCDF(2) = "windy"  
      fileMetaData % numComponentsXDMF(1) = 2
      call initNamesXDMF(fileMetaData, nc_id)
      fileMetaData % varNameXDMF(1) = 'wind_velocity'
      exit
   case("maxele","zeta_max")
      fileMetaData % fileTypeDesc = "an ADCIRC maximum water surface elevation (maxele.63) file"
      ! Check to see if this is a newer-style min/max file that records
      ! the time of the min or max, and if so, prepare to convert the
      ! time of occurrence metadata as well.     
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("initial_river_elevation")
      fileMetaData % fileTypeDesc = "an ADCIRC initial river elevation (fort.88) file"
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)
      call initNamesXDMF(fileMetaData, nc_id)
      exit    
   case("maxwvel","wind_max")
      fileMetaData % fileTypeDesc = "an ADCIRC maximum wind speed (maxwvel.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("maxvel","vel_max")
      fileMetaData % fileTypeDesc = "an ADCIRC maximum current speed (maxvel.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("maxrs","radstress_max")
      fileMetaData % fileTypeDesc = "an ADCIRC maximum wave radiation stress gradient (maxrs.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("minpr","pressure_min")
      fileMetaData % fileTypeDesc = "an ADCIRC minimum barometric pressure (minpr.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)      
      exit
   case("endrisinginun")
      fileMetaData % fileTypeDesc = "an ADCIRC nodes with inundation rising at end of simulation (endrisinginun.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("initiallydry")
      fileMetaData % fileTypeDesc = "an ADCIRC dry nodes at cold start (initiallydry.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)
      exit 
   case("inun_time")
      fileMetaData % fileTypeDesc = "an ADCIRC total time inundated (inundationtime.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("everdried")
      fileMetaData % fileTypeDesc = "an ADCIRC ever dried (everdried.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)
      exit   
   case("inun_max")
      fileMetaData % fileTypeDesc = "an ADCIRC maximum inundation depth (maxinundepth.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)
      exit      
   case("radstress_x","radstress_y")
      fileMetaData % fileTypeDesc = "an ADCIRC wave radiation stress gradient (rads.64) file"
      call initFileMetaData(fileMetaData, thisVarName, 2, 1)
      fileMetaData % varNameNetCDF(1) = "radstress_x"  
      fileMetaData % varNameNetCDF(2) = "radstress_y"  
      fileMetaData % numComponentsXDMF(1) = 2
      call initNamesXDMF(fileMetaData, nc_id)
      fileMetaData % varNameXDMF(1) = 'wave_radiation_stress_gradient'      
   case("swan_DIR")
      fileMetaData % fileTypeDesc = "a SWAN wave direction (swan_DIR.63) file"
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      exit   
   case("swan_HS")
      fileMetaData % fileTypeDesc = "a SWAN significant wave height (swan_HS.63) file"
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("swan_HS_max")
      fileMetaData % fileTypeDesc = "a SWAN maximum significant wave height (swan_HS_max.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)      
      exit
   case("swan_TMM10")
      fileMetaData % fileTypeDesc = "a SWAN mean absolute wave period (swan_TMM10.63) file"
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("swan_TM01")
      fileMetaData % fileTypeDesc = "SWAN mean absolute wave period (swan_TM01.63) file"
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("swan_TM02")
      fileMetaData % fileTypeDesc = "a SWAN mean absolute zero crossing period (swan_TM02.63) file"
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("swan_TPS")
      fileMetaData % fileTypeDesc = "a SWAN smoothed peak period (swan_TPS.63) file"
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("swan_TPS_max")
      fileMetaData % fileTypeDesc = "a SWAN maximum smoothed peak period (swan_TPS_max.63) file"
      call initMinMaxFileMetaData(fileMetaData, thisVarName, nvar, nc_id)
      call initNamesXDMF(fileMetaData, nc_id)      
      exit
   case("ESLNodes")
      fileMetaData % fileTypeDesc = "an elemental slope limiter active nodes (ESLNodes.63) file"
      call initFileMetaData(fileMetaData, thisVarName, 1, 1)     
      call initNamesXDMF(fileMetaData, nc_id)
      exit
   case("swan_windx","swan_windy")
      fileMetaData % fileTypeDesc = "a SWAN wind velocity (swan_WIND.64) file"
      call initFileMetaData(fileMetaData, thisVarName, 2, 1)
      fileMetaData % varNameNetCDF(1) = "swan_windx"  
      fileMetaData % varNameNetCDF(2) = "swan_windy"  
      fileMetaData % numComponentsXDMF(1) = 2
      call initNamesXDMF(fileMetaData, nc_id)
      fileMetaData % varNameXDMF(1) = 'swan_wind_velocity' 
      exit
   case default
      cycle     ! did not recognize this variable name
   end select
end do
!
! Form the file name of XDMF xml file and open it.
write(6,'(a)') 'INFO: generateXDMF.f90: Writing XDMF xml header for this NetCDF file.'
xmf = trim(fileMetaData%netCDFFile)//".xmf"
open(fileMetaData%xmfUnit,file=xmf,status='replace')
! write the beginning of the XDMF xml file
write(fileMetaData%xmfUnit,'(A)') '<?xml version="1.0" ?>'
write(fileMetaData%xmfUnit,'(A)') '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
write(fileMetaData%xmfUnit,'(A)') '<Xdmf Version="2.0">'
write(fileMetaData%xmfUnit,'(A)') '   <Domain Name= "'//adjustl(trim(agrid))//'">'
!    
! Bomb out if we did not recognize any of the variable names in the file.
if ( fileMetaData % initialized.eqv..false. ) then
   write(6,'(a)') 'INFO: generateXDMF.f90: Did not recognize any of the variables in the file.'
   write(6,'(a)') 'INFO: generateXDMF.f90: The xml file will only contain mesh-related information.'
   write(fileMetaData%xmfUnit,'(A)') '      <Grid Name="'//adjustl(trim(agrid))//'" GridType="Uniform">'
   ! Write mesh portion of XDMF xml file.
   call writeMeshTopologyGeometryDepth(fileMetaData)
   ! finish off the xml so the user can at least look at the mesh
   call writeFooterXML(fileMetaData%xmfUnit)
   write(6,'(a)') "INFO: generateXDMF.f90: Terminating after writing mesh-related into to xml file."
   stop
else
   ! log the guessed type of the file for the user
   write(6,'(a)') "INFO: generateXDMF.f90: Preparing to write XDMF xml for "//trim(fileMetaData%fileTypeDesc)//"."    
endif
!
! If the file only contains data that are not time varying, 
! (e.g., hotstart files and min/max files), then write XDMF Attributes
! to the same Grid as the mesh itself and be done with it
if ( fileMetaData%timeVarying.eqv..false. ) then
   write(fileMetaData%xmfUnit,'(A)') '      <Grid GridType="Uniform">'
   call writeMeshTopologyGeometryDepth(fileMetaData)
   call writeAttributesXML(fileMetaData, 1, 1)
   write(fileMetaData%xmfUnit,'(A)') '   </Grid>'
   call writeFooterXML(fileMetaData%xmfUnit)
   write(6,'(a)') 'INFO: generateXDMF.f90: Finished writing XDMF xml for '//trim(fileMetaData%fileTypeDesc)//'.' 
   stop
endif
!
! Load up the time values (in seconds), if the data are time varying.
allocate(timesec(nSnaps))
call check(nf90_get_var(nc_id, NC_VarID_time, timesec, (/ 1 /), (/ nSnaps /) ))
!
! Write meta data for time varying data snapshots to XML.
write(6,'(a,i0,a)') 'INFO: generateXDMF.f90: There are ',nSnaps,' time values (snapshots) in the file.' 
!
! Close the netcdf4 file, we have all the information we need.  
call check(nf90_close(nc_id))
!
! Write the metadata for each snapshot in time. 
write(fileMetaData%xmfUnit,'(A)') '     <Grid Name="TimeSeries" GridType="Collection" CollectionType="Temporal">'
do iSnap=1,nSnaps
   call writeTimeVaryingGrid(fileMetaData, timesec(iSnap))
   call writeTimeVaryingAttributesXML(fileMetaData, iSnap, nSnaps)
   write(fileMetaData%xmfUnit,'(A)') '      </Grid>'
end do
write(fileMetaData%xmfUnit,'(A)') '   </Grid>'
call writeFooterXML(fileMetaData%xmfUnit)
!
write(6,'(A)') 'INFO: generateXDMF.f90: Finished generating XDMF xml for this NetCDF file.'
!----------------------------------------------------------------------
end program generateXDMF
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!  S U B R O U T I N E     I N I T   F I L E   M E T A  D A T A 
!----------------------------------------------------------------------
! Allocate memory to hold variable names, variable IDs, etc for
! variables in the targetted NetCDF4 files so the metadata can be
! appropriately written to the XDMF XML. Also initialize the newly
! allocated variables to reasonable values. 
!----------------------------------------------------------------------
subroutine initFileMetaData(fmd, firstVarName, numNC, numXDMF)
use netcdf
use asgsio, only : fileMetaData_t
implicit none
type(fileMetaData_t), intent(inout) :: fmd
character(NF90_MAX_NAME), intent(in) :: firstVarName
integer, intent(in) :: numNC
integer, intent(in) :: numXDMF
!
fmd % timeVarying = .true.       ! initialize to most common value
fmd % timeOfOccurrence = .false. ! only relevant to min/max files
!
! NetCDF
fmd % numVarNetCDF = numNC
allocate(fmd % varNameNetCDF(numNC))
fmd % varNameNetCDF(:) = 'error: not_set'
fmd % varNameNetCDF(1) = trim(firstVarName) ! initialize to most common value
allocate(fmd % nc_varID(numNC))
fmd % nc_varID(:) = -999
allocate(fmd % nc_type(numNC))
fmd % nc_type(:) = -999
!
! XDMF
fmd % numVarXDMF = numXDMF
allocate(fmd % varNameXDMF(numXDMF))
fmd % varNameXDMF(:) = 'error: not_set'
allocate(fmd % numComponentsXDMF(numXDMF))
fmd % numComponentsXDMF(:) = 1         ! initialize to most common value
allocate(fmd % dataCenter(numXDMF))
fmd%dataCenter(:) = 'Node'                 ! initialize to most common value
allocate(fmd % typeXDMF(numXDMF))
fmd%typeXDMF(:) = 'Float'                 ! initialize to most common value
allocate(fmd % precisionXDMF(numXDMF))
fmd%precisionXDMF(:) = 8                 ! initialize to most common value
fmd%initialized = .true. 
!----------------------------------------------------------------------
end subroutine initFileMetaData
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! S U B R O U T I N E   I N I T  M I N  M A X  F I L E  M E T A D A T A
!----------------------------------------------------------------------
! Checks for the existence of time of occurrence data in the min max
! file before initializing the file metadata. 
!----------------------------------------------------------------------
subroutine initMinMaxFileMetaData(fmd, someVarName, nvar, ncid)
use netcdf
use asgsio, only : check, fileMetaData_t
implicit none
type(fileMetaData_t), intent(inout) :: fmd 
character(NF90_MAX_NAME), intent(in) :: someVarName 
integer, intent(in) :: nvar
integer, intent(in) :: ncid
!
character(NF90_MAX_NAME) timeOfVarName 
character(NF90_MAX_NAME) aVarName
integer :: j
!     
timeOfVarName = 'time_of_'//trim(someVarName)
if (trim(someVarName).eq."inun_time") then
   timeOfVarName = 'last_'//trim(someVarName)
endif
do j=1,nvar
   call check(nf90_inquire_variable(ncid, j, aVarName))
   if (trim(aVarName).eq.trim(timeOfVarName)) then
      write(6,'(a)') 'INFO: The file contains time of occurrence data.'
      fmd % timeOfOccurrence = .true.
      exit
   endif 
end do
if (fmd % timeOfOccurrence.eqv..true.) then
   call initFileMetaData(fmd, someVarName, 2, 2)
   fmd % varNameNetCDF(2) = trim(timeOfVarName)
   fmd % timeOfOccurrence = .true. ! was reset in initFileMetaData   
else
   call initFileMetaData(fmd, someVarName, 1, 1)
endif
fmd % timeVarying = .false. ! was reset in initFileMetaData     
!----------------------------------------------------------------------
end subroutine initMinMaxFileMetaData
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!  S U B R O U T I N E     I N I T   N A M E S   X D M F
!----------------------------------------------------------------------
! Initialize the names of the variables in the XMDF files to reasonable
! names (that will be renamed, at least in the case of vector data, 
! in the calling routine). 
! Added the initialization of the data type. 
!----------------------------------------------------------------------
subroutine initNamesXDMF(fmd, ncid)
use netcdf
use asgsio, only : check, fileMetaData_t
implicit none
type(fileMetaData_t) :: fmd
integer :: ncid
integer :: i, j

! find the netcdf variable IDs
do i=1,fmd % numVarNetCDF
   call check(nf90_inq_varid(ncid, fmd % varNameNetCDF(i), fmd % nc_varID(i)))
   call check(nf90_inquire_variable(ncid, fmd%nc_varID(i), fmd%varNameNetCDF(i), fmd%nc_type(i)))
end do
! set the names of the XDMF data 
i=1 ! netcdf variable counter
j=1 ! xdmf variable counter
do 
   ! for vector data, the name will be replaced in the calling routine anyway
   if (trim(fmd%varNameNetCDF(i)).ne."noff") then
      !write(6,'(a,a)') 'DEBUG: generateXDMF: seeking NetCDF variable ID for ',trim(fmd%varNameNetCDF(i))
      ! the standard_name attribute is missing for noff in some netcdf hotstart files   
      call check(nf90_get_att(ncid, fmd % nc_varID(i), 'standard_name', fmd % varNameXDMF(j)))
   endif
   !
   ! Apply a fix for old versions of ADCIRC that misnamed nodecode and 
   ! noff in the netcdf hotstart files (nodecode was given the standard_name
   ! of "element_wet_or_dry" while noff was given no standard name at all). 
   if (trim(fmd%varNameNetCDF(i)).eq."nodecode") then
      fmd%varNameXDMF(j) = "node_wet_or_dry"
   endif
   if (trim(fmd%varNameNetCDF(i)).eq."noff") then
      fmd%varNameXDMF(j) = "element_wet_or_dry"
   endif
   !write(6,'(a,i0,a)') 'DEBUG: generateXDMF: varNameNetCDF(',i,')=',trim(fmd%varNameNetCDF(i))
   !write(6,'(a,i0,a)') 'DEBUG: generateXDMF: varNameXDMF(',j,')=',trim(fmd%varNameXDMF(j))

   select case(fmd%nc_type(i))
   case(NF90_INT)
      fmd%typeXDMF(j) = "Int"
      fmd%precisionXDMF(j) = 4
   case(NF90_FLOAT)
      fmd%typeXDMF(j) = "Float"
      fmd%precisionXDMF(j) = 4
   case(NF90_DOUBLE)
      fmd%typeXDMF(j) = "Float"
      fmd%precisionXDMF(j) = 8
   case default
      write(6,'(a,a,a)') 'ERROR: generateXDMF: The netCDF variable ',trim(fmd%varNameNetCDF(i)),' uses an unknown data type.'
      stop
   end select

   i = i + fmd % numComponentsXDMF(j) ! multi component (vector) data only need 1 name
   j = j + 1
   if (j.gt.fmd % numVarXDMF) exit
end do
!----------------------------------------------------------------------
end subroutine initNamesXDMF
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!                   S U B R O U T I N E     
! W R I T E   M E S H   T O P O L O G Y   G E O M E T R Y   D E P T H    
!----------------------------------------------------------------------
! Writes the mesh portion of the XML. 
!----------------------------------------------------------------------
subroutine writeMeshTopologyGeometryDepth(fmd)
use asgsio, only : fileMetaData_t
use adcmesh, only : agrid, ne, np
implicit none
type(fileMetaData_t), intent(in) :: fmd
!

write(fmd%xmfUnit,'(A)') '         <Topology Name="ADCIRCMesh"'
write(fmd%xmfUnit,'(A)') '                   TopologyType="Triangle"'
write(fmd%xmfUnit,'(A)') '                   NodesPerElement="3"'
write(fmd%xmfUnit,'(A,i0,A)') '                   NumberOfElements="',ne,'"'
write(fmd%xmfUnit,'(A)') '                   BaseOffset="1">'
write(fmd%xmfUnit,'(A,i0,A)') '            <DataItem Dimensions="',ne,'  3"'
write(fmd%xmfUnit,'(A)') '                      DataType="Int"'
write(fmd%xmfUnit,'(A)') '                      Format="HDF">'//trim(fmd%netCDFFile)//':/element'
write(fmd%xmfUnit,'(A)') '            </DataItem>'
write(fmd%xmfUnit,'(A)') '         </Topology>'
write(fmd%xmfUnit,'(A)') '         <Geometry Name="NodeLocations"'
write(fmd%xmfUnit,'(A)') '                   GeometryType="X_Y">'
write(fmd%xmfUnit,'(A,i0,A)') '            <DataItem Dimensions="',np,'"'
write(fmd%xmfUnit,'(A)') '                      NumberType="Float"'
write(fmd%xmfUnit,'(A)') '                      Precision="8"'
if (fmd%useCPP.eqv..true.) then
   write(fmd%xmfUnit,'(A)') '                      Format="HDF">'//trim(fmd%netCDFFile)//':/x_cpp'
else
   write(fmd%xmfUnit,'(A)') '                      Format="HDF">'//trim(fmd%netCDFFile)//':/x'
endif
write(fmd%xmfUnit,'(A)') '            </DataItem>'
write(fmd%xmfUnit,'(A,i0,A)') '            <DataItem Dimensions="',np,'"'
write(fmd%xmfUnit,'(A)') '                      NumberType="Float"'
write(fmd%xmfUnit,'(A)') '                      Precision="8"'
if (fmd%useCPP.eqv..true.) then
   write(fmd%xmfUnit,'(A)') '                      Format="HDF">'//trim(fmd%netCDFFile)//':/y_cpp'
else
   write(fmd%xmfUnit,'(A)') '                      Format="HDF">'//trim(fmd%netCDFFile)//':/y'
endif
write(fmd%xmfUnit,'(A)') '            </DataItem>'
write(fmd%xmfUnit,'(A)') '         </Geometry>'
write(fmd%xmfUnit,'(A)') '         <Attribute Name="BathymetricDepth"'
write(fmd%xmfUnit,'(A)') '                    AttributeType="Scalar"'
write(fmd%xmfUnit,'(A)') '                    Center="Node">'
write(fmd%xmfUnit,'(A,i0,A)') '            <DataItem Dimensions="',np,'"'
write(fmd%xmfUnit,'(A)') '                      NumberType="Float"'
write(fmd%xmfUnit,'(A)') '                      Precision="8"'
write(fmd%xmfUnit,'(A)') '                      Format="HDF">'//trim(fmd%netCDFFile)//':/depth'
write(fmd%xmfUnit,'(A)') '            </DataItem>'
write(fmd%xmfUnit,'(A)') '         </Attribute>'
!----------------------------------------------------------------------
end subroutine writeMeshTopologyGeometryDepth
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!  S U B R O U T I NE   W R I T E   T I M E   V A R Y I N G   G R I D 
!----------------------------------------------------------------------
! Creates the Grid metadata inside the Temporal collection to describe
! each time snap.  
!----------------------------------------------------------------------
subroutine writeTimeVaryingGrid(fmd, thisTime)
use asgsio, only : fileMetaData_t
implicit none
type(fileMetaData_t), intent(in) :: fmd
real(8), intent(in) :: thisTime

! Write the Grid xml for the time varying data
! now write XDMF XML data for this dataset
write(fmd%xmfUnit,'(A,E22.15,A)') '      <Grid Name="Time=',thisTime,'" GridType="Uniform">'
write(fmd%xmfUnit,'(13x,A,E22.15,A)') '<Time Value="',thisTime,'"/>'
call writeMeshTopologyGeometryDepth(fmd)
!----------------------------------------------------------------------
end subroutine writeTimeVaryingGrid
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!  S U B R O U T I N E    W R I T E   A T T R I B U T E S   X M L 
!----------------------------------------------------------------------
! Writes the Attribute(s) metadata to an XML file.
!----------------------------------------------------------------------
subroutine writeAttributesXML(fmd, iSnap, nSnaps)
use adcmesh, only : np, ne
use asgsio, only : fileMetaData_t
implicit none
type(fileMetaData_t), intent(in) :: fmd
integer, intent(in) :: iSnap
integer, intent(in) :: nSnaps
!
integer :: dataItemDimensions
character(len=20) :: attributeType
integer :: i, j, k
!
i=1 ! netCDF variable counter
j=1 ! XDMF variable counter
do 
   attributeType = "Scalar"
   if (fmd%numComponentsXDMF(j).gt.1) then
      attributeType = "Vector"
   endif
   dataItemDimensions = np
   if (trim(fmd%dataCenter(j)).eq."Cell") then
      dataItemDimensions = ne
   endif
   !
   write(fmd%xmfUnit,'(A)') '         <Attribute Name="'//trim(fmd%varNameXDMF(j))//'"'
   write(fmd%xmfUnit,'(A)') '                    Center="'//trim(fmd%dataCenter(j))//'"'
   write(fmd%xmfUnit,'(A)') '                    AttributeType="'//trim(attributeType)//'">'
   !
   ! Scalar attribute
   if (trim(attributeType).eq."Scalar") then
      write(fmd%xmfUnit,'(A,i0,A)') '             <DataItem Dimensions="',dataItemDimensions,'"'
      write(fmd%xmfUnit,'(a,a,a)') '                    NumberType="',trim(fmd%typeXDMF(j)),'"'
      write(fmd%xmfUnit,'(a,i0,a)') '                    Precision="',fmd%precisionXDMF(j),'"'
      write(fmd%xmfUnit,'(A)') '                    Format="HDF">'//trim(fmd%netCDFFile)//':/'//trim(fmd%varNameNetCDF(i))
      write(fmd%xmfUnit,'(A)') '            </DataItem>'
      write(fmd%xmfUnit,'(A)') '         </Attribute>' ! end of scalar attribute
   !
   ! Vector attribute
   else 
      write(fmd%xmfUnit,'(13x,A)')         '        <DataItem ItemType="Function"'
      write(fmd%xmfUnit,'(13x,A,i0,A)')    '             Dimensions="',dataItemDimensions,' 3"'
      write(fmd%xmfUnit,'(13x,A)')         '             Function="JOIN($0, $1, 0*$0)">'
      do k=0,fmd%numComponentsXDMF(j)-1
         write(fmd%xmfUnit,'(13x,A)')      '           <DataItem ItemType="HyperSlab"'
         write(fmd%xmfUnit,'(13x,A,i0,A)') '                Dimensions="',dataItemDimensions,'"'
         write(fmd%xmfUnit,'(13x,A)')      '                Type="HyperSlab">'
         write(fmd%xmfUnit,'(13x,A)')      '              <DataItem Dimensions="3 2"'
         write(fmd%xmfUnit,'(13x,A)')      '                  Format="XML">'
         write(fmd%xmfUnit,'(13x,A,i0,A)') '                 ',iSnap-1,' 0'
         write(fmd%xmfUnit,'(13x,A)')      '              1 1'
         write(fmd%xmfUnit,'(13x,A,i0)')   '                  1 ',dataItemDimensions
         write(fmd%xmfUnit,'(13x,A)')      '              </DataItem>' ! end of dimensions
         write(fmd%xmfUnit, &
         '(13x,A,i0,i0,A)')        '              <DataItem Dimensions="',nSnaps-1,dataItemDimensions,'"'
         write(fmd%xmfUnit,'(13x,a,a,a)')      '                         NumberType="',trim(fmd%typeXDMF(j)),'"'
         write(fmd%xmfUnit,'(13x,a,i0,a)')      '                         Precision="',fmd%precisionXDMF(j),'" Format="HDF">'//trim(fmd%netCDFFile)//':/'//trim(fmd%varNameNetCDF(i+k))
         write(fmd%xmfUnit,'(13x,A)')      '               </DataItem>' ! end of Dimensions
         write(fmd%xmfUnit,'(13x,A)')      '            </DataItem>' ! end of HyperSlab
      enddo
      write(fmd%xmfUnit,'(13x,A)')         '         </DataItem>' ! end of FUNCTION
      write(fmd%xmfUnit,'(A)')             '      </Attribute>' ! end of Vector Attribute
   endif
   i = i + fmd%numComponentsXDMF(j)
   j = j + 1
   if (j.gt.fmd%numVarXDMF) then
      exit
   endif
end do
!----------------------------------------------------------------------
end subroutine writeAttributesXML
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!                     S U B R O U T I N E    
!   W R I T E   T I M E   V A R Y I N G  A T T R I B U T E S   X M L 
!----------------------------------------------------------------------
! Writes the Attribute(s) metadata to an XML file for a particular 
! snapshot in time. This requires the use of a hyperslab for both
! Scalar and Vector Attributes, because the ADCIRC time series data
! in a NetCDF file are held in one large m x t matrix (where t is the 
! time dimension). 
!----------------------------------------------------------------------
subroutine writeTimeVaryingAttributesXML(fmd, iSnap, nSnaps)
use adcmesh, only : np, ne
use asgsio, only : fileMetaData_t
implicit none
type(fileMetaData_t), intent(in) :: fmd
integer, intent(in) :: iSnap
integer, intent(in) :: nSnaps
!
integer :: domainExtent
character(len=20) :: attributeType
integer :: i, j, k
!
attributeType = "Scalar"
domainExtent = np
!
i=1 ! netCDF variable counter
j=1 ! XDMF variable counter
do 
   if (fmd%numComponentsXDMF(j).gt.1) then
      attributeType = "Vector"
   endif
   if (trim(fmd%dataCenter(j)).eq."Cell") then
      domainExtent = ne
   endif
   !
   write(fmd%xmfUnit,'(A)') '         <Attribute Name="'//trim(fmd%varNameXDMF(j))//'"'
   write(fmd%xmfUnit,'(A)') '                    Center="'//trim(fmd%dataCenter(j))//'"'
   write(fmd%xmfUnit,'(A)') '                    AttributeType="'//trim(attributeType)//'">'
   !
   ! Scalar attribute
   if (trim(attributeType).eq."Scalar") then
      write(fmd%xmfUnit,'(A)') '          <DataItem ItemType="HyperSlab"'
      write(fmd%xmfUnit,'(A,i0,A)') '                    Dimensions="',domainExtent,'"'      
      write(fmd%xmfUnit,'(A)') '                    Type="HyperSlab">'

      write(fmd%xmfUnit,'(A)') '             <DataItem Dimensions="3  2"'
      write(fmd%xmfUnit,'(A)') '                       Format="XML">'
      write(fmd%xmfUnit,'(A,i0,A)') '                         ',iSnap-1,' 0'
      write(fmd%xmfUnit,'(A)') '                            1 1'
      write(fmd%xmfUnit,'(A,i0)') '                         1 ',domainExtent
      write(fmd%xmfUnit,'(A)') '            </DataItem>'
            
      write(fmd%xmfUnit,'(A,i0,1x,i0,A)') '                <DataItem Dimensions="',nSnaps-1,domainExtent,'"'
      write(fmd%xmfUnit,'(a,a,a)') '                       NumberType="',trim(fmd%typeXDMF(j)),'"'
      write(fmd%xmfUnit,'(a,i0,a)') '                       Precision="',fmd%precisionXDMF(j),'"'
      write(fmd%xmfUnit,'(A)') '                       Format="HDF">'//trim(fmd%netCDFFile)//':/'//trim(fmd%varNameNetCDF(i))
      write(fmd%xmfUnit,'(A)') '                  </DataItem>'
      write(fmd%xmfUnit,'(A)') '               </DataItem>'
      write(fmd%xmfUnit,'(A)') '         </Attribute>' ! end of scalar attribute
   !
   ! Vector attribute
   else 
      write(fmd%xmfUnit,'(13x,A)')         '        <DataItem ItemType="Function"'
      write(fmd%xmfUnit,'(13x,A,i0,A)')    '             Dimensions="',domainExtent,' 3"'
      write(fmd%xmfUnit,'(13x,A)')         '             Function="JOIN($0, $1, 0*$0)">'
      do k=0,fmd%numComponentsXDMF(j)-1
         write(fmd%xmfUnit,'(13x,A)')      '           <DataItem ItemType="HyperSlab"'
         write(fmd%xmfUnit,'(13x,A,i0,A)') '                Dimensions="',domainExtent,'"'
         write(fmd%xmfUnit,'(13x,A)')      '                Type="HyperSlab">'
         write(fmd%xmfUnit,'(13x,A)')      '              <DataItem Dimensions="3 2"'
         write(fmd%xmfUnit,'(13x,A)')      '                  Format="XML">'
         write(fmd%xmfUnit,'(13x,A,i0,A)') '                 ',iSnap-1,' 0'
         write(fmd%xmfUnit,'(13x,A)')      '                  1 1'
         write(fmd%xmfUnit,'(13x,A,i0)')   '                  1 ',domainExtent
         write(fmd%xmfUnit,'(13x,A)')      '              </DataItem>' ! end of dimensions
         write(fmd%xmfUnit, &
         '(13x,A,i0,1x,i0,A)')        '              <DataItem Dimensions="',nSnaps-1,domainExtent,'"'
         write(fmd%xmfUnit,'(13x,a,a,a)')      '                         NumberType="',trim(fmd%typeXDMF(j)),'"'
         write(fmd%xmfUnit,'(13x,a,i0,a)')      '                         Precision="',fmd%precisionXDMF(j),'"' 
         write(fmd%xmfUnit,'(13x,A)')      '                         Format="HDF">'//trim(fmd%netCDFFile)//':/'//trim(fmd%varNameNetCDF(i+k))
         write(fmd%xmfUnit,'(13x,A)')      '               </DataItem>' ! end of Dimensions
         write(fmd%xmfUnit,'(13x,A)')      '            </DataItem>' ! end of HyperSlab
      enddo
      write(fmd%xmfUnit,'(13x,A)')         '         </DataItem>' ! end of FUNCTION
      write(fmd%xmfUnit,'(A)')             '      </Attribute>' ! end of Vector Attribute
   endif
   i = i + fmd%numComponentsXDMF(j)
   j = j + 1
   if (j.gt.fmd%numVarXDMF) then
      exit
   endif
end do
!----------------------------------------------------------------------
end subroutine writeTimeVaryingAttributesXML
!----------------------------------------------------------------------



!----------------------------------------------------------------------
!  S U B R O U T I N E     W R I T E   F O O T E R   X M L  
!----------------------------------------------------------------------
subroutine writeFooterXML(xmfUnit)
implicit none
integer, intent(in) :: xmfUnit

write(xmfUnit,'(A)') '   </Domain>'
write(xmfUnit,'(A)') '</Xdmf>'
close(xmfUnit)
!----------------------------------------------------------------------
end subroutine writeFooterXML
!----------------------------------------------------------------------

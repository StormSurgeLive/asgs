!--------------------------------------------------------------------------
! generateXDMF.f90
!
! A program to generate XDMF xml for NetCDF4 formatted ADCIRC files.
!
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
! Example for compiling this program with g95:
!
! g95 -o generateXDMF.x -ffree-form -ffree-line-length-huge -I/usr/local/netcdf/netcdf-4.1.1/f90 generateXDMF.f90 -L/usr/local/hdf5/hdf5-1.8.8/hdf5/lib -lnetcdf -lhdf5_hl -lhdf5 -lhdf5_fortran -lz
!
!Example for compiling this program with gfortran:
!
!gfortran -o generateXDMF.x -ffree-form -ffree-line-length-none -I/usr/include generateXDMF.f90 -lnetcdf -lnetcdff -lz
!
! For compiling on blueridge at RENCI with ifort 20130516:
!ifort -o generateXDMF.x -i-dynamic -I/projects/ncfs/apps/netcdf/netcdf-fortran-4.2/include -L/projects/ncfs/apps/netcdf/netcdf-fortran-4.2/lib generateXDMF.f90 -lnetcdf -lnetcdff -lz
!
! For compiling with pgf90:
! pgf90 -o generateXDMF.x -Mpreprocess -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/opt/cray/netcdf/4.1.3/pgi/109/include generateXDMF.f90 -L/opt/cray/netcdf/4.1.3/pgi/109/lib  -lnetcdf -lnetcdff
!
! Compiling with pgf90 on garnet at ERDC 20130926:
! pgf90 -o generateXDMF.x -Mpreprocess -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/opt/cray/netcdf/4.3.0/pgi/121/include generateXDMF.f90 -L/opt/cray/netcdf/4.3.0/pgi/121/lib -lnetcdf -lnetcdff
! 
! Example for ifort on Diamond at ERDC 20130726:
! ifort -cpp -o generateXDMF.x -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -i-dynamic -I/usr/local/usp/PETtools/CE/pkgs/netcdf-4.2.1.1-intel-serial/include -L/usr/local/usp/PETtools/CE/pkgs/netcdf-4.2.1.1-intel-serial/lib generateXDMF.f90 -lnetcdf -lnetcdff -lz

program generateXDMF
use netcdf
use asgsio, only : check
use adcmesh
implicit none
integer :: iargc
character(1024) :: datafile
character(1024) :: xmf ! name of XDMF xml file
logical :: fileFound = .false.
integer :: ncStatus
integer :: NC_DimID_time
integer :: NC_VarID_time
integer :: num_components
character(120) :: standard_name(3)
character(120) :: dataDesc      ! how the variable is described
character(120) :: dataMagDesc   ! how the magnitude of the vector is described
character(120) :: fileTypeDesc  ! how the file is described
character(120) :: vectorMagDesc ! how the vector magnitude is 
                                ! described (only for vector datasets)
character(NF90_MAX_NAME) :: varname(3)
character(NF90_MAX_NAME) :: varMagName
character(NF90_MAX_NAME) :: avarname
character(NF90_MAX_NAME) :: timeofvarname
integer :: NC_VarID(3)
integer :: nc_id
character(NF90_MAX_NAME) :: thisVarName
integer :: ndim     ! number of dimensions in the file
integer :: nvar     ! number of variables in the file
integer :: natt     ! number of global attributes in the file
integer :: ncformat ! whether netcdf 3 or netcdf 4
integer :: ndset    ! number of datasets in the file (length of unlimited dimension)
logical :: multisets ! .true. for files with multiple types of data, like hotstart files
real(8), allocatable :: timesec(:)  ! time in seconds associated with each dataset
integer argcount
character(1024) :: cmdlineopt
character(1024) :: cmdlinearg
logical :: useCPP ! true if we should refer to cpp coordinates in the netcdf file
logical :: useMag ! true if vector magnitude data are found in the file
logical :: fileTypeLogged ! true if file type has been logged as INFO message
integer i, j ! loop counters

multisets = .false.
useCPP = .false.
useMag = .false.
fileTypeLogged = .false.
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
            write(6,'(a)') "INFO: generateXDMF.f90:  Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            datafile = trim(cmdlinearg)
         case("--use-cpp")
            useCPP = .true.
            write(6,'(a)') "INFO: generateXDMF.f90:  Processing ",trim(cmdlineopt),"."
         case default
      end select
   end do
end if
!
!     Check to see if file exists
inquire(FILE=trim(datafile),EXIST=fileFound)
if (fileFound.eqv..false.) then
   write(6,'("ERROR: The file ",A," was not found.")') trim(datafile)
   stop
else
   ! netcdf file exists; open it
   call check(nf90_open(trim(datafile), NF90_NOWRITE, nc_id))
endif

call check(nf90_inquire(nc_id, formatNum=ncformat))
if ( (ncformat.ne.nf90_format_netcdf4).and.(ncformat.ne.nf90_format_netcdf4_classic) ) then
   write(6,*) "ERROR: This file is netcdf3 format; XDMF requires netcdf4 formatted files."
   call check(nf90_close(nc_id))
   stop
endif
write(6,'(a)') "INFO: generateXDMF.f90:  Generating XDMF xml for this NetCDF file."
!
! form file name of XDMF xml file and open it
xmf = trim(datafile)//".xmf"
open(10,file=xmf,status='replace')
! write the beginning of the XDMF xml file
write(10,'(A)') '<?xml version="1.0" ?>'
write(10,'(A)') '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
write(10,'(A)') '<Xdmf Version="2.0">'
write(10,'(A)') '   <Domain>'
!
! Inquire about mesh dimensions, variables, and attributes
call check(nf90_inq_dimid(nc_id, "node", NC_DimID_node))
call check(nf90_inquire_dimension(nc_id, NC_DimID_node, len=np))
call check(nf90_inq_dimid(nc_id, "nele", NC_DimID_nele))
call check(nf90_inquire_dimension(nc_id, NC_DimID_nele, len=ne))
agrid = "mesh"
ncStatus = nf90_get_att(nc_id, NF90_GLOBAL, 'agrid', agrid)
if ( ncStatus.ne.NF90_NOERR ) then
   call check(nf90_get_att(nc_id, NF90_GLOBAL, 'grid', agrid))
endif
!
! write mesh portion of XDMF xml file
write(10,'(A)') '      <Grid Name="'//adjustl(trim(agrid))//'" GridType="Uniform">'
write(10,'(A)') '         <Topology Name="ADCIRCMesh"'
write(10,'(A)') '                   TopologyType="Triangle"'
write(10,'(A)') '                   NodesPerElement="3"'
write(10,'(A,I12,A)') '                   NumberOfElements="',ne,'"'
write(10,'(A)') '                   BaseOffset="1">'
write(10,'(A,I12,A)') '            <DataItem Dimensions="',ne,'  3"'
write(10,'(A)') '                      DataType="Int"'
write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/element'
write(10,'(A)') '            </DataItem>'
write(10,'(A)') '         </Topology>'
write(10,'(A)') '         <Geometry Name="NodeLocations"'
write(10,'(A)') '                   GeometryType="X_Y">'
write(10,'(A,I12,A)') '            <DataItem Dimensions="',np,'"'
write(10,'(A)') '                      NumberType="Float"'
write(10,'(A)') '                      Precision="8"'
if (useCPP.eqv..true.) then
   write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/x_cpp'
else
   write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/x'
endif
write(10,'(A)') '            </DataItem>'
write(10,'(A,I12,A)') '            <DataItem Dimensions="',np,'"'
write(10,'(A)') '                      NumberType="Float"'
write(10,'(A)') '                      Precision="8"'
if (useCPP.eqv..true.) then
   write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/y_cpp'
else
   write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/y'
endif
write(10,'(A)') '            </DataItem>'
write(10,'(A)') '         </Geometry>'
write(10,'(A)') '         <Attribute Name="BathymetricDepth"'
write(10,'(A)') '                    AttributeType="Scalar"'
write(10,'(A)') '                    Center="Node">'
write(10,'(A,I12,A)') '            <DataItem Dimensions="',np,'"'
write(10,'(A)') '                      NumberType="Float"'
write(10,'(A)') '                      Precision="8"'
write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/depth'
write(10,'(A)') '            </DataItem>'
write(10,'(A)') '         </Attribute>'
!
! have a look at how much data is in the file
call check(nf90_inquire(nc_id, unlimitedDimId=NC_DimID_time))
call check(nf90_inquire_dimension(nc_id, NC_DimID_time, len=ndset))
call check(nf90_inq_varid(nc_id, 'time', NC_VarID_time))
! determine the type of data stored in the file
call check(nf90_inquire(nc_id, ndim, nvar, natt, nc_dimid_time, ncformat))
if ( (ncformat.eq.nf90_format_netcdf4).or.(ncformat.eq.nf90_format_netcdf4_classic) ) then
   write(6,'(a)') "INFO: generateXDMF.f90:  The data file uses netcdf4 formatting."
endif
!
! determine the type of file that we have
varname(1) = "null"
do i=1,nvar
   call check(nf90_inquire_variable(nc_id, i, thisVarName))
   select case(trim(thisVarName))
   case("zeta")
      fileTypeDesc = "an ADCIRC water surface elevation file"
      varname(1) = trim(thisVarName)
      num_components = 1
   case("zeta1","zeta2")
      fileTypeDesc = "the elevations in an ADCIRC hotstart file"
      varname(1) = "zeta1"
      varname(2) = "zeta2"
      num_components = 1 
      ndset = 2
      multisets = .true.
      exit
   case("u-vel","v-vel")
      fileTypeDesc = "an ADCIRC water current velocity file"
      num_components = 2
      varname(1) = "u-vel"
      varname(2) = "v-vel"
   case("vel_mag","wind_mag","radstress_mag","swan_wind_mag") 
      write(6,'(a)') "INFO: generateXDMF.f90:  The file contains vector magnitude data."
      varMagName = trim(thisVarName)
      useMag = .true.
   case("pressure")
      fileTypeDesc = "an ADCIRC barometric pressure file"
      num_components = 1
      varname(1) = "pressure"
   case("windx","windy")
      fileTypeDesc = "an ADCIRC wind velocity file"
      num_components = 2
      varname(1) = "windx"
      varname(2) = "windy"
   case("maxele","zeta_max")
      fileTypeDesc = "an ADCIRC maximum water surface elevation (maxele.63) file"
      num_components = 1
      ndset = 1
      varname(1) = trim(thisVarName)
      ! check to see if this is a new-style min/max file that records
      ! the time of the min or max, and if so, prepare to convert the
      ! time information as well
      timeOfVarName = 'time_of_'//trim(thisVarName)
      do j=1,nvar
         call check(nf90_inquire_variable(nc_id, j, aVarName))
         if (trim(aVarName).eq.trim(timeOfVarName)) then
            write(6,'(a)') 'INFO: The file contains time of occurrence data.'
            multisets = .true.
            varname(2) = trim(timeOfVarName)
            num_components = 2
            ndset = 2
            exit
         endif 
      end do
   case("maxwvel","wind_max")
      fileTypeDesc = "an ADCIRC maximum wind speed (maxwvel.63) file"
      num_components = 1
      ndset = 1
      varname(1) = trim(thisVarName)
      do j=1,nvar
         call check(nf90_inquire_variable(nc_id, j, aVarName))
         if (trim(aVarName).eq.trim(timeOfVarName)) then
            write(6,'(a)') 'INFO: The file contains time of occurrence data.'
            multisets = .true.
            varname(2) = trim(timeOfVarName)
            num_components = 2
            ndset = 2
            exit
         endif 
      end do
   case("maxvel","vel_max")
      fileTypeDesc = "an ADCIRC maximum current speed (maxvel.63) file"
      num_components = 1
      ndset = 1
      varname(1) = trim(thisVarName)
   case("maxrs","radstress_max")
      fileTypeDesc = "an ADCIRC maximum wave radiation stress gradient (maxrs.63) file"
      num_components = 1
      ndset = 1
      varname(1) = trim(thisVarName)
   case("minpr","pressure_min")
      fileTypeDesc = "an ADCIRC minimum barometric pressure (minpr.63) file"
      num_components = 1
      ndset = 1
      varname(1) = trim(thisVarName)
   case("radstress_x","radstress_y")
      fileTypeDesc = "an ADCIRC wave radiation stress gradient (rads.64) file"
      num_components = 2
      varname(1) = "radstress_x"
      varname(2) = "radstress_y"
   case("swan_DIR")
      fileTypeDesc = "a SWAN wave direction (swan_DIR.63) file"
      num_components = 1
      varname(1) = "swan_DIR"
   case("swan_HS")
      fileTypeDesc = "a SWAN significant wave height (swan_HS.63) file"
      num_components = 1
      varname(1) = "swan_HS"
   case("swan_HS_max")
      fileTypeDesc = "a SWAN maximum significant wave height (swan_HS_max.63) file"
      num_components = 1
      ndset = 1
      varname(1) = "swan_HS_max"
   case("swan_TMM10")
      fileTypeDesc = "a SWAN mean absolute wave period (swan_TMM10.63) file"
      num_components = 1
      varname(1) = "swan_TMM10"
   case("swan_TM01")
      fileTypeDesc = "SWAN mean absolute wave period (swan_TM01.63) file"
      num_components = 1
      varname(1) = "swan_TM01"
   case("swan_TM02")
      fileTypeDesc = "a SWAN mean absolute zero crossing period (swan_TM02.63) file"
      num_components = 1
      varname(1) = "swan_TM02"
   case("swan_TPS")
      fileTypeDesc = "a SWAN smoothed peak period (swan_TPS.63) file"
      num_components = 1
      varname(1) = "swan_TPS"
   case("swan_TPS_max")
      fileTypeDesc = "a SWAN maximum smoothed peak period (swan_TPS_max.63) file"
      num_components = 1
      ndset = 1
      varname(1) = "swan_TPS_max" 
   case("ESLNodes")
      fileTypeDesc = "an elemental slope limiter active nodes (ESLNodes.63) file"
      num_components = 1
      varname(1) = "ESLNodes"
      ndset = 1         
   case("swan_windx","swan_windy")
      fileTypeDesc = "a SWAN wind velocity (swan_WIND.64) file"  
      num_components = 2
      varname(1) = "swan_windx"
      varname(2) = "swan_windy"
   case default
      cycle     ! did not recognize this variable name
   end select
end do
!    
! bomb out if we did not recognize any of the variable names in the file
if ( trim(varname(1)).eq."null" ) then
   write(6,'(a)') "INFO: generateXDMF.f90: Did not recognize any of the variables in the file."
   write(6,'(a)') "INFO: generateXDMF.f90: The xml file will only contain mesh-related information."
   ! finish off the xml so the user can at least look at the mesh
   write(10,'(A)') '      </Grid>'
   write(10,'(A)') '   </Domain>'
   write(10,'(A)') '</Xdmf>'
   close(10)
   write(6,'(a)') "INFO: generateXDMF.f90: Terminating after writing mesh-related into to xml file."
   stop
else
   ! log the guessed type of the file for the user
   write(6,'(a)') "INFO: generateXDMF.f90:  Preparing to write XDMF xml for "//trim(fileTypeDesc)//"."    
endif

if ( (ndset.eq.1).or.(multisets.eqv..true.) ) then
   call check(nf90_inq_varid(nc_id, varname(1), NC_VarID(1)))
   call check(nf90_get_att(nc_id, NC_VarID(1), 'standard_name', standard_name(1)))
   write(10,'(A)') '         <Attribute Name="'//trim(standard_name(1))//'"'
   write(10,'(A)') '                    AttributeType="Scalar"'
   write(10,'(A)') '                    Center="Node">'
   write(10,'(A,I12,A)') '            <DataItem Dimensions="',np,'"'
   write(10,'(A)') '                      NumberType="Float"'
   write(10,'(A)') '                      Precision="8"'
   write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/'//trim(varname(1))
   write(10,'(A)') '            </DataItem>'
   write(10,'(A)') '         </Attribute>'
   !
   if ( multisets.eqv..true. ) then
      do i=2,ndset
         call check(nf90_inq_varid(nc_id, varname(i), NC_VarID(i)))
         call check(nf90_get_att(nc_id, NC_VarID(i), 'standard_name', standard_name(i)))
         write(10,'(A)') '         <Attribute Name="'//trim(standard_name(i))//'"'
         write(10,'(A)') '                    AttributeType="Scalar"'
         write(10,'(A)') '                    Center="Node">'
         write(10,'(A,I12,A)') '            <DataItem Dimensions="',np,'"'
         write(10,'(A)') '                      NumberType="Float"'
         write(10,'(A)') '                      Precision="8"'
         write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/'//trim(varname(i))
         write(10,'(A)') '            </DataItem>'
         write(10,'(A)') '         </Attribute>'
      end do
   end if
   write(10,'(A)') '      </Grid>'
   write(10,'(A)') '   </Domain>'
   write(10,'(A)') '</Xdmf>'
   close(10)
   call check(nf90_close(nc_id))
   write(6,'(A)') "INFO: generateXDMF.f90:  Finished generating XDMF xml for this NetCDF file."
   stop
endif
!
! load up the time values (in seconds)
allocate(timesec(ndset))
call check(nf90_get_var(nc_id, NC_VarID_time, timesec, (/ 1 /), (/ ndset /) ))
!
! grab the standard name(s) of the data for writing to the XDMF xml file
do i=1,num_components
   call check(nf90_inq_varid(nc_id, varname(i), NC_VarID(i)))
   call check(nf90_get_att(nc_id, NC_VarID(i), 'standard_name', standard_name(i)))
   dataDesc = standard_name(1)
   if ( num_components.eq.2 ) then
      select case(trim(varname(1)))
      case("windx")
         dataDesc = "wind_velocity"
      case("u-vel")
         dataDesc = "water current velocity"
      case("radstress_x")
         dataDesc = "radiation_stress_gradient"
      case("swan_windx")
         dataDesc = "swan_wind_velocity"
      case default
         write(6,*) "ERROR: The variable name "//trim(varname(1))//" was unrecognized."
         stop
      end select
      dataMagDesc = trim(dataDesc)//"_magnitude"
   endif
end do
!
! write the XDMF xml for the time varying data
! TODO: This should be modularized. Its turning into spaghetti.
write(10,'(A)') '      </Grid>'
write(10,'(A)') '      <Grid Name="TimeSeries"'
write(10,'(A)') '            GridType="Collection"'
write(10,'(A)') '            CollectionType="Temporal">'
do i=1,ndset
   ! now write XDMF XML data for this dataset
   write(10,'(A,E14.6,A)') '         <Grid Name="Time=',timesec(i),'"'
   write(10,'(9x,A)') '      GridType="Uniform">'
   write(10,'(A)') '         <Topology Name="ADCIRCMesh"'
   write(10,'(A)') '                   TopologyType="Triangle"'
   write(10,'(A)') '                   NodesPerElement="3"'
   write(10,'(A,I12,A)') '                   NumberOfElements="',ne,'"'
   write(10,'(A)') '                   BaseOffset="1">'
   write(10,'(A,I12,A)') '            <DataItem Dimensions="',ne,'  3"'
   write(10,'(A)') '                      DataType="Int"'
   write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/element'
   write(10,'(A)') '            </DataItem>'
   write(10,'(A)') '         </Topology>'
   write(10,'(A)') '         <Geometry Name="NodeLocations"'
   write(10,'(A)') '                   GeometryType="X_Y">'
   write(10,'(A,I12,A)') '            <DataItem Dimensions="',np,'"'
   write(10,'(A)') '                      NumberType="Float"'
   write(10,'(A)') '                      Precision="8"'
   if (useCPP.eqv..true.) then
      write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/x_cpp'
   else
      write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/x'
   endif
   write(10,'(A)') '            </DataItem>'
   write(10,'(A,I12,A)') '            <DataItem Dimensions="',np,'"'
   write(10,'(A)') '                      NumberType="Float"'
   write(10,'(A)') '                      Precision="8"'
   if (useCPP.eqv..true.) then
      write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/y_cpp'
   else
      write(10,'(A)') '                      Format="HDF">'//trim(datafile)//':/y'
   endif
   write(10,'(A)') '            </DataItem>'
   write(10,'(A)') '         </Geometry>'
   write(10,'(13x,A)') '<Attribute Name="BathymetricDepth"'
   write(10,'(13x,A)') '           AttributeType="Scalar"'
   write(10,'(13x,A)') '           Center="Node">'
   write(10,'(13x,A,I12,A)') '   <DataItem Dimensions="',np,'"'
   write(10,'(13x,A)') '             NumberType="Float"'
   write(10,'(13x,A)') '             Precision="8"'
   write(10,'(13x,A)') '             Format="HDF">'//trim(datafile)//':/depth'
   write(10,'(13x,A)') '   </DataItem>'
   write(10,'(13x,A)') '</Attribute>'
   write(10,'(13x,A,E14.6,A)') '<Time Value="',timesec(i),'"/>'
   write(10,'(13x,A)') '<Attribute Name="'//trim(dataDesc)//'"'
   write(10,'(13x,A)') '           Center="Node"'
   if (num_components.eq.1) then
      write(10,'(13x,A)') '           AttributeType="Scalar">'
   else
      write(10,'(13x,A)') '           AttributeType="Vector">'
      write(10,'(13x,A)') '   <DataItem ItemType="Function"'
      write(10,'(13x,A,I12,A)') '                Dimensions="',np,' 3"'
      write(10,'(13x,A)') '                Function="JOIN($0, $1, 0*$0)">'
   endif
   do j=1,num_components
      write(10,'(13x,A)') '      <DataItem ItemType="HyperSlab"'
      write(10,'(13x,A,I12,A)') '                Dimensions="',np,'"'
      write(10,'(13x,A)') '               Type="HyperSlab">'
      write(10,'(13x,A)') '        <DataItem Dimensions="3 2"'
      write(10,'(13x,A)') '                  Format="XML">'
      write(10,'(13x,A,I5,A)') '                 ',i-1,' 0'
      write(10,'(13x,A)') '                     1 1'
      write(10,'(13x,A,I12)') '                     1 ',np
      write(10,'(13x,A)') '          </DataItem>'
      write(10,'(13x,A,I5,I12,A)') '          <DataItem Dimensions="',ndset-1,np,'"'
      write(10,'(13x,A)') '                 NumberType="Float"'
      write(10,'(13x,A)') '                 Precision="8" Format="HDF">'//trim(datafile)//":/"//trim(varname(j))
      write(10,'(13x,A)') '            </DataItem>'
      write(10,'(13x,A)') '         </DataItem>'
   enddo
   if (num_components.ne.1) then
      write(10,'(13x,A)') '      </DataItem>' ! end of FUNCTION
   endif
   write(10,'(13x,A)') '   </Attribute>'
   if (useMag.eqv..true.) then
      ! add the magnitude of the vector
      write(10,'(13x,A)') '<Attribute Name="'//trim(dataMagDesc)//'"'
      write(10,'(13x,A)') '           Center="Node"'
      write(10,'(13x,A)') '           AttributeType="Scalar">'
      write(10,'(13x,A)') '      <DataItem ItemType="HyperSlab"'
      write(10,'(13x,A,I12,A)') '                Dimensions="',np,'"'
      write(10,'(13x,A)') '               Type="HyperSlab">'
      write(10,'(13x,A)') '        <DataItem Dimensions="3 2"'
      write(10,'(13x,A)') '                  Format="XML">'
      write(10,'(13x,A,I5,A)') '                 ',i-1,' 0'
      write(10,'(13x,A)') '                     1 1'
      write(10,'(13x,A,I12)') '                     1 ',np
      write(10,'(13x,A)') '          </DataItem>'
      write(10,'(13x,A,I5,I12,A)') '          <DataItem Dimensions="',ndset-1,np,'"'
      write(10,'(13x,A)') '                 NumberType="Float"'
      write(10,'(13x,A)') '                 Precision="8" Format="HDF">'//trim(datafile)//":/"//trim(varMagName)
      write(10,'(13x,A)') '            </DataItem>'
      write(10,'(13x,A)') '         </DataItem>'
      write(10,'(13x,A)') '</Attribute>'
   endif
   write(10,'(13x,A)') '</Grid>' ! end of this time snap
end do
write(10,'(A)') '      </Grid>' ! end of temporal collection
write(10,'(A)') '   </Domain>'
write(10,'(A)') '</Xdmf>'
close(10)
call check(nf90_close(nc_id))
write(6,'(A)') 'INFO: generateXDMF.f90:  Finished generating XDMF xml for this NetCDF file.'
!----------------------------------------------------------------------
   end program generateXDMF
!----------------------------------------------------------------------

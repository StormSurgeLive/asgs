!--------------------------------------------------------------------------
! generateXDMF.f90
!
! A program to generate XDMF xml for NetCDF4 formatted ADCIRC files.
!
!--------------------------------------------------------------------------
! Copyright(C) 2012--2017 Jason Fleming
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
use asgsio
use adcmesh
use logging
implicit none
integer :: iargc
character(len=20) :: logLevelOpt
logical :: setLogLevel = .false.
!
type(fileMetaData_t), allocatable :: fileMetaData(:)
logical :: fileFound = .false.
integer :: iSnap    ! snapshot counter
integer :: numFiles ! number of data files to refer to from the xml file
integer :: fi       ! file counter
integer :: olun     ! i/o unit number where xdmf xml will be written 
integer :: nSnaps
!
! NetCDF related variables
integer :: ncStatus
integer :: ncformat ! whether netcdf 3 or netcdf 4
character(NF90_MAX_NAME) :: thisVarName ! netcdf variable names
!
! XDMF XML related variables.
character(2048) :: xdmfFile ! netcdf variable names
!
! Maureparticle variables
integer :: partID ! particle ID    (temp variable)
real(8) :: lon ! particle longitude (temp variable)
real(8) :: lat ! particle latitude (temp variable)
real(8) :: timesec ! time in seconds corresponding to a particular snap (temp variable)
real(8) :: oldtimesec ! time in seconds corresponding to a particular snap
integer :: ele ! element number where a particle is found (temp variable)
integer :: particleCount = 0
integer :: lineNum = 1 ! line about to be read from Maureparticle file
integer :: numLines ! total number of lines in the Maureparticle file
integer :: maureIndex ! index of the maureparticle file
logical :: writeParticleFile = .false. ! true if the xdmf file is for particles
type(station_t), allocatable :: particles(:)
integer, parameter :: MAX_DATASETS = 1e8 
integer, allocatable :: numParticles(:) ! temporarily hold number of particles per dataset
integer, allocatable :: pTimesec(:) ! temporarily hold time stamp of particle datasets in seconds
logical :: meshInitialized = .false. ! true if the associated mesh has been read in
logical :: meshOnly = .false. ! true if only the mesh xml will be written
!
integer oldnp ! used to detect differences in number of nodes between data files
integer oldne ! used to detect differences in number of elements between data files
integer i, j ! loop counters
!
xdmfFile = 'null'
numFiles = 0
argcount = iargc() ! count up command line options
if (argcount.gt.0) then
   ! count the number of files for processing
   i=0
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
         case("--logfile")
            i = i + 1
            call getarg(i, cmdlinearg)
            call initLogging(availableUnitNumber(),'generateXDMF.f90',trim(cmdlinearg))
            write(scratchMessage,'(a,a,a,a,a)') 'Processing "',trim(cmdlineopt),' ',trim(cmdlinearg),'".'
            call allMessage(INFO,scratchMessage)
         case("--loglevel")
            i = i + 1
            call getarg(i, loglevelopt)
            write(scratchMessage,'(a,a,a,a,a)') 'Processing "',trim(cmdlineopt),' ',trim(cmdlinearg),'".'                        
            call allMessage(INFO,scratchMessage)
            setLogLevel = .true.
         case("--datafile","--maureparticle")
            numFiles = numFiles + 1
         case default
            ! skip it
      end select
   end do
   ! init logging if not already done
   if (loggingInitialized.eqv..false.) then
      call initLogging(availableUnitNumber(),'generateXDMF.f90')
   endif
   if (setLogLevel.eqv..true.) then
      call setLoggingLevel(loglevelopt)
   endif
   ! allocate space to hold metadata for each of the data files
   ! we will refer to
   allocate(fileMetaData(numFiles))
   fileMetaData(:) % useCPP = .false.
   fileMetaData(:) % initialized = .false.
   i=0
   fi=1  ! file index
   do while (i.lt.argcount)
      i = i + 1
      call getarg(i, cmdlineopt)
      select case(trim(cmdlineopt))
         case("--datafile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(scratchMessage,'(a,a,a,a,a)') 'Processing "',trim(cmdlineopt),' ',trim(cmdlinearg),'".'
            call allMessage(INFO,scratchMessage)
            fileMetaData(fi)%dataFileName = trim(cmdlinearg)
            fileMetaData(fi)%fileFormat = NETCDFG
            fi = fi + 1
         case("--maureparticle")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(scratchMessage,'(a,a,a,a,a)') 'Processing "',trim(cmdlineopt),' ',trim(cmdlinearg),'".'
            call allMessage(INFO,scratchMessage)
            fileMetaData(fi)%dataFileName = trim(cmdlinearg)
            fileMetaData(fi)%fileFormat = MAUREPT
            writeParticleFile = .true.
            fi = fi + 1
         case("--xdmffile")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(scratchMessage,'(a,a,a,a,a)') 'Processing "',trim(cmdlineopt),' ',trim(cmdlinearg),'".'
            call allMessage(INFO,scratchMessage)
            xdmfFile = trim(cmdlinearg)
         case("--logfile") ! already processed
            i = i + 1
         case("--loglevel") ! already processed
            i = i + 1
         case("--use-cpp")
            fileMetaData(:) % useCPP = .true.
            write(scratchMessage,'(a,a,a)') 'Processing ',trim(cmdlineopt),'.'
            call allMessage(INFO,scratchMessage)
         case default
            write(scratchMessage,'(a,a,a)') 'Command line option ',trim(cmdlineopt),' was not recognized.'  
            call allMessage(WARNING,scratchMessage)
      end select
   end do
end if
if (numFiles.eq.0) then
   call allMessage(ERROR,'Please provide the name of a data file with the --datafile argument.')
   stop
endif
!
! Check to see if each file exists; if the file exists, initialize dimensions
do fi=1,numFiles
   call checkFileExistence(fileMetaData(fi)%dataFileName)
   if (errorIO.gt.0) then
      stop
   endif
   select case(fileMetaData(fi)%fileFormat)
   case(NETCDFG)
      ! netcdf file exists; open it
      call check(nf90_open(trim(fileMetaData(fi)%dataFileName), NF90_NOWRITE, fileMetaData(fi)%nc_id))
      !
      ! Make sure the file is NetCDF4 formatted (i.e., HDF5 underneath) because
      ! this is required for XDMF.
      call check(nf90_inquire(fileMetaData(fi)%nc_id, formatNum=ncformat))
      if ( (ncformat.ne.nf90_format_netcdf4).and.(ncformat.ne.nf90_format_netcdf4_classic) ) then
         call allMessage(ERROR,'The file '//trim(fileMetaData(fi)%dataFileName)//' is netcdf3 format; XDMF requires netcdf4 formatted files.')
         call check(nf90_close(fileMetaData(fi)%nc_id))
         stop
      else    
         fileMetaData(fi)%fileFormat = NETCDF4
      endif
      !
      ! Inquire netCDF file about mesh dimensions, variables, and attributes.
      call check(nf90_inq_dimid(fileMetaData(fi)%nc_id, "node", fileMetaData(fi)%NC_DimID_node))
      call check(nf90_inquire_dimension(fileMetaData(fi)%nc_id, fileMetaData(fi)%NC_DimID_node, len=np))
      call check(nf90_inq_dimid(fileMetaData(fi)%nc_id, "nele", fileMetaData(fi)%NC_DimID_nele))
      call check(nf90_inquire_dimension(fileMetaData(fi)%nc_id, fileMetaData(fi)%NC_DimID_nele, len=ne))
      agrid = "mesh"
      call allMessage(INFO,'Read mesh dimensions from netCDF successfully.')
      ! Some netcdf files have the comment line at the top of the fort.14 in
      ! an attribute named "agrid" while in others the attribute is named "grid".  
      ncStatus = nf90_get_att(fileMetaData(fi)%nc_id, NF90_GLOBAL, 'agrid', agrid)
      if ( ncStatus.ne.NF90_NOERR ) then
         call check(nf90_get_att(fileMetaData(fi)%nc_id, NF90_GLOBAL, 'grid', agrid))
      endif
      call check(nf90_close(fileMetaData(fi)%nc_id))
   case(MAUREPT)
      maureIndex = fi  ! record which of the files is a maureparticle file
      ! open the file and determine the number of datasets as well as
      ! the number of particles in each dataset
      fileMetaData(fi)%fun = availableUnitNumber()
      call openFileForRead(fileMetaData(fi)%fun,fileMetaData(fi)%dataFileName)
      fileMetaData(fi) % nSnaps = 1
      fileMetaData(fi) % maxParticles = 0 
      allocate(numParticles(MAX_DATASETS)) ! allocate temporary space for num particles per dataset
      allocate(pTimesec(MAX_DATASETS)) ! allocate temporary space for particle dataset timestamps
      ! read first line of file
      lineNum = 1
      read(unit=fileMetaData(fi)%fun,fmt=*,end=123,err=456,iostat=errorIO) partID, lon, lat, timesec, ele
      pTimesec(1) = timesec
      oldtimesec = timesec
      particleCount = 1
      call allMessage(INFO,'Counting total number of datasets and peak number of particles in maureparticle file.') 
      do ! loop until we run out of data
         ! read each line in the file; label 456 is at the end of this program and
         ! provides the line number that we attempted to read in error message prior to exiting
         lineNum = lineNum + 1
         read(unit=fileMetaData(fi)%fun,fmt=*,end=123,err=456,iostat=errorIO) partID, lon, lat, timesec, ele
         if (abs(timesec-oldtimesec).le.1.e-6) then
            ! same dataset
            particleCount = particleCount + 1
            fileMetaData(fi) % maxParticles = max(particleCount,fileMetaData(fi) % maxParticles)
         else 
            ! reached a new dataset
            write(scratchMessage,'("There are ",i0," particles in dataset ",i0,".")') particleCount, fileMetaData(fi) % nSnaps
            call allMessage(DEBUG,scratchMessage)
            ! increment the number of datasets
            fileMetaData(fi) % nSnaps = fileMetaData(fi) % nSnaps + 1
            ! grab the timestamp
            pTimesec(fileMetaData(fi) % nSnaps) = timesec            
            ! reset the counter
            particleCount = 1
         endif
         ! set the number of particles in this dataset
         numParticles(fileMetaData(fi) % nSnaps) = particleCount
         oldtimesec = timesec
      end do
      ! jump to 123 when running out of data
      123 call allMessage(INFO,'Finished counting number of datasets and peak number of particles in maureparticle file.') 
      numLines = lineNum - 1  
      write(scratchMessage,'("There are ",i0," datasets in the maureparticle file and the peak number of particles is ",i0,".")') fileMetaData(fi)%nSnaps, fileMetaData(fi) % maxParticles
      call allMessage(INFO,scratchMessage)
      ! allocate space for particle data
      allocate(particles(fileMetaData(fi)%maxParticles))
      allocate(fileMetaData(fi)%numParticlesPerSnap(fileMetaData(fi)%nSnaps))      
      fileMetaData(fi) % numParticlesPerSnap(1:fileMetaData(fi)%nSnaps) = numParticles(1:fileMetaData(fi)%nSnaps)
      allocate(fileMetaData(fi)%timesec(fileMetaData(fi)%nSnaps))
      fileMetaData(fi) % timesec(1:fileMetaData(fi)%nSnaps) = pTimesec(1:fileMetaData(fi)%nSnaps)
      ! free up temp arrays
      deallocate(numParticles)
      deallocate(pTimesec)
      ! rewind back to the beginning for one more read to actually process the data
      rewind(fileMetaData(fi)%fun) 
 246  fileMetaData(fi) % initialized = .true.
   case default
      ! should be unreachable
      call allMessage(ERROR,'Only able to read netCDF4 files from ADCIRC and output files from Maureparticle.')
      stop            
   end select
end do 
!
! Have a look at how much data is in each netcdf file.
do fi=1,numFiles
   if (fileMetaData(fi)%fileFormat.ne.NETCDF4) then
      cycle
   endif
   call check(nf90_open(trim(fileMetaData(fi)%dataFileName), NF90_NOWRITE, fileMetaData(fi)%nc_id))
   call check(nf90_inquire(fileMetaData(fi)%nc_id, fileMetaData(fi)%ndim, fileMetaData(fi)%nvar, fileMetaData(fi)%natt, fileMetaData(fi)%nc_dimid_time, ncformat))
   call check(nf90_close(fileMetaData(fi)%nc_id))
end do 
!
! determine whether any of the files is a nodal attributes file or otherwise
! time invariant
fileMetaData(:)%timeVarying = .true.
do fi=1,numFiles
   if (fileMetaData(fi)%fileFormat.ne.NETCDF4) then
      cycle
   endif
   call check(nf90_open(trim(fileMetaData(fi)%dataFileName), NF90_NOWRITE, fileMetaData(fi)%nc_id))
   do i=1,fileMetaData(fi)%natt
      call check(nf90_inq_attname(fileMetaData(fi)%nc_id, NF90_GLOBAL, i, thisVarName))
      if (trim(thisVarName).eq.'nodalAttributesComment') then
         fileMetaData(fi) % dataFileType = 'fort.13' ! adcirc nodal attributes file
         fileMetaData(fi) % timeVarying = .false.
         exit
      endif
   end do
   do i=1,fileMetaData(fi)%nvar
      call check(nf90_inquire_variable(fileMetaData(fi) % nc_id, i, thisVarName))
      if (trim(thisVarName).eq.'inundationmask') then
         fileMetaData(fi) % timeVarying = .false.
         exit
      endif
   end do
   ! get information about the time dimension and values if the data
   ! are time varying
   if (fileMetaData(fi) % timeVarying.eqv..true.) then
      call check(nf90_inquire(fileMetaData(fi) % nc_id, unlimitedDimId=fileMetaData(fi) % NC_DimID_time))
      call check(nf90_inquire_dimension(fileMetaData(fi) % nc_id, fileMetaData(fi) % NC_DimID_time, len=fileMetaData(fi) % nSnaps))
      call check(nf90_inq_varid(fileMetaData(fi) % nc_id, 'time', fileMetaData(fi) % NC_VarID_time))
   endif
   call check(nf90_close(fileMetaData(fi)%nc_id))
end do 
!
! Determine the type of netCDF file that we have based on the name(s) 
! of the variable(s) in the file. Set the number of variables, number
! of components for each one and their names for the XML file. 
do fi=1,numFiles
   if ( trim(fileMetaData(fi) % dataFileType).eq.'fort.13') then
      fileMetaData(fi) % fileTypeDesc = 'an ADCIRC nodal attributes file (fort.13)'
      fileMetaData(fi) % timeVarying = .false. 
      ! Count the number of nodal attributes in the file
      call allMessage(ERROR,'Nodal attributes files are not yet supported by adcirc2netcdf.")')
      stop
      ! TODO: structure this like the hotstart file setup below.
   endif
   if ( fileMetaData(fi) % fileFormat .eq. MAUREPT ) then
      fileMetaData(fi) % fileTypeDesc = 'a time varying maureparticle output file'
      fileMetaData(fi) % dataFileType = 'maureparticle'
      cycle ! go to the next file
   endif
   call check(nf90_open(trim(fileMetaData(fi)%dataFileName), NF90_NOWRITE, fileMetaData(fi)%nc_id))
   do i=1,fileMetaData(fi) % nvar
      call check(nf90_inquire_variable(fileMetaData(fi) % nc_id, i, thisVarName))
      select case(trim(thisVarName))
      case("zeta")
         fileMetaData(fi) % fileTypeDesc = 'a time varying 2D ADCIRC water surface elevation file (fort.63)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("eta1")
         fileMetaData(fi) % fileTypeDesc = 'a time varying 2D ADCIRC water surface elevation at previous time step file (eta1.63)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit         
      case("eta2")
         fileMetaData(fi) % fileTypeDesc = 'a time varying 2D ADCIRC water surface elevation at current time step file (eta2.63)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("tk")
         fileMetaData(fi) % fileTypeDesc = 'a time varying 2D bottom friction file (tk.63)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("offset")
         fileMetaData(fi) % fileTypeDesc = 'a time varying water level offset file (offset.63)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("tau0")
         fileMetaData(fi) % fileTypeDesc = 'a time varying tau0 file (fort.90)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("coefdiagonal")
         fileMetaData(fi) % fileTypeDesc = 'a fully consistent ADCIRC LHS matrix diagonal file (coefdiagonal.63)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit         
      case("coefele")
         fileMetaData(fi) % fileTypeDesc = 'an element mass matrix coefficient file (coefele.100)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % dataCenter(1) = 'Cell' ! noff
         exit
      case("nodecode")
         fileMetaData(fi) % fileTypeDesc = 'a node wet/dry state file (nodecode.63)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit  
      case("noff")
         fileMetaData(fi) % fileTypeDesc = 'an element wet/dry state file (noff.100)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % dataCenter(1) = 'Cell' ! noff
         exit          
      case("dryelementareacheck")
         fileMetaData(fi) % fileTypeDesc = 'a dry element area check (dryelementareacheck.100)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % dataCenter(1) = 'Cell' ! noff
         exit
      case("nneighele")
         fileMetaData(fi) % fileTypeDesc = 'a number of elemental neighbors attached to each node file (nneighele.63)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % timeVarying = .false.
         exit  
      case("nodeids")
         fileMetaData(fi) % fileTypeDesc = 'a fortran indexed node IDs file (nodeids.63)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % timeVarying = .false.
         exit  
      case("elementids")
         fileMetaData(fi) % fileTypeDesc = 'a fortran indexed element IDs file (elementids.100)'
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % dataCenter(1) = 'Cell' ! element IDs
         fileMetaData(fi) % timeVarying = .false.
         exit          
      case("zetad")
         fileMetaData(fi) % fileTypeDesc = 'a 2D ADCIRC hotstart file (fort.67/fort.68)'
         fileMetaData(fi) % timeVarying = .false. 
         call initFileMetaData(fileMetaData(fi), thisVarName, 7, 6)
         fileMetaData(fi) % varNameNetCDF(1) = "zeta1"  ! eta1 
         fileMetaData(fi) % varNameNetCDF(2) = "zeta2"  ! eta2 
         fileMetaData(fi) % varNameNetCDF(3) = "zetad"  ! EtaDisc 
         fileMetaData(fi) % varNameNetCDF(4) = "u-vel"  ! uu2 \_combine as vector in XDMF_
         fileMetaData(fi) % varNameNetCDF(5) = "v-vel"  ! vv2 /
         fileMetaData(fi) % varNameNetCDF(6) = "nodecode"  ! nodecode                   
         fileMetaData(fi) % varNameNetCDF(7) = "noff"   ! noff<---element/cell centered
         !
         fileMetaData(fi) % numComponents(4) = 2  ! velocity (uu2,vv2)
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % varNameXDMF(4) = 'hot_start_velocity'
         fileMetaData(fi) % dataCenter(6) = 'Cell' ! noff
         fileMetaData(fi) % timeVarying = .false.
         exit
      case("u-vel","v-vel")
         fileMetaData(fi) % fileTypeDesc = 'a 2D ADCIRC water current velocity file (fort.64)'     
         call initFileMetaData(fileMetaData(fi), thisVarName, 2, 1)
         fileMetaData(fi) % varNameNetCDF(1) = "u-vel"  ! uu2 in ADCIRC
         fileMetaData(fi) % varNameNetCDF(2) = "v-vel"  ! vv2 in ADCIRC
         fileMetaData(fi) % numComponents(1) = 2
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % varNameXDMF(1) = 'water_current_velocity'
         exit
      case("uu1-vel","vv1-vel")
         fileMetaData(fi) % fileTypeDesc = 'a 2D ADCIRC water current velocity at previous time step file (uu1vv1.64)'     
         call initFileMetaData(fileMetaData(fi), thisVarName, 2, 1)
         fileMetaData(fi) % varNameNetCDF(1) = "uu1-vel"  ! uu1 in ADCIRC
         fileMetaData(fi) % varNameNetCDF(2) = "vv1-vel"  ! vv1 in ADCIRC
         fileMetaData(fi) % numComponents(1) = 2
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % varNameXDMF(1) = 'water_current_velocity_at_previous_timestep'
         exit
      case("pressure")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC barometric pressure file (fort.73)"
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)
         call initNamesXDMF(fileMetaData(fi))
         exit 
      case("windx","windy")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC wind velocity file (fort.74)"
         call initFileMetaData(fileMetaData(fi), thisVarName, 2, 1)
         fileMetaData(fi) % varNameNetCDF(1) = "windx"  
         fileMetaData(fi) % varNameNetCDF(2) = "windy"  
         fileMetaData(fi) % numComponents(1) = 2
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % varNameXDMF(1) = 'wind_velocity'
         exit
      case("maxele","zeta_max")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC maximum water surface elevation (maxele.63) file"
         ! Check to see if this is a newer-style min/max file that records
         ! the time of the min or max, and if so, prepare to convert the
         ! time of occurrence metadata as well.     
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("initial_river_elevation")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC initial river elevation (fort.88) file"
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)
         call initNamesXDMF(fileMetaData(fi))
         exit    
      case("maxwvel","wind_max")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC maximum wind speed (maxwvel.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("maxvel","vel_max")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC maximum current speed (maxvel.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("maxrs","radstress_max")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC maximum wave radiation stress gradient (maxrs.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("minpr","pressure_min")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC minimum barometric pressure (minpr.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))      
         exit
      case("endrisinginun")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC nodes with inundation rising at end of simulation (endrisinginun.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("initiallydry")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC dry nodes at cold start (initiallydry.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         exit 
      case("inundationmask")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC inundation mask (inundationmask.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % timeVarying = .false.
         exit          
      case("inun_time")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC total time inundated (inundationtime.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("everdried")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC ever dried (everdried.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         exit   
      case("inun_max")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC maximum inundation depth (maxinundepth.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))
         exit      
      case("radstress_x","radstress_y")
         fileMetaData(fi) % fileTypeDesc = "an ADCIRC wave radiation stress gradient (rads.64) file"
         call initfileMetaData(fileMetaData(fi), thisVarName, 2, 1)
         fileMetaData(fi) % varNameNetCDF(1) = "radstress_x"  
         fileMetaData(fi) % varNameNetCDF(2) = "radstress_y"  
         fileMetaData(fi) % numComponents(1) = 2
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % varNameXDMF(1) = 'wave_radiation_stress_gradient'      
      case("swan_DIR")
         fileMetaData(fi) % fileTypeDesc = "a SWAN wave direction (swan_DIR.63) file"
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit   
      case("swan_HS")
         fileMetaData(fi) % fileTypeDesc = "a SWAN significant wave height (swan_HS.63) file"
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("swan_HS_max")
         fileMetaData(fi) % fileTypeDesc = "a SWAN maximum significant wave height (swan_HS_max.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))      
         exit
      case("swan_TMM10")
         fileMetaData(fi) % fileTypeDesc = "a SWAN mean absolute wave period (swan_TMM10.63) file"
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("swan_TM01")
         fileMetaData(fi) % fileTypeDesc = "SWAN mean absolute wave period (swan_TM01.63) file"
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("swan_TM02")
         fileMetaData(fi) % fileTypeDesc = "a SWAN mean absolute zero crossing period (swan_TM02.63) file"
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("swan_TPS")
         fileMetaData(fi) % fileTypeDesc = "a SWAN smoothed peak period (swan_TPS.63) file"
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("swan_TPS_max")
         fileMetaData(fi) % fileTypeDesc = "a SWAN maximum smoothed peak period (swan_TPS_max.63) file"
         call initMinMaxFileMetaData(fileMetaData(fi), thisVarName, fileMetaData(fi) % nvar)
         call initNamesXDMF(fileMetaData(fi))      
         exit
      case("ESLNodes")
         fileMetaData(fi) % fileTypeDesc = "an elemental slope limiter active nodes (ESLNodes.63) file"
         call initFileMetaData(fileMetaData(fi), thisVarName, 1, 1)     
         call initNamesXDMF(fileMetaData(fi))
         exit
      case("swan_windx","swan_windy")
         fileMetaData(fi) % fileTypeDesc = "a SWAN wind velocity (swan_WIND.64) file"
         call initFileMetaData(fileMetaData(fi), thisVarName, 2, 1)
         fileMetaData(fi) % varNameNetCDF(1) = "swan_windx"  
         fileMetaData(fi) % varNameNetCDF(2) = "swan_windy"  
         fileMetaData(fi) % numComponents(1) = 2
         call initNamesXDMF(fileMetaData(fi))
         fileMetaData(fi) % varNameXDMF(1) = 'swan_wind_velocity' 
         exit
      case default
         cycle     ! did not recognize this variable name
      end select
   end do
   call check(nf90_close(fileMetaData(fi)%nc_id))
end do

!
! Form the file name of XDMF xml file and open it.
call allMessage(INFO,'Writing XDMF xml header.')
if (trim(xdmfFile).eq.'null') then
   xdmfFile = trim(fileMetaData(1)%dataFileName)
   do fi=2,numFiles
      xdmfFile = trim(xdmfFile) // '_' // trim(fileMetaData(fi)%dataFileName)
   end do
   xdmfFile = trim(xdmfFile)//".xmf"
   if (writeParticleFile.eqv..true.) then
      xdmfFile = 'particles.xmf'
   endif
endif
olun = availableUnitNumber()
open(olun,file=xdmfFile,status='replace')
! write the beginning of the XDMF xml file
write(olun,'(a)') '<?xml version="1.0" ?>'
write(olun,'(a)') '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
write(olun,'(a)') '<Xdmf Version="2.0">'
!
if (writeParticleFile.eqv..true.) then
   write(olun,'('//ind('+')//',a)') '<Domain Name="maureparticle">'
else
   write(olun,'('//ind('+')//',a)') '<Domain Name="'//adjustl(trim(agrid))//'">'
endif
!    
! Bomb out if we did not recognize any of the variable names in the file.
do fi=1,numFiles
   if ( fileMetaData(fi) % initialized.eqv..false. ) then
      meshonly = .true.
      call allMessage(INFO,'Did not recognize any of the variables in the file '//trim(fileMetaData(fi)%dataFileName)//'.')
      call allMessage(INFO,'The xml file will only contain mesh-related information.')
      write(olun,'('//ind('+')//',a)') '<Grid Name="'//adjustl(trim(agrid))//'" GridType="Uniform">'
      ! Write mesh portion of XDMF xml file.
      call writeMeshTopologyGeometryDepth(fileMetaData(fi), olun, meshonly)
      ! finish off the xml so the user can at least look at the mesh
      write(olun,'('//ind('-')//',a)') '</Grid>'
      call writeFooterXML(olun)
      close(olun)
      call allMessage(INFO,'Terminating after writing mesh-related into to xml file.')
      stop
   else
      ! log the guessed type of the file(s) for the user
      call allMessage(INFO,'Preparing to write XDMF xml for '//trim(fileMetaData(fi)%fileTypeDesc)//'.')
   endif
end do
!
! If the file only contains data that are not time varying, 
! (e.g., hotstart files, min/max files, and nodal attributes files), 
! then write XDMF Attributes to the same Grid as the mesh itself and 
! be done with it
if ( fileMetaData(1)%timeVarying.eqv..false. ) then
   write(olun,'('//ind('+')//',A)') '<Grid GridType="Uniform">'
   call writeMeshTopologyGeometryDepth(fileMetaData(1), olun, meshonly)
   do fi=1,numFiles
      call writeAttributesXML(fileMetaData(fi), 1, 1, olun)
   end do
   write(olun,'('//ind('|')//',A)') '</Grid>'
   call writeFooterXML(olun)
   close(olun)
   do fi=1,numFiles
      call allMessage(INFO,'Finished writing XDMF xml for '//trim(fileMetaData(fi)%fileTypeDesc)//'.') 
   end do
   stop
endif
!
! Load up the time values (in seconds), if the data are time varying.
! Load up the node and element tables if we are writing a maureparticle 
! file. 
do fi=1,numFiles
   if (fileMetaData(fi)%fileFormat.eq.NETCDF4) then
      allocate(fileMetaData(fi)%timesec(fileMetaData(fi)%nSnaps))
      call check(nf90_open(trim(fileMetaData(fi)%dataFileName), NF90_NOWRITE, fileMetaData(fi)%nc_id))
      call check(nf90_get_var(fileMetaData(fi)%nc_id, fileMetaData(fi)%NC_VarID_time, fileMetaData(fi)%timesec, (/ 1 /), (/ fileMetaData(fi)%nSnaps /) ))
      if ((writeParticleFile.eqv..true.).and.(meshInitialized.eqv..false.)) then
         call findMeshDimsNetCDF(fileMetaData(fi)%dataFileName,fileMetaData(fi)%nc_id)
         call readMeshNetCDF(fileMetaData(fi)%dataFileName,fileMetaData(fi)%nc_id)
         meshInitialized = .true.
      endif
      call check(nf90_close(fileMetaData(fi)%nc_id))
   endif
end do
!
! checks
do fi=2,numFiles
   ! Check to make sure that all the files are either time varying or that
   ! they are all time invariant
   if ( fileMetaData(fi)%timeVarying.neqv.fileMetaData(fi-1)%timeVarying ) then
      call allMessage(ERROR,'Cannot mix time varying files with time invariant files.')
      stop
   endif
   ! 
   ! Check to make sure that all the files have the same number of datasets
   if ( fileMetaData(fi)%nSnaps.ne.fileMetaData(fi-1)%nSnaps ) then
      call allMessage(ERROR,'The files do not all have the same number of datasets.')
      stop
   endif   
   nSnaps = fileMetaData(fi)%nSnaps
   !
   ! Check to make sure all the datasets have the same time stamp
   do iSnap=1,nSnaps
      if ( fileMetaData(fi)%timesec(iSnap).ne.fileMetaData(fi-1)%timesec(iSnap) ) then
         call allMessage(ERROR,'Corresponding datasets from different files have different time stamps.')
         stop
      endif
   end do
end do
!
! Write meta data for time varying data snapshots to XML.
write(scratchMessage,'(a,i0,a)') 'There are ',fileMetaData(1)%nSnaps,' time values (snapshots) in the file(s).' 
call allMessage(INFO,scratchMessage)
!
! Write the metadata for each snapshot in time. 
write(olun,'('//ind('+')//',A)') '<Grid Name="TimeSeries" GridType="Collection" CollectionType="Temporal">'
!
! TODO: This assumes all files have the same number of datasets and that
! the datasets correspond to the same times
if (writeParticleFile.eqv..true.) then
   do iSnap=1,fileMetaData(maureIndex)%nSnaps
      call readTimeVaryingParticlePositions(fileMetaData(maureIndex),particles,fileMetaData(maureIndex)%maxParticles,iSnap)
      call writeTimeVaryingGrid(fileMetaData(maureIndex), iSnap, olun) ! just the header
      call writeTimeVaryingParticlePositions(fileMetaData(maureIndex),particles,fileMetaData(maureIndex)%maxParticles,iSnap,olun)
      ! if there are files other than a particle file, then load these
      ! as well and interpolate all the quantities found there
      if (numFiles.gt.1) then
         do fi=1,numFiles
            ! FIXME: need to load bathy depth to interpolate that too
            if (fi.eq.maureIndex) then
               cycle
            endif
            call interpolateAndWriteTimeVaryingAttributesXML(fileMetaData(fi), fileMetaData(maureIndex), particles, fileMetaData(maureIndex)%maxParticles, iSnap, olun)
         end do
      endif
      write(olun,'('//ind('-')//',A)') '</Grid>' ! closing element for time varying grid
   end do
else
   ! writing XDMF xml for ADCIRC meshed data
   do iSnap=1,fileMetaData(1)%nSnaps      
      call writeTimeVaryingGrid(fileMetaData(1), iSnap, olun)
      call writeMeshTopologyGeometryDepth(fileMetaData(1), olun, meshonly)
      do fi=1,numFiles
         call writeTimeVaryingAttributesXML(fileMetaData(fi), iSnap, fileMetaData(1)%nSnaps, olun)
      end do
      write(olun,'('//ind('-')//',A)') '</Grid>' ! closing element for time varying grid
   end do
end if
write(olun,'('//ind('-')//',A)') '</Grid>' ! closing element for temporal grid collection
call writeFooterXML(olun)
close(olun)
!
call allMessage(INFO,'Finished generating XDMF xml.')

stop

! jump to here when encountering i/o error when reading Maureparticle file
456 write(scratchMessage,'("Attempted to read line ",i0," from the maureparticle file ",a," when an i/o error occurred. The Fortran error code was ",i0,".")') lineNum, trim(fileMetaData(fi)%dataFileName), errorIO
call allMessage(ERROR,scratchMessage)
stop
!----------------------------------------------------------------------
end program generateXDMF
!----------------------------------------------------------------------

! read one dataset of particle positions from the maureparticle file
subroutine readTimeVaryingParticlePositions(fmd, p, mp, isnap)
use asgsio
use adcmesh, only : station_t
use logging
implicit none
type(fileMetaData_t), intent(inout) :: fmd
integer, intent(in) :: iSnap ! dataset counter
integer, intent(in) :: mp ! max number of particles 
type(station_t), intent(inout) :: p(mp) ! particles
integer :: lineNum ! line number counter
integer :: ip ! particle counter
!
lineNum=0
do ip=1,fmd%numParticlesPerSnap(iSnap)
   lineNum = lineNum + 1
   read(unit=fmd%fun,fmt=*,end=482,err=802,iostat=errorIO) p(ip)%iID, p(ip)%lon, p(ip)%lat, fmd%timesec(iSnap), p(ip)%elementIndex
   p(ip)%elementFound = .true.
   p(ip)%z = 0.d0 ! TODO: could provide options for setting this
end do
return

! jump to here when end of file is reached unexpectedly
482 write(scratchMessage,'("Unexpectedly encountered end-of-file when reading from the Maureparticle file ",a,".")') trim(fmd%dataFileName)
! jump to here when encountering i/o error when reading Maureparticle file
call allMessage(ERROR,scratchMessage)
802 write(scratchMessage,'("Attempted to read line ",i0," from the maureparticle file ",a," when an i/o error occurred. The Fortran error code was ",i0,".")') lineNum, trim(fmd%dataFileName), errorIO
call allMessage(ERROR,scratchMessage)
stop

end subroutine ReadTimeVaryingParticlePositions


! write one dataset of particle positions to the XDMF xml file
subroutine writeTimeVaryingParticlePositions(fmd, p, mp, iSnap, olun)
use asgsio, only : fileMetaData_t, ind
use adcmesh, only : station_t
implicit none
type(fileMetaData_t), intent(inout) :: fmd
integer, intent(in) :: mp ! max number of particles 
type(station_t), intent(inout) :: p(mp) ! particles
integer, intent(in) :: iSnap  ! dataset to work on
integer, intent(in) :: olun   ! i/o unit number of xdmf xml file 
integer :: ip ! particle counter

write(olun,'('//ind('|')//',a,i0,a)') '<Topology TopologyType="POLYVERTEX" NumberOfElements="',fmd%numParticlesPerSnap(iSnap),'" NodesPerElement="1"/>'
write(olun,'('//ind('|')//',a)') '<Geometry GeometryType="XYZ">'
write(olun,'('//ind('+')//',a,i0,a)') '<DataItem ItemType="Uniform" Dimensions="',fmd%numParticlesPerSnap(iSnap),' 3" Format="XML">'
do ip=1,fmd%numParticlesPerSnap(iSnap)
   write(olun,'('//ind('|')//',f15.7,f15.7,f15.7)') p(ip)%lon, p(ip)%lat, p(ip)%z
end do
write(olun,'('//ind('|')//',a)')      '</DataItem>'
write(olun,'('//ind('-')//',a)') '</Geometry>'

end subroutine writeTimeVaryingParticlePositions


! reads data array(s) defined on an adcirc mesh at a particular time, 
! interpolates them to the current particle positions, and writes the 
! interpolated data set(s) to XDMF xml
subroutine interpolateAndWriteTimeVaryingAttributesXML(afmd, pfmd, p, mp, iSnap, olun)
use asgsio
use adcmesh
implicit none
! TODO: this assumes the same number of datasets in each file and that 
! the datasets correspond to the same times in seconds
type(fileMetaData_t), intent(inout) :: afmd ! datafile containing attribute values on mesh
type(fileMetaData_t), intent(inout) :: pfmd ! datafile containing particle positions
integer, intent(in) :: mp ! max number of particles 
type(station_t), intent(inout) :: p(mp) ! particles
integer, intent(in) :: iSnap  ! dataset to work on
integer, intent(in) :: olun   ! i/o unit number of xdmf xml file 
real(8), allocatable :: adcirc_data(:,:) ! (np,numComponents)
real(8), allocatable :: interpVals(:,:)  ! interpolated values (numComp,numParticles)
real(8), allocatable :: sp(:) ! particle speeds for 2D velocity
integer :: nc_start(2) ! node number and dataset to start the reading
integer :: nc_count(2) ! number of nodes and datasets to read in 
character(len=100) :: attributeType
logical :: dryNode ! true if the node is dry and values there are undefined
integer :: i ! netcdf variable counter
integer :: k ! particle counter
integer :: v ! variable counter
integer :: n ! node counter
!
! read the attribute values: assume multiple attribute values in a single
! netcdf file are a multicomponent dataset (i.e., a vector)
allocate(adcirc_data(np,afmd%numVarNetCDF))
nc_start = (/ 1, iSnap /)
nc_count = (/ np, 1 /)
call check(nf90_open(trim(afmd%dataFileName), NF90_NOWRITE, afmd%nc_id))
do v=1,afmd%numVarNetCDF
   !write(6,*) 'afmd%nc_id=',afmd%nc_id,' afmd%varID(v)=',afmd%nc_varID(v),' v=',v,' afmd%numVarNetCDF=',afmd%numVarNetCDF,' iSnap=',iSnap
   call check(nf90_get_var(afmd%nc_id,afmd%nc_varID(v),adcirc_data(:,v),nc_start,nc_count))
end do
call check(nf90_close(afmd%nc_id))
if (trim(afmd%varNameNetCDF(1)).eq.'u-vel') then
   allocate(sp(pfmd%numParticlesPerSnap(iSnap))) ! for computing velocity magnitudes
endif 
!
! compute the interpolation weights for all particles in this snap
do k=1,pfmd%numParticlesPerSnap(iSnap)
   call computeStationWeights(p(k))
end do
!
! for each component, look up the data values at the three nodes of 
! the resident element and linearly interpolate them at the particle
! position
allocate(interpVals(afmd%numVarNetCDF,pfmd%numParticlesPerSnap(iSnap)))
do v=1,afmd%numVarNetCDF
   do k=1,pfmd%numParticlesPerSnap(iSnap)
      ! look up the three nodes that make up this element
      do n=1,3
         p(k)%n(n) = nm(p(k)%elementIndex,n)
      end do
      dryNode = .false.
      ! search for dry or undefined nodal values
      if (p(k)%elementIndex.ne.0) then
         do n=1,3
            if ( adcirc_data(p(k)%n(n),v).eq.-99999 ) then
               dryNode = .true. ! at least one of the three nodes has an undefined value
            endif
         end do
      endif
      if (p(k)%elementIndex.eq.0 .or. dryNode.eqv..true.) then
         interpVals(v,k) = -99999.0
      else
         interpVals(v,k) = 0.d0
         ! now interpolate this variable using the weights at the three nodes
         do n=1,3
            interpVals(v,k) = interpVals(v,k) + adcirc_data(p(k)%n(n),v) * p(k)%w(n) 
         enddo
      endif
   end do
end do
!
! now write the interpolated values to XDMF xml
attributeType = "Scalar"
if (afmd%numVarNetCDF.gt.1) then
   attributeType = "Vector"
endif
!
! TODO: This assumes that if the netcdf contains a multicomponent quantity
! (e.g., a 2D vector) that there is only one XDMF variable name to describe
! this quantity 
write(olun,'('//ind('|')//',A)') '<Attribute Name="'//trim(afmd%varNameXDMF(1))//'" AttributeType="'//trim(attributeType)//'" Center="Node">'
write(olun,'('//ind('+')//',a,i0,a)')    '<DataItem DataType="Float" Dimensions="',pfmd%numParticlesPerSnap(iSnap),' 3" Format="XML">'
do k=1,pfmd%numParticlesPerSnap(iSnap)
   ! TODO: Fix this hack for 2D velocity
   if (trim(afmd%varNameNetCDF(1)).eq.'u-vel') then
      ! fill in zero for the third component : velocity in the z direction
      write(olun,'('//ind('|')//',f15.7,f15.7,f15.7)') (interpVals(v,k), v=1,afmd%numVarNetCDF), 0.d0
      ! compute particle speeds while we're at it
      sp(k) = sqrt(interpVals(1,k)**2 + interpVals(2,k)**2)
   else
      write(olun,'('//ind('|')//',f15.7,f15.7,f15.7)') (interpVals(v,k), v=1,afmd%numVarNetCDF)
   endif
end do
write(olun,'('//ind('|')//',a)')    '</DataItem>'
write(olun,'('//ind('-')//',a)') '</Attribute>'
! write an extra attribute containing the particle speeds
if (trim(afmd%varNameNetCDF(1)).eq.'u-vel') then
   write(olun,'('//ind('|')//',a)') '<Attribute AttributeType="Scalar" Center="Node" Name="particle_speed">'
   write(olun,'('//ind('+')//',a,i0,a)')  '<DataItem DataType="Float" Dimensions="',pfmd%numParticlesPerSnap(iSnap),' 1" Format="XML">'
   do k=1,pfmd%numParticlesPerSnap(iSnap)
      write(olun,'('//ind('|')//',f15.7)') sp(k)   
   end do
   write(olun,'('//ind('|')//',a)')    '</DataItem>'
   write(olun,'('//ind('-')//',a)') '</Attribute>'
endif
deallocate(adcirc_data)
if (trim(afmd%varNameNetCDF(1)).eq.'u-vel') then
   deallocate(sp)
endif
deallocate(interpVals)

end subroutine interpolateAndWriteTimeVaryingAttributesXML


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
allocate(fmd % numComponents(numXDMF))
fmd % numComponents(:) = 1         ! initialize to most common value
allocate(fmd % dataCenter(numXDMF))
fmd%dataCenter(:) = 'Node'                 ! initialize to most common value
allocate(fmd % numberType(numXDMF))
fmd%numberType(:) = 'Float'                 ! initialize to most common value
allocate(fmd % numberPrecision(numXDMF))
fmd%numberPrecision(:) = 8                 ! initialize to most common value
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
subroutine initMinMaxFileMetaData(fmd, someVarName, nvar)
use netcdf
use asgsio, only : check, fileMetaData_t
use logging, only : allMessage, INFO
implicit none
type(fileMetaData_t), intent(inout) :: fmd 
character(NF90_MAX_NAME), intent(in) :: someVarName 
integer, intent(in) :: nvar
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
   call check(nf90_inquire_variable(fmd%nc_id, j, aVarName))
   if (trim(aVarName).eq.trim(timeOfVarName)) then
      call allMessage(INFO,'The file contains time of occurrence data.')
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
subroutine initNamesXDMF(fmd)
use netcdf
use asgsio, only : check, fileMetaData_t
use logging, only : allMessage, ERROR
implicit none
type(fileMetaData_t) :: fmd
integer :: i, j

! find the netcdf variable IDs
do i=1,fmd % numVarNetCDF
   call check(nf90_inq_varid(fmd%nc_id, fmd % varNameNetCDF(i), fmd % nc_varID(i)))
   call check(nf90_inquire_variable(fmd % nc_id, fmd%nc_varID(i), fmd%varNameNetCDF(i), fmd%nc_type(i)))
end do
! set the names of the XDMF data 
i=1 ! netcdf variable counter
j=1 ! xdmf variable counter
do 
   ! for vector data, the name will be replaced in the calling routine anyway
   if (trim(fmd%varNameNetCDF(i)).ne."noff") then
      !write(6,'(a,a)') 'DEBUG: generateXDMF: seeking NetCDF variable ID for ',trim(fmd%varNameNetCDF(i))
      ! the standard_name attribute is missing for noff in some netcdf hotstart files   
      call check(nf90_get_att(fmd%nc_id, fmd % nc_varID(i), 'standard_name', fmd % varNameXDMF(j)))
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
      fmd%numberType(j) = "Int"
      fmd%numberPrecision(j) = 4
   case(NF90_FLOAT)
      fmd%numberType(j) = "Float"
      fmd%numberPrecision(j) = 4
   case(NF90_DOUBLE)
      fmd%numberType(j) = "Float"
      fmd%numberPrecision(j) = 8
   case default
      call allMessage(ERROR,'The netCDF variable '//trim(fmd%varNameNetCDF(i))//' uses an unknown data type.')
      stop
   end select

   i = i + fmd % numComponents(j) ! multi component (vector) data only need 1 name
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
subroutine writeMeshTopologyGeometryDepth(fmd, olun, meshonly)
use asgsio, only : fileMetaData_t, ind
use adcmesh, only : agrid, ne, np
implicit none
type(fileMetaData_t), intent(in) :: fmd
integer, intent(in) :: olun ! i/o unit number to write XDMF xml to
logical, intent(in) :: meshonly ! true if only the mesh xml are being written
character(len=1) :: indent
!
indent = '|'
if (meshonly.eqv..true.) then
   indent = '+'
endif
write(olun,'('//ind(indent)//',a)') '<Topology Name="ADCIRCMesh"'
write(olun,'('//ind('|')//',A)')     '  TopologyType="Triangle"'
write(olun,'('//ind('|')//',A)')     '  NodesPerElement="3"'
write(olun,'('//ind('|')//',A,i0,A)')'  NumberOfElements="',ne,'"'
write(olun,'('//ind('|')//',A)')     '  BaseOffset="1">'
write(olun,'('//ind('+')//',A,i0,A)') '<DataItem Dimensions="',ne,'  3"'
write(olun,'('//ind('|')//',A)')       '  DataType="Int"'
write(olun,'('//ind('|')//',A)')       '  Format="HDF">'//trim(fmd%dataFileName)//':/element'
write(olun,'('//ind('|')//',A)')      '</DataItem>'
write(olun,'('//ind('-')//',A)') '</Topology>'
write(olun,'('//ind('|')//',A)') '<Geometry Name="NodeLocations"'
write(olun,'('//ind('|')//',A)') '  GeometryType="X_Y">'
write(olun,'('//ind('+')//',A,i0,A)') '<DataItem Dimensions="',np,'"'
write(olun,'('//ind('|')//',A)')      '   NumberType="Float"'
write(olun,'('//ind('|')//',A)')      '   Precision="8"'
if (fmd%useCPP.eqv..true.) then
   write(olun,'('//ind('|')//',A)')   '   Format="HDF">'//trim(fmd%dataFileName)//':/x_cpp'
else
   write(olun,'('//ind('|')//',A)')   '   Format="HDF">'//trim(fmd%dataFileName)//':/x'
endif
write(olun,'('//ind('|')//',A)')      '</DataItem>'
write(olun,'('//ind('|')//',A,i0,A)') '<DataItem Dimensions="',np,'"'
write(olun,'('//ind('|')//',A)')      '  NumberType="Float"'
write(olun,'('//ind('|')//',A)')      '  Precision="8"'
if (fmd%useCPP.eqv..true.) then
   write(olun,'('//ind('|')//',A)')   '  Format="HDF">'//trim(fmd%dataFileName)//':/y_cpp'
else
   write(olun,'('//ind('|')//',A)')   '  Format="HDF">'//trim(fmd%dataFileName)//':/y'
endif
write(olun,'('//ind('|')//',A)')      '</DataItem>'
write(olun,'('//ind('-')//',A)')   '</Geometry>'
write(olun,'('//ind('|')//',A)')   '<Attribute Name="BathymetricDepth"'
write(olun,'('//ind('|')//',A)')   '  AttributeType="Scalar"'
write(olun,'('//ind('|')//',A)')   '  Center="Node">'
write(olun,'('//ind('+')//',A,i0,A)') '<DataItem Dimensions="',np,'"'
write(olun,'('//ind('|')//',A)')      '  NumberType="Float"'
write(olun,'('//ind('|')//',A)')      '  Precision="8"'
write(olun,'('//ind('|')//',A)')      '  Format="HDF">'//trim(fmd%dataFileName)//':/depth'
write(olun,'('//ind('|')//',A)')      '</DataItem>'
write(olun,'('//ind('-')//',A)')   '</Attribute>'
!----------------------------------------------------------------------
end subroutine writeMeshTopologyGeometryDepth
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!  S U B R O U T I NE   W R I T E   T I M E   V A R Y I N G   G R I D 
!----------------------------------------------------------------------
! Creates the Grid metadata inside the Temporal collection to describe
! each time snap.  
!----------------------------------------------------------------------
subroutine writeTimeVaryingGrid(fmd, isnap, olun)
use asgsio, only : fileMetaData_t, ind
implicit none
type(fileMetaData_t), intent(in) :: fmd
integer, intent(in) :: isnap ! the dataset number from the file
integer, intent(in) :: olun ! i/o unit number to write XDMF xml to
character(len=1) :: indent
!
indent = '|'
if (isnap.eq.1) then
   indent = '+'
endif
! Write the Grid xml for the time varying data
! now write XDMF XML data for this dataset
write(olun,'('//ind(indent)//',A,E22.15,A)') '<Grid Name="Time=',fmd%timesec(isnap),'" GridType="Uniform">'
write(olun,'('//ind('+')//',A,E22.15,A)') '<Time Value="',fmd%timesec(isnap),'"/>'
!----------------------------------------------------------------------
end subroutine writeTimeVaryingGrid
!----------------------------------------------------------------------


!----------------------------------------------------------------------
!  S U B R O U T I N E    W R I T E   A T T R I B U T E S   X M L 
!----------------------------------------------------------------------
! Writes the Attribute(s) metadata to an XML file.
!----------------------------------------------------------------------
subroutine writeAttributesXML(fmd, iSnap, nSnaps, olun)
use adcmesh, only : np, ne
use asgsio, only : fileMetaData_t, ind
implicit none
type(fileMetaData_t), intent(in) :: fmd
integer, intent(in) :: iSnap
integer, intent(in) :: nSnaps
integer, intent(in) :: olun ! i/o unit number to write XDMF xml to
!
integer :: dataItemDimensions
character(len=20) :: attributeType
integer :: i, j, k
!
i=1 ! netCDF variable counter
j=1 ! XDMF variable counter
do 
   attributeType = "Scalar"
   if (fmd%numComponents(j).gt.1) then
      attributeType = "Vector"
   endif
   dataItemDimensions = np
   if (trim(fmd%dataCenter(j)).eq."Cell") then
      dataItemDimensions = ne
   endif
   !
   write(olun,'('//ind('|')//',A)') '<Attribute Name="'//trim(fmd%varNameXDMF(j))//'"'
   write(olun,'('//ind('|')//',A)') '  Center="'//trim(fmd%dataCenter(j))//'"'
   write(olun,'('//ind('|')//',A)') '  AttributeType="'//trim(attributeType)//'">'
   !
   ! Scalar attribute
   if (trim(attributeType).eq."Scalar") then
      write(olun,'('//ind('+')//',A,i0,A)') '<DataItem Dimensions="',dataItemDimensions,'"'
      write(olun,'('//ind('|')//',a,a,a)')  '  NumberType="',trim(fmd%numberType(j)),'"'
      write(olun,'('//ind('|')//',a,i0,a)') '  Precision="',fmd%numberPrecision(j),'"'
      write(olun,'('//ind('|')//',A)')      '  Format="HDF">'//trim(fmd%dataFileName)//':/'//trim(fmd%varNameNetCDF(i))
      write(olun,'('//ind('|')//',A)')      '</DataItem>'
      write(olun,'('//ind('-')//',A)')   '</Attribute>' ! end of scalar attribute
   !
   ! Vector attribute
   else 
      write(olun,'('//ind('|')//',A)')      '<DataItem ItemType="Function"'
      write(olun,'('//ind('|')//',A,i0,A)') '  Dimensions="',dataItemDimensions,' 3"'
      write(olun,'('//ind('|')//',A)')      '  Function="JOIN($0, $1, 0*$0)">'
      do k=0,fmd%numComponents(j)-1
         if (k.eq.0) then
            write(olun,'('//ind('+')//',A)')   '<DataItem ItemType="HyperSlab"'
         else 
            write(olun,'('//ind('|')//',A)')   '<DataItem ItemType="HyperSlab"'
         endif
         write(olun,'('//ind('|')//',A,i0,A)') '  Dimensions="',dataItemDimensions,'"'
         write(olun,'('//ind('|')//',A)')      '  Type="HyperSlab">'
         write(olun,'('//ind('+')//',A)')         '<DataItem Dimensions="3 2"'
         write(olun,'('//ind('|')//',A)')         '  Format="XML">'
         write(olun,'('//ind('|')//',A,i0,A)')    '  ',iSnap-1,' 0'
         write(olun,'('//ind('|')//',A)')         '  1 1'
         write(olun,'('//ind('|')//',A,i0)')      '  1 ',dataItemDimensions
         write(olun,'('//ind('|')//',A)')         '</DataItem>' ! end of dimensions
         ! a steady vector attribute still has to have a first 
         ! dimension of 1 instead of nSnaps-1
         write(olun,'('//ind('|')//',A,i0,A)')    '<DataItem Dimensions="1 ',dataItemDimensions,'"' 
         write(olun,'('//ind('|')//',a,a,a)')     '  NumberType="',trim(fmd%numberType(j)),'"'
         write(olun,'('//ind('|')//',a,i0,a)')    '  Precision="',fmd%numberPrecision(j),'" Format="HDF">'//trim(fmd%dataFileName)//':/'//trim(fmd%varNameNetCDF(i+k))
         write(olun,'('//ind('|')//',A)')         '</DataItem>' ! end of Dimensions
         write(olun,'('//ind('-')//',A)')      '</DataItem>' ! end of HyperSlab
      enddo
      write(olun,'('//ind('-')//',A)')      '</DataItem>' ! end of FUNCTION
      write(olun,'('//ind('-')//',A)')   '</Attribute>' ! end of Vector Attribute
   endif
   i = i + fmd%numComponents(j)
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
subroutine writeTimeVaryingAttributesXML(fmd, iSnap, nSnaps, olun)
use adcmesh, only : np, ne
use asgsio, only : fileMetaData_t, ind
implicit none
type(fileMetaData_t), intent(in) :: fmd
integer, intent(in) :: iSnap
integer, intent(in) :: nSnaps
integer, intent(in) :: olun ! i/o unit number to write XDMF xml to
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
   if (fmd%numComponents(j).gt.1) then
      attributeType = "Vector"
   endif
   if (trim(fmd%dataCenter(j)).eq."Cell") then
      domainExtent = ne
   endif
   !
   write(olun,'('//ind('|')//',A)') '<Attribute Name="'//trim(fmd%varNameXDMF(j))//'"'
   write(olun,'('//ind('|')//',A)') '  Center="'//trim(fmd%dataCenter(j))//'"'
   write(olun,'('//ind('|')//',A)') '  AttributeType="'//trim(attributeType)//'">'
   !
   ! Scalar attribute
   if (trim(attributeType).eq."Scalar") then
      write(olun,'('//ind('+')//',A)')    '<DataItem ItemType="HyperSlab"'
      write(olun,'('//ind('|')//',A,i0,A)')'  Dimensions="',domainExtent,'"'      
      write(olun,'('//ind('|')//',A)')    '   Type="HyperSlab">'

      write(olun,'('//ind('+')//',A)')    '<DataItem Dimensions="3  2"'
      write(olun,'('//ind('|')//',A)')    '  Format="XML">'
      write(olun,'('//ind('|')//',A,i0,A)')'  ',iSnap-1,' 0'
      write(olun,'('//ind('|')//',A)')    '  1 1'
      write(olun,'('//ind('|')//',A,i0)') '  1 ',domainExtent
      write(olun,'('//ind('|')//',A)')    '</DataItem>'
            
      write(olun,'('//ind('|')//',A,i0,1x,i0,A)') '<DataItem Dimensions="',nSnaps-1,domainExtent,'"'
      write(olun,'('//ind('|')//',a,a,a)')        '  NumberType="',trim(fmd%numberType(j)),'"'
      write(olun,'('//ind('|')//',a,i0,a)')       '  Precision="',fmd%numberPrecision(j),'"'
      write(olun,'('//ind('|')//',A)')            '  Format="HDF">'//trim(fmd%dataFileName)//':/'//trim(fmd%varNameNetCDF(i))
      write(olun,'('//ind('|')//',A)')            '</DataItem>'
      write(olun,'('//ind('-')//',A)')         '</DataItem>'
      write(olun,'('//ind('-')//',A)')      '</Attribute>' ! end of scalar attribute
   !
   ! Vector attribute
   else 
      write(olun,'('//ind('+')//',A)')      '<DataItem ItemType="Function"'
      write(olun,'('//ind('|')//',A,i0,A)') '  Dimensions="',domainExtent,' 3"'
      write(olun,'('//ind('|')//',A)')      '  Function="JOIN($0, $1, 0*$0)">'
      do k=0,fmd%numComponents(j)-1
         if (k.eq.0) then
            write(olun,'('//ind('+')//',A)')   '<DataItem ItemType="HyperSlab"'
         else      
            write(olun,'('//ind('|')//',A)')   '<DataItem ItemType="HyperSlab"'
         endif
         write(olun,'('//ind('|')//',A,i0,A)') '  Dimensions="',domainExtent,'"'
         write(olun,'('//ind('|')//',A)')      '  Type="HyperSlab">'
         write(olun,'('//ind('+')//',A)')         '<DataItem Dimensions="3 2"'
         write(olun,'('//ind('|')//',A)')         '  Format="XML">'
         write(olun,'('//ind('|')//',A,i0,A)')    '  ',iSnap-1,' 0'
         write(olun,'('//ind('|')//',A)')         '  1 1'
         write(olun,'('//ind('|')//',A,i0)')      '  1 ',domainExtent
         write(olun,'('//ind('|')//',A)')         '</DataItem>' ! end of dimensions
         write(olun,'('//ind('|')//',A,i0,1x,i0,A)') '<DataItem Dimensions="',nSnaps-1,domainExtent,'"'
         write(olun,'('//ind('|')//',a,a,a)')        '  NumberType="',trim(fmd%numberType(j)),'"'
         write(olun,'('//ind('|')//',a,i0,a)')       '  Precision="',fmd%numberPrecision(j),'"' 
         write(olun,'('//ind('|')//',A)')            '  Format="HDF">'//trim(fmd%dataFileName)//':/'//trim(fmd%varNameNetCDF(i+k))
         write(olun,'('//ind('|')//',A)')            '</DataItem>' ! end of Dimensions
         write(olun,'('//ind('-')//',A)')         '</DataItem>' ! end of HyperSlab
      enddo
      write(olun,'('//ind('-')//',A)')         '</DataItem>' ! end of FUNCTION
      write(olun,'('//ind('-')//',A)')      '</Attribute>' ! end of Vector Attribute
   endif
   i = i + fmd%numComponents(j)
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
subroutine writeFooterXML(olun)
use asgsio, only : ind
implicit none
integer, intent(in) :: olun

write(olun,'('//ind('-')//',a)') '</Domain>'
write(olun,'(a)') '</Xdmf>'

!----------------------------------------------------------------------
end subroutine writeFooterXML
!----------------------------------------------------------------------


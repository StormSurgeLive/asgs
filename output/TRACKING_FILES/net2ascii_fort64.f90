
! The data file read by this program is produced by ASGS ADCIRC using netCDF 
! formatting option.

! This program is derived from the netCDF tutorial:
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial

! Full documentation of the netCDF Fortran 90 API can be found at:
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90
!
! compile on ranger by using the netcdf module and the following command
! pgf90 -O net2ascii_fort64.f90 -o net2ascii_fort64.exe -I$TACC_NETCDF_INC -L$TACC_NETCDF_LIB -lnetcdf
!

program netcdf2ascii_fort64
  use netcdf
  implicit none

  ! This is the name of the data file we will read.
  character (len = *), parameter :: FILE_NAME = "fort64.nc"
  integer :: ncid, ncstatus,status

  ! We are reading data, an unstructured FE grid.
  ! time, node, u, v

  integer, parameter :: NDIMS  = 2
  integer :: ZeroCount
  ! Dimension Names
  character (len = *), parameter :: NOD_NAME = "node"
  character (len = *), parameter :: REC_NAME = "time"
  integer :: node_DimID, Rec_DimID
  integer :: numNodes, numRecs

  ! We will read current velocity fields. 
  character (len = *), parameter :: UVEL_NAME = "u-vel"
  character (len = *), parameter :: VVEL_NAME = "v-vel"
  character (len = *), parameter :: TSMP_NAME = "time"
  integer :: uvel_VarID
  integer :: vvel_VarID
  integer :: tmsp_VarID
  integer :: dimids(NDIMS)

  ! To check the units attributes.
  character (len = *), parameter :: UNITS = "units"
  character (len = *), parameter :: TSMP_UNITS = "seconds"
  character (len = *), parameter :: VEL_UNITS = "mps"
  integer, parameter :: MAX_ATT_LEN = 80
  integer :: att_len
  character*(MAX_ATT_LEN) :: tmps_base_date_in
  character*(MAX_ATT_LEN) :: CS_Date_chr , name2
  character*(MAX_ATT_LEN) :: vel_units_in
  integer :: xtype2, ndims2,numDims2
  integer, dimension(2) :: dimids2
  integer :: nAtts2
  integer :: CS_Date
  integer :: year,mo,dy,hr,mn,sc

  ! We will learn about the data file and store results in these
  ! program variables.
  integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
  ! Loop indices
  integer :: node,rec,i,NumNonDefaultNodes
  integer :: numnodes2, numtimes2
  real :: outputdt,dt
  ! Read the data into these arrays.
  real,ALLOCATABLE :: uvel(:,:)
  real,ALLOCATABLE :: vvel(:,:)
  real,ALLOCATABLE :: timestamp(:)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Open the file. 
  status =   nf90_open(FILE_NAME, nf90_nowrite, ncid)
   if(status /= nf90_noerr) then
      print *, nf90_strerror(status)
      stop "Stopped"
    end if

  ! There are a number of inquiry functions in netCDF which can be
  ! used to learn about an unknown netCDF file. NF90_INQ tells how many
  ! netCDF variables, dimensions, and global attributes are in the
  ! file; also the dimension id of the unlimited dimension, if there
  ! is one.
  status =  ( nf90_inquire(ncid, ndims_in, nvars_in, ngatts_in, unlimdimid_in) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  ! Get the dimensions so we can allocate the variables
  ! number of nodes
  status =  ( nf90_inq_dimid(ncid, NOD_NAME, node_DimID) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  status =  ( nf90_inquire_dimension(ncid, node_DimID, len = numNodes) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  ! number of records
  status =  ( nf90_inq_dimid(ncid, REC_NAME, rec_DimID) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  status =  ( nf90_inquire_dimension(ncid, rec_DimID, len = numRecs) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
    print *,"numRecs =", numRecs 
    print *,"numNodes =", numNodes 
  ! ALLOCATE Variable Arrays
  ALLOCATE(uvel(numRecs,numNodes),vvel(numRecs,numNodes),timestamp(numRecs))

  status =  ( nf90_inq_varid(ncid, TSMP_NAME, tmsp_VarID) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  ! Get the varids of the current velocity netCDF variables.
  status =  ( nf90_inq_varid(ncid, UVEL_NAME, uvel_VarID) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if

   print *, UVEL_NAME, uvel_VarID

  status =  ( nf90_inq_varid(ncid, VVEL_NAME, vvel_VarID) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if

  status =  ( nf90_get_att(ncid, tmsp_VarID, "base_date", tmps_base_date_in) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  status =  ( nf90_inquire_attribute(ncid, tmsp_VarID, "base_date", len = att_len) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  read(tmps_base_date_in,FMT='(i4.4, 5(1x,i2.2) )') year,mo,dy,hr,mn,sc
  write(CS_Date_chr ,'(a3,i4.4, 5(i2.2))') "cs:",year, mo, dy, hr, mn, sc
  print *, "CS_Date = ",TRIM(CS_Date_chr)
  
   status =  ( nf90_get_att(ncid, nf90_global, "dt", dt) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if

   status =  ( nf90_get_var(ncid, tmsp_VarID, timestamp, start=(/1/), count=(/numRecs/)) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      print *, "timestamp"
      stop "Stopped"
    end if
      outputdt=timestamp(2)-timestamp(1)
  ! open fort.64 file and write the headers
   open(20,file='fort.64',status='unknown')
   write(20,*) TRIM(CS_Date_chr)  !HEADER1
   write(20,*) NumRecs, numNodes, outputdt, nint(outputdt/dt)
   ! now read in data one record at a time and write to
   ! output file

    status= nf90_Inquire_Variable(ncid, uvel_VarID, ndims=numDims2)
    status= nf90_Inquire_Variable(ncid, uvel_VarID, dimids=dimids2(:numDims2))
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      print *, "u-velinfo"
      stop "Stopped"
    end if
     print *, uvel_VarID, numDims2, dimids2
  status= nf90_Inquire_Dimension(ncid, dimids2(1), len= numtimes2) 
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      print *, "u-velinfo dimid1"
      stop "Stopped"
    end if 
  status= nf90_Inquire_Dimension(ncid, dimids2(2), len= numnodes2) 
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      print *, "u-velinfo dimid2"
      stop "Stopped"
    end if 
     print *, uvel_VarID, numDims2, dimids2, numtimes2, numnodes2
    

   status =  ( nf90_get_var(ncid, uvel_VarID, uvel, start=(/1,1/), count=(/numNodes,numRecs/) ) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      print *, "u-vel"
      stop "Stopped"
    end if
   status =  ( nf90_get_var(ncid, vvel_VarID, vvel, start=(/1,1/), count=(/numNodes,numRecs/)  ) )
   if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      print *, "v-vel"
      stop "Stopped"
    end if

   do rec = 1, NumRecs

   NumNonDefaultNodes=0
   do node = 1, numNodes
    if (  ( (uvel(node,rec) .eq. 0. ) .AND. (vvel(node,rec) .eq. 0. ) ) ) then
         NumNonDefaultNodes=NumNonDefaultNodes
    else
        NumNonDefaultNodes=NumNonDefaultNodes+1
    endif
    !  .AND. ( (uvel(node,rec) .gt. -9999.) .AND.  (vvel(node,rec) .gt. -9999.)  ) ) &
   enddo
         
   ! write time stamp data for current record
   write(20,'(e16.10,4x,i10,4x,i10,4x,f8.0)') timestamp(rec), nint(timestamp(rec)/dt), NumNonDefaultNodes, -99999.
   ! loop through nodes and write output
   do node = 1, numNodes
    if (  ( (uvel(node,rec) .eq. 0. ) .AND. (vvel(node,rec) .eq. 0. ) ) ) then
        
    else
     write(20,'(i10,2x,e18.10,2x,e18.10)') node, uvel(node,rec), vvel(node,rec)
    endif
   enddo


  ! next record 
  enddo

  ! Close the file. This frees up any internal netCDF resources
  ! associated with the file.
  status =  ( nf90_close(ncid) )
    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if

  ! If we got this far, everything worked as expected. Yipee! 
  print *,"*** SUCCESS reading fort64.nc!"

 
  DEALLOCATE(uvel,vvel,timestamp)



end program netcdf2ascii_fort64


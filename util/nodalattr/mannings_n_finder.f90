! Example of compilation with gfortran:
! gfortran -ffree-line-length-none -o mannings_n_finder.x -I/home/jason/asgs/trunk/output -I/usr/include  mannings_n_finder.f90  -lnetcdff
!
! Example of compiling with gfortran with debugging turned on:
! gfortran -g -O0 -Wall -ffree-line-length-none -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal -o mannings_n_finder.x -I/home/jason/asgs/trunk/output -I/usr/include  mannings_n_finder.f90  -lnetcdff
!
! Example of compiling with gfortran with profiling and test coverage turned on:
! gfortran -pg -O0 -fprofile-arcs -ftest-coverage -Wall -ffree-line-length-none -o mannings_n_finder.x -I/home/jason/asgs/trunk/output -I/usr/include mannings_n_finder.f90 -lnetcdff
!-----------------------------------------------------------------------
include 'adcmesh.f90'
include 'landuse.f90'
program mannings_n_finder
use adcmesh
use landuse
implicit none
character(len=1024) :: outputfile
! 
integer :: luclass   ! land use code at a particular mesh node
real, allocatable :: manningsn(:) ! (np) Manning's n vale at each mesh node
real, allocatable :: diff(:)      ! (np) difference between land use mannings n and the default
real :: defaultManningsN ! expected to be the most common value

logical :: classFound ! .true. if the mesh node is in the area covered by NLCD data
logical :: codeFound ! .true. if lookup table contains the NLCD code
integer :: n_col  ! column number in the land cover data file 
integer :: n_row  ! row number in the land cover data file 
character(1024) :: cmdlinearg
character(1024) :: cmdlineopt
integer :: argcount
integer :: i, j
character(len=1024) :: nodal_attr_name = 'mannings_n_at_sea_floor'
!
! process command line options
argcount = iargc() ! count up command line options
write(6,*) 'There are ',argcount,' command line options.'
i=0
do while (i.lt.argcount)
   i = i + 1
   call getarg(i, cmdlineopt)
   select case(trim(cmdlineopt))
   case("--verbose")
      write(6,*) "INFO: Processing ",trim(cmdlineopt),"."
      verbose = .true.
   case("--meshfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      meshFileName = trim(cmdlinearg)
   case("--outputfile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      outputfile = trim(cmdlinearg)
   case("--landusefile")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      landusefile = trim(cmdlinearg)
   case("--lookuptable")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      lookupTableName = trim(cmdlinearg)
   case("--default-mannings-n")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) defaultManningsN
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
write(6,*) 'INFO: The default Mannings n value is ',defaultManningsN,'.'
!
! load the specified lookup table
call loadLookupTable()
!
! load the specified land cover data file
call loadLandUse()
!
! Open and read the mesh file.
write(6,*) 'INFO: Reading mesh file.'
call read14()
write(6,*) 'INFO: Finished reading mesh file.'
!
! Project each mesh node into albers equal area conic. 
call computeAlbersEqualAreaConic()

! Figure out the NLCD row and column that corresponds to each mesh node
! location. Then convert the NLCD classification to an equivalent 
! manning's n using the user-specified lookup table.

! If the mesh node is outside the area covered by the NLCD data,
! the land use classification is set to -9999 for that node, and the 
! lookup table is used to convert the -9999 value to a default manning's n.
allocate(manningsn(np))
manningsn = defaultManningsN
do i=1,np
   classFound = .false.
   codeFound = .false.
   ! make sure this node is in our NLCD area
   if ( (xalbers(i).gt.xmin).and.(xalbers(i).lt.xmax).and. &
        (yalbers(i).gt.ymin).and.(yalbers(i).lt.ymax) ) then
      n_col = nint(abs((xalbers(i)-xmin)/res)) + 1 ! count columns from left
      n_row = nint(abs((ymax-yalbers(i))/res)) + 1 ! count rows from top
      luclass = nlcd_class(n_row,n_col)
      classFound = .true.
      ! use the lookup table to convert land use code to mannings n
      do j=1,nlc
         if (lookupTableKey(j).eq.luclass) then
            codeFound = .true.
            manningsn(i) = lookupTableValue(j)
            exit
         endif
      end do
   else
      if (verbose.eqv..true.) then
         write(6,*) 'WARNING: Node ',i,' is outside the area covered by NLCD data.'
         write(6,*) 'xalbers=',xalbers(i),' yalbers=',yalbers(i)
      endif
   endif
   ! deal with the case where the land use data contains codes not found
   ! in the user-specified lookup table
   if ((classFound.eqv..true.).and.(codeFound.eqv..false.).and.(verbose.eqv..true.)) then
      write(6,*) 'ERROR: The land use code ',luclass, &
      'was not found in the lookup table.' 
   end if
end do
! write the nondefault values
allocate(diff(np))
! machine precision prevents us from simply checking whether the 
! mannings n .ne. the default value
diff = abs(manningsn(:) - defaultManningsN)
open(12,file=trim(outputfile),action='write',status='replace')
! write the number of nondefault values
write(12,'(A)') trim(nodal_attr_name)
write(12,*) count(diff.gt.1.e-6)
do i=1,np
   if (diff(i).gt.1.e-6) then
      write(12,*) i,manningsn(i)
   endif
end do
!-----------------------------------------------------------------------      
end program mannings_n_finder
!-----------------------------------------------------------------------

!if (nlcd_class.eq.11) mannings_n=0.02    !Open Water
!if (nlcd_class.eq.12) mannings_n=0.010   !Perennial Ice/Snow
!if (nlcd_class.eq.21) mannings_n=0.020   !Developed - Open Space
!if (nlcd_class.eq.22) mannings_n=0.050   !Developed - Low Intensity
!if (nlcd_class.eq.23) mannings_n=0.100   !Developed - Medium Intensity
!if (nlcd_class.eq.24) mannings_n=0.150   !Developed - High Intensity
!if (nlcd_class.eq.31) mannings_n=0.090   !Barren Land (Rock/Sand/Clay)
!if (nlcd_class.eq.32) mannings_n=0.040   !Unconsolidated Shore
!if (nlcd_class.eq.41) mannings_n=0.100   !Deciduous Forest
!if (nlcd_class.eq.42) mannings_n=0.110   !Evergreen Forest
!if (nlcd_class.eq.43) mannings_n=0.100   !Mixed Forest
!if (nlcd_class.eq.51) mannings_n=0.040   !Dwarf Scrub
!if (nlcd_class.eq.52) mannings_n=0.050   !Shrub/Scrub
!if (nlcd_class.eq.71) mannings_n=0.034   !Grassland/Herbaceous
!if (nlcd_class.eq.72) mannings_n=0.030   !Sedge/Herbaceous
!if (nlcd_class.eq.73) mannings_n=0.027   !Lichens
!if (nlcd_class.eq.74) mannings_n=0.025   !Moss
!if (nlcd_class.eq.81) mannings_n=0.033   !Pasture/Hay
!if (nlcd_class.eq.82) mannings_n=0.037   !Cultivated Crops
!if (nlcd_class.eq.90) mannings_n=0.100   !Woody Wetlands
!if (nlcd_class.eq.91) mannings_n=0.100   !Palustrine Forested Wetland
!if (nlcd_class.eq.92) mannings_n=0.048   !Palustrine Scrub/Shrib Wetland
!if (nlcd_class.eq.93) mannings_n=0.100   !Estuarine Forested Wetland
!if (nlcd_class.eq.94) mannings_n=0.048   !Estuarine Scrub/Shrub Wetland
!if (nlcd_class.eq.95) mannings_n=0.045   !Emergent Herbaceous Wetlands
!if (nlcd_class.eq.96) mannings_n=0.045   !Palustrine Emergent Wetland (Persistant)
!if (nlcd_class.eq.97) mannings_n=0.045   !Estuarine Emergent Wetland
!if (nlcd_class.eq.98) mannings_n=0.015   !Palustrine Aquatic Bed
!if (nlcd_class.eq.99) mannings_n=0.015   !Estuarine Aquatic Bed

!------------------------------------------------------------------
! mannings_n_finder.f90: Reads ADCIRC mesh file and land use data
! and produces manning's n values.
!------------------------------------------------------------------
! Copyright(C) 2017 Jason Fleming
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
!------------------------------------------------------------------
! Compile with accompanying makefile. 
!------------------------------------------------------------------
program mannings_n_finder
use adcmesh
use ioutil
use logging
use landuse
implicit none
type(mesh_t) :: m
character(len=1024) :: outputfile 
integer :: luclass   ! land use code at a particular mesh node
real, allocatable :: manningsn(:) ! (np) Manning's n vale at each mesh node
real, allocatable :: diff(:)      ! (np) difference between land use mannings n and the default
real :: defaultManningsN ! expected to be the most common value
logical :: classFound ! .true. if the mesh node is in the area covered by NLCD data
logical :: codeFound ! .true. if lookup table contains the NLCD code
integer :: n_col  ! column number in the land cover data file 
integer :: n_row  ! row number in the land cover data file 
integer :: i, j
character(len=1024) :: nodal_attr_name = 'mannings_n_at_sea_floor'
integer :: mnUnit
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
      m%meshFileName = trim(cmdlinearg)
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
call read14(m)
write(6,*) 'INFO: Finished reading mesh file.'
!
! Project each mesh node into albers equal area conic. 
call computeAlbersEqualAreaConic(m)

! Figure out the NLCD row and column that corresponds to each mesh node
! location. Then convert the NLCD classification to an equivalent 
! manning's n using the user-specified lookup table.

! If the mesh node is outside the area covered by the NLCD data,
! the land use classification is set to -9999 for that node, and the 
! lookup table is used to convert the -9999 value to a default manning's n.
allocate(manningsn(m%np))
manningsn = defaultManningsN
do i=1,m%np
   classFound = .false.
   codeFound = .false.
   ! make sure this node is in our NLCD area
   if ( (m%xalbers(i).gt.xmin).and.(m%xalbers(i).lt.xmax).and. &
        (m%yalbers(i).gt.ymin).and.(m%yalbers(i).lt.ymax) ) then
      n_col = nint(abs((m%xalbers(i)-xmin)/res)) + 1 ! count columns from left
      n_row = nint(abs((ymax-m%yalbers(i))/res)) + 1 ! count rows from top
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
         write(6,*) 'xalbers=',m%xalbers(i),' yalbers=',m%yalbers(i)
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
allocate(diff(m%np))
! machine precision prevents us from simply checking whether the 
! mannings n .ne. the default value
diff = abs(manningsn(:) - defaultManningsN)
mnUnit = availableUnitNumber()
open(mnUnit,file=trim(outputfile),action='write',status='replace')
! write the number of nondefault values
write(mnUnit,'(A)') trim(nodal_attr_name)
write(mnUnit,*) count(diff.gt.1.e-6)
do i=1,m%np
   if (diff(i).gt.1.e-6) then
      write(mnUnit,*) i,manningsn(i)
   endif
end do
close(mnUnit)
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

!------------------------------------------------------------------
! surface_roughness.f90: Reads ADCIRC mesh file and land use data
! and produces surface roughness values.
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
program surface_roughness
use adcmesh
use ioutil
use logging
use landuse
implicit none
type(mesh_t) :: m
character(len=1024) :: outputfile
character(len=1024) :: nodal_attr_name = 'surface_directional_effective_roughness_length'
character(len=1024) :: canopyLookupTableName
logical :: genCanopy ! .true. if the user wants to generate canopy coefficient
! 
integer :: luclass   ! land use code at a particular mesh node
real, allocatable :: surfruf(:,:) ! (12,np) surface roughness length (m) at each mesh node in 12 directions
!
real(8) :: myTrigAngle     ! trig angle associated with each compass direction
real(8) :: myCos(12)       ! cosine of each trig angle
real(8) :: mySin(12)       ! sine of each trig angle
real(8) :: dist            ! distance (m) from mesh node along line of averaging 
real(8) :: avgDist         ! distance (m) upwind that values should be averaged
real(8) :: wtDist         ! weighting distance (m) for weighted averaging
real(8), allocatable :: std_weight_factor(:) ! expected distance weights along line of averaging
real(8), allocatable :: weight_factor(:) ! actual distance weights (some may be zeroed)
real(8), allocatable :: surf_rough_l(:) ! surface roughnesses along line of averaging
logical, allocatable :: areDefaultValues(:) ! (np) .true. if all the values along the line are zero
real, allocatable :: canopy(:) !(np) 0.0 if the canopy cuts off all wind stress
integer :: numPoints ! number of points along the line where wt average is computed
integer :: n_col  ! column number in the land cover data file 
integer :: n_row  ! row number in the land cover data file 
real(8) :: thisx, thisy ! albers coordinates of a point along a line for wt avg computation
integer :: chunk ! reporting 10% for progress bar
integer :: step  ! reporting 3.33% for progress bar
integer :: progress ! percent progress reading file
integer :: sfUnit
integer :: ccUnit
integer :: i, j, k, c
! new bounding box in albers equal area conic that takes into account 
!the averaging radius
real(8) avgxmax, avgymax, avgxmin, avgymin
!
avgDist = 10000.d0
wtDist = 3000.d0
genCanopy = .false.
call initLogging(availableUnitNumber(),'surface_roughness.f90')
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
   case("--roughness-lookuptable")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      lookupTableName = trim(cmdlinearg)
   case("--averaging-distance")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) avgDist
   case("--weighting-distance")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      read(cmdlinearg,*) wtDist
   case("--gen-canopy")
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      genCanopy = .true.
   case("--canopy-lookuptable")
      i = i + 1
      call getarg(i, cmdlinearg)
      write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
      canopyLookupTableName = trim(cmdlinearg)
   case default
      write(6,*) "WARNING: Command line option ",i," '",TRIM(cmdlineopt),"' was not recognized."
   end select
end do
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
allocate(surfruf(12,m%np))
surfruf = 0.d0                ! initialize all values to zero
!
! we use the same number of points in all directions, and all are
! equally spaced along the line where the weighted average is computed
! with spacing equal to the cell size of the land use data
numPoints = int(avgDist/res)  
allocate(std_weight_factor(numPoints))
allocate(weight_factor(numPoints))
allocate(surf_rough_l(numPoints))
!
! pre-compute the sin and cos so we don't have to do it over and over 
! inside the loop
!
! loop over the twelve compass directions, 1=north (0 deg on the compass)
! then CW (as the compass) but keeping in mind that trig functions expect 
! 0 degress to be to the right (east) and proceeding CCW
do j=1,12
   myTrigAngle = 90.0 - (j-1)*30.0
   myCos(j) = cos(myTrigAngle*deg2rad)
   mySin(j) = sin(myTrigAngle*deg2rad)
end do
! pre-compute the exp in the standard weighting factor 
! so we don't have to do it over and over inside the loop
do k=1,numPoints
   dist = res*dble(k-1)
   std_weight_factor(k) = exp(-dist**2/(2.d0*wtDist**2))
end do
! new bounding box that takes into account the size of the upwind
! weighted averaging window in albers equal area conic
avgxmin = xmin + avgDist
avgxmax = xmax - avgDist
avgymin = ymin + avgDist
avgymax = ymax - avgDist
! compute parameters related to printing a progress bar
progress=10
chunk=m%np/10
step=m%np/30
write(6,'(a)',advance='no') 'INFO: Computing directional surface roughness for each node in the mesh.'
do i=1,m%np
   ! update progress bar
   if (mod(i,chunk).eq.0) then
      write(6,'(i0,"%")',advance='no') progress
      progress = progress + 10
   elseif (mod(i,step).eq.0) then
      write(6,'(a1)',advance='no') '.'     
   endif
   ! make sure this node as well as the the area around it (used for averaging) 
   ! is fully inside our NLCD area
   if ( (m%xalbers(i).gt.avgxmin).and.(m%xalbers(i).lt.avgxmax).and. &
        (m%yalbers(i).gt.avgymin).and.(m%yalbers(i).lt.avgymax) ) then
      ! loop over the 12 directions
      do j=1,12
         ! loop over the points on the line where the weighted average
         ! is calculated; compute the land use row and column associated 
         ! with each point 
         do k=1,numPoints
            thisx = m%xalbers(i) + (k-1)*(myCos(j)*res)
            thisy = m%yalbers(i) + (k-1)*(mySin(j)*res)
            n_col = nint(abs((thisx-xmin)/res)) + 1 ! count columns from left
            n_row = nint(abs((ymax-thisy)/res)) + 1 ! count rows from top
            luclass = nlcd_class(n_row,n_col)
            ! if we encounter a "missing" value, just leave this point 
            ! out of the calculation and go to the next point along this line
            if ((luclass.eq.127).or.(luclass.eq.-9999)) then
               weight_factor(k) = 0.d0
               cycle
            endif
            ! use the lookup table to convert land use code 
            do c=1,nlc
               if (lookupTableKey(c).eq.luclass) then
                  surf_rough_l(k) = lookupTableValue(c)
                  weight_factor(k) = std_weight_factor(k)
                  exit
               endif
               ! if we got to here, we didn't jump out of the loop, and
               ! so we didn't find our land use value in the lookup table
               weight_factor(k) = 0.d0
            end do
         end do
         if (sum(weight_factor).ne.0.d0) then
            surfruf(j,i) = dot_product(weight_factor,surf_rough_l)/sum(weight_factor)
         endif
      end do
   else
      if (verbose.eqv..true.) then
         write(6,*) 'WARNING: Node ',i,' is outside the area covered by NLCD data.'
         write(6,*) 'xalbers=',m%xalbers(i),' yalbers=',m%yalbers(i)
      endif
   endif
end do
if (progress.eq.100) then
   write(6,'(i0)',advance='yes') progress,'.'
else 
   write(6,'(a1)',advance='yes') '.'
endif
write(6,*) 'INFO: Finished computing directional surface roughness for each node in the mesh.'
! write the nondefault values
write(6,*) 'INFO: Writing nondefault surface roughness values.'
! count the nondefault values
allocate(areDefaultValues(m%np))
areDefaultValues = .true.
do i=1,m%np  
   if (any(surfruf(:,i).ne.0.d0)) then
      areDefaultValues(i) = .false.
   endif
end do
sfUnit = availableUnitNumber()
open(sfUnit,file='surface_roughness.'//trim(outputfile),action='write',status='replace')
write(sfUnit,'(A)') trim(nodal_attr_name)
! write the number of nondefault values
write(sfUnit,*) count(areDefaultValues.eqv..false.)
do i=1,m%np
   if (areDefaultValues(i).eqv..false.) then
      write(sfUnit,130) i,(surfruf(j,i), j=10,1,-1),(surfruf(j,i), j=12,11,-1) 
   endif
end do
close(sfUnit)
write(6,*) 'INFO: Finished writing nondefault surface roughness values.'
130   format(i7,12(2x,f13.6))
!
! canopy coefficient, if requested
if (genCanopy.eqv..true.) then
   nodal_attr_name = 'surface_canopy_coefficient'
   write(6,*) 'INFO: Computing canopy coefficient values.'
   !
   ! load the specified lookup table
   lookupTableName = canopyLookupTableName
   call loadLookupTable()
   !
   allocate(canopy(m%np))
   canopy = 1
   do i=1,m%np
      ! make sure this node is inside our NLCD area
      if ( (m%xalbers(i).gt.xmin).and.(m%xalbers(i).lt.xmax).and. &
           (m%yalbers(i).gt.ymin).and.(m%yalbers(i).lt.ymax) ) then
         n_col = nint(abs((m%xalbers(i)-xmin)/res)) + 1 ! count columns from left
         n_row = nint(abs((ymax-m%yalbers(i))/res)) + 1 ! count rows from top
         luclass = nlcd_class(n_row,n_col)
         ! if we encounter a "missing" value, set it to the default of 1 
         if ((luclass.eq.127).or.(luclass.eq.-9999)) then
            canopy(i) = 1.0
         else
            do c=1,nlc
               if (lookupTableKey(c).eq.luclass) then
                  canopy(i) = lookupTableValue(c)
                  exit
               endif
               ! if we got to here, we didn't jump out of the loop, and
               ! so we didn't find our land use value in the lookup table
               canopy(i) = 1.0
            end do
         endif
      endif
   end do    
   write(6,*) 'INFO: Finished computing canopy coefficient values.'
   write(6,*) 'INFO: Writing canopy coefficient values.'
   ccUnit = availableUnitNumber()
   open(ccUnit,file='canopy.'//trim(outputfile),action='write',status='replace')
   write(ccUnit,'(A)') trim(nodal_attr_name)
   ! write the number of nondefault values
   write(ccUnit,*) count(canopy.eq.0.0)
   do i=1,m%np
      if (canopy(i).eq.0.0) then
         write(ccUnit,'(i0," ",f3.1)') i,canopy(i)
      endif
   end do
   close(ccUnit)
   write(6,*) 'INFO: Finished writing canopy coefficient values.'
endif

!-----------------------------------------------------------------------      
end program surface_roughness
!-----------------------------------------------------------------------

!SELECT CASE (nlcd_class)
!CASE (11) surf_rough_l = .001d0      ! Open Water
!CASE (12) surf_rough_l = .012d0      ! Perennial Ice/Snow
!CASE (21) surf_rough_l = .100d0      ! Developed - Open Space
!CASE (22) surf_rough_l = .300d0      ! Developed - Low Intensity
!CASE (23) surf_rough_l = .400d0      ! Developed - Medium Intensity
!CASE (24) surf_rough_l = .550d0      ! Developed - High Intensity
!CASE (31) surf_rough_l = .040d0      ! Barren Land (Rock/Sand/Clay)
!CASE (32) surf_rough_l = .090d0      ! Unconsolidated Shore
!CASE (41) surf_rough_l = .650d0      ! Deciduous Forest
!CASE (42) surf_rough_l = .720d0      ! Evergreen Forest
!CASE (43) surf_rough_l = .710d0      ! Mixed Forest
!CASE (51) surf_rough_l = .100d0      ! Dwarf Scrub
!CASE (52) surf_rough_l = .120d0      ! Shrub/Scrub
!CASE (71) surf_rough_l = .040d0      ! Grassland/Herbaceous
!CASE (72) surf_rough_l = .030d0      ! Sedge/Herbaceous
!CASE (73) surf_rough_l = .025d0      ! Lichens
!CASE (74) surf_rough_l = .020d0      ! Moss
!CASE (81) surf_rough_l = .060d0      ! Pasture/Hay
!CASE (82) surf_rough_l = .060d0      ! Cultivated Crops
!CASE (90) surf_rough_l = .550d0      ! Woody Wetlands
!CASE (91) surf_rough_l = .550d0      ! Palustrine Forested Wetland
!CASE (92) surf_rough_l = .120d0      ! Palustrine Scrub/Shrub Wetland
!CASE (93) surf_rough_l = .550d0      ! Estuarine Forested Wetland
!CASE (94) surf_rough_l = .120d0      ! Estuarine Scrub/Shrub Wetland
!CASE (95) surf_rough_l = .110d0      ! Emergent Herbaceous Wetlands
!CASE (96) surf_rough_l = .110d0      ! Palustrine Emergent Wetland (Persistent)
!CASE (97) surf_rough_l = .110d0      ! Estuarine  Emergent Wetland
!CASE (98) surf_rough_l = .030d0      ! Palustrine Aquatic Bed
!CASE (99) surf_rough_l = .030d0      ! Estuarine  Aquatic Bed
!CASE (127)   surf_rough_l = missing     ! Missing - usually water boundaries
!CASE (-9999) surf_rough_l = missing

!n_canopy=-9999
!if (nlcd_class.eq.11) n_canopy=1   !Open Water
!if (nlcd_class.eq.12) n_canopy=1   !Perennial Ice/Snow
!if (nlcd_class.eq.21) n_canopy=1   !Developed - Open Space
!if (nlcd_class.eq.22) n_canopy=1   !Developed - Low Intensity
!if (nlcd_class.eq.23) n_canopy=1   !Developed - Medium Intensity
!if (nlcd_class.eq.24) n_canopy=1   !Developed - High Intensity
!if (nlcd_class.eq.31) n_canopy=1   !Barren Land (Rock/Sand/Clay)
!if (nlcd_class.eq.32) n_canopy=1   !Unconsolidated Shore
!if (nlcd_class.eq.41) n_canopy=0   !Deciduous Forest
!if (nlcd_class.eq.42) n_canopy=0   !Evergreen Forest
!if (nlcd_class.eq.43) n_canopy=0   !Mixed Forest
!if (nlcd_class.eq.51) n_canopy=1   !Dwarf Scrub
!if (nlcd_class.eq.52) n_canopy=1   !Shrub/Scrub
!if (nlcd_class.eq.71) n_canopy=1   !Grassland/Herbaceous
!if (nlcd_class.eq.72) n_canopy=1   !Sedge/Herbaceous
!if (nlcd_class.eq.73) n_canopy=1   !Lichens
!if (nlcd_class.eq.74) n_canopy=1   !Moss
!if (nlcd_class.eq.81) n_canopy=1   !Pasture/Hay
!if (nlcd_class.eq.82) n_canopy=1   !Cultivated Crops
!if (nlcd_class.eq.90) n_canopy=0   !Woody Wetlands
!if (nlcd_class.eq.91) n_canopy=0   !Palustrine Forested Wetland
!if (nlcd_class.eq.92) n_canopy=0   !Palustrine Scrub/Shrib Wetland
!if (nlcd_class.eq.93) n_canopy=0   !Estuarine Forested Wetland
!if (nlcd_class.eq.94) n_canopy=1   !Estuarine Scrub/Shrub Wetland
!if (nlcd_class.eq.95) n_canopy=1   !Emergent Herbaceous Wetlands
!if (nlcd_class.eq.96) n_canopy=1   !Palustrine Emergent Wetland (Persistant)
!if (nlcd_class.eq.97) n_canopy=1   !Estuarine Emergent Wetland
!if (nlcd_class.eq.98) n_canopy=1   !Palustrine Aquatic Bed
!if (nlcd_class.eq.99) n_canopy=1   !Estuarine Aquatic Bed
!if (nlcd_class.eq.127) n_canopy=1  !No data class
!if (nlcd_class.eq.-9999) n_canopy=1  !No data class
!if (nlcd_class.eq.0) n_canopy=1  !No data class
!if (n_canopy.eq.-9999) then
!   write(*,*)"Undefined land cover class: ",nlcd_class
!endif

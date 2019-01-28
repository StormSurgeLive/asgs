!-----------------------------------------------------------------------
!               M O D U L E    L A N D  U S E
!-----------------------------------------------------------------------
! jgf: Module for handling land use data and lookup tables.
!
! Example grass script to convert binary NLCD img to ArcGrid ascii that
! is readable by this utility program
! set the boundaries of the subregion of interest (this is for southern louisiana)
! g.region n=1028000 s=715500 w=502000 e=1200000 -ba
! output from grass gis:
! north latitude:   32:11:53.090189N
! south latitude:   28:48:41.864475N
! west longitude:   90:49:37.812644W
! east longitude:   83:15:52.815807W
! center longitude: 87:02:45.314225W
! center latitude:  30:30:17.477332N
!
! this region is for south carolina
! g.region n=1428000 s=915500 w=1202000 e=1800000 -ba
! output from grass gis:
! north latitude:   35:08:55.385768N
! south latitude:   29:42:46.449034N
! west longitude:   83:24:03.761388W
! east longitude:   76:10:13.538411W
! center longitude: 79:47:08.6499W
! center latitude:  32:25:50.917401N
!
! get the boundaries as shell script variables
! eval `g.region -g`
! create the subregion image
! gdal_translate -projwin $w $n $e $s /home/jason/adcirc/util/fort13/nlcd2006_landcover_4-20-11_se5.img nlcd2006_cut.img
! import the subregion image
! r.in.gdal --overwrite nlcd2006_cut.img out=myLandUseLarge
! write out the subregion as ascii ArcGrid file
! r.out.arc --verbose input=myLandUseLarge output=myLandUseLargeAscii.grd
! 20180218: alternative
! r.out.gdal format=AAIGrid input=myLandUseLarge output=myLandUseLargeAscii.grd
! ^now have a file that can be processed for manning's n etc
!
! example of grass WMS general capabilities request
!r.in.wms mapserver="http://mapserver.flightgear.org/ms" -l
!
! example of gdalwarp:
!gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' raw_spot.tif utm11.tif
!
! example grass script to download imagery via WMS
!#set the region
!g.region n=4926990 s=4914840 w=591570 e=607800 res=30 -p
!# using r.in.wms to create RGB data to get a satellite coverage
!r.in.wms layers=global_mosaic mapserver=http://wms.jpl.nasa.gov/wms.cgi \
!        output=wms_global_mosaic
!# export the data to VTK
!r.out.vtk rgbmaps=wms_global_mosaic.red,wms_global_mosaic.green,wms_global_mosaic.blue \
!          elevation=elevation.10m output=/tmp/out.vtk
!
!# visualize in Paraview or other VTK viewer:
!paraview --data=/tmp/out.vtk
!
! example grass script to download land cover data via WMS
!g.region -p n=37N s=33N w=85W e=75W res=0:00:30
!r.in.wms output=nc_landcover mapserver="http://mapserver.flightgear.org/ms" \
!         layer=LANDCOVER format=png
!
! DRG from Terraserver download
!g.region res=1.2 -ap
!g.region save=drg-resolution
!r.in.wms output=terraserver-drg mapserver="http://terraserver.microsoft.com/ogcmap6.ashx" \
!         layers=DRG region=drg-resolution format=jpeg srs=EPSG:26910
!
!-----------------------------------------------------------------------
module landuse
!-----------------------------------------------------------------------
!
! variables associated with lookup tables
integer :: nlc ! number of rows in the lookup table
integer, allocatable :: lookupTableKey(:)
real, allocatable :: lookupTableValue(:)
character(len=1024) :: lookupTableName
logical :: lookupTableInitialized = .false. ! .true. if lookuptable was loaded previously
!
! variables associated with land use data
integer :: ncols  ! number of columns in the land cover data file
integer :: nrows  ! number of rows in the land cover data file
integer :: nodata ! land use code indicating no data at the location
real(8) :: res   ! land use data resolution (m)
real(8) :: xmin, xmax ! x land use data extent in albers equal area conic
real(8) :: ymin, ymax ! y land use data extent in albers equal area conic
integer, allocatable ::  nlcd_class(:,:) ! matrix of nlcd data
logical :: landUseInitialized = .false. ! .true. if land use data was loaded previously
character(len=1024) :: landUseFile
!
!---------
contains
!---------
!
!-----------------------------------------------------------------------
!      S U B R O U T I N E     L O A D  L A N D  U S E
!-----------------------------------------------------------------------
! jgf: Loads up a set of land use data and echoes the metadata to stdout.
!-----------------------------------------------------------------------
subroutine loadLandUse()
use logging
use ioutil, only : openFileForRead
implicit none
integer :: j, k
integer :: chunk ! reporting 10% for progress bar
integer :: step  ! reporting 3.33% for progress bar
integer :: progress ! percent progress reading file
character(len=10) :: c_ncols
character(len=10) :: c_nrows
character(len=10) :: c_xllcorner  
character(len=10) :: c_yllcorner            
character(len=10) :: c_cellsize            
character(len=10) :: c_nodata             
integer :: errorIO      
!
write(6,*) 'INFO: Reading land use metadata.'
call openFileForRead(13,landUseFile,errorIO)
read(13,*) c_ncols,ncols     ! e.g. ncols 23268
read(13,*) c_nrows,nrows     !      nrows 10416
read(13,*) c_xllcorner,xmin  !      xllcorner 502000
read(13,*) c_yllcorner,ymin  !      yllcorner 715500
read(13,*) c_cellsize,res    !      cellsize 29.998281
read(13,*) c_nodata,nodata   !      NODATA_value -9999
!
write(6,*) 'INFO: Land use metadata as follows:'
write(6,*) trim(c_ncols),' : ',ncols
write(6,*) trim(c_nrows),' : ',nrows
write(6,*) trim(c_xllcorner),' : ',xmin
write(6,*) trim(c_yllcorner),' : ',ymin
write(6,*) trim(c_cellsize),' : ',res
write(6,*) trim(c_nodata),' : ',nodata
!
xmax = xmin+(ncols-1)*res
ymax = ymin+(nrows-1)*res
write(6,*) 'INFO: Computed xmax=',xmax,' and ymax=',ymax,'.'
!
write(6,'(a)',advance='no') 'INFO: Reading land use data'
if (landUseInitialized.eqv..true.) then
   deallocate(nlcd_class)
endif
allocate(nlcd_class(nrows,ncols))
progress=10
chunk=nrows/10
step=nrows/30
do j=1,nrows
   read(13,*) (nlcd_class(j,k),k=1,ncols)
   ! write out the progress bar
   if (mod(j,chunk).eq.0) then
      write(6,'(i0,"%")',advance='no') progress
      progress = progress + 10
   elseif (mod(j,step).eq.0) then
      write(6,'(a1)',advance='no') '.'
   endif
enddo
if (progress.eq.100) then
   write(6,'(i0)',advance='yes') progress,'.'
else 
   write(6,'(a1)',advance='yes') '.'
endif
close(13)
landUseInitialized = .true.
write(6,*) 'INFO: Finished reading land use data file.'
!-----------------------------------------------------------------------
end subroutine loadLandUse
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     S U B R O U T I N E   L O A D  L O O K U P  T A B L E 
!-----------------------------------------------------------------------
! jgf: Loads a set of key/value pairs for land use data and checks it
! for duplicates. The key is expected to be an integer while the value
! is expected to be a real number. 
!-----------------------------------------------------------------------
subroutine loadLookupTable()
use logging
use ioutil, only : openFileForRead, availableUnitNumber
implicit none
integer :: i, j
integer :: ltUnit
integer :: errorIO
! 
write(6,*) 'INFO: Reading lookup table.'
ltUnit = availableUnitNumber()
call openFileForRead(ltUnit,lookupTableName,errorIO)
read(ltUnit,*) nlc ! total number of rows in the lookup table
if (lookupTableInitialized.eqv..true.) then
   deallocate(lookupTableKey)
   deallocate(lookupTableValue)
endif
allocate(lookupTableKey(nlc))
allocate(lookupTableValue(nlc))
do j=1,nlc
   read(ltUnit,*) lookupTableKey(j), lookupTableValue(j)
enddo
close(ltUnit)
write(6,*) 'INFO: Finished reading lookup table.'
write(6,*) 'INFO: Checking lookup table for duplicate values.'
! check the lookup table to be sure that each code is only listed once
do i=1,nlc
   do j=1,nlc
      ! of course the code matches itself; go to the next one
      if (i.eq.j) then
         cycle
      endif
      if (lookupTableKey(i).eq.lookupTableKey(j)) then
         write(6,*) 'ERROR: The key code ',lookupTableKey(i), &
         ' is listed twice in the lookup table in positions ',i,' and ',j,'.'
         error stop
      end if
   end do
end do
lookupTableInitialized = .true.          
write(6,*) 'INFO: Finished checking lookup table for duplicate keys.'
write(6,*) 'INFO: All lookup table keys are unique, as expected.'
!-----------------------------------------------------------------------
end subroutine loadLookupTable
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
end module landuse
!-----------------------------------------------------------------------

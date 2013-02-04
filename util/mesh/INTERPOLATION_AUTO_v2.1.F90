!  DESCRIPTION:
!  READS IN AN ADCIRC .14/.GRD AND A DEM (*.FLT FORMAT) AND
!  OUTPUTS A NEW FORT.14 WITH NODAL ELEVATIONS INTERPOLATED FROM THE DEM.
!
!   ****************************************************************************
!
!   ****************************************************************************
!   INTERPOLATION_AUTO VERSION 1.3 02/15/2012
!
!  v1.2 02/14/2012 - ADJUSTED TO NOT CRASH IF MESH NODES ARE NOT IN
!          NUMERICAL ORDER
!        - ADDED TRIGGER TO OUTPUT GRID OF MESH ELEMENT SIZES
!  v1.3 02/15/2012 - COMBINED READING OF INPUT FILE
!        - REMOVED AMBIGUOUS LINES OF CODE
!       v2.0 10/08/2012 - READ IN FLT/GRD FORMAT, RATHER THAN ASCII DEM
!       v2.1 01/30/2013 - ADDED IN COMMAND LINE ARGUMENTS
!
!   4 Feb 2013 - (Jason Fleming) Add comment here to show an example
!                of compiling this code using gfortran: 
!   gfortran -o bilskie_interp.x INTERPOLATION_AUTO_v2.1.F90
!
!   ****************************************************************************
!
!  INTERPOLATION_AUTO
!
!  DEVELOPED BY:
!                       
!  MATTHEW V. BILSKIE, E.I. (Matt.Bilske@gmail.com)
!       UNIVESITY OF CENTRAL FLORIDA
!  CHAMPS LAB (CHAMPSLAB.COM)
!
!     PLEASE EMAIL WITH QUESTIONS/BUGS
!
!   ****************************************************************************
!
!   CITATION (PLEASE CITE THE FOLLOWING PUBLICATION):
!
!   Matthew V. Bilskie, Scott C. Hagen, Topographic accuracy assessment of bare
!   earth lidar-derived unstructured meshes, Advances in Water Resources,
!   Volume 52, February 2013, Pages 165-177
!
!
!   ****************************************************************************
!
!     CODE DESCRIPTION: READS IN AN ADCIRC MESH FILE (.14/.GRD) AND AN ASCII
!  DEM FILE (*.FLT FORMAT) AND OUPUTS A NEW ADCIRC MESH FILE
!  WITH NODAL ELEVATION INTERPOLATED FROM THE DEM.
!  THE AUTOMATIC METHODS COMPUTES MESH ELEMENT SIZES AND AVERAGES DEMS CELLS
!  WITHIN 25% OF THE ELEMENT AREA FOR A GIVEN NODE. THIS METHOD IS EXPLAINED
!  IN: Matthew V. Bilskie, Scott C. Hagen, Topographic accuracy assessment 
!           of bare earth lidar-derived unstructured meshes, Advances in Water 
!           Resources, Volume 52, February 2013, Pages 165-177
!  
!       AUTO-AVERAGING IS RECOMMENDED FOR DEMS WITH CELL SIZE = 5 M.
!
!       ONLY ELEVATIONS EQUAL TO 0 IN ORIGINAL MESH WILL GET A NEW ELEVAITON VALUE
!     
!   **************************************************************************** 
!
!   ****************************************************************************
!  
!  PLANNED UPDATES
!  
!  1. ALLOW FOR GEOGRAPHICAL COORDINATES
!  2. OPTION TO REPLACE ALL NODES OR ONLY THOSE WHERE ELEV = 0.0
!     - USEFUL IF WET NODES INTERPOLATED FROM BATHY DATASET FIRST
!  3. RENUMBER MESH OR CHANGE CODE TO NOT CRASH IF MESH IS NOT RENUMBERED 
!     RATHER THAN JUST STOP AND TELL THE USER
!
!   ****************************************************************************

program Interpolation

!  **************************************************************************

   character*10 ndum    ! Dummy character
   character*60 inputfile     ! Main input file
   character*60 meshfile      ! Name of ADCIRC mesh - read from input file
   character*60 header     ! Header of meshfile
   character*60 DEMfile         ! Name of FLT/GRD DEM bindary *.flt - read from input file
   character*60 DEMfile_HDR        ! Name of FLT/GRD DEM HEADER *.hdr - read from input file
   character*60 newmeshfile   ! Name of output interpolated output mesh file - read from input file
   character*60 esize_meshfile   ! Name of output ADCIRC mesh file with z-values as avg. element size for each node

   logical found        ! boolean variable used in finding if required input files exist

   real, allocatable :: raster_elevation(:,:)   ! Matrix - Stores the raster DEM file in matrix form
   real, allocatable :: x_mesh(:)      ! Vector - x coordinate of mesh 
   real, allocatable :: y_mesh(:)      ! Vector - y coordinate of mesh
   real, allocatable :: z_mesh(:)      ! Vector - z coordinate of mesh
   real, allocatable :: min_dist(:) ! Value used in finding the closest DEM point to a node that is outside the DEM
   real, allocatable :: min_dist_z(:)  ! Z Value at a minimum distance from the variable above
   real, allocatable :: element_sizes(:)  ! Vector - Element size
   real, allocatable :: node_size(:)   ! Vector - Average element size for given node
   real, allocatable :: element_size(:) ! Vector - Element size for each element

   integer, allocatable :: NID(:)      ! Vector - Node ID of mesh nodes
   integer, allocatable :: EID(:)      ! Vector - Element ID of mesh elements
   integer, allocatable :: NHY(:)      ! Vector - NHY=3 for ADCIRC MESH
   integer, allocatable :: NM1(:)      ! Vector - Connecting nodes for element
   integer, allocatable :: NM2(:)
   integer, allocatable :: NM3(:)
   integer, allocatable :: node_count(:)  ! Vector - Keeps track of # of time nodes used to create an element
   integer, allocatable :: node_elem(:,:)  ! Matrix - row = node, col = all elements that the node is used in

   integer mesh_size_trig        ! Trigger to output mesh element grid - read from input file
   integer NCOLS,NROWS        ! Columns and Rows of input raster
   integer NP,NE           ! # of nodes and elements in ADCIRC Mesh
   integer in_points       ! Counter for number of mesh nodes within the DEM .ne. no data value
   integer no_data_count         ! Counter of points falling within a DEM cell that equal no data value
   integer temp_row,temp_col
   integer curNodeID,count,cell_dist
   integer screenOutput       ! 0=no screen output, 1=screen ouput; when computing node elevations from DEM - read from input file
   integer auto_cell_dist        ! -1=automatic averaging method, 0=direct lookup, > 0 = brute force averaging - read from input file
   integer neighbors                       ! Paramter for max. number of elements connected to one node
   integer minsize_count
   integer min_element
   integer i,j

   real mult_fac            ! Multiplication factor to convert elevation coordinates (i.e. m to ft or flip elev. by -1) - read from input file
   real x_in,y_in,z_in        ! Current mesh node x, y, and z value - Used for simpliar equations within loop
   real x_toplt,y_toplt,x_botrt,y_botrt   ! Coordinates of the current DEM cell corners
   real x_y_dist           ! Cell size read from raster file
   real nodata_value       ! No data file read from raster (typically = -99999)
   real x_inc,y_inc        ! DEM cell size in x and y directions.  Equal for a rectangular DEM cell
   real rast_z           ! DEM cell value for current node                        ! NOT USED?
   real P_x,P_y            ! Coordinates of the current DEM cell center
   real TL_Z,TR_Z,BL_Z,BR_Z      ! Z value of the 4 surrounding DEM cells during interpolation (TL=top left, etc.)
   real z_average               ! Average z value of neighboring DEM cells
   real z_check            ! Used in checking nodata value for DEM cell
   real dist            ! Distance between 2 nodes (returned from calcDistance subroutine)
   real d1d2,d1d3,d2d3        ! Distance between nodes (i.e. d1d2=b/w nodes 1 and 2)

   character(len=60) :: temparg
   character(len=60) :: VERSION = 'VERSION 2.1'
   integer :: iargc

!  LOAD INPUT FILE AND READ VALUES
   found = .false.
   if (command_argument_count().gt.0) then   ! also use IARGC()
      i = 0
      do while (i < command_argument_count() ) 
         i = i + 1
         call get_command_argument(i,temparg)  ! also use get_arg(i,temparg)
         if ((temparg(1:2).eq."-I").or.(temparg(1:2).eq."-i")) then
            found = .true.
            i = i + 1
            call get_command_argument(i,temparg)
            read(temparg,*) inputfile
         elseif((temparg(1:2).eq."-V").or.(temparg(1:2).eq."-v")) then
            write(*,*)VERSION
            stop
         elseif(temparg(1:2).eq."-h") then
            write(*,*)'See header information in FORTRAN file'
            write(*,*)
            stop
         endif
      enddo
    endif

!   ****************************************************************************
   write(*,*)''
   write(*,*)'******************************************************'
   write(*,*)''
   write(*,*)'               -INTERPOLATION_AUTO-                   '
   write(*,*)'  PROGRAM TO INTERPOLATE A FLT/GRD RASTER DEM TO      '
   write(*,*)'                   ADCIRC MESH NODES                  '
   write(*,*)''
   write(*,*)'                MATTHEW V. BILSKIE, E.I.              '
   write(*,*)'                Matt.Bilskie@gmail.com                '
   write(*,*)'                 BILSKIE & HAGEN, 2013                '
   write(*,*)'' 
   write(*,*)'GO KNIGHTS!'
   write(*,*)'******************************************************'
   write(*,*)''
!     **************************************************************************
       
   if (found.eqv..false.) then
       write(*,*)'Name of input file: '
       read(*,*)inputfile
    endif

   inquire(file=inputfile,exist=found) ! Checks to see if input file exists
   if (found) then
       write(*,1011)inputfile
       open(100,file=inputfile,status='old')
   else
       write(*,1010)inputfile
       stop
   endif
   read(100,*)
   read(100,*)

!  Reads name of mesh from input file.
   read(100,*)meshfile
   inquire(file=meshfile,exist=found)
   if (found) then
       write(*,1011)meshfile
       open(14,file=meshfile,status='old') 
   else
       write(*,1010)meshfile
       stop
   endif

   read(100,*)mesh_size_trig
   read(100,*)esize_meshfile

!  Load the DEM file and read the header info.
   read(100,*) DEMfile
   DEMfile_HDR=TRIM(DEMfile)//".hdr"
   DEMfile=TRIM(DEMfile)//".flt"
   inquire(file=DEMfile,exist=found)
   if (found) then
       write(*,1011)DEMfile
       open(unit=101,file=DEMfile,status='old',form='unformatted',access='stream')
   else
      write(*,1010)DEMfile
      stop
   endif
      inquire(file=DEMfile_HDR,exist=found)
   if (found) then
      write(*,1011)DEMfile_HDR
      open(unit=111,file=DEMfile_HDR,status='old')
   else
      write(*,1010)DEMfile_HDR
      stop
   endif

   if(mesh_size_trig.eq.0)then
      write(*,*)'ELEMENT SIZE MESH WILL NOT BE CREATED'
      write(*,*)
   else
      write(*,*)esize_meshfile,' WILL BE CREATED'
      write(*,*)
   endif

!  Read remaining parameters
   read(100,*)mult_fac
   read(100,*)cell_dist
   read(100,*)screenOutput
   read(100,*)newmeshfile

   write(*,1012)mult_fac
   if (cell_dist.eq.0) write(*,*)'DIRECT LOOKUP WILL BE USED'
   if (cell_dist.gt.0) write(*,1013)((2*cell_dist)+1)**2
   if (cell_dist.eq.-1) then
      write(*,*)'AUTOMATIC CELL AVERAGING WILL BE USED'
   endif
   if (screenOutput.eq.0) then
      write(*,*)'SCREEN OUTPUT IS SUPRESSED'
      write(*,*)
   else
      write(*,*)'ALL SCREEN OUTPUT IS DISPLAYED'
      write(*,*)
   endif
   open(24,file=newmeshfile,status='unknown')
   write(*,*)'NEW MESH FILE: ',newmeshfile
   write(*,*)
!     **************************************************************************
!  Read ADCIRC mesh file
   read(14,100)header
   read(14,*)NE,NP

   neighbors = 50
        
   write(*,32)NP
   write(*,33)NE

   allocate ( x_mesh(NP) )
   allocate ( y_mesh(NP) )
   allocate ( z_mesh(NP) )
   allocate ( NID(NP) )
   allocate ( EID(NE) )
   allocate ( NHY(NE) )
   allocate ( NM1(NE) )
   allocate ( NM2(NE) )
   allocate ( NM3(NE) )
   allocate ( min_dist(3) )
   allocate ( min_dist_z(3) )
   allocate ( node_size(NP) )
   allocate ( node_count(NP))
   allocate ( node_elem(NP,neighbors))
   allocate ( element_size(NE))

   do i=1,NP
       read(14,*)NID(i),x_mesh(i),y_mesh(i),z_mesh(i)
   enddo
   do i=1,NE
       read(14,*)EID(i),NHY(i),NM1(i),NM2(i),NM3(i)
   enddo
   close(14)
   write(*,*)'NODES/ELEMENTS PROCESSED'
   write(*,*)
!     ************************************************************************** 
!  Read header info from *.hdr & allocate memory for DEM matrix storage
!       Similar to reading NLCD files - Crystal Fulcher's Mannings_n_finder
   read(111,*) ndum, NCOLS
   read(111,*) ndum, NROWS
   read(111,*) ndum, x_botlt
   read(111,*) ndum, y_botlt
   read(111,*) ndum, x_y_dist
   read(111,*) ndum, nodata_value
   allocate ( raster_elevation(NROWS,NCOLS) )
        
!  Compute the corners of the raster bounds and store raster into matrix

!  This area could use some upgrades in terms of allowing multiple smaller
!  raster files that are faster to store and easier to search

   x_inc = x_y_dist
   y_inc = x_y_dist

   x_botrt=x_botlt+(NCOLS)*x_inc
   y_botrt=y_botlt
   x_toplt=x_botlt
   y_toplt=y_botlt+(NROWS)*y_inc
     
   write(*,30)NROWS
   write(*,31)NCOLS
     
   write(*,*)'STORING RASTER INTO MATRIX...'
   do i=1,NROWS
      read(101) (raster_elevation(i,j),j=1,NCOLS)
   enddo
   write(*,*)'STORING RASTER COMPLETE'
   write(*,*)''
   close(101)
   
!     **************************************************************************
!     **************************************************************************
!  Compute average element size and assign to nodes if AUTOMATIC method is chosen

!  This section could use some efficiency upgrades

     minsize_count=0
     min_element=100000000.0

     if (cell_dist.eq.-1) then
         write(*,*)'COMPUTING ELEMENT SIZES...'
         do i=1,NE
            node_count(NM1(i))=node_count(NM1(i))+1
            node_count(NM2(i))=node_count(NM2(i))+1
            node_count(NM3(i))=node_count(NM3(i))+1
 
            node_elem(NM1(i),node_count(NM1(i)))=i
            node_elem(NM2(i),node_count(NM2(i)))=i
            node_elem(NM3(i),node_count(NM3(i)))=i
         enddo ! End i loop

         do i=1,NE
            call calcDistance(x_mesh(NM1(i)),y_mesh(NM1(i)),x_mesh(NM2(i)),y_mesh(NM2(i)),dist)
            d1d2=dist
             
            call calcDistance(x_mesh(NM1(i)),y_mesh(NM1(i)),x_mesh(NM3(i)),y_mesh(NM3(i)),dist)
            d1d3=dist
             
            call calcDistance(x_mesh(NM2(i)),y_mesh(NM2(i)),x_mesh(NM3(i)),y_mesh(NM3(i)),dist)
            d2d3=dist
             
            size=min(d1d2,d1d3,d2d3)
            element_size(i)=(d1d2+d2d3+d1d3)/3 ! Average edge length for element i
         enddo ! End i loop

         ! Average all element sizes connected to node i and store in node_size(i)
         do i=1,NP
             do j=1,node_count(i)
                 node_size(i) = node_size(i) + element_size(node_elem(i,j))
             enddo
             node_size(i)=node_size(i)/node_count(i)
         enddo
         write(*,*)'ELEMENT SIZE VECTOR COMPLETE'
      write(*,*)
      if (mesh_size_trig.eq.1) then
         open(199,file=esize_meshfile)
         write(199,*)header
         write(199,*)NE,NP
         do i=1,NP
            write(199,120)NID(i),x_mesh(i),y_mesh(i),node_size(i)
         enddo
         do i=1,NE
            write(199,130)EID(i),NHY(i),NM1(i),NM2(i),NM3(i)
         enddo
         close(199)
         write(*,1014)esize_meshfile
         write(*,*)
      endif
   endif

!     **************************************************************************  

!  Main Portion of the program

   write(24,100)header
   write(24,110)NE,NP
   write(*,*)'ASSIGNING MESH NODE ELEVATIONS...'
   !open(121,file='cell_distance.txt')
   do i=1,NP
   
      if (z_mesh(i).ne.0) GOTO 6000
   
         x_in=x_mesh(NID(i))
         y_in=y_mesh(NID(i))

!      **********************************************************************
!      Checks to see if the points lie in the raster bounds

         if ( (x_in.gt.x_toplt) .and. (x_in.lt.x_botrt) .and. (y_in.gt.y_botrt) .and. (y_in.lt.y_toplt) ) then
            x_diff=x_toplt-x_in
            y_diff=y_toplt-y_in
                              
            n_col=abs(x_diff/x_inc)
            col=abs(x_diff/x_inc)
            decimal=col-n_col
            if (decimal.lt.0.5) n_col=n_col+1
            if (decimal.gt.0.5) n_col=n_col+1
            if (n_col.eq.0) n_col=1
            if (n_row.eq.0) n_row=1
                              
            n_row=abs(y_diff/y_inc)
            row=abs(y_diff/y_inc)
            decimal=row-n_row
            if (decimal.lt.0.5) n_row=n_row+1
            if (decimal.gt.0.5) n_row=n_row+1
               
            rast_z=raster_elevation(n_row,n_col)

      !     ******************************************************************
      !     Checks to see if the current DEM contains a no data value   
            count=0  
            z_average=0.      
            if (rast_z.ne.nodata_value) then
      !         Node is inside raster and has value not equal nodatavalue
               in_points=in_points+1 

!         **************************************************************
!              Checks what interpolation option is chosen            

               if (cell_dist.eq.0) then
                  ! Means a direct lookup will be used
                  z_mesh(i)=rast_z*mult_fac
               elseif (cell_dist.eq.-1) then
!                  An automatic cell averaging scheme based on 25% element size is used
                  auto_cell_dist=ceiling((0.25*node_size(i))/x_inc)
                  !write(*,*)auto_cell_dist
                  !write(121,*)i,auto_cell_dist
            
               if (auto_cell_dist.lt.1) then
                  z_mesh(i)=rast_z*mult_fac
                  GOTO 5000
               endif 
   
               do k=n_row-auto_cell_dist,n_row+auto_cell_dist
                  do l=n_col-auto_cell_dist,n_col+auto_cell_dist
                  if ((k.gt.auto_cell_dist).and.(k.lt.NROWS))then ! If cell window falls outside of the DEM
                     if ((l.gt.auto_cell_dist).and.(l.lt.NCOLS)) then
                        z_check=raster_elevation(k,l)
                        if (z_check.ne.nodata_value) then
                           !write(*,*)i,z_check 
                           z_average=z_average+raster_elevation(k,l)
                           count=count+1
                        endif
                     endif
                  endif
               enddo
            enddo

            z_average=z_average/count
            z_mesh(i)=z_average*mult_fac
            !write(*,*)i,z_average
5000        continue
            z_average=0.
            count=0
            
         else
!            A cell averaging scheme will be used based on cell_dist
!            Find each DEM cell around the node

            do k=n_row-cell_dist,n_row+cell_dist
               do l=n_col-cell_dist,n_col+cell_dist
                  if ((k.gt.cell_dist).and.(k.lt.NROWS))then
                     if ((l.gt.cell_dist).and.(l.lt.NCOLS)) then
                        z_check=raster_elevation(k,l)
                        if (z_check.ne.nodata_value) then
                           z_average=z_average+raster_elevation(k,l)
                           count=count+1
                        endif
                     endif
                  endif
               enddo
            enddo

            z_average=z_average/count
            z_mesh(i)=z_average*mult_fac
            z_average=0.
            count=0.

         endif
!         **************************************************************
            
      else ! Node has a nodata value
         no_data_count=no_data_count+1
         z_mesh(i)=0.
      endif
   else ! Node is outside the DEM
      no_data_count=no_data_count+1
      z_mesh(i)=0.
   endif
!     **********************************************************************
6000  continue
   if (screenOutput.eq.1) write(*,*)'Processed node',NID(i),'/',NP
!      write node table with new elevations to newmeshfile
   write(24,120)NID(i),x_mesh(i),y_mesh(i),z_mesh(i)
   enddo
   write(*,*)'FINISHED ASSINGING NODE ELEVATIONS'
   write(*,*)
   !close(121)
!     **************************************************************************
!  Write out the element table for the new mesh file with interpolated z values from DEM.

   !open(24,file=newmeshfile,status='unknown')
   do i=1,NE
       write(24,130)EID(i),NHY(i),NM1(i),NM2(i),NM3(i)
   enddo

   write(*,1014)newmeshfile
   close(24)
!     **************************************************************************

20    format(I9,I3,I7,I7,I7)
30    format("NUMBER OF ROWS ",I11)
31    format("NUMBER OF COLUMNS ",I11,/)
32    format("NUMBER OF NODES ",I11)
33    format("NUMBER OF ELEMENTS ",I11,/)
100   format(a60)
110   format(I9,2x,I9)
120   format(2x,I9,2x,f17.10,2x,f18.10,2x,f16.10)
130   format(2x,I9,2x,I1,2x,I9,2x,I9,2x,I9)

1010  format(' File ',a60,/,'WAS NOT FOUND!  PROGRAM TERMINATING',/)
1011  format(' File ',a60,/,'WAS FOUND!   Opening & Processing file',/)
1012  format(' MULTIPLICATION FACTOR ',f7.5,x,'EMPLOYED')
1013  format(' THE CELL AREA USED IS: ', i2)
1014    format(a60,/,'WRITTEN!',/)

   End program INTERPOLATION
!   ****************************************************************************

!   ****************************************************************************

!  Computes the distances between two nodes and returns dist.
   subroutine calcDistance(x_cor1,y_cor1,x_cor2,y_cor2,dist)
   implicit none
   real x_cor1,y_cor1,x_cor2,y_cor2
   real dist

   dist=(x_cor2-x_cor1)*(x_cor2-x_cor1)
   dist=dist+( (y_cor2-y_cor1)*(y_cor2-y_cor1) )
   dist=SQRT(dist)

   return
   end
!   ****************************************************************************


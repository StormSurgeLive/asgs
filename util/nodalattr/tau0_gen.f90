!   Robert Weaver
!   Oct 31, 2008 
!
! this program will generate data for tau0 base to be used in the fort.13 file
! the data is formatted to be read in when the tau0 flag is set to -3.0 in the fort.15 file 
! for versions fo ADCIRC later than 47.31 and older versions of the code
! which may have been modified seperately from that kept on the svn repository
! For these versions, setting the flag of tau0 = -3.0 implies that a base value for tau0 
! will be read in from the fort.13 file.   

! For tau0 = -3  we need only one value for tau0 ( a min or base value) in the fort.13 file, 
! the variable is *primitive_weighting_in_continuity_equation*

! The simple Fortran program makes use of a subroutine from the adcirc source code, 
! to find the neighboring elements.  If the average distance between a node and its
! neighbors is less than or equal to a critical value (currently set to 1750 m) 
! then tau0 base will be set to the time varying flag value of 0.03.

! Otherwise the base value is depth dependant:
! 0.005 for depths greater than 10m
! 0.02  for depths less than 10m

! in order to keep the file size down, this program sets the default
! value to 0.03 and only writes out the points where the distance 
! between nodes is greater than the critical distance

      ! Taylor Asher: Added command line options to support automation.
      !
      ! Jason Fleming, May 2013: Added the include for adcmesh.f90; moved the 
      ! computation of the neighbor table and cpp projection to the
      ! mesh module in adcmesh.f90. Added more command line options for
      ! greater control of tau0 settings.

      INCLUDE 'adcmesh.f90'
      PROGRAM tau0_gen
      USE ADCMESH
      IMPLICIT NONE
      character(1024) :: nodal_attr_name 
      double precision, allocatable ::  dx_avg(:)  !(np)
      double precision, allocatable ::  tau0_min(:) !(nodes)
      double precision :: dx_crit = 1750.d0
      double precision :: h_break = 10.d0 
      double precision :: tau0_default = 0.030d0
      double precision :: tau0_break = 0.02d0
      double precision :: tau0_deep = 0.005d0
      !TGA-BEGIN:  Added for fort.13 generation automation
      character*90 gridfile,outputfile
      CALL GETARG(1,gridfile)
      call getarg(2,outputfile)
      !CTGA-END:  Added for fort.13 generation automation
      ! initializations
      nodal_attr_name = 'primitive_weighting_in_continuity_equation'
      !
      ! Load fort.14
      call read14()
      CALL computeNeighborTable()
      !
      ! compute the distance from every node to each of its neighbors and then
      ! divide by the total number of neighbors for that node
      allocate(dx_avg(np))
      dx_avg(:) = 0.0d0
      DO N=1, NP
         DO J=2, NNEIGH(N)
          dx_avg(N) = dx_avg(N) + sqrt( (x_cpp(NeiTab(N,1))-x_cpp(NeiTab(N,J)))**2 &
                  + (y_cpp(NeiTab(N,1))-y_cpp(NeiTab(N,J)))**2 )
          ENDDO
          dx_avg(N) = dx_avg(N)/NNEIGH(N)
      ENDDO
      !
      ! now we have average dx for each node, test to see if greater than dx_crit
      ! and write fort.13 accordingly
      allocate(tau0_min(np))
      tau0_min(:) = 0.d0
      ! where the mesh spacing is greater or equal to the critical value 
      where ( dx_avg.ge.dx_crit) 
         ! where the depth is also less than the break depth 
         where ( xyd(3,:).le.h_break )
            tau0_min = tau0_crit
         elsewhere
            ! where the depth is greater than the break depth
            tau0_min = tau0_deep
         end where
      elsewhere
         ! where mesh spacing is less than the critical value
         tau0_min = tau0_default
      end where
      
      ! write output in fort.13 format for one attribute
      open(unit=13,file=outputfile,status='unknown')
      write(13,'(A)') trim(nodal_attr_name)
      write(13,'(I8)') count(tau0_min.ne.tau0_default)
      do n=1, np
         if ( tau0_min.ne.tau0_default ) then
            write(13,'(i8,f13.6)') n, tau0_min(n)
         endif
      end do
      close(13)
!-------------------------------------------------------------------
      end program tau0_gen
!-------------------------------------------------------------------

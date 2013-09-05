!--------------------------------------------------------------------------
! checkAdcircMesh.f90
!
! A program to find ADCIRC mesh boundaries that are only two nodes long. 
!
!--------------------------------------------------------------------------
! Copyright(C) 2013 Jason Fleming
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
!-----------------------------------------------------------------------

! Example of how to compile with gfortran:
! gfortran -o checkAdcircMesh.x -ffree-form -ffree-line-length-none -I/usr/include checkAdcircMesh.f90 -lnetcdf -lnetcdff -lz
!
! Example for compiling with ifort on croatan at RENCI:
! ifort -o checkAdcircMesh.x -i-dynamic -cpp -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/projects/ncfs/apps/croatan/netcdf/include checkAdcircMesh.f90 -L/projects/ncfs/apps/croatan/netcdf/lib -lnetcdf -lnetcdff -lz
!
include 'adcmesh.f90'
!-----+---------+---------+---------+---------+---------+---------+
!
!   P R O G R A M    C H E C K   A D C I R C   M E S H
!
!-----+---------+---------+---------+---------+---------+---------+
   program checkAdcircMesh

      USE adcmesh
      USE netcdf
      IMPLICIT NONE
      integer         :: argcount  ! number of command line arguments
      integer         :: iargc     ! function to return command line arguments
      character(2048) :: cmdlineopt ! command line option
      character(2048) :: cmdlinearg ! content of command line argument
      integer :: i, j
      logical :: shortBoundary
      
      ! initializations
      shortBoundary = .false.
      !
      ! Report netcdf version
      write(6,*) "INFO: checkAdcircMesh was compiled with the following netcdf library: ", &
         trim(nf90_inq_libvers())

      ! Process command line options
      argcount = iargc() ! count up command line options
      if (argcount.gt.0) then
         i=0
         do while (i.lt.argcount)
            i = i + 1
            call getarg(i, cmdlineopt)
            select case(trim(cmdlineopt))
               case("--meshfile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  meshFileName = trim(cmdlinearg)
               case default
                  write(6,*) "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
            end select
         end do
      end if    
      !
      ! Load fort.14
      call read14()
      !
      ! check to see if any boundary is two nodes long, and if so, 
      ! output the data for that boundary
      do i=1, nope
         if ( nvdll(i).eq.2 ) then
            shortBoundary = .true.
            write(6,'(A,I0,A)') "WARNING: The mesh file '" // trim(meshFileName) // &
               "' has an open boundary (number ",i,") that consists of only 2 nodes:"
            write(6,'(I0,A)') nvdll(i)," ! nvdll, number of nodes on the boundary"
            do j=1, nvdll(i)
               write(6,'(I0,A)') nbdv(i,j)," ! nbdv, node number"
            end do
         end if 
      end do
      do i=1, nbou
         if ( nvell(i).eq.2 ) then
            shortBoundary = .true.
            write(6,'(A,I0,A)') "WARNING: The mesh file '" // trim(meshFileName) // &
               "' has a flux boundary (number ",i,") that consists of only 2 nodes:"
            write(6,'(I0,A,I0,A)') nvell(i),"   ",ibtype(i), &
               " ! nvell  ibtype : number of nodes on the boundary and the boundary type"
            do j=1, nvell(i)
               write(6,'(I0,A)') nbvv(i,j)," ! nbvv: node number"
            end do
         end if
      end do
      if ( shortBoundary.eqv..false. ) then
         write(6,'(A)') "INFO: All boundaries were longer than two nodes."      
      endif
      write(6,'(A)') "INFO: Finished checking boundary information."
!----------------------------------------------------------------------
   end program checkAdcircMesh
!----------------------------------------------------------------------

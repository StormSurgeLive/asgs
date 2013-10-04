!--------------------------------------------------------------------------
! adcircFluxBC.f90
!
! A program to calculate flux per unit width on flux boundaries, based
! on a given flux value and a mesh with boundaries of the associated 
! type.
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
! gfortran -o adcircFluxBC.x -ffree-form -ffree-line-length-none -I/usr/include adcircFluxBC.f90 -lnetcdf -lnetcdff -lz
!
! Compiling with pgf90 on garnet at ERDC 20130926:
! pgf90 -o adcircFluxBC.x -Mpreprocess -DHAVE_NETCDF4 -DNETCDF_CAN_DEFLATE -I/opt/cray/netcdf/4.3.0/pgi/121/include adcircFluxBC.f90 -L/opt/cray/netcdf/4.3.0/pgi/121/lib -lnetcdf -lnetcdff

include 'adcmesh.f90'
!-----+---------+---------+---------+---------+---------+---------+
!
!   P R O G R A M    A D C I R C  F L U X   B  C 
!
!-----+---------+---------+---------+---------+---------+---------+
   program adcircFluxBC

      USE adcmesh
      USE netcdf
      IMPLICIT NONE
      CHARACTER(1000) :: meshFile, boundaryFile
      character(1000) :: line, comment
      integer         :: argcount  ! number of command line arguments
      integer         :: iargc     ! function to return command line arguments
      character(2048) :: cmdlineopt ! command line option
      character(2048) :: cmdlinearg ! content of command line argument
      real(8) :: prevdist
      real(8) :: nextdist
      integer :: btype
      integer :: bindex
      integer :: boundaryIndex
      integer :: nodeindex
      integer :: boundaryNodes
      integer :: numBoundaries
      integer :: thisNodeNum
      integer :: prevNodeNum
      integer :: thisBoundaryNum
      integer :: nextNodeNum
      integer, allocatable :: boundaryIndices(:)
      real(8), allocatable :: dist(:)
      real(8), allocatable :: dep(:)
      real(8), allocatable :: sumDepths(:)
      real(8), allocatable :: sumLengths(:)
      integer :: i, j
      
      ! initializations

      meshFile = "null"
      boundaryFile = "null"
      slam0 = -60.0d0 ! 60 deg west longitude
      sfea0 = 35.d0   ! 35 deg north latitude
      btype = 52
      !
      ! Report netcdf version
      write(6,*) "INFO: adcircFluxBC was compiled with the following netcdf library: ", &
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
               case("--boundary-type")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  read(cmdlinearg,*) btype 
                  write(6,*) "INFO: The boundary type is ",btype,"."
               case("--cpp")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  read(cmdlinearg,*) slam0
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  read(cmdlinearg,*) sfea0
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",slam0," ",sfea0,"."
               case("--boundaryfile")
                  i = i + 1
                  call getarg(i, cmdlinearg)
                  write(6,*) "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
                  boundaryFile = trim(cmdlinearg)                     
               case default
                  write(6,*) "WARNING: Command line option '",TRIM(cmdlineopt),"' was not recognized."
            end select
         end do
      end if    
      !
      ! Load fort.14
      call read14()
      ! 
      ! count how many boundaries we have of the specified type
      numBoundaries = count(ibtype.eq.btype)
      if ( numBoundaries.eq.0 ) then
         write(6,*) "ERROR: Type ",btype," boundaries were not found in the mesh file."
         stop
      endif
      allocate(boundaryIndices(numBoundaries)) ! index into the nvell array
      allocate(sumDepths(numBoundaries))
      allocate(sumLengths(numBoundaries))      
      bindex = 1
      boundaryNodes = 0
      do i=1, nbou
         if ( ibtype(i).eq.btype ) then
            boundaryIndices(bindex) = i
            bindex = bindex + 1
            boundaryNodes = boundaryNodes + nvell(i)
         endif
      enddo
      allocate(dist(boundaryNodes))
      allocate(dep(boundaryNodes))
      write(6,'("INFO: There are ",I3," boundaries of type ",I3,".")') numBoundaries, btype
      !
      ! now find the location of the boundary nodes, how far apart they
      ! are, and the depth at each one, for use in calculating the flux
      ! per unit width
      write(6,*) "INFO: The center of the projection is lat=",slam0," lon=",sfea0,"."
      call computeCPP() ! get the mesh coords in the same sys adcirc uses
      nodeindex=1
      do i=1,numBoundaries
         thisBoundaryNum = boundaryIndices(i)
         ! handle first node; use only half the distance to the next node
         thisNodeNum = nbvv(thisBoundaryNum,1)
         nextNodeNum = nbvv(thisBoundaryNum,2)
         dist(nodeindex) = 0.5d0 &
            * sqrt( (x_cpp(nextNodeNum)-x_cpp(thisNodeNum))**2 + &
                     (y_cpp(nextNodeNum)-y_cpp(thisNodeNum))**2 )
         dep(nodeindex) = 0.5d0 * xyd(3,thisNodeNum) ! use only half the depth
         sumDepths(i) = sumDepths(i) + dep(nodeindex) ! accumulate total depth
         sumLengths(i) = sumLengths(i) + dist(nodeindex) ! accumulate total length
         nodeindex = nodeindex + 1
         do j=2, nvell(thisBoundaryNum)-1
            prevNodeNum = nbvv(thisBoundaryNum,j-1)
            thisNodeNum = nbvv(thisBoundaryNum,j)
            nextNodeNum = nbvv(thisBoundaryNum,j+1)
            prevdist = sqrt( (x_cpp(thisNodeNum)-x_cpp(prevNodeNum))**2 + &
                              (y_cpp(thisNodeNum)-y_cpp(prevNodeNum))**2 )
            nextdist = sqrt( (x_cpp(nextNodeNum)-x_cpp(thisNodeNum))**2 + &
                              (y_cpp(nextNodeNum)-y_cpp(thisNodeNum))**2 )
            dist(nodeindex) = 0.5d0 * prevdist + 0.5d0 * nextdist
            dep(nodeindex) = xyd(3,thisNodeNum) ! use the whole depth here
            sumDepths(i) = sumDepths(i) + dep(nodeindex) ! accumulate total depth
            sumLengths(i) = sumLengths(i) + dist(nodeindex) ! accumulate total length
            nodeindex = nodeindex + 1
         end do
         ! handle last node; use only half the distance from the previous node
         thisNodeNum = nbvv(thisBoundaryNum,nvell(thisBoundaryNum))
         prevNodeNum = nbvv(thisBoundaryNum,nvell(thisBoundaryNum)-1)
         dist(nodeindex) = 0.5d0 * sqrt( (x_cpp(thisNodeNum)-x_cpp(prevNodeNum))**2 + &
                           (y_cpp(thisNodeNum)-y_cpp(prevNodeNum))**2 )
         dep(nodeindex) = 0.5d0 * xyd(3,thisNodeNum) ! use only half the depth
         sumDepths(i) = sumDepths(i) + dep(nodeindex) ! accumulate total depth
         sumLengths(i) = sumLengths(i) + dist(nodeindex) ! accumulate total length
         nodeindex = nodeindex + 1
      end do
      !
      ! open the output file where we will write our boundary data
      open(file=trim(boundaryFile),unit=12,action='write',status='replace')
      nodeindex = 1
      write(12,'(A)') '# agrid: ' // trim(agrid)
      write(12,'(I3," # boundaryType")') btype
      write(12,'(I3," # numBoundaries")') numBoundaries
      do i=1, numBoundaries
         thisBoundaryNum = boundaryIndices(i)
         write(12,'(I9,I3,F15.8,F15.8," # boundaryNum numNodes totalEffDepth(m) totalLength(m)")') &
         i, nvell(thisBoundaryNum), sumDepths(i), sumLengths(i)
         do j=1,nvell(thisBoundaryNum)
            write(12,'(I0,F15.8,F15.8," # nodeNum effDepth(m) effLength(m)")') &
            nbvv(thisBoundaryNum,j), dep(nodeindex), dist(nodeindex)
            nodeindex = nodeindex + 1
         end do
      end do
 99   continue
      close(11)
      close(12)
      write(6,'(A)') "INFO: Finished writing boundary information."
!----------------------------------------------------------------------
   end program adcircFluxBC
!----------------------------------------------------------------------

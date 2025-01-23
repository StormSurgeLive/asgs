!--------------------------------------------------------------------------
! applyBoundaryTypes.f90 : Extract boundaries and apply boundary types
! (not boundary conditions) to unassigned nodes.
!--------------------------------------------------------------------------
! Copyright(C) 2023 Jason Fleming
!
! This file is part of the tools.adcirc.live (ASGS).
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

!-----+---------+---------+---------+---------+---------+---------+
!
!   P R O G R A M    A P P L Y   B O U N D A R Y   T Y P E S
!
!-----+---------+---------+---------+---------+---------+---------+
program applyBoundaryTypes
use adcmesh
use ioutil
use logging
use netcdf
implicit none
type(mesh_t) :: m
type(meshNetCDF_t) :: n
logical :: gredit               ! .true. if gredit.bnd boundary file should be written

integer :: i, j, k
!
! initializations
if (loggingInitialized.eqv..false.) then
   call initLogging(availableUnitNumber(),'applyBoundaryTypes.f90')
endif
m%sfea0 = 30.d0
m%slam0 = 0.d0
m%externalBoundaryNodeHint = 0
m%oneElevationBoundary = .false.
gredit = .false.
!
! Report netcdf version
write(6,*) "INFO: applyBoundaryTypes was compiled with the following netcdf library: ", trim(nf90_inq_libvers())
!
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
            write(6,'(a,a,a,a,a)') "INFO: Processing ",trim(cmdlineopt)," ",trim(cmdlinearg),"."
            m%meshFileName = trim(cmdlinearg)
         case("--sfea0")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) m%sfea0
         case("--slam0")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) m%slam0
         case("--external-boundary-node")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) m%externalBoundaryNodeHint
         case("--min-elevation-boundary-length")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) m%minElevationBoundaryLength
         case("--min-elevation-boundary-depth")
            i = i + 1
            call getarg(i, cmdlinearg)
            write(6,'(a)') "INFO: Processing "//trim(cmdlineopt)//" "//trim(cmdlinearg)//"."
            read(cmdlinearg,*) m%minElevationBoundaryDepth
         case("--write-gredit-boundary-file")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            gredit = .true.
         case("--one-elevation-boundary")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            m%oneElevationBoundary = .true.
         case("--verbose")
            write(6,'(a,a,a)') "INFO: Processing ",trim(cmdlineopt),"."
            verbose = .true.
         case default
            write(6,'(a,a,a)') 'WARNING: Command line option "',TRIM(cmdlineopt),'" was not recognized.'
      end select
   end do
end if
!
! Load fort.14
call read14(m)
write(6,'("INFO: Computing nodal and elemental neighbor tables.")')
call computeNeighborTable(m)
write(6,'("INFO: Finding boundary edges.")')
call findBoundaryEdges(m)
write(6,'("INFO: Sorting boundary edges.")')
call sortBoundaryEdges(m)
write(6,'("INFO: Finding external boundary.")')
call findExternalBoundary(m)
write(6,'("INFO: Extracting external boundary.")')
call extractExternalBoundary(m)
write(6,'("INFO: Finding start and end of elevation specified boundaries.")')
call findElevationSpecifiedBoundaries(m)
write(6,'("INFO: Finding start and end of flux specified boundaries.")')
call findFluxSpecifiedExternalBoundaries(m)
! start writing mesh file
write(6,'("INFO: Writing mesh file header.")')
call writeMeshHeaderASCII(m)
! write node and element tables
write(6,'("INFO: Writing mesh file node table.")')
call writeMeshNodeTableASCII(m)
write(6,'("INFO: Writing mesh file element table.")')
call writeMeshElementTableASCII(m)
! write elevation boundary table
write(6,'("INFO: Writing mesh elevation boundary table.")')
call writeMeshElevationBoundaryTableASCII(m)
! write flux boundary table
write(6,'("INFO: Writing mesh flux boundary table.")')
call writeMeshFluxBoundaryTableASCII(m)

!C....Write GREDIT boundary file
!99 open(10,file='gredit.bnd')
!write(10,'(a80)') 'gredit.bnd'
!write(10,*) m%numBoundFound
!k=0
!do i=1,m%numBoundFound
!  write(10,*) m%nbn(i),1
!  do j=1,m%nbn(i)
!    k=k+1
!    write(10,*) m%conbnode(k)
!    end do
!  end do
!close(10)

!----------------------------------------------------------------------
end program applyBoundaryTypes
!----------------------------------------------------------------------
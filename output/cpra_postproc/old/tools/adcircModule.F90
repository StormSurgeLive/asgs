!-----GPL----------------------------------------------------------------------
!
! This file is part of the arcadis-util library
! Copyright (C) 2010-2016  Arcadis
!
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: Zachary Cobell, zachary.cobell@arcadis.com
!  Arcadis
!  11001 W. 120th Ave, Suite 200
!  Broomfield, CO 80021
!
!  All indications and logos of, and references to, "Arcadis"
!  are registered trademarks of Arcadis, and remain the property of
!  Arcadis. All rights reserved.
!
!------------------------------------------------------------------------------
!
!  File: ADCIRCmodule.F90
!
!------------------------------------------------------------------------------
!


!----------------------------------------------------------------------------------!
!                                                                                  !
!                           START ADCIRC ROUTINES                                  !
!                                                                                  !
!----------------------------------------------------------------------------------!

!> \brief Module useful for programming when modeling with ADCIRC
!> \author Zach Cobell
!> \copyright GNU Public License, Version 3
        MODULE ADCIRCMODULE
#ifdef _NETCDF
            USE NETCDF
#endif
            USE KDTREE2MODULE
            USE GENERICMODULE


            !...Variables that we will save in memory related to processing
            !   command line arguments comming from C++ code. These are not
            !   exposed to outside code

            !>Variable retained holding the command line arguments when using C++
            CHARACTER(200),ALLOCATABLE,SAVE,PRIVATE :: ADCModules_CommandLineArgs(:)

            !>Variable holding the number of command line arguments
            INTEGER,SAVE,PRIVATE                    :: ADCModules_NumCommandLineArgs

!................................................................................
!
!                      ADCIRC TYPE VARIABLES
!
!................................................................................

            !>Type BoundaryListing is a derived type for ADCIRC boundary conditions
            TYPE BoundaryListing
                !>Number of nodes for this particular boundary
                INTEGER                    :: NumNodes
                !>ADCIRC model boundary type code
                INTEGER                    :: Code
                !>Array of nodes on first side of the boundary
                INTEGER,ALLOCATABLE        :: N1(:)
                !>Array of nodes on second side of boundary (may be unused)
                INTEGER,ALLOCATABLE        :: N2(:)
                !>Crest height of weir type boundaries
                REAL(8),ALLOCATABLE        :: Crest(:)
                !>Coefficient of supercitical flow for weir type boundaries
                REAL(8),ALLOCATABLE        :: Supercritical(:)
                !>Coefficient of subcritical flow for weir type boundaries
                REAL(8),ALLOCATABLE        :: Subcritical(:)
            END TYPE

            !>Type Grid is a derived type containing all adcirc grid information
            TYPE Grid
                !>Title line found in mesh file
                CHARACTER(200)             :: title
                !>Number of elements in the ADCIRC mesh
                INTEGER                    :: NumElements
                !>Number of nodes in the ADCIRC mesh
                INTEGER                    :: NumNodes
                !>2D array that is NumNodes by 3 containing the Nodal
                !>position and elevation. Index 2 contains x, y, and z
                !>as index 1, 2, and 3.
                REAL(8),ALLOCATABLE        :: nodes(:,:)
                !>Connectivity table for the ADCIRC mesh that is
                !>NumElements by 3. References the node table.
                INTEGER,ALLOCATABLE        :: conn(:,:)
                !>Number of open boundaries in the ADCIRC mesh
                INTEGER                    :: NumOpenBoundaries
                !>Number of open boundary nodes in the ADCIRC mesh
                INTEGER                    :: TotNumOpenBoundaryNodes
                !>Number of land boundaries in the ADCIRC mesh
                INTEGER                    :: NumLandBoundaries
                !>Total number of land boundary nodes in the ADCIRC mesh
                INTEGER                    :: TotNumLandBoundaryNodes
                !>Array of open boundaries
                TYPE(BoundaryListing),ALLOCATABLE :: OceanBC(:)
                !>Array of land boundaries
                TYPE(BoundaryListing),ALLOCATABLE :: LandBC(:)
            END TYPE

            !>Type Fort13 contains information for individual parameters in a
            !>fort.13
            TYPE Parameters
                !>Name of this nodal parameter
                CHARACTER(200)      :: Attribute
                !>Default value for this nodal parameter
                REAL                :: DefaultValue
                !>Units read in from the fort.13 file
                CHARACTER(10)       :: Units
                !>Number of values for each node. Usually 1 or 12.
                INTEGER             :: NumValues
                !>Array that contains the values for this nodal parameter at each
                !>node
                REAL,ALLOCATABLE    :: Values(:,:)
            END TYPE

            !>The global fort.13 structure which relies on TYPE Parameters
            TYPE fort13
                !>Title found in the header of the fort.13 file
                CHARACTER(200)      :: Title
                !>Number of nodes in the mesh that this file was created for
                INTEGER             :: NumNodes
                !>Number of nodal parameters that are found within this file
                INTEGER             :: NumAttributes
                !>Array containing the nodal parameters
                TYPE(parameters),ALLOCATABLE :: nodal_param(:)
            END TYPE

            !>Type Timestep contains the information from a single ADCIRC timestep
            TYPE Timestep
                !>Integer timestep number for this output cycle
                !> \f$TS = Time \cdot DT\f$
                INTEGER                    :: TS
                !>Number of non-default nodes. Used when ADCIRC output is written
                !>in sparse format
                INTEGER                    :: NumNonDefault
                !>Value not written to ADCIRC output files. Used when ADCIRC
                !>output is written in sparse format.
                REAL(8)                    :: DefaultValue
                !>Time of the current output cycle
                !> \f$Time = \frac{TS}{DT}\f$
                REAL(8)                    :: Time
                !>Values contains the array of output values for the current
                !>output cycle
                REAL(8),ALLOCATABLE        :: Values(:,:)
            END TYPE

            !>Type AdcircOutput contains all the information inside an ADCIRC output file
            TYPE AdcircOutput
                !>Title string found in ADCIRC output file
                CHARACTER(200)             :: title
                !>netCDF variable locator for first output array
                CHARACTER(200)             :: NC_VARIABLE1
                !>netCDF variable locator for second output array
                CHARACTER(200)             :: NC_VARIABLE2
                !>Interval between output cycles in the ADCIRC output file
                REAL(8)                    :: dt
                !>Number of columns in output array. Scalar or Vector quantity
                INTEGER                    :: NumValues
                !>Number of nodes in mesh used to generate this output file
                INTEGER                    :: NumNodes
                !>Number of time steps in this output file
                INTEGER                    :: NumTimeSteps
                !>Number of ADCIRC time steps between output intervals
                INTEGER                    :: Interval
                !>File format identifier. Generally not used.
                INTEGER                    :: FileFormat
                !>Array of output cycles
                TYPE(Timestep),ALLOCATABLE :: Output(:)
            END TYPE

            !>The following is a type created for ADCIRC hot start output. The naming convention
            !>mirrors that which is found within the ADCIRC code (v50).
            TYPE HOTSTART2D
                REAL(8),ALLOCATABLE :: ETA1(:)
                REAL(8),ALLOCATABLE :: ETA2(:)
                REAL(8),ALLOCATABLE :: ETADisc(:)
                REAL(8),ALLOCATABLE :: UU2(:)
                REAL(8),ALLOCATABLE :: VV2(:)
                INTEGER,ALLOCATABLE :: NNODECODE(:)
                INTEGER,ALLOCATABLE :: NOFF(:)
                REAL(8),ALLOCATABLE :: CH1(:)
                INTEGER             :: IMHS
                REAL(8)             :: TimeLoc
                INTEGER             :: InputFileFmtVn
                INTEGER             :: ITHS
                INTEGER             :: NP_G_IN
                INTEGER             :: NE_G_IN
                INTEGER             :: NP_A_IN
                INTEGER             :: NE_A_IN
                INTEGER             :: IESTP
                INTEGER             :: NSCOUE
                INTEGER             :: IVSTP
                INTEGER             :: NSCOUV
                INTEGER             :: ICSTP
                INTEGER             :: NSCOUC
                INTEGER             :: IPSTP
                INTEGER             :: IWSTP
                INTEGER             :: NSCOUM
                INTEGER             :: IGEP
                INTEGER             :: NSCOUGE
                INTEGER             :: IGVP
                INTEGER             :: NSCOUGV
                INTEGER             :: IGCP
                INTEGER             :: NSCOUGC
                INTEGER             :: IGPP
                INTEGER             :: IGWP
                INTEGER             :: NSCOUGW
            END TYPE

            !>Inner type to hold element table information
            TYPE ElementTableInner
                !>Number of elements conntected to this node
                INTEGER                        :: NumElementsAroundMe
                !>Array of elements connected to this node
                INTEGER,ALLOCATABLE            :: ElementsAroundMe(:)
            END TYPE

            !>Type for the global element table structure
            TYPE ElementTable
                !>Number of nodes within the mesh this table was created for
                INTEGER                             :: NumNodes
                !>Array of element lists for each node
                TYPE(ElementTableInner),ALLOCATABLE :: Node(:)
            END TYPE

            !>Inner type to hold node table information
            TYPE NODETABLE_INNER
                !>Number of nodes connected to this node
                INTEGER :: NumNodesAroundMe
                !>Array of nodes connected to this node
                INTEGER,ALLOCATABLE :: NodesAroundMe(:)
                !>Distance to each node connected to this node (meters)
                REAL(8),ALLOCATABLE :: distance(:)
            END TYPE

            !>Type for global node table information
            TYPE NODETABLE
                !>Number of nodes within the mesh this table was created for
                INTEGER :: NumNodes
                !>Array of node lists for each node
                TYPE(NODETABLE_INNER),ALLOCATABLE :: node(:)
            END TYPE


            !>Listing of output types written to ADCIRC files
            CHARACTER(200) :: NETCDF_TYPES(38)
            !>Long name for each type of ADCIRC output
            CHARACTER(200) :: NC_LONGNAME(38)
            !>Common name for each type of ADCIRC output
            CHARACTER(200) :: NC_STDNAME(38)

            CONTAINS

!................................................................................
!
!                      ADCIRC GRIDS
!
!................................................................................

            !>This subroutine will read an ADCIRC mesh and return an ADCIRC
            !>grid type variable. It will also perform some basic checks to
            !>ensure that the grid is relatively sane and can be used in
            !>other code to follow. Will automatically determine an FORTRAN
            !>read unit so as not to conflict with other user written code
            !>\author Zach Cobell
            SUBROUTINE ReadGrid(gridfile,MyGrid)
            !...Modular subroutine to read ADCIRC grid. Must have Grid data structure from GlobalGrid module
#ifdef _NETCDF
                USE NETCDF
#endif
                IMPLICIT NONE
                !>String containing the name of the file to be read
                CHARACTER(*),INTENT(IN)        :: gridfile
                !>Output variable of type GRID containing the information
                !>read from the file
                TYPE(Grid),INTENT(OUT)         :: MyGrid


                INTEGER                        :: I,J
                INTEGER                        :: N = 0
                INTEGER                        :: JunkI
                INTEGER                        :: JunkI2
                INTEGER                        :: READUNIT
                LOGICAL                        :: ISNETCDF
                
                INTEGER                        :: NODEDIM,ELEDIM
                INTEGER                        :: NETAVAR,NVDLLVAR
                INTEGER                        :: MAXNODES,NBOUDIM
                INTEGER                        :: NOPEDIM,MXNVELLVAR
                INTEGER                        :: IBTYPEVAR,NVELLVAR
                INTEGER                        :: ELVAR,XVAR,YVAR,ZVAR
                INTEGER                        :: MXNVDLLVAR,NDBVVAR
                INTEGER                        :: NBVVVAR,NVELVAR
                INTEGER                        :: CHUNK,CHUNK2,DMY(2)
                INTEGER,ALLOCATABLE            :: NUMNODES_OPENBNDRY(:)
                INTEGER,ALLOCATABLE            :: LANDBOUNDARY_ENTRIES(:,:)
                INTEGER,ALLOCATABLE            :: OPENBOUNDARY_ENTRIES(:,:)
                INTEGER,ALLOCATABLE            :: NumNodes_LandBndry(:)
                INTEGER,ALLOCATABLE            :: ELTEMP(:,:),IBTYPE(:),TEMPI(:,:)

                REAL(8),ALLOCATABLE            :: TEMPR(:)

                !...Determine if file is netcdf
                IF(INDEX(gridfile,".nc").GT.1)THEN
                    ISNETCDF=.TRUE.
                ELSE
                    ISNETCDF=.FALSE.
                ENDIF


                IF(ISNETCDF)THEN
#ifndef _NETCDF
                    WRITE(*,'(A)') "ERROR: ADCModules not compiled for NetCDF."
                    STOP
#else
                   !...Open NetCDF
                   !WRITE(*,'(A,$)') "WARNING - not all information available "//&
                   !                 "in ADCIRC netCDF grid format!"
                    CALL CHECK(NF90_OPEN(TRIM(GRIDFILE),NF90_NOWRITE,READUNIT))
                    
                    !...Get sizes
                    CALL CHECK(NF90_INQ_DIMID(READUNIT,"node",NODEDIM))
                    CALL CHECK(NF90_INQ_DIMID(READUNIT,"nele",ELEDIM))
                    CALL CHECK(NF90_INQUIRE_DIMENSION(READUNIT,NODEDIM,LEN=MYGRID%NUMNODES))
                    CALL CHECK(NF90_INQUIRE_DIMENSION(READUNIT,ELEDIM,LEN=MYGRID%NUMELEMENTS))
                    ALLOCATE(MYGRID%NODES(1:MYGRID%NUMNODES,1:3))
                    ALLOCATE(MYGRID%CONN(1:MYGRID%NUMELEMENTS,1:3))
                    ALLOCATE(ELTEMP(1:3,1:MYGRID%NUMELEMENTS))
                    
                    !...Get X,Y,Z
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'x',XVAR))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'y',YVAR))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'depth',ZVAR))
                   
                    ALLOCATE(TempR(1:MYGRID%NUMNODES))
                    CALL CHECK(NF90_GET_VAR(READUNIT,XVAR,TEMPR))
                    MYGRID%NODES(:,1) = TEMPR(:)
                    CALL CHECK(NF90_GET_VAR(READUNIT,YVAR,TEMPR))
                    MYGRID%NODES(:,2) = TEMPR(:)
                    CALL CHECK(NF90_GET_VAR(READUNIT,ZVAR,TEMPR))
                    MYGRID%NODES(:,3) = TEMPR(:)
                    
                    !...Get elements
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'element',ELVAR))
                    CHUNK=500000
                    ALLOCATE(TEMPI(1:3,1:CHUNK))
                    DO I = 1,MYGRID%NUMELEMENTS,CHUNK
                        IF(I+CHUNK.GT.MYGRID%NUMELEMENTS)THEN
                            CHUNK2 = MYGRID%NUMELEMENTS-I+1
                        ELSE
                            CHUNK2 = CHUNK
                        ENDIF
                        CALL CHECK(NF90_GET_VAR(READUNIT,ELVAR,TEMPI,START=(/1,I/),COUNT=(/3,CHUNK2/)))
                        ELTEMP(1:3,I:I+CHUNK2-1) = TEMPI(1:3,1:CHUNK2)
                    ENDDO
                    MYGRID%CONN(:,1) = ELTEMP(1,:)
                    MYGRID%CONN(:,2) = ELTEMP(2,:)
                    MYGRID%CONN(:,3) = ELTEMP(3,:)
#if 0                    
                    !...Get Open Boundaries
                    CALL CHECK(NF90_INQ_DIMID(READUNIT,'nope',NOPEDIM))
                    CALL CHECK(NF90_INQUIRE_DIMENSION(READUNIT,NOPEDIM,LEN=MYGRID%NUMOPENBOUNDARIES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'neta',NETAVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NETAVAR,MYGRID%TOTNUMOPENBOUNDARYNODES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nvdll',NVDLLVAR))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'max_nvdll',MXNVDLLVAR))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nbdv',NDBVVAR))
                    ALLOCATE(NUMNODES_OPENBNDRY(1:MYGRID%NUMOPENBOUNDARIES))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NVDLLVAR,NUMNODES_OPENBNDRY))
                    CALL CHECK(NF90_GET_VAR(READUNIT,MXNVDLLVAR,MAXNODES))
                    ALLOCATE(OPENBOUNDARY_ENTRIES(1:MYGRID%NUMOPENBOUNDARIES,1:MAXNODES))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NDBVVAR,OPENBOUNDARY_ENTRIES))
                    ALLOCATE(MYGRID%OCEANBC(1:MYGRID%NUMOPENBOUNDARIES))
                    DO I = 1,MYGRID%NUMOPENBOUNDARIES
                        MYGRID%OCEANBC(I)%NUMNODES = NUMNODES_OPENBNDRY(I)
                        ALLOCATE(MYGRID%OCEANBC(I)%N1(1:NUMNODES_OPENBNDRY(I)))
                        DO J = 1,NUMNODES_OPENBNDRY(I)
                            MYGRID%OCEANBC(I)%N1(J) = OPENBOUNDARY_ENTRIES(I,J)
                        ENDDO
                    ENDDO
                    
                    !...Get Land Boundaries
                    CALL CHECK(NF90_INQ_DIMID(READUNIT,'nbou',NBOUDIM))
                    CALL CHECK(NF90_INQUIRE_DIMENSION(READUNIT,NBOUDIM,LEN=MYGRID%NUMLANDBOUNDARIES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nvel',NVELVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NVELVAR,MYGRID%TOTNUMLANDBOUNDARYNODES))
                    ALLOCATE(NUMNODES_LANDBNDRY(1:MYGRID%NUMLANDBOUNDARIES))
                    ALLOCATE(IBTYPE(1:MYGRID%NUMLANDBOUNDARIES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nvell',NVELLVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NVELLVAR,NUMNODES_LANDBNDRY))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'ibtype',IBTYPEVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,IBTYPEVAR,IBTYPE))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'max_nvell',MXNVELLVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,MXNVELLVAR,MAXNODES))
                    ALLOCATE(LANDBOUNDARY_ENTRIES(1:MYGRID%NUMLANDBOUNDARIES,1:MAXNODES))
                    CALL CHECK(NF90_INQ_VARID(READUNIT,'nbvv',NBVVVAR))
                    CALL CHECK(NF90_GET_VAR(READUNIT,NBVVVAR,LANDBOUNDARY_ENTRIES))
                    ALLOCATE(MYGRID%LANDBC(1:MYGRID%NUMLANDBOUNDARIES))
                    DO I = 1,MYGRID%NUMLANDBOUNDARIES
                        MYGRID%LANDBC(I)%NUMNODES = NUMNODES_LANDBNDRY(I)
                        MYGRID%LANDBC(I)%CODE = IBTYPE(I)
                        SELECT CASE(IBTYPE(I))
                            CASE(0,1,2,10,11,12,20,21,22,30,52)
                                ALLOCATE(MYGRID%LANDBC(I)%N1(1:NUMNODES_LANDBNDRY(I)))
                            CASE(3,13,23)
                                ALLOCATE(MYGRID%LANDBC(I)%N1(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%CREST(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%SUPERCRITICAL(1:NUMNODES_LANDBNDRY(I)))                           
                                
                            CASE(4,24,5,25)
                                ALLOCATE(MYGRID%LANDBC(I)%N1(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%N2(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%CREST(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%SUPERCRITICAL(1:NUMNODES_LANDBNDRY(I)))
                                ALLOCATE(MYGRID%LANDBC(I)%SUBCRITICAL(1:NUMNODES_LANDBNDRY(I)))
                            
                            CASE DEFAULT
                                ALLOCATE(MYGRID%LANDBC(I)%N1(1:NUMNODES_LANDBNDRY(I)))
                        END SELECT
                    ENDDO
#endif                    
                
#endif                
                ELSE
                    READUNIT = GetFreeUnit()
                    OPEN(FILE=TRIM(gridfile),UNIT=READUNIT,ACTION="READ")

                    !...Read header
                    READ(READUNIT,'(A)') MyGrid%title
                    READ(READUNIT,*) MyGrid%NumElements,MyGrid%NumNodes
                    ALLOCATE(MyGrid%nodes(1:MyGrid%NumNodes,1:3))
                    ALLOCATE(MyGrid%conn(1:MyGrid%NumElements,1:3))

                    !...Read nodes
                    DO I = 1,MyGrid%NumNodes
                        READ(READUNIT,*) JunkI,MyGrid%nodes(I,1),&
                            MyGrid%nodes(I,2),MyGrid%nodes(I,3)
                        IF(JunkI.NE.I)THEN
                            WRITE(*,'(A)') ""
                            WRITE(*,'(A)') "ERROR: Mesh needs renumbering."
                            !CALL Quit
                            STOP
                        ENDIF
                        !CALL ShowBar(MyGrid%NumNodes+MyGrid%NumElements,I,N)
                    ENDDO

                    !...Read elements
                    DO I = 1,MyGrid%NumElements
                        READ(READUNIT,*) JunkI,JunkI2,MyGrid%conn(I,1),&
                            MyGrid%conn(I,2),MyGrid%conn(I,3)
                       IF(JunkI.NE.I)THEN
                            WRITE(*,'(A)') ""
                            WRITE(*,'(A)') "ERROR: Mesh needs renumbering."
                            !CALL Quit
                            STOP
                        ENDIF
                        !CALL ShowBar(MyGrid%NumNodes+MyGrid%NumElements,MyGrid%NumNodes+I,N)
                    ENDDO

                    !...Read Open BCs
                    READ(READUNIT,*,END=100) MyGrid%NumOpenBoundaries
                    READ(READUNIT,*,END=100) MyGrid%TotNumOpenBoundaryNodes
                    ALLOCATE(MyGrid%OceanBC(MyGrid%NumOpenBoundaries))
                    DO I = 1,MyGrid%NumOpenBoundaries
                        READ(READUNIT,*) MyGrid%OceanBC(I)%NumNodes
                        ALLOCATE(MyGrid%OceanBC(I)%N1(1:MyGrid%OceanBC(I)%NumNodes))
                        DO J = 1,MyGrid%OceanBC(I)%NumNodes
                            READ(READUNIT,*) MyGrid%OceanBC(I)%N1(J)
                        ENDDO
                    ENDDO

                    !...Read Land BCs
                    READ(READUNIT,*,END=200) MyGrid%NumLandBoundaries
                    READ(READUNIT,*,END=200) MyGrid%TotNumLandBoundaryNodes
                    ALLOCATE(MyGrid%LandBC(MyGrid%NumLandBoundaries))
                    DO I = 1,MyGrid%NumLandBoundaries
                        READ(READUNIT,*) MyGrid%LandBC(I)%NumNodes,&
                            MyGrid%LandBC(I)%Code
                        SELECT CASE(MyGrid%LandBC(I)%Code)
                            CASE(0,1,10,11,12,20,21,22,52)
                                ALLOCATE(MyGrid%LandBC(I)%N1(1:MyGrid%LandBC(I)%NumNodes))
                                DO J = 1,MyGrid%LandBC(I)%NumNodes
                                    READ(READUNIT,*) MyGrid%LandBC(I)%N1(J)
                                ENDDO
                            CASE(13,23)
                                ALLOCATE(MyGrid%LandBC(I)%N1(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Crest(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Supercritical(1:MyGrid%LandBC(I)%NumNodes))
                                DO J = 1,MyGrid%LandBC(I)%NumNodes
                                    READ(READUNIT,*) MyGrid%LandBC(I)%N1(J), &
                                               MyGrid%LandBC(I)%Crest(J), &
                                               MyGrid%LandBC(I)%Supercritical(J)
                                ENDDO
                            CASE(24)
                                ALLOCATE(MyGrid%LandBC(I)%N1(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%N2(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Crest(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Supercritical(1:MyGrid%LandBC(I)%NumNodes))
                                ALLOCATE(MyGrid%LandBC(I)%Subcritical(1:MyGrid%LandBC(I)%NumNodes))
                                DO J = 1,MyGrid%LandBC(I)%NumNodes
                                    READ(READUNIT,*) MyGrid%LandBC(I)%N1(J), &
                                               MyGrid%LandBC(I)%N2(J), &
                                               MyGrid%LandBC(I)%Crest(J), &
                                               MyGrid%LandBC(I)%Subcritical(J), &
                                               MyGrid%LandBC(I)%Supercritical(J)
                                ENDDO
                            CASE DEFAULT
                                ALLOCATE(MyGrid%LandBC(I)%N1(1:MyGrid%LandBC(I)%NumNodes))
                                WRITE(*,'(2A,I0)') "WARNING: Unknown boundary ",&
                                    "condition. ADCIRC TYPE = ",&
                                    MyGrid%LandBC(I)%Code
                                WRITE(*,'(A)') "         READ AS A SINGLE NODE."
                                DO J = 1,MyGrid%LandBC(I)%NumNodes
                                    READ(READUNIT,*) MyGrid%LandBC(I)%N1(J)
                                ENDDO
                        END SELECT
                    ENDDO

                    CLOSE(READUNIT)

                    RETURN

100                 CONTINUE
                    MyGrid%NumOpenBoundaries = 0
                    MyGrid%TotNumOpenBoundaryNodes = 0
200                 CONTINUE
                    MyGrid%NumLandBoundaries = 0
                    MyGrid%TotNumLandBoundaryNodes = 0
                    RETURN
                ENDIF

            END SUBROUTINE

            !>Subroutine to write an adcirc grid. Will automatically determine
            !>the FORTRAN write unit so as not to disturb other user written
            !>code
            !>\author Zach Cobell
            SUBROUTINE WriteGrid(gridname,MyGrid)

                IMPLICIT NONE

                !>Name of the file to write the ADCIRC mesh to
                CHARACTER(*),INTENT(IN) :: gridname
                !>ADCIRC mesh that is to be written
                TYPE(grid),INTENT(IN)   :: MyGrid

                INTEGER                 :: I,J
                INTEGER                 :: WRITEUNIT
                INTEGER                 :: N = 0

                WRITEUNIT = GetFreeUnit()

                OPEN(FILE=TRIM(gridname),UNIT=WRITEUNIT,ACTION="WRITE")
                WRITE(WRITEUNIT,'(A)') TRIM(MyGrid%title)
                WRITE(WRITEUNIT,*) MyGrid%NumElements,MyGrid%NumNodes
                DO I = 1,MyGrid%NumNodes
                    WRITE(WRITEUNIT,'(I10,3(2X,F16.8))') I,MyGrid%Nodes(I,1),MyGrid%Nodes(I,2),MyGrid%Nodes(I,3)
                ENDDO
                DO I = 1,MyGrid%NumElements
                    WRITE(WRITEUNIT,'(5(I10,2X))') I,3,MyGrid%conn(I,1),MyGrid%conn(I,2),MyGrid%conn(I,3)
                ENDDO
                WRITE(WRITEUNIT,'(I10)') MyGrid%NumOpenBoundaries
                WRITE(WRITEUNIT,'(I10)') MyGrid%TotNumOpenBoundaryNodes
                DO I = 1,MyGrid%NumOpenBoundaries
                    WRITE(WRITEUNIT,'(I10)') MyGrid%OceanBC(I)%NumNodes
                    DO J = 1,MyGrid%OceanBC(I)%NumNodes
                        WRITE(WRITEUNIT,*) MyGrid%OceanBC(I)%N1(J)
                    ENDDO
                ENDDO
                WRITE(WRITEUNIT,*) MyGrid%NumLandBoundaries
                WRITE(WRITEUNIT,*) MyGrid%TotNumLandBoundaryNodes
                DO I = 1,MyGrid%NumLandBoundaries
                    WRITE(WRITEUNIT,'(I6,4X,I6,4X,A,I6)') MyGrid%LandBC(I)%NumNodes,MyGrid%LandBC(I)%Code,"   !=seg",I
                    SELECT CASE(MyGrid%LandBC(I)%Code)
                        CASE(0,1,10,11,12,20,21,22,52)
                            DO J = 1,MyGrid%LandBC(I)%NumNodes
                                WRITE(WRITEUNIT,'(I10)') MyGrid%LandBC(I)%N1(J)
                            ENDDO
                        CASE(13,23)
                            DO J = 1,MyGrid%LandBC(I)%NumNodes
                                WRITE(WRITEUNIT,'(I10,2X,F16.3,2X,F16.3)') MyGrid%LandBC(I)%N1(J),MyGrid%LandBC(I)%Crest(J),&
                                    MyGrid%LandBC(I)%Supercritical(J)
                            ENDDO
                        CASE(24)
                            DO J = 1,MyGrid%LandBC(I)%NumNodes
                                WRITE(WRITEUNIT,'(I10,2X,I10,2X,F16.3,2X,F16.3,2X,F16.3,2X,F16.3)') &
                                    MyGrid%LandBC(I)%N1(J),MyGrid%LandBC(I)%N2(J),&
                                    MyGrid%LandBC(I)%Crest(J),MyGrid%LandBC(I)%Subcritical(J),&
                                    MyGrid%LandBC(I)%Supercritical(J)
                            ENDDO

                        CASE DEFAULT
                            DO J = 1,MyGrid%LandBC(I)%NumNodes
                                WRITE(WRITEUNIT,'(I10)') MyGrid%LandBC(I)%N1(J)
                            ENDDO
                    END SELECT
                ENDDO

                CLOSE(WRITEUNIT)

                RETURN


            END SUBROUTINE

!................................................................................
!
!                      Nodal Attributes
!
!................................................................................

            !>Subroutine to read a fort.13 file into the fort13 structure.
            !>Read unit will be automatically determined.
            !>\author Zach Cobell
            SUBROUTINE Read13(fort13_file,My13)
                IMPLICIT NONE

                !>File to read the nodal parameter information from
                CHARACTER(*),INTENT(IN) :: fort13_file
                !>Variable containing the fort.13 information read from the file
                TYPE(fort13),INTENT(OUT) :: My13

                CHARACTER(200)  :: JunkC
                CHARACTER(200)  :: TempC
                CHARACTER(200)  :: MyAttribute
                INTEGER   :: I,J,K
                INTEGER   :: IDX
                INTEGER   :: node
                INTEGER   :: READUNIT
                INTEGER   :: NumNonDefault
                REAL(8)   :: VLUE
                REAL(8)   :: DFLT
                REAL(8),ALLOCATABLE :: temp_array(:,:)
                LOGICAL   :: exists

                READUNIT = GETFREEUNIT()

                INQUIRE(FILE=TRIM(fort13_file),EXIST=exists)
                IF(.NOT.exists)THEN
                    WRITE(*,'(A)') "13 file does not exist."
                    STOP
                ENDIF

                OPEN(FILE=TRIM(fort13_file),UNIT=READUNIT,ACTION="READ")

                READ(READUNIT,'(A)') My13%title
                READ(READUNIT,*) My13%NumNodes
                READ(READUNIT,*) My13%NumAttributes
                ALLOCATE(My13%nodal_param(1:My13%NumAttributes))
                DO I = 1,My13%NumAttributes
                    READ(READUNIT,'(A)') TempC
                    My13%nodal_param(I)%Attribute = ADJUSTL(TempC)
                    READ(READUNIT,'(A)') My13%nodal_param(I)%units
                    READ(READUNIT,*) My13%nodal_param(I)%NumValues
                    READ(READUNIT,*) Dflt
                    ALLOCATE(My13%nodal_param(I)%values(&
                        1:My13%NumNodes,1:My13%nodal_param(I)%NumValues))
                    My13%nodal_param(I)%values = Dflt
                    My13%nodal_param(I)%DefaultValue = Dflt
                ENDDO

                DO I = 1,My13%NumAttributes
                    READ(READUNIT,'(A)') MyAttribute
                    READ(READUNIT,*) NumNonDefault
                    IDX = FindAttributeIndex(MyAttribute,My13)

                    IF(My13%nodal_param(IDX)%NumValues.EQ.1)THEN
                        DO J = 1,NumNonDefault
                            READ(READUNIT,*) node,vlue
                            My13%nodal_param(IDX)%values(node,1) = vlue
                        ENDDO
                    ELSEIF(My13%nodal_param(IDX)%NumValues.GT.1)THEN
                        DO J = 1,NumNonDefault
                            READ(READUNIT,*) node,(My13%nodal_param(IDX)%&
                                values(node,k),k=1,My13%nodal_param(IDX)%NumValues)
                        ENDDO
                    ENDIF

                ENDDO

                CLOSE(READUNIT)

            END SUBROUTINE

            !>This subroutine builds an empty fort.13 file. This is useful so
            !>that the user does not need to
            !>\author Zach Cobell
            SUBROUTINE BuildEmptyFort13(NumNodes,NumAttributes,&
                NumValues,DefaultValues,Units,ParamNames,OUTPUT)

                IMPLICIT NONE

                !>Number of nodes to size the arrays for
                INTEGER,INTENT(IN)       :: NumNodes
                !>Number of attributes to build in the empty file
                INTEGER,INTENT(IN)       :: NumAttributes
                !>Number of values for each of the parameters, generally 1 or 12
                INTEGER,INTENT(IN)       :: NumValues(*)
                !>Default values for each parameter
                REAL(8),INTENT(IN)       :: DefaultValues(*)
                !>Names of each parameter in the empty fort.13 file
                CHARACTER(*),INTENT(IN)  :: ParamNames(*)
                !>Units to be used for each parameter in the empty fort.13 file
                CHARACTER(*),INTENT(IN)  :: Units(*)
                !>Variable containing the empty fort.13 file
                TYPE(FORT13),INTENT(OUT) :: OUTPUT

                INTEGER                  :: I

                OUTPUT%TITLE         = "EmptyFort13"
                OUTPUT%NumNodes      = NumNodes
                OUTPUT%NumAttributes = NumAttributes
                ALLOCATE(OUTPUT%nodal_param(1:NumAttributes))
                DO I = 1,NumAttributes
                    ALLOCATE(OUTPUT%nodal_param(I)% &
                        values(1:NumNodes,1:NumValues(I)))
                    OUTPUT%nodal_param(I)%NumValues    = NumValues(I)
                    OUTPUT%nodal_param(I)%DefaultValue = DefaultValues(I)
                    OUTPUT%nodal_param(I)%values(:,:)  = DefaultValues(I)
                    OUTPUT%nodal_param(I)%Attribute    = ParamNames(I)
                    OUTPUT%nodal_param(I)%Units        = Units(I)
                ENDDO

                RETURN

            END SUBROUTINE

            !>Subroutine to write a fort.13 file
            !>\author Zach Cobell
            SUBROUTINE Write13(filename,Input13,REDUCE)
                IMPLICIT NONE

                !>Name of the file to write
                CHARACTER(*),INTENT(IN)     :: filename
                !>Variable to write the fort.13 from
                TYPE(fort13),INTENT(IN)     :: Input13
                !>Optional: Select an optimal value for the default value
                !>in the header of the fort.13. This will likely reduce file
                !>size
                LOGICAL,INTENT(IN),OPTIONAL :: Reduce

                INTEGER                 :: I,J,K
                INTEGER,ALLOCATABLE     :: defaults(:)
                INTEGER                 :: WRITEUNIT
                CHARACTER(50)           :: MultipleValueFormat
                REAL(8)                 :: default_value
                LOGICAL                 :: DoReduce
                TYPE(FORT13)            :: My13

                WRITEUNIT = GETFREEUNIT()

                !...Check if new default values should be calculated
                IF(.NOT.PRESENT(REDUCE))THEN
                    DoReduce = .TRUE.
                ELSE
                    DoReduce = Reduce
                ENDIF

                My13 = Input13
                IF(DoReduce)THEN
                    CALL Reduce13(My13)
                ENDIF

                OPEN(FILE=TRIM(filename),UNIT=WRITEUNIT,ACTION="WRITE")

                WRITE(WRITEUNIT,'(A)') TRIM(ADJUSTL(My13%title))
                WRITE(WRITEUNIT,'(I0)') My13%NumNodes
                WRITE(WRITEUNIT,'(I0)') My13%NumAttributes
                !...Count non default values
                ALLOCATE(defaults(1:My13%NumAttributes))
                defaults(:) = 0
                DO I = 1,My13%NumAttributes
                    default_value = My13%nodal_param(I)%DefaultValue
                    IF(My13%nodal_param(I)%NumValues.EQ.1)THEN
                        DO J = 1,My13%NumNodes
                            IF(My13%nodal_param(I)%values(J,1).NE.&
                                My13%nodal_param(I)%DefaultValue)THEN
                                    defaults(I) = defaults(I) + 1
                            ENDIF
                        ENDDO
                    ELSEIF(My13%nodal_param(I)%NumValues.GT.1)THEN
                        DO J = 1,My13%NumNodes
                            IF(SUM(My13%nodal_param(I)%values(J,:)).NE.0d0)THEN
                                defaults(I) = defaults(I) + 1
                            ENDIF
                        ENDDO
                    ENDIF
                 ENDDO

                 !...Write 13 header
                 DO I = 1,My13%NumAttributes
                    WRITE(WRITEUNIT,'(A)') TRIM(ADJUSTL(My13%nodal_param(I)%Attribute))
                    WRITE(WRITEUNIT,'(A)') TRIM(ADJUSTL(My13%nodal_param(I)%units))
                    WRITE(WRITEUNIT,'(I0)') My13%nodal_param(I)%NumValues
                    IF(My13%nodal_param(I)%NumValues.EQ.1)THEN
                        WRITE(WRITEUNIT,'(F0.6)') My13%nodal_param(I)%DefaultValue
                    ELSEIF(My13%nodal_param(I)%Numvalues.GT.1)THEN
                        WRITE(MultipleValueFormat,'(A,I0,A)') "(",&
                               My13%nodal_param(I)%Numvalues,"(F10.6,X))"
                        WRITE(WRITEUNIT,TRIM(MultipleValueFormat)) &
                            (My13%nodal_param(I)%DefaultValue,k=1,&
                            My13%nodal_param(I)%Numvalues)
                    ENDIF
                 ENDDO


                 DO I = 1,My13%NumAttributes
                    WRITE(WRITEUNIT,'(A)') TRIM(My13%nodal_param(I)%Attribute)
                    WRITE(WRITEUNIT,'(I0)') defaults(I)
                    IF(My13%nodal_param(I)%Numvalues.EQ.1)THEN
                        DO J = 1,My13%NumNodes
                            IF(My13%nodal_param(I)%values(J,1).NE.&
                                My13%nodal_param(I)%DefaultValue)THEN
                                WRITE(WRITEUNIT,'(I10,2X,F10.6)') J, My13%nodal_param(I)%values(J,1)
                            ENDIF
                        ENDDO
                    ELSEIF(My13%nodal_param(I)%Numvalues.GT.1)THEN
                        WRITE(MultipleValueFormat,'(A,I0,A)') "(I0,2X,",&
                            My13%nodal_param(I)%Numvalues,"(F10.6,X))"
                        DO J = 1,My13%NumNodes
                           IF(SUM(My13%nodal_param(I)%values(J,:)).NE.0d0)THEN
                                WRITE(WRITEUNIT,MultipleValueFormat) J,&
                                    (My13%nodal_param(I)%values(J,K),&
                                    K=1,My13%nodal_param(I)%Numvalues)
                           ENDIF
                        ENDDO
                    ENDIF
                ENDDO


            END SUBROUTINE

            !>Subroutine that selects the best value for the default value in
            !>a fort.13 file. This is done by first sorting all values for each
            !>parameter, then generating a unique list, and finally counting the
            !>number of times each item on the unique list appears.
            !>\author Zach Cobell
            SUBROUTINE Reduce13(My13,DETAILIN)
                IMPLICIT NONE

                !>The fort.13 variable to be optimized. It will be edited
                !>in place, so there is no need to specify an output
                !>variable.
                TYPE(Fort13),INTENT(INOUT) :: My13
                !>Optional: The user may wish for the code to print status
                !>information about the selection of parameters. This can be
                !>helpful in diagnosing obvious issues with your fort.13 file
                !>such as many values where few are expected.
                LOGICAL,INTENT(IN),OPTIONAL  :: DETAILIN

                INTEGER                    :: I
                INTEGER                    :: N
                REAL(8),ALLOCATABLE        :: A(:,:)
                REAL(8),ALLOCATABLE        :: V(:)
                REAL(8),ALLOCATABLE        :: TEMP(:)
                INTEGER,ALLOCATABLE        :: U(:)
                LOGICAL                    :: DETAIL

                IF(PRESENT(DETAILIN))THEN
                    DETAIL = DETAILIN
                ELSE
                    DETAIL = .FALSE.
                ENDIF

                ALLOCATE(A(1:My13%NumAttributes,1:My13%NumNodes))
                ALLOCATE(U(1:My13%NumAttributes))
                ALLOCATE(V(1:My13%NumAttributes))
                N = My13%NumNodes
                ALLOCATE(TEMP(1:N))

                !...Use HEAPSORT to sort all parameters in ascending order
                !   very quickly.
                DO I = 1,My13%NumAttributes
                    IF(My13%nodal_param(I)%Numvalues.LT.2)THEN
                        A(I,:) = My13%nodal_param(I)%values(:,1)
                        IF(DETAIL)WRITE(*,'(3A,$)') "Start Heapsort on ",&
                        TRIM(My13%nodal_param(I)%attribute),"..."
                        TEMP(1:N) = A(I,1:N)
                        CALL HEAPSORT(My13%NumNodes,TEMP)
                        A(I,1:N) = TEMP(1:N)
                        IF(DETAIL)WRITE(*,'(A)') "done!"
                    ENDIF
                ENDDO

                IF(DETAIL)WRITE(*,'(A)') ""

                !...Count Unique Values
                IF(DETAIL) WRITE(*,'(A)') "Counting unique values..."
                DO I = 1,My13%NumAttributes
                IF(My13%nodal_param(I)%NumValues.LT.2)THEN
                        TEMP(1:N) = A(I,1:N)
                        CALL Unique(TEMP,My13%NumNodes,U(I))
                        IF(DETAIL)WRITE(*,'(2A,I0)') TRIM(My13%nodal_param(I)%attribute),&
                            ": ",U(I)
                    ENDIF
                ENDDO

                !...Find Frequency
                IF(DETAIL) WRITE(*,'(A)') ""
                IF(DETAIL) WRITE(*,'(A)') "Finding value with maximum frequency..."
                DO I = 1, My13%NumAttributes
                    IF(My13%nodal_param(I)%NumValues.LT.2)THEN
                        TEMP(1:N) = A(I,1:N)
                        CALL Frequency(TEMP,My13%NumNodes,U(I),V(I))
                        IF(DETAIL)WRITE(*,'(3A,F0.6)') "New default for ",&
                            TRIM(My13%nodal_param(I)%attribute),": ",V(I)
                        My13%nodal_param(I)%DefaultValue = V(I)
                    ENDIF
                ENDDO

            END SUBROUTINE

!................................................................................
!
!                      ADCIRC Output
!
!................................................................................

            !>Subroutine to write ADCIRC Output to file in ASCII format from an
            !>ADCIRCOutput variable with sparse or full format depending user
            !>selection
            !>\author Zach Cobell
            SUBROUTINE WriteADCIRCOutputASCII(OutputFile,MyOutput,USparse)
                IMPLICIT NONE
                !>Name of file to be written
                CHARACTER(*),INTENT(IN)       :: OutputFile
                !>Variable containing ADCIRC output information
                TYPE(ADCIRCOutput),INTENT(IN) :: MyOutput
                !>Optionally select sparse or full output. Default: Full
                LOGICAL,INTENT(IN),OPTIONAL   :: USparse

                CHARACTER(200)     :: WRITEFORMAT
                INTEGER            :: I
                INTEGER            :: J
                INTEGER            :: K
                INTEGER            :: WRITEUNIT
                LOGICAL            :: Sparse

                WRITEUNIT = GETFREEUNIT()

                !...DEFAULT TO NON-SPARSE
                IF(PRESENT(USparse))THEN
                    SPARSE = USPARSE
                ELSE
                    SPARSE = .FALSE.
                ENDIF

                WRITE(WRITEFORMAT,'(A,I0,A)') "(2X,I8,", &
                                  MyOutput%NumValues,"(2X,1PE20.10E3))"

                OPEN(FILE=TRIM(OutputFile),UNIT=WRITEUNIT,ACTION="WRITE")
                WRITE(WRITEUNIT,'(A)') TRIM(MyOutput%Title)
                WRITE(WRITEUNIT,&
                        '(I0,2X,I0,2X,F0.6,2X,I0,2X,I0,2X,A,2X,I0)') &
                    MyOutput%NumTimeSteps,MyOutput%NumNodes,&
                    DBLE(MyOutput%dt),INT(MyOutput%dt),&
                    MyOutput%NumValues,"FileFmtVersion:",&
                    MyOutput%Fileformat
                DO I = 1,MyOutput%NumTimeSteps
                    IF(Sparse)THEN
                        WRITE(WRITEUNIT,*) MyOutput%Output(I)%Time,&
                            MyOutput%Output(I)%TS, &
                            MyOutput%Output(I)%NumNonDefault,&
                            MyOutput%Output(I)%DefaultValue
                    ELSE
                        WRITE(WRITEUNIT,*) MyOutput%Output(I)%Time,&
                            MyOutput%Output(I)%TS
                    ENDIF
                    IF(Sparse)THEN
                        DO J = 1,MyOutput%NumNodes
                            IF(ALL(MyOutput%Output(I)%Values(J,:).NE.&
                                    MyOutput%Output(I)%DefaultValue))THEN
                                WRITE(WRITEUNIT,WRITEFORMAT) &
                                    (MyOutput%Output(I)%Values(J,K), &
                                     K=1,MyOutput%NumValues)
                            ENDIF
                        ENDDO
                    ELSE
                        DO J = 1,MyOutput%NumNodes
                            WRITE(WRITEUNIT,WRITEFORMAT) J, &
                                (MyOutput%Output(I)%Values(J,K),K=1,&
                                 MyOutput%NumValues)
                        ENDDO
                    ENDIF
                ENDDO
                CLOSE(WRITEUNIT)

            END SUBROUTINE

            !>Used for initializing a netCDF output file. If the file is not
            !>to be netCDF (determined by the .nc extension), then an ASCII file
            !>is written when this routine is called.
            !>\author Zach Cobell
            SUBROUTINE InitializeADCIRCOutput(Filename,MyOutput,&
                    MyMesh)
                IMPLICIT NONE
                !>Filename to be written. Also used to determine format
                CHARACTER(*),INTENT(IN)          :: filename
                !>ADCIRC Output data to be written
                TYPE(ADCIRCOUTPUT),INTENT(IN)    :: MyOutput
                !>Optional: If using netCDF, ADCIRC mesh data can be written to
                !>the file as would be in a standard ADCIRC output file. This is
                !>recommended, however, it is not required.
                TYPE(GRID),INTENT(IN),OPTIONAL   :: MyMesh

                IF(INDEX(filename,".nc").GT.0)THEN
                    IF(PRESENT(MyMesh))THEN
                        CALL InitializeADCIRCNetCDFOutput(filename,MyOutput,&
                            MyMesh)
                    ELSE
                        CALL InitializeADCIRCNetCDFOutput(filename,MyOutput)
                    ENDIF
                ELSE
                    CALL WRITEADCIRCOUTPUTASCII(filename,MyOutput) !...Temporarily preserves old behavior
                !    CALL InitializeADCIRCASCIIOutput(filename,MyOutput,&
                !        FileUnit)
                ENDIF
                RETURN

            END SUBROUTINE

            !>Subroutine to initialize an ADCIRC netCDF output file. This will
            !>set up the variables and write the mesh data if it is provided.
            !>Note that the user should not have the need to call this routine
            !>and should instead use the WriteADCIRCOutput routine to have features
            !>handled automatically
            !>\author Zach Cobell
            SUBROUTINE InitializeADCIRCNetCDFOutput(filename,MyOutput,&
                    MyMesh)
                IMPLICIT NONE
                !>Filename to be written
                CHARACTER(*),INTENT(IN)        :: filename
                !>Output data to be written
                TYPE(ADCIRCOUTPUT),INTENT(IN)  :: MyOutput
                !>Optional: Mesh data to be written.
                TYPE(GRID),INTENT(IN),OPTIONAL :: MyMesh

                CHARACTER(200)                 :: NC_VARIABLE
                CHARACTER(200)                 :: NC_VARIABLE2

                INTEGER                        :: I
                INTEGER                        :: NCOL
                INTEGER                        :: NCID
                INTEGER                        :: VAR_IDX
                INTEGER                        :: NC_FILETYPE
                INTEGER                        :: VARID_TIME
                INTEGER                        :: VARID_X
                INTEGER                        :: VARID_Y
                INTEGER                        :: VARID_DEPTH
                INTEGER                        :: VARID_ELEMENT
                INTEGER                        :: VARID_ADC(2)
                INTEGER                        :: DIMID_NODE
                INTEGER                        :: DIMID_NELE
                INTEGER                        :: DIMID_NVERTEX
                INTEGER                        :: DIMID_SINGLE
                INTEGER                        :: DIMID_TIME
                INTEGER,ALLOCATABLE            :: HOLDER2(:,:)

                REAL(8),ALLOCATABLE            :: HOLDER(:)

                LOGICAL                        :: exists

#ifndef _NETCDF
                WRITE(*,'(A)') "ERROR: ADCModules not compiled for "//&
                               "NetCDF."
                STOP
#else

                NC_FILETYPE=NF90_CLOBBER
#ifdef HAVE_NETCDF4
                NC_FILETYPE=IOR(NF90_HDF5,NF90_CLASSIC_MODEL)
#endif

                CALL CHECK(NF90_CREATE(TRIM(filename),NC_FILETYPE,NCID))
                CALL CHECK(NF90_DEF_DIM(NCID,'time',NF90_UNLIMITED,&
                    DIMID_TIME))
                CALL CHECK(NF90_DEF_VAR(NCID,'time',NF90_DOUBLE,&
                    DIMID_TIME,VARID_TIME))

                NC_VARIABLE  = MyOutput%NC_VARIABLE1
                NC_VARIABLE2 = MyOutput%NC_VARIABLE2
                NCOL         = MyOutput%NumValues

                DO I = 1,SIZE(NETCDF_TYPES)
                    IF(TRIM(NC_VARIABLE).EQ.NETCDF_TYPES(I))THEN
                        VAR_IDX = I
                        EXIT
                    ENDIF
                    IF(I.EQ.SIZE(NETCDF_TYPES))THEN
                        WRITE(*,'(A)') "ERROR: Invalid NetCDF variable."
                        STOP
                    ENDIF
                ENDDO

                !...Start putting mesh variables if mesh was input
                !   Note: Not full ADCIRC implementation
                CALL CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,"dt",MyOutput%dt))
                IF(PRESENT(MYMESH))THEN
                    CALL CHECK(NF90_DEF_DIM(NCID,'node',MyMesh%NumNodes,DIMID_NODE))
                    CALL CHECK(NF90_DEF_DIM(NCID,'nele',MyMesh%NumElements,DIMID_NELE))
                    CALL CHECK(NF90_DEF_DIM(NCID,'nvertex',3,DIMID_NVERTEX))
                    CALL CHECK(NF90_DEF_DIM(NCID,'single',1,DIMID_SINGLE))

                    CALL CHECK(NF90_DEF_VAR(NCID,'x',NF90_DOUBLE,DIMID_NODE,VARID_X))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_X,'long_name','longitude'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_X,'standard_name','longitude'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_X,'units','degrees_east'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_X,'positive','east'))

                    CALL CHECK(NF90_DEF_VAR(NCID,'y',NF90_DOUBLE,DIMID_NODE,VARID_Y))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_Y,'long_name','latitude'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_Y,'standard_name','latitude'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_Y,'units','degrees_north'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_Y,'positive','north'))

                    CALL CHECK(NF90_DEF_VAR(NCID,'element',NF90_INT,(/DIMID_NVERTEX,DIMID_NELE/),VARID_ELEMENT))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ELEMENT,'long_name','element'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ELEMENT,'standard_name','face_node_connectivity'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ELEMENT,'units','nondimensional'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ELEMENT,'start_index',1))

                    CALL CHECK(NF90_DEF_VAR(NCID,'depth',NF90_DOUBLE,VARID_DEPTH))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'long_name','distance_from_geoid'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'standard_name','depth_below_geoid'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'location','node'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'mesh','adcirc_mesh'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_DEPTH,'units','m'))

#ifdef HAVE_NETCDF4
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_X,1,1,2))
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_Y,1,1,2))
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_DEPTH,1,1,2))
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_ELEMENT,1,1,2))
#endif

                ENDIF

                !...Adcirc Variable
                CALL CHECK(NF90_DEF_VAR(NCID,NETCDF_TYPES(VAR_IDX),NF90_DOUBLE,(/DIMID_NODE,DIMID_TIME/),VARID_ADC(1)))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'_FillValue',-99999.0D0))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'long_name',NC_LONGNAME(VAR_IDX)))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'standard_name',NC_STDNAME(VAR_IDX)))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'coordinates','time y x'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'location','node'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'mesh','adcirc_mesh'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'units','metric'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'positive','east'))
                CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(1),'dry_Value',-99999.0d0))
                IF(NCOL.EQ.2)THEN
                    CALL CHECK(NF90_DEF_VAR(NCID,NETCDF_TYPES(VAR_IDX+1),NF90_DOUBLE,(/DIMID_NODE,DIMID_TIME/),VARID_ADC(2)))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'_FillValue',-99999.0D0))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'long_name',NC_LONGNAME(VAR_IDX+1)))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'standard_name',NC_STDNAME(VAR_IDX+1)))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'coordinates','time y x'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'location','node'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'mesh','adcirc_mesh'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'units','metric'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'positive','east'))
                    CALL CHECK(NF90_PUT_ATT(NCID,VARID_ADC(2),'dry_Value',-99999.0d0))
                ENDIF
#ifdef HAVE_NETCDF4
                CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_ADC(1),1,1,2))
                IF(NCOL.EQ.2)THEN
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,VARID_ADC(2),1,1,2))
                ENDIF
#endif

                !...Finished with definitions
                CALL CHECK(NF90_ENDDEF(NCID))

                IF(PRESENT(MyMesh))THEN
                    ALLOCATE(HOLDER(1:MyMesh%NumNodes))
                    HOLDER(:) = MyMesh%Nodes(:,1)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID_X,Holder))
                    HOLDER(:) = MyMesh%Nodes(:,2)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID_Y,Holder))
                    HOLDER(:) = MyMesh%Nodes(:,3)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID_DEPTH,Holder))
                    DEALLOCATE(HOLDER)
                    ALLOCATE(HOLDER2(1:3,1:MyMesh%NumElements))
                    Holder2(1,:) = MyMesh%Conn(:,1)
                    Holder2(2,:) = MyMesh%Conn(:,2)
                    Holder2(3,:) = MyMesh%Conn(:,3)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID_ELEMENT,Holder2))
                ENDIF

                !...Close NetCDF file
                CALL CHECK(NF90_CLOSE(NCID))

                RETURN
#endif

            END SUBROUTINE

            !>This routine writes the next output interval for an ADCIRC netCDF
            !>output file. If it is called for an ADCII file, the entire file
            !>will be written to the specified file.
            !>\author Zach Cobell
            SUBROUTINE WriteADCIRCOutput(Filename,MyOutput,MyMesh,FirstCall)
                IMPLICIT NONE

                !>File to be written
                CHARACTER(*),INTENT(IN)        :: Filename
                !>Output data to be written
                TYPE(ADCIRCOUTPUT),INTENT(IN)  :: MyOutput
                !>Optional: Mesh data to be written
                TYPE(GRID),INTENT(IN),OPTIONAL :: MyMesh
                !>Variable to inform the code if this is the first time the
                !>routine is being called and if initialization is required. It
                !>will be set to false upon exit automatically
                LOGICAL,INTENT(INOUT)          :: FirstCall
                LOGICAL                        :: exists

                !...Check if initialization has occured
                IF(FirstCall)THEN
                    IF(PRESENT(MyMesh))THEN
                        CALL InitializeADCIRCOutput(Filename,MyOutput,MyMesh)
                    ELSE
                        CALL InitializeADCIRCOutput(Filename,MyOutput)
                    ENDIF
                    FirstCall = .FALSE.
                ENDIF

                IF(INDEX(Filename,".nc").GT.0)THEN
                    CALL WriteADCIRCOutputNETCDF(Filename,MyOutput)
                ELSE
                    CALL WriteADCIRCOutputASCII(Filename,MyOutput)
                ENDIF

                RETURN

            END SUBROUTINE

            !>This subroutine writes the next output cycle to an ADCIRC
            !>netCDF formatted file. The user should not have need to call this
            !>routine.
            !>\author Zach Cobell
            SUBROUTINE WriteADCIRCOutputNETCDF(Filename,MyOutput)
                IMPLICIT NONE

                !>File to be written
                CHARACTER(*),INTENT(IN)       :: Filename
                !>Output data to be written
                TYPE(ADCIRCOUTPUT),INTENT(IN) :: MYOUTPUT

                INTEGER                       :: NCID
                INTEGER                       :: VARID1
                INTEGER                       :: VARID2
                INTEGER                       :: NCOLS
                INTEGER                       :: NATT
                INTEGER                       :: NVAR
                INTEGER                       :: NDIM
                INTEGER                       :: DIMID_TIME
                INTEGER                       :: VARID_TIME
                INTEGER                       :: NC_FORMAT
                INTEGER                       :: NSNAP
                INTEGER                       :: NC_COUNT(2)
                INTEGER                       :: NC_START(2)

                REAL(8),ALLOCATABLE           :: HOLDER(:)
                REAL(8)                       :: Time(1)

#ifdef _NETCDF
                !...Open the netcdf file
                CALL CHECK(NF90_OPEN(TRIM(FILENAME),NF90_WRITE,NCID))

                !...Find the netcdf variable
                CALL GetNETCDFVarID(NCID,VARID1,VARID2,NCOLS)

                !...Find out how many datasets currently in file
                CALL CHECK(NF90_INQUIRE(NCID,NDIM,NVAR,NATT,DIMID_TIME,NC_FORMAT))
                CALL CHECK(NF90_INQUIRE_DIMENSION(NCID,DIMID_TIME,LEN=NSNAP))
                CALL CHECK(NF90_INQ_VARID(NCID,"time",VARID_TIME))

                !...Increment to the next snap
                NSNAP = NSNAP + 1

                !...Put new time into file
                Time(1) = MyOutput%Output(1)%Time
                CALL CHECK(NF90_PUT_VAR(NCID,VARID_TIME,Time,(/NSNAP/),(/1/)))

                !...Put the record into the file
                ALLOCATE(HOLDER(1:MyOutput%NumNodes))
                HOLDER(:) = MyOutput%Output(1)%Values(:,1)
                NC_COUNT = (/MyOutput%NumNodes, 1/)
                NC_START = (/1,NSNAP/)

                CALL CHECK(NF90_PUT_VAR(NCID,VARID1,HOLDER,NC_START,NC_COUNT))
                IF(NCOLS.EQ.2)THEN
                    HOLDER(:) = MyOutput%Output(1)%Values(:,2)
                    CALL CHECK(NF90_PUT_VAR(NCID,VARID2,HOLDER,NC_START,NC_COUNT))
                ENDIF
                DEALLOCATE(HOLDER)

                !...Close NetCDF
                CALL CHECK(NF90_CLOSE(NCID))

                RETURN
#endif

            END SUBROUTINE

            !>This routine will read an entire ADCIRC ASCII output file. It can
            !>be used for global or station output files.
            !>\author Zach Cobell
            SUBROUTINE ReadADCIRCOutput(filename,MyOutput)
                IMPLICIT NONE

                !>File to be read
                CHARACTER(*),INTENT(IN) :: filename
                !>Variable to store data read from ADCIRC output file
                TYPE(AdcircOutput),INTENT(OUT) :: MyOutput

                CHARACTER(500) :: JunkC
                INTEGER :: I
                INTEGER :: J
                INTEGER :: K
                INTEGER :: JunkI
                INTEGER :: TempN
                INTEGER :: READUNIT
                REAL(8),ALLOCATABLE :: TempV(:)
                REAL(8) :: JunkR
                LOGICAL :: exists
                LOGICAL :: sparse

                READUNIT = GETFREEUNIT()

                INQUIRE(FILE=TRIM(filename),EXIST=exists)
                IF(.NOT.exists)THEN
                    WRITE(*,'(A)') ""
                    WRITE(*,'(A)') "ADCIRC output file doesn't exist."
                    STOP
                ENDIF

                OPEN(FILE=TRIM(filename),UNIT=READUNIT,ACTION="READ")
                READ(READUNIT,'(A)') MyOutput%title
                READ(READUNIT,'(A)') JunkC

                MyOutput%NumTimeSteps = -999999
                MyOutput%NumNodes = -999999
                MyOutput%dt = -999999
                MyOutput%NumValues = -999999
                MyOutput%Fileformat = -999999

                READ(JunkC,*,END=100,ERR=100) MyOutput%NumTimeSteps,MyOutput%NumNodes,JunkR,&
                    MyOutput%dt,MyOutput%NumValues,JunkC,&
                    MyOutput%Fileformat
100             CONTINUE
                IF( ( MyOutput%NumTimeSteps.LE.-900000 ) .OR. ( MyOutput%NumNodes.LE.-900000 ) &
                    .OR. ( MyOutput%dt.LE.-900000 ) )THEN
                    WRITE(*,'(A)') "Error reading file."
                    STOP
                ELSEIF(MyOutput%NumValues.LE.-900000)THEN
                    MyOutput%NumValues = 1
                ELSEIF(MyOutput%Fileformat.LE.-900000)THEN
                    MyOutput%FileFormat = 1050624
                ENDIF

                !...Determine output format (Sparse/Full)
                READ(READUNIT,'(A)') JunkC
                sparse = .FALSE.
                READ(JunkC,*,END=200,ERR=200) JunkR,JunkI,JunkI,JunkR
                sparse = .TRUE.
200             CONTINUE
                BACKSPACE(READUNIT)
                ALLOCATE(MyOutput%Output(1:MyOutput%NumTimeSteps))
                ALLOCATE(TempV(1:MyOutput%NumValues))
                DO K = 1,MyOutput%NumTimeSteps
                    IF(sparse)THEN
                        READ(READUNIT,*) MyOutput%Output(K)%Time,MyOutput%Output(K)%TS,&
                            MyOutput%Output(K)%NumNonDefault,MyOutput%Output(K)%DefaultValue
                    ELSE
                        READ(READUNIT,*) MyOutput%Output(K)%Time,MyOutput%Output(K)%TS
                        MyOutput%Output(K)%NumNonDefault = MyOutput%NumNodes
                        MyOutput%Output(K)%DefaultValue = -99999d0
                    ENDIF
                    ALLOCATE(MyOutput%Output(K)%Values(MyOutput%NumNodes,1:MyOutput%NumValues))
                    MyOutput%Output(K)%Values(1:MyOutput%NumNodes,1:MyOutput%NumValues) = &
                        MyOutput%Output(K)%DefaultValue
                    DO I = 1,MyOutput%Output(K)%NumNonDefault
                        READ(READUNIT,*) TempN,(TempV(J),J=1,MyOutput%NumValues)
                        MyOutput%Output(K)%Values(TempN,:) = TempV(:)
                    ENDDO
                ENDDO

                CLOSE(READUNIT)

                RETURN

            END SUBROUTINE

!................................................................................
!
!                      ADCIRC Binary Hot Start
!
!................................................................................

        !>Subroutine to read an ADCIRC 2D binary hot start file.
        !>\author Zach Cobell
        SUBROUTINE ReadHotStart2D(Filename,HSOut)
            IMPLICIT NONE

            !>File to be read
            CHARACTER(*),INTENT(IN)      :: FILENAME
            !>Variable containing 2D hot start data
            TYPE(HOTSTART2D),INTENT(OUT) :: HSOut

            INTEGER                 :: IHOT
            INTEGER                 :: IHOTSTP


            IHOT = GETFREEUNIT()
            OPEN(FILE=TRIM(Filename),UNIT=IHOT,ACTION="READ",&
                ACCESS="DIRECT",RECL=8,STATUS="OLD")

            !...Begin hot start read
            IHOTSTP = 1
            READ(IHOT,REC=IHOTSTP) HSOUT%InputFileFmtVn ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IMHS           ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%TimeLoc        ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%ITHS           ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NP_G_IN        ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NE_G_IN        ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NP_A_IN        ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NE_A_IN        ; IHOTSTP = IHOTSTP + 1
            ALLOCATE(HSOUT%ETA1(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%ETA2(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%ETADisc(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%UU2(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%VV2(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%NNODECODE(1:HSOUT%NP_G_IN))
            ALLOCATE(HSOUT%NOFF(1:HSOUT%NE_G_IN))
            IF(HSOUT%IMHS.EQ.10)ALLOCATE(HSOUT%CH1(1:HSOUT%NP_G_IN))

            CALL BinaryRead2D(HSOUT%ETA1,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryRead2D(HSOUT%ETA2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryRead2D(HSOUT%ETADisc,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryRead2D(HSOUT%UU2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryRead2D(HSOUT%VV2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            IF(HSOUT%IMHS.EQ.10)CALL BinaryRead2D(HSOUT%CH1,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryIntRead2D(HSOUT%NNODECODE,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryIntRead2D(HSOUT%NOFF,HSOUT%NE_G_IN,IHOT,IHOTSTP)
            READ(IHOT,REC=IHOTSTP) HSOUT%IESTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUE ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IVSTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUV ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%ICSTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUC ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IPSTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IWSTP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUM ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGEP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUGE ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGVP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUGV ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGCP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUGC ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGPP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%IGWP ; IHOTSTP = IHOTSTP + 1
            READ(IHOT,REC=IHOTSTP) HSOUT%NSCOUGW ; IHOTSTP = IHOTSTP + 1
            !...End hot start read
            CLOSE(IHOT)
            RETURN

        END SUBROUTINE

        !>Subroutine to read a real(8) array from a ADCIRC binary hot start file
        !>\author Zach Cobell
        SUBROUTINE BinaryRead2D(array,length,lun,counter)
            IMPLICIT NONE
            !>Array containing the information read from the hot start file
            REAL(8),INTENT(OUT),DIMENSION(:) :: ARRAY(*)
            !>Length of the array that will be read
            INTEGER,INTENT(IN)               :: LENGTH
            !>FORTRAN unit number to read
            INTEGER,INTENT(IN)               :: LUN
            !>Counter that keeps track of binary record position
            INTEGER,INTENT(INOUT)            :: COUNTER

            INTEGER                          :: I
            REAL(8)                          :: TempR

            DO I = 1,LENGTH
                READ(LUN,REC=COUNTER) ARRAY(I)
                COUNTER = COUNTER + 1
            ENDDO

            RETURN

        END SUBROUTINE

        !>Subroutine to read an integer array form an ADCIRC binary hot start file
        !>\author Zach Cobell
        SUBROUTINE BinaryIntRead2D(array,length,lun,counter)
            IMPLICIT NONE
            !>Array containing the information read from the hot start file
            INTEGER,INTENT(OUT),DIMENSION(:) :: ARRAY(*)
            !>Length of the array that will be read
            INTEGER,INTENT(IN)               :: LENGTH
            !>FORTRAN unit number to read
            INTEGER,INTENT(IN)               :: LUN
            !>Counter that keeps track of binary record position
            INTEGER,INTENT(INOUT)            :: COUNTER

            INTEGER                          :: I

            DO I = 1,LENGTH
                READ(LUN,REC=COUNTER) ARRAY(I)
                COUNTER = COUNTER + 1
            ENDDO

            RETURN

        END SUBROUTINE

        !>Subroutine to write a 2D ADCIRC hot start file
        !>\author Zach Cobell
        SUBROUTINE WriteHotStart2D(Filename,HSOut)
            IMPLICIT NONE
            !>File to be written
            CHARACTER(*),INTENT(IN)      :: FILENAME
            !>2D hot start data to be written
            TYPE(HOTSTART2D),INTENT(IN)  :: HSOut

            INTEGER                      :: IHOT
            INTEGER                      :: IHOTSTP

            IHOT = GETFREEUNIT()
            OPEN(FILE=TRIM(Filename),UNIT=IHOT,ACTION="WRITE",&
                ACCESS="DIRECT",RECL=8)

            !...Begin hot start write
            IHOTSTP = 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%InputFileFmtVn ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IMHS           ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%TimeLoc        ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%ITHS           ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NP_G_IN        ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NE_G_IN        ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NP_A_IN        ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NE_A_IN        ; IHOTSTP = IHOTSTP + 1
            CALL BinaryWrite2D(HSOUT%ETA1,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryWrite2D(HSOUT%ETA2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryWrite2D(HSOUT%ETADisc,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryWrite2D(HSOUT%UU2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryWrite2D(HSOUT%VV2,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            IF(HSOUT%IMHS.EQ.10)CALL BinaryWrite2D(HSOUT%CH1,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryIntWrite2D(HSOUT%NNODECODE,HSOUT%NP_G_IN,IHOT,IHOTSTP)
            CALL BinaryIntWrite2D(HSOUT%NOFF,HSOUT%NE_G_IN,IHOT,IHOTSTP)
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IESTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUE ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IVSTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUV ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%ICSTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUC ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IPSTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IWSTP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUM ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGEP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUGE ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGVP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUGV ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGCP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUGC ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGPP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%IGWP ; IHOTSTP = IHOTSTP + 1
            WRITE(IHOT,REC=IHOTSTP) HSOUT%NSCOUGW ; IHOTSTP = IHOTSTP + 1
            !...End hot start read
            RETURN

        END SUBROUTINE

        !>Subroutine to write a real(8) array to a binary hot start file
        !>\author Zach Cobell
        SUBROUTINE BinaryWrite2D(array,length,lun,counter)
            IMPLICIT NONE
            !>Array to write to file
            REAL(8),INTENT(IN),DIMENSION(:)  :: ARRAY(*)
            !>Length of array to write
            INTEGER,INTENT(IN)               :: LENGTH
            !>FORTRAN unit number to write to
            INTEGER,INTENT(IN)               :: LUN
            !>Binary record position counter
            INTEGER,INTENT(INOUT)            :: COUNTER
            
            INTEGER                          :: I
            REAL(8)                          :: TempR

            DO I = 1,LENGTH
                WRITE(LUN,REC=COUNTER) ARRAY(I)
                COUNTER = COUNTER + 1
            ENDDO

            RETURN

        END SUBROUTINE

        !>Subroutine to write an integer array to a binary hot start file
        !>\author Zach Cobell
        SUBROUTINE BinaryIntWrite2D(array,length,lun,counter)
            IMPLICIT NONE
            !>Array to write to file
            INTEGER,INTENT(IN),DIMENSION(:)  :: ARRAY(*)
            !>Length of array to write
            INTEGER,INTENT(IN)               :: LENGTH
            !>FORTRAN unit number to write to
            INTEGER,INTENT(IN)               :: LUN
            !>Binary record position counter
            INTEGER,INTENT(INOUT)            :: COUNTER
            INTEGER                          :: I

            DO I = 1,LENGTH
                WRITE(LUN,REC=COUNTER) ARRAY(I)
                COUNTER = COUNTER + 1
            ENDDO

            RETURN

        END SUBROUTINE


!................................................................................
!
!                      Misc
!
!................................................................................

            !>This function will return the index of a particular attribute name
            !>in a fort.13 file, allowing the user to quickly search for
            !>a specific index.
            !>\author Zach Cobell
            INTEGER FUNCTION FindAttributeIndex(attributename,My13)
                IMPLICIT NONE
                !>Name of attribute to be searched for
                CHARACTER(*),INTENT(IN)   :: attributename
                !>fort.13 data container to locate attribute
                TYPE(fort13),INTENT(IN)   :: My13
                INTEGER        :: I

                FindAttributeIndex = -1
                DO I = 1,My13%NumAttributes
                    IF(TRIM(ADJUSTL(My13%nodal_param(I)%Attribute)).EQ.&
                        TRIM(ADJUSTL(attributename)))THEN
                            FindAttributeIndex = I
                            EXIT
                    ENDIF
                ENDDO

                RETURN

            END FUNCTION

            !>Subroutine to display a progress bar
            !>\author Zach Cobell
            SUBROUTINE ShowBar(ntotal, now, ncount)
                IMPLICIT NONE
                !>Iteration that will be indexed to 100% completion
                INTEGER,INTENT(IN)   :: ntotal
                !>Current iteration
                INTEGER,INTENT(IN)   :: now
                !>A variable passed back and forth between this routine.
                !>The user should initially set it to zero and then
                !>leave it alone. This subroutine will manipulate it as needed.
                INTEGER,INTENT(OUT)  :: ncount

                INTEGER              :: nout
                INTEGER              :: n
                IF(now.EQ.1)THEN
                    WRITE(*,'(A)') "0%                      50%                      100%"
                    WRITE(*,'(A,$)') "|"
                ENDIF
                nout = INT(DBLE(ntotal/50))
                n = MOD(ntotal,50)
                IF (n.NE.0) nout = nout + 1
                IF(now.GE.ncount*nout)THEN
                    ncount = ncount + 1
                    SELECT CASE(ncount)
                        CASE(10, 20, 30, 40)
                            WRITE(6,'(A,$)') '+'
                        CASE(50)
                            WRITE(6,'(A)') "|"
                        CASE DEFAULT
                            WRITE(6,'(A,$)') '-'
                    END SELECT
                ENDIF

                RETURN

            END SUBROUTINE

            !>Subroutine that will evoke a function to quit
            !>a program. It will pause, asking the user to press
            !>enter to exit before stopping execution.
            !>\author Zach Cobell
            SUBROUTINE Quit
                WRITE(*,'(A)') ""
                WRITE(*,'(A)') "<--Press enter to exit-->"
                READ(*,*)
                STOP
            END SUBROUTINE

            !>This function will transform a single point to UTM coordinates
            !>from Geographic coordinates.
            !>\author Zach Cobell
            !>\author Seizo Tanaka
            SUBROUTINE LatLon2UTM(MyX,MyY,MyZone,MyHorizontal,UTM_X,UTM_Y)

                IMPLICIT NONE

                INTRINSIC :: DACOS

                !..IN/OUT Variables
                !>Horizontal system to be used for input and output
                INTEGER,INTENT(IN)  :: MyHorizontal
                !>UTM Zone
                INTEGER,INTENT(IN)  :: MyZone
                !>X coordinate in decimal degrees
                REAL(8),INTENT(IN)  :: MyX
                !>Y coordinate in decimal degrees
                REAL(8),INTENT(IN)  :: MyY
                !>Output X coordinate in UTM coordinates
                REAL(8),INTENT(OUT) :: UTM_X
                !>Output Y coordinate in UTM coordinates
                REAL(8),INTENT(OUT) :: UTM_Y

                !...Local Variables
                REAL(8) :: DLat
                REAL(8) :: DLon
                REAL(8) :: RLat
                REAL(8) :: RLon
                REAL(8) :: X
                REAL(8) :: Y
                REAL(8),PARAMETER :: UTMScaleFactor = 0.9996d0
                REAL(8) :: A
                REAL(8) :: B
                REAL(8) :: PI
                REAL(8) :: DN
                REAL(8) :: SALPHA
                REAL(8) :: SBETA
                REAL(8) :: SGAMMA
                REAL(8) :: SDELTA
                REAL(8) :: SEPSILON
                REAL(8) :: SLENGTH
                REAL(8) :: CMERIDIAN
                REAL(8) :: SEP2
                REAL(8) :: SNU2
                REAL(8) :: SN
                REAL(8) :: T
                REAL(8) :: T2
                REAL(8) :: TMP
                REAL(8) :: S1
                REAL(8) :: SL
                REAL(8) :: SL3COEF
                REAL(8) :: SL4COEF
                REAL(8) :: SL5COEF
                REAL(8) :: SL6COEF
                REAL(8) :: SL7COEF
                REAL(8) :: SL8COEF
                INTEGER :: I

                SELECT CASE(MyHorizontal)
                    CASE(1)
                       A = 6378137.d0      ! Equatorial Radius
                       B = 6356752.3141d0  ! Polar Radius
                    CASE(2)
                       A = 6378137.d0      ! Equatorial Radius
                       B = 6356752.3142d0  ! Polar Radius
                    CASE(3)
                       A = 6378135.d0      ! Equatorial Radius
                       B = 6356750.5000d0  ! Polar Radius
                   CASE DEFAULT
                       WRITE(*,'(A)') "Invalid Horizontal System."
                       STOP
                END SELECT

                PI = DACOS(-1.0D0)

                DLON = MyX
                DLAT = MyY
                RLAT = DLAT * PI / 180.0D0
                RLON = DLON * PI / 180.0D0
                DN = (A-B) / (A+B)
                SALPHA = ((A+B)/2.D0) * ( 1.D0 + DN**(2)/4.D0 + DN**(4)/ 64.D0 )
                SBETA = ( -3.D0*DN/2.D0 ) + ( 9.D0*DN**(3)/16.D0 ) + (-3.D0*DN**(5)/32.D0)
                SGAMMA = ( 15.D0*DN**2)/16.D0 - (15.D0*DN**(4))/32.D0
                SDELTA = (-35.D0*DN**(3))/48.D0 + (105.D0*DN**(5))/256.D0
                SEPSILON = 315.D0 * DN ** (4) / 512.D0
                SLENGTH = SALPHA * ( RLAT + SBETA  * DSIN(2.D0*RLAT) + SGAMMA * DSIN(4.D0*RLAT) &
                                   + SDELTA * DSIN(6.D0*RLAT) + SEPSILON * DSIN(8.D0*RLAT) )
                CMERIDIAN = ( -183.D0 + MyZone*6.D0 ) * PI / 180.D0

                SEP2 = (A**(2) - B **(2)) / (B**(2))
                SNU2 = SEP2 * (DCOS(RLAT) ** (2))
                SN   = A*A / ( B*DSQRT(1.D0+SNU2) )
                T    = DTAN(RLAT)
                T2   = T * T
                TMP  = ( T2*T2*T2 ) - T**(6)
                SL   = RLON - CMERIDIAN
                SL3COEF = 1.D0 - T2 + SNU2
                SL4COEF = 5.D0 - T2 + 9.D0*SNU2 + 4.D0*SNU2*SNU2
                SL5COEF = 5.D0 - 18.D0*T2 + T2*T2 + 14.D0*SNU2 - 58.D0*  T2*SNU2
                SL6COEF = 61.D0 - 58.D0*T2 + T2*T2 + 270.D0*SNU2 - 330.D0*  T2*SNU2
                SL7COEF = 61.D0 - 479.D0*T2 + 179.D0*T2*T2 - T2*T2*T2
                SL8COEF = 1385.D0 - 3311.D0*T2 + 543.D0*T2*T2 - T2*T2*T2
                X = SN * DCOS(RLAT)                * SL                &
                    + SN * DCOS(RLAT)**(3) * SL3COEF * SL**(3) /    6.D0 &
                    + SN * DCOS(RLAT)**(5) * SL5COEF * SL**(5) /  120.D0 &
                    + SN * DCOS(RLAT)**(7) * SL7COEF * SL**(7) / 5040.D0
                Y = SLENGTH &
                    + T * SN * DCOS(RLAT)**(2)           * SL**(2) /     2.D0 &
                    + T * SN * DCOS(RLAT)**(4) * SL4COEF * SL**(4) /    24.D0 &
                    + T * SN * DCOS(RLAT)**(6) * SL6COEF * SL**(6) /   720.D0 &
                    + T * SN * DCOS(RLAT)**(8) * SL8COEF * SL**(8) / 40320.D0

                x = x * UTMScaleFactor + 500000.d0
                y = y * UTMScaleFactor

                IF(Y.LT.0.0D0)THEN
                    Y = Y + 10000000.D0
                ENDIF

                UTM_X = X
                UTM_Y = Y

                RETURN

            END SUBROUTINE

            !>This function will transform an ADCIRC grid to UTM coordinates
            !>from Geographic coordinates.
            !>\author Zach Cobell
            !>\author Seizo Tanaka
            SUBROUTINE LatLon2UTMGrid(MyMesh,MyZone,MyHorizontal,MyUTMMesh)

                IMPLICIT NONE

                INTRINSIC :: DACOS

                !..IN/OUT Variables
                !>ADCIRC grid in geographic coordiantes
                TYPE(grid),INTENT(IN) :: MyMesh
                !>Output ADCIRC grid in UTM coordinates
                TYPE(grid),INTENT(OUT) :: MyUTMMesh
                !>Horizontal system to use
                INTEGER,INTENT(IN) :: MyHorizontal
                !>UTM Zone to transform into
                INTEGER,INTENT(IN) :: MyZone

                !...Local Variables
                REAL(8) :: DLat
                REAL(8) :: DLon
                REAL(8) :: RLat
                REAL(8) :: RLon
                REAL(8) :: X
                REAL(8) :: Y
                REAL(8),PARAMETER :: UTMScaleFactor = 0.9996d0
                REAL(8) :: A
                REAL(8) :: B
                REAL(8) :: PI
                REAL(8) :: DN
                REAL(8) :: SALPHA
                REAL(8) :: SBETA
                REAL(8) :: SGAMMA
                REAL(8) :: SDELTA
                REAL(8) :: SEPSILON
                REAL(8) :: SLENGTH
                REAL(8) :: CMERIDIAN
                REAL(8) :: SEP2
                REAL(8) :: SNU2
                REAL(8) :: SN
                REAL(8) :: T
                REAL(8) :: T2
                REAL(8) :: TMP
                REAL(8) :: S1
                REAL(8) :: SL
                REAL(8) :: SL3COEF
                REAL(8) :: SL4COEF
                REAL(8) :: SL5COEF
                REAL(8) :: SL6COEF
                REAL(8) :: SL7COEF
                REAL(8) :: SL8COEF
                INTEGER :: I

                SELECT CASE(MyHorizontal)
                    CASE(1)
                       A = 6378137.d0      ! Equatorial Radius
                       B = 6356752.3141d0  ! Polar Radius
                    CASE(2)
                       A = 6378137.d0      ! Equatorial Radius
                       B = 6356752.3142d0  ! Polar Radius
                    CASE(3)
                       A = 6378135.d0      ! Equatorial Radius
                       B = 6356750.5000d0  ! Polar Radius
                   CASE DEFAULT
                       WRITE(*,'(A)') "Invalid Horizontal System."
                       STOP
                END SELECT

                PI = DACOS(-1.0D0)
                MyUTMMesh = MyMesh !...Copy structure of mesh to UTM mesh

                DO I = 1,MyUTMMesh%NumNodes
                    DLON = MYMESH%NODES(I,1)
                    DLAT = MYMESH%NODES(I,2)
                    RLAT = DLAT * PI / 180.0D0
                    RLON = DLON * PI / 180.0D0
                    DN = (A-B) / (A+B)
                    SALPHA = ((A+B)/2.D0) * ( 1.D0 + DN**(2)/4.D0 + DN**(4)/ 64.D0 )
                    SBETA = ( -3.D0*DN/2.D0 ) + ( 9.D0*DN**(3)/16.D0 ) + (-3.D0*DN**(5)/32.D0)
                    SGAMMA = ( 15.D0*DN**2)/16.D0 - (15.D0*DN**(4))/32.D0
                    SDELTA = (-35.D0*DN**(3))/48.D0 + (105.D0*DN**(5))/256.D0
                    SEPSILON = 315.D0 * DN ** (4) / 512.D0
                    SLENGTH = SALPHA * ( RLAT + SBETA  * DSIN(2.D0*RLAT) + SGAMMA * DSIN(4.D0*RLAT) &
                                       + SDELTA * DSIN(6.D0*RLAT) + SEPSILON * DSIN(8.D0*RLAT) )
                    CMERIDIAN = ( -183.D0 + MyZone*6.D0 ) * PI / 180.D0

                    SEP2 = (A**(2) - B **(2)) / (B**(2))
                    SNU2 = SEP2 * (DCOS(RLAT) ** (2))
                    SN   = A*A / ( B*DSQRT(1.D0+SNU2) )
                    T    = DTAN(RLAT)
                    T2   = T * T
                    TMP  = ( T2*T2*T2 ) - T**(6)
                    SL   = RLON - CMERIDIAN
                    SL3COEF = 1.D0 - T2 + SNU2
                    SL4COEF = 5.D0 - T2 + 9.D0*SNU2 + 4.D0*SNU2*SNU2
                    SL5COEF = 5.D0 - 18.D0*T2 + T2*T2 + 14.D0*SNU2 - 58.D0*  T2*SNU2
                    SL6COEF = 61.D0 - 58.D0*T2 + T2*T2 + 270.D0*SNU2 - 330.D0*  T2*SNU2
                    SL7COEF = 61.D0 - 479.D0*T2 + 179.D0*T2*T2 - T2*T2*T2
                    SL8COEF = 1385.D0 - 3311.D0*T2 + 543.D0*T2*T2 - T2*T2*T2
                    X = SN * DCOS(RLAT)                * SL                &
                        + SN * DCOS(RLAT)**(3) * SL3COEF * SL**(3) / 6.D0 &
                        + SN * DCOS(RLAT)**(5) * SL5COEF * SL**(5) / 120.D0 &
                        + SN * DCOS(RLAT)**(7) * SL7COEF * SL**(7) / 5040.D0
                    Y = SLENGTH &
                        + T * SN * DCOS(RLAT)**(2) * SL**(2) / 2.D0 &
                        + T * SN * DCOS(RLAT)**(4) * SL4COEF * SL**(4) /    24.D0 &
                        + T * SN * DCOS(RLAT)**(6) * SL6COEF * SL**(6) /   720.D0 &
                        + T * SN * DCOS(RLAT)**(8) * SL8COEF * SL**(8) / 40320.D0

                    x = x * UTMScaleFactor + 500000.d0
                    y = y * UTMScaleFactor

                    IF(Y.LT.0.0D0)THEN
                        Y = Y + 10000000.D0
                    ENDIF

                    MyUTMMesh%nodes(I,1) = X
                    MyUTMMesh%nodes(I,2) = Y

                ENDDO

                RETURN

            END SUBROUTINE

            !>Heapsort algorithm from Numerical Recipies in Fortran for
            !>REAL(8) variables.
            !>\author Zach Cobell
            SUBROUTINE HEAPSORT(N,RA)
                IMPLICIT NONE
                !>Number of items to sort
                INTEGER,INTENT(IN)    :: N
                !>The array that will be sorted in place
                REAL(8),INTENT(INOUT) :: RA(N)

                INTEGER :: L,IR,J,I
                REAL(8) :: RRA

                L = (N / 2) + 1
                IR = N
10              CONTINUE
                IF(L.GT.1)THEN
                    L = L - 1
                    RRA = RA(L)
                ELSE
                    RRA = RA(IR)
                    RA(IR) = RA(1)
                    IR = IR - 1
                    IF(IR.EQ.1)THEN
                        RA(1) = RRA
                        RETURN
                    ENDIF
                ENDIF
                I = L
                J = L + L
20              IF(J.LE.IR)THEN
                    IF(J.LT.IR)THEN
                        IF(RA(J).LT.RA(J+1))THEN
                            J = J + 1
                        ENDIF
                    ENDIF
                    IF(RRA < RA(J))THEN
                        RA(I) = RA(J)
                        I = J
                        J = J + J
                    ELSE
                        J = IR + J
                    ENDIF
                    GOTO 20
                ENDIF
                RA(I) = RRA
                GOTO 10
            END SUBROUTINE

            !>Heapsort algorithm from Numerical Recipies in Fortran for
            !>INTEGER variables.
            !>\author Zach Cobell
            SUBROUTINE HEAPSORTINT(N,RA)
                IMPLICIT NONE
                !>Number of items to sort
                INTEGER,INTENT(IN) :: N
                !>The array that will be sorted in place
                INTEGER,INTENT(INOUT) :: RA(N)

                INTEGER :: L,IR,J,I
                INTEGER :: RRA

                L = (N / 2) + 1
                IR = N
10              CONTINUE
                IF(L.GT.1)THEN
                    L = L - 1
                    RRA = RA(L)
                ELSE
                    RRA = RA(IR)
                    RA(IR) = RA(1)
                    IR = IR - 1
                    IF(IR.EQ.1)THEN
                        RA(1) = RRA
                        RETURN
                    ENDIF
                ENDIF
                I = L
                J = L + L
20              IF(J.LE.IR)THEN
                    IF(J.LT.IR)THEN
                        IF(RA(J).LT.RA(J+1))THEN
                            J = J + 1
                        ENDIF
                    ENDIF
                    IF(RRA < RA(J))THEN
                        RA(I) = RA(J)
                        I = J
                        J = J + J
                    ELSE
                        J = IR + J
                    ENDIF
                    GOTO 20
                ENDIF
                RA(I) = RRA
                GOTO 10
            END SUBROUTINE

            SUBROUTINE Unique(A,NumNodes,N)
                IMPLICIT NONE
                REAL(8) :: PREV_VAL
                INTEGER :: N
                INTEGER :: I
                INTEGER :: NumNodes
                REAL(8) :: Tolorance
                REAL(8),DIMENSION(NumNodes) :: A
                PREV_VAL = A(1)
                N = 1
                Tolorance = EPSILON(1.0D0)
                DO I = 2,NumNodes
                    IF(DABS(A(I)-PREV_VAL).LE.Tolorance)CYCLE
                    PREV_VAL = A(I)
                    N = N + 1
                ENDDO
                RETURN
            END SUBROUTINE

            SUBROUTINE Frequency(A,N,U,V)
                IMPLICIT NONE
                REAL(8) :: Prev_Val
                REAL(8) :: V
                INTEGER :: N
                INTEGER :: U
                INTEGER :: I
                INTEGER :: idx
                REAL(8),ALLOCATABLE :: counter(:,:)
                REAL(8),DIMENSION(N) :: A
                REAL(8) :: M
                REAL(8) :: Tolerance

                Tolerance = EPSILON(1.0D0)

                IF(U.GT.1)THEN
                    IF(ALLOCATED(Counter))DEALLOCATE(Counter)
                    ALLOCATE(Counter(1:U,2))
                    Counter = -1
                    idx = 1
                    Prev_Val = A(1)
                    DO I = 2,N
                        IF(DABS(A(I)-Prev_VAL).LE.Tolerance)THEN
                            IF(counter(idx,1).EQ.-1d0)THEN
                                counter(idx,1) = Prev_VAL
                                counter(idx,2) = 1d0
                            ENDIF
                            counter(idx,2) = counter(idx,2) + 1d0
                        ELSE
                            Prev_VAL = A(I)
                            idx = idx + 1
                        ENDIF
                    ENDDO

                    M = -1
                    DO I = 1,U
                        IF(counter(I,2).GT.M)THEN
                            M = counter(I,2)
                            V = counter(I,1)
                        ENDIF
                    ENDDO
                ELSE
                    V = A(1)
                ENDIF

                RETURN

            END SUBROUTINE

            SUBROUTINE ComputeElementArea(MyMesh,Area)

                IMPLICIT NONE

                TYPE(grid),INTENT(IN)             :: MyMesh
                REAL(8),ALLOCATABLE,INTENT(INOUT) :: Area(:)
                REAL(8) :: X1,X2,X3
                REAL(8) :: Y1,Y2,Y3
                REAL(8) :: S,S1,S2,S3
                REAL(8) :: A
                INTEGER :: I

                IF(.NOT.ALLOCATED(Area))ALLOCATE(Area(1:MyMesh%NumElements))
                DO I = 1,MyMesh%NumElements
                    X1 = MyMesh%nodes(MyMesh%Conn(I,1),1)
                    X2 = MyMesh%nodes(MyMesh%Conn(I,2),1)
                    X3 = MyMesh%nodes(MyMesh%Conn(I,3),1)
                    Y1 = MyMesh%nodes(MyMesh%Conn(I,1),2)
                    Y2 = MyMesh%nodes(MyMesh%Conn(I,2),2)
                    Y3 = MyMesh%nodes(MyMesh%Conn(I,3),2)
                    S1 = DISTANCE(X1,Y1,X2,Y2) !*1000D0 !Side1 (m)
                    S2 = DISTANCE(X2,Y2,X3,Y3) !*1000D0 !Side2 (m)
                    S3 = DISTANCE(X3,Y3,X1,Y1) !*1000D0 !Side3 (m)
                    S  = (S1+S2+S3)/2D0               !Perimiter/2 (m)
                    A  = SQRT(S*(S-S1)*(S-S2)*(S-S3)) !Area (m2)
                    Area(I) = A
                ENDDO

                RETURN

            END SUBROUTINE

            SUBROUTINE ComputeGridScale(MyMesh,GridScale,Cartesian)
                IMPLICIT NONE
                TYPE(GRID),INTENT(IN) :: MyMesh
                LOGICAL,INTENT(IN),OPTIONAL :: Cartesian
                REAL(8),ALLOCATABLE,INTENT(OUT) :: GridScale(:)
                LOGICAL :: C

                INTEGER :: I,J,J1,J2
                REAL(8) :: D

                IF(PRESENT(Cartesian))THEN
                    C = CARTESIAN
                ELSE
                    C = .FALSE.
                ENDIF

                ALLOCATE(GridScale(1:MyMesh%NumNodes))
                DO I = 1,MyMesh%NumElements
                    DO J = 1,3
                        J1 = J
                        J2 = J + 1
                        IF(J2.GT.3)THEN
                            J2 = 1
                        ENDIF
                        D = Distance( MyMesh%nodes(MyMesh%conn(I,J1),1),  &
                                      MyMesh%nodes(MyMesh%conn(I,J1),2),  &
                                      MyMesh%nodes(MyMesh%conn(I,J2),1),  &
                                      MyMesh%nodes(MyMesh%conn(I,J2),2),C )
                        IF(D.GT.gridscale(MyMesh%conn(I,J1)))THEN
                            gridscale(MyMesh%conn(I,J1)) = D
                        ENDIF
                        IF(D.GT.gridscale(MyMesh%conn(I,J2)))THEN
                            gridscale(MyMesh%conn(I,J2)) = D
                        ENDIF
                    ENDDO
                ENDDO

            END SUBROUTINE


            SUBROUTINE BuildElementTable(MyMesh,MyElementTable)
                IMPLICIT NONE
                TYPE(grid),INTENT(IN)            :: MyMesh
                TYPE(ElementTable),INTENT(INOUT) :: MyElementTable
                INTEGER :: I,J,N1

                !...Allocation of outer arrays
                ALLOCATE(MyElementTable%Node(1:MyMesh%NumNodes))
                MyElementTable%Node(:)%NumElementsAroundMe = 0

                !...Run the routine in three parts, first find out how many elements
                !   are around a specific node for allocation purposes. This slower
                !   scheme will avoid allocating unused memory. It is possible to
                !   allocate the number of elements to a "maximum" value and leave
                !   the rest of the slots unused when the max is not reached, however,
                !   I've chosen this scheme for better use with the very large meshes
                DO I = 1,MyMesh%NumElements
                    DO J = 1,3
                        N1 = MyMesh%Conn(I,J)
                        MyElementTable%Node(N1)%NumElementsAroundMe = &
                            MyElementTable%Node(N1)%NumElementsAroundMe + 1
                    ENDDO
                ENDDO

                !...Now, dynamically allocate the arrays
                DO I = 1,MyMesh%NumNodes
                    ALLOCATE(MyElementTable%Node(I)%ElementsAroundMe(&
                        1:MyElementTable%Node(I)%NumElementsAroundMe))
                ENDDO


                !...Finally, list the elements around the node
                !   Set counter array back to zero and build it back up
                MyElementTable%Node(:)%NumElementsAroundMe = 0
                DO I = 1,MyMesh%NumElements
                    DO J = 1,3
                        N1 = MyMesh%Conn(I,J)
                        MyElementTable%Node(N1)%NumElementsAroundMe = &
                            MyElementTable%Node(N1)%NumElementsAroundMe + 1
                        MyElementTable%Node(N1)%ElementsAroundMe(&
                            MyElementTable%Node(N1)%NumElementsAroundMe) = I
                    ENDDO
                ENDDO

            END SUBROUTINE

            SUBROUTINE BuildConnectivityTable(MyMesh,Table)
                IMPLICIT NONE
                TYPE(GRID),INTENT(IN)       :: MyMesh
                TYPE(NODETABLE),INTENT(OUT) :: Table
                TYPE(ElementTable)          :: EleTable
                INTEGER                     :: I,J,K,N,IDX,IG
                INTEGER                     :: nnan_max = 20
                REAL(8)                     :: X1,X2,Y1,Y2
                INTEGER,ALLOCATABLE         :: list(:),list2(:)
                LOGICAL,ALLOCATABLE         :: ignore(:)

                CALL BuildElementTable(mymesh,EleTable)
                ALLOCATE(list(1:nnan_max))
                ALLOCATE(ignore(1:nnan_max))
                ALLOCATE(table%node(1:mymesh%numnodes))

                DO I = 1,mymesh%NumNodes

                    !...Initialize variables
                    list(:) = -1
                    ignore(:) = .FALSE.
                    n = 0
                    ig = 0

                    !...Find all the nodes
                    DO J = 1,EleTable%node(I)%NumElementsAroundMe
                        DO K = 1,3
                            IF(mymesh%conn(EleTable%node(I)%ElementsAroundMe(J),K).NE.I)THEN
                                n = n + 1
                                list(n) = mymesh%conn(EleTable%node(I)%ElementsAroundMe(J),K)
                            ENDIF
                        ENDDO
                    ENDDO

                    !...Sort
                    CALL HEAPSORTINT(nnan_max,list)
                    DO J = 1,nnan_max
                        IF(J.EQ.1.AND.list(J).EQ.-1)THEN
                            ignore(J) = .TRUE.
                        ELSEIF(list(J).EQ.-1.OR.list(J).EQ.list(J-1))THEN
                            ignore(J) = .TRUE.
                            ig = ig + 1
                        ENDIF
                    ENDDO

                    !...Remove duplicates
                    n = nnan_max - ig - 1
                    idx = 0
                    ALLOCATE(list2(1:n))
                    DO J = 1,nnan_max
                        IF(ignore(J))CYCLE
                        idx = idx + 1
                        list2(idx) = list(J)
                    ENDDO

                    !...Save the new list
                    Table%node(I)%NumNodesAroundMe = n
                    ALLOCATE(Table%node(I)%NodesAroundMe(1:n))
                    ALLOCATE(Table%node(I)%distance(1:n))
                    Table%node(I)%NodesAroundMe(:) = list2(:)

                    !...Prep list2 for next node
                    DEALLOCATE(list2)
                ENDDO

                !...Compute the linear distance between the
                !   center node and neighbors
                DO I = 1,mymesh%NumNodes
                    X1 = mymesh%nodes(I,1)
                    Y1 = mymesh%nodes(I,2)
                    DO J = 1,table%node(I)%NumNodesAroundMe
                        X2 = mymesh%nodes(table%node(I)%NodesAroundMe(J),1)
                        Y2 = mymesh%nodes(table%node(I)%NodesAroundMe(J),2)
                        Table%node(I)%distance(J) = haversine(X1,Y1,X2,Y2)
                    ENDDO
                ENDDO

                RETURN

            END SUBROUTINE


            SUBROUTINE MakeGridNodeKDTREE(mesh,tree)

                IMPLICIT NONE

                TYPE(grid),INTENT(IN)             :: mesh
                TYPE(KDTREE2),POINTER,INTENT(OUT) :: tree
                REAL(8),ALLOCATABLE               :: XY(:,:)

                ALLOCATE(XY(1:2,1:mesh%NumNodes))

                XY(1,:) = mesh%nodes(:,1)
                XY(2,:) = mesh%nodes(:,2)

                Tree => KDTREE2_CREATE(xy,SORT=.TRUE.,REARRANGE=.TRUE.)

                RETURN

            END SUBROUTINE


            SUBROUTINE MakeElementKDTREE(MyMesh,MyTree)

                IMPLICIT NONE

                INTEGER               :: I
                INTEGER               :: N1
                INTEGER               :: N2
                INTEGER               :: N3
                REAL(8)               :: X1
                REAL(8)               :: X2
                REAL(8)               :: X3
                REAL(8)               :: Y1
                REAL(8)               :: Y2
                REAL(8)               :: Y3
                REAL(8),ALLOCATABLE   :: XY(:,:)
                TYPE(grid),INTENT(IN) :: MyMesh
                TYPE(KDTREE2),POINTER,INTENT(OUT) :: MyTree

                !...Compute Element centers
                ALLOCATE(XY(1:2,1:MyMesh%NumElements))
                DO I = 1,MyMesh%NumElements
                    N1 = MyMesh%conn(I,1)
                    N2 = MyMesh%conn(I,2)
                    N3 = MyMesh%conn(I,3)
                    X1 = MyMesh%nodes(N1,1)
                    X2 = MyMesh%nodes(N2,1)
                    X3 = MyMesh%nodes(N3,1)
                    Y1 = MyMesh%nodes(N1,2)
                    Y2 = MyMesh%nodes(N2,2)
                    Y3 = MyMesh%nodes(N3,2)
                    XY(1,I) = ( X1 + X2 + X3 ) / 3D0
                    XY(2,I) = ( Y1 + Y2 + Y3 ) / 3D0
                ENDDO

                MyTree => KDTREE2_CREATE(xy,SORT=.TRUE.,REARRANGE=.TRUE.)

                RETURN

            END SUBROUTINE




            SUBROUTINE FindElement(X,Y,mesh,tree,E,W1,W2,W3,ElementFound,USearchDepth)

                IMPLICIT NONE

                !...OPTIONAL PARAMETERS
                INTEGER,INTENT(OUT),OPTIONAL     :: E             !...Return element that was found
                INTEGER,INTENT(IN),OPTIONAL      :: USearchDepth  !...Set the maximum search depth
                REAL(8),INTENT(OUT),OPTIONAL     :: W1,W2,W3      !...Return the weights of the 3 nodes
                LOGICAL,INTENT(OUT),OPTIONAL     :: ElementFound  !...Return if the x,y resides in an element

                !...REQUIRED PARAMETERS
                REAL(8),INTENT(IN)               :: X
                REAL(8),INTENT(IN)               :: Y
                TYPE(grid),INTENT(IN)            :: MESH
                TYPE(KDTREE2),POINTER,INTENT(IN) :: TREE

                INTEGER                          :: SearchDepth = 20 !...Maximum number of near elements to check
                INTEGER                          :: MyE

                REAL(8)                          :: X1,X2,X3
                REAL(8)                          :: Y1,Y2,Y3
                REAL(8)                          :: S1,S2,S3
                REAL(8)                          :: TA
                INTEGER                          :: N1,N2,N3
                INTEGER                          :: I
                TYPE(KDTREE2_RESULT),ALLOCATABLE :: KDRESULTS(:)
                TYPE(KDTREE2_RESULT),ALLOCATABLE :: KDRESULTS2(:)
                LOGICAL                          :: SmallSearch
                LOGICAL                          :: Found
                LOGICAL                          :: BADIN
                LOGICAL                          :: EXTENDEDINFOWEIGHT


                !...User specified search depth, otherwise 20
                IF(PRESENT(USEARCHDEPTH))THEN
                    SearchDepth=USEARCHDEPTH
                ENDIF

                !...Sanity Check on depth of search
                IF(Tree%N.LT.SearchDepth)THEN
                    SearchDepth = Tree%N
                ENDIF

                ALLOCATE(KDRESULTS(1:SearchDepth))
                CALL KDTREE2_N_NEAREST(TP=TREE,QV=(/X,Y/),NN=SearchDepth,&
                        RESULTS=KDRESULTS)

                !...SANITY CHECK ON INPUTS
                BADIN=.FALSE.
                IF(PRESENT(W1).AND.&
                    ((.NOT.PRESENT(W2)).OR.&
                     (.NOT.PRESENT(W3)).OR.&
                     (.NOT.PRESENT(E))))THEN
                        BADIN=.TRUE.
                ELSEIF(PRESENT(W2).AND.&
                    ((.NOT.PRESENT(W1)).OR.&
                     (.NOT.PRESENT(W3)).OR.&
                     (.NOT.PRESENT(E))))THEN
                        BADIN=.TRUE.
                ELSEIF(PRESENT(W3).AND.&
                    ((.NOT.PRESENT(W2)).OR.&
                     (.NOT.PRESENT(W1)).OR.&
                     (.NOT.PRESENT(E))))THEN
                        BADIN=.TRUE.
                ENDIF

                IF(PRESENT(W1))THEN
                    EXTENDEDINFOWEIGHT=.TRUE.
                ELSE
                    EXTENDEDINFOWEIGHT=.FALSE.
                ENDIF

                IF(BADIN)THEN
                    WRITE(*,'(A)') "ERROR: Please check input parameters to 'FindElement' subroutine."
                    WRITE(*,'(A)') "   W1, W2, W3 and E are all optional prameters but must be"
                    WRITE(*,'(A)') "   specified so that if any of W1, W2, or W3 are specified,"
                    WRITE(*,'(A)') "   all must be specified, including E."
                    STOP
                ENDIF

                Found = .FALSE.
                FindEL: DO I = 1,SearchDepth
                    MyE = KDRESULTS(I)%IDX
                    IF(PRESENT(E))E=MyE
                    N1 = mesh%conn(MyE,1)
                    N2 = mesh%conn(MyE,2)
                    N3 = mesh%conn(MyE,3)
                    X1 = mesh%nodes(N1,1)
                    X2 = mesh%nodes(N2,1)
                    X3 = mesh%nodes(N3,1)
                    Y1 = mesh%nodes(N1,2)
                    Y2 = mesh%nodes(N2,2)
                    Y3 = mesh%nodes(N3,2)
                    S1 = ABS((X2*Y3-X3*Y2)-(X*Y3-X3*Y)+(X*Y2-X2*Y))
                    S2 = ABS((X*Y3-X3*Y)-(X1*Y3-X3*Y1)+(X1*Y-X*Y1))
                    S3 = ABS((X2*Y-X*Y2)-(X1*Y-X*Y1)+(X1*Y2-X2*Y1))
                    TA = ABS((X2*Y3-X3*Y2)-(X1*Y3-X3*Y1)+(X1*Y2-X2*Y1))
                    IF((S1+S2+S3).LE.1.001D0*TA)THEN
                        IF(EXTENDEDINFOWEIGHT)THEN
                            W1 = ((X-X3)*(Y2-Y3)+(X2-X3)*(Y3-Y))/TA
                            W2 = ((X-X1)*(Y3-Y1)-(Y-Y1)*(X3-X1))/TA
                            W3 = ((Y-Y1)*(X2-X1)-(X-X1)*(Y2-Y1))/TA
                        ENDIF
                        Found = .TRUE.
                        EXIT FindEL
                    ENDIF
                ENDDO FindEL

                IF(.NOT.Found)THEN
                    IF(PRESENT(E))E  = KDRESULTS(1)%IDX
                    IF(EXTENDEDINFOWEIGHT)THEN
                        W1 = 1D0/3D0
                        W2 = 1D0/3D0
                        W3 = 1D0/3D0
                    ENDIF
                ENDIF

                IF(PRESENT(ELEMENTFOUND))ELEMENTFOUND=FOUND

                RETURN

            END SUBROUTINE

!------------------------------------------------------------------------------!
!                                                                              !
!       THE FOLLOWING ARE DEALLOCATION ROUTINES TO BE CALLED TO DESTROY A      !
!       DERRIVED TYPE VARIABLE CREATED BY THIS CODE                            !
!                                                                              !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE DESTROY_ADCIRC(MESH,NODALATTRIBUTES,ADCOUTPUT)
            IMPLICIT NONE

            TYPE(GRID),INTENT(INOUT),OPTIONAL         :: MESH
            TYPE(FORT13),INTENT(INOUT),OPTIONAL       :: NODALATTRIBUTES
            TYPE(ADCIRCOUTPUT),INTENT(INOUT),OPTIONAL :: ADCOUTPUT
            INTEGER                                   :: DOMESH
            INTEGER                                   :: DONODALATTRIBUTES
            INTEGER                                   :: DOADCOUTPUT
            INTEGER                                   :: I

            DOMESH=0
            DONODALATTRIBUTES=0
            DOADCOUTPUT=0

            IF(PRESENT(MESH))DOMESH=1
            IF(PRESENT(NODALATTRIBUTES))DONODALATTRIBUTES=1
            IF(PRESENT(ADCOUTPUT))DOADCOUTPUT=1

            IF(DOMESH+DONODALATTRIBUTES+DOADCOUTPUT.GT.1)THEN
                WRITE(*,'(A)') "ERROR: Too many inputs specified. Only specify one."
                STOP
            ENDIF

            IF(DOMESH+DONODALATTRIBUTES+DOADCOUTPUT.LT.1)THEN
                WRITE(*,'(A)') "ERROR: Too few inputs specified. You must specify one."
            ENDIF

            IF(DOMESH.EQ.1)THEN
                IF(ALLOCATED(MESH%NODES))DEALLOCATE(MESH%NODES)
                IF(ALLOCATED(MESH%CONN))DEALLOCATE(MESH%CONN)
                IF(MESH%NUMOPENBOUNDARIES.GT.0)THEN
                    DO I = 1,MESH%NUMLANDBOUNDARIES
                        IF(ALLOCATED(MESH%LANDBC(I)%N1))&
                            DEALLOCATE(MESH%LANDBC(I)%N1)
                        IF(ALLOCATED(MESH%LANDBC(I)%N2))&
                            DEALLOCATE(MESH%LANDBC(I)%N2)
                        IF(ALLOCATED(MESH%LANDBC(I)%CREST))&
                            DEALLOCATE(MESH%LANDBC(I)%CREST)
                        IF(ALLOCATED(MESH%LANDBC(I)%SUPERCRITICAL))&
                            DEALLOCATE(MESH%LANDBC(I)%SUPERCRITICAL)
                        IF(ALLOCATED(MESH%LANDBC(I)%SUBCRITICAL))&
                            DEALLOCATE(MESH%LANDBC(I)%SUBCRITICAL)
                    ENDDO
                ENDIF
                IF(MESH%NUMLANDBOUNDARIES.GT.0)THEN
                    DO I = 1,MESH%NUMLANDBOUNDARIES
                        IF(ALLOCATED(MESH%LANDBC(I)%N1))&
                            DEALLOCATE(MESH%LANDBC(I)%N1)
                        IF(ALLOCATED(MESH%LANDBC(I)%N2))&
                            DEALLOCATE(MESH%LANDBC(I)%N2)
                        IF(ALLOCATED(MESH%LANDBC(I)%CREST))&
                            DEALLOCATE(MESH%LANDBC(I)%CREST)
                        IF(ALLOCATED(MESH%LANDBC(I)%SUPERCRITICAL))&
                            DEALLOCATE(MESH%LANDBC(I)%SUPERCRITICAL)
                        IF(ALLOCATED(MESH%LANDBC(I)%SUBCRITICAL))&
                            DEALLOCATE(MESH%LANDBC(I)%SUBCRITICAL)
                    ENDDO
                ENDIF
                MESH%Title = ""
                MESH%NumElements = 0
                MESH%NumNodes = 0
                MESH%NumOpenBoundaries = 0
                MESH%NumLandBoundaries = 0
                MESH%TotNumOpenBoundaryNodes = 0
                MESH%TotNumLandBoundaryNodes = 0
            ENDIF

            IF(DONODALATTRIBUTES.EQ.1)THEN
                IF(NODALATTRIBUTES%NumAttributes.GT.0)THEN
                    DO I = 1,NODALATTRIBUTES%NumAttributes
                        IF(ALLOCATED(NODALATTRIBUTES%NODAL_PARAM(I)%VALUES))&
                            DEALLOCATE(NODALATTRIBUTES%NODAL_PARAM(I)%VALUES)
                        NODALATTRIBUTES%NODAL_PARAM(I)%Attribute = ""
                        NODALATTRIBUTES%NODAL_PARAM(I)%DefaultValue = 0D0
                        NODALATTRIBUTES%NODAL_PARAM(I)%Units = ""
                        NODALATTRIBUTES%NODAL_PARAM(I)%NumValues = 0
                    ENDDO
                    IF(ALLOCATED(NODALATTRIBUTES%nodal_param))&
                        DEALLOCATE(NODALATTRIBUTES%nodal_param)
                ENDIF
                NODALATTRIBUTES%Title = ""
                NODALATTRIBUTES%NumNodes = 0
                NODALATTRIBUTES%NumAttributes = 0
            ENDIF

            IF(DOADCOUTPUT.EQ.1)THEN
                IF(ADCOUTPUT%NumTimeSteps.GT.0)THEN
                    DO I = 1,ADCOUTPUT%NumTimeSteps
                        IF(ALLOCATED(ADCOUTPUT%OUTPUT(I)%VALUES))&
                            DEALLOCATE(ADCOUTPUT%OUTPUT(I)%VALUES)
                        ADCOUTPUT%OUTPUT(I)%TS = 0
                        ADCOUTPUT%OUTPUT(I)%NumNonDefault = 0
                        ADCOUTPUT%OUTPUT(I)%DefaultValue = 0
                        ADCOUTPUT%Output(I)%Time = 0
                    ENDDO
                    IF(ALLOCATED(ADCOUTPUT%OUTPUT))&
                        DEALLOCATE(ADCOUTPUT%OUTPUT)
                ENDIF
                ADCOUTPUT%TITLE = ""
                ADCOUTPUT%DT = 0D0
                ADCOUTPUT%NUMVALUES = 0
                ADCOUTPUT%NumNodes = 0
                ADCOUTPUT%NumTimeSteps = 0
                ADCOUTPUT%INTERVAL = 0
                ADCOUTPUT%FILEFORMAT = 0
            ENDIF

            RETURN

        END SUBROUTINE


!------------------------------------------------------------------------------!
!                                                                              !
!       THE FOLLOWING ARE ArcGIS ASCII FORMAT READ/WRITE ROUTINES              !
!                                                                              !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE ASCII2BINARY(FILENAME,REALDATA,INTDATA)

            CHARACTER(*),INTENT(IN)     :: FILENAME
            LOGICAL,INTENT(IN),OPTIONAL :: REALDATA
            LOGICAL,INTENT(IN),OPTIONAL :: INTDATA

            INTEGER :: NCOL
            INTEGER :: NROW
            INTEGER :: DUM_INT
            INTEGER :: RECLENR
            INTEGER :: RECLENI
            INTEGER :: STARTRECR
            INTEGER :: STARTRECI
            INTEGER :: I,J,IDX,N
            INTEGER :: ND_VALUEI
            INTEGER :: READUNIT,WRITEUNIT
            INTEGER,ALLOCATABLE :: TEMPCOLI(:)
            REAL(8) :: CellSize
            REAL(8) :: XLLGAP
            REAL(8) :: YLLGAP
            REAL(8) :: YU,YL,XL,XR
            REAL(8) :: ND_VALUER
            REAL(8) :: DUM_REAL
            REAL(8),ALLOCATABLE :: TEMPCOLR(:)

            !...Get Read/Write Unit numbers
            READUNIT = GETFREEUNIT()
            WRITEUNIT = GETFREEUNIT()

            !...Sanity check input
            IF(PRESENT(REALDATA).AND.PRESENT(INTDATA))THEN
                IF(REALDATA.EQV.INTDATA)THEN
                    WRITE(*,'(A)') "ERROR: Must Select only one file format."
                    STOP
                ENDIF
            ELSEIF(.NOT.PRESENT(REALDATA).AND..NOT.PRESENT(INTDATA))THEN
                WRITE(*,'(A)') "ERROR: Must select a file format."
                STOP
            ENDIF

            !...Set the write length of the record
            INQUIRE(IOLENGTH=RECLENR) DUM_REAL
            INQUIRE(IOLENGTH=RECLENI) DUM_INT

            !...First "DATA" Record for REAL after header
            STARTRECR = 10

            !...First "DATA" Record for INT after header, Convert BYTE offset
            STARTRECI = (RECLENR*(STARTRECR-1))/RECLENI

            !...Read header from ASCII
            OPEN(FILE=TRIM(FILENAME),UNIT=1,ACTION="READ")
            READ(READUNIT,*) JunkC, NCOL
            READ(READUNIT,*) JunkC, NROW
            READ(READUNIT,*) JunkC, xllGAP
            READ(READUNIT,*) JunkC, yllGAP
            READ(READUNIT,*) JunkC, cellsize
            IF(REALDATA)THEN
                READ(READUNIT,*) JunkC, ND_ValueR
            ELSEIF(INTDATA)THEN
                READ(READUNIT,*) JunkC, ND_ValueI
            ENDIF

            !...Find corners
            xl = xllGAP
            xr = xllGAP + (dble(ncol)-1) * cellsize
            yl = yllGAP
            yu = yllGAP + (dble(nrow)-1) * cellsize

            !...Allocate value holding array
            IF(REALDATA)THEN
                ALLOCATE(TEMPCOLR(1:NCOL))
            ELSEIF(INTDATA)THEN
                ALLOCATE(TEMPCOLI(1:NCOL))
            ENDIF

            !...Open binary file and write sequential records for each
            !   data type
            IF(REALDATA)THEN
                OPEN(FILE=TRIM(FILENAME)//".binary",UNIT=2,ACTION="WRITE",&
                    ACCESS="DIRECT",RECL=RECLENR)
                !...HEADER
                WRITE(WRITEUNIT,REC=1) 0D0        !...Confirm data type, 0=Real, 1=Int
                WRITE(WRITEUNIT,REC=2) DBLE(NCOL) !...Number of columns
                WRITE(WRITEUNIT,REC=3) DBLE(NROW) !...Number of rows
                WRITE(WRITEUNIT,REC=4) XL         !...Left Corners X
                WRITE(WRITEUNIT,REC=5) XR         !...Right Corners X
                WRITE(WRITEUNIT,REC=6) YL         !...Lower Corners Y
                WRITE(WRITEUNIT,REC=7) YU         !...Upper Corners Y
                WRITE(WRITEUNIT,REC=8) CellSize   !...Size of each cell
                WRITE(WRITEUNIT,REC=9) ND_ValueR  !...No Data value (Real)
                IDX = STARTRECR-1
                N = 0
                !...BODY
                DO I = 1,NROW
                    READ(READUNIT,*) (TEMPCOLR(J),J=1,NCOL)
                    DO J = 1,NCOL
                        IDX = IDX + 1
                        WRITE(WRITEUNIT,REC=IDX) TEMPCOLR(J)
                        CALL SHOWBAR(NROW*NCOL,IDX,N)
                    ENDDO
                ENDDO
                CLOSE(READUNIT)
                CLOSE(WRITEUNIT)
            ELSEIF(INTDATA)THEN
                OPEN(FILE=TRIM(FILENAME)//".binary",UNIT=WRITEUNIT,ACTION="WRITE",&
                    ACCESS="DIRECT",RECL=RECLENR)
                !...HEADER
                WRITE(WRITEUNIT,REC=1) 1D0        !...Confirm data type, 0=Real, 1=Int
                WRITE(WRITEUNIT,REC=2) DBLE(NCOL) !...Number of columns
                WRITE(WRITEUNIT,REC=3) DBLE(NROW) !...Number of rows
                WRITE(WRITEUNIT,REC=4) XL         !...Left Corners X
                WRITE(WRITEUNIT,REC=5) XR         !...Right Corners X
                WRITE(WRITEUNIT,REC=6) YL         !...Lower Corners Y
                WRITE(WRITEUNIT,REC=7) YU         !...Upper Corners Y
                WRITE(WRITEUNIT,REC=8) CellSize   !...Size of each cell
                WRITE(WRITEUNIT,REC=9) DBLE(ND_ValueI)  !...No Data value (Int)
                CLOSE(WRITEUNIT)
                OPEN(FILE=TRIM(FILENAME)//".binary",UNIT=WRITEUNIT,ACTION="WRITE",&
                    ACCESS="DIRECT",RECL=RECLENI)
                IDX = STARTRECI
                N = 0
                !...BODY
                DO I = 1,NROW
                    READ(READUNIT,*) (TEMPCOLI(J),J=1,NCOL)
                    DO J = 1,NCOL
                        IDX = IDX + 1
                        WRITE(WRITEUNIT,REC=IDX) TEMPCOLI(J)
                        CALL SHOWBAR(NROW*NCOL,IDX,N)
                    ENDDO
                ENDDO
                CLOSE(READUNIT)
                CLOSE(WRITEUNIT)
            ENDIF


        END SUBROUTINE

        SUBROUTINE READBINARYDATA(FILENAME,X,Y,FLAG,VALUESINT,VALUESREAL)

            CHARACTER(*),INTENT(IN)        :: FILENAME
            REAL(8),INTENT(IN)             :: X
            REAL(8),INTENT(IN)             :: Y
            REAL(8),INTENT(IN)             :: FLAG
            INTEGER,INTENT(INOUT),OPTIONAL :: VALUESINT(:,:)
            REAL(8),INTENT(INOUT),OPTIONAL :: VALUESREAL(:,:)

            !...Flag values to match Griddata functions
            !   -777   nearest
            !   -888   highest in 1x
            !   -999   1x
            !   -950   2x
            !   -960   4x
            !   -970   8x

            IF(FLAG.EQ.-777D0)THEN

            ELSEIF(FLAG.EQ.-888D0)THEN

            ELSEIF(FLAG.EQ.-999D0)THEN

            ELSEIF(FLAG.EQ.-950D0)THEN

            ELSEIF(FLAG.EQ.-960D0)THEN

            ELSEIF(FLAG.EQ.-970D0)THEN

            ENDIF


        END SUBROUTINE


#ifdef _NETCDF
!------------------------------------------------------------------------------!
!                                                                              !
!       THE FOLLOWING FUNCTIONS ARE FOR ADCIRC USE WITH NETCDF                 !
!                                                                              !
!------------------------------------------------------------------------------!
!
!  USE: nc-config --flibs  }
!               and        }   These two commands will show you how to compile NetCDF on your system
!       nc-config --fflags }
!
!       WARNING: Make sure the path to NetCDF lib directory is part of your LD_LIBRARY_PATH variable
!
!
            SUBROUTINE INITIALIZE_NETCDF()
                IMPLICIT NONE
                !...This subroutine just sets up an array we can
                !   use later. Matches all variables listed in
                !   NETCDFIO.f as of 10/8/2012

                NETCDF_TYPES(:)  = ""
                NETCDF_TYPES(1)  = "sigmat"       !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(2)  = "salinity"     !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(3)  = "temperature"  !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(4)  = "u-vel3D"      !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(5)  = "v-vel3D"      !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(6)  = "w-vel3D"      !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(7)  = "q20"          !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(8)  = "l"            !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(9)  = "ev"           !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(10) = "qsurfkp1"     !...Not implemented, placed here just to follow ADCIRC
                NETCDF_TYPES(11) = "zeta"
                NETCDF_TYPES(12) = "zeta_max"
                NETCDF_TYPES(13) = "u-vel"
                NETCDF_TYPES(14) = "v-vel"
                NETCDF_TYPES(15) = "vel_max"
                NETCDF_TYPES(16) = "pressure"
                NETCDF_TYPES(17) = "pressure_min"
                NETCDF_TYPES(18) = "windx"
                NETCDF_TYPES(19) = "windy"
                NETCDF_TYPES(20) = "wind_max"
                NETCDF_TYPES(21) = "radstress_x"
                NETCDF_TYPES(22) = "radstress_y"
                NETCDF_TYPES(23) = "radstress_max"
                NETCDF_TYPES(24) = "swan_HS"
                NETCDF_TYPES(25) = "swan_HS_max"
                NETCDF_TYPES(26) = "swan_DIR"
                NETCDF_TYPES(27) = "swan_DIR_max"
                NETCDF_TYPES(28) = "swan_TM01"
                NETCDF_TYPES(29) = "swan_TM01_max"
                NETCDF_TYPES(30) = "swan_TPS"
                NETCDF_TYPES(31) = "swan_TPS_max"
                NETCDF_TYPES(32) = "swan_windx"
                NETCDF_TYPES(33) = "swan_windy"
                NETCDF_TYPES(34) = "swan_wind_max"
                NETCDF_TYPES(35) = "swan_TM02"
                NETCDF_TYPES(36) = "swan_TM02_max"
                NETCDF_TYPES(37) = "swan_TMM10"
                NETCDF_TYPES(38) = "swan_TMM10_max"

                NC_LONGNAME(:)  = ""
                NC_LONGNAME(1)  = "water column vertically varying density"
                NC_LONGNAME(2)  = "water column vertically varying salinity"
                NC_LONGNAME(3)  = "water column vertically varying temperature"
                NC_LONGNAME(4)  = "water column vertically varying east/west velocity"
                NC_LONGNAME(5)  = "water column vertically varying north/south velocity"
                NC_LONGNAME(6)  = "water column vertically varying up/down velocity"
                NC_LONGNAME(7)  = "water column vertically varying turbulent kinetic energy"
                NC_LONGNAME(8)  = "water column vertically varying mixing length"
                NC_LONGNAME(9)  = "water column vertically varying eddy viscosity"
                NC_LONGNAME(10) = "sea surface temperature at the k+1 time level"
                NC_LONGNAME(11) = "water surface elevation above geoid"
                NC_LONGNAME(12) = "maximum water surface elevation above geoid"
                NC_LONGNAME(13) = "water column vertically averaged east/west velocity"
                NC_LONGNAME(14) = "water column vertically averaged north/south velocity"
                NC_LONGNAME(15) = "maximum water column vertically averaged velocity"
                NC_LONGNAME(16) = "air pressure at sea level"
                NC_LONGNAME(17) = "minimum air pressure at sea level"
                NC_LONGNAME(18) = "wind velocity in x-direction"
                NC_LONGNAME(19) = "wind velocity in y-direction"
                NC_LONGNAME(20) = "maximum wind velocity"
                NC_LONGNAME(21) = "radiation stress gradient x component"
                NC_LONGNAME(22) = "radiation stress gradient y component"
                NC_LONGNAME(23) = "maximum radiation stress gradient"
                NC_LONGNAME(24) = "significant wave height"
                NC_LONGNAME(25) = "maximum significant wave height"
                NC_LONGNAME(26) = "mean wave direction"
                NC_LONGNAME(27) = "maximum mean wave direction"
                NC_LONGNAME(28) = "mean absolute wave period"
                NC_LONGNAME(29) = "maximum TM01 mean wave period"
                NC_LONGNAME(30) = "smoothed peak period"
                NC_LONGNAME(31) = "maximum smoothed peak period"
                NC_LONGNAME(32) = "wind velocity in x-direction"
                NC_LONGNAME(33) = "wind velocity in y-direction"
                NC_LONGNAME(34) = "maximum wind stress"
                NC_LONGNAME(35) = "mean absoloute zero crossing period"
                NC_LONGNAME(36) = "maximum TM02 mean wave period"
                NC_LONGNAME(37) = "mean absolute wave period"
                NC_LONGNAME(38) = "maximum TMM10 mean wave period"

                NC_STDNAME(:)  = ""
                NC_STDNAME(1)  = "water_density_vertically_varying"
                NC_STDNAME(2)  = "water_salinity_vertically_varying"
                NC_STDNAME(3)  = "water_temperature_vertically_varying"
                NC_STDNAME(4)  = "eastward_water_velocity_vertically_varying"
                NC_STDNAME(5)  = "northward_water_velocity_vertically_varying"
                NC_STDNAME(6)  = "upward_water_velocity_vertically_varying"
                NC_STDNAME(7)  = "turbulent_kinetic_energy_vertically_varying"
                NC_STDNAME(8)  = "water_mixing_length_vertically_varying"
                NC_STDNAME(9)  = "water_eddy_viscosity_vertically_varying"
                NC_STDNAME(10) = "future sea surface temperature"
                NC_STDNAME(11) = "sea_surface_height_above_geoid"
                NC_STDNAME(12) = "maximum_sea_surface_height_above_geoid"
                NC_STDNAME(13) = "x_water_velocity_depth_averaged"
                NC_STDNAME(14) = "y_water_velocity_depth_averaged"
                NC_STDNAME(15) = "maximum_water_velocity_depth_averaged"
                NC_STDNAME(16) = "air_pressure_at_sea_level"
                NC_STDNAME(17) = "minimum_air_pressure_at_sea_level"
                NC_STDNAME(18) = "x_wind"
                NC_STDNAME(19) = "y_wind"
                NC_STDNAME(20) = "maximum_wind"
                NC_STDNAME(21) = "radiation_stress_gradient_x"
                NC_STDNAME(22) = "radiation_stress_gradient_y"
                NC_STDNAME(23) = "maximum_radiation_stress"
                NC_STDNAME(24) = "sea_surface_wave_significant_height"
                NC_STDNAME(25) = "maximum_sea_surface_wave_significant_height"
                NC_STDNAME(26) = "sea_surface_wave_to_direction"
                NC_STDNAME(27) = "maximum_sea_surface_wave_to_direction"
                NC_STDNAME(28) = "sea_surface_wave_mean_period_from_variance_spectral_density_first_frequency_moment"
                NC_STDNAME(29) = "maximum_sea_surface_wave_mean_period_from_variance_spectral_density_first_frequency_moment"
                NC_STDNAME(30) = "sea_surface_wave_period_at_variance_spectral_density_maximum"
                NC_STDNAME(31) = "maximum_sea_surface_wave_period_at_variance_spectral_density_maximum"
                NC_STDNAME(32) = "x_wind"
                NC_STDNAME(33) = "y_wind"
                NC_STDNAME(34) = "maximum_wind"
                NC_STDNAME(35) = "sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment"
                NC_STDNAME(36) = "maximum_sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment"
                NC_STDNAME(37) = "sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment"
                NC_STDNAME(38) = "maximum_sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment"


        !....NOTE: The reason some are not implemented is because multiple variables appear in those
        !          NetCDF files and a user input option will need to be speicified before these can
        !          be correctly enabled. In the current scheme, the first variable listed above will
        !          always be found. If need be, you can reorder these to plot the desired variable.


                RETURN

            END SUBROUTINE


            SUBROUTINE FindMyNetCDFVariable(NCID,Vector)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN) :: NCID
                LOGICAL,INTENT(IN),OPTIONAL :: Vector
                INTEGER :: I
                INTEGER :: J
                INTEGER :: NVAR
                CHARACTER(200) :: NC_NAME

                CALL CHECK(NF90_INQUIRE(NCID,NVARIABLES=NVAR))

                CALL Initialize_NETCDF()
                DO I = 1,NVAR
                    CALL CHECK(NF90_INQUIRE_VARIABLE(NCID,I,&
                        NAME=NC_NAME))
                    DO J = 1,SIZE(NETCDF_TYPES)
                        IF(TRIM(ADJUSTL(NC_NAME)).EQ.&
                                TRIM(ADJUSTL(NETCDF_TYPES(J))))THEN
                            IF(.NOT.PRESENT(VECTOR))RETURN
                            IF(.NOT.Vector)RETURN
                            IF(Vector)THEN
                                SELECT CASE(J)
                                    CASE(13,14,18,19,21,22,33,34)
                                        RETURN
                                    CASE DEFAULT
                                        CONTINUE
                                END SELECT
                            ENDIF
                        ENDIF
                    ENDDO
                    IF(I.EQ.NVAR)THEN
                        WRITE(*,'(A)') &
                            "ADCIRC NetCDF Variable not found in file."
                        STOP
                    ENDIF
                ENDDO

            END SUBROUTINE


            SUBROUTINE GetNETCDFVarID(NCID,VARID1,VARID2,NCOLS,VarName1,VarName2)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN)  :: NCID
                INTEGER,INTENT(OUT) :: VARID1
                INTEGER,INTENT(OUT) :: VARID2
                INTEGER,INTENT(OUT) :: NCOLS
                CHARACTER(*),INTENT(OUT),OPTIONAL :: VarName1,VarName2
                INTEGER             :: I
                INTEGER             :: J
                INTEGER             :: NVAR
                CHARACTER(200)      :: NC_NAME

                CALL CHECK(NF90_INQUIRE(NCID,NVARIABLES=NVAR))

                DO I = 1,NVAR
                    CALL CHECK(NF90_INQUIRE_VARIABLE(NCID,I,&
                        NAME=NC_NAME))
                    DO J = 1,SIZE(NETCDF_TYPES)
                        IF(TRIM(NC_NAME).EQ.TRIM(NETCDF_TYPES(J)))THEN
                            VARID1 = I
                            IF(PRESENT(VarName1))THEN
                                VarName1 = NC_NAME
                            ENDIF
                            SELECT CASE(J)
                                CASE(13,18,21,33)
                                    NCOLS=2
                                    CALL CHECK(NF90_INQ_VARID(NCID,&
                                        TRIM(NETCDF_TYPES(J+1)),VARID2))
                                    IF(PRESENT(VarName2))THEN
                                        VarName2 = NETCDF_TYPES(J+1)
                                    ENDIF
                                CASE DEFAULT
                                    NCOLS=1
                                    VARID2=-1
                            END SELECT
                            RETURN
                        ENDIF
                    ENDDO
                ENDDO
                STOP
            END SUBROUTINE

            SUBROUTINE ReadMyNETCDFVariable(NCID,RECORD,VARID1,VEC1,VARID2,VEC2,NumNodes)
                USE netcdf
                IMPLICIT NONE
                INTEGER,INTENT(IN)           :: NCID
                INTEGER,INTENT(IN)           :: RECORD
                INTEGER,INTENT(IN)           :: VARID1
                INTEGER,INTENT(IN),OPTIONAL  :: VARID2
                INTEGER,INTENT(IN)           :: NumNodes
                REAL(8),INTENT(OUT)             :: VEC1(:)
                REAL(8),INTENT(OUT),OPTIONAL    :: VEC2(:)
                CALL CHECK(NF90_GET_VAR(NCID,VARID1,VEC1,START=(/1,RECORD/),COUNT=(/NumNodes,1/)))
                IF(PRESENT(VARID2).AND.PRESENT(VEC2))THEN
                    CALL CHECK(NF90_GET_VAR(NCID,VARID2,VEC2,START=(/1,RECORD/),COUNT=(/NumNodes,1/)))
                ENDIF
                RETURN
            END SUBROUTINE


            SUBROUTINE ReadADCIRCOutputNC(Filename,MyOutput,Record)
                IMPLICIT NONE

                CHARACTER(*),INTENT(IN)               :: Filename
                INTEGER,DIMENSION(NF90_MAX_VAR_DIMS)  :: NC_DimIDs
                INTEGER                               :: I
                INTEGER                               :: J
                INTEGER                               :: JunkI
                INTEGER                               :: NC_FILE
                INTEGER                               :: NC_ID1
                INTEGER                               :: NC_ID2
                INTEGER                               :: NC_Status
                INTEGER                               :: NC_Var
                INTEGER                               :: NC_Var2
                INTEGER                               :: NumNodes
                INTEGER                               :: NumRecs
                INTEGER                               :: NumValues
                INTEGER                               :: ierr
                INTEGER,INTENT(IN)                    :: Record
                REAL(8),ALLOCATABLE                   :: DATA1(:)
                REAL(8),ALLOCATABLE                   :: DATA2(:)
                REAL(8)                               :: DefaultValue
                REAL(8)                               :: NC_TIME(1)
                REAL(8)                               :: NC_TIME2(2)
                REAL(8)                               :: DT
                REAL(8)                               :: DTDP
                REAL(8)                               :: Interval
                TYPE(ADCIRCOutput),INTENT(OUT)        :: MyOutput

                !...Open and read netcdf file for specified time snap
                CALL Check(NF90_OPEN(TRIM(Filename),NF90_NOWRITE,&
                    NC_ID1))
                CALL Check(NF90_INQ_DIMID(NC_ID1,"node",JunkI))
                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,JunkI,&
                    len=NumNodes))
                CALL Check(NF90_INQUIRE(NC_ID1,NC_Var))
                CALL Check(NF90_INQ_VARID(NC_ID1,'time',NC_Var))
                CALL Check(NF90_INQUIRE_VARIABLE(NC_ID1,NC_Var,&
                    dimids=NC_DimIDs))
                CALL Check(NF90_INQUIRE_DIMENSION(NC_ID1,NC_DimIDs(1),len=NumRecs))
                CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time,start=(/Record/),count=(/1/)))
                ierr = NF90_GET_ATT(NC_ID1,NF90_GLOBAL,"dt",DTDP)
                IF(ierr.NE.0)DTDP=1D0

                IF(NumRecs.GT.1)THEN
                    CALL Check(NF90_GET_VAR(NC_ID1,NC_Var,NC_Time2,&
                        start=(/1/),count=(/2/)))
                    Interval = INT((NC_TIME2(2) - NC_TIME2(1)) / DTDP)
                    DT   = NC_TIME2(2) - NC_TIME2(1)
                ELSE
                    Interval = INT(NC_TIME(1)/DTDP)
                    DT   = NC_TIME(1)
                ENDIF

                CALL Check(NF90_GET_ATT(NC_ID1,NF90_GLOBAL,"dt",DTDP))
                CALL FindMyNetCDFVariable(NC_ID1)
                CALL GetNETCDFVarID(NC_ID1,NC_VAR,NC_VAR2,NumValues)
                CALL Check(NF90_GET_ATT(NC_ID1,NC_Var,'_FillValue',&
                    DefaultValue))

                IF(NumValues.EQ.1)THEN
                    ALLOCATE(DATA1(1:NumNodes))
                    CALL ReadMyNetCDFVariable(NCID=NC_ID1,&
                        RECORD=Record,VARID1=NC_Var,VEC1=DATA1,&
                        NumNodes=NumNodes)
                ELSE
                    ALLOCATE(DATA1(1:NumNodes))
                    ALLOCATE(DATA2(1:NumNodes))
                    CALL ReadMyNetCDFVariable(NCID=NC_ID1,&
                        RECORD=Record,VARID1=NC_Var,VEC1=DATA1,&
                        VARID2=NC_Var2,VEC2=DATA2,NumNodes=NumNodes)
                ENDIF
                CALL Check(NF90_CLOSE(NC_ID1))

                !...Assemble the read data into AdcircOutput structure
                MyOutput%Title        = "NetCDFOutput"
                MyOutput%dt           = DT
                MyOutput%NumNodes     = NumNodes
                MyOutput%NumValues    = NumValues
                MyOutput%NumTimeSteps = 1
                MyOutput%Interval     = Interval
                MyOutput%FileFormat   = 1050624
                ALLOCATE(MyOutput%Output(1:1))
                MyOutput%Output(1)%TS            = INT(NC_TIME(1))
                MyOutput%Output(1)%NumNonDefault = 0
                MyOutput%Output(1)%DefaultValue  = DefaultValue
                MyOutput%Output(1)%Time          = NC_TIME(1)
                ALLOCATE(MyOutput%Output(1)%Values(1:NumNodes,&
                    1:NumValues))
                MyOutput%Output(1)%Values(:,1)   = DATA1(:)
                IF(NumValues.EQ.2)THEN
                    MyOutput%Output(1)%Values(:,2) = DATA2(:)
                ENDIF
                !...Count default value nodes
                IF(Numvalues.EQ.1)THEN
                    DO I = 1,NumNodes
                        IF(MyOutput%Output(1)%Values(I,1).EQ.&
                                DefaultValue)THEN
                            MyOutput%Output(1)%NumNonDefault = &
                                MyOutput%Output(1)%NumNonDefault + 1
                        ENDIF
                    ENDDO
                ELSEIF(NumValues.EQ.2)THEN
                    DO I = 1,NumNodes
                        IF((MyOutput%Output(1)%Values(I,1).EQ.&
                           DefaultValue).AND.&
                           (MyOutput%Output(1)%Values(I,1).EQ.&
                            DefaultValue))THEN
                                MyOutput%Output(1)%NumNonDefault = &
                                MyOutput%Output(1)%NumNonDefault + 1
                        ENDIF
                    ENDDO
                ENDIF

            END SUBROUTINE

#endif

!...Some functions for parsing command line arguments from C++
#ifdef SINGLEEXE
            !>Subroutine to read and parse the command line arguments that come from the C++
            !>code and save them in an array local to this code for retrival later.
            SUBROUTINE ParseCPPCommandLineArgs(InputLength,InputArguments,NumCommandLineArgs)
                USE,INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE

                !>Integer conforming to C standard containing the length of the
                !>command line argument string
                INTEGER(C_INT),INTENT(IN),VALUE        :: InputLength
                !>Character array forming to the C standard containing the
                !>command line arguments

                CHARACTER(C_CHAR),INTENT(IN)           :: InputArguments(*)
                INTEGER,INTENT(OUT)                    :: NumCommandLineArgs
                CHARACTER(InputLength)                 :: InputArguments2
                INTEGER                                :: I,N,PrevSpace

                !...Convert the C_CHAR to a FORTRAN Character array
                DO I = 1,InputLength
                    InputArguments2(I:I) = InputArguments(I)
                ENDDO

                !...Count the spaces so we can determine the command line argument list
                ADCModules_NumCommandLineArgs = 0
                DO I = 1,InputLength
                    IF(InputArguments2(I:I).EQ." ")THEN
                        ADCModules_NumCommandLineArgs = ADCModules_NumCommandLineArgs + 1
                    ENDIF
                ENDDO

                !...Parse the command line arguments into a character array
                ALLOCATE(ADCModules_CommandLineArgs(1:ADCModules_NumCommandLineArgs))
                N = 0
                PrevSpace = 0
                DO I = 2,InputLength
                    IF(InputArguments2(I:I).EQ." ")THEN
                        N = N + 1
                        ADCModules_CommandLineArgs(N) = ADJUSTL(InputArguments2(PrevSpace+1:I-1))
                        PrevSpace = I
                    ELSEIF(I.EQ.InputLength)THEN
                        N = N + 1
                        ADCModules_CommandLineArgs(N) = ADJUSTL(InputArguments2(PrevSpace+1:I))
                    ENDIF
                ENDDO

                NumCommandLineArgs = ADCModules_NumCommandLineArgs

                RETURN
            END SUBROUTINE
#endif

            SUBROUTINE GETARG2(IDX,CLO)
                IMPLICIT NONE
                INTEGER,INTENT(IN)       :: IDX
                CHARACTER(*),INTENT(OUT) :: CLO
#ifndef SINGLEEXE
                CALL GETARG(IDX,CLO)
#else
                IF(IDX.GT.ADCModules_NumCommandLineArgs)THEN
                    CLO = ""
                ELSE
                    CLO = TRIM(ADCModules_CommandLineArgs(IDX))
                ENDIF
#endif
            END SUBROUTINE
                        
            SUBROUTINE SYSTEMCALL(SYSCOMMAND)
                IMPLICIT NONE               
                INTEGER  :: IERR
#ifdef SINGLEEXE                
                EXTERNAL :: SYSTEMCALLC
#endif                
                CHARACTER(*),INTENT(IN) :: SYSCOMMAND
#ifdef SINGLEEXE                
                CALL SYSTEMCALLC(SYSCOMMAND//CHAR(0))
#else
                CALL SYSTEM(SYSCOMMAND)
#endif                
                RETURN
            END SUBROUTINE

        END MODULE ADCIRCMODULE

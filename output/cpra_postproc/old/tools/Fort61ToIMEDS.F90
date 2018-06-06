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
!  File: Fort61ToIMEDS.F90
!
!------------------------------------------------------------------------------


#ifndef SINGLEEXE
        PROGRAM Fort61ToIMEDS
#else
        SUBROUTINE Fort61ToIMEDS_main() BIND(C)
#endif        
            USE adcircModule
            USE DATEMODULE
            USE IMEDSMODULE
            IMPLICIT NONE
            
            CHARACTER(200)     :: f61file,station_file,output_file
            CHARACTER(200)     :: coldstartstring
            
            INTEGER            :: NumStations,NumSnaps
            INTEGER            :: I,J
            
            TYPE(IMEDS)        :: StationData
            TYPE(ADCIRCOutput) :: f61
            TYPE(DATEVAR)      :: ColdStart,CurrentTime
            
            LOGICAL            :: exists
            
            WRITE(*,'(A,$)') "Name of fort.61 file: "
            READ(*,'(A)') f61file
            WRITE(*,'(A,$)') "Name of station file: "
            READ(*,'(A)') station_file
            WRITE(*,'(A,$)') "Name of output file: "
            READ(*,'(A)') output_file
            WRITE(*,'(A,$)') &
                "Reference data (cold start of ADCIRC) [yyyymmddhhmmss]: "
            READ(*,'(A)') coldstartstring
            
            exists = FindFile(f61file)
            exists = FindFile(station_file)
            
            READ(ColdStartString(1:4),'(I4)') ColdStart%Year
            READ(ColdStartString(5:6),'(I2)') ColdStart%Month
            READ(ColdStartString(7:8),'(I2)') ColdStart%Day
            READ(ColdStartString(9:10),'(I2)') ColdStart%Hour
            READ(ColdStartString(11:12),'(I2)') ColdStart%Minute
            READ(ColdStartString(13:14),'(I2)') ColdStart%Second
            
            WRITE(*,'(A,$)') "Reading data..."
            CALL ReadADCIRCOutput(f61file,f61)
            OPEN(FILE=TRIM(Station_File),UNIT=1,ACTION="READ")
            READ(1,*) NumStations
            IF(NumStations.NE.f61%NumNodes)THEN
                WRITE(*,'(A)') &
                    "ERROR: Station file must match fort.61 file."
                STOP
            ENDIF
            ALLOCATE(StationData%Station(1:NumStations))
            DO I = 1,NumStations
                READ(1,*) StationData%Station(I)%Longitude,&
                    StationData%Station(I)%Latitude
            ENDDO
            CLOSE(1)
            WRITE(*,'(A)') "done!"
            
            StationData%NStations = NumStations
            StationData%HEADER1   = "ADCIRCMODELDATA"
            StationData%HEADER2   = "ADCIRCMODELDATA"
            StationData%HEADER3   = "ADCIRC   MODELTIME   MODELDATUM"
            
            NumSnaps = f61%NumTimeSteps
            DO I = 1,NumStations
                ALLOCATE(StationData%Station(I)%YEAR(1:NumSnaps))
                ALLOCATE(StationData%Station(I)%MONTH(1:NumSnaps))
                ALLOCATE(StationData%Station(I)%DAY(1:NumSnaps))
                ALLOCATE(StationData%Station(I)%MINUTE(1:NumSnaps))
                ALLOCATE(StationData%Station(I)%HOUR(1:NumSnaps))
                ALLOCATE(StationData%Station(I)%SECOND(1:NumSnaps))
                ALLOCATE(StationData%Station(I)%VALUE(1:NumSnaps))
                WRITE(StationData%Station(I)%Station_Name,'(A,I0)') &
                    "STATION_",I
                StationData%Station(I)%Station_Index = I
                StationData%Station(I)%NSnaps = NumSnaps
            ENDDO
            
            DO I = 1,NumSnaps
                CALL DATEADD(ColdStart,INT(f61%Output(I)%Time,8)&
                    ,CurrentTime)
                DO J = 1,NumStations
                    StationData%Station(J)%Year(I)= CurrentTime%Year
                    StationData%Station(J)%Month(I) = CurrentTime%Month
                    StationData%Station(J)%Day(I) = CurrentTime%Day
                    StationData%Station(J)%Hour(I) = CurrentTime%Hour
                    StationData%Station(J)%Minute(I) = CurrentTime%Minute
                    StationData%Station(J)%Second(I) = CurrentTime%Second
                    StationData%Station(J)%VALUE(I) = f61%Output(I)%Values(J,1)
                ENDDO
            ENDDO
           
            WRITE(*,'(A,$)') "Writing output..."
            CALL Write_IMEDS(output_file,stationdata)
            WRITE(*,'(A)') "done!"
            
            
#ifndef SINGLEEXE
        END PROGRAM
#else
        END SUBROUTINE
#endif  

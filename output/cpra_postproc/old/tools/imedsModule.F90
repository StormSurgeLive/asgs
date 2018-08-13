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
!  File: IMEDSModule.F90
!
!------------------------------------------------------------------------------

        MODULE IMEDSModule
            USE GENERICMODULE
            IMPLICIT NONE

            !>IMEDS Data Container
            TYPE IMEDS_DATA
                !>Y position in decimal degrees
                REAL(8)                      :: LATITUDE
                !>X position in decimal degrees
                REAL(8)                      :: LONGITUDE
                !>Name of the current station
                CHARACTER(200)               :: STATION_NAME
                !>Number of output snaps in this container
                INTEGER                      :: NSNAPS
                !>Index of this station in the global IMEDS file
                INTEGER                      :: STATION_INDEX
                !>Year for each output cycle
                INTEGER,ALLOCATABLE          :: YEAR(:)
                !>Month for each output cycle
                INTEGER,ALLOCATABLE          :: MONTH(:)
                !>Day for each output cycle
                INTEGER,ALLOCATABLE          :: DAY(:)
                !>Hour for each output cycle
                INTEGER,ALLOCATABLE          :: HOUR(:)
                !>Minute for each output cycle
                INTEGER,ALLOCATABLE          :: MINUTE(:)
                !>Second for each output cycle (generally unused)
                INTEGER,ALLOCATABLE          :: SECOND(:)
                !>Measured (or modeled) value for each output cycle
                REAL(8),ALLOCATABLE          :: VALUE(:)
            END TYPE IMEDS_DATA

            !>IMEDS Type variable (Wrapper)
            TYPE IMEDS
                !>Number of stations found within this file
                INTEGER                      :: NSTATIONS
                CHARACTER(200)               :: HEADER1
                CHARACTER(200)               :: HEADER2
                CHARACTER(200)               :: HEADER3
                !>Data container for the array of stations
                TYPE(IMEDS_DATA),ALLOCATABLE :: STATION(:)
            END TYPE IMEDS


            CONTAINS

            SUBROUTINE READ_IMEDS(filename,outIMEDS)
                IMPLICIT NONE
                CHARACTER(*),INTENT(IN) :: filename
                TYPE(IMEDS),INTENT(OUT) :: outIMEDS

                IF(INDEX(filename,".nc").GT.0)THEN
#ifdef _NETCDF                
                    CALL READ_IMEDS_NETCDF(filename,outIMEDS)
#else
                    WRITE(*,'(A)') "ERROR: netCDF not available."
                    STOP
#endif                    
                ELSE
                    CALL READ_IMEDS_ASCII(filename,outIMEDS)
                ENDIF

            END SUBROUTINE READ_IMEDS


            SUBROUTINE WRITE_IMEDS(filename,outIMEDS,source)
                IMPLICIT NONE
                CHARACTER(*),INTENT(IN)          :: filename
                CHARACTER(*),INTENT(IN),OPTIONAL :: source
                TYPE(IMEDS),INTENT(IN)           :: outIMEDS

                IF(INDEX(filename,".nc").GT.0)THEN
#ifdef _NETCDF                
                    IF(PRESENT(source))THEN
                        CALL WRITE_IMEDS_NETCDF(filename,outIMEDS,source)
                    ELSE
                        CALL WRITE_IMEDS_NETCDF(filename,outIMEDS)
                    ENDIF
#else
                    WRITE(*,'(A)') "ERROR: netCDF not available."
#endif                    
                ELSE
                    CALL WRITE_IMEDS_ASCII(filename,outIMEDS)
                ENDIF
            END SUBROUTINE WRITE_IMEDS


#ifdef _NETCDF
            SUBROUTINE READ_IMEDS_NETCDF(filename,output)
                USE NETCDF
                USE DATEMODULE
                IMPLICIT NONE
                CHARACTER(*),INTENT(IN) :: filename
                TYPE(IMEDS),INTENT(OUT) :: output

                CHARACTER(200)          :: tempc,refDateString
                CHARACTER(1),ALLOCATABLE :: station_names(:,:)
                INTEGER                 :: I,J
                INTEGER                 :: NCID
                INTEGER                 :: dimid_numstations,length_dimid,varid_data,varid_time
                INTEGER                 :: varid_stationName,varid_x,varid_y
                INTEGER(8),ALLOCATABLE  :: time(:)
                
                REAL(8),ALLOCATABLE     :: x(:),y(:),val(:)

                TYPE(DATEVAR)           :: refDate,date


                CALL CHECK(NF90_OPEN(TRIM(filename),NF90_NOWRITE,NCID))

                CALL CHECK(NF90_INQ_DIMID(NCID,"numStations",dimid_numstations))
                CALL CHECK(NF90_INQUIRE_DIMENSION(NCID,dimid_numstations,LEN=output%nstations))

                ALLOCATE(output%station(1:output%nstations))
                ALLOCATE(x(1:output%nstations))
                ALLOCATE(y(1:output%nstations))

                DO I = 1,output%nstations
                    tempC(:) = " "
                    WRITE(tempc,'(A,I4.4)') "stationLength_",I
                    CALL CHECK(NF90_INQ_DIMID(NCID,tempc,length_dimid))
                    CALL CHECK(NF90_INQUIRE_DIMENSION(NCID,length_dimid,LEN=output%station(i)%nsnaps))
                    ALLOCATE(output%station(I)%year(1:output%station(i)%nsnaps))
                    ALLOCATE(output%station(I)%month(1:output%station(i)%nsnaps))
                    ALLOCATE(output%station(I)%day(1:output%station(i)%nsnaps))
                    ALLOCATE(output%station(I)%hour(1:output%station(i)%nsnaps))
                    ALLOCATE(output%station(I)%minute(1:output%station(i)%nsnaps))
                    ALLOCATE(output%station(I)%second(1:output%station(i)%nsnaps))
                    ALLOCATE(output%station(I)%value(1:output%station(i)%nsnaps))
                ENDDO
                    
                CALL CHECK(NF90_INQ_VARID(NCID,"stationName",varid_stationName))
                CALL CHECK(NF90_INQ_VARID(NCID,"stationXCoordinate",varid_x))
                CALL CHECK(NF90_INQ_VARID(NCID,"stationYCoordinate",varid_y))

                CALL CHECK(NF90_GET_VAR(NCID,varid_x,x))
                CALL CHECK(NF90_GET_VAR(NCID,varid_y,y))

                ALLOCATE(station_names(1:200,1:output%nstations))
                DO I = 1,output%nstations
                    tempC(:) = " "
                    WRITE(tempC,'(A,I4.4)') "station_",I
                    CALL CHECK(NF90_GET_VAR(NCID,varid_stationName,output%station(i)%station_name,START=(/1,i/),COUNT=(/200,1/)))
                    CALL CHECK(NF90_INQ_VARID(ncid,"data_"//TRIM(tempC),varid_data))
                    CALL CHECK(NF90_INQ_VARID(ncid,"time_"//TRIM(tempC),varid_time))
                    CALL CHECK(NF90_GET_ATT(ncid,varid_time,"referenceDate",refDateString))

                    READ(refDateString(1:4),*) refDate%year
                    READ(refDateString(6:7),*) refDate%month
                    READ(refDateString(9:10),*) refDate%day
                    READ(refDateString(12:13),*) refDate%hour
                    READ(refDateString(15:16),*) refDate%minute
                    READ(refDateString(18:19),*) refDate%second

                    ALLOCATE(time(1:output%station(i)%nsnaps))
                    ALLOCATE(val(1:output%station(i)%nsnaps))

                    CALL CHECK(NF90_GET_VAR(ncid,varid_data,output%station(i)%value))
                    CALL CHECK(NF90_GET_VAR(ncid,varid_time,time))

                    DO J = 1,output%station(i)%nsnaps
                        CALL DATEADD(refDate,time(j),date)
                        output%station(i)%year(j) = date%year
                        output%station(i)%month(j) = date%month
                        output%station(i)%day(j) = date%day 
                        output%station(i)%hour(j) = date%hour
                        output%station(i)%minute(j) = date%minute
                        output%station(i)%second(j) = date%second
                    ENDDO

                    DEALLOCATE(time)
                    DEALLOCATE(val)
                    
                ENDDO


            END SUBROUTINE READ_IMEDS_NETCDF

            SUBROUTINE WRITE_IMEDS_NETCDF(filename,input,source)
                USE NETCDF
                USE DATEMODULE
                IMPLICIT NONE
                CHARACTER(*),INTENT(IN) :: filename
                CHARACTER(*),INTENT(IN),OPTIONAL :: source
                TYPE(IMEDS),INTENT(IN)  :: input
            
                CHARACTER(200)          :: tempC
                CHARACTER(200)          :: user,host,date,Now,BigBen,Zone
                CHARACTER(200)          :: units,verticalDatum
            
                INTEGER                :: I,J,offsetTime
                INTEGER                :: values(7)
            
                INTEGER                 :: NCID
                INTEGER                 :: dimid_numstations
                INTEGER                 :: dimid_stationname
                INTEGER                 :: varid_stationname
                INTEGER                 :: varid_stationx,varid_stationy
                INTEGER,ALLOCATABLE     :: varid_stationdata(:)
                INTEGER,ALLOCATABLE     :: varid_stationtime(:)
                INTEGER,ALLOCATABLE     :: dimid_station_len(:)
                INTEGER(8),ALLOCATABLE  :: tempInt(:)

                REAL(8)                 :: x,y

                TYPE(DATEVAR)           :: epochDate
                TYPE(DATEVAR)           :: tempDate
            
                epochDate%year = 1970
                epochDate%month = 1
                epochDate%day = 1
                epochDate%hour = 0
                epochDate%minute = 0
                epochDate%second = 0

                !...Creates the file
                CALL CHECK(NF90_CREATE(filename,NF90_HDF5,NCID))

                !...Start defining variable sizes
                CALL CHECK(NF90_DEF_DIM(ncid,"numStations",input%nstations,dimid_numstations))
                CALL CHECK(NF90_DEF_DIM(ncid,"stationNameLen",200,dimid_stationname))

                ALLOCATE(DIMID_STATION_LEN(1:input%NStations))
                ALLOCATE(VARID_STATIONtime(1:input%NStations))
                ALLOCATE(VARID_STATIONdata(1:input%NStations))
                DO I = 1,input%nstations
                    tempC(:) = " "
                    WRITE(tempc,'(A,I4.4)') "stationLength_",I
                    CALL CHECK(NF90_DEF_DIM(ncid,TRIM(tempc),input%station(I)%nsnaps,dimid_station_len(I)))
                ENDDO

                !...Start defining variables
                CALL CHECK(NF90_DEF_VAR(NCID,"stationName",NF90_CHAR,(/dimid_stationname,dimid_numstations/),varid_stationname))
                CALL CHECK(NF90_DEF_VAR(NCID,"stationXCoordinate",NF90_DOUBLE,(/dimid_numstations/),varid_stationx))
                CALL CHECK(NF90_DEF_VAR(NCID,"stationYCoordinate",NF90_DOUBLE,(/dimid_numstations/),varid_stationy))

                CALL CHECK(NF90_PUT_ATT(NCID,varid_stationx,"HorizontalProjectionName","WGS84"))
                CALL CHECK(NF90_PUT_ATT(NCID,varid_stationx,"HorizontalProjectionEPSG",4326))
                CALL CHECK(NF90_PUT_ATT(NCID,varid_stationy,"HorizontalProjectionName","WGS84"))
                CALL CHECK(NF90_PUT_ATT(NCID,varid_stationy,"HorizontalProjectionEPSG",4326))
                
                DO I = 1,input%nstations
                    tempC(:) = " "
                    WRITE(tempC,'(A,I4.4)') "station_",I
                    CALL CHECK(NF90_DEF_VAR(NCID,"time_"//TRIM(tempC),NF90_INT64,(/dimid_station_len(I)/),varid_stationtime(I)))
                    CALL CHECK(NF90_PUT_ATT(NCID,varid_stationtime(I),"referenceDate","1970-01-01 00:00:00"))
                    CALL CHECK(NF90_PUT_ATT(NCID,varid_stationtime(I),"timezone","utc"))
                    CALL CHECK(NF90_DEF_VAR(NCID,"data_"//TRIM(tempC),NF90_DOUBLE,(/dimid_station_len(I)/),varid_stationdata(I)))
                    !CALL CHECK(NF90_PUT_ATT(NCID,varid_stationdata(I),"VerticalProjection",TRIM(verticalDatum)))
                    !CALL CHECK(NF90_PUT_ATT(NCID,varid_stationdata(I),"units",TRIM(units)))
                    CALL CHECK(NF90_PUT_ATT(NCID,varid_stationtime(I),"StationName",TRIM(input%station(I)%station_name)))
                    CALL CHECK(NF90_PUT_ATT(NCID,varid_stationdata(I),"StationName",TRIM(input%station(I)%station_name)))
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,varid_stationtime(I),1,1,2))
                    CALL CHECK(NF90_DEF_VAR_DEFLATE(NCID,varid_stationdata(I),1,1,2))
                ENDDO

                !...Get the current user from the shell
                CALL GET_ENVIRONMENT_VARIABLE("USER",User)
                CALL GET_ENVIRONMENT_VARIABLE("HOSTNAME",Host)

                !...Get the current date and time
                CALL DATE_AND_TIME(now,bigben,zone,values)
                WRITE(date,100) values(1),values(2),values(3),&
                    values(5),values(6),values(7),values(4)/60
100             FORMAT(I4,'-',I2.2,'-',i2.2,' ',i2,':',i2.2,':',i2.2,' ',i3.2,':00')

                IF(PRESENT(source))THEN
                    CALL CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,"Source",TRIM(source)))
                ENDIF
                CALL CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'creation_date',TRIM(date)))
                CALL CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'created_by',TRIM(User)))
                CALL CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'host',TRIM(host)))
                CALL CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'netCDF_version',TRIM(NF90_INQ_LIBVERS())))
                CALL CHECK(NF90_PUT_ATT(NCID,NF90_GLOBAL,'fileformat',20180123))

                CALL CHECK(NF90_ENDDEF(NCID))

                DO I = 1,input%nstations
                    ALLOCATE(tempInt(1:input%station(I)%nsnaps))
                    DO J = 1,input%station(I)%nsnaps
                        tempDate%year = input%station(I)%year(J)
                        tempDate%month = input%station(I)%month(J)
                        tempDate%day = input%station(I)%day(J)
                        tempDate%hour = input%station(I)%hour(J)
                        tempDate%minute = input%station(I)%minute(J)
                        tempDate%second = input%station(I)%second(J)
                        tempInt(J) = juliansec(tempDate) - juliansec(epochDate) + INT(offsetTime*3600,8)
                    ENDDO

                    tempC(:) = " "
                    WRITE(tempC,'(A)') input%station(I)%station_name
                    CALL CHECK(NF90_PUT_VAR(ncid,varid_stationname,tempC,START=(/1,I/),COUNT=(/200,1/)))
                    
                    CALL CHECK(NF90_PUT_VAR(ncid,varid_stationtime(I),tempInt))
                    CALL CHECK(NF90_PUT_VAR(ncid,varid_stationdata(I),input%station(I)%value))

                    x = input%station(I)%longitude
                    y = input%station(I)%latitude
                    CALL CHECK(NF90_PUT_VAR(ncid,varid_stationx,(/x/),START=(/I/),COUNT=(/1/)))
                    CALL CHECK(NF90_PUT_VAR(ncid,varid_stationy,(/y/),START=(/I/),COUNT=(/1/)))

                    DEALLOCATE(tempInt)

                ENDDO

                CALL CHECK(NF90_CLOSE(NCID))

            END SUBROUTINE WRITE_IMEDS_NETCDF
#endif            

            !>Subroutine to read an IMEDS formatted file and save the data in a 
            !>container
            !>\author Zach Cobell
            SUBROUTINE READ_IMEDS_ASCII(Filename,OutIMEDS)
                IMPLICIT NONE
                !>File to be read
                CHARACTER(*),INTENT(IN)   :: Filename
                !>Data container where information will
                !>be read into
                TYPE(IMEDS),INTENT(OUT)   :: OutIMEDS

                CHARACTER(200)            :: TempName
                CHARACTER(2000)           :: TempChar

                INTEGER                   :: TempINT
                INTEGER                   :: IOS,IOS2
                INTEGER                   :: IMEDUNIT
                INTEGER                   :: I,J

                REAL(8)                   :: TempX,TempY
                REAL(8)                   :: TempREAL

                LOGICAL                   :: exists
                LOGICAL                   :: HASSEC

                exists = FindFile(Filename)
                IMEDUNIT=GETFREEUNIT()

                OPEN(FILE=TRIM(Filename),UNIT=IMEDUNIT,ACTION="READ")

                !...Read 3 header lines into structure
                READ(IMEDUNIT,'(A)') OutIMEDS%HEADER1
                READ(IMEDUNIT,'(A)') OutIMEDS%HEADER2
                READ(IMEDUNIT,'(A)') OutIMEDS%HEADER3

                !...Start counting things up
                OutIMEDS%NSTATIONS = 0
                DO  !...Outer infinite loop over stations
                    READ(IMEDUNIT,*,IOSTAT=IOS) TempName,TempY,TempX
                    IF(IOS.NE.0)THEN
                        BACKSPACE(IMEDUNIT)
                        EXIT
                    ENDIF
                    DO !...Inner infinite loop over data
                        READ(IMEDUNIT,*,IOSTAT=IOS) TempINT,TempINT,TempINT,TempINT,TempREAL
                        IF(IOS.NE.0)THEN
                            BACKSPACE(IMEDUNIT)
                            EXIT
                        ENDIF
                    ENDDO
                    OutIMEDS%NSTATIONS = OutIMEDS%NSTATIONS + 1
                ENDDO

                ALLOCATE(OutIMEDS%Station(1:OutIMEDS%NSTATIONS))

                HASSEC = .FALSE.

                !...Start the loop over, this time counting snaps
                REWIND(IMEDUNIT)
                READ(IMEDUNIT,'(A)') TempName
                READ(IMEDUNIT,'(A)') TempName
                READ(IMEDUNIT,'(A)') TempName
                DO I = 1,OutIMEDS%NSTATIONS
                    READ(IMEDUNIT,*) OutIMEDS%STATION(I)%STATION_NAME,&
                        OutIMEDS%STATION(I)%LATITUDE,OutIMEDS%STATION(I)%LONGITUDE
                    OutIMEDS%STATION(I)%NSNAPS = 0
                    DO !...Inner infinite loop to count
                        READ(IMEDUNIT,'(A)',IOSTAT=IOS2) TEMPCHAR
                        IF(IOS2.NE.0)THEN
                            EXIT
                        ENDIF
                        READ(TEMPCHAR,*,IOSTAT=IOS) TempINT,TempINT,TempINT,TempINT,TempINT,TempINT,TempREAL
                        IF(IOS.NE.0)THEN
                            READ(TEMPCHAR,*,IOSTAT=IOS) TempINT,TempINT,TempINT,TempINT,TempINT,TempREAL
                            IF(IOS.NE.0)THEN
                                BACKSPACE(IMEDUNIT)
                                EXIT
                            ELSE
                                HASSEC=.FALSE.
                            ENDIF
                        ELSE
                            HASSEC=.TRUE.
                        ENDIF
                        OutIMEDS%STATION(I)%NSNAPS = OutIMEDS%STATION(I)%NSNAPS + 1
                    ENDDO
                    ALLOCATE(OutIMEDS%STATION(I)%YEAR(1:OutIMEDS%STATION(I)%NSNAPS))
                    ALLOCATE(OutIMEDS%STATION(I)%MONTH(1:OutIMEDS%STATION(I)%NSNAPS))
                    ALLOCATE(OutIMEDS%STATION(I)%DAY(1:OutIMEDS%STATION(I)%NSNAPS))
                    ALLOCATE(OutIMEDS%STATION(I)%HOUR(1:OutIMEDS%STATION(I)%NSNAPS))
                    ALLOCATE(OutIMEDS%STATION(I)%MINUTE(1:OutIMEDS%STATION(I)%NSNAPS))
                    ALLOCATE(OutIMEDS%STATION(I)%SECOND(1:OutIMEDS%STATION(I)%NSNAPS))
                    ALLOCATE(OutIMEDS%STATION(I)%VALUE(1:OutIMEDS%STATION(I)%NSNAPS))
                ENDDO

                !...Final pass for data
                REWIND(IMEDUNIT)
                READ(IMEDUNIT,'(A)') TempName
                READ(IMEDUNIT,'(A)') TempName
                READ(IMEDUNIT,'(A)') TempName
                DO I = 1,OutIMEDS%NSTATIONS
                    READ(IMEDUNIT,*) TempName
                    DO J = 1,OutIMEDS%STATION(I)%NSNAPS
                        IF(.NOT.HASSEC)THEN
                            READ(IMEDUNIT,*) OutIMEDS%STATION(I)%YEAR(J),&
                                             OutIMEDS%STATION(I)%MONTH(J),&
                                             OutIMEDS%STATION(I)%DAY(J),&
                                             OutIMEDS%STATION(I)%HOUR(J),&
                                             OutIMEDS%STATION(I)%MINUTE(J),&
                                             OutIMEDS%STATION(I)%VALUE(J)
                            OutIMEDS%Station(I)%SECOND(J) = 0
                        ELSE
                            READ(IMEDUNIT,*) OutIMEDS%STATION(I)%YEAR(J),&
                                             OutIMEDS%STATION(I)%MONTH(J),&
                                             OutIMEDS%STATION(I)%DAY(J),&
                                             OutIMEDS%STATION(I)%HOUR(J),&
                                             OutIMEDS%STATION(I)%MINUTE(J),&
                                             OutIMEDS%Station(I)%SECOND(J),&
                                             OutIMEDS%STATION(I)%VALUE(J)
                        ENDIF
                    ENDDO
                ENDDO


                CLOSE(IMEDUNIT)

            END SUBROUTINE READ_IMEDS_ASCII
            
            !>Subroutine to write an IMEDS data container to 
            !>the IMEDS file format
            !>\author Zach Cobell
            SUBROUTINE WRITE_IMEDS_ASCII(Filename,IMEDSData)
                IMPLICIT NONE
    
                !>File to be written
                CHARACTER(*),INTENT(IN) :: Filename
                !>IMEDS data container to be written
                TYPE(IMEDS),INTENT(IN)  :: IMEDSData

                CHARACTER(200) :: StationName
                INTEGER        :: IMEDSUNIT
                INTEGER        :: I,J

                IMEDSUNIT=GetFreeUnit()
                OPEN(FILE=TRIM(Filename),UNIT=IMEDSUNIT,ACTION="WRITE")
                WRITE(IMEDSUNIT,'(A)') TRIM(IMEDSData%HEADER1)
                WRITE(IMEDSUNIT,'(A)') TRIM(IMEDSData%HEADER2)
                WRITE(IMEDSUNIT,'(A)') TRIM(IMEDSData%HEADER3)
                DO I = 1,IMEDSData%NSTATIONS
                    !...Remove spaces from station names
                    StationName = ""
                    DO J = 1,LEN_TRIM(IMEDSData%Station(I)%Station_Name)
                        IF(IMEDSData%Station(I)%Station_Name(J:J).EQ." ")THEN
                            StationName(J:J) = "_"
                        ELSE
                            StationName(J:J) = IMEDSData%Station(I)%Station_Name(J:J)
                        ENDIF
                    ENDDO
                    WRITE(IMEDSUNIT,'(A,2X,F0.8,2X,F0.8)') &
                        TRIM(StationName),&
                        IMEDSData%Station(I)%Latitude,&
                        IMEDSData%Station(I)%Longitude
                    DO J = 1,IMEDSData%Station(I)%NSNAPS
                        WRITE(IMEDSUNIT,201) IMEDSData%Station(I)%YEAR(J),&
                                             IMEDSData%Station(I)%MONTH(J),&
                                             IMEDSData%Station(I)%DAY(J),&
                                             IMEDSData%Station(I)%HOUR(J),&
                                             IMEDSData%Station(I)%MINUTE(J),&
                                             IMEDSData%Station(I)%SECOND(J),&
                                             IMEDSData%Station(I)%VALUE(J)
                    ENDDO
                ENDDO
                CLOSE(IMEDSUNIT)

201             FORMAT(6I7,F18.8)

            END SUBROUTINE WRITE_IMEDS_ASCII


        END MODULE IMEDSMODULE

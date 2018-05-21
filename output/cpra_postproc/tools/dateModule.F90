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
!  File: datemodule.F90
!
!------------------------------------------------------------------------------

        MODULE DATEMODULE
#ifdef _QT
            USE QTDATEMODULE
#endif
            IMPLICIT NONE

            TYPE DATEVAR
                INTEGER :: YEAR=0
                INTEGER :: MONTH=0
                INTEGER :: DAY=0
                INTEGER :: HOUR=0
                INTEGER :: MINUTE=0
                INTEGER :: SECOND=0
            END TYPE DATEVAR

            CONTAINS

!...Note that we will use Qt's date math since it is more
!   robust than my hacky routine here. If we don't have
!   Qt, we default back to my hacky routine.
#ifdef _QT
            SUBROUTINE DATEADD(DATE1,ADDSEC,RESULTDATE)
                IMPLICIT NONE
                TYPE(DATEVAR),INTENT(IN)  :: DATE1
                INTEGER(8),INTENT(IN)     :: ADDSEC
                TYPE(DATEVAR),INTENT(OUT) :: RESULTDATE

                CALL qtdate_add(DATE1%YEAR,DATE1%MONTH,DATE1%DAY,&
                               DATE1%HOUR,DATE1%MINUTE,DATE1%SECOND,&
                               ADDSEC,RESULTDATE%YEAR,RESULTDATE%MONTH,&
                               RESULTDATE%DAY,RESULTDATE%HOUR,RESULTDATE%MINUTE,&
                               RESULTDATE%SECOND)

            END SUBROUTINE DATEADD
#else
            SUBROUTINE DATEADD(DATE1,ADDSEC,RESULTDATE)
                IMPLICIT NONE
                TYPE(DATEVAR),INTENT(IN)  :: DATE1
                INTEGER(8),   INTENT(IN)  :: ADDSEC
                TYPE(DATEVAR),INTENT(OUT) :: RESULTDATE
                INTEGER(8) :: JD

                JD=JULIANSEC(DATE1)
                RESULTDATE=GREGORIAN(JD+ADDSEC)

                RETURN

            END SUBROUTINE
#endif            
            
            CHARACTER(200) FUNCTION WRITE_DATE(MYDATE)
                IMPLICIT NONE
                
                TYPE(DATEVAR),INTENT(IN) :: MYDATE
                CHARACTER(200)           :: DATE_STRING
                
                WRITE(DATE_STRING,'(I4.4,"/",I2.2,"/",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
                                     MYDATE%YEAR,MYDATE%MONTH,&
                                     MYDATE%DAY,MYDATE%HOUR,&
                                     MYDATE%MINUTE,MYDATE%SECOND
                                     
                WRITE_DATE = DATE_STRING
                
            END FUNCTION WRITE_DATE
                
            
            INTEGER(8) FUNCTION GET_TIMEZONE_OFFSET(TZ)
                IMPLICIT NONE
                CHARACTER(*),INTENT(IN) :: TZ
                INTEGER(8)              :: OFFSET_SECONDS
                ! Abbreviation    Offset Seconds    Offset           Time zone name
                !     ADT             -10800        UTC -3       Atlantic Daylight Time
                !     AKDT            -28800        UTC -8       Alaska Daylight Time
                !     AKST            -32400        UTC -9       Alaska Standard Time
                !     AST             -14400        UTC -4       Atlantic Standard Time
                !     CDT             -18000        UTC -5       Central Daylight Time
                !     CST             -21600        UTC -6       Central Standard Time
                !     EDT             -14400        UTC -4       Eastern Daylight Time
                !     EGST                 0        UTC +0       Eastern Greenland Summer Time
                !     EGT              -3600        UTC -1       East Greenland Time
                !     EST             -18000        UTC -5       Eastern Standard Time
                !     GMT                  0        UTC +0       Greenwich Mean Time
                !     HADT            -32400        UTC -9       Hawaii-Aleutian Daylight Time
                !     HAST            -36000        UTC -10      Hawaii-Aleutian Standard Time
                !     MDT             -21600        UTC -6       Mountain Daylight Time
                !     MST             -25200        UTC -7       Mountain Standard Time
                !     NDT              -9000        UTC -2:30    Newfoundland Daylight Time
                !     NST             -12600        UTC -3:30    Newfoundland Standard Time
                !     PDT             -25200        UTC -7       Pacific Daylight Time
                !     PMDT             -7200        UTC -2       Pierre & Miquelon Daylight Time
                !     PMST            -10800        UTC -3       Pierre & Miquelon Standard Time
                !     PST             -28800        UTC -8       Pacific Standard Time
                !     WGST             -7200        UTC -2       Western Greenland Summer Time
                !     WGT             -10800        UTC -3       West Greenland Time
                SELECT CASE(TRIM(TZ))
                    CASE("ADT")
                        OFFSET_SECONDS = -10800
                    CASE("AKDT")
                        OFFSET_SECONDS = -28800
                    CASE("AKST")
                        OFFSET_SECONDS = -32400                
                    CASE("AST")
                        OFFSET_SECONDS = -14400
                    CASE("CDT")
                        OFFSET_SECONDS = -18000
                    CASE("CST")
                        OFFSET_SECONDS = -21600
                    CASE("EDT")
                        OFFSET_SECONDS = -14400
                    CASE("EGST")
                        OFFSET_SECONDS = 0
                    CASE("EGT")
                        OFFSET_SECONDS = -3600
                    CASE("EST")
                        OFFSET_SECONDS = -18000
                    CASE("GMT")
                        OFFSET_SECONDS = 0
                    CASE("HADT")
                        OFFSET_SECONDS = -32400
                    CASE("HAST")
                        OFFSET_SECONDS = -36000
                    CASE("MDT")
                        OFFSET_SECONDS = -21600
                    CASE("MST")
                        OFFSET_SECONDS = -25200
                    CASE("NDT")
                        OFFSET_SECONDS = -9000
                    CASE("NST")
                        OFFSET_SECONDS = -12600
                    CASE("PDT")
                        OFFSET_SECONDS = -25200
                    CASE("PMDT")
                        OFFSET_SECONDS = -7200
                    CASE("PMST")
                        OFFSET_SECONDS = -10800
                    CASE("PST")
                        OFFSET_SECONDS = -28800
                    CASE("WGST")
                        OFFSET_SECONDS = -7200
                    CASE("WGT")
                        OFFSET_SECONDS = -10800
                    CASE("UTC")
                        OFFSET_SECONDS = 0
                    CASE DEFAULT
                        WRITE(*,'(A)') "ERROR: Time zone not available"
                        STOP
                END SELECT
                
                GET_TIMEZONE_OFFSET = OFFSET_SECONDS
                
            END FUNCTION GET_TIMEZONE_OFFSET
           

            SUBROUTINE TIMEZONE_CHANGE(DATE1,TZ1,TZ2,RESULTDATE)
                IMPLICIT NONE
                
                TYPE(DATEVAR),INTENT(IN)     :: DATE1
                CHARACTER(*),INTENT(IN)      :: TZ1
                CHARACTER(*),INTENT(IN)      :: TZ2
                TYPE(DATEVAR),INTENT(OUT)    :: RESULTDATE
                INTEGER(8)                   :: OFFSET_FROM,OFFSET_TO
                INTEGER(8)                   :: OFFSET_TOTAL
                
                OFFSET_FROM = GET_TIMEZONE_OFFSET(TZ1)
                OFFSET_TO   = GET_TIMEZONE_OFFSET(TZ2)
                
                OFFSET_TOTAL = -(OFFSET_FROM - OFFSET_TO)
                
                CALL DATEADD(DATE1,OFFSET_TOTAL,RESULTDATE)
                
                RETURN
                
            END SUBROUTINE TIMEZONE_CHANGE
                
#ifdef _QT
            INTEGER(8) FUNCTION JULIANSEC(MYDATE)
                IMPLICIT NONE
                TYPE(DATEVAR),INTENT(IN) :: MYDATE
                CALL qtdate_juliansec(MYDATE%YEAR,MYDATE%MONTH,&
                                      MYDATE%DAY,MYDATE%HOUR,&
                                      MYDATE%MINUTE,MYDATE%SECOND,&
                                      JULIANSEC)
            END FUNCTION JULIANSEC
#else                
            INTEGER(8) FUNCTION JULIANSEC(MYDATE)
                IMPLICIT NONE
                TYPE(DATEVAR),INTENT(IN) :: MYDATE
                INTEGER(8) :: I,J,K

                I = MYDATE%YEAR
                J = MYDATE%MONTH
                K = MYDATE%DAY

                IF((I.GT.2099).OR.(I.LT.1801))THEN
                    WRITE(*,*) "YEAR OUT OF RANGE!"
                    STOP
                ENDIF

                JULIANSEC = (K-32075+1461*(I+4800+(J-14)/12)/4+367* &
                    (J-2-(J-14)/12*12)/12-3*((I+4900+(J-14)/12)/100)/4)*86400

                JULIANSEC = JULIANSEC + (MYDATE%HOUR*3600) + (MYDATE%MINUTE*60) + MYDATE%SECOND

                RETURN

            END FUNCTION JULIANSEC
#endif            

#ifdef _QT
            TYPE(DATEVAR) FUNCTION GREGORIAN(JUL)
                IMPLICIT NONE
                INTEGER(8),INTENT(IN) :: JUL
                CALL qtdate_gregorian(JUL,GREGORIAN%YEAR,&
                                      GREGORIAN%MONTH,&
                                      GREGORIAN%DAY,&
                                      GREGORIAN%HOUR,&
                                      GREGORIAN%MINUTE,&
                                      GREGORIAN%SECOND)
            END FUNCTION GREGORIAN

#else
            TYPE(DATEVAR) FUNCTION GREGORIAN(JUL)
                IMPLICIT NONE
                INTEGER(8),INTENT(IN) :: JUL
                INTEGER(8)            :: I,J,K,L,N
                INTEGER(8)            :: JULDAY
                INTEGER(8)            :: JULREM

                JULDAY = JUL/86400
                JULREM = JUL-(JULDAY*86400)

                L= JULDAY+68569
                N= 4*L/146097
                L= L-(146097*N+3)/4
                I= 4000*(L+1)/1461001
                L= L-1461*I/4+31
                J= 80*L/2447
                K= L-2447*J/80
                L= J/11
                J= J+2-12*L
                I= 100*(N-49)+I+L

                GREGORIAN%YEAR = I
                GREGORIAN%MONTH = J
                GREGORIAN%DAY = K
                GREGORIAN%HOUR = JULREM/3600
                JULREM = JULREM - GREGORIAN%HOUR*3600
                GREGORIAN%MINUTE = JULREM/60
                JULREM = JULREM - GREGORIAN%MINUTE*60
                GREGORIAN%SECOND = JULREM

                RETURN

            END FUNCTION GREGORIAN
#endif  

            TYPE(DATEVAR) FUNCTION ReadDateString(String)
                IMPLICIT NONE
                CHARACTER(*),INTENT(IN) :: String
                READ(String(1:4),'(I4)')   ReadDateString%Year
                READ(String(5:6),'(I2)')   ReadDateString%Month
                READ(String(7:8),'(I2)')   ReadDateString%Day
                READ(String(9:10),'(I2)')  ReadDateString%Hour
                READ(String(11:12),'(I2)') ReadDateString%Minute
                READ(String(13:14),'(I2)') ReadDateString%Second
            END FUNCTION ReadDateString

        END MODULE DATEMODULE

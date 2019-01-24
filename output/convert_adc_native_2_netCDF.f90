! NETCDF Converter for ADCIRC Native ASCII global output files
! BOB : V1.0 : 30 Dec 04
! BOB : V1.1 : 16 Feb 05 : include proper time units specification
! BOB : V1.2 : 18 Feb,05 : add global attributes for SCOOP project
! BOB : V1.3 : 19 Oct 07 : modified for SCOOP distribution
! BOB : V1.4 : 16 Jun 08 : modified for NCFS
! jgf20110622: 
!  +Updated compilation info
!  +Added the time units string as a command line option

! compile: 
! netCDF must have been compiled with the FORTRAN90 interface included, using the same compiler to be used to compile convert_adc_native_2_netCDF.f90  
! if the netcdf installation is in /opt/local, then the compile line is: 
! ifort -traceback -static -fast convert_adc_native_2_netCDF.f90 -o convert_adc_native_2_netCDF  -L/opt/local/lib -lnetcdf -I/opt/local/include
!
! jgf20110622: The following worked for me on blueridge:
! ifort -traceback -static  Strings.f90 convert_adc_native_2_netCDF.f90 -o convert_adc_native_2_netCDF  -L/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.0.1-icc-ifort/lib -lnetcdf -I/shared/apps/RHEL-5/x86_64/NetCDF/netcdf-4.0.1-icc-ifort/include
!
! jgf20110905: The following worked on garnet at erdc:
!pgf90  -Bstatic Strings.f90 convert_adc_native_2_netCDF.f90 -o convert_adc_native_2_netCDF  -L /opt/cray/netcdf/4.1.1.0/netcdf-pgi/lib -lnetcdff -L/opt/cray/hdf5/default/hdf5-pgi/lib -I /opt/cray/netcdf/4.1.1.0/netcdf-pgi/include -lnetcdf -lhdf5_hl -lhdf5 -lhdf5_fortran -lz
!
! jgf20110907: The following worked on diamond at erdc:
!ifort -traceback -static -cpp Strings.f90 -static Precision.f90 convert_adc_native_2_netCDF.f90 -o convert_adc_native_2_netCDF -L/usr/local/usp/PETtools/CE/pkgs/netcdf-4.0.1-serial/lib -lnetcdf -L/usr/local/usp/PETtools/CE/pkgs/hdf5-1.8.4-serial/lib -I/usr/local/usp/PETtools/CE/pkgs/netcdf-4.0.1-serial/include -lhdf5_hl -lhdf5 -lhdf5_fortran -lz


PROGRAM ADCPOSTNETCDF

IMPLICIT NONE
LOGICAL FILEEXIST,IgnoreDate
INTEGER II,JJ,FORTUNIT,L,IERR
INTEGER UNITLIST(8),NUNITS
LOGICAL            :: cmdLineDateProvided
CHARACTER          :: SH*2,SM*2,SD*2,SYYYY*4

!ARGUMENT PASSING DECLARATIONS.  NOTE: GETARG IS NOT STANDARD F90
!EXTERNAL GETARG
INTEGER*4 IARGC
!EXTERNAL IARGC
CHARACTER(LEN=132) :: FORTFILE
CHARACTER(LEN=2)  :: CUNIT

IgnoreDate=.True.
cmdLineDateProvided=.False.

DATA NUNITS/8/
DATA UNITLIST/61,62,63,64,71,72,73,74/

! IF NO INPUT ARGUMENTS, CHECK AND PROCESS LOCAL 
! FORT.?? FILES.

FORTFILE='fort.xx'

IF (IARGC().EQ.0)THEN
   DO JJ=1,NUNITS
      II=UNITLIST(JJ)
      WRITE(CUNIT,'(I2)')II
      FORTFILE(6:7)=CUNIT
      INQUIRE(FILE=TRIM(FORTFILE),EXIST=FILEEXIST)
      IF (FILEEXIST) THEN
         WRITE(*,'(/A)') TRIM(FORTFILE)//' file found'
         CALL POSTNETCDF(FORTFILE,IgnoreDate,cmdLineDateProvided,SYYYY,SM,SD,SH,IERR)
         IF(IERR.EQ.1)THEN
            WRITE(*,*)'1'
            STOP
         END IF
      ELSE
         WRITE(*,*) TRIM(FORTFILE)//' file NOT found'
!          WRITE(*,*)'1'
      ENDIF
   END DO
   
ELSE
 
   II=1
   DO WHILE(II.le.IARGC())
      CALL GETARG(II,FORTFILE)
      SELECT CASE(FORTFILE(1:2))
      CASE("-y")
         cmdLineDateProvided = .true. ! date must come first in cmd line args
         II=II+1
         CALL GETARG(II,SYYYY)
      CASE("-m")
         II=II+1
         CALL GETARG(II,SM)
      CASE("-d")
         II=II+1
         CALL GETARG(II,SD)
      CASE("-h")
         II=II+1
         CALL GETARG(II,SH)
      CASE DEFAULT
         L=LEN(TRIM(FORTFILE))
         INQUIRE(FILE=TRIM(FORTFILE),EXIST=FILEEXIST)
         IF (FILEEXIST) THEN
!           WRITE(*,'(/A)') TRIM(FORTFILE)//' file found'
            CALL POSTNETCDF(FORTFILE,IgnoreDate,cmdLineDateProvided,SYYYY,SM,SD,SH,IERR)
            IF(IERR.EQ.1)THEN
               WRITE(*,*)'1'
               STOP
            END IF
         ELSE
!           WRITE(*,*) TRIM(FORTFILE)//' file NOT found'
            WRITE(*,*)'1'
            STOP
         ENDIF
      END SELECT
      II=II+1
   END DO
END IF   ! END IF (IARGC.EQ.0)THEN
WRITE(*,*)'0'

END PROGRAM ADCPOSTNETCDF


SUBROUTINE POSTNETCDF(FORTFILE,IgnoreDate,cmdLineDateProvided,SYYYY,SM,SD,SH,IERR)
!
!---------------------------------------------------------------------------C
!                                                                           C
!  This routine builds a netCDF formatted version of the global elevation   C
!  and velocity fields using the already-globalized fort.63 and fort.64     C
!  files.  Eventually, this routine will embed all the globalized           C
!  timeseries output into the netCDF file.                                  C
!                                                                           C
!  Brian O. Blanton                                                         C
!  30 December, 2004                                                        C
!  Department of Marine Sciences                                            C
!  University of NC, Chapel Hill                                            C
!  CB#3300, 12-7 Venable Hall                                               C
!  Chapel Hill, NC, 27599-3300                                              C
!  brian_blanton@unc.edu                                                    C
!                                                                           C
!  This version uses the experimental F90 netCDF 
!  implementation, version 3.5.  This routine will not work with f77
!  due to overloading of the function calls in netCDF F90.
!
! 1) This subroutine must be compiled by telling the compiler the 
!    absolute path to the f90 netCDF modules.  This replaces the 
!    usual "Include 'netcdf.inc'".  With INTEL F90, this might look like:
!    f90 sroucecode.f90 -c -module /usr/local/mod \
!    -L/usr/local/netcdf/lib -lnetcdf.  Linking can then 
!    be done with F77.  The alternative is to copy the typeSizes.mod 
!    and netcdf.mod files to the cwd.  
! 2) Currently, all variables are stored as floating point.  Future
!    versions will scale the fields by expected ranges and store the 
!    fields as short integers.   
!    Many variables have a relatively small dynamic range, and are 
!    therefore scaled and stored as short integers to save space.  
!    The scaling is determined by the specified MIN and MAX of 
!    these variables, which is defined in the following lines.
!    These may need to be changed for your particular case!
!    This will be implemented later.

!---------------------------------------------------------------------------C
!
USE typesizes
USE netcdf
USE strings

! FORCE TYPE DEFINITION
IMPLICIT NONE

INTEGER            :: IERR
LOGICAL            :: IgnoreDate
LOGICAL            :: cmdLineDateProvided

!......netCDF declarations
CHARACTER(LEN=132) :: NETCDFFILENAME, &
                      NETCDFFILETITLE,NETCDFFILEHOST, & 
                      NETCDFFILEINST,NETCDFFILEIURL, & 
                      NETCDFFILEPROJ,NETCDFFILEPURL,NETCDFFILECONT, & 
                      NETCDFFILEMODNAME,NETCDFFILEMODVERS, & 
                      NETCDFFILEMODDOM,NETCDFFILEIODURL,NETCDFFILECONV, & 
                      NETCDFFILEINSTCODE

CHARACTER(LEN=*)   :: FORTFILE
CHARACTER(LEN=2)   :: CUNIT

! netCDF file, dimension, variable ID's
! NETCDF  FILE ID
INTEGER           :: NCFILEID            
! DIMENSION IDS
INTEGER           :: MNPID,NFACEID,MNEID,TIMID           
INTEGER           :: FSCALARID,ISCALARID              
! VARIABLE IDS
INTEGER           :: ZETAID,UBARID,VBARID,CDID,AHID
INTEGER           :: NEID,NNID,TIME_ID
!
INTEGER           :: NN,NE,NEDIM,NODE

! fort file header declarations 
CHARACTER         :: INLINE*132,TIME_UNIT_STR*72,SH*2,SM*2,SD*2,SYYYY*4
INTEGER           :: NDSET,NSTEMP,NP,ITEMP,IT,IT1,NW
INTEGER           :: Y,M,D,KD
REAL              :: DT,HRMN,DT_DAYS,FILVAL
REAL*8            :: FKD1,FCSDKD,FNOWKD,TIMEOUT,TIMESHIFT

REAL, ALLOCATABLE :: ZETA(:),UBAR(:),VBAR(:),UTEMP(:)
INTEGER, ALLOCATABLE :: IFIL(:)

! COUNTERS, FLAGS
INTEGER           :: I,II,J,IDX

INTEGER :: NLineTokens
INTEGER, PARAMETER :: MaxLineTokens=100
CHARACTER(LEN=132), ALLOCATABLE :: LineArgs(:)
LOGICAL            :: IsCompact=.FALSE.
INTEGER            :: L  ! trimmed length of FORTFILE

ALLOCATE(LineArgs(MaxLineTokens))
L=LEN(TRIM(FORTFILE))
CUNIT=FORTFILE(L-1:L)
IERR=0

! Global Attributes for netCDF file
NETCDFFILETITLE='ADCIRC post-globalized netCDF file'  
NETCDFFILEHOST='NaN'
NETCDFFILEINST='NaN'
NETCDFFILEINSTCODE='RENCI'
NETCDFFILEIODURL='NaN'
NETCDFFILEIURL='www.renci.org'
NETCDFFILEPROJ='RENCI/UNC EXPERIMENTAL FORECAST SYSTEM'
NETCDFFILEPURL='www.renci.org'
NETCDFFILECONT='brian_blanton@renci.org'
NETCDFFILEMODNAME='ADCIRC'
NETCDFFILEMODVERS='50.xx'
NETCDFFILEMODDOM='nc6b'
NETCDFFILECONV='CF-x.x'

! SET OUTPUT netCDF FILE NAME 
NETCDFFILETITLE=TRIM(NETCDFFILETITLE)//' for '//TRIM(FORTFILE)
NETCDFFILENAME=TRIM(FORTFILE)//'.nc'

!OPEN INPUT FORT FILE 
OPEN(199,FILE=FORTFILE)
READ(199,'(A85)') INLINE
WRITE(*,'(A85)')INLINE

if (.NOT.IgnoreDate)THEN
   BACKSPACE(199)
   READ(199,'(I4,1x,I2,1x,I4)')M,D,Y
   WRITE(*,*)M,D,Y
   CALL GDAY(D,M,Y,KD)
   FCSDKD=FLOAT(KD)
END IF

READ(199,'(a)')INLINE
READ(INLINE,*) NDSET,NP,DT,NSTEMP,ITEMP
WRITE(*,*) NDSET,NP,DT,NSTEMP,ITEMP
!3645 FORMAT(1X,I10,1X,I10,1X,E15.7,1X,I5,1X,I5)

DT_DAYS=DT/86400.D0

! get first time in file
READ(199,*)FKD1,IT1
BACKSPACE(199)
IF (IgnoreDate.eqv..false.) THEN
   ! ADD COLD_START_DATE to MODEL TIME
   FKD1=FKD1/86400.+FCSDKD
   
   ! TIMESHIFT FROM MODEL TIME TO GREGORIAN TIME 
   TIMESHIFT=FKD1-FCSDKD-DT_DAYS

   !Truncate hour part of FKD1
   HRMN=(24.0*(FKD1-DT_DAYS-INT(FKD1)))
   FKD1=FLOAT(INT(FKD1))

   ! CONVERT GREGORIAN DAY TO D/M/Y 
   CALL DMY(D,M,Y,INT(FKD1))

   ! CONVERT TIMES TO STRINGS
   WRITE(SH,'(I2.2)')INT(HRMN)
   WRITE(SD,'(I2.2)')D
   WRITE(SM,'(I2.2)')M
   WRITE(SYYYY,'(I4)')Y

   ! FORM NEW time:units WITH GREGORIAN TIME
   TIME_UNIT_STR='Hours since '//SYYYY//'-'//SM//'-'//SD//' '//SH//':00:00 UTC'

   WRITE(*,*)'  COLD-START D,M,Y=',D,M,Y
   WRITE(*,*)'  COLD-START KD   =',FCSDKD
ELSE IF (cmdLineDateProvided.eqv..true.) THEN
   ! this represents the run start time
   TIME_UNIT_STR='Hours since '//SYYYY//'-'//SM//'-'//SD//' '//SH//':00:00 UTC'
   TIMESHIFT=FKD1/86400.D0
ELSE
   TIME_UNIT_STR='Hours'
END IF



WRITE(*,3646) NDSET,NP,DT,NSTEMP,ITEMP,TIMESHIFT
3646 FORMAT('NDSET =',I/,'NP    =',I/,'DT    =',F/,'NSTEMP=',I/,'ITEMP =',I/,'TIMESHIFT=',F)

WRITE(*,*)'   New TIME_UNIT_STR=',TRIM(TIME_UNIT_STR)

SELECT CASE (CUNIT)
CASE ('61','63','71','73')
   ALLOCATE(ZETA(NP))
CASE ('62','64','72','74')
   ALLOCATE(UBAR(NP),VBAR(NP))
END SELECT

! CREATE netCDF FILE 
WRITE(*,*)'   Creating netCDF file = '//TRIM(NETCDFFILENAME)
CALL STATUSCHECK(NF90_CREATE(PATH=TRIM(NETCDFFILENAME),CMODE=NF90_CLOBBER,NCID=NCFILEID))

!begin define-mode
! global attributes
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='title',                   VALUES=NETCDFFILETITLE))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='host',                    VALUES=NETCDFFILEHOST))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='institution',             VALUES=NETCDFFILEINST))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='institution_code',        VALUES=NETCDFFILEINSTCODE))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='institution_url',         VALUES=NETCDFFILEIURL))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='institution_OPeNDAP_url', VALUES=NETCDFFILEIODURL))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='contact',                 VALUES=NETCDFFILECONT))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='project',                 VALUES=NETCDFFILEPROJ))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='project_url',             VALUES=NETCDFFILEPURL))
!CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='start_datenum',           VALUES=FKDSTART))
!CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='end_datenum',             VALUES='Run Not Finished!'))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='model_name',              VALUES=NETCDFFILEMODNAME))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='model_version',           VALUES=NETCDFFILEMODVERS))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='model_domain',            VALUES=NETCDFFILEMODDOM))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='Conventions',             VALUES=NETCDFFILECONV))
!CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='history',                 VALUES=NETCDFFILEMODDOM))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='format_category',         VALUES='ocean_state_estimate'))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='_FillValue',              VALUES=nf90_fill_float))
!CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='missing_value',           VALUES=nf90_fill_float))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='missing_value',           VALUES=-99999.))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=NF90_GLOBAL,NAME='dry_value',               VALUES=-99999.))

! define dimensions
WRITE(*,*)'      Defining dimensions ...'
CALL STATUSCHECK(NF90_DEF_DIM(NCID=NCFILEID,NAME='node',        LEN=NP,            DIMID=MNPID))
CALL STATUSCHECK(NF90_DEF_DIM(NCID=NCFILEID,NAME='time',        LEN=NF90_UNLIMITED,DIMID=TIMID))
CALL STATUSCHECK(NF90_DEF_DIM(NCID=NCFILEID,NAME='float_scalar',LEN=1,             DIMID=FSCALARID))
CALL STATUSCHECK(NF90_DEF_DIM(NCID=NCFILEID,NAME='int_scalar',  LEN=1,             DIMID=ISCALARID))

! define variables
WRITE(*,*)'      Defining variables ...'
! Scalars

! Vectors
CALL STATUSCHECK(NF90_DEF_VAR(NCID=NCFILEID,               NAME='time',         XTYPE=NF90_FLOAT, DIMIDS=(/TIMID/),VARID=TIME_ID))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=TIME_ID, NAME='long_name',    VALUES='Time of HC or FC'))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=TIME_ID, NAME='units',        VALUES=TRIM(TIME_UNIT_STR)))
CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=TIME_ID, NAME='standard_name',VALUES='time'))

SELECT CASE (CUNIT)
CASE ('61','63')

   CALL STATUSCHECK(NF90_DEF_VAR(NCID=NCFILEID,NAME='zeta',  XTYPE=NF90_FLOAT,DIMIDS=(/MNPID, TIMID/),VARID=ZETAID))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='_FillValue',   VALUES=nf90_fill_float))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='missing_value',VALUES=-99999.))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='dry_value',    VALUES=-99999.))
   SELECT CASE(trim(FORTFILE))
   CASE("fort.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Surface Elevation'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='m'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='sea_surface_elevation'))
   CASE("maxele.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Maximum Surface Elevation'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='m'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='sea_surface_elevation'))
   CASE("maxrs.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Maximum Wave Radiation Stress'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='m/s^2'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='wave_radiation_stress'))
   CASE("maxvel.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Maximum Water Speed'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='m/s'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='sea_water_speed'))
   CASE("maxwvel.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Maximum Wind Speed'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='m/s'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='wind_speed'))
   CASE("minpr.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Minimum Surface Atmospheric Pressure'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='m H2O'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='wind_speed'))
   CASE("swan_DIR.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Mean Wave Direction'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='degrees'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='mean_wave_direction'))
   CASE("swan_DIR_max.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Maximum Mean Wave Direction'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='degrees'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='mean_wave_direction'))
   CASE("swan_HS.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Significant Wave Height'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='m'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='significant_wave_height'))
   CASE("swan_HS_max.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Maximum Significant Wave Height'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='m'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='maximum_significant_wave_height'))
   CASE("swan_TMM10.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Mean Absolute Wave Period'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='s'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='mean_absolute_wave_period'))
   CASE("swan_TMM10_max.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Maximum Mean Absolute Wave Period'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='s'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='maximum_mean_absolute_wave_period'))
   CASE("swan_TPS.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Peak Wave Period'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='s'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='peak_wave_period'))
   CASE("swan_TPS_max.63")
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Maximum Peak Wave Period'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='s'))
      CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='maximum_peak_wave_period'))
   CASE DEFAULT
      WRITE(*,*) "ERROR: convert_adc_native_2_netcdf.f90:"
      WRITE(*,*) "The file name ",trim(FORTFILE)," was not recognized."
      WRITE(*,*) "This file will not be processed."
   END SELECT
CASE ('71','73')

   ! We will use the same dimid's as 61,63 so that the code further below does not need amending
   
   CALL STATUSCHECK(NF90_DEF_VAR(NCID=NCFILEID,NAME='SurfAtmPress',  XTYPE=NF90_FLOAT,DIMIDS=(/MNPID, TIMID/),VARID=ZETAID))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='long_name',    VALUES='Surface Atm Pressure'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='units',        VALUES='m H2O'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='standard_name',VALUES='surface_atmos_press'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='_FillValue',   VALUES=nf90_fill_float))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='missing_value',VALUES=-99999.))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=ZETAID, NAME='dry_value',    VALUES=-99999.))
   
CASE ('62','64')

   CALL STATUSCHECK(NF90_DEF_VAR(NCID=NCFILEID,NAME='ubar',  XTYPE=NF90_FLOAT,DIMIDS=(/MNPID, TIMID/),VARID=UBARID))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='long_name',    VALUES='Vert. Avg. E/W Velocity'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='units',        VALUES='m/s'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='standard_name',VALUES='eastward_sea_water_velocity'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='_FillValue',   VALUES=nf90_fill_float))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='missing_value',VALUES=-99999.))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='dry_value',    VALUES=-99999.))

   CALL STATUSCHECK(NF90_DEF_VAR(NCID=NCFILEID,NAME='vbar',  XTYPE=NF90_FLOAT,DIMIDS=(/MNPID, TIMID/),VARID=VBARID))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='long_name',    VALUES='Vert. Avg. N/S Velocity'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='units',        VALUES='m/s'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='standard_name',VALUES='northward_sea_water_velocity'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='_FillValue',   VALUES=nf90_fill_float))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='missing_value',VALUES=-99999.))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='dry_value',    VALUES=-99999.))

CASE ('72','74')

   ! We will use the same dimid's as 62,64 so that the code further below does not need amending

   CALL STATUSCHECK(NF90_DEF_VAR(NCID=NCFILEID,NAME='TauX',  XTYPE=NF90_FLOAT,DIMIDS=(/MNPID, TIMID/),VARID=UBARID))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='long_name',    VALUES='E/W Wind Velocity'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='units',        VALUES='m/s'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='standard_name',VALUES='eastward_surface_wind_velocity'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='_FillValue',   VALUES=nf90_fill_float))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='missing_value',VALUES=-99999.))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=UBARID, NAME='dry_value',    VALUES=-99999.))

   CALL STATUSCHECK(NF90_DEF_VAR(NCID=NCFILEID,NAME='TauY',  XTYPE=NF90_FLOAT,DIMIDS=(/MNPID, TIMID/),VARID=VBARID))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='long_name',    VALUES='N/S Wind Velocity'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='units',        VALUES='m/s'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='standard_name',VALUES='northward_surface_wind_velocity'))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='_FillValue',   VALUES=nf90_fill_float))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='missing_value',VALUES=-99999.))
   CALL STATUSCHECK(NF90_PUT_ATT(NCID=NCFILEID,VARID=VBARID, NAME='dry_value',    VALUES=-99999.))
   
END SELECT

! END OF DEFINE MODE
CALL STATUSCHECK(NF90_ENDDEF(NCFILEID))

! Determine if file is compact or not.
READ(199,'(a)',END=999)INLINE
call parse(INLINE,' ',LineArgs,NLineTokens)
If (NLineTokens.EQ.4)IsCompact=.True.
BACKSPACE(199)
IF (IsCompact) THEN
   WRITE(*,*)'File is compact.'
ELSE
   WRITE(*,*)'File is not compact.'
ENDIF

SELECT CASE (CUNIT)
CASE ('61','63','71','73')
   I=1
   DO   ! loop until file ends 
      IF (IsCompact) THEN
      
         READ(199,*,END=999,ERR=998)TIMEOUT,IT,NW,FILVAL
         TIMEOUT=(TIMEOUT/86400.D0 - TIMESHIFT)*24.
         WRITE(*,'(I6,1x,F10.4,1x,I10)')I,TIMEOUT,IT
         ZETA=FILVAL
         ALLOCATE(UTEMP(NW),IFIL(NW))
         READ(199,*,END=999,ERR=998)(IFIL(J),UTEMP(J),J=1,NW)
         ZETA(IFIL)=UTEMP
         DEALLOCATE(UTEMP,IFIL)

      ELSE
      
         READ(199,*,END=999,ERR=998)TIMEOUT,IT
         TIMEOUT=(TIMEOUT/86400.D0 - TIMESHIFT)*24.
         WRITE(*,'(I6,1x,F10.4,1x,I10)')I,TIMEOUT,IT

         DO J=1,NP
            READ(199,*,END=999,ERR=998)II,ZETA(J)
         END DO
	 !WRITE(*,*)II,ZETA(NP)

      END IF

      CALL STATUSCHECK(NF90_PUT_VAR(NCID=NCFILEID,VARID=TIME_ID,VALUES=TIMEOUT,START=(/I/)))
      CALL STATUSCHECK(NF90_PUT_VAR(NCID=NCFILEID,VARID=ZETAID,VALUES=ZETA,START=(/1,I/)))
      I=I+1
   END DO

CASE ('62','64','72','74')
   I=1
   DO  ! loop until file ends

      READ(199,*,END=999,ERR=998)TIMEOUT,IT
      TIMEOUT=(TIMEOUT/86400.D0 - TIMESHIFT)*24.
      WRITE(*,*)I,TIMEOUT,IT

      DO J=1,NP
         READ(199,*,END=999,ERR=998)II,UBAR(J),VBAR(J)
      END DO

      CALL STATUSCHECK(NF90_PUT_VAR(NCID=NCFILEID,VARID=TIME_ID,VALUES=TIMEOUT,START=(/I/)))
      CALL STATUSCHECK(NF90_PUT_VAR(NCID=NCFILEID,VARID=UBARID,VALUES=UBAR,START=(/1,I/)))
      CALL STATUSCHECK(NF90_PUT_VAR(NCID=NCFILEID,VARID=VBARID,VALUES=VBAR,START=(/1,I/)))
      I=I+1
   END DO

END SELECT

GOTO 1000
998 WRITE(*,*) "ERROR: convert_adc_native_2_netcdf: Error reading dataset ",I,TIMEOUT,IT," line ",J,"."

999 CONTINUE

1000 CONTINUE

! FINALLY, CLOSE NETCDF FILE
CALL STATUSCHECK(NF90_CLOSE(NCFILEID))

! OK, REALLY FINALLY, CLOSE INPUT FORT FILE
CLOSE(199)

END SUBROUTINE POSTNETCDF


! SUBROUTINE THAT EVALUATES THE RETURN FLAG STATUS FROM EACH CALL TO netCDF
SUBROUTINE STATUSCHECK(FLAG)
USE typesizes
USE netcdf
INTEGER, INTENT (IN) :: FLAG
IF(FLAG /= NF90_NOERR)THEN
   WRITE(*,*)TRIM(NF90_STRERROR(FLAG))
   WRITE(*,*)'1'
   STOP
END IF
END SUBROUTINE STATUSCHECK


!*********************************************************************
!*********************************************************************
      SUBROUTINE GDAY(IDD,IMM,IYear,KD)
!
!  GIVEN DAY,MONTH,(EACH 2 DIGITS) and year (four digits), GDAY RETURNS
!  THE DAY#, KD BASED ON THE GREGORIAN CALENDAR.
!  THE GREGORIAN CALENDAR, CURRENTLY 'UNIVERSALLY' IN USE WAS
!  INITIATED IN EUROPE IN THE SIXTEENTH CENTURY. NOTE THAT GDAY
!  IS VALID ONLY FOR GREGORIAN CALENDAR DATES.
!
!   KD=1 CORRESPONDS TO JANUARY 1, 0000
!       
!       Note that the Gregorian reform of the Julian calendar 
!       omitted 10 days in 1582 in order to restore the date
!       of the vernal equinox to March 21 (the day after
!       Oct 4, 1582 became Oct 15, 1582), and revised the leap 
!       year rule so that centurial years not divisible by 400
!       were not leap years.
!
!   THIS ROUTINE WAS WRITTEN BY EUGENE NEUFELD, AT IOS, IN JUNE 1990.
!
      INTEGER NDP(13)
      INTEGER NDM(12)
        
      DATA NDP/0,31,59,90,120,151,181,212,243,273,304,334,365/
      DATA NDM/31,28,31,30,31,30,31,31,30,31,30,31/
!
        lp=6
! make iyy and icc variables
        icc=iyear/100
        iyy=iyear-icc*100

!  TEST FOR INVALID INPUT:
      IF(ICC.LT.0)THEN
         WRITE(LP,5000)ICC
         STOP
      ENDIF
      IF(IYY.LT.0.OR.IYY.GT.99)THEN
         WRITE(LP,5010)IYY
         STOP
      ENDIF
      IF(IMM.LE.0.OR.IMM.GT.12)THEN
         WRITE(LP,5020)IMM
         STOP
      ENDIF
      IF(IDD.LE.0)THEN
         WRITE(LP,5030)IDD
         STOP
      ENDIF
      IF(IMM.NE.2.AND.IDD.GT.NDM(IMM))THEN
         WRITE(LP,5030)IDD
         STOP
      ENDIF
      IF(IMM.EQ.2.AND.IDD.GT.29)THEN
         WRITE(LP,5030)IDD
         STOP
      ENDIF
      IF(IMM.EQ.2.AND.IDD.GT.28.AND.((IYY/4)*4-IYY.NE.0.OR.(IYY.EQ.0.AND.(ICC/4)*4-ICC.NE.0)))THEN
         WRITE(LP,5030)IDD
         STOP
      ENDIF
5000  FORMAT(' INPUT ERROR. ICC = ',I7)
5010  FORMAT(' INPUT ERROR. IYY = ',I7)
5020  FORMAT(' INPUT ERROR. IMM = ',I7)
5030  FORMAT(' INPUT ERROR. IDD = ',I7)
!
!  CALCULATE DAY# OF LAST DAY OF LAST CENTURY:
      KD = ICC*36524 + (ICC+3)/4
!
!  CALCULATE DAY# OF LAST DAY OF LAST YEAR:
      KD = KD + IYY*365 + (IYY+3)/4
!
!  ADJUST FOR CENTURY RULE:
!  (VIZ. NO LEAP-YEARS ON CENTURYS EXCEPT WHEN THE 2-DIGIT
!  CENTURY IS DIVISIBLE BY 4.)
      IF(IYY.GT.0.AND.(ICC-(ICC/4)*4).NE.0) KD=KD-1
!  KD NOW TRULY REPRESENTS THE DAY# OF THE LAST DAY OF LAST YEAR.
!
!  CALCULATE DAY# OF LAST DAY OF LAST MONTH:
      KD = KD + NDP(IMM)
!
!  ADJUST FOR LEAP YEARS:
      IF(IMM.GT.2.AND.((IYY/4)*4-IYY).EQ.0.AND.((IYY.NE.0).OR.(((ICC/4)*4-ICC).EQ.0)))   KD=KD+1
!  KD NOW TRULY REPRESENTS THE DAY# OF THE LAST DAY OF THE LAST
!  MONTH.
!
!  CALCULATE THE CURRENT DAY#:
      KD = KD + IDD
      RETURN
!
!
      ENTRY DMY(IDD,IMM,IYear,KD)
!
!  GIVEN THE (GREGORIAN) DAY#, KD, AS CALCULATED ABOVE IN THIS ROUTINE,
!  ENTRY DMY RETURNS THE (GREGORIAN) DAY, MONTH, YEAR AND CENTURY.
!
!  TEST FOR VALID INPUT:
      IF(KD.LE.0) WRITE(LP,5040)KD
5040  FORMAT(' KD = ',I7,'  INVALID INPUT. DMY STOP.')
!
!  SAVE KD
      KKD=KD
!  CALCULATE ICC AND SUBTRACT THE NUMBER OF DAYS REPRESENTED BY ICC
!  FROM KKD
!  JFH IS THE NUMBER OF 400 YEAR INTERVALS UP TO KKD
!  JCC IS THE NUMBER OF ADDITIONAL CENTURIES UP TO KKD
      JFH = KKD/146097
      KKD = KKD - JFH*146097
      IF(KKD.LT.36525)THEN
         JCC = 0
      ELSE
         KKD = KKD - 36525
         JCC = 1 + KKD/36524
         KKD = KKD - (JCC-1)*36524
      END IF
      ICC = 4*JFH + JCC
      IF(KKD.EQ.0)THEN
         ICC = ICC-1
         IYY = 99
         IMM = 12
         IDD = 31
!        RETURN
        go to 110
      ENDIF
!
!  CALCULATE IYY. JFY IS THE NUMBER OF FOUR YEAR INTERVALS IN THE
!  CURRENT CENTURY. THE FIRST FOUR YEAR INTERVAL IS SHORT (1460 DAYS
!  RATHER THAN 1461)IF THE CURRENT CENTURY IS NOT DIVISIBLE BY 4, AND
!  IN THIS CASE JCC.NE.0 AS CALCULATED ABOVE.
!
!  CALCULATE JFY:
      JFY = 0
      IF(JCC.EQ.0)GOTO 10
      IF(KKD.LT.1460)GOTO 10
      JFY = 1
      KKD = KKD - 1460
10    KK = KKD/1461
      JFY = JFY + KK
      KKD = KKD - KK*1461
!
!  CALCULATE JYY, THE REMAINING YEARS OF THE CURRENT CENTURY UP TO THE
!  CURRENT DAY:
      JYY = 0
!  THE NEXT YEAR IS NOT A LEAP YEAR IF JFY=0 AND JCC.NE.0.
      IF(JFY.EQ.0.AND.JCC.NE.0)GOTO 20
      IF(KKD.LT.366)GOTO 30
      JYY = 1
      KKD = KKD - 366
20    JYYY = KKD/365
      JYY = JYY + JYYY
      KKD = KKD - JYYY*365
30    IYY = 4*JFY + JYY
      IF(KKD.EQ.0) THEN
         IYY=IYY-1
         IMM=12
         IDD=31
!        RETURN
        go to 110
      END IF
!
!  SET L=1 IF WE HAVE A LEAP YEAR.
      L=0
      IF(IYY-(IYY/4)*4.NE.0)GOTO 40
      IF(IYY.EQ.0.AND.(ICC-(ICC/4)*4).NE.0)GOTO 40
      L=1
!
!  CALCULATE IMM AND IDD
40    IF(KKD.GT.31) GOTO 50
      IMM=1
      IDD=KKD
!      RETURN
       go to 110
!
50    IF(KKD.GT.59)GOTO 60
      IMM = 2
      IDD = KKD-31
!      RETURN
        go to 110
!
60    IF(KKD.GT.60)GOTO 70
      IF(L.EQ.0)GOTO 70
      IMM = 2
      IDD = 29
!      RETURN
        go to 110
!!
70    IF(L.EQ.1) KKD=KKD-1
      DO 80 I=4,13
         IF(KKD.GT.NDP(I))GOTO 80
         IMM = I-1
         IDD = KKD - NDP(I-1)
!        RETURN
        go to 110
!!
80    CONTINUE

 110    iyear=icc*100+iyy
        return

90    WRITE(LP,5050)
5050  FORMAT(' ERROR IN DMY.')
      STOP
      END

        Subroutine Up_date(kd,ssec)
! 
! this subroutine updates the current gregorian day number kd
! and resets the time, ssec, accordingly
! it inherently assumes that the variable is updated every day!

      integer kd
      real ssec
        
      kd=kd+aint(ssec/86400.0)
      ssec=MOD(ssec,86400.0)
 
      return
      end


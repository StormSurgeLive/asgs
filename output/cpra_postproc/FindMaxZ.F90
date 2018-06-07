    Program FindMaxZ

    IMPLICIT NONE

    INTEGER                 :: I
    INTEGER                 :: NE,NP,tempI
    INTEGER                 :: ietamax
    REAL(8),ALLOCATABLE     :: eta(:)
    REAL(8)                 :: x,y,z
    REAL(8)                 :: etamax
    REAL(8)                 :: cint

    REAL(8)                 :: xmin = -89.5
    REAL(8)                 :: xmax = -83.5
    REAL(8)                 :: ymin = 28.75
    REAL(8)                 :: ymax = 31.25

    CHARACTER(100)          :: replace
    CHARACTER(9)            :: czmax
    CHARACTER(9)            :: ccint
    CHARACTER(100)          :: CMD

    LOGICAL                 :: fe

    etamax = -999

    INQUIRE(FILE='maxele.63',EXIST=fe)

    IF (.NOT.fe) THEN
        INQUIRE(FILE='maxele.63.nc',EXIST=fe)
        IF (fe) THEN
            CALL SYSTEM("netcdf2adcirc --datafile maxele.63.nc")
        ELSE
            STOP
        ENDIF
    ENDIF

    OPEN(UNIT=63,FILE='maxele.63',ACTION='READ')
    READ(14,*)
    READ(14,*)NE,NP
    READ(63,*)
    READ(63,*)
    READ(63,*)

    ALLOCATE(eta(NP))

    DO I = 1, NP
        READ(14,*)tempI,x,y,z
        READ(63,*)tempI,eta(tempI)
        IF( (x.lt.xmax).AND.(x.gt.xmin).and.(y.lt.ymax).and.(y.gt.ymin) ) THEN
            IF(eta(tempI).GT.etamax) etamax = eta(tempI)
        ENDIF
    ENDDO
    CLOSE(63)

    OPEN(UNIT=10,FILE='zmax.txt',STATUS='UNKNOWN')
    WRITE(10,*)NINT(etamax)
    CLOSE(10)
    
    ietamax = NINT(etamax)
    WRITE(czmax,'(I2)')ietamax
    replace = """s/%zmax%/"//TRIM(czmax)//"     "//"/g"""
    CMD = "sed -i "//TRIM(replace)//" FG51_GOM_maxele.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z1.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z2.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z3.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z4.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z5.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z6.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z7.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z8.inp"
    CALL SYSTEM(TRIM(CMD))

    WRITE(czmax,'(F7.2)')etamax
    replace = """s/%zmax%/"//ADJUSTR(czmax)//"     "//"/g"""
    CMD = "sed -i "//TRIM(replace)//" labels.txt"
    CALL SYSTEM(TRIM(CMD))
    CALL SYSTEM("mv labels.txt labels.temp.txt")
    CMD = "tr -d ' ' < labels.temp.txt > labels.txt"
    CALL SYSTEM(TRIM(CMD))
    CALL SYSTEM("rm labels.temp.txt")
    replace = """s/MaxSurge/Max Surge/g"""
    CMD = "sed -i "//TRIM(replace)//" labels.txt"
    CALL SYSTEM(TRIM(CMD))

    IF (ietamax > 6) then
        cint = 1.0
    ELSE
        cint = 0.5
    ENDIF
    WRITE(ccint,'(F3.1)')cint
    replace = """s/%cint%/"//TRIM(ccint)//"      "//"/g"""
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_GOM_maxele.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z1.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z2.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z3.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z4.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z5.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z6.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z7.inp"
    CALL SYSTEM(TRIM(CMD))
    CMD = "sed -i "//TRIM(replace)//" FG51_NGOM_maxele_z8.inp"
    CALL SYSTEM(TRIM(CMD))


    INQUIRE(FILE='maxele.63.nc',EXIST=fe)
    IF(fe)THEN
        INQUIRE(FILE='maxele.63',EXIST=fe)
        IF(fe) CALL SYSTEM("rm maxele.63")
    ENDIF

    END PROGRAM


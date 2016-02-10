# CMake project file for NORMA

cmake_minimum_required (VERSION 2.6)
project (ASGS)
enable_language (Fortran)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

# default installation
get_filename_component (default_prefix ".." ABSOLUTE)
set (CMAKE_INSTALL_PREFIX ${default_prefix} CACHE STRING
      "Choose the installation directory; by default it installs in the directory where the source code was found."
      FORCE)

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -cpp -ffree-line-length-none")
  set (CMAKE_Fortran_FLAGS_DEBUG   " -cpp -ffree-line-length-none -g -O0 -fbacktrace -fbounds-check -ffpe-trap=zero,invalid,underflow,overflow,denormal")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-fpp")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -O0 -fpp -traceback -debug -check all")
elseif (Fortran_COMPILER_NAME MATCHES "pgf90")
  # pgf90
  set (CMAKE_Fortran_FLAGS_RELEASE "-Mpreprocess -Mextend -Minform,inform -tp=shanghai-64 -fast")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-Mextend -g -O0 -traceback -Mbounds -Mchkfpstk -Mchkptr -Mchkstk -DALL_TRACE -DFLUSH_MESSAGES -DFULL_STACK -tp=shanghai-64")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

# build executables
# TODO: Fix and add ec2001v2d_tide_interp
set (EXECUTABLES tide_fac FES952_interp tides_ec2001)
foreach (p ${EXECUTABLES}) 
   add_executable ("${p}.x" "${p}.f")
endforeach (p)

# install executables and scripts
foreach (p ${EXECUTABLES})
   install (TARGETS "${p}.x" RUNTIME DESTINATION "tides")
endforeach (p)

## build executables
#set (NMPROGRAMS "diagstd" "diagrtb" "proj_modes_bin" "pdbmat")
#set (EXECUTABLES "NORMA.exe" ${NMPROGRAMS})
#set (SCRIPTS "gen_pert.sh" "pert_multi_mode.sh")

##add_executable ("NORMA.exe" "NORMA.f")
#foreach (p ${NMPROGRAMS})
#  add_executable (${p} "${p}.f")
#endforeach (p)

## install executables and scripts
#install (TARGETS ${EXECUTABLES} 
#         RUNTIME DESTINATION "bin")
#install (PROGRAMS ${SCRIPTS}
#         DESTINATION "bin") 

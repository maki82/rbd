cmake_minimum_required(VERSION 3.7.0)
enable_language(Fortran)


SET(GCC_COVERAGE_COMPILE_FLAGS "-g -Og")
#SET(GCC_COVERAGE_LINK_FLAGS    "-lgcov")

SET( CMAKE_Fortran_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}" )
#SET( CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${GCC_COVERAGE_LINK_FLAGS}" )


set(CMAKE_CXX_COMPILER "/usr/bin/g++")
set(CMAKE_C_COMPILER "/usr/bin/gcc")
set(CMAKE_Fortran_COMPILER "/usr/bin/gfortran-8")

find_package(BLAS)
find_package(LAPACK)
#find_library(BLAS)
#find_library(LAPACK)
if(LAPACK_FOUND AND BLAS_FOUND)
   set(lapackblas_libraries "${BLAS_LIBRARIES} ${LAPACK_LIBRARIES}")
endif()

#add_library(${lapackblas_libraries})
project (rbd)
add_executable(rbd rbd.f90 point.f90 point_sub1.f90 basic.f90 joints.f90)
target_link_libraries(rbd -llapack -lblas)





cmake_minimum_required(VERSION 3.0.0)
enable_language(Fortran)
set(CMAKE_CXX_COMPILER "/usr/bin/g++")
set(CMAKE_C_COMPILER "/usr/bin/gcc")
set(CMAKE_Fortran_COMPILER "/usr/bin/gfortran-8")

project (rbd)
add_executable(rbd rbd.f90 body.f90 body_sub1.f90)

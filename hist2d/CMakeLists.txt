project(hist2d)
cmake_minimum_required(VERSION 2.8)
set(CMAKE_MODULE_PATH "/home/phil/cmake")
set(LIBRARY_OUTPUT_PATH  ${CMAKE_BINARY_DIR}/testlib)
set(CMAKE_INSTALL_PREFIX /home/phil/lib/python)
set(CYTHON_CXX_EXTENSION "cxx")
set(CYTHON_EXECUTABLE "cython")

include(UseCython)
find_package(PythonLibs27 REQUIRED)
find_package(Numpy27 REQUIRED)
include_directories(${PYTHON_INCLUDE_PATH} ${NUMPY_INCLUDE_PATH})

#python executable name
set(PYTHON python27)
include_directories(${PYTHON_INCLUDE_PATH} ${NUMPY_INCLUDE_PATH} ${CMAKE_SOURCE_DIR})

set_source_files_properties(
  ${CMAKE_SOURCE_DIR}/hist2d.pyx
  PROPERTIES CYTHON_IS_CXX TRUE )

compile_pyx(hist2d test hist2d.pyx)
add_custom_target(doit ALL SOURCES ${test})
message("test target is ${test}")
add_library(hist2d SHARED ${test})
set_target_properties(hist2d PROPERTIES PREFIX "")
install(TARGETS hist2d LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX})

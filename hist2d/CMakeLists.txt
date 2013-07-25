project(hist2d)
cmake_minimum_required(VERSION 2.8)
message("system is ${CMAKE_SYSTEM_NAME}")
set(homedir $ENV{HOME})
set(CMAKE_MODULE_PATH "${homedir}/cmake")
set(LIBRARY_OUTPUT_PATH  ${CMAKE_BINARY_DIR}/testlib)
set(CMAKE_INSTALL_PREFIX ${homedir}/lib/python)
set_source_files_properties(
  ${hist2d_SOURCE_DIR}/hist2d.pyx
  PROPERTIES CYTHON_IS_CXX TRUE )


include_directories(${PYTHON_INCLUDE_PATH} ${NUMPY_INCLUDE_PATH} ${CMAKE_SOURCE_DIR})
include(UseCython)
message("python is ${PYTHON_EXECUTABLE}")


set(CYTHON_CXX_EXTENSION "cxx")

find_package(PythonLibs REQUIRED)
find_package(Numpy REQUIRED)
find_package(PyInclude REQUIRED)
include_directories(${TRUE_PYTHON_INC} ${NUMPY_INCLUDE_PATH} ${hist2d_SOURCE_DIR}})


cython_add_module(hist2d hist2d.pyx)

set_target_properties(hist2d PROPERTIES PREFIX "")
install(TARGETS hist2d LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX})

add_custom_target(test COMMAND ${PYTHON_EXECUTABLE} ${hist2d_SOURCE_DIR}/test_hist.py 
                    hist2d ${LIBRARY_OUTPUT_PATH}
                    DEPENDS hist2d COMMENT "execute test_hist.py")

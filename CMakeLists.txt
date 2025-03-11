cmake_minimum_required(VERSION 3.25.1)
project(scr VERSION 1.0.0)

# Define INSTALL_PREFIX and PROJECT_VERSION as cache variables
set(INSTALL_PREFIX "" CACHE STRING "The installation prefix")
set(PROJECT_VERSION ${PROJECT_VERSION} CACHE STRING "The project version")

# Set default INSTALL_PREFIX if not specified
if(NOT INSTALL_PREFIX)
    set(INSTALL_PREFIX "/usr/local")
endif()

# Set the CMAKE_INSTALL_PREFIX to the INSTALL_PREFIX variable
set(CMAKE_INSTALL_PREFIX ${INSTALL_PREFIX})

set(COMPILER_INFO "${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")
message(STATUS "Compiler Info: ${COMPILER_INFO}")

configure_file(
    ${PROJECT_SOURCE_DIR}/src/include/config.h.in
    ${PROJECT_SOURCE_DIR}/src/include/config.h
    @ONLY
)

# Include directories
include_directories(${PROJECT_SOURCE_DIR}/src/include)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set LLVM include directories
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Source files
file(GLOB_RECURSE SOURCES
    src/*.cxx
    src/include/ds/*.hxx
    src/main.cxx
)

# Add executable
add_executable(scr ${SOURCES})

# Link LLVM libraries
llvm_map_components_to_libnames(LLVM_LIBS core orcjit native)
target_link_libraries(scr ${LLVM_LIBS})

# Install targets
install(TARGETS scr DESTINATION bin)

# Uninstall target
if(NOT TARGET uninstall)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
    IMMEDIATE @ONLY)

  add_custom_target(uninstall
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
endif()

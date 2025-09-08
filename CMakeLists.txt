cmake_minimum_required(VERSION 3.25)

project(sila)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# TODO: fail build if LLVM is not found or it's in wrong version.

add_executable(sila)

set(CMAKE_STATIC_LIBRARY_PREFIX "")
add_library(libsila)

set(SILA_TARGETS sila libsila)

foreach(target ${SILA_TARGETS})
  target_compile_features(${target} PRIVATE cxx_std_20)

  target_compile_options(${target} PRIVATE
    -Wall
    -Wextra
    -Wconversion
    -Wdouble-promotion
    -Wno-unused-parameter
    -Wno-unused-function
    -Wno-sign-conversion
    -fsanitize=undefined
    -fsanitize-trap
  )

  target_include_directories(${target} PUBLIC
    $<INSTALL_INTERFACE:include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  )
endforeach()

add_subdirectory(lib)
add_subdirectory(driver)

# Find the libraries that correspond to the LLVM components that we wish to use.
llvm_map_components_to_libnames(llvm_libs support core irreader)

# Link against LLVM libraries.
target_link_libraries(sila libsila ${llvm_libs})

if (SILA_BUILD_TESTS)
  add_subdirectory(unittests)
endif()

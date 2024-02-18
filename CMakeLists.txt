cmake_minimum_required(VERSION 3.28)

project(sila VERSION 0.1)

add_executable(${PROJECT_NAME})
set(CMAKE_STATIC_LIBRARY_PREFIX "")
add_library(lib${PROJECT_NAME})

set(SILA_TARGETS ${PROJECT_NAME} lib${PROJECT_NAME})

foreach(target ${SILA_TARGETS})
  target_compile_features(${target} PRIVATE cxx_std_20)

  target_compile_options(${target} PRIVATE
    -pedantic
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

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

if (SILA_BUILD_TESTING)
  add_subdirectory(test)
endif()

add_subdirectory(source)

# Find the libraries that correspond to the LLVM components that we wish to
# use
llvm_map_components_to_libnames(SILA_LLVM_LIBS core)

# Link against LLVM libraries
target_link_libraries(lib${PROJECT_NAME} ${SILA_LLVM_LIBS})

target_link_libraries(${PROJECT_NAME} lib${PROJECT_NAME})

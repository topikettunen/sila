include(FetchContent)
FetchContent_Declare(
  googletest
  GIT_REPOSITORY git@github.com:google/googletest.git
  GIT_TAG v1.17.0
  GIT_SHALLOW 1
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(
  testsila
  LexerTests.cpp
)

target_compile_features(testsila PRIVATE cxx_std_20)

target_compile_options(testsila PRIVATE
  -Wall
  -Wextra
  -Wconversion
  -Wdouble-promotion
  -Wno-unused-parameter
  -Wno-unused-function
  -Wno-sign-conversion

  # TODO: For some reason, StringRef keeps throwing these. Fix if possible.
  -Wno-shorten-64-to-32

  -fsanitize=undefined
  -fsanitize-trap
)

target_include_directories(testsila PUBLIC
  $<INSTALL_INTERFACE:include>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

target_link_libraries(testsila PRIVATE
  libsila
  GTest::gtest_main
  ${llvm_libs}
)

include(GoogleTest)

gtest_discover_tests(testsila)

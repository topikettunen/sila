target_sources(${PROJECT_NAME} PRIVATE main.cpp)

target_sources(lib${PROJECT_NAME} PRIVATE
  Driver/Driver.cpp
  Parse/Lexer.cpp
)

target_sources(sila PRIVATE main.cc)

set(sources
  lex.cc
  token.cc
)

target_sources(libsila PRIVATE ${sources})

#pragma once

#define SILA_NONCOPYABLE(Class)                                                \
  Class(Class const &) = delete;                                               \
  Class &operator=(Class const &) = delete;

#define SILA_NONMOVABLE(Class)                                                 \
  Class(Class &&) noexcept = delete;                                           \
  Class &operator=(Class &&) noexcept = delete;

#define SILA_NONCOPYABLE_NONMOVABLE(Class)                                     \
  SILA_NONCOPYABLE(Class)                                                      \
  SILA_NONMOVABLE(Class)

#define SILA_DEFAULT_COPYABLE(Class)                                           \
  Class(Class const &) = default;                                              \
  Class &operator=(Class const &) = default;

#define SILA_DEFAULT_MOVABLE(Class)                                            \
  Class(Class &&) noexcept = default;                                          \
  Class &operator=(Class &&) noexcept = default;

#define SILA_DEFAULT_COPYABLE_MOVABLE(Class)                                   \
  SILA_DEFAULT_COPYABLE(Class)                                                 \
  SILA_DEFAULT_MOVABLE(Class)

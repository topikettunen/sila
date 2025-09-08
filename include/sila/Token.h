#pragma once

#include <unordered_map>
#include <string_view>

#include "sila/Common.h"

namespace sila {

enum class TokenKind : u8 {
#define TOKEN(X) X,
#include "sila/TokenKinds.def"
};

std::string_view getTokenText(TokenKind kind);

struct Token {
  TokenKind Kind;
  char const *Location;
  u32 Length;
  std::string_view Text;
};

Token newToken(TokenKind kind, char const *loc, u32 len, std::string_view text);

struct PunctInfo {
  const char* Text;
  TokenKind Kind;
};

void sortPuncts();
std::vector<PunctInfo> getPuncts();

struct KeywordInfo {
  const char* Text;
  TokenKind Kind;
};

std::unordered_map<std::string_view, TokenKind> getKeywords();

} // namespace sila

#pragma once

#include <unordered_map>
#include <string_view>

#include <llvm/ADT/StringRef.h>

#include "sila/Common.h"

namespace sila {

enum class TokenKind : u8 {
#define TOKEN(X) X,
#include "sila/TokenKinds.def"
};

llvm::StringRef getTokenText(TokenKind kind);

struct Token {
  TokenKind Kind;
  char const *Location;
  i32 Length;
  llvm::StringRef Text;
};

Token newToken(TokenKind kind, char const *loc, i32 len, llvm::StringRef text);

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

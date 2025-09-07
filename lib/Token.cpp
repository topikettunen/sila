#include <set>

#include "sila/Token.h"

namespace sila {

namespace {

std::string_view tokenNames[] = {
#define TOKEN(X) #X,
#include "sila/TokenKinds.def"
};

std::vector<PunctInfo> puncts = {
#define PUNCTUATOR(X,Y) {Y, TokenKind::X},
#include "sila/TokenKinds.def"
};

std::set<llvm::StringRef> const keywords = {
#define KEYWORD(X) #X,
#include "sila/TokenKinds.def"
};

std::unordered_map<std::string_view, TokenKind> keywordsMap = {
#define KEYWORD(X) {#X, TokenKind::KW_##X},
#include "sila/TokenKinds.def"
};

} // namespace

llvm::StringRef getTokenText(TokenKind k) { return tokenNames[(u8)k]; }

Token newToken(TokenKind kind, char const *loc, i32 len, llvm::StringRef text) {
  return Token{
    .Kind = kind,
    .Location = loc,
    .Length = len,
    .Text = text
  };
}

// Needs to be called once in the startup. Sorts punctuators by descending
// length to ensure longest tokens win.
void sortPuncts() {
  auto sortByDescendingLen = [](auto &a, auto &b) {
    return std::strlen(a.Text) > std::strlen(b.Text);
  };
  std::sort(puncts.begin(), puncts.end(), sortByDescendingLen);
}

std::vector<PunctInfo> getPuncts() { return puncts; }

std::unordered_map<std::string_view, TokenKind> getKeywords() { return keywordsMap; }

} // namespace sila

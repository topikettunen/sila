#include "sila/Lexer.h"
#include <gtest/gtest.h>

namespace {

struct ExpectedToken {
  std::string text;
  sila::TokenKind kind;
};

struct LexerTestCase {
  std::string input;
  std::vector<ExpectedToken> expected;
};

void runTests(std::vector<LexerTestCase> tests) {
  for (const auto &t : tests) {
    sila::Lexer lex;
    lex.lex(t.input);

    ASSERT_EQ(lex.Tokens.size(), t.expected.size()) << "Input: " << t.input;

    for (int i = 0; i < std::ssize(lex.Tokens); ++i) {
      EXPECT_EQ(lex.Tokens[i].Text, t.expected[i].text)
          << "Mismatch at token " << i << " for input: " << t.input;
      EXPECT_EQ(lex.Tokens[i].Kind, t.expected[i].kind)
          << "Kind mismatch at token " << i << " for input: " << t.input;
    }
  }
}

} // namespace

TEST(LexerTests, ArithLiterals) {
  std::vector<LexerTestCase> tests = {
    // Integer literals
    {
      .input = "    42   ",
      .expected = {{"42", sila::TokenKind::integer_literal}},
    },
    {
      .input = "123;",
      .expected = {
        {"123", sila::TokenKind::integer_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "1 +    2;",
      .expected = {
        {"1", sila::TokenKind::integer_literal},
        {"+", sila::TokenKind::plus},
        {"2", sila::TokenKind::integer_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "1 - 2;",
      .expected = {
        {"1", sila::TokenKind::integer_literal},
        {"-", sila::TokenKind::minus},
        {"2", sila::TokenKind::integer_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
    // Floating literals
    {
      .input = "3.14;",
      .expected = {
        {"3.14", sila::TokenKind::floating_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "0.5 + 1.25;",
      .expected = {
        {"0.5", sila::TokenKind::floating_literal},
        {"+", sila::TokenKind::plus},
        {"1.25", sila::TokenKind::floating_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
    // Exponential literals
    {
      .input = "1e10;",
      .expected = {
        {"1e10", sila::TokenKind::floating_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "2E-3;",
      .expected = {
        {"2E-3", sila::TokenKind::floating_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "6.02e23;",
      .expected = {
        {"6.02e23", sila::TokenKind::floating_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
  };

  runTests(tests);
}

TEST(LexerTests, Punctuators) {
  std::vector<LexerTestCase> tests = {
    // Equality vs assignment
    {
      .input = "a == b;",
      .expected = {
        {"a", sila::TokenKind::identifier},
        {"==", sila::TokenKind::equal_equal},
        {"b", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "x = y;",
      .expected = {
        {"x", sila::TokenKind::identifier},
        {"=", sila::TokenKind::equal},
        {"y", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },

    // Not equal vs not
    {
      .input = "!flag;",
      .expected = {
        {"!", sila::TokenKind::exclaim},
        {"flag", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "a != b;",
      .expected = {
        {"a", sila::TokenKind::identifier},
        {"!=", sila::TokenKind::not_equal},
        {"b", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },

    // Less / less-equal / shifts
    {
      .input = "a < b;",
      .expected = {
        {"a", sila::TokenKind::identifier},
        {"<", sila::TokenKind::less},
        {"b", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "a <= b;",
      .expected = {
        {"a", sila::TokenKind::identifier},
        {"<=", sila::TokenKind::less_equal},
        {"b", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "x << y;",
      .expected = {
        {"x", sila::TokenKind::identifier},
        {"<<", sila::TokenKind::less_less},
        {"y", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "x <<= y;",
      .expected = {
        {"x", sila::TokenKind::identifier},
        {"<<=", sila::TokenKind::less_less_equal},
        {"y", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },

    // Greater / greater-equal / shifts
    {
      .input = "a > b;",
      .expected = {
        {"a", sila::TokenKind::identifier},
        {">", sila::TokenKind::greater},
        {"b", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "a >= b;",
      .expected = {
        {"a", sila::TokenKind::identifier},
        {">=", sila::TokenKind::greater_equal},
        {"b", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "x >> y;",
      .expected = {
        {"x", sila::TokenKind::identifier},
        {">>", sila::TokenKind::greater_greater},
        {"y", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "x >>= y;",
      .expected = {
        {"x", sila::TokenKind::identifier},
        {">>=", sila::TokenKind::greater_greater_equal},
        {"y", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },

    // Plus variants
    {
      .input = "i++;",
      .expected = {
        {"i", sila::TokenKind::identifier},
        {"++", sila::TokenKind::plus_plus},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "i += 1;",
      .expected = {
        {"i", sila::TokenKind::identifier},
        {"+=", sila::TokenKind::plus_equal},
        {"1", sila::TokenKind::integer_literal},
        {";", sila::TokenKind::semicolon},
      },
    },

    // Minus variants
    {
      .input = "j--;",
      .expected = {
        {"j", sila::TokenKind::identifier},
        {"--", sila::TokenKind::minus_minus},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "j -= 2;",
      .expected = {
        {"j", sila::TokenKind::identifier},
        {"-=", sila::TokenKind::minus_equal},
        {"2", sila::TokenKind::integer_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "ptr->field;",
      .expected = {
        {"ptr", sila::TokenKind::identifier},
        {"->", sila::TokenKind::arrow},
        {"field", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },

    // Logical and/or
    {
      .input = "a && b;",
      .expected = {
        {"a", sila::TokenKind::identifier},
        {"&&", sila::TokenKind::amp_amp},
        {"b", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "mask &= val;",
      .expected = {
        {"mask", sila::TokenKind::identifier},
        {"&=", sila::TokenKind::amp_equal},
        {"val", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "x & y;",
      .expected = {
        {"x", sila::TokenKind::identifier},
        {"&", sila::TokenKind::amp},
        {"y", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "a || b;",
      .expected = {
        {"a", sila::TokenKind::identifier},
        {"||", sila::TokenKind::pipe_pipe},
        {"b", sila::TokenKind::identifier},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "flags |= 1;",
      .expected = {
        {"flags", sila::TokenKind::identifier},
        {"|=", sila::TokenKind::pipe_equal},
        {"1", sila::TokenKind::integer_literal},
        {";", sila::TokenKind::semicolon},
      },
    },
    {
      .input = "a ## b",
      .expected = {
        {"a", sila::TokenKind::identifier},
        {"##", sila::TokenKind::hash_hash},
        {"b", sila::TokenKind::identifier},
      },
    },
  };

  runTests(tests);
}

TEST(LexerTests, IdentifiersAndKeywords) {
  std::vector<LexerTestCase> tests = {
    // Identifiers
    {
      .input = "foo",
      .expected = {{"foo", sila::TokenKind::identifier}},
    },
    {
      .input = "_bar123",
      .expected = {{"_bar123", sila::TokenKind::identifier}},
    },
    // Keywords
    {
      .input = "if",
      .expected = {{"if", sila::TokenKind::KW_if}},
    },
    {
      .input = "return var",
      .expected = {{"return", sila::TokenKind::KW_return}, {"var", sila::TokenKind::KW_var}},
    },
    {
      .input = "true false",
      .expected = {{"true", sila::TokenKind::KW_true}, {"false", sila::TokenKind::KW_false}},
    },
    // Not a keyword
    {
      .input = "iff",
      .expected = {{"iff", sila::TokenKind::identifier}},
    },
  };

  runTests(tests);
}

TEST(LexerTests, StringLiterals) {
  std::vector<LexerTestCase> tests = {
    {
      .input = "\"hello\"",
      .expected = {{"\"hello\"", sila::TokenKind::string_literal}},
    },
    {
      .input = "\"a \\\"quoted\\\" word\"",
      .expected = {{"\"a \\\"quoted\\\" word\"", sila::TokenKind::string_literal}},
    },
    {
      .input = "\"line\\nfeed\"",
      .expected = {{"\"line\\nfeed\"", sila::TokenKind::string_literal}},
    },
  };
  runTests(tests);
}

TEST(LexerTests, CharLiterals) {
  std::vector<LexerTestCase> tests = {
    {
      .input = "'a'",
      .expected = {{"'a'", sila::TokenKind::char_literal}},
    },
    {
      .input = "'\\n'",
      .expected = {{"'\\n'", sila::TokenKind::char_literal}},
    },
    {
      .input = "'\\''",
      .expected = {{"'\\''", sila::TokenKind::char_literal}},
    },
  };

  runTests(tests);
}

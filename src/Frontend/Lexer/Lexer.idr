module Frontend.Lexer.Lexer

import Derive.Prelude
import Language.Reflection

import Frontend.Token
import Frontend.Source
import Frontend.Lexer.Error

-- idris2-parser manual tooling for span tracking:
-- Position, begin, next, Bounded, bounded, ...
import Text.Parse.Manual

%default total
%language ElabReflection

public export
LocatedToken : Type
LocatedToken = Located Token

public export
LocatedLexerErr : Type
LocatedLexerErr = Located LexerErr

--------------------------------------------------------------------------------
-- Main entry point: lexProgram
--------------------------------------------------------------------------------
public export
lexProgram : String -> Either LocatedLexerErr (List LocatedToken)


- Every successful token consumes at least one character, and every error-recovery step also consumes at least one character. That keeps totality and avoids accidental infinite loops.

- symbolTable in Token.idr is ordered from longest to shortest, the lexer must use longest match first to disambiguate.

- Preserve raw numeric literals in the lexer. Do not parse integer bounds, suffixes, signedness, or floating-point precision in the lexer. The lexer should preserve the spelling.

- Keep unary minus out of numeric literals. -1 should be lexed as two tokens: TokSym SymMinus and TokIntLitRaw "1". Later the parser can then decide how to interpret the minus sign based on context.

- Identifiers should be lexed as:

  identifier_start = letter or _
  identifier_rest  = letter or digit or _

  Corner case: `_` should be lexed as: TokUnderscore` and not as an identifier with the name `_`. 

  On the other hand: `_x` should be lexed as an identifier TokIdent with the name `_x`.

  Leaf allows apostrophes in identifiers: `foo'`

- Do not make the lexer context-sensitive, If `basis` is a keyword, it is always a keyword. If it is an identifier, it is always an identifier.

- Keep parser concerns out of the lexer. The lexer should not decide that this is a function call: H(&q)

- Source code is orgnized like this:

  Frontend/
    Token.idr
    Source.idr
    Lexer/
      Error.idr
      Helpers.idr
      Rules.idr
      Lexer.idr
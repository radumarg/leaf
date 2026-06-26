module Frontend.Lexer.Lexer
import Derive.Prelude
import Language.Reflection

import Frontend.Token
import Frontend.Lexer.Error

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Main entry point: lexProgram
--------------------------------------------------------------------------------
public export
lexProgram : String -> Either (Bounded LexerErr) (List (Bounded Token))
lexProgram inputString = TODO

- I want manual lexer based primarily on Stefan Höck's idris2-parser Text.Lex.Manual with this signature:
 lexProgram : String -> Either (Bounded LexerErr) (List (Bounded Token))

- I plan to use use idris2-parser Bounded for tokens and my own Located for AST nodes. So for the current lexer, I will return Bounded Token.

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

- Validate bitstring literals in the lexer. bs"++" is special enough to deserve a dedicated scanner.

- Make lexical errors structured. Do not return plain strings

- Keep comments and whitespace out of the token stream. But always update source positions.

- Avoid context-sensitive lexing. Let the parser and later frontend phases handle meaning.

- Source code is orgnized like this:

  Frontend/
    Token.idr
    Source.idr
    Lexer/
      Error.idr
      Helpers.idr
      Lexer.idr
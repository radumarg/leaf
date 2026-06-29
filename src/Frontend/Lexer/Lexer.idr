module Frontend.Lexer.Lexer

import Data.Bits
import Derive.Prelude
import Language.Reflection
import Text.Bounds
import Text.ILex
import Text.ParseError

import Frontend.Token
import Frontend.Lexer.Errors
import Frontend.Lexer.Rules

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Translating ilex native errors into Leaf's public LexerError.
--
-- Latest idris2-ilex exposes `BoundedErr e`, i.e. a `Bounded (InnerError e)`.
-- The prompt asks `lexProgram` to expose only `Bounded LexerError`, so this
-- module is the single place where native ilex failures are translated.
--------------------------------------------------------------------------------
public export
translateInnerLexerError : InnerError LexerError -> LexerError
translateInnerLexerError (Custom lexerError) =
  lexerError

translateInnerLexerError EOI =
  LexUnexpectedEndOfInput

translateInnerLexerError (Expected expectedTokens actualText) =
  LexUnexpectedInput expectedTokens actualText

translateInnerLexerError (ExpectedChar characterClass) =
  LexUnexpectedInput [interpolate characterClass] ""

translateInnerLexerError ExpectedEOI =
  LexUnexpectedInput ["end of input"] "more input"

translateInnerLexerError (InvalidControl characterValue) =
  LexUnexpectedInput [] (show characterValue)

translateInnerLexerError InvalidEscape =
  LexInternalLexerError "Invalid escape sequence reported by ilex"

translateInnerLexerError (OutOfBounds rawText) =
  LexInternalLexerError ("Out-of-bounds value reported by ilex: " ++ rawText)

translateInnerLexerError (Unclosed delimiterText) =
  case delimiterText == "block comment" of
    True => LexUnterminatedBlockComment
    False => LexUnclosedDelimiter delimiterText

translateInnerLexerError (Unknown actualText) =
  LexUnexpectedInput [] actualText

translateInnerLexerError (InvalidByte byteValue) =
  LexInvalidUtf8Byte byteValue

--------------------------------------------------------------------------------
-- Main entry point: lexProgram
--
-- The lexer itself already emits `Bounded Token` and `BoundedErr LexerError`
-- using the current source-positioned ilex API.  No legacy byte-bound conversion layer is needed here.
--------------------------------------------------------------------------------
public export
lexProgram : String -> Either (Bounded LexerError) (List (Bounded Token))
lexProgram inputString =
  case runString leafLexer inputString of
    Left ilexError =>
      Left (map translateInnerLexerError ilexError)

    Right boundedTokens =>
      Right boundedTokens
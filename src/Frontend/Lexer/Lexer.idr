module Frontend.Lexer.Lexer

import Text.Bounds
import Text.ILex
import Text.ParseError

import Frontend.Token
import Frontend.Lexer.Errors
import Frontend.Lexer.Rules

%default total

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
-- The installed ilex tracks positions as raw byte offsets while lexing and only
-- exposes `Bounded` (line/column) values via a position map built from the whole
-- input once lexing has finished. This module is therefore also the single place
-- where ilex's byte-offset results are converted to line/column positions and
-- native ilex failures are translated into Leaf's public `LexerError`.
--------------------------------------------------------------------------------
public export
lexProgram : String -> Either (Bounded LexerError) (List (Bounded Token))
lexProgram inputString =
  let pm := stringPositionMap inputString in
  case runString leafLexer inputString of
    Left byteBoundedError =>
      Left (toBounded (map translateInnerLexerError byteBoundedError))

    Right byteBoundedTokens =>
      Right (map toBounded byteBoundedTokens)
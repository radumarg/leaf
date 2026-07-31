module Frontend.Lexer.Error

import Data.Bits
import Derive.Prelude
import Language.Reflection
import Text.Bounds
import Text.ParseError

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- LexerError
--
-- This is the public, user-facing Leaf lexer error type.  The ilex runtime
-- still uses `InnerError LexerError` internally, because native ilex failures
-- such as unexpected input and unexpected or invalid bytes are represented by
-- `InnerError`.  `Frontend.Lexer.Lexer` translates those native failures into
-- the constructors below so `lexFile` exposes exactly `Bounded LexerError`.
--------------------------------------------------------------------------------
public export
data LexerError
  = LexUnexpectedEndOfInput
  | LexUnexpectedInput (List String) String
  | LexUnclosed String
  | LexUnexpectedOrInvalidByte Bits8
  | LexUnterminatedBlockComment
  | LexInvalidBasisStringLiteral String
  | LexInvalidByteLiteral String
  | LexInvalidByteStringLiteral String
  | LexInvalidStringLiteral String
  | LexInvalidNumberLiteral String
  | LexUnterminatedStringLiteral
  | LexUnterminatedBasisStringLiteral
  | LexUnterminatedByteStringLiteral
  | LexUnterminatedByteLiteral
  | LexOrdinaryCharLiteralNeedsToken
  | LexInternalLexerError String

%runElab derive "LexerError" [Show, Eq]

--------------------------------------------------------------------------------
-- Human-readable error messages.
--------------------------------------------------------------------------------
export
Interpolation LexerError where
  interpolate LexUnexpectedEndOfInput =
    "Unexpected end of input"

  interpolate (LexUnexpectedInput expected actual) =
    case expected of
      [] =>
        "Unexpected input " ++ actual

      firstExpected :: remainingExpected =>
        "Expected " ++ show (firstExpected :: remainingExpected) ++
        ", but got " ++ actual

  interpolate (LexUnclosed description) =
    "Unclosed " ++ description

  interpolate (LexUnexpectedOrInvalidByte byteValue) =
    "Unexpected or invalid byte in Leaf source: 0x" ++ toHex byteValue

  interpolate LexUnterminatedBlockComment =
    "Unterminated block comment"

  interpolate (LexInvalidBasisStringLiteral rawText) =
    "Invalid Leaf basis-string literal " ++ rawText ++
    ". Expected bs\"...\" with only 0, 1, +, -, i, and I."

  interpolate (LexInvalidByteLiteral rawText) =
    "Invalid Leaf byte literal " ++ rawText ++
    ". Expected one printable ASCII byte, a simple byte escape, or a hexadecimal byte escape of the form b'\\xNN'."

  interpolate (LexInvalidByteStringLiteral rawText) =
    "Invalid Leaf byte-string literal " ++ rawText ++
    ". Expected printable ASCII bytes or supported byte escapes."

  interpolate (LexInvalidStringLiteral rawText) =
    "Invalid Leaf string literal " ++ rawText ++
    ". Normal Leaf strings currently allow only ASCII letters, digits, and underscores, with no escapes."

  interpolate (LexInvalidNumberLiteral rawText) =
    "Invalid Leaf number literal " ++ rawText ++
    ". Expected a Rust-style integer or floating-point literal supported by Leaf."

  interpolate LexUnterminatedStringLiteral =
    "Unterminated string literal"

  interpolate LexUnterminatedBasisStringLiteral =
    "Unterminated basis-string literal"

  interpolate LexUnterminatedByteStringLiteral =
    "Unterminated byte-string literal"

  interpolate LexUnterminatedByteLiteral =
    "Unterminated byte literal"

  interpolate LexOrdinaryCharLiteralNeedsToken =
    "Ordinary character literals are not currently part of Leaf's token set. Use a byte literal like b'a' or add a dedicated character token."

  interpolate (LexInternalLexerError message) =
    "Internal Leaf lexer error: " ++ message

||| Render a lexer error together with its source bounds.
public export
renderLexerError : Bounded LexerError -> String
renderLexerError (B lexerError NoBounds) =
  "Lexer error: " ++ interpolate lexerError
renderLexerError (B lexerError bounds) =
  "Lexer error at " ++ interpolate bounds ++ ": " ++ interpolate lexerError

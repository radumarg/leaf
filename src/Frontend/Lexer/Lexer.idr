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

-- Leaf never calls ilex's own `unclosed`/`unclosedIfEOI`/`unclosedIfNLorEOI`
-- helpers (block comments are tracked entirely through `Rules.idr`'s own
-- depth-counting state machine and EOI handler instead), so this native
-- `Unclosed` error is never actually produced by `leafLexer`. It still has to
-- be translated to satisfy totality over `InnerError`'s constructors; this is
-- a generic, faithful mapping rather than a guess at Leaf-specific meaning.
translateInnerLexerError (Unclosed delimiterText) =
  LexUnclosedDelimiter delimiterText

translateInnerLexerError (Unknown actualText) =
  LexUnexpectedInput [] actualText

translateInnerLexerError (InvalidByte byteValue) =
  LexInvalidUtf8Byte byteValue

--------------------------------------------------------------------------------
-- Clamping byte positions to the input.
--
-- At true end-of-input, ilex's runner can report a `ByteBounds` end position
-- past the end of the byte stream (observed for unterminated block comments,
-- the one Leaf construct whose error is only ever detected via the EOI
-- handler rather than via a greedy "unterminated ..." regex match). Such an
-- out-of-range position makes `toBounds` fail its position-map lookup and
-- silently collapse the whole span to `NoBounds`, discarding an otherwise
-- correct start position along with it.
--
-- A position past the end of the input is never meaningful, so clamping
-- every reported position to the input's length is always safe, and for an
-- "unterminated" error it is exactly the correct end position by
-- construction (the span runs to the end of the input).
--------------------------------------------------------------------------------
clampBytePos : Nat -> BytePos -> BytePos
clampBytePos inputByteLength (BP pos) =
  BP (min pos inputByteLength)

clampByteBounds : Nat -> ByteBounds -> ByteBounds
clampByteBounds inputByteLength NoBB =
  NoBB
clampByteBounds inputByteLength (BB start end) =
  BB (clampBytePos inputByteLength start) (clampBytePos inputByteLength end)

clampByteBounded : Nat -> ByteBounded a -> ByteBounded a
clampByteBounded inputByteLength (B val bounds) =
  B val (clampByteBounds inputByteLength bounds)

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
  let pm := stringPositionMap inputString
      inputByteLength := pred pm.size
  in case runString leafLexer inputString of
    Left byteBoundedError =>
      Left
        (toBounded
          (clampByteBounded inputByteLength (map translateInnerLexerError byteBoundedError)))

    Right byteBoundedTokens =>
      Right (map (toBounded . clampByteBounded inputByteLength) byteBoundedTokens)

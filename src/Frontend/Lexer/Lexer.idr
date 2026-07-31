module Frontend.Lexer.Lexer

import Text.Bounds
import Text.ILex
import Text.ParseError

import Frontend.Token
import Frontend.Lexer.Error
import Frontend.Lexer.Rules

%default total

--------------------------------------------------------------------------------
-- Translating ilex native errors into Leaf's public LexerError.
--
-- `runString leafLexer` returns an `Either (BBErr LexerError) ...`, where an
-- `BBErr e` is a byte-bounded `InnerError e`. This module is the single place
-- where those native ilex failures are translated so `lexFile` exposes only
-- `Bounded LexerError`.
--------------------------------------------------------------------------------
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

translateInnerLexerError (Unclosed description) =
  LexUnclosed description

translateInnerLexerError (Unknown actualText) =
  LexUnexpectedInput [] actualText

translateInnerLexerError (InvalidByte byteValue) =
  LexUnexpectedOrInvalidByte byteValue

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
-- Position-map input
--
-- Leaf treats a bare carriage return as a line break, but ilex's position map
-- advances lines only for line-feed bytes. Replace only bare carriage returns
-- in the text used to build the map. Both characters occupy one UTF-8 byte, so
-- all byte offsets still refer to the original input. CRLF pairs stay intact,
-- avoiding an extra line advance. The original text is always passed unchanged
-- to `runString`, preserving token contents.
--------------------------------------------------------------------------------
positionMapInput : String -> String
positionMapInput =
  pack . replaceBareCarriageReturns . unpack
  where
    replaceBareCarriageReturns : List Char -> List Char
    replaceBareCarriageReturns [] =
      []
    replaceBareCarriageReturns ('\r' :: '\n' :: remaining) =
      '\r' :: '\n' :: replaceBareCarriageReturns remaining
    replaceBareCarriageReturns ('\r' :: remaining) =
      '\n' :: replaceBareCarriageReturns remaining
    replaceBareCarriageReturns (character :: remaining) =
      character :: replaceBareCarriageReturns remaining

--------------------------------------------------------------------------------
-- Main entry point: lexFile
--
-- The installed ilex tracks positions as raw byte offsets while lexing and only
-- exposes `Bounded` (line/column) values via a position map built from the whole
-- input once lexing has finished. This module is therefore also the single place
-- where ilex's byte-offset results are converted to line/column positions and
-- native ilex failures are translated into Leaf's public `LexerError`.
--------------------------------------------------------------------------------
public export
lexFile : String -> Either (Bounded LexerError) (List (Bounded Token))
lexFile inputString =
  let pm := stringPositionMap (positionMapInput inputString)
      inputByteLength := pred pm.size
  in case runString leafLexer inputString of
    Left byteBoundedError =>
      Left
        (toBounded
          (clampByteBounded inputByteLength (map translateInnerLexerError byteBoundedError)))

    Right byteBoundedTokens =>
      Right (map (toBounded . clampByteBounded inputByteLength) byteBoundedTokens)

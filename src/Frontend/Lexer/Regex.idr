module Frontend.Lexer.Regex

import Text.ILex

%default total
%hide Prelude.(>>)
%hide Prelude.not

--------------------------------------------------------------------------------
-- Whitespace and line structure.
--
-- The prompt explicitly asks us to skip CRLF, LF, CR, tab, and spaces.
--------------------------------------------------------------------------------
lineBreak : RExp True
lineBreak =
      "\r\n"
  <|> '\n'
  <|> '\r'

horizontalWhitespace : RExp True
horizontalWhitespace =
  oneof [' ', '\t']

export
leafWhitespace : RExp True
leafWhitespace =
  plus (horizontalWhitespace <|> lineBreak)

notLineBreakChar : RExp True
notLineBreakChar =
  dot && not '\n' && not '\r'

--------------------------------------------------------------------------------
-- ASCII helpers used by string and number candidates.
--------------------------------------------------------------------------------
asciiLower : RExp True
asciiLower = range 'a' 'z'

asciiUpper : RExp True
asciiUpper = range 'A' 'Z'

asciiLetter : RExp True
asciiLetter = asciiLower <|> asciiUpper

asciiAlphaNum : RExp True
asciiAlphaNum = asciiLetter <|> digit

asciiAlphaNumUnderscore : RExp True
asciiAlphaNumUnderscore = asciiAlphaNum <|> '_'

--------------------------------------------------------------------------------
-- Identifier-like text.
--
-- There is intentionally one identifier rule.  `tokenFromIdentLike` in
-- Frontend.Token owns the classification into TokUnderscore, booleans,
-- keywords, primitive types, state literals, builtins, and ordinary identifiers.
--
-- Leaf follows the prompt's rule:
--   start = Unicode alphabetic or '_'
--   rest  = Unicode alphabetic, Unicode digit, '_', or apostrophe
--
-- Therefore `foo'` is a single identifier and `fn'` is not split into `fn` plus
-- an apostrophe.
--------------------------------------------------------------------------------
identifierStart : RExp True
identifierStart = alpha <|> '_'

identifierRest : RExp True
identifierRest = alphaNum <|> '_' <|> '\''

export
identifierLike : RExp True
identifierLike = identifierStart >> star identifierRest

--------------------------------------------------------------------------------
-- Numeric literal components.
--
-- Unary minus is deliberately not included here.  `-7` is lexed as the symbol
-- `-` followed by `TokIntLitRaw "7"`.
--------------------------------------------------------------------------------
decimalDigits : RExp True
decimalDigits = digit >> star (digit <|> '_')

binaryDigits : RExp True
binaryDigits = bindigit >> star (bindigit <|> '_')

octalDigits : RExp True
octalDigits = octdigit >> star (octdigit <|> '_')

hexDigits : RExp True
hexDigits = hexdigit >> star (hexdigit <|> '_')

--------------------------------------------------------------------------------
-- Strict digit runs, used only for validating an already-matched literal (see
-- `Rules.idr`'s `numberClassifier`), never for the broad candidates above.
--
-- Interior `_` is freely placed (including consecutive underscores), but a
-- decimal digit run may not start or end with `_`, while a radix digit run
-- (used after `0b`/`0o`/`0x`) may start with `_` (Rust-style `0x_FF`) but still
-- may not end with one. The permissive `decimalDigits`/`binaryDigits`/etc.
-- above must stay permissive so malformed spellings are swallowed into one
-- token instead of being split, so these are separate definitions rather than
-- a tightened version of them.
--------------------------------------------------------------------------------
strictDigitRun : RExp True -> RExp True
strictDigitRun singleDigit =
  singleDigit >> opt (star (singleDigit <|> '_') >> singleDigit)

strictRadixDigitRun : RExp True -> RExp True
strictRadixDigitRun singleDigit =
  star (singleDigit <|> '_') >> singleDigit

strictDecimalDigits : RExp True
strictDecimalDigits = strictDigitRun digit

strictBinaryDigits : RExp True
strictBinaryDigits = strictRadixDigitRun bindigit

strictOctalDigits : RExp True
strictOctalDigits = strictRadixDigitRun octdigit

strictHexDigits : RExp True
strictHexDigits = strictRadixDigitRun hexdigit

signedIntegerTypeSuffix : RExp True
signedIntegerTypeSuffix =
      "i8"
  <|> "i16"
  <|> "i32"
  <|> "i64"
  <|> "i128"

unsignedIntegerTypeSuffix : RExp True
unsignedIntegerTypeSuffix =
      "u8"
  <|> "u16"
  <|> "u32"
  <|> "u64"
  <|> "u128"

integerTypeSuffix : RExp True
integerTypeSuffix = signedIntegerTypeSuffix <|> unsignedIntegerTypeSuffix

integerSuffix : RExp True
integerSuffix = opt '_' >> integerTypeSuffix

floatTypeSuffix : RExp True
floatTypeSuffix = "f32" <|> "f64"

floatSuffix : RExp True
floatSuffix = opt '_' >> floatTypeSuffix

exponentPart : RExp True
exponentPart =
  oneof ['e', 'E'] >> opt (oneof ['+', '-']) >> strictDecimalDigits

--------------------------------------------------------------------------------
-- Valid numeric regexes.
--
-- These are the actual source of truth for what counts as a well-formed
-- integer/float literal: `Rules.idr`'s `numberClassifier` runs the already
-- broadly-matched raw text back through these via `Text.ILex.Stack.value`,
-- rather than re-validating it with hand-written `List Char` recursion.
--------------------------------------------------------------------------------
binaryIntegerLiteral : RExp True
binaryIntegerLiteral =
  ("0b" <|> "0B") >> strictBinaryDigits >> opt integerSuffix

octalIntegerLiteral : RExp True
octalIntegerLiteral =
  ("0o" <|> "0O") >> strictOctalDigits >> opt integerSuffix

hexIntegerLiteral : RExp True
hexIntegerLiteral =
  ("0x" <|> "0X") >> strictHexDigits >> opt integerSuffix

decimalIntegerLiteral : RExp True
decimalIntegerLiteral =
  strictDecimalDigits >> opt integerSuffix

export
integerLiteral : RExp True
integerLiteral =
      binaryIntegerLiteral
  <|> octalIntegerLiteral
  <|> hexIntegerLiteral
  <|> decimalIntegerLiteral

--------------------------------------------------------------------------------
-- Required float union from the prompt:
--
--   digit+ '.' digit+ exp? suf?
--   digit+ exp suf?
--   digit+ ('f32' | 'f64')
--
-- The rule does not admit trailing-dot floats.  This also preserves the required
-- tokenization of `1..2` as integer, range operator, integer.
--------------------------------------------------------------------------------
export
floatLiteral : RExp True
floatLiteral =
      (strictDecimalDigits >> '.' >> strictDecimalDigits >> opt exponentPart >> opt floatSuffix)
  <|> (strictDecimalDigits >> exponentPart >> opt floatSuffix)
  <|> (strictDecimalDigits >> floatSuffix)

--------------------------------------------------------------------------------
-- Broad numeric candidates.
--
-- The lexer must not split malformed numeric spellings into smaller legal
-- tokens.  For example, `0b102` must become a number-literal error rather than
-- `0b10` followed by `2`.  These candidates therefore consume complete
-- number-looking spans, and Rules.idr validates/classifies the raw spelling.
--
-- We intentionally do not consume a bare trailing dot.  That keeps `1..2`
-- correct and leaves `1.` as `1` followed by `.`, which the parser can reject if
-- it appears in a context where a member-access dot is not meaningful.
--------------------------------------------------------------------------------
radixNumberCandidate : RExp True
radixNumberCandidate =
      (("0b" <|> "0B") >> star (asciiAlphaNum <|> '_'))
  <|> (("0o" <|> "0O") >> star (asciiAlphaNum <|> '_'))
  <|> (("0x" <|> "0X") >> star (asciiAlphaNum <|> '_'))

looseExponentPart : RExp True
looseExponentPart =
  oneof ['e', 'E'] >> opt (oneof ['+', '-']) >> star (asciiAlphaNum <|> '_')

dottedDecimalNumberCandidate : RExp True
dottedDecimalNumberCandidate =
  decimalDigits >> '.' >> decimalDigits >> opt looseExponentPart >> star (asciiAlphaNum <|> '_')

exponentNumberCandidate : RExp True
exponentNumberCandidate =
  decimalDigits >> looseExponentPart >> star (asciiAlphaNum <|> '_')

suffixedDecimalNumberCandidate : RExp True
suffixedDecimalNumberCandidate =
  decimalDigits >> (asciiLetter <|> '_') >> star (asciiAlphaNum <|> '_')

plainDecimalNumberCandidate : RExp True
plainDecimalNumberCandidate = decimalDigits

export
numberCandidate : RExp True
numberCandidate =
      dottedDecimalNumberCandidate
  <|> radixNumberCandidate
  <|> exponentNumberCandidate
  <|> suffixedDecimalNumberCandidate
  <|> plainDecimalNumberCandidate

--------------------------------------------------------------------------------
-- Digits immediately followed by `.`, `..`, or `..=` with no further digit.
--
-- `dottedDecimalNumberCandidate` only consumes a `.` when a digit follows, by
-- design (see above), so the underlying lexer engine never has to choose
-- between treating that `.` as the start of a float or as a separate `.`/`..`/
-- `..=` symbol. This candidate covers exactly the complementary case: digits
-- followed by one or two dots (optionally `..=`) with no digit after the dot,
-- so `1..2` still lexes as `1`, `..`, `2` and `1.` still lexes as `1`, `.`.
--------------------------------------------------------------------------------
export
digitsThenDotOperatorCandidate : RExp True
digitsThenDotOperatorCandidate =
  decimalDigits >> ('.' >> opt ('.' >> opt '='))

--------------------------------------------------------------------------------
-- String, basis-string, byte literal, byte-string, and ordinary char candidates.
--
-- These candidates are deliberately broader than the set of valid literals.
-- Rules.idr validates the raw spelling and raises a structured error instead of
-- letting bad literals split into unrelated tokens.
--------------------------------------------------------------------------------
normalStringBodyCandidate : RExp True
normalStringBodyCandidate =
      ('\\' >> dot)
  <|> (dot && not '"' && not '\n' && not '\r')

export
normalStringCandidate : RExp True
normalStringCandidate =
  '"' >> star normalStringBodyCandidate >> '"'

export
unterminatedNormalStringCandidate : RExp True
unterminatedNormalStringCandidate =
  '"' >> star normalStringBodyCandidate

export
basisStringCandidate : RExp True
basisStringCandidate =
  'b' >> 's' >> '"' >> star (dot && not '"' && not '\n' && not '\r') >> '"'

export
unterminatedBasisStringCandidate : RExp True
unterminatedBasisStringCandidate =
  'b' >> 's' >> '"' >> star (dot && not '"' && not '\n' && not '\r')

-- Byte strings allow the same body characters as normal strings (anything
-- but '"', a line break, or a bare backslash); the two candidates share one
-- definition so they can't silently drift apart.
byteStringBodyCandidate : RExp True
byteStringBodyCandidate = normalStringBodyCandidate

export
byteStringCandidate : RExp True
byteStringCandidate =
  'b' >> '"' >> star byteStringBodyCandidate >> '"'

export
unterminatedByteStringCandidate : RExp True
unterminatedByteStringCandidate =
  'b' >> '"' >> star byteStringBodyCandidate

byteLiteralBodyCandidate : RExp True
byteLiteralBodyCandidate =
      ('\\' >> dot)
  <|> (dot && not '\'' && not '\n' && not '\r')

export
byteLiteralCandidate : RExp True
byteLiteralCandidate =
  'b' >> '\'' >> star byteLiteralBodyCandidate >> '\''

export
unterminatedByteLiteralCandidate : RExp True
unterminatedByteLiteralCandidate =
  'b' >> '\'' >> star byteLiteralBodyCandidate

export
ordinaryCharLiteralCandidate : RExp True
ordinaryCharLiteralCandidate =
  '\'' >> star byteLiteralBodyCandidate >> '\''

--------------------------------------------------------------------------------
-- Strict string/byte-literal regexes, used only for validating the raw text
-- already matched by the broad candidates above (see `Rules.idr`'s
-- `normalStringValidator`/`basisStringValidator`/`byteLiteralValidator`/
-- `byteStringValidator`), never for matching directly.
--------------------------------------------------------------------------------
export
normalStringLiteralStrict : RExp True
normalStringLiteralStrict =
  '"' >> star asciiAlphaNumUnderscore >> '"'

basisStringChar : RExp True
basisStringChar =
  oneof ['0', '1', '+', '-', 'i', 'I']

export
basisStringLiteralStrict : RExp True
basisStringLiteralStrict =
  'b' >> 's' >> '"' >> star basisStringChar >> '"'

simpleByteEscape : RExp True
simpleByteEscape =
  '\\' >> oneof ['n', 'r', 't', '0', '\\', '\'', '"']

hexByteEscape : RExp True
hexByteEscape =
  "\\x" >> hexdigit >> hexdigit

export
byteLiteralStrict : RExp True
byteLiteralStrict =
  'b' >> '\'' >>
    ((range32 0x20 0x7e && not '\\' && not '\'') <|> simpleByteEscape <|> hexByteEscape) >>
    '\''

export
byteStringLiteralStrict : RExp True
byteStringLiteralStrict =
  'b' >> '"' >>
    star ((range32 0x20 0x7e && not '\\' && not '"') <|> simpleByteEscape <|> hexByteEscape) >>
    '"'

--------------------------------------------------------------------------------
-- Comments and documentation comments.
--
-- Required corner cases:
--   ///!  outer line doc
--   ////  normal line comment
--   //!/  inner line doc
--
-- Maximal munch makes `////` a normal comment because the normal-comment rule
-- consumes the whole line while the outer-doc rule can only consume `///`.
--------------------------------------------------------------------------------
lineCommentTail : RExp False
lineCommentTail = star notLineBreakChar

outerDocLineBody : RExp False
outerDocLineBody =
  opt ((dot && not '/' && not '\n' && not '\r') >> lineCommentTail)

export
outerDocLineComment : RExp True
outerDocLineComment =
  '/' >> '/' >> '/' >> outerDocLineBody

export
innerDocLineComment : RExp True
innerDocLineComment =
  '/' >> '/' >> '!' >> lineCommentTail

export
normalLineComment : RExp True
normalLineComment =
  '/' >> '/' >> lineCommentTail

export
emptyOuterBlockComment : RExp True
emptyOuterBlockComment =
  '/' >> '*' >> '*' >> '/'

export
starOnlyOuterBlockComment : RExp True
starOnlyOuterBlockComment =
  '/' >> '*' >> '*' >> '*' >> '/'

--------------------------------------------------------------------------------
-- Block comments.
--
-- Outer block docs are classified by the opening delimiter.  `/**/` and
-- `/***/` are normal comments, not docs, so the outer-doc opener consumes one
-- first body character that must be neither `*` nor `/`.  Inner block docs may
-- be empty, so `/*!*/` is a valid inner doc comment.
--------------------------------------------------------------------------------
outerBlockDocFirstBodyChar : RExp True
outerBlockDocFirstBodyChar =
      (dot && not '*' && not '/' && not '\n' && not '\r')
  <|> lineBreak

export
outerBlockDocOpen : RExp True
outerBlockDocOpen =
  '/' >> '*' >> '*' >> outerBlockDocFirstBodyChar

export
innerBlockDocOpen : RExp True
innerBlockDocOpen =
  '/' >> '*' >> '!'

export
normalBlockCommentOpen : RExp True
normalBlockCommentOpen =
  '/' >> '*'

export
blockCommentClose : RExp True
blockCommentClose =
  '*' >> '/'

export
blockCommentBodyChunk : RExp True
blockCommentBodyChunk =
  plus (dot && not '*' && not '/' && not '\n' && not '\r')

export
blockCommentLineBreak : RExp True
blockCommentLineBreak = lineBreak

export
blockCommentSingleStar : RExp True
blockCommentSingleStar = '*'

export
blockCommentSingleSlash : RExp True
blockCommentSingleSlash = '/'

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
public export
lineBreak : RExp True
lineBreak =
      "\r\n"
  <|> '\n'
  <|> '\r'

public export
horizontalWhitespace : RExp True
horizontalWhitespace =
  oneof [' ', '\t']

public export
leafWhitespace : RExp True
leafWhitespace =
  plus (horizontalWhitespace <|> lineBreak)

public export
notLineBreakChar : RExp True
notLineBreakChar =
  dot && not '\n' && not '\r'

--------------------------------------------------------------------------------
-- ASCII helpers used by string and number candidates.
--------------------------------------------------------------------------------
public export
asciiLower : RExp True
asciiLower = range 'a' 'z'

public export
asciiUpper : RExp True
asciiUpper = range 'A' 'Z'

public export
asciiLetter : RExp True
asciiLetter = asciiLower <|> asciiUpper

public export
asciiAlphaNum : RExp True
asciiAlphaNum = asciiLetter <|> digit

public export
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
public export
identifierStart : RExp True
identifierStart = alpha <|> '_'

public export
identifierRest : RExp True
identifierRest = alphaNum <|> '_' <|> '\''

public export
identifierLike : RExp True
identifierLike = identifierStart >> star identifierRest

--------------------------------------------------------------------------------
-- Numeric literal components.
--
-- Unary minus is deliberately not included here.  `-7` is lexed as the symbol
-- `-` followed by `TokIntLitRaw "7"`.
--------------------------------------------------------------------------------
public export
decimalDigits : RExp True
decimalDigits = digit >> star (digit <|> '_')

public export
binaryDigits : RExp True
binaryDigits = bindigit >> star (bindigit <|> '_')

public export
octalDigits : RExp True
octalDigits = octdigit >> star (octdigit <|> '_')

public export
hexDigits : RExp True
hexDigits = hexdigit >> star (hexdigit <|> '_')

public export
signedIntegerTypeSuffix : RExp True
signedIntegerTypeSuffix =
      "i8"
  <|> "i16"
  <|> "i32"
  <|> "i64"
  <|> "i128"

public export
unsignedIntegerTypeSuffix : RExp True
unsignedIntegerTypeSuffix =
      "u8"
  <|> "u16"
  <|> "u32"
  <|> "u64"
  <|> "u128"

public export
integerTypeSuffix : RExp True
integerTypeSuffix = signedIntegerTypeSuffix <|> unsignedIntegerTypeSuffix

public export
integerSuffix : RExp True
integerSuffix = opt '_' >> integerTypeSuffix

public export
floatTypeSuffix : RExp True
floatTypeSuffix = "f32" <|> "f64"

public export
floatSuffix : RExp True
floatSuffix = opt '_' >> floatTypeSuffix

public export
exponentPart : RExp True
exponentPart =
  oneof ['e', 'E'] >> opt (oneof ['+', '-']) >> decimalDigits

--------------------------------------------------------------------------------
-- Valid numeric regexes.
--
-- These are used by the broad numeric candidate and documented separately so the
-- intended accepted forms remain visible.
--------------------------------------------------------------------------------
public export
binaryIntegerLiteral : RExp True
binaryIntegerLiteral =
  ("0b" <|> "0B") >> binaryDigits >> opt integerSuffix

public export
octalIntegerLiteral : RExp True
octalIntegerLiteral =
  ("0o" <|> "0O") >> octalDigits >> opt integerSuffix

public export
hexIntegerLiteral : RExp True
hexIntegerLiteral =
  ("0x" <|> "0X") >> hexDigits >> opt integerSuffix

public export
decimalIntegerLiteral : RExp True
decimalIntegerLiteral =
  decimalDigits >> opt integerSuffix

public export
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
public export
floatLiteral : RExp True
floatLiteral =
      (decimalDigits >> '.' >> decimalDigits >> opt exponentPart >> opt floatSuffix)
  <|> (decimalDigits >> exponentPart >> opt floatSuffix)
  <|> (decimalDigits >> floatSuffix)

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
public export
radixNumberCandidate : RExp True
radixNumberCandidate =
      (("0b" <|> "0B") >> star (asciiAlphaNum <|> '_'))
  <|> (("0o" <|> "0O") >> star (asciiAlphaNum <|> '_'))
  <|> (("0x" <|> "0X") >> star (asciiAlphaNum <|> '_'))

public export
looseExponentPart : RExp True
looseExponentPart =
  oneof ['e', 'E'] >> opt (oneof ['+', '-']) >> star (asciiAlphaNum <|> '_')

public export
dottedDecimalNumberCandidate : RExp True
dottedDecimalNumberCandidate =
  decimalDigits >> '.' >> decimalDigits >> opt looseExponentPart >> star (asciiAlphaNum <|> '_')

public export
exponentNumberCandidate : RExp True
exponentNumberCandidate =
  decimalDigits >> looseExponentPart >> star (asciiAlphaNum <|> '_')

public export
suffixedDecimalNumberCandidate : RExp True
suffixedDecimalNumberCandidate =
  decimalDigits >> (asciiLetter <|> '_') >> star (asciiAlphaNum <|> '_')

public export
plainDecimalNumberCandidate : RExp True
plainDecimalNumberCandidate = decimalDigits

public export
numberCandidate : RExp True
numberCandidate =
      dottedDecimalNumberCandidate
  <|> floatLiteral
  <|> radixNumberCandidate
  <|> exponentNumberCandidate
  <|> suffixedDecimalNumberCandidate
  <|> plainDecimalNumberCandidate

--------------------------------------------------------------------------------
-- String, basis-string, byte literal, byte-string, and ordinary char candidates.
--
-- These candidates are deliberately broader than the set of valid literals.
-- Rules.idr validates the raw spelling and raises a structured error instead of
-- letting bad literals split into unrelated tokens.
--------------------------------------------------------------------------------
public export
normalStringBodyCandidate : RExp True
normalStringBodyCandidate =
      ('\\' >> dot)
  <|> (dot && not '"' && not '\n' && not '\r')

public export
normalStringCandidate : RExp True
normalStringCandidate =
  '"' >> star normalStringBodyCandidate >> '"'

public export
unterminatedNormalStringCandidate : RExp True
unterminatedNormalStringCandidate =
  '"' >> star normalStringBodyCandidate

public export
basisStringCandidate : RExp True
basisStringCandidate =
  'b' >> 's' >> '"' >> star (dot && not '"' && not '\n' && not '\r') >> '"'

public export
unterminatedBasisStringCandidate : RExp True
unterminatedBasisStringCandidate =
  'b' >> 's' >> '"' >> star (dot && not '"' && not '\n' && not '\r')

public export
byteStringBodyCandidate : RExp True
byteStringBodyCandidate =
      ('\\' >> dot)
  <|> (dot && not '"' && not '\n' && not '\r')

public export
byteStringCandidate : RExp True
byteStringCandidate =
  'b' >> '"' >> star byteStringBodyCandidate >> '"'

public export
unterminatedByteStringCandidate : RExp True
unterminatedByteStringCandidate =
  'b' >> '"' >> star byteStringBodyCandidate

public export
byteLiteralBodyCandidate : RExp True
byteLiteralBodyCandidate =
      ('\\' >> dot)
  <|> (dot && not '\'' && not '\n' && not '\r')

public export
byteLiteralCandidate : RExp True
byteLiteralCandidate =
  'b' >> '\'' >> star byteLiteralBodyCandidate >> '\''

public export
unterminatedByteLiteralCandidate : RExp True
unterminatedByteLiteralCandidate =
  'b' >> '\'' >> star byteLiteralBodyCandidate

public export
ordinaryCharLiteralCandidate : RExp True
ordinaryCharLiteralCandidate =
  '\'' >> star byteLiteralBodyCandidate >> '\''

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
public export
lineCommentTail : RExp False
lineCommentTail = star notLineBreakChar

public export
outerDocLineBody : RExp False
outerDocLineBody =
  opt ((dot && not '/' && not '\n' && not '\r') >> lineCommentTail)

public export
outerDocLineComment : RExp True
outerDocLineComment =
  '/' >> '/' >> '/' >> outerDocLineBody

public export
innerDocLineComment : RExp True
innerDocLineComment =
  '/' >> '/' >> '!' >> lineCommentTail

public export
normalLineComment : RExp True
normalLineComment =
  '/' >> '/' >> lineCommentTail

--------------------------------------------------------------------------------
-- Block comments.
--
-- Outer block docs are classified by the opening delimiter.  `/**/` and
-- `/***/` are normal comments, not docs, so the outer-doc opener consumes one
-- first body character that must be neither `*` nor `/`.  Inner block docs may
-- be empty, so `/*!*/` is a valid inner doc comment.
--------------------------------------------------------------------------------
public export
outerBlockDocFirstBodyChar : RExp True
outerBlockDocFirstBodyChar =
      (dot && not '*' && not '/' && not '\n' && not '\r')
  <|> lineBreak

public export
outerBlockDocOpen : RExp True
outerBlockDocOpen =
  '/' >> '*' >> '*' >> outerBlockDocFirstBodyChar

public export
innerBlockDocOpen : RExp True
innerBlockDocOpen =
  '/' >> '*' >> '!'

public export
normalBlockCommentOpen : RExp True
normalBlockCommentOpen =
  '/' >> '*'

public export
blockCommentClose : RExp True
blockCommentClose =
  '*' >> '/'

public export
blockCommentBodyChunk : RExp True
blockCommentBodyChunk =
  plus (dot && not '*' && not '/' && not '\n' && not '\r')

public export
blockCommentLineBreak : RExp True
blockCommentLineBreak = lineBreak

public export
blockCommentSingleStar : RExp True
blockCommentSingleStar = '*'

public export
blockCommentSingleSlash : RExp True
blockCommentSingleSlash = '/'
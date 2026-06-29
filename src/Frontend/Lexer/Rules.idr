module Frontend.Lexer.Rules

import Data.ByteString
import Data.List
import Data.Prim.Bits32
import Data.Linear.Ref1
import Data.String
import Derive.Prelude
import Language.Reflection
import Syntax.T1
import Text.ILex
import Text.ILex.Interfaces
import Text.ParseError

import Frontend.Token
import Frontend.Lexer.Errors
import Frontend.Lexer.Regex

%default total
%language ElabReflection
%hide Prelude.(>>)
%hide Prelude.(>>=)
%hide Prelude.(<*)
%hide Prelude.pure
%hide Prelude.not

--------------------------------------------------------------------------------
-- Lexer states.
--
-- The prompt requires exactly two states:
--   * Initial        ordinary Leaf source text
--   * InBlockComment counting state for nested block comments
--
-- Arbitrary nesting is represented by `commentDepth`, not by adding more states.
--------------------------------------------------------------------------------
public export
leafStateCount : Bits32
leafStateCount = 2

public export
0 LeafState : Type
LeafState = Index 2

public export
initialState : LeafState
initialState = Ini

public export
inBlockCommentState : LeafState
inBlockCommentState = 1

--------------------------------------------------------------------------------
-- Documentation-comment mode.
--
-- This mode is chosen by the outermost block-comment opener. Nested block
-- comments only affect `commentDepth`; they never change the doc/non-doc mode
-- of the outer comment.
--------------------------------------------------------------------------------
public export
data CommentMode
  = NormalBlockComment
  | OuterBlockDocComment
  | InnerBlockDocComment

%runElab derive "CommentMode" [Show, Eq]

isDocCommentMode : CommentMode -> Bool
isDocCommentMode NormalBlockComment = False
isDocCommentMode OuterBlockDocComment = True
isDocCommentMode InnerBlockDocComment = True

commentModeToToken : CommentMode -> String -> Maybe Token
commentModeToToken NormalBlockComment _ = Nothing
commentModeToToken OuterBlockDocComment rawText = Just (TokOuterDoc rawText)
commentModeToToken InnerBlockDocComment rawText = Just (TokInnerDoc rawText)

--------------------------------------------------------------------------------
-- Mutable ilex stack.
--
-- This version intentionally uses the installed Text.ILex interfaces:
--   * HasBytes.bytes     -- one current token byte string, maintained by ilex
--   * HasPosition        -- current source line/column plus opener stack
--   * HasError           -- first stored BoundedErr LexerError
--   * HasStack           -- emitted Bounded Token values
--
-- It relies only on current Text.ILex position, byte, error, and stack
-- interfaces.
--------------------------------------------------------------------------------
public export
record LeafLexerStack (q : Type) where
  constructor MkLeafLexerStack
  currentBytes       : Ref q ByteString
  currentLine        : Ref q Nat
  currentColumn      : Ref q Nat
  openingPositions   : Ref q (SnocList Position)
  outputTokens       : Ref q (SnocList (Bounded Token))
  pendingError       : Ref q (Maybe (BoundedErr LexerError))
  commentDepth       : Ref q Nat
  commentMode        : Ref q CommentMode
  commentTextPieces  : Ref q (SnocList String)

export
HasBytes LeafLexerStack where
  bytes = currentBytes

export
HasPosition LeafLexerStack where
  line = currentLine
  col = currentColumn
  positions = openingPositions

export
HasError LeafLexerStack LexerError where
  error = pendingError

export
HasStack LeafLexerStack (SnocList (Bounded Token)) where
  stack = outputTokens

public export
initLeafLexerStack : F1 q (LeafLexerStack q)
initLeafLexerStack = T1.do
  currentBytesRef      <- ref1 empty
  currentLineRef       <- ref1 Z
  currentColumnRef     <- ref1 Z
  openingPositionsRef  <- ref1 [<]
  outputTokensRef      <- ref1 [<]
  pendingErrorRef      <- ref1 Nothing
  commentDepthRef      <- ref1 Z
  commentModeRef       <- ref1 NormalBlockComment
  commentTextPiecesRef <- ref1 [<]

  pure $ MkLeafLexerStack
    currentBytesRef
    currentLineRef
    currentColumnRef
    openingPositionsRef
    outputTokensRef
    pendingErrorRef
    commentDepthRef
    commentModeRef
    commentTextPiecesRef

--------------------------------------------------------------------------------
-- Local rule wrappers.
--
-- These wrappers deliberately use the installed Text.ILex action helpers.
--
-- `readRule` is for single-line/string-like lexemes. It passes the matched text
-- to the action and then advances the column.
--
-- `bytesRule` is for lexemes whose matched bytes may contain newlines. It passes
-- the matched ByteString to the action and then advances line/column with
-- `multiline`.
--------------------------------------------------------------------------------
public export
0 LeafRule : Type -> Type
LeafRule q = (RExp True, Step q 2 LeafLexerStack)

readRule :
     RExp True
  -> ((sk : LeafLexerStack q) => String -> F1 q LeafState)
  -> LeafRule q
readRule expression action =
  Text.ILex.Interfaces.read expression action

bytesRule :
     RExp True
  -> ((sk : LeafLexerStack q) => ByteString -> F1 q LeafState)
  -> LeafRule q
bytesRule expression action =
  multiline expression action

ignoreRule :
     RExp True
  -> LeafRule q
ignoreRule expression =
  multiline' expression initialState

--------------------------------------------------------------------------------
-- Bounds helpers.
--
-- The action wrappers call the action before advancing the current position, and
-- the runner has already written the current token bytes to `bytes`. Therefore
-- a Bounded value for the current lexeme can be constructed from the current
-- position and `incBytes currentBytes currentPosition`.
--------------------------------------------------------------------------------
currentTokenBounds :
     (sk : LeafLexerStack q)
  => F1 q Bounds
currentTokenBounds = T1.do
  startPosition <- getPosition
  tokenBytes <- read1 (bytes sk)
  pure (BS startPosition (incBytes tokenBytes startPosition))

boundedHere :
     (sk : LeafLexerStack q)
  => a
  -> F1 q (Bounded a)
boundedHere value = T1.do
  tokenBounds <- currentTokenBounds
  pure (B value tokenBounds)

currentTokenString :
     (sk : LeafLexerStack q)
  => F1 q String
currentTokenString = T1.do
  tokenBytes <- read1 (bytes sk)
  pure (toString tokenBytes)

popOpenPositionForCurrentToken :
     (sk : LeafLexerStack q)
  => F1 q Bounds
popOpenPositionForCurrentToken = T1.do
  endPositionStart <- getPosition
  tokenBytes <- read1 (bytes sk)
  let endPosition = incBytes tokenBytes endPositionStart
  read1 (positions sk) >>= \case
    openStack :< openPosition => T1.do
      write1 (positions sk) openStack
      pure (BS openPosition endPosition)

    [<] =>
      pure NoBounds

--------------------------------------------------------------------------------
-- General character helpers used by literal validators.
--------------------------------------------------------------------------------
charBetween : Char -> Char -> Char -> Bool
charBetween lower upper value =
  lower <= value && value <= upper

isAsciiDigitChar : Char -> Bool
isAsciiDigitChar value = charBetween '0' '9' value

isAsciiLetterChar : Char -> Bool
isAsciiLetterChar value =
  charBetween 'a' 'z' value || charBetween 'A' 'Z' value

isAsciiAlphaNumUnderscoreChar : Char -> Bool
isAsciiAlphaNumUnderscoreChar value =
  isAsciiLetterChar value || isAsciiDigitChar value || value == '_'

isBinaryDigitChar : Char -> Bool
isBinaryDigitChar value = value == '0' || value == '1'

isOctalDigitChar : Char -> Bool
isOctalDigitChar value = charBetween '0' '7' value

isHexDigitChar : Char -> Bool
isHexDigitChar value =
     charBetween '0' '9' value
  || charBetween 'a' 'f' value
  || charBetween 'A' 'F' value

isBasisStringChar : Char -> Bool
isBasisStringChar value =
     value == '0'
  || value == '1'
  || value == '+'
  || value == '-'
  || value == 'i'
  || value == 'I'

isPlainByteLiteralChar : Char -> Bool
isPlainByteLiteralChar value =
     charBetween ' ' '~' value
  && value /= '\\'
  && value /= '\''

isPlainByteStringChar : Char -> Bool
isPlainByteStringChar value =
     charBetween ' ' '~' value
  && value /= '\\'
  && value /= '"'

isSimpleByteEscapeChar : Char -> Bool
isSimpleByteEscapeChar value =
     value == 'n'
  || value == 'r'
  || value == 't'
  || value == '0'
  || value == '\\'
  || value == '\''
  || value == '"'

allChars : (Char -> Bool) -> List Char -> Bool
allChars predicate [] = True
allChars predicate (value :: rest) =
  case predicate value of
    True  => allChars predicate rest
    False => False

anyChar : (Char -> Bool) -> List Char -> Bool
anyChar predicate [] = False
anyChar predicate (value :: rest) =
  case predicate value of
    True  => True
    False => anyChar predicate rest

lastCharSatisfies : (Char -> Bool) -> List Char -> Bool
lastCharSatisfies predicate [] = False
lastCharSatisfies predicate (value :: []) = predicate value
lastCharSatisfies predicate (_ :: rest) = lastCharSatisfies predicate rest

--------------------------------------------------------------------------------
-- List/string prefix and suffix helpers.
--------------------------------------------------------------------------------
splitLast : List a -> Maybe (List a, a)
splitLast [] = Nothing
splitLast (value :: []) = Just ([], value)
splitLast (value :: rest) =
  case splitLast rest of
    Nothing => Nothing
    Just (initialValues, lastValue) => Just (value :: initialValues, lastValue)

dropPrefixChars : List Char -> List Char -> Maybe (List Char)
dropPrefixChars [] remainingChars = Just remainingChars
dropPrefixChars (prefixChar :: prefixRest) (valueChar :: valueRest) =
  case prefixChar == valueChar of
    True  => dropPrefixChars prefixRest valueRest
    False => Nothing
dropPrefixChars _ _ = Nothing

stripReversedSuffix : List Char -> List Char -> Maybe (List Char)
stripReversedSuffix [] remainingReversedChars =
  Just (reverse remainingReversedChars)
stripReversedSuffix (suffixChar :: suffixRest) (valueChar :: valueRest) =
  case suffixChar == valueChar of
    True  => stripReversedSuffix suffixRest valueRest
    False => Nothing
stripReversedSuffix _ [] = Nothing

stripSuffixChars : List Char -> List Char -> Maybe (List Char)
stripSuffixChars suffixChars valueChars =
  stripReversedSuffix (reverse suffixChars) (reverse valueChars)

stripFirstMatchingSuffix : List (List Char) -> List Char -> Maybe (List Char)
stripFirstMatchingSuffix [] valueChars = Nothing
stripFirstMatchingSuffix (suffixChars :: remainingSuffixes) valueChars =
  case stripSuffixChars suffixChars valueChars of
    Just strippedValue => Just strippedValue
    Nothing => stripFirstMatchingSuffix remainingSuffixes valueChars

payloadBetweenQuotes : List Char -> Maybe (List Char)
payloadBetweenQuotes ('"' :: rest) =
  case splitLast rest of
    Just (payload, '"') => Just payload
    _ => Nothing
payloadBetweenQuotes _ = Nothing

payloadAfterPrefixAndQuotes : List Char -> List Char -> Maybe (List Char)
payloadAfterPrefixAndQuotes prefixChars valueChars =
  case dropPrefixChars prefixChars valueChars of
    Just remainingChars => payloadBetweenQuotes remainingChars
    Nothing => Nothing

payloadBetweenApostrophes : List Char -> Maybe (List Char)
payloadBetweenApostrophes ('\'' :: rest) =
  case splitLast rest of
    Just (payload, '\'') => Just payload
    _ => Nothing
payloadBetweenApostrophes _ = Nothing

byteLiteralPayload : String -> Maybe (List Char)
byteLiteralPayload rawText =
  case dropPrefixChars ['b'] (unpack rawText) of
    Just remainingChars => payloadBetweenApostrophes remainingChars
    Nothing => Nothing

--------------------------------------------------------------------------------
-- Literal validators.
--------------------------------------------------------------------------------
validByteEscapePayload : List Char -> Bool
validByteEscapePayload ('\\' :: 'x' :: firstHex :: secondHex :: []) =
  isHexDigitChar firstHex && isHexDigitChar secondHex
validByteEscapePayload ('\\' :: escapedChar :: []) =
  isSimpleByteEscapeChar escapedChar
validByteEscapePayload _ = False

validByteLiteralPayload : List Char -> Bool
validByteLiteralPayload (value :: []) = isPlainByteLiteralChar value
validByteLiteralPayload payload = validByteEscapePayload payload

validByteStringPayload : List Char -> Bool
validByteStringPayload [] = True
validByteStringPayload ('\\' :: 'x' :: firstHex :: secondHex :: rest) =
  case isHexDigitChar firstHex && isHexDigitChar secondHex of
    True  => validByteStringPayload rest
    False => False
validByteStringPayload ('\\' :: escapedChar :: rest) =
  case isSimpleByteEscapeChar escapedChar of
    True  => validByteStringPayload rest
    False => False
validByteStringPayload (value :: rest) =
  case isPlainByteStringChar value of
    True  => validByteStringPayload rest
    False => False

validNormalStringLiteral : String -> Bool
validNormalStringLiteral rawText =
  case payloadBetweenQuotes (unpack rawText) of
    Just payload => allChars isAsciiAlphaNumUnderscoreChar payload
    Nothing => False

validBasisStringLiteral : String -> Bool
validBasisStringLiteral rawText =
  case payloadAfterPrefixAndQuotes ['b', 's'] (unpack rawText) of
    Just payload => allChars isBasisStringChar payload
    Nothing => False

validByteLiteral : String -> Bool
validByteLiteral rawText =
  case byteLiteralPayload rawText of
    Just payload => validByteLiteralPayload payload
    Nothing => False

validByteStringLiteral : String -> Bool
validByteStringLiteral rawText =
  case payloadAfterPrefixAndQuotes ['b'] (unpack rawText) of
    Just payload => validByteStringPayload payload
    Nothing => False

--------------------------------------------------------------------------------
-- Numeric literal validation.
--
-- The lexer preserves raw spelling. These checks validate only lexical shape:
-- base prefix, digit alphabet, underscore placement, exponent syntax, and suffix
-- spelling. They do not check integer bounds or floating-point precision.
--------------------------------------------------------------------------------
public export
data NumberLiteralKind
  = IntegerNumberLiteral
  | FloatingNumberLiteral

%runElab derive "NumberLiteralKind" [Show, Eq]

integerSuffixes : List (List Char)
integerSuffixes =
  [ unpack "_i128", unpack "_u128"
  , unpack "_i64",  unpack "_u64"
  , unpack "_i32",  unpack "_u32"
  , unpack "_i16",  unpack "_u16"
  , unpack "_i8",   unpack "_u8"
  , unpack "i128",  unpack "u128"
  , unpack "i64",   unpack "u64"
  , unpack "i32",   unpack "u32"
  , unpack "i16",   unpack "u16"
  , unpack "i8",    unpack "u8"
  ]

floatSuffixes : List (List Char)
floatSuffixes =
  [ unpack "_f64", unpack "_f32", unpack "f64", unpack "f32" ]

stripKnownIntegerSuffix : List Char -> (List Char, Bool)
stripKnownIntegerSuffix chars =
  case stripFirstMatchingSuffix integerSuffixes chars of
    Just body => (body, True)
    Nothing => (chars, False)

stripKnownFloatSuffix : List Char -> (List Char, Bool)
stripKnownFloatSuffix chars =
  case stripFirstMatchingSuffix floatSuffixes chars of
    Just body => (body, True)
    Nothing => (chars, False)

validDigitSequence : Bool -> (Char -> Bool) -> List Char -> Bool
validDigitSequence allowLeadingUnderscore digitPredicate chars =
  case chars of
    [] => False
    firstChar :: _ =>
      case allowLeadingUnderscore || firstChar /= '_' of
        False => False
        True =>
             allChars (\value => digitPredicate value || value == '_') chars
          && anyChar digitPredicate chars
          && lastCharSatisfies digitPredicate chars

validDecimalDigitSequence : List Char -> Bool
validDecimalDigitSequence = validDigitSequence False isAsciiDigitChar

validRadixDigitSequence : (Char -> Bool) -> List Char -> Bool
validRadixDigitSequence digitPredicate chars =
  validDigitSequence True digitPredicate chars

stripRadixPrefix : List Char -> Maybe (Char, List Char)
stripRadixPrefix ('0' :: 'b' :: rest) = Just ('b', rest)
stripRadixPrefix ('0' :: 'B' :: rest) = Just ('b', rest)
stripRadixPrefix ('0' :: 'o' :: rest) = Just ('o', rest)
stripRadixPrefix ('0' :: 'O' :: rest) = Just ('o', rest)
stripRadixPrefix ('0' :: 'x' :: rest) = Just ('x', rest)
stripRadixPrefix ('0' :: 'X' :: rest) = Just ('x', rest)
stripRadixPrefix _ = Nothing

validRadixIntegerLiteral : List Char -> Bool
validRadixIntegerLiteral chars =
  case stripRadixPrefix chars of
    Just ('b', rest) =>
      let (digitChars, _) = stripKnownIntegerSuffix rest in
        validRadixDigitSequence isBinaryDigitChar digitChars

    Just ('o', rest) =>
      let (digitChars, _) = stripKnownIntegerSuffix rest in
        validRadixDigitSequence isOctalDigitChar digitChars

    Just ('x', rest) =>
      let (digitChars, _) = stripKnownIntegerSuffix rest in
        validRadixDigitSequence isHexDigitChar digitChars

    _ => False

validDecimalIntegerLiteral : List Char -> Bool
validDecimalIntegerLiteral chars =
  case stripRadixPrefix chars of
    Just _ => False
    Nothing =>
      let (digitChars, _) = stripKnownIntegerSuffix chars in
        validDecimalDigitSequence digitChars

validIntegerLiteral : String -> Bool
validIntegerLiteral rawText =
  let chars = unpack rawText in
    validRadixIntegerLiteral chars || validDecimalIntegerLiteral chars

splitAtFirstGo :
     (Char -> Bool)
  -> List Char
  -> List Char
  -> Maybe (List Char, List Char)
splitAtFirstGo predicate reversedPrefix [] = Nothing
splitAtFirstGo predicate reversedPrefix (value :: rest) =
  case predicate value of
    True => Just (reverse reversedPrefix, rest)
    False => splitAtFirstGo predicate (value :: reversedPrefix) rest

splitAtFirst : (Char -> Bool) -> List Char -> Maybe (List Char, List Char)
splitAtFirst predicate chars =
  splitAtFirstGo predicate [] chars

containsCharWhere : (Char -> Bool) -> List Char -> Bool
containsCharWhere = anyChar

isExponentMarker : Char -> Bool
isExponentMarker value = value == 'e' || value == 'E'

isDotChar : Char -> Bool
isDotChar value = value == '.'

splitExponent : List Char -> Maybe (List Char, List Char)
splitExponent chars =
  case splitAtFirst isExponentMarker chars of
    Just (mantissaChars, exponentChars) =>
      case containsCharWhere isExponentMarker exponentChars of
        True => Nothing
        False => Just (mantissaChars, exponentChars)
    Nothing => Nothing

validExponentPayload : List Char -> Bool
validExponentPayload ('+' :: rest) = validDecimalDigitSequence rest
validExponentPayload ('-' :: rest) = validDecimalDigitSequence rest
validExponentPayload rest = validDecimalDigitSequence rest

validDottedFloatBody : List Char -> List Char -> Bool
validDottedFloatBody beforeDot afterDotAndMaybeExponent =
  case validDecimalDigitSequence beforeDot of
    False => False
    True =>
      case splitExponent afterDotAndMaybeExponent of
        Just (afterDot, exponentPayload) =>
             validDecimalDigitSequence afterDot
          && validExponentPayload exponentPayload

        Nothing =>
          validDecimalDigitSequence afterDotAndMaybeExponent

validExponentFloatBody : List Char -> Bool
validExponentFloatBody chars =
  case splitExponent chars of
    Just (mantissaChars, exponentPayload) =>
         validDecimalDigitSequence mantissaChars
      && validExponentPayload exponentPayload
    Nothing => False

validFloatLiteral : String -> Bool
validFloatLiteral rawText =
  let (bodyChars, hadFloatSuffix) = stripKnownFloatSuffix (unpack rawText) in
    case splitAtFirst isDotChar bodyChars of
      Just (beforeDot, afterDotAndMaybeExponent) =>
        validDottedFloatBody beforeDot afterDotAndMaybeExponent

      Nothing =>
        case splitExponent bodyChars of
          Just _ => validExponentFloatBody bodyChars
          Nothing =>
            case hadFloatSuffix of
              True => validDecimalDigitSequence bodyChars
              False => False

classifyNumberLiteral : String -> Maybe NumberLiteralKind
classifyNumberLiteral rawText =
  case validFloatLiteral rawText of
    True => Just FloatingNumberLiteral
    False =>
      case validIntegerLiteral rawText of
        True => Just IntegerNumberLiteral
        False => Nothing

--------------------------------------------------------------------------------
-- Token and error actions.
--------------------------------------------------------------------------------
emitBoundedToken :
     (sk : LeafLexerStack q)
  => Bounded Token
  -> F1 q LeafState
emitBoundedToken boundedToken =
  pushStackAs boundedToken initialState

emitToken :
     (sk : LeafLexerStack q)
  => Token
  -> F1 q LeafState
emitToken token = T1.do
  boundedToken <- boundedHere token
  emitBoundedToken boundedToken

rememberFatalError :
     (sk : LeafLexerStack q)
  => LexerError
  -> F1 q LeafState
rememberFatalError lexerError = T1.do
  existingError <- read1 (error sk)
  case existingError of
    Just _ => pure initialState
    Nothing => T1.do
      errorBounds <- currentTokenBounds
      write1 (error sk) (Just (B (Custom lexerError) errorBounds))
      pure initialState

emitValidatedLiteral :
     (sk : LeafLexerStack q)
  => (String -> Bool)
  -> (String -> LexerError)
  -> (String -> Token)
  -> String
  -> F1 q LeafState
emitValidatedLiteral validator errorBuilder tokenBuilder rawText =
  case validator rawText of
    True => emitToken (tokenBuilder rawText)
    False => rememberFatalError (errorBuilder rawText)

emitNormalStringLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitNormalStringLiteral =
  emitValidatedLiteral
    validNormalStringLiteral
    LexInvalidStringLiteral
    TokStringLitRaw

emitBasisStringLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitBasisStringLiteral =
  emitValidatedLiteral
    validBasisStringLiteral
    LexInvalidBasisStringLiteral
    TokBasisStringLitRaw

emitByteLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitByteLiteral =
  emitValidatedLiteral
    validByteLiteral
    LexInvalidByteLiteral
    TokByteLitRaw

emitByteStringLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitByteStringLiteral =
  emitValidatedLiteral
    validByteStringLiteral
    LexInvalidByteStringLiteral
    TokByteStringLitRaw

emitNumberLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitNumberLiteral rawText =
  case classifyNumberLiteral rawText of
    Just IntegerNumberLiteral =>
      emitToken (TokIntLitRaw rawText)

    Just FloatingNumberLiteral =>
      emitToken (TokFloatLitRaw rawText)

    Nothing =>
      rememberFatalError (LexInvalidNumberLiteral rawText)

emitUnterminatedStringLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitUnterminatedStringLiteral _ =
  rememberFatalError LexUnterminatedStringLiteral

emitInvalidBasisStringLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitInvalidBasisStringLiteral rawText =
  rememberFatalError (LexInvalidBasisStringLiteral rawText)

emitInvalidByteStringLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitInvalidByteStringLiteral rawText =
  rememberFatalError (LexInvalidByteStringLiteral rawText)

emitInvalidByteLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitInvalidByteLiteral rawText =
  rememberFatalError (LexInvalidByteLiteral rawText)

emitOrdinaryCharLiteralError :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitOrdinaryCharLiteralError _ =
  rememberFatalError LexOrdinaryCharLiteralNeedsToken

--------------------------------------------------------------------------------
-- Block-comment actions.
--------------------------------------------------------------------------------
resetCommentText :
     (sk : LeafLexerStack q)
  => F1' q
resetCommentText =
  write1 (commentTextPieces sk) [<]

appendCurrentBytesToCommentText :
     (sk : LeafLexerStack q)
  => F1' q
appendCurrentBytesToCommentText = T1.do
  mode <- read1 (commentMode sk)
  case isDocCommentMode mode of
    False => pure ()
    True => T1.do
      rawPiece <- currentTokenString
      push1 (commentTextPieces sk) rawPiece

collectedCommentText :
     (sk : LeafLexerStack q)
  => F1 q String
collectedCommentText = T1.do
  pieces <- replace1 (commentTextPieces sk) [<]
  pure (snocPack pieces)

beginBlockComment :
     (sk : LeafLexerStack q)
  => CommentMode
  -> ByteString
  -> F1 q LeafState
beginBlockComment mode _ = T1.do
  pushPosition
  write1 (commentDepth sk) 1
  write1 (commentMode sk) mode
  resetCommentText
  appendCurrentBytesToCommentText
  pure inBlockCommentState

beginNestedBlockComment :
     (sk : LeafLexerStack q)
  => ByteString
  -> F1 q LeafState
beginNestedBlockComment _ = T1.do
  pushPosition
  mod1 (commentDepth sk) S
  appendCurrentBytesToCommentText
  pure inBlockCommentState

finishOutermostBlockComment :
     (sk : LeafLexerStack q)
  => CommentMode
  -> Bounds
  -> F1 q LeafState
finishOutermostBlockComment mode fullCommentBounds = T1.do
  rawCommentText <- collectedCommentText
  write1 (commentMode sk) NormalBlockComment
  case commentModeToToken mode rawCommentText of
    Nothing => pure initialState
    Just docToken =>
      emitBoundedToken (B docToken fullCommentBounds)

closeBlockComment :
     (sk : LeafLexerStack q)
  => ByteString
  -> F1 q LeafState
closeBlockComment _ = T1.do
  appendCurrentBytesToCommentText
  currentDepth <- read1 (commentDepth sk)
  case currentDepth of
    Z =>
      rememberFatalError LexUnterminatedBlockComment

    S remainingDepth =>
      case remainingDepth of
        Z => T1.do
          mode <- read1 (commentMode sk)
          fullCommentBounds <- popOpenPositionForCurrentToken
          write1 (commentDepth sk) Z
          finishOutermostBlockComment mode fullCommentBounds

        S _ => T1.do
          popPosition
          write1 (commentDepth sk) remainingDepth
          pure inBlockCommentState

consumeBlockCommentText :
     (sk : LeafLexerStack q)
  => ByteString
  -> F1 q LeafState
consumeBlockCommentText _ = T1.do
  appendCurrentBytesToCommentText
  pure inBlockCommentState

--------------------------------------------------------------------------------
-- Symbol rule generation.
--------------------------------------------------------------------------------
symbolRuleFromTableEntry :
     (String, Symbol)
  -> Maybe (LeafRule q)
symbolRuleFromTableEntry (symbolText, symbol) =
  case unpack symbolText of
    [] => Nothing
    symbolChars@(_ :: _) =>
      Just (readRule (chars symbolChars) (\_ => emitToken (TokSym symbol)))

public export
symbolRules : List (LeafRule q)
symbolRules =
  mapMaybe symbolRuleFromTableEntry symbolTable

--------------------------------------------------------------------------------
-- Initial-state rules.
--
-- Ordering is intentional:
--   1. documentation comments before ordinary comments and before `/`
--   2. broad literal candidates before identifiers
--   3. broad number candidate before identifier-like suffixes
--   4. single identifier rule through `tokenFromIdentLike`
--   5. generated symbol rules from `symbolTable`
--------------------------------------------------------------------------------
public export
initialRules : List (LeafRule q)
initialRules =
  [ readRule outerDocLineComment (\rawText => emitToken (TokOuterDoc rawText))
  , readRule innerDocLineComment (\rawText => emitToken (TokInnerDoc rawText))

  , bytesRule outerBlockDocOpen (beginBlockComment OuterBlockDocComment)
  , bytesRule innerBlockDocOpen (beginBlockComment InnerBlockDocComment)
  , ignoreRule normalLineComment
  , bytesRule normalBlockCommentOpen (beginBlockComment NormalBlockComment)
  , ignoreRule leafWhitespace

  , readRule basisStringCandidate emitBasisStringLiteral
  , readRule byteStringCandidate emitByteStringLiteral
  , readRule byteLiteralCandidate emitByteLiteral
  , readRule normalStringCandidate emitNormalStringLiteral

  -- Unterminated candidates come after closed-literal candidates, so a valid
  -- string wins by maximal munch. They come before identifiers so `bs"bad` is
  -- not split into `bs` and a string fragment.
  , readRule unterminatedBasisStringCandidate emitInvalidBasisStringLiteral
  , readRule unterminatedByteStringCandidate emitInvalidByteStringLiteral
  , readRule unterminatedByteLiteralCandidate emitInvalidByteLiteral
  , readRule unterminatedNormalStringCandidate emitUnterminatedStringLiteral
  , readRule ordinaryCharLiteralCandidate emitOrdinaryCharLiteralError

  , readRule numberCandidate emitNumberLiteral
  , readRule identifierLike (\rawText => emitToken (tokenFromIdentLike rawText))
  ] ++ symbolRules

--------------------------------------------------------------------------------
-- Block-comment rules.
--
-- Active only in `InBlockComment`. `/*` increments depth, `*/` decrements depth,
-- and only the outermost close returns to Initial.
--------------------------------------------------------------------------------
public export
blockCommentRules : List (LeafRule q)
blockCommentRules =
  [ bytesRule normalBlockCommentOpen beginNestedBlockComment
  , bytesRule blockCommentClose closeBlockComment
  , bytesRule blockCommentBodyChunk consumeBlockCommentText
  , bytesRule blockCommentLineBreak consumeBlockCommentText
  , bytesRule blockCommentSingleStar consumeBlockCommentText
  , bytesRule blockCommentSingleSlash consumeBlockCommentText
  ]

--------------------------------------------------------------------------------
-- DFAs, error handlers, and final P1 lexer.
--------------------------------------------------------------------------------
public export
leafLexerSteps : Lex1 q 2 LeafLexerStack
leafLexerSteps =
  lex1
    [ E initialState (dfa initialRules)
    , E inBlockCommentState (dfa blockCommentRules)
    ]

oldestOpenPosition : SnocList Position -> Maybe Position
oldestOpenPosition [<] =
  Nothing
oldestOpenPosition (olderPositions :< openPosition) =
  case olderPositions of
    [<] =>
      Just openPosition

    _ =>
      oldestOpenPosition olderPositions

unterminatedCommentBounds :
     LeafLexerStack q
  -> F1 q Bounds
unterminatedCommentBounds stackValue = T1.do
  lineValue <- read1 (currentLine stackValue)
  columnValue <- read1 (currentColumn stackValue)
  let endPosition = P lineValue columnValue
  openPositions <- read1 (positions stackValue)
  case oldestOpenPosition openPositions of
    Just openPosition =>
      pure (BS openPosition endPosition)

    Nothing =>
      pure NoBounds

public export
unterminatedBlockCommentError :
     LeafLexerStack q
  -> F1 q (BoundedErr LexerError)
unterminatedBlockCommentError stackValue = T1.do
  storedError <- read1 (error stackValue)
  case storedError of
    Just existingError =>
      pure existingError

    Nothing => T1.do
      unclosedBounds <- unterminatedCommentBounds stackValue
      pure (B (Custom LexUnterminatedBlockComment) unclosedBounds)

public export
leafLexerErrors :
  Arr32 2 (LeafLexerStack q -> F1 q (BoundedErr LexerError))
leafLexerErrors =
  errs [E inBlockCommentState unterminatedBlockCommentError]

public export
leafLexerEOI :
     LeafState
  -> LeafLexerStack q
  -> F1 q (Either (BoundedErr LexerError) (List (Bounded Token)))
leafLexerEOI _ stackValue = T1.do
  storedError <- read1 (error stackValue)
  case storedError of
    Just existingError =>
      pure (Left existingError)

    Nothing => T1.do
      blockCommentDepth <- read1 (commentDepth stackValue)
      case blockCommentDepth of
        S _ => T1.do
          unclosedBounds <- unterminatedCommentBounds stackValue
          pure (Left (B (Custom LexUnterminatedBlockComment) unclosedBounds))

        Z => T1.do
          tokens <- getList (outputTokens stackValue)
          pure (Right (tokens ++ [B TokEOF NoBounds]))

public export
leafLexer : Parser1 (BoundedErr LexerError) 2 LeafLexerStack (List (Bounded Token))
leafLexer =
  P initialState
    initLeafLexerStack
    leafLexerSteps
    snocChunk
    leafLexerErrors
    leafLexerEOI

module Frontend.Lexer.Rules

import Data.List
import Data.Linear.Ref1
import Data.String
import Syntax.T1
import Text.ILex
import Text.ILex.Derive
import Text.ILex.Interfaces
import Text.ILex.Stack
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
-- A known limitation of this pinned ilex version, worked around below for
-- three distinct grammar shapes. If you hit a case that doesn't fit any of
-- these, it's likely the same underlying bug -- read this first rather than
-- re-deriving it from scratch (or, worse, "simplifying" away one of the
-- workarounds below because it looks redundant; that's exactly what almost
-- happened to the block-comment-opener rules).
--
-- (a) Backtrack memory is lost across a node that is extensible but not
--     itself an accept. Concretely: if the lexer has already matched a
--     shorter valid token, then continues into a longer candidate that turns
--     out not to be a real accept anywhere along the way, it hard-fails
--     instead of falling back to the shorter match -- but *only* if some byte
--     in between led to a state with further transitions and no accept of
--     its own. A shorter match followed immediately by a dead end backtracks
--     fine; a shorter match, then one or more such "extend-only" bytes, then
--     a dead end, does not.
--       - `digitsThenDotOperatorCandidate` / `emitDigitsThenDotOperator`
--         (this file and `Regex.idr`): without it, `1..2` hard-fails instead
--         of lexing as `1`, `..`, `2`, because the node reached after `1.`
--         extends (a digit could follow) but isn't itself an accept.
--       - `allStarsOuterBlockComment` (`Regex.idr`, wired into `initialRules`
--         below): without it, `/**/`, `/***/`, `/****/`, and so on hard-fail
--         instead of lexing as ordinary (non-doc) comments, for the same
--         reason -- the node after a run of `*` extends toward a doc
--         comment's first body character but isn't an accept there. A real
--         `/***`-style banner comment opener (any run of two or more stars
--         not immediately followed by a closing `/`) hit the exact same dead
--         end, so the rule is now generalized to any run length via
--         `outerDocStarRun` instead of special-cased per exact length.
--       - `bareOuterBlockCommentOpen` (`Regex.idr`, wired into `initialRules`
--         below): the sub-case of the same dead end where the star run is
--         cut off by true end of input, with nothing left to say whether it
--         would have closed or become a doc comment -- e.g. a file truncated
--         right after `/***`. Falls back to an (eventually unterminated)
--         plain block comment, same as a bare `/*` at true end of input.
--     If you add a new rule whose prefix overlaps an existing shorter rule
--     with a possible dead end in between, test that overlap directly; don't
--     assume maximal munch alone covers it.
--
-- (b) At true end-of-input, ilex's own `endPos` can report a byte position
--     past the end of the input, inside a custom multi-state lexer like this
--     one. Worked around by `unterminatedCommentBounds` below, whose result
--     gets clamped to the input's actual length in
--     `Frontend.Lexer.Lexer.lexProgram` (`clampByteBounded`) before being
--     converted to a line/column position.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Lexer states.
--
-- The prompt requires exactly two states:
--   * Initial        ordinary Leaf source text
--   * InBlockComment counting state for nested block comments
--
-- Arbitrary nesting is represented by `commentDepth`, not by adding more states.
--------------------------------------------------------------------------------
%runElab deriveParserState "LeafSz" "LeafState" ["Initial", "InBlockComment"]

--------------------------------------------------------------------------------
-- Documentation-comment mode.
--
-- This mode is chosen by the outermost block-comment opener. Nested block
-- comments only affect the extra nesting depth; they never change the doc/non-doc mode
-- of the outer comment.
--------------------------------------------------------------------------------
data CommentMode
  = NormalBlockComment
  | OuterBlockDocComment
  | InnerBlockDocComment

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
-- The installed Text.ILex.Stack.Stack record already supplies HasBytes,
-- HasStringLits, HasBBErr, and HasStack instances (it derives them via
-- `%runElab derive "Stack" [FullStack]`). `LeafStack` only has to carry what
-- that generic record does not already provide: the emitted token buffer and
-- block-comment depth/mode counters. `commentDepth` counts nested comments
-- beyond the outermost block comment while in `InBlockComment`.
--------------------------------------------------------------------------------
record LeafStack where
  constructor MkLeafStack
  outputTokens : SnocList (ByteBounded Token)
  commentDepth : Nat
  commentMode  : CommentMode

0 LeafLexerStack : Type -> Type
LeafLexerStack = Stack LexerError LeafStack LeafSz

initLeafStack : LeafStack
initLeafStack = MkLeafStack [<] Z NormalBlockComment

--------------------------------------------------------------------------------
-- Local rule alias.
--------------------------------------------------------------------------------
0 LeafRule : Type -> Type
LeafRule q = (RExp True, Step q LeafSz LeafLexerStack)

--------------------------------------------------------------------------------
-- Bounds and position helpers built on top of the installed Text.ILex
-- position/error machinery (`Text.ILex.Interfaces`).
--------------------------------------------------------------------------------
oldestOpenPosition : SnocList BytePos -> Maybe BytePos
oldestOpenPosition [<] =
  Nothing
oldestOpenPosition (olderPositions :< openPosition) =
  case olderPositions of
    [<] =>
      Just openPosition

    _ =>
      oldestOpenPosition olderPositions

-- At true end-of-input ilex's own `endPos` can report a byte position past
-- the end of the input (a known quirk of this ilex version's EOI bookkeeping
-- inside a custom multi-state lexer). That overshoot is harmless here: it
-- gets clamped to the input's actual length in `Frontend.Lexer.Lexer.lexProgram`
-- before being converted to a line/column position, so there's no need to
-- special-case or pre-validate the bound this function returns.
unterminatedCommentBounds :
     LeafLexerStack q
  -> F1 q ByteBounds
unterminatedCommentBounds stackValue = T1.do
  endPosition <- endPos
  openPositions <- read1 (positions stackValue)
  case oldestOpenPosition openPositions of
    Just openPosition =>
      pure (BB openPosition endPosition)

    Nothing =>
      pure NoBB

--------------------------------------------------------------------------------
-- Suffix-stripping helper, used by `classifyDotOperatorSuffix` below to split
-- the `.`/`..`/`..=` operator off the trailing end of a matched
-- `digitsThenDotOperatorCandidate`.
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Literal validation.
--
-- Rather than re-parsing the already-matched raw text by hand, run it back
-- through the *strict* regexes from `Frontend.Lexer.Regex` (`integerLiteral`,
-- `floatLiteral`, `normalStringLiteralStrict`, ...) via `Text.ILex.Stack.value`,
-- ilex's own primitive for classifying a whole string against a set of regex
-- alternatives. Its "done" state has an empty DFA for anything but `Ignore`
-- alternatives, so any leftover unconsumed input after the first match fails
-- outright -- this rejects partial/prefix matches for free, with no
-- hand-rolled slicing, and keeps the strict regexes as the single source of
-- truth for what counts as a well-formed literal instead of a second,
-- independently-maintained copy of the same grammar.
--------------------------------------------------------------------------------
runValueMatch : Parser1 (BBErr Void) a -> String -> Maybe a
runValueMatch parser text =
  case runString parser text of
    Right result => Just result
    Left _        => Nothing

matchesLiteral : Parser1 (BBErr Void) () -> String -> Bool
matchesLiteral parser text =
  case runValueMatch parser text of
    Just () => True
    Nothing => False

data NumberLiteralKind
  = IntegerNumberLiteral
  | FloatingNumberLiteral

numberClassifier : PVal1 q Void NumberLiteralKind
numberClassifier =
  value Nothing
    [ (integerLiteral, const IntegerNumberLiteral)
    , (floatLiteral,   const FloatingNumberLiteral)
    ]

classifyNumberLiteral : String -> Maybe NumberLiteralKind
classifyNumberLiteral = runValueMatch numberClassifier

normalStringValidator : PVal1 q Void ()
normalStringValidator =
  value Nothing [(normalStringLiteralStrict, const ())]

validNormalStringLiteral : String -> Bool
validNormalStringLiteral = matchesLiteral normalStringValidator

basisStringValidator : PVal1 q Void ()
basisStringValidator =
  value Nothing [(basisStringLiteralStrict, const ())]

validBasisStringLiteral : String -> Bool
validBasisStringLiteral = matchesLiteral basisStringValidator

byteLiteralValidator : PVal1 q Void ()
byteLiteralValidator =
  value Nothing [(byteLiteralStrict, const ())]

validByteLiteral : String -> Bool
validByteLiteral = matchesLiteral byteLiteralValidator

byteStringValidator : PVal1 q Void ()
byteStringValidator =
  value Nothing [(byteStringLiteralStrict, const ())]

validByteStringLiteral : String -> Bool
validByteStringLiteral = matchesLiteral byteStringValidator

--------------------------------------------------------------------------------
-- Splits the dot-operator suffix matched by `digitsThenDotOperatorCandidate`
-- (one of `.`, `..`, or `..=`) off the trailing end of the matched text,
-- returning the leading digits together with the matched `Symbol`.
--------------------------------------------------------------------------------
classifyDotOperatorSuffix : List Char -> (List Char, Symbol)
classifyDotOperatorSuffix chars =
  case stripSuffixChars (unpack "..=") chars of
    Just digitChars => (digitChars, SymDotDotEq)
    Nothing =>
      case stripSuffixChars (unpack "..") chars of
        Just digitChars => (digitChars, SymDotDot)
        Nothing =>
          case stripSuffixChars (unpack ".") chars of
            Just digitChars => (digitChars, SymDot)
            Nothing => (chars, SymDot)

--------------------------------------------------------------------------------
-- Token and error actions.
--------------------------------------------------------------------------------
emitBoundedToken :
     (sk : LeafLexerStack q)
  => ByteBounded Token
  -> F1 q LeafState
emitBoundedToken boundedToken = T1.do
  st <- getStack
  putStackAs ({ outputTokens $= (:< boundedToken) } st) Initial

emitToken :
     (sk : LeafLexerStack q)
  => Token
  -> F1 q LeafState
emitToken token = T1.do
  boundedToken <- bounded' token
  emitBoundedToken boundedToken

rememberFatalError :
     (sk : LeafLexerStack q)
  => LexerError
  -> F1 q LeafState
rememberFatalError lexerError = T1.do
  existingError <- read1 (error sk)
  case existingError of
    Just _ => pure Initial
    Nothing => failHere (Custom lexerError) Initial

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

emitDigitsThenDotOperator :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitDigitsThenDotOperator rawText = T1.do
  let (digitChars, dotSymbol) = classifyDotOperatorSuffix (unpack rawText)
  let digitText = pack digitChars
  case classifyNumberLiteral digitText of
    Just IntegerNumberLiteral => T1.do
      let digitsLength = length digitChars
      tokenStart <- startPos
      tokenEnd <- endPos
      let digitsEnd = incLen digitsLength tokenStart
      _ <- emitBoundedToken (B (TokIntLitRaw digitText) (BB tokenStart digitsEnd))
      emitBoundedToken (B (TokSym dotSymbol) (BB digitsEnd tokenEnd))

    _ =>
      rememberFatalError (LexInvalidNumberLiteral digitText)

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
--
-- `blockComment` lives on `LeafStack`; doc-comment text pieces are
-- accumulated using the built-in string-literal accumulator
-- (`pushStr'`/`getStr`) instead of a hand-rolled SnocList field.
--------------------------------------------------------------------------------
appendTextIfDoc :
     (sk : LeafLexerStack q)
  => CommentMode
  -> String
  -> F1' q
appendTextIfDoc mode rawText =
  case isDocCommentMode mode of
    False => pure ()
    True => pushStr' rawText

beginBlockComment :
     (sk : LeafLexerStack q)
  => CommentMode
  -> String
  -> F1 q LeafState
beginBlockComment mode rawText = T1.do
  pushPosition
  st <- getStack
  putStack ({ commentDepth := Z, commentMode := mode } st)
  appendTextIfDoc mode rawText
  pure InBlockComment

beginNestedBlockComment :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
beginNestedBlockComment rawText = T1.do
  pushPosition
  st <- getStack
  putStack ({ commentDepth $= S } st)
  appendTextIfDoc st.commentMode rawText
  pure InBlockComment

finishOutermostBlockComment :
     (sk : LeafLexerStack q)
  => CommentMode
  -> ByteBounds
  -> F1 q LeafState
finishOutermostBlockComment mode fullCommentBounds = T1.do
  rawCommentText <- getStr
  case commentModeToToken mode rawCommentText of
    Nothing => pure Initial
    Just docToken =>
      emitBoundedToken (B docToken fullCommentBounds)

closeBlockComment :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
closeBlockComment rawText = T1.do
  st <- getStack
  appendTextIfDoc st.commentMode rawText
  case st.commentDepth of
    Z => T1.do
      fullCommentBounds <- closeBounds
      finishOutermostBlockComment st.commentMode fullCommentBounds

    S remainingDepth => T1.do
      popPosition
      putStack ({ commentDepth := remainingDepth } st)
      pure InBlockComment

consumeBlockCommentText :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
consumeBlockCommentText rawText = T1.do
  st <- getStack
  appendTextIfDoc st.commentMode rawText
  pure InBlockComment

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
      Just (string (chars symbolChars) (\_ => emitToken (TokSym symbol)))

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
initialRules : List (LeafRule q)
initialRules =
  [ string outerDocLineComment (\rawText => emitToken (TokOuterDoc rawText))
  , string innerDocLineComment (\rawText => emitToken (TokInnerDoc rawText))

  , ignore' allStarsOuterBlockComment
  , string outerBlockDocOpen (beginBlockComment OuterBlockDocComment)
  , string innerBlockDocOpen (beginBlockComment InnerBlockDocComment)
  , string bareOuterBlockCommentOpen (beginBlockComment NormalBlockComment)
  , ignore' normalLineComment
  , string normalBlockCommentOpen (beginBlockComment NormalBlockComment)
  , ignore' leafWhitespace

  , string basisStringCandidate emitBasisStringLiteral
  , string byteStringCandidate emitByteStringLiteral
  , string byteLiteralCandidate emitByteLiteral
  , string normalStringCandidate emitNormalStringLiteral

  -- Unterminated candidates come after closed-literal candidates, so a valid
  -- string wins by maximal munch. They come before identifiers so `bs"bad` is
  -- not split into `bs` and a string fragment.
  , string unterminatedBasisStringCandidate emitInvalidBasisStringLiteral
  , string unterminatedByteStringCandidate emitInvalidByteStringLiteral
  , string unterminatedByteLiteralCandidate emitInvalidByteLiteral
  , string unterminatedNormalStringCandidate emitUnterminatedStringLiteral
  , string ordinaryCharLiteralCandidate emitOrdinaryCharLiteralError

  , string numberCandidate emitNumberLiteral
  , string digitsThenDotOperatorCandidate emitDigitsThenDotOperator
  , string identifierLike (\rawText => emitToken (tokenFromIdentLike rawText))
  ] ++ symbolRules

--------------------------------------------------------------------------------
-- Block-comment rules.
--
-- Active only in `InBlockComment`. `/*` increments depth, `*/` decrements depth,
-- and only the outermost close returns to Initial.
--------------------------------------------------------------------------------
blockCommentRules : List (LeafRule q)
blockCommentRules =
  [ string normalBlockCommentOpen beginNestedBlockComment
  , string blockCommentClose closeBlockComment
  , string blockCommentBodyChunk consumeBlockCommentText
  , string blockCommentLineBreak consumeBlockCommentText
  , string blockCommentSingleStar consumeBlockCommentText
  , string blockCommentSingleSlash consumeBlockCommentText
  ]

--------------------------------------------------------------------------------
-- DFAs, error handlers, and final P1 lexer.
--------------------------------------------------------------------------------
leafLexerSteps : Lex1 q LeafSz LeafLexerStack
leafLexerSteps =
  lex1
    [ E Initial (dfa initialRules)
    , E InBlockComment (dfa blockCommentRules)
    ]

makeUnterminatedBlockCommentError :
     LeafLexerStack q
  -> F1 q (BBErr LexerError)
makeUnterminatedBlockCommentError stackValue = T1.do
  unclosedBounds <- unterminatedCommentBounds stackValue
  pure (B (Custom LexUnterminatedBlockComment) unclosedBounds)

leafLexerEOI :
     LeafState
  -> LeafLexerStack q
  -> F1 q (Either (BBErr LexerError) (List (ByteBounded Token)))
leafLexerEOI _ stackValue = T1.do
  storedError <- read1 (error stackValue)
  case storedError of
    Just existingError =>
      pure (Left existingError)

    Nothing => T1.do
      openPositions <- read1 (positions stackValue)
      st <- read1 (stack stackValue)
      case oldestOpenPosition openPositions of
        Just _ =>
          Left <$> makeUnterminatedBlockCommentError stackValue

        Nothing => T1.do
          eofPosition <- endPos
          pure (Right (st.outputTokens <>> [B TokEOF (BB eofPosition eofPosition)]))

export
leafLexer : Lexer LexerError Token
leafLexer =
  P Initial
    (init initLeafStack)
    leafLexerSteps
    noChunk
    (errs [])
    leafLexerEOI

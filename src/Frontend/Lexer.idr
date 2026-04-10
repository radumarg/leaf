module Frontend.Lexer

import Derive.Prelude
import Language.Reflection

import Frontend.AST
import Frontend.Token

-- idris2-parser manual tooling for span tracking:
-- Position, begin, next, Bounded, bounded, ...
import Text.Parse.Manual

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- LexerErr: lexical errors that can happen before parsing.
-- These errors are also Bounded so you can report line/col spans.
--------------------------------------------------------------------------------
public export
data LexerErr
  = LexUnexpectedChar Char
  | LexUnterminatedString
  | LexUnterminatedBlockComment
  | LexInvalidBitStringLiteral String
  | LexInvalidNumberLiteral String
  | LexFuelExhausted

%runElab derive "LexerErr" [Show, Eq]

--------------------------------------------------------------------------------
-- Utility: advance Position by a list of characters (for span calculation).
--------------------------------------------------------------------------------
advanceMany : Position -> List Char -> Position
advanceMany startPosition charsConsumed =
  foldl (flip next) startPosition charsConsumed

mkBoundedHere : a -> Position -> Position -> Bounded a
mkBoundedHere value startPosition endPosition =
  bounded value startPosition endPosition

--------------------------------------------------------------------------------
-- Small generic helpers.
--------------------------------------------------------------------------------

-- Fuel helper for the lexer.
-- We keep the same idea as before: enough steps if each iteration consumes
-- at least one character.
fuelForChars : List Char -> Nat
fuelForChars chars = S (length chars)

mkBoundedToken : Token -> Position -> Position -> Bounded Token
mkBoundedToken token startPosition endPosition =
  mkBoundedHere token startPosition endPosition

--------------------------------------------------------------------------------
-- Identifier character classes.
-- [A-Za-z_] start, then [A-Za-z0-9_] continue.
--------------------------------------------------------------------------------
isIdentStartChar : Char -> Bool
isIdentStartChar c = isAlpha c || c == '_'

isIdentContinueChar : Char -> Bool
isIdentContinueChar c = isAlphaNum c || c == '_'

--------------------------------------------------------------------------------
-- takeWhileList: purely functional helper for char streams
--------------------------------------------------------------------------------
takeWhileList : (a -> Bool) -> List a -> (List a, List a)
takeWhileList predicate xs =
  case xs of
    [] => ([], [])
    x :: rest =>
      if predicate x
        then
          let (taken, remaining) = takeWhileList predicate rest in
            (x :: taken, remaining)
        else
          ([], xs)

--------------------------------------------------------------------------------
-- startsWithList: checks prefix matching for multi-character operators/comments
-- startsWithList prefixChars fullChars
--------------------------------------------------------------------------------
startsWithList : List Char -> List Char -> Bool
startsWithList [] _ = True
startsWithList _ [] = False
startsWithList (p :: ps) (f :: fs) =
  if p == f then startsWithList ps fs else False

dropList : Nat -> List a -> List a
dropList Z xs = xs
dropList (S k) xs =
  case xs of
    [] => []
    _ :: rest => dropList k rest

--------------------------------------------------------------------------------
-- Identifier / symbol classification helpers.
--
-- These helpers keep the main lexing loop readable by isolating the logic for
-- "what token does this already-consumed text correspond to?".
--------------------------------------------------------------------------------

-- Order matters:
--   1) "_" => TokUnderscore
--   2) reserved keywords (let, if, ...)
--   3) reserved type keywords (int, QReg, ...)
--   4) gate keywords (H, CX, ...)
--   5) otherwise identifier
classifyIdentString : String -> Token
classifyIdentString identString =
  if identString == "_"
    then TokUnderscore
    else
      case keywordFromString identString of
        Just kw => TokKw kw
        Nothing =>
          case typeFromString identString of
            Just typPrimName => TokTypPrim typPrimName
            Nothing =>
              case gateFromString identString of
                Just gateName => TokGate gateName
                Nothing => TokIdent identString

-- Single-character symbol lookup.
-- Multi-character operators are handled separately in lexSymbol.
singleCharSymbolToken : Char -> Maybe Token
singleCharSymbolToken c =
  case c of
    '?' => Just (TokSym SymQuestion)
    '&' => Just (TokSym SymAmp)
    '(' => Just (TokSym SymLParen)
    ')' => Just (TokSym SymRParen)
    '[' => Just (TokSym SymLBracket)
    ']' => Just (TokSym SymRBracket)
    '{' => Just (TokSym SymLBrace)
    '}' => Just (TokSym SymRBrace)
    ',' => Just (TokSym SymComma)
    ';' => Just (TokSym SymSemi)
    ':' => Just (TokSym SymColon)
    '.' => Just (TokSym SymDot)
    '!' => Just (TokSym SymBang)
    '=' => Just (TokSym SymEq)
    '+' => Just (TokSym SymPlus)
    '-' => Just (TokSym SymMinus)
    '*' => Just (TokSym SymStar)
    '/' => Just (TokSym SymSlash)
    '%' => Just (TokSym SymPercent)
    '>' => Just (TokSym SymGt)
    '<' => Just (TokSym SymLt)
    '|' => Just (TokSym SymPipe)
    '^' => Just (TokSym SymCaret)
    _   => Nothing

-- Multi-character operators are matched longest-first.
multiCharSymbolCandidates : List (List Char, Symbol)
multiCharSymbolCandidates =
  [ (unpack "..=", SymDotDotEq)
  , (unpack "..",  SymDotDot)
  , (unpack "=>",  SymFatArrow)
  , (unpack "->",  SymArrow)
  , (unpack "::",  SymDoubleColon)
  , (unpack "==",  SymEqEq)
  , (unpack "!=",  SymNotEq)
  , (unpack ">=",  SymGe)
  , (unpack "<=",  SymLe)
  , (unpack "&&",  SymAndAnd)
  , (unpack "||",  SymOrOr)
  , (unpack "+=",  SymPlusEq)
  , (unpack "-=",  SymMinusEq)
  , (unpack "*=",  SymStarEq)
  , (unpack "/=",  SymSlashEq)
  , (unpack "%=",  SymPercentEq)
  , (unpack ":=",  SymWalrusEq)
  ]

--------------------------------------------------------------------------------
-- Comment skipping
--------------------------------------------------------------------------------

-- Skip // ... until newline or EOF
skipLineComment : Position -> List Char -> (Position, List Char)
skipLineComment currentPosition charsRemaining =
  case charsRemaining of
    [] => (currentPosition, [])
    c :: cs =>
      if c == '\n'
        then (advanceMany currentPosition [c], cs)
        else skipLineComment (advanceMany currentPosition [c]) cs

-- Skip /* ... */ (non-nesting; easy to extend later)
--
-- NOTE (improvement):
--   This function now expects:
--     * commentStartPosition = position of the '/' that started "/*"
--     * charsAfterStart     = characters AFTER the initial "/*"
--   This gives better spans for LexUnterminatedBlockComment.
skipBlockComment : Position -> List Char -> Either (Bounded LexerErr) (Position, List Char)
skipBlockComment commentStartPosition charsAfterStart =
  let
    -- We begin scanning *after* consuming the initial "/*"
    scanStartPosition : Position
    scanStartPosition = advanceMany commentStartPosition ['/', '*']

    go : Position -> List Char -> Either (Bounded LexerErr) (Position, List Char)
    go currentPosition cs =
      case cs of
        [] =>
          -- No silent error: unterminated block comment is a hard lexical error
          Left (mkBoundedHere LexUnterminatedBlockComment commentStartPosition currentPosition)

        -- Found the closing "*/"
        '*' :: '/' :: rest =>
          let endPosition = advanceMany currentPosition ['*', '/'] in
            Right (endPosition, rest)

        c :: rest =>
          go (advanceMany currentPosition [c]) rest
  in
    go scanStartPosition charsAfterStart

--------------------------------------------------------------------------------
-- String literal lexing: " ... "
-- Supports basic escapes: \n, \t, \r, \", \\ .
--
-- NOTE (improvement):
--   This function now expects:
--     * stringQuoteStartPosition = position of the opening quote '"'
--     * charsAfterQuote          = characters AFTER the opening quote
--   This gives better spans for LexUnterminatedString.
--------------------------------------------------------------------------------

decodeStringEscape : Char -> Char
decodeStringEscape esc =
  case esc of
    'n'  => '\n'
    't'  => '\t'
    'r'  => '\r'
    '"'  => '"'
    '\\' => '\\'
    other => other

lexStringLiteral : Position -> List Char -> Either (Bounded LexerErr) (String, Position, List Char)
lexStringLiteral stringQuoteStartPosition charsAfterQuote =
  let
    -- We begin scanning *after* consuming the opening '"'
    scanStartPosition : Position
    scanStartPosition = advanceMany stringQuoteStartPosition ['"']

    go : Position -> List Char -> List Char -> Either (Bounded LexerErr) (String, Position, List Char)
    go currentPosition accumulatorChars cs =
      case cs of
        [] =>
          Left (mkBoundedHere LexUnterminatedString stringQuoteStartPosition currentPosition)

        -- closing quote
        '"' :: rest =>
          let endPosition = advanceMany currentPosition ['"'] in
            Right (pack (reverse accumulatorChars), endPosition, rest)

        -- escape sequence
        '\\' :: esc :: rest =>
          let decodedChar = decodeStringEscape esc in
            go (advanceMany currentPosition ['\\', esc]) (decodedChar :: accumulatorChars) rest

        -- ordinary character
        c :: rest =>
          go (advanceMany currentPosition [c]) (c :: accumulatorChars) rest
  in
    go scanStartPosition [] charsAfterQuote

--------------------------------------------------------------------------------
-- Bitstring literal lexing: b"0101"
-- Accepts only characters '0' and '1' between the quotes.
--
-- This function expects:
--   * bitStringStartPosition = position of the 'b'
--   * charsAfterPrefix       = characters AFTER the prefix b"
--------------------------------------------------------------------------------
lexBitStringLiteral : Position -> List Char -> Either (Bounded LexerErr) (String, Position, List Char)
lexBitStringLiteral bitStringStartPosition charsAfterPrefix =
  let
    scanStartPosition : Position
    scanStartPosition = advanceMany bitStringStartPosition ['b', '"']

    go : Position -> List Char -> List Char -> Either (Bounded LexerErr) (String, Position, List Char)
    go currentPosition accumulatorChars cs =
      case cs of
        [] =>
          Left (mkBoundedHere LexUnterminatedString bitStringStartPosition currentPosition)

        '"' :: rest =>
          let endPosition = advanceMany currentPosition ['"'] in
            Right (pack (reverse accumulatorChars), endPosition, rest)

        '0' :: rest =>
          go (advanceMany currentPosition ['0']) ('0' :: accumulatorChars) rest

        '1' :: rest =>
          go (advanceMany currentPosition ['1']) ('1' :: accumulatorChars) rest

        c :: _ =>
          Left
            (mkBoundedHere
              (LexInvalidBitStringLiteral ("invalid bitstring character: " ++ singleton c))
              bitStringStartPosition
              (advanceMany currentPosition [c]))
  in
    go scanStartPosition [] charsAfterPrefix

--------------------------------------------------------------------------------
-- Number lexing:
--  * int:    123
--  * float:  123.45
-- Important rule for ranges:
--  * "1..6" must lex as TokIntLitRaw "1" then TokSym SymDotDot then TokIntLitRaw "6"
-- This lexer explicitly checks for ".." after digits before treating '.' as float.
--------------------------------------------------------------------------------
lexNumberLiteral : Position -> List Char -> Either (Bounded LexerErr) (Token, Position, List Char)
lexNumberLiteral startPosition charsRemaining =
  let (digitChars, restAfterDigits) = takeWhileList isDigit charsRemaining
      integerToken = TokIntLitRaw (pack digitChars)
      integerEndPosition = advanceMany startPosition digitChars in
    case restAfterDigits of
      '.' :: '.' :: _ =>
        -- Range begins: keep integer only
        Right (integerToken, integerEndPosition, restAfterDigits)

      '.' :: nextChar :: restTail =>
        if isDigit nextChar
          then
            let (fractionChars, remainingChars) = takeWhileList isDigit (nextChar :: restTail)
                fullChars = digitChars ++ ('.' :: fractionChars)
                endPosition = advanceMany startPosition fullChars in
              Right (TokFloatLitRaw (pack fullChars), endPosition, remainingChars)
          else
            -- Treat "123." as int "123" + '.' symbol later
            Right (integerToken, integerEndPosition, restAfterDigits)

      _ =>
        Right (integerToken, integerEndPosition, restAfterDigits)

--------------------------------------------------------------------------------
-- Ident / keyword / type / gate lexing
--
-- Order matters:
--   1) "_" => TokUnderscore
--   2) reserved keywords (let, if, ...)
--   3) reserved type keywords (int, QReg, ...)
--   4) gate keywords (H, CX, ...)
--   5) otherwise identifier
--------------------------------------------------------------------------------
lexIdentOrKeywordOrTypeOrGate : Position -> List Char -> (Token, Position, List Char)
lexIdentOrKeywordOrTypeOrGate startPosition charsRemaining =
  let (identChars, restChars) = takeWhileList isIdentContinueChar charsRemaining
      identString = pack identChars
      endPosition = advanceMany startPosition identChars
      token = classifyIdentString identString
  in
    (token, endPosition, restChars)

--------------------------------------------------------------------------------
-- Symbol lexing: longest-match first (..= before .., >= before >, etc.)
--------------------------------------------------------------------------------

trySymbolPattern : Position -> List Char -> (List Char, Symbol) -> Maybe (Token, Position, List Char)
trySymbolPattern startPosition charsRemaining (patternChars, sym) =
  if startsWithList patternChars charsRemaining
    then
      let endPosition = advanceMany startPosition patternChars
          remainingChars = dropList (length patternChars) charsRemaining in
        Just (TokSym sym, endPosition, remainingChars)
    else
      Nothing

trySymbolPatterns : Position -> List Char -> List (List Char, Symbol) -> Maybe (Token, Position, List Char)
trySymbolPatterns startPosition charsRemaining candidates =
  case candidates of
    [] => Nothing
    candidate :: rest =>
      case trySymbolPattern startPosition charsRemaining candidate of
        Just ok => Just ok
        Nothing => trySymbolPatterns startPosition charsRemaining rest

lexSymbol : Position -> List Char -> Either (Bounded LexerErr) (Token, Position, List Char)
lexSymbol startPosition charsRemaining =
  case trySymbolPatterns startPosition charsRemaining multiCharSymbolCandidates of
    Just ok =>
      Right ok

    Nothing =>
      case charsRemaining of
        [] =>
          -- Defensive; lexSymbol is normally called with non-empty input.
          Left (mkBoundedHere (LexUnexpectedChar '\0') startPosition startPosition)

        c :: rest =>
          case singleCharSymbolToken c of
            Just token =>
              let endPosition = advanceMany startPosition [c] in
                Right (token, endPosition, rest)

            Nothing =>
              let endPosition = advanceMany startPosition [c] in
                Left (mkBoundedHere (LexUnexpectedChar c) startPosition endPosition)

--------------------------------------------------------------------------------
-- Main entry point: lexProgram
--
-- Produces a list of tokens, each with bounds.
-- No silent failures: invalid characters or unterminated comments/strings
-- return Left (Bounded LexerErr).
--------------------------------------------------------------------------------
public export
lexProgram : String -> Either (Bounded LexerErr) (List (Bounded Token))
lexProgram inputString =
  let charsAll = unpack inputString
      fuel = fuelForChars charsAll
  in go fuel begin charsAll
where
  go : Nat -> Position -> List Char -> Either (Bounded LexerErr) (List (Bounded Token))
  go Z currentPosition _ =
    -- If we ever get here, something failed to make progress; report a hard lexer error.
    Left (mkBoundedHere LexFuelExhausted currentPosition currentPosition)

  go (S fuelLeft) currentPosition charsRemaining =
    case charsRemaining of
      [] =>
        Right []

      c :: rest =>
        -- The main loop is organized as a priority order of lexical forms.
        --
        -- The ordering matters:
        --   * comments must be recognized before '/' as a symbol
        --   * bitstring literals b"..." must be recognized before identifiers
        --   * numbers must handle the range case 1..6 specially
        if isSpace c then
          -- 1) skip whitespace
          go fuelLeft (advanceMany currentPosition [c]) rest

        else if startsWithList (unpack "//") (c :: rest) then
          -- 2) skip line comments: //
          let afterSlashesPosition = advanceMany currentPosition ['/', '/']
              afterSlashesChars = dropList 2 (c :: rest)
              (newPosition, remainingChars) =
                skipLineComment afterSlashesPosition afterSlashesChars
          in
            go fuelLeft newPosition remainingChars

        else if startsWithList (unpack "/*") (c :: rest) then
          -- 3) skip block comments: /* ... */
          let afterStartChars = dropList 2 (c :: rest) in
            case skipBlockComment currentPosition afterStartChars of
              Left err => Left err
              Right (newPosition, remainingChars) =>
                go fuelLeft newPosition remainingChars

        else if c == '"' then
          -- 4) string literal
          case lexStringLiteral currentPosition rest of
            Left err => Left err
            Right (stringValue, endPosition, remainingChars) =>
              let boundedToken =
                    mkBoundedToken (TokStringLit stringValue) currentPosition endPosition
              in
                (boundedToken ::) <$> go fuelLeft endPosition remainingChars

        else if isDigit c then
          -- 5) number literal
          case lexNumberLiteral currentPosition (c :: rest) of
            Left err => Left err
            Right (token, endPosition, remainingChars) =>
              let boundedToken = mkBoundedToken token currentPosition endPosition
              in
                (boundedToken ::) <$> go fuelLeft endPosition remainingChars

        else if c == 'b' && startsWithList ['b', '"'] (c :: rest) then
          -- 6) bitstring literal: b"..."
          let charsAfterPrefix = dropList 2 (c :: rest) in
            case lexBitStringLiteral currentPosition charsAfterPrefix of
              Left err => Left err
              Right (bitStringValue, endPosition, remainingChars) =>
                let boundedToken =
                      mkBoundedToken (TokBitStringLit bitStringValue) currentPosition endPosition
                in
                  (boundedToken ::) <$> go fuelLeft endPosition remainingChars

        else if isIdentStartChar c then
          -- 7) identifier/keyword/type/gate
          let (token, endPosition, remainingChars) =
                lexIdentOrKeywordOrTypeOrGate currentPosition (c :: rest)
              boundedToken = mkBoundedToken token currentPosition endPosition
          in
            (boundedToken ::) <$> go fuelLeft endPosition remainingChars

        else
          -- 8) symbols/operators
          case lexSymbol currentPosition (c :: rest) of
            Left err => Left err
            Right (token, endPosition, remainingChars) =>
              let boundedToken = mkBoundedToken token currentPosition endPosition
              in
                (boundedToken ::) <$> go fuelLeft endPosition remainingChars
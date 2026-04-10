module Frontend.Parser

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
-- Parser errors (kept small and explicit).
-- We wrap errors in `Bounded` so we can attach the source span of the token that
-- caused the error.
--------------------------------------------------------------------------------
public export
data ParseErr
  = ParseUnexpectedEOF
  | ParseExpected String
  | ParseFuelExhausted String

%runElab derive "ParseErr" [Show, Eq]

outOfFuelErr : List (Bounded Token) -> Bounded ParseErr
outOfFuelErr tokens =
  let err = ParseFuelExhausted "parser fuel exhausted" in
  case tokens of
    [] => bounded err begin begin
    (B _ tokBounds) :: _ => B err tokBounds

--------------------------------------------------------------------------------
-- Parser type:
--   a parser consumes a list of (Bounded Token) and either:
--     * returns an error with source span, OR
--     * returns a value + remaining tokens
--------------------------------------------------------------------------------
public export
Parser : Type -> Type
Parser a = List (Bounded Token) -> Either (Bounded ParseErr) (a, List (Bounded Token))

--------------------------------------------------------------------------------
-- Small generic helpers used throughout the parser.
--
-- These helpers are intentionally simple:
--   * they only centralize repeated token-matching boilerplate
--   * they do NOT change parser behavior
--   * they keep error locations attached to the head token
--------------------------------------------------------------------------------

-- Small fuel helpers.
-- We keep the same formulas as before, but naming them makes intent clearer.
fuelFor : List a -> Nat
fuelFor xs = 2 * length xs + 16

exprFuelFor : List a -> Nat
exprFuelFor xs = 3 * length xs + 32

-- Generic helper:
--   expectMaybe "identifier" matcher
-- tries to consume one token that matcher recognizes.
expectMaybe : String -> (Token -> Maybe a) -> Parser a
expectMaybe expectedText matcher tokens =
  case tokens of
    [] =>
      Left (bounded (ParseExpected expectedText) begin begin)

    (B tok tokenBounds :: rest) =>
      case matcher tok of
        Just value => Right (value, rest)
        Nothing => Left (B (ParseExpected expectedText) tokenBounds)

-- Generic optional version of expectMaybe.
-- It never fails. It either consumes and returns Just x, or leaves the stream untouched.
acceptMaybe : (Token -> Maybe a) -> Parser (Maybe a)
acceptMaybe matcher tokens =
  case tokens of
    [] =>
      Right (Nothing, [])

    (B tok _ :: rest) =>
      case matcher tok of
        Just value => Right (Just value, rest)
        Nothing => Right (Nothing, tokens)

--------------------------------------------------------------------------------
-- Basic token stream helpers.
--------------------------------------------------------------------------------

-- Look at the next token without consuming it.
peekToken : List (Bounded Token) -> Maybe Token
peekToken [] = Nothing
peekToken (B tok _ :: _) = Just tok

-- Consume exactly one token and return it.
popToken : Parser Token
popToken [] = Left (bounded ParseUnexpectedEOF begin begin)
popToken (B tok _ :: remainingTokens) = Right (tok, remainingTokens)

-- Produce an error located at the next token if possible; otherwise at begin..begin.
failAtHead : ParseErr -> List (Bounded Token) -> Either (Bounded ParseErr) a
failAtHead parseErr [] = Left (bounded parseErr begin begin)
failAtHead parseErr (B _ tokenBounds :: _) = Left (B parseErr tokenBounds)

--------------------------------------------------------------------------------
-- “expect” / “accept” helpers.
-- - expectX: must match, otherwise error
-- - acceptX: optionally consume if matches, otherwise do nothing
--
-- These are still exposed as dedicated helpers because the rest of the parser
-- reads much better with domain names like expectSymbol / expectKeyword than
-- with raw generic matcher code everywhere.
--------------------------------------------------------------------------------

matchSymbol : Symbol -> Token -> Maybe ()
matchSymbol expectedSymbol tok =
  case tok of
    TokSym sym =>
      if sym == expectedSymbol then Just () else Nothing
    _ => Nothing

matchKeyword : Keyword -> Token -> Maybe ()
matchKeyword expectedKeyword tok =
  case tok of
    TokKw kw =>
      if kw == expectedKeyword then Just () else Nothing
    _ => Nothing

matchIdentName : Token -> Maybe String
matchIdentName tok =
  case tok of
    TokIdent identName => Just identName
    _ => Nothing

expectSymbol : Symbol -> Parser ()
expectSymbol expectedSymbol =
  expectMaybe ("symbol " ++ show expectedSymbol) (matchSymbol expectedSymbol)

acceptSymbol : Symbol -> Parser Bool
acceptSymbol expectedSymbol tokens =
  case acceptMaybe (matchSymbol expectedSymbol) tokens of
    Left err => Left err
    Right (Nothing, remainingTokens) => Right (False, remainingTokens)
    Right (Just (), remainingTokens) => Right (True, remainingTokens)

expectKeyword : Keyword -> Parser ()
expectKeyword expectedKeyword =
  expectMaybe ("keyword " ++ show expectedKeyword) (matchKeyword expectedKeyword)

acceptKeyword : Keyword -> Parser Bool
acceptKeyword expectedKeyword tokens =
  case acceptMaybe (matchKeyword expectedKeyword) tokens of
    Left err => Left err
    Right (Nothing, remainingTokens) => Right (False, remainingTokens)
    Right (Just (), remainingTokens) => Right (True, remainingTokens)

expectIdentName : Parser String
expectIdentName =
  expectMaybe "identifier" matchIdentName

--------------------------------------------------------------------------------
-- Small number helpers.
-- We keep integers/floats as RAW STRINGS in Literal, but we occasionally need a Nat:
--   * tuple indexing: t.0, t.1
--   * array repeat count: [0; 8] uses Nat in the AST
--   * size expressions: QReg[8] uses Nat in SizeExpr
--------------------------------------------------------------------------------

digitToNat : Char -> Maybe Nat
digitToNat c =
  if isDigit c then Just (cast (ord c - ord '0')) else Nothing

digitsToNat : String -> Maybe Nat
digitsToNat rawDigits =
  let chars = unpack rawDigits in
  case chars of
    [] => Nothing
    _  =>
      foldl
        (\acc, ch =>
            case acc of
              Nothing => Nothing
              Just n =>
                case digitToNat ch of
                  Nothing => Nothing
                  Just d  => Just (n * 10 + d)
        )
        (Just 0)
        chars

--------------------------------------------------------------------------------
-- Small classification helpers for tokens that map directly into AST forms.
--
-- These helpers keep repeated "case token of ..." logic in one place, while
-- still keeping the main parsing code explicit.
--------------------------------------------------------------------------------

literalFromToken : Token -> Maybe Literal
literalFromToken tok =
  case tok of
    TokIntLitRaw rawInt => Just (LitIntRaw rawInt)
    TokFloatLitRaw rawFloat => Just (LitFloatRaw rawFloat)
    TokStringLit rawString => Just (LitString rawString)
    TokBitStringLit rawBits => Just (LitBitString rawBits)
    TokKw KwTrue => Just (LitBool True)
    TokKw KwFalse => Just (LitBool False)
    _ => Nothing

assignOpFromSymbol : Symbol -> Maybe AssignOp
assignOpFromSymbol sym =
  case sym of
    SymEq        => Just AssignEq
    SymPlusEq    => Just AssignAddEq
    SymMinusEq   => Just AssignSubEq
    SymStarEq    => Just AssignMulEq
    SymSlashEq   => Just AssignDivEq
    SymPercentEq => Just AssignRemEq
    SymWalrusEq  => Just AssignWalrusEq
    _ => Nothing

-- Builtin specification table.
--
-- This is the single source of truth for builtin/keyword correspondence.
-- The Bool flag says whether that builtin keyword should be recognized by
-- parseAtom as a builtin expression form.
--
-- We keep BuiltinImport in the table for completeness, but mark it False so we
-- preserve the current parser behavior.
builtinSpecs : List (BuiltinName, Keyword, Bool)
builtinSpecs =
  [ (BuiltinAbs,        KwAbs,        True)
  , (BuiltinAdjoint,    KwAdjoint,    True)
  , (BuiltinAcos,       KwAcos,       True)
  , (BuiltinAsin,       KwAsin,       True)
  , (BuiltinAtan,       KwAtan,       True)
  , (BuiltinBarrier,    KwBarrier,    True)
  , (BuiltinCeil,       KwCeil,       True)
  , (BuiltinCos,        KwCos,        True)
  , (BuiltinDiscard,    KwDiscard,    True)
  , (BuiltinExp,        KwExp,        True)
  , (BuiltinFloor,      KwFloor,      True)
  , (BuiltinImport,     KwImport,     False)
  , (BuiltinLn,         KwLn,         True)
  , (BuiltinLog10,      KwLog10,      True)
  , (BuiltinLog2,       KwLog2,       True)
  , (BuiltinMax,        KwMax,        True)
  , (BuiltinMeasr,      KwMeasr,      True)
  , (BuiltinMin,        KwMin,        True)
  , (BuiltinParam,      KwParam,      True)
  , (BuiltinPow,        KwPow,        True)
  , (BuiltinQAlloc,     KwQAlloc,     True)
  , (BuiltinReset,      KwReset,      True)
  , (BuiltinRound,      KwRound,      True)
  , (BuiltinSin,        KwSin,        True)
  , (BuiltinSqrt,       KwSqrt,       True)
  , (BuiltinTan,        KwTan,        True)
  , (BuiltinUncompute,  KwUncompute,  True)
  ]

lookupBuiltinKeyword : BuiltinName -> List (BuiltinName, Keyword, Bool) -> Maybe Keyword
lookupBuiltinKeyword builtinName specs =
  case specs of
    [] => Nothing
    (candidateBuiltinName, kw, _) :: rest =>
      if candidateBuiltinName == builtinName
         then Just kw
         else lookupBuiltinKeyword builtinName rest

lookupBuiltinByKeyword : Keyword -> List (BuiltinName, Keyword, Bool) -> Maybe BuiltinName
lookupBuiltinByKeyword kw specs =
  case specs of
    [] => Nothing
    (builtinName, candidateKeyword, allowedInParseAtom) :: rest =>
      if candidateKeyword == kw && allowedInParseAtom
         then Just builtinName
         else lookupBuiltinByKeyword kw rest

builtinKeyword : BuiltinName -> Keyword
builtinKeyword builtinName =
  case lookupBuiltinKeyword builtinName builtinSpecs of
    Just kw => kw
    Nothing => KwImport

validateBuiltinCallArgs : BuiltinName -> List Expr -> Maybe String
validateBuiltinCallArgs builtinName args =
  case builtinName of
    BuiltinParam =>
      case args of
        [_] => Nothing
        _ => Just "Param expects exactly one argument"

    _ => Nothing

-- Reverse mapping for parseAtom dispatch.
-- This is derived from `builtinSpecs`, so there is only one mapping to maintain.
builtinFromKeyword : Keyword -> Maybe BuiltinName
builtinFromKeyword kw =
  lookupBuiltinByKeyword kw builtinSpecs

--------------------------------------------------------------------------------
-- Comma-separated list helper until a closing symbol.
-- Supports:
--   - empty list: ()
--   - trailing comma: (a, b, )
--
-- Internally this now uses a reversed accumulator for readability and to avoid
-- repeated `++ [x]`. The external behavior is unchanged.
--------------------------------------------------------------------------------
mutual
  parseCommaSep0Until : (fuel : Nat) -> (closingSymbol : Symbol) -> Parser a -> Parser (List a)
  parseCommaSep0Until Z closingSymbol parseOne tokens =
    Left (outOfFuelErr tokens)

  parseCommaSep0Until (S fuel) closingSymbol parseOne tokens =
    case peekToken tokens of
      Just (TokSym s) =>
        if s == closingSymbol
          then Right ([], tokens)
          else parseCommaSep1Until fuel closingSymbol parseOne tokens
      _ =>
        parseCommaSep1Until fuel closingSymbol parseOne tokens

  parseCommaSep1Until : (fuel : Nat) -> (closingSymbol : Symbol) -> Parser a -> Parser (List a)
  parseCommaSep1Until Z closingSymbol parseOne tokens =
    Left (outOfFuelErr tokens)

  parseCommaSep1Until (S fuel) closingSymbol parseOne tokens =
    case parseOne tokens of
      Left err => Left err
      Right (firstValue, tokensAfterFirst) =>
        -- `revValues` stores elements in reverse order for cheap accumulation.
        go fuel [firstValue] tokensAfterFirst
    where
      go : (fuelGo : Nat)
        -> List a
        -> List (Bounded Token)
        -> Either (Bounded ParseErr) (List a, List (Bounded Token))
      go Z revValues currentTokens =
        Left (outOfFuelErr currentTokens)

      go (S fuelGo) revValues currentTokens =
        case acceptSymbol SymComma currentTokens of
          Left err => Left err

          Right (False, _) =>
            Right (reverse revValues, currentTokens)

          Right (True, tokensAfterComma) =>
            -- Allow trailing comma: if next token is closing, stop.
            case peekToken tokensAfterComma of
              Just (TokSym s) =>
                if s == closingSymbol
                  then Right (reverse revValues, tokensAfterComma)
                  else
                    case parseOne tokensAfterComma of
                      Left err => Left err
                      Right (nextValue, tokensAfterNext) =>
                        go fuelGo (nextValue :: revValues) tokensAfterNext

              _ =>
                case parseOne tokensAfterComma of
                  Left err => Left err
                  Right (nextValue, tokensAfterNext) =>
                    go fuelGo (nextValue :: revValues) tokensAfterNext

--------------------------------------------------------------------------------
-- SIZE EXPRESSIONS (for symbolic `n` in types)
--   SizeExpr ::= Nat | Ident
-- Examples:
--   QReg[8]  => SizeNat 8
--   QReg[n]  => SizeVar "n"
--------------------------------------------------------------------------------
parseSizeExpr : Parser SizeExpr
parseSizeExpr tokens =
  case tokens of
    (B (TokIntLitRaw rawDigits) rawBounds :: rest) =>
      case digitsToNat rawDigits of
        Just n  => Right (SizeNat n, rest)
        Nothing => Left (B (ParseExpected "natural number for size (digits only)") rawBounds)

    (B (TokIdent name) _ :: rest) =>
      Right (SizeVar name, rest)

    _ =>
      failAtHead (ParseExpected "size expression (Nat literal or identifier)") tokens

--------------------------------------------------------------------------------
-- TYPE PARSING
--
-- TypExpr in your AST:
--   TypUnit
--   TypPrim TypPrimName
--   TypTuple (List TypExpr)
--   TypArrayFixed TypExpr SizeExpr
--
-- We implement internal "fuel" so the parser remains total even with recursion.
--------------------------------------------------------------------------------

mutual
  -- Internal fuel for type parsing (bounded by token length).
  -- If this hits 0, it means the input is pathological or our grammar is looping.
  public export
  parseTypExpr : Parser TypExpr
  parseTypExpr tokens =
    parseTypExprFuel (fuelFor tokens) tokens

  parseTypExprFuel : Nat -> Parser TypExpr
  parseTypExprFuel Z tokens =
    failAtHead (ParseFuelExhausted "type expression") tokens

  parseTypExprFuel (S remainingFuel) tokens =
    case peekToken tokens of
      -- Parentheses: either unit type `()` or tuple type `(T1, T2, ...)`
      Just (TokSym SymLParen) =>
        case expectSymbol SymLParen tokens of
          Left err => Left err
          Right ((), tokens1) =>
            case peekToken tokens1 of
              -- Unit: ()
              Just (TokSym SymRParen) =>
                case expectSymbol SymRParen tokens1 of
                  Left err => Left err
                  Right ((), tokens2) => Right (TypUnit, tokens2)

              -- Otherwise parse first type, then decide tuple vs grouping
              _ =>
                case parseTypExprFuel remainingFuel tokens1 of
                  Left err => Left err
                  Right (firstTypExpr, tokens2) =>
                    case peekToken tokens2 of
                      -- Tuple type requires a comma
                      Just (TokSym SymComma) =>
                        case expectSymbol SymComma tokens2 of
                          Left err => Left err
                          Right ((), tokens3) =>
                            case parseCommaSep0Until remainingFuel SymRParen (parseTypExprFuel remainingFuel) tokens3 of
                              Left err => Left err
                              Right (moreTypes, tokens4) =>
                                case expectSymbol SymRParen tokens4 of
                                  Left err => Left err
                                  Right ((), tokens5) =>
                                    Right (TypTuple (firstTypExpr :: moreTypes), tokens5)

                      -- Grouping: (T) -> T
                      _ =>
                        case expectSymbol SymRParen tokens2 of
                          Left err => Left err
                          Right ((), tokens3) => Right (firstTypExpr, tokens3)

      -- Fixed array type: [T; n]
      Just (TokSym SymLBracket) =>
        parseTypArrayFixedFuel remainingFuel tokens

      -- Primitive type keyword token (already classified by lexer)
      Just (TokTypPrim typPrimName) =>
        case popToken tokens of
          Left err => Left err
          Right (_, remainingTokens) =>
            Right (TypPrim typPrimName, remainingTokens)

      _ =>
        failAtHead (ParseExpected "type expression") tokens

  --------------------------------------------------------------------------------
  -- Updated parseTypArrayFixed (as requested): [T; n] with SizeExpr
  --------------------------------------------------------------------------------
  public export
  parseTypArrayFixed : Parser TypExpr
  parseTypArrayFixed tokens =
    parseTypArrayFixedFuel (fuelFor tokens) tokens

  parseTypArrayFixedFuel : Nat -> Parser TypExpr
  parseTypArrayFixedFuel Z tokens =
    failAtHead (ParseFuelExhausted "array type") tokens

  parseTypArrayFixedFuel (S remainingFuel) tokens =
    case expectSymbol SymLBracket tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parseTypExprFuel remainingFuel tokens1 of
          Left err => Left err
          Right (elementTypeExpr, tokens2) =>
            case expectSymbol SymSemi tokens2 of
              Left err => Left err
              Right ((), tokens3) =>
                case parseSizeExpr tokens3 of
                  Left err => Left err
                  Right (lengthSizeExpr, tokens4) =>
                    case expectSymbol SymRBracket tokens4 of
                      Left err => Left err
                      Right ((), tokens5) =>
                        Right (TypArrayFixed elementTypeExpr lengthSizeExpr, tokens5)

--------------------------------------------------------------------------------
-- LITERAL PARSING
--------------------------------------------------------------------------------
parseLiteral : Parser Literal
parseLiteral tokens =
  case tokens of
    (B tok _ :: rest) =>
      case literalFromToken tok of
        Just lit =>
          Right (lit, rest)

        Nothing =>
          case tok of
            -- Unit literal: ()
            TokSym SymLParen =>
              case expectSymbol SymLParen tokens of
                Left err => Left err
                Right ((), tokens1) =>
                  case expectSymbol SymRParen tokens1 of
                    Left err => Left err
                    Right ((), tokens2) =>
                      Right (LitUnit, tokens2)

            _ =>
              failAtHead (ParseExpected "literal") tokens

    _ =>
      failAtHead (ParseExpected "literal") tokens

-- Small helper used in parseAtom when a token class is known to be a literal.
parseLiteralExpr : Parser Expr
parseLiteralExpr tokens =
  case parseLiteral tokens of
    Left err => Left err
    Right (lit, rest) => Right (ELit lit, rest)

--------------------------------------------------------------------------------
-- PATTERN PARSING (let bindings + match arms)
--------------------------------------------------------------------------------

-- Parse pattern qualifiers with two invariants:
--   * at most one `scratch`
--   * at most one linearity qualifier (`linear` or `affine`)
--
-- This helper is easy to misread because it deliberately stops parsing
-- qualifiers once it sees a duplicate or a non-qualifier token.
-- At that point it returns the qualifiers parsed so far and leaves the
-- remaining tokens untouched for the surrounding pattern parser.
--
-- Internally this uses a reverse accumulator to avoid repeated list appends.
parsePatternQualifiersAcc : Bool -> Maybe PatternQualifier -> List PatternQualifier -> Parser (List PatternQualifier)
parsePatternQualifiersAcc hasScratch maybeLinearity revQualifiers tokens =
  case tokens of
    (B (TokKw KwScratch) _ :: rest) =>
      case hasScratch of
        True => Right (reverse revQualifiers, tokens)
        False => parsePatternQualifiersAcc True maybeLinearity (PatQualScratch :: revQualifiers) rest

    (B (TokKw KwLinear) _ :: rest) =>
      case maybeLinearity of
        Nothing => parsePatternQualifiersAcc hasScratch (Just PatQualLinear) (PatQualLinear :: revQualifiers) rest
        Just _ => Right (reverse revQualifiers, tokens)

    (B (TokKw KwAffine) _ :: rest) =>
      case maybeLinearity of
        Nothing => parsePatternQualifiersAcc hasScratch (Just PatQualAffine) (PatQualAffine :: revQualifiers) rest
        Just _ => Right (reverse revQualifiers, tokens)

    _ => Right (reverse revQualifiers, tokens)

parsePatternQualifiers : Parser (List PatternQualifier)
parsePatternQualifiers tokens0 =
  parsePatternQualifiersAcc False Nothing [] tokens0

mutual
  parsePatternFuel : Nat -> Parser Pattern
  parsePatternFuel Z tokens =
    -- If you have a better error constructor, use it; this keeps things simple.
    failAtHead (ParseExpected "pattern (out of fuel)") tokens

  parsePatternFuel (S fuel) tokens =
    case peekToken tokens of
      Just (TokKw KwScratch) => parseQualifiedPatternFuel fuel tokens
      Just (TokKw KwLinear) => parseQualifiedPatternFuel fuel tokens
      Just (TokKw KwAffine) => parseQualifiedPatternFuel fuel tokens
      _ => parseBasePatternFuel fuel tokens

  parseQualifiedPatternFuel : Nat -> Parser Pattern
  parseQualifiedPatternFuel fuelLeft tokens0 =
    case parsePatternQualifiers tokens0 of
      Left err => Left err
      Right (qualifiers, tokens1) =>
        case parseBasePatternFuel fuelLeft tokens1 of
          Left err => Left err
          Right (pattern, tokens2) => Right (PatQualified qualifiers pattern, tokens2)

  parseBasePatternFuel : Nat -> Parser Pattern
  parseBasePatternFuel fuelLeft tokensBase =
    case tokensBase of
      -- Wildcard: _
      (B TokUnderscore _ :: rest) =>
        Right (PatWildcard, rest)

      -- Variable: x
      (B (TokIdent name) _ :: rest) =>
        Right (PatVarName name, rest)

      -- Literal patterns
      (B tok _ :: _) =>
        case literalFromToken tok of
          Just _ =>
            case parseLiteral tokensBase of
              Left err => Left err
              Right (lit, rest) => Right (PatLit lit, rest)

          Nothing =>
            case tok of
              -- Tuple pattern or unit: (...) / ()
              TokSym SymLParen =>
                case expectSymbol SymLParen tokensBase of
                  Left err => Left err
                  Right ((), tokens1) =>
                    case peekToken tokens1 of
                      Just (TokSym SymRParen) =>
                        case expectSymbol SymRParen tokens1 of
                          Left err => Left err
                          Right ((), tokens2) => Right (PatUnit, tokens2)

                      _ =>
                        case parsePatternFuel fuelLeft tokens1 of
                          Left err => Left err
                          Right (firstPat, tokens2) =>
                            case peekToken tokens2 of
                              Just (TokSym SymComma) =>
                                case expectSymbol SymComma tokens2 of
                                  Left err => Left err
                                  Right ((), tokens3) =>
                                    -- Fuel for the comma-separated tail list.
                                    let commaFuel : Nat = fuelFor tokens3 in
                                    case parseCommaSep0Until commaFuel SymRParen (parsePatternFuel fuelLeft) tokens3 of
                                      Left err => Left err
                                      Right (morePats, tokens4) =>
                                        case expectSymbol SymRParen tokens4 of
                                          Left err => Left err
                                          Right ((), tokens5) =>
                                            Right (PatTuple (firstPat :: morePats), tokens5)

                              _ =>
                                -- Grouping parentheses for patterns: (x) -> x
                                case expectSymbol SymRParen tokens2 of
                                  Left err => Left err
                                  Right ((), tokens3) => Right (firstPat, tokens3)

              _ =>
                failAtHead (ParseExpected "pattern") tokensBase

      _ =>
        failAtHead (ParseExpected "pattern") tokensBase

parsePattern : Parser Pattern
parsePattern tokens =
  -- Big enough to cover recursive descent over the remaining input
  let patternFuel : Nat = fuelFor tokens in
  parsePatternFuel patternFuel tokens

--------------------------------------------------------------------------------
-- OPERATOR TABLES (BinaryOp + precedence)
--------------------------------------------------------------------------------
binaryOpFromSymbol : Symbol -> Maybe BinaryOp
binaryOpFromSymbol sym =
  case sym of
    SymPlus    => Just OpAdd
    SymMinus   => Just OpSub
    SymStar    => Just OpMul
    SymSlash   => Just OpDiv
    SymPercent => Just OpRem

    SymLt      => Just OpLt
    SymLe      => Just OpLe
    SymGt      => Just OpGt
    SymGe      => Just OpGe

    SymEqEq    => Just OpEqEq
    SymNotEq   => Just OpNotEq

    SymAndAnd  => Just OpAndAnd
    SymOrOr    => Just OpOrOr

    SymPipe    => Just OpBitOr
    SymCaret   => Just OpBitXor

    SymDotDot   => Just OpRangeExcl
    SymDotDotEq => Just OpRangeIncl

    _ => Nothing

binaryPrecedence : BinaryOp -> Nat
binaryPrecedence op =
  case op of
    OpRangeExcl => 1
    OpRangeIncl => 1

    OpOrOr      => 2
    OpAndAnd    => 3

    OpBitOr     => 4
    OpBitXor    => 5

    OpEqEq      => 6
    OpNotEq     => 6

    OpLt        => 7
    OpLe        => 7
    OpGt        => 7
    OpGe        => 7

    OpAdd       => 8
    OpSub       => 8

    OpMul       => 9
    OpDiv       => 9
    OpRem       => 9

mutual
  --------------------------------------------------------------------------------
  -- EXPRESSIONS (Pratt / precedence climbing + postfix chain)
  --
  -- We keep it total by:
  --   * bounding recursion using a Nat "fuel" derived from token length
  --------------------------------------------------------------------------------
  public export
  parseExpr : Parser Expr
  parseExpr tokens =
    parseExprPrec (exprFuelFor tokens) 0 tokens

  -- Parse with minimum precedence (Pratt-style).
  parseExprPrec : Nat -> Nat -> Parser Expr
  parseExprPrec Z _ tokens =
    failAtHead (ParseFuelExhausted "expression") tokens

  parseExprPrec (S remainingFuel) minPrecedence tokens =
    case parsePrefix remainingFuel tokens of
      Left err => Left err
      Right (leftExpr, tokensAfterLeft) =>
        parseInfixLoop remainingFuel leftExpr tokensAfterLeft
    where
      parseInfixLoop : Nat -> Expr -> List (Bounded Token) -> Either (Bounded ParseErr) (Expr, List (Bounded Token))
      parseInfixLoop Z currentLeftExpr currentTokens =
        Left (bounded (ParseFuelExhausted "infix loop") begin begin)

      parseInfixLoop (S loopFuel) currentLeftExpr currentTokens =
        case currentTokens of
          (B (TokSym sym) _ :: _) =>
            case binaryOpFromSymbol sym of
              Nothing => Right (currentLeftExpr, currentTokens)
              Just op =>
                let opPrec = binaryPrecedence op in
                if opPrec < minPrecedence then
                  Right (currentLeftExpr, currentTokens)
                else
                  -- consume operator
                  case popToken currentTokens of
                    Left err => Left err
                    Right (_, tokensAfterOp) =>
                      -- parse RHS at higher precedence (right-associative tie-break)
                      case parseExprPrec loopFuel (opPrec + 1) tokensAfterOp of
                        Left err => Left err
                        Right (rightExpr, tokensAfterRhs) =>
                          parseInfixLoop loopFuel (EBinary op currentLeftExpr rightExpr) tokensAfterRhs
          _ =>
            Right (currentLeftExpr, currentTokens)

  -- Prefix forms: unary ops or atom + postfix chain
  parsePrefix : Nat -> Parser Expr
  parsePrefix Z tokens =
    failAtHead (ParseFuelExhausted "prefix expression") tokens

  parsePrefix (S remainingFuel) tokens =
    case peekToken tokens of
      Just (TokSym SymMinus) =>
        case expectSymbol SymMinus tokens of
          Left err => Left err
          Right ((), tokens1) =>
            case parsePrefix remainingFuel tokens1 of
              Left err => Left err
              Right (subExpr, tokens2) =>
                Right (EUnary UnaryNeg subExpr, tokens2)

      Just (TokSym SymBang) =>
        case expectSymbol SymBang tokens of
          Left err => Left err
          Right ((), tokens1) =>
            case parsePrefix remainingFuel tokens1 of
              Left err => Left err
              Right (subExpr, tokens2) =>
                Right (EUnary UnaryNot subExpr, tokens2)

      _ =>
        case parseAtom remainingFuel tokens of
          Left err => Left err
          Right (atomExpr, tokens1) =>
            parsePostfixChain remainingFuel atomExpr tokens1

  isCallableExpr : Expr -> Bool
  isCallableExpr (EVarName _) = True
  isCallableExpr (EField _ _) = True
  isCallableExpr (ETupleIndex _ _) = True
  isCallableExpr (ECall _ _) = True
  isCallableExpr _ = False

  -- Postfix chain: calls, indexing, field access, tuple indexing, casts
  --
  -- This function repeatedly extends an already-parsed expression on the left:
  --   f(x)[0].field as T
  --
  -- The key idea is:
  --   * parsePrefix gives us the initial expression
  --   * parsePostfixChain keeps consuming postfix continuations until none match
  parsePostfixChain : Nat -> Expr -> Parser Expr
  parsePostfixChain Z _ tokens =
    failAtHead (ParseFuelExhausted "postfix chain") tokens

  parsePostfixChain (S remainingFuel) currentExpr tokens =
    case peekToken tokens of
      -- Function call: f(...)
      Just (TokSym SymLParen) =>
        if isCallableExpr currentExpr
          then
            case expectSymbol SymLParen tokens of
              Left err => Left err
              Right ((), tokens1) =>
                case parseCommaSep0Until remainingFuel SymRParen (parseExprPrec remainingFuel 0) tokens1 of
                  Left err => Left err
                  Right (argExprs, tokens2) =>
                    case expectSymbol SymRParen tokens2 of
                      Left err => Left err
                      Right ((), tokens3) =>
                        parsePostfixChain remainingFuel (ECall currentExpr argExprs) tokens3
          else Right (currentExpr, tokens)

      -- Indexing: a[0]
      Just (TokSym SymLBracket) =>
        case expectSymbol SymLBracket tokens of
          Left err => Left err
          Right ((), tokens1) =>
            case parseExprPrec remainingFuel 0 tokens1 of
              Left err => Left err
              Right (indexExpr, tokens2) =>
                case expectSymbol SymRBracket tokens2 of
                  Left err => Left err
                  Right ((), tokens3) =>
                    parsePostfixChain remainingFuel (EIndex currentExpr indexExpr) tokens3

      -- Dot field or tuple index: a.len or t.0
      Just (TokSym SymDot) =>
        case expectSymbol SymDot tokens of
          Left err => Left err
          Right ((), tokens1) =>
            case tokens1 of
              (B (TokIdent fieldName) _ :: rest) =>
                parsePostfixChain remainingFuel (EField currentExpr fieldName) rest

              (B (TokIntLitRaw rawDigits) rawBounds :: rest) =>
                case digitsToNat rawDigits of
                  Just n  => parsePostfixChain remainingFuel (ETupleIndex currentExpr n) rest
                  Nothing => Left (B (ParseExpected "tuple index after '.' (digits)") rawBounds)

              _ =>
                failAtHead (ParseExpected "field name or tuple index after '.'") tokens1

      -- Cast: x as int
      Just (TokKw KwAs) =>
        case expectKeyword KwAs tokens of
          Left err => Left err
          Right ((), tokens1) =>
            case parseTypExpr tokens1 of
              Left err => Left err
              Right (castTypeExpr, tokens2) =>
                parsePostfixChain remainingFuel (ECastAs currentExpr castTypeExpr) tokens2

      _ =>
        Right (currentExpr, tokens)

  --------------------------------------------------------------------------------
  -- Control prefix parsing for ctrl(...) / negctrl(...)
  --------------------------------------------------------------------------------

  -- Parse one control argument inside ctrl(...):
  --   ctrl(q0)                       => ControlArgExpr (EVarName "q0")
  --   ctrl(q0 = true, q1 = false)    => ControlArgNamed ...
  --   ctrl(a[0])                     => ControlArgExpr <expr>
  --
  -- The only ambiguity here is an identifier followed by '=':
  --   that is treated as the named-polarity form.
  -- Otherwise identifiers are treated as ordinary expression arguments.
  parseControlArg : Nat -> Parser ControlArg
  parseControlArg fuel tokens =
    case tokens of
      (B (TokIdent candidateName) _ :: rest) =>
        -- Named polarity form: q0 = true/false
        case peekToken rest of
          Just (TokSym SymEq) =>
            case expectSymbol SymEq rest of
              Left err => Left err
              Right ((), tokensAfterEq) =>
                case peekToken tokensAfterEq of
                  Just (TokKw KwTrue) =>
                    case expectKeyword KwTrue tokensAfterEq of
                      Left err => Left err
                      Right ((), tokensAfterBool) =>
                        Right (ControlArgNamed (MkControlNamedArg candidateName True), tokensAfterBool)

                  Just (TokKw KwFalse) =>
                    case expectKeyword KwFalse tokensAfterEq of
                      Left err => Left err
                      Right ((), tokensAfterBool) =>
                        Right (ControlArgNamed (MkControlNamedArg candidateName False), tokensAfterBool)

                  _ =>
                    failAtHead (ParseExpected "true/false after '=' in ctrl(...)") tokensAfterEq

          _ =>
            -- Positional arg form; keep it as an Expr (variable)
            Right (ControlArgExpr (EVarName candidateName), rest)

      _ =>
        -- General expression control (ctrl(a[0]), ctrl(f(x))...)
        case parseExprPrec fuel 0 tokens of
          Left err => Left err
          Right (expr, remainingTokens) =>
            Right (ControlArgExpr expr, remainingTokens)

  parseControlArgs : Nat -> Parser (List ControlArg)
  parseControlArgs fuel tokens =
    case expectSymbol SymLParen tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parseCommaSep0Until fuel SymRParen (parseControlArg fuel) tokens1 of
          Left err => Left err
          Right (args, tokens2) =>
            case expectSymbol SymRParen tokens2 of
              Left err => Left err
              Right ((), tokens3) =>
                Right (args, tokens3)

  -- Parse zero or more prefixes:
  --   ctrl(...) negctrl(...) ctrl(...)
  --
  -- As with other list-producing helpers, we accumulate in reverse for clarity
  -- and efficiency, then reverse once at the end.
  parseControlPrefixes : Nat -> Parser (List ControlPrefix)
  parseControlPrefixes fuel tokens =
    go fuel [] tokens
    where
      go : Nat
        -> List ControlPrefix
        -> List (Bounded Token)
        -> Either (Bounded ParseErr) (List ControlPrefix, List (Bounded Token))

      go Z revPrefixes currentTokens =
        -- Out of fuel: stop (or error; choose what matches your parser style)
        Right (reverse revPrefixes, currentTokens)

      go (S fuelLeft) revPrefixes currentTokens =
        case peekToken currentTokens of
          Just (TokKw KwCtrl) =>
            case expectKeyword KwCtrl currentTokens of
              Left err => Left err
              Right ((), tokens1) =>
                case parseControlArgs fuelLeft tokens1 of
                  Left err => Left err
                  Right (args, tokens2) =>
                    go fuelLeft (PrefixCtrl args :: revPrefixes) tokens2

          Just (TokKw KwNegCtrl) =>
            case expectKeyword KwNegCtrl currentTokens of
              Left err => Left err
              Right ((), tokens1) =>
                case parseControlArgs fuelLeft tokens1 of
                  Left err => Left err
                  Right (args, tokens2) =>
                    go fuelLeft (PrefixNegCtrl args :: revPrefixes) tokens2

          _ =>
            Right (reverse revPrefixes, currentTokens)

  --------------------------------------------------------------------------------
  -- Gate application / control blocks:
  --   ctrl(q0) negctrl(q1) H(q2)
  --   ctrl(q0,q1) { ... }
  --   H(q0)      (prefix list = [])
  --------------------------------------------------------------------------------
  parseGateOrControl : Nat -> Parser Expr
  parseGateOrControl Z tokens =
    Left (outOfFuelErr tokens)

  parseGateOrControl (S fuelLeft) tokens =
    case parseControlPrefixes fuelLeft tokens of
      Left err => Left err
      Right (controlPrefixes, tokens1) =>
        case tokens1 of
          -- Gate application: TokGate <name> (args...)
          (B (TokGate gateName) _ :: restAfterGate) =>
            case expectSymbol SymLParen restAfterGate of
              Left err => Left err
              Right ((), tokensAfterLParen) =>
                case parseCommaSep0Until fuelLeft SymRParen (parseExprPrec fuelLeft 0) tokensAfterLParen of
                  Left err => Left err
                  Right (gateArgs, tokensAfterArgs) =>
                    case expectSymbol SymRParen tokensAfterArgs of
                      Left err => Left err
                      Right ((), tokensAfterRParen) =>
                        Right (EGateApply controlPrefixes gateName gateArgs, tokensAfterRParen)

          -- Control block: ctrl(...) { ... }
          (B (TokSym SymLBrace) _ :: _) =>
            case parseBlockExprFuel fuelLeft tokens1 of
              Left err => Left err
              Right (blk, tokensAfterBlock) =>
                Right (EControlBlock controlPrefixes blk, tokensAfterBlock)

          _ =>
            failAtHead (ParseExpected "gate name or '{' after ctrl(...) prefixes") tokens1

  --------------------------------------------------------------------------------
  -- BLOCKS + STATEMENTS
  --
  -- Rust-like:
  --   { stmt*; tailExpr? }
  -- - statements end in ';' (except break/return/continue can omit ';' by spec)
  -- - tail expression appears right before '}' and does NOT end in ';'
  --
  -- IMPORTANT: no silent fallback:
  --   if we can't parse a statement or a valid tail expression, we error.
  --------------------------------------------------------------------------------
  parseBlockExprFuel : Nat -> Parser BlockExpr
  parseBlockExprFuel Z tokens =
    Left (outOfFuelErr tokens)

  parseBlockExprFuel (S fuelLeft) tokens =
    case expectSymbol SymLBrace tokens of
      Left err => Left err
      Right ((), tokensAfterLBrace) =>
        case parseBlockBody fuelLeft [] tokensAfterLBrace of
          Left err => Left err
          Right (statements, tailExprMaybe, tokensBeforeRBrace) =>
            case expectSymbol SymRBrace tokensBeforeRBrace of
              Left err => Left err
              Right ((), tokensAfterRBrace) =>
                Right (MkBlockExpr statements tailExprMaybe, tokensAfterRBrace)

  parseBlockExpr : Parser BlockExpr
  parseBlockExpr tokens =
    -- Local fuel for the block: proportional to remaining tokens.
    let blockFuel : Nat = fuelFor tokens in
    parseBlockExprFuel blockFuel tokens

  -- Parse zero or more statements, then an optional tail expression, until '}'.
  --
  -- Internally, statements are accumulated in reverse order to avoid repeated
  -- `++ [stmt]`. We reverse exactly once when the block finishes.
  parseBlockBody :
      Nat
    -> List Stmt
    -> List (Bounded Token)
    -> Either (Bounded ParseErr) (List Stmt, Maybe Expr, List (Bounded Token))
  parseBlockBody Z revAccumulatedStatements tokens =
    failAtHead (ParseFuelExhausted "block body") tokens

  parseBlockBody (S remainingFuel) revAccumulatedStatements tokens =
    case peekToken tokens of
      Just (TokSym SymRBrace) =>
        Right (reverse revAccumulatedStatements, Nothing, tokens)

      _ =>
        -- Either parse a statement OR parse an expression and decide statement/tail.
        case parseStmtOrTail remainingFuel tokens of
          Left err => Left err
          Right (Left stmt, tokensAfterStmt) =>
            parseBlockBody remainingFuel (stmt :: revAccumulatedStatements) tokensAfterStmt

          Right (Right tailExpr, tokensAfterTailExpr) =>
            Right (reverse revAccumulatedStatements, Just tailExpr, tokensAfterTailExpr)

  -- Either:
  --   Left stmt  (a real statement)
  --   Right expr (the tail expression)
  --
  -- This helper is the main block-level disambiguation point.
  -- After parsing the leading expression, it decides whether we saw:
  --   * an assignment statement
  --   * an expression statement ending in ';'
  --   * the final tail expression before '}'
  parseStmtOrTail : Nat -> Parser (Either Stmt Expr)
  parseStmtOrTail fuel tokens =
    case peekToken tokens of
      Just (TokKw KwLet) =>
        case parseLetStmt fuel tokens of
          Left err => Left err
          Right (stmt, remainingTokens) => Right (Left stmt, remainingTokens)

      Just (TokKw KwBreak) =>
        case parseBreakStmt fuel tokens of
          Left err => Left err
          Right (stmt, remainingTokens) => Right (Left stmt, remainingTokens)

      Just (TokKw KwContinue) =>
        case parseContinueStmt tokens of
          Left err => Left err
          Right (stmt, remainingTokens) => Right (Left stmt, remainingTokens)

      Just (TokKw KwReturn) =>
        case parseReturnStmt fuel tokens of
          Left err => Left err
          Right (stmt, remainingTokens) => Right (Left stmt, remainingTokens)

      _ =>
        -- Parse an expression then decide:
        --   - assignment statement?  (lhs = rhs;)
        --   - expression statement?  (expr;)
        --   - tail expression?       (expr right before '}')
        case parseExprPrec fuel 0 tokens of
          Left err => Left err
          Right (lhsExpr, tokensAfterExpr) =>
            case peekToken tokensAfterExpr of
              Just (TokSym sym) =>
                case assignOpFromSymbol sym of
                  -- Assignment statements
                  Just assignOp =>
                    parseAssignStmt fuel assignOp lhsExpr tokensAfterExpr

                  Nothing =>
                    case sym of
                      -- Expression statement: expr ;
                      SymSemi =>
                        case expectSymbol SymSemi tokensAfterExpr of
                          Left err => Left err
                          Right ((), tokensAfterSemi) =>
                            Right (Left (StmtExpr lhsExpr True), tokensAfterSemi)

                      -- Tail expression: expr }
                      SymRBrace =>
                        Right (Right lhsExpr, tokensAfterExpr)

                      _ =>
                        failAtHead (ParseExpected "assignment op, ';', or '}' after expression") tokensAfterExpr

              _ =>
                failAtHead (ParseExpected "assignment op, ';', or '}' after expression") tokensAfterExpr

  parseAssignStmt : Nat -> AssignOp -> Expr -> List (Bounded Token) -> Either (Bounded ParseErr) (Either Stmt Expr, List (Bounded Token))
  parseAssignStmt fuel assignOp lhsExpr tokensStartingAtAssign =
    -- Consume the assignment operator token (we already know which one it is)
    case popToken tokensStartingAtAssign of
      Left err => Left err
      Right (_, tokensAfterAssignOp) =>
        case parseExprPrec fuel 0 tokensAfterAssignOp of
          Left err => Left err
          Right (rhsExpr, tokensAfterRhs) =>
            case expectSymbol SymSemi tokensAfterRhs of
              Left err => Left err
              Right ((), tokensAfterSemi) =>
                Right (Left (StmtAssign lhsExpr assignOp rhsExpr), tokensAfterSemi)

  --------------------------------------------------------------------------------
  -- Specific statements
  --------------------------------------------------------------------------------

  -- let pattern [: type]? = expr ;
  parseLetStmt : Nat -> Parser Stmt
  parseLetStmt fuel tokens =
    case expectKeyword KwLet tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parsePattern tokens1 of
          Left err => Left err
          Right (bindingPattern, tokens2) =>
            -- Optional strict typing annotation: : TypExpr
            case acceptSymbol SymColon tokens2 of
              Left err => Left err
              Right (False, tokensNoColon) =>
                parseLetAfterAnnotation fuel bindingPattern Nothing tokensNoColon
              Right (True, tokensAfterColon) =>
                case parseTypExpr tokensAfterColon of
                  Left err => Left err
                  Right (typeAnnotation, tokensAfterType) =>
                    parseLetAfterAnnotation fuel bindingPattern (Just typeAnnotation) tokensAfterType
    where
      parseLetAfterAnnotation : Nat -> Pattern -> Maybe TypExpr -> List (Bounded Token) -> Either (Bounded ParseErr) (Stmt, List (Bounded Token))
      parseLetAfterAnnotation localFuel pat maybeTypeExpr tokensA =
        case expectSymbol SymEq tokensA of
          Left err => Left err
          Right ((), tokensB) =>
            case parseExprPrec localFuel 0 tokensB of
              Left err => Left err
              Right (valueExpr, tokensC) =>
                case expectSymbol SymSemi tokensC of
                  Left err => Left err
                  Right ((), tokensD) =>
                    Right (StmtLet pat maybeTypeExpr valueExpr, tokensD)

  -- break [expr]? [;]?
  parseBreakStmt : Nat -> Parser Stmt
  parseBreakStmt fuel tokens =
    case expectKeyword KwBreak tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case peekToken tokens1 of
          Just (TokSym SymSemi) =>
            case expectSymbol SymSemi tokens1 of
              Left err => Left err
              Right ((), tokens2) => Right (StmtBreak Nothing, tokens2)

          Just (TokSym SymRBrace) =>
            Right (StmtBreak Nothing, tokens1)

          _ =>
            case parseExprPrec fuel 0 tokens1 of
              Left err => Left err
              Right (breakValueExpr, tokens2) =>
                case acceptSymbol SymSemi tokens2 of
                  Left err => Left err
                  Right (_, tokens3) =>
                    Right (StmtBreak (Just breakValueExpr), tokens3)

  -- continue [;]?
  parseContinueStmt : Parser Stmt
  parseContinueStmt tokens =
    case expectKeyword KwContinue tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case acceptSymbol SymSemi tokens1 of
          Left err => Left err
          Right (_, tokens2) =>
            Right (StmtContinue, tokens2)

  -- return [expr]? [;]?
  parseReturnStmt : Nat -> Parser Stmt
  parseReturnStmt fuel tokens =
    case expectKeyword KwReturn tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case peekToken tokens1 of
          Just (TokSym SymSemi) =>
            case expectSymbol SymSemi tokens1 of
              Left err => Left err
              Right ((), tokens2) => Right (StmtReturn Nothing, tokens2)

          Just (TokSym SymRBrace) =>
            Right (StmtReturn Nothing, tokens1)

          _ =>
            case parseExprPrec fuel 0 tokens1 of
              Left err => Left err
              Right (returnValueExpr, tokens2) =>
                case acceptSymbol SymSemi tokens2 of
                  Left err => Left err
                  Right (_, tokens3) =>
                    Right (StmtReturn (Just returnValueExpr), tokens3)

  --------------------------------------------------------------------------------
  -- A parser for qmatch
  --------------------------------------------------------------------------------
  parseQMatchLabel : Parser QMatchLabel
  parseQMatchLabel tokens =
    case tokens of
      (B (TokIntLitRaw rawDigits) rawBounds :: rest) =>
        case digitsToNat rawDigits of
          Just n  => Right (QMatchLabelNat n, rest)
          Nothing => Left (B (ParseExpected "qmatch decimal label") rawBounds)

      (B (TokBitStringLit bits) _ :: rest) =>
        Right (QMatchLabelBitString bits, rest)

      _ =>
        failAtHead (ParseExpected "qmatch label (decimal integer or bitstring literal)") tokens

  parseQMatchArm : Nat -> Parser QMatchArm
  parseQMatchArm Z tokens =
    failAtHead (ParseFuelExhausted "qmatch arm") tokens

  parseQMatchArm (S fuelLeft) tokens =
    case parseQMatchLabel tokens of
      Left err => Left err
      Right (label, tokens1) =>
        case expectSymbol SymFatArrow tokens1 of
          Left err => Left err
          Right ((), tokens2) =>
            case parseBlockExprFuel fuelLeft tokens2 of
              Left err => Left err
              Right (bodyBlk, tokens3) =>
                Right (MkQMatchArm label bodyBlk, tokens3)

  parseQMatchArms : Nat -> Parser (List QMatchArm)
  parseQMatchArms Z tokens =
    failAtHead (ParseFuelExhausted "qmatch arms") tokens

  parseQMatchArms (S fuelLeft) tokens =
    case peekToken tokens of
      Just (TokSym SymRBrace) =>
        Right ([], tokens)

      _ =>
        case parseQMatchArm fuelLeft tokens of
          Left err => Left err
          Right (arm, tokens1) =>
            case parseQMatchArms fuelLeft tokens1 of
              Left err => Left err
              Right (moreArms, tokens2) =>
                Right (arm :: moreArms, tokens2)

  --------------------------------------------------------------------------------
  -- Helpers used by parseAtom.
  --
  -- These are separated out so that parseAtom itself becomes a readable dispatcher
  -- rather than a very large nested case tree.
  --------------------------------------------------------------------------------

  -- match scrutinee { pat => expr, ... }
  parseMatchArm : Nat -> Parser MatchArm
  parseMatchArm fuelLeft armTokens =
    case parsePattern armTokens of
      Left err => Left err
      Right (armPattern, tokensA) =>
        case expectSymbol SymFatArrow tokensA of
          Left err => Left err
          Right ((), tokensB) =>
            case parseExprPrec fuelLeft 0 tokensB of
              Left err => Left err
              Right (armBodyExpr, tokensC) =>
                Right (MkMatchArm armPattern armBodyExpr, tokensC)

  -- Blocks: { ... }
  parseBlockExprAtom : Nat -> Parser Expr
  parseBlockExprAtom fuelLeft tokens =
    case parseBlockExprFuel fuelLeft tokens of
      Left err => Left err
      Right (blk, rest) => Right (EBlock blk, rest)

  -- if cond { ... } else expr?
  parseIfExprAtom : Nat -> Parser Expr
  parseIfExprAtom fuelLeft tokens =
    case expectKeyword KwIf tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parseExprPrec fuelLeft 0 tokens1 of
          Left err => Left err
          Right (condExpr, tokens2) =>
            case parseBlockExprFuel fuelLeft tokens2 of
              Left err => Left err
              Right (thenBlk, tokens3) =>
                case acceptKeyword KwElse tokens3 of
                  Left err => Left err
                  Right (False, tokensNoElse) =>
                    Right (EIf condExpr thenBlk Nothing, tokensNoElse)
                  Right (True, tokensAfterElse) =>
                    case parseExprPrec fuelLeft 0 tokensAfterElse of
                      Left err => Left err
                      Right (elseExpr, tokensAfterElseExpr) =>
                        Right (EIf condExpr thenBlk (Just elseExpr), tokensAfterElseExpr)

  -- qif cond { ... } qelse expr?
  parseQIfExprAtom : Nat -> Parser Expr
  parseQIfExprAtom fuelLeft tokens =
    case expectKeyword KwQif tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parseExprPrec fuelLeft 0 tokens1 of
          Left err => Left err
          Right (condExpr, tokens2) =>
            case parseBlockExprFuel fuelLeft tokens2 of
              Left err => Left err
              Right (qthenBlk, tokens3) =>
                case acceptKeyword KwQelse tokens3 of
                  Left err => Left err
                  Right (False, tokensNoQElse) =>
                    Right (EQIf condExpr qthenBlk Nothing, tokensNoQElse)
                  Right (True, tokensAfterQElse) =>
                    case parseExprPrec fuelLeft 0 tokensAfterQElse of
                      Left err => Left err
                      Right (qelseExpr, tokensAfterQElseExpr) =>
                        Right (EQIf condExpr qthenBlk (Just qelseExpr), tokensAfterQElseExpr)

  -- loop { ... }
  parseLoopAtom : Nat -> Parser Expr
  parseLoopAtom fuelLeft tokens =
    case expectKeyword KwLoop tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parseBlockExprFuel fuelLeft tokens1 of
          Left err => Left err
          Right (blk, rest) =>
            Right (ELoop blk, rest)

  -- while cond { ... }
  parseWhileAtom : Nat -> Parser Expr
  parseWhileAtom fuelLeft tokens =
    case expectKeyword KwWhile tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parseExprPrec fuelLeft 0 tokens1 of
          Left err => Left err
          Right (condExpr, tokens2) =>
            case parseBlockExprFuel fuelLeft tokens2 of
              Left err => Left err
              Right (blk, rest) =>
                Right (EWhile condExpr blk, rest)

  -- for pat in iterable { ... }
  parseForAtom : Nat -> Parser Expr
  parseForAtom fuelLeft tokens =
    case expectKeyword KwFor tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parsePattern tokens1 of
          Left err => Left err
          Right (loopPattern, tokens2) =>
            case expectKeyword KwIn tokens2 of
              Left err => Left err
              Right ((), tokens3) =>
                case parseExprPrec fuelLeft 0 tokens3 of
                  Left err => Left err
                  Right (iterExpr, tokens4) =>
                    case parseBlockExprFuel fuelLeft tokens4 of
                      Left err => Left err
                      Right (blk, rest) =>
                        Right (EFor loopPattern iterExpr blk, rest)

  -- match scrutinee { pat => expr, ... }
  parseMatchAtom : Nat -> Parser Expr
  parseMatchAtom fuelLeft tokens =
    case expectKeyword KwMatch tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parseExprPrec fuelLeft 0 tokens1 of
          Left err => Left err
          Right (scrutineeExpr, tokens2) =>
            case expectSymbol SymLBrace tokens2 of
              Left err => Left err
              Right ((), tokens3) =>
                case parseCommaSep0Until fuelLeft SymRBrace (parseMatchArm fuelLeft) tokens3 of
                  Left err => Left err
                  Right (arms, tokens4) =>
                    case expectSymbol SymRBrace tokens4 of
                      Left err => Left err
                      Right ((), tokens5) =>
                        Right (EMatch scrutineeExpr arms, tokens5)

  -- qmatch scrutinee { 0 => { ... } b"01" => { ... } }
  parseQMatchAtom : Nat -> Parser Expr
  parseQMatchAtom fuelLeft tokens =
    case expectKeyword KwQmatch tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case parseExprPrec fuelLeft 0 tokens1 of
          Left err => Left err
          Right (scrutineeExpr, tokens2) =>
            case expectSymbol SymLBrace tokens2 of
              Left err => Left err
              Right ((), tokens3) =>
                case parseQMatchArms fuelLeft tokens3 of
                  Left err => Left err
                  Right (arms, tokens4) =>
                    case expectSymbol SymRBrace tokens4 of
                      Left err => Left err
                      Right ((), tokens5) =>
                        Right (EQMatch scrutineeExpr arms, tokens5)

  -- Parentheses:
  --   ()          => unit literal
  --   (x)         => grouping
  --   (x, y, z)   => tuple
  parseParenAtom : Nat -> Parser Expr
  parseParenAtom fuelLeft tokens =
    case expectSymbol SymLParen tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case peekToken tokens1 of
          Just (TokSym SymRParen) =>
            case expectSymbol SymRParen tokens1 of
              Left err => Left err
              Right ((), tokens2) =>
                Right (ELit LitUnit, tokens2)

          _ =>
            case parseExprPrec fuelLeft 0 tokens1 of
              Left err => Left err
              Right (firstExpr, tokens2) =>
                case peekToken tokens2 of
                  Just (TokSym SymComma) =>
                    case expectSymbol SymComma tokens2 of
                      Left err => Left err
                      Right ((), tokens3) =>
                        case parseCommaSep0Until fuelLeft SymRParen (parseExprPrec fuelLeft 0) tokens3 of
                          Left err => Left err
                          Right (moreExprs, tokens4) =>
                            case expectSymbol SymRParen tokens4 of
                              Left err => Left err
                              Right ((), tokens5) =>
                                Right (ETuple (firstExpr :: moreExprs), tokens5)

                  _ =>
                    case expectSymbol SymRParen tokens2 of
                      Left err => Left err
                      Right ((), tokens3) =>
                        Right (firstExpr, tokens3)

  -- Array:
  --   [e1, e2, e3] => EArrayLiteral
  --   [e; 8]       => EArrayRepeat (Expr) (Nat)
  parseArrayAtom : Nat -> Parser Expr
  parseArrayAtom fuelLeft tokens =
    case expectSymbol SymLBracket tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case peekToken tokens1 of
          Just (TokSym SymRBracket) =>
            case expectSymbol SymRBracket tokens1 of
              Left err => Left err
              Right ((), tokens2) =>
                Right (EArrayLiteral [], tokens2)

          _ =>
            case parseExprPrec fuelLeft 0 tokens1 of
              Left err => Left err
              Right (firstExpr, tokens2) =>
                case peekToken tokens2 of
                  Just (TokSym SymSemi) =>
                    -- Repeat form: [expr; Nat]
                    case expectSymbol SymSemi tokens2 of
                      Left err => Left err
                      Right ((), tokens3) =>
                        case tokens3 of
                          (B (TokIntLitRaw rawDigits) rawBounds :: tokens4) =>
                            case digitsToNat rawDigits of
                              Nothing => Left (B (ParseExpected "natural number after ';' in [x; n]") rawBounds)
                              Just n  =>
                                case expectSymbol SymRBracket tokens4 of
                                  Left err => Left err
                                  Right ((), tokens5) =>
                                    Right (EArrayRepeat firstExpr n, tokens5)
                          _ =>
                            failAtHead (ParseExpected "Nat literal after ';' in [x; n]") tokens3

                  Just (TokSym SymComma) =>
                    -- Literal list form: [e1, e2, ...]
                    case expectSymbol SymComma tokens2 of
                      Left err => Left err
                      Right ((), tokens3) =>
                        case parseCommaSep0Until fuelLeft SymRBracket (parseExprPrec fuelLeft 0) tokens3 of
                          Left err => Left err
                          Right (moreExprs, tokens4) =>
                            case expectSymbol SymRBracket tokens4 of
                              Left err => Left err
                              Right ((), tokens5) =>
                                Right (EArrayLiteral (firstExpr :: moreExprs), tokens5)

                  Just (TokSym SymRBracket) =>
                    case expectSymbol SymRBracket tokens2 of
                      Left err => Left err
                      Right ((), tokens3) =>
                        Right (EArrayLiteral [firstExpr], tokens3)

                  _ =>
                    failAtHead (ParseExpected "',' or ']' after array element") tokens2

  -- Identifier: variable or macro call name!(...)
  parseIdentOrMacroAtom : Nat -> Parser Expr
  parseIdentOrMacroAtom fuelLeft tokens =
    case tokens of
      (B (TokIdent name) _ :: _) =>
        case popToken tokens of
          Left err => Left err
          Right (_, tokensAfterIdent) =>
            case peekToken tokensAfterIdent of
              Just (TokSym SymBang) =>
                -- Macro call: ident ! ( args )
                case expectSymbol SymBang tokensAfterIdent of
                  Left err => Left err
                  Right ((), tokens1) =>
                    case expectSymbol SymLParen tokens1 of
                      Left err => Left err
                      Right ((), tokens2) =>
                        case parseCommaSep0Until fuelLeft SymRParen (parseExprPrec fuelLeft 0) tokens2 of
                          Left err => Left err
                          Right (macroArgs, tokens3) =>
                            case expectSymbol SymRParen tokens3 of
                              Left err => Left err
                              Right ((), tokens4) =>
                                Right (EMacroCall name macroArgs, tokens4)

              _ =>
                Right (EVarName name, tokensAfterIdent)

      _ =>
        failAtHead (ParseExpected "expression atom") tokens

  --------------------------------------------------------------------------------
  -- ATOMS (Expr forms that do not start with infix operators)
  --------------------------------------------------------------------------------
  parseAtom : Nat -> Parser Expr
  parseAtom Z tokens =
    Left (outOfFuelErr tokens)

  parseAtom (S fuelLeft) tokens =
    case peekToken tokens of
      -- Blocks: { ... }
      Just (TokSym SymLBrace) =>
        parseBlockExprAtom fuelLeft tokens

      -- if cond { ... } else expr?
      Just (TokKw KwIf) =>
        parseIfExprAtom fuelLeft tokens

      -- qif cond { ... } qelse expr?
      Just (TokKw KwQif) =>
        parseQIfExprAtom fuelLeft tokens

      -- loop { ... }
      Just (TokKw KwLoop) =>
        parseLoopAtom fuelLeft tokens

      -- while cond { ... }
      Just (TokKw KwWhile) =>
        parseWhileAtom fuelLeft tokens

      -- for pat in iterable { ... }
      Just (TokKw KwFor) =>
        parseForAtom fuelLeft tokens

      -- match scrutinee { pat => expr, ... }
      Just (TokKw KwMatch) =>
        parseMatchAtom fuelLeft tokens

      -- qmatch scrutinee { 0 => { ... } b"01" => { ... } }
      Just (TokKw KwQmatch) =>
        parseQMatchAtom fuelLeft tokens

      -- Builtins (keywords)
      Just (TokKw kw) =>
        case builtinFromKeyword kw of
          Just builtinName =>
            parseBuiltinCall fuelLeft builtinName tokens

          Nothing =>
            case kw of
              -- Gate / control pipeline:
              --   ctrl(...) negctrl(...) H(q)
              --   H(q)
              KwCtrl =>
                parseGateOrControl fuelLeft tokens

              KwNegCtrl =>
                parseGateOrControl fuelLeft tokens

              -- Literals
              KwTrue =>
                parseLiteralExpr tokens

              KwFalse =>
                parseLiteralExpr tokens

              _ =>
                failAtHead (ParseExpected "expression atom") tokens

      Just (TokGate _) =>
        parseGateOrControl fuelLeft tokens

      Just (TokSym SymLParen) =>
        parseParenAtom fuelLeft tokens

      Just (TokSym SymLBracket) =>
        parseArrayAtom fuelLeft tokens

      Just (TokIdent _) =>
        parseIdentOrMacroAtom fuelLeft tokens

      Just (TokIntLitRaw _) =>
        parseLiteralExpr tokens

      Just (TokFloatLitRaw _) =>
        parseLiteralExpr tokens

      Just (TokStringLit _) =>
        parseLiteralExpr tokens

      Just (TokBitStringLit _) =>
        parseLiteralExpr tokens

      _ =>
        failAtHead (ParseExpected "expression atom") tokens

  -- Builtin calls:
  --   math helpers (abs, acos, asin, atan, ceil, cos, exp, floor, ln, log2, log10, pow, round, sin, sqrt, tan)
  --   quantum helpers (qalloc, measr, reset, adjoint, discard, uncompute)
  --   misc helpers (max, min)
  --   qalloc() / qalloc(8) / qalloc   (we allow optional parens)
  parseBuiltinCall : Nat -> BuiltinName -> Parser Expr
  parseBuiltinCall Z builtinName tokens =
    failAtHead (ParseExpected "builtin call (out of fuel)") tokens

  parseBuiltinCall (S fuelLeft) builtinName tokens =
    case expectKeyword (builtinKeyword builtinName) tokens of
      Left err => Left err
      Right ((), tokens1) =>
        case acceptSymbol SymLParen tokens1 of
          Left err => Left err

          Right (False, tokensNoParen) =>
            -- Only allow the no-paren form for qalloc (optional convenience)
            case builtinName of
              BuiltinQAlloc => Right (EBuiltinCall builtinName [], tokensNoParen)
              _ => failAtHead (ParseExpected "builtin call must have parentheses") tokensNoParen

          Right (True, tokensAfterLParen) =>
            case parseCommaSep0Until fuelLeft SymRParen (parseExprPrec fuelLeft 0) tokensAfterLParen of
              Left err => Left err
              Right (args, tokensAfterArgs) =>
                case validateBuiltinCallArgs builtinName args of
                  Just expectedText =>
                    failAtHead (ParseExpected expectedText) tokensAfterArgs

                  Nothing =>
                    case expectSymbol SymRParen tokensAfterArgs of
                      Left err => Left err
                      Right ((), tokensAfterRParen) =>
                        Right (EBuiltinCall builtinName args, tokensAfterRParen)

--------------------------------------------------------------------------------
-- TOP-LEVEL: function declarations + program parsing
--------------------------------------------------------------------------------
parseFnParam : Parser FnParam
parseFnParam tokens =
  case expectIdentName tokens of
    Left err => Left err
    Right (paramName, tokens1) =>
      case expectSymbol SymColon tokens1 of
        Left err => Left err
        Right ((), tokens2) =>
          case parseTypExpr tokens2 of
            Left err => Left err
            Right (paramType, tokens3) =>
              Right (MkFnParam paramName paramType, tokens3)

parseFnDecl : Parser FnDecl
parseFnDecl tokens =
  case expectKeyword KwFn tokens of
    Left err => Left err
    Right ((), tokens1) =>
      case expectIdentName tokens1 of
        Left err => Left err
        Right (fnName, tokens2) =>
          case expectSymbol SymLParen tokens2 of
            Left err => Left err
            Right ((), tokens3) =>
              let fnFuel : Nat = fuelFor tokens3 in
              case parseCommaSep0Until fnFuel SymRParen parseFnParam tokens3 of
                Left err => Left err
                Right (params, tokens4) =>
                  case expectSymbol SymRParen tokens4 of
                    Left err => Left err
                    Right ((), tokens5) =>
                      -- Optional return type: -> TypExpr
                      case acceptSymbol SymArrow tokens5 of
                        Left err => Left err
                        Right (False, tokensNoArrow) =>
                          case parseBlockExpr tokensNoArrow of
                            Left err => Left err
                            Right (bodyBlk, tokensAfterBody) =>
                              Right (MkFnDecl fnName params Nothing bodyBlk, tokensAfterBody)

                        Right (True, tokensAfterArrow) =>
                          case parseTypExpr tokensAfterArrow of
                            Left err => Left err
                            Right (returnType, tokensAfterType) =>
                              case parseBlockExpr tokensAfterType of
                                Left err => Left err
                                Right (bodyBlk, tokensAfterBody) =>
                                  Right (MkFnDecl fnName params (Just returnType) bodyBlk, tokensAfterBody)

-- Top-level statements: must end with ';' (no tail expression at top-level).
parseTopStmt : Parser Stmt
parseTopStmt tokens =
  let topFuel : Nat = exprFuelFor tokens in
  case peekToken tokens of
    Just (TokKw KwLet) =>
      parseLetStmt topFuel tokens

    Just (TokKw KwBreak) =>
      parseBreakStmt topFuel tokens

    Just (TokKw KwContinue) =>
      parseContinueStmt tokens

    Just (TokKw KwReturn) =>
      parseReturnStmt topFuel tokens

    _ =>
      case parseExprPrec topFuel 0 tokens of
        Left err => Left err
        Right (expr, tokensAfterExpr) =>
          case peekToken tokensAfterExpr of
            Just (TokSym sym) =>
              case assignOpFromSymbol sym of
                Just assignOp =>
                  finishTopAssign topFuel assignOp expr tokensAfterExpr

                Nothing =>
                  case sym of
                    SymSemi =>
                      case expectSymbol SymSemi tokensAfterExpr of
                        Left err => Left err
                        Right ((), remainingTokens) =>
                          Right (StmtExpr expr True, remainingTokens)

                    _ =>
                      failAtHead (ParseExpected "top-level statement must end with ';' or be an assignment") tokensAfterExpr

            _ =>
              failAtHead (ParseExpected "top-level statement must end with ';' or be an assignment") tokensAfterExpr
  where
    finishTopAssign : Nat -> AssignOp -> Expr -> List (Bounded Token) -> Either (Bounded ParseErr) (Stmt, List (Bounded Token))
    finishTopAssign fuel assignOp lhsExpr tokensStartingAtAssign =
      case popToken tokensStartingAtAssign of
        Left err => Left err
        Right (_, tokensAfterAssignOp) =>
          case parseExprPrec fuel 0 tokensAfterAssignOp of
            Left err => Left err
            Right (rhsExpr, tokensAfterRhs) =>
              case expectSymbol SymSemi tokensAfterRhs of
                Left err => Left err
                Right ((), tokensAfterSemi) =>
                  Right (StmtAssign lhsExpr assignOp rhsExpr, tokensAfterSemi)

parseItem : Parser Item
parseItem tokens =
  case peekToken tokens of
    Just (TokKw KwFn) =>
      case parseFnDecl tokens of
        Left err => Left err
        Right (fnDecl, remainingTokens) =>
          Right (ItemFnDecl fnDecl, remainingTokens)
    _ =>
      case parseTopStmt tokens of
        Left err => Left err
        Right (stmt, remainingTokens) =>
          Right (ItemStmt stmt, remainingTokens)

public export
parseProgram : Parser Program
parseProgram tokens =
  let programFuel : Nat = fuelFor tokens in
  go programFuel [] tokens
  where
    go : Nat -> List Item -> List (Bounded Token) -> Either (Bounded ParseErr) (Program, List (Bounded Token))
    go Z _ currentTokens =
      failAtHead (ParseFuelExhausted "program") currentTokens

    go (S remainingFuel) revAccumulatedItems currentTokens =
      case peekToken currentTokens of
        Nothing =>
          Right (MkProgram (reverse revAccumulatedItems), currentTokens)

        _ =>
          case parseItem currentTokens of
            Left err => Left err
            Right (item, tokensAfterItem) =>
              go remainingFuel (item :: revAccumulatedItems) tokensAfterItem

-- Convenience: require full consumption.
public export
parseProgramAll : List (Bounded Token) -> Either (Bounded ParseErr) Program
parseProgramAll tokens =
  case parseProgram tokens of
    Left err => Left err
    Right (program, remainingTokens) =>
      case remainingTokens of
        [] => Right program
        _  => failAtHead (ParseExpected "end of input") remainingTokens
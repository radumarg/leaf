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
  | ParseUnexpectedToken Token
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
--------------------------------------------------------------------------------

expectSymbol : Symbol -> Parser ()
expectSymbol expectedSymbol tokens =
  case tokens of
    (B (TokSym sym) symBounds :: rest) =>
      if sym == expectedSymbol
         then Right ((), rest)
         else Left (B (ParseExpected ("symbol " ++ show expectedSymbol)) symBounds)
    _ => failAtHead (ParseExpected ("symbol " ++ show expectedSymbol)) tokens

acceptSymbol : Symbol -> Parser Bool
acceptSymbol expectedSymbol tokens =
  case tokens of
    (B (TokSym sym) _ :: rest) =>
      if sym == expectedSymbol
         then Right (True, rest)
         else Right (False, tokens)
    _ => Right (False, tokens)

expectKeyword : Keyword -> Parser ()
expectKeyword expectedKeyword tokens =
  case tokens of
    (B (TokKw kw) kwBounds :: rest) =>
      if kw == expectedKeyword
         then Right ((), rest)
         else Left (B (ParseExpected ("keyword " ++ show expectedKeyword)) kwBounds)
    _ => failAtHead (ParseExpected ("keyword " ++ show expectedKeyword)) tokens

acceptKeyword : Keyword -> Parser Bool
acceptKeyword expectedKeyword tokens =
  case tokens of
    (B (TokKw kw) _ :: rest) =>
      if kw == expectedKeyword
         then Right (True, rest)
         else Right (False, tokens)
    _ => Right (False, tokens)

expectIdentName : Parser String
expectIdentName tokens =
  case tokens of
    (B (TokIdent identName) _ :: rest) => Right (identName, rest)
    _ => failAtHead (ParseExpected "identifier") tokens

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
-- Comma-separated list helper until a closing symbol.
-- Supports:
--   - empty list: ()
--   - trailing comma: (a, b, )
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
      Right (firstValue, tokensAfterFirst) => go fuel [firstValue] tokensAfterFirst
    where
      go : (fuelGo : Nat)
        -> List a
        -> List (Bounded Token)
        -> Either (Bounded ParseErr) (List a, List (Bounded Token))
      go Z accumulatedValues currentTokens =
        Left (outOfFuelErr currentTokens)

      go (S fuelGo) accumulatedValues currentTokens =
        case peekToken currentTokens of
          Just (TokSym SymComma) =>
            case expectSymbol SymComma currentTokens of
              Left err => Left err
              Right ((), tokensAfterComma) =>
                -- Allow trailing comma: if next token is closing, stop.
                case peekToken tokensAfterComma of
                  Just (TokSym s) =>
                    if s == closingSymbol
                      then Right (accumulatedValues, tokensAfterComma)
                      else
                        case parseOne tokensAfterComma of
                          Left err => Left err
                          Right (nextValue, tokensAfterNext) =>
                            go fuelGo (accumulatedValues ++ [nextValue]) tokensAfterNext
                  _ =>
                    case parseOne tokensAfterComma of
                      Left err => Left err
                      Right (nextValue, tokensAfterNext) =>
                        go fuelGo (accumulatedValues ++ [nextValue]) tokensAfterNext
          _ =>
            Right (accumulatedValues, currentTokens)

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
    let typFuel : Nat = 2 * length tokens + 16 in
    parseTypExprFuel typFuel tokens

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
    let typFuel : Nat = 2 * length tokens + 16 in
    parseTypArrayFixedFuel typFuel tokens

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
    (B (TokIntLitRaw rawInt) _ :: rest) =>
      Right (LitIntRaw rawInt, rest)

    (B (TokFloatLitRaw rawFloat) _ :: rest) =>
      Right (LitFloatRaw rawFloat, rest)

    (B (TokStringLit rawString) _ :: rest) =>
      Right (LitString rawString, rest)

    (B (TokBitStringLit rawBits) _ :: rest) =>
      Right (LitBitString rawBits, rest)

    (B (TokKw KwTrue) _ :: rest) =>
      Right (LitBool True, rest)

    (B (TokKw KwFalse) _ :: rest) =>
      Right (LitBool False, rest)

    -- Unit literal: ()
    (B (TokSym SymLParen) _ :: _) =>
      case expectSymbol SymLParen tokens of
        Left err => Left err
        Right ((), tokens1) =>
          case expectSymbol SymRParen tokens1 of
            Left err => Left err
            Right ((), tokens2) =>
              Right (LitUnit, tokens2)

    _ =>
      failAtHead (ParseExpected "literal") tokens

--------------------------------------------------------------------------------
-- PATTERN PARSING (let bindings + match arms)
--------------------------------------------------------------------------------
parsePatternFuel : Nat -> Parser Pattern
parsePatternFuel Z tokens =
  -- If you have a better error constructor, use it; this keeps things simple.
  failAtHead (ParseExpected "pattern (out of fuel)") tokens

parsePatternFuel (S fuel) tokens =
  case tokens of
    -- Wildcard: _
    (B TokUnderscore _ :: rest) =>
      Right (PatWildcard, rest)

    -- Variable: x
    (B (TokIdent name) _ :: rest) =>
      Right (PatVarName name, rest)

    -- Literal patterns
    (B (TokIntLitRaw _) _ :: _) =>
      case parseLiteral tokens of
        Left err => Left err
        Right (lit, rest) => Right (PatLit lit, rest)

    (B (TokFloatLitRaw _) _ :: _) =>
      case parseLiteral tokens of
        Left err => Left err
        Right (lit, rest) => Right (PatLit lit, rest)

    (B (TokStringLit _) _ :: _) =>
      case parseLiteral tokens of
        Left err => Left err
        Right (lit, rest) => Right (PatLit lit, rest)

    (B (TokBitStringLit _) _ :: _) =>
      case parseLiteral tokens of
        Left err => Left err
        Right (lit, rest) => Right (PatLit lit, rest)

    (B (TokKw KwTrue) _ :: _) =>
      case parseLiteral tokens of
        Left err => Left err
        Right (lit, rest) => Right (PatLit lit, rest)

    (B (TokKw KwFalse) _ :: _) =>
      case parseLiteral tokens of
        Left err => Left err
        Right (lit, rest) => Right (PatLit lit, rest)

    -- Tuple pattern or unit: (...) / ()
    (B (TokSym SymLParen) _ :: _) =>
      case expectSymbol SymLParen tokens of
        Left err => Left err
        Right ((), tokens1) =>
          case peekToken tokens1 of
            Just (TokSym SymRParen) =>
              case expectSymbol SymRParen tokens1 of
                Left err => Left err
                Right ((), tokens2) => Right (PatUnit, tokens2)

            _ =>
              case parsePatternFuel fuel tokens1 of
                Left err => Left err
                Right (firstPat, tokens2) =>
                  case peekToken tokens2 of
                    Just (TokSym SymComma) =>
                      case expectSymbol SymComma tokens2 of
                        Left err => Left err
                        Right ((), tokens3) =>
                          -- Fuel for the comma-separated tail list.
                          let commaFuel : Nat = 2 * length tokens3 + 16 in
                          case parseCommaSep0Until commaFuel SymRParen (parsePatternFuel fuel) tokens3 of
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
      failAtHead (ParseExpected "pattern") tokens

parsePattern : Parser Pattern
parsePattern tokens =
  -- Big enough to cover recursive descent over the remaining input
  let patternFuel : Nat = 2 * length tokens + 16 in
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
    let exprFuel : Nat = 3 * length tokens + 32 in
    parseExprPrec exprFuel 0 tokens

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
  parseControlPrefixes : Nat -> Parser (List ControlPrefix)
  parseControlPrefixes fuel tokens =
    go fuel [] tokens
    where
      go : Nat
        -> List ControlPrefix
        -> List (Bounded Token)
        -> Either (Bounded ParseErr) (List ControlPrefix, List (Bounded Token))

      go Z accumulatedPrefixes currentTokens =
        -- Out of fuel: stop (or error; choose what matches your parser style)
        Right (accumulatedPrefixes, currentTokens)

      go (S fuelLeft) accumulatedPrefixes currentTokens =
        case peekToken currentTokens of
          Just (TokKw KwCtrl) =>
            case expectKeyword KwCtrl currentTokens of
              Left err => Left err
              Right ((), tokens1) =>
                case parseControlArgs fuelLeft tokens1 of
                  Left err => Left err
                  Right (args, tokens2) =>
                    go fuelLeft (accumulatedPrefixes ++ [PrefixCtrl args]) tokens2

          Just (TokKw KwNegCtrl) =>
            case expectKeyword KwNegCtrl currentTokens of
              Left err => Left err
              Right ((), tokens1) =>
                case parseControlArgs fuelLeft tokens1 of
                  Left err => Left err
                  Right (args, tokens2) =>
                    go fuelLeft (accumulatedPrefixes ++ [PrefixNegCtrl args]) tokens2

          _ =>
            Right (accumulatedPrefixes, currentTokens)

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
    let blockFuel : Nat = 2 * length tokens + 16 in
    parseBlockExprFuel blockFuel tokens

  -- Parse zero or more statements, then an optional tail expression, until '}'.
  parseBlockBody :
      Nat
    -> List Stmt
    -> List (Bounded Token)
    -> Either (Bounded ParseErr) (List Stmt, Maybe Expr, List (Bounded Token))
  parseBlockBody Z accumulatedStatements tokens =
    failAtHead (ParseFuelExhausted "block body") tokens

  parseBlockBody (S remainingFuel) accumulatedStatements tokens =
    case peekToken tokens of
      Just (TokSym SymRBrace) =>
        Right (accumulatedStatements, Nothing, tokens)

      _ =>
        -- Either parse a statement OR parse an expression and decide statement/tail.
        case parseStmtOrTail remainingFuel tokens of
          Left err => Left err
          Right (Left stmt, tokensAfterStmt) =>
            parseBlockBody remainingFuel (accumulatedStatements ++ [stmt]) tokensAfterStmt

          Right (Right tailExpr, tokensAfterTailExpr) =>
            Right (accumulatedStatements, Just tailExpr, tokensAfterTailExpr)

  -- Either:
  --   Left stmt  (a real statement)
  --   Right expr (the tail expression)
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
              -- Assignment statements
              Just (TokSym SymEq)       => parseAssignStmt fuel AssignEq lhsExpr tokensAfterExpr
              Just (TokSym SymPlusEq)   => parseAssignStmt fuel AssignAddEq lhsExpr tokensAfterExpr
              Just (TokSym SymMinusEq)  => parseAssignStmt fuel AssignSubEq lhsExpr tokensAfterExpr
              Just (TokSym SymStarEq)   => parseAssignStmt fuel AssignMulEq lhsExpr tokensAfterExpr
              Just (TokSym SymSlashEq)  => parseAssignStmt fuel AssignDivEq lhsExpr tokensAfterExpr
              Just (TokSym SymPercentEq)=> parseAssignStmt fuel AssignRemEq lhsExpr tokensAfterExpr
              Just (TokSym SymWalrusEq) => parseAssignStmt fuel AssignWalrusEq lhsExpr tokensAfterExpr

              -- Expression statement: expr ;
              Just (TokSym SymSemi) =>
                case expectSymbol SymSemi tokensAfterExpr of
                  Left err => Left err
                  Right ((), tokensAfterSemi) =>
                    Right (Left (StmtExpr lhsExpr True), tokensAfterSemi)

              -- Tail expression: expr }
              Just (TokSym SymRBrace) =>
                Right (Right lhsExpr, tokensAfterExpr)

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
  -- ATOMS (Expr forms that do not start with infix operators)
  --------------------------------------------------------------------------------
  parseAtom : Nat -> Parser Expr
  parseAtom Z tokens =
    Left (outOfFuelErr tokens)

  parseAtom (S fuelLeft) tokens =
    case peekToken tokens of
      -- Blocks: { ... }
      Just (TokSym SymLBrace) =>
        case parseBlockExprFuel fuelLeft tokens of
          Left err => Left err
          Right (blk, rest) => Right (EBlock blk, rest)

      -- if cond { ... } else expr?
      Just (TokKw KwIf) =>
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
      Just (TokKw KwQif) =>
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
      Just (TokKw KwLoop) =>
        case expectKeyword KwLoop tokens of
          Left err => Left err
          Right ((), tokens1) =>
            case parseBlockExprFuel fuelLeft tokens1 of
              Left err => Left err
              Right (blk, rest) =>
                Right (ELoop blk, rest)

      -- while cond { ... }
      Just (TokKw KwWhile) =>
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
      Just (TokKw KwFor) =>
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
      Just (TokKw KwMatch) =>
        case expectKeyword KwMatch tokens of
          Left err => Left err
          Right ((), tokens1) =>
            case parseExprPrec fuelLeft 0 tokens1 of
              Left err => Left err
              Right (scrutineeExpr, tokens2) =>
                case expectSymbol SymLBrace tokens2 of
                  Left err => Left err
                  Right ((), tokens3) =>
                    let parseMatchArm : Parser MatchArm
                        parseMatchArm armTokens =
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
                    in
                    case parseCommaSep0Until fuelLeft SymRBrace parseMatchArm tokens3 of
                      Left err => Left err
                      Right (arms, tokens4) =>
                        case expectSymbol SymRBrace tokens4 of
                          Left err => Left err
                          Right ((), tokens5) =>
                            Right (EMatch scrutineeExpr arms, tokens5)

      -- qmatch scrutinee { 0 => { ... } b"01" => { ... } }
      Just (TokKw KwQmatch) =>
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

      -- Builtins (keywords)
      Just (TokKw KwAbs) =>
        parseBuiltinCall fuelLeft BuiltinAbs tokens

      Just (TokKw KwAdjoint) =>
        parseBuiltinCall fuelLeft BuiltinAdjoint tokens

      Just (TokKw KwAcos) =>
        parseBuiltinCall fuelLeft BuiltinAcos tokens

      Just (TokKw KwAsin) =>
        parseBuiltinCall fuelLeft BuiltinAsin tokens

      Just (TokKw KwAtan) =>
        parseBuiltinCall fuelLeft BuiltinAtan tokens

      Just (TokKw KwBarrier) =>
        parseBuiltinCall fuelLeft BuiltinBarrier tokens

      Just (TokKw KwCeil) =>
        parseBuiltinCall fuelLeft BuiltinCeil tokens

      Just (TokKw KwCos) =>
        parseBuiltinCall fuelLeft BuiltinCos tokens

      Just (TokKw KwDiscard) =>
        parseBuiltinCall fuelLeft BuiltinDiscard tokens

      Just (TokKw KwExp) =>
        parseBuiltinCall fuelLeft BuiltinExp tokens

      Just (TokKw KwFloor) =>
        parseBuiltinCall fuelLeft BuiltinFloor tokens

      Just (TokKw KwLn) =>
        parseBuiltinCall fuelLeft BuiltinLn tokens

      Just (TokKw KwLog10) =>
        parseBuiltinCall fuelLeft BuiltinLog10 tokens

      Just (TokKw KwLog2) =>
        parseBuiltinCall fuelLeft BuiltinLog2 tokens

      Just (TokKw KwMax) =>
        parseBuiltinCall fuelLeft BuiltinMax tokens

      Just (TokKw KwMeasr) =>
        parseBuiltinCall fuelLeft BuiltinMeasr tokens

      Just (TokKw KwMin) =>
        parseBuiltinCall fuelLeft BuiltinMin tokens

      Just (TokKw KwParam) =>
        parseBuiltinCall fuelLeft BuiltinParam tokens

      Just (TokKw KwPow) =>
        parseBuiltinCall fuelLeft BuiltinPow tokens

      Just (TokKw KwQAlloc) =>
        parseBuiltinCall fuelLeft BuiltinQAlloc tokens

      Just (TokKw KwReset) =>
        parseBuiltinCall fuelLeft BuiltinReset tokens

      Just (TokKw KwRound) =>
        parseBuiltinCall fuelLeft BuiltinRound tokens

      Just (TokKw KwSin) =>
        parseBuiltinCall fuelLeft BuiltinSin tokens

      Just (TokKw KwSqrt) =>
        parseBuiltinCall fuelLeft BuiltinSqrt tokens

      Just (TokKw KwTan) =>
        parseBuiltinCall fuelLeft BuiltinTan tokens

      Just (TokKw KwUncompute) =>
        parseBuiltinCall fuelLeft BuiltinUncompute tokens

      -- Gate / control pipeline:
      --   ctrl(...) negctrl(...) H(q)
      --   H(q)
      Just (TokKw KwCtrl) =>
        parseGateOrControl fuelLeft tokens

      Just (TokKw KwNegCtrl) =>
        parseGateOrControl fuelLeft tokens

      Just (TokGate _) =>
        parseGateOrControl fuelLeft tokens

      -- Parentheses:
      --   ()          => unit literal
      --   (x)         => grouping
      --   (x, y, z)   => tuple
      Just (TokSym SymLParen) =>
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
      Just (TokSym SymLBracket) =>
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
      Just (TokIdent name) =>
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

      -- Literals
      Just (TokIntLitRaw _) =>
        case parseLiteral tokens of
          Left err => Left err
          Right (lit, rest) => Right (ELit lit, rest)

      Just (TokFloatLitRaw _) =>
        case parseLiteral tokens of
          Left err => Left err
          Right (lit, rest) => Right (ELit lit, rest)

      Just (TokStringLit _) =>
        case parseLiteral tokens of
          Left err => Left err
          Right (lit, rest) => Right (ELit lit, rest)

      Just (TokKw KwTrue) =>
        case parseLiteral tokens of
          Left err => Left err
          Right (lit, rest) => Right (ELit lit, rest)

      Just (TokKw KwFalse) =>
        case parseLiteral tokens of
          Left err => Left err
          Right (lit, rest) => Right (ELit lit, rest)

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
    let expectedKw : Keyword =
          case builtinName of
          BuiltinAbs      => KwAbs
          BuiltinAdjoint  => KwAdjoint
          BuiltinAcos     => KwAcos
          BuiltinAsin     => KwAsin
          BuiltinAtan     => KwAtan
          BuiltinBarrier  => KwBarrier
          BuiltinCeil     => KwCeil
          BuiltinCos      => KwCos
          BuiltinDiscard  => KwDiscard
          BuiltinExp      => KwExp
          BuiltinFloor    => KwFloor
          BuiltinImport   => KwImport
          BuiltinLn       => KwLn
          BuiltinLog10    => KwLog10
          BuiltinLog2     => KwLog2
          BuiltinMax      => KwMax
          BuiltinMeasr   => KwMeasr
          BuiltinMin     => KwMin
          BuiltinParam   => KwParam
          BuiltinPow     => KwPow
          BuiltinQAlloc  => KwQAlloc
          BuiltinReset   => KwReset
          BuiltinRound   => KwRound
          BuiltinSin     => KwSin
          BuiltinSqrt    => KwSqrt
          BuiltinTan     => KwTan
          BuiltinUncompute => KwUncompute
    in
    case expectKeyword expectedKw tokens of
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
              let fnFuel : Nat = 2 * length tokens3 + 16 in
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
  let topFuel : Nat = 3 * length tokens + 32 in
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
            Just (TokSym SymEq)        => finishTopAssign topFuel AssignEq expr tokensAfterExpr
            Just (TokSym SymPlusEq)    => finishTopAssign topFuel AssignAddEq expr tokensAfterExpr
            Just (TokSym SymMinusEq)   => finishTopAssign topFuel AssignSubEq expr tokensAfterExpr
            Just (TokSym SymStarEq)    => finishTopAssign topFuel AssignMulEq expr tokensAfterExpr
            Just (TokSym SymSlashEq)   => finishTopAssign topFuel AssignDivEq expr tokensAfterExpr
            Just (TokSym SymPercentEq) => finishTopAssign topFuel AssignRemEq expr tokensAfterExpr
            Just (TokSym SymWalrusEq)  => finishTopAssign topFuel AssignWalrusEq expr tokensAfterExpr

            Just (TokSym SymSemi) =>
              case expectSymbol SymSemi tokensAfterExpr of
                Left err => Left err
                Right ((), remainingTokens) =>
                  Right (StmtExpr expr True, remainingTokens)

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
  let programFuel : Nat = 2 * length tokens + 16 in
  go programFuel [] tokens
  where
    go : Nat -> List Item -> List (Bounded Token) -> Either (Bounded ParseErr) (Program, List (Bounded Token))
    go Z _ currentTokens =
      failAtHead (ParseFuelExhausted "program") currentTokens

    go (S remainingFuel) accumulatedItems currentTokens =
      case peekToken currentTokens of
        Nothing =>
          Right (MkProgram accumulatedItems, currentTokens)

        _ =>
          case parseItem currentTokens of
            Left err => Left err
            Right (item, tokensAfterItem) =>
              go remainingFuel (accumulatedItems ++ [item]) tokensAfterItem

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

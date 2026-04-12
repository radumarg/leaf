module Frontend.Token

import Derive.Prelude
import Language.Reflection

import Frontend.AST

%default total
%language ElabReflection

----------------------------------------------------------------------
-- Token is the output of lexing.
-- The lexer converts raw source text into a list of (Bounded Token).
-- "Bounded" comes from idris2-parser and attaches source span info.
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Keywords: reserved words that affect syntax.
----------------------------------------------------------------------
public export
data Keyword
  = KwAbs | KwAdjoint | KwAffine | KwAs | KwAcos | KwAsin | KwAtan
  | KwBarrier |KwBreak | KwCeil | KwClassical | KwCos | KwCtrl | KwContinue | KwDiscard
  | KwElse | KwExp | KwFalse | KwFloor | KwFn | KwFor | KwGeneral | KwIf | KwImport | KwIn
  | KwLet | KwLn | KwLinear | KwLog10 | KwLog2 | KwLoop
  | KwMatch | KwMax | KwMeasr | KwMin | KwNegCtrl
  | KwParam | KwPow | KwRound | KwQAlloc | KwQelse | KwQif | KwQmatch | KwReset | KwReturn
  | KwScratch | KwSin | KwSqrt | KwTan | KwTrue | KwUncompute | KwUnitary | KwWeaken | KwWhile

----------------------------------------------------------------------
-- Symbols: punctuation and operators.
----------------------------------------------------------------------
public export
data Symbol
  = SymQuestion             -- ?
  | SymAmp                  -- &   (reserved; && exists too)
  | SymLParen | SymRParen   -- ( )
  | SymLBracket | SymRBracket -- [ ]
  | SymLBrace | SymRBrace   -- { }
  | SymComma | SymSemi | SymColon
  | SymDot                  -- .
  | SymBang                 -- !
  | SymEq                   -- =
  | SymPlus | SymMinus | SymStar | SymSlash | SymPercent
  | SymPlusEq | SymMinusEq | SymStarEq | SymSlashEq | SymPercentEq | SymWalrusEq
  | SymGt | SymGe | SymLt | SymLe
  | SymEqEq | SymNotEq
  | SymAndAnd | SymOrOr
  | SymDotDot | SymDotDotEq -- .. and ..=
  | SymDoubleColon          -- ::
  | SymPipe | SymCaret      -- | and ^
  | SymArrow                -- ->
  | SymFatArrow             -- =>  (match arm separator)

----------------------------------------------------------------------
-- Token:
--   TokIdent "x"
--   TokIntLitRaw "123"
--   TokFloatLitRaw "3.14"
--   TokStringLit "Hello"
--   TokKw KwLet
--   TokTypPrim TypPrimI32
--   TokGate GateH
--   TokSym SymPlusEq
--   TokUnderscore
----------------------------------------------------------------------
public export
data Token
  = TokIdent        String
  | TokIntLitRaw    String
  | TokFloatLitRaw  String
  | TokBitStringLit String
  | TokStringLit    String
  | TokKw           Keyword
  | TokTypPrim      TypPrimName
  | TokGate         GateName
  | TokSym          Symbol
  | TokUnderscore

----------------------------------------------------------------------
-- Mappings used by lexer: identifier text -> token category
----------------------------------------------------------------------
public export
keywordFromString : String -> Maybe Keyword
keywordFromString s =
  case s of
    "affine"    => Just KwAffine
    "abs"       => Just KwAbs
    "adjoint"   => Just KwAdjoint
    "acos"      => Just KwAcos
    "asin"      => Just KwAsin
    "as"        => Just KwAs
    "atan"      => Just KwAtan
    "barrier"   => Just KwBarrier
    "break"     => Just KwBreak
    "ceil"      => Just KwCeil
    "classical" => Just KwClassical
    "cos"       => Just KwCos
    "ctrl"      => Just KwCtrl
    "continue"  => Just KwContinue
    "discard"   => Just KwDiscard
    "else"      => Just KwElse
    "exp"       => Just KwExp
    "false"     => Just KwFalse
    "floor"     => Just KwFloor
    "fn"        => Just KwFn
    "for"       => Just KwFor
    "general"   => Just KwGeneral
    "if"        => Just KwIf
    "import"    => Just KwImport
    "in"        => Just KwIn
    "let"       => Just KwLet
    "linear"    => Just KwLinear
    "ln"        => Just KwLn
    "log10"     => Just KwLog10
    "log2"      => Just KwLog2
    "loop"      => Just KwLoop
    "match"     => Just KwMatch
    "max"       => Just KwMax
    "measr"     => Just KwMeasr
    "min"       => Just KwMin
    "negctrl"   => Just KwNegCtrl
    "Param"     => Just KwParam
    "pow"       => Just KwPow
    "qalloc"    => Just KwQAlloc
    "qelse"     => Just KwQelse
    "qif"       => Just KwQif
    "qmatch"    => Just KwQmatch
    "round"     => Just KwRound
    "reset"     => Just KwReset
    "return"    => Just KwReturn
    "sin"       => Just KwSin
    "sqrt"      => Just KwSqrt
    "scratch"   => Just KwScratch
    "tan"       => Just KwTan
    "true"      => Just KwTrue
    "uncompute" => Just KwUncompute
    "unitary"   => Just KwUnitary
    "weaken"    => Just KwWeaken
    "while"     => Just KwWhile
    _           => Nothing

public export
gateFromString : String -> Maybe GateName
gateFromString s =
  case s of
    "Id"    => Just GateId
    "X"     => Just GateX
    "Y"     => Just GateY
    "Z"     => Just GateZ
    "H"     => Just GateH
    "S"     => Just GateS
    "SDG"   => Just GateSDG
    "T"     => Just GateT
    "TDG"   => Just GateTDG
    "SX"    => Just GateSX
    "SXDG"  => Just GateSXDG
    "RX"    => Just GateRX
    "RY"    => Just GateRY
    "RZ"    => Just GateRZ
    "U1"    => Just GateU1
    "U2"    => Just GateU2
    "U3"    => Just GateU3
    "CNOT"  => Just GateCNOT
    "CX"    => Just GateCX
    "CY"    => Just GateCY
    "CZ"    => Just GateCZ
    "CS"    => Just GateCS
    "CSDG"  => Just GateCSDG
    "CT"    => Just GateCT
    "CTDG"  => Just GateCTDG
    "CSX"   => Just GateCSX
    "CSXDG" => Just GateCSXDG
    "CRX"   => Just GateCRX
    "CRY"   => Just GateCRY
    "CRZ"   => Just GateCRZ
    "CU1"   => Just GateCU1
    "CU2"   => Just GateCU2
    "CU3"   => Just GateCU3
    "SWAP"  => Just GateSWAP
    "RXX"   => Just GateRXX
    "RYY"   => Just GateRYY
    "RZZ"   => Just GateRZZ
    "CCX"   => Just GateCCX
    "CSWAP" => Just GateCSWAP
    "GPI"   => Just GateGPI
    "GPI2"  => Just GateGPI2
    "MS"    => Just GateMS
    _       => Nothing

public export
typeFromString : String -> Maybe TypPrimName
typeFromString s =
  case s of
    "angle32" => Just TypPrimAngle32
    "angle64" => Just TypPrimAngle64
    "bit"     => Just TypPrimBit
    "bool"    => Just TypPrimBool
    "f32"     => Just TypPrimF32
    "f64"     => Just TypPrimF64
    "i8"      => Just TypPrimI8
    "i16"     => Just TypPrimI16
    "i32"     => Just TypPrimI32
    "i64"     => Just TypPrimI64
    "i128"    => Just TypPrimI128
    "param"   => Just TypPrimParam
    "u8"      => Just TypPrimU8
    "u16"     => Just TypPrimU16
    "u32"     => Just TypPrimU32
    "u64"     => Just TypPrimU64
    "u128"    => Just TypPrimU128
    "qubit"   => Just TypPrimQubit
    _         => Nothing

----------------------------------------------------------------------
-- Implementation and Derivations for debugging/testing
----------------------------------------------------------------------

public export
showKeywordLeaf : Keyword -> String
showKeywordLeaf kw =
  case kw of
    KwAbs       => "abs"
    KwAdjoint   => "adjoint"
    KwAffine    => "affine"
    KwAs        => "as"
    KwAcos      => "acos"
    KwAsin      => "asin"
    KwAtan      => "atan"
    KwBarrier   => "barrier"
    KwBreak     => "break"
    KwCeil      => "ceil"
    KwClassical => "classical"
    KwCos       => "cos"
    KwCtrl      => "ctrl"
    KwContinue  => "continue"
    KwDiscard   => "discard"
    KwElse      => "else"
    KwExp       => "exp"
    KwFalse     => "false"
    KwFloor     => "floor"
    KwFn        => "fn"
    KwFor       => "for"
    KwGeneral   => "general"
    KwIf        => "if"
    KwImport    => "import"
    KwIn        => "in"
    KwLet       => "let"
    KwLn        => "ln"
    KwLinear    => "linear"
    KwLog10     => "log10"
    KwLog2      => "log2"
    KwLoop      => "loop"
    KwMatch     => "match"
    KwMax       => "max"
    KwMeasr     => "measr"
    KwMin       => "min"
    KwNegCtrl   => "negctrl"
    KwParam     => "Param"
    KwPow       => "pow"
    KwRound     => "round"
    KwQAlloc    => "qalloc"
    KwQelse     => "qelse"
    KwQif       => "qif"
    KwQmatch    => "qmatch"
    KwReset     => "reset"
    KwReturn    => "return"
    KwScratch   => "scratch"
    KwSin       => "sin"
    KwSqrt      => "sqrt"
    KwTan       => "tan"
    KwTrue      => "true"
    KwUncompute => "uncompute"
    KwUnitary   => "unitary"
    KwWeaken    => "weaken"
    KwWhile     => "while"

public export
showSymbolLeaf : Symbol -> String
showSymbolLeaf sym =
  case sym of
    SymQuestion    => "?"
    SymAmp         => "&"
    SymLParen      => "("
    SymRParen      => ")"
    SymLBracket    => "["
    SymRBracket    => "]"
    SymLBrace      => "{"
    SymRBrace      => "}"
    SymComma       => ","
    SymSemi        => ";"
    SymColon       => ":"
    SymDot         => "."
    SymBang        => "!"
    SymEq          => "="
    SymPlus        => "+"
    SymMinus       => "-"
    SymStar        => "*"
    SymSlash       => "/"
    SymPercent     => "%"
    SymPlusEq      => "+="
    SymMinusEq     => "-="
    SymStarEq      => "*="
    SymSlashEq     => "/="
    SymPercentEq   => "%="
    SymWalrusEq    => ":="
    SymGt          => ">"
    SymGe          => ">="
    SymLt          => "<"
    SymLe          => "<="
    SymEqEq        => "=="
    SymNotEq       => "!="
    SymAndAnd      => "&&"
    SymOrOr        => "||"
    SymDotDot      => ".."
    SymDotDotEq    => "..="
    SymDoubleColon => "::"
    SymPipe        => "|"
    SymCaret       => "^"
    SymArrow       => "->"
    SymFatArrow    => "=>"

public export
implementation Show Keyword where
  show = showKeywordLeaf

public export
implementation Show Symbol where
  show = showSymbolLeaf

%runElab derive "Keyword" [Eq]
%runElab derive "Symbol" [Eq]
%runElab derive "Token" [Show, Eq]
-----------------------------------------------------------------------
--- Token is the output of lexing.
-----------------------------------------------------------------------

module Frontend.Token

import Derive.Prelude
import Language.Reflection

%default total
%language ElabReflection

------------------------------------------------------
-- Primitive built-in types
------------------------------------------------------
public export
data TypPrimName
  = TypPrimAngle32
  | TypPrimAngle64
  | TypPrimBit
  | TypPrimBool
  | TypPrimF32
  | TypPrimF64
  | TypPrimI8
  | TypPrimI16
  | TypPrimI32
  | TypPrimI64
  | TypPrimI128
  | TypPrimParam
  | TypPrimU8
  | TypPrimU16
  | TypPrimU32
  | TypPrimU64
  | TypPrimU128
  | TypPrimQubit
  | TypePrimSQubit
  | TypPrimString
  | TypPrimStrSlice

------------------------------------------------------
-- GateName: enumerates the built-in quantum gates.
------------------------------------------------------
public export
data GateName
  = GateId | GateX | GateY | GateZ | GateH
  | GateS | GateSDG | GateT | GateTDG
  | GateSX | GateSXDG
  | GateRX | GateRY | GateRZ
  | GateU1 | GateU2 | GateU3
  | GateCNOT | GateCX
  | GateCY | GateCZ | GateCS | GateCSDG | GateCT | GateCTDG
  | GateCSX | GateCSXDG
  | GateCRX | GateCRY | GateCRZ
  | GateCU1 | GateCU2 | GateCU3
  | GateSWAP
  | GateRXX | GateRYY | GateRZZ
  | GateCCX | GateCSWAP
  | GateGPI | GateGPI2 | GateMS | GateZZ

----------------------------------------------------------
-- StateBasisName: enumerates quantum state basis values.
----------------------------------------------------------
public export
data StateBasisName
  = StateZero
  | StateOne
  | StatePlus
  | StateMinus
  | StatePlusI
  | StateMinusI

----------------------------------------------------------------
-- ContractName: enumerates built-in quantum contract literals.
----------------------------------------------------------------
public export
data ContractName
  = ContractClean
  | ContractStabilized
  | ContractBasis
  | ContractSeparable
  | ContractIsolated
  | ContractProduct
  | ContractCollapsed

----------------------------------------------------------------------
-- Keywords: reserved words that affect syntax.
----------------------------------------------------------------------
public export
data Keyword
  = KwAdjoint | KwAffine | KwAs
  | KwBarrier | KwBreak
  | KwCoisometry | KwClassical | KwCtrl | KwContinue | KwConst | KwDiscard
  | KwElse | KwEnsures | KwEnum | KwFn | KwFor | KwGeneral
  | KwIf | KwImpl | KwIn | KwIsometry
  | KwLet | KwLinear | KwLoop
  | KwMatch | KwMeasr | KwMod | KwMut | KwPub
  | KwQAlloc | KwQenum | KwQelse | KwQif | KwQmatch | KwReset | KwRequires | KwReturn
  | KwSelse | KwSif | KwSmatch | KwScratch | KwSelf | KwStruct | KwSupports | KwThen
  | KwUncompute | KwUncompsafe | KwUnitary | KwUse | KwWeaken | KwWhile

----------------------------------------------------------------------
-- Symbols: punctuation and operators.
----------------------------------------------------------------------
public export
data Symbol
  = SymHash                               -- #   (starts an annotation: #[...])
  | SymAmp                                -- &   (reserved; && exists too)
  | SymLParen | SymRParen                 -- ( )
  | SymLBracket | SymRBracket             -- [ ]
  | SymLBrace | SymRBrace                 -- { }
  | SymComma | SymSemi | SymColon         -- , ; :
  | SymDot                                -- .
  | SymBang                               -- !
  | SymEq                                 -- =
  | SymPlus | SymMinus | SymStar | SymSlash | SymPercent -- + - * / %
  | SymPlusEq | SymMinusEq | SymStarEq | SymSlashEq | SymPercentEq
  | SymWalrusEq                           --  :=
  | SymGt | SymGe | SymLt | SymLe         -- >, >=, <, <=
  | SymEqEq | SymNotEq                    -- ==, !=
  | SymAndAnd | SymOrOr                   -- && and ||
  | SymDotDot | SymDotDotEq               -- .. and ..=
  | SymDoubleColon                        -- ::
  | SymPipe | SymCaret                    -- | and ^
  | SymArrow                              -- ->
  | SymFatArrow                           -- =>  (match arm separator)
  | SymShl | SymShlEq | SymShr | SymShrEq -- <<, <<=, >>, >>=
  | SymAndEq | SymOrEq  |SymCaretEq       -- "&=", "|=", "^="
 
------------------------------------------------------------------------------------------------------------
-- A symbol table ordered from longest to shortest, the lexer must use longest match first to disambiguate.
-------------------------------------------------------------------------------------------------------------
public export
symbolTable : List (String, Symbol)
symbolTable =
  [ -- 3-character symbols
    (">>=", SymShrEq)
  , ("<<=", SymShlEq)
  , ("..=", SymDotDotEq)

    -- 2-character symbols
  , ("=>",  SymFatArrow)
  , ("->",  SymArrow)
  , ("==",  SymEqEq)
  , ("!=",  SymNotEq)
  , (">=",  SymGe)
  , ("<=",  SymLe)
  , ("&&",  SymAndAnd)
  , ("||",  SymOrOr)
  , ("+=",  SymPlusEq)
  , ("-=",  SymMinusEq)
  , ("*=",  SymStarEq)
  , ("/=",  SymSlashEq)
  , ("%=",  SymPercentEq)
  , ("&=",  SymAndEq)
  , ("|=",  SymOrEq)
  , ("^=",  SymCaretEq)
  , (":=",  SymWalrusEq)
  , ("::",  SymDoubleColon)
  , ("..",  SymDotDot)
  , (">>",  SymShr)
  , ("<<",  SymShl)

    -- 1-character symbols
  , ("#", SymHash)
  , ("&",   SymAmp)
  , ("(",   SymLParen)
  , (")",   SymRParen)
  , ("[",   SymLBracket)
  , ("]",   SymRBracket)
  , ("{",   SymLBrace)
  , ("}",   SymRBrace)
  , (",",   SymComma)
  , (";",   SymSemi)
  , (":",   SymColon)
  , (".",   SymDot)
  , ("!",   SymBang)
  , ("=",   SymEq)
  , ("+",   SymPlus)
  , ("-",   SymMinus)
  , ("*",   SymStar)
  , ("/",   SymSlash)
  , ("%",   SymPercent)
  , (">",   SymGt)
  , ("<",   SymLt)
  , ("|",   SymPipe)
  , ("^",   SymCaret)
  ]

----------------------------------------------------------------------
-- Token:
--   TokIdent "x"
--   TokIntLitRaw "123"
--   TokFloatLitRaw "3.14"
--   TokStringLitRaw "Hello"
--   TokBoolLit True
--   TokStateLit StateZero
--   TokContractLit ContractClean
--   TokKw KwLet
--   TokTypPrim TypPrimI32
--   TokGate GateH
--   TokSym SymPlusEq
--   TokUnderscore
----------------------------------------------------------------------
public export
data Token
  = TokIdent            String
  | TokIntLitRaw        String
  | TokFloatLitRaw      String
  | TokByteLitRaw       String
  | TokByteStringLitRaw String
  | TokBitStringLit     String
  | TokStringLitRaw     String
  | TokBoolLit          Bool
  | TokStateLit         StateBasisName
  | TokContractLit      ContractName
  | TokKw               Keyword
  | TokTypPrim          TypPrimName
  | TokGate             GateName
  | TokSym              Symbol
  | TokUnderscore
  | TokEOF
----------------------------------------------------------------------
-- Mappings used by lexer: identifier text -> token category
----------------------------------------------------------------------
public export
keywordFromString : String -> Maybe Keyword
keywordFromString s =
  case s of
    "affine"      => Just KwAffine
    "adjoint"     => Just KwAdjoint
    "as"          => Just KwAs
    "barrier"     => Just KwBarrier
    "break"       => Just KwBreak
    "coisometry"  => Just KwCoisometry
    "classical"   => Just KwClassical
    "const"       => Just KwConst
    "ctrl"        => Just KwCtrl
    "continue"    => Just KwContinue
    "discard"     => Just KwDiscard
    "else"        => Just KwElse
    "ensures"     => Just KwEnsures
    "enum"        => Just KwEnum
    "fn"          => Just KwFn
    "for"         => Just KwFor
    "general"     => Just KwGeneral
    "if"          => Just KwIf
    "impl"        => Just KwImpl
    "in"          => Just KwIn
    "isometry"    => Just KwIsometry
    "let"         => Just KwLet
    "linear"      => Just KwLinear
    "loop"        => Just KwLoop
    "match"       => Just KwMatch
    "measr"       => Just KwMeasr
    "mod"         => Just KwMod
    "mut"         => Just KwMut
    "pub"         => Just KwPub
    "qalloc"      => Just KwQAlloc
    "qenum"       => Just KwQenum
    "qelse"       => Just KwQelse
    "qif"         => Just KwQif
    "qmatch"      => Just KwQmatch
    "requires"    => Just KwRequires
    "reset"       => Just KwReset
    "return"      => Just KwReturn
    "selse"       => Just KwSelse
    "sif"         => Just KwSif
    "smatch"      => Just KwSmatch
    "scratch"     => Just KwScratch
    "self"        => Just KwSelf
    "struct"      => Just KwStruct
    "supports"    => Just KwSupports
    "then"        => Just KwThen
    "uncompute"   => Just KwUncompute
    "uncompsafe"  => Just KwUncompsafe
    "unitary"     => Just KwUnitary
    "use"         => Just KwUse
    "weaken"      => Just KwWeaken
    "while"       => Just KwWhile
    _             => Nothing

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
    "ZZ"    => Just GateZZ
    _       => Nothing

public export
stateBasisFromString : String -> Maybe StateBasisName
stateBasisFromString s =
  case s of
    "zero"   => Just StateZero
    "one"    => Just StateOne
    "plus"   => Just StatePlus
    "minus"  => Just StateMinus
    "plusi"  => Just StatePlusI
    "minusi" => Just StateMinusI
    _        => Nothing

public export
contractFromString : String -> Maybe ContractName
contractFromString s =
  case s of
    "clean"       => Just ContractClean
    "stabilized"  => Just ContractStabilized
    "basis"       => Just ContractBasis
    "separable"   => Just ContractSeparable
    "isolated"    => Just ContractIsolated
    "product"     => Just ContractProduct
    _             => Nothing

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
    "squbit"  => Just TypPrimSQubit
    "str"     => Just TypPrimStrSlice
    "String"  => Just TypPrimString
    _         => Nothing

----------------------------------------------------------------------
-- Implementation and Derivations for debugging/testing
----------------------------------------------------------------------

public export
showKeywordLeaf : Keyword -> String
showKeywordLeaf kw =
  case kw of
    KwAdjoint   => "adjoint"
    KwAffine    => "affine"
    KwAs        => "as"
    KwBarrier   => "barrier"
    KwBreak     => "break"
    KwCoisometry => "coisometry"
    KwClassical => "classical"
    KwConst     => "const"
    KwCtrl      => "ctrl"
    KwContinue  => "continue"
    KwDiscard   => "discard"
    KwElse      => "else"
    KwEnsures   => "ensures"
    KwEnum      => "enum"
    KwFn        => "fn"
    KwFor       => "for"
    KwGeneral   => "general"
    KwIf        => "if"
    KwImpl      => "impl"
    KwIn        => "in"
    KwIsometry  => "isometry"
    KwLet       => "let"
    KwLinear    => "linear"
    KwLoop      => "loop"
    KwMatch     => "match"
    KwMeasr     => "measr"
    KwMod       => "mod"
    KwMut       => "mut"
    KwPub       => "pub"
    KwQAlloc    => "qalloc"
    KwQenum     => "qenum"
    KwQelse     => "qelse"
    KwQif       => "qif"
    KwQmatch    => "qmatch"
    KwReset     => "reset"
    KwRequires  => "requires"
    KwReturn    => "return"
    KwScratch   => "scratch"
    KwSelf      => "self"
    KwSelse     => "selse"
    KwSif       => "sif"
    KwSmatch    => "smatch"
    KwSupports  => "supports"
    KwThen      => "then"
    KwUncompute => "uncompute"
    KwUncompsafe => "uncompsafe"
    KwUnitary   => "unitary"
    KwUse       => "use"
    KwWeaken    => "weaken"
    KwWhile     => "while"

public export
showStateBasisLeaf : StateBasisName -> String
showStateBasisLeaf sb =
  case sb of
    StateZero   => "zero"
    StateOne    => "one"
    StatePlus   => "plus"
    StateMinus  => "minus"
    StatePlusI  => "plusi"
    StateMinusI => "minusi"

public export
showSymbolLeaf : Symbol -> String
showSymbolLeaf sym =
  case sym of
    SymAmp         => "&"
    SymHash        => "#"
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
    SymShl         => "<<"
    SymShlEq       => "<<="
    SymShr         => ">>"
    SymShrEq       => ">>="
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
    SymAndEq       => "&="
    SymOrEq        => "|="
    SymCaretEq     => "^="
    SymBang        => "!"

public export
implementation Show Keyword where
  show = showKeywordLeaf

public export
implementation Show Symbol where
  show = showSymbolLeaf

%runElab derive "GateName" [Show, Eq]
%runElab derive "TypPrimName" [Show, Eq]
%runElab derive "StateBasisName" [Show, Eq]
%runElab derive "ContractName" [Show, Eq]
%runElab derive "Keyword" [Eq]
%runElab derive "Symbol" [Eq]
%runElab derive "Token" [Show, Eq]
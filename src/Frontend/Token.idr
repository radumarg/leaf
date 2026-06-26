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
  | TypPrimChar
  | TypPrimString
  | TypPrimStrSlice
  | TypPrimQubit
  | TypPrimQState

----------------------------------------------------------
-- StateBasisName: enumerates quantum basis states values.
-- These are globally reserved quantum state literals.
----------------------------------------------------------
public export
data StateBasisName
  = StateZero
  | StateOne
  | StatePlus
  | StateMinus
  | StatePlusI
  | StateMinusI

----------------------------------------------------------------------
-- Keywords: reserved words that affect syntax.
----------------------------------------------------------------------
public export
data Keyword
  = KwAdjoint
  | KwAffine
  | KwAs
  | KwBreak
  | KwCoisometry
  | KwClassical
  | KwContinue
  | KwConst
  | KwElse
  | KwEnsures
  | KwEnum
  | KwFn
  | KwFor
  | KwGeneral
  | KwIf
  | KwImpl
  | KwIn
  | KwIsometry
  | KwLet
  | KwLinear
  | KwLoop
  | KwMatch
  | KwMod
  | KwMut
  | KwPub
  | KwQenum
  | KwQelse
  | KwQif
  | KwQmatch
  | KwRequires
  | KwReturn
  | KwSelse
  | KwSif
  | KwSmatch
  | KwScratch
  | KwSelf
  | KwStruct
  | KwSupports
  | KwThen
  | KwUncompsafe
  | KwUnitary
  | KwUse
  | KwWhile

----------------------------------------------------------------------
-- Builtin: reserved intrinsic function names.
-- These cannot be shadowed by user declarations.
----------------------------------------------------------------------
public export
data Builtin
  = BuiltinBarrier
  | BuiltinCtrl
  | BuiltinOn
  | BuiltinApply
  | BuiltinBasis
  | BuiltinClean
  | BuiltinDiscard
  | BuiltinIsolated
  | BuiltinMeasr
  | BuiltinProduct
  | BuiltinQAlloc
  | BuiltinReset
  | BuiltinTensor
  | BuiltinSeparable
  | BuiltinStabilized
  | BuiltinUncompute
  | BuiltinWeaken

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
  | SymAndEq | SymOrEq | SymCaretEq       -- "&=", "|=", "^="
 
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
--   TokByteLitRaw "b'a'"
--   TokByteStringLitRaw "b\"hello\""
--   TokBasisStringLitRaw "bs\"01+-iI\""
--   TokCharLitRaw "'a'"
--   TokStringLitRaw "\"Hello\""
--   TokOuterDoc "/// docs for following item"
--   TokInnerDoc "//! docs for enclosing item"
--   TokBoolLit True
--   TokStateLit StateZero
--   TokKw KwLet
--   TokTypPrim TypPrimI32
--   TokSym SymPlusEq
--   TokBuiltin BuiltinMeasr
--   TokUnderscore
--   TokEOF
----------------------------------------------------------------------
public export
data Token
  = TokIdent             String
  | TokIntLitRaw         String
  | TokFloatLitRaw       String
  | TokByteLitRaw        String
  | TokByteStringLitRaw  String
  | TokBasisStringLitRaw String
  | TokCharLitRaw        String
  | TokStringLitRaw      String
  | TokOuterDoc          String   -- /// line  or  /** … */ block: documents the item that FOLLOWS
  | TokInnerDoc          String   -- //! line  or  /*! … */ block: documents the ENCLOSING item
  | TokBoolLit           Bool
  | TokStateLit          StateBasisName
  | TokKw                Keyword
  | TokTypPrim           TypPrimName
  | TokSym               Symbol
  | TokBuiltin           Builtin
  | TokUnderscore
  | TokEOF
----------------------------------------------------------------------
-- Mappings used by lexer: identifier text -> token category
----------------------------------------------------------------------
public export
keywordFromString : String -> Maybe Keyword
keywordFromString s =
  case s of
    "adjoint"     => Just KwAdjoint
    "affine"      => Just KwAffine
    "as"          => Just KwAs
    "break"       => Just KwBreak
    "coisometry"  => Just KwCoisometry
    "classical"   => Just KwClassical
    "const"       => Just KwConst
    "continue"    => Just KwContinue
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
    "mod"         => Just KwMod
    "mut"         => Just KwMut
    "pub"         => Just KwPub
    "qenum"       => Just KwQenum
    "qelse"       => Just KwQelse
    "qif"         => Just KwQif
    "qmatch"      => Just KwQmatch
    "requires"    => Just KwRequires
    "return"      => Just KwReturn
    "selse"       => Just KwSelse
    "sif"         => Just KwSif
    "smatch"      => Just KwSmatch
    "scratch"     => Just KwScratch
    "self"        => Just KwSelf
    "struct"      => Just KwStruct
    "supports"    => Just KwSupports
    "then"        => Just KwThen
    "uncompsafe"  => Just KwUncompsafe
    "unitary"     => Just KwUnitary
    "use"         => Just KwUse
    "while"       => Just KwWhile
    _             => Nothing

public export
builtinFromString : String -> Maybe Builtin
builtinFromString s =
  case s of
    "barrier"    => Just BuiltinBarrier
    "ctrl"       => Just BuiltinCtrl
    "on"         => Just BuiltinOn
    "apply"      => Just BuiltinApply
    "basis"      => Just BuiltinBasis
    "clean"      => Just BuiltinClean
    "discard"    => Just BuiltinDiscard
    "isolated"   => Just BuiltinIsolated
    "measr"      => Just BuiltinMeasr
    "product"    => Just BuiltinProduct
    "qalloc"     => Just BuiltinQAlloc
    "reset"      => Just BuiltinReset
    "tensor"     => Just BuiltinTensor
    "separable"  => Just BuiltinSeparable
    "stabilized" => Just BuiltinStabilized
    "uncompute"  => Just BuiltinUncompute
    "weaken"     => Just BuiltinWeaken
    _             => Nothing

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
typeFromString : String -> Maybe TypPrimName
typeFromString s =
  case s of
    "angle32" => Just TypPrimAngle32
    "angle64" => Just TypPrimAngle64
    "bit"     => Just TypPrimBit
    "bool"    => Just TypPrimBool
    "char"    => Just TypPrimChar
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
    "qstate"  => Just TypPrimQState
    "str"     => Just TypPrimStrSlice
    "String"  => Just TypPrimString
    _         => Nothing

public export
boolFromString : String -> Maybe Bool
boolFromString s =
  case s of
    "true"  => Just True
    "false" => Just False
    _       => Nothing


---------------------------------------------------------------------
-- single authoritative classification order for token-like strings:
-- bools, keywords, types, state basis, builtins, identifiers
---------------------------------------------------------------------
public export
tokenFromIdentLike : String -> Token
tokenFromIdentLike s =
  case s of
    "_" => TokUnderscore
    _ =>
      case boolFromString s of
        Just b => TokBoolLit b
        Nothing =>
          case keywordFromString s of
            Just kw => TokKw kw
            Nothing =>
              case typeFromString s of
                Just ty => TokTypPrim ty
                Nothing =>
                  case stateBasisFromString s of
                    Just st => TokStateLit st
                    Nothing =>
                      case builtinFromString s of
                        Just bi => TokBuiltin bi
                        Nothing => TokIdent s

----------------------------------------------------------------------
-- Implementation and Derivations for debugging/testing
----------------------------------------------------------------------

public export
showKeywordLeaf : Keyword -> String
showKeywordLeaf kw =
  case kw of
    KwAdjoint  => "adjoint"
    KwAffine    => "affine"
    KwAs        => "as"
    KwBreak     => "break"
    KwCoisometry => "coisometry"
    KwClassical => "classical"
    KwConst     => "const"
    KwContinue  => "continue"
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
    KwMod       => "mod"
    KwMut       => "mut"
    KwPub       => "pub"
    KwQenum     => "qenum"
    KwQelse     => "qelse"
    KwQif       => "qif"
    KwQmatch    => "qmatch"
    KwRequires  => "requires"
    KwReturn    => "return"
    KwScratch   => "scratch"
    KwSelf      => "self"
    KwSelse     => "selse"
    KwSif       => "sif"
    KwSmatch    => "smatch"
    KwStruct    => "struct"
    KwSupports  => "supports"
    KwThen      => "then"
    KwUncompsafe => "uncompsafe"
    KwUnitary   => "unitary"
    KwUse       => "use"
    KwWhile     => "while"

public export
showBuiltinLeaf : Builtin -> String
showBuiltinLeaf b =
  case b of
    BuiltinBarrier   => "barrier"
    BuiltinCtrl      => "ctrl"
    BuiltinOn        => "on"
    BuiltinApply     => "apply"
    BuiltinBasis     => "basis"
    BuiltinClean     => "clean"
    BuiltinDiscard   => "discard"
    BuiltinIsolated  => "isolated"
    BuiltinMeasr     => "measr"
    BuiltinProduct   => "product"
    BuiltinQAlloc    => "qalloc"
    BuiltinReset     => "reset"
    BuiltinTensor    => "tensor"
    BuiltinSeparable => "separable"
    BuiltinStabilized => "stabilized"
    BuiltinUncompute => "uncompute"
    BuiltinWeaken    => "weaken"

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
showTypPrimLeaf : TypPrimName -> String
showTypPrimLeaf ty =
  case ty of
    TypPrimAngle32  => "angle32"
    TypPrimAngle64  => "angle64"
    TypPrimBit      => "bit"
    TypPrimBool     => "bool"
    TypPrimChar     => "char"
    TypPrimF32      => "f32"
    TypPrimF64      => "f64"
    TypPrimI8       => "i8"
    TypPrimI16      => "i16"
    TypPrimI32      => "i32"
    TypPrimI64      => "i64"
    TypPrimI128     => "i128"
    TypPrimParam    => "param"
    TypPrimU8       => "u8"
    TypPrimU16      => "u16"
    TypPrimU32      => "u32"
    TypPrimU64      => "u64"
    TypPrimU128     => "u128"
    TypPrimQubit    => "qubit"
    TypPrimQState   => "qstate"
    TypPrimString   => "String"
    TypPrimStrSlice => "str"

public export
implementation Show Keyword where
  show = showKeywordLeaf

public export
implementation Show Builtin where
  show = showBuiltinLeaf

public export
implementation Show Symbol where
  show = showSymbolLeaf

public export
implementation Show StateBasisName where
  show = showStateBasisLeaf

public export
implementation Show TypPrimName where
  show = showTypPrimLeaf

%runElab derive "TypPrimName" [Eq]
%runElab derive "StateBasisName" [Eq]
%runElab derive "Builtin" [Eq]
%runElab derive "Keyword" [Eq]
%runElab derive "Symbol" [Eq]
%runElab derive "Token" [Show, Eq]
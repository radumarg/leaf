module Frontend.Syntax.Literal

import Frontend.Token
import Frontend.ASTPhases

%default total

--------------------------------------------------------------------------------
-- Literals in the AST
--------------------------------------------------------------------------------
-- One node family for every literal form in Leaf source:
--
--   1000  1_000  0xff_u8  0b1111_0000i64      -- integer literals
--   1.0  1.0e-3  12E+99_f64  5f32             -- float literals
--   "Hello"                                   -- string literals
--   b'a'  b'\n'  b'\x41'                      -- byte literals
--   b"hello"  b"ABC\x41"                      -- byte-string literals
--   bs"10+-iI"                                -- basis-string literals
--   true  false                               -- boolean literals
--   ()                                        -- unit literal
--   zero one plus minus plusi minusi          -- quantum state literals
--
--   * Numeric, string, byte, byte-string, and basis-string literals preserve
--     their RAW SOURCE SPELLING, exactly as the lexer captured it: quotes,
--     prefixes (0x/0o/0b/b/bs), digit separators (1_000), exponents, and
--     type suffixes (0xff_u8, 5f32) all included.
--------------------------------------------------------------------------------

public export
data LiteralNode
  = LiteralIntegerRaw     String          -- e.g. "1_000", "0xff_u8", "0b1010"
  | LiteralFloatRaw       String          -- e.g. "1.0e-3", "12E+99_f64", "5f32"
  | LiteralStringRaw      String          -- raw spelling including quotes
  | LiteralByteRaw        String          -- e.g. "b'a'", "b'\\x41'"
  | LiteralByteStringRaw  String          -- e.g. "b\"hello\""
  | LiteralBasisStringRaw String          -- e.g. "bs\"10+-iI\""
  | LiteralBoolean        Bool            -- true / false, already decoded
  | LiteralUnit                           -- ()
  | LiteralQuantumState   BasisStateName  -- zero / one / plus / minus / plusi / minusi

--------------------------------------------------------------------------------
-- Phase wrappers
--------------------------------------------------------------------------------

public export
Literal : AstPhase -> Type
Literal phase = AstNode phase LiteralNode

public export
SurfaceLiteral : Type
SurfaceLiteral = Literal SurfaceAstPhase

public export
CanonicalLiteral : Type
CanonicalLiteral = Literal CanonicalAstPhase

public export
ResolvedLiteral : Type
ResolvedLiteral = Literal ResolvedAstPhase

public export
TypedLiteral : Type
TypedLiteral = Literal TypedAstPhase
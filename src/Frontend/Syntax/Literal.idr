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
-- Design choices:
--
--   * Numeric, string, byte, byte-string, and basis-string literals preserve
--     their RAW SOURCE SPELLING, exactly as the lexer captured it: quotes,
--     prefixes (0x/0o/0b/b/bs), digit separators (1_000), exponents, and
--     type suffixes (0xff_u8, 5f32) all included. The AST does NOT eagerly
--     normalize. Later passes parse suffixes, infer types, check ranges, and
--     validate escapes/basis characters -- and can report precise errors
--     ("suffix u8 out of range for value 0x1ff") against the exact spelling
--     the user wrote. This mirrors the TokIntLitRaw / TokFloatLitRaw / ...
--     convention in Token.idr.
--
--   * Boolean and quantum-state literals are stored DECODED, because the
--     lexer already decodes them (TokBoolLit Bool, TokStateLit
--     BasisStateName) and each has a fixed spelling per value -- raw text
--     would add nothing. `BasisStateName` is reused from Frontend.Token
--     rather than duplicated: `zero`/`one`/`plus`/`minus`/`plusi`/`minusi`
--     have exactly one authoritative enumeration in the frontend.
--
--   * `()` is treated as the unit LITERAL, per the spec's basic-type table
--     (`let unit : () = ();`). Deciding that a source `()` is this literal
--     rather than an empty tuple expression is the parser's job; the AST
--     simply provides the constructor.
--
--   * Basis-string literals (bs"...") appear both as expressions (qstate
--     initialization, ctrl(...).on(bs"10")) and as qmatch/smatch patterns.
--     This module only defines the literal itself; the pattern module will
--     reference the same raw-spelling convention.
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
-- Literals are phase-invariant payloads, like doc comments: resolution never
-- touches them, and type checking attaches its result via the TypedAstNode
-- wrapper rather than by rewriting the literal. All four aliases are provided
-- so every phase's expression tree can embed literals directly.
--------------------------------------------------------------------------------

public export
SurfaceLiteral : Type
SurfaceLiteral = SurfaceAstNode LiteralNode

public export
CanonicalLiteral : Type
CanonicalLiteral = CanonicalAstNode LiteralNode

public export
ResolvedLiteral : Type
ResolvedLiteral = ResolvedAstNode LiteralNode

public export
TypedLiteral : Type
TypedLiteral = TypedAstNode LiteralNode
module Frontend.Syntax.Attribute

import Frontend.ASTPhases
import Frontend.Syntax.Name

%default total

--------------------------------------------------------------------------------
-- Attributes (annotations) in the AST
--------------------------------------------------------------------------------
-- Leaf attributes are Rust-style `#[...]` annotations placed before an item:
--
--   #[qasm_gate]
--   #[qasm_gate("my_gate")]
--   #[qasm_def]
--   #[qasm_def("my_def")]
--
-- Design choices:
--
--   * Every phase stores an attribute GENERICALLY: a name plus an optional
--     argument list -- indexed by `phase : AstPhase` like every other node
--     family, following ASTPhases.idr's XFor convention.
--
--   * The parser enforces argument SHAPE directly, for every attribute
--     regardless of name: `#[name]` (no arguments) or `#[name("string")]`
--     (exactly one string literal argument). Anything else -- `#[name()]`,
--     a non-string argument, more than one argument -- is a parse error.
--
--   * Attribute NAME is a separate, semantic question from shape: unknown
--     names are preserved by the parser and rejected by a later validation
--     pass (`PostParseValidation`) instead.
--
--   * Attribute names are NEVER resolved to program symbols -- they are
--     compiler-directed metadata, not references to bindings. So unlike
--     every other name in the tree, an attribute's name does NOT route
--     through the `Name phase` family (which carries a SymbolId from
--     ResolvedAstPhase onward) -- it stays the plain textual `NameNode` at every
--     phase, ResolvedAstPhase and TypedAstPhase included.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Known attribute kinds
--------------------------------------------------------------------------------
-- The attributes the compiler currently understands needed by the
-- `PostParseValidation` pass.
--------------------------------------------------------------------------------

public export
data KnownAttributeKind
  = KnownQasmGate  -- #[qasm_gate] / #[qasm_gate("name")]
                   -- compile to an OpenQASM 3 gate (unitary subroutine)
  | KnownQasmDef   -- #[qasm_def] / #[qasm_def("name")]
                   -- compile to a named OpenQASM 3 def subroutine

public export
showKnownAttributeLeaf : KnownAttributeKind -> String
showKnownAttributeLeaf kind =
  case kind of
    KnownQasmGate => "qasm_gate"
    KnownQasmDef  => "qasm_def"

public export
implementation Show KnownAttributeKind where
  show = showKnownAttributeLeaf

public export
recognizeKnownAttribute : String -> Maybe KnownAttributeKind
recognizeKnownAttribute s =
  case s of
    "qasm_gate" => Just KnownQasmGate
    "qasm_def"  => Just KnownQasmDef
    _           => Nothing

--------------------------------------------------------------------------------
-- Attribute arguments
--------------------------------------------------------------------------------
-- `parseAttribute` accepts at most one argument, and only a string literal:
--
--   #[qasm_gate("my_gate")]              -- string literal argument
--
-- Literal spellings are preserved raw (quotes included), matching the
-- convention used for literal tokens elsewhere in the frontend. Phase-
-- invariant payload, like Literal.idr/Doc.idr: no phase ever rewrites an
-- argument's raw spelling.
--------------------------------------------------------------------------------

public export
data AttributeArgumentNode
  = AttributeArgumentStringLit String  -- raw spelling, e.g. "\"my_gate\""

public export
AttributeArgument : AstPhase -> Type
AttributeArgument phase = AstNode phase AttributeArgumentNode

public export
SurfaceAttributeArgument : Type
SurfaceAttributeArgument = AttributeArgument SurfaceAstPhase

public export
CanonicalAttributeArgument : Type
CanonicalAttributeArgument = AttributeArgument CanonicalAstPhase

public export
ResolvedAttributeArgument : Type
ResolvedAttributeArgument = AttributeArgument ResolvedAstPhase

public export
TypedAttributeArgument : Type
TypedAttributeArgument = AttributeArgument TypedAstPhase

--------------------------------------------------------------------------------
-- Attribute node
--------------------------------------------------------------------------------
-- One `#[...]` annotation.
--
-- `attributeArguments` distinguishes the two source forms:
--
--   Nothing  <=>  #[qasm_gate]      -- no argument list written at all
--   Just []  <=>  #[qasm_gate()]    -- an explicit, empty argument list
--
-- The distinction is preserved because it is visible in source (and a later
-- pass may well want to reject the `Just []` form for known attributes).
--
-- `attributeName` is deliberately `AstNode phase NameNode`, NOT
-- `Name phase` -- see the module header: attribute names never resolve to a
-- SymbolId at any phase.
--------------------------------------------------------------------------------

public export
record AttributeNode (phase : AstPhase) where
  constructor MkAttributeNode
  attributeName      : AstNode phase NameNode
  attributeArguments : Maybe (List (AttributeArgument phase))

public export
Attribute : AstPhase -> Type
Attribute phase = AstNode phase (AttributeNode phase)

public export
SurfaceAttribute : Type
SurfaceAttribute = Attribute SurfaceAstPhase

public export
CanonicalAttribute : Type
CanonicalAttribute = Attribute CanonicalAstPhase

public export
ResolvedAttribute : Type
ResolvedAttribute = Attribute ResolvedAstPhase

public export
TypedAttribute : Type
TypedAttribute = Attribute TypedAstPhase

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
--   * The surface AST stores every attribute GENERICALLY: a name plus an
--     optional argument list. 
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
--   * Attribute names are never resolved to program symbols -- they are
--     compiler-directed metadata, not references to bindings -- so they reuse
--     the plain textual `Name` machinery and stay textual in every phase.
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
-- convention used for literal tokens elsewhere in the frontend.
--------------------------------------------------------------------------------

public export
data AttributeArgumentNode
  = AttributeArgumentStringLit String  -- raw spelling, e.g. "\"my_gate\""

public export
SurfaceAttributeArgument : Type
SurfaceAttributeArgument = SurfaceAstNode AttributeArgumentNode

public export
CanonicalAttributeArgument : Type
CanonicalAttributeArgument = CanonicalAstNode AttributeArgumentNode

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
--------------------------------------------------------------------------------

public export
record AttributeNode (name : Type) (argument : Type) where
  constructor MkAttributeNode
  attributeName      : name
  attributeArguments : Maybe (List argument)

public export
SurfaceAttribute : Type
SurfaceAttribute = SurfaceAstNode (AttributeNode SurfaceName SurfaceAttributeArgument)

public export
CanonicalAttribute : Type
CanonicalAttribute = CanonicalAstNode (AttributeNode CanonicalName CanonicalAttributeArgument)

--------------------------------------------------------------------------------
-- Later phases
--------------------------------------------------------------------------------
-- Unlike doc comments (Doc.idr), attributes do not get Resolved/Typed aliases
-- here. Attribute names never resolve to SymbolIds, so `ResolvedName` (which
-- carries a mandatory SymbolId) is the wrong instantiation. Post-resolution
-- declarations should either keep carrying `CanonicalAttribute` unchanged, or
-- an attribute-validation pass should replace attributes with a checked form
-- (known kind + validated argument, unknown preserved). That decision belongs
-- to the phase that owns it and is deliberately not prejudged here.
--------------------------------------------------------------------------------
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
--     optional argument list. It does NOT bake "known vs. unknown" into the
--     node shape. A malformed known attribute such as
--
--       #[qasm_gate(foo, "bar", 3)]
--
--     still parses into the same node shape as a well-formed one, so a later
--     validation pass can report "qasm_gate expects at most one string
--     argument" against the exact spans the user wrote, instead of the parser
--     having to reject or silently downgrade it to "unknown".
--
--   * Unknown attributes are preserved verbatim:
--
--       #[some_future_attribute(foo, "bar")]
--
--     parses fine; whether the compiler understands the attribute is a
--     semantic question, not a syntactic one.
--
--   * `KnownAttributeKind` therefore lives NEXT TO the node, not inside it:
--     `recognizeKnownAttribute` classifies an attribute name after parsing.
--     Storing the classification on the node would duplicate the name and
--     create a keep-in-sync invariant for no benefit.
--
--   * Attribute names are never resolved to program symbols -- they are
--     compiler-directed metadata, not references to bindings -- so they reuse
--     the plain textual `Name` machinery and stay textual in every phase.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Known attribute kinds
--------------------------------------------------------------------------------
-- The attributes the compiler currently understands. Spelled out directly
-- (like `boolFromString` in Token.idr) rather than via the Finite/findByShow
-- machinery: with two constructors the derivation buys nothing.
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

-- Classify an attribute name. `Nothing` means the attribute is unknown to
-- this compiler version; unknown attributes are preserved, and whether they
-- are an error or a warning is a later pass's policy decision.
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
-- The argument grammar inside `#[name(...)]` is deliberately small: bare
-- identifiers and literals, comma-separated. This is NOT the full expression
-- grammar -- attributes are metadata, not code.
--
--   #[qasm_gate("my_gate")]              -- string literal argument
--   #[some_future_attribute(foo, "bar")] -- identifier + string arguments
--
-- Literal spellings are preserved raw (quotes included), matching the
-- convention used for literal tokens elsewhere in the frontend.
--------------------------------------------------------------------------------

public export
data AttributeArgumentNode
  = AttributeArgumentName      String  -- bare identifier, e.g. foo
  | AttributeArgumentStringLit String  -- raw spelling, e.g. "\"my_gate\""
  | AttributeArgumentIntLit    String  -- raw spelling, e.g. "3"

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
--
-- Parameterized over the name and argument node types so each phase can
-- instantiate it with its own located wrappers, following the `PathNode` /
-- `QualifiedNameNode` pattern in Name.idr.
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
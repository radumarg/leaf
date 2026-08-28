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
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Known attribute kinds
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

module Frontend.Syntax.Common

import Derive.Prelude
import Language.Reflection

%default total
%language ElabReflection

-- Derive.Prelude re-exports Language.Reflection, whose TT module defines its
-- own `Visibility` (the elaborator's Private/Export/Public for declarations).
-- Hide it so the unqualified name unambiguously means Leaf's Visibility.
%hide Language.Reflection.TT.Visibility

--------------------------------------------------------------------------------
-- Shared syntax enums
--------------------------------------------------------------------------------
-- Small enums used across declarations, parameters, types, and let-bindings.
-- Like Operator.idr, these are VOCABULARY, not located nodes: none carries a
-- span or a phase wrapper. When a diagnostic needs to point at the keyword
-- itself ("`pub` is not allowed here", "`mut` is never written on a qubit
-- reference"), the owning node records the keyword's SourceSpan alongside
-- the enum value.
--
-- Each enum carries a `show...Leaf` source-spelling function in the
-- Token.idr style, so diagnostics quote exactly what the user wrote.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Visibility
--------------------------------------------------------------------------------
-- Whether an item is marked `pub`. `VisibilityPrivate` means NO keyword was
-- written -- there is no `priv` spelling in Leaf -- so items store a plain
-- `Visibility` rather than a `Maybe Visibility`; absence IS the private case.
--------------------------------------------------------------------------------

public export
data Visibility = VisibilityPublic 

%runElab derive "Visibility" [Eq]

public export
showVisibilityLeaf : Visibility -> String
showVisibilityLeaf v = "pub"

public export
implementation Show Visibility where
  show = showVisibilityLeaf

--------------------------------------------------------------------------------
-- Mutability
--------------------------------------------------------------------------------
-- Whether a BINDING is marked `mut`:
--
--   let mut x = 0;
--   fn f(mut x: i32) -> i32 { ... }
--
-- As with Visibility, `Immutable` means no keyword written. Reference
-- mutability (&T vs. &mut T) is NOT this type: that is BorrowKind in
-- Operator.idr, kept separate because the two occupy different grammatical
-- positions and Leaf's qubit rules treat them differently.
--------------------------------------------------------------------------------

public export
data Mutability
  = Mutable    -- `mut` written in source
  | Immutable  -- nothing written (the default)

%runElab derive "Mutability" [Eq]

public export
showMutabilityLeaf : Mutability -> String
showMutabilityLeaf m =
  case m of
    Mutable   => "mut"
    Immutable => ""

public export
implementation Show Mutability where
  show = showMutabilityLeaf

--------------------------------------------------------------------------------
-- Function effects
--------------------------------------------------------------------------------
-- The optional effect qualifier written before `fn`:
--
--   classical fn f(x: i32) -> i32 { ... }
--   unitary fn had(q: qubit) -> qubit { ... }
--
-- The enum has a constructor for EVERY spellable effect, including `general`,
-- even though general is the semantic default. A function declaration should
-- store `Maybe FunctionEffect`:
--
--   Nothing              -- no qualifier written; treated as general later
--   Just EffectGeneral   -- the user explicitly wrote `general`
--
-- The distinction is surface-real (explicit `general` is used for API
-- specification per the spec) and costs nothing to keep. Collapsing
-- `Nothing` into general is a canonicalization step, not a parsing step.
--------------------------------------------------------------------------------

public export
data FunctionEffect
  = EffectClassical   -- no quantum operations
  | EffectUncompsafe  -- only uncomputation-safe quantum operations
  | EffectUnitary     -- unitary; #outputs == #inputs
  | EffectIsometry    -- unitary; #outputs >  #inputs
  | EffectCoisometry  -- unitary; #outputs <  #inputs
  | EffectGeneral     -- may measure / reset / discard (the default)

%runElab derive "FunctionEffect" [Eq]

public export
showFunctionEffectLeaf : FunctionEffect -> String
showFunctionEffectLeaf e =
  case e of
    EffectClassical  => "classical"
    EffectUncompsafe => "uncompsafe"
    EffectUnitary    => "unitary"
    EffectIsometry   => "isometry"
    EffectCoisometry => "coisometry"
    EffectGeneral    => "general"

public export
implementation Show FunctionEffect where
  show = showFunctionEffectLeaf

--------------------------------------------------------------------------------
-- Support clauses
--------------------------------------------------------------------------------
-- The kinds that may appear after `supports` in a function signature:
--
--   unitary fn f(q: &qubit) supports adjoint, ctrl { ... }
--
-- A declaration stores the written clause as a list of these, in source
-- order. (Lexically, `adjoint` arrives as a keyword token and `ctrl` as a
-- builtin token; that asymmetry is the parser's problem, not the AST's.)
--------------------------------------------------------------------------------

public export
data SupportKind
  = SupportAdjoint  -- supports adjoint
  | SupportCtrl     -- supports ctrl

%runElab derive "SupportKind" [Eq]

public export
showSupportKindLeaf : SupportKind -> String
showSupportKindLeaf s =
  case s of
    SupportAdjoint => "adjoint"
    SupportCtrl    => "ctrl"

public export
implementation Show SupportKind where
  show = showSupportKindLeaf

--------------------------------------------------------------------------------
-- Quantum storage qualifiers
--------------------------------------------------------------------------------
-- The qualifiers that may appear between `let` and the binder:
--
--   let linear q: qubit = qalloc();
--   let affine q: qubit = qalloc();
--   let scratch q: qubit = qalloc();
--   let scratch linear qs: [qubit; 2] = qalloc(2);
--   let linear scratch q: qubit = qalloc();
--
-- Binders store `List QuantumStorageQualifier` IN SOURCE ORDER, so
-- `scratch linear` and `linear scratch` round-trip differently even though
-- they mean the same thing. The AST does not forbid nonsense combinations
-- (`linear affine`, `scratch scratch`): rejecting those with a good message
-- ("`linear` and `affine` are mutually exclusive") is a validation pass's
-- job, using the qualifiers' spans recorded on the binder.
--------------------------------------------------------------------------------

public export
data QuantumStorageQualifier
  = QualifierLinear   -- consume exactly once; discard must be explicit
  | QualifierAffine   -- consume at most once; implicit discard allowed
  | QualifierScratch  -- auto-uncomputed and reclaimed at end of scope

%runElab derive "QuantumStorageQualifier" [Eq]

public export
showQuantumStorageQualifierLeaf : QuantumStorageQualifier -> String
showQuantumStorageQualifierLeaf q =
  case q of
    QualifierLinear  => "linear"
    QualifierAffine  => "affine"
    QualifierScratch => "scratch"

public export
implementation Show QuantumStorageQualifier where
  show = showQuantumStorageQualifierLeaf

--------------------------------------------------------------------------------
-- Initializer markers
--------------------------------------------------------------------------------
-- The operator between a let-binder and its initializer:
--
--   let q: qubit = f(q);    -- ordinary binding
--   let q: qubit := f(q);   -- := marks the binding for AUTOMATIC
--                           -- UNCOMPUTATION when the enclosing fn returns
--
-- `:=` is deliberately NOT an AssignmentOperator (Operator.idr): it is legal
-- only in `let` bindings, never in assignment statements (there is no
-- `x := e;`), and it changes the binding's semantics rather than performing
-- a different operation. The Let node requires exactly one of these two
-- markers whenever an initializer is present, which makes `x := 5;`
-- unrepresentable as an assignment statement by construction.
--
-- NOTE: this type supersedes `LetBindingOperator` in Operator.idr, which
-- should be deleted from there -- one home for this distinction, not two.
--------------------------------------------------------------------------------

public export
data InitializerMarker
  = InitializerEquals         -- =
  | InitializerAutoUncompute  -- :=

%runElab derive "InitializerMarker" [Eq]

public export
showInitializerMarkerLeaf : InitializerMarker -> String
showInitializerMarkerLeaf m =
  case m of
    InitializerEquals        => "="
    InitializerAutoUncompute => ":="

public export
implementation Show InitializerMarker where
  show = showInitializerMarkerLeaf